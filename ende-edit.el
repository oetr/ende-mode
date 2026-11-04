;;; ende-edit.el --- Structured edit protocols for ende -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter Samarin
;;
;; Author: Peter Samarin <peter.samarin@gmail.com>
;; Maintainer: Peter Samarin <peter.samarin@gmail.com>
;; Keywords: convenience, tools
;; URL: https://github.com/oetr/ende-mode
;; SPDX-License-Identifier: AGPL-3.0-only

;; This file is part of Ende.

;; Ende is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, version 3 of the License.
;;
;; Ende is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with Ende.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Parsing and validation for document replacement and insertion results.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'ende-operation)
(require 'ende-protocol)

(defcustom ende-max-write-anywhere-edits 100
  "Maximum number of edits accepted from one `ende-write-anywhere' result."
  :type 'integer
  :group 'ende)

(defcustom ende-max-normalized-edits 10000
  "Maximum number of expanded structured edits accepted from one result.

This is a generous safety ceiling for edits expanded from `all: true'; it is
not intended to limit ordinary product usage."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Maximum normalized edits"))
  :group 'ende)

(define-error 'ende-edit-resource-error
	      "Structured edit resource limit exceeded")

(defun ende-edit--validate-limits ()
  "Validate raw and normalized structured-edit limits."
  (dolist (option (list (cons 'ende-max-write-anywhere-edits
                              ende-max-write-anywhere-edits)
                        (cons 'ende-max-normalized-edits
                              ende-max-normalized-edits)))
    (unless (or (null (cdr option))
                (and (integerp (cdr option)) (> (cdr option) 0)))
      (error "%s must be nil or a positive integer" (car option))))
  t)

(defun ende-edit--normalized-limit-error ()
  "Signal that structured-edit expansion exceeded its safety ceiling."
  (signal 'ende-edit-resource-error
          (list (format "normalized edit count exceeds %s"
                        ende-max-normalized-edits))))

(defun ende-edit--parse-document (content operation)
  "Parse JSON CONTENT for OPERATION and return its document hash table."
  (condition-case caught
      (ende-protocol-json-read-string content)
    (error
     (error "%s result is not valid JSON: %s"
            operation (error-message-string caught)))))

(defun ende-edit--validate-document
    (document operation &optional allow-empty maximum)
  "Validate the common protocol envelope in DOCUMENT for OPERATION.
When ALLOW-EMPTY is non-nil, accept an empty edits array.  MAXIMUM overrides
`ende-max-write-anywhere-edits' when non-nil."
  (ende-edit--validate-limits)
  (unless (and (hash-table-p document)
               (equal (ende-protocol-json-get "version" document) 1))
    (error "%s result has an unsupported protocol version" operation))
  (let ((edits (ende-protocol-json-get "edits" document)))
    (unless (vectorp edits)
      (error "%s result must contain an edits array" operation))
    (when (and (not allow-empty) (zerop (length edits)))
      (error "%s result contains no edits" operation))
    (when (and (or maximum ende-max-write-anywhere-edits)
               (> (length edits) (or maximum ende-max-write-anywhere-edits)))
      (error "%s result contains too many edits" operation))
    edits))

(defun ende-edit--required-fields (object fields)
  "Return missing JSON FIELDS from OBJECT."
  (and (hash-table-p object)
       (cl-remove-if
        (lambda (field)
          (ende-protocol-json-has-key-p field object))
        fields)))

(defun ende-edit--anchor-offsets (source anchor &optional limit)
  "Return zero-based SOURCE offsets at which ANCHOR occurs.
When LIMIT is non-nil, signal as soon as more than LIMIT offsets are found."
  (let ((case-fold-search nil)
        (start 0)
        (offsets nil))
    (while (and (< start (length source))
                (string-match (regexp-quote anchor) source start))
      (push (match-beginning 0) offsets)
      (when (and limit (> (length offsets) limit))
        (ende-edit--normalized-limit-error))
      (setq start (1+ (match-beginning 0))))
    (nreverse offsets)))

(defun ende-edit--parse-anchor-edits
    (index raw source positions &optional normalized-limit allow-empty-text)
  "Parse one anchor-based RAW edit at INDEX against SOURCE.
POSITIONS lists the accepted values for the edit's position field.
NORMALIZED-LIMIT is the remaining expansion budget for this raw edit.
When ALLOW-EMPTY-TEXT is non-nil, accept an empty `text' value."
  (let* ((anchor (and (hash-table-p raw)
                      (ende-protocol-json-get "anchor" raw)))
         (position (and (hash-table-p raw)
                        (ende-protocol-json-get "position" raw)))
         (occurrence-present (and (hash-table-p raw)
                                  (ende-protocol-json-has-key-p
                                   "occurrence" raw)))
         (occurrence (and occurrence-present
                          (ende-protocol-json-get "occurrence" raw)))
         (all-present (and (hash-table-p raw)
                           (ende-protocol-json-has-key-p "all" raw)))
         (all-value (and all-present
                         (ende-protocol-json-get "all" raw)))
         (all (eq all-value t))
         (text (and (hash-table-p raw)
                    (ende-protocol-json-get "text" raw))))
    (unless (and (hash-table-p raw)
                 (ende-protocol-json-has-key-p "anchor" raw)
                 (ende-protocol-json-has-key-p "position" raw)
                 (ende-protocol-json-has-key-p "text" raw))
      (error "missing required anchor, position, or text field"))
    (unless (and (stringp anchor) (not (string-empty-p anchor)))
      (error "anchor must be a non-empty string"))
    (unless (member position positions)
      (error "position must be one of: %s"
             (mapconcat #'identity positions ", ")))
    (unless (and (stringp text)
                 (or allow-empty-text (not (string-empty-p text))))
      (error "text must be a %sstring"
             (if allow-empty-text "" "non-empty ")))
    (when (and all-present (not (or (eq all-value t)
                                    (eq all-value :false)
                                    (null all-value))))
      (error "all must be true when present"))
    (when (and occurrence-present
               (not (and (integerp occurrence) (>= occurrence 1))))
      (error "occurrence must be a positive integer"))
    (let* ((offset-limit (and all normalized-limit))
           (offsets (ende-edit--anchor-offsets source anchor offset-limit))
           (selected-offsets
            (cond
             (all
              (when (and normalized-limit
                         (> (length offsets) normalized-limit))
                (ende-edit--normalized-limit-error))
              offsets)
             (occurrence
              (unless (<= occurrence (length offsets))
                (error "occurrence %d is not present" occurrence))
              (list (nth (1- occurrence) offsets)))
             ((= (length offsets) 1) offsets)
             ((zerop (length offsets)) nil)
             (t (error "anchor matches %d times; specify occurrence or all"
                       (length offsets))))))
      (when (and normalized-limit
                 (<= normalized-limit 0)
                 selected-offsets)
        (ende-edit--normalized-limit-error))
      (unless selected-offsets
        (error "anchor was not found"))
      (mapcar
       (lambda (offset)
         (let ((edit-start (if (equal position "after")
                               (+ offset (length anchor))
                             offset)))
           (list :index index :offset offset
                 :anchor-length (length anchor)
                 :position position :text text :raw raw
                 :start edit-start
                 :end (if (equal position "replace")
                          (+ offset (length anchor))
			edit-start))))
       selected-offsets))))

(defun ende-edit-parse-insertions (content source &optional allow-empty)
  "Parse insertion edits from CONTENT against SOURCE.
Return a plist containing `:edits' and individual `:errors'.
When ALLOW-EMPTY is non-nil, accept an empty edits array."
  (let* ((document (ende-edit--parse-document content "insert-anywhere"))
         (raw-edits (ende-edit--validate-document
                     document "insert-anywhere" allow-empty))
         (edits nil)
         (errors nil))
    (dotimes (index (length raw-edits))
      (condition-case caught
          (let ((parsed
                 (ende-edit--parse-anchor-edits
                  index (aref raw-edits index) source
                  '("before" "after")
                  (and ende-max-normalized-edits
                       (- ende-max-normalized-edits
                          (length edits))))))
            (setq edits (nconc edits parsed)))
        (ende-edit-resource-error
         (signal (car caught) (cdr caught)))
        ((error quit)
         (push (list :index index
                     :message (error-message-string caught)
                     :raw (aref raw-edits index))
               errors))))
    (setq errors (nreverse errors))
    (list :edits edits :errors errors)))

(defun ende-edit--record-conflict (conflicts left right)
  (let ((left-index (plist-get left :index))
        (right-index (plist-get right :index)))
    (unless (gethash left-index conflicts)
      (puthash left-index (format "overlaps edit %d" (1+ right-index)) conflicts))
    (unless (gethash right-index conflicts)
      (puthash right-index (format "overlaps edit %d" (1+ left-index)) conflicts))))

(defun ende-edit-parse-replacements
    (content source &optional allow-empty-text maximum operation)
  "Parse anchor-based replacement edits from CONTENT against SOURCE.
Return valid `:edits' and individual `:errors'.  ALLOW-EMPTY-TEXT permits
empty replacement text.  MAXIMUM overrides the normal raw-edit limit.
OPERATION names the source in diagnostics, defaulting to `write-anywhere'.
CONTENT may also be a vector of already parsed edit objects."
  (let* ((operation (or operation "write-anywhere"))
         (raw-edits
          (if (stringp content)
              (ende-edit--validate-document
               (ende-edit--parse-document content operation)
               operation nil maximum)
            (progn
              (ende-edit--validate-limits)
              (unless (and (vectorp content) (not (stringp content)))
                (error "%s edits must be an array" operation))
              (when (zerop (length content))
                (error "%s result contains no edits" operation))
              (let ((limit (or maximum ende-max-write-anywhere-edits)))
                (when (and limit (> (length content) limit))
                  (error "%s result contains too many edits" operation)))
              content)))
         (edits nil)
         (errors nil))
    (dotimes (index (length raw-edits))
      (condition-case caught
          (let ((parsed
                 (ende-edit--parse-anchor-edits
                  index (aref raw-edits index) source
                  '("before" "after" "replace")
                  (and ende-max-normalized-edits
                       (- ende-max-normalized-edits
                          (length edits)))
                  allow-empty-text)))
            (setq edits (nconc edits parsed)))
        (ende-edit-resource-error
         (signal (car caught) (cdr caught)))
        ((error quit)
         (push (list :index index
                     :message (error-message-string caught)
                     :raw (aref raw-edits index))
               errors))))
    (setq errors (nreverse errors))
    (let ((conflict-messages (make-hash-table :test #'eql))
          (max-end nil)
          (max-edit nil)
          (last-zero-start nil)
          (last-zero-edit nil)
          (sorted
           (sort (copy-sequence edits)
                 (lambda (left right)
                   (let ((left-start (plist-get left :start))
                         (right-start (plist-get right :start))
                         (left-replace
                          (equal (plist-get left :position) "replace"))
                         (right-replace
                          (equal (plist-get right :position) "replace")))
                     (or (< left-start right-start)
                         (and (= left-start right-start)
                              (or (and (not left-replace) right-replace)
                                  (and (eq left-replace right-replace)
                                       (< (plist-get left :index)
                                          (plist-get right :index)))))))))))
      (dolist (edit sorted)
        (let* ((start (plist-get edit :start))
               (end (plist-get edit :end))
               (replace (equal (plist-get edit :position) "replace")))
          (when (and (not replace)
                     last-zero-edit
                     (= start last-zero-start))
            (ende-edit--record-conflict conflict-messages edit last-zero-edit))
          (when (and replace
                     last-zero-edit
                     (= start last-zero-start)
                     (equal (plist-get last-zero-edit :position) "before"))
            (ende-edit--record-conflict conflict-messages edit last-zero-edit))
          (when (and (not replace) max-edit
                     (< (plist-get max-edit :start) start)
                     (> max-end start))
            (ende-edit--record-conflict conflict-messages edit max-edit))
          (when replace
            (when (and max-edit (> max-end start))
              (ende-edit--record-conflict conflict-messages edit max-edit))
            (when (or (null max-end) (> end max-end))
              (setq max-end end
                    max-edit edit)))
          (if replace
              (setq last-zero-start nil
                    last-zero-edit nil)
            (setq last-zero-start start
                  last-zero-edit edit))))
      (setq edits
            (cl-remove-if
             (lambda (edit) (gethash (plist-get edit :index)
                                     conflict-messages))
             edits))
      (dotimes (index (length raw-edits))
        (when-let* ((message (gethash index conflict-messages)))
          (push (list :index index :message message
                      :raw (aref raw-edits index))
                errors))))
    (list :edits edits :errors errors)))

(defun ende-edit-parse-tool-replacements (raw-edits source maximum)
  "Parse tool RAW-EDITS against SOURCE with raw-edit limit MAXIMUM.
RAW-EDITS must be a vector of JSON-compatible edit objects.  Matching is
case-sensitive and empty replacement text is accepted.  Return the same
partial-result shape as `ende-edit-parse-replacements'; callers should reject
the complete request if `:errors' is non-nil."
  (unless (and (vectorp raw-edits) (not (stringp raw-edits)))
    (error "edit_file edits must be an array"))
  (let ((case-fold-search nil))
    (ende-edit-parse-replacements
     raw-edits source t maximum "edit_file")))

(defun ende-edit-format-failures (failures &optional separator)
  "Format edit FAILURES using SEPARATOR, defaulting to a newline."
  (mapconcat
   (lambda (failure)
     (format "Edit %d: %s%s"
             (1+ (plist-get failure :index))
             (plist-get failure :message)
             (if-let* ((raw (plist-get failure :raw)))
                 (format "\n%s" (json-encode raw))
               "")))
   failures (or separator "\n")))

(defun ende-edit-format-insertion-failures (failures &optional separator)
  "Format insertion FAILURES using SEPARATOR, defaulting to a newline."
  (ende-edit-format-failures failures separator))

(defun ende-edit-add-insertion-failures (result failures)
  "Return RESULT annotated with insertion FAILURES."
  (if (null failures)
      result
    (if (listp result)
        (let ((annotated (copy-sequence result)))
          (plist-put annotated :insert-anywhere-errors failures))
      (list :content result :insert-anywhere-errors failures))))

(defun ende-edit-add-replacement-failures (result failures)
  "Return RESULT annotated with replacement FAILURES."
  (if (null failures)
      result
    (if (listp result)
        (let ((annotated (copy-sequence result)))
          (plist-put annotated :write-anywhere-errors failures))
      (list :content result :write-anywhere-errors failures))))

(provide 'ende-edit)

;;; ende-edit.el ends here
