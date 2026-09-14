;;; deslop-edit.el --- Structured edit protocols for deslop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools, llm

;;; Commentary:

;; Parsing and validation for document replacement and insertion results.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'deslop-operation)
(require 'deslop-protocol)

(defcustom deslop-max-write-anywhere-edits 100
  "Maximum number of edits accepted from one `deslop-write-anywhere' result."
  :type 'integer
  :group 'deslop)

(defun deslop-edit--parse-document (content operation)
  "Parse JSON CONTENT for OPERATION and return its document hash table."
  (condition-case caught
      (deslop-protocol--json-read-string content)
    (error
     (error "%s result is not valid JSON: %s"
            operation (error-message-string caught)))))

(defun deslop-edit--validate-document (document operation)
  "Validate the common protocol envelope in DOCUMENT for OPERATION."
  (unless (and (hash-table-p document)
               (equal (deslop-protocol--json-get "version" document) 1))
    (error "%s result has an unsupported protocol version" operation))
  (let ((edits (deslop-protocol--json-get "edits" document)))
    (unless (vectorp edits)
      (error "%s result must contain an edits array" operation))
    (when (zerop (length edits))
      (error "%s result contains no edits" operation))
    (when (and deslop-max-write-anywhere-edits
               (> (length edits) deslop-max-write-anywhere-edits))
      (error "%s result contains too many edits" operation))
    edits))

(defun deslop-edit--required-fields (object fields)
  "Return missing JSON FIELDS from OBJECT."
  (and (hash-table-p object)
       (cl-remove-if
        (lambda (field)
          (deslop-protocol--json-has-key-p field object))
        fields)))

(defun deslop-edit-replacement-overlaps-p (left right)
  "Return non-nil when replacement edits LEFT and RIGHT overlap."
  (let ((left-start (plist-get left :start))
        (left-end (plist-get left :end))
        (right-start (plist-get right :start))
        (right-end (plist-get right :end)))
    (cond
     ((and (= left-start left-end) (= right-start right-end))
      (= left-start right-start))
     ((= left-start left-end)
      (and (< right-start left-start) (< left-start right-end)))
     ((= right-start right-end)
      (and (< left-start right-start) (< right-start left-end)))
     (t (and (< left-start right-end) (< right-start left-end))))))

(defun deslop-edit--anchor-offsets (source anchor)
  "Return all zero-based SOURCE offsets at which ANCHOR occurs."
  (let ((start 0)
        (offsets nil))
    (while (and (< start (length source))
                (string-match (regexp-quote anchor) source start))
      (push (match-beginning 0) offsets)
      (setq start (1+ (match-beginning 0))))
    (nreverse offsets)))

(defun deslop-edit--parse-anchor-edits (index raw source positions)
  "Parse one anchor-based RAW edit at INDEX against SOURCE.
POSITIONS lists the accepted values for the edit's position field."
  (let* ((anchor (and (hash-table-p raw)
                      (deslop-protocol--json-get "anchor" raw)))
         (position (and (hash-table-p raw)
                        (deslop-protocol--json-get "position" raw)))
         (occurrence (and (hash-table-p raw)
                          (deslop-protocol--json-get "occurrence" raw)))
         (all (and (hash-table-p raw)
                   (deslop-protocol--json-get "all" raw)))
         (text (and (hash-table-p raw)
                    (deslop-protocol--json-get "text" raw)))
         (offsets (and (stringp anchor)
                       (deslop-edit--anchor-offsets source anchor))))
    (unless (and (hash-table-p raw)
                 (deslop-protocol--json-has-key-p "anchor" raw)
                 (deslop-protocol--json-has-key-p "position" raw)
                 (deslop-protocol--json-has-key-p "text" raw))
      (error "missing required anchor, position, or text field"))
    (unless (and (stringp anchor) (not (string-empty-p anchor)))
      (error "anchor must be a non-empty string"))
    (unless (member position positions)
      (error "position must be one of: %s"
             (mapconcat #'identity positions ", ")))
    (unless (and (stringp text) (not (string-empty-p text)))
      (error "text must be a non-empty string"))
    (let ((selected-offsets
           (cond
            (all
             (unless (eq all t)
               (error "all must be true when present"))
             offsets)
            (occurrence
             (unless (and (integerp occurrence) (>= occurrence 1))
               (error "occurrence must be a positive integer"))
             (unless (<= occurrence (length offsets))
               (error "occurrence %d is not present" occurrence))
             (list (nth (1- occurrence) offsets)))
            ((= (length offsets) 1) offsets)
            ((zerop (length offsets)) nil)
            (t (error "anchor matches %d times; specify occurrence or all"
                      (length offsets))))))
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

(defun deslop-edit-parse-insertions (content source)
  "Parse insertion edits from CONTENT against SOURCE.
Return a plist containing `:edits' and individual `:errors'."
  (let* ((document (deslop-edit--parse-document content "insert-anywhere"))
         (raw-edits (deslop-edit--validate-document document "insert-anywhere"))
         (edits nil)
         (errors nil))
    (dotimes (index (length raw-edits))
      (condition-case caught
          (setq edits
                (append edits
                        (deslop-edit--parse-anchor-edits
                         index (aref raw-edits index) source
                         '("before" "after"))))
        ((error quit)
         (push (list :index index
                     :message (error-message-string caught)
                     :raw (aref raw-edits index))
               errors))))
    (setq errors (nreverse errors))
    (list :edits edits :errors errors)))

(defun deslop-edit-parse-replacements (content source)
  "Parse anchor-based replacement edits from CONTENT against SOURCE.
Return a plist containing valid `:edits' and individual `:errors'."
  (let* ((document (deslop-edit--parse-document content "write-anywhere"))
         (raw-edits (deslop-edit--validate-document document "write-anywhere"))
         (edits nil)
         (errors nil))
    (dotimes (index (length raw-edits))
      (condition-case caught
          (setq edits
                (append edits
                        (deslop-edit--parse-anchor-edits
                         index (aref raw-edits index) source
                         '("before" "after" "replace"))))
        ((error quit)
         (push (list :index index
                     :message (error-message-string caught)
                     :raw (aref raw-edits index))
               errors))))
    (setq errors (nreverse errors))
    (let ((conflicting nil)
          (conflict-messages (make-hash-table :test #'eql)))
      (cl-loop for tail on edits do
        (let ((left (car tail)))
          (dolist (right (cdr tail))
            (when (and (or (equal (plist-get left :position) "replace")
                           (equal (plist-get right :position) "replace"))
                       (deslop-edit-replacement-overlaps-p left right))
              (push left conflicting)
              (push right conflicting)
              (unless (gethash (plist-get left :index) conflict-messages)
                (puthash (plist-get left :index)
                         (format "overlaps edit %d"
                                 (1+ (plist-get right :index)))
                         conflict-messages))
              (unless (gethash (plist-get right :index) conflict-messages)
                (puthash (plist-get right :index)
                         (format "overlaps edit %d"
                                 (1+ (plist-get left :index)))
                         conflict-messages))))))
      (setq edits
            (cl-remove-if (lambda (edit) (memq edit conflicting)) edits))
      (dotimes (index (length raw-edits))
        (when-let* ((message (gethash index conflict-messages)))
          (push (list :index index :message message
                      :raw (aref raw-edits index))
                errors))))
    (list :edits edits :errors errors)))

(defun deslop-edit-format-failures (failures &optional separator)
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

(defun deslop-edit-format-insertion-failures (failures &optional separator)
  "Format insertion FAILURES using SEPARATOR, defaulting to a newline."
  (deslop-edit-format-failures failures separator))

(defun deslop-edit-add-insertion-failures (result failures)
  "Return RESULT annotated with insertion FAILURES."
  (if (null failures)
      result
    (if (listp result)
        (let ((annotated (copy-sequence result)))
          (plist-put annotated :insert-anywhere-errors failures))
      (list :content result :insert-anywhere-errors failures))))

(defun deslop-edit-add-replacement-failures (result failures)
  "Return RESULT annotated with replacement FAILURES."
  (if (null failures)
      result
    (if (listp result)
        (let ((annotated (copy-sequence result)))
          (plist-put annotated :write-anywhere-errors failures))
      (list :content result :write-anywhere-errors failures))))

(provide 'deslop-edit)

;;; deslop-edit.el ends here
