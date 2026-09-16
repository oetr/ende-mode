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

(defcustom deslop-max-normalized-edits 10000
  "Maximum number of expanded structured edits accepted from one result.

This is a generous safety ceiling for edits expanded from `all: true'; it is
not intended to limit ordinary product usage."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Maximum normalized edits"))
  :group 'deslop)

(define-error 'deslop-edit-resource-error
  "Structured edit resource limit exceeded")

(defun deslop-edit--validate-limits ()
  "Validate raw and normalized structured-edit limits."
  (dolist (option (list (cons 'deslop-max-write-anywhere-edits
                              deslop-max-write-anywhere-edits)
                        (cons 'deslop-max-normalized-edits
                              deslop-max-normalized-edits)))
    (unless (or (null (cdr option))
                (and (integerp (cdr option)) (> (cdr option) 0)))
      (error "%s must be nil or a positive integer" (car option))))
  t)

(defun deslop-edit--normalized-limit-error ()
  "Signal that structured-edit expansion exceeded its safety ceiling."
  (signal 'deslop-edit-resource-error
          (list (format "normalized edit count exceeds %s"
                        deslop-max-normalized-edits))))

(defun deslop-edit--parse-document (content operation)
  "Parse JSON CONTENT for OPERATION and return its document hash table."
  (condition-case caught
       (deslop-protocol-json-read-string content)
    (error
     (error "%s result is not valid JSON: %s"
            operation (error-message-string caught)))))

(defun deslop-edit--validate-document (document operation &optional allow-empty)
  "Validate the common protocol envelope in DOCUMENT for OPERATION.
When ALLOW-EMPTY is non-nil, accept an empty edits array."
  (deslop-edit--validate-limits)
  (unless (and (hash-table-p document)
                (equal (deslop-protocol-json-get "version" document) 1))
    (error "%s result has an unsupported protocol version" operation))
  (let ((edits (deslop-protocol-json-get "edits" document)))
    (unless (vectorp edits)
      (error "%s result must contain an edits array" operation))
    (when (and (not allow-empty) (zerop (length edits)))
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
          (deslop-protocol-json-has-key-p field object))
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
       (and (<= right-start left-start) (< left-start right-end)))
      ((= right-start right-end)
       (and (<= left-start right-start) (< right-start left-end)))
     (t (and (< left-start right-end) (< right-start left-end))))))

(defun deslop-edit--anchor-offsets (source anchor &optional limit)
  "Return zero-based SOURCE offsets at which ANCHOR occurs.
When LIMIT is non-nil, signal as soon as more than LIMIT offsets are found."
  (let ((start 0)
        (offsets nil))
    (while (and (< start (length source))
                (string-match (regexp-quote anchor) source start))
      (push (match-beginning 0) offsets)
      (when (and limit (> (length offsets) limit))
        (deslop-edit--normalized-limit-error))
      (setq start (1+ (match-beginning 0))))
    (nreverse offsets)))

(defun deslop-edit--parse-anchor-edits
    (index raw source positions &optional normalized-limit)
  "Parse one anchor-based RAW edit at INDEX against SOURCE.
POSITIONS lists the accepted values for the edit's position field.
NORMALIZED-LIMIT is the remaining expansion budget for this raw edit."
  (let* ((anchor (and (hash-table-p raw)
                       (deslop-protocol-json-get "anchor" raw)))
         (position (and (hash-table-p raw)
                         (deslop-protocol-json-get "position" raw)))
           (occurrence-present (and (hash-table-p raw)
                                   (deslop-protocol-json-has-key-p
                                    "occurrence" raw)))
           (occurrence (and occurrence-present
                            (deslop-protocol-json-get "occurrence" raw)))
           (all-present (and (hash-table-p raw)
                             (deslop-protocol-json-has-key-p "all" raw)))
           (all-value (and all-present
                           (deslop-protocol-json-get "all" raw)))
          (all (eq all-value t))
          (text (and (hash-table-p raw)
                      (deslop-protocol-json-get "text" raw))))
     (unless (and (hash-table-p raw)
                  (deslop-protocol-json-has-key-p "anchor" raw)
                  (deslop-protocol-json-has-key-p "position" raw)
                  (deslop-protocol-json-has-key-p "text" raw))
      (error "missing required anchor, position, or text field"))
    (unless (and (stringp anchor) (not (string-empty-p anchor)))
      (error "anchor must be a non-empty string"))
    (unless (member position positions)
      (error "position must be one of: %s"
             (mapconcat #'identity positions ", ")))
    (unless (and (stringp text) (not (string-empty-p text)))
      (error "text must be a non-empty string"))
    (when (and all-present (not (or (eq all-value t)
                                    (eq all-value :false)
                                    (null all-value))))
      (error "all must be true when present"))
    (when (and occurrence-present
               (not (and (integerp occurrence) (>= occurrence 1))))
      (error "occurrence must be a positive integer"))
    (let* ((offset-limit (and all normalized-limit))
           (offsets (deslop-edit--anchor-offsets source anchor offset-limit))
           (selected-offsets
            (cond
             (all
              (when (and normalized-limit
                         (> (length offsets) normalized-limit))
                (deslop-edit--normalized-limit-error))
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
        (deslop-edit--normalized-limit-error))
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

(defun deslop-edit-parse-insertions (content source &optional allow-empty)
  "Parse insertion edits from CONTENT against SOURCE.
Return a plist containing `:edits' and individual `:errors'.
When ALLOW-EMPTY is non-nil, accept an empty edits array."
  (let* ((document (deslop-edit--parse-document content "insert-anywhere"))
         (raw-edits (deslop-edit--validate-document
                     document "insert-anywhere" allow-empty))
         (edits nil)
         (errors nil))
    (dotimes (index (length raw-edits))
      (condition-case caught
          (let ((parsed
                 (deslop-edit--parse-anchor-edits
                  index (aref raw-edits index) source
                  '("before" "after")
                  (and deslop-max-normalized-edits
                       (- deslop-max-normalized-edits
                          (length edits))))))
            (setq edits (nconc edits parsed)))
        (deslop-edit-resource-error
         (signal (car caught) (cdr caught)))
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
          (let ((parsed
                 (deslop-edit--parse-anchor-edits
                  index (aref raw-edits index) source
                  '("before" "after" "replace")
                  (and deslop-max-normalized-edits
                       (- deslop-max-normalized-edits
                          (length edits))))))
            (setq edits (nconc edits parsed)))
        (deslop-edit-resource-error
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
            (unless (gethash (plist-get edit :index) conflict-messages)
              (puthash (plist-get edit :index)
                       (format "overlaps edit %d"
                               (1+ (plist-get last-zero-edit :index)))
                       conflict-messages))
            (unless (gethash (plist-get last-zero-edit :index)
                             conflict-messages)
              (puthash (plist-get last-zero-edit :index)
                       (format "overlaps edit %d"
                               (1+ (plist-get edit :index)))
                       conflict-messages)))
           (when (and replace
                      last-zero-edit
                      (= start last-zero-start)
                      (equal (plist-get last-zero-edit :position) "before"))
             (unless (gethash (plist-get edit :index) conflict-messages)
               (puthash (plist-get edit :index)
                        (format "overlaps edit %d"
                                (1+ (plist-get last-zero-edit :index)))
                        conflict-messages))
             (unless (gethash (plist-get last-zero-edit :index)
                              conflict-messages)
               (puthash (plist-get last-zero-edit :index)
                        (format "overlaps edit %d"
                                (1+ (plist-get edit :index)))
                        conflict-messages)))
           (when (and (not replace) max-edit
                     (< (plist-get max-edit :start) start)
                     (> max-end start))
            (unless (gethash (plist-get edit :index) conflict-messages)
              (puthash (plist-get edit :index)
                       (format "overlaps edit %d"
                               (1+ (plist-get max-edit :index)))
                       conflict-messages))
            (unless (gethash (plist-get max-edit :index) conflict-messages)
              (puthash (plist-get max-edit :index)
                       (format "overlaps edit %d"
                               (1+ (plist-get edit :index)))
                       conflict-messages)))
          (when replace
            (when (and max-edit (> max-end start))
              (unless (gethash (plist-get edit :index) conflict-messages)
                (puthash (plist-get edit :index)
                         (format "overlaps edit %d"
                                 (1+ (plist-get max-edit :index)))
                         conflict-messages))
              (unless (gethash (plist-get max-edit :index)
                               conflict-messages)
                (puthash (plist-get max-edit :index)
                         (format "overlaps edit %d"
                                 (1+ (plist-get edit :index)))
                         conflict-messages)))
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
