;;; deslop-edit.el --- Structured edit protocols for deslop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools, llm

;;; Commentary:

;; Parsing and validation for document replacement and insertion results.

;;; Code:

(require 'cl-lib)
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

(defun deslop-edit-parse-replacements (content source)
  "Parse replacement edits from CONTENT and validate them against SOURCE."
  (let* ((document (deslop-edit--parse-document content "write-anywhere"))
         (raw-edits (deslop-edit--validate-document document "write-anywhere"))
         (edits nil)
         (source-length (length source)))
    (dotimes (index (length raw-edits))
      (let* ((raw (aref raw-edits index))
             (missing (deslop-edit--required-fields
                       raw '("start" "end" "expected" "text")))
             (start (and (hash-table-p raw)
                         (deslop-protocol--json-get "start" raw)))
             (end (and (hash-table-p raw)
                       (deslop-protocol--json-get "end" raw)))
             (expected (and (hash-table-p raw)
                            (deslop-protocol--json-get "expected" raw)))
             (text (and (hash-table-p raw)
                        (deslop-protocol--json-get "text" raw))))
        (when missing
          (error "write-anywhere edit %d is missing required field%s: %s"
                 (1+ index)
                 (if (= (length missing) 1) "" "s")
                 (mapconcat #'identity missing ", ")))
        (unless (and (integerp start) (integerp end)
                     (<= 0 start end source-length)
                     (stringp expected) (stringp text)
                     (not (string-empty-p text)))
          (error "write-anywhere edit %d has invalid fields" (1+ index)))
        (unless (string= expected (substring source start end))
          (error "write-anywhere edit %d does not match its expected text"
                 (1+ index)))
        (push (list :index index :start start :end end
                    :expected expected :text text)
              edits)))
    (setq edits (nreverse edits))
    (cl-loop for tail on edits do
      (dolist (right (cdr tail))
        (when (deslop-edit-replacement-overlaps-p (car tail) right)
          (error "write-anywhere edits %d and %d overlap"
                 (1+ (plist-get (car tail) :index))
                 (1+ (plist-get right :index))))))
    edits))

(defun deslop-edit--anchor-offsets (source anchor)
  "Return all zero-based SOURCE offsets at which ANCHOR occurs."
  (let ((start 0)
        (offsets nil))
    (while (and (< start (length source))
                (string-match (regexp-quote anchor) source start))
      (push (match-beginning 0) offsets)
      (setq start (1+ (match-beginning 0))))
    (nreverse offsets)))

(defun deslop-edit--parse-insertion (index raw source)
  "Parse one insertion RAW edit at INDEX against SOURCE."
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
    (unless (member position '("before" "after"))
      (error "position must be before or after"))
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
         (list :index index :offset offset
               :anchor-length (length anchor)
               :position position :text text))
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
                        (deslop-edit--parse-insertion
                         index (aref raw-edits index) source)))
        ((error quit)
         (push (list :index index :message (error-message-string caught))
               errors))))
    (setq errors (nreverse errors))
    (unless edits
      (error "insert-anywhere produced no valid insertions: %s"
             (deslop-edit-format-insertion-failures errors "; ")))
    (list :edits edits :errors errors)))

(defun deslop-edit-format-insertion-failures (failures &optional separator)
  "Format insertion FAILURES using SEPARATOR, defaulting to a newline."
  (mapconcat
   (lambda (failure)
     (format "Edit %d: %s"
             (1+ (plist-get failure :index))
             (plist-get failure :message)))
   failures (or separator "\n")))

(defun deslop-edit-add-insertion-failures (result failures)
  "Return RESULT annotated with insertion FAILURES."
  (if (null failures)
      result
    (if (listp result)
        (let ((annotated (copy-sequence result)))
          (plist-put annotated :insert-anywhere-errors failures))
      (list :content result :insert-anywhere-errors failures))))

(provide 'deslop-edit)

;;; deslop-edit.el ends here
