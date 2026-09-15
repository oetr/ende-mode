;;; deslop-tools.el --- Project tools for deslop agents -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools, llm

;;; Commentary:

;; A provider-independent registry and implementation of project-scoped read
;; tools plus validated source edits.  The agent/transport layer can use the
;; registry later without having to know how Emacs buffers are resolved.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'deslop-core)
(require 'deslop-edit)
(require 'deslop-project)
(require 'deslop-review)

(defcustom deslop-tools-max-listed-files 200
  "Maximum files returned by one `list_project_files' call."
  :type 'integer
  :group 'deslop)

(defcustom deslop-tools-max-read-lines 500
  "Maximum lines returned by one file or buffer read call."
  :type 'integer
  :group 'deslop)

(defcustom deslop-tools-max-search-files 1000
  "Maximum files inspected by one `search_project' call."
  :type 'integer
  :group 'deslop)

(defcustom deslop-tools-max-search-results 100
  "Maximum matches returned by one `search_project' call."
  :type 'integer
  :group 'deslop)

(defcustom deslop-tools-max-search-context-lines 3
  "Maximum context lines returned around one search match."
  :type 'integer
  :group 'deslop)

(defcustom deslop-tools-max-edits 100
  "Maximum edits accepted by one `edit_source' call."
  :type 'integer
  :group 'deslop)

(defcustom deslop-tools-direct-write nil
  "Non-nil to apply `edit_source' proposals immediately.

When nil, model edits become pending change sets and must be applied with
`deslop-tools-apply-change-set'."
  :type 'boolean
  :group 'deslop)

;;;; Tool registry

(cl-defstruct (deslop-tool (:constructor deslop-tool--create))
  "One model-callable deslop tool."
  name description parameters handler read-only)

(defvar deslop-tools--registry nil
  "Registered model tools, keyed by their symbolic names.")

(defun deslop-tool-register
    (name description parameters handler &optional read-only)
  "Register tool NAME with DESCRIPTION, PARAMETERS, and HANDLER.

HANDLER receives an arguments object and a canonical project root.  NAME is a
symbol internally and is exposed to model transports as a string.  Pass
`:write' as READ-ONLY for a mutating tool."
  (unless (and (symbolp name) (functionp handler))
    (error "invalid deslop tool registration: %s" name))
    (let ((tool (deslop-tool--create
               :name name :description description :parameters parameters
               :handler handler :read-only (not (eq read-only :write)))))
     (setf (alist-get name deslop-tools--registry) tool)
    tool))

(defun deslop-tool--find (name)
  "Return the registered tool called NAME, or nil."
  (let ((name (if (symbolp name) (symbol-name name) (format "%s" name))))
    (cdr (cl-find-if
          (lambda (entry)
            (string= name (symbol-name (car entry))))
     deslop-tools--registry))))

(defun deslop-tool-definitions ()
  "Return registered tools in OpenAI-compatible function-tool shape."
  (mapcar
   (lambda (tool)
     (list (cons :type "function")
           (cons :function
                 (list (cons :name (symbol-name (deslop-tool-name tool)))
                       (cons :description (deslop-tool-description tool))
                       (cons :parameters (deslop-tool-parameters tool))))))
   (mapcar #'cdr deslop-tools--registry)))

(defun deslop-tool-call (name arguments &optional root)
  "Invoke model tool NAME with ARGUMENTS within project ROOT.

Return a JSON-compatible result object.  Tool failures are returned as data so
an agent can recover from an invalid path, regexp, or stale source version."
  (let ((tool (deslop-tool--find name)))
    (if (null tool)
        (list :ok nil :tool (format "%s" name)
              :error (format "unknown deslop tool: %s" name))
      (condition-case caught
          (let ((root (deslop-project-normalize-root
                       (or root (deslop-project-root)))))
            (list :ok t
                  :tool (symbol-name (deslop-tool-name tool))
                  :result (funcall (deslop-tool-handler tool) arguments root)))
        ((error quit)
         (list :ok nil
               :tool (symbol-name (deslop-tool-name tool))
               :error (error-message-string caught)))))))

;;;; Arguments and source resolution

(defun deslop-tools--argument (arguments name)
  "Return NAME from JSON hash table, plist, or alist ARGUMENTS."
  (cond
   ((hash-table-p arguments)
    (gethash name arguments))
   ((listp arguments)
    (or (plist-get arguments (intern (concat ":" name)))
        (alist-get name arguments nil nil #'equal)
        (alist-get (intern name) arguments)))
   (t nil)))

(defun deslop-tools--required-string (arguments name)
  "Return required string argument NAME from ARGUMENTS."
  (let ((value (deslop-tools--argument arguments name)))
    (unless (and (stringp value) (not (string-empty-p value)))
      (error "tool argument %s must be a non-empty string" name))
    value))

(defun deslop-tools--bounded-integer
    (arguments name default maximum &optional minimum)
  "Return integer NAME from ARGUMENTS bounded by MINIMUM and MAXIMUM."
  (let ((value (or (deslop-tools--argument arguments name) default))
        (minimum (or minimum 1)))
    (unless (and (integerp value) (>= value minimum) (<= value maximum))
      (error "tool argument %s must be an integer from %d to %d"
             name minimum maximum))
    value))

(defun deslop-tools--resolve-source (arguments root)
  "Resolve one path or buffer_id in ARGUMENTS below ROOT.

Return a plist containing `:kind', `:path', `:relative', and optionally
`:buffer'.  A path that has a visiting buffer resolves to that buffer so
unsaved changes take precedence over disk contents."
  (let* ((path (deslop-tools--argument arguments "path"))
         (buffer-id (deslop-tools--argument arguments "buffer_id"))
         (has-path (and (stringp path) (not (string-empty-p path))))
         (has-buffer (and (stringp buffer-id)
                          (not (string-empty-p buffer-id)))))
    (when (eq has-path has-buffer)
      (error "provide exactly one of path or buffer_id"))
    (if has-buffer
        (let* ((buffer (deslop-project-buffer-for-id buffer-id root))
               (buffer-path (deslop-project--buffer-path buffer))
               (relative (file-relative-name buffer-path root)))
          (list :kind 'buffer :buffer buffer :path buffer-path
                :relative relative :buffer-id buffer-id))
      (let* ((canonical (deslop-project-path path root))
             (buffer (deslop-project-visiting-buffer path root))
             (relative (file-relative-name canonical root)))
        (list :kind (if buffer 'buffer 'file)
              :buffer buffer :path canonical :relative relative
              :buffer-id (and buffer (deslop-project-buffer-id buffer)))))))

(defun deslop-tools--with-source (source function)
  "Call FUNCTION with current source buffer selected for SOURCE.

For a disk source, a temporary buffer is used and the file is not visited.
FUNCTION receives no arguments and may return any value."
  (let ((buffer (plist-get source :buffer))
        (path (plist-get source :path)))
    (if buffer
        (with-current-buffer buffer
          (save-restriction
            (widen)
            (funcall function)))
      (deslop-project--assert-readable-file path)
      (with-temp-buffer
        (insert-file-contents path)
        (funcall function)))))

(defun deslop-tools--source-digest ()
  "Return a SHA-256 digest of the complete current source buffer."
  (secure-hash 'sha256 (current-buffer) (point-min) (point-max)))

(defun deslop-tools--source-line-count ()
  "Return the number of lines in the complete current source buffer."
  (count-lines (point-min) (point-max)))

(defun deslop-tools--source-metadata (source)
  "Return metadata for SOURCE using its current contents."
  (let ((path (plist-get source :path))
        (buffer (plist-get source :buffer)))
    (deslop-tools--with-source
     source
     (lambda ()
       (list :path (plist-get source :relative)
             :source_kind (if buffer "buffer" "file")
             :buffer_id (and buffer (deslop-project-buffer-id buffer))
             :line_count (deslop-tools--source-line-count)
             :byte_count (string-bytes (buffer-string))
             :digest (deslop-tools--source-digest)
             :buffer_modified (and buffer (buffer-modified-p))
             :disk_mtime
             (and (file-exists-p path)
                  (format-time-string
                   "%Y-%m-%dT%H:%M:%S%z"
                   (file-attribute-modification-time
                     (file-attributes path)))))))))

(defun deslop-tools--line-slice (start-line line-count)
  "Return a bounded line slice from the current source buffer."
  (let ((total (deslop-tools--source-line-count)))
    (when (and (> total 0) (> start-line total))
      (error "start_line %d is beyond the end of the source" start-line))
    (goto-char (point-min))
    (forward-line (1- start-line))
    (let* ((beg (point))
           (actual-start (line-number-at-pos beg)))
      (forward-line line-count)
      (let* ((end (point))
             (actual-end
              (if (= beg end)
                  (1- actual-start)
                (line-number-at-pos (max beg (1- end))))))
        (list :start_line actual-start
              :end_line actual-end
              :total_lines total
              :content (buffer-substring-no-properties beg end)
              :truncated (< actual-end total))))))

;;;; Read tools

(defun deslop-tools--project-info (_arguments root)
  "Return basic information about project ROOT."
  (list :root root
        :name (file-name-nondirectory (directory-file-name root))
        :file_count (length (deslop-project-files root))
        :buffer_count (length (deslop-project-buffers root))))

(defun deslop-tools--file-record (path root include-metadata)
  "Return a compact project file record for PATH below ROOT."
  (let* ((relative (file-relative-name path root))
         (attributes (file-attributes path))
         (buffer (deslop-project-visiting-buffer relative root))
         (record (list :path relative
                       :byte_count (file-attribute-size attributes)
                       :buffer_id (and buffer (deslop-project-buffer-id buffer))
                       :buffer_modified (and buffer (buffer-modified-p)))))
    (if include-metadata
        (deslop-tools--source-metadata
         (list :kind (if buffer 'buffer 'file) :buffer buffer :path path
               :relative relative :buffer-id
               (and buffer (deslop-project-buffer-id buffer))))
      record)))

(defun deslop-tools--list-project-files (arguments root)
  "List allowed files below ROOT, optionally filtered by a glob."
  (let* ((glob (deslop-tools--argument arguments "glob"))
         (include-metadata (eq (deslop-tools--argument
                                arguments "include_metadata") t))
         (maximum (deslop-tools--bounded-integer
                   arguments "max_results" deslop-tools-max-listed-files
                   deslop-tools-max-listed-files))
         (glob-regexp (and glob (wildcard-to-regexp glob)))
         (files
          (cl-remove-if-not
           (lambda (path)
             (or (null glob-regexp)
                 (string-match-p
                  glob-regexp (file-relative-name path root))))
           (deslop-project-files root)))
         (truncated (> (length files) maximum)))
    (list :files
          (mapcar (lambda (path)
                    (deslop-tools--file-record path root include-metadata))
                  (cl-subseq files 0 (min maximum (length files))))
          :truncated truncated)))

(defun deslop-tools--file-info (arguments root)
  "Return metadata for one project file."
  (let* ((path-value (deslop-tools--required-string arguments "path"))
         (path (deslop-project-path path-value root))
         (attributes (progn
                       (deslop-project--assert-readable-file path)
                       (file-attributes path)))
         (source (deslop-tools--resolve-source
                  (list :path path-value) root))
         (metadata (deslop-tools--source-metadata source)))
    (plist-put metadata :disk_byte_count (file-attribute-size attributes))))

(defun deslop-tools--read-file (arguments root)
  "Read a bounded range from one project file."
  (let* ((source (deslop-tools--resolve-source arguments root))
         (start-line (deslop-tools--bounded-integer
                      arguments "start_line" 1 most-positive-fixnum))
         (line-count (deslop-tools--bounded-integer
                      arguments "line_count" deslop-tools-max-read-lines
                      deslop-tools-max-read-lines)))
    (deslop-tools--with-source
     source
     (lambda ()
       (append (list :path (plist-get source :relative)
                     :source_kind (if (plist-get source :buffer)
                                      "buffer" "file")
                     :buffer_id (plist-get source :buffer-id)
                     :digest (deslop-tools--source-digest))
               (deslop-tools--line-slice start-line line-count))))))

(defun deslop-tools--buffer-record (buffer root)
  "Return metadata for allowed project BUFFER."
  (let ((path (deslop-project--buffer-path buffer)))
    (deslop-tools--source-metadata
     (list :kind 'buffer :buffer buffer :path path
           :relative (file-relative-name path root)
           :buffer-id (deslop-project-buffer-id buffer)))))

(defun deslop-tools--list-project-buffers (_arguments root)
  "List all allowed live visiting buffers below ROOT."
  (list :buffers
        (mapcar (lambda (buffer)
                  (with-current-buffer buffer
                    (append (list :name (buffer-name buffer))
                            (deslop-tools--buffer-record buffer root))))
                (sort (deslop-project-buffers root)
                      (lambda (left right)
                        (string< (or (buffer-file-name left) "")
                                 (or (buffer-file-name right) "")))))))

(defun deslop-tools--read-buffer (arguments root)
  "Read a bounded range from one allowed visiting buffer."
  (let* ((buffer-id (deslop-tools--required-string arguments "buffer_id"))
         (source (deslop-tools--resolve-source
                  (list :buffer_id buffer-id) root))
         (start-line (deslop-tools--bounded-integer
                      arguments "start_line" 1 most-positive-fixnum))
         (line-count (deslop-tools--bounded-integer
                      arguments "line_count" deslop-tools-max-read-lines
                      deslop-tools-max-read-lines)))
    (deslop-tools--with-source
     source
     (lambda ()
       (append (list :path (plist-get source :relative)
                     :source_kind "buffer"
                     :buffer_id buffer-id
                     :digest (deslop-tools--source-digest))
               (deslop-tools--line-slice start-line line-count))))))

(defun deslop-tools--search-source
    (source regexp context-before context-after maximum)
  "Search SOURCE for REGEXP and return at most MAXIMUM matches."
  (let ((matches nil)
        (finished nil))
    (deslop-tools--with-source
     source
     (lambda ()
       (goto-char (point-min))
        (while (and (not finished)
                    (< (length matches) maximum)
                    (re-search-forward regexp nil t))
         (let* ((match-start (match-beginning 0))
                (line (line-number-at-pos match-start))
                (column (save-excursion
                          (goto-char match-start)
                          (current-column)))
                (match-line
                 (save-excursion
                   (goto-char match-start)
                   (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position))))
                (context
                 (save-excursion
                   (goto-char match-start)
                   (beginning-of-line)
                   (forward-line (- context-before))
                   (let ((context-start (point)))
                     (goto-char match-start)
                     (forward-line context-after)
                     (buffer-substring-no-properties
                      context-start (line-end-position))))))
           (push (list :path (plist-get source :relative)
                       :source_kind (if (plist-get source :buffer)
                                        "buffer" "file")
                       :buffer_id (plist-get source :buffer-id)
                       :line line :column column
                       :match match-line :context context)
                 matches))
          ;; Ensure a zero-width regexp cannot keep the search at one point.
          (when (= (point) (match-beginning 0))
            (if (= (point) (point-max))
                (setq finished t)
              (forward-char 1))))))
    (nreverse matches)))

(defun deslop-tools--search-project (arguments root)
  "Search allowed project files and current visiting buffers."
  (let* ((regexp (deslop-tools--required-string arguments "regexp"))
         (glob (deslop-tools--argument arguments "glob"))
         (context-before
          (deslop-tools--bounded-integer
           arguments "context_before" deslop-tools-max-search-context-lines
           deslop-tools-max-search-context-lines 0))
         (context-after
          (deslop-tools--bounded-integer
           arguments "context_after" deslop-tools-max-search-context-lines
           deslop-tools-max-search-context-lines 0))
         (maximum (deslop-tools--bounded-integer
                   arguments "max_results" deslop-tools-max-search-results
                   deslop-tools-max-search-results))
         (glob-regexp (and glob (wildcard-to-regexp glob)))
         (files
          (cl-remove-if-not
           (lambda (path)
             (or (null glob-regexp)
                 (string-match-p glob-regexp
                                 (file-relative-name path root))))
           (deslop-project-files root)))
         (matches nil)
         (files-inspected 0)
         (truncated nil))
    ;; Validate the regexp before opening project files.
    (string-match-p regexp "")
    (catch 'finished
      (dolist (path (cl-subseq
                     files 0 (min deslop-tools-max-search-files
                                   (length files))))
        (cl-incf files-inspected)
        (let* ((relative (file-relative-name path root))
               (buffer (deslop-project-visiting-buffer relative root))
               (source (list :kind (if buffer 'buffer 'file)
                             :buffer buffer :path path :relative relative
                             :buffer-id
                             (and buffer (deslop-project-buffer-id buffer)))))
          (setq matches
                (append matches
                        (deslop-tools--search-source
                         source regexp context-before context-after
                         (- maximum (length matches)))))
          (when (>= (length matches) maximum)
            (setq truncated t)
            (throw 'finished nil))))
      (when (> (length files) deslop-tools-max-search-files)
        (setq truncated t)))
    (list :matches matches :files_inspected files-inspected
          :truncated truncated)))

;;;; Change sets and editing

(cl-defstruct (deslop-change-set (:constructor deslop-change-set--create))
  "Validated edits waiting to be applied to one project source."
  id root source-kind path buffer-id base-digest edits status created-at overlays)

(defvar deslop-tools--next-change-set-id 0
  "Next numeric identifier assigned to a pending change set.")

(defvar deslop-tools--change-sets (make-hash-table :test #'equal)
  "Change sets retained for explicit user application or rejection.")

(defun deslop-tools--sequence (value)
  "Return VALUE as a list when it is a JSON array or Lisp sequence."
  (cond
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t nil)))

(defun deslop-tools--edit-value (edit name)
  "Return NAME from edit object EDIT."
  (deslop-tools--argument edit name))

(defun deslop-tools--validate-edits (raw-edits source)
  "Validate RAW-EDITS against complete SOURCE and return normalized edits."
  (let ((raw-edits (deslop-tools--sequence raw-edits)))
    (unless raw-edits
      (error "edit_source requires a non-empty edits array"))
    (when (> (length raw-edits) deslop-tools-max-edits)
      (error "edit_source contains too many edits"))
    (let ((edits nil)
          (source-length (length source))
          (index 0))
      (dolist (raw raw-edits)
        (let ((start (deslop-tools--edit-value raw "start"))
              (end (deslop-tools--edit-value raw "end"))
              (expected (deslop-tools--edit-value raw "expected"))
              (replacement (or (deslop-tools--edit-value raw "replacement")
                               (deslop-tools--edit-value raw "text")))
              (reason (deslop-tools--edit-value raw "reason")))
          (unless (and (integerp start) (integerp end)
                       (<= 0 start end source-length)
                       (stringp expected) (stringp replacement))
            (error "edit %d has invalid start, end, expected, or replacement"
                   (1+ index)))
          (unless (string= expected (substring source start end))
            (error "edit %d does not match its expected text" (1+ index)))
          (push (list :index index :start start :end end
                      :expected expected :replacement replacement
                      :reason reason)
                edits))
        (cl-incf index))
      (setq edits (nreverse edits))
      (cl-loop for tail on edits do
        (dolist (right (cdr tail))
          (when (deslop-edit-replacement-overlaps-p (car tail) right)
            (error "edits %d and %d overlap"
                   (1+ (plist-get (car tail) :index))
                   (1+ (plist-get right :index))))))
      edits)))

(defun deslop-tools--change-set-result (change-set)
  "Return a JSON-compatible summary of CHANGE-SET."
  (list :change_set_id (deslop-change-set-id change-set)
        :status (deslop-change-set-status change-set)
        :source_kind (deslop-change-set-source-kind change-set)
        :path (deslop-change-set-path change-set)
        :buffer_id (deslop-change-set-buffer-id change-set)
        :base_digest (deslop-change-set-base-digest change-set)
        :edits (deslop-change-set-edits change-set)))

(defun deslop-tools--edit-source (arguments root)
  "Validate an edit proposal for one project source."
  (let* ((source (deslop-tools--resolve-source arguments root))
         (base-digest (deslop-tools--required-string arguments "base_digest"))
         (edits-argument (deslop-tools--argument arguments "edits"))
         (change-set nil))
    (deslop-tools--with-source
     source
     (lambda ()
       (let ((current-digest (deslop-tools--source-digest))
             (content (buffer-string)))
         (unless (string= base-digest current-digest)
           (error "source digest no longer matches the supplied base_digest"))
         (setq change-set
               (deslop-change-set--create
                :id (format "change-set-%d"
                            (cl-incf deslop-tools--next-change-set-id))
                :root root
                :source-kind (if (plist-get source :buffer) "buffer" "file")
                :path (plist-get source :relative)
                :buffer-id (plist-get source :buffer-id)
                :base-digest base-digest
                :edits (deslop-tools--validate-edits edits-argument content)
                :status "pending"
                :created-at (float-time)))
          (puthash (deslop-change-set-id change-set) change-set
                   deslop-tools--change-sets))))
     (if deslop-tools-direct-write
         (deslop-tools--apply-change-set-object change-set)
       (deslop-tools--change-set-result change-set))))

(defun deslop-tools--apply-edits-in-buffer (edits &optional review)
  "Apply normalized descending EDITS in the current buffer.

When REVIEW is non-nil, create review overlays for generated replacements and
return them."
  (let ((overlays nil))
    (dolist (edit (sort (copy-sequence edits)
                        (lambda (left right)
                          (> (plist-get left :start)
                             (plist-get right :start)))))
      (let* ((start (+ (point-min) (plist-get edit :start)))
             (end (+ (point-min) (plist-get edit :end)))
             (expected (plist-get edit :expected))
             (replacement (plist-get edit :replacement)))
        (unless (string= expected
                         (buffer-substring-no-properties start end))
          (error "source changed while applying edit %d"
                 (1+ (plist-get edit :index))))
        (goto-char start)
        (delete-region start end)
        (let ((review-start (point)))
          (insert replacement)
          (when review
            (push (deslop-review-create-overlay
                   (current-buffer) review-start (point) nil nil expected)
                  overlays)))))
    overlays))

(defun deslop-tools--current-change-set-source (change-set)
  "Resolve the current source represented by CHANGE-SET."
  (if (deslop-change-set-buffer-id change-set)
      (deslop-tools--resolve-source
       (list :buffer_id (deslop-change-set-buffer-id change-set))
       (deslop-change-set-root change-set))
    (deslop-tools--resolve-source
     (list :path (deslop-change-set-path change-set))
     (deslop-change-set-root change-set))))

(defun deslop-tools--apply-change-set-object (change-set)
  "Apply CHANGE-SET after revalidating its source and every edit."
  (let* ((source (deslop-tools--current-change-set-source change-set))
         (buffer (plist-get source :buffer))
         (path (plist-get source :path))
         (created-overlays nil))
    (condition-case failure
        (let ((result
               (deslop-tools--with-source
                source
                (lambda ()
                  (let ((content (buffer-string)))
                    (unless (string=
                             (deslop-change-set-base-digest change-set)
                             (deslop-tools--source-digest))
                      (error "source changed since the change set was created"))
                    (deslop-tools--validate-edits
                     (deslop-change-set-edits change-set) content)
                    (when buffer
                      (deslop-review-assert-available
                       buffer (point-min) (point-max))
                      (barf-if-buffer-read-only)
                      (let ((inhibit-read-only nil))
                        (atomic-change-group
                          (setq created-overlays
                                (deslop-tools--apply-edits-in-buffer
                                 (deslop-change-set-edits change-set) t)))))
                    (unless buffer
                      (deslop-tools--apply-edits-in-buffer
                       (deslop-change-set-edits change-set))
                      (write-region (point-min) (point-max) path nil 'silent))
                    (list :new_digest (deslop-tools--source-digest)
                          :applied_edits
                          (length (deslop-change-set-edits change-set))))))))
           (setf (deslop-change-set-status change-set) "applied"
                 (deslop-change-set-overlays change-set) created-overlays)
           (remhash (deslop-change-set-id change-set) deslop-tools--change-sets)
           (append (deslop-tools--change-set-result change-set) result))
      ((error quit)
       (dolist (overlay created-overlays)
         (deslop--ignore-errors #'deslop-review-delete-overlay overlay))
       (signal (car failure) (cdr failure))))))

(defun deslop-tools-change-set (id)
  "Return pending change set ID, or signal an error."
  (or (and (stringp id) (gethash id deslop-tools--change-sets))
      (error "unknown deslop change set: %s" id)))

(defun deslop-tools-apply-change-set (id)
  "Apply change set ID after a final source validation."
  (interactive (list (read-string "Apply deslop change set: ")))
  (let ((change-set (deslop-tools-change-set id)))
    (unless (string= (deslop-change-set-status change-set) "pending")
      (user-error "change set %s is already %s"
                  id (deslop-change-set-status change-set)))
    (let ((result (deslop-tools--apply-change-set-object change-set)))
      (message "deslop: applied %s" id)
      result)))

(defun deslop-tools-reject-change-set (id)
  "Reject pending change set ID."
  (interactive (list (read-string "Reject deslop change set: ")))
  (let ((change-set (deslop-tools-change-set id)))
     (unless (string= (deslop-change-set-status change-set) "pending")
       (user-error "change set %s is already %s"
                   id (deslop-change-set-status change-set)))
     (setf (deslop-change-set-status change-set) "rejected")
     (remhash id deslop-tools--change-sets)
     (message "deslop: rejected %s" id)
    (deslop-tools--change-set-result change-set)))

(defun deslop-tools-clear-change-sets ()
  "Clear retained deslop tool change sets."
  (interactive)
  (clrhash deslop-tools--change-sets))

;;;; Tool schemas and registrations

(defconst deslop-tools--empty-object-schema
  '((:type . "object") (:properties . ()) (:additionalProperties . :false)))

(defconst deslop-tools--path-schema
  '((:type . "object")
    (:properties
     . ((:path . ((:type . "string")
                  (:description . "Project-relative file path")))))
    (:required . ("path"))
    (:additionalProperties . :false)))

(defconst deslop-tools--edit-item-schema
  (list
   (cons :type "object")
   (cons :properties
         (list
          (cons :start '((:type . "integer") (:minimum . 0)))
          (cons :end '((:type . "integer") (:minimum . 0)))
          (cons :expected '((:type . "string")))
          (cons :replacement '((:type . "string")))
          (cons :reason '((:type . "string")))))
   (cons :required '("start" "end" "expected" "replacement"))
   (cons :additionalProperties :false)))

(defconst deslop-tools--edit-schema
  (list
   (cons :type "object")
   (cons :properties
         (list
          (cons :path '((:type . "string")))
          (cons :buffer_id '((:type . "string")))
          (cons :base_digest '((:type . "string")))
          (cons :edits
                (list (cons :type "array")
                      (cons :items deslop-tools--edit-item-schema)))))
   (cons :required '("base_digest" "edits"))
   (cons :additionalProperties :false)))

(deslop-tool-register
 'project_info
 "Return the current project root and compact source counts."
 deslop-tools--empty-object-schema #'deslop-tools--project-info)

(deslop-tool-register
 'list_project_files
 "List allowed regular files below the project root."
 '((:type . "object")
   (:properties
    . ((:glob . ((:type . "string")
                 (:description . "Optional project-relative wildcard")))
       (:max_results . ((:type . "integer") (:minimum . 1)))
       (:include_metadata . ((:type . "boolean")))))
   (:additionalProperties . :false))
 #'deslop-tools--list-project-files)

(deslop-tool-register
 'file_info
 "Return line count, size, digest, and visiting-buffer state for a project file."
 deslop-tools--path-schema #'deslop-tools--file-info)

(deslop-tool-register
 'read_file
 "Read a bounded, 1-based line range from a project file or its visiting buffer."
 '((:type . "object")
   (:properties
    . ((:path . ((:type . "string")))
       (:start_line . ((:type . "integer") (:minimum . 1)))
        (:line_count . ((:type . "integer") (:minimum . 1)))))
   (:required . ("path"))
   (:additionalProperties . :false))
 #'deslop-tools--read-file)

(deslop-tool-register
 'list_project_buffers
 "List live visiting buffers whose files belong to the project."
 deslop-tools--empty-object-schema #'deslop-tools--list-project-buffers)

(deslop-tool-register
 'read_buffer
 "Read a bounded, 1-based line range from an allowed visiting buffer."
 '((:type . "object")
   (:properties
    . ((:buffer_id . ((:type . "string")))
       (:start_line . ((:type . "integer") (:minimum . 1)))
        (:line_count . ((:type . "integer") (:minimum . 1)))))
   (:required . ("buffer_id"))
   (:additionalProperties . :false))
 #'deslop-tools--read-buffer)

(deslop-tool-register
 'search_project
 "Search allowed project files and current visiting buffers with an Emacs regexp."
 '((:type . "object")
   (:properties
    . ((:regexp . ((:type . "string")))
       (:glob . ((:type . "string")))
       (:context_before . ((:type . "integer") (:minimum . 0)))
       (:context_after . ((:type . "integer") (:minimum . 0)))
       (:max_results . ((:type . "integer") (:minimum . 1)))))
   (:required . ("regexp"))
   (:additionalProperties . :false))
 #'deslop-tools--search-project)

(deslop-tool-register
 'edit_source
 "Create validated edits for one project file or visiting buffer. Edits use zero-based character offsets in the complete source, exact expected text, and replacement text."
 deslop-tools--edit-schema
 #'deslop-tools--edit-source :write)

(provide 'deslop-tools)

;;; deslop-tools.el ends here
