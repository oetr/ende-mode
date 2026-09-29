;;; ende-tools.el --- Project tools for ende agents -*- lexical-binding: t; -*-

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
(require 'ende-core)
(require 'ende-edit)
(require 'ende-project)
(require 'ende-review)

(defcustom ende-tools-max-listed-files 200
  "Maximum files returned by one `list_project_files' call."
  :type 'integer
  :group 'ende)

(defcustom ende-tools-max-read-lines 500
  "Maximum lines returned by one file or buffer read call."
  :type 'integer
  :group 'ende)

(defcustom ende-tools-max-search-files 1000
  "Maximum files inspected by one `search_project' call."
  :type 'integer
  :group 'ende)

(defcustom ende-tools-max-search-results 100
  "Maximum matches returned by one `search_project' call."
  :type 'integer
  :group 'ende)

(defcustom ende-tools-max-search-context-lines 3
  "Maximum context lines returned around one search match."
  :type 'integer
  :group 'ende)

(defcustom ende-tools-max-edits 100
  "Maximum edits accepted by one `edit_source' call."
  :type 'integer
  :group 'ende)

(defcustom ende-tools-direct-write nil
  "Non-nil to apply `edit_source' proposals immediately.

When nil, model edits become pending change sets and must be applied with
`ende-tools-apply-change-set'."
  :type 'boolean
  :group 'ende)

;;;; Tool registry

(cl-defstruct (ende-tool (:constructor ende-tool--create))
  "One model-callable ende tool."
  name description parameters handler read-only requires-resource)

(defvar ende-tools--registry nil
  "Registered model tools, keyed by their symbolic names.")

(defvar ende-tools--access-config nil
  "Dynamically bound resolved chat access configuration for a tool call.")

(defvar ende-tools--access-tool nil
  "Tool symbol currently being authorized by a handler.")

(defun ende-tool-register
    (name description parameters handler &optional read-only resource-policy)
  "Register tool NAME with DESCRIPTION, PARAMETERS, and HANDLER.

HANDLER receives an arguments object and a canonical project root.  NAME is a
symbol internally and is exposed to model transports as a string.  Pass
`:write' as READ-ONLY for a mutating tool.  RESOURCE-POLICY may be
`:no-resource' for tools that do not access project paths."
  (unless (and (symbolp name) (functionp handler))
    (error "invalid ende tool registration: %s" name))
  (let ((tool (ende-tool--create
               :name name :description description :parameters parameters
               :handler handler :read-only (not (eq read-only :write))
               :requires-resource (not (eq resource-policy :no-resource)))))
    (setf (alist-get name ende-tools--registry) tool)
    tool))

(defun ende-tool--find (name)
  "Return the registered tool called NAME, or nil."
  (let ((name (if (symbolp name) (symbol-name name) (format "%s" name))))
    (cdr (cl-find-if
          (lambda (entry)
            (string= name (symbol-name (car entry))))
	  ende-tools--registry))))

(defun ende-tool-definitions ()
  "Return registered tools in OpenAI-compatible function-tool shape."
  (mapcar
   (lambda (tool)
     (list (cons :type "function")
           (cons :function
                 (list (cons :name (symbol-name (ende-tool-name tool)))
                       (cons :description (ende-tool-description tool))
                       (cons :parameters (ende-tool-parameters tool))))))
   (mapcar #'cdr ende-tools--registry)))

(defun ende-tool-call (name arguments &optional root access-config)
  "Invoke model tool NAME with ARGUMENTS within project ROOT.

Return a JSON-compatible result object.  Tool failures are returned as data so
an agent can recover from an invalid path, regexp, or stale source version.
When ACCESS-CONFIG is non-nil, it must be the normalized chat configuration
and controls tool activation and path authorization."
  (let ((tool (ende-tool--find name)))
    (if (null tool)
        (list :ok nil :tool (format "%s" name)
              :error (format "unknown ende tool: %s" name))
      (condition-case caught
          (let ((tool-name (ende-tool-name tool)))
            (when access-config
              (unless (plist-get access-config :root)
                (error "chat tool configuration has no explicit root"))
              (unless (memq tool-name (plist-get access-config :active-tools))
                (error "tool is not active in this chat: %s" tool-name))
              (when (and (ende-tool-requires-resource tool)
                         (null (cdr (assq
                                     tool-name
                                     (plist-get access-config
                                                :tool-resources)))))
                (error "tool has no assigned resources: %s" tool-name)))
            (let* ((root (ende-project-normalize-root
                          (or (and access-config
                                   (plist-get access-config :root))
                              root
                              (ende-project-root))))
                   (ende-tools--access-config access-config)
                   (ende-tools--access-tool tool-name))
              (list :ok t
                    :tool (symbol-name tool-name)
                    :result (funcall (ende-tool-handler tool) arguments root))))
        ((error quit)
         (list :ok nil
               :tool (symbol-name (ende-tool-name tool))
               :error (error-message-string caught)))))))

(defun ende-tools--resource-allows-p (relative resource)
  "Return non-nil when RESOURCE permits project-relative path RELATIVE."
  (and (cl-some (lambda (regexp) (string-match-p regexp relative))
                (plist-get resource :include-regexps))
       (not (cl-some (lambda (regexp) (string-match-p regexp relative))
                     (plist-get resource :exclude-regexps)))))

(defun ende-tools--path-authorized-p (path root)
  "Return non-nil when canonical PATH is allowed by the current tool policy."
  (if (null ende-tools--access-config)
      t
    (let* ((root (file-name-as-directory root))
           (canonical
            (condition-case nil
                (if (file-exists-p path)
                    (file-truename path)
                  (expand-file-name path root))
              (error nil)))
           (relative (and canonical
                          (file-in-directory-p canonical root)
                          (file-relative-name canonical root)))
           (assignments (plist-get ende-tools--access-config
                                   :tool-resources))
           (resource-names (cdr (assq ende-tools--access-tool assignments)))
           (resources (plist-get ende-tools--access-config :resources)))
      (and relative
           (not (ende-project--excluded-p relative))
           (cl-some
            (lambda (name)
              (let ((resource (cdr (assq name resources))))
                (and resource
                     (ende-tools--resource-allows-p relative resource))))
            resource-names)))))

(defun ende-tools--assert-path-authorized (path root)
  "Signal unless PATH is allowed for the current tool in ROOT."
  (unless (ende-tools--path-authorized-p path root)
    (error "path is not authorized by this chat's resources: %s"
           (file-relative-name path root)))
  path)

;;;; Arguments and source resolution

(defun ende-tools--argument (arguments name)
  "Return NAME from JSON hash table, plist, or alist ARGUMENTS."
  (cond
   ((hash-table-p arguments)
    (gethash name arguments))
   ((listp arguments)
    (or (plist-get arguments (intern (concat ":" name)))
        (alist-get name arguments nil nil #'equal)
        (alist-get (intern name) arguments)))
   (t nil)))

(defun ende-tools--required-string (arguments name)
  "Return required string argument NAME from ARGUMENTS."
  (let ((value (ende-tools--argument arguments name)))
    (unless (and (stringp value) (not (string-empty-p value)))
      (error "tool argument %s must be a non-empty string" name))
    value))

(defun ende-tools--bounded-integer
    (arguments name default maximum &optional minimum)
  "Return integer NAME from ARGUMENTS bounded by MINIMUM and MAXIMUM."
  (let ((value (or (ende-tools--argument arguments name) default))
        (minimum (or minimum 1)))
    (unless (and (integerp value) (>= value minimum) (<= value maximum))
      (error "tool argument %s must be an integer from %d to %d"
             name minimum maximum))
    value))

(defun ende-tools--resolve-source (arguments root)
  "Resolve one path or buffer_id in ARGUMENTS below ROOT.

Return a plist containing `:kind', `:path', `:relative', and optionally
`:buffer'.  A path that has a visiting buffer resolves to that buffer so
unsaved changes take precedence over disk contents."
  (let* ((path (ende-tools--argument arguments "path"))
         (buffer-id (ende-tools--argument arguments "buffer_id"))
         (has-path (and (stringp path) (not (string-empty-p path))))
         (has-buffer (and (stringp buffer-id)
                          (not (string-empty-p buffer-id)))))
    (when (eq has-path has-buffer)
      (error "provide exactly one of path or buffer_id"))
    (if has-buffer
        (let* ((buffer (ende-project-buffer-for-id buffer-id root))
               (buffer-path (ende-project-buffer-path buffer))
               (relative (file-relative-name buffer-path root)))
          (ende-tools--assert-path-authorized buffer-path root)
          (list :kind 'buffer :buffer buffer :path buffer-path
                :relative relative :buffer-id buffer-id))
      (let* ((canonical
              (ende-tools--assert-path-authorized
               (ende-project-path path root) root))
             (buffer (ende-project-visiting-buffer path root))
             (relative (file-relative-name canonical root)))
        (list :kind (if buffer 'buffer 'file)
              :buffer buffer :path canonical :relative relative
              :buffer-id (and buffer (ende-project-buffer-id buffer)))))))

(defun ende-tools--with-source (source function)
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
      (ende-project-assert-readable-file path)
      (with-temp-buffer
        (insert-file-contents path)
        (funcall function)))))

(defun ende-tools--source-digest ()
  "Return a SHA-256 digest of the complete current source buffer."
  (secure-hash 'sha256 (current-buffer) (point-min) (point-max)))

(defun ende-tools--source-line-count ()
  "Return the number of lines in the complete current source buffer."
  (count-lines (point-min) (point-max)))

(defun ende-tools--source-metadata (source)
  "Return metadata for SOURCE using its current contents."
  (let ((path (plist-get source :path))
        (buffer (plist-get source :buffer)))
    (ende-tools--with-source
     source
     (lambda ()
       (list :path (plist-get source :relative)
             :source_kind (if buffer "buffer" "file")
             :buffer_id (and buffer (ende-project-buffer-id buffer))
             :line_count (ende-tools--source-line-count)
             :byte_count (string-bytes (buffer-string))
             :digest (ende-tools--source-digest)
             :buffer_modified (and buffer (buffer-modified-p))
             :disk_mtime
             (and (file-exists-p path)
                  (format-time-string
                   "%Y-%m-%dT%H:%M:%S%z"
                   (file-attribute-modification-time
                    (file-attributes path)))))))))

(defun ende-tools--line-slice (start-line line-count)
  "Return a bounded line slice from the current source buffer."
  (let ((total (ende-tools--source-line-count)))
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

(defun ende-tools--project-info (_arguments root)
  "Return basic information about project ROOT."
  (list :root root
        :name (file-name-nondirectory (directory-file-name root))
        :file_count (length (ende-project-files root))
        :buffer_count (length (ende-project-buffers root))))

(defun ende-tools--file-record (path root include-metadata)
  "Return a compact project file record for PATH below ROOT."
  (let* ((relative (file-relative-name path root))
         (attributes (file-attributes path))
         (buffer (ende-project-visiting-buffer relative root))
         (record (list :path relative
                       :byte_count (file-attribute-size attributes)
                       :buffer_id (and buffer (ende-project-buffer-id buffer))
                       :buffer_modified (and buffer (buffer-modified-p)))))
    (if include-metadata
        (ende-tools--source-metadata
         (list :kind (if buffer 'buffer 'file) :buffer buffer :path path
               :relative relative :buffer-id
               (and buffer (ende-project-buffer-id buffer))))
      record)))

(defun ende-tools--list-project-files (arguments root)
  "List allowed files below ROOT, optionally filtered by a glob."
  (let* ((glob (ende-tools--argument arguments "glob"))
         (include-metadata (eq (ende-tools--argument
                                arguments "include_metadata") t))
         (maximum (ende-tools--bounded-integer
                   arguments "max_results" ende-tools-max-listed-files
                   ende-tools-max-listed-files))
         (glob-regexp (and glob (wildcard-to-regexp glob)))
         (files
          (cl-remove-if-not
           (lambda (path)
             (and (ende-tools--path-authorized-p path root)
                  (or (null glob-regexp)
                      (string-match-p
                       glob-regexp (file-relative-name path root)))))
           (ende-project-files root)))
         (truncated (> (length files) maximum)))
    (list :files
          (mapcar (lambda (path)
                    (ende-tools--file-record path root include-metadata))
                  (cl-subseq files 0 (min maximum (length files))))
          :truncated truncated)))

(defun ende-tools--file-info (arguments root)
  "Return metadata for one project file."
  (let* ((path-value (ende-tools--required-string arguments "path"))
         (path (ende-tools--assert-path-authorized
                (ende-project-path path-value root) root))
         (attributes (progn
                       (ende-project-assert-readable-file path)
                       (file-attributes path)))
         (source (ende-tools--resolve-source
                  (list :path path-value) root))
         (metadata (ende-tools--source-metadata source)))
    (plist-put metadata :disk_byte_count (file-attribute-size attributes))))

(defun ende-tools--read-file (arguments root)
  "Read a bounded range from one project file."
  (let* ((source (ende-tools--resolve-source arguments root))
         (start-line (ende-tools--bounded-integer
                      arguments "start_line" 1 most-positive-fixnum))
         (line-count (ende-tools--bounded-integer
                      arguments "line_count" ende-tools-max-read-lines
                      ende-tools-max-read-lines)))
    (ende-tools--with-source
     source
     (lambda ()
       (append (list :path (plist-get source :relative)
                     :source_kind (if (plist-get source :buffer)
                                      "buffer" "file")
                     :buffer_id (plist-get source :buffer-id)
                     :digest (ende-tools--source-digest))
               (ende-tools--line-slice start-line line-count))))))

(defun ende-tools--buffer-record (buffer root)
  "Return metadata for allowed project BUFFER."
  (let ((path (ende-project-buffer-path buffer)))
    (ende-tools--source-metadata
     (list :kind 'buffer :buffer buffer :path path
           :relative (file-relative-name path root)
           :buffer-id (ende-project-buffer-id buffer)))))

(defun ende-tools--list-project-buffers (_arguments root)
  "List all allowed live visiting buffers below ROOT."
  (list :buffers
        (mapcar
         (lambda (buffer)
           (with-current-buffer buffer
             (append (list :name (buffer-name buffer))
                     (ende-tools--buffer-record buffer root))))
         (sort
          (cl-remove-if-not
           (lambda (buffer)
             (ende-tools--path-authorized-p
              (ende-project-buffer-path buffer) root))
           (ende-project-buffers root))
          (lambda (left right)
            (string< (or (buffer-file-name left) "")
                     (or (buffer-file-name right) "")))))))

(defun ende-tools--read-buffer (arguments root)
  "Read a bounded range from one allowed visiting buffer."
  (let* ((buffer-id (ende-tools--required-string arguments "buffer_id"))
         (source (ende-tools--resolve-source
                  (list :buffer_id buffer-id) root))
         (start-line (ende-tools--bounded-integer
                      arguments "start_line" 1 most-positive-fixnum))
         (line-count (ende-tools--bounded-integer
                      arguments "line_count" ende-tools-max-read-lines
                      ende-tools-max-read-lines)))
    (ende-tools--with-source
     source
     (lambda ()
       (append (list :path (plist-get source :relative)
                     :source_kind "buffer"
                     :buffer_id buffer-id
                     :digest (ende-tools--source-digest))
               (ende-tools--line-slice start-line line-count))))))

(defun ende-tools--search-source
    (source regexp context-before context-after maximum)
  "Search SOURCE for REGEXP and return at most MAXIMUM matches."
  (let ((matches nil)
        (finished nil))
    (ende-tools--with-source
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

(defun ende-tools--search-project (arguments root)
  "Search allowed project files and current visiting buffers."
  (let* ((regexp (ende-tools--required-string arguments "regexp"))
         (glob (ende-tools--argument arguments "glob"))
         (context-before
          (ende-tools--bounded-integer
           arguments "context_before" ende-tools-max-search-context-lines
           ende-tools-max-search-context-lines 0))
         (context-after
          (ende-tools--bounded-integer
           arguments "context_after" ende-tools-max-search-context-lines
           ende-tools-max-search-context-lines 0))
         (maximum (ende-tools--bounded-integer
                   arguments "max_results" ende-tools-max-search-results
                   ende-tools-max-search-results))
         (glob-regexp (and glob (wildcard-to-regexp glob)))
         (files
          (cl-remove-if-not
           (lambda (path)
             (and (ende-tools--path-authorized-p path root)
                  (or (null glob-regexp)
                      (string-match-p glob-regexp
                                      (file-relative-name path root)))))
           (ende-project-files root)))
         (matches nil)
         (files-inspected 0)
         (truncated nil))
    ;; Validate the regexp before opening project files.
    (string-match-p regexp "")
    (catch 'finished
      (dolist (path (cl-subseq
                     files 0 (min ende-tools-max-search-files
                                  (length files))))
        (cl-incf files-inspected)
        (let* ((relative (file-relative-name path root))
               (buffer (ende-project-visiting-buffer relative root))
               (source (list :kind (if buffer 'buffer 'file)
                             :buffer buffer :path path :relative relative
                             :buffer-id
                             (and buffer (ende-project-buffer-id buffer)))))
          (setq matches
                (append matches
                        (ende-tools--search-source
                         source regexp context-before context-after
                         (- maximum (length matches)))))
          (when (>= (length matches) maximum)
            (setq truncated t)
            (throw 'finished nil))))
      (when (> (length files) ende-tools-max-search-files)
        (setq truncated t)))
    (list :matches matches :files_inspected files-inspected
          :truncated truncated)))

;;;; Change sets and editing

(cl-defstruct (ende-change-set (:constructor ende-change-set--create))
  "Validated edits waiting to be applied to one project source."
  id root source-kind path buffer-id base-digest edits status created-at overlays)

(defvar ende-tools--next-change-set-id 0
  "Next numeric identifier assigned to a pending change set.")

(defvar ende-tools--change-sets (make-hash-table :test #'equal)
  "Change sets retained for explicit user application or rejection.")

(defun ende-tools--sequence (value)
  "Return VALUE as a list when it is a JSON array or Lisp sequence."
  (cond
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t nil)))

(defun ende-tools--edit-value (edit name)
  "Return NAME from edit object EDIT."
  (ende-tools--argument edit name))

(defun ende-tools--validate-edits (raw-edits source)
  "Validate RAW-EDITS against complete SOURCE and return normalized edits."
  (let ((raw-edits (ende-tools--sequence raw-edits)))
    (unless raw-edits
      (error "edit_source requires a non-empty edits array"))
    (when (> (length raw-edits) ende-tools-max-edits)
      (error "edit_source contains too many edits"))
    (let ((edits nil)
          (source-length (length source))
          (index 0))
      (dolist (raw raw-edits)
        (let ((start (ende-tools--edit-value raw "start"))
              (end (ende-tools--edit-value raw "end"))
              (expected (ende-tools--edit-value raw "expected"))
              (replacement (or (ende-tools--edit-value raw "replacement")
                               (ende-tools--edit-value raw "text")))
              (reason (ende-tools--edit-value raw "reason")))
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
		 (when (ende-edit-replacement-overlaps-p (car tail) right)
		   (error "edits %d and %d overlap"
			  (1+ (plist-get (car tail) :index))
			  (1+ (plist-get right :index))))))
      edits)))

(defun ende-tools--change-set-result (change-set)
  "Return a JSON-compatible summary of CHANGE-SET."
  (list :change_set_id (ende-change-set-id change-set)
        :status (ende-change-set-status change-set)
        :source_kind (ende-change-set-source-kind change-set)
        :path (ende-change-set-path change-set)
        :buffer_id (ende-change-set-buffer-id change-set)
        :base_digest (ende-change-set-base-digest change-set)
        :edits (ende-change-set-edits change-set)))

(defun ende-tools--edit-source (arguments root)
  "Validate an edit proposal for one project source."
  (let* ((source (ende-tools--resolve-source arguments root))
         (base-digest (ende-tools--required-string arguments "base_digest"))
         (edits-argument (ende-tools--argument arguments "edits"))
         (change-set nil))
    (ende-tools--with-source
     source
     (lambda ()
       (let ((current-digest (ende-tools--source-digest))
             (content (buffer-string)))
         (unless (string= base-digest current-digest)
           (error "source digest no longer matches the supplied base_digest"))
         (setq change-set
               (ende-change-set--create
                :id (format "change-set-%d"
                            (cl-incf ende-tools--next-change-set-id))
                :root root
                :source-kind (if (plist-get source :buffer) "buffer" "file")
                :path (plist-get source :relative)
                :buffer-id (plist-get source :buffer-id)
                :base-digest base-digest
                :edits (ende-tools--validate-edits edits-argument content)
                :status "pending"
                :created-at (float-time)))
         (puthash (ende-change-set-id change-set) change-set
                  ende-tools--change-sets))))
    (if ende-tools-direct-write
        (ende-tools--apply-change-set-object change-set)
      (ende-tools--change-set-result change-set))))

(defun ende-tools--apply-edits-in-buffer (edits &optional review)
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
            (push (ende-review-create-overlay
                   (current-buffer) review-start (point) nil nil expected)
                  overlays)))))
    overlays))

(defun ende-tools--current-change-set-source (change-set)
  "Resolve the current source represented by CHANGE-SET."
  (if (ende-change-set-buffer-id change-set)
      (ende-tools--resolve-source
       (list :buffer_id (ende-change-set-buffer-id change-set))
       (ende-change-set-root change-set))
    (ende-tools--resolve-source
     (list :path (ende-change-set-path change-set))
     (ende-change-set-root change-set))))

(defun ende-tools--apply-change-set-object (change-set)
  "Apply CHANGE-SET after revalidating its source and every edit."
  (let* ((source (ende-tools--current-change-set-source change-set))
         (buffer (plist-get source :buffer))
         (path (plist-get source :path))
         (created-overlays nil))
    (condition-case failure
        (let ((result
               (ende-tools--with-source
                source
                (lambda ()
                  (let ((content (buffer-string)))
                    (unless (string=
                             (ende-change-set-base-digest change-set)
                             (ende-tools--source-digest))
                      (error "source changed since the change set was created"))
                    (ende-tools--validate-edits
                     (ende-change-set-edits change-set) content)
                    (when buffer
                      (ende-review-assert-available
                       buffer (point-min) (point-max))
                      (barf-if-buffer-read-only)
                      (let ((inhibit-read-only nil))
                        (atomic-change-group
                          (setq created-overlays
                                (ende-tools--apply-edits-in-buffer
                                 (ende-change-set-edits change-set) t)))))
                    (unless buffer
                      (ende-tools--apply-edits-in-buffer
                       (ende-change-set-edits change-set))
                      (write-region (point-min) (point-max) path nil 'silent))
                    (list :new_digest (ende-tools--source-digest)
                          :applied_edits
                          (length (ende-change-set-edits change-set))))))))
          (setf (ende-change-set-status change-set) "applied"
                (ende-change-set-overlays change-set) created-overlays)
          (remhash (ende-change-set-id change-set) ende-tools--change-sets)
          (append (ende-tools--change-set-result change-set) result))
      ((error quit)
       (dolist (overlay created-overlays)
         (ende-ignore-errors #'ende-review-delete-overlay overlay))
       (signal (car failure) (cdr failure))))))

(defun ende-tools-change-set (id)
  "Return pending change set ID, or signal an error."
  (or (and (stringp id) (gethash id ende-tools--change-sets))
      (error "unknown ende change set: %s" id)))

(defun ende-tools-apply-change-set (id)
  "Apply change set ID after a final source validation."
  (interactive (list (read-string "Apply ende change set: ")))
  (let ((change-set (ende-tools-change-set id)))
    (unless (string= (ende-change-set-status change-set) "pending")
      (user-error "change set %s is already %s"
                  id (ende-change-set-status change-set)))
    (let ((result (ende-tools--apply-change-set-object change-set)))
      (message "ende: applied %s" id)
      result)))

(defun ende-tools-reject-change-set (id)
  "Reject pending change set ID."
  (interactive (list (read-string "Reject ende change set: ")))
  (let ((change-set (ende-tools-change-set id)))
    (unless (string= (ende-change-set-status change-set) "pending")
      (user-error "change set %s is already %s"
                  id (ende-change-set-status change-set)))
    (setf (ende-change-set-status change-set) "rejected")
    (remhash id ende-tools--change-sets)
    (message "ende: rejected %s" id)
    (ende-tools--change-set-result change-set)))

(defun ende-tools-clear-change-sets ()
  "Clear retained ende tool change sets."
  (interactive)
  (clrhash ende-tools--change-sets))

;;;; Tool schemas and registrations

(defconst ende-tools--empty-object-schema
  (list (cons :type "object")
        (cons :properties (make-hash-table))
        (cons :additionalProperties :json-false)))

(defconst ende-tools--path-schema
  '((:type . "object")
    (:properties
     . ((:path . ((:type . "string")
                  (:description . "Project-relative file path")))))
    (:required . ("path"))
    (:additionalProperties . :json-false)))

(defconst ende-tools--edit-item-schema
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
   (cons :additionalProperties :json-false)))

(defconst ende-tools--edit-schema
  (list
   (cons :type "object")
   (cons :properties
         (list
          (cons :path '((:type . "string")
                        (:description . "Project-relative file path")))
          (cons :buffer_id '((:type . "string")))
          (cons :base_digest '((:type . "string")))
          (cons :edits
                (list (cons :type "array")
                      (cons :items ende-tools--edit-item-schema)))))
   (cons :required '("base_digest" "edits"))
   (cons :additionalProperties :json-false)))

(ende-tool-register
 'project_info
 "Return the current project root and compact source counts."
 ende-tools--empty-object-schema #'ende-tools--project-info nil :no-resource)

(ende-tool-register
 'list_project_files
 "List allowed regular files below the project root."
 '((:type . "object")
   (:properties
    . ((:glob . ((:type . "string")
                 (:description . "Optional project-relative wildcard")))
       (:max_results . ((:type . "integer") (:minimum . 1)))
       (:include_metadata . ((:type . "boolean")))))
   (:additionalProperties . :json-false))
 #'ende-tools--list-project-files)

(ende-tool-register
 'file_info
 "Return line count, size, digest, and visiting-buffer state for a project file."
 ende-tools--path-schema #'ende-tools--file-info)

(ende-tool-register
 'read_file
 "Read a bounded, 1-based line range from a project-relative path or its visiting buffer."
 '((:type . "object")
   (:properties
    . ((:path . ((:type . "string")
                 (:description . "Project-relative file path")))
       (:start_line . ((:type . "integer") (:minimum . 1)))
       (:line_count . ((:type . "integer") (:minimum . 1)))))
   (:required . ("path"))
   (:additionalProperties . :json-false))
 #'ende-tools--read-file)

(ende-tool-register
 'list_project_buffers
 "List live visiting buffers whose files belong to the project."
 ende-tools--empty-object-schema #'ende-tools--list-project-buffers)

(ende-tool-register
 'read_buffer
 "Read a bounded, 1-based line range from an allowed visiting buffer."
 '((:type . "object")
   (:properties
    . ((:buffer_id . ((:type . "string")))
       (:start_line . ((:type . "integer") (:minimum . 1)))
       (:line_count . ((:type . "integer") (:minimum . 1)))))
   (:required . ("buffer_id"))
   (:additionalProperties . :json-false))
 #'ende-tools--read-buffer)

(ende-tool-register
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
   (:additionalProperties . :json-false))
 #'ende-tools--search-project)

(ende-tool-register
 'edit_source
 "Create validated edits for one project-relative file path or visiting buffer. Edits use zero-based character offsets in the complete source, exact expected text, and replacement text."
 ende-tools--edit-schema
 #'ende-tools--edit-source :write)

(provide 'ende-tools)

;;; ende-tools.el ends here
