;;; ende-tools.el --- Project tools for ende agents -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools, llm

;;; Commentary:

;; A provider-independent registry and implementation of project-scoped read
;; tools, disk-file edits, file creation, and agent progress reporting.
;; Path-based tools use disk contents; buffer tools explicitly use visiting-buffer
;; contents.  about_thinking is always available, requires no project permissions,
;; and reports public activity rather than provider reasoning traces.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ende-core)
(require 'ende-edit)
(require 'ende-project)
(require 'ende-url)
(require 'ende-patch)
(require 'ende-web-fetch)
(require 'ende-sandbox)

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

(defcustom ende-tools-max-search-output-bytes 16384
  "Maximum serialized bytes returned by one `search_project' call."
  :type 'integer
  :group 'ende)

(defcustom ende-tools-max-search-context-lines 20
  "Maximum context lines returned around one search match."
  :type 'integer
  :group 'ende)

(defcustom ende-tools-max-edits 100
  "Maximum edits accepted by one `edit_file' call."
  :type 'integer
  :group 'ende)

(defcustom ende-tools-max-mutation-operations 50
  "Maximum file operations accepted by one batch mutation tool call."
  :type 'integer
  :group 'ende)

(defcustom ende-tools-max-prepared-bytes (* 20 1024 1024)
  "Maximum combined original and prepared data for one mutation batch."
  :type 'integer
  :group 'ende)

;;;; Tool registry

(cl-defstruct (ende-tool (:constructor ende-tool--create))
  "One model-callable ende tool."
  name description parameters handler resource-kind requires-resources requires-root
  mutates-local async always-available)

(defvar ende-tools--registry nil
  "Registered model tools, keyed by their symbolic names.")

(defvar ende-tools--access-config nil
  "Dynamically bound resolved chat access configuration for a tool call.")

(defvar ende-tools--access-tool nil
  "Tool symbol currently being authorized by a handler.")

(defvar ende-tools--execution-owner nil
  "Ende job ID currently dispatching a tool call.")

(defvar ende-tools--register-cancellation-function nil
  "Optional function used to attach and detach an active tool cancellation.")

(defvar ende-tools--report-activity-function nil
  "Job-scoped callback accepting an activity title and status paragraph.
Return non-nil if the originating agent job still accepts reports.")

(define-error 'ende-tool-mutation-failure "Ende mutation failed")

(defun ende-tool-register
    (name description parameters handler &optional read-only resource-policy options)
  "Register tool NAME with DESCRIPTION, PARAMETERS, and HANDLER.

HANDLER receives an arguments object and a canonical project root.  NAME is a
symbol internally and is exposed to model transports as a string.  Pass
`:write' as READ-ONLY for a local-file-mutating tool.  RESOURCE-POLICY keeps
the historic `:no-resource' registration form; OPTIONS may specify
`:resource-kind' (`file', `url', or `none'), `:requires-resources',
`:requires-root', `:async', and `:always-available'.  File and URL tools require
resources by default; resource-free tools do not.  Always-available tools must
be resource-free, rootless, and non-mutating; chat configuration cannot disable
them."
  (unless (and (symbolp name) (functionp handler))
    (error "invalid ende tool registration: %s" name))
  (let* ((kind (or (plist-get options :resource-kind)
                   (if (eq resource-policy :no-resource) 'none 'file)))
         (requires-resources
          (if (plist-member options :requires-resources)
              (plist-get options :requires-resources)
            (not (eq kind 'none))))
         (requires-root (if (plist-member options :requires-root)
                            (plist-get options :requires-root)
                          t))
         (tool (ende-tool--create
                :name name :description description :parameters parameters
                :handler handler :resource-kind kind
                :requires-resources requires-resources
                :requires-root requires-root
                :async (eq (plist-get options :async) t)
                :always-available (eq (plist-get options :always-available) t)
                :mutates-local (eq read-only :write))))
    (unless (memq kind '(file url none))
      (error "invalid ende tool resource kind: %s" kind))
    (unless (memq requires-resources '(nil t))
      (error "invalid ende tool resource requirement: %s" requires-resources))
    (when (and (eq kind 'none) requires-resources)
      (error "resource-free tool cannot require assigned resources: %s" name))
    (when (and (ende-tool-always-available tool)
               (or (not (eq kind 'none)) requires-root
                   (ende-tool-mutates-local tool)))
      (error "always-available tool must be resource-free and non-mutating: %s"
             name))
    (setf (alist-get name ende-tools--registry) tool)
    tool))

(defun ende-tool-read-only (tool)
  "Return non-nil when TOOL does not mutate local files."
  (not (ende-tool-mutates-local tool)))

(defun ende-tool--find (name)
  "Return the registered tool called NAME, or nil."
  (let ((name (if (symbolp name) (symbol-name name) (format "%s" name))))
    (cdr (cl-find-if
          (lambda (entry)
            (string= name (symbol-name (car entry))))
	  ende-tools--registry))))

(defun ende-tool-active-names (configured)
  "Return CONFIGURED names plus always-available tools, without duplicates.
Do not modify CONFIGURED.  Resource-free tools are not implicitly activated."
  (delete-dups
   (append configured
           (cl-loop for (name . tool) in ende-tools--registry
                    when (ende-tool-always-available tool) collect name))))

(defun ende-tool-definitions (&optional access-config)
  "Return registered tools in OpenAI-compatible function-tool shape.
Use ACCESS-CONFIG to generate its effective host-command schema."
  (mapcar
   (lambda (tool)
     (list (cons :type "function")
           (cons :function
                 (list (cons :name (symbol-name (ende-tool-name tool)))
                       (cons :description (ende-tool-description tool))
                       (cons :parameters
                             (if (eq (ende-tool-name tool) 'run_host_command)
                                 (ende-tools--host-command-schema access-config)
                               (ende-tool-parameters tool)))))))
   (mapcar #'cdr ende-tools--registry)))

(defun ende-tool--call-root (tool root access-config)
  (let* ((tool-name (ende-tool-name tool))
         (root-required (ende-tool-requires-root tool))
         (assigned (cdr (assq tool-name
                              (plist-get access-config :tool-resources)))))
    (when access-config
      (unless (memq tool-name (ende-tool-active-names
                               (plist-get access-config :active-tools)))
        (error "tool is not active in this chat: %s" tool-name))
      (when (and (ende-tool-requires-resources tool) (null assigned))
        (error "tool has no assigned resources: %s" tool-name)))
    (when (and access-config root-required
               (null (plist-get access-config :root)))
      (error "chat tool configuration has no explicit root"))
    (let ((root-value
           ;; Mandatory activity tools must not depend on a configured project.
           (unless (ende-tool-always-available tool)
             (or root
                 (and access-config root-required
                      (plist-get access-config :root))))))
      (cond
       (root-value (ende-project-normalize-root root-value))
       (root-required (ende-project-root))
       (t nil)))))

(defconst ende-tools--json-boolean-fields
  '(:ok :accepted :truncated :visiting_buffer :buffer_modified :process_ok
    :stdout_truncated :stderr_truncated)
  "Tool-result fields whose values are semantically booleans.")

(defun ende-tools--json-boolean-value (value)
  "Return VALUE encoded as a JSON boolean, respecting false sentinels."
  (if (memq value '(nil :false :json-false)) :json-false t))

(defun ende-tools--json-normalize-value (value)
  "Normalize boolean fields recursively in protocol VALUE only."
  (cond
   ((and (vectorp value) (not (stringp value)))
    (vconcat (mapcar #'ende-tools--json-normalize-value (append value nil))))
   ((and (listp value) (keywordp (car-safe value))
         (zerop (% (length value) 2)))
    (let ((copy (copy-sequence value))
          (tail nil))
      (setq tail copy)
      (while tail
        (let* ((field (car tail))
               (item (cadr tail)))
          (setcar (cdr tail)
                  (cond
                   ((and (eq field :visiting_buffer_modified)
                         (or (not (plist-member value :visiting_buffer))
                             (not (plist-get value :visiting_buffer))))
                    item)
                   ((memq field '(:visiting_buffer_modified :buffer_modified))
                    (ende-tools--json-boolean-value item))
                   ((memq field ende-tools--json-boolean-fields)
                    (ende-tools--json-boolean-value item))
                   (t (ende-tools--json-normalize-value item))))
          (setq tail (cddr tail))))
      copy))
   (t value)))

(defun ende-tools--json-tool-result (result)
  "Copy tool RESULT with semantically boolean fields JSON-normalized."
  (ende-tools--json-normalize-value result))

(defun ende-tools--json-encode-tool-result (result)
  "Serialize tool RESULT with boolean fields encoded as JSON booleans."
  (json-encode (ende-tools--json-tool-result result)))

(defun ende-tool--failure-result (tool caught)
  (if (eq (car-safe caught) 'ende-tool-mutation-failure)
      (list :ok nil
            :tool (symbol-name (ende-tool-name tool))
            :error (or (cadr caught) (error-message-string caught))
            :recovery (caddr caught))
    (list :ok nil
          :tool (symbol-name (ende-tool-name tool))
          :error (error-message-string caught))))

(defun ende-tool-call (name arguments &optional root access-config)
  "Invoke synchronous tool NAME with ARGUMENTS within project ROOT."
  (let ((tool (ende-tool--find name)))
    (if (null tool)
        (list :ok nil :tool (format "%s" name)
              :error (format "unknown ende tool: %s" name))
      (condition-case caught
          (let* ((tool-name (ende-tool-name tool))
                 (root (ende-tool--call-root tool root access-config))
                 (ende-tools--access-config access-config)
                 (ende-tools--access-tool tool-name))
            (when (ende-tool-async tool)
              (if (null access-config)
                  (error "URL is not authorized by this chat's URL resources")
                (error "tool %s requires asynchronous dispatch" tool-name)))
            (list :ok t :tool (symbol-name tool-name)
                  :result (funcall (ende-tool-handler tool) arguments root)))
        ((ende-tool-mutation-failure error quit)
         (ende-tool--failure-result tool caught))))))

(defun ende-tool-call-async (name arguments root access-config callback)
  (let ((tool (ende-tool--find name)))
    (if (null tool)
        (funcall callback
                 (list :ok nil :tool (format "%s" name)
                       :error (format "unknown ende tool: %s" name)))
      (if (not (ende-tool-async tool))
          (funcall callback (ende-tool-call name arguments root access-config))
        (let ((settled nil))
          (condition-case caught
              (let* ((tool-name (ende-tool-name tool))
                     (call-root (ende-tool--call-root
                                 tool root access-config))
                     (ende-tools--access-config access-config)
                     (ende-tools--access-tool tool-name))
                (funcall
                 (ende-tool-handler tool) arguments call-root
                 (lambda (value error)
                   (unless settled
                     (setq settled t)
                     (funcall
                      callback
                      (if error
                          (ende-tool--failure-result
                           tool (list 'error (format "%s" error)))
                        (list :ok t :tool (symbol-name tool-name)
                              :result value)))))))
            ((ende-tool-mutation-failure error quit)
             (unless settled
               (setq settled t)
               (funcall callback (ende-tool--failure-result tool caught))))))))))

(defun ende-tools--resource-allows-p (relative resource)
  "Return non-nil when RESOURCE permits root-relative path RELATIVE."
  (and (cl-some (lambda (regexp) (string-match-p regexp relative))
                (plist-get resource :include-regexps))
       (not (cl-some (lambda (regexp) (string-match-p regexp relative))
                     (plist-get resource :exclude-regexps)))))

(defun ende-tools--file-resource-root (resource project-root)
  "Return RESOURCE's fixed root, or PROJECT-ROOT when it has none."
  (or (plist-get resource :root) project-root))

(defun ende-tools--path-scopes-for
    (tool path project-root &optional no-follow-final project-only)
  "Return matching file scopes for TOOL and PATH under PROJECT-ROOT."
  (let* ((project-root (ende-project-normalize-root project-root))
         (expanded (and (stringp path) (not (file-remote-p path))
                        (expand-file-name path project-root)))
         (canonical
          (and expanded
               (condition-case nil
                   (cond
                    (no-follow-final
                     (let ((parent (file-name-directory expanded)))
                       (when (file-directory-p parent)
                         (expand-file-name (file-name-nondirectory expanded)
                                           (file-truename parent)))))
                    ((file-exists-p expanded) (file-truename expanded))
                    (t expanded))
                 (error nil))))
         scopes)
    (when canonical
      (if (null ende-tools--access-config)
          (when (file-in-directory-p canonical project-root)
            (let ((relative (file-relative-name canonical project-root)))
              (unless (ende-project--excluded-p relative)
                (push (list :root project-root :relative relative
                            :public-path relative :fixed-root nil)
                      scopes))))
        (let* ((assignments (plist-get ende-tools--access-config :tool-resources))
               (names (cdr (assq tool assignments)))
               (resources (plist-get ende-tools--access-config :resources)))
          (dolist (name names)
            (let* ((resource (cdr (assq name resources)))
                   (fixed-root (and resource (plist-get resource :root)))
                   (effective-root
                    (and resource (not (and project-only fixed-root))
                         (ende-tools--file-resource-root resource project-root)))
                   (effective-root
                    (and effective-root
                         (condition-case nil
                             (ende-project-normalize-root effective-root)
                           (error nil)))))
              (when (and resource
                         (eq (plist-get resource :type) 'file)
                         effective-root
                         (file-in-directory-p canonical effective-root))
                (let ((relative (file-relative-name canonical effective-root)))
                  (when (and (not (ende-project--excluded-p relative))
                             (ende-tools--resource-allows-p relative resource))
                    (push (list :root effective-root :relative relative
                                :public-path
                                (if fixed-root
                                    canonical
                                  (file-relative-name canonical project-root))
                                :fixed-root fixed-root)
                          scopes)))))))))
    (nreverse scopes)))

(defun ende-tools--path-scope-for
    (tool path root &optional no-follow-final project-only)
  "Return one matching authorization scope for TOOL and PATH below ROOT."
  (car (ende-tools--path-scopes-for
        tool path root no-follow-final project-only)))

(defun ende-tools--path-authorized-for-p
    (tool path root &optional no-follow-final)
  "Return non-nil when PATH is authorized for TOOL below project ROOT.
When NO-FOLLOW-FINAL is non-nil, PATH's final component is not followed."
  (and (ende-tools--path-scope-for
        tool path root no-follow-final)
       t))

(defun ende-tools--path-authorized-p (path root)
  "Return non-nil when canonical PATH is allowed by the current tool policy."
  (ende-tools--path-authorized-for-p ende-tools--access-tool path root))

(defun ende-tools--authorized-project-files (tool root)
  "Return project files authorized for TOOL below ROOT."
  (let ((root (ende-project-normalize-root root)))
    (cl-remove-if-not
     (lambda (path)
       (ende-tools--path-scope-for tool path root nil t))
     (ende-project-files root))))

(defun ende-tools--assert-path-authorized-for
    (tool path root &optional no-follow-final)
  "Signal unless PATH is allowed for TOOL in ROOT."
  (unless (ende-tools--path-authorized-for-p
           tool path root no-follow-final)
    (error "path is not authorized for %s by this chat's resources: %s"
           tool path))
  path)

(defun ende-tools--assert-path-authorized (path root)
  "Signal unless PATH is allowed for the current tool in ROOT."
  (ende-tools--assert-path-authorized-for
   ende-tools--access-tool path root))

(defun ende-tools--url-resource-allows-p (url resource)
  "Return non-nil when URL is included and not excluded by RESOURCE."
  (and (cl-some (lambda (pattern)
                  (ende-url-pattern-allows-p pattern url))
                (plist-get resource :include-url-patterns))
       (not (cl-some (lambda (pattern)
                       (ende-url-pattern-allows-p pattern url))
                     (plist-get resource :exclude-url-patterns)))))

(defun ende-tools--url-authorized-p (url)
  "Return non-nil when current tool's frozen URL grants permit URL."
  (if (null ende-tools--access-config)
      nil
    (let* ((assignments (plist-get ende-tools--access-config :tool-resources))
           (resource-names (cdr (assq ende-tools--access-tool assignments)))
           (resources (plist-get ende-tools--access-config :resources)))
      (cl-some (lambda (name)
                 (let ((resource (cdr (assq name resources))))
                   (and resource
                        (eq (plist-get resource :type) 'url)
                        (ende-tools--url-resource-allows-p url resource))))
               resource-names))))

(defun ende-tools--web-url-permission (url)
  "Return URL policy details for normalized URL, or nil when denied."
  (let* ((normalized (if (plist-get url :url) url (ende-url-normalize url)))
         (allowed (ende-tools--url-authorized-p normalized)))
    (when allowed
      (let* ((assignments (plist-get ende-tools--access-config :tool-resources))
             (resource-names (cdr (assq ende-tools--access-tool assignments)))
             (resources (plist-get ende-tools--access-config :resources))
             (private
              (cl-some
               (lambda (name)
                 (let ((resource (cdr (assq name resources))))
                   (and resource
                        (eq (plist-get resource :type) 'url)
                        (eq (plist-get resource :allow-private-addresses) t)
                        (ende-tools--url-resource-allows-p normalized resource))))
               resource-names)))
        (list :authorized t :allow-private-addresses private)))))

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
`:buffer'.  This resolver is for buffer-oriented access; path-based file tools
use `ende-tools--resolve-disk-source' instead."
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

(defun ende-tools--resolve-disk-source (arguments root)
  "Resolve an authorized file in ARGUMENTS below project ROOT.

The returned source always reads from disk, even when a visiting buffer
exists.  Visiting-buffer state is retained only as separate metadata."
  (let* ((path-value (ende-tools--required-string arguments "path"))
         (_ (when (file-remote-p path-value)
              (error "remote/TRAMP paths are not supported")))
         (_ (when (and (file-name-absolute-p path-value)
                       (null ende-tools--access-config))
              (error "absolute paths require an explicit file resource grant")))
         (candidate (if (file-name-absolute-p path-value)
                        (expand-file-name path-value)
                      (expand-file-name path-value root)))
         (_ (when (and (not (file-name-absolute-p path-value))
                       (not (file-in-directory-p candidate root)))
              (error "relative path is outside the project: %s" path-value)))
         (path (if (file-exists-p candidate)
                   (file-truename candidate)
                 candidate))
         (scope (ende-tools--path-scope-for
                 ende-tools--access-tool path root))
         (_ (unless scope
              (error "path is not authorized for %s by this chat's resources: %s"
                     ende-tools--access-tool path)))
         (relative (plist-get scope :relative)))
    (ende-project-assert-readable-file path)
    (list :kind 'file :path path :root (plist-get scope :root)
          :relative relative :public-path (plist-get scope :public-path)
          :visiting-buffer
          (car (ende-tools--visiting-buffers-for-path path)))))


(defun ende-tools--source-kind-name (source)
  "Return the public source-kind name for SOURCE."
  (if (eq (plist-get source :kind) 'buffer) "buffer" "file"))

(defun ende-tools--source-visiting-buffer (source)
  "Return the buffer associated with SOURCE, if any."
  (if (eq (plist-get source :kind) 'buffer)
      (plist-get source :buffer)
    (plist-get source :visiting-buffer)))

(defun ende-tools--source-buffer-metadata (source)
  "Return visiting-buffer status for SOURCE without using its contents."
  (let* ((buffer (ende-tools--source-visiting-buffer source))
         (modified (and buffer
                        (with-current-buffer buffer
                          (buffer-modified-p)))))
    (if (eq (plist-get source :kind) 'buffer)
        (list :buffer_id (and buffer (ende-project-buffer-id buffer))
              :buffer_modified modified)
      (list :visiting_buffer (and buffer t)
            :visiting_buffer_modified modified))))

(defun ende-tools--with-disk-source (source function)
  "Call FUNCTION with detected coding system in a temporary disk buffer.

SOURCE must be a disk source.  The temporary buffer does not visit the file or
activate a major mode."
  (let ((path (plist-get source :path)))
    (ende-project-assert-readable-file path)
    (with-temp-buffer
      (let ((coding-system-for-read 'undecided))
        (insert-file-contents path))
      (funcall function (or last-coding-system-used 'utf-8-unix)))))

(defun ende-tools--with-source (source function)
  "Call FUNCTION with current source buffer selected for SOURCE.

For a disk source, a temporary buffer is used and the file is not visited.
FUNCTION receives no arguments and may return any value."
  (if (eq (plist-get source :kind) 'buffer)
      (let ((buffer (plist-get source :buffer)))
        (unless (buffer-live-p buffer)
          (error "project buffer is no longer available"))
        (with-current-buffer buffer
          (save-restriction
            (widen)
            (funcall function))))
    (ende-tools--with-disk-source source
				  (lambda (_coding-system)
				    (funcall function)))))

(defun ende-tools--source-digest ()
  "Return a SHA-256 digest of the complete current source buffer."
  (secure-hash 'sha256 (current-buffer) (point-min) (point-max)))

(defun ende-tools--source-line-count ()
  "Return the number of lines in the complete current source buffer."
  (count-lines (point-min) (point-max)))

(defun ende-tools--source-metadata (source)
  "Return metadata for SOURCE using its current contents."
  (let* ((path (plist-get source :path))
         (disk-source (eq (plist-get source :kind) 'file))
         (attributes (and disk-source (file-attributes path)))
         (buffer-metadata (ende-tools--source-buffer-metadata source)))
    (ende-tools--with-source
     source
     (lambda ()
       (append
        (list :path (or (plist-get source :public-path)
                        (plist-get source :relative))
              :source_kind (ende-tools--source-kind-name source)
              :line_count (ende-tools--source-line-count)
              :byte_count (if disk-source
                              (file-attribute-size attributes)
                            (string-bytes (buffer-string)))
              :digest (ende-tools--source-digest)
              :disk_mtime
              (and attributes
                   (format-time-string
                    "%Y-%m-%dT%H:%M:%S%z"
                    (file-attribute-modification-time attributes))))
        buffer-metadata)))))

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
         (source (ende-tools--resolve-disk-source
                  (list :path relative) root))
         (attributes (file-attributes path))
         (record (append (list :path relative :source_kind "file"
                               :byte_count (file-attribute-size attributes))
                         (ende-tools--source-buffer-metadata source))))
    (if include-metadata
        (ende-tools--source-metadata source)
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
             (or (null glob-regexp)
                 (string-match-p
                  glob-regexp (file-relative-name path root))))
           (ende-tools--authorized-project-files 'list_project_files root)))
         (truncated (> (length files) maximum)))
    (list :files
          (vconcat
           (mapcar (lambda (path)
                     (ende-tools--file-record path root include-metadata))
                   (cl-subseq files 0 (min maximum (length files)))))
          :truncated truncated)))

(defun ende-tools--file-info (arguments root)
  "Return disk-derived metadata and separate visiting-buffer state."
  (let* ((path-value (ende-tools--required-string arguments "path"))
         (source (ende-tools--resolve-disk-source
                  (list :path path-value) root))
         (attributes (file-attributes (plist-get source :path)))
         (metadata (ende-tools--source-metadata source)))
    (plist-put metadata :disk_byte_count (file-attribute-size attributes))))

(defun ende-tools--read-file (arguments root)
  "Read a bounded range from disk, regardless of visiting-buffer contents."
  (let* ((source (ende-tools--resolve-disk-source arguments root))
         (start-line (ende-tools--bounded-integer
                      arguments "start_line" 1 most-positive-fixnum))
         (line-count (ende-tools--bounded-integer
                      arguments "line_count" ende-tools-max-read-lines
                      ende-tools-max-read-lines)))
    (ende-tools--with-source
     source
     (lambda ()
       (append (list :path (or (plist-get source :public-path)
                               (plist-get source :relative))
                     :source_kind "file"
                     :digest (ende-tools--source-digest))
               (ende-tools--source-buffer-metadata source)
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
        (vconcat
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
                      (or (buffer-file-name right) ""))))))))

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
    (source regexp context-before context-after maximum accept-match)
  "Search SOURCE for REGEXP, accepting at most MAXIMUM matches.

ACCEPT-MATCH is called for each match record and must return non-nil to keep it.
It is called with nil instead of a record when one source line or context block
is already too large to consider within the configured output byte limit."
  (let ((matches nil)
        (finished nil)
        (budget-exhausted nil))
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
                (line-start (save-excursion
                              (goto-char match-start)
                              (line-beginning-position)))
                (line-end (save-excursion
                            (goto-char match-start)
                            (line-end-position)))
                (context-start
                 (and (or (> context-before 0) (> context-after 0))
                      (save-excursion
                        (goto-char match-start)
                        (beginning-of-line)
                        (forward-line (- context-before))
                        (point))))
                (context-end
                 (and context-start
                      (save-excursion
                        (goto-char match-start)
                        (forward-line context-after)
                        (line-end-position)))))
           (if (or (> (- line-end line-start)
                      ende-tools-max-search-output-bytes)
                   (and context-start
                        (> (- context-end context-start)
                           ende-tools-max-search-output-bytes)))
               (unless (funcall accept-match nil)
                 (setq finished t
                       budget-exhausted t))
             (let ((record
                    (append
                     (list :line line :column column
                           :match (buffer-substring-no-properties
                                   line-start line-end))
                     (and context-start
                          (list :context
                                (buffer-substring-no-properties
                                 context-start context-end))))))
               (if (funcall accept-match record)
                   (push record matches)
                 (setq finished t
                       budget-exhausted t))))
           ;; Ensure a zero-width regexp cannot keep the search at one point.
           (when (and (not finished) (= (point) (match-beginning 0)))
             (if (= (point) (point-max))
                 (setq finished t)
               (forward-char 1)))))))
    (list :matches (nreverse matches) :budget_exhausted budget-exhausted)))

(defun ende-tools--search-json-bytes (value)
  "Return serialized byte length of search result VALUE."
  (string-bytes (ende-tools--json-encode-tool-result value)))

(defun ende-tools--search-regexp (arguments)
  "Normalize one search form from ARGUMENTS into an Emacs regexp."
  (let* ((forms '("query" "patterns" "regexp"))
         (keys (mapcar #'ende-tools--property-name
                       (ende-tools--object-keys arguments)))
         (present (cl-remove-if-not (lambda (name) (member name keys)) forms)))
    (unless (= (length present) 1)
      (error "search_project requires exactly one of query, patterns, or regexp"))
    (pcase (car present)
      ("query"
       (regexp-quote (ende-tools--required-string arguments "query")))
      ("patterns"
       (let* ((raw (ende-tools--argument arguments "patterns"))
              (patterns (ende-tools--sequence raw)))
         (unless (and (vectorp raw) (not (stringp raw))
                      patterns
                      (cl-every (lambda (pattern)
                                  (and (stringp pattern)
                                       (not (string-empty-p pattern))))
                                patterns))
           (error "patterns must be a non-empty array of non-empty strings"))
         (mapconcat #'regexp-quote patterns "\\|")))
      ("regexp"
       (ende-tools--required-string arguments "regexp")))))

(defun ende-tools--search-project (arguments root)
  "Search allowed project files on disk with an Emacs regexp."
  (let* ((regexp (ende-tools--search-regexp arguments))
         (glob (ende-tools--argument arguments "glob"))
         (context-before
          (ende-tools--bounded-integer
           arguments "context_before" 0
           ende-tools-max-search-context-lines 0))
         (context-after
          (ende-tools--bounded-integer
           arguments "context_after" 0
           ende-tools-max-search-context-lines 0))
         (maximum (ende-tools--bounded-integer
                   arguments "max_results" (min 30 ende-tools-max-search-results)
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
         (files-result nil)
         (files-result-count 0)
         (files-array-bytes 2)
         (envelope-bytes
          (- (ende-tools--search-json-bytes
              (list :files []
                    :files_inspected
                    (min ende-tools-max-search-files (length files))
                    :truncated :json-false))
             2))
         (matches-count 0)
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
               (source (ende-tools--resolve-disk-source
                        (list :path relative) root))
               (file-metadata
                (append (list :path relative
                              :source_kind
                              (ende-tools--source-kind-name source))
                        (ende-tools--source-buffer-metadata source)))
               (file-base-bytes nil)
               (file-match-count 0)
               (file-bytes 0)
               (search-result
                (ende-tools--search-source
                 source regexp context-before context-after
                 (- maximum matches-count)
                 (lambda (record)
                   (if (null record)
                       nil
                     (let* ((record-bytes
                             (ende-tools--search-json-bytes record))
                            (first-match (= file-match-count 0))
                            (base-delta
                             (if first-match
                                 (progn
                                   (unless file-base-bytes
                                     (setq file-base-bytes
                                           (ende-tools--search-json-bytes
                                            (append file-metadata
                                                    (list :matches [])))))
                                   (+ file-base-bytes record-bytes))
                               (1+ record-bytes)))
                            (candidate-file-bytes
                             (+ file-bytes base-delta))
                            (candidate-bytes
                             (+ envelope-bytes files-array-bytes
                                (if (and first-match
                                         (> files-result-count 0))
                                    1 0)
                                candidate-file-bytes)))
                       (when (<= candidate-bytes
                                 ende-tools-max-search-output-bytes)
                         (setq file-bytes candidate-file-bytes)
                         (cl-incf file-match-count)
                         t))))))
               (file-matches (plist-get search-result :matches)))
          (when file-matches
            (push (append file-metadata
                          (list :matches (vconcat file-matches)))
                  files-result)
            (cl-incf files-array-bytes
                     (+ file-bytes (if (> files-result-count 0) 1 0)))
            (cl-incf files-result-count)
            (cl-incf matches-count (length file-matches)))
          (when (plist-get search-result :budget_exhausted)
            (setq truncated t)
            (throw 'finished nil))
          (when (>= matches-count maximum)
            (setq truncated t)
            (throw 'finished nil))))
      (when (> (length files) ende-tools-max-search-files)
        (setq truncated t)))
    (let ((result (list :files (vconcat (nreverse files-result))
                        :files_inspected files-inspected
                        :truncated truncated)))
      ;; Keep a final guard around the incremental accounting above.
      (when (> (ende-tools--search-json-bytes result)
               ende-tools-max-search-output-bytes)
        (error "search_project result exceeded its configured output byte limit"))
      result)))

;;;; Change sets and writes

(cl-defstruct (ende-change-set (:constructor ende-change-set--create))
  "A validated disk mutation, prepared separately from its commit."
  operation root project-root source-kind path public-path base-digest anchor-edits edits
  content absolute-path expected-exists original-content original-bytes
  raw-digest coding-system file-mode identity prepared-coding-system
  strict-path-policy strict-buffer-policy authorization-tools visiting-buffers
  recovery-path stage-path committed-identity prepared-raw-digest state)

(defvar ende-tools--mutation-locks (make-hash-table :test #'equal)
  "Project roots currently locked by an Ende disk mutation.")

(defun ende-tools--file-identity (attributes)
  "Return stable identity and mutable preconditions from file ATTRIBUTES."
  (list (file-attribute-file-identifier attributes)
        (file-attribute-size attributes)
        (file-attribute-modification-time attributes)
        (file-attribute-status-change-time attributes)
        (file-attribute-modes attributes)))

(defun ende-tools--literal-file-bytes (path)
  "Return PATH contents as uninterpreted unibyte data."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defun ende-tools--decoded-file (path)
  "Return decoded disk contents and their coding system for PATH."
  (with-temp-buffer
    (let ((coding-system-for-read 'undecided))
      (insert-file-contents path))
    (list :content (buffer-substring-no-properties (point-min) (point-max))
          :coding-system (or last-coding-system-used 'utf-8-unix))))

(defun ende-tools--public-content-digest (content)
  "Return CONTENT's digest in the same representation as read_file."
  (with-temp-buffer
    (insert content)
    (ende-tools--source-digest)))

(defun ende-tools--capture-file (path)
  "Capture PATH's bytes, decoded text, coding, identity, and mode consistently."
  (when (file-symlink-p path)
    (error "final-component symlinks are not allowed: %s" path))
  (unless (and (file-regular-p path) (file-readable-p path))
    (error "mutation source is not a readable regular file: %s" path))
  (when (and ende-project-max-file-bytes
             (> (file-attribute-size (file-attributes path))
                ende-project-max-file-bytes))
    (error "project file is larger than the tool limit: %s" path))
  (let* ((before (file-attributes path))
         (before-identity (ende-tools--file-identity before))
         (bytes (ende-tools--literal-file-bytes path))
         (decoded (ende-tools--decoded-file path))
         (after (file-attributes path))
         (after-bytes (ende-tools--literal-file-bytes path)))
    (unless (and after
                 (equal before-identity (ende-tools--file-identity after))
                 (string= (secure-hash 'sha256 bytes)
                          (secure-hash 'sha256 after-bytes)))
      (error "source changed while preparing mutation: %s" path))
    (list :content (plist-get decoded :content)
          :bytes bytes
          :raw-digest (secure-hash 'sha256 bytes)
          :digest (ende-tools--public-content-digest
                   (plist-get decoded :content))
          :coding-system (plist-get decoded :coding-system)
          :file-mode (file-modes path)
          :identity before-identity)))

(defun ende-tools--strict-relative-path (path)
  "Validate relative literal mutation PATH and return it unchanged."
  (unless (and (stringp path) (not (string-empty-p path)))
    (error "mutation path must be a non-empty relative string"))
  (when (or (file-name-absolute-p path)
            (file-remote-p path)
            (string-match-p "[[:cntrl:]\\\\]" path))
    (error "mutation path must be local: %s" path))
  (when (cl-some (lambda (component)
                   (or (string-empty-p component)
                       (member component '("." ".."))))
                 (split-string path "/" nil))
    (error "mutation path must not contain empty, . or .. components: %s" path))
  path)

(defun ende-tools--strict-absolute-path (path)
  "Validate absolute local mutation PATH and return it unchanged."
  (unless (and (stringp path) (file-name-absolute-p path)
               (not (file-remote-p path))
               (not (string-match-p "[[:cntrl:]\\\\]" path))
               (not (member ".." (split-string path "/" t)))
               (not (member "." (split-string path "/" t))))
    (error "mutation path must be an absolute local path without . or ..: %s"
           path))
  path)

(defun ende-tools--resolve-mutation-path (path root &optional strict)
  "Resolve PATH through its existing parent, optionally below scope ROOT.
The final component is deliberately not followed through a symlink."
  (unless (and (stringp path) (not (string-empty-p path))
               (not (file-remote-p path))
               (not (string-match-p "[[:cntrl:]\\\\]" path)))
    (error "mutation target must be a non-empty local path"))
  (if (file-name-absolute-p path)
      (when strict (ende-tools--strict-absolute-path path))
    (when strict (ende-tools--strict-relative-path path)))
  (let* ((root (and root (ende-project-normalize-root root)))
         (expanded (expand-file-name path (or root default-directory)))
         (parent (file-name-directory expanded))
         (name (file-name-nondirectory expanded)))
    (when (file-remote-p expanded)
      (error "remote mutation paths are not supported"))
    (unless (file-directory-p parent)
      (error "parent directory does not exist: %s"
             (file-name-directory path)))
    (let* ((canonical-parent (file-truename parent))
           (canonical (expand-file-name name canonical-parent))
           (relative (and root (file-relative-name canonical root))))
      ;; Inspect the link itself before containment helpers can resolve its
      ;; final component.
      (when (file-symlink-p canonical)
        (error "final-component symlinks are not allowed: %s" path))
      (when (and root (not (file-in-directory-p canonical root)))
        (error "path is outside the authorized resource root: %s" path))
      (when (and relative (ende-project--excluded-p relative))
        (error "path is excluded from tool access: %s" path))
      (list :path canonical :relative relative :root root))))

(defun ende-tools--authorized-mutation-target (path project-root tools)
  "Safely resolve PATH and find a scope for the first TOOL.
Every TOOL must independently authorize the canonical target.
Return canonical target and root-relative/public path details."
  (when (and (file-name-absolute-p path)
             (null ende-tools--access-config))
    (error "absolute paths require an explicit file resource grant"))
  (let* ((project-root (ende-project-normalize-root project-root))
         (candidate
          (ende-tools--resolve-mutation-path
           path (unless (file-name-absolute-p path) project-root) t))
         (canonical (plist-get candidate :path))
         (scope-lists
          (mapcar (lambda (tool)
                    (ende-tools--path-scopes-for tool canonical project-root t))
                  tools))
         (scope (caar scope-lists)))
    (unless (and scope (cl-every #'identity scope-lists))
      (error "path is not authorized for %s by this chat's resources: %s"
             (mapconcat #'symbol-name tools ", ") canonical))
    (let* ((resolved (ende-tools--resolve-mutation-path
                      canonical (plist-get scope :root) t)))
      (append resolved
              (list :project-root project-root
                    :public-path (plist-get scope :public-path)
                    :fixed-root (plist-get scope :fixed-root))))))

(defun ende-tools--visiting-buffers-for-path (path)
  "Return every visiting buffer that names PATH, including missing files."
  (cl-remove-if-not
   (lambda (buffer)
     (let ((buffer-path (and (buffer-live-p buffer)
                             (buffer-file-name buffer))))
       (when (and buffer-path (not (file-remote-p buffer-path)))
         (let ((candidate
                (condition-case nil
                    (if (file-exists-p buffer-path)
                        (file-truename buffer-path)
                      (expand-file-name buffer-path))
                  (error nil))))
           (and candidate (file-equal-p candidate path))))))
   (buffer-list)))

(defun ende-tools--modified-buffers (buffers)
  "Return modified live buffers among BUFFERS."
  (cl-remove-if-not
   (lambda (buffer)
     (and (buffer-live-p buffer)
          (with-current-buffer buffer (buffer-modified-p))))
   buffers))

(defun ende-tools--mutation-existing-p (change-set)
  "Return non-nil when CHANGE-SET expects an existing source."
  (eq (ende-change-set-expected-exists change-set) t))

(defun ende-tools--mutation-assert-authorized (change-set path)
  "Reauthorize CHANGE-SET's complete operation scope at PATH."
  (let ((tools (ende-change-set-authorization-tools change-set))
        (root (ende-project-normalize-root
               (ende-change-set-root change-set)))
        (project-root (or (ende-change-set-project-root change-set)
                          (ende-change-set-root change-set))))
    (dolist (tool tools)
      (let ((scopes (ende-tools--path-scopes-for
                     tool path project-root t)))
        (unless (and scopes
                     (or (not (eq tool (car tools)))
                         (cl-some
                          (lambda (scope)
                            (equal root
                                   (ende-project-normalize-root
                                    (plist-get scope :root))))
                          scopes)))
          (error "path is no longer authorized for %s: %s" tool path))))))

(defun ende-tools--validate-change-set (change-set)
  "Revalidate target identity, permissions and buffer policy for CHANGE-SET."
  (let* ((root (ende-change-set-root change-set))
         (resolved (ende-tools--resolve-mutation-path
                    (ende-change-set-path change-set) root
                    (ende-change-set-strict-path-policy change-set)))
         (path (plist-get resolved :path))
         (exists (or (file-exists-p path) (file-symlink-p path))))
    (unless (equal path (ende-change-set-absolute-path change-set))
      (error "mutation target resolved differently since preparation: %s"
             (ende-change-set-path change-set)))
    (ende-tools--mutation-assert-authorized change-set path)
    (unless (eq (not (null exists))
		(ende-tools--mutation-existing-p change-set))
      (error (if exists
                 "file appeared after mutation preparation: %s"
               "file disappeared after mutation preparation: %s")
             (ende-change-set-path change-set)))
    (when (ende-tools--mutation-existing-p change-set)
      (let ((current (ende-tools--capture-file path)))
        (unless (and (equal (plist-get current :identity)
                            (ende-change-set-identity change-set))
                     (equal (plist-get current :raw-digest)
                            (ende-change-set-raw-digest change-set)))
          (error "source changed since mutation preparation: %s"
                 (ende-change-set-path change-set)))))
    (let ((buffers (ende-tools--visiting-buffers-for-path path)))
      (when (and (ende-change-set-strict-buffer-policy change-set)
                 (ende-tools--modified-buffers buffers))
        (error "unsaved visiting buffer prevents mutation of %s"
               (ende-change-set-path change-set)))
      (setf (ende-change-set-visiting-buffers change-set) buffers))
    path))

(defun ende-tools--validate-change-sets (change-sets)
  "Validate the complete CHANGE-SETS batch before commit."
  (dolist (change-set change-sets)
    (ende-tools--validate-change-set change-set)))

(defun ende-tools--set-snapshot (change-set snapshot)
  "Store file SNAPSHOT preconditions and source details in CHANGE-SET."
  (setf (ende-change-set-original-content change-set)
        (plist-get snapshot :content)
        (ende-change-set-original-bytes change-set) (plist-get snapshot :bytes)
        (ende-change-set-raw-digest change-set)
        (plist-get snapshot :raw-digest)
        (ende-change-set-base-digest change-set) (plist-get snapshot :digest)
        (ende-change-set-coding-system change-set)
        (plist-get snapshot :coding-system)
        (ende-change-set-file-mode change-set) (plist-get snapshot :file-mode)
        (ende-change-set-identity change-set) (plist-get snapshot :identity))
  change-set)

(defun ende-tools--change-set-size (change-set)
  "Estimate original and prepared bytes held by CHANGE-SET."
  (+ (string-bytes (or (ende-change-set-original-bytes change-set) ""))
     (string-bytes (or (ende-change-set-content change-set) ""))))

(defun ende-tools--assert-change-set-limits (change-sets)
  "Reject excessive operation count or cumulative prepared data."
  (when (> (length change-sets) ende-tools-max-mutation-operations)
    (error "mutation batch contains too many file operations (maximum %d)"
           ende-tools-max-mutation-operations))
  (let ((total (cl-loop for change-set in change-sets
                        sum (ende-tools--change-set-size change-set))))
    (when (> total ende-tools-max-prepared-bytes)
      (error "mutation batch exceeds the %d-byte prepared-data limit"
             ende-tools-max-prepared-bytes))))

(defun ende-tools--assert-change-set-targets-unique (change-sets)
  "Reject duplicate canonical paths and hard-link aliases in CHANGE-SETS."
  (let ((paths (make-hash-table :test #'equal))
        (identities (make-hash-table :test #'equal)))
    (dolist (change-set change-sets)
      (let ((path (ende-change-set-absolute-path change-set)))
        (when (gethash path paths)
          (error "mutation batch targets %s more than once"
                 (ende-change-set-path change-set)))
        (puthash path t paths)
        (when (ende-tools--mutation-existing-p change-set)
          (let* ((attributes (file-attributes path))
                 (identity (and attributes
                                (file-attribute-file-identifier attributes))))
            (when (and identity (gethash identity identities))
              (error "mutation batch targets the same existing file through aliases: %s"
                     (ende-change-set-path change-set)))
            (when identity
              (puthash identity t identities))))))))

(defun ende-tools--write-private-bytes (path bytes)
  "Write raw BYTES to existing private PATH without encoding conversion."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert bytes)
    (let ((coding-system-for-write 'no-conversion)
          (inhibit-message t))
      (write-region (point-min) (point-max) path nil 'silent))))

(defun ende-tools--write-raw-exclusive (path bytes mode)
  "Exclusively create PATH from uninterpreted BYTES and restore MODE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert bytes)
    (let ((coding-system-for-write 'no-conversion)
          (inhibit-message t))
      (write-region (point-min) (point-max) path nil 'silent nil 'excl)))
  (when mode (set-file-modes path mode))
  path)

(defun ende-tools--write-change-set-stage (change-set)
  "Create a private, adjacent staged file for existing-file CHANGE-SET."
  (let* ((path (ende-change-set-absolute-path change-set))
         (directory (file-name-directory path))
         (stage (make-temp-file (expand-file-name ".ende-stage-" directory))))
    (set-file-modes stage #o600)
    (unwind-protect
        (progn
          (with-temp-buffer
            (when (eq (ende-change-set-prepared-coding-system change-set)
                      'no-conversion)
              (set-buffer-multibyte nil))
            (insert (ende-change-set-content change-set))
            (let ((coding-system-for-write
                   (or (ende-change-set-prepared-coding-system change-set)
                       (ende-change-set-coding-system change-set)
                       'utf-8-unix))
                  (require-final-newline nil)
                  (inhibit-message t))
              (write-region (point-min) (point-max) stage nil 'silent)))
          (setf (ende-change-set-stage-path change-set) stage
                (ende-change-set-prepared-raw-digest change-set)
                (secure-hash 'sha256 (ende-tools--literal-file-bytes stage))
                (ende-change-set-committed-identity change-set)
                (ende-tools--file-identity (file-attributes stage)))
          stage)
      (unless (equal stage (ende-change-set-stage-path change-set))
        (when (file-exists-p stage)
          (delete-file stage))))))

(defun ende-tools--write-change-set-recovery (change-set)
  "Create restrictive recovery material for existing CHANGE-SET."
  (let* ((path (ende-change-set-absolute-path change-set))
         (recovery (make-temp-file
                    (expand-file-name ".ende-recovery-"
                                      (file-name-directory path)))))
    (unwind-protect
        (progn
          (set-file-modes recovery #o600)
          (ende-tools--write-private-bytes
           recovery (ende-change-set-original-bytes change-set))
          (set-file-modes recovery #o600)
          (setf (ende-change-set-recovery-path change-set) recovery)
          recovery)
      (unless (equal recovery (ende-change-set-recovery-path change-set))
        (when (file-exists-p recovery)
          (delete-file recovery))))))

(defun ende-tools--prepare-change-set-resources (change-sets)
  "Create all private recovery and staging files for CHANGE-SETS."
  (dolist (change-set change-sets)
    (when (ende-tools--mutation-existing-p change-set)
      (ende-tools--write-change-set-recovery change-set)
      (unless (eq (ende-change-set-operation change-set) 'delete)
        (ende-tools--write-change-set-stage change-set)))))

(defun ende-tools--file-current-raw-digest (path)
  "Return PATH's raw digest, or nil when no regular file exists."
  (when (and (file-regular-p path) (not (file-symlink-p path)))
    (secure-hash 'sha256 (ende-tools--literal-file-bytes path))))

(defun ende-tools--file-current-identity (path)
  "Return PATH's stable file identifier, or nil if it cannot be read."
  (condition-case nil
      (and (file-regular-p path) (not (file-symlink-p path))
           (file-attribute-file-identifier (file-attributes path)))
    (error nil)))

(defun ende-tools--change-set-result-entry (change-set status)
  "Return a JSON-compatible per-file report for CHANGE-SET and STATUS."
  (list :operation (symbol-name (ende-change-set-operation change-set))
        :path (or (ende-change-set-public-path change-set)
                  (ende-change-set-path change-set))
        :status status))

(defun ende-tools--recovery-error-entry (change-set caught)
  "Return recovery status for CHANGE-SET after rollback condition CAUGHT."
  (append (ende-tools--change-set-result-entry change-set "recovery_needed")
          (list :recovery_error (error-message-string caught))))

(defun ende-tools--restore-change-set (change-set)
  "Best-effort rollback for one CHANGE-SET; return its actual-state report."
  (let* ((path (ende-change-set-absolute-path change-set))
         (exists (or (file-exists-p path) (file-symlink-p path)))
         (operation (ende-change-set-operation change-set))
         (current-digest (and exists
                              (ende-tools--file-current-raw-digest path)))
         (original-p (and exists
                          (equal current-digest
                                 (ende-change-set-raw-digest change-set))
                          (equal (ende-tools--file-current-identity path)
                                 (car (ende-change-set-identity change-set)))))
         (new-p (and exists
                     (equal current-digest
                            (ende-change-set-prepared-raw-digest change-set))
                     (equal (ende-tools--file-current-identity path)
                            (car (ende-change-set-committed-identity change-set)))))
         (entry (ende-tools--change-set-result-entry change-set "not_applied")))
    (condition-case caught
        (cond
         ((memq operation '(create add))
          (cond
           ((not exists) nil)
           (new-p
            (let ((delete-by-moving-to-trash nil))
              (delete-file path))
            (setq entry
                  (ende-tools--change-set-result-entry change-set "restored")))
           (t
            (setq entry
                  (ende-tools--change-set-result-entry
                   change-set "concurrent_change_preserved")))))
         ((eq operation 'delete)
          (cond
           (original-p nil)
           ((not exists)
            (let ((bytes (ende-tools--literal-file-bytes
                          (ende-change-set-recovery-path change-set))))
              (ende-tools--resolve-mutation-path
               (ende-change-set-path change-set)
               (ende-change-set-root change-set)
               (ende-change-set-strict-path-policy change-set))
              (ende-tools--write-raw-exclusive
               path bytes (ende-change-set-file-mode change-set)))
            (setq entry
                  (ende-tools--change-set-result-entry change-set "restored")))
           (t
            (setq entry
                  (ende-tools--change-set-result-entry
                   change-set "concurrent_change_preserved")))))
         (t
          (cond
           (original-p nil)
           (new-p
            (let* ((recovery (ende-change-set-recovery-path change-set))
                   (bytes (ende-tools--literal-file-bytes recovery))
                   (stage (make-temp-file
                           (expand-file-name
                            ".ende-restore-" (file-name-directory path)))))
              (unwind-protect
                  (progn
                    (set-file-modes stage #o600)
                    (ende-tools--write-private-bytes stage bytes)
                    (unless (equal current-digest
                                   (ende-tools--file-current-raw-digest path))
                      (error "target changed during rollback"))
                    (rename-file stage path t)
                    (when (ende-change-set-file-mode change-set)
                      (set-file-modes path
                                      (ende-change-set-file-mode change-set)))
                    (setq entry
                          (ende-tools--change-set-result-entry
                           change-set "restored")))
                (when (file-exists-p stage)
                  (delete-file stage)))))
           (t
            (setq entry
                  (ende-tools--change-set-result-entry
                   change-set "concurrent_change_preserved"))))))
      (error
       (setq entry (ende-tools--recovery-error-entry change-set caught)))
      (quit
       (setq entry (ende-tools--recovery-error-entry change-set caught))))
    (when (and (ende-change-set-recovery-path change-set)
               (not (equal (plist-get entry :status) "recovery_needed"))
               (not (equal (plist-get entry :status)
                           "concurrent_change_preserved")))
      (let ((recovery (ende-change-set-recovery-path change-set)))
        (when (file-exists-p recovery)
          (delete-file recovery))
        (setf (ende-change-set-recovery-path change-set) nil)))
    (when (ende-change-set-recovery-path change-set)
      (setq entry
            (append entry
                    (list :recovery_file
                          (file-relative-name
                           (ende-change-set-recovery-path change-set)
                           (ende-change-set-root change-set))))))
    entry))

(defun ende-tools--rollback-change-sets (change-sets)
  "Roll back attempted CHANGE-SETS in reverse order and return a report."
  (vconcat
   (mapcar (lambda (change-set)
             (if (memq (ende-change-set-state change-set) '(committing applied))
                 (ende-tools--restore-change-set change-set)
               (ende-tools--change-set-result-entry
                change-set "not_applied")))
           (reverse change-sets))))

(defun ende-tools--commit-change-set (change-set)
  "Apply one fully prepared CHANGE-SET after its final revalidation."
  (let* ((path (ende-tools--validate-change-set change-set))
         (operation (ende-change-set-operation change-set)))
    (setf (ende-change-set-state change-set) 'committing)
    (pcase operation
      ((or 'create 'add)
       (let* ((coding-system
               (or (ende-change-set-prepared-coding-system change-set)
                   (and (eq operation 'create)
                        (ende-change-set-coding-system change-set))
                   'utf-8-unix))
              (coding-system-for-write coding-system))
         (ende-tools--write-new-file (ende-change-set-content change-set)
                                     path))
       (setf (ende-change-set-prepared-raw-digest change-set)
             (ende-tools--file-current-raw-digest path)
             (ende-change-set-committed-identity change-set)
             (list (ende-tools--file-current-identity path))
             (ende-change-set-state change-set) 'applied))
      ('delete
       (if delete-by-moving-to-trash
           (move-file-to-trash path)
         (delete-file path))
       (when (or (file-exists-p path) (file-symlink-p path))
         (error "delete operation did not remove the target: %s"
                (ende-change-set-path change-set)))
       (setf (ende-change-set-state change-set) 'applied))
      (_
       (unless (ende-change-set-stage-path change-set)
         (error "missing staged contents for %s"
                (ende-change-set-path change-set)))
       (rename-file (ende-change-set-stage-path change-set) path t)
       (setf (ende-change-set-stage-path change-set) nil)
       (when (ende-change-set-file-mode change-set)
         (set-file-modes path (ende-change-set-file-mode change-set)))
       (setf (ende-change-set-state change-set) 'applied)))))

(defun ende-tools--cleanup-change-set-resources (change-sets keep-recovery)
  "Clean staging resources for CHANGE-SETS, retaining recovery if requested."
  (dolist (change-set change-sets)
    (let ((stage (ende-change-set-stage-path change-set))
          (recovery (ende-change-set-recovery-path change-set)))
      (when (and stage (file-exists-p stage))
        (condition-case nil (delete-file stage) (error nil)))
      (when (and recovery (not (member recovery keep-recovery))
                 (file-exists-p recovery))
        (condition-case nil (delete-file recovery) (error nil))))))

(defvar ende-tools--mutation-retained-recovery nil
  "Recovery files that must survive mutation cleanup.")

(defun ende-tools--signal-mutation-commit-failure
    (change-sets caught)
  "Roll back CHANGE-SETS and signal a structured failure for CAUGHT."
  (let ((report (ende-tools--rollback-change-sets change-sets)))
    (dolist (entry (append report nil))
      (when (plist-get entry :recovery_file)
        (let ((change-set
               (cl-find-if
                (lambda (candidate)
                  (equal (or (ende-change-set-public-path candidate)
                             (ende-change-set-path candidate))
                         (plist-get entry :path)))
                change-sets)))
          (push (expand-file-name
                 (plist-get entry :recovery_file)
                 (ende-change-set-root change-set))
                ende-tools--mutation-retained-recovery))))
    (signal 'ende-tool-mutation-failure
            (list (format "mutation commit failed: %s"
                          (error-message-string caught))
                  report))))

(defun ende-tools--sync-change-sets-to-sandbox (change-sets)
  "Mirror committed CHANGE-SETS that are authorized for `run_sandbox_command'."
  (let ((sandbox (and ende-tools--execution-owner
                      (ende-sandbox-for-owner
                       ende-tools--execution-owner))))
    (when (and sandbox (not (ende-sandbox-closing sandbox)))
      (condition-case nil
          (let ((syncs
                 (delq nil
                       (mapcar
                        (lambda (change-set)
                          (let ((scope
                                 (ende-tools--path-scope-for
                                  'run_sandbox_command
                                  (ende-change-set-absolute-path change-set)
                                  (ende-change-set-project-root change-set)
                                  t t)))
                            (and scope
                                 (cons change-set
                                       (plist-get scope :relative)))))
                        change-sets))))
            (when syncs
              (ende-sandbox-with-quiesced-workspace
               sandbox
               (lambda ()
                 (dolist (sync syncs)
                   (let ((change-set (car sync))
                         (relative (cdr sync)))
                     (pcase (ende-change-set-operation change-set)
                       ('delete
                        (ende-sandbox-delete-file sandbox relative))
                       ((or 'edit 'create 'add 'update)
                        (ende-sandbox-sync-file
                         sandbox (ende-change-set-absolute-path change-set)
                         relative)))))))))
        ((error quit)
          (condition-case nil
              (ende-sandbox-invalidate sandbox)
            ((error quit) nil)))))))

(defun ende-tools--commit-change-sets (change-sets)
  "Commit fully validated CHANGE-SETS with best-effort reverse rollback."
  (unless change-sets
    (error "mutation batch must contain at least one operation"))
  (let* ((roots
          (delete-dups
           (apply #'append
                  (mapcar
                   (lambda (change-set)
                     (delq nil
                           (mapcar
                            (lambda (root)
                              (and root (ende-project-normalize-root root)))
                            (list (ende-change-set-project-root change-set)
                                  (ende-change-set-root change-set)))))
                   change-sets))))
         (locked nil)
         (ende-tools--mutation-retained-recovery nil))
    (ende-tools--assert-change-set-limits change-sets)
    (ende-tools--assert-change-set-targets-unique change-sets)
    (ende-tools--validate-change-sets change-sets)
    (when (cl-some (lambda (root) (gethash root ende-tools--mutation-locks)) roots)
      (error "another Ende mutation is already committing in a target scope"))
    (dolist (root roots) (puthash root t ende-tools--mutation-locks))
    (setq locked roots)
    (unwind-protect
        (progn
          (ende-tools--prepare-change-set-resources change-sets)
          (ende-tools--validate-change-sets change-sets)
          (let ((result
                 (condition-case caught
                     (progn
                       (dolist (change-set change-sets)
                         (ende-tools--commit-change-set change-set))
                       (mapcar
                        (lambda (change-set)
                          (ende-tools--change-set-result-entry
                           change-set "applied"))
                        change-sets))
                   (error
                    (ende-tools--signal-mutation-commit-failure
                     change-sets caught))
                   (quit
                    (ende-tools--signal-mutation-commit-failure
                     change-sets caught)))))
            (ende-tools--sync-change-sets-to-sandbox change-sets)
            result))
      (unwind-protect
          (ende-tools--cleanup-change-set-resources
           change-sets ende-tools--mutation-retained-recovery)
        (dolist (root locked)
          (remhash root ende-tools--mutation-locks))))))

(defun ende-tools--sequence (value)
  "Return VALUE as a list when it is a JSON array or Lisp sequence."
  (cond
   ((and (vectorp value) (not (stringp value))) (append value nil))
   ((listp value) value)
   (t nil)))

(defun ende-tools--object-keys (object)
  "Return property names from JSON OBJECT, alist, or plist."
  (cond
   ((hash-table-p object)
    (let (keys)
      (maphash (lambda (key _value) (push key keys)) object)
      keys))
   ((and (listp object) (cl-every #'consp object))
    (mapcar #'car object))
   ((and (listp object) (zerop (% (length object) 2)))
    (cl-loop for (key _value) on object by #'cddr collect key))
   (t
    (error "tool argument must be an object"))))

(defun ende-tools--property-name (key)
  "Return KEY as a string property name."
  (cond
   ((stringp key) key)
   ((keywordp key) (substring (symbol-name key) 1))
   ((symbolp key) (symbol-name key))
   (t (format "%s" key))))

(defun ende-tools--assert-properties (object allowed context)
  "Signal when OBJECT has a property outside ALLOWED for CONTEXT."
  (dolist (key (ende-tools--object-keys object))
    (let ((name (ende-tools--property-name key)))
      (unless (member name allowed)
        (error "%s has unknown property %s" context name)))))

(defun ende-tools--json-object (object properties)
  "Copy PROPERTIES from OBJECT into a JSON-compatible hash table."
  (let ((keys (mapcar #'ende-tools--property-name
                      (ende-tools--object-keys object)))
        (copy (make-hash-table :test #'equal)))
    (dolist (name properties)
      (when (member name keys)
        (puthash name (ende-tools--argument object name) copy)))
    copy))

(defun ende-tools--anchor-edits (raw-edits)
  "Validate and copy RAW-EDITS into a vector of JSON-compatible objects."
  (let ((edits (ende-tools--sequence raw-edits)))
    (unless edits
      (error "edit_file requires a non-empty edits array"))
    (when (> (length edits) ende-tools-max-edits)
      (error "edit_file contains too many edits"))
    (vconcat
     (mapcar (lambda (edit)
               (let ((keys (mapcar #'ende-tools--property-name
                                   (ende-tools--object-keys edit))))
                 (ende-tools--assert-properties
                  edit '("anchor" "position" "text" "occurrence" "all")
                  "edit item")
                 (when (and (member "all" keys)
                            (not (eq (ende-tools--argument edit "all") t)))
                   (error "edit item property all must be true when present"))
                 (when (and (member "occurrence" keys)
                            (member "all" keys))
                   (error "edit item cannot specify both occurrence and all"))
                 (ende-tools--json-object
                  edit '("anchor" "position" "text" "occurrence" "all"))))
             edits))))

(defun ende-tools--normalize-anchor-edits (raw-edits source)
  "Resolve RAW-EDITS against SOURCE into validated change-set edits."
  (let* ((parsed (ende-edit-parse-tool-replacements
                  raw-edits source ende-tools-max-edits))
         (failures (plist-get parsed :errors)))
    (when failures
      (error "edit_file failed validation: %s"
             (ende-edit-format-failures failures)))
    (mapcar (lambda (edit)
              (let ((start (plist-get edit :start))
                    (end (plist-get edit :end)))
                (list :index (plist-get edit :index)
                      :start start
                      :end end
                      :expected (substring-no-properties source start end)
                      :replacement (plist-get edit :text)
                      :position (plist-get edit :position))))
            (plist-get parsed :edits))))

(defun ende-tools--change-set-result (change-set)
  "Return a JSON-compatible summary of the applied CHANGE-SET."
  (let* ((operation (or (ende-change-set-operation change-set) 'edit))
         (result (list :operation (symbol-name operation)
                       :status "applied"
                       :source_kind (ende-change-set-source-kind change-set)
                       :path (ende-change-set-path change-set))))
    (when (eq operation 'edit)
      (setq result
            (append result
                    (list :edit_count
                          (length (ende-change-set-edits change-set))))))
    result))

(defun ende-tools--edit-file (arguments root)
  "Apply validated anchor edits directly to an authorized file on disk."
  (ende-tools--assert-properties arguments '("path" "edits") "edit_file")
  (let* ((path (ende-tools--required-string arguments "path"))
         (anchor-edits (ende-tools--anchor-edits
                        (ende-tools--argument arguments "edits")))
         (source (ende-tools--resolve-disk-source
                  (list (cons "path" path)) root))
         (absolute-path (plist-get source :path))
         (snapshot (ende-tools--capture-file absolute-path))
         (content (plist-get snapshot :content))
         (edits (ende-tools--normalize-anchor-edits anchor-edits content))
         (result-content
          (with-temp-buffer
            (insert content)
            (ende-tools--apply-edits-in-buffer edits)
            (buffer-substring-no-properties (point-min) (point-max))))
         (change-set
          (ende-change-set--create
           :operation 'edit
           :root (plist-get source :root)
           :project-root root
           :source-kind "file"
           :path (plist-get source :public-path)
           :public-path (plist-get source :public-path)
           :absolute-path absolute-path
           :expected-exists t
           :anchor-edits anchor-edits
           :edits edits
           :content result-content
           :authorization-tools '(edit_file))))
    (ende-tools--set-snapshot change-set snapshot)
    (ende-tools--apply-change-set-object change-set)))

(defun ende-tools--create-file-path (path root)
  "Return the canonical authorized creation PATH below project ROOT."
  (let ((resolved (ende-tools--authorized-mutation-target
                   path root (list ende-tools--access-tool))))
    (plist-get resolved :path)))

(defun ende-tools--assert-new-file-path (path root)
  "Signal if PATH already names a filesystem entry."
  (when (or (file-exists-p path) (file-symlink-p path))
    (error "file already exists; use edit_file to modify it: %s"
           (file-relative-name path root)))
  path)

(defun ende-tools--write-new-file (content path &optional coding-system)
  "Write CONTENT to new PATH, failing if the filesystem entry exists."
  (let ((coding-system (or coding-system coding-system-for-write)))
    (with-temp-buffer
      (when (eq coding-system 'no-conversion)
        (set-buffer-multibyte nil))
      (insert content)
      (let ((coding-system-for-write coding-system))
        (write-region (point-min) (point-max) path nil 'silent nil 'excl)))))

(defun ende-tools--create-file (arguments root)
  "Create a new authorized file immediately when the chat permits it."
  (ende-tools--assert-properties arguments '("path" "content") "create_file")
  (let* ((path (ende-tools--required-string arguments "path"))
         (content (ende-tools--argument arguments "content")))
    (unless (stringp content)
      (error "tool argument content must be a string"))
    (let* ((target (ende-tools--authorized-mutation-target
                    path root '(create_file)))
           (canonical (plist-get target :path))
           (scope-root (plist-get target :root))
           (public-path (plist-get target :public-path)))
      (ende-tools--assert-new-file-path canonical scope-root)
      (ende-tools--apply-change-set-object
       (ende-change-set--create
        :operation 'create
        :root scope-root :project-root root
        :source-kind "file"
        :path public-path :public-path public-path
        :absolute-path canonical :strict-path-policy t
        :authorization-tools '(create_file)
        :content content)))))

(defun ende-tools--apply-edits-in-buffer (edits)
  "Apply normalized descending EDITS in the current buffer.

No visiting buffer or review state is involved in this operation."
  (dolist (edit (sort (copy-sequence edits)
                      (lambda (left right)
                        (let ((left-start (plist-get left :start))
                              (right-start (plist-get right :start)))
                          (or (> left-start right-start)
                              (and (= left-start right-start)
                                   (equal (plist-get left :position)
                                          "replace")
                                   (not (equal
                                         (plist-get right :position)
                                         "replace"))))))))
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
      (insert replacement)))
  t)

(defun ende-tools--current-change-set-source (change-set)
  "Resolve the current disk source represented by CHANGE-SET."
  (ende-tools--resolve-disk-source
   (list :path (ende-change-set-public-path change-set))
   (or (ende-change-set-project-root change-set)
       (ende-change-set-root change-set))))

(defun ende-tools--apply-create-change-set-object (change-set)
  "Apply new-file CHANGE-SET without replacing an existing filesystem entry."
  (let* ((root (ende-change-set-root change-set))
         (project-root (or (ende-change-set-project-root change-set) root))
         (target (ende-tools--authorized-mutation-target
                  (ende-change-set-public-path change-set) project-root
                  (ende-change-set-authorization-tools change-set)))
         (path (plist-get target :path)))
    (ende-tools--assert-new-file-path path root)
    (setf (ende-change-set-absolute-path change-set) path
          (ende-change-set-path change-set) (plist-get target :public-path)
          (ende-change-set-expected-exists change-set) nil)
    (ende-tools--commit-change-sets (list change-set))
    (ende-tools--change-set-result change-set)))

(defun ende-tools--apply-change-set-object (change-set)
  "Apply CHANGE-SET through its operation-specific validation path."
  (if (eq (ende-change-set-operation change-set) 'create)
      (ende-tools--apply-create-change-set-object change-set)
    (ende-tools--apply-edit-change-set-object change-set)))

(defun ende-tools--apply-edit-change-set-object (change-set)
  "Apply disk-file CHANGE-SET after revalidating its source and every edit."
  (let* ((application
          (progn
            (setf (ende-change-set-expected-exists change-set) t)
            (ende-tools--commit-change-sets (list change-set))
            (list :applied_edits
                  (length (ende-change-set-edits change-set)))))
         (visiting-buffer
          (car (ende-tools--visiting-buffers-for-path
                (ende-change-set-absolute-path change-set))))
         (visiting-modified
          (and visiting-buffer
               (with-current-buffer visiting-buffer
                 (buffer-modified-p))))
         (result
          (append (ende-tools--change-set-result change-set)
                  application
                  (list :write_target "disk_file"
                        :visiting_buffer (and visiting-buffer t)
                        :visiting_buffer_modified visiting-modified
                        :visiting_buffer_warning
                        (and visiting-buffer
                             (if visiting-modified
                                 (concat
                                  "The visiting buffer was left unchanged. "
                                  "It has unsaved changes; saving that old "
                                  "buffer later could overwrite this disk edit.")
                               (concat
                                "The visiting buffer was left unchanged and "
                                "may not reflect this disk edit.")))))))
    result))

(defun ende-tools--delete-file (arguments root)
  "Delete one authorized regular file without touching visiting buffers."
  (ende-tools--assert-properties
   arguments '("path" "expected_digest") "delete_file")
  (let* ((path (ende-tools--required-string arguments "path"))
         (keys (mapcar #'ende-tools--property-name
                       (ende-tools--object-keys arguments)))
         (expected (ende-tools--argument arguments "expected_digest")))
    (when (and (member "expected_digest" keys)
               (not (and (stringp expected)
                         (string-match-p "\\`[[:xdigit:]]\\{64\\}\\'"
                                         expected))))
      (error "expected_digest must be a SHA-256 hex digest"))
    (let* ((resolved (ende-tools--authorized-mutation-target
                      path root '(delete_file)))
           (absolute (plist-get resolved :path))
           (scope-root (plist-get resolved :root))
           (public-path (plist-get resolved :public-path)))
      ;; This deliberately authorizes the unresolved final component: the
      ;; resolver has already canonicalized its parent and rejected symlinks.
      (ende-tools--assert-path-authorized-for
       'delete_file absolute root t)
      (unless (or (file-exists-p absolute) (file-symlink-p absolute))
        (error "file does not exist: %s" public-path))
      (unless (file-regular-p absolute)
        (error "delete_file only removes regular files: %s" public-path))
      (let* ((snapshot (ende-tools--capture-file absolute))
             (change-set
              (ende-change-set--create
               :operation 'delete :root scope-root :project-root root
               :source-kind "file"
               :path public-path :public-path public-path
               :absolute-path absolute :expected-exists t
               :strict-path-policy t :strict-buffer-policy t
               :authorization-tools '(delete_file))))
        (when (and expected
                   (not (string-equal expected (plist-get snapshot :digest))))
          (error "expected_digest does not match the current file: %s" public-path))
        (ende-tools--set-snapshot change-set snapshot)
        (ende-tools--commit-change-sets (list change-set))
        (let* ((buffers (ende-change-set-visiting-buffers change-set))
               (present (and buffers t)))
          (list :operation "delete" :path public-path :status "applied"
                :visiting_buffer present
                :visiting_buffer_count (length buffers)
                :visiting_buffer_warning
                (and present
                     (concat "Clean visiting buffers were left unchanged. "
                             "Saving one later could recreate the deleted file."))))))))

(defun ende-tools--patch-change-set (operation root)
  "Create an unprepared change set for parsed patch OPERATION in ROOT."
  (let ((kind (plist-get operation :operation)))
    (ende-change-set--create
     :operation kind :root root :source-kind "file"
     :path (plist-get operation :path)
     :expected-exists (not (eq kind 'add))
     :strict-path-policy t :strict-buffer-policy t
     :authorization-tools '(apply_patch)
     :anchor-edits (plist-get operation :hunks)
     :content (plist-get operation :content)
     :prepared-coding-system (and (eq kind 'add) 'utf-8-unix))))

(defun ende-tools--patch-authorize-targets (change-sets root)
  "Resolve all CHANGE-SETS targets and authorize every scope before reads."
  (dolist (change-set change-sets)
    (let* ((tools (ende-change-set-authorization-tools change-set))
           (resolved (ende-tools--authorized-mutation-target
                      (ende-change-set-path change-set) root tools)))
      (setf (ende-change-set-root change-set) (plist-get resolved :root)
            (ende-change-set-project-root change-set) root
            (ende-change-set-absolute-path change-set)
            (plist-get resolved :path)
            (ende-change-set-path change-set) (plist-get resolved :public-path)
            (ende-change-set-public-path change-set)
            (plist-get resolved :public-path))))
  (ende-tools--assert-change-set-targets-unique change-sets))

(defun ende-tools--prepare-patch-change-sets (change-sets)
  "Read sources and compute every result for pre-authorized CHANGE-SETS."
  (dolist (change-set change-sets)
    (let ((path (ende-change-set-absolute-path change-set)))
      (if (ende-tools--mutation-existing-p change-set)
          (progn
            (unless (or (file-exists-p path) (file-symlink-p path))
              (error "patch source does not exist: %s"
                     (ende-change-set-path change-set)))
            (unless (file-regular-p path)
              (error "patch operation requires a regular file: %s"
                     (ende-change-set-path change-set)))
            (let* ((snapshot (ende-tools--capture-file path))
                   (buffers (ende-tools--visiting-buffers-for-path path)))
              (when (ende-tools--modified-buffers buffers)
                (error "unsaved visiting buffer prevents patching %s"
                       (ende-change-set-path change-set)))
              (when (string-match-p "\0" (plist-get snapshot :content))
                (error "binary file cannot be patched as text: %s"
                       (ende-change-set-path change-set)))
              (ende-tools--set-snapshot change-set snapshot)
              (setf (ende-change-set-visiting-buffers change-set) buffers)
              (when (eq (ende-change-set-operation change-set) 'update)
                (setf (ende-change-set-content change-set)
                      (ende-patch-apply-update
                       (plist-get snapshot :content)
                       (ende-change-set-anchor-edits change-set))
                      (ende-change-set-prepared-coding-system change-set)
                      (plist-get snapshot :coding-system)))))
        (when (or (file-exists-p path) (file-symlink-p path))
          (error "cannot add over an existing filesystem entry: %s"
                 (ende-change-set-path change-set))))))
  (ende-tools--assert-change-set-limits change-sets)
  change-sets)

(defun ende-tools--patch-result-entry (change-set)
  "Return successful operation and buffer-warning details for CHANGE-SET."
  (let* ((buffers (ende-change-set-visiting-buffers change-set))
         (present (and buffers t))
         (operation (ende-change-set-operation change-set))
         (warning
          (and present
               (if (eq operation 'delete)
                   (concat "Clean visiting buffers were left unchanged. "
                           "Saving one later could recreate the deleted file.")
                 (concat "The visiting buffer was left unchanged and may be "
                         "stale relative to this patch.")))))
    (append (ende-tools--change-set-result-entry change-set "applied")
            (list :visiting_buffer present
                  :visiting_buffer_count (length buffers)
                  :visiting_buffer_warning warning))))

(defun ende-tools--apply-patch (arguments root)
  "Apply one fully parsed, authorized patch as a single mutation batch."
  (ende-tools--assert-properties arguments '("patch") "apply_patch")
  (let* ((patch (ende-tools--required-string arguments "patch"))
         ;; Parsing the complete language precedes all path and filesystem work.
         (operations (ende-patch-parse patch))
         (change-sets (mapcar (lambda (operation)
                                (ende-tools--patch-change-set operation root))
                              operations)))
    (ende-tools--patch-authorize-targets change-sets root)
    (ende-tools--prepare-patch-change-sets change-sets)
    (ende-tools--commit-change-sets change-sets)
    (list :files (vconcat (mapcar #'ende-tools--patch-result-entry change-sets)))))

(defun ende-tools--web-fetch (arguments _root callback)
  (ende-tools--assert-properties arguments '("url" "max_chars") "web_fetch")
  (let* ((url (ende-tools--required-string arguments "url"))
         (keys (mapcar #'ende-tools--property-name
                       (ende-tools--object-keys arguments)))
         (access-config ende-tools--access-config)
         (access-tool ende-tools--access-tool)
         (register-cancellation ende-tools--register-cancellation-function)
         (max-chars
          (and (member "max_chars" keys)
               (ende-tools--bounded-integer
                arguments "max_chars" ende-web-fetch-max-output-chars
                ende-web-fetch-max-output-chars))))
    (ende-web-fetch
     url
     (lambda (normalized)
       (let ((ende-tools--access-config access-config)
             (ende-tools--access-tool access-tool))
         (ende-tools--web-url-permission normalized)))
     callback max-chars register-cancellation)))

;;;; Agent activity

(defconst ende-tools--activity-title-limit 120
  "Maximum characters accepted for an agent activity title.")

(defconst ende-tools--activity-status-limit 1000
  "Maximum characters accepted for an agent activity paragraph.")

(defun ende-tools--activity-text (arguments name limit &optional single-line)
  "Validate activity argument NAME in ARGUMENTS, bounded by LIMIT.
SINGLE-LINE disallows control characters, including newlines and tabs.
Return trimmed plain text without text properties."
  (let ((text (ende-tools--required-string arguments name)))
    (when (string-blank-p text)
      (error "%s must not be blank" name))
    (when (> (length text) limit)
      (error "%s exceeds %d characters" name limit))
    (when (and single-line (string-match-p "[[:cntrl:]]" text))
      (error "%s must be a single line without control characters" name))
    (string-trim (substring-no-properties text))))

(defun ende-tools--about-thinking (arguments _root)
  "Report ARGUMENTS as the originating agent job's current activity."
  (ende-tools--assert-properties arguments '("title" "status") "about_thinking")
  (let ((title (ende-tools--activity-text
                arguments "title" ende-tools--activity-title-limit t))
        (status (ende-tools--activity-text
                 arguments "status" ende-tools--activity-status-limit)))
    (unless (and (functionp ende-tools--report-activity-function)
                 (funcall ende-tools--report-activity-function title status))
      (error "about_thinking requires an active agent session"))
    (list :accepted t)))

(defun ende-tools--run-sandbox-command (arguments root callback)
  "Execute an authorized command asynchronously in the current job sandbox."
  (ende-tools--assert-properties
   arguments '("command" "cwd" "timeout") "run_sandbox_command")
  (let* ((owner ende-tools--execution-owner)
         (command (ende-tools--required-string arguments "command"))
         (cwd (or (ende-tools--argument arguments "cwd") "."))
         (timeout (ende-tools--bounded-integer
                   arguments "timeout" 300 3600))
         (sandbox (and owner (ende-sandbox-for-owner owner))))
    (unless owner
      (error "run_sandbox_command requires an active Ende job"))
    (unless sandbox
      (let ((image (plist-get ende-tools--access-config :podman-image))
            (files (ende-tools--authorized-project-files 'run_sandbox_command root)))
        (setq sandbox (ende-sandbox-create owner root image files))))
    (ende-sandbox-run-command
     sandbox command cwd timeout callback
     ende-tools--register-cancellation-function)))

(defconst ende-tools--host-command-output-limit 65536
  "Maximum characters captured from each host command output stream.")

(defun ende-tools--run-host-command-process
    (command cwd timeout callback register-cancellation)
  "Run host COMMAND in CWD, timing out after TIMEOUT seconds.
CALLBACK receives a result and error.  REGISTER-CANCELLATION associates the
process-group cancellation function with the active Ende job."
  (let ((stdout (list ""))
        (stderr (list ""))
        (stderr-process nil)
        (process nil)
        (timer nil)
        (registered nil)
        (finished nil))
    (cl-labels
        ((capture (output chunk)
           (let ((remaining
                  (- ende-tools--host-command-output-limit
                     (length (car output)))))
             (when (> remaining 0)
               (setcar output
                       (concat (car output)
                               (substring chunk 0 (min remaining (length chunk))))))
             (when (> (length chunk) remaining)
               (setcdr output t))))
         (kill-group ()
           (when (processp process)
             (ignore-errors
               (signal-process (- (process-id process)) 'sigkill))))
         (clean-stderr ()
           (when stderr-process
             (when (process-live-p stderr-process)
               (ignore-errors (accept-process-output stderr-process 0)))
             (ignore-errors (delete-process stderr-process))))
         (unregister-cancellation ()
           (when registered
             (setq registered nil)
             (ignore-errors (funcall register-cancellation nil))))
         (finish (code error)
           (unless finished
             (setq finished t)
             (when (timerp timer)
               (cancel-timer timer))
             (clean-stderr)
             (unregister-cancellation)
             (funcall
              callback
              (unless error
                (list :exit_code code
                      :process_ok (= code 0)
                      :stdout (car stdout)
                      :stderr (car stderr)
                      :stdout_truncated (if (cdr stdout) t :json-false)
                      :stderr_truncated (if (cdr stderr) t :json-false)))
              error)))
         (cancel ()
           (unless finished
             (kill-group)
             (finish nil "command cancelled")))
         (on-timeout ()
           (unless finished
             (kill-group)
             (finish 124 nil)))
         (on-sentinel (proc _event)
           (unless (process-live-p proc)
             (finish (if (eq (process-status proc) 'exit)
                         (process-exit-status proc)
                       125)
                     nil))))
      (condition-case caught
          (progn
            (setq stderr-process
                  (make-pipe-process
                   :name (generate-new-buffer-name "ende-host-command-stderr")
                   :buffer nil :noquery t :coding 'utf-8-unix
                   :filter (lambda (_proc chunk) (capture stderr chunk)))
                  process
                  (let ((default-directory cwd))
                    (make-process
                     :name (generate-new-buffer-name "ende-host-command")
                     :command (if (stringp command)
                                  (list "/bin/sh" "-c" command)
                                command)
                     :buffer nil :stderr stderr-process
                     :connection-type 'pipe :coding 'utf-8-unix :noquery t
                     :filter (lambda (_proc chunk) (capture stdout chunk))
                     :sentinel #'on-sentinel)))
            (process-send-eof process)
            (setq timer (run-at-time timeout nil #'on-timeout))
            (when (functionp register-cancellation)
              (setq registered t)
              (funcall register-cancellation #'cancel)))
        ((error quit)
         (kill-group)
         (finish nil (error-message-string caught)))))))

(defun ende-tools--host-command-cwd (cwd root)
  "Resolve host command CWD relative to ROOT and require a local directory."
  (unless (and (stringp cwd) (not (string-empty-p cwd)))
    (error "cwd must be a non-empty string"))
  (when (string-match-p (string 0) cwd)
    (error "cwd must not contain NUL characters"))
  (when (file-remote-p cwd)
    (error "remote/TRAMP working directories are not supported"))
  (let ((directory (expand-file-name cwd root)))
    (when (file-remote-p directory)
      (error "remote/TRAMP working directories are not supported"))
    (unless (file-directory-p directory)
      (error "cwd is not an existing directory: %s" cwd))
    (file-name-as-directory directory)))

(defun ende-tools--argv-prefix-p (prefix argv)
  "Return non-nil when PREFIX is an argv-list prefix of ARGV."
  (and (<= (length prefix) (length argv))
       (equal prefix (cl-subseq argv 0 (length prefix)))))

(defun ende-tools--run-host-command (arguments root callback)
  "Execute a host command asynchronously from ROOT under resolved policy."
  (let* ((policy (plist-get ende-tools--access-config :host-command-policy))
         (restricted (and (listp policy) (plist-get policy :restricted))))
    (ende-tools--assert-properties
     arguments '("command" "cwd" "timeout") "run_host_command")
    (let* ((command-value (ende-tools--argument arguments "command"))
           (argv
            (when restricted
              (unless (hash-table-p command-value)
                (error "command must be an object with program and args"))
              (ende-tools--assert-properties
               command-value '("program" "args") "command")
              (let* ((program (ende-tools--required-string command-value "program"))
                     (raw-args (ende-tools--argument command-value "args")))
                (unless (and (vectorp raw-args) (not (stringp raw-args))
                             (cl-every #'stringp (append raw-args nil)))
                  (error "command args must be an array of strings"))
                (cons program (append raw-args nil)))))
           (command (if restricted argv
                      (ende-tools--required-string arguments "command")))
           (cwd (or (ende-tools--argument arguments "cwd") "."))
           (timeout (ende-tools--bounded-integer arguments "timeout" 300 3600)))
      (when (and restricted
                 (not (or (member (car argv) (plist-get policy :open))
                          (member argv (plist-get policy :fixed))
                          (cl-some (lambda (prefix)
                                     (ende-tools--argv-prefix-p prefix argv))
                                   (plist-get policy :prefix)))))
        (error "host command is not allowed by restrict-host-commands"))
      (when (if restricted
                (cl-some (lambda (part) (string-match-p (string 0) part)) argv)
              (string-match-p (string 0) command))
        (error "command must not contain NUL characters"))
      (ende-tools--run-host-command-process
       command (ende-tools--host-command-cwd cwd root) timeout callback
       ende-tools--register-cancellation-function))))

(defun ende-tools--copy-from-sandbox (arguments root)
  "Copy one sandbox regular file to an independently authorized host path."
  (ende-tools--assert-properties
   arguments '("source" "destination") "copy_from_sandbox")
  (let* ((owner ende-tools--execution-owner)
         (sandbox (and owner (ende-sandbox-for-owner owner)))
         (source (ende-tools--required-string arguments "source"))
         (destination (ende-tools--required-string arguments "destination")))
    (unless (and owner sandbox (not (ende-sandbox-closing sandbox)))
      (error "copy_from_sandbox requires an active Ende job"))
    (ende-tools--strict-relative-path destination)
    (let* ((resolved (ende-tools--resolve-mutation-path destination root t))
           (path (plist-get resolved :path))
           (relative (plist-get resolved :relative))
           (exists (or (file-exists-p path) (file-symlink-p path))))
      (ende-tools--assert-path-authorized-for
       'copy_from_sandbox path root t)
      (when (and exists (not (file-regular-p path)))
        (error "copy destination must be a regular file: %s" relative))
      (let* ((bytes
              (ende-sandbox-with-quiesced-workspace
               sandbox
               (lambda ()
                 (let* ((source-path
                         (ende-sandbox-source-file owner source))
                        (attributes (file-attributes source-path))
                        (size (file-attribute-size attributes)))
                   (when (> size ende-tools-max-prepared-bytes)
                     (error "sandbox source exceeds the prepared-data limit"))
                   (ende-tools--literal-file-bytes source-path)))))
             (change-set
              (ende-change-set--create
               :operation (if exists 'edit 'create)
               :root root :project-root root
               :source-kind "file" :path relative
               :absolute-path path :expected-exists (and exists t)
               :strict-path-policy t :strict-buffer-policy t
               :authorization-tools '(copy_from_sandbox)
               :content bytes :prepared-coding-system 'no-conversion)))
        (when exists
          (ende-tools--set-snapshot
           change-set (ende-tools--capture-file path)))
        (ende-tools--commit-change-sets (list change-set))
        (ende-tools--change-set-result change-set)))))

;;;; Tool schemas and registrations

(defconst ende-tools--empty-object-schema
  (list (cons :type "object")
        (cons :properties (make-hash-table))
        (cons :additionalProperties :json-false)))

(defconst ende-tools--path-schema
  '((:type . "object")
    (:properties
     . ((:path . ((:type . "string")
                  (:minLength . 1)
                  (:description . "Project-relative path or authorized absolute path")))))
    (:required . ("path"))
    (:additionalProperties . :json-false)))

(defconst ende-tools--create-file-schema
  '((:type . "object")
    (:properties
     . ((:path . ((:type . "string")
                  (:minLength . 1)
                  (:description . "Project-relative path or authorized absolute path; parent directory must already exist")))
        (:content . ((:type . "string")
                     (:description . "Complete file contents; may be empty")))))
    (:required . ("path" "content"))
    (:additionalProperties . :json-false)))

(defconst ende-tools--delete-file-schema
  '((:type . "object")
    (:properties
     . ((:path . ((:type . "string")
                  (:minLength . 1)
                  (:description . "Project-relative path or authorized absolute path")))
        (:expected_digest . ((:type . "string")
                             (:pattern . "^[0-9a-fA-F]{64}$")
                             (:description . "Optional SHA-256 precondition")))))
    (:required . ("path"))
    (:additionalProperties . :json-false)))

(defconst ende-tools--apply-patch-schema
  `((:type . "object")
    (:properties
     . ((:patch . ((:type . "string")
                   (:minLength . 1)
                   (:maxLength . ,ende-patch-max-chars)
                   (:description . "Limited patch format: *** Begin Patch, one or more *** Add File, *** Update File, or *** Delete File operations, @@ update hunks with exact update context, then *** End Patch.")))))
    (:required . ("patch"))
    (:additionalProperties . :json-false)))

(defconst ende-tools--web-fetch-schema
  `((:type . "object")
    (:properties
     . ((:url . ((:type . "string")
                 (:minLength . 1)
                 (:description . "Absolute HTTP(S) URL authorized by URL resources")))
        (:max_chars . ((:type . "integer")
                       (:minimum . 1)
                       (:maximum . ,ende-web-fetch-max-output-chars)
                       (:description . "Optional returned-character ceiling, bounded by the configured maximum.")))))
    (:required . ("url"))
    (:additionalProperties . :json-false)))

(defconst ende-tools--run-sandbox-command-schema
  '((:type . "object")
    (:properties
     . ((:command . ((:type . "string") (:minLength . 1)))
        (:cwd . ((:type . "string") (:minLength . 1)
                 (:description . "Optional workspace-relative working directory constrained to the current private workspace; defaults to \".\".")))
        (:timeout . ((:type . "integer") (:minimum . 1) (:maximum . 3600)))))
    (:required . ("command"))
    (:additionalProperties . :json-false)))

(defconst ende-tools--run-host-command-schema
  '((:type . "object")
    (:properties
     . ((:command . ((:type . "string") (:minLength . 1)))
        (:cwd . ((:type . "string") (:minLength . 1)
                 (:description
                  . "Optional local working directory; relative values are resolved from the project root, absolute local directories are accepted, and the default is \".\".")))
        (:timeout . ((:type . "integer") (:minimum . 1) (:maximum . 3600)))))
    (:required . ("command"))
    (:additionalProperties . :json-false)))

(defun ende-tools--host-command-schema (access-config)
  "Return the run_host_command schema for ACCESS-CONFIG."
  (let ((policy (plist-get access-config :host-command-policy)))
    (if (not (and (listp policy) (plist-get policy :restricted)))
        ende-tools--run-host-command-schema
      (let* ((open (plist-get policy :open))
             (fixed (plist-get policy :fixed))
             (prefix (plist-get policy :prefix))
             (branches
              (append
               (when open
                 (list
                  (list :type "object"
                        :properties
                        (list :program (list :enum (vconcat open))
                              :args '(:type "array" :items (:type "string")))
                        :description "Run the named executable with arbitrary arguments."
                        :required '["program" "args"]
                        :additionalProperties :json-false)))
               (mapcar
                (lambda (argv)
                  (list :type "object"
                        :properties
                        (list :program (list :const (car argv))
                              :args (list :type "array"
                                          :items '(:type "string")
                                          :minItems (length (cdr argv))
                                          :description
                                          (format
                                           "Arguments must begin with %s; additional arguments are allowed."
                                           (prin1-to-string (cdr argv)))))
                        :description "Run with the required argv prefix."
                        :required '["program" "args"]
                        :additionalProperties :json-false))
                prefix)
               (mapcar
                (lambda (argv)
                  (list :type "object"
                        :properties
                        (list :program (list :const (car argv))
                              :args (list :const (vconcat (cdr argv))))
                        :description "Run exactly this argv."
                        :required '["program" "args"]
                        :additionalProperties :json-false))
                fixed)))
             (branches
              (cl-remove-if
               (lambda (branch)
                 (member (alist-get :const
                                    (alist-get :program
                                               (alist-get :properties branch)))
                         open))
               branches)))
        (list :type "object"
              :properties
              (list :command (list (if prefix :anyOf :oneOf)
                                   (vconcat branches))
                    :cwd (alist-get :cwd
                                    (alist-get :properties
                                               ende-tools--run-host-command-schema))
                    :timeout (alist-get :timeout
                                        (alist-get :properties
                                                   ende-tools--run-host-command-schema)))
              :required '["command"]
              :additionalProperties :json-false)))))

(defconst ende-tools--copy-from-sandbox-schema
  '((:type . "object")
    (:properties
     . ((:source . ((:type . "string") (:minLength . 1)
                    (:description . "Workspace-relative path to an existing regular file")))
        (:destination . ((:type . "string") (:minLength . 1)
                         (:description . "Independently authorized project-relative destination; creates a missing file or replaces an existing regular file")))))
    (:required . ("source" "destination"))
    (:additionalProperties . :json-false)))

(defconst ende-tools--edit-item-schema
  (list
   (cons :type "object")
   (cons :properties
         (list
          (cons :anchor
                '((:type . "string")
                  (:minLength . 1)
                  (:description . "Text matched against the original file contents; normally identify one location.")))
          (cons :position
                '((:type . "string")
                  (:enum . ("before" "after" "replace"))
                  (:description . "Insert before or after the anchor, or replace it.")))
          (cons :text '((:type . "string")))
          (cons :occurrence
                '((:type . "integer")
                  (:minimum . 1)
                  (:description . "Select a particular repeated anchor (1-based).")))
          (cons :all
                '((:type . "boolean")
                  (:const . t)
                  (:description . "Apply this edit to every matching occurrence.")))))
   (cons :not '((:required . ["occurrence" "all"])))
   (cons :required '("anchor" "position" "text"))
   (cons :additionalProperties :json-false)))

(defconst ende-tools--edit-file-schema
  (list
   (cons :type "object")
   (cons :description
         "Changes disk contents without modifying an already visiting buffer.")
   (cons :properties
         (list
          (cons :path '((:type . "string")
                        (:minLength . 1)
                        (:description . "Project-relative path or authorized absolute path")))
          (cons :edits
                (list (cons :type "array")
                      (cons :minItems 1)
                      (cons :maxItems ende-tools-max-edits)
                      (cons :description
                            "Resolve all edits against the same original file, not sequentially; edits must not overlap.")
                      (cons :items ende-tools--edit-item-schema)))))
   (cons :required '("path" "edits"))
   (cons :additionalProperties :json-false)))

(setq ende-tools--registry nil)

(ende-tool-register
 'about_thinking
 (concat "Update your user-visible progress report with a short title and "
         "1-3 sentences. Use at the start of substantive work, before the "
         "tools it describes, and when changing phases or encountering a "
         "blocker. Report current actions and progress, not private reasoning; "
         "avoid unchanged updates. Always available, without project permissions.")
 (list
  (cons :type "object")
  (cons :properties
        (list
         (cons :title
               (list (cons :type "string") (cons :minLength 1)
                     (cons :maxLength ende-tools--activity-title-limit)
                     (cons :description "Short, single-line activity title")))
         (cons :status
               (list (cons :type "string") (cons :minLength 1)
                     (cons :maxLength ende-tools--activity-status-limit)
                     (cons :description
                           "1-3 sentences about current activity, progress, or a blocker")))))
  (cons :required '("title" "status"))
  (cons :additionalProperties :json-false))
 #'ende-tools--about-thinking nil :no-resource
 '(:requires-root nil :always-available t))

(ende-tool-register
 'project_info
 "Return the current project root and compact source counts."
 ende-tools--empty-object-schema #'ende-tools--project-info nil :no-resource)

(ende-tool-register
 'list_project_files
 "List allowed regular files on disk below the project root."
 `((:type . "object")
   (:properties
    . ((:glob . ((:type . "string") (:minLength . 1)
                 (:description . "Optional project-relative wildcard")))
       (:max_results . ((:type . "integer") (:minimum . 1)
                        (:maximum . ,ende-tools-max-listed-files)
                        (:description . "Defaults to the configured maximum.")))
       (:include_metadata . ((:type . "boolean")))))
   (:additionalProperties . :json-false))
 #'ende-tools--list-project-files)

(ende-tool-register
 'file_info
 "Return disk-derived file metadata and separate visiting-buffer state."
 ende-tools--path-schema #'ende-tools--file-info)

(ende-tool-register
 'read_file
 "Read a bounded, 1-based line range from an authorized file on disk."
 `((:type . "object")
   (:properties
    . ((:path . ((:type . "string") (:minLength . 1)
                 (:description . "Project-relative path or authorized absolute path")))
       (:start_line . ((:type . "integer") (:minimum . 1)))
       (:line_count . ((:type . "integer") (:minimum . 1)
                       (:maximum . ,ende-tools-max-read-lines)
                       (:description . "Defaults to the configured maximum.")))))
   (:required . ("path"))
   (:additionalProperties . :json-false))
 #'ende-tools--read-file)

(ende-tool-register
 'list_project_buffers
 "List live visiting buffers whose files belong to the project; this reports buffers, not disk contents."
 ende-tools--empty-object-schema #'ende-tools--list-project-buffers)

(ende-tool-register
 'read_buffer
 "Read a bounded, 1-based line range from a visiting buffer, including unsaved contents."
 `((:type . "object")
   (:properties
    . ((:buffer_id . ((:type . "string") (:minLength . 1)))
       (:start_line . ((:type . "integer") (:minimum . 1)))
       (:line_count . ((:type . "integer") (:minimum . 1)
                       (:maximum . ,ende-tools-max-read-lines)
                       (:description . "Defaults to the configured maximum.")))))
   (:required . ("buffer_id"))
   (:additionalProperties . :json-false))
 #'ende-tools--read-buffer)

(ende-tool-register
 'search_project
 "Search allowed project files on disk. Exactly one search selector is required: query, patterns, or regexp (which uses Emacs regular-expression syntax, not PCRE). Results group matching lines by file; use context_before and context_after for extra context, or read_file for more source."
 `((:type . "object")
   (:properties
    . ((:query . ((:type . "string") (:minLength . 1)
                  (:description . "Literal string to search for.")))
       (:patterns . ((:type . "array") (:minItems . 1)
                     (:items . ((:type . "string") (:minLength . 1)))
                     (:description . "Literal strings to search for with OR semantics.")))
       (:regexp . ((:type . "string") (:minLength . 1)
                   (:description . "Explicit Emacs regular expression for advanced searches (not PCRE).")))
       (:glob . ((:type . "string") (:minLength . 1)))
       (:context_before . ((:type . "integer") (:minimum . 0)
                           (:maximum . ,ende-tools-max-search-context-lines)
                           (:description . "Defaults to 0.")))
       (:context_after . ((:type . "integer") (:minimum . 0)
                          (:maximum . ,ende-tools-max-search-context-lines)
                          (:description . "Defaults to 0.")))
       (:max_results . ((:type . "integer") (:minimum . 1)
                        (:maximum . ,ende-tools-max-search-results)
                        (:description . "Defaults to 30.")))))
   (:oneOf
    . [((:required . ["query"]))
       ((:required . ["patterns"]))
       ((:required . ["regexp"]))])
   (:additionalProperties . :json-false))
 #'ende-tools--search-project)

(ende-tool-register
 'edit_file
 "Apply exact-anchor edits directly to an authorized disk file without changing visiting buffers."
 ende-tools--edit-file-schema
 #'ende-tools--edit-file :write)

(ende-tool-register
 'create_file
 "Create a new authorized file; fail if it exists."
 ende-tools--create-file-schema
 #'ende-tools--create-file :write)

(ende-tool-register
 'delete_file
 "Delete one authorized regular disk file without modifying visiting buffers."
 ende-tools--delete-file-schema
 #'ende-tools--delete-file :write)

(ende-tool-register
 'apply_patch
 "Apply an exact-context multi-file text patch to paths authorized for apply_patch."
 ende-tools--apply-patch-schema
 #'ende-tools--apply-patch :write)

(ende-tool-register
 'web_fetch
 "Fetch bounded textual content from a URL explicitly permitted by this chat."
 ende-tools--web-fetch-schema
 #'ende-tools--web-fetch nil :no-resource
 '(:resource-kind url :requires-root nil :async t))

(ende-tool-register
 'run_sandbox_command
 (concat "Run a shell command in the current job's private development workspace. "
         "Use this as the normal tool for builds, tests, linters, repository "
         "inspection, temporary diagnostics, and changes intended to stay "
         "in that workspace; use dedicated project mutation tools for desired "
         "host-project edits.")
 ende-tools--run-sandbox-command-schema
 #'ende-tools--run-sandbox-command nil nil
 '(:resource-kind file :requires-resources nil :async t))

(ende-tool-register
 'run_host_command
 (concat "Run an arbitrary shell command directly on the host with the "
         "permissions and environment of the Emacs process, bypassing the "
         "private development workspace. Use only when host execution is "
         "specifically required; do not use instead of edit_file, create_file, "
         "delete_file, or apply_patch when modifying project source.")
 ende-tools--run-host-command-schema
 #'ende-tools--run-host-command nil :no-resource '(:async t))

(ende-tool-register
 'copy_from_sandbox
 "Copy one regular file from the job workspace to an independently authorized project path."
 ende-tools--copy-from-sandbox-schema
 #'ende-tools--copy-from-sandbox :write)

(provide 'ende-tools)

;;; ende-tools.el ends here
