;;; ende-chat-config.el --- Declarative chat tool configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools, llm

;;; Commentary:

;; Parse and resolve the deliberately small data language used by Org chat
;; buffers.  Configuration is never evaluated as Emacs Lisp.

;;; Code:

(require 'cl-lib)
(require 'org-element)
(require 'subr-x)
(require 'ende-project)
(require 'ende-url)
(require 'ende-tools)

(defvar ende-chat-config--block-line nil
  "Line of the configuration block currently being applied, if any.")

(defvar ende-chat-config--block-state nil
  "State used to validate order-sensitive forms in the current block.")

(defun ende-chat-config--error (format-string &rest arguments)
  "Signal a configuration error formatted with FORMAT-STRING and ARGUMENTS."
  (error "ende chat config: %s%s"
         (apply #'format format-string arguments)
         (if ende-chat-config--block-line
             (format " (block at line %d)" ende-chat-config--block-line)
           "")))

(defun ende-chat-config--reject-reader-dispatch (source)
  "Reject Emacs reader dispatch syntax in SOURCE, outside strings/comments."
  (let ((index 0)
        (length (length source))
        (in-string nil)
        (escaped nil)
        (in-comment nil))
    (while (< index length)
      (let ((character (aref source index)))
        (cond
         (in-comment
          (when (= character ?\n)
            (setq in-comment nil)))
         (in-string
          (cond
           (escaped (setq escaped nil))
           ((= character ?\\) (setq escaped t))
           ((= character ?\") (setq in-string nil))))
         ((= character ?\;) (setq in-comment t))
         ((= character ?\") (setq in-string t))
         ((= character ?#)
          (ende-chat-config--error
           "reader dispatch syntax is not allowed")))
        (setq index (1+ index))))))

(defun ende-chat-config--skip-space (source position)
  "Return the first non-whitespace, non-comment position in SOURCE."
  (let ((length (length source))
        done)
    (while (and (< position length) (not done))
      (cond
       ((memq (aref source position) '(?\s ?\t ?\r ?\n))
        (setq position (1+ position)))
       ((= (aref source position) ?\;)
        (setq position
              (or (string-match "\n" source position) length)))
       (t (setq done t))))
    position))

(defun ende-chat-config--read-forms (source)
  "Read data forms from SOURCE without Emacs reader dispatch syntax."
  (ende-chat-config--reject-reader-dispatch source)
  (let ((position 0)
        forms)
    (setq position (ende-chat-config--skip-space source position))
    (while (< position (length source))
      (condition-case caught
          (let* ((parsed (read-from-string source position))
                 (form (car parsed))
                 (end (cdr parsed)))
            (unless (> end position)
              (ende-chat-config--error "could not read configuration"))
            (push form forms)
            (setq position (ende-chat-config--skip-space source end)))
        (end-of-file
         (signal (car caught) (cdr caught)))
        (error
         (ende-chat-config--error "%s" (error-message-string caught)))))
    (nreverse forms)))

(defun ende-chat-config--proper-list-p (value)
  "Return non-nil when VALUE is a proper list."
  (and (listp value) (proper-list-p value)))

(defun ende-chat-config--name (value context)
  "Validate and return symbolic VALUE used as a name in CONTEXT."
  (unless (and (symbolp value)
               (not (keywordp value))
               (string-match-p "\\`[[:alnum:]_-]+\\'"
                               (symbol-name value)))
    (ende-chat-config--error "invalid %s name: %S" context value))
  value)

(defun ende-chat-config--tool-name (value)
  "Validate and return registered tool VALUE."
  (let ((name (ende-chat-config--name value "tool")))
    (unless (ende-tool--find name)
      (ende-chat-config--error "unknown tool: %s" name))
    name))

(defun ende-chat-config--file-pattern (value resource)
  "Validate existing project-relative file pattern VALUE for RESOURCE."
  (unless (and (stringp value)
               (not (string-empty-p value))
               (not (file-name-absolute-p value))
               (not (member ".." (split-string value "/" t))))
    (ende-chat-config--error
     "invalid path pattern in resource %s: %S" resource value))
  value)

(defun ende-chat-config--resource-type (value resource)
  "Validate resource type VALUE for RESOURCE."
  (unless (memq value '(file url))
    (ende-chat-config--error "unsupported type %S in resource %s" value resource))
  value)

(defun ende-chat-config--local-blocks (tree)
  "Map Org nodes in TREE to their direct `ende' configuration blocks."
  (let ((blocks (make-hash-table :test #'eq)))
    (org-element-map tree 'special-block
		     (lambda (block)
		       (let* ((section (org-element-property :parent block))
			      (node (and (eq (org-element-type section) 'section)
					 (org-element-property :parent section))))
			 (when (and node
				    (memq (org-element-type node) '(org-data headline))
				    (string= (downcase (or (org-element-property :type block) ""))
					     "ende"))
			   (puthash node (cons block (gethash node blocks)) blocks))))
		     nil nil nil)
    blocks))

(defun ende-chat-config--block-source (block)
  "Return the body text of Org configuration BLOCK."
  (let ((begin (org-element-property :contents-begin block))
        (end (org-element-property :contents-end block)))
    (if (and begin end)
        (buffer-substring-no-properties begin end)
      "")))

(defun ende-chat-config-document-block-source (tree)
  "Return the document-level Ende block source in parsed Org TREE, or nil."
  (let ((blocks (gethash tree (ende-chat-config--local-blocks tree))))
    (when blocks
      (when (cdr blocks)
        (ende-chat-config--error "only one config block is allowed per Org node"))
      ;; Org's :end includes following blank lines, not part of the block.
      (let* ((block (car blocks))
             (end (- (org-element-property :end block)
                     (or (org-element-property :post-blank block) 0))))
        (buffer-substring-no-properties
         (org-element-property :begin block)
         end)))))

(defun ende-chat-config--block-forms (block)
  "Read the forms in configuration BLOCK."
  (let ((parameters (string-trim
                     (or (org-element-property :parameters block) ""))))
    (unless (string-empty-p parameters)
      (ende-chat-config--error "ende blocks do not accept parameters"))
    (ende-chat-config--read-forms (ende-chat-config--block-source block))))

(defun ende-chat-config--agent-definition (form)
  "Validate and return the (NAME . BODY) definition in FORM."
  (unless (and (ende-chat-config--proper-list-p form) (>= (length form) 2))
    (ende-chat-config--error "malformed agent-config definition: %S" form))
  (let ((name (ende-chat-config--name (nth 1 form) "agent config"))
        (body (cddr form)))
    (dolist (body-form body)
      (unless (ende-chat-config--proper-list-p body-form)
        (ende-chat-config--error "malformed form in agent-config %s: %S"
                                 name body-form))
      (when (eq (car body-form) 'agent-config)
        (ende-chat-config--error "nested agent-config definitions are not allowed")))
    (cons name body)))

(defun ende-chat-config--copy (configuration)
  "Return an independent copy of CONFIGURATION."
  (copy-tree configuration))

(defun ende-chat-config--lookup (name definitions)
  "Return NAME's definition in alist DEFINITIONS."
  (cdr (assq name definitions)))

(defun ende-chat-config--put (name value definitions)
  "Set NAME to VALUE in DEFINITIONS and return the updated alist."
  (if (assq name definitions)
      (cons (cons name value) (assq-delete-all name definitions))
    (cons (cons name value) definitions)))

(defun ende-chat-config--canonical-root (path)
  "Expand and canonicalize existing directory PATH."
  (unless (stringp path)
    (ende-chat-config--error "root must be a string"))
  (let ((expanded (expand-file-name path)))
    (unless (file-directory-p expanded)
      (ende-chat-config--error "root is not an existing directory: %s" path))
    (ende-project-normalize-root expanded)))

(defun ende-chat-config--podman-image (value)
  "Validate and return digest-pinned Podman image VALUE."
  (unless (and (stringp value)
               (string-match-p
                "\\`[[:graph:]]+@sha256:[[:xdigit:]]\\{64\\}\\'"
                value))
    (ende-chat-config--error
     "podman-image must be pinned with a sha256 digest"))
  value)

(defun ende-chat-config--remove-grants (predicate configuration)
  "Remove grants matching PREDICATE from CONFIGURATION.
Grants are (RESOURCE . TARGET) pairs, keyed by tool."
  (let (assignments)
    (dolist (entry (plist-get configuration :grants))
      (let ((remaining (cl-remove-if predicate (cdr entry))))
        (when remaining
          (push (cons (car entry) remaining) assignments))))
    (plist-put configuration :grants (nreverse assignments))))

(defun ende-chat-config--define-group (form configuration)
  "Install group FORM in CONFIGURATION, revoking its inherited permissions."
  (unless (and (ende-chat-config--proper-list-p form) (>= (length form) 3))
    (ende-chat-config--error "malformed group definition: %S" form))
  (let* ((name (ende-chat-config--name (nth 1 form) "group"))
         (groups (plist-get configuration :groups))
         (tools (delete-dups
                 (mapcar #'ende-chat-config--tool-name (cddr form)))))
    (when (ende-tool--find name)
      (ende-chat-config--error "group name conflicts with registered tool: %s"
                               name))
    (when (assq name groups)
      (setq configuration
            (plist-put configuration :active-targets
                       (remq name (plist-get configuration :active-targets)))
            configuration
            (ende-chat-config--remove-grants
             (lambda (grant) (eq (cdr grant) name)) configuration)))
    (plist-put configuration :groups
               (ende-chat-config--put name tools groups))))

(defun ende-chat-config--define-resource (form configuration)
  "Install resource FORM in CONFIGURATION, revoking its inherited grants."
  (unless (and (ende-chat-config--proper-list-p form) (>= (length form) 3))
    (ende-chat-config--error "malformed resource definition: %S" form))
  (let* ((name (ende-chat-config--name (nth 1 form) "resource"))
         (resources (plist-get configuration :resources))
         (tail (cddr form))
         (type 'file)
         (type-seen nil)
         (resource-root nil)
         (root-seen nil)
         (private nil)
         (private-seen nil)
         (rules-seen nil)
         includes excludes include-patterns exclude-patterns)
    (while tail
      (let ((form-part (pop tail)))
        (cond
         ((and (ende-chat-config--proper-list-p form-part)
               (= (length form-part) 2)
               (memq (car form-part) '(include exclude)))
          (setq rules-seen t)
          (let ((pattern
                 (if (eq type 'file)
                     (ende-chat-config--file-pattern (cadr form-part) name)
                   (unless (stringp (cadr form-part))
                     (ende-chat-config--error
                      "URL pattern in resource %s must be a string" name))
                   (condition-case caught
                       (ende-url-pattern-compile (cadr form-part))
                     (error
                      (ende-chat-config--error
                       "invalid URL pattern in resource %s: %s"
                       name (error-message-string caught)))))))
            (if (eq (car form-part) 'include)
                (progn (push (cadr form-part) includes)
                       (when (eq type 'url)
                         (push pattern include-patterns)))
              (push (cadr form-part) excludes)
              (when (eq type 'url)
                (push pattern exclude-patterns)))))
         ((memq form-part '(:type :root :allow-private-addresses))
          (when rules-seen
            (ende-chat-config--error
             "resource options must precede include/exclude rules (%s)" name))
          (unless tail
            (ende-chat-config--error "resource option %s needs a value" form-part))
          (let ((value (pop tail)))
            (pcase form-part
              (:type
               (when type-seen
                 (ende-chat-config--error "resource %s repeats :type" name))
               (setq type (ende-chat-config--resource-type value name)
                     type-seen t))
              (:root
               (when root-seen
                 (ende-chat-config--error "resource %s repeats :root" name))
               (when (and (stringp value) (file-remote-p value))
                 (ende-chat-config--error
                  "resource :root must be a local directory (%s)" name))
               (setq resource-root
                     (ende-chat-config--canonical-root value)
                     root-seen t))
              (:allow-private-addresses
               (when private-seen
                 (ende-chat-config--error
                  "resource %s repeats :allow-private-addresses" name))
               (unless (memq value '(t nil))
                 (ende-chat-config--error
                  ":allow-private-addresses must be t or nil in resource %s" name))
               (setq private value private-seen t)))))
         (t
          (ende-chat-config--error "malformed rule or option in resource %s: %S"
                                   name form-part)))))
    (unless includes
      (ende-chat-config--error "resource %s needs an explicit include" name))
    (setq includes (nreverse includes)
          excludes (nreverse excludes)
          include-patterns (nreverse include-patterns)
          exclude-patterns (nreverse exclude-patterns))
    (when (and private-seen (not (eq type 'url)))
      (ende-chat-config--error
       ":allow-private-addresses is only valid for URL resources (%s)" name))
    (when (and root-seen (not (eq type 'file)))
      (ende-chat-config--error
       ":root is only valid for file resources (%s)" name))
    (when (and private
               (cl-some (lambda (pattern)
                          (memq (plist-get pattern :host-kind)
                                '(any-dns subdomains)))
                        (append include-patterns exclude-patterns)))
      (ende-chat-config--error
       "private-enabled resource %s must use exact URL hosts" name))
    (when (assq name resources)
      (let ((revoked (plist-get configuration :revoked-resources)))
        (dolist (entry (plist-get configuration :grants))
          (when (assq name (cdr entry))
            (setq revoked
                  (ende-chat-config--put
                   (car entry)
                   (cl-adjoin name (cdr (assq (car entry) revoked)))
                   revoked))))
        (setq configuration
              (plist-put configuration :revoked-resources revoked))))
    (setq configuration
          (ende-chat-config--remove-grants
           (lambda (grant) (eq (car grant) name)) configuration))
    (plist-put configuration :resources
               (ende-chat-config--put
                name (list :type type
                           :root resource-root
                           :include includes :exclude excludes
                           :allow-private-addresses private
                           :include-regexps
                           (and (eq type 'file)
                                (mapcar #'wildcard-to-regexp includes))
                           :exclude-regexps
                           (and (eq type 'file)
                                (mapcar #'wildcard-to-regexp excludes))
                           :include-url-patterns include-patterns
                           :exclude-url-patterns exclude-patterns)
                resources))))

(defun ende-chat-config--target-tools (name configuration)
  "Return the tools selected by group or tool NAME in CONFIGURATION."
  (let ((name (ende-chat-config--name name "tool or group")))
    (or (ende-chat-config--lookup name (plist-get configuration :groups))
        (list (ende-chat-config--tool-name name)))))

(defun ende-chat-config--resolve-tools (names configuration)
  "Expand tool or group NAMES using CONFIGURATION's group definitions."
  (let (tools)
    (dolist (name names)
      (dolist (tool (ende-chat-config--target-tools name configuration))
        (push tool tools)))
    (delete-dups (nreverse tools))))

(defun ende-chat-config--host-command-program (program)
  "Validate PROGRAM as a bare executable name or local absolute path."
  (unless (and (not (member program '("." "..")))
               (not (file-remote-p program))
               (not (string-match-p "[[:cntrl:]]" program))
               (or (string-match-p "\\`[^/[:space:]]+\\'" program)
                   (and (file-name-absolute-p program)
                        (not (string-prefix-p "~" program)))))
    (ende-chat-config--error
     "host command executable must be a bare name or local absolute path: %S"
     program)))

(defun ende-chat-config--host-command-rules (rules)
  "Validate and normalize host command RULES."
  (unless rules
    (ende-chat-config--error "restrict-host-commands needs at least one rule"))
  (let (open fixed prefix)
    (dolist (rule rules)
      (cond
       ((stringp rule)
        (ende-chat-config--host-command-program rule)
        (push rule open))
       ((and (ende-chat-config--proper-list-p rule)
             (memq (car rule) '(:fixed-args :prefix-args))
             (cdr rule)
             (cl-every #'stringp (cdr rule))
             (or (eq (car rule) :fixed-args)
                 (cdr (cdr rule))))
        (let ((argv (cdr rule)))
          (ende-chat-config--host-command-program (car argv))
          (if (eq (car rule) :fixed-args)
              (push argv fixed)
            (push argv prefix))))
       (t
        (ende-chat-config--error
         "host command rule must be an executable string, (:fixed-args COMMAND ARG...), or (:prefix-args COMMAND ARG...): %S"
         rule))))
    (setq open (delete-dups (nreverse open))
          fixed (delete-dups (nreverse fixed))
          prefix (delete-dups (nreverse prefix)))
    (setq prefix
          (cl-remove-if
           (lambda (argv)
             (or (member (car argv) open)
                 (cl-some (lambda (other)
                            (and (not (equal argv other))
                                 (ende-tools--argv-prefix-p other argv)))
                          prefix)))
           prefix)
          fixed
          (cl-remove-if
           (lambda (argv)
             (or (member (car argv) open)
                 (cl-some (lambda (rule)
                            (ende-tools--argv-prefix-p rule argv))
                          prefix)))
           fixed))
    (append (list :restricted t :open open :fixed fixed)
            (when prefix (list :prefix prefix)))))

(defun ende-chat-config--host-command-enabled-p (configuration)
  "Return non-nil when run_host_command is active in CONFIGURATION."
  (memq 'run_host_command
        (cl-set-difference
         (ende-chat-config--resolve-tools
          (plist-get configuration :active-targets) configuration)
         (plist-get configuration :excluded-tools))))

(defun ende-chat-config--resource-names (names configuration)
  "Validate resource NAMES against CONFIGURATION."
  (unless names
    (ende-chat-config--error "at least one resource is required"))
  (delete-dups
   (mapcar (lambda (name)
             (let ((name (ende-chat-config--name name "resource")))
               (unless (assq name (plist-get configuration :resources))
                 (ende-chat-config--error "unknown resource: %s" name))
               name))
           names)))

(defun ende-chat-config--check-assignment-types (tools resources configuration)
  "Reject incompatible RESOURCES assigned to TOOLS."
  (dolist (tool-name tools)
    (let* ((tool (ende-tool--find tool-name))
           (kind (ende-tool-resource-kind tool)))
      (unless (eq kind 'none)
        (dolist (resource-name resources)
          (let* ((resource (ende-chat-config--lookup
                            resource-name (plist-get configuration :resources)))
                 (type (plist-get resource :type)))
            (unless (eq kind type)
              (ende-chat-config--error
               "resource %s has type %s and cannot be assigned to %s (%s)"
               resource-name type tool-name kind))))))))

(defun ende-chat-config--validate-grant-types (configuration)
  "Validate all grants in CONFIGURATION, including inactive tool grants."
  (let ((resources (plist-get configuration :resources)))
    (dolist (entry (plist-get configuration :grants))
      (let* ((tool (ende-tool--find (car entry)))
             (kind (ende-tool-resource-kind tool)))
        (unless (eq kind 'none)
          (dolist (grant (cdr entry))
            (let* ((resource (ende-chat-config--lookup (car grant) resources))
                   (type (plist-get resource :type)))
              (unless (eq kind type)
                (ende-chat-config--error
                 "resource %s has type %s and cannot be assigned to %s (%s)"
                 (car grant) type (car entry) kind))))))))
  configuration)

(defun ende-chat-config--assign-resources (form configuration additive)
  "Apply resource assignment FORM to CONFIGURATION.
When ADDITIVE is non-nil, union with each tool's current assignment."
  (unless (ende-chat-config--proper-list-p form)
    (ende-chat-config--error "malformed resource assignment: %S" form))
  (let ((separator (cl-position :resources form)))
    (unless (and separator (>= separator 2)
                 (< separator (1- (length form))))
      (ende-chat-config--error
       "resource assignment needs tools, :resources, and resources: %S" form))
    (let ((names (cl-subseq form 1 separator))
          (raw-resources (nthcdr (1+ separator) form)))
      (when (memq :resources raw-resources)
        (ende-chat-config--error
         "resource assignment may contain only one :resources separator: %S"
         form))
      (let ((tools (ende-chat-config--resolve-tools names configuration))
            (resources (ende-chat-config--resource-names
                        raw-resources configuration))
            (assignments (plist-get configuration :grants)))
        (ende-chat-config--check-assignment-types
         tools resources configuration)
        ;; Clear once so overlapping targets retain both origins.
        (unless additive
          (setq assignments
                (cl-remove-if (lambda (entry) (memq (car entry) tools))
                              assignments)))
        (dolist (name (delete-dups names))
          (dolist (tool (ende-chat-config--target-tools name configuration))
            (let ((current (cdr (assq tool assignments)))
                  (grants (mapcar (lambda (resource) (cons resource name))
                                  resources)))
              (setq assignments
                    (ende-chat-config--put
                     tool (delete-dups (append current grants)) assignments)))))
        (plist-put configuration :grants assignments)))))

(defun ende-chat-config--apply-form (form configuration)
  "Apply supported operation FORM to CONFIGURATION."
  (unless (ende-chat-config--proper-list-p form)
    (ende-chat-config--error "configuration form must be a list: %S" form))
  (pcase (car form)
    ('root
     (unless (and (= (length form) 2) (stringp (cadr form)))
       (ende-chat-config--error "root expects one path string"))
     (plist-put configuration :root
                (ende-chat-config--canonical-root (cadr form))))
    ('podman-image
     (unless (= (length form) 2)
       (ende-chat-config--error "podman-image expects one image string"))
     (plist-put configuration :podman-image
                (ende-chat-config--podman-image (cadr form))))
    ('spawn-agent-config
     (unless (and (= (length form) 2)
                  (symbolp (cadr form))
                  (not (keywordp (cadr form))))
       (ende-chat-config--error "spawn-agent-config expects one name"))
     (plist-put configuration :spawn-agent-config
                (ende-chat-config--name (cadr form) "agent config")))
    ('provider
     (unless (and (= (length form) 2) (symbolp (cadr form))
                  (not (keywordp (cadr form)))
                  (memq (cadr form) (ende-provider-names)))
       (ende-chat-config--error "provider expects a configured provider name"))
     (plist-put configuration :provider (cadr form)))
    ('model
     (unless (and (= (length form) 2) (stringp (cadr form))
                  (not (string-empty-p (cadr form))))
       (ende-chat-config--error "model expects one non-empty model string"))
     (plist-put configuration :model (cadr form)))
    ('group (ende-chat-config--define-group form configuration))
    ('resource (ende-chat-config--define-resource form configuration))
    ('tools
     (unless (and (>= (length form) 2)
                  (cl-every #'symbolp (cdr form)))
       (ende-chat-config--error "tools expects one or more tool or group names"))
     (dolist (name (cdr form))
       (ende-chat-config--target-tools name configuration))
     (let ((selected (ende-chat-config--resolve-tools
                      (cdr form) configuration)))
       (when (and (plist-get ende-chat-config--block-state :restricted)
                  (memq 'run_host_command selected))
         (ende-chat-config--error
          "enable run_host_command before restricting it"))
       (setq configuration
             (plist-put configuration :active-targets
                        (delete-dups
                         (append (plist-get configuration :active-targets)
                                 (cdr form)))))
       (setq configuration
             (plist-put configuration :excluded-tools
                        (cl-set-difference
                         (plist-get configuration :excluded-tools) selected)))
       (when (and (plist-get ende-chat-config--block-state :restricted)
                  (memq 'run_host_command selected))
         (ende-chat-config--error
          "enable run_host_command before restricting it"))
       (when (memq 'run_host_command selected)
         (setq configuration
               (plist-put configuration :host-command-policy :unrestricted))
         (setq ende-chat-config--block-state
               (plist-put ende-chat-config--block-state :host-enabled-local t)))
       configuration))
    ('clear-tools
     (unless (cl-every (lambda (name)
                         (and (symbolp name) (not (keywordp name))))
                       (cdr form))
       (ende-chat-config--error "clear-tools expects tool names"))
     (if (null (cdr form))
         (progn
           ;; Mandatory tools are added during normalization, not inherited here.
           (setq configuration
                 (plist-put configuration :active-targets nil))
           (setq configuration
                 (plist-put configuration :excluded-tools nil))
           (setq ende-chat-config--block-state
                 (plist-put ende-chat-config--block-state
                            :host-enabled-local nil))
           configuration)
       (let ((tools (mapcar #'ende-chat-config--tool-name (cdr form))))
         (setq configuration
               (plist-put configuration :excluded-tools
                          (delete-dups
                           (append (plist-get configuration :excluded-tools)
                                   tools))))
         (when (memq 'run_host_command tools)
           (setq ende-chat-config--block-state
                 (plist-put ende-chat-config--block-state
                            :host-enabled-local nil)))
         configuration)))
    ('restrict-host-commands
     (when (plist-get ende-chat-config--block-state :restricted)
       (ende-chat-config--error
        "only one restrict-host-commands form is allowed per block"))
     (unless (or (ende-chat-config--host-command-enabled-p configuration)
                 (plist-get ende-chat-config--block-state :host-enabled-local))
       (ende-chat-config--error
        "restrict-host-commands requires enabled run_host_command"))
     (let ((policy (ende-chat-config--host-command-rules (cdr form))))
       (setq ende-chat-config--block-state
             (plist-put ende-chat-config--block-state :restricted t))
       (plist-put configuration :host-command-policy policy)))
    ('set-tool-resources
     (ende-chat-config--assign-resources form configuration nil))
    ('add-tool-resources
     (ende-chat-config--assign-resources form configuration t))
    ('clear-tool-resources
     (unless (= (length form) 1)
       (ende-chat-config--error "clear-tool-resources takes no arguments"))
     (plist-put configuration :grants nil))
    (_ (ende-chat-config--error "unsupported form: %S" (car form)))))

(defun ende-chat-config--empty ()
  "Return fresh resolver state, including activation and grant provenance."
  (list :root nil :podman-image nil :groups nil :resources nil
        :provider nil :model nil
        :active-targets nil :excluded-tools nil
        :host-command-policy :unrestricted
        :grants nil :revoked-resources nil))

(defun ende-chat-config--normalize (configuration)
  "Convert resolver state CONFIGURATION to a runtime access policy."
  (list :root (plist-get configuration :root)
        :provider (plist-get configuration :provider)
        :model (plist-get configuration :model)
        :podman-image (plist-get configuration :podman-image)
        :groups (plist-get configuration :groups)
        :resources (plist-get configuration :resources)
        :active-tools
        (ende-tool-active-names
         (cl-set-difference
          (ende-chat-config--resolve-tools
           (plist-get configuration :active-targets) configuration)
          (plist-get configuration :excluded-tools)))
        :host-command-policy
        (if (memq 'run_host_command
                  (cl-set-difference
                   (ende-chat-config--resolve-tools
                    (plist-get configuration :active-targets) configuration)
                   (plist-get configuration :excluded-tools)))
            (plist-get configuration :host-command-policy)
          :inactive)
        :spawn-agent-config (plist-get configuration :spawn-agent-config)
        :tool-resources
        (mapcar (lambda (entry)
                  (cons (car entry) (delete-dups (mapcar #'car (cdr entry)))))
                (plist-get configuration :grants))))

(defun ende-chat-config--validate-access (normalized configuration)
  "Check active tools in NORMALIZED using CONFIGURATION for diagnostics."
  (when (cl-some (lambda (tool-name)
                   (ende-tool-requires-root (ende-tool--find tool-name)))
                 (plist-get normalized :active-tools))
    (unless (plist-get normalized :root)
      (ende-chat-config--error "an explicit root is required by the active tools")))
  (dolist (tool (plist-get normalized :active-tools))
    (when (and (ende-tool-requires-resources (ende-tool--find tool))
               (null (cdr (assq tool (plist-get normalized :tool-resources)))))
      (let ((revoked (cdr (assq tool (plist-get configuration
						:revoked-resources)))))
        (ende-chat-config--error
         (concat "active tool %s has no assigned resources%s; "
                 "use (set-tool-resources %s :resources RESOURCE)")
         tool
         (if revoked
             (format " after resource redefinition revoked %s"
                     (mapconcat #'symbol-name revoked ", "))
           "")
         tool))))
  (when (and (memq 'run_sandbox_command (plist-get normalized :active-tools))
             (not (plist-get normalized :podman-image)))
    (ende-chat-config--error
     "active tool run_sandbox_command requires a digest-pinned podman-image")))

(defun ende-chat-config--apply-blocks (blocks inherited &optional supplied-forms)
  "Apply BLOCKS over INHERITED and return the resulting configuration."
  (when (> (length blocks) 1)
    (ende-chat-config--error "only one config block is allowed per Org node"))
  (if (null blocks)
      inherited
    (let* ((configuration (ende-chat-config--copy inherited))
           (ende-chat-config--block-line
            (line-number-at-pos (org-element-property :begin (car blocks)) t))
           (forms (or supplied-forms
                      (ende-chat-config--block-forms (car blocks))))
           (root-count 0)
           (image-count 0)
           (spawn-config-count 0)
           (agent-config-names nil)
           (ende-chat-config--block-state
            (list :restricted nil :host-enabled-local nil))
           group-names resource-names)
      (unless (string-empty-p
               (string-trim
                (or (org-element-property :parameters (car blocks)) "")))
        (ende-chat-config--error "ende blocks do not accept parameters"))
      ;; Allow forward references.
      (dolist (form forms)
        (unless (ende-chat-config--proper-list-p form)
          (ende-chat-config--error "configuration form must be a list: %S" form))
        (pcase (car form)
          ('group
           (when (memq (nth 1 form) group-names)
             (ende-chat-config--error "group is defined twice in this block: %s"
                                      (nth 1 form)))
           (setq configuration
                 (ende-chat-config--define-group form configuration))
           (push (nth 1 form) group-names))
          ('resource
           (when (memq (nth 1 form) resource-names)
             (ende-chat-config--error "resource is defined twice in this block: %s"
                                      (nth 1 form)))
           (setq configuration
                 (ende-chat-config--define-resource form configuration))
           (push (nth 1 form) resource-names))
          ('root
           (cl-incf root-count)
           (when (> root-count 1)
             (ende-chat-config--error "only one root form is allowed per block"))
           (setq configuration
                 (ende-chat-config--apply-form form configuration)))
          ('podman-image
           (cl-incf image-count)
           (when (> image-count 1)
             (ende-chat-config--error
              "only one podman-image form is allowed per block"))
           (setq configuration
                 (ende-chat-config--apply-form form configuration)))
          ('agent-config
           (let ((definition (ende-chat-config--agent-definition form)))
             (when (assq (car definition) agent-config-names)
               (ende-chat-config--error
                "agent-config is defined twice in this block: %s"
                (car definition)))
             (push definition agent-config-names)))))
      (dolist (form forms)
        (unless (memq (car-safe form)
                      '(root podman-image group resource agent-config))
          (when (eq (car-safe form) 'spawn-agent-config)
            (cl-incf spawn-config-count)
            (when (> spawn-config-count 1)
              (ende-chat-config--error
               "spawn-agent-config is repeated in this block")))
          (setq configuration
                (ende-chat-config--apply-form form configuration))))
      (setq configuration
            (plist-put configuration :agent-config-definitions
                       (append (plist-get configuration
                                          :agent-config-definitions)
                               (nreverse agent-config-names))))
      configuration)))

(defun ende-chat-config--normalize-named-policy
    (name body registry-names visible)
  "Normalize isolated named agent policy NAME with symbolic REGISTRY-NAMES.
Copy only group and resource definitions from VISIBLE."
  (let* ((configuration (ende-chat-config--empty))
         (ende-chat-config--block-state
          (list :restricted nil :host-enabled-local nil))
         (group-names nil)
         (resource-names nil)
         (root-count 0)
         (image-count 0)
         (spawn-count 0))
    (setf (plist-get configuration :groups)
          (copy-tree (plist-get visible :groups))
          (plist-get configuration :resources)
          (copy-tree (plist-get visible :resources)))
    (dolist (form body)
      (unless (ende-chat-config--proper-list-p form)
        (ende-chat-config--error "malformed form in agent-config %s: %S"
                                 name form))
      (pcase (car form)
        ('agent-config
         (ende-chat-config--error "nested agent-config definitions are not allowed"))
        ('group
         (when (memq (nth 1 form) group-names)
           (ende-chat-config--error "group is defined twice in agent-config %s: %s"
                                    name (nth 1 form)))
         (setq configuration
               (ende-chat-config--define-group form configuration))
         (push (nth 1 form) group-names))
        ('resource
         (when (memq (nth 1 form) resource-names)
           (ende-chat-config--error "resource is defined twice in agent-config %s: %s"
                                    name (nth 1 form)))
         (setq configuration
               (ende-chat-config--define-resource form configuration))
         (push (nth 1 form) resource-names))
        ('root
         (cl-incf root-count)
         (when (> root-count 1)
           (ende-chat-config--error "root is repeated in agent-config %s" name))
         (setq configuration
               (ende-chat-config--apply-form form configuration)))
        ('podman-image
         (cl-incf image-count)
         (when (> image-count 1)
           (ende-chat-config--error "podman-image is repeated in agent-config %s"
                                    name))
         (setq configuration
               (ende-chat-config--apply-form form configuration)))
        ((or 'provider 'model)
         (setq configuration
               (ende-chat-config--apply-form form configuration)))))
    (dolist (form body)
      (unless (memq (car-safe form)
                    '(group resource root podman-image provider model))
        (when (eq (car-safe form) 'spawn-agent-config)
          (cl-incf spawn-count)
          (when (> spawn-count 1)
            (ende-chat-config--error
             "spawn-agent-config is repeated in agent-config %s" name)))
        (setq configuration
              (ende-chat-config--apply-form form configuration))))
    (setq configuration (ende-chat-config--validate-grant-types configuration))
    (let ((normalized (ende-chat-config--normalize configuration)))
      (ende-chat-config--validate-access normalized configuration)
      (when (and (memq 'spawn_agent (plist-get normalized :active-tools))
                 (null (plist-get configuration :spawn-agent-config)))
        (ende-chat-config--error
         "agent-config %s enables spawn_agent without a selected policy" name))
      (when (and (plist-get configuration :spawn-agent-config)
                 (not (memq (plist-get configuration :spawn-agent-config)
                            registry-names)))
        (ende-chat-config--error "unknown agent-config %s"
                                 (plist-get configuration :spawn-agent-config)))
      normalized)))

(defun ende-chat-config--capability-policy-lines (label policy)
  "Return deterministic capability lines for LABEL and normalized POLICY."
  (let ((tools (sort (mapcar #'symbol-name
                            (copy-sequence (plist-get policy :active-tools)))
                     #'string<))
        (resources (sort (copy-sequence (plist-get policy :tool-resources))
                         (lambda (left right)
                           (string< (symbol-name (car left))
                                    (symbol-name (car right))))))
        (lines nil))
    (push (format "%s direct tools: %s" label
                  (if tools (mapconcat #'identity tools ", ") "none"))
          lines)
    (when (plist-get policy :root)
      (push (format "%s root: %s" label (plist-get policy :root)) lines))
    (when (plist-get policy :provider)
      (push (format "%s provider: %s" label (plist-get policy :provider))
            lines))
    (when (plist-get policy :model)
      (push (format "%s model: %s" label (plist-get policy :model)) lines))
    (dolist (grant resources)
      (let* ((names (sort (delete-dups (copy-sequence (cdr grant)))
                          (lambda (left right)
                            (string< (symbol-name left) (symbol-name right)))))
             (resource-lines
              (mapcar
               (lambda (name)
                 (let ((resource (ende-chat-config--lookup
                                  name (plist-get policy :resources))))
                   (format "%s resource %s: %s root=%s include=%s exclude=%s%s"
                           label name (plist-get resource :type)
                           (or (plist-get resource :root) "project root")
                           (mapconcat #'identity
                                      (plist-get resource :include) ", ")
                           (if (plist-get resource :exclude)
                               (mapconcat #'identity
                                          (plist-get resource :exclude) ", ")
                             "none")
                           (if (plist-get resource :allow-private-addresses)
                               " private-addresses=yes" ""))))
               names)))
        (setq lines
              (nconc (nreverse resource-lines)
                     (cons (format "%s grants for %s: %s" label (car grant)
                                   (mapconcat #'symbol-name names ", "))
                           lines)))))
    (when (plist-get policy :root)
      (push (format "%s project root: %s" label (plist-get policy :root))
            lines))
    (when (plist-get policy :host-command-policy)
      (let ((command-policy (plist-get policy :host-command-policy)))
        (when (and (listp command-policy)
                   (plist-get command-policy :restricted))
          (push (format "%s host commands: open=%s fixed=%s prefix=%s" label
                        (or (mapconcat #'identity
                                       (plist-get command-policy :open) ", ")
                            "none")
                        (or (mapconcat
                             (lambda (argv) (mapconcat #'identity argv " "))
                             (plist-get command-policy :fixed) "; ")
                            "none")
                        (or (mapconcat
                             (lambda (argv) (mapconcat #'identity argv " "))
                             (plist-get command-policy :prefix) "; ")
                            "none"))
                lines))))
    (when (memq 'run_host_command (plist-get policy :active-tools))
      (push (format "%s host-command policy: %s" label
                    (plist-get policy :host-command-policy)) lines))
    (when (memq 'run_sandbox_command (plist-get policy :active-tools))
      (push (format "%s sandbox image: %s" label
                    (or (plist-get policy :podman-image) "missing")) lines))
    (nreverse lines)))

(defun ende-chat-config-capabilities (environment)
  "Format canonical model-visible capabilities in frozen ENVIRONMENT."
  (let* ((policy (plist-get environment :access-policy))
         (registry (plist-get environment :registry))
         (selected (plist-get environment :spawn-agent-config))
         (direct (ende-chat-config--capability-policy-lines "You" policy))
         (delegated
          (cl-loop for (name . named-policy) in registry
                   append
                   (append
                    (list (format "agent-config %s:" name))
                    (ende-chat-config--capability-policy-lines
                     (format "  %s" name) named-policy)
                    (list (format "  spawn-agent-config: %s"
                                  (or (plist-get named-policy
                                                 :spawn-agent-config)
                                      "none")))))))
    (mapconcat #'identity
               (append (list "Direct capabilities:") direct
                       (list "Capabilities available through configured delegation:")
                       delegated
                       (when selected
                         (list (format "Selected child policy: %s" selected)))
                       (when (memq 'spawn_agent (plist-get policy :active-tools))
                         (list (concat "Children may run concurrently. Writes are "
                                       "not coordinated; avoid assigning overlapping "
                                       "mutations unless that is intentional."))))
               "\n")))

(defun ende-chat-config--agent-registry (definitions visible)
  "Normalize agent DEFINITIONS using definitions from VISIBLE."
  (let ((names (mapcar #'car definitions)))
    (mapcar
     (lambda (definition)
       (cons (car definition)
             (ende-chat-config--normalize-named-policy
              (car definition) (cdr definition) names visible)))
     definitions)))

(defun ende-chat-config--merge-agent-definitions (current additions)
  "Replace same-name CURRENT definitions with ADDITIONS in scope order."
  (dolist (definition additions current)
    (setq current
          (cons definition (assq-delete-all (car definition) current)))))

(defun ende-chat-config-resolve-environment (tree parent-table headline)
  "Resolve root policy and isolated visible agent registry for HEADLINE."
  (let* ((blocks-by-node (ende-chat-config--local-blocks tree))
         (ancestry nil)
         (current headline)
         (configuration (ende-chat-config--empty))
         (definitions nil))
    (while current
      (push current ancestry)
      (setq current (gethash current parent-table)))
    (dolist (node (cons tree ancestry))
      (let ((blocks (gethash node blocks-by-node)))
        (when blocks
          (let* ((forms (ende-chat-config--block-forms (car blocks)))
                 (policy-forms
                  (cl-remove-if
                   (lambda (form) (eq (car-safe form) 'agent-config))
                   forms))
                 (registry-forms
                  (cl-remove-if-not
                   (lambda (form) (eq (car-safe form) 'agent-config))
                   forms))
                 (seen-names nil))
            (dolist (form registry-forms)
              (let ((name (ende-chat-config--name (nth 1 form)
                                                  "agent config")))
                (when (memq name seen-names)
                  (ende-chat-config--error
                   "agent-config is defined twice in this block: %s" name))
                (push name seen-names)))
            (setq configuration
                  (ende-chat-config--apply-blocks
                   blocks configuration policy-forms)
                  definitions
                  (ende-chat-config--merge-agent-definitions
                   definitions
                   (mapcar #'ende-chat-config--agent-definition
                           registry-forms)))))))
    (let* ((registry-names (mapcar #'car definitions))
           (registry
            (ende-chat-config--agent-registry definitions configuration))
           (selected (plist-get configuration :spawn-agent-config))
           (active (memq 'spawn_agent
                         (plist-get (ende-chat-config--normalize configuration)
                                    :active-tools))))
      (when (and selected (not (memq selected registry-names)))
        (ende-chat-config--error "unknown agent-config %s" selected))
      (when (and active (null selected))
        (ende-chat-config--error
         "active spawn_agent requires spawn-agent-config"))
      (setq configuration
            (ende-chat-config--validate-grant-types configuration))
      (let ((policy (ende-chat-config--normalize configuration)))
        (ende-chat-config--validate-access policy configuration)
        (list :access-policy policy
              :spawn-agent-config selected
              :registry registry)))))

(defun ende-chat-config-resolve (tree parent-table headline)
  "Resolve chat tool configuration for HEADLINE in parsed Org TREE.
PARENT-TABLE maps headlines to their parents.  Apply the document block,
ancestor blocks, and HEADLINE's own block, never preceding sibling blocks.
Return a normalized access policy.  Always-available tools remain active even
without config blocks or after `clear-tools'; resource grants cannot disable
them.  Definitions need not be used.  An explicit root is required only when
an active tool needs a project.  Invalid forms or
active tools without required resources or roots signal errors before dispatch."
  (let* ((blocks-by-node (ende-chat-config--local-blocks tree))
         (ancestry nil)
         (current headline)
         (configuration (ende-chat-config--empty)))
    (while current
      (push current ancestry)
      (setq current (gethash current parent-table)))
    (dolist (node (cons tree ancestry))
      (let ((blocks (gethash node blocks-by-node)))
        (when blocks
          (setq configuration
                (ende-chat-config--apply-blocks blocks configuration)))))
    (setq configuration (ende-chat-config--validate-grant-types configuration))
    (let ((normalized (ende-chat-config--normalize configuration)))
      (ende-chat-config--validate-access normalized configuration)
      normalized)))

(provide 'ende-chat-config)

;;; ende-chat-config.el ends here
