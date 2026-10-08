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
         ((memq form-part '(:type :allow-private-addresses))
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
    ('group (ende-chat-config--define-group form configuration))
    ('resource (ende-chat-config--define-resource form configuration))
    ('tools
     (unless (and (>= (length form) 2)
                  (cl-every #'symbolp (cdr form)))
       (ende-chat-config--error "tools expects one or more tool or group names"))
     (dolist (name (cdr form))
       (ende-chat-config--target-tools name configuration))
     (plist-put configuration :active-targets
                (delete-dups
                 (append (plist-get configuration :active-targets)
                         (cdr form)))))
    ('clear-tools
     (unless (= (length form) 1)
       (ende-chat-config--error "clear-tools takes no arguments"))
     ;; Mandatory tools are added during normalization, not inherited here.
     (plist-put configuration :active-targets nil))
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
  (list :root nil :groups nil :resources nil :active-targets nil
        :grants nil :revoked-resources nil))

(defun ende-chat-config--normalize (configuration)
  "Convert resolver state CONFIGURATION to a runtime access policy."
  (list :root (plist-get configuration :root)
        :groups (plist-get configuration :groups)
        :resources (plist-get configuration :resources)
        :active-tools
        (ende-tool-active-names
         (ende-chat-config--resolve-tools
          (plist-get configuration :active-targets) configuration))
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
    (when (and (not (eq (ende-tool-resource-kind (ende-tool--find tool)) 'none))
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
         tool)))))

(defun ende-chat-config--apply-blocks (blocks inherited)
  "Apply BLOCKS over INHERITED and return the resulting configuration."
  (when (> (length blocks) 1)
    (ende-chat-config--error "only one config block is allowed per Org node"))
  (if (null blocks)
      inherited
    (let* ((configuration (ende-chat-config--copy inherited))
           (ende-chat-config--block-line
            (line-number-at-pos (org-element-property :begin (car blocks)) t))
           (forms (ende-chat-config--read-forms
                   (ende-chat-config--block-source (car blocks))))
           (root-count 0)
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
                 (ende-chat-config--apply-form form configuration)))))
      (dolist (form forms)
        (unless (memq (car-safe form) '(root group resource))
          (setq configuration
                (ende-chat-config--apply-form form configuration))))
      configuration)))

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
