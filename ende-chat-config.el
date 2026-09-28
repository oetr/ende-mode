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
(require 'ende-tools)

(defun ende-chat-config--error (format-string &rest arguments)
  "Signal a configuration error formatted with FORMAT-STRING and ARGUMENTS."
  (error "ende chat config: %s" (apply #'format format-string arguments)))

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

(defun ende-chat-config--pattern (value resource)
  "Validate resource path pattern VALUE for RESOURCE."
  (unless (and (stringp value)
               (not (string-empty-p value))
               (not (file-name-absolute-p value))
               (not (member ".." (split-string value "/" t))))
    (ende-chat-config--error
     "invalid path pattern in resource %s: %S" resource value))
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

(defun ende-chat-config--define-group (form configuration)
  "Add group definition FORM to CONFIGURATION."
  (unless (and (ende-chat-config--proper-list-p form) (>= (length form) 3))
    (ende-chat-config--error "malformed group definition: %S" form))
  (let* ((name (ende-chat-config--name (nth 1 form) "group"))
         (groups (plist-get configuration :groups)))
    (when (or (assq name groups) (ende-tool--find name))
      (ende-chat-config--error "group is already defined: %s" name))
    (let ((tools (mapcar #'ende-chat-config--tool-name (cddr form))))
      (setq configuration
            (plist-put configuration :groups
                       (append groups (list (cons name (delete-dups tools)))))))
    configuration))

(defun ende-chat-config--define-resource (form configuration)
  "Add resource definition FORM to CONFIGURATION."
  (unless (and (ende-chat-config--proper-list-p form) (>= (length form) 3))
    (ende-chat-config--error "malformed resource definition: %S" form))
  (let* ((name (ende-chat-config--name (nth 1 form) "resource"))
         (resources (plist-get configuration :resources))
         includes excludes)
    (when (assq name resources)
      (ende-chat-config--error "resource is already defined: %s" name))
    (dolist (rule (cddr form))
      (unless (and (ende-chat-config--proper-list-p rule)
                   (= (length rule) 2)
                   (memq (car rule) '(include exclude)))
        (ende-chat-config--error "malformed rule in resource %s: %S" name rule))
      (let ((pattern (ende-chat-config--pattern (cadr rule) name)))
        (if (eq (car rule) 'include)
            (push pattern includes)
          (push pattern excludes))))
    (unless includes
      (ende-chat-config--error "resource %s needs an explicit include" name))
    (setq includes (nreverse includes)
          excludes (nreverse excludes))
    (setq configuration
          (plist-put configuration :resources
                     (append resources
                             (list (cons name
                                         (list :include includes
                                               :exclude excludes
                                               :include-regexps
                                               (mapcar #'wildcard-to-regexp
                                                       includes)
                                               :exclude-regexps
                                               (mapcar #'wildcard-to-regexp
                                                       excludes)))))))
    configuration))

(defun ende-chat-config--resolve-tools (names configuration)
  "Expand tool or group NAMES using CONFIGURATION's group definitions."
  (let (tools)
    (dolist (raw-name names)
      (let* ((name (ende-chat-config--name raw-name "tool or group"))
             (group (ende-chat-config--lookup
                     name (plist-get configuration :groups))))
        (setq tools
              (append tools (or group (list (ende-chat-config--tool-name name)))))))
    (delete-dups tools)))

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
            (assignments (plist-get configuration :tool-resources)))
        (dolist (tool tools)
          (let ((current (and additive (assq tool assignments)
                              (cdr (assq tool assignments)))))
            (setq assignments
                  (ende-chat-config--put
                   tool (delete-dups (append current resources)) assignments))))
        (plist-put configuration :tool-resources assignments)))))

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
     (plist-put configuration :active-tools
                (delete-dups
                 (append (plist-get configuration :active-tools)
                         (ende-chat-config--resolve-tools
                          (cdr form) configuration)))))
    ('clear-tools
     (unless (= (length form) 1)
       (ende-chat-config--error "clear-tools takes no arguments"))
     (plist-put configuration :active-tools nil))
    ('set-tool-resources
     (ende-chat-config--assign-resources form configuration nil))
    ('add-tool-resources
     (ende-chat-config--assign-resources form configuration t))
    ('clear-tool-resources
     (unless (= (length form) 1)
       (ende-chat-config--error "clear-tool-resources takes no arguments"))
     (plist-put configuration :tool-resources nil))
    (_ (ende-chat-config--error "unsupported form: %S" (car form)))))

(defun ende-chat-config--empty ()
  "Return an empty inherited configuration."
  '(:root nil :groups nil :resources nil :active-tools nil
    :tool-resources nil))

(defun ende-chat-config--apply-blocks (blocks inherited)
  "Apply BLOCKS over INHERITED and return the resulting configuration."
  (when (> (length blocks) 1)
    (ende-chat-config--error "only one config block is allowed per Org node"))
  (if (null blocks)
      inherited
    (let* ((configuration (ende-chat-config--copy inherited))
           (forms (ende-chat-config--read-forms
                   (ende-chat-config--block-source (car blocks))))
           (root-count 0))
      (unless (string-empty-p
               (string-trim
                (or (org-element-property :parameters (car blocks)) "")))
        (ende-chat-config--error "ende blocks do not accept parameters"))
      ;; Definitions are visible throughout their block, independent of order.
      (dolist (form forms)
        (unless (ende-chat-config--proper-list-p form)
          (ende-chat-config--error "configuration form must be a list: %S" form))
        (pcase (car form)
          ('group
           (setq configuration
                 (ende-chat-config--define-group form configuration)))
          ('resource
           (setq configuration
                 (ende-chat-config--define-resource form configuration)))
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
PARENT-TABLE maps headlines to their parents.  Return an empty normalized
configuration when no block applies, or a resolved configuration with a
canonical root.  A configured scope without an explicit inherited or local
root, or any other invalid form, signals an error."
  (let* ((blocks-by-node (ende-chat-config--local-blocks tree))
         (ancestry nil)
         (current headline)
         (configuration (ende-chat-config--empty))
         (configured nil))
    (while current
      (push current ancestry)
      (setq current (gethash current parent-table)))
    (dolist (node (cons tree ancestry))
      (let ((blocks (gethash node blocks-by-node)))
        (when blocks
          (setq configured t
                configuration
                (ende-chat-config--apply-blocks blocks configuration)))))
    (when (and configured (null (plist-get configuration :root)))
      (ende-chat-config--error "an explicit root is required"))
    configuration))

(provide 'ende-chat-config)

;;; ende-chat-config.el ends here
