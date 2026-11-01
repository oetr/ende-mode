;;; ende-controls.el --- Small request control encoder for ende -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, llm

;;; Commentary:

;; Pure validation and encoding for the small set of request controls supported
;; by ende provider profiles.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst ende-controls--names
  '(:temperature :thinking :reasoning-effort)
  "Control names understood by the shared encoder.")

(defconst ende-controls--descriptor-keys '(:path :values :choices)
  "Descriptor properties accepted by the shared encoder.")

(defconst ende-controls--reserved-path-keys
  '("model" "messages" "input" "stream" "stream_options" "tools"
    "tool_choice" "max_tokens" "max_output_tokens"
    "max_completion_tokens" "store" "system" "instructions"
    "previous_response_id" "conversation")
  "Request fields owned by protocol builders, not control descriptors.")

(defun ende-controls--proper-plist-p (value)
  "Return non-nil when VALUE is a proper even-length plist."
  (and (proper-list-p value)
       (zerop (% (length value) 2))))

(defun ende-controls--json-key-name (key)
  "Return KEY's JSON spelling for a keyword, symbol, or string key."
  (cond
   ((stringp key) key)
   ((keywordp key) (substring (symbol-name key) 1))
   ((symbolp key) (symbol-name key))))

(defun ende-controls--valid-value-p (control value)
  "Return non-nil when VALUE is a canonical value for CONTROL."
  (or (null value)
      (pcase control
        (:temperature (numberp value))
        (:thinking (memq value '(on off)))
        (:reasoning-effort
         (and (stringp value) (not (string-empty-p value)))))))

(defun ende-controls--validate-canonical-value (control value context)
  "Signal when VALUE is invalid for CONTROL in CONTEXT."
  (unless (ende-controls--valid-value-p control value)
    (error "%s for %s must be %s"
           context control
           (pcase control
             (:temperature "nil or numeric")
             (:thinking "nil, on, or off")
             (:reasoning-effort "nil or a nonempty string")))))

(defun ende-controls--validate-descriptor (control descriptor &optional partial)
  "Validate nonempty DESCRIPTOR for CONTROL.
When PARTIAL is non-nil, permit inherited descriptors to supply :path."
  (unless (ende-controls--proper-plist-p descriptor)
    (error "Malformed %s control descriptor: expected a plist" control))
  (let ((seen nil))
    (cl-loop for (key _value) on descriptor by #'cddr do
             (unless (memq key ende-controls--descriptor-keys)
               (error "Unknown %s control descriptor property: %S"
                      control key))
             (when (memq key seen)
               (error "Duplicate %s control descriptor property: %S"
                      control key))
             (push key seen)))
  (if (not (plist-member descriptor :path))
      (unless partial
        (error "%s control descriptor must include :path" control))
    (let ((path (plist-get descriptor :path)))
      (unless (and (proper-list-p path) path
                   (cl-every (lambda (key)
                               (and (stringp key)
                                    (not (string-empty-p key))))
                             path))
        (error "%s control :path must be a nonempty list of nonempty strings"
               control))
      (when-let* ((reserved (and (member (car path)
                                         ende-controls--reserved-path-keys)
                                 (car path))))
        (error "%s control path may not modify request field %s"
               control reserved))))
  (when (plist-member descriptor :choices)
    (let ((choices (plist-get descriptor :choices)))
      (unless (and (proper-list-p choices) choices)
        (error "%s control :choices must be a nonempty list" control))
      (let ((seen nil))
        (dolist (choice choices)
          (when (null choice)
            (error "%s control :choices may not contain nil" control))
          (ende-controls--validate-canonical-value
           control choice ":choices entry")
          (when (member choice seen)
            (error "Duplicate %s control choice: %S" control choice))
          (push choice seen)))))
  (when (plist-member descriptor :values)
    (let ((values (plist-get descriptor :values))
          (seen nil))
      (unless (or (null values) (proper-list-p values))
        (error "%s control :values must be an alist" control))
      (dolist (entry values)
        (unless (and (consp entry)
                     (ende-controls--valid-value-p control (car entry)))
          (error "Malformed %s control :values entry: %S" control entry))
        (when (member (car entry) seen)
          (error "Duplicate %s control :values entry: %S"
                 control (car entry)))
        (push (car entry) seen)))))

(defun ende-controls--path-conflict-p (left right)
  "Return non-nil when JSON paths LEFT and RIGHT overlap."
  (let ((common (cl-loop for l in left
                         for r in right
                         while (equal l r)
                         collect l)))
    (= (length common) (min (length left) (length right)))))

(defun ende-controls--validate-descriptor-list (descriptors partial)
  "Validate DESCRIPTORS, permitting missing paths when PARTIAL is non-nil."
  (unless (or (null descriptors) (proper-list-p descriptors))
    (error ":controls must be a proper list"))
  (let ((seen nil)
        (paths nil))
    (dolist (entry descriptors)
      (unless (and (proper-list-p entry) entry
                   (memq (car entry) ende-controls--names))
        (error "Invalid control descriptor entry: %S" entry))
      (let ((control (car entry))
            (descriptor (cdr entry)))
        (when (memq control seen)
          (error "Duplicate control descriptor: %S" control))
        (push control seen)
        (when descriptor
          (ende-controls--validate-descriptor control descriptor partial)
          (when (plist-member descriptor :path)
            (let ((path (plist-get descriptor :path)))
              (dolist (other paths)
                (when (ende-controls--path-conflict-p path (cdr other))
                  (error "Conflicting control paths for %s and %s"
                         control (car other))))
              (push (cons control path) paths))))))
    descriptors))

(defun ende-controls-validate-descriptors (descriptors)
  "Validate complete DESCRIPTORS and return them unchanged."
  (ende-controls--validate-descriptor-list descriptors nil))

(defun ende-controls-merge-descriptors (base overrides)
  "Merge control descriptor OVERRIDES into BASE.

Entries merge by control name and descriptor property.  A name-only entry
disables that inherited control; list-valued properties are replaced whole."
  (ende-controls-validate-descriptors base)
  (ende-controls--validate-descriptor-list overrides t)
  (let ((result (copy-tree base)))
    (dolist (entry overrides)
      (let* ((control (car entry))
             (descriptor (cdr entry))
             (old (assq control result)))
        (if (null descriptor)
            (setq result (assq-delete-all control result))
          (let ((merged (copy-sequence (cdr old))))
            (cl-loop for (key value) on descriptor by #'cddr do
                     (setq merged (plist-put merged key (copy-tree value))))
            (if old
                (setcdr old merged)
              (setq result (append result (list (cons control merged)))))))))
    (ende-controls-validate-descriptors result)))

(defun ende-controls--object-alist-p (object)
  "Return non-nil when OBJECT is an alist suitable as a JSON object."
  (or (null object)
      (and (proper-list-p object)
           (cl-every (lambda (entry)
                       (and (consp entry)
                            (ende-controls--json-key-name (car entry))))
                     object))))

(defun ende-controls--matching-key-p (entry name)
  "Return non-nil when alist ENTRY has JSON key NAME."
  (equal (ende-controls--json-key-name (car entry)) name))

(defun ende-controls--replace-property (object name value)
  "Return OBJECT with JSON property NAME replaced by VALUE exactly once."
  (unless (ende-controls--object-alist-p object)
    (error "Control path expected a JSON object at %s" name))
  (let* ((existing (cl-find-if
                    (lambda (entry)
                      (ende-controls--matching-key-p entry name))
                    object))
         (key (or (and existing (car existing))
                  (intern (concat ":" name))))
         (inserted nil)
         result)
    (dolist (entry object)
      (if (ende-controls--matching-key-p entry name)
          (unless inserted
            (push (cons key value) result)
            (setq inserted t))
        (push entry result)))
    (if inserted
        (nreverse result)
      (append (nreverse result) (list (cons key value))))))

(defun ende-controls--set-path (object path value)
  "Return OBJECT with PATH set to VALUE, replacing existing JSON keys."
  (if (null (cdr path))
      (ende-controls--replace-property object (car path) value)
    (unless (ende-controls--object-alist-p object)
      (error "Control path expected a JSON object at %s" (car path)))
    (let* ((name (car path))
           (existing (cl-find-if
                      (lambda (entry)
                        (ende-controls--matching-key-p entry name))
                      object))
           (child (and existing (cdr existing))))
      (when (and existing (not (ende-controls--object-alist-p child)))
        (error "Control path cannot enter non-object field %s" name))
      (ende-controls--replace-property
       object name
       (ende-controls--set-path child (cdr path) value)))))

(defun ende-controls--wire-value (control descriptor value)
  "Return VALUE translated by DESCRIPTOR for CONTROL."
  (when (plist-member descriptor :choices)
    (unless (member value (plist-get descriptor :choices))
      (error "%s value %S is unsupported; choose one of %s"
             control value
             (mapconcat (lambda (choice) (format "%s" choice))
                        (plist-get descriptor :choices) ", "))))
  (if (plist-member descriptor :values)
      (let ((mapping (assoc value (plist-get descriptor :values))))
        (unless mapping
          (error "%s value %S has no configured wire mapping" control value))
        (cdr mapping))
    (if (symbolp value) (symbol-name value) value)))

(defun ende-controls-encode (body descriptors values)
  "Return JSON object BODY with active controls from VALUES encoded.

DESCRIPTORS is a validated control descriptor list.  VALUES is a plist whose
control values are canonical values; nil values are omitted."
  (ende-controls-validate-descriptors descriptors)
  (unless (ende-controls--object-alist-p body)
    (error "Control encoder requires a JSON object"))
  (unless (ende-controls--proper-plist-p values)
    (error "Control values must be a plist"))
  (dolist (control ende-controls--names)
    (when (plist-member values control)
      (ende-controls--validate-canonical-value
       control (plist-get values control) "Configured value")))
  (let* ((thinking-off (eq (plist-get values :thinking) 'off))
         (result (copy-tree body)))
    (dolist (control ende-controls--names)
      (let* ((value (plist-get values control))
             (inactive (and (eq control :reasoning-effort) thinking-off)))
        (when (and value (not inactive))
          (let* ((entry (assq control descriptors))
                 (descriptor (cdr entry)))
            (unless descriptor
              (error "Active %s control has no configured JSON mapping"
                     control))
            (setq result
                  (ende-controls--set-path
                   result (plist-get descriptor :path)
                   (ende-controls--wire-value control descriptor value)))))))
    result))

(provide 'ende-controls)

;;; ende-controls.el ends here
