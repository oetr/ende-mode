;;; ende-provider-config.el --- Provider configuration values -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'ende-core)
(require 'ende-controls)

(defun ende-provider-config--finite-p (value)
  "Return non-nil when VALUE is a finite real number."
  (and (numberp value)
       (or (integerp value)
           (and (= value value) (< (abs value) 1.0e+INF)))))

(defun ende-provider-config--plist (value context &optional json)
  "Validate VALUE as a unique-key plist in CONTEXT.
When JSON is non-nil, compare keyword and string keys by JSON spelling."
  (unless (ende-controls--proper-plist-p value)
    (error "%s: use a proper, even-length plist" context))
  (let ((seen (make-hash-table :test #'equal)))
    (cl-loop for (key _value) on value by #'cddr
             for name = (if json
                            (cond ((stringp key) key)
                                  ((keywordp key)
                                   (substring (symbol-name key) 1)))
                          (and (keywordp key) key))
             do
             (unless name
               (error "%s: use %s keys" context
                      (if json "keyword or string" "keyword")))
             (when (gethash name seen)
               (error "%s: remove duplicate key %s" context name))
             (puthash name t seen)))
  value)

(defun ende-provider-config-copy (value &optional ancestors)
  "Return a detached copy of configuration VALUE; reject cycles.
ANCESTORS contains the objects on the current traversal path."
  (when (memq value ancestors)
    (error "Provider configuration must not contain cycles"))
  (cond
   ((stringp value) (substring-no-properties value))
   ((consp value)
    (let ((path (cons value ancestors)))
      (cons (ende-provider-config-copy (car value) path)
            (ende-provider-config-copy (cdr value) path))))
   ((vectorp value)
    (apply #'vector
           (mapcar (lambda (element)
                     (ende-provider-config-copy element (cons value ancestors)))
                   value)))
   ((hash-table-p value)
    (let ((copy (make-hash-table :test (hash-table-test value))))
      (maphash (lambda (key element)
                 (puthash (ende-provider-config-copy key (cons value ancestors))
                          (ende-provider-config-copy element (cons value ancestors))
                          copy))
               value)
      copy))
   (t value)))

(defun ende-provider-config--json-value (value ancestors)
  "Encode raw JSON VALUE, rejecting cycles through ANCESTORS."
  (when (memq value ancestors)
    (error "Raw parameters must not contain cycles"))
  (cond
   ((or (null value) (eq value t) (stringp value))
    (if (stringp value) (substring-no-properties value) value))
   ((eq value :json-false) json-false)
   ((ende-provider-config--finite-p value) value)
   ((vectorp value)
    (apply #'vector
           (mapcar (lambda (element)
                     (ende-provider-config--json-value
                      element (cons value ancestors)))
                   value)))
   ((and (hash-table-p value) (zerop (hash-table-count value)))
    (make-hash-table :test #'equal))
   ((consp value)
    (ende-provider-config--json-object value ancestors))
   (t (error "Raw parameter value is not a supported JSON value"))))

(defun ende-provider-config--json-object (parameters ancestors)
  "Encode raw object PARAMETERS, rejecting cycles through ANCESTORS."
  (ende-provider-config--plist parameters "Raw parameters" t)
  (let ((object (make-hash-table :test #'equal)))
    (cl-loop for (key value) on parameters by #'cddr
             for name = (if (keywordp key) (substring (symbol-name key) 1) key)
             do (puthash (copy-sequence name)
                         (ende-provider-config--json-value
                          value (cons parameters ancestors))
                         object))
    object))

(defun ende-provider-config-parameters (parameters &optional protected)
  "Validate PARAMETERS and return a detached JSON object.
PROTECTED contains adapter-owned JSON paths.  Raw objects cannot replace
protected paths or their ancestors.  Recognized credential fields are rejected
at every nesting level; arbitrary values are not guaranteed nonsecret."
  (let ((object (ende-provider-config--json-object parameters nil)))
    (cl-labels
        ((check (value prefix)
           (when (hash-table-p value)
             (maphash
              (lambda (key child)
                (let ((path (append prefix (list key))))
                  (when (string-match-p
                         "\\(?:authorization\\|api[_-]?key\\|access[_-]?token\\|refresh[_-]?token\\|password\\|secret\\|cookie\\|credential\\)"
                         (downcase key))
                    (error "Raw parameters: remove credential-bearing field"))
                  (when (cl-some
                         (lambda (owned)
                           (ende-controls--path-conflict-p path owned))
                         protected)
                    (error "Raw parameters: %s is adapter-owned" key))
                  (check child path)))
              value))
           (when (vectorp value)
             (mapc (lambda (child) (check child prefix)) value))))
      (check object nil))
    object))

(defun ende-provider-config-merge-parameters (base overrides)
  "Merge raw parameter maps BASE and OVERRIDES by normalized top-level key.
Nil OVERRIDES clears BASE.  Nested objects and arrays replace whole values."
  (ende-provider-config-parameters base)
  (ende-provider-config-parameters overrides)
  (when overrides
    (let ((result (ende-provider-config-copy base)))
      (cl-loop for (key value) on overrides by #'cddr
               for name = (ende-controls--json-key-name key)
               do (setq result
                        (cl-loop for (old-key old-value) on result by #'cddr
                                 unless (equal name (ende-controls--json-key-name old-key))
                                 append (list old-key old-value)))
               do (setq result
                        (append result (list (ende-provider-config-copy key)
                                             (ende-provider-config-copy value)))))
      result)))

(provide 'ende-provider-config)
;;; ende-provider-config.el ends here
