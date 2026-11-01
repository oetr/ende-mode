;;; ende-credentials.el --- Exact auth-source bindings -*- lexical-binding: t; -*-

;;; Code:
(require 'auth-source)
(require 'cl-lib)
(require 'subr-x)
(require 'rfc2104)

(defvar ende-credentials--bindings (make-hash-table :test #'eq :weakness 'key))
(defvar ende-credentials--session-key nil)


(defvar auth-source-netrc-cache)

(defun ende-credentials--port (value)
  "Return a numeric stored port VALUE, rejecting aliases and wildcards."
  (cond ((and (integerp value) (< 0 value 65536)) value)
        ((and (stringp value) (string-match-p "\\`[0-9]+\\'" value))
         (let ((port (string-to-number value))) (and (< 0 port 65536) port)))))

(defun ende-credentials--candidates (host port user)
  "Find exact stored HOST, PORT and USER bindings using native backends."
  (let ((auth-source-debug nil) (auth-source-do-cache nil)
        (auth-source-netrc-cache nil) matches)
    (dolist (source auth-sources)
      (let ((backend (auth-source-backend-parse source)))
        (when (cl-every
               (lambda (field)
                 (auth-source-search-collection (slot-value backend (car field)) (cdr field)))
               (list (cons 'host host) (cons 'port (number-to-string port)) (cons 'user user)))
          (unless (member (cons (slot-value backend 'type) (slot-value backend 'search-function))
                          '((netrc . auth-source-netrc-search) (plstore . auth-source-plstore-search)))
            (error "Credential backend cannot prove an exact stored binding"))
          (let ((entries (auth-source-search-backends
                          (list backend) (list :host host :user user :port t)
                          1024 nil nil nil)))
            (when (>= (length entries) 1024)
              (error "Credential search is incomplete"))
            (dolist (entry entries)
              (let ((stored-host (plist-get entry :host))
                    (stored-user (plist-get entry :user))
                    (stored-port (ende-credentials--port (plist-get entry :port))))
                (unless (and (stringp stored-host) (equal stored-host host)
                             (stringp stored-user) (equal stored-user user) stored-port)
                  (error "Credential record contains a wildcard or unverifiable binding"))
                (when (= stored-port port)
                  (push (cons (slot-value backend 'source) entry) matches))))))))
    matches))

(defun ende-credentials--resolve (profile)
  "Resolve exactly one native auth-source record for PROFILE.
Never serialize backend records or backend errors into diagnostics."
  (condition-case nil
      (let* ((entries (ende-credentials--candidates
                       (plist-get profile :auth-source-host)
                       (plist-get profile :auth-source-port)
                       (plist-get profile :auth-source-user))))
        (unless (= (length entries) 1)
          (error "Missing or ambiguous credentials"))
        (let* ((value (plist-get (cdar entries) :secret))
               (secret (if (functionp value) (funcall value) value)))
          (unless (and (stringp secret) (not (string-empty-p secret))
                       (not (string-match-p "[[:cntrl:]]" secret)))
            (error "Invalid credential"))
          (cons (caar entries) secret)))
    ((error quit) (error "No unique exact auth-source credential; check endpoint host, numeric port, user, and backend"))))

(defun ende-credentials--signature (secret)
  "Return a process-private keyed signature of SECRET."
  (unless ende-credentials--session-key
    (setq ende-credentials--session-key
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally "/dev/urandom" nil nil 32)
            (buffer-string))))
  (rfc2104-hash (lambda (value) (secure-hash 'sha256 value))
                64 32 ende-credentials--session-key secret))

(defun ende-credentials--binding (profile record)
  "Return private continuity state for PROFILE and resolved RECORD."
  (and record
       (list (car record) (plist-get profile :auth-source-host)
             (plist-get profile :auth-source-port) (plist-get profile :auth-source-user)
             (ende-credentials--signature (cdr record)))))

(defun ende-credentials-capture (profile)
  "Return detached PROFILE bound to its current credential, without a secret.
Unprovable credentials stay unusable even if a credential later appears."
  (let ((copy (copy-tree profile)))
    (when (and (eq (plist-get profile :auth-method) 'auth-source)
               (not (plist-member profile :credential-binding)))
      (let* ((record (condition-case nil (ende-credentials--resolve profile) (error nil)))
             (state (ende-credentials--binding profile record)) id)
        (when state
          (maphash (lambda (key value) (when (equal state value) (setq id key)))
                   ende-credentials--bindings))
        (unless id
          (setq id (make-symbol "ende-credential"))
          (puthash id state ende-credentials--bindings))
        (setq copy (plist-put copy :credential-binding id))))
    copy))

(defun ende-credentials-secret (profile)
  "Resolve PROFILE, rejecting replacement of a captured credential binding."
  (let* ((record (ende-credentials--resolve profile))
         (id (plist-get profile :credential-binding)))
    (when (plist-member profile :credential-binding)
      (unless (equal (gethash id ende-credentials--bindings)
                     (ende-credentials--binding profile record))
        (error "Queued credential binding changed or cannot be verified; submit a new request")))
    (cdr record)))

(provide 'ende-credentials)
;;; ende-credentials.el ends here
