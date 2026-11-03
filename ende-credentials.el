;;; ende-credentials.el --- Exact auth-source bindings -*- lexical-binding: t; -*-

;;; Code:
(require 'auth-source)
(require 'cl-lib)
(require 'subr-x)
(require 'rfc2104)
(require 'auth-source-pass)

(defvar ende-credentials--bindings (make-hash-table :test #'eq :weakness 'key))
(defvar ende-credentials--session-key nil)


(defvar auth-source-netrc-cache)

(defun ende-credentials--port (value)
  "Return a numeric stored port VALUE, rejecting aliases and wildcards."
  (cond ((and (integerp value) (< 0 value 65536)) value)
        ((and (stringp value) (string-match-p "\\`[0-9]+\\'" value))
         (let ((port (string-to-number value))) (and (< 0 port 65536) port)))))

(defun ende-credentials--discover-backends ()
  "Return parsed, non-ignored auth-source backends."
  (let (backends)
    (dolist (source auth-sources)
      (let ((backend (condition-case nil (auth-source-backend-parse source) (error nil))))
        (when (and backend (not (eq (slot-value backend 'type) 'ignore)))
          (push backend backends))))
    (nreverse backends)))

(defun ende-credentials--backend-name (backend)
  "Return a safe diagnostic name for BACKEND."
  (let ((type (slot-value backend 'type)))
    (cond ((memq type '(netrc plstore secrets password-store)) (symbol-name type))
          ((eq type 'ignore) "ignored")
          (t "custom"))))

(defun ende-credentials--searchable-p (backend host user)
  "Return non-nil if BACKEND can search HOST and USER exactly."
  (cl-every
   (lambda (field)
     (auth-source-search-collection (slot-value backend (car field)) (cdr field)))
   (list (cons 'host host) (cons 'user user))))

(defun ende-credentials--search-backend (backend host user)
  "Search BACKEND for HOST and USER, leaving port verification to its adapter."
  (let ((auth-source-debug nil))
    (condition-case nil
        (auth-source-search-backends
         (list backend) (list :host host :user user :port t) 1024 nil nil nil)
      (error
       (error "Credential search failed for auth-source backend %s"
              (ende-credentials--backend-name backend))))))

(defun ende-credentials--adapter (backend)
  "Return the identity adapter for BACKEND, or nil when unsupported."
  (let ((type (slot-value backend 'type))
        (search (slot-value backend 'search-function)))
    (cond ((and (eq type 'netrc) (eq search 'auth-source-netrc-search)) 'netrc)
          ((and (eq type 'plstore) (eq search 'auth-source-plstore-search)) 'plstore))))

(defun ende-credentials--stored-binding (backend entry)
  "Independently extract a stored identity from BACKEND's ENTRY."
  (unless (ende-credentials--adapter backend)
    (error "Unsupported auth-source backend %s: stored host, port, and user cannot be verified"
           (ende-credentials--backend-name backend)))
  (let ((host (plist-get entry :host))
        (port (ende-credentials--port (plist-get entry :port)))
        (user (plist-get entry :user)))
    (unless (and (stringp host) (stringp user) port)
      (error "Unverifiable stored identity in auth-source backend %s"
             (ende-credentials--backend-name backend)))
    (list :stored-host host :stored-port port :stored-user user)))

(defun ende-credentials--verified-binding-p (binding host port user)
  "Return non-nil when BINDING exactly proves HOST, PORT, and USER."
  (and (equal (plist-get binding :stored-host) host)
       (equal (plist-get binding :stored-port) port)
       (equal (plist-get binding :stored-user) user)))

(defun ende-credentials--secret (entry)
  "Extract the secret from ENTRY, suppressing backend diagnostics."
  (let ((value (plist-get entry :secret)))
    (condition-case nil
        (if (functionp value) (funcall value) value)
      (error (error "Stored auth-source secret is unavailable")))))

(defun ende-credentials--candidates (host port user)
  "Find exact stored HOST, PORT and USER bindings using verified adapters."
  (unless (and (stringp host) (stringp user) (ende-credentials--port port))
    (error "Credential lookup requires a host, user, and numeric port"))
  (let ((auth-source-do-cache nil)
        (auth-source-netrc-cache nil)
        matches)
    (dolist (backend (ende-credentials--discover-backends))
      (when (ende-credentials--searchable-p backend host user)
        (unless (ende-credentials--adapter backend)
          (error "Unsupported auth-source backend %s: stored host, numeric port, and user cannot be verified"
                 (ende-credentials--backend-name backend)))
        (let ((entries (ende-credentials--search-backend backend host user)))
          (unless (listp entries)
            (error "Invalid credential search results for auth-source backend %s"
                   (ende-credentials--backend-name backend)))
          (when (>= (length entries) 1024)
            (error "Credential search is incomplete for auth-source backend %s"
                   (ende-credentials--backend-name backend)))
          (dolist (entry entries)
            (let ((binding (ende-credentials--stored-binding backend entry)))
              (unless (and (stringp (plist-get binding :stored-host))
                           (stringp (plist-get binding :stored-user)))
                (error "Unverifiable stored host or user in auth-source backend %s"
                       (ende-credentials--backend-name backend)))
              (unless (ende-credentials--port (plist-get binding :stored-port))
                (error "Unverifiable stored port in auth-source backend %s"
                       (ende-credentials--backend-name backend)))
              (when (ende-credentials--verified-binding-p binding host port user)
                (push (list :source (slot-value backend 'source)
                            :binding binding
                            :entry entry)
                      matches)))))))
    matches))

(defun ende-credentials--resolve (profile)
  "Resolve exactly one native auth-source record for PROFILE."
  (let* ((host (plist-get profile :auth-source-host))
         (port (plist-get profile :auth-source-port))
         (user (plist-get profile :auth-source-user))
         (entries (ende-credentials--candidates host port user)))
    (unless (= (length entries) 1)
      (error "Missing or ambiguous exact auth-source credential"))
    (let* ((candidate (car entries))
           (secret (ende-credentials--secret (plist-get candidate :entry))))
      (unless (and (stringp secret) (not (string-empty-p secret))
                   (not (string-match-p "[[:cntrl:]]" secret)))
        (error "Invalid or unavailable auth-source credential"))
      (cons (plist-get candidate :source) secret))))

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
