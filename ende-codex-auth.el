;;; ende-codex-auth.el --- Private Codex account records -*- lexical-binding: t; -*-

;;; Commentary:

;; Trusted OpenAI identity validation and private account storage.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'ende-rs256)
(require 'ende-protocol)
(require 'ende-provider-config)
(require 'ende-curl)

(defconst ende-codex-auth--issuer "https://auth.openai.com")
(defconst ende-codex-auth--jwks-uri
  "https://auth.openai.com/.well-known/jwks.json")

(define-error 'ende-codex-identity-error "OpenAI identity could not be verified")

(defun ende-codex-auth--json (bytes)
  "Parse a UTF-8 JSON object from BYTES, rejecting duplicate members."
  (let* ((text (decode-coding-string bytes 'utf-8-unix))
         (value (ende-protocol-json-read-string text))
         (json-object-type 'alist) (json-key-type 'string)
         (json-array-type 'vector) (json-null :null) (json-false :false))
    (unless (and (hash-table-p value)
                 (not (cl-loop for char across text thereis (> char #x10ffff))))
      (signal 'ende-codex-identity-error nil))
    (cl-labels ((check (object)
                  (cond
                   ((vectorp object) (mapc #'check object))
                   ((consp object)
                    (let ((seen (make-hash-table :test #'equal)))
                      (dolist (entry object)
                        (when (gethash (car entry) seen)
                          (signal 'ende-codex-identity-error nil))
                        (puthash (car entry) t seen)
                        (check (cdr entry))))))))
      (check (json-read-from-string text)))
    value))

(defun ende-codex-auth--jwks ()
  "Fetch a bounded key set from the pinned OpenAI HTTPS endpoint."
  (let* ((url (ende-curl--validated-url (list :url ende-codex-auth--jwks-uri)))
         (origin (ende-url-normalize ende-codex-auth--issuer))
         (destination (ende-url-normalize url))
         (ende-connect-timeout 10)
         (ende-max-response-header-bytes 16384)
         (ende-max-stderr-bytes 4096)
         (received 0) resources process overflow)
    (unless (and (equal (plist-get origin :scheme) "https")
                 (equal url (concat ende-codex-auth--issuer "/.well-known/jwks.json"))
                 (equal (plist-get destination :host) "auth.openai.com")
                 (= (plist-get destination :port) 443))
      (signal 'ende-codex-identity-error nil))
    (unwind-protect
        (progn
          (setq resources
                (ende-curl-start-request
                 :profile (list :url url) :name "ende-openai-jwks"
                 :command (lambda (headers _config)
                            (append (ende-curl-base-command url)
                                    (ende-curl-response-options headers)
                                    (ende-curl-timeout-options 15 65536)
                                    (list "--header" "Accept: application/json" "--url" url)))
                 :filter (lambda (proc chunk)
                           (cl-incf received (string-bytes chunk))
                           (if (> received 65536)
                               (progn (setq overflow t) (delete-process proc))
                             (with-current-buffer (process-buffer proc)
                               (goto-char (point-max)) (insert chunk))))
                 :sentinel #'ignore)
                process (plist-get resources :process))
          (set-process-coding-system process 'binary 'binary)
          (with-current-buffer (plist-get resources :response-buffer)
            (set-buffer-multibyte nil))
          (let ((deadline (+ (float-time) 16)))
            (while (and (process-live-p process) (< (float-time) deadline))
              (accept-process-output process 0.05)
              (when (> (file-attribute-size
                        (file-attributes (plist-get resources :headers-file))) 16384)
                (setq overflow t)
                (delete-process process))))
          (unless (and (not overflow) (eq (process-status process) 'exit)
                       (= (process-exit-status process) 0)
                       (= (or (car (ende-curl-response-headers
                                    (plist-get resources :headers-file))) 0) 200))
            (signal 'ende-codex-identity-error nil))
          (let* ((jwks (with-current-buffer (plist-get resources :response-buffer)
                         (ende-codex-auth--json (buffer-string))))
                 (keys (gethash "keys" jwks)))
            (unless (and (vectorp keys) (<= 1 (length keys) 16))
              (signal 'ende-codex-identity-error nil))
            (mapc #'ende-rs256--public-key keys)
            jwks))
      (when (processp process)
        (ende-curl--cancel-idle-timer process))
      (ende-curl-cleanup-resources
       (list (plist-get resources :response-buffer) (plist-get resources :stderr-buffer))
       (list (plist-get resources :headers-file) (plist-get resources :config-file))
       (list process (plist-get resources :stderr-process))))))

(defun ende-codex-auth--claims (claims client-id nonce)
  "Validate authenticated CLAIMS against CLIENT-ID and login NONCE."
  (let* ((now (floor (float-time)))
         (skew 60)
         (audience (gethash "aud" claims))
         (azp (gethash "azp" claims :absent))
         (expires (gethash "exp" claims))
         (issued (gethash "iat" claims))
         (not-before (gethash "nbf" claims :absent)))
    (unless (and (equal (gethash "iss" claims) ende-codex-auth--issuer)
                 (if (stringp audience) (equal audience client-id)
                   (and (vectorp audience) (> (length audience) 0)
                        (cl-every #'ende-codex-auth--text-p audience)
                        (member client-id (append audience nil))
                        (or (= (length audience) 1) (equal azp client-id))))
                 (or (eq azp :absent) (equal azp client-id))
                 (integerp expires) (> expires (- now skew))
                 (integerp issued) (<= 0 issued (+ now skew)) (< issued expires)
                 (or (eq not-before :absent)
                     (and (integerp not-before) (<= 0 not-before (+ now skew))
                          (< not-before expires)))
                 (ende-codex-auth--text-p (gethash "sub" claims))
                 (if nonce (equal (gethash "nonce" claims) nonce)
                   (eq (gethash "nonce" claims :absent) :absent)))
      (signal 'ende-codex-identity-error nil))
    claims))

(defun ende-codex-auth--signed-claims (id-token jwks)
  "Return claims from ID-TOKEN only after RS256 verification with trusted JWKS."
  (unless (and (stringp id-token) (<= (length id-token) 65536))
    (signal 'ende-codex-identity-error nil))
  (let* ((parts (split-string id-token "\\." nil))
         (_ (unless (and (= (length parts) 3)
                         (cl-every (lambda (part) (> (length part) 0)) parts))
              (signal 'ende-codex-identity-error nil)))
         (header (ende-codex-auth--json (ende-rs256--decode (car parts)))))
    (unless (and (equal (gethash "alg" header) "RS256")
                 (ende-rs256--kid-p (gethash "kid" header))
                 (cl-every (lambda (key) (eq (gethash key header :absent) :absent))
                           '("crit" "b64" "jku" "jwk" "x5u")))
      (signal 'ende-codex-identity-error nil))
    (ende-rs256-verify (concat (car parts) "." (cadr parts))
                       (nth 2 parts) (funcall jwks) (gethash "kid" header))
    (ende-codex-auth--json (ende-rs256--decode (cadr parts)))))

(defun ende-codex-auth--identity-guard (function)
  "Call FUNCTION without exposing tokens in errors or the debugger."
  (let ((debug-on-error nil) (debug-on-quit nil) (debug-on-signal nil))
    (condition-case nil
        (funcall function)
      (error (signal 'ende-codex-identity-error nil)))))

(defun ende-codex--verify-id-token (id-token expected-client-id expected-nonce)
  "Authenticate ID-TOKEN for EXPECTED-CLIENT-ID and EXPECTED-NONCE.
Return claims only after signature and OpenID validation."
  (ende-codex-auth--identity-guard
   (lambda ()
     (unless (and (ende-codex-auth--text-p expected-client-id)
                  (ende-codex-auth--text-p expected-nonce))
       (signal 'ende-codex-identity-error nil))
     (ende-codex-auth--claims
      (ende-codex-auth--signed-claims id-token #'ende-codex-auth--jwks)
      expected-client-id expected-nonce))))

(defun ende-codex-auth--verify-refresh (id-token record)
  "Authenticate refreshed ID-TOKEN without changing RECORD's identity."
  (ende-codex-auth--identity-guard
   (lambda ()
     (let* ((keys nil)
            (source (lambda () (or keys (setq keys (ende-codex-auth--jwks)))))
            (old (ende-codex-auth--signed-claims (plist-get record :id-token) source))
            (new (ende-codex-auth--signed-claims id-token source))
            (client (plist-get record :client-id))
            (nonce (when (ende-protocol-json-has-key-p "nonce" new)
                     (let ((original (gethash "nonce" old)))
                       (unless (ende-codex-auth--text-p original)
                         (signal 'ende-codex-identity-error nil))
                       original))))
       (ende-codex-auth--claims new client nonce)
       (unless (and (equal (gethash "iss" old) ende-codex-auth--issuer)
                    (equal (gethash "sub" old) (plist-get record :subject))
                    (equal (gethash "sub" new) (plist-get record :subject))
                    (equal (gethash "aud" new) (gethash "aud" old))
                    (integerp (gethash "iat" old))
                    (>= (gethash "iat" new) (gethash "iat" old)))
         (signal 'ende-codex-identity-error nil))
       new))))

(defun ende-codex-auth--refresh-scope (payload)
  "Reject an explicit loss of identity or refresh scope in token PAYLOAD."
  (when (ende-protocol-json-has-key-p "scope" payload)
    (let ((scope (gethash "scope" payload)))
      (unless (and (stringp scope)
                   (cl-loop for char across scope
                            always (or (= char 32) (= char 33)
                                       (<= 35 char 91) (<= 93 char 126)))
                   (let ((granted (split-string scope " ")))
                     (and (member "openid" granted) (member "offline_access" granted))))
        (signal 'ende-codex-identity-error nil)))))

(defvar ende-codex-auth--directory
  (expand-file-name "ende-codex/" user-emacs-directory)
  "Private local storage directory; never contains user configuration.")

(define-error 'ende-codex-auth-error "Codex credential storage is unavailable or unsafe")
(define-error 'ende-codex-auth-conflict
              "Codex credentials changed; retry from the current registration"
              'ende-codex-auth-error)
(define-error 'ende-codex-auth-busy
              "Codex credential storage is locked by another writer"
              'ende-codex-auth-error)

(defconst ende-codex-auth--fields
  '((:client-id . "client_id") (:subject . "subject") (:host-id . "host_id")
    (:registration-id . "registration_id") (:revision . "revision")
    (:access-token . "access_token") (:refresh-token . "refresh_token")
    (:id-token . "id_token") (:expires-at . "expires_at")
    (:account-id . "account_id")))

(defconst ende-codex-auth--credential-fields
  '(:client-id :subject :access-token :refresh-token :id-token :expires-at :account-id))

(defun ende-codex-auth--fail ()
  "Signal a value-free storage error, never including file contents."
  (signal 'ende-codex-auth-error nil))

(defun ende-codex-auth--guard (function)
  "Call FUNCTION without exposing credentials through debugger or errors."
  (let ((debug-on-error nil) (debug-on-quit nil) (debug-on-signal nil))
    (condition-case failure
        (funcall function)
      (ende-codex-auth-error (signal (car failure) nil))
      (error (ende-codex-auth--fail)))))

(defun ende-codex-auth--alias (account)
  "Validate ACCOUNT using the provider's existing :account grammar."
  (ende-provider-config--settings (list :account account) '(:account) "Codex account")
  (symbol-name account))

(defun ende-codex-auth--new-id ()
  "Return a cryptographically random opaque UUID URN."
  (let ((bytes (with-temp-buffer
                 (set-buffer-multibyte nil)
                 (insert-file-contents-literally "/dev/urandom" nil nil 16)
                 (string-to-list (buffer-string)))))
    (unless (= (length bytes) 16) (ende-codex-auth--fail))
    (setf (nth 6 bytes) (logior #x40 (logand #x0f (nth 6 bytes)))
          (nth 8 bytes) (logior #x80 (logand #x3f (nth 8 bytes))))
    (let ((hex (mapconcat (lambda (byte) (format "%02x" byte)) bytes "")))
      (format "urn:uuid:%s-%s-%s-%s-%s"
              (substring hex 0 8) (substring hex 8 12) (substring hex 12 16)
              (substring hex 16 20) (substring hex 20 32)))))

(defun ende-codex-auth--id-p (value)
  "Return non-nil for an opaque UUID URN VALUE."
  (and (stringp value)
       (let ((case-fold-search nil))
         (string-match-p
          "\\`urn:uuid:[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-4[0-9a-f]\\{3\\}-[89ab][0-9a-f]\\{3\\}-[0-9a-f]\\{12\\}\\'"
          value))))

(defun ende-codex-auth--text-p (value)
  "Return non-nil for a nonempty, single-line credential VALUE."
  (and (stringp value) (not (string-blank-p value))
       (not (string-match-p "[[:cntrl:]]" value))))

(defun ende-codex-auth--validate-credentials (credentials)
  "Validate CREDENTIALS without rendering any part of them in errors."
  (unless (and (proper-list-p credentials) (cl-evenp (length credentials)))
    (ende-codex-auth--fail))
  (let (seen)
    (cl-loop for (key _value) on credentials by #'cddr do
             (unless (and (memq key ende-codex-auth--credential-fields)
                          (not (memq key seen)))
               (ende-codex-auth--fail))
             (push key seen)))
  (dolist (key '(:client-id :subject :access-token :refresh-token :id-token))
    (unless (ende-codex-auth--text-p (plist-get credentials key))
      (ende-codex-auth--fail)))
  (unless (and (integerp (plist-get credentials :expires-at))
               (>= (plist-get credentials :expires-at) 0))
    (ende-codex-auth--fail))
  (when (plist-member credentials :account-id)
    (unless (ende-codex-auth--text-p (plist-get credentials :account-id))
      (ende-codex-auth--fail))))

(defun ende-codex-auth--attributes (path directory private)
  "Check existing PATH as DIRECTORY or a regular file.
PRIVATE requires owner-only modes; ancestors must not permit replacement by
other users.  No symbolic links or multiply linked credential files are safe."
  (setq path (directory-file-name path))
  (let ((attributes (file-attributes path 'integer)))
    (when attributes
      (let ((type (file-attribute-type attributes))
            (owner (file-attribute-user-id attributes))
            (modes (file-modes path)))
        (unless (and (if directory (eq type t) (null type))
                     (integerp modes)
                     (if private
                         (and (= owner (user-uid))
                              (= (logand modes #o7777) (if directory #o700 #o600)))
                       (and (memq owner (list 0 (user-uid)))
                            (or (= (logand modes #o022) 0)
                                ;; A trusted sticky ancestor such as /tmp cannot
                                ;; let another user replace our private directory.
                                (/= (logand modes #o1000) 0))))
                     (or directory (= (file-attribute-link-number attributes) 1)))
          (ende-codex-auth--fail))))
    attributes))

(defun ende-codex-auth--ensure-directory (path create private)
  "Validate PATH and its ancestors; CREATE missing directories privately."
  (let ((parent (file-name-directory (directory-file-name path))))
    (unless (equal (directory-file-name path) (directory-file-name parent))
      (ende-codex-auth--ensure-directory parent create nil)))
  (unless (ende-codex-auth--attributes path t private)
    (when create
      (let ((original (default-file-modes)))
        (unwind-protect
            (progn (set-default-file-modes #o700) (make-directory path))
          (set-default-file-modes original)))
      (unless (ende-codex-auth--attributes path t private)
        (ende-codex-auth--fail)))))

(defun ende-codex-auth--path (create)
  "Return checked local storage directory, optionally CREATE it."
  (unless (and (stringp ende-codex-auth--directory)
               (file-name-absolute-p ende-codex-auth--directory)
               (not (file-remote-p ende-codex-auth--directory)))
    (ende-codex-auth--fail))
  (let ((directory (directory-file-name (expand-file-name ende-codex-auth--directory))))
    (ende-codex-auth--ensure-directory directory create t)
    directory))

(defun ende-codex-auth--decode-record (account entry host-id)
  "Decode checked JSON ENTRY for ACCOUNT under installation HOST-ID."
  (unless (hash-table-p entry) (ende-codex-auth--fail))
  (let ((record (list :account account)) credentials)
    (maphash (lambda (key _value)
               (unless (rassoc key ende-codex-auth--fields) (ende-codex-auth--fail)))
             entry)
    (dolist (field ende-codex-auth--fields)
      (let ((value (gethash (cdr field) entry :missing)))
        (unless (eq value :missing)
          (setq record (plist-put record (car field) value))
          (when (memq (car field) ende-codex-auth--credential-fields)
            (setq credentials (plist-put credentials (car field) value))))))
    (ende-codex-auth--validate-credentials credentials)
    (unless (and (equal (plist-get record :host-id) host-id)
                 (ende-codex-auth--id-p (plist-get record :registration-id))
                 (ende-codex-auth--id-p (plist-get record :revision)))
      (ende-codex-auth--fail))
    record))

(defun ende-codex-auth--read-state (directory)
  "Read and validate private JSON state under DIRECTORY, or return nil."
  (let* ((file (expand-file-name "accounts.json" directory))
         (attributes (ende-codex-auth--attributes file nil t)))
    (when attributes
      (when (> (file-attribute-size attributes) (* 1024 1024))
        (ende-codex-auth--fail))
      (let* ((state (with-temp-buffer
                      (insert-file-contents-literally file)
                      (ende-protocol-json-read-string (buffer-string))))
             (host-id (and (hash-table-p state) (gethash "host_id" state)))
             (accounts (and (hash-table-p state) (gethash "accounts" state))))
        (unless (and (hash-table-p state) (= (hash-table-count state) 3)
                     (equal (gethash "version" state) 1)
                     (ende-codex-auth--id-p host-id) (hash-table-p accounts))
          (ende-codex-auth--fail))
        (maphash (lambda (alias entry)
                   (unless (and (stringp alias)
                                (let ((case-fold-search nil))
                                  (string-match-p "\\`[a-z0-9][a-z0-9_-]*\\'" alias)))
                     (ende-codex-auth--fail))
                   (ende-codex-auth--decode-record alias entry host-id))
                 accounts)
        state))))

(defun ende-codex-auth--write-state (directory state)
  "Atomically write STATE beside its destination in private DIRECTORY."
  (let ((file (expand-file-name "accounts.json" directory)) temporary)
    (unwind-protect
        (progn
          (ende-codex-auth--attributes file nil t)
          (setq temporary (make-temp-file (expand-file-name ".accounts-" directory)))
          (set-file-modes temporary #o600)
          (let ((coding-system-for-write 'utf-8-unix)
                (write-region-annotate-functions nil)
                (write-region-post-annotation-function nil))
            (write-region (concat (json-encode state) "\n") nil temporary nil 'silent))
          (ende-codex-auth--attributes temporary nil t)
          (ende-codex-auth--attributes file nil t)
          (rename-file temporary file t)
          (setq temporary nil))
      (when temporary (delete-file temporary)))))

(defun ende-codex-auth--transaction (function)
  "Run FUNCTION with fresh state under an exclusive cross-instance lock.
Never steal an existing lock, including after a crash.  Every writer rereads
under the lock and must still compare the individual account revision."
  (let* ((directory (ende-codex-auth--path t))
         (lock (expand-file-name ".writer-lock" directory)) acquired)
    (unwind-protect
        (progn
          (condition-case nil
              (let ((original (default-file-modes)))
                (unwind-protect
                    (progn (set-default-file-modes #o700) (make-directory lock)
                           (setq acquired t))
                  (set-default-file-modes original)))
            (file-already-exists (signal 'ende-codex-auth-busy nil)))
          (ende-codex-auth--attributes lock t t)
          (let ((state (or (ende-codex-auth--read-state directory)
                           (let ((new (make-hash-table :test #'equal)))
                             (puthash "version" 1 new)
                             (puthash "host_id" (ende-codex-auth--new-id) new)
                             (puthash "accounts" (make-hash-table :test #'equal) new)
                             new))))
            (prog1 (funcall function state)
              (ende-codex-auth--write-state directory state))))
      (when acquired (delete-directory lock)))))

(defun ende-codex-auth-host-id ()
  "Return the installation's persistent opaque ID, creating it if necessary.
This does not register, select, or authenticate an account."
  (ende-codex-auth--guard
   (lambda ()
     (let ((state (ende-codex-auth--read-state (ende-codex-auth--path nil))))
       (if state (gethash "host_id" state)
         (ende-codex-auth--transaction (lambda (new) (gethash "host_id" new))))))))

(defun ende-codex-auth-read (account)
  "Return ACCOUNT's detached private record, never importing other credentials."
  (ende-codex-auth--guard
   (lambda ()
     (let* ((alias (ende-codex-auth--alias account))
            (state (ende-codex-auth--read-state (ende-codex-auth--path nil)))
            (entry (and state (gethash alias (gethash "accounts" state)))))
       (unless entry (ende-codex-auth--fail))
       (ende-codex-auth--decode-record account entry (gethash "host_id" state))))))

(defun ende-codex-auth--encode-record (record)
  "Encode private RECORD using only the fixed storage fields."
  (let ((entry (make-hash-table :test #'equal)))
    (dolist (field ende-codex-auth--fields)
      (when (plist-member record (car field))
        (puthash (cdr field) (plist-get record (car field)) entry)))
    entry))

(defun ende-codex-auth--register (account credentials &optional expected-revision replace)
  "Store externally validated CREDENTIALS as a new ACCOUNT registration.
An existing alias always requires explicit REPLACE authorization and its exact
EXPECTED-REVISION, even for another registration of the same subject.  This
internal function is not a login command or an identity verifier."
  (ende-codex-auth--guard
   (lambda ()
     (let ((alias (ende-codex-auth--alias account)))
       (ende-codex-auth--validate-credentials credentials)
       (ende-codex-auth--transaction
        (lambda (state)
          (let* ((accounts (gethash "accounts" state))
                 (old (gethash alias accounts)))
            (unless (and (equal expected-revision (and old (gethash "revision" old)))
                         (or (not old) replace))
              (signal 'ende-codex-auth-conflict nil))
            (let ((record (append (list :account account
                                        :host-id (gethash "host_id" state)
                                        :registration-id (ende-codex-auth--new-id)
                                        :revision (ende-codex-auth--new-id))
                                  (copy-tree credentials))))
              (puthash alias (ende-codex-auth--encode-record record) accounts)
              (ende-codex-auth--decode-record
               account (gethash alias accounts) (gethash "host_id" state))))))))))

(defun ende-codex-auth--same-registration-p (left right)
  "Return non-nil when private records LEFT and RIGHT name one registration."
  (and left right
       (cl-every (lambda (key) (equal (plist-get left key) (plist-get right key)))
                 '(:account :host-id :registration-id :client-id :subject))))

(defun ende-codex-auth--rotate (expected credentials)
  "Rotate EXPECTED's tokens using CREDENTIALS for the same verified registration.
Refuse stale snapshots and changed subjects or issued client IDs.  Only refresh
rotation preserves the registration identity used by queued requests."
  (ende-codex-auth--guard
   (lambda ()
     (let* ((account (plist-get expected :account))
            (alias (ende-codex-auth--alias account)))
       (ende-codex-auth--validate-credentials credentials)
       (unless (and (equal (plist-get expected :subject) (plist-get credentials :subject))
                    (equal (plist-get expected :client-id) (plist-get credentials :client-id)))
         (signal 'ende-codex-auth-conflict nil))
       (ende-codex-auth--transaction
        (lambda (state)
          (let* ((accounts (gethash "accounts" state))
                 (entry (gethash alias accounts))
                 (current (and entry (ende-codex-auth--decode-record
                                      account entry (gethash "host_id" state)))))
            (unless (and (ende-codex-auth--same-registration-p expected current)
                         (equal (plist-get expected :revision) (plist-get current :revision)))
              (signal 'ende-codex-auth-conflict nil))
            (let ((record (append (list :account account
                                        :host-id (plist-get current :host-id)
                                        :registration-id (plist-get current :registration-id)
                                        :revision (ende-codex-auth--new-id))
                                  (copy-tree credentials))))
              (puthash alias (ende-codex-auth--encode-record record) accounts)
              (ende-codex-auth--decode-record
               account (gethash alias accounts) (gethash "host_id" state))))))))))

(provide 'ende-codex-auth)
;;; ende-codex-auth.el ends here
