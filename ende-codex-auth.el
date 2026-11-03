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
(require 'ende-lock)

(defconst ende-codex--jwks-url "https://auth.openai.com/.well-known/jwks.json")

(defvar ende-codex-auth--trusted-jwks nil
  "Dynamically bound keys retrieved from the pinned endpoint.")

(defun ende-codex-auth--endpoint (url expected)
  "Validate URL against a fixed trusted EXPECTED endpoint before HTTPS."
  (let ((actual (url-generic-parse-url (ende-curl--validated-url (list :url url))))
        (trusted (url-generic-parse-url expected)))
    (unless (and (member expected
                         (cons ende-codex--jwks-url
                               (cl-loop for (_method . spec) in ende-codex--auth-methods
                                        append (append
                                                (mapcar (lambda (key) (plist-get spec key))
                                                        '(:authorize-url :token-url :models-url :responses-url :usage-url))
                                                (when (plist-get spec :models-query)
                                                  (list (concat
                                                         (plist-get spec :models-url) "?"
                                                         (ende-codex-auth--form
                                                          (mapcar (lambda (key)
                                                                    (cons key ende-client-version))
                                                                  (plist-get spec :models-query))))))))))
                 (equal (url-type actual) "https")
                 (equal (url-host actual) (url-host trusted))
                 (= (or (url-port actual) 443) 443)
                 (equal (url-filename actual) (url-filename trusted)))
      (error "Codex requires the fixed official HTTPS endpoint"))
    url))

(defun ende-codex-auth--scopes (scope)
  "Return the separate granted tokens in valid OAuth SCOPE."
  (unless (and (stringp scope) (<= 1 (length scope) 4096)
               (cl-loop for char across scope
                        always (or (= char 32) (= char 33)
                                   (<= 35 char 91) (<= 93 char 126))))
    (signal 'ende-codex-identity-error nil))
  (let ((tokens (split-string scope " " nil)))
    (unless (and (not (member "" tokens))
                 (= (length tokens) (length (delete-dups (copy-sequence tokens)))))
      (signal 'ende-codex-identity-error nil))
    tokens))

(defun ende-codex-auth--form (parameters)
  "Encode PARAMETERS as an application/x-www-form-urlencoded string."
  (mapconcat (lambda (parameter)
               (concat (url-hexify-string (car parameter)) "="
                       (url-hexify-string (cdr parameter))))
             parameters "&"))

(defun ende-codex-auth--token-payload (method payload &optional refresh)
  "Validate METHOD's token endpoint PAYLOAD, optionally for REFRESH."
  (ende-codex--auth-spec method)
  (unless (and (hash-table-p payload)
               (cl-every (lambda (key)
                           (let ((value (gethash key payload)))
                             (and (ende-codex-auth--text-p value)
                                  (<= (length value) 65536))))
                         (if refresh '("access_token")
                           '("access_token" "refresh_token" "id_token")))
               (stringp (gethash "token_type" payload))
               (equal (downcase (gethash "token_type" payload)) "bearer")
               (integerp (gethash "expires_in" payload))
               (> (gethash "expires_in" payload) 0))
    (signal 'ende-codex-identity-error nil))
  (when (ende-protocol-json-has-key-p "refresh_token" payload)
    (unless (ende-codex-auth--text-p (gethash "refresh_token" payload))
      (signal 'ende-codex-identity-error nil)))
  (when (or (and (not refresh) (ende-codex--auth-property method :resource))
            (ende-protocol-json-has-key-p "scope" payload))
    (ende-codex-auth--scopes (gethash "scope" payload)))
  (when (ende-protocol-json-has-key-p "earliest_refresh_at" payload)
    (unless (and (integerp (gethash "earliest_refresh_at" payload))
                 (>= (gethash "earliest_refresh_at" payload) 0)
                 (< (gethash "earliest_refresh_at" payload)
                    (+ (floor (float-time)) (gethash "expires_in" payload))))
      (signal 'ende-codex-identity-error nil)))
  payload)

(defconst ende-codex-auth--issuer "https://auth.openai.com")

(define-error 'ende-codex-identity-error "OpenAI identity could not be verified")

(cl-defstruct (ende-codex-auth--request (:constructor ende-codex-auth--make-request))
  resources callback timer finished (bytes 0))

(defun ende-codex-auth--request-finish (request payload failure &optional cancelled)
  "Clean up REQUEST and deliver PAYLOAD or FAILURE unless CANCELLED."
  (unless (ende-codex-auth--request-finished request)
    (setf (ende-codex-auth--request-finished request) t)
    (when-let* ((timer (ende-codex-auth--request-timer request))) (cancel-timer timer))
    (let* ((resources (ende-codex-auth--request-resources request))
           (process (plist-get resources :process)))
      (when (processp process) (ende-curl--cancel-idle-timer process))
      (ende-curl-cleanup-resources
       (list (plist-get resources :response-buffer) (plist-get resources :stderr-buffer))
       (list (plist-get resources :headers-file) (plist-get resources :config-file))
       (list process (plist-get resources :stderr-process))))
    (let ((callback (ende-codex-auth--request-callback request)))
      (setf (ende-codex-auth--request-callback request) nil)
      (unless cancelled (funcall callback payload failure)))))

(defun ende-codex-auth--request-fail (request)
  "Fail REQUEST without including credentials or untrusted diagnostics."
  (ende-codex-auth--request-finish
   request nil (ende-error-create :kind 'auth :message "OpenAI authorization request failed")))

(defun ende-codex-auth--check-headers (request)
  "Bound REQUEST's header file even while no response body arrives."
  (unless (ende-codex-auth--request-finished request)
    (let ((attributes (file-attributes
                       (plist-get (ende-codex-auth--request-resources request) :headers-file))))
      (unless (and attributes (<= (file-attribute-size attributes) 16384))
        (ende-codex-auth--request-fail request)))))

(defun ende-codex-auth--request-sentinel (request process _event)
  "Validate terminal PROCESS response for authorization REQUEST."
  (when (and (not (ende-codex-auth--request-finished request))
             (memq (process-status process) '(exit signal closed failed)))
    (let ((debug-on-error nil) (debug-on-signal nil) (debug-on-quit nil)
          (ende-max-response-header-bytes 16384))
      (condition-case nil
          (if (/= (process-exit-status process) 0)
              (ende-codex-auth--request-fail request)
            (let* ((resources (ende-codex-auth--request-resources request))
                   (status (car (ende-curl-response-headers (plist-get resources :headers-file))))
                   (payload (with-current-buffer (plist-get resources :response-buffer)
                              (ende-codex-auth--json (buffer-string)))))
              (if (equal status 200)
                  (ende-codex-auth--request-finish request payload nil)
                (let ((code (gethash "error" payload)))
                  (ende-codex-auth--request-finish
                   request nil
                   (ende-error-create
                    :kind 'auth :message "OpenAI authorization request was rejected"
                    :details (list :status status
                                   :code (and (stringp code) (<= (length code) 100)
                                              (string-match-p "\\`[a-z_]+\\'" code) code))))))))
        ((error quit) (ende-codex-auth--request-fail request))))))

(defun ende-codex-auth--request (url form limit callback)
  "Request bounded authorization JSON at pinned URL with optional stdin FORM.
CALLBACK receives (PAYLOAD ERROR).  Return a cancellation function."
  (ende-codex-auth--endpoint url url)
  (let ((request (ende-codex-auth--make-request :callback callback))
        (ende-connect-timeout 10) (ende-max-stderr-bytes 4096))
    (condition-case nil
        (let* ((resources
                (ende-curl-start-request
                 :profile (list :url url) :name "ende-openai-authorization"
                 :command
                 (lambda (headers _config)
                   (append (ende-curl-base-command url)
                           (ende-curl-response-options headers)
                           (ende-curl-timeout-options 15 limit)
                           '("--header" "Accept: application/json")
                           (when form
                             '("--request" "POST" "--header"
                               "Content-Type: application/x-www-form-urlencoded"
                               "--data-binary" "@-"))
                           (list "--url" url)))
                 :filter
                 (lambda (process chunk)
                   (unless (ende-codex-auth--request-finished request)
                     (cl-incf (ende-codex-auth--request-bytes request) (string-bytes chunk))
                     (if (> (ende-codex-auth--request-bytes request) limit)
                         (ende-codex-auth--request-fail request)
                       (with-current-buffer (process-buffer process)
                         (goto-char (point-max)) (insert chunk)))))
                 :sentinel (apply-partially #'ende-codex-auth--request-sentinel request)))
               (process (plist-get resources :process)))
          (setf (ende-codex-auth--request-resources request) resources
                (ende-codex-auth--request-timer request)
                (run-at-time 0.2 0.2 #'ende-codex-auth--check-headers request))
          (set-process-coding-system process 'binary 'binary)
          (with-current-buffer (plist-get resources :response-buffer) (set-buffer-multibyte nil))
          (when form (process-send-string process form))
          (process-send-eof process))
      ((error quit) (ende-codex-auth--request-fail request)))
    (lambda () (ende-codex-auth--request-finish request nil nil t))))

(defun ende-codex-auth--jwks-async (callback)
  "Retrieve bounded trusted keys asynchronously for CALLBACK."
  (ende-codex-auth--request
   ende-codex--jwks-url nil 65536
   (lambda (jwks failure)
     (let ((keys (and jwks (gethash "keys" jwks))))
       (if (and (not failure) (vectorp keys) (<= 1 (length keys) 16))
           (funcall callback jwks nil)
         (funcall callback nil (or failure (ende-error-create
                                            :kind 'auth :message "OpenAI signing keys are unavailable"))))))))

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
  "Return the keys trusted for the current verification."
  (or ende-codex-auth--trusted-jwks
      (signal 'ende-codex-identity-error nil)))

(defun ende-codex-auth--claims (claims client-id nonce &optional ignore-nonce)
  "Validate authenticated CLAIMS against CLIENT-ID and login NONCE.
IGNORE-NONCE is reserved for the state/PKCE-protected native protocol."
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
                 (or ignore-nonce
                     (if nonce (equal (gethash "nonce" claims) nonce)
                       (eq (gethash "nonce" claims :absent) :absent))))
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

(defun ende-codex--verify-id-token (method id-token expected-client-id expected-nonce)
  "Authenticate ID-TOKEN for EXPECTED-CLIENT-ID and EXPECTED-NONCE.
Return claims only after signature and OpenID validation."
  (ende-codex-auth--identity-guard
   (lambda ()
     (unless (and (ende-codex-auth--text-p expected-client-id)
                  (or (not (ende-codex--auth-property method :nonce))
                      (ende-codex-auth--text-p expected-nonce)))
       (signal 'ende-codex-identity-error nil))
     (ende-codex-auth--claims
      (ende-codex-auth--signed-claims id-token #'ende-codex-auth--jwks)
      expected-client-id expected-nonce (not (ende-codex--auth-property method :nonce))))))

(defun ende-codex-native-account-id-from-claims (claims)
  "Extract the native workspace identifier from verified CLAIMS."
  (let* ((auth (gethash "https://api.openai.com/auth" claims))
         (account (and (hash-table-p auth) (gethash "chatgpt_account_id" auth))))
    (unless (ende-codex-auth--text-p account)
      (signal 'ende-codex-identity-error nil))
    account))

(defun ende-codex-auth--refresh-nonce (claims record)
  "Return the nonce that refreshed CLAIMS must repeat from RECORD.
RECORD's token was verified before storage, so only its payload is decoded."
  (when (ende-protocol-json-has-key-p "nonce" claims)
    (let* ((token (plist-get record :id-token))
           (parts (and (stringp token) (split-string token "\\." nil)))
           (nonce (and (= (length parts) 3)
                       (gethash "nonce" (ende-codex-auth--json
                                         (ende-rs256--decode (nth 1 parts)))))))
      (unless (ende-codex-auth--text-p nonce)
        (signal 'ende-codex-identity-error nil))
      nonce)))

(defun ende-codex-auth--refresh-claims (id-token record jwks)
  "Verify replacement ID-TOKEN for RECORD against trusted JWKS.
The replacement must continue RECORD's subject, issued client, and nonce."
  (let ((claims (ende-codex-auth--signed-claims id-token (lambda () jwks))))
    (let ((native (ende-codex--auth-property (plist-get record :auth-method) :account-header)))
      (ende-codex-auth--claims claims (plist-get record :client-id)
                               (unless native (ende-codex-auth--refresh-nonce claims record)) native)
      (when native
        (unless (equal (ende-codex-native-account-id-from-claims claims)
                       (plist-get record :chatgpt-account-id))
          (signal 'ende-codex-identity-error nil))))
    (unless (equal (gethash "sub" claims) (plist-get record :subject))
      (signal 'ende-codex-identity-error nil))
    claims))

(defun ende-codex-auth--verify-refresh (id-token record callback)
  "Verify refreshed ID-TOKEN for RECORD asynchronously.
CALLBACK receives (CLAIMS FAILURE); CLAIMS is non-nil only when the
replacement token was verified against the pinned JWKS endpoint."
  (ende-codex-auth--jwks-async
   (lambda (jwks failure)
     (if failure
         (funcall callback nil failure)
       (condition-case nil
           (funcall callback
                    (ende-codex-auth--identity-guard
                     (lambda () (ende-codex-auth--refresh-claims id-token record jwks)))
                    nil)
         ((error quit)
          (funcall callback nil (ende-error-create
                                 :kind 'auth
                                 :message "OpenAI identity could not be verified"))))))))

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
    (:scope . "scope") (:email . "email") (:chatgpt-account-id . "chatgpt_account_id")
    (:earliest-refresh-at . "earliest_refresh_at") (:needs-login . "needs_login")))

(defconst ende-codex-auth--credential-fields
  '(:client-id :subject :access-token :refresh-token :id-token :expires-at
	       :scope :email :chatgpt-account-id :earliest-refresh-at :needs-login))

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

(defun ende-codex-auth--key (method account)
  "Return the independent storage key for METHOD and ACCOUNT."
  (ende-codex--auth-spec method)
  (concat (symbol-name method) "/" (ende-codex-auth--alias account)))

(defun ende-codex-auth--validate-method (method credentials)
  "Validate the METHOD-specific binding in CREDENTIALS."
  (ende-codex--auth-spec method)
  (when (ende-codex--auth-property method :account-header)
    (unless (and (equal (plist-get credentials :client-id)
                        (ende-codex--auth-property method :client-id))
                 (ende-codex-auth--text-p (plist-get credentials :chatgpt-account-id)))
      (ende-codex-auth--fail))))

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
  (when (plist-member credentials :needs-login)
    (unless (eq (plist-get credentials :needs-login) t) (ende-codex-auth--fail)))
  (when (and (plist-get credentials :needs-login)
             (or (plist-member credentials :access-token) (plist-member credentials :refresh-token)))
    (ende-codex-auth--fail))
  (dolist (key (if (plist-get credentials :needs-login) '(:client-id :subject :id-token)
                 '(:client-id :subject :access-token :refresh-token :id-token)))
    (unless (ende-codex-auth--text-p (plist-get credentials key))
      (ende-codex-auth--fail)))
  (unless (and (integerp (plist-get credentials :expires-at))
               (>= (plist-get credentials :expires-at) 0))
    (ende-codex-auth--fail))
  (when (plist-member credentials :email)
    (unless (and (ende-codex-auth--text-p (plist-get credentials :email))
                 (<= (length (plist-get credentials :email)) 320))
      (ende-codex-auth--fail)))
  (when (plist-member credentials :scope)
    (ende-codex-auth--scopes (plist-get credentials :scope)))
  (when (plist-member credentials :earliest-refresh-at)
    (unless (and (integerp (plist-get credentials :earliest-refresh-at))
                 (<= 0 (plist-get credentials :earliest-refresh-at)
                     (plist-get credentials :expires-at)))
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

(defun ende-codex-auth--lock-name-p (name)
  "Return non-nil when NAME is a recognized credential lock."
  (and (stringp name)
       (or (equal name ".writer-lock")
           (string-match-p
            "\\`\\.refresh-\\(?:chatgpt-plan-oauth\\|codex-oauth\\)-[a-z0-9][a-z0-9_-]*\\'"
            name))))

(defun ende-codex-auth--lock-owner (lock)
  "Return validated owner metadata in LOCK, nil if absent, or :invalid."
  (let* ((file (expand-file-name "owner" lock))
         (attributes (file-attributes file 'integer)))
    (if (null attributes)
        :invalid
      (when (file-symlink-p file)
        (ende-codex-auth--fail))
      (ende-codex-auth--attributes file nil t)
      (if (> (file-attribute-size attributes) 4096)
          :invalid
        (condition-case nil
            (let* ((data (with-temp-buffer
                           (insert-file-contents-literally file)
                           (json-parse-string (buffer-string) :object-type 'hash-table)))
                   (pid (and (hash-table-p data) (gethash "pid" data)))
                   (host (and (hash-table-p data) (gethash "host" data)))
                   (started (and (hash-table-p data) (gethash "started" data)))
                   (id (and (hash-table-p data) (gethash "id" data))))
              (if (and (hash-table-p data) (= (hash-table-count data) 4)
                       (integerp pid) (> pid 0)
                       (ende-codex-auth--text-p host)
                       (or (null started) (stringp started))
                       (ende-codex-auth--id-p id))
                  (list :pid pid :host host :started started :id id)
                :invalid))
          (error :invalid))))))

(defun ende-codex-auth--process-start (pid)
  "Return PID's stable Linux start ticks, or nil if unavailable."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents-literally (format "/proc/%d/stat" pid))
        (goto-char (point-max))
        (when (search-backward ") " nil t)
          (let ((ticks (nth 19 (split-string
                                (buffer-substring-no-properties (+ (point) 2)
                                                                (point-max))))))
            (and ticks (string-match-p "\\`[0-9]+\\'" ticks) ticks))))
    (error nil)))

(defun ende-codex-auth--raw-lock (lock)
  "Create LOCK and its owner metadata inside the shared mutex."
  (let ((original (default-file-modes)) identity complete)
    (unwind-protect
        (progn
          (unwind-protect
              (progn
                (set-default-file-modes #o700)
                (make-directory lock)
                (setq identity (ende-codex-auth--lock-identity lock)))
            (set-default-file-modes original))
          (ende-codex-auth--attributes lock t t)
          (let* ((started (ende-codex-auth--process-start (emacs-pid)))
                 (owner (let ((data (make-hash-table :test #'equal)))
                          (puthash "pid" (emacs-pid) data)
                          (puthash "host" (system-name) data)
                          (puthash "started" started data)
                          (puthash "id" (ende-codex-auth--new-id) data)
                          data))
                 (file (expand-file-name "owner" lock))
                 (original (default-file-modes)))
            (unwind-protect
                (progn
                  (set-default-file-modes #o600)
                  (with-temp-buffer
                    (insert (json-encode owner) "\n")
                    (write-region (point-min) (point-max) file nil 'silent)))
              (set-default-file-modes original))
            (set-file-modes file #o600)
            (ende-codex-auth--attributes file nil t)
            (setq identity (ende-codex-auth--lock-identity lock)
                  complete t)
            identity))
      (when (and identity (not complete)
                 (equal (car identity) (car (ende-codex-auth--lock-identity lock))))
        (ende-codex-auth--remove-lock lock)))))

(defun ende-codex-auth--with-lock-guard (function &optional timeout)
  "Call FUNCTION under the mutex, optionally waiting TIMEOUT milliseconds."
  (condition-case nil
      (ende-lock-call (expand-file-name ".lock-guard.sqlite" (ende-codex-auth--path t))
                      function timeout)
    (ende-lock-busy (signal 'ende-codex-auth-busy nil))
    (ende-lock-error (ende-codex-auth--fail))))

(defun ende-codex-auth--lock-contents (lock)
  "Validate LOCK and return its recognized contents."
  (unless (ende-codex-auth--attributes lock t t)
    (user-error "Credential lock no longer exists"))
  (let ((contents (directory-files lock nil directory-files-no-dot-files-regexp)))
    (unless (or (null contents) (equal contents '("owner")))
      (user-error "Credential lock has unrecognized contents"))
    contents))

(defun ende-codex-auth--remove-lock (lock)
  "Remove only recognized contents of LOCK inside the shared mutex."
  (when (ende-codex-auth--lock-contents lock)
    (ende-codex-auth--attributes (expand-file-name "owner" lock) nil t)
    (delete-file (expand-file-name "owner" lock)))
  (delete-directory lock))

(defun ende-codex-auth--delete-lock-now (lock identity timeout)
  "Remove LOCK only when its complete IDENTITY is unchanged."
  (ende-codex-auth--with-lock-guard
   (lambda ()
     (when (and (file-exists-p lock)
                (equal identity (ende-codex-auth--lock-identity lock)))
       (ende-codex-auth--remove-lock lock)))
   timeout))

(defun ende-codex-auth--delete-lock-retry (lock identity)
  "Retry deleting LOCK after transient guard contention."
  (condition-case nil
      (ende-codex-auth--delete-lock-now lock identity 0)
    (ende-codex-auth-busy
     (run-at-time 0.5 nil #'ende-codex-auth--delete-lock-retry lock identity))
    ((error quit) nil)))

(defun ende-codex-auth--delete-lock (lock identity)
  "Remove LOCK, deferring only transient guard contention."
  (condition-case nil
      (ende-codex-auth--delete-lock-now lock identity 5000)
    (ende-codex-auth-busy
     (run-at-time 0.5 nil #'ende-codex-auth--delete-lock-retry lock identity))))

(defun ende-codex-auth--make-lock (lock)
  "Create LOCK and owner metadata while holding the shared mutex."
  (ende-codex-auth--with-lock-guard
   (lambda () (ende-codex-auth--raw-lock lock))))

(defun ende-codex-auth--lock-identity (lock)
  "Return LOCK's identity, entries, and exact bounded owner contents."
  (let ((attributes (ende-codex-auth--attributes lock t t)))
    (when attributes
      (let* ((contents (ende-codex-auth--lock-contents lock))
             (file (expand-file-name "owner" lock))
             (owner (and contents (ende-codex-auth--attributes file nil t))))
        (when (and owner (> (file-attribute-size owner) 4096))
          (ende-codex-auth--fail))
        (list (list (file-attribute-device-number attributes)
                    (file-attribute-inode-number attributes))
              contents
              (when owner
                (list (file-attribute-device-number owner)
                      (file-attribute-inode-number owner)
                      (with-temp-buffer
                        (insert-file-contents-literally file)
                        (buffer-string)))))))))

(defun ende-codex-auth--lock-owner-status (owner)
  "Return whether OWNER is active, abandoned, or unverifiable."
  (cond
   ((not (listp owner)) 'unknown)
   ((not (equal (plist-get owner :host) (system-name))) 'unknown)
   ((not (fboundp 'process-attributes)) 'unknown)
   (t
    (let ((attributes (condition-case nil
                          (process-attributes (plist-get owner :pid))
                        (error :unavailable))))
      (cond
       ((eq attributes :unavailable) 'unknown)
       ((null attributes) 'abandoned)
       ((equal (cdr (assq 'state attributes)) "Z") 'abandoned)
       (t
        (let ((started (ende-codex-auth--process-start (plist-get owner :pid))))
          (if (or (null started) (null (plist-get owner :started))
                  (equal started (plist-get owner :started)))
              'active
            'abandoned))))))))

(defun ende-codex-auth--recover-lock (name &optional confirmed)
  "Recover recognized abandoned lock NAME, requiring CONFIRMED if uncertain."
  (unless (ende-codex-auth--lock-name-p name)
    (user-error "Not a recognized credential lock"))
  (let* ((directory (ende-codex-auth--path nil))
         (lock (expand-file-name name directory)))
    (ende-codex-auth--lock-contents lock)
    (let* ((owner (ende-codex-auth--lock-owner lock))
           (identity (ende-codex-auth--lock-identity lock))
           (status (ende-codex-auth--lock-owner-status owner)))
      (unless identity (user-error "Credential lock no longer exists"))
      (when (eq status 'active)
        (user-error "Credential lock owner is still active"))
      (when (and (eq status 'unknown) (not confirmed))
        (unless (yes-or-no-p "Cannot verify lock owner; remove it anyway? ")
          (user-error "Credential lock was not recovered"))
        (setq confirmed t))
      (ende-codex-auth--with-lock-guard
       (lambda ()
         (unless (equal identity (ende-codex-auth--lock-identity lock))
           (user-error "Credential lock changed during recovery"))
         (let ((current (ende-codex-auth--lock-owner-status
                         (ende-codex-auth--lock-owner lock))))
           (when (eq current 'active)
             (user-error "Credential lock owner is still active"))
           (when (and (eq current 'unknown) (not confirmed))
             (user-error "Credential lock now requires confirmation; retry recovery")))
         (ende-codex-auth--remove-lock lock)
         t)))))

;;;###autoload
(defun ende-codex-auth-recover-lock ()
  "Interactively recover an abandoned private credential lock."
  (interactive)
  (let* ((directory (ende-codex-auth--path nil))
         (locks (cl-remove-if-not
                 #'ende-codex-auth--lock-name-p
                 (directory-files directory nil "\\`\\."))))
    (if (null locks)
        (message "ende: no credential locks found")
      (let ((name (completing-read "Recover credential lock: " locks nil t)))
        (when (ende-codex-auth--recover-lock name)
          (message "ende: recovered %s" name))))))

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

(defun ende-codex-auth--decode-record (method account entry host-id)
  "Decode checked JSON ENTRY for ACCOUNT under installation HOST-ID."
  (unless (hash-table-p entry) (ende-codex-auth--fail))
  (let ((record (list :auth-method method :account account)) credentials)
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
    (ende-codex-auth--validate-method method credentials)
    (unless (and (equal (plist-get record :host-id) host-id)
                 (ende-codex-auth--id-p (plist-get record :registration-id))
                 (ende-codex-auth--id-p (plist-get record :revision)))
      (ende-codex-auth--fail))
    record))

(defun ende-codex-auth--read-state (directory)
  "Read and validate private JSON state under DIRECTORY, or return nil."
  (let* ((file (expand-file-name "connections.json" directory))
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
                     (equal (gethash "version" state) 2)
                     (ende-codex-auth--id-p host-id) (hash-table-p accounts))
          (ende-codex-auth--fail))
        (maphash (lambda (key entry)
                   (unless (and (stringp key)
                                (let ((case-fold-search nil))
                                  (string-match "\\`\\(chatgpt-plan-oauth\\|codex-oauth\\)/\\([a-z0-9][a-z0-9_-]*\\)\\'" key)))
                     (ende-codex-auth--fail))
                   (let ((method (intern (match-string 1 key)))
                         (account (intern (match-string 2 key))))
                     (ende-codex-auth--decode-record method account entry host-id)))
                 accounts)
        state))))

(defun ende-codex-auth--write-state (directory state)
  "Atomically write STATE beside its destination in private DIRECTORY."
  (let ((file (expand-file-name "connections.json" directory)) temporary)
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
Never recover an existing lock automatically.  Every writer rereads under the
lock and must still compare the individual account revision."
  (let* ((directory (ende-codex-auth--path t))
         (lock (expand-file-name ".writer-lock" directory)) lock-id)
    (unwind-protect
        (progn
          (condition-case nil
              (setq lock-id (ende-codex-auth--make-lock lock))
            (file-already-exists (signal 'ende-codex-auth-busy nil)))
          (let ((state (or (ende-codex-auth--read-state directory)
                           (let ((new (make-hash-table :test #'equal)))
                             (puthash "version" 2 new)
                             (puthash "host_id" (ende-codex-auth--new-id) new)
                             (puthash "accounts" (make-hash-table :test #'equal) new)
                             new))))
            (prog1 (funcall function state)
              (ende-codex-auth--write-state directory state))))
      (when lock-id (ende-codex-auth--delete-lock lock lock-id)))))

(defun ende-codex-auth-host-id ()
  "Return the installation's persistent opaque ID, creating it if necessary.
This does not register, select, or authenticate an account."
  (ende-codex-auth--guard
   (lambda ()
     (let ((state (ende-codex-auth--read-state (ende-codex-auth--path nil))))
       (if state (gethash "host_id" state)
         (ende-codex-auth--transaction (lambda (new) (gethash "host_id" new))))))))

(defun ende-codex-auth-lookup (method account)
  "Return METHOD and ACCOUNT's validated record or nil; unsafe storage errors."
  (ende-codex-auth--guard
   (lambda ()
     (let* ((alias (ende-codex-auth--key method account))
            (state (ende-codex-auth--read-state (ende-codex-auth--path nil)))
            (entry (and state (gethash alias (gethash "accounts" state)))))
       (when entry
         (ende-codex-auth--decode-record method account entry (gethash "host_id" state)))))))

(defun ende-codex-auth-read (method account)
  "Return METHOD and ACCOUNT's detached record, never importing credentials."
  (or (ende-codex-auth-lookup method account) (ende-codex-auth--fail)))

(defun ende-codex-auth--encode-record (record)
  "Encode private RECORD using only the fixed storage fields."
  (let ((entry (make-hash-table :test #'equal)))
    (dolist (field ende-codex-auth--fields)
      (when (plist-member record (car field))
        (puthash (cdr field) (plist-get record (car field)) entry)))
    entry))

(defun ende-codex-auth--register (method account credentials &optional expected-revision replace)
  "Store validated CREDENTIALS as a new METHOD and ACCOUNT registration.
An existing alias always requires explicit REPLACE authorization and its exact
EXPECTED-REVISION, even for another registration of the same subject.  This
internal function is not a login command or an identity verifier."
  (ende-codex-auth--guard
   (lambda ()
     (let ((alias (ende-codex-auth--key method account)))
       (ende-codex-auth--validate-credentials credentials)
       (ende-codex-auth--validate-method method credentials)
       (ende-codex-auth--transaction
        (lambda (state)
          (let* ((accounts (gethash "accounts" state))
                 (old (gethash alias accounts)))
            (unless (and (equal expected-revision (and old (gethash "revision" old)))
                         (or (not old) replace))
              (signal 'ende-codex-auth-conflict nil))
            (let ((record (append (list :auth-method method :account account
                                        :host-id (gethash "host_id" state)
                                        :registration-id (ende-codex-auth--new-id)
                                        :revision (ende-codex-auth--new-id))
                                  (copy-tree credentials))))
              (puthash alias (ende-codex-auth--encode-record record) accounts)
              (ende-codex-auth--decode-record
               method account (gethash alias accounts) (gethash "host_id" state))))))))))

(defun ende-codex-auth--same-registration-p (left right)
  "Return non-nil when private records LEFT and RIGHT name one registration."
  (and left right
       (cl-every (lambda (key) (equal (plist-get left key) (plist-get right key)))
                 '(:auth-method :account :host-id :registration-id :client-id :subject :chatgpt-account-id))))

(defun ende-codex-auth--rotate (expected credentials)
  "Rotate EXPECTED's tokens using CREDENTIALS for the same verified registration.
Refuse stale snapshots and changed subjects or issued client IDs.  Only refresh
rotation preserves the registration identity used by queued requests."
  (ende-codex-auth--guard
   (lambda ()
     (let* ((method (plist-get expected :auth-method))
            (account (plist-get expected :account))
            (alias (ende-codex-auth--key method account)))
       (ende-codex-auth--validate-credentials credentials)
       (ende-codex-auth--validate-method method credentials)
       (unless (and (equal (plist-get expected :subject) (plist-get credentials :subject))
                    (equal (plist-get expected :client-id) (plist-get credentials :client-id))
                    (equal (plist-get expected :chatgpt-account-id)
                           (plist-get credentials :chatgpt-account-id)))
         (signal 'ende-codex-auth-conflict nil))
       (ende-codex-auth--transaction
        (lambda (state)
          (let* ((accounts (gethash "accounts" state))
                 (entry (gethash alias accounts))
                 (current (and entry (ende-codex-auth--decode-record
                                      method account entry (gethash "host_id" state)))))
            (unless (and (ende-codex-auth--same-registration-p expected current)
                         (equal (plist-get expected :revision) (plist-get current :revision)))
              (signal 'ende-codex-auth-conflict nil))
            (let ((record (append (list :auth-method method :account account
                                        :host-id (plist-get current :host-id)
                                        :registration-id (plist-get current :registration-id)
                                        :revision (ende-codex-auth--new-id))
                                  (copy-tree credentials))))
              (puthash alias (ende-codex-auth--encode-record record) accounts)
              (ende-codex-auth--decode-record
               method account (gethash alias accounts) (gethash "host_id" state))))))))))

(defun ende-codex-auth--credentials (record)
  "Extract storage credentials from private RECORD."
  (cl-loop for key in ende-codex-auth--credential-fields
           when (plist-member record key) append (list key (plist-get record key))))

(defun ende-codex-auth--require-login (record)
  "Invalidate RECORD's unusable tokens without changing registration identity."
  (let ((credentials (ende-codex-auth--credentials record)))
    (dolist (key '(:access-token :refresh-token :earliest-refresh-at))
      (cl-remf credentials key))
    (ende-codex-auth--rotate record (plist-put credentials :needs-login t))))

(provide 'ende-codex-auth)
;;; ende-codex-auth.el ends here
