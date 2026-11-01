;;; ende-codex-login.el --- Browser authorization for Ende -*- lexical-binding: t; -*-

;;; Code:

(require 'browse-url)
(require 'url-util)
(require 'ende-codex-auth)

(declare-function ende-model-selection-for-buffer "ende-provider" (&optional buffer))
(declare-function ende-provider-profile "ende-provider" (&optional name))

(cl-defstruct (ende-codex--login (:constructor ende-codex--make-login))
  account existing client-id state nonce verifier redirect host-id listener clients
  timer cancel callback consumed finished)

(defvar ende-codex--pending-login nil)
(defvar ende-codex--issued-clients (make-hash-table :test #'equal)
  "Issued IDs awaiting a verified registration after a consumed code failed.")

(defun ende-codex--base64url (bytes)
  "Encode BYTES as unpadded base64url."
  (base64url-encode-string bytes t))

(defun ende-codex--login-random ()
  "Return an independently generated 256-bit security value."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally "/dev/urandom" nil nil 32)
    (unless (= (buffer-size) 32) (error "System randomness unavailable"))
    (ende-codex--base64url (buffer-string))))

(defun ende-codex--pkce-challenge (verifier)
  "Return the S256 challenge for VERIFIER."
  (ende-codex--base64url (secure-hash 'sha256 verifier nil nil t)))

(defun ende-codex--form (parameters)
  "Encode PARAMETERS for an OAuth query or form."
  (mapconcat (lambda (pair)
               (concat (url-hexify-string (car pair)) "="
                       (url-hexify-string (cdr pair))))
             parameters "&"))

(defun ende-codex--login-live-p (attempt)
  "Return non-nil if ATTEMPT can still change credentials."
  (and (eq attempt ende-codex--pending-login)
       (not (ende-codex--login-finished attempt))))

(defun ende-codex--login-close-listener (attempt)
  "Close ATTEMPT's listener and all accepted connections."
  (dolist (process (cons (ende-codex--login-listener attempt)
                        (ende-codex--login-clients attempt)))
    (when (processp process)
      (set-process-filter process #'ignore)
      (set-process-sentinel process #'ignore)
      (when (process-live-p process) (delete-process process))))
  (setf (ende-codex--login-listener attempt) nil
        (ende-codex--login-clients attempt) nil))

(defun ende-codex--login-finish (attempt record failure)
  "Finish ATTEMPT once, cleaning every resource before reporting RECORD or FAILURE."
  (when (ende-codex--login-live-p attempt)
    (setf (ende-codex--login-finished attempt) t)
    (setq ende-codex--pending-login nil)
    (ende-codex--login-close-listener attempt)
    (when-let* ((timer (ende-codex--login-timer attempt))) (cancel-timer timer))
    (when-let* ((cancel (ende-codex--login-cancel attempt))) (funcall cancel))
    (let ((callback (ende-codex--login-callback attempt)))
      (setf (ende-codex--login-state attempt) nil
            (ende-codex--login-nonce attempt) nil
            (ende-codex--login-verifier attempt) nil
            (ende-codex--login-existing attempt) nil
            (ende-codex--login-cancel attempt) nil
            (ende-codex--login-timer attempt) nil
            (ende-codex--login-callback attempt) nil)
      (when callback (funcall callback record failure)))))

(defun ende-codex--login-fail (attempt &optional message)
  "Fail ATTEMPT with a safe MESSAGE."
  (ende-codex--login-finish
   attempt nil (ende-error-create :kind 'auth :message (or message "ChatGPT authorization could not be verified"))))

(defun ende-codex-cancel-login ()
  "Cancel the active browser authorization without modifying accounts."
  (interactive)
  (when ende-codex--pending-login
    (ende-codex--login-fail ende-codex--pending-login "ChatGPT login cancelled")))

(defun ende-codex--login-authorize-url (attempt)
  "Build ATTEMPT's authorization URL without exposing it in messages."
  (let* ((existing (ende-codex--login-existing attempt))
         (client (ende-codex--login-client-id attempt))
         (parameters
          `(("client_id" . ,client) ("response_type" . "code")
            ("redirect_uri" . ,(ende-codex--login-redirect attempt))
            ("scope" . ,ende-codex--scopes) ("resource" . ,ende-codex--resource)
            ("state" . ,(ende-codex--login-state attempt))
            ("nonce" . ,(ende-codex--login-nonce attempt))
            ("code_challenge_method" . "S256")
            ("code_challenge" . ,(ende-codex--pkce-challenge (ende-codex--login-verifier attempt)))
            ("ext_agent_host_id" . ,(ende-codex--login-host-id attempt)))))
    (when (equal client "dynamic_agent_client")
      (push '("agent_name_hint" . "Ende") parameters))
    (when (plist-get existing :id-token)
      (push (cons "id_token_hint" (plist-get existing :id-token)) parameters))
    (when (plist-get existing :email)
      (push (cons "login_hint" (plist-get existing :email)) parameters))
    (concat (ende-codex-auth--endpoint ende-codex--authorize-url ende-codex--authorize-url)
            "?" (ende-codex--form parameters))))

(defun ende-codex--login-save (attempt payload jwks failure)
  "Verify PAYLOAD with trusted JWKS and save ATTEMPT unless FAILURE occurred."
  (when (ende-codex--login-live-p attempt)
    (if failure (ende-codex--login-finish attempt nil failure)
      (let ((debug-on-error nil) (debug-on-signal nil) (debug-on-quit nil))
        (condition-case nil
            (let* ((ende-codex-auth--trusted-jwks jwks)
                   (existing (ende-codex--login-existing attempt))
                   (client (ende-codex--login-client-id attempt))
                   (claims (ende-codex--verify-id-token
                            (gethash "id_token" payload) client (ende-codex--login-nonce attempt)))
                   (subject (gethash "sub" claims))
                   (credentials
                    (list :client-id client :subject subject
                          :access-token (gethash "access_token" payload)
                          :refresh-token (gethash "refresh_token" payload)
                          :id-token (gethash "id_token" payload) :scope (gethash "scope" payload)
                          :expires-at (+ (floor (float-time)) (gethash "expires_in" payload)))))
              (when existing
                (unless (and (equal client (plist-get existing :client-id))
                             (equal subject (plist-get existing :subject)))
                  (signal 'ende-codex-identity-error nil)))
              (when (and (eq (gethash "email_verified" claims) t)
                         (ende-codex-auth--text-p (gethash "email" claims))
                         (<= (length (gethash "email" claims)) 320))
                (setq credentials (plist-put credentials :email (gethash "email" claims))))
              (when (gethash "earliest_refresh_at" payload)
                (setq credentials (plist-put credentials :earliest-refresh-at
                                             (gethash "earliest_refresh_at" payload))))
              (let ((record (if existing (ende-codex-auth--rotate existing credentials)
                              (ende-codex-auth--register (ende-codex--login-account attempt) credentials))))
                (remhash (list (ende-codex--login-host-id attempt) (ende-codex--login-account attempt))
                         ende-codex--issued-clients)
                (ende-codex--login-finish attempt record nil)))
          ((error quit) (ende-codex--login-fail attempt)))))))

(defun ende-codex--login-exchanged (attempt payload failure)
  "Validate ATTEMPT's token PAYLOAD and fetch signing keys, or report FAILURE."
  (when (ende-codex--login-live-p attempt)
    (if failure (ende-codex--login-finish attempt nil failure)
      (condition-case nil
          (progn
            (ende-codex-auth--token-payload payload)
            (setf (ende-codex--login-cancel attempt)
                  (ende-codex-auth--jwks-async
                   (apply-partially #'ende-codex--login-save attempt payload))))
        ((error quit) (ende-codex--login-fail attempt))))))

(defun ende-codex--login-identifier-p (value)
  "Return non-nil for a bounded nonempty single-line OAuth identifier VALUE."
  (and (ende-codex-auth--text-p value) (<= (length value) 4096)
       (not (string-match-p "[[:space:]]" value))))

(defun ende-codex--login-query (query)
  "Decode callback QUERY, rejecting duplicate security parameters."
  (let (parameters)
    (dolist (field (split-string query "&" nil))
      (unless (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" field)
        (signal 'ende-codex-identity-error nil))
      (let ((key (match-string 1 field)) (value (match-string 2 field)))
        (cl-labels ((decode (text)
                      (when (string-match-p "%\\(?:[^0-9a-fA-F]\\|[0-9a-fA-F][^0-9a-fA-F]\\|[0-9a-fA-F]?\\'\\)" text)
                        (signal 'ende-codex-identity-error nil))
                      (decode-coding-string
                       (url-unhex-string (replace-regexp-in-string "+" " " text t t)) 'utf-8-unix)))
          (setq key (decode key) value (decode value)))
        (when (and (member key '("state" "code" "client_id" "error")) (assoc key parameters))
          (signal 'ende-codex-identity-error nil))
        (push (cons key value) parameters)))
    parameters))

(defun ende-codex--login-receive-callback (attempt parameters)
  "Validate callback PARAMETERS for ATTEMPT in protocol order and exchange once."
  (unless (and (ende-codex--login-live-p attempt)
               (not (ende-codex--login-consumed attempt))
               (equal (cdr (assoc "state" parameters)) (ende-codex--login-state attempt)))
    (signal 'ende-codex-identity-error nil))
  (setf (ende-codex--login-consumed attempt) t)
  (ende-codex--login-close-listener attempt)
  (if (assoc "error" parameters)
      (ende-codex--login-fail attempt "ChatGPT authorization was declined")
    (let* ((code (cdr (assoc "code" parameters)))
           (returned (cdr (assoc "client_id" parameters)))
           (saved (ende-codex--login-client-id attempt)))
      (unless (ende-codex--login-identifier-p code) (signal 'ende-codex-identity-error nil))
      (if (equal saved "dynamic_agent_client")
          (unless (and (ende-codex--login-identifier-p returned)
                       (not (equal returned "dynamic_agent_client")))
            (signal 'ende-codex-identity-error nil))
        (when (and returned (not (equal returned saved))) (signal 'ende-codex-identity-error nil)))
      (when (equal saved "dynamic_agent_client")
        (setf (ende-codex--login-client-id attempt) returned)
        (puthash (list (ende-codex--login-host-id attempt) (ende-codex--login-account attempt))
                 returned ende-codex--issued-clients))
      (setf (ende-codex--login-cancel attempt)
            (ende-codex-auth--request
             ende-codex--token-url
             (ende-codex--form
              `(("grant_type" . "authorization_code")
                ("client_id" . ,(ende-codex--login-client-id attempt)) ("code" . ,code)
                ("code_verifier" . ,(ende-codex--login-verifier attempt))
                ("redirect_uri" . ,(ende-codex--login-redirect attempt))
                ("resource" . ,ende-codex--resource)))
             262144 (apply-partially #'ende-codex--login-exchanged attempt))))))

(defun ende-codex--login-http-response (process success)
  "Send a static, secret-free HTTP response to PROCESS for SUCCESS."
  (let ((body (if success
                  "<!doctype html><title>Ende</title><p>Authorization received. You can return to Emacs.</p>"
                "<!doctype html><title>Ende</title><p>Authorization was not accepted. Return to Emacs.</p>")))
    (process-send-string
     process (format "HTTP/1.1 %s\r\nContent-Type: text/html; charset=utf-8\r\nCache-Control: no-store\r\nContent-Security-Policy: default-src 'none'\r\nConnection: close\r\nContent-Length: %d\r\n\r\n%s"
                     (if success "200 OK" "400 Bad Request") (string-bytes body) body))))

(defun ende-codex--login-http-filter (attempt process chunk)
  "Accumulate bounded HTTP CHUNK from PROCESS for ATTEMPT."
  (let ((debug-on-error nil) (debug-on-signal nil) (debug-on-quit nil))
    (condition-case nil
        (let* ((request (concat (process-get process :request) chunk))
               (end (string-match "\r\n\r\n" request))
               (lines (split-string request "\r\n" nil)))
          (unless (and (ende-codex--login-live-p attempt)
                       (not (ende-codex--login-consumed attempt))
                       (<= (string-bytes request) 16384)
                       (<= (length (car lines)) 8192)
                       (cl-every (lambda (line) (<= (length line) 2048)) (cdr lines))
                       (<= (length lines) 35))
            (signal 'ende-codex-identity-error nil))
          (process-put process :request request)
          (when end
            (unless (= (+ end 4) (length request)) (signal 'ende-codex-identity-error nil))
            (unless (string-match "\\`GET /auth/callback[?]\\([^ #[:cntrl:]]+\\) HTTP/1\\.[01]\\'" (car lines))
              (signal 'ende-codex-identity-error nil))
            (let ((query (match-string 1 (car lines))) headers)
              (dolist (line (butlast (cdr lines) 2))
                (unless (string-match "\\`\\([!#$%&'*+.^_`|~0-9A-Za-z-]+\\):[ \t]*\\([^[:cntrl:]]*\\)\\'" line)
                  (signal 'ende-codex-identity-error nil))
                (let ((key (downcase (match-string 1 line))) (value (string-trim (match-string 2 line))))
                  (when (assoc key headers) (signal 'ende-codex-identity-error nil))
                  (push (cons key value) headers)))
              (unless (and (equal (cdr (assoc "host" headers))
                                  (substring (ende-codex--login-redirect attempt) 7 -14))
                           (not (assoc "transfer-encoding" headers))
                           (or (not (assoc "content-length" headers))
                               (equal (cdr (assoc "content-length" headers)) "0")))
                (signal 'ende-codex-identity-error nil))
              (let ((parameters (ende-codex--login-query query)))
                (unless (equal (cdr (assoc "state" parameters)) (ende-codex--login-state attempt))
                  (signal 'ende-codex-identity-error nil))
                (ende-codex--login-http-response process t)
                (process-put process :request nil)
                (ende-codex--login-receive-callback attempt parameters)))))
      ((error quit)
       (when (process-live-p process)
         (ignore-errors (ende-codex--login-http-response process nil)))
       (ende-codex--login-fail attempt)))))

(defun ende-codex--login-listen (attempt)
  "Bind ATTEMPT's one-shot callback listener specifically to IPv4 loopback."
  (let ((listener
         (make-network-process
          :name "ende-chatgpt-login" :server t :host "127.0.0.1" :family 'ipv4
          :service t :noquery t :coding 'binary
          :filter (apply-partially #'ende-codex--login-http-filter attempt)
          :sentinel #'ignore
          :log (lambda (_server client _message)
                 (if (or (not (ende-codex--login-live-p attempt))
                         (ende-codex--login-consumed attempt)
                         (>= (length (ende-codex--login-clients attempt)) 4))
                     (delete-process client)
                   (push client (ende-codex--login-clients attempt))
                   (set-process-query-on-exit-flag client nil)
                   (set-process-coding-system client 'binary 'binary))))))
    (setf (ende-codex--login-listener attempt) listener
          (ende-codex--login-redirect attempt)
          (format "http://127.0.0.1:%d/auth/callback" (process-contact listener :service)))))

(defun ende-codex--login-default-account ()
  "Suggest the current Codex profile's alias, otherwise default."
  (or (condition-case nil
          (when (fboundp 'ende-model-selection-for-buffer)
            (let* ((selection (ende-model-selection-for-buffer))
                   (profile (ende-provider-profile (plist-get selection :provider))))
              (when (eq (plist-get profile :service) 'openai-codex)
                (plist-get profile :account))))
        (error nil))
      'default))

(defun ende-codex--login-report (record failure)
  "Report completion using only verified RECORD metadata or safe FAILURE."
  (if failure (message "%s" (ende-error-message failure))
    (let ((scopes (ende-codex-auth--scopes (plist-get record :scope))))
      (message "ChatGPT account %s connected%s%s"
               (plist-get record :account)
               (if (member "chatgpt.tokens.use.direct" scopes) "" "; plan usage was not authorized")
               (if (member "offline_access" scopes) "" "; renewable access was not authorized")))))

;;;###autoload
(defun ende-codex-login (account)
  "Authorize existing Ende ACCOUNT alias using the system browser.
Only one attempt may run at a time; use `ende-codex-cancel-login' to cancel."
  (interactive
   (list (intern (read-string "ChatGPT account: " (symbol-name (ende-codex--login-default-account))))))
  (ende-codex-auth--alias account)
  (when ende-codex--pending-login (user-error "A ChatGPT login is active; cancel it first"))
  (let* ((existing (ende-codex-auth-lookup account))
         (host (ende-codex-auth-host-id))
         (attempt (ende-codex--make-login
                   :account account :existing existing :host-id host
                   :client-id (or (plist-get existing :client-id)
                                  (gethash (list host account) ende-codex--issued-clients)
                                  "dynamic_agent_client")
                   :state (ende-codex--login-random) :nonce (ende-codex--login-random)
                   :verifier (ende-codex--login-random) :callback #'ende-codex--login-report)))
    (setq ende-codex--pending-login attempt)
    (let ((debug-on-error nil) (debug-on-signal nil) (debug-on-quit nil))
      (condition-case nil
          (progn
            (ende-codex--login-listen attempt)
            (setf (ende-codex--login-timer attempt)
                  (run-at-time 120 nil #'ende-codex--login-fail attempt "ChatGPT login timed out"))
            (browse-url (ende-codex--login-authorize-url attempt))
            (message "Complete ChatGPT login in your browser; M-x ende-codex-cancel-login cancels"))
        ((error quit) (ende-codex--login-fail attempt "ChatGPT login could not be started"))))))

(add-hook 'ende-queue-abort-functions #'ende-codex-cancel-login)

(provide 'ende-codex-login)
;;; ende-codex-login.el ends here
