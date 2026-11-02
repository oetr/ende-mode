;;; ende-codex-login.el --- Browser authorization for Ende -*- lexical-binding: t; -*-

;;; Code:

(require 'browse-url)
(require 'url-util)
(require 'ende-codex-auth)

(cl-defstruct (ende-codex--login (:constructor ende-codex--make-login))
  method account existing client-id state nonce verifier redirect host-id listener clients
  timer cancel callback consumed finished)

(defvar ende-codex--pending-login nil)
(defvar ende-codex--issued-clients (make-hash-table :test #'equal)
  "Issued IDs awaiting a verified registration after a consumed code failed.")

(defvar ende-codex-login-complete-functions nil
  "Functions called with (RECORD FAILURE) when an OAuth login completes.")

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
      (when-let* ((timer (process-get process :timeout)))
        (cancel-timer timer)
        (process-put process :timeout nil))
      (when (process-live-p process) (delete-process process))))
  (setf (ende-codex--login-listener attempt) nil
        (ende-codex--login-clients attempt) nil))

(defun ende-codex--login-discard-timeout (process)
  "Cancel PROCESS's incomplete-request timeout."
  (when-let* ((timer (process-get process :timeout)))
    (cancel-timer timer)
    (process-put process :timeout nil)))

(defun ende-codex--login-discard-client (attempt process)
  "Remove PROCESS and its timeout from ATTEMPT."
  (ende-codex--login-discard-timeout process)
  (setf (ende-codex--login-clients attempt)
        (delq process (ende-codex--login-clients attempt))))

(defun ende-codex--login-reject-client (attempt process)
  "Reject one invalid callback connection without affecting ATTEMPT."
  (when (process-live-p process)
    (ignore-errors (ende-codex--login-http-response process nil))
    (delete-process process))
  (ende-codex--login-discard-client attempt process))

(defun ende-codex--login-client-sentinel (attempt process _event)
  "Remove a closed callback PROCESS from ATTEMPT immediately."
  (unless (process-live-p process)
    (ende-codex--login-discard-client attempt process)))

(defun ende-codex--login-client-timeout (attempt process)
  "Reject an incomplete callback PROCESS after its short timeout."
  (ende-codex--login-reject-client attempt process))

(defun ende-codex--login-stop-accepting (attempt &optional preserve)
  "Stop ATTEMPT's listener and reject any other pending clients."
  (let ((listener (ende-codex--login-listener attempt)))
    (when (processp listener)
      (set-process-filter listener #'ignore)
      (set-process-sentinel listener #'ignore)
      (when (process-live-p listener) (delete-process listener)))
    (setf (ende-codex--login-listener attempt) nil))
  (dolist (client (copy-sequence (ende-codex--login-clients attempt)))
    (unless (eq client preserve)
      (ende-codex--login-reject-client attempt client))))

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
            ("scope" . ,(ende-codex--auth-property (ende-codex--login-method attempt) :scopes))
            ("state" . ,(ende-codex--login-state attempt))
            ("nonce" . ,(ende-codex--login-nonce attempt))
            ("code_challenge_method" . "S256")
            ("code_challenge" . ,(ende-codex--pkce-challenge (ende-codex--login-verifier attempt)))
            ("ext_agent_host_id" . ,(ende-codex--login-host-id attempt)))))
    (if (ende-codex--auth-property (ende-codex--login-method attempt) :nonce)
        (push (cons "resource" (ende-codex--auth-property (ende-codex--login-method attempt) :resource)) parameters)
      (setq parameters (cl-remove-if (lambda (pair) (member (car pair) '("nonce" "ext_agent_host_id"))) parameters))
      (push '("id_token_add_organizations" . "true") parameters)
      (push '("codex_cli_simplified_flow" . "true") parameters))
    (when (equal client "dynamic_agent_client")
      (push '("agent_name_hint" . "Ende") parameters))
    (when (plist-get existing :id-token)
      (push (cons "id_token_hint" (plist-get existing :id-token)) parameters))
    (when (plist-get existing :email)
      (push (cons "login_hint" (plist-get existing :email)) parameters))
    (concat (ende-codex--auth-property (ende-codex--login-method attempt) :authorize-url)
            "?" (ende-codex-auth--form parameters))))

(defun ende-codex--login-save (attempt payload jwks failure)
  "Verify PAYLOAD with trusted JWKS and save ATTEMPT unless FAILURE occurred."
  (when (ende-codex--login-live-p attempt)
    (if failure (ende-codex--login-finish attempt nil failure)
      (let ((debug-on-error nil) (debug-on-signal nil) (debug-on-quit nil))
        (condition-case nil
            (let* ((ende-codex-auth--trusted-jwks jwks)
                   (existing (ende-codex--login-existing attempt))
                   (method (ende-codex--login-method attempt))
                   (client (ende-codex--login-client-id attempt))
                   (claims (ende-codex--verify-id-token
                            method (gethash "id_token" payload) client (ende-codex--login-nonce attempt)))
                   (subject (gethash "sub" claims))
                   (credentials
                    (list :client-id client :subject subject
                          :access-token (gethash "access_token" payload)
                          :refresh-token (gethash "refresh_token" payload)
                          :id-token (gethash "id_token" payload)
                          :scope (or (gethash "scope" payload)
                                     (ende-codex--auth-property method :scopes))
                          :expires-at (+ (floor (float-time)) (gethash "expires_in" payload)))))
              (when (ende-codex--auth-property method :account-header)
                (setq credentials
                      (plist-put credentials :chatgpt-account-id
                                 (ende-codex-native-account-id-from-claims claims))))
              ;; A deliberate browser reconnection establishes a new binding.
              ;; Refresh, in contrast, must preserve subject and workspace.
              (when (and (eq (gethash "email_verified" claims) t)
                         (ende-codex-auth--text-p (gethash "email" claims))
                         (<= (length (gethash "email" claims)) 320))
                (setq credentials (plist-put credentials :email (gethash "email" claims))))
              (when (gethash "earliest_refresh_at" payload)
                (setq credentials (plist-put credentials :earliest-refresh-at
                                             (gethash "earliest_refresh_at" payload))))
              (let ((record (ende-codex-auth--register
                             method (ende-codex--login-account attempt) credentials
                             (plist-get existing :revision) (and existing t))))
                (remhash (list (ende-codex--login-method attempt) (ende-codex--login-host-id attempt) (ende-codex--login-account attempt))
                         ende-codex--issued-clients)
                (ende-codex--login-finish attempt record nil)))
          ((error quit) (ende-codex--login-fail attempt)))))))

(defun ende-codex--login-exchanged (attempt payload failure)
  "Validate ATTEMPT's token PAYLOAD and fetch signing keys, or report FAILURE."
  (when (ende-codex--login-live-p attempt)
    (if failure (ende-codex--login-finish attempt nil failure)
      (condition-case nil
          (progn
            (ende-codex-auth--token-payload (ende-codex--login-method attempt) payload)
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
        (when (assoc key parameters)
          (signal 'ende-codex-identity-error nil))
        (push (cons key value) parameters)))
    parameters))

(defun ende-codex--login-validate-callback (attempt parameters)
  "Validate callback PARAMETERS for ATTEMPT without consuming its state."
  (dolist (key '("state" "code" "client_id" "error"))
    (when (> (cl-count key parameters :key #'car :test #'equal) 1)
      (signal 'ende-codex-identity-error nil)))
  (unless (and (ende-codex--login-live-p attempt)
               (not (ende-codex--login-consumed attempt))
               (equal (cdr (assoc "state" parameters)) (ende-codex--login-state attempt)))
    (signal 'ende-codex-identity-error nil))
  (if (assoc "error" parameters)
      (progn
        (when (assoc "code" parameters)
          (signal 'ende-codex-identity-error nil))
        (unless (ende-codex--login-identifier-p (cdr (assoc "error" parameters)))
          (signal 'ende-codex-identity-error nil))
        'denied)
    (let ((code (cdr (assoc "code" parameters)))
          (returned (cdr (assoc "client_id" parameters)))
          (saved (ende-codex--login-client-id attempt)))
      (unless (ende-codex--login-identifier-p code) (signal 'ende-codex-identity-error nil))
      (if (equal saved "dynamic_agent_client")
          (unless (and (ende-codex--login-identifier-p returned)
                       (not (equal returned "dynamic_agent_client")))
            (signal 'ende-codex-identity-error nil))
        (when (and returned (not (equal returned saved))) (signal 'ende-codex-identity-error nil)))
      (list code returned))))

(defun ende-codex--login-receive-callback (attempt parameters)
  "Validate and consume callback PARAMETERS for ATTEMPT exactly once."
  (let ((callback (ende-codex--login-validate-callback attempt parameters)))
    (setf (ende-codex--login-consumed attempt) t)
    (ende-codex--login-stop-accepting attempt)
    (if (eq callback 'denied)
        (ende-codex--login-fail attempt "ChatGPT authorization was declined")
      (let ((code (car callback))
            (returned (cadr callback)))
        (when (equal (ende-codex--login-client-id attempt) "dynamic_agent_client")
          (setf (ende-codex--login-client-id attempt) returned)
          (puthash (list (ende-codex--login-method attempt)
                         (ende-codex--login-host-id attempt)
                         (ende-codex--login-account attempt))
                   returned ende-codex--issued-clients))
        (condition-case nil
            (setf (ende-codex--login-cancel attempt)
                  (ende-codex-auth--request
                   (ende-codex--auth-property (ende-codex--login-method attempt) :token-url)
                   (ende-codex--code-exchange-body (ende-codex--login-method attempt) attempt code)
                   262144
                   (apply-partially #'ende-codex--login-exchanged attempt)))
          ((error quit)
           (ende-codex--login-fail attempt)))))))

(defun ende-codex--code-exchange-body (method attempt code)
  "Encode METHOD's code exchange for ATTEMPT and CODE."
  (ende-codex-auth--form
   (append `(("grant_type" . "authorization_code")
             ("client_id" . ,(ende-codex--login-client-id attempt)) ("code" . ,code)
             ("code_verifier" . ,(ende-codex--login-verifier attempt))
             ("redirect_uri" . ,(ende-codex--login-redirect attempt)))
           (when-let* ((resource (ende-codex--auth-property method :resource)))
             (list (cons "resource" resource))))))

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
  (unless (process-get process :responded)
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
                (let ((key (downcase (match-string 1 line)))
                      (value (string-trim (match-string 2 line))))
                  (when (assoc key headers)
                    (signal 'ende-codex-identity-error nil))
                  (push (cons key value) headers)))
              (unless (and (equal (cdr (assoc "host" headers))
                                  (substring (ende-codex--login-redirect attempt) 7 -14))
                           (not (assoc "transfer-encoding" headers))
                           (or (not (assoc "content-length" headers))
                               (equal (cdr (assoc "content-length" headers)) "0")))
                (signal 'ende-codex-identity-error nil))
              (let ((parameters (ende-codex--login-query query)))
                (ende-codex--login-discard-timeout process)
                (ende-codex--login-http-response process t)
                (process-put process :responded t)
                (process-put process :request nil)
                (ende-codex--login-receive-callback attempt parameters)))))
      ((error quit) (ende-codex--login-reject-client attempt process))))))

(defun ende-codex--login-listen (attempt)
  "Bind ATTEMPT's one-shot callback listener specifically to IPv4 loopback."
  (let ((listener
         (make-network-process
          :name "ende-chatgpt-login" :server t :host "127.0.0.1" :family 'ipv4
          :service (or (ende-codex--auth-property (ende-codex--login-method attempt) :callback-port) t)
          :noquery t :coding 'binary
          :filter (apply-partially #'ende-codex--login-http-filter attempt)
          :sentinel #'ignore
          :log (lambda (_server client _message)
                 (if (or (not (ende-codex--login-live-p attempt))
                         (ende-codex--login-consumed attempt)
                         (>= (length (ende-codex--login-clients attempt)) 4))
                     (delete-process client)
                   (push client (ende-codex--login-clients attempt))
                   (set-process-query-on-exit-flag client nil)
                   (set-process-coding-system client 'binary 'binary)
                   (set-process-filter client
                                       (apply-partially #'ende-codex--login-http-filter attempt))
                   (set-process-sentinel client
                                         (apply-partially #'ende-codex--login-client-sentinel attempt))
                   (process-put client :timeout
                               (run-at-time 5 nil #'ende-codex--login-client-timeout attempt client)))))))
    (setf (ende-codex--login-listener attempt) listener
          (ende-codex--login-redirect attempt)
          (or (ende-codex--auth-property (ende-codex--login-method attempt) :redirect-uri)
              (format "http://127.0.0.1:%d/auth/callback" (process-contact listener :service))))))

(defun ende-codex--login-report (record failure)
  "Report completion using only verified RECORD metadata or safe FAILURE."
  (run-hook-with-args 'ende-codex-login-complete-functions record failure)
  (if failure (message "%s" (ende-error-message failure))
    (let ((scopes (ende-codex-auth--scopes (plist-get record :scope))))
      (message "ChatGPT account %s connected%s%s"
               (plist-get record :account)
               (if (or (not (ende-codex--auth-property (plist-get record :auth-method) :resource))
                       (member "chatgpt.tokens.use.direct" scopes))
                   "" "; plan usage was not authorized")
               (if (member "offline_access" scopes) "" "; renewable access was not authorized")))))

(defun ende-codex--begin-login (method account callback)
  "Authorize METHOD and ACCOUNT through the browser, reporting to CALLBACK.
Only one attempt may run at a time; cancellation releases every resource."
  (ende-codex--auth-spec method)
  (ende-codex-auth--alias account)
  (when ende-codex--pending-login (user-error "A ChatGPT login is active; cancel it first"))
  (let* ((existing (ende-codex-auth-lookup method account))
         (host (ende-codex-auth-host-id))
         (attempt (ende-codex--make-login
                   :method method :account account :existing existing :host-id host
                   :client-id (or (ende-codex--auth-property method :client-id)
                                  (plist-get existing :client-id)
                                  (gethash (list method host account) ende-codex--issued-clients)
                                  "dynamic_agent_client")
                   :state (ende-codex--login-random)
                   :nonce (and (ende-codex--auth-property method :nonce) (ende-codex--login-random))
                   :verifier (ende-codex--login-random) :callback callback)))
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
