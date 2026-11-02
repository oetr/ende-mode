;;; ende-codex-dual-test.el --- Dual OAuth integration -*- lexical-binding: t; -*-

(require 'ert)
(require 'ende-test-helpers)
(require 'ende-codex-auth-test)
(require 'ende-selector)
(require 'ende-agent)

(defun ende-codex-dual-test--credentials (method)
  "Return synthetic credentials specific to METHOD."
  (let ((credentials (ende-codex-auth-test--credentials)))
    (setq credentials (plist-put credentials :scope (ende-codex--auth-property method :scopes)))
    (if (eq method 'codex-oauth)
        (append (list :chatgpt-account-id "workspace")
                (plist-put credentials :client-id ende-codex-native-client-id))
      credentials)))

(ert-deftest ende-codex-dual-wire-body-and-affinity ()
  (let ((ende-builtin-provider-settings nil) bodies)
    (dolist (method '(chatgpt-plan-oauth codex-oauth))
      (let* ((profile (ende-codex--account-profile
                       (ende-provider-config-resolve 'openai-codex "model" nil (list :auth-method method))))
             (profile (plist-put profile :codex-session-id "stable-session"))
             (out (ende-test--run-stream-request
                   'codex '("data: {\"type\":\"response.completed\",\"response\":{\"output\":[]}}\n\n")
                   nil nil profile)))
        (should-not (plist-get out :error))
        (should (member (ende-codex--auth-property method :responses-url) (plist-get out :command)))
        (should (string-match-p "session-id: stable-session" (plist-get out :config-text)))
        (should (eq (not (null (string-match-p "ChatGPT-Account-Id:" (plist-get out :config-text))))
                    (eq method 'codex-oauth)))
        (push (plist-get out :request-body) bodies)))
    (should (equal (car bodies) (cadr bodies)))))

(ert-deftest ende-codex-dual-queue-reconnect-and-delegation ()
  (ende-codex-auth-test--with-store
    (let* ((ende-builtin-provider-settings nil)
           (ende-credentials--bindings (make-hash-table :test #'eq))
           (siwc (ende-codex-auth--register 'chatgpt-plan-oauth 'personal
                                          (ende-codex-dual-test--credentials 'chatgpt-plan-oauth)))
           (native (ende-codex-auth--register 'codex-oauth 'personal
                                            (ende-codex-dual-test--credentials 'codex-oauth)))
           (profile (ende-codex-capture-binding
                     (ende-provider--compose-profile 'openai-codex "model" nil
                                                     '(:auth-method codex-oauth :account personal))))
           (other (ende-codex-capture-binding
                   (ende-provider--compose-profile 'openai-codex "model" nil
                                                   '(:auth-method chatgpt-plan-oauth :account personal))))
           (family (ende--make-job-family :inference-template profile))
           (child (ende-agent-profile-for-policy family nil)))
      (should (eq (plist-get child :auth-method) 'codex-oauth))
      (should (eq (plist-get child :codex-binding) (plist-get profile :codex-binding)))
      (should-not (ende-codex--check-binding child native))
      (should-error (ende-codex--check-binding profile siwc))
      (let ((replacement (ende-codex-auth--register
                          'codex-oauth 'personal (ende-codex-dual-test--credentials 'codex-oauth)
                          (plist-get native :revision) t)))
        (should-error (ende-codex--check-binding child replacement))
        (should-not (ende-codex--check-binding other siwc)))
      (should-error (ende-codex--profile-auth
                     (plist-put (copy-tree profile) :account 'missing))))))

(ert-deftest ende-codex-dual-refresh-isolation-and-rotation ()
  (ende-codex-auth-test--with-store
    (let* ((ende-codex--refresh-flights (make-hash-table :test #'equal))
           (siwc (ende-codex-auth--register 'chatgpt-plan-oauth 'personal
                                          (ende-codex-dual-test--credentials 'chatgpt-plan-oauth)))
           (native (ende-codex-auth--register 'codex-oauth 'personal
                                            (ende-codex-dual-test--credentials 'codex-oauth)))
           flights results cancels)
      (cl-letf (((symbol-function 'ende-codex--lock-refresh)
                 (lambda (flight) (cl-pushnew flight flights))))
        (dolist (record (list native native siwc))
          (push (ende-codex--refresh-auth record (lambda (result) (push result results))) cancels))
        (should (= 2 (length flights)))
        (should (= 2 (hash-table-count ende-codex--refresh-flights)))
        (dolist (flight flights)
          (let ((method (plist-get (ende-codex--refresh-flight-auth flight) :auth-method)))
            (ende-codex--refresh-response
             flight 200 (ende-codex-auth--json
                         (format "{\"access_token\":\"new-%s\",\"refresh_token\":\"rotated-%s\",\"token_type\":\"Bearer\",\"expires_in\":1200}" method method)))))
        (should (= 3 (length results)))
        (should (= 0 (hash-table-count ende-codex--refresh-flights)))
        (dolist (method '(chatgpt-plan-oauth codex-oauth))
          (let ((record (ende-codex-auth-read method 'personal)))
            (should (equal (plist-get record :refresh-token) (format "rotated-%s" method)))
            (should (ende-codex-auth--same-registration-p
                     record (if (eq method 'codex-oauth) native siwc)))))
        (mapc #'funcall cancels)))))

(ert-deftest ende-codex-dual-native-identity-continuity ()
  (let* ((claims (ende-codex-auth--json
                  (format "{\"iss\":\"https://auth.openai.com\",\"aud\":\"%s\",\"sub\":\"subject\",\"iat\":%d,\"exp\":%d,\"https://api.openai.com/auth\":{\"chatgpt_account_id\":\"workspace\"}}"
                          ende-codex-native-client-id (- (floor (float-time)) 10) (+ (floor (float-time)) 3600))))
         (record (list :auth-method 'codex-oauth :client-id ende-codex-native-client-id
                       :subject "subject" :chatgpt-account-id "workspace")))
    (cl-letf (((symbol-function 'ende-codex-auth--signed-claims) (lambda (&rest _) claims)))
      (should (eq claims (ende-codex--verify-id-token 'codex-oauth "synthetic" ende-codex-native-client-id nil)))
      (should (eq claims (ende-codex-auth--refresh-claims "synthetic" record nil)))
      (dolist (pair '((:subject . "changed") (:chatgpt-account-id . "changed") (:client-id . "issued-siwc")))
        (should-error (ende-codex-auth--refresh-claims
                       "synthetic" (plist-put (copy-tree record) (car pair) (cdr pair)) nil)))
      (dolist (pair '(("iss" . "https://attacker.test") ("aud" . "wrong") ("exp" . 1) ("sub" . "")))
        (let ((original (gethash (car pair) claims)))
          (puthash (car pair) (cdr pair) claims)
          (should-error (ende-codex--verify-id-token 'codex-oauth "synthetic" ende-codex-native-client-id nil))
          (puthash (car pair) original claims))))))

(ert-deftest ende-codex-dual-listener-cancellation ()
  (let ((ende-codex--pending-login nil) listener)
    (unwind-protect
        (cl-letf (((symbol-function 'browse-url) #'ignore)
                  ((symbol-function 'ende-codex-auth-lookup) (lambda (&rest _) nil))
                  ((symbol-function 'ende-codex-auth-host-id) (lambda () "host")))
          (ende-codex--begin-login 'codex-oauth 'personal #'ignore)
          (setq listener (ende-codex--login-listener ende-codex--pending-login))
          (should (= 1455 (process-contact listener :service)))
          (should (equal "127.0.0.1" (process-contact listener :host)))
          (should (equal "http://localhost:1455/auth/callback"
                         (ende-codex--login-redirect ende-codex--pending-login)))
          (ende-codex-cancel-login)
          (should-not ende-codex--pending-login)
          (should-not (process-live-p listener)))
      (ende-codex-cancel-login))))

(ert-deftest ende-codex-dual-recognized-catalogs ()
  (should (equal (ende-codex--parse-catalog "{\"data\":[{\"id\":\"model\"}]}" 'chatgpt-plan-oauth)
                 '((:id "model"))))
  (should (equal (plist-get (car (ende-codex--parse-catalog
                                "{\"models\":[{\"slug\":\"model\",\"visibility\":\"list\"}]}" 'codex-oauth)) :id) "model"))
  (dolist (method '(chatgpt-plan-oauth codex-oauth))
    (should-error (ende-codex--parse-catalog "{\"error\":\"denied\"}" method))))

(ert-deftest ende-codex-dual-catalog-command-keeps-query-out-of-profile-validation ()
  (let* ((native (ende-provider-config-resolve 'openai-codex nil nil
                                                '(:auth-method codex-oauth)))
         (url (ende-codex--catalog-request-url native))
         (command (ende-codex--catalog-command url "headers" "config")))
    (should (equal (ende-codex--catalog-url native)
                   "https://chatgpt.com/backend-api/codex/models"))
    (should (equal (ende-curl--validated-url (list :url (ende-codex--catalog-url native)))
                   "https://chatgpt.com/backend-api/codex/models"))
    (should (equal (cadr (member "--url" command)) url))
    (should (string-match-p "client_version=1.0.0" (cadr (member "--url" command))))))

(ert-deftest ende-codex-dual-siwc-model-catalog-recognized-shapes ()
  (should (equal (ende-codex--parse-catalog
                  "{\"data\":[{\"id\":\"gpt-public\"}]}" 'chatgpt-plan-oauth)
                 '((:id "gpt-public"))))
  (should (equal (mapcar (lambda (entry) (plist-get entry :id))
                         (ende-codex--parse-catalog
                          "{\"models\":[{\"slug\":\"gpt-plan\",\"visibility\":\"list\",\"display_name\":\"GPT Plan\"}]}"
                          'chatgpt-plan-oauth))
                 '("gpt-plan")))
  (should-error (ende-codex--parse-catalog "{\"error\":\"denied\"}" 'chatgpt-plan-oauth)))

(ert-deftest ende-codex-dual-catalog-url-advertises-ende-version ()
  (let ((siwc (ende-provider-config-resolve 'openai-codex nil nil
                                             '(:auth-method chatgpt-plan-oauth)))
        (native (ende-provider-config-resolve 'openai-codex nil nil
                                               '(:auth-method codex-oauth))))
    (should (equal (ende-codex--catalog-request-url siwc)
                   "https://api.openai.com/v1/models"))
    (should (equal (ende-codex--catalog-request-url native)
                   (concat "https://chatgpt.com/backend-api/codex/models?client_version="
                           (url-hexify-string ende-client-version))))
    (should (string= ende-client-version "1.0.0"))))

(ert-deftest ende-codex-dual-selector-method-draft-and-apply ()
  (save-window-excursion
    (let* ((source (generate-new-buffer " *dual-selector-source*"))
           (ende-providers '(openai-codex))
           (ende-builtin-provider-settings '((openai-codex :models ("model"))))
           (ende-model-selection--last-used nil)
           (ende-model-selection--buffer-selections (make-hash-table :test #'equal))
           (ende-provider--catalog-cache (make-hash-table :test #'equal))
           connected)
      (unwind-protect
          (cl-letf (((symbol-function 'browse-url) (lambda (&rest _) (ert-fail "Unexpected browser")))
                    ((symbol-function 'ende-codex-auth-lookup) (lambda (&rest _) nil))
                    ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                    ((symbol-function 'ende-codex--begin-login)
                     (lambda (method account _callback) (setq connected (list method account)))))
            (switch-to-buffer source)
            (ende-selector-open)
            (let* ((selector (current-buffer))
                   (old-key (plist-get (ende-selector--provider-profile) :model-discovery-key)))
              (ende-selector-configure-connection)
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (_prompt choices &rest _)
                           (should (equal choices '("chatgpt-plan-oauth" "codex-oauth")))
                           "codex-oauth")))
                (ende-selector-connection-auth))
              (should-not connected)
              (let ((profile (ende-selector-connection--profile)))
                (should (equal (plist-get profile :url) "https://chatgpt.com/backend-api/codex/responses"))
                (should-not (equal old-key (plist-get profile :model-discovery-key))))
              (ende-selector-connection-apply)
              (should (equal connected '(codex-oauth default)))
              (should (eq (current-buffer) selector))
              (should (eq (plist-get (ende-selector--provider-profile) :auth-method) 'codex-oauth))
              (ende-selector-configure-connection)
              (should (eq (plist-get (ende-selector-connection--profile) :auth-method) 'codex-oauth))
              (should (string-match-p "codex-oauth" (buffer-string)))
              (ende-selector-connection-cancel)))
        (when-let* ((selector (get-buffer ende-selector-buffer-name)))
          (with-current-buffer selector (ende-selector-close)))
        (kill-buffer source)))))

(ert-deftest ende-codex-dual-native-login-saves-verified-account ()
  (ende-codex-auth-test--with-store
    (let* ((ende-codex--pending-login nil)
           (claims (ende-codex-auth--json
                    (format "{\"iss\":\"https://auth.openai.com\",\"aud\":\"%s\",\"sub\":\"verified\",\"iat\":%d,\"exp\":%d,\"https://api.openai.com/auth\":{\"chatgpt_account_id\":\"workspace\"}}"
                            ende-codex-native-client-id (- (floor (float-time)) 10) (+ (floor (float-time)) 3600))))
           (payload (ende-codex-auth--json
                     "{\"access_token\":\"native-access\",\"refresh_token\":\"native-refresh\",\"id_token\":\"signed-token\",\"token_type\":\"Bearer\",\"expires_in\":1234}"))
           result failure requests listener)
      (cl-letf (((symbol-function 'browse-url) #'ignore)
                ((symbol-function 'ende-codex-auth--signed-claims) (lambda (&rest _) claims))
                ((symbol-function 'ende-codex-auth--jwks-async)
                 (lambda (callback) (funcall callback 'trusted-keys nil) #'ignore))
                ((symbol-function 'ende-codex-auth--request)
                 (lambda (url form _limit callback)
                   (push (list url form) requests)
                   (funcall callback payload nil) #'ignore)))
        (unwind-protect
            (progn
              (ende-codex--begin-login 'codex-oauth 'personal
                                       (lambda (record error) (setq result record failure error)))
              (let ((attempt ende-codex--pending-login))
                (setq listener (ende-codex--login-listener attempt))
                (ende-codex--login-receive-callback
                 attempt (list (cons "state" (ende-codex--login-state attempt)) '("code" . "authorization-code")))
                (should-error (ende-codex--login-receive-callback attempt '(("code" . "authorization-code")))))
              (should-not failure)
              (should-not (process-live-p listener))
              (should (eq (plist-get result :auth-method) 'codex-oauth))
              (should (equal (plist-get result :chatgpt-account-id) "workspace"))
              (should (equal (plist-get result :client-id) ende-codex-native-client-id))
              (should (<= (abs (- (plist-get result :expires-at) (+ (floor (float-time)) 1234))) 1))
              (should (equal result (ende-codex-auth-read 'codex-oauth 'personal)))
              (should-not (ende-codex-auth-lookup 'chatgpt-plan-oauth 'personal))
              (should (= 1 (length requests)))
              (should (equal (caar requests) "https://auth.openai.com/oauth/token"))
              (should-not (string-match-p "resource=" (cadar requests))))
          (ende-codex-cancel-login))))))

(ert-deftest ende-codex-dual-refresh-http-boundary ()
  (let ((ende-codex--refresh-flights (make-hash-table :test #'equal))
        (ende-codex--refresh-processes nil)
        (real-make-process (symbol-function 'make-process))
        command sent result cancel)
    (dolist (method '(chatgpt-plan-oauth codex-oauth))
      (let ((auth (append (list :auth-method method :account 'personal)
                          (ende-codex-dual-test--credentials method))))
        (unwind-protect
            (cl-letf (((symbol-function 'ende-codex--lock-refresh) #'ende-codex--start-refresh-flight)
                      ((symbol-function 'make-process)
                       (lambda (&rest args)
                         (setq command (plist-get args :command))
                         (funcall real-make-process :name "ende-refresh-mock" :command '("cat")
                                  :noquery t :connection-type 'pipe :filter #'ignore :sentinel #'ignore)))
                      ((symbol-function 'process-send-string) (lambda (_process body) (setq sent body)))
                      ((symbol-function 'process-send-eof) #'ignore)
                      ((symbol-function 'ende-codex-auth--rotate)
                       (lambda (expected credentials) (append (list :auth-method (plist-get expected :auth-method)) credentials))))
              (setq result nil cancel (ende-codex--refresh-auth auth (lambda (value) (setq result value))))
              (should (member (ende-codex--auth-property method :token-url) command))
              (should (equal sent (ende-codex--refresh-body method auth)))
              (let* ((flight (gethash (ende-codex--refresh-key auth) ende-codex--refresh-flights))
                     (process (ende-codex--refresh-flight-process flight)))
                (with-temp-file (ende-codex--refresh-flight-headers-file flight)
                  (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n"))
                (ende-codex--refresh-filter
                 flight process "{\"access_token\":\"new-access\",\"token_type\":\"Bearer\",\"expires_in\":1800}")
                (cl-letf (((symbol-function 'process-status) (lambda (_) 'exit))
                          ((symbol-function 'process-exit-status) (lambda (_) 0)))
                  (ende-codex--refresh-sentinel flight process "finished")))
              (should (equal (plist-get result :access-token) "new-access"))
              (should (equal (plist-get result :refresh-token) (plist-get auth :refresh-token))))
          (when cancel (funcall cancel)))))))

(ert-deftest ende-codex-dual-profile-and-header-injection-rejected ()
  (let ((ende-builtin-provider-settings nil))
    (dolist (method '(chatgpt-plan-oauth codex-oauth))
      (let* ((profile (ende-provider--compose-profile 'openai-codex "model" nil (list :auth-method method)))
             (other (if (eq method 'codex-oauth) 'chatgpt-plan-oauth 'codex-oauth)))
        (should (ende-provider-validate-profile profile))
        (dolist (pair '((:url . :responses-url) (:models-url . :models-url)))
          (should-error
           (ende-provider-validate-profile
            (plist-put (copy-tree profile) (car pair) (ende-codex--auth-property other (cdr pair))))))
        (should-error (ende-provider-validate-profile (plist-put (copy-tree profile) :base-url "https://example.org"))))))
  (dolist (header '("ChatGPT-Account-Id: attacker" "originator: opencode"))
    (should-error (ende-provider-config--headers (list header)))))

(provide 'ende-codex-dual-test)
;;; ende-codex-dual-test.el ends here
