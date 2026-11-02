;;; ende-codex-login-test.el --- OAuth trust boundary -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'ende-codex-auth-test)
(require 'ende-rs256-fixtures)
(require 'ende-codex-login)

(ert-deftest ende-codex-login-trust-and-lifecycle ()
  (ende-codex-auth-test--with-store
   (let ((ende-codex--pending-login nil)
         (ende-codex--issued-clients (make-hash-table :test #'equal))
         (jwks (json-parse-string
                (json-encode `((keys . [,(alist-get 'jwk ende-rs256-test--fixtures)]))))) browser result failure requests)
     (puthash "keys" (vconcat (gethash "keys" jwks)
                              (vector (json-parse-string "{\"kty\":\"EC\",\"kid\":\"unrelated\"}"))) jwks)
     (cl-letf (((symbol-function 'float-time) (lambda (&rest _) 1800000100.0))
               ((symbol-function 'ende-codex--login-random) (lambda () "test-nonce"))
               ((symbol-function 'browse-url) (lambda (url &rest _) (setq browser url)))
               ((symbol-function 'ende-codex-auth--jwks-async)
                (lambda (callback) (funcall callback jwks nil) #'ignore)))
       (dolist (scenario '(valid state signature nonce identity permission denied))
         (let* ((before (ende-codex-auth-lookup 'chatgpt-plan-oauth 'personal))
                (token (alist-get (if (eq scenario 'nonce) 'nonce 'valid) ende-rs256-test--fixtures))
                (scope (if (eq scenario 'permission) "openid offline_access" (ende-codex--auth-property 'chatgpt-plan-oauth :scopes)))
                client)
           (when (eq scenario 'signature) (setq token (concat (substring token 0 -2) "AA")))
           (when (eq scenario 'identity)
             (setq before (ende-codex-auth--register 'chatgpt-plan-oauth
                           'personal (plist-put (ende-codex-auth-test--credentials) :client-id "test-client")
                           (plist-get before :revision) t)))
           (cl-letf (((symbol-function 'ende-codex-auth--request)
                      (lambda (url form _limit callback)
                        (push (list url form) requests)
                        (funcall callback
                                 (ende-codex-auth--json
                                  (json-encode `((access_token . "access") (refresh_token . "refresh")
                                                 (id_token . ,token) (token_type . "Bearer")
                                                 (expires_in . 3600) (scope . ,scope)))) nil)
                        #'ignore)))
             (setq result nil failure nil requests nil)
             (ende-codex--begin-login 'chatgpt-plan-oauth 'personal #'ende-codex--login-report)
             (let* ((attempt ende-codex--pending-login)
                    (listener (ende-codex--login-listener attempt))
                    (port (process-contact listener :service))
                    (query (ende-codex-auth--form
                            `(("state" . ,(if (eq scenario 'state) "wrong" "test-nonce"))
                              (,(if (eq scenario 'denied) "error" "code") . ,(if (eq scenario 'denied) "access_denied" "secret-code"))
                              ("client_id" . "test-client")))))
               (setf (ende-codex--login-callback attempt)
                     (lambda (record error) (setq result record failure error)))
               (should (string-prefix-p (ende-codex--auth-property 'chatgpt-plan-oauth :authorize-url) browser))
               (unwind-protect
                   (progn
                     (setq client (make-network-process :name "ende-login-test" :host "127.0.0.1"
                                                        :service port :family 'ipv4 :noquery t
                                                        :filter #'ignore :sentinel #'ignore))
                     (process-send-string client "GET /auth/callback?")
                     (accept-process-output nil 0.01)
                     (process-send-string client
                                          (format "%s HTTP/1.1\r\nHost: 127.0.0.1:%d\r\n\r\n" query port))
                     (let ((tries 100))
                       (while (and ende-codex--pending-login (> tries 0))
                         (cl-decf tries) (accept-process-output nil 0.01)))
                     (should-not ende-codex--pending-login)
                     (should-not (process-live-p listener))
                     (if (memq scenario '(valid permission identity))
                         (progn (should result) (should-not failure)
                                (should (equal (plist-get result :scope) scope)))
                       (should failure)
                       (should (equal before (ende-codex-auth-lookup 'chatgpt-plan-oauth 'personal))))
                     (when (memq scenario '(state denied)) (should-not requests)))
                 (when client (delete-process client))
                 (ende-codex-cancel-login))))
           (when (eq scenario 'identity)
             (ende-codex-auth--register 'chatgpt-plan-oauth 'personal
                                        (list :client-id "test-client" :subject "test-subject"
                                              :access-token "access" :refresh-token "refresh" :id-token token
                                              :expires-at 1800003700)
                                        (plist-get (ende-codex-auth-read 'chatgpt-plan-oauth 'personal) :revision) t))))
       (ende-codex--begin-login 'chatgpt-plan-oauth 'personal #'ende-codex--login-report)
       (let ((listener (ende-codex--login-listener ende-codex--pending-login)))
         (ende-reload)
         (should-not ende-codex--pending-login)
         (should-not (process-live-p listener)))))))

(provide 'ende-codex-login-test)
;;; ende-codex-login-test.el ends here
