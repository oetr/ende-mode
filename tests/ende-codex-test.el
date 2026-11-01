;;; ende-codex-test.el --- Codex contracts -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'ende-test-helpers)
(require 'ende-codex-auth-test)
(require 'ende-codex-usage)

(ert-deftest ende-codex-account-isolation-and-refresh ()
  (ende-codex-auth-test--with-store
   (let* ((personal (ende-codex-auth--register 'personal (ende-codex-auth-test--credentials)))
          (work (ende-codex-auth--register 'work (ende-codex-auth-test--credentials "work")))
          (profile (ende-codex-capture-binding (ende-codex-auth-test--profile 'personal)))
          (payload (ende-codex-auth--json
                    "{\"access_token\":\"rotated-access\",\"refresh_token\":\"rotated-refresh\",\"token_type\":\"Bearer\",\"expires_in\":3600}"))
          resolved)
     (ende-codex--resolve-auth (lambda (auth) (setq resolved auth)) profile)
     (should (equal personal resolved))
     (let ((next (ende-codex--store-refresh personal "rotated-access" "rotated-refresh" 3600 nil payload)))
       (should (ende-codex-auth--same-registration-p personal next))
       (should (equal (plist-get personal :scope) (plist-get next :scope)))
       (should-error (ende-codex--store-refresh personal "stale" "stale" 3600 nil payload)
                     :type 'ende-codex-auth-conflict)
       (should-not (ende-codex--check-binding profile next))
       (should (equal work (ende-codex-auth-read 'work)))
       (puthash "scope" "openid offline_access" payload)
       (setq next (ende-codex--store-refresh next "limited" "limited-refresh" 3600 nil payload))
       (ende-codex--resolve-auth (lambda (auth) (setq resolved auth)) profile)
       (should (ende-error-p resolved))
       (setq next (ende-codex-auth--require-login next))
       (should (plist-get next :needs-login))
       (should-not (plist-get next :access-token))
       (should-not (plist-get next :refresh-token))
       (should (ende-codex-auth--same-registration-p personal next)))
     (should-not (string-match-p "scope=" (ende-codex--refresh-body "secret" "issued")))
     (should (string-match-p "resource=" (ende-codex--refresh-body "secret" "issued")))
     (should (equal (ende-codex--validated-refresh-url) ende-codex--token-url)))))

(ert-deftest ende-codex-stream-contract ()
  (let* ((stream (concat "data: {\"type\":\"response.output_text.delta\",\"delta\":\"answer\"}\n\n"
                         "data: {\"type\":\"response.completed\",\"response\":{\"usage\":{\"output_tokens\":7}}}\n\n"))
         (out (ende-test--run-stream-request 'codex (list stream))))
    (should-not (plist-get out :error))
    (should (equal (plist-get (plist-get out :result) :content) "answer"))
    (should-not (string-match-p "transport-test-token" (prin1-to-string (plist-get out :command))))
    (should (string-match-p "Authorization: Bearer transport-test-token" (plist-get out :config-text))))
  (let* ((definition
          (list (cons :type "function")
                (cons :function (list (cons :name "read_file")
                                      (cons :description "Read a file")
                                      (cons :parameters '((:type . "object")))))))
         (body (ende-codex--request-body
                (list '("system" . "Be brief") '("user" . "Read it")
                      (list :tool-call-id "call_1" :content "File contents"))
                (list :model "model-a" :tool-definitions (list definition))))
         (input (alist-get :input body))
         (tools (alist-get :tools body)))
    (should (eq (alist-get :store body) json-false))
    (should (eq (alist-get :stream body) t))
    (should (equal (alist-get :model body) "model-a"))
    (should (equal (alist-get :role (car input)) "developer"))
    (should (equal (alist-get :call_id (car (last input))) "call_1"))
    (should (equal (alist-get :type (car tools)) "namespace"))
    (should (equal (alist-get :name (car (alist-get :tools (car tools)))) "read_file"))
    (should-not (alist-get :max_output_tokens body))))

(ert-deftest ende-codex-endpoints-are-pinned ()
  (dolist (url (list ende-codex--authorize-url ende-codex--token-url
                     ende-codex--jwks-url ende-codex--models-url
                     ende-codex--responses-url))
    (should (equal (ende-codex-auth--endpoint url url) url)))
  (should-error (ende-codex-auth--endpoint
                 "https://auth.openai.com:8443/api/accounts/authorize"
                 ende-codex--authorize-url))
  (should-error (ende-codex-auth--endpoint
                 ende-codex--models-url ende-codex--responses-url))
  (should-error (ende-codex-auth--endpoint
                 "https://attacker.test/api/accounts/authorize"
                 ende-codex--authorize-url))
  (dolist (url '("http://auth.openai.com/api/accounts/authorize"
                 "https://auth.openai.com/api/accounts/authorize?x=1"
                 "https://auth.openai.com/api/accounts/authorize#x"
                 "https://auth.openai.com/api/accounts/token"))
    (should-error (ende-codex-auth--endpoint url ende-codex--authorize-url))))

(ert-deftest ende-codex-requires-completion-event ()
  (let ((completed (ende-test--run-stream-request
                    'codex
                    (list "data: {\"type\":\"response.completed\",\"response\":{\"output\":[]}}\n\n")))
        (done (ende-test--run-stream-request
               'codex
               (list "data: {\"type\":\"response.done\",\"response\":{\"output\":[]}}\n\n")))
        (missing (ende-test--run-stream-request
                  'codex
                  (list "data: {\"type\":\"response.output_text.delta\",\"delta\":\"partial\"}\n\n"))))
    (should-not (plist-get completed :error))
    (should (plist-get completed :result))
    (should (ende-error-p (plist-get done :error)))
    (should (ende-error-p (plist-get missing :error)))))

(ert-deftest ende-codex-catalog-keeps-listed-metadata ()
  (let* ((models (json-parse-string
                  (json-encode
                   (list (cons 'models
                               (vector
                                (list (cons 'slug "model-a")
                                      (cons 'display_name "Model A")
                                      (cons 'visibility "list")
                                      (cons 'default_reasoning_level "high")
                                      (cons 'supported_reasoning_levels
                                            (vector (list (cons 'effort "low"))
                                                    (list (cons 'effort "high")))))
                                (list (cons 'slug "internal")
                                      (cons 'visibility "hidden"))))))))
         (entries (delq nil (mapcar #'ende-codex--catalog-entry
                                    (append (gethash "models" models) nil))))
         (catalog (ende-provider--catalog-from-models entries)))
    (should (equal (plist-get catalog :models) (list "model-a")))
    (should (equal (plist-get (car (plist-get catalog :details)) :name) "Model A"))
    (should (equal (plist-get (car entries) :efforts) (list "low" "high")))
    (should (equal (plist-get (car entries) :default) "high"))))

(ert-deftest ende-codex-cache-key-follows-registration ()
  (let* ((base (list :name 'codex :service 'openai-codex :api 'openai-responses
                     :account 'personal :model-discovery-function #'ignore))
         (first (ende-provider--catalog-key
                 (plist-put (copy-sequence base) :model-discovery-key '("registration-a"))))
         (second (ende-provider--catalog-key
                  (plist-put (copy-sequence base) :model-discovery-key '("registration-b")))))
    (should first)
    (should-not (equal first second))))

(ert-deftest ende-codex-usage-normalizes-and-renders-quota-windows ()
  (let ((quota
         (ende-codex-usage-parse
          "{\"rate_limit\":{\"primary_window\":{\"used_percent\":25,\"reset_at\":2000003600,\"limit_window_seconds\":18000},\"secondary_window\":{\"used_percent\":50,\"reset_at\":2000010000,\"limit_window_seconds\":604800}}}")))
    (should (equal (ende-codex-usage-string quota 2000000000)
                   "1h: 75% left · weekly: 50% left · reset in 1h"))
    (should-error (ende-codex-usage-parse "{\"rate_limit\":{}}"))))

(ert-deftest ende-codex-usage-keeps-auth-out-of-url-arguments ()
  (let* ((request nil)
         (buffer (generate-new-buffer " *ende-usage-request-test*"))
         (probe (ende-codex-usage--make-request))
         (auth '(:access-token "secret-access")))
    (unwind-protect
        (cl-letf (((symbol-function 'ende-codex-usage--account-id)
                   (lambda (_auth) "account-id"))
                  ((symbol-function 'ende-curl-start-request)
                   (lambda (&rest arguments)
                     (setq request arguments)
                     (list :response-buffer buffer))))
          (ende-codex-usage--start probe auth)
          (let* ((headers (plist-get request :internal-headers))
                 (command (funcall (plist-get request :command) "/tmp/headers" nil)))
            (should (member "Authorization: Bearer secret-access" headers))
            (should (member "ChatGPT-Account-ID: account-id" headers))
            (should (member "GET" command))
            (should (equal (car (last command))
                           "https://chatgpt.com/backend-api/wham/usage"))
            (should-not (member "secret-access" command))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest ende-codex-usage-reports-http-status-without-response-body ()
  (let* ((headers (make-temp-file "ende-usage-headers-"))
         (buffer (generate-new-buffer " *ende-usage-response*"))
         (process (make-process :name "ende-usage-status-test"
                                :command '("true") :noquery t))
         (result nil)
         (request
          (ende-codex-usage--make-request
           :callback (lambda (quota error) (setq result (list quota error))))))
    (unwind-protect
        (progn
          (while (process-live-p process)
            (accept-process-output process 0.01))
          (with-temp-file headers
            (insert "HTTP/1.1 401 Unauthorized\r\n\r\n"))
          (with-current-buffer buffer
            (insert "secret access token in rejected body"))
          (setf (ende-codex-usage--request-resources request)
                (list :process process :response-buffer buffer :headers-file headers))
          (ende-codex-usage--sentinel request process "finished\n")
          (should (equal (ende-error-details (cadr result)) '(:status 401)))
          (should (equal (ende-error-message (cadr result))
                         "Usage request returned HTTP 401"))
          (should-not (string-match-p "secret access token"
                                      (ende-error-message (cadr result)))))
      (when (process-live-p process) (delete-process process))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (when (file-exists-p headers) (delete-file headers)))))

(ert-deftest ende-codex-usage-coalesces-and-caches-results ()
  (let ((ende-codex-usage--cache (make-hash-table :test #'equal))
        (ende-codex-usage--flights (make-hash-table :test #'equal))
        (auth '(:account personal :host-id "host" :registration-id "registration"
                :client-id "client" :subject "subject"))
        (starts 0) complete values)
    (cl-letf (((symbol-function 'float-time) (lambda (&optional _) 2000000000))
              ((symbol-function 'ende-codex--profile-auth) (lambda (_profile) auth))
              ((symbol-function 'ende-codex-usage-fetch)
               (lambda (_profile callback)
                 (cl-incf starts)
                 (setq complete callback)
                 #'ignore)))
      (ende-codex-usage-probe nil (lambda (quota error) (push (list quota error) values)))
      (ende-codex-usage-probe nil (lambda (quota error) (push (list quota error) values)))
      (should (= starts 1))
      (funcall complete '(:primary (:used-percent 20)) nil)
      (ende-codex-usage-probe nil (lambda (quota error) (push (list quota error) values)))
      (should (= starts 1))
      (should (= (length values) 3))
      (should (equal (caar values) '(:primary (:used-percent 20)))))))

(provide 'ende-codex-test)
;;; ende-codex-test.el ends here
