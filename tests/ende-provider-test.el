;;; ende-provider-test.el --- Provider configuration tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Coverage for provider profiles, configuration, and runtime settings.

;;; Code:

(require 'ert)
(require 'ende)
(require 'ende-test-helpers)
(require 'cl-lib)



(ert-deftest ende-test/provider-list-and-configured-models-are-selectable ()
  (let ((ende-providers '(openai local))
        (ende-provider-profiles
         '((local :models ("local-one" "local-two")))))
    (should (equal (ende-provider-names) '(openai local)))
    (should (equal (ende-provider-models
                    (ende-provider-profile 'local))
                   '("local-one" "local-two")))))



(ert-deftest ende-test/provider-resolves-legacy-effective-profiles ()
  (let* ((ende-provider-endpoint "http://localhost:9000/v1/chat/completions")
         (ende-provider-model "legacy-model")
         (local (ende-provider-resolve-profile '(:temperature 0.4)))
         (named '(:name remote :url "https://remote.test/chat/completions"
			:model "remote-model"))
         (batch '(:transport openrouter-batch
			     :batch-url "https://openrouter.ai/api/v1/batches"))
         (responses
          (let ((ende-codex-endpoint "https://chatgpt.com/backend-api/codex/responses")
                (ende-codex-model "gpt-configured"))
            (ende-provider-resolve-profile '(:wire-api responses)))))
    (should (eq (plist-get local :name) 'local))
    (should (equal (plist-get local :url) ende-provider-endpoint))
    (should (equal (plist-get local :model) "legacy-model"))
    (should (= (plist-get local :temperature) 0.4))
    (should
     (equal (ende-provider-resolve-profile named)
            (append named '(:controls ((:temperature :path ("temperature")))))))
    (should
     (equal (ende-provider-resolve-profile batch)
            (append batch
                    '(:controls ((:temperature :path ("temperature"))
                                 (:reasoning-effort
                                  :path ("reasoning" "effort")))))))
    (should (equal (plist-get responses :url)
                   "https://chatgpt.com/backend-api/codex/responses"))
    (should (equal (plist-get responses :model) "gpt-configured"))))



(ert-deftest ende-test/typed-openai-profile-is-isolated-from-built-ins ()
  (let* ((ende-provider-endpoint "http://localhost:9100/v1/chat/completions")
         (ende-provider-model "local-configured")
         (ende-provider-temperature 1.4)
         (ende-codex-model "codex-configured")
         (ende-provider-profiles
          '((generic-openai :type openai
                            :url "https://example.test/chat/completions"
                            :model "generic-model"
                            :auth-source-host "example.test"
			    :auth-source-user "ende"
			    :auth-source-port 443
			    :api-key "discard-this-secret")
            (no-defaults :type openai
                         :url "https://example.test/custom/generate")))
         (profile (ende-provider-profile 'generic-openai))
         (no-defaults (ende-provider-profile 'no-defaults))
         (snapshot (ende-provider-profile-snapshot 'generic-openai)))
    (should (eq (plist-get profile :type) 'openai))
    (should (equal (plist-get profile :url)
                   "https://example.test/chat/completions"))
    (should (equal (plist-get profile :model) "generic-model"))
    (should-not (plist-member profile :temperature))
    (should (eq (plist-get profile :stream-usage) t))
    (should (plist-get profile :model-explicit-p))
    (should-not (plist-get profile :wire-api))
    (should-not (plist-get profile :auth-method))
    (should-not (plist-get profile :model-discovery-key))
    (should-not (plist-get profile :transport))
    (should-not (plist-get profile :batch-url))
    (should-not (plist-get profile :model-discovery-function))
    (should-not (plist-get profile :request-headers))
    (should-not (plist-get profile :api-key))
    (should-not (ende-provider-models-url profile))
    (should-not (ende-provider-model-discovery-available-p profile))
    (should-not (plist-get no-defaults :model))
    (should-not (plist-get no-defaults :model-explicit-p))
    (should-not (plist-member no-defaults :temperature))
    (should (equal (plist-get no-defaults :url)
                   "https://example.test/custom/generate"))
    (should-not (plist-get snapshot :api-key))
    (should-not (string-match-p "discard-this-secret"
                                (prin1-to-string snapshot)))))

(ert-deftest ende-test/provider-resolves-typed-profiles-with-static-defaults ()
  (let* ((generic '(:type openai :url "https://example.test/v1/chat/completions"
                          :model "generic-model"))
         (unnamed (ende-provider-resolve-profile generic))
         (named (ende-provider-resolve-profile
                 (append '(:name generic) (copy-tree generic))))
         (overrides
          (ende-provider-resolve-profile
           '(:name generic :type openai
		   :url "https://example.test/v1/chat/completions"
		   :model "generic-model" :max-concurrency 3 :parallel t
		   :stream-usage nil :require-sse nil :temperature nil)))
         (ende-provider-profiles
          '((generic-openai :type openai
                            :url "https://example.test/chat/completions"
                            :model "snapshot-model" :temperature nil)))
         (snapshot
          (ende-provider-resolve-profile
           (ende-provider-profile 'generic-openai))))
    (dolist (key '(:type :max-concurrency :parallel :stream-usage :require-sse))
      (should (equal (plist-get unnamed key) (plist-get named key))))
    (should (eq (plist-get unnamed :type) 'openai))
    (should (= (plist-get unnamed :max-concurrency) 1))
    (should-not (plist-get unnamed :parallel))
    (should (eq (plist-get unnamed :stream-usage) t))
    (should (eq (plist-get unnamed :require-sse) t))
    (should (= (plist-get overrides :max-concurrency) 3))
    (should (eq (plist-get overrides :parallel) t))
    (should-not (plist-get overrides :stream-usage))
    (should-not (plist-get overrides :require-sse))
    (should (plist-member overrides :temperature))
    (should-not (plist-get overrides :temperature))
    (let ((ende-provider-profiles
           '((generic-openai :type openai
                             :url "https://example.test/v2/chat/completions"
                             :model "changed-model" :temperature 0.5))))
      (let ((resolved (ende-provider-resolve-profile snapshot)))
        (should (equal (plist-get resolved :url)
                       "https://example.test/chat/completions"))
        (should (equal (plist-get resolved :model) "snapshot-model"))
        (should-not (plist-get resolved :temperature)))))
  (let* ((ende-codex-endpoint "https://chatgpt.com/backend-api/codex/responses")
         (ende-codex-model "snapshot-codex-model")
         (snapshot (ende-provider-resolve-profile
                    (ende-provider-profile 'openai))))
    (let* ((ende-codex-endpoint "https://chatgpt.com/changed")
           (ende-codex-model "changed-codex-model")
           (ende-provider-profiles '((openai :temperature 0.1)))
           (resolved (ende-provider-resolve-profile snapshot)))
      (should (equal (plist-get resolved :url)
                     "https://chatgpt.com/backend-api/codex/responses"))
      (should (equal (plist-get resolved :model) "snapshot-codex-model"))
      (should (eq (plist-get resolved :wire-api) 'responses))
      (should (eq (plist-get resolved :auth-method) 'opencode-oauth))
      (should-not (plist-member resolved :temperature))
      (should-not (plist-get resolved :type)))))

(ert-deftest ende-test/provider-validates-request-profiles-before-dispatch ()
  (let ((valid
         (list '(:url "https://example.test/v1" :model "legacy-model")
               '(:type openai :url "https://example.test/v1"
                       :model "generic-model" :temperature nil :stream-usage nil)
               '(:wire-api responses
                           :url "https://chatgpt.com/backend-api/codex/responses"
                           :model "codex-model")))
        (invalid
         (list '(:type unsupported :url "https://example.test/v1"
                       :model "model")
               '(:url "https://example.test/v1" :model "")
               '(:type openai :url "https://example.test/v1"
                       :model "model" :stream-usage 1)
               '(:url "https://example.test/v1" :model "model"
                      :temperature "warm")
               '(:url "https://example.test/v1" :model "model"
                      :auth-source-host "other.test"
                      :auth-source-user "ende" :auth-source-port 443)))
        (started nil))
    (dolist (profile valid)
      (should (ende-provider-validate-profile profile)))
    (dolist (profile invalid)
      (let ((events nil)
            (errors nil))
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest _)
                     (setq started t)
                     (ert-fail "Started a request with an invalid profile"))))
          (ende-llm-default
           '(("user" . "question"))
           (list :on-event (lambda (event) (push event events))
                 :on-error (lambda (error) (push error errors)))
           profile))
        (should-not started)
        (should (= (length errors) 1))
        (should (ende-error-p (car errors)))
        (should (eq (ende-error-kind (car errors)) 'startup))
        (should (eq (plist-get (car events) :type) :error))))))

(ert-deftest ende-test/typed-openai-name-does-not-select-codex-auth ()
  (let ((codex-called nil)
        (started nil))
    (cl-letf (((symbol-function 'ende-codex-default)
               (lambda (&rest _) (setq codex-called t)))
              ((symbol-function 'ende-llm--start-session)
               (lambda (session _messages)
                 (setq started (ende-llm--session-profile session)))))
      (ende-llm-default
       '(("user" . "question"))
       '(:on-error ignore)
       '(:name openai :type openai :wire-api responses
               :url "https://example.test/v1" :model "generic-model")))
    (should-not codex-called)
    (should (eq (plist-get started :type) 'openai))
    (should-not (plist-get started :auth-method))))



(ert-deftest ende-test/typed-openai-request-options-are-explicit ()
  (let* ((ende-provider-profiles
          '((generic-openai :type openai
                            :url "https://example.test/chat/completions"
                            :model "generic-model")))
         (profile (ende-provider-profile 'generic-openai))
         (messages '(("user" . "question")))
         (body (ende-llm--request-body messages profile))
         (no-usage
          (ende-llm--request-body
           messages (plist-put (copy-tree profile) :stream-usage nil)))
         (temperature
          (ende-llm--request-body
           messages (plist-put (copy-tree profile) :temperature 0.3))))
    (should-not (assq :temperature body))
    (should (equal (alist-get :stream_options body)
                   '((:include_usage . t))))
    (should-not (assq :stream_options no-usage))
    (should (= (alist-get :temperature temperature) 0.3))))



(ert-deftest ende-test/typed-openai-auth-source-is-host-scoped-and-private ()
  (let* ((ende-provider-profiles
          '((deepseek :type openai
                      :url "https://api.deepseek.com/chat/completions"
                      :model "deepseek-chat"
                      :auth-source-host "api.deepseek.com"
                      :auth-source-user "ende"
                      :auth-source-port 443)))
         (profile (ende-provider-profile 'deepseek))
         (token "deepseek-secret-token")
         (config nil))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest arguments)
                 (should (equal (plist-get arguments :host) "api.deepseek.com"))
                 (should (equal (plist-get arguments :user) "ende"))
                 (should (= (plist-get arguments :port) 443))
                 (list (list :secret token)))))
      (setq config (ende-curl--config profile))
      (unwind-protect
          (let ((command (ende-curl-command profile "/tmp/headers" config)))
            (should-not (member token command))
            (should (equal (cadr (member "--url" command))
                           "https://api.deepseek.com/chat/completions"))
            (should-not (string-match-p token (prin1-to-string profile)))
            (with-temp-buffer
              (insert-file-contents config)
              (should (string-match-p
                       "Authorization: Bearer deepseek-secret-token"
                       (buffer-string))))
            (should (= (logand (file-modes config) #o777) #o600)))
        (when (file-exists-p config)
          (delete-file config))))))



(ert-deftest ende-test/chat-request-body-encodes-openrouter-effort-by-profile-map ()
  (let* ((messages '(("user" . "question")))
         (profile (plist-put (ende-provider-profile 'openrouter)
                             :reasoning-effort "high"))
         (body (ende-llm--request-body messages profile)))
    (should (equal (alist-get :reasoning body)
                   '((:effort . "high"))))
    (should (equal (alist-get :temperature body) 0.2))))

(ert-deftest ende-test/chat-request-body-rejects-unmapped-active-control ()
  (should-error
   (ende-llm--request-body
    '(("user" . "question"))
    '(:model "ordinary-model" :reasoning-effort "high"))))



(ert-deftest ende-test/provider-runtime-concurrency-and-parallel-settings ()
  (ende-provider-clear-runtime-settings)
  (let ((ende-provider-profiles
         '((remote :max-concurrency 3 :parallel nil))))
    (let ((profile (ende-provider-profile 'remote)))
      (should (= (ende-provider-max-concurrency profile) 3))
      (should-not (ende-provider-parallel-enabled-p profile)))
    (ende-provider-set-runtime-setting 'remote :max-concurrency 5)
    (ende-provider-set-runtime-setting 'remote :parallel t)
    (let ((profile (ende-provider-profile 'remote)))
      (should (= (ende-provider-max-concurrency profile) 5))
      (should (ende-provider-parallel-enabled-p profile)))))



(ert-deftest ende-test/openrouter-profiles-advertise-ende-mode ()
  (dolist (name '(openrouter openrouter-batch))
    (let* ((profile (ende-provider-profile name))
           (headers (plist-get profile :request-headers))
           (config
            (ende-curl--config
             (list :url (plist-get profile :url)
                   :request-headers headers))))
      (unwind-protect
          (progn
            (should (equal headers
                           '("HTTP-Referer: https://github.com/oetr/ende-mode"
                             "X-OpenRouter-Title: ende-mode")))
            (with-temp-buffer
              (insert-file-contents config)
              (should (equal
                       (buffer-string)
                       (concat
                        "header = \"HTTP-Referer: https://github.com/oetr/ende-mode\"\n"
                        "header = \"X-OpenRouter-Title: ende-mode\"\n")))))
        (when (file-exists-p config)
          (delete-file config))))))

(provide 'ende-provider-test)

;;; ende-provider-test.el ends here
