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
    (should (equal (ende-provider-resolve-profile named) named))
    (should (equal (ende-provider-resolve-profile batch) batch))
    (should (equal (plist-get responses :url)
                   "https://chatgpt.com/backend-api/codex/responses"))
    (should (equal (plist-get responses :model) "gpt-configured"))))



(ert-deftest ende-test/typed-openai-profile-is-isolated-from-built-ins ()
  (let* ((ende-provider-endpoint "http://localhost:9100/v1/chat/completions")
         (ende-provider-model "local-configured")
         (ende-provider-temperature 1.4)
         (ende-codex-model "codex-configured")
         (ende-provider-profiles
          '((deepseek :type openai
                      :url "https://api.deepseek.com/chat/completions"
                      :model "deepseek-chat"
                      :auth-source-host "api.deepseek.com"
                      :auth-source-user "ende"
                      :auth-source-port 443
                      :api-key "discard-this-secret")
            (no-defaults :type openai
                         :url "https://example.test/custom/generate")))
         (profile (ende-provider-profile 'deepseek))
         (no-defaults (ende-provider-profile 'no-defaults))
         (snapshot (ende-provider-profile-snapshot 'deepseek)))
    (should (eq (plist-get profile :type) 'openai))
    (should (equal (plist-get profile :url)
                   "https://api.deepseek.com/chat/completions"))
    (should (equal (plist-get profile :model) "deepseek-chat"))
    (should-not (plist-member profile :temperature))
    (should (eq (plist-get profile :stream-usage) t))
    (should (plist-get profile :model-explicit-p))
    (should-not (plist-get profile :wire-api))
    (should-not (plist-get profile :auth-method))
    (should-not (plist-get profile :model-discovery-key))
    (should-not (plist-get profile :reasoning-effort-wire-api))
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



(ert-deftest ende-test/typed-openai-request-options-are-explicit ()
  (let* ((ende-provider-profiles
          '((deepseek :type openai
                      :url "https://api.deepseek.com/chat/completions"
                      :model "deepseek-chat")))
         (profile (ende-provider-profile 'deepseek))
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



(ert-deftest ende-test/provider-openrouter-reasoning-policy-is-separate ()
  (should (equal
           (ende-provider-openrouter-reasoning-parameters
            '(:reasoning-effort-wire-api openrouter :reasoning-effort "high"))
           '((:reasoning (:effort . "high")))))
  (should-not
   (ende-provider-openrouter-reasoning-parameters
    '(:reasoning-effort-wire-api responses :reasoning-effort high))))

(ert-deftest ende-test/chat-request-body-uses-openrouter-reasoning-policy ()
  (let* ((messages '(("user" . "question")))
         (openrouter
          (ende-llm--request-body
           messages
           '(:model "openrouter/model"
		    :reasoning-effort-wire-api openrouter
		    :reasoning-effort "high")))
         (ordinary
          (ende-llm--request-body
           messages
           '(:model "ordinary-model" :reasoning-effort "high"))))
    (should (equal (alist-get :reasoning openrouter)
                   '((:effort . "high"))))
    (should-not (alist-get :reasoning ordinary))))



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
