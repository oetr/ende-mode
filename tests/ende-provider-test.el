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
