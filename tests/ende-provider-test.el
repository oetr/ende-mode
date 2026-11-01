;;; ende-provider-test.el --- Live provider contracts -*- lexical-binding: t; -*-

;;; Code:
(require 'ert)
(require 'ende-test-helpers)

(ert-deftest ende-test/provider-resolution-and-request-snapshot ()
  (let* ((ende-providers '(local))
         (ende-custom-providers
          (ende-test--custom-profile
           'local :models '("one" "two") :output-tokens nil
           :parameters '(:seed 7 :options (:mode "original"))
           :model-settings '(("two" :temperature nil))))
         (profile (ende-provider-profile-for-model 'local "two"))
         (snapshot (ende-provider-profile-snapshot 'local)))
    (should (equal (ende-provider-names) '(local)))
    (should (equal (ende-provider-configured-model profile) "one"))
    (should (eq (plist-get profile :api) 'openai-chat-completions))
    (should (eq (plist-get profile :auth-method) 'none))
    (should-error (ende-provider-profile-for-model 'local "outside"))
    (setf (plist-get (cdar ende-custom-providers) :temperature) 0.1)
    (should (= (plist-get snapshot :temperature) 0.8))
    (let ((body (ende-llm--request-body '(("user" . "hello")) profile)))
      (should-not (assq :max_tokens body))
      (should-not (assq :temperature body))
      (should (= (cdr (assoc "seed" body)) 7)))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _) (ert-fail "Unauthenticated request looked up credentials"))))
      (should-not (ende-curl--config profile)))
    (ende-provider-clear-runtime-settings)
    (unwind-protect
        (progn
          (ende-provider-set-runtime-setting 'local :max-concurrency 3)
          (ende-provider-set-runtime-setting 'local :parallel t)
          (should (= (ende-provider-max-concurrency profile) 3))
          (should (ende-provider-parallel-enabled-p (ende-provider-profile 'local))))
      (ende-provider-clear-runtime-settings)))
  (let ((ende-providers nil) (ende-custom-providers nil)
        (ende-builtin-provider-settings nil))
    (should-not (ende-provider-names))
    (should-error (ende-provider-profile))
    (should-error (ende-selector-open))))

(ert-deftest ende-test/provider-service-api-and-dispatch-are-distinct ()
  (let ((ende-providers '(deepseek openai openai-codex))
        (ende-builtin-provider-settings
         '((deepseek :models ("deepseek-flash"))
           (openai :models ("public-model"))
           (openai-codex :models ("subscription-model")))))
    (let ((deepseek (ende-provider-profile 'deepseek)))
      (should (ende-provider-deepseek-model-verified-p deepseek))
      (should (eq (plist-get deepseek :reasoning-history-field) 'reasoning_content))
      (should-error (ende-provider-validate-controls
                     (ende-plist-merge deepseek '(:thinking on :reasoning-effort "none")))))
    (dolist (entry '((openai . ende-responses-default) (openai-codex . ende-codex-default)))
      (let (called)
        (cl-letf (((symbol-function (cdr entry))
                   (lambda (_messages _callbacks profile) (setq called profile))))
          (ende-llm-default '(("user" . "hello")) nil (ende-provider-profile (car entry))))
        (should (eq (plist-get called :service) (car entry)))
        (should (eq (plist-get called :api) 'openai-responses))))))

(ert-deftest ende-test/model-control-history-is-pair-keyed-and-buffer-isolated ()
  (let ((ende-model-selection-scope 'buffer)
        (ende-model-selection--shared-control-preferences (make-hash-table :test #'equal))
        (ende-model-selection--buffer-control-preferences (make-hash-table :test #'equal))
        (first (generate-new-buffer " *ende-controls-first*"))
        (second (generate-new-buffer " *ende-controls-second*")))
    (unwind-protect
        (progn
          (ende-model-selection-save-control-preferences
           'remote "m" '(:temperature nil :thinking off :reasoning-effort "high") first)
          (ende-model-selection-remember-pair 'remote "m" second)
          (ende-model-selection-save-control-preferences 'remote "m" '(:temperature 0.4) first)
          (should (equal (ende-model-selection-control-preferences 'remote "m" second)
                         '(:temperature nil :thinking off :reasoning-effort "high")))
          (should-not (ende-model-selection-control-preferences 'other "m" second))
          (should-not (ende-model-selection-control-preferences 'remote "other" second))
          (let* ((profile '(:temperature 0.8 :thinking on :reasoning-effort "high"
                           :controls ((:temperature :path ("temperature"))
                                      (:thinking :path ("thinking") :choices (on off))
                                      (:reasoning-effort :path ("reasoning" "effort")))))
                 (resolved (ende-provider-profile-with-control-preferences
                            profile '(:temperature nil :thinking off)))
                 (body (ende-controls-encode nil (plist-get resolved :controls) resolved)))
            (should (= (plist-get profile :temperature) 0.8))
            (should (equal (plist-get resolved :reasoning-effort) "high"))
            (should-not (assq :temperature body))
            (should-not (assoc "reasoning" body)))
          (let ((ende-model-selection-scope 'global))
            (should (equal (ende-model-selection-control-preferences 'remote "m" second)
                           '(:temperature 0.4)))))
      (kill-buffer first) (kill-buffer second))))

(provide 'ende-provider-test)
;;; ende-provider-test.el ends here
