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
           'remote "m" nil '(:temperature nil :thinking off :reasoning-effort "high") first)
          (ende-model-selection-remember-pair 'remote "m" nil second)
          (ende-model-selection-save-control-preferences 'remote "m" nil '(:temperature 0.4) first)
          (should (equal (ende-model-selection-control-preferences 'remote "m" nil second)
                         '(:temperature nil :thinking off :reasoning-effort "high")))
          (should-not (ende-model-selection-control-preferences 'other "m" nil second))
          (should-not (ende-model-selection-control-preferences 'remote "other" nil second))
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
            (should (equal (ende-model-selection-control-preferences 'remote "m" nil second)
                           '(:temperature 0.4)))))
      (kill-buffer first) (kill-buffer second))))

(ert-deftest ende-test/model-control-history-follows-resolved-binding ()
  (let ((ende-providers '(remote))
        (ende-custom-providers
         '((remote :api openai-responses :auth-method auth-source
                   :auth-source-user "first" :models ("m")
                   :endpoints ((openai-responses . "https://remote.test/v1/responses")
                               (openai-chat-completions . "https://remote.test/v1/chat/completions")))))
        (ende-model-selection-scope 'global)
        (ende-model-selection--shared-control-preferences (make-hash-table :test #'equal)))
    (ende-model-selection-save-control-preferences 'remote "m" nil '(:temperature 0.4))
    (let ((key (ende-model-selection--pair-key 'remote "m" nil)))
      ;; Catalog growth does not change the resolved request binding.
      (let ((ende-custom-providers
             (list (cons 'remote (ende-plist-merge
                                 (cdar ende-custom-providers) '(:models ("m" "other")))))))
        (should (equal key (ende-model-selection--pair-key 'remote "m" nil))))
      (dolist (overrides
               '((:auth-source-user "second")
                 (:endpoints ((openai-responses . "https://other.test/v1/responses")
                              (openai-chat-completions . "https://remote.test/v1/chat/completions")))))
        (let ((ende-custom-providers
               (list (cons 'remote (ende-plist-merge (cdar ende-custom-providers) overrides)))))
          (should-not (equal key (ende-model-selection--pair-key 'remote "m" nil)))
          (should-not (ende-model-selection-control-preferences 'remote "m" nil))
          (ende-model-selection-save-control-preferences 'remote "m" nil '(:temperature 0.2))))
      (should (equal (ende-model-selection-control-preferences 'remote "m" nil)
                     '(:temperature 0.4))))
    (let ((ende-custom-providers nil)
          (ende-builtin-provider-settings '((remote :provider openai :account work :models ("m")))))
      (ende-model-selection-save-control-preferences 'remote "m" nil '(:temperature 0.6))
      (let ((ende-builtin-provider-settings '((remote :provider openai :account personal :models ("m")))))
        (should-not (ende-model-selection-control-preferences 'remote "m" nil)))
      (should (equal (ende-model-selection-control-preferences 'remote "m" nil)
                     '(:temperature 0.6))))))

(ert-deftest ende-test/provider-api-selection-and-local-check ()
  (let ((ende-providers '(openai))
        (ende-builtin-provider-settings '((openai :models ("public-model")))))
    (cl-letf (((symbol-function 'ende-credentials-secret)
               (lambda (&rest _) (ert-fail "Local checker requested credentials")))
              ((symbol-function 'make-process)
               (lambda (&rest _) (ert-fail "Local checker started a network request"))))
      (should (eq (plist-get (car (ende-provider-check)) :credentials) 'unchecked)))
    (with-temp-buffer
      (ende-selector-mode)
      (setq ende-selector--providers '(openai))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "openai-chat-completions")))
        (ende-test--select-api))
      (should (eq (plist-get (ende-selector--resolved-profile) :api) 'openai-chat-completions))
      (should (equal (plist-get (ende-selector--resolved-profile) :url)
                     "https://api.openai.com/v1/chat/completions")))))

(ert-deftest ende-test/provider-remembered-selection-rejects-rebinding ()
  (let ((ende-providers '(local))
        (ende-custom-providers (ende-test--custom-profile 'local))
        (ende-model-selection--provenance (make-hash-table :test #'equal))
        (ende-model-selection--buffer-selections (make-hash-table :test #'equal))
        (ende-model-selection--last-used nil))
    (with-temp-buffer
      (ende-model-selection-save '(:provider local :model "test-model"))
      (should (ende-provider-profile-for-buffer))
      (setf (plist-get (cdar ende-custom-providers) :base-url) "https://other.test/v1")
      (should-error (ende-provider-profile-for-buffer))
      (ende-model-selection-save '(:provider local :model "test-model"))
      (should (equal (plist-get (ende-provider-profile-for-buffer) :url)
                     "https://other.test/v1/chat/completions")))))

(ert-deftest ende-test/remembered-discovered-model-revalidated-before-enqueue ()
  (ende-test--reset)
  (let* ((ende-providers '(remote))
         (ende-custom-providers
          '((remote :api openai-responses :auth-method none
                    :models-url "https://remote.test/v1/models"
                    :endpoints ((openai-responses . "https://remote.test/v1/responses")
                                (openai-chat-completions . "https://remote.test/v1/chat/completions")))))
         (ende-model-selection-scope 'buffer)
         (ende-provider-discovery-enabled t)
         (ende-provider-discovery-ttl 30)
         (ende-llm-function (ende-test--fake-llm '(:stall) t))
         (clock 1000.0)
         (requests 0)
         (payload "{\"data\":[{\"id\":\"selected\"},{\"id\":\"other\"}]}")
         (failure nil)
         (profile (ende-provider-profile-for-model
                   'remote nil 'openai-chat-completions))
         (key (ende-provider--catalog-key profile)))
    (unwind-protect
        (with-temp-buffer
          (insert "source")
          (cl-letf (((symbol-function 'float-time) (lambda (&optional _) clock))
                    ((symbol-function 'ende-provider--start-live-model-request)
                     (lambda (_profile _url callback)
                       (cl-incf requests)
                       (funcall callback
                                (unless failure
                                  (ende-protocol-json-read-string payload))
                                failure)
                       #'ignore)))
            (cl-labels ((refresh ()
                          (ende-provider-clear-discovery-cache)
                          (ende-provider-probe-models profile #'ignore))
                        (selected-model ()
                          (plist-get (ende-provider-profile-for-buffer) :model)))
              (refresh)
              (ende-model-selection-save
               '(:provider remote :model "selected" :api openai-chat-completions))
              (should (equal (selected-model) "selected"))
              (let ((generation (ende-provider-models-generation profile)))
                (setq payload "{\"data\":[{\"id\":\"other\"}]}")
                (refresh)
                ;; Another API's catalog must not validate this selection.
                (puthash (ende-provider--catalog-key (ende-provider-profile 'remote))
                         (list :time clock :models '("selected") :generation 100)
                         ende-provider--catalog-cache)
                (should (string-match-p
                         "Model selected is no longer available; select a model again"
                         (error-message-string
                          (should-error (ende-insert-at-cursor) :type 'user-error))))
                (should-not ende-queue)
                (should-not ende-test--calls)
                (should (= ende--next-job-id 0))
                (should (= requests 2))
                ;; Expiry and failed discovery are not negative model evidence.
                (setq clock (+ clock 60))
                (should-not (ende-provider-cached-models profile))
                (should (equal (selected-model) "selected"))
                (setq failure (ende-error-create :kind 'transport :message "offline"))
                (ende-provider-discover profile)
                (should (ende-provider--cached-failure-p key))
                (should-not (ende-provider-cached-models profile))
                (should (equal (selected-model) "selected"))
                ;; Membership, not generation equality, permits the request.
                (setq failure nil
                      payload "{\"data\":[{\"id\":\"other\"},{\"id\":\"selected\"}]}")
                (refresh)
                (should (> (ende-provider-models-generation profile) generation))
                (should (equal (plist-get (ende-job-profile (ende-insert-at-cursor)) :model)
                               "selected"))
                (should (= (length ende-test--calls) 1))
                (should (= requests 4))))))
      (ende-test--reset))))

(provide 'ende-provider-test)
;;; ende-provider-test.el ends here
