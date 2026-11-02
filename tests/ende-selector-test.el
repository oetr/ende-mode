;;; ende-selector-test.el --- Selector tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Coverage for provider, model, and effort selection and selector actions.

;;; Code:

(require 'ert)
(require 'ende)
(require 'ende-test-helpers)
(require 'cl-lib)
(require 'ende-chat-test-helpers)

(defun ende-test--selector-ask-from-buffer (source)
  "Run the selector's ask action for SOURCE with the test model."
  (let ((ende-model-selection--last-used nil)
        (ende-model-selection--buffer-selections
         (make-hash-table :test #'equal))
        (ende-model-selection--shared-control-preferences
         (make-hash-table :test #'equal))
        (ende-model-selection--buffer-control-preferences
         (make-hash-table :test #'equal)))
    (with-temp-buffer
      (ende-selector-mode)
      (setq ende-selector--source-buffer source
            ende-selector--providers '(remote)
            ende-selector--provider-index 0
            ende-selector--models '("selected-model")
            ende-selector--model-index 0
            ende-selector--model-state :ready
            ende-selector--effort nil)
      (cl-letf (((symbol-function 'ende-selector-close) #'ignore))
        (call-interactively
         (lookup-key ende-selector-mode-map (kbd "a")))))))

(ert-deftest ende-test/model-selection-restores-by-buffer-identity ()
  (let* ((name " *ende-model-selection-identity*")
         (file "/tmp/ende-model-selection-identity.el")
         (other-file "/tmp/ende-model-selection-other.el")
         (source nil)
         (other nil)
         (reopened nil)
         (different nil)
         (ende-model-selection-scope 'buffer)
         (ende-model-selection--last-used nil)
         (ende-model-selection--buffer-selections
          (make-hash-table :test #'equal))
         (ende-model-selection--shared-control-preferences
          (make-hash-table :test #'equal))
         (ende-model-selection--buffer-control-preferences
          (make-hash-table :test #'equal))
         (ende-custom-providers
          (append (ende-test--custom-profile 'local :models '("default-model"))
                  '((remote :api openai-chat-completions :auth-method none
                            :base-url "https://remote.test/v1"
                            :controls ((:reasoning-effort :path ("reasoning" "effort"))))))))
    (unwind-protect
        (progn
          (setq source (get-buffer-create name))
          (with-current-buffer source
            (setq buffer-file-name file)
            (ende-mode 1)
            (should-not (ende-model-selection-for-buffer source))
            (should (equal (plist-get
                            (ende-provider-profile-for-buffer source) :model)
                           "default-model"))
            (should (= (hash-table-count
                        ende-model-selection--buffer-selections)
                       0))
            (ende-model-selection-save
             '(:provider remote :model "file-model"
                         :control-preferences
                         (:reasoning-effort "high" :temperature nil))
             source))
          (should (equal
                   (ende-model-selection-control-preferences
                    'remote "file-model" nil source)
                   '(:reasoning-effort "high" :temperature nil)))
          (kill-buffer source)
          (setq source nil
                other (get-buffer-create " *ende-model-selection-latest*"))
          (with-current-buffer other
            (setq buffer-file-name other-file)
            (ende-model-selection-save
             '(:provider openrouter :model "latest-model") other))
          (kill-buffer other)
          (setq other nil
                reopened (get-buffer-create name))
          (with-current-buffer reopened
            (setq buffer-file-name file)
            (ende-mode 1)
            (should (equal (ende-model-selection-for-buffer reopened)
                           '(:provider remote :model "file-model"
                                       :control-preferences
                                       (:reasoning-effort "high"
							  :temperature nil))))
            (should (equal (plist-get
                            (ende-provider-profile-for-buffer reopened) :model)
                           "file-model"))
            (should (equal
                     (plist-get
                      (ende-provider-profile-for-buffer reopened)
                      :reasoning-effort)
                     "high"))
            (should (equal
                     (ende-model-selection-control-preferences
                      'remote "file-model" nil reopened)
                     '(:reasoning-effort "high" :temperature nil)))
            (should (plist-member
                     (ende-provider-profile-for-buffer reopened)
                     :temperature))
            (should-not (plist-get
                         (ende-provider-profile-for-buffer reopened)
                         :temperature)))
          (kill-buffer reopened)
          (setq reopened nil
                different (get-buffer-create name))
          (with-current-buffer different
            (setq buffer-file-name other-file)
            (ende-mode 1)
            (should (equal (ende-model-selection-for-buffer different)
                           '(:provider openrouter :model "latest-model")))))
      (dolist (buffer (list source other reopened different))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))



(ert-deftest ende-test/model-selection-global-scope-shares-one-choice ()
  (let ((ende-model-selection-scope 'global)
        (ende-model-selection--last-used nil)
        (ende-model-selection--buffer-selections
         (make-hash-table :test #'equal))
        (ende-model-selection--shared-control-preferences
         (make-hash-table :test #'equal))
        (ende-model-selection--buffer-control-preferences
         (make-hash-table :test #'equal))
        (first (generate-new-buffer " *ende-model-selection-global-one*"))
        (second (generate-new-buffer " *ende-model-selection-global-two*")))
    (unwind-protect
        (progn
          (ende-model-selection-save
           '(:provider openai :model "shared-model"
                       :control-preferences (:reasoning-effort "medium"))
           first)
          (should (equal (ende-model-selection-for-buffer second)
                         '(:provider openai :model "shared-model"
                                     :control-preferences
                                     (:reasoning-effort "medium"))))
          (should (equal
                   (ende-model-selection-control-preferences
                    'openai "shared-model" nil second)
                   '(:reasoning-effort "medium")))
          (should (= (hash-table-count
                      ende-model-selection--buffer-selections)
                     0)))
      (dolist (buffer (list first second))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))



(ert-deftest ende-test/selector-confirm-requires-valid-model ()
  (ende-test--reset)
  (let ((source (generate-new-buffer " *ende-selector-confirm-source*"))
        (selector (generate-new-buffer " *ende-selector-confirm*"))
        (ende-providers '(remote))
        (ende-custom-providers ende-test--custom-providers)
        (ende-model-selection-scope 'buffer))
    (unwind-protect
        (progn
          (ende-model-selection-save '(:provider remote :model "missing") source)
          (with-current-buffer selector
            (ende-selector-mode)
            (setq ende-selector--source-buffer source
                  ende-selector--providers '(remote))
            (let ((profile (ende-provider-profile 'remote)))
              (ende-selector--apply-model-catalog
               profile '("other") nil 1 "missing")
              (should (equal (ende-selector--model-name) "other"))
              (should (buffer-live-p selector))
              (should (equal (plist-get (ende-model-selection-for-buffer source) :model)
                             "missing"))
              ;; An absent model must also fail visibly without closing.
              (ende-selector--apply-model-catalog profile nil nil 2 nil)
              (should-not (ende-selector--model-name))
              (should-error (ende-selector-confirm) :type 'user-error)
              (should (buffer-live-p selector))
              (should (equal (plist-get (ende-model-selection-for-buffer source) :model)
                             "missing"))
              (ende-selector--apply-model-catalog profile '("other") nil 3 nil)
              (ende-selector-confirm)))
          (should-not (buffer-live-p selector))
          (should (equal (plist-get (ende-model-selection-for-buffer source) :model)
                         "other")))
      (dolist (buffer (list source selector))
        (when (buffer-live-p buffer) (kill-buffer buffer)))
      (ende-test--reset))))

(ert-deftest ende-test/selector-runs-from-source-with-selected-profile ()
  (let* ((source (generate-new-buffer " *ende-selector-source*"))
         (selector (generate-new-buffer " *ende-selector-test*"))
         (ende-providers '(local))
         (ende-custom-providers
          '((local :api openai-chat-completions :auth-method none :base-url "http://localhost:8080/v1" :models ("one" "two")
                   :reasoning-effort "high"
                   :controls ((:reasoning-effort
                               :path ("reasoning" "effort")
                               :choices ("high" "low"))))))
         (queued nil)
         (ende-model-selection-scope 'buffer)
         (ende-model-selection--last-used nil)
         (ende-model-selection--buffer-selections
          (make-hash-table :test #'equal))
         (ende-model-selection--shared-control-preferences
          (make-hash-table :test #'equal))
         (ende-model-selection--buffer-control-preferences
          (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert "source"))
          (with-current-buffer selector
            (ende-selector-mode)
            (setq ende-selector--source-buffer source
                  ende-selector--providers '(local)
                  ende-selector--provider-index 0
                  ende-selector--models '("one" "two")
                  ende-selector--model-index 0
                  ende-selector--model-state :ready)
            (ende-selector-next-model)
            (cl-letf (((symbol-function 'ende-enqueue-operation)
                       (lambda (type instruction profile)
                         (setq queued
                               (list type instruction profile (current-buffer))))))
              (ende-selector-insert)))
          (should (eq (nth 0 queued) 'insert))
          (should (string= (plist-get (nth 2 queued) :model) "two"))
          (should (equal (plist-get (nth 2 queued) :reasoning-effort)
                         "high"))
          (should (equal ende-model-selection--last-used
                         '(:provider local :model "two" :account nil :auth-method none :api openai-chat-completions)))
          (should (equal (ende-model-selection-for-buffer source)
                         '(:provider local :model "two" :account nil :auth-method none :api openai-chat-completions)))
          (should (eq (nth 3 queued) source)))
      (dolist (buffer (list source selector))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))



(ert-deftest ende-test/selector-api-controls-and-dispatched-profiles-stay-isolated ()
  (let ((source (generate-new-buffer " *ende-selector-api-source*"))
        (ende-providers '(openai))
        (ende-builtin-provider-settings '((openai :models ("gpt-5-test"))))
        (ende-custom-providers nil)
        (ende-model-selection-scope 'buffer)
        (ende-model-selection--last-used nil)
        (ende-model-selection--provenance (make-hash-table :test #'equal))
        (ende-model-selection--buffer-selections (make-hash-table :test #'equal))
        (ende-model-selection--shared-control-preferences (make-hash-table :test #'equal))
        (ende-model-selection--buffer-control-preferences (make-hash-table :test #'equal))
        (dispatched nil))
    (unwind-protect
        (cl-labels
            ((select-api (api)
               (cl-letf (((symbol-function 'completing-read)
                          (lambda (&rest _) (symbol-name api))))
                 (ende-test--select-api)))
             (record (type instruction profile)
               (push (list type instruction (copy-tree profile) (current-buffer))
                     dispatched))
             (check-profile (profile api effort)
               (should (eq (plist-get profile :name) 'openai))
               (should (equal (plist-get profile :model) "gpt-5-test"))
               (should (eq (plist-get profile :api) api))
               (should (equal (plist-get profile :url)
                              (if (eq api 'openai-responses)
                                  "https://api.openai.com/v1/responses"
                                "https://api.openai.com/v1/chat/completions")))
               (should (equal (plist-get profile :reasoning-effort) effort))
               (should (equal
                        (plist-get (ende-provider-control-descriptor
                                    profile :reasoning-effort) :path)
                        (if (eq api 'openai-responses)
                            '("reasoning" "effort") '("reasoning_effort"))))))
          (with-current-buffer source (org-mode))
          (with-temp-buffer
            (ende-selector-mode)
            (setq ende-selector--source-buffer source
                  ende-selector--providers '(openai))
            (cl-letf (((symbol-function 'ende-selector-close) #'ignore)
                      ((symbol-function 'ende-enqueue-operation) #'record)
                      ((symbol-function 'ende-ask)
                       (lambda (profile) (record 'chat nil profile)))
                      ((symbol-function 'ende-compact)
                       (lambda (profile) (record 'chat-compact nil profile)))
                      ((symbol-function 'ende-rubber-duck)
                       (lambda (instruction profile)
                         (record 'rubber-duck instruction profile))))
              ;; An implicit default and an explicit default share one history.
              (ende-selector--set-provider 0)
              (ende-selector--set-control-preference :reasoning-effort "high")
              (should (equal (ende-model-selection--pair-key 'openai "gpt-5-test" nil)
                             (ende-model-selection--pair-key
                              'openai "gpt-5-test" 'openai-responses)))
              ;; The other API must not inherit the first API's explicit effort.
              (select-api 'openai-chat-completions)
              (should-not ende-selector--control-preferences)
              (should-not ende-selector--effort)
              (should (eq ende-selector--effort-state :none))
              (ende-selector--set-control-preference :reasoning-effort "low")
              (ende-selector--remember-selected-pair)
              (dolist (scope '(buffer global))
                (let ((ende-model-selection-scope scope))
                  (should (equal (ende-model-selection-control-preferences
                                  'openai "gpt-5-test" nil source)
                                 '(:reasoning-effort "high")))
                  (should (equal (ende-model-selection-control-preferences
                                  'openai "gpt-5-test" 'openai-chat-completions source)
                                 '(:reasoning-effort "low")))))
              (should (= (hash-table-count ende-model-selection--shared-control-preferences) 2))
              ;; Switching and confirming restore each API's own controls.
              (dolist (entry '((openai-responses . "high")
                               (openai-chat-completions . "low")))
                (select-api (car entry))
                (should (equal ende-selector--effort (cdr entry)))
                (should (eq ende-selector--effort-state :remembered))
                (ende-selector-confirm)
                (should (eq (plist-get (ende-model-selection-for-buffer source) :api)
                            (car entry)))
                (check-profile (ende-provider-profile-for-buffer source t)
                               (car entry) (cdr entry))
                ;; Inspect profiles at operation boundaries, not rendered text.
                (dolist (operation '((insert) (improve "Improve") (summarize)
                                     (write-anywhere "Rewrite") (insert-anywhere "Extend")
                                     (chat) (chat-compact) (rubber-duck "Explain")))
                  (setq dispatched nil)
                  (apply #'ende-selector--run operation)
                  (should (= (length dispatched) 1))
                  (should (equal (take 2 (car dispatched))
                                 (list (car operation) (cadr operation))))
                  (should (eq (nth 3 (car dispatched)) source))
                  (check-profile (nth 2 (car dispatched)) (car entry) (cdr entry))))
              ;; Unmapped preferences cannot affect a request.
              (should-error (ende-selector--set-control-preference :thinking 'on)
                            :type 'user-error)
              (ende-selector-reset-controls)
              (should-not (ende-model-selection-control-preferences
                           'openai "gpt-5-test" 'openai-chat-completions source))
              (should (equal (ende-model-selection-control-preferences
                              'openai "gpt-5-test" nil source)
                             '(:reasoning-effort "high")))
              ;; Ordinary implicit-default selections still dispatch correctly.
              (setq ende-selector--api nil ende-selector--connection nil)
              (ende-selector--set-provider 0)
              (should (equal ende-selector--effort "high"))
              (ende-selector-insert)
              (check-profile (nth 2 (car dispatched)) 'openai-responses "high"))))
      (kill-buffer source))))

(ert-deftest ende-test/selector-api-switch-recomputes-inherited-effort ()
  (let ((ende-providers '(openai))
        (ende-builtin-provider-settings '((openai :models ("gpt-5-test"))))
        (ende-model-selection--last-used nil)
        (ende-model-selection--shared-control-preferences (make-hash-table :test #'equal))
        (ende-model-selection--buffer-control-preferences (make-hash-table :test #'equal))
        (resolve (symbol-function 'ende-provider-profile-for-model)))
    (with-temp-buffer
      (ende-selector-mode)
      (setq ende-selector--providers '(openai))
      ;; Supply API-specific inherited capabilities at the profile boundary.
      (cl-letf (((symbol-function 'ende-provider-profile-for-model)
                 (lambda (provider model &optional api connection)
                   (let ((profile (funcall resolve provider model api connection)))
                     (if (eq (plist-get profile :api) 'openai-responses)
                         (plist-put profile :reasoning-effort "high")
                       (plist-put profile :controls nil))))))
        (ende-selector--set-provider 0)
        (should-not ende-selector--control-preferences)
        (should (equal ende-selector--effort "high"))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) "openai-chat-completions")))
          (ende-test--select-api))
        (should-not ende-selector--control-preferences)
        (should-not ende-selector--effort)
        (should (eq ende-selector--effort-state :none))
        (should (ende-provider-validate-controls (ende-selector--resolved-profile)))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) "openai-responses")))
          (ende-test--select-api))
        (should-not ende-selector--control-preferences)
        (should (equal ende-selector--effort "high"))
        (should (eq ende-selector--effort-state :remembered))))))

(ert-deftest ende-test/selector-model-completion-updates-choice ()
  (let ((buffer (generate-new-buffer " *ende-selector-model-test*"))
        (completion-call nil)
        (ende-model-selection-scope 'buffer)
        (ende-model-selection--last-used nil)
        (ende-model-selection--buffer-selections
         (make-hash-table :test #'equal))
        (ende-model-selection--shared-control-preferences
         (make-hash-table :test #'equal))
        (ende-model-selection--buffer-control-preferences
         (make-hash-table :test #'equal)))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(remote)
                ende-selector--provider-index 0
                ende-selector--models
                '("remote/alpha" "remote/beta" "other/gamma")
                ende-selector--model-index 0
                ende-selector--model-state :ready)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (prompt collection predicate require-match
                                     initial-input history default &rest _)
		       (setq completion-call
                             (list :prompt prompt
                                   :collection collection
                                   :predicate predicate
                                   :require-match require-match
                                   :initial-input initial-input
                                   :history history
                                   :default default
                                   :extra-properties completion-extra-properties
                                   :styles completion-styles
                                   :ignore-case completion-ignore-case))
		       "remote/beta")))
            (ende-selector-select-model))
          (should (equal (plist-get completion-call :prompt) "Model: "))
          (should (equal (plist-get completion-call :collection)
                         '("remote/alpha" "remote/beta" "other/gamma")))
          (should-not (member "input" (plist-get completion-call :collection)))
          (should-not (plist-get completion-call :require-match))
          (should-not (plist-get completion-call :initial-input))
          (should (equal (plist-get completion-call :default)
                         "remote/alpha"))
          (should-not (plist-get completion-call :extra-properties))
          (should (equal (plist-get completion-call :styles) '(substring)))
          (should (plist-get completion-call :ignore-case))
          (should (= ende-selector--model-index 1))
          (should (string= (ende-selector--model-name) "remote/beta"))
          (should-not ende-model-selection--last-used)
          (should (equal
                   (ende-model-selection--control-preference-entry
                    'remote "remote/beta" nil buffer)
                   '(t))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest ende-test/selector-preserves-remembered-model-after-discovery ()
  (let ((buffer (generate-new-buffer " *ende-selector-discovery-test*"))
        (probe-callback nil)
        (ende-model-selection--last-used
         '(:provider remote :model "remote/remembered"))
        (ende-model-selection-scope 'buffer)
        (ende-model-selection--buffer-selections
         (make-hash-table :test #'equal))
        (ende-model-selection--shared-control-preferences
         (make-hash-table :test #'equal))
        (ende-model-selection--buffer-control-preferences
         (make-hash-table :test #'equal))
        (ende-custom-providers
         ende-test--custom-providers))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(remote)
                ende-selector--provider-index 0)
          (cl-letf (((symbol-function 'ende-provider-probe-models)
                     (lambda (_profile callback)
		       (setq probe-callback callback)
		       (lambda () nil))))
            (ende-selector--set-provider 0)
            (should (string= (ende-selector--model-name)
                             "remote/remembered"))
            (should (string-match-p "validating"
                                    (buffer-string)))
            (funcall probe-callback
                     '("remote/default" "remote/remembered") nil)
            (should (string= (ende-selector--model-name)
                             "remote/remembered"))
            (should (= (hash-table-count
                        ende-model-selection--shared-control-preferences)
                       0))
            (should (= (hash-table-count
                        ende-model-selection--buffer-control-preferences)
                       0))
            (ende-selector--set-provider 0)
            (ende-selector--set-models '("remote/default" "remote/remembered") "remote/remembered")
            (ende-selector--move-model 1)
            (funcall probe-callback
                     '("remote/default" "remote/remembered") nil)
            (should (equal (ende-selector--model-name) "remote/default"))
            (ende-selector--set-provider 0)
            (setq ende-model-selection--last-used
                  '(:provider remote :model "remote/default"))
            (funcall probe-callback
                     '("remote/default" "remote/remembered") nil)
            (should (equal (ende-selector--model-name) "remote/default"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest ende-test/selector-uses-cached-models-before-rendering ()
  (let* ((buffer (generate-new-buffer " *ende-selector-cached-models-test*"))
         (profile (ende-provider-profile 'remote))
         (ende-providers '(remote))
         (ende-custom-providers
          ende-test--custom-providers)
         (ende-model-selection--last-used
          '(:provider remote :model "remote/saved"))
         (ende-provider--catalog-cache (make-hash-table :test #'equal))
         (ende-provider--catalog-inflight (make-hash-table :test #'equal))
         (ende-provider--model-generation 0))
    (unwind-protect
        (progn
          (puthash
           (ende-provider--catalog-key profile)
           (list :models '("remote/first" "remote/saved")
                 :details '((:id "remote/first") (:id "remote/saved"))
                 :time (float-time) :generation 1)
           ende-provider--catalog-cache)
          (with-current-buffer buffer
            (ende-selector-mode)
            (setq ende-selector--providers '(remote)
                  ende-selector--provider-index 0)
            (cl-letf (((symbol-function 'ende-provider-probe-models)
		       (lambda (&rest _)
                         (error "cached model catalog was not used"))))
	      (ende-selector--set-provider 0)
	      (should (string= (ende-selector--model-name)
			       "remote/saved"))
	      (should (eq ende-selector--model-state :ready))
	      (setq ende-model-selection--last-used nil)
	      (ende-selector--set-provider 0)
	      (should (string= (ende-selector--model-name)
			       "remote/first")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest ende-test/selector-validates-first-model-without-preference ()
  (let ((buffer (generate-new-buffer " *ende-selector-first-model-test*"))
        (probe-callback nil)
        (ende-providers '(remote))
        (ende-custom-providers
         ende-test--custom-providers)
        (ende-model-selection--last-used nil))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(remote)
                ende-selector--provider-index 0)
          (cl-letf (((symbol-function 'ende-provider-probe-models)
                     (lambda (_profile callback)
		       (setq probe-callback callback)
		       (lambda () nil))))
            (ende-selector--set-provider 0)
            (should-not (ende-selector--model-name))
            (should (string-match-p "validating" (buffer-string)))
            (funcall probe-callback '("remote/first" "remote/second") nil)
            (let (dispatched)
              (cl-letf (((symbol-function 'ende-selector--dispatch)
                         (lambda (&rest _) (setq dispatched (ende-selector--model-name)))))
                (ende-selector--run 'insert))
              (should (equal dispatched "remote/first")))
            (should (string= (ende-selector--model-name)
                             "remote/first"))
            (should (eq ende-selector--model-state :ready))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest ende-test/selector-warns-once-per-model-and-catalog ()
  (let ((buffer (generate-new-buffer " *ende-selector-validation-test*"))
        (messages nil)
        (ende-providers '(remote))
        (ende-custom-providers
         ende-test--custom-providers)
        (ende-model-selection--last-used
         '(:provider remote :model "remote/missing"))
        (ende-selector--model-warnings (make-hash-table :test #'eq))
        (ende-provider--catalog-cache (make-hash-table :test #'equal))
        (ende-provider--catalog-inflight (make-hash-table :test #'equal))
        (ende-provider--model-generation 0))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(remote)
                ende-selector--provider-index 0)
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
		       (push (apply #'format format-string args) messages)))
                    ((symbol-function 'ende-provider-probe-models)
                     (lambda (_profile callback)
		       (funcall callback '("remote/first" "remote/second") nil)
		       (lambda () nil))))
            (ende-selector--set-provider 0)
            (should (string= (ende-selector--model-name) "remote/first"))
            (should (= (cl-count-if
                        (lambda (value)
                          (string-match-p "unavailable" value))
                        messages)
		       1))
            (ende-selector--set-provider 0)
            (should (= (cl-count-if
                        (lambda (value)
                          (string-match-p "unavailable" value))
                        messages)
		       1))
            (setq ende-model-selection--last-used
                  '(:provider remote :model "remote/second"))
            (ende-selector--set-provider 0)
            (should (string= (ende-selector--model-name)
                             "remote/second"))
            (should (= (cl-count-if
                        (lambda (value)
                          (string-match-p "unavailable" value))
                        messages)
		       1))
            (ende-provider-clear-discovery-cache)
            (setq ende-model-selection--last-used
                  '(:provider remote :model "remote/missing"))
            (ende-selector--set-provider 0)
            (should (= (cl-count-if
                        (lambda (value)
                          (string-match-p "unavailable" value))
                        messages)
		       2))
            (let ((before (length messages))
                  (profile (ende-provider-profile 'remote)))
              (dolist (models '(("a" "b") ("a" "b") ("b" "a")))
                (ende-selector--apply-model-catalog
                 profile models nil 0 "missing"))
              (should (= (length messages) (+ before 2)))
              (ende-selector--apply-model-catalog
               profile '("a" "missing") nil 0 "missing")
              (should (equal (ende-selector--model-name) "missing"))
              (should (= (length messages) (+ before 2)))
              (ende-selector--apply-model-catalog
               profile '("a" "b") nil 0 "missing")
              (should (= (length messages) (+ before 3))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest ende-test/selector-model-completion-rejects-incomplete-discovery ()
  (let ((buffer (generate-new-buffer " *ende-selector-model-loading-test*"))
        (called nil))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(remote)
                ende-selector--provider-index 0
                ende-selector--models '("remote/default")
                ende-selector--model-index 0
                ende-selector--model-state :loading)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _)
		       (setq called t)
		       "remote/default")))
            (should-error (ende-selector-select-model) :type 'user-error))
          (should-not called))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest ende-test/selector-effort-picker-remembers-setting-immediately ()
  (let ((buffer (generate-new-buffer " *ende-selector-effort-picker-test*"))
        (probed-model nil)
        (ende-providers '(remote))
        (ende-custom-providers
         '((remote :api openai-chat-completions :auth-method none :base-url "http://localhost:8080/v1" :models ("remote-model")
                   :controls ((:reasoning-effort :path ("reasoning" "effort")
                                                 :choices ("high" "low"))))))
        (ende-provider-discovery-enabled nil)
        (ende-model-selection-scope 'global)
        (ende-model-selection--last-used nil)
        (ende-model-selection--shared-control-preferences
         (make-hash-table :test #'equal))
        (ende-model-selection--buffer-control-preferences
         (make-hash-table :test #'equal)))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(remote)
                ende-selector--provider-index 0
                ende-selector--models '("remote-model")
                ende-selector--model-index 0
                ende-selector--model-state :ready)
          (ende-selector--set-control-context)
          (cl-letf (((symbol-function 'ende-provider-probe-reasoning-efforts)
                     (lambda (_profile model callback)
                       (setq probed-model model)
                       (funcall callback '("high" "low") "low" nil)
                       (lambda () nil)))
                    ((symbol-function 'completing-read)
                     (lambda (prompt collection _predicate require-match
                                     _initial _history default &rest _)
		       (should (string= prompt "Reasoning effort: "))
		       (should (equal collection
				      '("Provider default" "high" "low")))
		       (should require-match)
		       (should (equal default "Provider default"))
		       "high")))
            (ende-selector-select-effort))
          (should (equal probed-model "remote-model"))
          (should (equal ende-selector--control-preferences
                         '(:reasoning-effort "high")))
          (should (equal
                   (ende-model-selection-control-preferences
                    'remote "remote-model" nil buffer)
                   '(:reasoning-effort "high")))
          (should-not ende-model-selection--last-used)
          (should (string-match-p "Effort:.*high (explicit)"
                                  (buffer-string)))
          (ende-selector-close)
          (should-not ende-model-selection--last-used)
          (should (equal
                   (ende-model-selection-control-preferences
                    'remote "remote-model" nil buffer)
                   '(:reasoning-effort "high"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ende-test/selector-control-history-survives-switches-without-confirmation ()
  (let* ((test-buffer (generate-new-buffer "Test.org"))
         (dev-buffer (generate-new-buffer "Dev.org"))
         (selector nil)
         (ende-providers '(alpha beta))
         (ende-provider-discovery-enabled nil)
         (ende-model-selection-scope 'buffer)
         (ende-model-selection--last-used nil)
         (ende-model-selection--buffer-selections
          (make-hash-table :test #'equal))
         (ende-model-selection--shared-control-preferences
          (make-hash-table :test #'equal))
         (ende-model-selection--buffer-control-preferences
          (make-hash-table :test #'equal))
         (ende-custom-providers
          '((alpha :api openai-chat-completions :auth-method none :base-url "http://localhost:8080/v1" :models ("alpha-one" "shared-model")
                   :temperature 0.7 :thinking on :reasoning-effort "low"
                   :controls ((:temperature :path ("temperature"))
                              (:thinking :path ("thinking")
                                         :choices (on off))
                              (:reasoning-effort :path ("reasoning" "effort")
                                                 :choices ("high" "low"))))
            (beta :api openai-chat-completions :auth-method none :base-url "http://localhost:8080/v1" :models ("shared-model" "beta-two")
                  :temperature 0.3 :thinking on :reasoning-effort "low"
                  :controls ((:temperature :path ("temperature"))
                             (:thinking :path ("thinking")
                                        :choices (on off))
                             (:reasoning-effort :path ("reasoning" "effort")
                                                :choices ("high" "low")))))))
    (unwind-protect
        (progn
          (with-current-buffer test-buffer
            (ende-model-selection-save
             '(:provider alpha :model "alpha-one")))
          (with-current-buffer dev-buffer
            (ende-model-selection-save
             '(:provider beta :model "shared-model")))
          (cl-letf (((symbol-function 'display-buffer)
                     (lambda (&rest _) nil))
                    ((symbol-function 'ende-provider-model-discovery-available-p)
                     (lambda (_) nil))
                    ((symbol-function 'ende-provider-cached-models)
                     (lambda (_) nil))
                    ((symbol-function 'ende-selector--valid-model-p)
                     (lambda (&rest _) t)))
            (with-current-buffer test-buffer
              (ende-selector-open)))
          (setq selector (get-buffer ende-selector-buffer-name))
          (should (buffer-live-p selector))
          (with-current-buffer selector
            (should (eq (ende-selector--provider-name) 'alpha))
            (should (equal (ende-selector--model-name) "alpha-one"))
            (let ((value 0))
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (&rest _) "Set numeric value"))
                        ((symbol-function 'read-number)
                         (lambda (&rest _) value)))
                (ende-selector-select-temperature)))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "off")))
              (ende-selector-select-thinking))
            (cl-letf (((symbol-function 'ende-selector--probe-efforts)
                       (lambda (callback)
                         (funcall callback '("high" "low") "low" nil)))
                      ((symbol-function 'completing-read)
                       (lambda (&rest _) "high")))
              (ende-selector-select-effort))
            (should (equal
                     (ende-model-selection-control-preferences
                      'alpha "alpha-one" nil test-buffer)
                     '(:temperature 0 :thinking off :reasoning-effort "high")))
            (ende-selector-next-model)
            (should (equal (ende-selector--model-name) "shared-model"))
            (let ((value 0.4))
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (&rest _) "Set numeric value"))
                        ((symbol-function 'read-number)
                         (lambda (&rest _) value)))
                (ende-selector-select-temperature)))
            (cl-letf (((symbol-function 'ende-provider-model-discovery-available-p)
                       (lambda (_) nil))
                      ((symbol-function 'ende-provider-cached-models)
                       (lambda (_) nil))
                      ((symbol-function 'ende-selector--valid-model-p)
                       (lambda (&rest _) t)))
              (ende-selector-next-provider))
            (should (eq (ende-selector--provider-name) 'beta))
            (should (equal (ende-selector--model-name) "shared-model"))
            (let ((value 0.2))
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (&rest _) "Set numeric value"))
                        ((symbol-function 'read-number)
                         (lambda (&rest _) value)))
                (ende-selector-select-temperature)))
            (should (equal
                     (ende-model-selection-control-preferences
                      'alpha "shared-model" nil test-buffer)
                     '(:temperature 0.4)))
            (should (equal
                     (ende-model-selection-control-preferences
                      'beta "shared-model" nil test-buffer)
                     '(:temperature 0.2)))
            (cl-letf (((symbol-function 'ende-provider-model-discovery-available-p)
                       (lambda (_) nil))
                      ((symbol-function 'ende-provider-cached-models)
                       (lambda (_) nil))
                      ((symbol-function 'ende-selector--valid-model-p)
                       (lambda (&rest _) t)))
              (ende-selector-next-provider))
            (should (equal (ende-selector--model-name) "alpha-one"))
            (should (= (plist-get (ende-selector--resolved-profile)
                                  :temperature)
                       0))
            (should (eq (plist-get (ende-selector--resolved-profile)
                                   :thinking)
                        'off))
            (should (equal (plist-get (ende-selector--resolved-profile)
                                      :reasoning-effort)
                           "high"))
            (ende-selector-close))
          (should (equal (plist-get ende-model-selection--last-used :provider)
                         'beta))
          (should (equal (plist-get ende-model-selection--last-used :model)
                         "shared-model"))
          (cl-letf (((symbol-function 'display-buffer)
                     (lambda (&rest _) nil))
                    ((symbol-function 'ende-provider-model-discovery-available-p)
                     (lambda (_) nil))
                    ((symbol-function 'ende-provider-cached-models)
                     (lambda (_) nil))
                    ((symbol-function 'ende-selector--valid-model-p)
                     (lambda (&rest _) t)))
            (with-current-buffer dev-buffer
              (ende-selector-open)))
          (setq selector (get-buffer ende-selector-buffer-name))
          (with-current-buffer selector
            (should (eq (ende-selector--provider-name) 'beta))
            (cl-letf (((symbol-function 'ende-provider-model-discovery-available-p)
                       (lambda (_) nil))
                      ((symbol-function 'ende-provider-cached-models)
                       (lambda (_) nil))
                      ((symbol-function 'ende-selector--valid-model-p)
                       (lambda (&rest _) t)))
              (ende-selector-next-provider))
            (should (equal (ende-selector--model-name) "alpha-one"))
            (should (equal
                     (ende-model-selection-control-preferences
                      'alpha "alpha-one" nil dev-buffer)
                     '(:temperature 0 :thinking off :reasoning-effort "high")))
            (let ((value 0.9))
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (&rest _) "Set numeric value"))
                        ((symbol-function 'read-number)
                         (lambda (&rest _) value)))
                (ende-selector-select-temperature)))
            (ende-selector-close))
          (should (equal
                   (ende-model-selection-control-preferences
                    'alpha "alpha-one" nil dev-buffer)
                   '(:temperature 0.9 :thinking off :reasoning-effort "high")))
          (cl-letf (((symbol-function 'display-buffer)
                     (lambda (&rest _) nil))
                    ((symbol-function 'ende-provider-model-discovery-available-p)
                     (lambda (_) nil))
                    ((symbol-function 'ende-provider-cached-models)
                     (lambda (_) nil))
                    ((symbol-function 'ende-selector--valid-model-p)
                     (lambda (&rest _) t)))
            (with-current-buffer test-buffer
              (ende-selector-open)))
          (setq selector (get-buffer ende-selector-buffer-name))
          (with-current-buffer selector
            (should (eq (ende-selector--provider-name) 'alpha))
            (should (equal (ende-selector--model-name) "alpha-one"))
            (should (zerop (plist-get (ende-selector--resolved-profile)
                                      :temperature)))
            (should (equal
                     (ende-model-selection-control-preferences
                      'alpha "alpha-one" nil test-buffer)
                     '(:temperature 0 :thinking off :reasoning-effort "high")))
            (ende-selector-close))
          (should (equal
                   (gethash (ende-model-selection--pair-key 'alpha "alpha-one" nil)
                            ende-model-selection--shared-control-preferences)
                   '(:temperature 0 :thinking off :reasoning-effort "high")))
          (should (equal (plist-get ende-model-selection--last-used :provider)
                         'beta)))
      (dolist (buffer (list test-buffer dev-buffer selector
                            (get-buffer ende-selector-buffer-name)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest ende-test/selector-clearing-one-control-preserves-other-values ()
  (let ((buffer (generate-new-buffer " *ende-selector-clear-control*"))
        (ende-model-selection-scope 'buffer)
        (ende-model-selection--shared-control-preferences
         (make-hash-table :test #'equal))
        (ende-model-selection--buffer-control-preferences
         (make-hash-table :test #'equal))
        (ende-providers '(remote))
        (ende-custom-providers
         '((remote :api openai-chat-completions :auth-method none :base-url "http://localhost:8080/v1" :models ("remote-model") :temperature 0.2
                   :reasoning-effort "high"
                   :controls ((:temperature :path ("temperature"))
                              (:thinking :path ("thinking")
                                         :choices (on off))
                              (:reasoning-effort :path ("reasoning" "effort")
                                                 :choices ("high" "low")))))))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(remote)
                ende-selector--provider-index 0
                ende-selector--models '("remote-model")
                ende-selector--model-index 0
                ende-selector--model-state :ready)
          (ende-selector--set-control-context)
          (setq ende-selector--control-preferences
                '(:temperature 0.7 :thinking off :reasoning-effort "low"))
          (ende-selector--set-control-preference :temperature nil)
          (ende-selector--set-control-preference :reasoning-effort nil)
          (should (equal ende-selector--control-preferences
                         '(:temperature nil :thinking off
					:reasoning-effort nil)))
          (ende-selector-reset-controls)
          (should-not ende-selector--control-preferences)
          (should (= (plist-get (ende-selector--resolved-profile)
                                :temperature)
                     0.2))
          (should (equal (plist-get (ende-selector--resolved-profile)
                                    :reasoning-effort)
                         "high"))
          (should (equal
                   (ende-model-selection--control-preference-entry
                    'remote "remote-model" nil buffer)
                   '(t))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ende-test/selector-effort-probes-cancel-on-model-change ()
  (let ((buffer (generate-new-buffer " *ende-selector-effort-cancel-test*"))
        (callback nil)
        (cancelled 0)
        (completed nil)
        (ende-providers '(remote))
        (ende-custom-providers
         '((remote :api openai-chat-completions :auth-method none :base-url "http://localhost:8080/v1" :models ("first" "second")
                   :controls ((:reasoning-effort :path ("reasoning" "effort"))))))
        (ende-model-selection-scope 'global)
        (ende-model-selection--shared-control-preferences
         (make-hash-table :test #'equal))
        (ende-model-selection--buffer-control-preferences
         (make-hash-table :test #'equal)))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(remote)
                ende-selector--provider-index 0
                ende-selector--models '("first" "second")
                ende-selector--model-index 0
                ende-selector--model-state :ready)
          (ende-selector--set-control-context)
          (cl-letf (((symbol-function 'ende-selector--control-supported-p)
                     (lambda (&rest _) t))
                    ((symbol-function 'ende-provider-probe-reasoning-efforts)
                     (lambda (_profile _model probe-callback)
                       (setq callback probe-callback)
                       (lambda () (cl-incf cancelled))))
                    ((symbol-function 'completing-read)
                     (lambda (&rest _)
                       (setq completed t)
                       "high")))
            (ende-selector-select-effort)
            (should (eq ende-selector--effort-state :loading))
            (ende-selector-next-model)
            (should (= cancelled 1))
            (should (string= (ende-selector--model-name) "second"))
            (funcall callback '("high") "high" nil)
            (should-not completed)
            (should-not ende-selector--effort)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ende-test/selector-dispatches-configured-effort-without-discovery ()
  (let ((source (generate-new-buffer " *ende-selector-effort-source*"))
        (selector (generate-new-buffer " *ende-selector-effort-dispatch*"))
        (queued nil)
        (ende-providers '(remote))
        (ende-custom-providers
         '((remote :api openai-chat-completions :auth-method none :base-url "http://localhost:8080/v1" :models ("remote-model")
                   :controls ((:reasoning-effort :path ("reasoning" "effort"))))))
        (ende-model-selection-scope 'buffer)
        (ende-model-selection--last-used nil)
        (ende-model-selection--buffer-selections (make-hash-table :test #'equal))
        (ende-model-selection--shared-control-preferences
         (make-hash-table :test #'equal))
        (ende-model-selection--buffer-control-preferences
         (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (with-current-buffer source
            (ende-model-selection-save
             '(:provider remote :model "remote-model"
                         :control-preferences (:reasoning-effort "high"))))
          (with-current-buffer selector
            (ende-selector-mode)
            (setq ende-selector--source-buffer source
                  ende-selector--providers '(remote)
                  ende-selector--provider-index 0
                  ende-selector--models '("remote-model")
                  ende-selector--model-index 0
                  ende-selector--model-state :ready)
            (ende-selector--set-control-context)
            (cl-letf (((symbol-function 'ende-provider-probe-reasoning-efforts)
                       (lambda (&rest _)
                         (ert-fail "Job dispatch must not probe effort support")))
                      ((symbol-function 'ende-enqueue-operation)
                       (lambda (type instruction profile)
                         (setq queued (list type instruction profile)))))
              (ende-selector-insert)
              (should (equal (take 2 queued) '(insert nil)))))
          (should (equal (take 2 queued) '(insert nil)))
          (should (string= (plist-get (nth 2 queued) :model) "remote-model"))
          (should (string= (plist-get (nth 2 queued) :reasoning-effort) "high"))
          (should (equal
                   (plist-get (ende-model-selection-for-buffer source)
                              :control-preferences)
                   '(:reasoning-effort "high")))
          (should (buffer-live-p source)))
      (dolist (buffer (list source selector))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))



(ert-deftest ende-test/selector-uses-bottom-window-and-quits-it-on-close ()
  (let ((source (generate-new-buffer " *ende-selector-window-source*"))
        (display-action nil)
        (quit-window-call nil)
        (probe-profile nil)
        (selector nil)
        (ende-model-selection-scope 'buffer)
        (ende-model-selection--last-used nil)
        (ende-model-selection--buffer-selections
         (make-hash-table :test #'equal))
        (ende-model-selection--shared-control-preferences
         (make-hash-table :test #'equal))
        (ende-model-selection--buffer-control-preferences
         (make-hash-table :test #'equal)))
    (unwind-protect
        (with-current-buffer source
          (cl-letf (((symbol-function 'display-buffer)
                     (lambda (buffer action)
                       (setq display-action (list buffer action))
                       (selected-window)))
                    ((symbol-function 'quit-window)
                     (lambda (kill window)
                       (setq quit-window-call (list kill window))))
                    ((symbol-function 'ende-provider-probe-models)
                     (lambda (profile callback)
                       (setq probe-profile profile)
                       (funcall callback '("local-model") nil)
                       (lambda () nil))))
            (let ((ende-providers '(local))
                  (ende-custom-providers
                   '((local :api openai-chat-completions :auth-method none :base-url "http://localhost:8080/v1"))))
              (ende-selector-open)
              (setq selector (get-buffer ende-selector-buffer-name))
              (should (eq (plist-get probe-profile :name) 'local))
              (should (eq (car display-action) selector))
              (should (eq (car (car (cadr display-action)))
                          #'display-buffer-at-bottom))
              (should (eq (cdr (assq 'window-height
                                     (cdr (cadr display-action))))
                          #'ende-selector--fit-window))
              (should (equal (cdr (assq 'window-min-height
                                        (cdr (cadr display-action))))
                             ende-selector-window-min-height))
              (with-current-buffer selector
                (ende-selector-close))
              (should (equal (car quit-window-call) t))
              (should (eq (cadr quit-window-call) (selected-window)))
              (should-not (buffer-live-p selector)))))
      (when (buffer-live-p selector)
        (kill-buffer selector))
      (when (buffer-live-p source)
        (kill-buffer source)))))



(ert-deftest ende-test/selector-changes-provider-runtime-settings ()
  (ende-provider-clear-runtime-settings)
  (let ((ende-providers '(remote))
        (ende-custom-providers
         '((remote :api openai-chat-completions :auth-method none :base-url "http://localhost:8080/v1" :models ("remote-model") :temperature 0.8 :max-concurrency 2
                   :parallel nil))))
    (with-temp-buffer
      (ende-selector-mode)
      (setq ende-selector--providers '(remote)
            ende-selector--provider-index 0
            ende-selector--models '("remote-model")
            ende-selector--model-index 0)
      (cl-letf (((symbol-function 'read-number)
                 (lambda (&rest _) 4)))
        (ende-selector-set-concurrency))
      (should (= (ende-provider-max-concurrency
                  (ende-provider-profile 'remote))
                 4))
      (should-not (ende-selector--provider-parallel-p))
      (ende-selector-toggle-parallel)
      (should (ende-selector--provider-parallel-p))
      (ende-selector--render)
      (dolist (key '("Provider:" "Model:" "Concurrency:" "Parallel:"
                     "Temperature:" "Account:" "Auth:" "API:"
                     "i  - insert" "a  - ask" "d  - queue dashboard"
                     "I  - insert anywhere"
                     "k  - compact chat context"
                     "w  - write anywhere"
                     "m  - select/search model"
                     "R  - reset controls to profile settings"
                     "s  - summarize"
                     "r  - rubber duck"
                     "RET       - commit selection and close"
                     "C-c C-c   - approve all LLM changes"
                     "C-c C-k   - reject all LLM changes"))
        (should (string-match-p (regexp-quote key) (buffer-string))))
      (should (string-match-p "^T        - Temperature:.*0.8 (profile)$"
                              (buffer-string)))
      (should-not (string-match-p "Thinking:\\|Reasoning effort:" (buffer-string)))
      (should-not (string-match-p "Controls:\\|Reasoning:\\|E  - use provider-default"
                                  (buffer-string)))
      (should-not (string-match-p "T  - select temperature\\|t  - select thinking\\|e  - select reasoning"
                                  (buffer-string))))))

(ert-deftest ende-test/selector-effort-picker-selects-provider-default-with-e ()
  (let ((buffer (generate-new-buffer " *ende-selector-effort-default-test*"))
        (ende-providers '(remote))
        (ende-custom-providers
         '((remote :api openai-chat-completions :auth-method none :base-url "http://localhost:8080/v1" :models ("remote-model") :reasoning-effort "high"
                   :controls ((:reasoning-effort :path ("reasoning" "effort")
                                                 :choices ("high" "low"))))))
        (ende-model-selection-scope 'buffer)
        (ende-model-selection--shared-control-preferences
         (make-hash-table :test #'equal))
        (ende-model-selection--buffer-control-preferences
         (make-hash-table :test #'equal)))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(remote)
                ende-selector--provider-index 0
                ende-selector--models '("remote-model")
                ende-selector--model-index 0
                ende-selector--model-state :ready)
          (ende-selector--set-control-context)
          (cl-letf (((symbol-function 'ende-selector--probe-efforts)
                     (lambda (callback)
                       (funcall callback '("high" "low") "high" nil)))
                    ((symbol-function 'completing-read)
                     (lambda (prompt collection _predicate require-match
                                     _initial _history _default &rest _)
                       (should (equal prompt "Reasoning effort: "))
                       (should (equal collection
                                      '("Provider default" "high" "low")))
                       (should require-match)
                       "Provider default")))
            (ende-selector-select-effort))
          (should (plist-member ende-selector--control-preferences
                                :reasoning-effort))
          (should-not (plist-get ende-selector--control-preferences
                                 :reasoning-effort))
          (should-not (plist-get (ende-selector--resolved-profile)
                                 :reasoning-effort))
          (should (string-match-p
                   "^e        - Reasoning effort:.*Provider default (omitted)$"
                   (buffer-string))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest ende-test/selector-runtime-keybindings-survive-reload ()
  (let ((original-c (lookup-key ende-selector-mode-map (kbd "c")))
        (original-C (lookup-key ende-selector-mode-map (kbd "C")))
        (original-P (lookup-key ende-selector-mode-map (kbd "P")))
        (original-e (lookup-key ende-selector-mode-map (kbd "e")))
        (original-E (lookup-key ende-selector-mode-map (kbd "E")))
        (original-a (lookup-key ende-selector-mode-map (kbd "a")))
        (original-d (lookup-key ende-selector-mode-map (kbd "d")))
        (original-k (lookup-key ende-selector-mode-map (kbd "k"))))
    (unwind-protect
        (progn
          ;; Simulate a keymap retained from before the new bindings existed.
          (define-key ende-selector-mode-map (kbd "c") nil)
          (define-key ende-selector-mode-map (kbd "C") nil)
          (define-key ende-selector-mode-map (kbd "P") nil)
          (define-key ende-selector-mode-map (kbd "e") nil)
          (define-key ende-selector-mode-map (kbd "E") #'ignore)
          (define-key ende-selector-mode-map (kbd "a") nil)
          (define-key ende-selector-mode-map (kbd "d") nil)
          (define-key ende-selector-mode-map (kbd "k") nil)
          (with-temp-buffer
            (ende-selector-mode)
            (should (eq (key-binding (kbd "c"))
                        #'ende-selector-set-concurrency))
            (should (eq (key-binding (kbd "C"))
                        #'ende-selector-set-concurrency))
            (should (eq (key-binding (kbd "P"))
                        #'ende-selector-toggle-parallel))
            (should (eq (key-binding (kbd "n"))
                        #'ende-selector-next-provider))
            (should (eq (key-binding (kbd "p"))
                        #'ende-selector-previous-provider))
            (should (eq (key-binding (kbd "m"))
                        #'ende-selector-select-model))
            (should (eq (key-binding (kbd "e"))
                        #'ende-selector-select-effort))
            (should (eq (key-binding (kbd "T"))
                        #'ende-selector-select-temperature))
            (should (eq (key-binding (kbd "t"))
                        #'ende-selector-select-thinking))
            (should-not (lookup-key ende-selector-mode-map (kbd "E")))
            (should (eq (key-binding (kbd "a"))
                        #'ende-selector-ask))
            (should (eq (key-binding (kbd "d"))
                        #'ende-selector-dashboard))
            (should (eq (key-binding (kbd "k"))
                        #'ende-selector-compact))
            (should (eq (key-binding (kbd "RET"))
                        #'ende-selector-confirm))
            (should (eq (key-binding (kbd "C-c C-c"))
                        #'ende-selector-approve-source-output))
            (should (eq (key-binding (kbd "C-c C-k"))
                        #'ende-selector-reject-source-output))
            (should-not (eq (key-binding (kbd "DEL"))
                            #'ende-selector-reject-source-output))
            (should-not (eq (key-binding (kbd "<backspace>"))
                            #'ende-selector-reject-source-output))
            (should (eq (key-binding (kbd "w"))
                        #'ende-selector-write-anywhere))
            (should (eq (key-binding (kbd "I"))
                        #'ende-selector-insert-anywhere))))
      (define-key ende-selector-mode-map (kbd "c") original-c)
      (define-key ende-selector-mode-map (kbd "C") original-C)
      (define-key ende-selector-mode-map (kbd "P") original-P)
      (define-key ende-selector-mode-map (kbd "e") original-e)
      (define-key ende-selector-mode-map (kbd "E") original-E)
      (define-key ende-selector-mode-map (kbd "a") original-a)
      (define-key ende-selector-mode-map (kbd "d") original-d)
      (define-key ende-selector-mode-map (kbd "k") original-k))))

(ert-deftest ende-test/selector-dashboard-action-closes-and-opens-queue ()
  (let ((selector-closed nil)
        (dashboard-opened nil))
    (with-temp-buffer
      (ende-selector-mode)
      (cl-letf (((symbol-function 'ende-selector-close)
                 (lambda () (setq selector-closed t)))
                ((symbol-function 'ende-queue-open)
                 (lambda () (setq dashboard-opened t))))
        (call-interactively (key-binding (kbd "d")))))
    (should selector-closed)
    (should dashboard-opened)))



(ert-deftest ende-test/selector-rows-use-fixed-columns ()
  (should (equal (ende-selector--row "A" "Account:" "default")
                 "A        - Account:           default\n"))
  (should (equal (ende-selector--row nil "Auth:" 'codex-oauth)
                 "           Auth:              codex-oauth\n")))



(ert-deftest ende-test/selector-probe-callbacks-are-generation-bound ()
  (let ((buffer (generate-new-buffer " *ende-selector-probe-test*"))
        (callbacks nil)
        (cancelled 0)
        (ende-model-selection-scope 'buffer)
        (ende-model-selection--last-used nil)
        (ende-model-selection--buffer-selections
         (make-hash-table :test #'equal))
        (ende-custom-providers
         ende-test--custom-providers))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(remote)
                ende-selector--provider-index 0
                ende-selector--models '("configured")
                ende-selector--model-index 0)
          (cl-letf (((symbol-function 'ende-provider-probe-models)
                     (lambda (_profile callback)
                       (setq callbacks (append callbacks (list callback)))
                       (lambda () (setq cancelled (1+ cancelled))))))
            (ende-selector--set-provider 0)
            (let ((stale (car callbacks)))
              (ende-selector--cancel-probe)
              (ende-selector--set-provider 0)
              (funcall stale '("stale") nil)
              (should-not ende-selector--models)
              (let ((current (car (last callbacks))))
                (kill-buffer buffer)
                (should (= cancelled 2))
                (funcall current '("after-kill") nil)
                (should-not (buffer-live-p buffer))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ende-test/selector-retries-failed-model-probe ()
  (let* ((buffer (generate-new-buffer " *ende-selector-probe-retry-test*"))
         (attempts 0)
         (probe
          (lambda (_profile _url callback)
            (cl-incf attempts)
            (if (= attempts 1)
                (funcall
                 callback nil
                 (ende-error-create
                  :kind 'discovery
                  :message "temporary probe failure"))
              (funcall callback (ende-protocol-json-read-string "{\"data\":[{\"id\":\"remote/first\"}]}") nil))
            (lambda () nil)))
         (ende-providers '(remote))
         (ende-custom-providers ende-test--custom-providers)
         (ende-model-selection--last-used nil)
         (ende-selector--model-warnings (make-hash-table :test #'eq))
         (ende-provider--catalog-cache (make-hash-table :test #'equal))
         (ende-provider--catalog-inflight (make-hash-table :test #'equal))
         (ende-provider--model-generation 0))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(remote)
                ende-selector--provider-index 0)
          (cl-letf (((symbol-function 'ende-provider--start-live-model-request) probe))
            (ende-selector--set-provider 0)
            (should (= attempts 1))
            (should (eq ende-selector--model-state :error))
            (should (string-match-p "unavailable" (buffer-string)))

            (ende-selector--set-provider 0)
            (should (= attempts 2))
            (should (eq ende-selector--model-state :ready))
            (should (string= (ende-selector--model-name) "remote/first"))

            (ende-selector--set-provider 0)
            (should (= attempts 2))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest ende-test/selector-runs-static-model-without-discovery ()
  (let* ((source (generate-new-buffer " *ende-selector-configured-source*"))
         (selector (generate-new-buffer " *ende-selector-configured-model*"))
         (ende-providers '(deepseek))
         (ende-builtin-provider-settings
          '((deepseek :models ("deepseek-chat" "deepseek-reasoner"))))
         (ende-model-selection--last-used nil)
         (ende-model-selection--buffer-selections (make-hash-table :test #'equal))
         (ende-selector--model-warnings (make-hash-table :test #'eq))
         (ende-provider--catalog-cache (make-hash-table :test #'equal))
         (ende-provider--catalog-inflight (make-hash-table :test #'equal))
         (ende-provider--model-generation 0)
         (queued nil))
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert "source"))
          (with-current-buffer selector
            (ende-selector-mode)
            (setq ende-selector--source-buffer source
                  ende-selector--providers '(deepseek)
                  ende-selector--provider-index 0)
            (cl-letf (((symbol-function 'ende-enqueue-operation)
                       (lambda (type instruction profile)
                         (setq queued (list type instruction profile))))
                      ((symbol-function 'ende-provider-probe-models)
                       (lambda (&rest _) (ert-fail "Static catalog was probed"))))
              (ende-selector--set-provider 0)
              (should (eq ende-selector--model-state :ready))
              (should (equal ende-selector--models
                             '("deepseek-chat" "deepseek-reasoner")))
              (should (equal (ende-selector--model-name) "deepseek-chat"))
              (ende-selector-insert)))
          (should (eq (car queued) 'insert))
          (should (eq (plist-get (nth 2 queued) :api) 'openai-chat-completions))
          (should (equal (plist-get (nth 2 queued) :model) "deepseek-chat")))
      (dolist (buffer (list source selector))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest ende-test/selector-static-catalogs-bypass-discovery ()
  (cl-letf (((symbol-function 'ende-provider-probe-models)
             (lambda (&rest _) (ert-fail "Static catalog was probed"))))
    (dolist (name '(local openrouter openai-codex))
      (with-temp-buffer
        (ende-selector-mode)
        (setq ende-selector--providers (list name))
        (ende-selector--set-provider 0)
        (should (eq ende-selector--model-state :ready))
        (should (equal ende-selector--models
                       (ende-provider-models (ende-provider-profile name))))))))



(ert-deftest ende-test/selector-ask-includes-only-reasoning-ancestors ()
  (dolist (policy '(root-prefix current-section-only))
    (ende-test--reset)
    (let ((ende-chat-context-policy policy)
          (ende-chat-include-reasoning nil)
          (ende-custom-providers ende-test--custom-providers)
          (ende-llm-function (ende-test--fake-llm '("Answer"))))
      (ende-test--with-buffer
       (concat "* Root\n"
	       "** Reasoning cousin\n"
	       ":PROPERTIES:\n"
	       ":ENDE_ROLE: assistant\n"
	       ":ENDE_VISIBILITY: reasoning\n"
	       ":END:\nCousin reasoning text.\n"
	       "** Agent: Reasoning\n"
	       ":PROPERTIES:\n"
	       ":ENDE_ROLE: assistant\n"
	       ":ENDE_VISIBILITY: reasoning\n"
	       ":END:\nOuter reasoning context.\n"
	       "*** Reasoning aunt\n"
	       ":PROPERTIES:\n:ENDE_VISIBILITY: reasoning\n:END:\n"
	       "Aunt reasoning text.\n"
	       "*** Nested reasoning\n"
	       "Nested reasoning context.\n"
	       "**** Reasoning sibling\n"
	       ":PROPERTIES:\n:ENDE_VISIBILITY: reasoning\n:END:\n"
	       "Sibling reasoning text.\n"
	       "**** Follow-up\nQuestion from reasoning.\n")
       (org-mode)
       (ende-test--chat-goto "**** Follow-up")
       (let ((source (current-buffer)))
         (ende-test--selector-ask-from-buffer source))
       (let* ((call (car ende-test--calls))
              (messages (plist-get call :messages))
              (assistant-text
	       (mapconcat
                (lambda (message)
                  (if (equal (car message) "assistant")
                      (cdr message)
                    ""))
                messages "\n"))
              (final-message (car (last messages)))
              (document (ende-chat--build-document)))
         (should (equal (plist-get (plist-get call :options) :model)
                        "selected-model"))
         (should (string-match-p "Answer" (buffer-string)))
         (should (string-match-p "Outer reasoning context" assistant-text))
         (should (string-match-p "Nested reasoning context" assistant-text))
         (dolist (excluded '("Cousin reasoning text"
                             "Aunt reasoning text"
                             "Sibling reasoning text"))
           (should-not (string-match-p (regexp-quote excluded)
				       assistant-text)))
         (should (equal (car final-message) "user"))
         (should (string-match-p "Question from reasoning"
                                 (cdr final-message)))
         (let* ((question (ende-chat--headline-at-point document))
                (parent (ende-chat--parent document question)))
           (should (equal (ende-chat--property document question :role)
                          "user"))
           (should-not (ende-chat--property document question :visibility))
           (should (eq (ende-chat--effective-visibility document parent)
		       'reasoning))))))))



(ert-deftest ende-test/selector-ask-rejects-hidden-ancestors ()
  (ende-test--reset)
  (let ((ende-custom-providers ende-test--custom-providers)
        (ende-llm-function (ende-test--fake-llm '("Must not run"))))
    (ende-test--with-buffer
     (concat "* Root\n"
	     "** Hidden assistant context\n"
	     ":PROPERTIES:\n"
	     ":ENDE_ROLE: assistant\n"
	     ":ENDE_VISIBILITY: hidden\n"
	     ":END:\n"
	     "*** Follow-up\nQuestion inside hidden context.\n")
     (org-mode)
     (ende-test--chat-goto "*** Follow-up")
     (let ((source (current-buffer)))
       (should-error (ende-test--selector-ask-from-buffer source)
                     :type 'user-error))
     (ende-test--chat-goto "*** Follow-up")
     (should-not (org-entry-get nil ende-chat-role-property nil))
     (should-not (org-entry-get nil ende-chat-id-property nil))
     (should-not ende-test--calls))))



(ert-deftest ende-test/selector-chat-actions-use-selected-profile-in-org-buffer ()
  (dolist (key '("a" "k"))
    (ende-test--reset)
    (let ((ende-model-selection--last-used nil)
          (ende-custom-providers ende-test--custom-providers)
          (ende-llm-function (ende-test--fake-llm '("Reply"))))
      (ende-test--with-buffer "* Root\nEarlier context\n** Question\nAsk\n"
			      (org-mode)
			      (ende-test--chat-goto "** Question")
			      (let ((source (current-buffer)))
				(with-temp-buffer
				  (ende-selector-mode)
				  (setq ende-selector--source-buffer source
					ende-selector--providers '(remote)
					ende-selector--provider-index 0
					ende-selector--models '("selected-model")
					ende-selector--model-index 0
					ende-selector--model-state :ready
					ende-selector--effort nil)
				  (cl-letf (((symbol-function 'ende-selector-close) #'ignore))
				    (call-interactively (lookup-key ende-selector-mode-map (kbd key)))))
				(let ((profile (plist-get (car ende-test--calls) :options)))
				  (should (eq (plist-get profile :name) 'remote))
				  (should (equal (plist-get profile :model) "selected-model"))
				  (should (string-match-p "Reply" (buffer-string)))))))))



(ert-deftest ende-test/selector-ask-prepares-chat-from-non-org-buffer ()
  (dolist (initial '("" "* Existing\nPrevious material\n"))
    (ende-test--reset)
    (let ((ende-model-selection--last-used nil)
          (ende-llm-function (ende-test--fake-llm '("Reply"))))
      (ende-test--with-buffer "source text"
			      (let ((source (current-buffer))
				    (shown nil)
				    (chat (ende-chat--ensure-buffer)))
				(with-current-buffer chat
				  (insert initial))
				(with-temp-buffer
				  (ende-selector-mode)
				  (setq ende-selector--source-buffer source
					ende-selector--providers '(local)
					ende-selector--provider-index 0
					ende-selector--models '("test-model")
					ende-selector--model-index 0
					ende-selector--model-state :ready
					ende-selector--effort nil)
				  (cl-letf (((symbol-function 'ende-selector-close) #'ignore)
					    ((symbol-function 'pop-to-buffer)
					     (lambda (buffer) (setq shown buffer))))
				    (ende-selector-ask)
				    (with-current-buffer chat
				      (goto-char (point-max))
				      (insert "Question\n"))
				    (setq ende-selector--source-buffer chat)
				    (ende-selector-ask)))
				(should (eq shown chat))
				(with-current-buffer chat
				  (should (derived-mode-p 'org-mode))
				  (should (bound-and-true-p ende-mode))
				  (should (= (how-many "^\\* *$" (point-min) (point-max)) 1))
				  (should (string-match-p "Reply" (buffer-string)))
				  (should (string-match-p "^\\*\\* Agent: Reply$"
							  (buffer-string)))
				  (should (string-match-p ":ENDE_ROLE: assistant"
							  (buffer-string)))
				  (when (not (string-empty-p initial))
				    (should (string-match-p "Previous material"
							    (buffer-string))))))))))

(ert-deftest ende-test/selector-connection-drafts-and-escape-dispatch ()
  (save-window-excursion
    (let* ((source (generate-new-buffer " *ende-connection-source*"))
           (ende-providers '(openai))
           (ende-builtin-provider-settings '((openai :models ("first" "second"))))
           (ende-model-selection--last-used nil)
           (ende-model-selection--provenance (make-hash-table :test #'equal))
           (ende-model-selection--buffer-selections (make-hash-table :test #'equal))
           (ende-model-selection--shared-control-preferences (make-hash-table :test #'equal))
           (ende-model-selection--buffer-control-preferences (make-hash-table :test #'equal))
           (source-window (selected-window))
           (cancelled 0))
      (unwind-protect
          (progn
            (switch-to-buffer source)
            (dolist (escape (list (kbd "ESC") (kbd "<escape>")))
              (ende-selector-open)
              (let ((selector (current-buffer)))
                (ende-selector-configure-connection)
                (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "personal")))
                  (ende-selector-connection-account))
                (should (equal (plist-get ende-selector-connection--draft :account) "personal"))
                (should-error (ende-selector-connection--set :account "") )
                (should-error (ende-selector-connection--set :api 'anthropic-messages))
                (execute-kbd-macro escape)
                (should (eq (current-buffer) selector))
                (should (eq (plist-get (ende-selector--provider-profile) :account) 'default))
                (ende-selector-configure-connection)
                (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "personal"))
                          ((symbol-function 'completing-read)
                           (lambda (_prompt choices _predicate required &rest _)
                             (should required)
                             (should (equal choices '("openai-responses" "openai-chat-completions")))
                             "openai-chat-completions")))
                  (ende-selector-connection-account)
                  (ende-selector-connection-api))
                (ende-selector-connection-apply)
                (should (eq (current-buffer) selector))
                (let ((profile (ende-selector--resolved-profile)))
                  (should (equal (plist-get profile :auth-source-user) "personal"))
                  (should (equal (plist-get profile :url)
                                 "https://api.openai.com/v1/chat/completions"))
                  (should (equal (plist-get profile :model) "first")))
                (setq ende-selector--probe-cancel (lambda () (cl-incf cancelled)))
                (execute-kbd-macro escape)
                (should-not (buffer-live-p selector))
                (should (eq (selected-window) source-window))
                (should (eq (current-buffer) source))
                (should-not (ende-model-selection-for-buffer source))))
            (should (= cancelled 2)))
        (when-let* ((selector (get-buffer ende-selector-buffer-name)))
          (with-current-buffer selector (ende-selector-close)))
        (kill-buffer source)))))

(ert-deftest ende-test/selector-renders-codex-and-custom-identity ()
  (let ((ende-providers '(openai-codex))
        (ende-builtin-provider-settings '((openai-codex :models ("gpt-6.1-sol"))))
        (ende-custom-providers nil)
        (ende-model-selection--last-used nil))
    (with-temp-buffer
      (ende-selector-mode)
      (setq ende-selector--providers '(openai-codex))
      (ende-selector--set-provider 0)
      (should
       (string-prefix-p
        (concat "n/p      - Provider:          OpenAI Codex - https://api.openai.com/v1/responses\n"
                "A        - Account:           default\n"
                "           Auth:              codex-oauth\n"
                "           API:               openai-responses\n"
                "c/C      - Concurrency:       4\n"
                "P        - Parallel:          off\n\n"
                "C-n/C-p  - Model:             gpt-6.1-sol\n"
                "e        - Reasoning effort:  Provider default (omitted)\n\n")
        (buffer-string)))
      (should-not (string-match-p "Service\\|Catalog\\|not mapped\\|explicitly" (buffer-string)))
      (setq ende-selector--providers '(my-local-provider))
      (should (equal (ende-selector--display-provider) "my-local-provider")))))

(provide 'ende-selector-test)

;;; ende-selector-test.el ends here
