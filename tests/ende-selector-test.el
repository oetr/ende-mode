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
       (lookup-key ende-selector-mode-map (kbd "a"))))))

(ert-deftest ende-test/selector-quota-is-scoped-and-ignores-stale-results ()
  (let ((buffer (generate-new-buffer " *ende-selector-quota-test*"))
        (quota-callback nil)
        (cancel-count 0)
        (ende-providers '(local openai))
        (ende-provider-profiles
         '((local :model "test-model" :models ("test-model"))
           (openai :model "test-model" :auth-method opencode-oauth
                   :models ("test-model"))))
        (ende-provider--runtime-settings (make-hash-table :test #'equal))
        (ende-selector--last-selection nil))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(local openai)
                ende-selector--provider-index 0)
          (cl-letf (((symbol-function 'ende-provider-probe-models)
                     (lambda (_profile callback)
                       (funcall callback '("test-model") nil)
                       (lambda () nil)))
                    ((symbol-function 'ende-codex-probe-quota)
                     (lambda (callback)
                       (setq quota-callback callback)
                       (lambda () (cl-incf cancel-count)))))
            (ende-selector--set-provider 0)
            (should-not quota-callback)
            (should (string-match-p "Quota: unavailable"
                                    (buffer-string)))
            (ende-selector--set-provider 1)
            (should quota-callback)
            (should (eq ende-selector--quota-state :loading))
            (should (string-match-p "Quota: loading\.\.\."
                                    (buffer-string)))
            (let ((now (floor (float-time))))
              (funcall
               quota-callback
               (list :primary
                     (list :used-percent 28
                           :reset-at (+ now 18660)
                           :window-seconds 18000)
                     :secondary
                     (list :used-percent 59
                           :reset-at (+ now 7980)
                           :window-seconds 604800))
               nil)
              (should (eq ende-selector--quota-state :ready))
              (should (string-match-p
                       "Quota: 5h 1[01]m: 72% left"
                       (buffer-string)))
              (ende-selector--set-provider 1)
              (let ((stale-callback quota-callback))
                (ende-selector--set-provider 0)
                (should (= cancel-count 1))
                (should (eq ende-selector--quota-state :unavailable))
                (funcall stale-callback
                         (list :primary
                               (list :used-percent 1
                                     :reset-at (+ now 18660)
                                     :window-seconds 18000))
                         nil)
                (should (eq ende-selector--quota-state :unavailable))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



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
         (ende-provider-model "default-model"))
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
                         :reasoning-effort "high")
             source))
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
                                       :reasoning-effort "high")))
            (should (equal (plist-get
                            (ende-provider-profile-for-buffer reopened) :model)
                           "file-model"))
            (should (equal
                     (plist-get
                      (ende-provider-profile-for-buffer reopened)
                      :reasoning-effort)
                     "high")))
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
        (first (generate-new-buffer " *ende-model-selection-global-one*"))
        (second (generate-new-buffer " *ende-model-selection-global-two*")))
    (unwind-protect
        (progn
          (ende-model-selection-save
           '(:provider openai :model "shared-model"
                       :reasoning-effort "medium")
           first)
          (should (equal (ende-model-selection-for-buffer second)
                         '(:provider openai :model "shared-model"
                                     :reasoning-effort "medium")))
          (should (= (hash-table-count
                      ende-model-selection--buffer-selections)
                     0)))
      (dolist (buffer (list first second))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))



(ert-deftest ende-test/selector-runs-from-source-with-selected-profile ()
  (let* ((source (generate-new-buffer " *ende-selector-source*"))
         (selector (generate-new-buffer " *ende-selector-test*"))
         (ende-providers '(local))
         (ende-provider-profiles
          '((local :models ("one" "two")
                   :reasoning-effort "made-up"
                   :reasoning-effort-wire-api openrouter)))
         (ende-selector--last-selection nil)
         (queued nil))
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
                  ende-selector--model-index 0)
            (ende-selector-next-model)
            (cl-letf (((symbol-function 'ende-enqueue-operation)
                       (lambda (type instruction profile)
                         (setq queued
                               (list type instruction profile (current-buffer))))))
              (ende-selector-insert)))
          (should (eq (nth 0 queued) 'insert))
          (should (string= (plist-get (nth 2 queued) :model) "two"))
          (should-not (plist-get (nth 2 queued) :reasoning-effort))
          (should (equal ende-selector--last-selection
                         '(:provider local :model "two")))
          (should (equal (ende-model-selection-for-buffer source)
                         '(:provider local :model "two")))
          (should (eq (nth 3 queued) source)))
      (dolist (buffer (list source selector))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))



(ert-deftest ende-test/selector-model-completion-updates-choice ()
  (let ((buffer (generate-new-buffer " *ende-selector-model-test*"))
        (completion-call nil)
        (ende-selector--last-selection nil))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(remote)
                ende-selector--provider-index 0
                ende-selector--models
                '("remote/alpha" "remote/beta" "other/gamma")
                ende-selector--model-details
                '((:id "remote/alpha" :name "Alpha Model"
                       :pricing (:prompt "0.000003" :completion "0.000009"
					 :input-cache-read "0.000001"
					 :input-cache-write "0.000004"))
                  (:id "remote/beta" :name "Beta Model")
                  (:id "other/gamma" :name "Gamma Model"))
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
          (should (plist-get completion-call :require-match))
          (should-not (plist-get completion-call :initial-input))
          (should (equal (plist-get completion-call :default)
                         "remote/alpha"))
          (let* ((properties (plist-get completion-call :extra-properties))
                 (affixation (plist-get properties :affixation-function))
                 (affixed (funcall affixation
                                   (plist-get completion-call :collection)))
                 (priced (car affixed))
                 (unpriced (cadr affixed))
                 (priced-model (car priced))
                 (priced-suffix (nth 2 priced)))
            (should (functionp affixation))
            (should-not (plist-get properties :annotation-function))
            (should (string= (substring-no-properties priced-model)
                             "remote/alpha"))
            (should (eq (get-text-property 0 'face priced-model)
                        'ende-selector-model-face))
            (should
             (string=
              (substring-no-properties priced-suffix)
              "  in: $3  out: $9  cache-read: $1  cache-write: $4"))
            (should (eq (get-text-property (string-match "in:" priced-suffix)
                                           'face priced-suffix)
                        'ende-selector-price-muted-face))
            (should (eq (get-text-property (string-match "out:" priced-suffix)
                                           'face priced-suffix)
                        'ende-selector-price-output-face))
            (should (eq (get-text-property
                         (string-match "cache-read:" priced-suffix)
                         'face priced-suffix)
                        'ende-selector-price-muted-face))
            (should (equal (mapcar #'substring-no-properties
                                   (list (car unpriced) (nth 2 unpriced)))
                           '("remote/beta" ""))))
          (should (equal (plist-get completion-call :styles) '(substring)))
          (should (plist-get completion-call :ignore-case))
          (should (= ende-selector--model-index 1))
          (should (string= (ende-selector--model-name) "remote/beta"))
          (should (equal ende-selector--last-selection
                         '(:provider remote :model "remote/beta"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest ende-test/selector-preserves-remembered-model-after-discovery ()
  (let ((buffer (generate-new-buffer " *ende-selector-discovery-test*"))
        (probe-callback nil)
        (ende-selector--last-selection
         '(:provider remote :model "remote/remembered"))
        (ende-provider-profiles
         '((remote :url "http://remote.example/v1/chat/completions"
                   :model "remote/default"))))
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
                             "remote/remembered"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest ende-test/selector-uses-cached-models-before-rendering ()
  (let* ((buffer (generate-new-buffer " *ende-selector-cached-models-test*"))
         (profile '(:name remote
			  :url "http://remote.example/v1/chat/completions"))
         (ende-providers '(remote))
         (ende-provider-profiles
          '((remote :url "http://remote.example/v1/chat/completions")))
         (ende-selector--last-selection
          '(:provider remote :model "remote/saved"))
         (ende-provider--model-cache (make-hash-table :test #'equal))
         (ende-provider--model-inflight (make-hash-table :test #'equal))
         (ende-provider--model-cancellations (make-hash-table :test #'equal))
         (ende-provider--model-generation 0))
    (unwind-protect
        (progn
          (ende-provider--finish-model-discovery
           (ende-provider--model-cache-key profile)
           '("remote/first" "remote/saved") nil nil)
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
              (setq ende-selector--last-selection nil)
              (ende-selector--set-provider 0)
              (should (string= (ende-selector--model-name)
                               "remote/first")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest ende-test/selector-validates-first-model-without-preference ()
  (let ((buffer (generate-new-buffer " *ende-selector-first-model-test*"))
        (probe-callback nil)
        (ende-providers '(remote))
        (ende-provider-profiles
         '((remote :url "http://remote.example/v1/chat/completions")))
        (ende-selector--last-selection nil))
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
            (should (string= (ende-selector--model-name)
                             "remote/first"))
            (should (eq ende-selector--model-state :ready))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest ende-test/selector-validates-preferences-once-per-catalog ()
  (let ((buffer (generate-new-buffer " *ende-selector-validation-test*"))
        (messages nil)
        (ende-providers '(remote))
        (ende-provider-profiles
         '((remote :url "http://remote.example/v1/chat/completions")))
        (ende-selector--last-selection
         '(:provider remote :model "remote/missing"))
        (ende-selector--model-validations (make-hash-table :test #'equal))
        (ende-provider--model-cache (make-hash-table :test #'equal))
        (ende-provider--model-inflight (make-hash-table :test #'equal))
        (ende-provider--model-cancellations (make-hash-table :test #'equal))
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
            (setq ende-selector--last-selection
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
            (setq ende-selector--last-selection
                  '(:provider remote :model "remote/missing"))
            (ende-selector--set-provider 0)
            (should (= (cl-count-if
                        (lambda (value)
                          (string-match-p "unavailable" value))
                        messages)
                       2))))
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



(ert-deftest ende-test/selector-effort-picker-uses-live-values-and-remembers-choice ()
  (let ((buffer (generate-new-buffer " *ende-selector-effort-picker-test*"))
        (calls nil)
        (ende-provider-profiles '((remote :model "remote-model")))
        (ende-model-selection-scope 'global)
        (ende-selector--effort-selections (make-hash-table :test #'equal)))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(remote)
                ende-selector--provider-index 0
                ende-selector--models '("remote-model")
                ende-selector--model-index 0
                ende-selector--model-state :ready)
          (cl-letf (((symbol-function 'ende-provider-probe-reasoning-efforts)
                     (lambda (profile model callback)
                       (push (list profile model) calls)
                       (funcall callback '("high" "low") "low" nil)
                       (lambda () nil)))
                    ((symbol-function 'completing-read)
                     (lambda (prompt collection _predicate require-match
                                     _initial _history default &rest _)
                       (should (string= prompt "Reasoning effort: "))
                       (should (equal collection '("high" "low")))
                       (should require-match)
                       (should (string= default "low"))
                       "high")))
            (ende-selector-select-effort))
          (should (= (length calls) 1))
          (should (string= (cadar calls) "remote-model"))
          (should (string= ende-selector--effort "high"))
          (should (eq ende-selector--effort-state :ready))
          (should ende-selector--effort-validated-p)
          (should (string=
                   (gethash '(remote "remote-model")
                            ende-selector--effort-selections)
                   "high"))
          (should (string-match-p "Effort:.*high" (buffer-string))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest ende-test/selector-effort-is-saved-only-for-its-source-buffer ()
  (let* ((source (generate-new-buffer " *ende-effort-source-one*"))
         (other-source (generate-new-buffer " *ende-effort-source-two*"))
         (selector (generate-new-buffer " *ende-effort-source-selector*"))
         (ende-model-selection-scope 'buffer)
         (ende-model-selection--last-used nil)
         (ende-model-selection--buffer-selections
          (make-hash-table :test #'equal))
         (ende-provider-profiles '((remote :model "remote-model"))))
    (unwind-protect
        (progn
          (with-current-buffer source
            (ende-model-selection-save
             '(:provider remote :model "remote-model")))
          (with-current-buffer other-source
            (ende-model-selection-save
             '(:provider remote :model "remote-model")))
          (with-current-buffer selector
            (ende-selector-mode)
            (setq ende-selector--source-buffer source
                  ende-selector--providers '(remote)
                  ende-selector--provider-index 0
                  ende-selector--models '("remote-model")
                  ende-selector--model-index 0
                  ende-selector--model-state :ready)
            (cl-letf (((symbol-function 'ende-provider-probe-reasoning-efforts)
                       (lambda (_profile _model callback)
                         (funcall callback '("high" "low") "low" nil)
                         (lambda () nil)))
                      ((symbol-function 'completing-read)
                       (lambda (&rest _) "high")))
              (ende-selector-select-effort))
            (should (equal
                     (plist-get (ende-model-selection-for-buffer source)
                                :reasoning-effort)
                     "high"))
            (should-not
             (plist-get (ende-model-selection-for-buffer other-source)
                        :reasoning-effort))
            (setq ende-selector--source-buffer other-source)
            (ende-selector--set-effort-context)
            (should-not ende-selector--effort)))
      (dolist (buffer (list source other-source selector))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))



(ert-deftest ende-test/selector-effort-probes-cancel-on-model-change ()
  (let ((buffer (generate-new-buffer " *ende-selector-effort-cancel-test*"))
        (callback nil)
        (cancelled 0)
        (completed nil)
        (ende-provider-profiles '((remote :model "first")))
        (ende-model-selection-scope 'global)
        (ende-selector--effort-selections (make-hash-table :test #'equal)))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(remote)
                ende-selector--provider-index 0
                ende-selector--models '("first" "second")
                ende-selector--model-index 0
                ende-selector--model-state :ready)
          (cl-letf (((symbol-function 'ende-provider-probe-reasoning-efforts)
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



(ert-deftest ende-test/selector-validates-remembered-effort-before-dispatch ()
  (let ((source (generate-new-buffer " *ende-selector-effort-source*"))
        (selector (generate-new-buffer " *ende-selector-effort-dispatch*"))
        (probe-callback nil)
        (queued nil)
        (ende-provider-profiles '((remote :model "remote-model")))
        (ende-model-selection-scope 'global)
        (ende-selector--effort-selections (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (puthash '(remote "remote-model") "high"
                   ende-selector--effort-selections)
          (with-current-buffer selector
            (ende-selector-mode)
            (setq ende-selector--source-buffer source
                  ende-selector--providers '(remote)
                  ende-selector--provider-index 0
                  ende-selector--models '("remote-model")
                  ende-selector--model-index 0
                  ende-selector--model-state :ready)
            (ende-selector--set-effort-context)
            (cl-letf (((symbol-function 'ende-provider-probe-reasoning-efforts)
                       (lambda (_profile _model callback)
                         (setq probe-callback callback)
                         (lambda () nil)))
                      ((symbol-function 'ende-enqueue-operation)
                       (lambda (type instruction profile)
                         (setq queued (list type instruction profile)))))
              (ende-selector-insert)
              (should (eq ende-selector--effort-state :loading))
              (should-not queued)
              (funcall probe-callback '("high" "low") "low" nil)))
          (should (equal (take 2 queued) '(insert nil)))
          (should (string= (plist-get (nth 2 queued) :model) "remote-model"))
          (should (string= (plist-get (nth 2 queued) :reasoning-effort) "high")))
      (dolist (buffer (list source selector))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))



(ert-deftest ende-test/selector-rejects-effort-removed-by-live-validation ()
  (let ((source (generate-new-buffer " *ende-selector-effort-stale-source*"))
        (selector (generate-new-buffer " *ende-selector-effort-stale*"))
        (queued nil)
        (ende-provider-profiles '((remote :model "remote-model")))
        (ende-model-selection-scope 'global)
        (ende-selector--effort-selections (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (puthash '(remote "remote-model") "high"
                   ende-selector--effort-selections)
          (with-current-buffer selector
            (ende-selector-mode)
            (setq ende-selector--source-buffer source
                  ende-selector--providers '(remote)
                  ende-selector--provider-index 0
                  ende-selector--models '("remote-model")
                  ende-selector--model-index 0
                  ende-selector--model-state :ready)
            (ende-selector--set-effort-context)
            (cl-letf (((symbol-function 'ende-provider-probe-reasoning-efforts)
                       (lambda (_profile _model callback)
                         (funcall callback '("low") "low" nil)
                         (lambda () nil)))
                      ((symbol-function 'ende-enqueue-operation)
                       (lambda (&rest _)
                         (setq queued t))))
              (ende-selector-insert))
            (should-not queued)
            (should-not ende-selector--effort)
            (should (eq ende-selector--effort-state :error))
            (should-not (gethash '(remote "remote-model")
                                 ende-selector--effort-selections))))
      (dolist (buffer (list source selector))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))



(ert-deftest ende-test/selector-uses-bottom-window-and-quits-it-on-close ()
  (let ((source (generate-new-buffer " *ende-selector-window-source*"))
        (display-action nil)
        (quit-window-call nil)
        (probe-profile nil)
        (selector nil))
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
                  (ende-provider-profiles
                   '((local :model "local-model"))))
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
        (ende-provider-profiles
         '((remote :model "remote-model" :max-concurrency 2
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
      (dolist (key '("Provider:" "Model:" "Concurrency:" "Parallel:" "Effort:"
                     "Quota: unavailable"
                     "i  - insert" "a  - ask" "d  - queue dashboard"
                      "I  - insert anywhere"
                     "k  - compact chat context"
                     "w  - write anywhere"
                     "m  - select/search model"
                     "e  - select reasoning effort"
                     "s  - summarize"
                     "r  - rubber duck"
                     "RET       - confirm and save selection"
                     "C-c C-c   - approve all LLM changes"
                     "C-c C-k   - reject all LLM changes"))
        (should (string-match-p (regexp-quote key) (buffer-string)))))))



(ert-deftest ende-test/selector-runtime-keybindings-survive-reload ()
  (let ((original-c (lookup-key ende-selector-mode-map (kbd "c")))
        (original-C (lookup-key ende-selector-mode-map (kbd "C")))
        (original-P (lookup-key ende-selector-mode-map (kbd "P")))
        (original-e (lookup-key ende-selector-mode-map (kbd "e")))
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



(ert-deftest ende-test/selector-labels-are-single-line-and-display-bounded ()
  (let ((multiline (ende-selector--display-label "provider\nspoof" 24))
        (long (ende-selector--display-label
               "provider-name-that-is-longer-than-the-row" 24)))
    (should (string-match-p "provider spoof" multiline))
    (should-not (string-match-p "\n" multiline))
    (should (= (string-width multiline) 24))
    (should (string-suffix-p "..." (string-trim-right long)))
    (should (= (string-width long) 24))))



(ert-deftest ende-test/selector-probe-callbacks-are-generation-bound ()
  (let ((buffer (generate-new-buffer " *ende-selector-probe-test*"))
        (callbacks nil)
        (cancelled 0)
        (ende-model-selection-scope 'buffer)
        (ende-model-selection--last-used nil)
        (ende-model-selection--buffer-selections
         (make-hash-table :test #'equal))
        (ende-provider-profiles
         '((remote :url "http://remote.example/v1"
                   :model "configured"))))
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
              (should (equal ende-selector--models '("configured")))
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
          (lambda (callback)
            (cl-incf attempts)
            (if (= attempts 1)
                (funcall
                 callback nil
                 (ende-error-create
                  :kind 'discovery
                  :message "temporary probe failure"))
              (funcall callback '("remote/first") nil))
            (lambda () nil)))
         (ende-providers '(remote))
         (ende-provider-profiles
          (list (list 'remote
                      :url "http://remote.example/v1/chat/completions"
                      :model-discovery-function probe)))
         (ende-selector--last-selection nil)
         (ende-selector--model-validations (make-hash-table :test #'equal))
         (ende-provider--model-cache (make-hash-table :test #'equal))
         (ende-provider--model-inflight (make-hash-table :test #'equal))
         (ende-provider--model-cancellations (make-hash-table :test #'equal))
         (ende-provider--model-generation 0))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(remote)
                ende-selector--provider-index 0)
          (ende-selector--set-provider 0)
          (should (= attempts 1))
          (should (eq ende-selector--model-state :error))
          (should (string-match-p "probe failed" (buffer-string)))

          (ende-selector--set-provider 0)
          (should (= attempts 2))
          (should (eq ende-selector--model-state :ready))
          (should (string= (ende-selector--model-name) "remote/first"))

          (ende-selector--set-provider 0)
          (should (= attempts 2)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest ende-test/selector-ask-includes-only-reasoning-ancestors ()
  (dolist (policy '(root-prefix current-section-only))
    (ende-test--reset)
    (let ((ende-chat-context-policy policy)
          (ende-chat-include-reasoning nil)
          (ende-provider-profiles '((remote :model "configured-model")))
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
  (let ((ende-provider-profiles '((remote :model "configured-model")))
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
    (let ((ende-selector--last-selection nil)
          (ende-provider-profiles '((remote :model "configured-model")))
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
    (let ((ende-selector--last-selection nil)
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
					ende-selector--models '("local-model")
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

(provide 'ende-selector-test)

;;; ende-selector-test.el ends here
