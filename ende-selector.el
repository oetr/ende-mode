;;; ende-selector.el --- Provider and model selector for ende -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools, llm

;;; Commentary:

;; The selector is the entry point for all interactive ende requests.  It
;; keeps the source buffer separate from the selector buffer and attaches the
;; selected provider/model to the queued job.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ende-core)
(require 'ende-chat)
(require 'ende-provider)
(require 'ende-commands)
(require 'ende-rubber-duck)
(require 'ende-queue-ui)
(require 'ende-finished)

(defcustom ende-selector-buffer-name "*ende-selector*"
  "Buffer used by the provider and model selector."
  :type 'string
  :group 'ende)

(defcustom ende-selector-window-min-height 4
  "Minimum total height of the bottom selector window."
  :type 'integer
  :group 'ende)

(defface ende-selector-model-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face used for model IDs in the completion picker."
  :group 'ende)

(defface ende-selector-price-muted-face
  '((t :inherit shadow))
  "Face used for non-output prices in the completion picker."
  :group 'ende)

(defface ende-selector-price-output-face
  '((t :inherit error))
  "Face used for output prices in the completion picker."
  :group 'ende)

(when (and (boundp 'ende-selector--last-selection)
           (not (eq (indirect-variable 'ende-selector--last-selection)
                    'ende-model-selection--last-used))
           (null ende-model-selection--last-used))
  (setq ende-model-selection--last-used ende-selector--last-selection))

(defvaralias 'ende-selector--last-selection
  'ende-model-selection--last-used
  "Compatibility alias for the most recently used model selection.")

(defvar ende-selector--model-validations (make-hash-table :test #'equal)
  "Cached model preference validation results.")

(defvar-local ende-selector--source-buffer nil
  "Source buffer associated with the current selector buffer.")

(defvar-local ende-selector--providers nil
  "Provider names available in the current selector.")

(defvar-local ende-selector--provider-index 0
  "Index of the selected provider in the current selector.")

(defvar-local ende-selector--models nil
  "Model names available for the selected provider.")

(defvar-local ende-selector--model-details nil
  "Cached display metadata for models available to the selected provider.")

(defvar-local ende-selector--model-index 0
  "Index of the selected model in the current selector.")

(defvar-local ende-selector--model-state nil
  "State of model discovery for the selected provider.")

(defvar-local ende-selector--model-selection-generation 0
  "Generation incremented when the user changes the current model.")

(defvar-local ende-selector--control-preferences nil
  "Explicit control preferences for the current provider and model.")

(defvar-local ende-selector--control-context nil
  "Provider/model identity associated with `ende-selector--control-preferences'.")

(defvar-local ende-selector--effort nil
  "Reasoning effort selected for the current provider and model.")

(defvar-local ende-selector--effort-state :none
  "Live validation state of `ende-selector--effort'.")

(defvar-local ende-selector--effort-validated-context nil
  "Provider/model/effort context validated by the current selector probe.")

(defvar-local ende-selector--effort-cancel nil
  "Cancellation function for the current reasoning-effort probe.")

(defvar-local ende-selector--quota nil
  "Latest quota snapshot for the selected provider, or nil.")

(defvar-local ende-selector--quota-state :unavailable
  "Display state of the selected provider's quota snapshot.")

(defvar-local ende-selector--quota-cancel nil
  "Cancellation function for the current quota probe.")

(defvar ende-selector--next-effort-probe-generation 0
  "Next generation identifier assigned to a reasoning-effort probe.")

(defvar-local ende-selector--effort-probe-generation 0
  "Generation identifier for the current reasoning-effort probe.")

(defvar ende-selector--next-quota-probe-generation 0
  "Next generation identifier assigned to a quota probe.")

(defvar-local ende-selector--quota-probe-generation 0
  "Generation identifier for the current quota probe.")

(defvar-local ende-selector--pending-run nil
  "Action waiting for live reasoning-effort validation, or nil.")

(defvar-local ende-selector--probe-cancel nil
  "Cancellation function for the current model probe.")

(defvar ende-selector--next-probe-generation 0
  "Next generation identifier assigned to a selector model probe.")

(defvar-local ende-selector--probe-generation 0
  "Generation identifier for the current selector model probe.")

(defvar-local ende-selector--window nil
  "Window displaying the current selector buffer.")

(defun ende-selector--fit-window (window)
  "Fit WINDOW to the selector contents."
  (fit-window-to-buffer window nil ende-selector-window-min-height))

(defun ende-selector--provider-name ()
  "Return the selected provider name."
  (nth ende-selector--provider-index ende-selector--providers))

(defun ende-selector--provider-profile ()
  "Return the selected provider profile."
  (let ((model (ende-selector--model-name))
        (provider (ende-selector--provider-name)))
    (if model
        (ende-provider-profile-for-model provider model)
      (ende-provider-profile provider))))

(defun ende-selector--model-name ()
  "Return the selected model name, or nil while discovery is unavailable."
  (nth ende-selector--model-index ende-selector--models))

(defun ende-selector--control-context-key ()
  "Return the selected provider/model identity, or nil while incomplete."
  (let ((provider (ende-selector--provider-name))
        (model (ende-selector--model-name)))
    (and provider model (list provider model))))

(defun ende-selector--resolved-profile ()
  "Return the current provider/model profile with controls applied."
  (ende-provider-profile-for-selection
   (ende-selector--provider-name) (ende-selector--model-name)
   ende-selector--control-preferences))

(defun ende-selector--control-setting (control)
  "Return CONTROL's resolved selector setting details."
  (ende-provider-control-setting
   (ende-selector--provider-profile) ende-selector--control-preferences control))

(defun ende-selector--set-control-preference (control value)
  "Save canonical VALUE for CONTROL and refresh the current profile."
  (when ende-selector--pending-run
    (user-error "Wait for reasoning-effort validation to finish"))
  (let ((old-effort ende-selector--effort))
    (setq ende-selector--control-preferences
          (plist-put ende-selector--control-preferences control value)
          ende-selector--effort
          (plist-get (ende-selector--resolved-profile) :reasoning-effort))
    (unless (equal old-effort ende-selector--effort)
      (setq ende-selector--effort-validated-context nil
            ende-selector--effort-state
            (if ende-selector--effort :remembered :none)))
    (when (and (eq ende-selector--model-state :ready)
               (ende-selector--provider-name)
               (ende-selector--model-name))
      (ende-model-selection-save-control-preferences
       (ende-selector--provider-name) (ende-selector--model-name)
       ende-selector--control-preferences
       (or ende-selector--source-buffer (current-buffer))))
    (ende-selector--render)))

(defun ende-selector--remembered-model (provider)
  "Return the remembered model for PROVIDER, or nil."
  (let ((selection
         (ende-model-selection-for-buffer
          (or ende-selector--source-buffer (current-buffer)))))
    (and (eq provider (plist-get selection :provider))
         (plist-get selection :model))))

(defun ende-selector--preferred-model (provider profile)
  "Return the saved or explicitly configured model for PROVIDER PROFILE."
  (or (ende-selector--remembered-model provider)
      (ende-provider-configured-model profile)))

(defun ende-selector--model-validation-key (provider models generation model)
  "Return the validation key for MODEL in PROVIDER's catalog."
  (list provider generation model
        (secure-hash 'sha256 (prin1-to-string models))))

(defun ende-selector--valid-model-p (provider profile models generation model)
  "Return non-nil when MODEL is available in the current catalog.
Report an unavailable model once for each catalog generation."
  (when (and (stringp model) models)
    (let* ((key (ende-selector--model-validation-key
                 provider models generation model))
           (status (gethash key ende-selector--model-validations)))
      (if status
          (plist-get status :valid)
        (let ((valid (member model models)))
          (puthash key (list :valid valid)
                   ende-selector--model-validations)
          (unless valid
            (message
             "ende: model %s is unavailable for %s; using %s"
             model
             (ende-provider-client-name profile)
             (or (car models) "the first available model")))
          valid)))))

(defun ende-selector--model-detail (model)
  "Return cached display metadata for MODEL, or nil."
  (cl-find model ende-selector--model-details
           :key (lambda (detail) (plist-get detail :id))
           :test #'equal))

(defun ende-selector--model-candidates ()
  "Return completion display candidates paired with their model IDs."
  (mapcar
   (lambda (model)
     (cons (ende-bounded-single-line model 160 t) model))
   (cl-remove-if-not #'stringp ende-selector--models)))

(defun ende-selector--candidate-for-model (model candidates)
  "Return the completion candidate for MODEL in CANDIDATES."
  (car (cl-find model candidates :key #'cdr :test #'equal)))

(defun ende-selector--model-affixations (completions candidates details)
  "Return styled affixations for COMPLETIONS.
CANDIDATES maps displayed completion strings to model IDs.  DETAILS contains
the discovered pricing metadata for those model IDs."
  (mapcar
   (lambda (candidate)
     (let* ((model (cdr (cl-find candidate candidates
                                 :key #'car :test #'string=)))
            (detail (cl-find model details
                             :key (lambda (value) (plist-get value :id))
                             :test #'equal))
            (prices
             (and detail
                  (ende-provider-model-price-annotation
                   detail
                   (lambda (field text)
                     (propertize
                      text 'face
                      (if (eq field :completion)
                          'ende-selector-price-output-face
                        'ende-selector-price-muted-face)))))))
       (list (propertize candidate 'face 'ende-selector-model-face)
             ""
             (or prices ""))))
   completions))

(defun ende-selector--index-of (value values)
  "Return the index of VALUE in VALUES, or nil."
  (cl-position value values :test #'equal))

(defun ende-selector--display-provider ()
  "Return a display name for the selected provider."
  (ende-provider-client-name (ende-selector--provider-profile)))

(defun ende-selector--display-label (value width)
  "Return sanitized VALUE padded or truncated to display WIDTH columns."
  (let* ((text (or (ende-bounded-single-line value 80 t) ""))
         (text (truncate-string-to-width text width 0 nil "...")))
    (concat text (make-string (max 0 (- width (string-width text))) ?\s))))

(defun ende-selector--provider-concurrency ()
  "Return the selected provider's current concurrency limit."
  (ende-provider-max-concurrency (ende-selector--provider-profile)))

(defun ende-selector--provider-parallel-p ()
  "Return whether the selected provider defaults to parallel jobs."
  (ende-provider-parallel-enabled-p (ende-selector--provider-profile)))

(defun ende-selector--quota-supported-p ()
  "Return non-nil when the selected provider has an OAuth quota endpoint."
  (let ((profile (ende-selector--provider-profile)))
    (and (eq (ende-selector--provider-name) 'openai)
         (eq (plist-get profile :auth-method) 'opencode-oauth))))

(defun ende-selector--quota-display ()
  "Return the selected provider's quota display text."
  (pcase ende-selector--quota-state
    (:loading "loading...")
    (:ready (or (and ende-selector--quota
                     (ende-codex-quota-string ende-selector--quota))
                "unavailable"))
    (_ "unavailable")))

(defun ende-selector--control-value-label (_control setting)
  "Format SETTING with its preference source."
  (let ((value (plist-get setting :value))
        (source (plist-get setting :source)))
    (pcase source
      ('explicit
       (if value
           (format "%s (explicit)" value)
         "Provider default (omitted)"))
      ('profile
       (if value
           (format "%s (profile)" value)
         "Omitted (profile nil)"))
      (_
       (if (plist-get setting :provider-default-known)
           (format "Provider default (%s; omitted)"
                   (plist-get setting :provider-default))
         "Provider default (omitted)")))))

(defun ende-selector--temperature-ignored-p (profile)
  "Return non-nil when PROFILE's DeepSeek temperature has no effect."
  (let ((thinking (plist-get profile :thinking))
        (effort (plist-get profile :reasoning-effort)))
    (and (eq (plist-get profile :builtin-profile) 'deepseek)
         (ende-provider-deepseek-model-verified-p profile)
         (not (eq thinking 'off))
         (not (and (null thinking) (equal effort "none"))))))

(defun ende-selector--control-supported-p (profile control)
  "Return non-nil when the selector can edit CONTROL for PROFILE."
  (let ((descriptor (ende-provider-control-descriptor profile control)))
    (and descriptor
         (or (not (eq control :reasoning-effort))
             (plist-member descriptor :choices)
             (functionp
              (plist-get profile :reasoning-effort-probe-function))))))

(defun ende-selector--control-value (control profile)
  "Render CONTROL's effective value and source in PROFILE."
  (let* ((setting (ende-selector--control-setting control))
         (mapped (ende-provider-control-descriptor profile control))
         (text (if (and (not mapped)
                        (null (plist-get setting :value))
                        (eq (plist-get setting :source) 'provider-default))
                   "not mapped"
                 (ende-selector--control-value-label control setting))))
    (when (and (eq control :temperature)
               (ende-selector--temperature-ignored-p
                (ende-selector--resolved-profile)))
      (setq text (concat text " (no effect in thinking mode)")))
    (unless mapped
      (unless (equal text "not mapped")
        (setq text (concat text " (unverified mapping)"))))
    text))

(defun ende-selector--render ()
  "Render the current selector state."
  (let* ((profile (ende-selector--provider-profile))
         (model-name (ende-selector--model-name))
         (provider (ende-selector--display-provider))
         (model (or model-name
                    (if (memq ende-selector--model-state
                              '(:loading :validating))
                        "validating..."
                      "none available")))
         (model-status
          (pcase ende-selector--model-state
            ((or :loading :validating)
             (if model-name " validating..." ""))
            (:error " probe failed")
            (_ "")))
         (thinking (plist-get (ende-selector--control-setting :thinking)
                              :value))
         (effort-label
          (cond
           ((eq thinking 'off)
            (format "%s (inactive while thinking is off)"
                    (ende-selector--control-value
                     :reasoning-effort
                     profile)))
           ((eq ende-selector--effort-state :loading) "validating...")
           ((eq ende-selector--effort-state :invalid)
            (format "%s (unsupported; correct this setting)"
                    ende-selector--effort))
           ((eq ende-selector--effort-state :error)
            (if ende-selector--effort
                (format "%s (lookup failed; retry)" ende-selector--effort)
              "lookup failed; retry"))
           ((eq ende-selector--effort-state :unavailable)
            "capability information unavailable")
           (t (ende-selector--control-value :reasoning-effort profile))))
         (temperature-value
          (ende-selector--control-value :temperature profile))
         (thinking-value
          (if (and (ende-provider-deepseek-model-verified-p profile)
                   (null thinking)
                   (equal (plist-get
                           (ende-selector--control-setting :reasoning-effort)
                           :value)
                          "none"))
              "off (reasoning effort none)"
            (ende-selector--control-value :thinking profile)))
         (effort-value
          (if (and (not (ende-selector--control-supported-p
                         profile :reasoning-effort))
                   (not (eq ende-selector--effort-state :loading)))
              (if (plist-get (ende-selector--control-setting
                              :reasoning-effort)
                             :value)
                  (concat effort-label " (capability unverified)")
                "capability information unavailable")
            effort-label))
         (capability-note
          (when (and (eq (plist-get profile :builtin-profile) 'deepseek)
                     model-name
                     (not (ende-provider-deepseek-model-verified-p profile)))
            (format "DeepSeek control support is unverified for %s.\n"
                    model-name)))
         (compatibility-note
          (when-let* ((message
                       (ende-provider-control-compatibility-error
                        (ende-selector--resolved-profile))))
            (format "Invalid controls: %s\n" message))))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "%-8s - %-18s %s\n" "n/p" "Provider:" provider))
      (insert (format "%-8s - %-18s %s%s\n" "C-n/C-p" "Model:"
                      model-status model))
      (insert (format "%-8s - %-18s %d\n" "c/C" "Concurrency:"
                      (ende-selector--provider-concurrency)))
      (insert (format "%-8s - %-18s %s\n" "P" "Parallel:"
                      (if (ende-selector--provider-parallel-p) "on" "off")))
      (insert "\n")
      (insert (format "%-8s - %-18s %s\n" "T" "Temperature:"
                      temperature-value))
      (insert (format "%-8s - %-18s %s\n" "t" "Thinking:"
                      thinking-value))
      (insert (format "%-8s - %-18s %s\n" "e" "Reasoning effort:"
                      effort-value))
      (when capability-note (insert capability-note))
      (when compatibility-note (insert compatibility-note))
      (insert "\n")
      (insert (format "Quota: %s\n\n" (ende-selector--quota-display)))
      (insert "a  - ask\n"
              "d  - queue dashboard\n"
              "f  - finished jobs\n"
              "R  - reset controls to profile settings\n"
              "i  - insert\n"
              "I  - insert anywhere\n"
              "k  - compact chat context\n"
              "m  - select/search model\n"
	      "r  - rubber duck\n"
              "s  - summarize\n"
	      "w  - write anywhere\n\n"
              "C-c C-c   - approve all LLM changes\n"
              "C-c C-k   - reject all LLM changes\n"
              "RET       - commit selection and close\n"
              "q/ESC     - close without changing active selection\n")
      (goto-char (point-min)))))

(defun ende-selector--cancel-probe ()
  "Cancel the current model probe, if any."
  (let ((cancel ende-selector--probe-cancel))
    (setq ende-selector--probe-cancel nil
          ende-selector--probe-generation
          (cl-incf ende-selector--next-probe-generation))
    (when (functionp cancel)
      (ende-ignore-errors cancel))))

(defun ende-selector--cancel-effort-probe ()
  "Cancel the current reasoning-effort probe, if any."
  (let ((cancel ende-selector--effort-cancel))
    (setq ende-selector--effort-cancel nil
          ende-selector--effort-probe-generation
          (cl-incf ende-selector--next-effort-probe-generation))
    (when (functionp cancel)
      (ende-ignore-errors cancel))))

(defun ende-selector--cancel-quota-probe ()
  "Cancel the current quota probe, if any."
  (let ((cancel ende-selector--quota-cancel))
    (setq ende-selector--quota-cancel nil
          ende-selector--quota-probe-generation
          (cl-incf ende-selector--next-quota-probe-generation))
    (when (functionp cancel)
      (ende-ignore-errors cancel))))

(defun ende-selector--probe-quota ()
  "Probe the selected provider's quota and update the selector display."
  (ende-selector--cancel-quota-probe)
  (let* ((provider (ende-selector--provider-name))
         (selector-buffer (current-buffer))
         (generation (cl-incf ende-selector--next-quota-probe-generation))
         (completed nil))
    (setq ende-selector--quota-probe-generation generation
          ende-selector--quota-state :loading
          ende-selector--quota nil)
    (ende-selector--render)
    (let ((cancel
           (ende-codex-probe-quota
            (lambda (quota error)
              (setq completed t)
              (when (and (buffer-live-p selector-buffer)
                         (with-current-buffer selector-buffer
                           (and (= ende-selector--quota-probe-generation
                                   generation)
                                (eq (ende-selector--provider-name) provider))))
                (with-current-buffer selector-buffer
                  (setq ende-selector--quota-cancel nil
                        ende-selector--quota quota
                        ende-selector--quota-state
                        (if error :error :ready))
                  (ende-selector--render)))))))
      (when (and (not completed)
                 (= ende-selector--quota-probe-generation generation))
        (setq ende-selector--quota-cancel cancel)))))

(defun ende-selector--set-control-context ()
  "Restore explicit control preferences for the selected provider and model."
  (let* ((old-context ende-selector--control-context)
         (context (ende-selector--control-context-key))
         (provider (ende-selector--provider-name))
         (model (ende-selector--model-name))
         (preferences
          (and context
               (copy-tree
                (ende-model-selection-control-preferences
                 provider model
                 (or ende-selector--source-buffer (current-buffer)))))))
    (ende-selector--cancel-effort-probe)
    (setq ende-selector--control-context context
          ende-selector--control-preferences preferences
          ende-selector--effort
          (plist-get (ende-provider-profile-for-selection
                      provider model preferences)
                     :reasoning-effort)
          ende-selector--pending-run nil)
    (unless (equal old-context context)
      (setq ende-selector--effort-validated-context nil))
    (let* ((profile (ende-selector--resolved-profile))
           (active-effort
            (ende-provider-active-reasoning-effort profile)))
      (setq ende-selector--effort-state
            (cond
             ((null ende-selector--effort) :none)
             ((and active-effort
                   (equal ende-selector--effort-validated-context
                          (ende-provider-reasoning-effort-context
                           profile active-effort)))
              :ready)
             (t :remembered))))))

(defun ende-selector--remember-selected-pair ()
  "Record the current ready provider/model as recently used."
  (let ((provider (ende-selector--provider-name))
        (model (ende-selector--model-name)))
    (when (and (eq ende-selector--model-state :ready) provider model)
      (ende-model-selection-remember-pair
       provider model (or ende-selector--source-buffer (current-buffer))))))

(defun ende-selector--effort-error-message (error)
  "Return a useful human-readable message for effort probe ERROR."
  (if (ende-error-p error)
      (ende-error-message error)
    (format "%s" error)))

(defun ende-selector--probe-efforts (callback)
  "Resolve the current model's effort choices and call CALLBACK.
CALLBACK receives (EFFORTS DEFAULT ERROR).  Stale or cancelled callbacks are
ignored.  Return a cancellation function."
  (let ((model (ende-selector--model-name)))
    (unless (and (stringp model) (eq ende-selector--model-state :ready))
      (user-error "ende has no ready model for reasoning-effort discovery"))
    (ende-selector--cancel-effort-probe)
    (let* ((provider (ende-selector--provider-name))
           (profile (ende-selector--resolved-profile))
           (selector-buffer (current-buffer))
           (generation (cl-incf ende-selector--next-effort-probe-generation))
           (selection-generation ende-selector--model-selection-generation)
           (completed nil))
      (setq ende-selector--effort-probe-generation generation
            ende-selector--effort-state :loading)
      (ende-selector--render)
      (let ((cancel
             (ende-provider-probe-reasoning-efforts
              profile model
              (lambda (efforts default error)
                (setq completed t)
                (when (and (buffer-live-p selector-buffer)
                           (with-current-buffer selector-buffer
                             (and (= ende-selector--effort-probe-generation
                                     generation)
                                  (= ende-selector--model-selection-generation
                                     selection-generation)
                                  (eq (ende-selector--provider-name) provider)
                                  (equal (ende-selector--model-name) model))))
                  (with-current-buffer selector-buffer
                    (setq ende-selector--effort-cancel nil)
                    (funcall callback efforts default error)))))))
        (when (and (not completed)
                   (= ende-selector--effort-probe-generation generation))
          (setq ende-selector--effort-cancel cancel))))))

(defun ende-selector--set-models (models &optional preferred)
  "Set the current provider's MODELS and select PREFERRED when available."
  (let ((models (cl-remove-if-not #'stringp models))
        (previous-model (ende-selector--model-name)))
    (setq ende-selector--models models
          ende-selector--model-index
          (or (and (stringp preferred)
                   (ende-selector--index-of preferred models))
              0))
    (unless (equal previous-model (ende-selector--model-name))
      (ende-selector--set-control-context))))

(defun ende-selector--set-provisional-models (profile preferred)
  "Show PROFILE's provisional models while discovery validates PREFERRED."
  (let ((configured (ende-provider-models profile)))
    (cond
     (preferred
      (ende-selector--set-models
       (if (member preferred configured)
           configured
         (cons preferred configured))
       preferred))
     ((ende-provider-configured-models-explicit-p profile)
      (ende-selector--set-models configured))
     (t
      (ende-selector--set-models nil)))))

(defun ende-selector--apply-model-probe-error (profile preferred)
  "Keep explicitly configured PROFILE models usable after discovery fails."
  (let* ((configured-model (ende-provider-configured-model profile))
         (models (ende-provider-models profile))
         (explicit-p
          (and (eq (plist-get profile :type) 'openai)
               (or (and (stringp configured-model)
                        (not (string-empty-p configured-model)))
                   (and (ende-provider-configured-models-explicit-p profile)
                        models)))))
    (setq ende-selector--model-details nil)
    (if explicit-p
        (progn
          (when (and (stringp configured-model)
                     (not (member configured-model models)))
            (setq models (cons configured-model models)))
          (ende-selector--set-models
           models
           (or (and (stringp preferred) (member preferred models) preferred)
               configured-model))
          (setq ende-selector--model-state :ready))
      (setq ende-selector--model-state :error))))

(defun ende-selector--apply-model-catalog
    (profile models details generation preferred &optional error)
  "Apply a discovered or configured model CATALOG to PROFILE.
PREFERRED is selected only when it is present in MODELS."
  (let ((models (cl-remove-if-not #'stringp models)))
    (setq ende-selector--model-details (copy-tree details))
    (if error
        (ende-selector--apply-model-probe-error profile preferred)
      (let ((selected
             (and preferred
                  (ende-selector--valid-model-p
                   (ende-selector--provider-name)
                   profile models generation preferred)
                  preferred)))
        (ende-selector--set-models models selected)
        (setq ende-selector--model-state
              (if models :ready :error))))))

(defun ende-selector--probe-current-provider (&optional _preferred)
  "Probe the selected provider when it has dynamic model discovery."
  (let* ((provider (ende-selector--provider-name))
         (profile (ende-selector--provider-profile))
         (selector-buffer (current-buffer))
         (generation (cl-incf ende-selector--next-probe-generation))
         (selection-generation ende-selector--model-selection-generation)
         (completed nil))
    (setq ende-selector--probe-generation generation)
    (setq ende-selector--model-state :validating)
    (let ((callback
           (lambda (models error)
             (when (and (buffer-live-p selector-buffer)
                        (with-current-buffer selector-buffer
                          (= ende-selector--probe-generation generation)))
               (setq completed t)
               (with-current-buffer selector-buffer
                 (when (eq (ende-selector--provider-name) provider)
                   (setq ende-selector--probe-cancel nil)
                   (let* ((live-profile (ende-provider-profile provider))
                          (live-preferred
                           (if (= selection-generation
                                  ende-selector--model-selection-generation)
                               (ende-selector--preferred-model
                                provider live-profile)
                             (ende-selector--model-name))))
                     (if error
                         (ende-selector--apply-model-probe-error
                          live-profile live-preferred)
                       (ende-selector--apply-model-catalog
                        live-profile models
                        (ende-provider-model-details live-profile)
                        (ende-provider-models-generation live-profile)
                        live-preferred)))
                   (ende-selector--render)))))))
      (let ((cancel (ende-provider-probe-models profile callback)))
        (when (and (not completed)
                   (= ende-selector--probe-generation generation))
          (setq ende-selector--probe-cancel cancel))))))

(defun ende-selector--set-provider (index &optional remember-pair)
  "Select provider at INDEX and start dynamic model discovery if needed."
  (ende-selector--cancel-probe)
  (ende-selector--cancel-effort-probe)
  (ende-selector--cancel-quota-probe)
  (setq ende-selector--pending-run nil)
  (setq ende-selector--provider-index
        (mod index (max 1 (length ende-selector--providers))))
  (cl-incf ende-selector--model-selection-generation)
  (setq ende-selector--models nil
        ende-selector--model-details nil
        ende-selector--model-index 0
        ende-selector--quota nil
        ende-selector--quota-state :unavailable)
  (let* ((provider (ende-selector--provider-name))
         (profile (ende-selector--provider-profile))
         (preferred (ende-selector--preferred-model provider profile))
         (catalog (ende-provider-cached-models profile)))
    (cond
     (catalog
      (if (plist-get catalog :error)
          (ende-selector--apply-model-probe-error profile preferred)
        (ende-selector--apply-model-catalog
         profile
         (plist-get catalog :models)
         (plist-get catalog :details)
         (plist-get catalog :generation)
         preferred)))
     ((ende-provider-model-discovery-available-p profile)
      (ende-selector--set-provisional-models profile preferred)
      (setq ende-selector--model-state :validating)
      (ende-selector--probe-current-provider preferred))
     (t
      (ende-selector--apply-model-catalog
       profile (ende-provider-models profile) nil 0 preferred)))
    (ende-selector--set-control-context)
    (when remember-pair
      (ende-selector--remember-selected-pair))
    (when (ende-selector--quota-supported-p)
      (ende-selector--probe-quota))
    (ende-selector--render)))

(defun ende-selector-next-provider ()
  "Select the next configured provider."
  (interactive)
  (ende-selector--set-provider (1+ ende-selector--provider-index) t))

(defun ende-selector-previous-provider ()
  "Select the previous configured provider."
  (interactive)
  (ende-selector--set-provider (1- ende-selector--provider-index) t))

(defun ende-selector--move-model (delta)
  "Move the selected model by DELTA."
  (if (null ende-selector--models)
      (message "ende: no models available")
    (setq ende-selector--model-index
          (mod (+ ende-selector--model-index delta)
               (length ende-selector--models)))
    (cl-incf ende-selector--model-selection-generation)
    (ende-selector--set-control-context)
    (ende-selector--remember-selected-pair)
    (ende-selector--render)))

(defun ende-selector-next-model ()
  "Select the next model for the current provider."
  (interactive)
  (ende-selector--move-model 1))

(defun ende-selector-previous-model ()
  "Select the previous model for the current provider."
  (interactive)
  (ende-selector--move-model -1))

(defun ende-selector-select-model ()
  "Select a model for the current provider using completion.

The model list is already populated by provider discovery.  Completion is
performed locally so typing a search term never starts another network request."
  (interactive)
  (pcase ende-selector--model-state
    ((or :loading :validating)
     (user-error "ende model discovery is still loading"))
    (:error
     (user-error "ende model discovery failed; change provider or retry"))
    (_
     (let ((models (cl-remove-if-not #'stringp ende-selector--models)))
       (unless models
         (user-error "ende has no models available for this provider"))
       (let* ((candidates (ende-selector--model-candidates))
              (candidate-names (mapcar #'car candidates))
              (current (ende-selector--candidate-for-model
                        (ende-selector--model-name) candidates))
              (details ende-selector--model-details)
              (completion-extra-properties
               (list
                :affixation-function
                (lambda (completions)
                  (ende-selector--model-affixations
                   completions candidates details))))
              (completion-ignore-case t)
              (completion-styles '(substring))
              (choice (completing-read "Model: " candidate-names
                                       nil t nil nil current))
              (selected-model
               (cdr (cl-find choice candidates
                             :key #'car :test #'string=)))
              (index (cl-position selected-model models :test #'equal)))
         (when (and selected-model index)
           (setq ende-selector--models models
                 ende-selector--model-index index)
           (cl-incf ende-selector--model-selection-generation)
           (ende-selector--set-control-context)
           (ende-selector--remember-selected-pair)
           (ende-selector--render)))))))

(defun ende-selector--effort-validation-failed (error)
  "Retain the selected effort and show ERROR as unsupported or temporarily unknown."
  (let ((unsupported
         (and (ende-error-p error)
              (eq (ende-error-kind error) 'unsupported)))
        (capability-unknown
         (and (ende-error-p error)
              (or (eq (ende-error-kind error) 'capability)
                  (and (eq (ende-error-kind error) 'discovery)
                       (string-match-p "disabled"
                                       (ende-error-message error)))))))
    (setq ende-selector--effort-state
          (cond
           ((and unsupported ende-selector--effort) :invalid)
           (capability-unknown :unavailable)
           (t :error))
          ende-selector--effort-validated-context nil
          ende-selector--pending-run nil)
    (ende-selector--render)
    (message "ende: reasoning-effort %s"
             (ende-selector--effort-error-message error))))

(defun ende-selector--control-choice-label (value)
  "Return a completion label for canonical control VALUE."
  (if (symbolp value) (symbol-name value) (format "%s" value)))

(defun ende-selector--require-control-mapping (profile control)
  "Signal a useful error unless PROFILE maps CONTROL."
  (unless (ende-provider-control-descriptor profile control)
    (user-error "%s has no verified mapping for model %s"
                control (or (plist-get profile :model) "<unknown>"))))

(defun ende-selector-select-temperature ()
  "Set the temperature preference for the current provider/model."
  (interactive)
  (let* ((profile (ende-selector--provider-profile))
         (_ (ende-selector--require-control-mapping profile :temperature))
         (setting (ende-selector--control-setting :temperature))
         (current (plist-get setting :value))
         (action
          (completing-read
           "Temperature: " '("Provider default" "Set numeric value")
           nil t nil nil
           (if (numberp current) "Set numeric value" "Provider default"))))
    (if (equal action "Provider default")
        (ende-selector--set-control-preference :temperature nil)
      (ende-selector--set-control-preference
       :temperature
       (read-number "Temperature: " (if (numberp current) current 1.0))))))

(defun ende-selector-select-thinking ()
  "Set the thinking preference for the current provider/model."
  (interactive)
  (let* ((profile (ende-selector--provider-profile))
         (_ (ende-selector--require-control-mapping profile :thinking))
         (descriptor (ende-provider-control-descriptor profile :thinking))
         (choices (if (plist-member descriptor :choices)
                      (plist-get descriptor :choices)
                    '(on off)))
         (setting (ende-selector--control-setting :thinking))
         (current (plist-get setting :value))
         (labels (cons "Provider default"
                       (mapcar #'ende-selector--control-choice-label choices)))
         (choice
          (completing-read
           "Thinking: " labels nil t nil nil
           (and (memq current choices)
                (ende-selector--control-choice-label current)))))
    (ende-selector--set-control-preference
     :thinking
     (unless (equal choice "Provider default")
       (intern choice)))))

(defun ende-selector-reset-controls ()
  "Discard all control overrides and inherit the current profile settings."
  (interactive)
  (setq ende-selector--control-preferences nil
        ende-selector--effort-validated-context nil
        ende-selector--effort nil
        ende-selector--effort-state :none)
  (setq ende-selector--effort
        (plist-get (ende-selector--resolved-profile) :reasoning-effort)
        ende-selector--effort-state
        (if ende-selector--effort :remembered :none))
  (when (and (eq ende-selector--model-state :ready)
             (ende-selector--provider-name)
             (ende-selector--model-name))
    (ende-model-selection-save-control-preferences
     (ende-selector--provider-name) (ende-selector--model-name) nil
     (or ende-selector--source-buffer (current-buffer))))
  (ende-selector--render))

(defun ende-selector--choose-effort (efforts)
  "Ask for one of EFFORTS or Provider default."
  (let* ((setting (ende-selector--control-setting :reasoning-effort))
         (current (plist-get setting :value))
         (choices (cons "Provider default" efforts))
         (initial (and (or (not current) (member current efforts))
                       (or (and current (member current efforts) current)
                           "Provider default")))
         (choice
          (completing-read "Reasoning effort: " choices nil t
                           (and current (not (member current efforts)) current)
                           nil initial)))
    (if (equal choice "Provider default")
        (ende-selector--set-control-preference :reasoning-effort nil)
      (ende-selector--set-control-preference :reasoning-effort choice)
      (let* ((profile (ende-selector--resolved-profile))
             (active (ende-provider-active-reasoning-effort profile)))
        (when active
          (setq ende-selector--effort-validated-context
                (ende-provider-reasoning-effort-context profile active)))
        (setq ende-selector--effort-state
              (if active :ready :remembered))
        (ende-selector--render)))))

(defun ende-selector-select-effort ()
  "Choose a reasoning effort supported by the current profile/model."
  (interactive)
  (when (eq ende-selector--effort-state :loading)
    (user-error "ende reasoning-effort discovery is still loading"))
  (ende-selector--probe-efforts
   (lambda (efforts default error)
     (if error
         (if (and (ende-error-p error)
                  (eq (ende-error-kind error) 'capability))
             (when (equal
                    (completing-read "Reasoning effort: "
                                     '("Provider default") nil t nil nil
                                     "Provider default")
                    "Provider default")
               (ende-selector--set-control-preference
                :reasoning-effort nil))
           (ende-selector--effort-validation-failed error))
       (ignore default)
       (if (null efforts)
           (ende-selector--effort-validation-failed
            (ende-error-create
             :kind 'capability
             :message "No reasoning-effort choices are available"))
         (ende-selector--choose-effort efforts))))))

(defun ende-selector-set-concurrency ()
  "Set and save the selected provider's session concurrency limit."
  (interactive)
  (let* ((provider (ende-selector--provider-name))
         (current (ende-selector--provider-concurrency))
         (value (read-number "Max concurrent jobs: " current)))
    (ende-provider-set-runtime-setting provider :max-concurrency value)
    (ende-selector--render)
    (ende-process-next)))

(defun ende-selector-toggle-parallel ()
  "Toggle and save the selected provider's default parallel setting."
  (interactive)
  (let* ((provider (ende-selector--provider-name))
         (enabled (not (ende-selector--provider-parallel-p))))
    (ende-provider-set-runtime-setting provider :parallel enabled)
    (ende-selector--render)
    (ende-process-next)))

(defun ende-selector--review-source (function)
  "Call review FUNCTION for the selector's source buffer and close selector."
  (let ((source ende-selector--source-buffer))
    (unless (buffer-live-p source)
      (user-error "Source buffer no longer exists"))
    (with-current-buffer source
      (funcall function source))
    (ende-selector-close)))

(defun ende-selector-approve-source-output ()
  "Approve all generated output in the selector's source buffer."
  (interactive)
  (ende-selector--review-source #'ende-approve-all-output))

(defun ende-selector-reject-source-output ()
  "Reject all generated output in the selector's source buffer."
  (interactive)
  (ende-selector--review-source #'ende-reject-all-output))

(defun ende-selector--save-selection ()
  "Save the current provider/model and explicit controls."
  (let ((provider (ende-selector--provider-name))
        (model (ende-selector--model-name))
        (source (or ende-selector--source-buffer (current-buffer))))
    (when (and provider (stringp model))
      (ende-model-selection-save
       (append (list :provider provider :model model)
               (when ende-selector--control-preferences
                 (list :control-preferences
                       (copy-tree ende-selector--control-preferences))))
       source
       (not (eq ende-selector--model-state :ready))))))

(defun ende-selector-close ()
  "Close the selector without starting a request."
  (interactive)
  (ende-selector--cancel-probe)
  (ende-selector--cancel-effort-probe)
  (ende-selector--cancel-quota-probe)
  (setq ende-selector--pending-run nil)
  (let ((buffer (current-buffer))
        (window (or (and (window-live-p ende-selector--window)
                         ende-selector--window)
                    (get-buffer-window (current-buffer) t))))
    (setq ende-selector--window nil)
    (when (window-live-p window)
      (quit-window t window))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun ende-selector--buffer-killed ()
  "Cancel a selector probe when its buffer is killed externally."
  (ende-selector--cancel-probe)
  (ende-selector--cancel-effort-probe)
  (ende-selector--cancel-quota-probe)
  (setq ende-selector--pending-run nil))

(defun ende-selector-confirm ()
  "Confirm the current choice and close the selector."
  (interactive)
  (when (eq ende-selector--effort-state :invalid)
    (user-error "Choose a supported reasoning effort or Provider default"))
  (condition-case validation-error
      (ende-provider-validate-controls (ende-selector--resolved-profile))
    ((error quit)
     (user-error "%s" (error-message-string validation-error))))
  (ende-selector--save-selection)
  (ende-selector-close))

(defun ende-selector--dispatch (type instruction)
  "Dispatch TYPE and INSTRUCTION using the current live-validated selection."
  (let* ((source ende-selector--source-buffer)
         (provider (ende-selector--provider-name))
         (model (ende-selector--model-name))
         (profile (ende-provider-profile-for-selection
                   provider model ende-selector--control-preferences))
         (effort (ende-provider-active-reasoning-effort profile))
         (context (and effort
                       (ende-provider-reasoning-effort-context
                        profile effort))))
    (unless (buffer-live-p source)
      (user-error "ende source buffer no longer exists"))
    (unless (stringp model)
      (user-error "ende has no selected model"))
    (setq profile
          (plist-put profile :reasoning-effort-validated-context
                     (and (equal context
                                 ende-selector--effort-validated-context)
                          context)))
    (ende-selector--save-selection)
    (if (eq type 'chat)
        (if (with-current-buffer source
              (derived-mode-p 'org-mode))
            (progn
              (ende-selector-close)
              (with-current-buffer source
                (ende-ask profile)))
          (let ((buffer (ende-chat--ensure-buffer)))
            (ende-chat--ensure-draft-root buffer)
            (ende-selector-close)
            (pop-to-buffer buffer)
            (message "ende: chat buffer ready; press C-c e a to ask")))
      (when (and (eq type 'chat-compact)
                 (not (with-current-buffer source (derived-mode-p 'org-mode))))
        (user-error "Chat compaction requires an Org buffer"))
      (ende-selector-close)
      (with-current-buffer source
        (pcase type
          ('chat-compact (ende-compact profile))
          ('rubber-duck (ende-rubber-duck instruction profile))
          (_ (ende-enqueue-operation type instruction profile)))))))

(defun ende-selector--run (type &optional instruction)
  "Run TYPE from the source buffer using the selected provider/model.

A selected reasoning effort is checked immediately before dispatch when its
static or live capability source requires validation."
  (when ende-selector--pending-run
    (user-error "ende is validating the selected reasoning effort"))
  (when (eq ende-selector--effort-state :loading)
    (user-error "ende reasoning-effort discovery is still loading"))
  (when (eq ende-selector--effort-state :invalid)
    (user-error "Choose a supported reasoning effort or Provider default"))
  (ende-selector--save-selection)
  (let* ((profile (ende-selector--resolved-profile))
         (effort (ende-provider-active-reasoning-effort profile))
         (context (and effort
                       (ende-provider-reasoning-effort-context
                        profile effort))))
    (condition-case validation-error
        (ende-provider-validate-controls profile)
      ((error quit)
       (setq ende-selector--effort-state
             (if effort :invalid :error)
             ende-selector--pending-run nil)
       (ende-selector--render)
       (user-error "%s" (error-message-string validation-error))))
    (if (or (null effort)
            (equal context ende-selector--effort-validated-context))
        (ende-selector--dispatch type instruction)
      (setq ende-selector--pending-run (list type instruction))
      (ende-selector--probe-efforts
       (lambda (efforts _default error)
         (let ((validation-error
                (ende-provider-reasoning-effort-validation-error
                 profile effort efforts error)))
           (if validation-error
               (ende-selector--effort-validation-failed validation-error)
             (setq ende-selector--effort-validated-context context
                   ende-selector--effort-state :ready)
             (let ((pending ende-selector--pending-run))
               (setq ende-selector--pending-run nil)
               (ende-selector--render)
               (when pending
                 (ende-selector--dispatch (car pending) (cadr pending)))))))))))

(defun ende-selector-insert ()
  "Run insert using the current selector choice."
  (interactive)
  (ende-selector--run 'insert))

(defun ende-selector-ask ()
  "Ask the current Org question using the selector choice."
  (interactive)
  (ende-selector--run 'chat))

(defun ende-selector-dashboard ()
  "Close the selector and open the Ende queue dashboard."
  (interactive)
  (ende-selector-close)
  (ende-queue-open))

(defun ende-selector-finished-jobs ()
  "Close the selector and open the finished-jobs list."
  (interactive)
  (ende-selector-close)
  (ende-finished-open))

(defun ende-selector-compact ()
  "Compact the current chat context using the selector choice."
  (interactive)
  (ende-selector--run 'chat-compact))

(defun ende-selector-write-anywhere ()
  "Run a structured multi-edit using the current selector choice."
  (interactive)
  (ende-selector--run
   'write-anywhere
   (read-string "Prompt: ")))

(defun ende-selector-insert-anywhere ()
  "Run a structured multi-insertion using the current selector choice."
  (interactive)
  (ende-selector--run
   'insert-anywhere
   (read-string "Prompt: ")))

(defun ende-selector-improve ()
  "Prompt for an improvement and run it using the current choice."
  (interactive)
  (ende-selector--run
   'improve (read-string "How should the text be improved? ")))

(defun ende-selector-summarize ()
  "Run summarize using the current selector choice."
  (interactive)
  (ende-selector--run 'summarize))

(defun ende-selector-rubber-duck ()
  "Capture an explanation exercise using the current selector choice."
  (interactive)
  (ende-selector--run 'rubber-duck (ende-rubber-duck-read-level)))

(defvar ende-selector-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    map)
  "Keymap for `ende-selector-mode'.")

(defun ende-selector--install-keybindings ()
  "Install selector bindings, including when this file is reloaded."
  (define-key ende-selector-mode-map (kbd "n") #'ende-selector-next-provider)
  (define-key ende-selector-mode-map (kbd "p") #'ende-selector-previous-provider)
  (define-key ende-selector-mode-map (kbd "C-n") #'ende-selector-next-model)
  (define-key ende-selector-mode-map (kbd "C-p") #'ende-selector-previous-model)
  (define-key ende-selector-mode-map (kbd "c") #'ende-selector-set-concurrency)
  (define-key ende-selector-mode-map (kbd "C") #'ende-selector-set-concurrency)
  (define-key ende-selector-mode-map (kbd "P") #'ende-selector-toggle-parallel)
  (define-key ende-selector-mode-map (kbd "i") #'ende-selector-insert)
  (define-key ende-selector-mode-map (kbd "a") #'ende-selector-ask)
  (define-key ende-selector-mode-map (kbd "d") #'ende-selector-dashboard)
  (define-key ende-selector-mode-map (kbd "f") #'ende-selector-finished-jobs)
  (define-key ende-selector-mode-map (kbd "k") #'ende-selector-compact)
  (define-key ende-selector-mode-map (kbd "w") #'ende-selector-write-anywhere)
  (define-key ende-selector-mode-map (kbd "I") #'ende-selector-insert-anywhere)
  (define-key ende-selector-mode-map (kbd "m") #'ende-selector-select-model)
  (define-key ende-selector-mode-map (kbd "T") #'ende-selector-select-temperature)
  (define-key ende-selector-mode-map (kbd "t") #'ende-selector-select-thinking)
  (define-key ende-selector-mode-map (kbd "e") #'ende-selector-select-effort)
  (define-key ende-selector-mode-map (kbd "E") nil)
  (define-key ende-selector-mode-map (kbd "R") #'ende-selector-reset-controls)
  (define-key ende-selector-mode-map (kbd "s") #'ende-selector-summarize)
  (define-key ende-selector-mode-map (kbd "r") #'ende-selector-rubber-duck)
  (define-key ende-selector-mode-map (kbd "RET") #'ende-selector-confirm)
  (define-key ende-selector-mode-map (kbd "C-c C-c")
              #'ende-selector-approve-source-output)
  (define-key ende-selector-mode-map (kbd "C-c C-k")
              #'ende-selector-reject-source-output)
  (define-key ende-selector-mode-map (kbd "DEL") nil)
  (define-key ende-selector-mode-map (kbd "<backspace>") nil)
  (define-key ende-selector-mode-map (kbd "q") #'ende-selector-close)
  (define-key ende-selector-mode-map (kbd "ESC") #'ende-selector-close))

(ende-selector--install-keybindings)

(define-derived-mode ende-selector-mode special-mode "ende-selector"
  "Major mode for selecting a ende provider and model."
  (ende-selector--install-keybindings)
  (add-hook 'kill-buffer-hook #'ende-selector--buffer-killed nil t)
  (setq truncate-lines t)
  (setq buffer-read-only t))

;;;###autoload
(defun ende-selector-open ()
  "Open the provider/model selector for the current source buffer."
  (interactive)
  (let* ((source (current-buffer))
         (_ (ende-model-selection-initialize-buffer source))
         (providers (ende-provider-names))
         (selection (ende-model-selection-for-buffer source))
         (last-provider (plist-get selection :provider))
         (provider-index (or (cl-position last-provider providers :test #'eq) 0))
         (buffer (get-buffer-create ende-selector-buffer-name)))
    (with-current-buffer buffer
      (ende-selector--cancel-probe)
      (ende-selector--cancel-effort-probe)
      (ende-selector--cancel-quota-probe)
      (ende-selector-mode)
      (setq ende-selector--source-buffer source
            ende-selector--providers providers
            ende-selector--provider-index provider-index
            ende-selector--models nil
            ende-selector--model-details nil
            ende-selector--model-index 0
            ende-selector--model-selection-generation 0
            ende-selector--model-state :ready
            ende-selector--control-preferences nil
            ende-selector--control-context nil
            ende-selector--effort nil
            ende-selector--effort-state :none
            ende-selector--effort-validated-context nil
            ende-selector--quota nil
            ende-selector--quota-state :unavailable
            ende-selector--pending-run nil)
      (ende-selector--set-provider provider-index))
    (with-current-buffer buffer
      (ende-selector--remember-selected-pair))
    (let ((window
           (display-buffer
            buffer
            `((display-buffer-at-bottom)
              (window-height . ende-selector--fit-window)
              (window-min-height . ,ende-selector-window-min-height)))))
      (with-current-buffer buffer
        (setq ende-selector--window window))
      (when (window-live-p window)
        (ende-selector--fit-window window)
        (select-window window)))))

(provide 'ende-selector)

;;; ende-selector.el ends here
