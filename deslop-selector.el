;;; deslop-selector.el --- Provider and model selector for deslop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools, llm

;;; Commentary:

;; The selector is the entry point for all interactive deslop requests.  It
;; keeps the source buffer separate from the selector buffer and attaches the
;; selected provider/model to the queued job.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'deslop-core)
(require 'deslop-provider)
(require 'deslop-commands)
(require 'deslop-rubber-duck)

(defcustom deslop-selector-buffer-name "*deslop-selector*"
  "Buffer used by the provider and model selector."
  :type 'string
  :group 'deslop)

(defcustom deslop-selector-window-min-height 4
  "Minimum total height of the bottom selector window."
  :type 'integer
  :group 'deslop)

(defface deslop-selector-model-face
  '((t :inherit deslop-badge-face :weight bold))
  "Face used for model IDs in the completion picker."
  :group 'deslop)

(defface deslop-selector-price-muted-face
  '((t :inherit shadow))
  "Face used for non-output prices in the completion picker."
  :group 'deslop)

(defface deslop-selector-price-output-face
  '((t :inherit error))
  "Face used for output prices in the completion picker."
  :group 'deslop)

(defvar deslop-selector--last-selection nil
  "Most recently confirmed selector choice as a provider/model plist.")

(defvar-local deslop-selector--source-buffer nil
  "Source buffer associated with the current selector buffer.")

(defvar-local deslop-selector--providers nil
  "Provider names available in the current selector.")

(defvar-local deslop-selector--provider-index 0
  "Index of the selected provider in the current selector.")

(defvar-local deslop-selector--models nil
  "Model names available for the selected provider.")

(defvar-local deslop-selector--model-details nil
  "Cached display metadata for models available to the selected provider.")

(defvar-local deslop-selector--model-index 0
  "Index of the selected model in the current selector.")

(defvar-local deslop-selector--model-state nil
  "State of model discovery for the selected provider.")

(defvar-local deslop-selector--probe-cancel nil
  "Cancellation function for the current model probe.")

(defvar deslop-selector--next-probe-generation 0
  "Next generation identifier assigned to a selector model probe.")

(defvar-local deslop-selector--probe-generation 0
  "Generation identifier for the current selector model probe.")

(defvar-local deslop-selector--window nil
  "Window displaying the current selector buffer.")

(defun deslop-selector--fit-window (window)
  "Fit WINDOW to the selector contents."
  (fit-window-to-buffer window nil deslop-selector-window-min-height))

(defun deslop-selector--provider-name ()
  "Return the selected provider name."
  (nth deslop-selector--provider-index deslop-selector--providers))

(defun deslop-selector--provider-profile ()
  "Return the selected provider profile."
  (deslop-provider-profile (deslop-selector--provider-name)))

(defun deslop-selector--model-name ()
  "Return the selected model name, or nil while discovery is unavailable."
  (nth deslop-selector--model-index deslop-selector--models))

(defun deslop-selector--model-detail (model)
  "Return cached display metadata for MODEL, or nil."
  (cl-find model deslop-selector--model-details
           :key (lambda (detail) (plist-get detail :id))
           :test #'equal))

(defun deslop-selector--model-candidates ()
  "Return completion display candidates paired with their model IDs."
  (mapcar
   (lambda (model)
     (cons (deslop-bounded-single-line model 160 t) model))
   (cl-remove-if-not #'stringp deslop-selector--models)))

(defun deslop-selector--candidate-for-model (model candidates)
  "Return the completion candidate for MODEL in CANDIDATES."
  (car (cl-find model candidates :key #'cdr :test #'equal)))

(defun deslop-selector--model-affixations (completions candidates details)
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
                  (deslop-provider-model-price-annotation
                   detail
                   (lambda (field text)
                     (propertize
                      text 'face
                      (if (eq field :completion)
                          'deslop-selector-price-output-face
                        'deslop-selector-price-muted-face)))))))
       (list (propertize candidate 'face 'deslop-selector-model-face)
             ""
             (if prices (concat "\n" prices) ""))))
   completions))

(defun deslop-selector--index-of (value values)
  "Return the index of VALUE in VALUES, or nil."
  (cl-position value values :test #'equal))

(defun deslop-selector--display-provider ()
  "Return a display name for the selected provider."
  (deslop-provider-client-name (deslop-selector--provider-profile)))

(defun deslop-selector--display-label (value width)
  "Return sanitized VALUE padded or truncated to display WIDTH columns."
  (let* ((text (or (deslop-bounded-single-line value 80 t) ""))
         (text (truncate-string-to-width text width 0 nil "...")))
    (concat text (make-string (max 0 (- width (string-width text))) ?\s))))

(defun deslop-selector--provider-concurrency ()
  "Return the selected provider's current concurrency limit."
  (deslop-provider-max-concurrency (deslop-selector--provider-profile)))

(defun deslop-selector--provider-parallel-p ()
  "Return whether the selected provider defaults to parallel jobs."
  (deslop-provider-parallel-enabled-p (deslop-selector--provider-profile)))

(defun deslop-selector--render ()
  "Render the current selector state."
  (let* ((provider (deslop-selector--display-label
                    (deslop-selector--display-provider) 24))
         (model (deslop-selector--display-label
                 (or (deslop-selector--model-name) "(none available)") 24))
         (model-status
          (pcase deslop-selector--model-state
            (:loading " [loading]")
            (:error " [probe failed]")
            (_ ""))))
    (let ((inhibit-read-only t))
       (erase-buffer)
       (insert (format "Provider:     %s(n/p)\n" provider))
       (insert (format "Model:        %s%s(C-n / C-p)\n"
                       model model-status))
      (insert (format "Concurrency:  %-24d(c/C)\n"
                      (deslop-selector--provider-concurrency)))
      (insert (format "Parallel:     %-24s(P)\n\n"
                      (if (deslop-selector--provider-parallel-p)
                          "on"
                        "off")))
      (insert "i  - insert\n"
              "I  - insert anywhere\n"
              "w  - write anywhere\n"
              "m  - select/search model\n"
              "s  - summarize\n"
              "r  - rubber duck\n\n"
               "RET       - confirm choice\n"
               "C-c C-c   - approve all LLM changes\n"
               "C-c C-k   - reject all LLM changes\n"
               "q/ESC     - close\n")
      (goto-char (point-min)))))

(defun deslop-selector--cancel-probe ()
  "Cancel the current model probe, if any."
  (let ((cancel deslop-selector--probe-cancel))
    (setq deslop-selector--probe-cancel nil
          deslop-selector--probe-generation
          (cl-incf deslop-selector--next-probe-generation))
    (when (functionp cancel)
      (deslop-ignore-errors cancel))))

(defun deslop-selector--set-models (models &optional preferred)
  "Set the current provider's MODELS and select PREFERRED when available."
  (let* ((preferred (or preferred
                        (deslop-selector--model-name)
                        (plist-get deslop-selector--last-selection :model)))
         (models (cl-remove-if-not #'stringp models)))
    (setq deslop-selector--models models
          deslop-selector--model-index
          (or (deslop-selector--index-of preferred models) 0))))

(defun deslop-selector--probe-current-provider (&optional preferred)
  "Probe the selected provider when it has dynamic model discovery.
PREFERRED is the model selected before discovery started."
  (let* ((provider (deslop-selector--provider-name))
         (profile (deslop-selector--provider-profile))
         (probe (plist-get profile :model-discovery-function))
         (generic-probe (and (null probe)
                              (deslop-provider-models-url profile)))
         (selector-buffer (current-buffer))
         (generation (cl-incf deslop-selector--next-probe-generation))
         (completed nil))
    (setq deslop-selector--probe-generation generation)
    (when (or (functionp probe) generic-probe)
      (setq deslop-selector--model-state :loading)
      (deslop-selector--render)
      (let ((callback
             (lambda (models error)
               (when (and (buffer-live-p selector-buffer)
                          (with-current-buffer selector-buffer
                            (= deslop-selector--probe-generation generation)))
                 (setq completed t)
                 (with-current-buffer selector-buffer
                   (when (eq (deslop-selector--provider-name) provider)
                     (setq deslop-selector--probe-cancel nil)
                     (if error
                         (setq deslop-selector--model-state :error)
                       (deslop-selector--set-models
                        models (or preferred
                                   (deslop-selector--model-name)))
                       (setq deslop-selector--model-details
                             (deslop-provider-model-details profile))
                       (setq deslop-selector--model-state :ready))
                     (deslop-selector--render)))))))
        (let ((cancel (if (functionp probe)
                          (funcall probe callback)
                        (deslop-provider-probe-models profile callback))))
          (when (and (not completed)
                     (= deslop-selector--probe-generation generation))
            (setq deslop-selector--probe-cancel cancel)))))))

(defun deslop-selector--set-provider (index)
  "Select provider at INDEX and start dynamic model discovery if needed."
  (deslop-selector--cancel-probe)
  (setq deslop-selector--provider-index
        (mod index (max 1 (length deslop-selector--providers))))
  (let* ((provider (deslop-selector--provider-name))
         (profile (deslop-selector--provider-profile))
         (last-provider (plist-get deslop-selector--last-selection :provider))
         (preferred (and (eq provider last-provider)
                         (plist-get deslop-selector--last-selection :model))))
    (deslop-selector--set-models
     (deslop-provider-models profile)
     (or preferred (plist-get profile :model)))
    (setq deslop-selector--model-details
          (deslop-provider-model-details profile))
    (setq deslop-selector--model-state :ready)
    (deslop-selector--render)
    (deslop-selector--probe-current-provider
     (or preferred (deslop-selector--model-name)))))

(defun deslop-selector-next-provider ()
  "Select the next configured provider."
  (interactive)
  (deslop-selector--set-provider (1+ deslop-selector--provider-index)))

(defun deslop-selector-previous-provider ()
  "Select the previous configured provider."
  (interactive)
  (deslop-selector--set-provider (1- deslop-selector--provider-index)))

(defun deslop-selector--move-model (delta)
  "Move the selected model by DELTA."
  (if (null deslop-selector--models)
      (message "deslop: no models available")
    (setq deslop-selector--model-index
          (mod (+ deslop-selector--model-index delta)
               (length deslop-selector--models)))
    (deslop-selector--render)))

(defun deslop-selector-next-model ()
  "Select the next model for the current provider."
  (interactive)
  (deslop-selector--move-model 1))

(defun deslop-selector-previous-model ()
  "Select the previous model for the current provider."
  (interactive)
  (deslop-selector--move-model -1))

(defun deslop-selector-select-model ()
  "Select a model for the current provider using completion.

The model list is already populated by provider discovery.  Completion is
performed locally so typing a search term never starts another network request."
  (interactive)
  (pcase deslop-selector--model-state
    (:loading
     (user-error "deslop model discovery is still loading"))
    (:error
     (user-error "deslop model discovery failed; change provider or retry"))
    (_
     (let ((models (cl-remove-if-not #'stringp deslop-selector--models)))
       (unless models
         (user-error "deslop has no models available for this provider"))
        (let* ((candidates (deslop-selector--model-candidates))
               (candidate-names (mapcar #'car candidates))
               (current (deslop-selector--candidate-for-model
                         (deslop-selector--model-name) candidates))
               (details deslop-selector--model-details)
               (completion-extra-properties
                (list
                 :affixation-function
                 (lambda (completions)
                   (deslop-selector--model-affixations
                    completions candidates details))))
               (completion-ignore-case t)
               (completion-styles '(substring))
               (completions-format 'vertical)
               (choice (completing-read "Model: " candidate-names
                                        nil t nil nil current))
               (selected-model
                (cdr (cl-find choice candidates
                              :key #'car :test #'string=)))
               (index (cl-position selected-model models :test #'equal)))
          (when (and selected-model index)
            (setq deslop-selector--models models
                  deslop-selector--model-index index)
            (deslop-selector--save-selection)
            (deslop-selector--render)))))))

(defun deslop-selector-set-concurrency ()
  "Set and save the selected provider's session concurrency limit."
  (interactive)
  (let* ((provider (deslop-selector--provider-name))
         (current (deslop-selector--provider-concurrency))
         (value (read-number "Max concurrent jobs: " current)))
    (deslop-provider-set-runtime-setting provider :max-concurrency value)
    (deslop-selector--render)
    (deslop-process-next)))

(defun deslop-selector-toggle-parallel ()
  "Toggle and save the selected provider's default parallel setting."
  (interactive)
  (let* ((provider (deslop-selector--provider-name))
         (enabled (not (deslop-selector--provider-parallel-p))))
    (deslop-provider-set-runtime-setting provider :parallel enabled)
     (deslop-selector--render)
     (deslop-process-next)))

(defun deslop-selector--review-source (function)
  "Call review FUNCTION for the selector's source buffer and close selector."
  (let ((source deslop-selector--source-buffer))
    (unless (buffer-live-p source)
      (user-error "Source buffer no longer exists"))
    (with-current-buffer source
      (funcall function source))
    (deslop-selector-close)))

(defun deslop-selector-approve-source-output ()
  "Approve all generated output in the selector's source buffer."
  (interactive)
  (deslop-selector--review-source #'deslop-approve-all-output))

(defun deslop-selector-reject-source-output ()
  "Reject all generated output in the selector's source buffer."
  (interactive)
  (deslop-selector--review-source #'deslop-reject-all-output))

(defun deslop-selector--save-selection ()
  "Save the current provider/model choice."
  (setq deslop-selector--last-selection
        (list :provider (deslop-selector--provider-name)
              :model (deslop-selector--model-name))))

(defun deslop-selector-close ()
  "Close the selector without starting a request."
  (interactive)
  (deslop-selector--cancel-probe)
  (let ((buffer (current-buffer))
        (window (or (and (window-live-p deslop-selector--window)
                         deslop-selector--window)
                    (get-buffer-window (current-buffer) t))))
    (setq deslop-selector--window nil)
    (when (window-live-p window)
      (quit-window t window))
     (when (buffer-live-p buffer)
       (kill-buffer buffer))))

(defun deslop-selector--buffer-killed ()
  "Cancel a selector probe when its buffer is killed externally."
  (deslop-selector--cancel-probe))

(defun deslop-selector-confirm ()
  "Confirm the current choice and close the selector."
  (interactive)
  (deslop-selector--save-selection)
  (deslop-selector-close))

(defun deslop-selector--run (type &optional instruction)
  "Run TYPE from the source buffer using the selected provider/model."
  (let ((source deslop-selector--source-buffer)
        (profile (deslop-provider-profile-snapshot
                  (deslop-selector--provider-name)))
        (model (deslop-selector--model-name)))
    (unless (buffer-live-p source)
      (user-error "deslop source buffer no longer exists"))
    (unless (stringp model)
      (user-error "deslop has no selected model"))
    (setq profile (plist-put profile :model model))
    (deslop-selector--save-selection)
    (deslop-selector-close)
    (with-current-buffer source
      (if (eq type 'rubber-duck)
          (deslop-rubber-duck instruction profile)
        (deslop-enqueue-operation type instruction profile)))))

(defun deslop-selector-insert ()
  "Run insert using the current selector choice."
  (interactive)
  (deslop-selector--run 'insert))

(defun deslop-selector-write-anywhere ()
  "Run a structured multi-edit using the current selector choice."
  (interactive)
  (deslop-selector--run
   'write-anywhere
   (read-string "Prompt: ")))

(defun deslop-selector-insert-anywhere ()
  "Run a structured multi-insertion using the current selector choice."
  (interactive)
  (deslop-selector--run
   'insert-anywhere
   (read-string "Prompt: ")))

(defun deslop-selector-improve ()
  "Prompt for an improvement and run it using the current choice."
  (interactive)
  (deslop-selector--run
   'improve (read-string "How should the text be improved? ")))

(defun deslop-selector-summarize ()
  "Run summarize using the current selector choice."
  (interactive)
  (deslop-selector--run 'summarize))

(defun deslop-selector-rubber-duck ()
  "Capture an explanation exercise using the current selector choice."
  (interactive)
  (deslop-selector--run 'rubber-duck (deslop-rubber-duck-read-level)))

(defvar deslop-selector-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    map)
  "Keymap for `deslop-selector-mode'.")

(defun deslop-selector--install-keybindings ()
  "Install selector bindings, including when this file is reloaded."
  (define-key deslop-selector-mode-map (kbd "n") #'deslop-selector-next-provider)
  (define-key deslop-selector-mode-map (kbd "p") #'deslop-selector-previous-provider)
  (define-key deslop-selector-mode-map (kbd "C-n") #'deslop-selector-next-model)
  (define-key deslop-selector-mode-map (kbd "C-p") #'deslop-selector-previous-model)
  (define-key deslop-selector-mode-map (kbd "c") #'deslop-selector-set-concurrency)
  (define-key deslop-selector-mode-map (kbd "C") #'deslop-selector-set-concurrency)
  (define-key deslop-selector-mode-map (kbd "P") #'deslop-selector-toggle-parallel)
  (define-key deslop-selector-mode-map (kbd "i") #'deslop-selector-insert)
  (define-key deslop-selector-mode-map (kbd "w") #'deslop-selector-write-anywhere)
  (define-key deslop-selector-mode-map (kbd "I") #'deslop-selector-insert-anywhere)
  (define-key deslop-selector-mode-map (kbd "m") #'deslop-selector-select-model)
  (define-key deslop-selector-mode-map (kbd "s") #'deslop-selector-summarize)
  (define-key deslop-selector-mode-map (kbd "r") #'deslop-selector-rubber-duck)
  (define-key deslop-selector-mode-map (kbd "RET") #'deslop-selector-confirm)
  (define-key deslop-selector-mode-map (kbd "C-c C-c")
              #'deslop-selector-approve-source-output)
  (define-key deslop-selector-mode-map (kbd "C-c C-k")
              #'deslop-selector-reject-source-output)
  (define-key deslop-selector-mode-map (kbd "DEL") nil)
  (define-key deslop-selector-mode-map (kbd "<backspace>") nil)
  (define-key deslop-selector-mode-map (kbd "q") #'deslop-selector-close)
  (define-key deslop-selector-mode-map (kbd "ESC") #'deslop-selector-close))

(deslop-selector--install-keybindings)

(define-derived-mode deslop-selector-mode special-mode "deslop-selector"
  "Major mode for selecting a deslop provider and model."
  (deslop-selector--install-keybindings)
  (add-hook 'kill-buffer-hook #'deslop-selector--buffer-killed nil t)
  (setq truncate-lines t)
  (setq buffer-read-only t))

;;;###autoload
(defun deslop-selector-open ()
  "Open the provider/model selector for the current source buffer."
  (interactive)
  (let* ((source (current-buffer))
         (providers (deslop-provider-names))
         (last-provider (plist-get deslop-selector--last-selection :provider))
         (provider-index (or (cl-position last-provider providers :test #'eq) 0))
         (buffer (get-buffer-create deslop-selector-buffer-name)))
    (with-current-buffer buffer
      (deslop-selector--cancel-probe)
      (deslop-selector-mode)
      (setq deslop-selector--source-buffer source
            deslop-selector--providers providers
            deslop-selector--provider-index provider-index
            deslop-selector--models nil
            deslop-selector--model-details nil
            deslop-selector--model-index 0
            deslop-selector--model-state :ready)
      (deslop-selector--set-provider provider-index))
    (let ((window
           (display-buffer
            buffer
              `((display-buffer-at-bottom)
                (window-height . deslop-selector--fit-window)
                (window-min-height . ,deslop-selector-window-min-height)))))
      (with-current-buffer buffer
        (setq deslop-selector--window window))
      (when (window-live-p window)
        (deslop-selector--fit-window window)
        (select-window window)))))

(provide 'deslop-selector)

;;; deslop-selector.el ends here
