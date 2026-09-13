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

(defcustom deslop-selector-buffer-name "*deslop-selector*"
  "Buffer used by the provider and model selector."
  :type 'string
  :group 'deslop)

(defcustom deslop-selector-window-height 0.5
  "Fraction of the frame height used by the bottom selector window."
  :type 'number
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

(defvar-local deslop-selector--model-index 0
  "Index of the selected model in the current selector.")

(defvar-local deslop-selector--model-state nil
  "State of model discovery for the selected provider.")

(defvar-local deslop-selector--probe-cancel nil
  "Cancellation function for the current model probe.")

(defvar-local deslop-selector--window nil
  "Window displaying the current selector buffer.")

(defun deslop-selector--provider-name ()
  "Return the selected provider name."
  (nth deslop-selector--provider-index deslop-selector--providers))

(defun deslop-selector--provider-profile ()
  "Return the selected provider profile."
  (deslop-provider-profile (deslop-selector--provider-name)))

(defun deslop-selector--model-name ()
  "Return the selected model name, or nil while discovery is unavailable."
  (nth deslop-selector--model-index deslop-selector--models))

(defun deslop-selector--index-of (value values)
  "Return the index of VALUE in VALUES, or nil."
  (cl-position value values :test #'equal))

(defun deslop-selector--display-provider ()
  "Return a display name for the selected provider."
  (deslop-provider-client-name (deslop-selector--provider-profile)))

(defun deslop-selector--provider-concurrency ()
  "Return the selected provider's current concurrency limit."
  (deslop-provider-max-concurrency (deslop-selector--provider-profile)))

(defun deslop-selector--provider-parallel-p ()
  "Return whether the selected provider defaults to parallel jobs."
  (deslop-provider-parallel-enabled-p (deslop-selector--provider-profile)))

(defun deslop-selector--render ()
  "Render the current selector state."
  (let* ((provider (deslop-selector--display-provider))
         (model (deslop-selector--model-name))
         (model-status
          (pcase deslop-selector--model-state
            (:loading " [loading]")
            (:error " [probe failed]")
            (_ ""))))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "Provider:     %-24s(p/n)\n" provider))
      (insert (format "Model:        %-24s(C-n / C-p)\n"
                      (concat (or model "(none available)") model-status)))
      (insert (format "Concurrency:  %-24d(c/C)\n"
                      (deslop-selector--provider-concurrency)))
      (insert (format "Parallel:     %-24s(P)\n\n"
                      (if (deslop-selector--provider-parallel-p)
                          "on"
                        "off")))
      (insert "i  - insert\n"
              "I  - insert anywhere\n"
              "w  - write anywhere\n"
              "m  - improve\n"
              "s  - summarize\n\n"
              "RET       - approve all LLM changes\n"
              "BACKSPACE - reject all LLM changes\n"
              "q/ESC     - close\n")
      (goto-char (point-min)))))

(defun deslop-selector--cancel-probe ()
  "Cancel the current model probe, if any."
  (when deslop-selector--probe-cancel
    (funcall deslop-selector--probe-cancel)
    (setq deslop-selector--probe-cancel nil)))

(defun deslop-selector--set-models (models &optional preferred)
  "Set the current provider's MODELS and select PREFERRED when available."
  (let* ((preferred (or preferred
                        (deslop-selector--model-name)
                        (plist-get deslop-selector--last-selection :model)))
         (models (cl-remove-if-not #'stringp models)))
    (setq deslop-selector--models models
          deslop-selector--model-index
          (or (deslop-selector--index-of preferred models) 0))))

(defun deslop-selector--probe-current-provider ()
  "Probe the selected provider when it has dynamic model discovery."
  (let* ((provider (deslop-selector--provider-name))
         (profile (deslop-selector--provider-profile))
         (probe (plist-get profile :model-discovery-function))
         (selector-buffer (current-buffer)))
    (when (functionp probe)
      (setq deslop-selector--model-state :loading)
      (deslop-selector--render)
      (setq deslop-selector--probe-cancel
            (funcall probe
             (lambda (models error)
               (when (buffer-live-p selector-buffer)
                 (with-current-buffer selector-buffer
                   (when (eq (deslop-selector--provider-name) provider)
                     (setq deslop-selector--probe-cancel nil)
                     (if error
                         (setq deslop-selector--model-state :error)
                       (deslop-selector--set-models
                        models (deslop-selector--model-name))
                       (setq deslop-selector--model-state :ready))
                     (deslop-selector--render))))))))))

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
     (or preferred (plist-get profile :model))))
  (setq deslop-selector--model-state :ready)
  (deslop-selector--render)
  (deslop-selector--probe-current-provider))

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
       (deslop-enqueue-operation type instruction profile))))

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

(defvar deslop-selector-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    map)
  "Keymap for `deslop-selector-mode'.")

(defun deslop-selector--install-keybindings ()
  "Install selector bindings, including when this file is reloaded."
  (define-key deslop-selector-mode-map (kbd "p") #'deslop-selector-next-provider)
  (define-key deslop-selector-mode-map (kbd "n") #'deslop-selector-previous-provider)
  (define-key deslop-selector-mode-map (kbd "C-n") #'deslop-selector-next-model)
  (define-key deslop-selector-mode-map (kbd "C-p") #'deslop-selector-previous-model)
  (define-key deslop-selector-mode-map (kbd "c") #'deslop-selector-set-concurrency)
  (define-key deslop-selector-mode-map (kbd "C") #'deslop-selector-set-concurrency)
  (define-key deslop-selector-mode-map (kbd "P") #'deslop-selector-toggle-parallel)
  (define-key deslop-selector-mode-map (kbd "i") #'deslop-selector-insert)
  (define-key deslop-selector-mode-map (kbd "w") #'deslop-selector-write-anywhere)
  (define-key deslop-selector-mode-map (kbd "I") #'deslop-selector-insert-anywhere)
  (define-key deslop-selector-mode-map (kbd "m") #'deslop-selector-improve)
  (define-key deslop-selector-mode-map (kbd "s") #'deslop-selector-summarize)
  (define-key deslop-selector-mode-map (kbd "RET") #'deslop-selector-approve-source-output)
  (define-key deslop-selector-mode-map (kbd "DEL") #'deslop-selector-reject-source-output)
  (define-key deslop-selector-mode-map (kbd "<backspace>") #'deslop-selector-reject-source-output)
  (define-key deslop-selector-mode-map (kbd "q") #'deslop-selector-close)
  (define-key deslop-selector-mode-map (kbd "ESC") #'deslop-selector-close))

(deslop-selector--install-keybindings)

(define-derived-mode deslop-selector-mode special-mode "deslop-selector"
  "Major mode for selecting a deslop provider and model."
  (deslop-selector--install-keybindings)
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
      (deslop-selector-mode)
      (setq deslop-selector--source-buffer source
            deslop-selector--providers providers
            deslop-selector--provider-index provider-index
            deslop-selector--models nil
            deslop-selector--model-index 0
            deslop-selector--model-state :ready)
      (deslop-selector--set-provider provider-index))
    (let ((window
           (display-buffer
            buffer
              `((display-buffer-at-bottom)
                (window-height . ,deslop-selector-window-height)
                (window-min-height . 18)))))
      (with-current-buffer buffer
        (setq deslop-selector--window window))
      (when (window-live-p window)
        (select-window window)))))

(provide 'deslop-selector)

;;; deslop-selector.el ends here
