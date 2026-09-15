;;; deslop.el --- Local-LLM text assistance for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Version: 0.3
;; Keywords: convenience, tools, llm
;; Package-Requires: ((emacs "31.1"))

;;; Commentary:

;; deslop is a minor mode that queues requests to an OpenAI-compatible LLM.
;;
;;   C-c e   - deslop-selector-open

;;; Code:

(require 'cl-lib)
(require 'deslop-core)
(require 'deslop-operation)
(require 'deslop-provider)
(require 'deslop-review)
(require 'deslop-project)
(require 'deslop-target)
(require 'deslop-ui)
(require 'deslop-llm)
(require 'deslop-queue)
(require 'deslop-tools)
(require 'deslop-operations)
(require 'deslop-commands)
(require 'deslop-history)
(require 'deslop-selector)

;;;; Keymap and Minor Mode

(defun deslop--make-mode-map ()
  "Build the keymap for `deslop-mode'."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c e") #'deslop-selector-open)
    map))

(defvar deslop-mode-map (deslop--make-mode-map) "Keymap for `deslop-mode'.")

(defvar-local deslop--mode-line-string " deslop[0]"
  "Mode-line lighter showing jobs and pending reviews for the current buffer.")

(defun deslop--job-targets-buffer-p (job buffer)
  "Return non-nil when active or queued JOB targets BUFFER."
  (and (memq (deslop-job-state job) '(queued running))
       (eq (deslop-target-buffer (deslop-job-target job)) buffer)))

(defun deslop--buffer-job-count (buffer)
  "Return the number of active or queued jobs targeting BUFFER."
  (cl-count-if
   (lambda (job) (deslop--job-targets-buffer-p job buffer))
    (append deslop--active-jobs deslop-queue)))

(defun deslop--buffer-review-count (buffer)
  "Return the number of pending reviews in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (cl-count-if #'deslop-review-overlay-p
                     (overlays-in (point-min) (point-max)))))))

(defun deslop--update-mode-line (&optional buffer)
  "Update the deslop lighter for BUFFER, or the current buffer."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((jobs (deslop--buffer-job-count buffer))
              (reviews (deslop--buffer-review-count buffer)))
          (setq deslop--mode-line-string
                (if (> reviews 0)
                    (format " deslop[%d|%d]" jobs reviews)
                  (format " deslop[%d]" jobs))))
        (force-mode-line-update)))))

(defun deslop--mode-line-job-event (job _event)
  "Refresh the mode-line count for JOB's target buffer."
  (deslop--update-mode-line
   (deslop-target-buffer (deslop-job-target job))))

(defun deslop--mode-line-review-event (buffer)
  "Refresh the mode-line count for reviews in BUFFER."
  (deslop--update-mode-line buffer))

(add-hook 'deslop-queue-abort-functions #'deslop-provider-abort-discovery)
(add-hook 'deslop-queue-abort-functions #'deslop-ui-clear-all)
(add-hook 'deslop-job-event-functions #'deslop-ui-job-event)
(add-hook 'deslop-job-event-functions #'deslop--mode-line-job-event)
(add-hook 'deslop-review-change-functions #'deslop--mode-line-review-event)

;;;###autoload
(define-minor-mode deslop-mode
  "Minor mode for queueing LLM requests."
  :lighter deslop--mode-line-string
  :keymap deslop-mode-map
  (deslop--update-mode-line))

;;;###autoload
(defun deslop-reload ()
  "Abort active jobs and reload all deslop files in dependency order."
  (interactive)
  (deslop-abort-queue)
  (let* ((dir (or (file-name-directory (locate-library "deslop.el"))
                  (user-error "deslop.el not found on load-path")))
          (files '("deslop-core.el" "deslop-operation.el" "deslop-protocol.el"
                   "deslop-transport.el" "deslop-curl.el" "deslop-provider.el"
                   "deslop-codex.el"
                  "deslop-review.el" "deslop-project.el" "deslop-target.el"
                  "deslop-ui.el"
                  "deslop-llm.el" "deslop-queue.el" "deslop-edit.el"
                  "deslop-tools.el"
                  "deslop-summary.el" "deslop-operations.el"
                  "deslop-commands.el" "deslop-history.el"
                  "deslop-selector.el" "deslop.el")))
    (dolist (f files) (load (expand-file-name f dir)))
    (setq deslop-mode-map (deslop--make-mode-map))
    (message "deslop: reloaded from %s" dir)))

(provide 'deslop)

;;; deslop.el ends here
