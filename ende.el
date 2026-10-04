;;; ende.el --- Local-LLM text assistance for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Version: 0.3
;; Keywords: convenience, tools, llm
;; Package-Requires: ((emacs "31.1"))

;;; Commentary:

;; ende is a minor mode that queues requests to an OpenAI-compatible LLM.
;;
;;   C-c e   - ende-selector-open
;;
;; TRAMP source buffers are supported: constructed request bodies are sent to
;; local provider processes.  Generation, provider discovery, Codex model
;; probing, and OAuth refresh always run curl locally, never on the source
;; buffer's remote host.  Provider processes and their header/config files use
;; `temporary-file-directory', which must be an existing absolute local
;; directory; remote or relative values are rejected.  Refreshed Codex
;; credentials must also be stored in a local file.

;;; Code:

(require 'cl-lib)
(require 'ende-core)
(require 'ende-operation)
(require 'ende-provider)
(require 'ende-review)
(require 'ende-project)
(require 'ende-url)
(require 'ende-target)
(require 'ende-ui)
(require 'ende-llm)
(require 'ende-queue)
(require 'ende-tools)
(require 'ende-operations)
(require 'ende-chat)
(require 'ende-commands)
(require 'ende-rubber-duck)
(require 'ende-history)
(require 'ende-selector)

;;;; Keymap and Minor Mode

(defun ende--make-mode-map ()
  "Build the keymap for `ende-mode'."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c e") #'ende-selector-open)
    map))

(defvar ende-mode-map (ende--make-mode-map) "Keymap for `ende-mode'.")

(defvar-local ende--mode-line-string " ende[0]"
  "Mode-line lighter showing jobs and pending reviews for the current buffer.")

(defun ende--job-targets-buffer-p (job buffer)
  "Return non-nil when active or queued JOB targets BUFFER."
  (and (memq (ende-job-state job) '(queued running))
       (eq (ende-target-buffer (ende-job-target job)) buffer)))

(defun ende--buffer-job-count (buffer)
  "Return the number of active or queued jobs targeting BUFFER."
  (cl-count-if
   (lambda (job) (ende--job-targets-buffer-p job buffer))
   (append (ende-active-jobs) (ende-pending-jobs))))

(defun ende--buffer-review-count (buffer)
  "Return the number of pending reviews in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (cl-count-if #'ende-review-overlay-p
                     (overlays-in (point-min) (point-max)))))))

(defun ende--update-mode-line (&optional buffer)
  "Update the ende lighter for BUFFER, or the current buffer."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((jobs (ende--buffer-job-count buffer))
              (reviews (ende--buffer-review-count buffer)))
          (setq ende--mode-line-string
                (if (> reviews 0)
                    (format " ende[%d|%d]" jobs reviews)
                  (format " ende[%d]" jobs))))
        (force-mode-line-update)))))

(defun ende--mode-line-job-event (job _event)
  "Refresh the mode-line count for JOB's target buffer."
  (ende--update-mode-line
   (ende-target-buffer (ende-job-target job))))

(defun ende--mode-line-review-event (buffer)
  "Refresh the mode-line count for reviews in BUFFER."
  (ende--update-mode-line buffer))

(add-hook 'ende-queue-abort-functions #'ende-provider-abort-discovery)
(add-hook 'ende-queue-abort-functions #'ende-openrouter-batch-abort)
(add-hook 'ende-queue-abort-functions #'ende-ui-clear-all)
(add-hook 'ende-job-event-functions #'ende-ui-job-event)
(add-hook 'ende-job-event-functions #'ende--mode-line-job-event)
(add-hook 'ende-review-change-functions #'ende--mode-line-review-event)

;;;###autoload
(define-minor-mode ende-mode
  "Minor mode for queueing LLM requests."
  :lighter ende--mode-line-string
  :keymap ende-mode-map
  (when ende-mode
    (ende-model-selection-initialize-buffer))
  (ende--update-mode-line))

;;;###autoload
(defun ende-reload ()
  "Abort active jobs and reload all ende files in dependency order."
  (interactive)
  (ende-abort-queue)
  (let* ((dir (or (file-name-directory (locate-library "ende.el"))
                  (user-error "ende.el not found on load-path")))
         (files '("ende-core.el" "ende-operation.el" "ende-protocol.el"
                  "ende-transport.el" "ende-curl.el" "ende-provider.el"
                  "ende-codex.el"
                  "ende-review.el" "ende-project.el" "ende-url.el"
                  "ende-target.el" "ende-ui.el"
                  "ende-openrouter-batch.el" "ende-llm.el"
                  "ende-queue.el" "ende-edit.el" "ende-tools.el"
                  "ende-chat-config.el"
                  "ende-summary.el" "ende-operations.el"
                  "ende-chat.el" "ende-commands.el" "ende-rubber-duck.el"
                  "ende-history.el" "ende-selector.el" "ende.el")))
    (dolist (f files) (load (expand-file-name f dir)))
    (setq ende-mode-map (ende--make-mode-map))
    (message "ende: reloaded from %s" dir)))

(provide 'ende)

;;; ende.el ends here
