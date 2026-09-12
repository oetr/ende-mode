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
(require 'deslop-provider)
(require 'deslop-review)
(require 'deslop-target)
(require 'deslop-ui)
(require 'deslop-llm)
(require 'deslop-queue)
(require 'deslop-operations)
(require 'deslop-history)
(require 'deslop-selector)

;;;; Keymap and Minor Mode

(defun deslop--make-mode-map ()
  "Build the keymap for `deslop-mode'."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c e") #'deslop-selector-open)
    map))

(defvar deslop-mode-map (deslop--make-mode-map) "Keymap for `deslop-mode'.")

(add-hook 'deslop-queue-abort-functions #'deslop-provider-abort-discovery)
(add-hook 'deslop-queue-abort-functions #'deslop-ui-clear-all)

;;;###autoload
(define-minor-mode deslop-mode
  "Minor mode for queueing LLM requests."
  :lighter " deslop"
  :keymap deslop-mode-map)

;;;###autoload
(defun deslop-reload ()
  "Abort active jobs and reload all deslop files in dependency order."
  (interactive)
  (deslop-abort-queue)
  (let* ((dir (or (file-name-directory (locate-library "deslop.el"))
                  (user-error "deslop.el not found on load-path")))
          (files '("deslop-core.el" "deslop-protocol.el" "deslop-curl.el"
                    "deslop-provider.el" "deslop-codex.el" "deslop-review.el" "deslop-target.el"
                   "deslop-ui.el"
                    "deslop-llm.el" "deslop-queue.el"
                    "deslop-operations.el" "deslop-history.el" "deslop-selector.el"
                    "deslop.el")))
     (dolist (f files) (load (expand-file-name f dir)))
    (setq deslop-mode-map (deslop--make-mode-map))
    (message "deslop: reloaded from %s" dir)))

(provide 'deslop)

;;; deslop.el ends here
