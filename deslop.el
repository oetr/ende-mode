;;; deslop.el --- Local-LLM text assistance for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Version: 0.2
;; Keywords: convenience, tools, llm
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; deslop is a minor mode that queues requests to a local LLM served by
;; any OpenAI-compatible chat completions endpoint (llama.cpp server,
;; OpenRouter, Ollama, LM Studio).
;;
;;   C-c e i - deslop-insert-at-cursor : insert LLM completion at cursor
;;   C-c e a - deslop-abort-queue      : cancel active request and clear queue
;;
;; Features:
;; - Real-time visual progress overlays with spinner animation
;; - Live token counts and tokens-per-second measurement
;; - Robust SSE stream parsing with reasoning/thinking trace separation
;; - FIFO queue with process cancellation on abort
;; - Context-aware cursor placement before, inside, or after selected regions

;;; Code:

(require 'deslop-common)
(require 'deslop-ui)
(require 'deslop-llm)
(require 'deslop-queue)

;;;; Job Handler: Insert at Cursor

(defun deslop--run-insert (p)
  "Execute an insert job described by plist P."
  (let* ((buf (plist-get p :buffer))
         (ov (plist-get p :overlay))
         (selected (plist-get p :selected))
         (offset (plist-get p :offset)))
    (with-current-buffer buf
      (let* ((pos (if (and (overlayp ov) (overlay-buffer ov))
                      (overlay-start ov)
                    (point)))
             (ov-end (if (and (overlayp ov) (overlay-buffer ov))
                         (overlay-end ov)
                       pos))
             (region-text (if selected
                              (buffer-substring-no-properties pos ov-end)
                            ""))
             (reg-len (length region-text))
             (clamped-offset (max 0 (min reg-len (or offset 0))))
             (context (if selected
                          (deslop-marker-context region-text clamped-offset)
                        (let ((ctx (buffer-substring-no-properties (point-min) (point-max))))
                          (deslop-marker-context ctx (- pos (point-min))))))
             (messages (list (cons "system" deslop-insert-system)
                             (cons "user" (format "Task: %s\n\nDocument:\n%s"
                                                  deslop-insert-instruction context))))
             (callbacks
              (list
               :on-start (lambda (proc)
                           (when deslop--current-job
                             (plist-put (cdr deslop--current-job) :process proc)))
               :on-status (lambda (status)
                            (when ov
                              (deslop-ui-update-overlay ov :status status)))
               :on-usage (lambda (tokens speed)
                           (when ov
                             (deslop-ui-update-overlay ov :tokens tokens :speed speed)))
               :on-done (lambda (content)
                          (if (null content)
                              (deslop--skip "insert: empty or null content")
                            (let ((text (deslop-trim-trailing content)))
                              (with-current-buffer buf
                                (save-excursion
                                  (let* ((live-start (if (and (overlayp ov) (overlay-buffer ov))
                                                         (overlay-start ov)
                                                       pos))
                                         (live-end (if (and (overlayp ov) (overlay-buffer ov))
                                                       (overlay-end ov)
                                                     ov-end)))
                                    (cond
                                     ((null selected)
                                      (goto-char live-start)
                                      (insert text))
                                     ((<= offset 0)
                                      (goto-char live-start)
                                      (insert text "\n"))
                                     ((>= offset reg-len)
                                      (goto-char live-end)
                                      (insert "\n" text))
                                     (t
                                      (goto-char (+ live-start clamped-offset))
                                      (insert "\n" text "\n"))))))
                              (message "deslop: inserted %d chars in %s"
                                       (length text) (buffer-name buf))
                              (deslop-job-done))))
               :on-error (lambda (err)
                           (deslop--skip (format "insert LLM error: %s" err))))))
        (when ov
          (deslop-ui-update-overlay ov :status "Thinking"))
        (funcall deslop-llm-function messages callbacks)))))

(deslop-register-job-handler 'insert #'deslop--run-insert)

;;;; Interactive Commands

(defun deslop-insert-at-cursor ()
  "Queue a request to insert LLM text at cursor position.
If a region is highlighted, only that region is sent as context and
output is placed relative to cursor (before, inside, or after region).
If no region is active, full buffer is sent with a <CURSOR> marker."
  (interactive)
  (let* ((buf (current-buffer))
         (region (deslop-current-region))
         (pt (point))
         (ov (if region
                 (deslop-ui-create-overlay buf (car region) (cdr region) "Queued")
               (deslop-ui-create-overlay buf pt pt "Queued"))))
    (if region
        (let* ((s (car region))
               (offset (- pt s)))
          (deslop-enqueue
           (list 'insert :buffer buf
                         :selected t
                         :offset offset
                         :overlay ov)))
      (deslop-enqueue
       (list 'insert :buffer buf
                     :selected nil
                     :offset (- pt (point-min))
                     :overlay ov)))
    (message "deslop: queued insert-at-cursor")))

;;;; Keymap and Minor Mode

(defun deslop--make-mode-map ()
  "Build the keymap for `deslop-mode'."
  (let ((map (make-sparse-keymap))
        (prefix (make-sparse-keymap)))
    (define-key prefix "i" #'deslop-insert-at-cursor)
    (define-key prefix "a" #'deslop-abort-queue)
    (define-key map (kbd "C-c e") prefix)
    map))

(defvar deslop-mode-map
  (deslop--make-mode-map)
  "Keymap for `deslop-mode'.")

;;;###autoload
(define-minor-mode deslop-mode
  "Minor mode for queueing local LLM requests."
  :lighter " deslop"
  :keymap deslop-mode-map)

;;;###autoload
(defun deslop-reload ()
  "Abort active jobs and reload all deslop files in dependency order.
Save all deslop buffers before calling this.
Note: `defcustom' values you have set (via customize or your init)
survive reloads by design; reset them to pick up new file defaults."
  (interactive)
  (deslop-abort-queue)
  (let* ((dir (or (file-name-directory (locate-library "deslop.el"))
                  (user-error "deslop.el not found on load-path")))
         (files '("deslop-common.el" "deslop-ui.el" "deslop-llm.el"
                  "deslop-queue.el" "deslop.el")))
    (dolist (f files)
      (load (expand-file-name f dir)))
    (setq deslop-mode-map (deslop--make-mode-map))
    (message "deslop: reloaded from %s" dir)))

(provide 'deslop)

;;; deslop.el ends here
