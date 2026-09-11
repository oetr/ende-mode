;;; deslop-llm.el --- LLM streaming client and parser for deslop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools

;;; Commentary:

;; Asynchronous SSE streaming client for OpenAI-compatible endpoints.
;; Handles delta parsing, reasoning/thinking extraction, and token metrics.

;;; Code:

(require 'json)
(require 'subr-x)
(require 'deslop-common)

;;;; Hookable LLM caller

(defvar deslop-llm-function #'deslop-llm-default
  "Function used to dispatch requests to the LLM.
Called with (MESSAGES CALLBACKS &optional OPTIONS).
CALLBACKS is a plist with keys :on-start, :on-status, :on-delta,
:on-usage, :on-done, and :on-error.")

;;;; Streaming Content & Thinking Parser

(defun deslop-llm--emit-delta (callbacks type text)
  "Invoke :on-delta in CALLBACKS with TYPE and TEXT."
  (let ((cb (plist-get callbacks :on-delta)))
    (when (and cb (> (length text) 0))
      (funcall cb type text))))

(defun deslop-llm--emit-status (callbacks status)
  "Invoke :on-status in CALLBACKS with STATUS."
  (let ((cb (plist-get callbacks :on-status)))
    (when cb
      (funcall cb status))))

(defun deslop-llm--process-content (content in-thinking callbacks)
  "Process CONTENT string according to current IN-THINKING state.
Returns the updated in-thinking state (t or nil) and the cleaned content string."
  (let ((accum-content "")
        (remaining content))
    (while (> (length remaining) 0)
      (if in-thinking
          (if (string-match "</think>" remaining)
              (let ((pre (substring remaining 0 (match-beginning 0)))
                    (post (substring remaining (match-end 0))))
                (deslop-llm--emit-delta callbacks :thinking pre)
                (setq in-thinking nil)
                (deslop-llm--emit-status callbacks "Generating")
                (setq remaining post))
            (deslop-llm--emit-delta callbacks :thinking remaining)
            (setq remaining ""))
        (if (string-match "<think>" remaining)
            (let ((pre (substring remaining 0 (match-beginning 0)))
                  (post (substring remaining (match-end 0))))
              (deslop-llm--emit-delta callbacks :content pre)
              (setq accum-content (concat accum-content pre))
              (setq in-thinking t)
              (deslop-llm--emit-status callbacks "Thinking")
              (setq remaining post))
          (deslop-llm--emit-delta callbacks :content remaining)
          (setq accum-content (concat accum-content remaining))
          (setq remaining ""))))
    (cons in-thinking accum-content)))

;;;; Client Implementation

(defun deslop-llm-default (messages callbacks &optional options)
  "Send MESSAGES asynchronously using curl SSE streaming.
CALLBACKS is a plist containing:
  :on-start  (FN PROC)
  :on-status (FN STATUS-STR)
  :on-delta  (FN TYPE TEXT)
  :on-usage  (FN TOKENS SPEED)
  :on-done   (FN ACCUMULATED-CONTENT)
  :on-error  (FN ERROR-MSG)
OPTIONS can override :url, :model, :temperature."
  (let* ((url (or (plist-get options :url) deslop-url))
         (model (or (plist-get options :model) deslop-model))
         (temp (or (plist-get options :temperature) deslop-temperature))
         (body `((:model . ,model)
                 (:messages . ,(mapcar (lambda (m)
                                         `((:role . ,(car m))
                                           (:content . ,(cdr m))))
                                       messages))
                 (:temperature . ,temp)
                 (:stream . t)
                 (:stream_options . ((:include_usage . t)))))
         (json-data (encode-coding-string (json-encode body) 'utf-8))
         (proc-buf (generate-new-buffer " *deslop-curl*"))
         (accumulated-content "")
         (in-thinking nil)
         (token-count 0)
         (start-time (float-time))
         (on-start (plist-get callbacks :on-start))
         (on-usage (plist-get callbacks :on-usage))
         (on-done (plist-get callbacks :on-done))
         (on-error (plist-get callbacks :on-error))
         (proc
          (make-process
           :name "deslop-curl"
           :buffer proc-buf
           :connection-type 'pipe
           :command (list "curl" "-s" "-N"
                          "-X" "POST" url
                          "-H" "Content-Type: application/json"
                          "-d" json-data)
           :filter
           (lambda (_p string)
             (when (buffer-live-p proc-buf)
               (with-current-buffer proc-buf
                 (goto-char (point-max))
                 (insert string)
                 (goto-char (point-min))
                 ;; Process all complete lines
                 (while (re-search-forward "^data: *\\(.*\\)\r?\n" nil t)
                   (let ((raw (match-string 1)))
                     (delete-region (point-min) (match-end 0))
                     (unless (string= (string-trim raw) "[DONE]")
                       (condition-case nil
                           (let* ((json (json-read-from-string raw))
                                  (choices (alist-get 'choices json))
                                  (delta (and choices (> (length choices) 0)
                                              (alist-get 'delta (aref choices 0))))
                                  (usage (alist-get 'usage json))
                                  (reasoning (or (alist-get 'reasoning_content delta)
                                                 (alist-get 'reasoning delta)))
                                  (content (alist-get 'content delta)))

                             ;; Handle usage if reported
                             (when usage
                               (let ((reported-tokens (alist-get 'completion_tokens usage)))
                                 (when reported-tokens
                                   (setq token-count reported-tokens))))

                             ;; Handle explicit reasoning_content delta
                             (when (and reasoning (> (length reasoning) 0))
                               (setq token-count (1+ token-count))
                               (deslop-llm--emit-status callbacks "Thinking")
                               (deslop-llm--emit-delta callbacks :thinking reasoning))

                             ;; Handle content delta
                             (when (and content (> (length content) 0))
                               (setq token-count (1+ token-count))
                               (let ((res (deslop-llm--process-content content in-thinking callbacks)))
                                 (setq in-thinking (car res))
                                 (setq accumulated-content (concat accumulated-content (cdr res)))))

                             ;; Notify usage & speed
                             (when on-usage
                               (let* ((elapsed (max 0.001 (- (float-time) start-time)))
                                      (speed (/ (float token-count) elapsed)))
                                 (funcall on-usage token-count speed))))
                         (error nil))))))))
           :sentinel
           (lambda (p event)
             (let ((cancelled (process-get p :cancelled)))
               (when (buffer-live-p proc-buf)
                 (kill-buffer proc-buf))
               (cond
                (cancelled
                 (when on-error (funcall on-error "Process cancelled")))
                ((string-match-p "finished" event)
                 (when on-done (funcall on-done accumulated-content)))
                (t
                 (when on-error
                   (funcall on-error (format "Process error: %s" (string-trim event)))))))))))
    (when on-start
      (funcall on-start proc))
    proc))

(defalias 'deslop-llm-send #'deslop-llm-default)

(provide 'deslop-llm)

;;; deslop-llm.el ends here
