;;; deslop-history.el --- Retained deslop job output -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools

;;; Commentary:

;; History is an event sink.  Failures in this optional sink must never affect
;; queue settlement or buffer application.

;;; Code:

(require 'cl-lib)
(require 'outline)
(require 'subr-x)
(require 'deslop-common)
(require 'deslop-queue)

(cl-defstruct (deslop-history--entry
               (:constructor deslop-history--make-entry))
  job type prompt thinking-parts content-parts status usage result error
  started-at finished-at)

(defvar deslop-history--entries (make-hash-table :test #'eq)
  "In-progress history entries keyed by job object.")

(defun deslop-history--prompt-string (messages)
  "Format MESSAGES for storage in a history entry."
  (mapconcat (lambda (message)
               (format "%s:\n%s" (car message) (cdr message)))
             messages "\n\n"))

(defun deslop-history--entry (job)
  "Return the active history entry for JOB."
  (gethash job deslop-history--entries))

(defun deslop-history-begin (job messages)
  "Start retaining JOB with MESSAGES as its prompt."
  (when deslop-history-enabled
    (condition-case nil
        (puthash job
                 (deslop-history--make-entry
                  :job job
                  :type (deslop-job-type job)
                  :prompt (deslop-history--prompt-string messages)
                  :status "Queued"
                  :started-at (float-time))
                 deslop-history--entries)
      (error nil))))

(defun deslop-history-event (job event)
  "Record normalized EVENT for JOB without propagating sink errors."
  (when deslop-history-enabled
    (condition-case nil
        (let ((entry (deslop-history--entry job)))
          (when entry
            (pcase (plist-get event :type)
              (:status
               (setf (deslop-history--entry-status entry)
                     (plist-get event :status)))
              (:delta
               (if (eq (plist-get event :channel) :thinking)
                   (push (plist-get event :text)
                         (deslop-history--entry-thinking-parts entry))
                 (push (plist-get event :text)
                       (deslop-history--entry-content-parts entry))))
              (:usage
               (setf (deslop-history--entry-usage entry) event))
              (:done
               (let ((content (plist-get event :content)))
                 (when (and (stringp content) (> (length content) 0))
                   (push content (deslop-history--entry-content-parts entry))))
               (setf (deslop-history--entry-status entry) "Completed"
                     (deslop-history--entry-result entry)
                     (plist-get event :result)
                     (deslop-history--entry-finished-at entry) (float-time))
               (deslop-history--flush entry))
              (:error
               (setf (deslop-history--entry-status entry) "Failed"
                     (deslop-history--entry-error entry)
                     (plist-get event :error)
                     (deslop-history--entry-finished-at entry) (float-time))
               (deslop-history--flush entry))
              (:cancelled
               (setf (deslop-history--entry-status entry) "Cancelled"
                     (deslop-history--entry-finished-at entry) (float-time))
               (deslop-history--flush entry)))))
      (error nil))))

(defun deslop-history-cancel (job)
  "Record JOB as cancelled and flush its retained prompt."
  (deslop-history-event job (list :type :cancelled)))

(defun deslop-history--buffer ()
  "Return the history buffer, configuring outline folding once."
  (let ((buffer (get-buffer-create deslop-history-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'outline-mode)
        (outline-mode))
      (setq-local outline-regexp "^\*+ "))
    buffer))

(defun deslop-history--flush (entry)
  "Append ENTRY to the history buffer and remove its active state."
  (when deslop-history-enabled
    (let* ((job (deslop-history--entry-job entry))
           (result (deslop-history--entry-result entry))
           (error (deslop-history--entry-error entry))
           (thinking (mapconcat #'identity
                                (nreverse
                                 (deslop-history--entry-thinking-parts entry))
                                ""))
           (content (mapconcat #'identity
                               (nreverse
                                (deslop-history--entry-content-parts entry))
                               ""))
           (usage (deslop-history--entry-usage entry))
           (status (deslop-history--entry-status entry))
           (buffer (deslop-history--buffer)))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert (format "\n* Job %d: %s [%s] %s\n"
                        (deslop-job-id job)
                        (deslop-job-type job)
                        status
                        (format-time-string "%Y-%m-%d %H:%M:%S")))
        (insert "** Prompt\n" (deslop-history--entry-prompt entry) "\n")
        (when (> (length thinking) 0)
          (insert "** Thinking\n" thinking "\n"))
        (when (> (length content) 0)
          (insert "** Output\n" content "\n"))
        (when (listp result)
          (insert (format "** Metrics\nTokens: %s\nEstimated: %s\nSpeed: %.2f t/s\n"
                          (or (plist-get result :tokens) 0)
                          (if (plist-get result :estimated) "yes" "no")
                          (or (plist-get result :speed) 0.0))))
        (when usage
          (insert (format "Usage: %S\n" usage)))
        (when error
          (insert "** Error\n" (deslop-error-display-message error) "\n")))
      (remhash job deslop-history--entries))))

(provide 'deslop-history)

;;; deslop-history.el ends here
