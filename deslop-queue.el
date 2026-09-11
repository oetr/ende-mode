;;; deslop-queue.el --- FIFO job queue and execution engine for deslop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools

;;; Commentary:

;; FIFO queue management, job dispatching, and process cancellation for deslop.

;;; Code:

(require 'deslop-common)
(require 'deslop-ui)

;;;; Internal State

(defvar deslop-queue nil
  "Queue of pending jobs.  Each job is (TYPE . PLIST).")

(defvar deslop-running nil
  "Non-nil while a job is currently being processed.")

(defvar deslop--current-job nil
  "The job currently being processed, or nil.")

(defvar deslop--job-handlers nil
  "Alist mapping job type symbols to execution functions.")

;;;; Handler Registration & Dispatch

(defun deslop-register-job-handler (type handler)
  "Register HANDLER function for jobs of symbol TYPE.
HANDLER is called with the job's plist."
  (setf (alist-get type deslop--job-handlers) handler))

(defun deslop-enqueue (job)
  "Add JOB to the queue and trigger processing if idle."
  (setq deslop-queue (nconc deslop-queue (list job)))
  (deslop-process-next))

(defun deslop-process-next ()
  "Process the next queued job if engine is idle."
  (unless (or deslop-running (null deslop-queue))
    (setq deslop-running t)
    (let* ((job (pop deslop-queue))
           (type (car job))
           (plist (cdr job))
           (buf (plist-get plist :buffer))
           (handler (alist-get type deslop--job-handlers)))
      (setq deslop--current-job job)
      (cond
       ((or (null buf) (not (buffer-live-p buf)))
        (deslop--skip (format "%s: target buffer no longer exists" type)))
       ((null handler)
        (deslop--skip (format "%s: unknown job handler" type)))
       (t
        (condition-case err
            (funcall handler plist)
          (error
           (message "deslop error in job %s: %s" type (error-message-string err))
           (deslop-job-done))))))))

(defun deslop-job-done ()
  "Finish the active job, delete its overlay, and advance the queue."
  (when deslop--current-job
    (let ((ov (plist-get (cdr deslop--current-job) :overlay)))
      (when ov
        (deslop-ui-delete-overlay ov)))
    (setq deslop--current-job nil))
  (setq deslop-running nil)
  (deslop-process-next))

(defun deslop--skip (msg)
  "Skip the active job with log MSG and advance queue."
  (message "deslop: skipped (%s)" msg)
  (deslop-job-done))

(defun deslop-abort-queue ()
  "Abort all pending jobs, kill running LLM process, and clear overlays."
  (interactive)
  ;; Terminate active process if running
  (when deslop--current-job
    (let ((proc (plist-get (cdr deslop--current-job) :process)))
      (when (and (processp proc) (process-live-p proc))
        (process-put proc :cancelled t)
        (delete-process proc))))
  (deslop-ui-clear-all)
  (setq deslop-queue nil)
  (setq deslop--current-job nil)
  (setq deslop-running nil)
  (message "deslop: queue aborted"))

(provide 'deslop-queue)

;;; deslop-queue.el ends here
