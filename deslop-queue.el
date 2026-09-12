;;; deslop-queue.el --- FIFO job queue and execution engine for deslop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools

;;; Commentary:

;; FIFO queue management, job identity, lifecycle transitions, and process
;; cancellation for deslop.

;;; Code:

(require 'cl-lib)
(require 'deslop-common)
(require 'deslop-ui)

;;;; Job records

(defun deslop--cancel-process (process)
  "Mark PROCESS cancelled and delete it when it is still live."
  (when (and (processp process) (process-live-p process))
    ;; Set the property before deletion because sentinels may run reentrantly.
    (process-put process :deslop-cancelled t)
    (deslop--ignore-errors #'delete-process process)))

(cl-defstruct (deslop-job (:constructor deslop--make-job))
  "A queued LLM operation and its asynchronous lifecycle state."
  id type data state process overlay buffer
  start-marker end-marker cursor-marker placement source-snapshot source-digest
  restriction-beg restriction-end profile result error
  enqueued-at started-at finished-at)

(defvar deslop--next-job-id 0
  "Next numeric identifier assigned to a job.")

(defun deslop-job-get (job key)
  "Read KEY from JOB's operation data plist."
  (plist-get (deslop-job-data job) key))

(defun deslop-job-put (job key value)
  "Set KEY to VALUE in JOB's operation data plist."
  (setf (deslop-job-data job)
        (plist-put (deslop-job-data job) key value))
  value)

(defun deslop-job-terminal-p (job)
  "Non-nil when JOB has reached a terminal state."
  (memq (deslop-job-state job) '(completed failed cancelled skipped)))

(defun deslop-job-current-p (job)
  "Non-nil when JOB is the active, still-settleable job."
  (and (eq job deslop--current-job)
       (eq (deslop-job-state job) 'running)
       (not deslop--aborting)))

(defun deslop-job-process-set (job process)
  "Associate PROCESS with JOB, or terminate it if JOB is already stale."
  (if (deslop-job-current-p job)
    (setf (deslop-job-process job) process)
    (deslop--cancel-process process))
  process)

(defun deslop--job-delete-overlay (job)
  "Delete JOB's presentation overlay and release its markers."
  (let ((overlay (deslop-job-overlay job)))
    (setf (deslop-job-overlay job) nil)
    (when overlay
      (deslop--ignore-errors #'deslop-ui-delete-overlay overlay)))
  (dolist (marker (list (deslop-job-start-marker job)
                        (deslop-job-end-marker job)
                        (deslop-job-cursor-marker job)
                        (deslop-job-restriction-beg job)
                        (deslop-job-restriction-end job)))
    (when (markerp marker)
      (deslop--ignore-errors #'set-marker marker nil))))

(defun deslop--job-remove-buffer-hooks (job)
  "Remove target lifecycle hooks installed for JOB."
  (let ((buffer (deslop-job-buffer job))
        (kill-hook (deslop-job-get job :kill-buffer-hook))
        (before-hook (deslop-job-get job :before-change-hook))
        (after-hook (deslop-job-get job :after-change-hook)))
    (when (buffer-live-p buffer)
      (deslop--ignore-errors
       (lambda ()
         (with-current-buffer buffer
           (when kill-hook
             (remove-hook 'kill-buffer-hook kill-hook t))
           (when before-hook
             (remove-hook 'before-change-functions before-hook t))
           (when after-hook
             (remove-hook 'after-change-functions after-hook t))))))
    (deslop-job-put job :kill-buffer-hook nil)
    (deslop-job-put job :before-change-hook nil)
    (deslop-job-put job :after-change-hook nil)))

(defun deslop--job-change-overlaps-target-p (job beg end)
  "Return non-nil when the change BEG..END overlaps JOB's selected target."
  (let ((start (and (markerp (deslop-job-start-marker job))
                    (marker-position (deslop-job-start-marker job))))
        (finish (and (markerp (deslop-job-end-marker job))
                     (marker-position (deslop-job-end-marker job)))))
    (and (deslop-job-get job :selected)
         start finish
         (if (= beg end)
             (and (> beg start) (< beg finish))
           (< (max beg start) (min end finish))))))

(defun deslop--job-install-buffer-hooks (job)
  "Cancel JOB promptly when its immutable target becomes unusable."
  (let ((buffer (deslop-job-buffer job)))
    (when (buffer-live-p buffer)
      (let ((kill-hook
             (lambda ()
               (when (deslop-job-current-p job)
                 (deslop-job-fail
                  job (deslop-error-create
                       :kind 'target
                       :message "target buffer was killed")))))
            (before-hook
             (lambda (beg end)
               (when (and (deslop-job-current-p job)
                          (not (deslop-job-get job :applying))
                          (deslop--job-change-overlaps-target-p job beg end))
                 (deslop-job-put job :target-invalidated t))))
            (after-hook
             (lambda (&rest _)
               (when (and (deslop-job-current-p job)
                          (deslop-job-get job :target-invalidated))
                 (deslop-job-fail
                  job (deslop-error-create
                       :kind 'target
                       :message "selected source changed during generation"))))))
        (deslop-job-put job :kill-buffer-hook kill-hook)
        (deslop-job-put job :before-change-hook before-hook)
        (deslop-job-put job :after-change-hook after-hook)
        (with-current-buffer buffer
          (add-hook 'kill-buffer-hook kill-hook nil t)
          (add-hook 'before-change-functions before-hook nil t)
          (add-hook 'after-change-functions after-hook nil t))))))

(defun deslop--job-release-current (job)
  "Release JOB from the active slot and clean its presentation state."
  (when (eq deslop--current-job job)
    (setq deslop--current-job nil)
    (setq deslop-running nil))
  (deslop--ignore-errors #'deslop--job-remove-buffer-hooks job)
  (deslop--ignore-errors #'deslop--job-delete-overlay job))

;;;; Queue state

(defvar deslop-queue nil
  "Pending jobs in FIFO order.")

(defvar deslop--queue-tail nil
  "Tail cons cell of `deslop-queue'.")

(defvar deslop-running nil
  "Non-nil while a job is currently being processed.")

(defvar deslop--current-job nil
  "The job currently being processed, or nil.

Callbacks must not use this variable to identify their originating job.  It
is only the scheduler's active slot.")

(defvar deslop--aborting nil
  "Non-nil while queue state is being invalidated by an abort.")

(defvar deslop--dispatching nil
  "Non-nil while the scheduler is dispatching jobs iteratively.")

(defvar deslop--job-handlers nil
  "Alist mapping job type symbols to execution functions.")

(defun deslop-register-job-handler (type handler)
  "Register HANDLER function for jobs of symbol TYPE.
HANDLER receives one `deslop-job' object."
  (setf (alist-get type deslop--job-handlers) handler))

(defun deslop--queue-push (job)
  "Append JOB to the pending queue in constant time."
  (let ((cell (list job)))
    (if deslop--queue-tail
        (setcdr deslop--queue-tail cell)
      (setq deslop-queue cell))
    (setq deslop--queue-tail cell)))

(defun deslop--queue-pop ()
  "Pop and return the oldest pending job."
  (let ((job (car deslop-queue)))
    (setq deslop-queue (cdr deslop-queue))
    (when (null deslop-queue)
      (setq deslop--queue-tail nil))
    job))

(defun deslop--job-from-request (request)
  "Create a queued job from REQUEST, a (TYPE . PLIST) value."
  (let* ((type (car request))
         (data (copy-sequence (cdr request)))
         (now (float-time))
         (job (deslop--make-job
               :id (cl-incf deslop--next-job-id)
               :type type
               :data data
               :state 'queued
               :overlay (plist-get data :overlay)
               :buffer (plist-get data :buffer)
               :start-marker (plist-get data :start-marker)
               :end-marker (plist-get data :end-marker)
               :cursor-marker (plist-get data :cursor-marker)
                :placement (plist-get data :placement)
                :source-snapshot (plist-get data :source-snapshot)
                :source-digest (plist-get data :source-digest)
                :restriction-beg (plist-get data :restriction-beg)
               :restriction-end (plist-get data :restriction-end)
               :profile (or (plist-get data :profile)
                            (deslop-provider-profile-snapshot
                             (plist-get data :provider)))
               :enqueued-at now)))
    (setf (deslop-job-profile job)
          (plist-put (copy-tree (deslop-job-profile job))
                     :job-id (deslop-job-id job)))
    (deslop-job-put job :profile (deslop-job-profile job))
    job))

(defun deslop-enqueue (request)
  "Add REQUEST to the queue and trigger processing if idle.
Return the newly created `deslop-job'."
  (let ((job (deslop--job-from-request request)))
    (deslop--queue-push job)
    (when (deslop-job-overlay job)
      (deslop-ui-update-overlay
       (deslop-job-overlay job)
       :client (deslop-provider-client-name (deslop-job-profile job))
       :model (plist-get (deslop-job-profile job) :model)))
    (deslop-process-next)
    job))

;;;; Settlement and dispatch

(defun deslop--job-settle (job state &optional result error)
  "Settle active JOB exactly once with STATE, RESULT, and ERROR."
  (when (deslop-job-current-p job)
    (setf (deslop-job-state job) state
          (deslop-job-result job) result
          (deslop-job-error job) error
          (deslop-job-finished-at job) (float-time))
    (unwind-protect
        (let ((process (deslop-job-process job)))
          (deslop--cancel-process process))
      (deslop--job-release-current job)
      (unless deslop--aborting
        (deslop-process-next)))))

(defun deslop-job-finish (job &optional result)
  "Complete JOB with RESULT and advance the queue."
  (when (and (deslop-job-current-p job)
             (fboundp 'deslop-history-event))
    (deslop-history-event
     job (list :type :done :result result
               :content (if (stringp result)
                            result
                          (and (listp result)
                               (plist-get result :content))))))
  (deslop--job-settle job 'completed result nil))

(defun deslop-job-fail (job error)
  "Fail JOB with ERROR and advance the queue."
  (when (deslop-job-current-p job)
    (let ((id (deslop-job-id job)))
      (when (fboundp 'deslop-history-event)
        (deslop-history-event job (list :type :error :error error)))
      (deslop--job-settle job 'failed nil error)
      (condition-case nil
          (message "deslop: job %d failed: %s"
                   id (deslop-error-display-message error))
        ((error quit) nil)))))

(defun deslop-job-skip (job message)
  "Skip active JOB with MESSAGE and advance the queue."
  (when (deslop-job-current-p job)
    (let ((id (deslop-job-id job)))
      (deslop--job-settle job 'skipped nil
                          (deslop-error-create :kind 'skipped
                                               :message message))
      (message "deslop: skipped job %d (%s)" id message))))

(defun deslop-job-done (&optional job result)
  "Compatibility wrapper completing JOB or the current job."
  (deslop-job-finish (or job deslop--current-job) result))

(defun deslop--skip (job message)
  "Compatibility wrapper skipping JOB with MESSAGE."
  (deslop-job-skip (or job deslop--current-job) message))

(defun deslop--dispatch-job (job)
  "Start JOB and route scheduler or handler failures to settlement."
  (let* ((type (deslop-job-type job))
         (handler (alist-get type deslop--job-handlers))
         (buffer (deslop-job-buffer job)))
    (setq deslop-running t
          deslop--current-job job)
    (setf (deslop-job-state job) 'running
          (deslop-job-started-at job) (float-time))
    (cond
     ((not (buffer-live-p buffer))
      (deslop-job-skip
       job (format "%s: target buffer no longer exists" type)))
     ((null handler)
      (deslop-job-fail
       job (deslop-error-create
            :kind 'scheduler
            :message (format "unknown job handler: %s" type))))
     (t
      (deslop--job-install-buffer-hooks job)
      (condition-case caught
          (funcall handler job)
        ((error quit)
         (deslop-job-fail job caught)))))))

(defun deslop-process-next ()
  "Process the next queued job if the engine is idle."
  (unless deslop--dispatching
    (let ((deslop--dispatching t))
      (while (and (not deslop-running)
                  (not deslop--aborting)
                  deslop-queue)
        (deslop--dispatch-job (deslop--queue-pop))))))

;;;; Cancellation and status

(defun deslop--job-cancel (job)
  "Mark JOB cancelled and clean its local state without dispatching."
  (unless (deslop-job-terminal-p job)
    (setf (deslop-job-state job) 'cancelled)
    (deslop--ignore-errors #'deslop--job-remove-buffer-hooks job)
    (when (fboundp 'deslop-history-cancel)
      (deslop-history-cancel job))
    (deslop--ignore-errors #'deslop--job-delete-overlay job)))

(defun deslop-abort-queue ()
  "Abort the active request, cancel pending jobs, and clear overlays."
  (interactive)
  (let* ((active deslop--current-job)
         (pending deslop-queue)
         (jobs (append (and active (list active)) pending))
         (process (and active (deslop-job-process active))))
    ;; Invalidate every callback before deleting the process.  Process
    ;; sentinels may run reentrantly from `delete-process'.
    (setq deslop--aborting t)
    (unwind-protect
        (progn
          (setq deslop-queue nil)
          (setq deslop--queue-tail nil)
          (setq deslop--current-job nil)
          (setq deslop-running nil)
          (dolist (job jobs)
            (deslop--job-cancel job))
          (deslop--cancel-process process)
          (when (fboundp 'deslop-provider-abort-discovery)
            (deslop--ignore-errors #'deslop-provider-abort-discovery))
          (deslop--ignore-errors #'deslop-ui-clear-all))
      (setq deslop--aborting nil)
      (message "deslop: queue aborted"))))

(defun deslop-queue-status ()
  "Display and return a nonvisual snapshot of queue state."
  (interactive)
  (let ((status (list :running deslop-running
                      :active (and deslop--current-job
                                   (deslop-job-id deslop--current-job))
                      :pending (length deslop-queue))))
    (message "deslop: active=%s pending=%d"
             (or (plist-get status :active) "none")
             (plist-get status :pending))
    status))

(provide 'deslop-queue)

;;; deslop-queue.el ends here
