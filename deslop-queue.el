;;; deslop-queue.el --- FIFO job queue and execution engine for deslop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools

;;; Commentary:

;; FIFO queue management, job identity, lifecycle transitions, and process
;; cancellation for deslop.

;;; Code:

(require 'cl-lib)
(require 'deslop-core)
(require 'deslop-operation)
(require 'deslop-provider)
(require 'deslop-target)

;;;; Job records

(defun deslop--invoke-cancellation (cancel-function)
  "Invoke CANCEL-FUNCTION without allowing cleanup failures to escape."
  (when (functionp cancel-function)
    (deslop--ignore-errors cancel-function)))

(cl-defstruct (deslop-job (:constructor deslop--make-job))
  "A queued LLM operation and its asynchronous lifecycle state."
  id type state cancel-function overlay target profile instruction
  metadata metadata-rank result error
  enqueued-at started-at finished-at)

(defvar deslop--next-job-id 0
  "Next numeric identifier assigned to a job.")

(defvar deslop--current-job nil
  "One active job retained for compatibility and status display.

Callbacks must not use this variable to identify their originating job.  It
is the scheduler's first active job; `deslop--active-jobs' is authoritative.")

(defvar deslop--active-jobs nil
  "Active jobs, in reverse dispatch order.")

(defvar deslop--aborting nil
  "Non-nil while queue state is being invalidated by an abort.")

(defun deslop-job-terminal-p (job)
  "Non-nil when JOB has reached a terminal state."
  (memq (deslop-job-state job) '(completed failed cancelled skipped)))

(defun deslop-job-current-p (job)
  "Non-nil when JOB is the active, still-settleable job."
  (and (memq job deslop--active-jobs)
       (eq (deslop-job-state job) 'running)
       (not deslop--aborting)))

(defun deslop-job-cancellation-set (job cancel-function)
  "Associate CANCEL-FUNCTION with JOB, or invoke it if JOB is stale."
  (if (deslop-job-current-p job)
      (setf (deslop-job-cancel-function job) cancel-function)
    (deslop--invoke-cancellation cancel-function))
  cancel-function)

(defun deslop--job-release-current (job)
  "Release JOB from the active set and release its target."
  (setq deslop--active-jobs (delq job deslop--active-jobs)
        deslop--current-job (car deslop--active-jobs))
  (deslop--ignore-errors #'deslop-target-release (deslop-job-target job)))

;;;; Queue state

(defvar deslop-queue nil
  "Pending jobs in FIFO order.")

(defvar deslop--queue-tail nil
  "Tail cons cell of `deslop-queue'.")

(defvar deslop--dispatching nil
  "Non-nil while the scheduler is dispatching jobs iteratively.")

(defvar deslop--queue-suspended nil
  "Non-nil when dispatch is temporarily suspended.")

(defvar deslop--job-handlers nil
  "Alist mapping job type symbols to execution functions.")

(defvar deslop-job-event-functions nil
  "Functions called with a job and one normalized lifecycle event.")

(defvar deslop-queue-abort-functions nil
  "Functions called after all queued jobs have been invalidated.")

(defun deslop-job-emit-event (job event)
  "Send EVENT for JOB to lifecycle observers without propagating failures."
  (dolist (function deslop-job-event-functions)
    (deslop--ignore-errors function job event)))

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

(defun deslop--queue-remove-cell (cell)
  "Remove queue CELL and return its job."
  (let ((job (car cell)))
    (if (eq cell deslop-queue)
        (deslop--queue-pop)
      (let ((previous deslop-queue))
        (while (and previous (not (eq (cdr previous) cell)))
          (setq previous (cdr previous)))
        (unless previous
          (error "queue cell is not present"))
        (setcdr previous (cdr cell))
        (when (eq cell deslop--queue-tail)
          (setq deslop--queue-tail previous))
        job))))

(defun deslop--same-provider-p (left right)
  "Return non-nil when job profiles LEFT and RIGHT name the same provider."
  (equal (or (plist-get (deslop-job-profile left) :name)
             (plist-get (deslop-job-profile left) :url))
         (or (plist-get (deslop-job-profile right) :name)
             (plist-get (deslop-job-profile right) :url))))

(defun deslop--active-provider-jobs (job)
  "Return active jobs using the same provider as JOB."
  (cl-remove-if-not
   (lambda (active) (deslop--same-provider-p job active))
   deslop--active-jobs))

(defun deslop--write-job-p (job)
  "Return non-nil when JOB can modify its target buffer."
  (deslop-operation-writes-buffer-p (deslop-job-type job)))

(defun deslop--same-buffer-p (left right)
  "Return non-nil when jobs LEFT and RIGHT target the same buffer."
  (eq (deslop-target-buffer (deslop-job-target left))
      (deslop-target-buffer (deslop-job-target right))))

(defun deslop--buffer-write-locked-p (job)
  "Return non-nil when JOB conflicts with an active arbitrary write."
  (and (deslop--write-job-p job)
       (cl-some
        (lambda (active)
          (and (deslop--write-job-p active)
               (deslop--same-buffer-p job active)
                (or (deslop-operation-whole-buffer-p (deslop-job-type job))
                    (deslop-operation-whole-buffer-p
                     (deslop-job-type active)))))
        deslop--active-jobs)))

(defun deslop--job-admissible-p (job)
  "Return non-nil when JOB may start under provider concurrency rules."
  (unless (deslop--buffer-write-locked-p job)
    (let* ((active (deslop--active-provider-jobs job))
           (parallel (eq (plist-get (deslop-job-profile job) :parallel) t))
           (limit (deslop-provider-max-concurrency
                   (deslop-job-profile job))))
      (if parallel
          (and (cl-every
                (lambda (running)
                  (eq (plist-get (deslop-job-profile running) :parallel) t))
                active)
               (< (length active) limit))
        (null active)))))

(defun deslop--next-admissible-cell ()
  "Return the first FIFO queue cell whose provider can accept its job.
Only the oldest queued job for each provider is considered, so a blocked job
cannot be bypassed by a later job from that same provider."
  (let ((seen-providers nil))
    (catch 'found
      (cl-loop for cell on deslop-queue do
        (let* ((job (car cell))
               (profile (deslop-job-profile job))
               (provider (or (plist-get profile :name)
                             (plist-get profile :url))))
          (unless (memq provider seen-providers)
            (push provider seen-providers)
            (when (deslop--job-admissible-p job)
              (throw 'found cell)))))
      nil)))

(defun deslop--job-from-request (request)
  "Create a queued job from REQUEST, a (TYPE . PLIST) value."
  (let* ((type (car request))
         (data (cdr request))
         (target (plist-get data :target))
         (profile (copy-tree
                   (or (plist-get data :profile)
                       (deslop-provider-profile-snapshot
                        (plist-get data :provider)))))
         (now (float-time))
         (parallel (eq (plist-get profile :parallel) t))
         (job (deslop--make-job
                :id (cl-incf deslop--next-job-id)
                :type type
                :state 'queued
                :overlay (plist-get data :overlay)
                :target target
                :profile profile
                :instruction (plist-get data :instruction)
                :enqueued-at now)))
    (unless (deslop-target-p target)
      (error "deslop request is missing a target"))
    (when (and parallel
               (>= (length (deslop--active-provider-jobs job))
                   (deslop-provider-max-concurrency profile)))
      (setf (deslop-job-profile job)
            (plist-put (deslop-job-profile job) :parallel nil)))
    (setf (deslop-job-profile job)
          (plist-put (copy-tree (deslop-job-profile job))
                     :job-id (deslop-job-id job)))
    job))

(defun deslop-enqueue (request)
  "Add REQUEST to the queue and trigger processing if idle.
Return the newly created `deslop-job'."
  (let ((job (deslop--job-from-request request)))
    (deslop--queue-push job)
    (deslop-job-emit-event
     job
     (list :type :queued
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
    (deslop-job-emit-event job (list :type :settled :state state))
    (unwind-protect
        (deslop--invoke-cancellation (deslop-job-cancel-function job))
      (deslop--job-release-current job)
      (unless deslop--aborting
        (deslop-process-next)))))

(defun deslop-job-finish (job &optional result)
  "Complete JOB with RESULT and advance the queue."
  (when (deslop-job-current-p job)
    (deslop-job-emit-event
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
      (deslop-job-emit-event job (list :type :error :error error))
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

(defun deslop--dispatch-job (job)
  "Start JOB and route scheduler or handler failures to settlement."
  (let* ((type (deslop-job-type job))
         (handler (alist-get type deslop--job-handlers))
         (target (deslop-job-target job)))
    (push job deslop--active-jobs)
    (setq deslop--current-job (car deslop--active-jobs))
    (setf (deslop-job-state job) 'running
          (deslop-job-started-at job) (float-time))
    (cond
     ((not (deslop-target-live-p target))
      (deslop-job-skip
       job (format "%s: target buffer no longer exists" type)))
     ((null handler)
      (deslop-job-fail
       job (deslop-error-create
            :kind 'scheduler
            :message (format "unknown job handler: %s" type))))
     (t
      (condition-case caught
          (progn
            (deslop-target-activate
             target
             (lambda (message)
                (deslop-job-fail
                 job (deslop-error-create :kind 'target :message message)))
              (deslop-operation-allows-source-changes-p type))
            (deslop-target-assert-available target)
            (funcall handler job))
        ((error quit)
         (deslop-job-fail job caught)))))))

(defun deslop-process-next ()
  "Process all currently admissible queued jobs."
  (unless deslop--dispatching
    (let ((deslop--dispatching t)
          (done nil))
      (while (and (not deslop--queue-suspended)
                  (not deslop--aborting)
                  (not done))
        (let ((cell (deslop--next-admissible-cell)))
          (if cell
              (deslop--dispatch-job (deslop--queue-remove-cell cell))
            (setq done t)))))))

;;;; Cancellation and status

(defun deslop--job-cancel (job)
  "Mark JOB cancelled and clean its local state without dispatching."
  (unless (deslop-job-terminal-p job)
    (setf (deslop-job-state job) 'cancelled
          (deslop-job-finished-at job) (float-time))
    (deslop--invoke-cancellation (deslop-job-cancel-function job))
    (deslop--ignore-errors #'deslop-target-release (deslop-job-target job))
    (deslop-job-emit-event job '(:type :cancelled))
    (deslop-job-emit-event job '(:type :settled :state cancelled))))

(defun deslop-abort-queue ()
  "Abort the active request, cancel pending jobs, and clear overlays."
  (interactive)
  (let* ((active deslop--active-jobs)
         (pending deslop-queue)
         (jobs (append active pending)))
    ;; Invalidate callbacks before invoking transport cancellation because a
    ;; transport may settle reentrantly.
    (setq deslop--aborting t)
    (unwind-protect
        (progn
          (setq deslop-queue nil)
          (setq deslop--queue-tail nil)
          (setq deslop--active-jobs nil)
          (setq deslop--current-job nil)
          (dolist (job jobs)
            (deslop--job-cancel job))
          (dolist (function deslop-queue-abort-functions)
            (deslop--ignore-errors function)))
      (setq deslop--aborting nil)
      (message "deslop: queue aborted"))))

(defun deslop-queue-status ()
  "Display and return a nonvisual snapshot of queue state."
  (interactive)
  (let ((status (list :running (and deslop--active-jobs t)
                      :active (and deslop--current-job
                                   (deslop-job-id deslop--current-job))
                      :active-jobs (mapcar #'deslop-job-id deslop--active-jobs)
                      :pending (length deslop-queue))))
    (message "deslop: active=%s pending=%d"
             (or (plist-get status :active) "none")
             (plist-get status :pending))
    status))

(provide 'deslop-queue)

;;; deslop-queue.el ends here
