;;; ende-queue.el --- FIFO job queue and execution engine for ende -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools

;;; Commentary:

;; FIFO queue management, job identity, lifecycle transitions, and process
;; cancellation for ende.

;;; Code:

(require 'cl-lib)
(require 'ende-core)
(require 'ende-operation)
(require 'ende-provider)
(require 'ende-target)

;;;; Job records

(defun ende--invoke-cancellation (cancel-function)
  "Invoke CANCEL-FUNCTION without allowing cleanup failures to escape."
  (when (functionp cancel-function)
    (ende-ignore-errors cancel-function)))

(cl-defstruct (ende-job (:constructor ende--make-job))
  "A queued LLM operation and its asynchronous lifecycle state."
  id type state cancel-function overlay target profile instruction
  metadata metadata-rank result error structured-state
  enqueued-at started-at finished-at)

(cl-defstruct (ende-queue-fence (:constructor ende--make-fence))
  "A queue barrier for jobs present when the fence was created."
  provider cutoff)

(defvar ende--next-job-id 0
  "Next numeric identifier assigned to a job.")

(defvar ende--current-job nil
  "One active job retained for compatibility and status display.

Callbacks must not use this variable to identify their originating job.  It
is the scheduler's first active job; `ende--active-jobs' is authoritative.")

(defvar ende--active-jobs nil
  "Active jobs, in reverse dispatch order.")

(defvar ende--fences nil
  "Queue barriers that have not yet been crossed by later jobs.")

(defvar ende--aborting nil
  "Non-nil while queue state is being invalidated by an abort.")

(defvar-local ende--source-buffer-jobs nil
  "Jobs queued for or running against the current source buffer.")

;; Job ownership and cancellation must survive changes of major mode.
(put 'ende--source-buffer-jobs 'permanent-local t)
(put 'ende--source-buffer-killed 'permanent-local-hook t)

(defun ende--unregister-source-job (job)
  "Remove JOB from its source buffer's job registry."
  (let ((buffer (ende-target-buffer (ende-job-target job))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq ende--source-buffer-jobs
              (delq job ende--source-buffer-jobs))
        (when (null ende--source-buffer-jobs)
          (remove-hook 'kill-buffer-hook
                       #'ende--source-buffer-killed t)))))
  job)

(defun ende-job-terminal-p (job)
  "Non-nil when JOB has reached a terminal state."
  (memq (ende-job-state job) '(completed failed cancelled skipped)))

(defun ende-job-current-p (job)
  "Non-nil when JOB is the active, still-settleable job."
  (and (memq job ende--active-jobs)
       (eq (ende-job-state job) 'running)
       (not ende--aborting)))

(defun ende-job-cancellation-set (job cancel-function)
  "Associate CANCEL-FUNCTION with JOB, or invoke it if JOB is stale."
  (if (ende-job-current-p job)
      (setf (ende-job-cancel-function job) cancel-function)
    (ende--invoke-cancellation cancel-function))
  cancel-function)

(defun ende--job-release-current (job)
  "Release JOB from the active set and release its target."
  (setq ende--active-jobs (delq job ende--active-jobs)
        ende--current-job (car ende--active-jobs))
  (ende--unregister-source-job job)
  (ende-ignore-errors #'ende-target-release (ende-job-target job)))

;;;; Queue state

(defvar ende-queue nil
  "Pending jobs in FIFO order.")

(defun ende-active-jobs ()
  "Return a snapshot of currently active jobs.

The returned list may be changed by the caller without changing scheduler
state."
  (copy-sequence ende--active-jobs))

(defun ende-pending-jobs ()
  "Return a snapshot of jobs waiting in the FIFO queue.

The returned list may be changed by the caller without changing scheduler
state."
  (copy-sequence ende-queue))

(defvar ende--queue-tail nil
  "Tail cons cell of `ende-queue'.")

(defvar ende--dispatching nil
  "Non-nil while the scheduler is dispatching jobs iteratively.")

(defvar ende--queue-suspended nil
  "Non-nil when dispatch is temporarily suspended.")

(defvar ende--job-handlers nil
  "Alist mapping job type symbols to execution functions.")

(defvar ende-job-event-functions nil
  "Functions called with a job and one normalized lifecycle event.")

(defvar ende-queue-abort-functions nil
  "Functions called after all queued jobs have been invalidated.")

(defun ende-job-emit-event (job event)
  "Send EVENT for JOB to lifecycle observers without propagating failures."
  (dolist (function ende-job-event-functions)
    (ende-ignore-errors function job event)))

(defun ende-register-job-handler (type handler)
  "Register HANDLER function for jobs of symbol TYPE.
HANDLER receives one `ende-job' object."
  (setf (alist-get type ende--job-handlers) handler))

(defun ende--queue-push (job)
  "Append JOB to the pending queue in constant time."
  (let ((cell (list job)))
    (if ende--queue-tail
        (setcdr ende--queue-tail cell)
      (setq ende-queue cell))
    (setq ende--queue-tail cell)))

(defun ende--queue-pop ()
  "Pop and return the oldest pending job."
  (let ((job (car ende-queue)))
    (setq ende-queue (cdr ende-queue))
    (when (null ende-queue)
      (setq ende--queue-tail nil))
    job))

(defun ende--queue-remove-cell (cell)
  "Remove queue CELL and return its job."
  (let ((job (car cell)))
    (if (eq cell ende-queue)
        (ende--queue-pop)
      (let ((previous ende-queue))
        (while (and previous (not (eq (cdr previous) cell)))
          (setq previous (cdr previous)))
        (unless previous
          (error "queue cell is not present"))
        (setcdr previous (cdr cell))
        (when (eq cell ende--queue-tail)
          (setq ende--queue-tail previous))
        job))))

(defun ende--same-provider-p (left right)
  "Return non-nil when job profiles LEFT and RIGHT name the same provider."
  (equal (ende--job-provider-key left)
         (ende--job-provider-key right)))

(defun ende--job-provider-key (job)
  "Return the provider identity used for JOB's scheduling rules."
  (let ((profile (ende-job-profile job)))
    (or (plist-get profile :name)
        (plist-get profile :url))))

(defun ende--active-provider-jobs (job)
  "Return active jobs using the same provider as JOB."
  (cl-remove-if-not
   (lambda (active) (ende--same-provider-p job active))
   ende--active-jobs))

(defun ende--write-job-p (job)
  "Return non-nil when JOB can modify its target buffer."
  (ende-operation-writes-buffer-p (ende-job-type job)))

(defun ende--same-buffer-p (left right)
  "Return non-nil when jobs LEFT and RIGHT target the same buffer."
  (eq (ende-target-buffer (ende-job-target left))
      (ende-target-buffer (ende-job-target right))))

(defun ende--buffer-write-locked-p (job)
  "Return non-nil when JOB conflicts with an active arbitrary write."
  (and (ende--write-job-p job)
       (cl-some
        (lambda (active)
          (and (ende--write-job-p active)
               (ende--same-buffer-p job active)
               (or (ende-operation-whole-buffer-p (ende-job-type job))
                   (ende-operation-whole-buffer-p
                    (ende-job-type active)))))
        ende--active-jobs)))

(defun ende--fence-applies-p (fence job)
  "Return non-nil when FENCE can block JOB."
  (and (> (ende-job-id job) (ende-queue-fence-cutoff fence))
       (or (null (ende-queue-fence-provider fence))
           (equal (ende-queue-fence-provider fence)
                  (ende--job-provider-key job)))))

(defun ende--fence-prerequisite-p (fence job)
  "Return non-nil when JOB must finish before FENCE can be crossed."
  (and (<= (ende-job-id job) (ende-queue-fence-cutoff fence))
       (not (ende-job-terminal-p job))
       (or (null (ende-queue-fence-provider fence))
           (equal (ende-queue-fence-provider fence)
                  (ende--job-provider-key job)))))

(defun ende--fence-blocking-p (job)
  "Return non-nil when an unfinished fence blocks JOB.
Completed fences are discarded while checking JOB."
  (let ((jobs (append ende--active-jobs ende-queue))
        (remaining nil)
        (blocked nil))
    (dolist (fence ende--fences)
      (if (cl-some (lambda (prior)
                     (ende--fence-prerequisite-p fence prior))
                   jobs)
          (progn
            (push fence remaining)
            (when (ende--fence-applies-p fence job)
              (setq blocked t)))))
    (setq ende--fences (nreverse remaining))
    blocked))

(defun ende--job-admissible-p (job)
  "Return non-nil when JOB may start under provider concurrency rules."
  (unless (or (ende--fence-blocking-p job)
              (ende--buffer-write-locked-p job))
    (let* ((active (ende--active-provider-jobs job))
           (parallel (eq (plist-get (ende-job-profile job) :parallel) t))
           (limit (ende-provider-max-concurrency
                   (ende-job-profile job))))
      (if parallel
          (< (length active) limit)
        (null active)))))

(defun ende--next-admissible-cell ()
  "Return the first FIFO queue cell whose provider can accept its job.
Only the oldest queued job for each provider is considered, so a blocked job
cannot be bypassed by a later job from that same provider."
  (let ((seen-providers nil))
    (catch 'found
      (cl-loop for cell on ende-queue do
               (let* ((job (car cell))
		      (profile (ende-job-profile job))
		      (provider (or (plist-get profile :name)
				    (plist-get profile :url))))
		 (unless (member provider seen-providers)
		   (push provider seen-providers)
		   (when (ende--job-admissible-p job)
		     (throw 'found cell)))))
      nil)))

(defun ende--job-from-request (request)
  "Create a queued job from REQUEST, a (TYPE . PLIST) value."
  (let* ((type (car request))
         (data (cdr request))
         (target (plist-get data :target)))
    (unless (ende-target-p target)
      (error "ende request is missing a target"))
    (let* ((buffer (ende-target-buffer target))
           (profile
            (copy-tree
             (or (plist-get data :profile)
                 (and (plist-member data :provider)
                      (ende-provider-profile-snapshot
                       (plist-get data :provider)))
                 (ende-provider-profile-for-buffer buffer t))))
           (now (float-time))
           (job (ende--make-job
                 :id (cl-incf ende--next-job-id)
                 :type type
                 :state 'queued
                 :overlay (plist-get data :overlay)
                 :target target
                 :profile profile
                 :instruction (plist-get data :instruction)
                 :enqueued-at now)))
      (setf (ende-job-profile job)
            (plist-put (copy-tree (ende-job-profile job))
                       :job-id (ende-job-id job)))
      job)))

(defun ende-enqueue (request)
  "Add REQUEST to the queue and trigger processing if idle.
Return the newly created `ende-job'."
  (let ((job (ende--job-from-request request)))
    (let ((buffer (ende-target-buffer (ende-job-target job))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (cl-pushnew job ende--source-buffer-jobs :test #'eq)
          (add-hook 'kill-buffer-hook
                    #'ende--source-buffer-killed -50 t))))
    (ende--queue-push job)
    (ende-job-emit-event
     job
     (list :type :queued
           :client (ende-provider-client-name (ende-job-profile job))
           :model (plist-get (ende-job-profile job) :model)))
    (ende-process-next)
    job))

;;;; Settlement and dispatch

(defun ende--job-settle (job state &optional result error)
  "Settle active JOB exactly once with STATE, RESULT, and ERROR."
  (when (ende-job-current-p job)
    (setf (ende-job-state job) state
          (ende-job-result job) result
          (ende-job-error job) error
          (ende-job-finished-at job) (float-time))
    (ende-job-emit-event job (list :type :settled :state state))
    (unwind-protect
        (ende--invoke-cancellation (ende-job-cancel-function job))
      (ende--job-release-current job)
      (unless ende--aborting
        (ende-process-next)))))

(defun ende-job-finish (job &optional result)
  "Complete JOB with RESULT and advance the queue."
  (when (ende-job-current-p job)
    (ende-job-emit-event
     job (list :type :done :result result
               :content (if (stringp result)
                            result
                          (and (listp result)
                               (plist-get result :content))))))
  (ende--job-settle job 'completed result nil))

(defun ende-job-fail (job error &optional result)
  "Fail JOB with ERROR and advance the queue.
When RESULT is non-nil, retain it for history and job inspection."
  (when (ende-job-current-p job)
    (let ((id (ende-job-id job)))
      (ende-job-emit-event
       job (append (list :type :error :error error)
                   (when result (list :result result))))
      (ende--job-settle job 'failed result error)
      (condition-case nil
          (message "ende: job %d failed: %s"
                   id (ende-error-display-message error))
        ((error quit) nil)))))

(defun ende-job-skip (job message)
  "Skip active JOB with MESSAGE and advance the queue."
  (when (ende-job-current-p job)
    (let ((id (ende-job-id job)))
      (ende--job-settle job 'skipped nil
                        (ende-error-create :kind 'skipped
                                           :message message))
      (message "ende: skipped job %d (%s)" id message))))

(defun ende--dispatch-job (job)
  "Start JOB and route scheduler or handler failures to settlement."
  (let* ((type (ende-job-type job))
         (handler (alist-get type ende--job-handlers))
         (target (ende-job-target job)))
    (push job ende--active-jobs)
    (setq ende--current-job (car ende--active-jobs))
    (setf (ende-job-state job) 'running
          (ende-job-started-at job) (float-time))
    (cond
     ((not (ende-target-live-p target))
      (ende-job-skip
       job (format "%s: target buffer no longer exists" type)))
     ((null handler)
      (ende-job-fail
       job (ende-error-create
            :kind 'scheduler
            :message (format "unknown job handler: %s" type))))
     (t
      (condition-case caught
          (progn
            (ende-target-activate
             target
             (lambda (message)
               (ende-job-fail
                job (ende-error-create :kind 'target :message message)))
             (ende-operation-allows-source-changes-p type))
            (ende-target-assert-available target)
            (funcall handler job))
        ((error quit)
         (ende-job-fail job caught)))))))

(defun ende-process-next ()
  "Process all currently admissible queued jobs."
  (unless ende--dispatching
    (let ((ende--dispatching t)
          (done nil))
      (while (and (not ende--queue-suspended)
                  (not ende--aborting)
                  (not done))
        (let ((cell (ende--next-admissible-cell)))
          (if cell
              (ende--dispatch-job (ende--queue-remove-cell cell))
            (setq done t)))))))

;;;; Synchronization fences

(defun ende--create-fence (&optional provider)
  "Create a fence for PROVIDER, or for all providers when nil."
  (let* ((fence (ende--make-fence
                 :provider provider
                 :cutoff ende--next-job-id))
         (pending (cl-count-if
                   (lambda (job)
                     (ende--fence-prerequisite-p fence job))
                   (append ende--active-jobs ende-queue))))
    (push fence ende--fences)
    (ende-process-next)
    (message "ende: %s fence created for %d existing job%s"
             (if provider (format "provider %s" provider) "global")
             pending
             (if (= pending 1) "" "s"))
    fence))

(defun ende-fence-all ()
  "Prevent jobs queued after this command from crossing existing jobs."
  (interactive)
  (ende--create-fence))

(defun ende-fence-provider (provider)
  "Prevent later PROVIDER jobs from crossing existing PROVIDER jobs.
When called interactively, prompt for one of the configured providers."
  (interactive
   (list
    (intern
     (completing-read
      "Provider fence: "
      (mapcar #'symbol-name (ende-provider-names))
      nil t))))
  (unless (symbolp provider)
    (user-error "Provider fence requires a provider name"))
  (ende--create-fence provider))

;;;; Cancellation and status

(defun ende--job-cancel (job)
  "Mark JOB cancelled and clean its local state without dispatching."
  (unless (ende-job-terminal-p job)
    (setf (ende-job-state job) 'cancelled
          (ende-job-finished-at job) (float-time))
    (ende--unregister-source-job job)
    (ende--invoke-cancellation (ende-job-cancel-function job))
    (ende-ignore-errors #'ende-target-release (ende-job-target job))
    (ende-job-emit-event job '(:type :cancelled))
    (ende-job-emit-event job '(:type :settled :state cancelled))))

(defun ende--source-buffer-killed ()
  "Cancel all queued and active jobs targeting the current source buffer."
  (let* ((was-aborting ende--aborting)
         (jobs (copy-sequence ende--source-buffer-jobs))
         (active (cl-intersection jobs ende--active-jobs :test #'eq))
         (pending (cl-intersection jobs ende-queue :test #'eq)))
    ;; Remove jobs from scheduler state before invoking transport cancellation;
    ;; cancellation callbacks may run synchronously.
    (setq ende--source-buffer-jobs nil
          ende--aborting t)
    (unwind-protect
        (progn
          (dolist (job pending)
            (let ((cell (memq job ende-queue)))
              (when cell
                (ende--queue-remove-cell cell))))
          (setq ende--active-jobs
                (cl-set-difference ende--active-jobs active :test #'eq)
                ende--current-job (car ende--active-jobs))
          (dolist (job (append active pending))
            (ende--job-cancel job)))
      (setq ende--aborting was-aborting))
    (unless was-aborting
      (ende-process-next))))

(defun ende-abort-queue ()
  "Abort the active request, cancel pending jobs, and clear overlays."
  (interactive)
  (let* ((active ende--active-jobs)
         (pending ende-queue)
         (jobs (append active pending)))
    ;; Invalidate callbacks before invoking transport cancellation because a
    ;; transport may settle reentrantly.
    (setq ende--aborting t)
    (unwind-protect
        (progn
          (setq ende-queue nil)
          (setq ende--queue-tail nil)
          (setq ende--active-jobs nil)
          (setq ende--current-job nil)
          (dolist (job jobs)
            (ende--job-cancel job))
          (dolist (function ende-queue-abort-functions)
            (ende-ignore-errors function)))
      (setq ende--aborting nil)
      (setq ende--fences nil)
      (message "ende: queue aborted"))))

(defun ende-queue-status ()
  "Display and return a nonvisual snapshot of queue state."
  (interactive)
  (let ((status (list :running (and ende--active-jobs t)
                      :active (and ende--current-job
                                   (ende-job-id ende--current-job))
                      :active-jobs (mapcar #'ende-job-id ende--active-jobs)
                      :pending (length ende-queue)
                      :fences (length ende--fences))))
    (message "ende: active=%s pending=%d fences=%d"
             (or (plist-get status :active) "none")
             (plist-get status :pending)
             (plist-get status :fences))
    status))

(provide 'ende-queue)

;;; ende-queue.el ends here
