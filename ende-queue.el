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
  enqueued-at started-at finished-at dispatch-order status latest-user
  last-usage tool-call-count activity-title activity-status activity-updated-at
  pending-guidance)

(cl-defstruct (ende-queue-fence (:constructor ende--make-fence))
  "An ordered queue barrier with an explicit set of unfinished predecessors."
  id provider jobs)

(defun ende-job-latest-user-set (job text)
  "Retain a bounded inert latest-user excerpt TEXT on JOB."
  (when (stringp text)
    (setf (ende-job-latest-user job)
          (substring-no-properties text 0 (min 1200 (length text))))))

(defvar ende--next-job-id 0
  "Next numeric identifier assigned to a job.")

(defvar ende--next-dispatch-order 0
  "Next scheduler dispatch sequence assigned to a running job.")

(defvar ende--active-jobs nil
  "Active jobs, in reverse dispatch order.")

(defvar ende--next-fence-id 0
  "Next numeric identifier assigned to a queue fence.")

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

(defun ende-job-can-accept-guidance-p (job)
  "Return non-nil when running chat JOB can accept user guidance."
  (and (ende-job-current-p job)
       (eq (ende-job-type job) 'chat)))

(defun ende-job-pending-guidance-p (job)
  "Return non-nil when JOB has guidance awaiting a chat boundary."
  (consp (ende-job-pending-guidance job)))

(defun ende-queue-add-guidance (job text)
  "Append exact TEXT to running chat JOB's pending guidance FIFO."
  (unless (ende-job-can-accept-guidance-p job)
    (user-error "Selected job cannot accept guidance"))
  (unless (and (stringp text)
               (not (string-match-p "\\`[[:space:]]*\\'" text)))
    (user-error "Injected prompt must not be empty"))
  (setf (ende-job-pending-guidance job)
        (nconc (ende-job-pending-guidance job) (list text)))
  text)

(defun ende-job-take-pending-guidance (job)
  "Atomically take and clear JOB's pending guidance in insertion order."
  (prog1 (ende-job-pending-guidance job)
    (setf (ende-job-pending-guidance job) nil)))

(defun ende--job-release-current (job)
  "Release JOB from the active set and release its target."
  (setq ende--active-jobs (delq job ende--active-jobs))
  (ende--unregister-source-job job)
  (ende-ignore-errors #'ende-target-release (ende-job-target job)))

;;;; Queue state

(defvar ende-queue nil
  "Pending jobs and fence entries in their authoritative scheduler order.")

(defvar ende-queue-paused nil
  "Non-nil when user-requested pause prevents new jobs from starting.")

(defun ende-active-jobs ()
  "Return a snapshot of currently active jobs.

The returned list may be changed by the caller without changing scheduler
state."
  (copy-sequence ende--active-jobs))

(defun ende-pending-jobs ()
  "Return a snapshot of pending jobs, excluding fence entries."
  (copy-sequence (cl-remove-if-not #'ende-job-p ende-queue)))

(defun ende-queue-entries ()
  "Return a structural snapshot of pending jobs and fences in scheduler order."
  (copy-sequence ende-queue))

(defun ende-queue-fences ()
  "Return fences in scheduler order without changing scheduler state."
  (copy-sequence (cl-remove-if-not #'ende-queue-fence-p ende-queue)))

(defun ende-queue-fence-unfinished-predecessors (fence)
  "Return FENCE's unfinished predecessors without changing scheduler state."
  (let ((members (cl-remove-if #'ende-job-terminal-p
                               (ende-queue-fence-jobs fence))))
    (append
     (cl-remove-if-not
      (lambda (job) (memq job members))
      (sort (ende-active-jobs)
            (lambda (left right)
              (< (or (ende-job-dispatch-order left) (ende-job-id left))
                 (or (ende-job-dispatch-order right) (ende-job-id right))))))
     (cl-remove-if-not (lambda (job) (memq job members))
                       (ende-pending-jobs)))))

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

(defvar ende-queue-event-functions nil
  "Functions called with one queue-wide scheduler event.")

(defvar ende-queue-abort-functions nil
  "Functions called after all queued jobs have been invalidated.")

(defun ende-job-report-activity (job title status)
  "Report validated TITLE and STATUS for the originating agent JOB.
Return non-nil while JOB is current, including for an unchanged report.
Stale reports do nothing.  Transport status and scheduler state are untouched."
  (when (ende-job-current-p job)
    (unless (and (equal title (ende-job-activity-title job))
                 (equal status (ende-job-activity-status job)))
      (ende-job-emit-event
       job (list :type :agent-activity :title title :status status
                 :updated-at (float-time))))
    t))

(defun ende-job-emit-event (job event)
  "Send EVENT for JOB to lifecycle observers without propagating failures."
  (pcase (plist-get event :type)
    (:agent-activity
     (setf (ende-job-activity-title job) (plist-get event :title)
           (ende-job-activity-status job) (plist-get event :status)
           (ende-job-activity-updated-at job) (plist-get event :updated-at)))
    (:status
     (when (stringp (plist-get event :status))
       (setf (ende-job-status job) (plist-get event :status))))
    (:usage
     (when (plist-get event :usage)
       (setf (ende-job-last-usage job) (plist-get event :usage))))
    (:tool-call-count
     (let ((count (plist-get event :count)))
       (when (and (integerp count) (>= count 0))
         (setf (ende-job-tool-call-count job) count))))
    (:begin
     (unless (ende-job-latest-user job)
       (let ((messages (plist-get event :messages))
             latest)
         (dolist (entry messages)
           (when (and (consp entry) (equal (car entry) "user")
                      (stringp (cdr entry)))
             (setq latest (cdr entry))))
         (when latest
           ;; Retain only a bounded inert excerpt, never a duplicate transcript.
           (ende-job-latest-user-set job latest)))))
    (_ nil))
  (dolist (function ende-job-event-functions)
    (ende-ignore-errors function job event)))

(defun ende-queue-emit-event (event)
  "Send queue-wide EVENT to observers without propagating failures."
  (dolist (function ende-queue-event-functions)
    (ende-ignore-errors function event)))

(defun ende-register-job-handler (type handler)
  "Register HANDLER function for jobs of symbol TYPE.
HANDLER receives one `ende-job' object."
  (setf (alist-get type ende--job-handlers) handler))

(defun ende--queue-push (entry)
  "Append queue ENTRY in constant time."
  (let ((cell (list entry)))
    (if ende--queue-tail
        (setcdr ende--queue-tail cell)
      (setq ende-queue cell))
    (setq ende--queue-tail cell)))

(defun ende--queue-pop ()
  "Pop and return the oldest queue entry."
  (let ((entry (car ende-queue)))
    (setq ende-queue (cdr ende-queue))
    (when (null ende-queue)
      (setq ende--queue-tail nil))
    entry))

(defun ende--queue-remove-cell (cell)
  "Remove queue CELL and return its entry."
  (let ((entry (car cell)))
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
        entry))))

(defun ende--queue-sync-tail ()
  "Refresh the queue tail after a structural queue mutation."
  (setq ende--queue-tail (last ende-queue)))

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
  "Return non-nil when FENCE's scope includes JOB's provider."
  (or (null (ende-queue-fence-provider fence))
      (equal (ende-queue-fence-provider fence)
             (ende--job-provider-key job))))

(defun ende--fence-unfinished-p (fence)
  "Return non-nil when FENCE still has unsettled predecessors."
  (cl-some (lambda (job) (not (ende-job-terminal-p job)))
           (ende-queue-fence-jobs fence)))

(defun ende--queue-maintain-fences ()
  "Discard settled predecessors and remove fences with no remaining work."
  (let ((changed nil)
        entries)
    (dolist (entry ende-queue)
      (if (ende-queue-fence-p entry)
          (let ((jobs (cl-remove-if #'ende-job-terminal-p
                                    (ende-queue-fence-jobs entry))))
            (unless (= (length jobs) (length (ende-queue-fence-jobs entry)))
              (setf (ende-queue-fence-jobs entry) jobs)
              (setq changed t))
            (if jobs
                (push entry entries)
              (setq changed t)))
        (push entry entries)))
    (when changed
      (setq ende-queue (nreverse entries))
      (ende--queue-sync-tail)
      (ende-queue-emit-event '(:type :fences)))))

(defun ende--queue-fence-blockers (job)
  "Return unsettled fences positioned before queued JOB that apply to it."
  (let ((before t)
        blockers)
    (dolist (entry ende-queue)
      (cond
       ((eq entry job) (setq before nil))
       ((and before (ende-queue-fence-p entry)
             (ende--fence-applies-p entry job)
             (ende--fence-unfinished-p entry))
        (push entry blockers))))
    (nreverse blockers)))

(defun ende-job-explanation (job)
  "Return a pure readiness explanation for queued JOB.

The result plist contains `:ready' and ordered `:blockers'.  A blocker is a
plist whose `:kind' is `fence', `behind', `buffer', or `provider'.  Global
pause is intentionally excluded: readiness describes whether JOB could start
if user pause were ignored."
  (let ((blockers nil)
        (provider (ende--job-provider-key job))
        (before t)
        earlier-provider)
    (when (eq (ende-job-state job) 'queued)
      (dolist (entry ende-queue)
        (cond
         ((eq entry job) (setq before nil))
         ((and before (ende-job-p entry)
               (equal provider (ende--job-provider-key entry)))
          (unless earlier-provider (setq earlier-provider entry)))))
      (dolist (fence (ende--queue-fence-blockers job))
        (push (list :kind 'fence :fence fence) blockers))
      (when earlier-provider
        (push (list :kind 'behind :job earlier-provider) blockers))
      (when (ende--buffer-write-locked-p job)
        (push (list :kind 'buffer
                    :jobs (cl-remove-if-not
                           (lambda (active)
                             (and (ende--write-job-p active)
                                  (ende--same-buffer-p job active)
                                  (or (ende-operation-whole-buffer-p
                                       (ende-job-type job))
                                      (ende-operation-whole-buffer-p
                                       (ende-job-type active)))))
                           ende--active-jobs))
              blockers))
      (let* ((active (ende--active-provider-jobs job))
             (profile (ende-job-profile job))
             (parallel (eq (plist-get profile :parallel) t))
             (limit (ende-provider-max-concurrency profile)))
        (when (if parallel
                  (>= (length active) limit)
                active)
          (push (list :kind 'provider :jobs active) blockers))))
    (setq blockers (nreverse blockers))
    (list :ready (null blockers) :blockers blockers)))

(defun ende--job-admissible-p (job)
  "Return non-nil when JOB may start under all scheduler constraints."
  (plist-get (ende-job-explanation job) :ready))

(defun ende--next-admissible-cell ()
  "Return the first scheduler entry whose provider can accept its job.

Only the oldest queued job for each provider is considered, so it cannot be
bypassed by a later same-provider job."
  (let ((seen-providers nil))
    (catch 'found
      (cl-loop for cell on ende-queue do
               (let ((job (car cell)))
                 (when (ende-job-p job)
                   (let ((provider (ende--job-provider-key job)))
                     (unless (cl-member provider seen-providers :test #'equal)
                       (push provider seen-providers)
                       (when (ende--job-admissible-p job)
                         (throw 'found cell)))))))
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
    (let ((ende--queue-suspended t))
      (ende-job-emit-event job (list :type :settled :state state))
      (ende--job-release-current job)
      (ende--queue-maintain-fences)
      (ende-queue-emit-event '(:type :settled))
      ;; A transport's local cleanup may call back synchronously or enqueue work.
      (ende--invoke-cancellation (ende-job-cancel-function job)))
    (unless (or ende--aborting ende--queue-suspended)
      (ende-process-next))))

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
    (setf (ende-job-state job) 'running
          (ende-job-started-at job) (float-time)
          (ende-job-dispatch-order job) (cl-incf ende--next-dispatch-order)
          (ende-job-status job) "Running")
    (ende-job-emit-event job '(:type :started))
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
  (unless (or ende--dispatching ende-queue-paused)
    (let ((ende--dispatching t))
      (ende--queue-maintain-fences)
      (let ((done nil))
        (while (and (not ende-queue-paused)
                    (not ende--queue-suspended)
                    (not ende--aborting)
                    (not done))
          (let ((cell (ende--next-admissible-cell)))
            (if cell
                (let ((job (ende--queue-remove-cell cell)))
                  (ende--queue-sync-tail)
                  (ende--dispatch-job job))
              (setq done t))))))))

;;;; Synchronization fences

(defun ende--create-fence (&optional provider)
  "Create a fence for PROVIDER, or for all providers when nil."
  (let* ((fence (ende--make-fence
                 :provider provider))
         (jobs (cl-remove-if-not
                (lambda (job)
                  (and (ende--fence-applies-p fence job)
                       (not (ende-job-terminal-p job))))
                (append ende--active-jobs (ende-pending-jobs))))
         (pending (length jobs)))
    (setf (ende-queue-fence-jobs fence) jobs)
    (when (> pending 0)
      (setf (ende-queue-fence-id fence) (cl-incf ende--next-fence-id))
      (ende--queue-push fence)
      (ende--queue-sync-tail)
      (ende-queue-emit-event '(:type :fences)))
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

;;;; Dashboard scheduler operations

(defun ende-queue-pause ()
  "Persistently pause new scheduler starts."
  (interactive)
  (unless ende-queue-paused
    (setq ende-queue-paused t)
    (ende-queue-emit-event '(:type :paused)))
  ende-queue-paused)

(defun ende-queue-resume ()
  "Resume scheduling and start all currently eligible jobs."
  (interactive)
  (when ende-queue-paused
    (setq ende-queue-paused nil)
    (ende-queue-emit-event '(:type :resumed)))
  (ende-process-next)
  ende-queue-paused)

(defun ende-queue-live-job-by-id (id)
  "Return the live job with stable ID, or nil."
  (cl-find id (append ende--active-jobs (ende-pending-jobs))
           :key #'ende-job-id :test #'eql))

(defun ende-queue-change-policy (ids policy)
  "Set queued jobs named by IDS to POLICY, then dispatch once if allowed.

POLICY is `parallel' or `wait-idle'.  All IDs are validated before any profile
is changed, making a batch update atomic with respect to scheduler dispatch."
  (unless (memq policy '(parallel wait-idle))
    (user-error "Unsupported concurrency policy: %s" policy))
  (let ((jobs (mapcar #'ende-queue-live-job-by-id ids)))
    (unless (and jobs
                 (cl-every #'identity jobs)
                 (cl-every (lambda (job) (eq (ende-job-state job) 'queued))
                           jobs))
      (user-error "Selected jobs changed; policy was not updated"))
    (dolist (job jobs)
      (setf (ende-job-profile job)
            (plist-put (copy-tree (ende-job-profile job))
                       :parallel (eq policy 'parallel))))
    (dolist (job jobs)
      (ende-job-emit-event job (list :type :policy :policy policy)))
    (ende-queue-emit-event '(:type :policy))
    (ende-process-next)
    jobs))

(defun ende--queue-entry-position-before-p (job fence)
  "Return non-nil when queued JOB occurs before FENCE."
  (let ((before t)
        found)
    (dolist (entry ende-queue)
      (cond
       ((eq entry job) (setq found before))
       ((eq entry fence) (setq before nil))))
    found))

(defun ende--queue-update-moved-job-fences (job)
  "Update scoped fence membership for moved queued JOB."
  (dolist (fence (ende-queue-fences))
    (when (ende--fence-applies-p fence job)
      (if (ende--queue-entry-position-before-p job fence)
          (cl-pushnew job (ende-queue-fence-jobs fence) :test #'eq)
        (setf (ende-queue-fence-jobs fence)
              (delq job (ende-queue-fence-jobs fence)))))))

(defun ende-queue-move (id direction)
  "Move queued job ID one scheduler entry by DIRECTION (-1 or 1).

The adjacent entry may be another job or a fence.  Fence positions and
membership are updated before eligible work is dispatched."
  (unless (memq direction '(-1 1))
    (error "Direction must be -1 or 1"))
  (let* ((job (ende-queue-live-job-by-id id))
         (cell (and job (memq job ende-queue)))
         (other (and cell (if (> direction 0) (cdr cell)
                            (and (not (eq cell ende-queue))
                                 (let ((previous ende-queue))
                                   (while (not (eq (cdr previous) cell))
                                     (setq previous (cdr previous)))
                                   previous))))))
    (unless (and job (eq (ende-job-state job) 'queued) cell)
      (user-error "Job is no longer queued"))
    (unless other
      (user-error "Job is already at that end of the queue"))
    (if (> direction 0)
        (let ((entry (car other)))
          (setcar other job)
          (setcar cell entry))
      (let ((entry (car cell)))
        (setcar cell (car other))
        (setcar other entry)))
    (ende--queue-update-moved-job-fences job)
    (ende--queue-sync-tail)
    (ende--queue-maintain-fences)
    (ende-queue-emit-event '(:type :reordered))
    (ende-process-next)
    job))

;;;; Cancellation and status

(defun ende--job-cancel (job)
  "Cancel JOB through the atomic scheduler cancellation path."
  (ende--cancel-live-jobs (list job)))

(defun ende--cancel-live-jobs (jobs)
  "Atomically invalidate live JOBS, then invoke cleanup and dispatch once."
  (let* ((active (copy-sequence ende--active-jobs))
         (pending (ende-pending-jobs))
         (members (append active pending))
         (was-suspended ende--queue-suspended)
         (live (cl-remove-if-not
                (lambda (job)
                  (and (ende-job-p job)
                       (not (ende-job-terminal-p job))
                       (memq job members)))
                (cl-delete-duplicates (copy-sequence jobs) :test #'eq)))
         (remaining (cl-set-difference active live :test #'eq)))
    (when live
      ;; Hold dispatch through lifecycle notifications and cancellation
      ;; callbacks; the complete selected set is already invalid before either.
      (let ((ende--queue-suspended t))
        (setq ende-queue
              (cl-remove-if
               (lambda (entry) (and (ende-job-p entry) (memq entry live)))
               ende-queue)
              ende--active-jobs remaining)
        (let ((now (float-time)))
          (dolist (job live)
            (setf (ende-job-state job) 'cancelled
                  (ende-job-finished-at job) now)))
        (ende--queue-sync-tail)
        (dolist (job live)
          (ende--unregister-source-job job)
          (ende-ignore-errors #'ende-target-release (ende-job-target job)))
        (dolist (job live)
          (ende-job-emit-event job '(:type :cancelled))
          (ende-job-emit-event job '(:type :settled :state cancelled)))
        (ende--queue-maintain-fences)
        (ende-queue-emit-event '(:type :cancelled))
        (dolist (job live)
          (ende--invoke-cancellation (ende-job-cancel-function job))))
      (unless (or ende--aborting was-suspended)
        (ende-process-next)))
    live))

(defun ende-queue-cancel-ids (ids)
  "Cancel live jobs identified by IDS as one dispatch-atomic mutation."
  (let ((jobs (mapcar #'ende-queue-live-job-by-id ids)))
    (unless (and jobs (cl-every #'identity jobs))
      (user-error "Selected jobs changed; nothing was cancelled"))
    (ende--cancel-live-jobs jobs)))

(defun ende--source-buffer-killed ()
  "Cancel all queued and active jobs targeting the current source buffer."
  (let ((jobs (copy-sequence ende--source-buffer-jobs)))
    (setq ende--source-buffer-jobs nil)
    (ende--cancel-live-jobs jobs)))

(defun ende-abort-queue ()
  "Abort the active request, cancel pending jobs, and clear overlays."
  (interactive)
  (let* ((was-aborting ende--aborting)
         (active (copy-sequence ende--active-jobs))
         (pending (ende-pending-jobs))
         (jobs (append active pending)))
    ;; Invalidate callbacks before invoking transport cancellation because a
    ;; transport may settle reentrantly.
    (setq ende--aborting t)
    (unwind-protect
        (progn
          (setq ende-queue nil)
          (setq ende--queue-tail nil)
          (setq ende--active-jobs nil)
          (let ((now (float-time)))
            (dolist (job jobs)
              (unless (ende-job-terminal-p job)
                (setf (ende-job-state job) 'cancelled
                      (ende-job-finished-at job) now))))
          (dolist (job jobs)
            (unless (memq (ende-job-state job) '(completed failed skipped))
              (ende--unregister-source-job job)
              (ende-ignore-errors #'ende-target-release (ende-job-target job))
              (ende-job-emit-event job '(:type :cancelled))
              (ende-job-emit-event job '(:type :settled :state cancelled))))
          (let ((ende--queue-suspended t))
            (dolist (job jobs)
              (when (eq (ende-job-state job) 'cancelled)
                (ende--invoke-cancellation (ende-job-cancel-function job)))))
          (dolist (function ende-queue-abort-functions)
            (ende-ignore-errors function)))
      (setq ende--aborting was-aborting)
      (setq ende-queue nil)
      (setq ende--queue-tail nil)
      (ende-queue-emit-event '(:type :aborted))
      (message "ende: queue aborted"))))

(defun ende-queue-status ()
  "Display and return a nonvisual snapshot of queue state."
  (interactive)
  (let* ((active (car ende--active-jobs))
         (status (list :running (and ende--active-jobs t)
                      :active (and active (ende-job-id active))
                      :active-jobs (mapcar #'ende-job-id ende--active-jobs)
                      :pending (length (ende-pending-jobs))
                      :fences (length (ende-queue-fences))
                      :paused ende-queue-paused)))
    (message "ende: active=%s pending=%d fences=%d"
             (or (plist-get status :active) "none")
             (plist-get status :pending)
             (plist-get status :fences))
    status))

(provide 'ende-queue)

;;; ende-queue.el ends here
