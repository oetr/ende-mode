;;; ende-queue.el --- FIFO job queue and execution engine for ende -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools

;;; Commentary:

;; FIFO queue management, job identity, lifecycle transitions, and process
;; cancellation for ende.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'ende-core)
(require 'ende-operation)
(require 'ende-provider)
(require 'ende-codex)
(require 'ende-target)
(require 'ende-work-buffer)

;;;; Job records

(defun ende--invoke-cancellation (cancel-function)
  "Invoke CANCEL-FUNCTION without allowing cleanup failures to escape."
  (when (functionp cancel-function)
    (ende-ignore-errors cancel-function)))

(cl-defstruct (ende-job-family (:constructor ende--make-job-family))
  "A top-level job and its contiguous scheduler region."
  root parallel max-concurrency policy-frozen-p inference-template registry origin
  queue-marker queue-tail members archived-p origin-name source-marker source-file
  source-line question-id request-id)

(cl-defstruct (ende-job (:constructor ende--make-job))
  "A queued LLM operation and its asynchronous lifecycle state."
  id type state cancel-function overlay target profile inference-template instruction
  metadata metadata-rank result error structured-state
  enqueued-at started-at finished-at dispatch-order status latest-user
  last-usage reasoning-summary-item reasoning-summary-text
  tool-call-count agent-tool-rounds activity-title activity-status activity-updated-at
  pending-guidance family parent children schedule-path next-child-index
  resume-function execution-generation access-policy active-spawn-batch
  scheduling-overrides owned-buffer owned-buffer-hook delegated-settlement
  title work-buffer request-phase title-attempts title-generated-p)

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

(defcustom ende-queue-finished-limit 30
  "Maximum completed top-level job trees retained in this Emacs session."
  :type 'natnum :group 'ende)

(defvar ende--finished-families nil
  "Retained terminal families, newest completion first.")

(defvar ende--retained-jobs (make-hash-table :test #'eql)
  "Published retained jobs indexed by stable ID.")

(defvar ende--settlement-depth 0
  "Nesting depth of atomic settlement batches.")

(defvar ende--settled-families nil
  "Families awaiting final retention after the outer settlement batch.")

(defun ende-queue-retained-job-by-id (id)
  "Return the published retained job with ID."
  (gethash id ende--retained-jobs))

(defun ende--retain-published-job (job)
  "Register successfully published JOB in its family's display history."
  (let ((family (ende-job-family job)))
    (setf (ende-job-family-members family)
          (nconc (ende-job-family-members family) (list job)))
    (puthash (ende-job-id job) job ende--retained-jobs)))

(defun ende-queue--source-killed ()
  "Save navigation and display metadata before the current source dies."
  (maphash
   (lambda (_id job)
     (let ((family (ende-job-family job)))
       (when (and (eq job (ende-job-family-root family))
                  (eq (current-buffer) (ende-job-family-origin family)))
         (setf (ende-job-family-origin-name family) (buffer-name)
               (ende-job-family-source-file family) buffer-file-name
               (ende-job-family-source-line family)
               (line-number-at-pos (ende-job-family-source-marker family) t)))))
   ende--retained-jobs))

(put 'ende-queue--source-killed 'permanent-local-hook t)

(defun ende-queue--track-source (family)
  "Capture FAMILY's root navigation metadata."
  (let* ((root (ende-job-family-root family))
         (target (ende-job-target root))
         (buffer (ende-job-family-origin family))
         (instruction (ende-job-instruction root)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-restriction
          (widen)
          (let ((position (or (plist-get instruction :question-marker)
                              (ende-target-cursor target) (point-min))))
            (setf (ende-job-family-origin-name family) (buffer-name)
                  (ende-job-family-source-marker family) (copy-marker position t)
                  (ende-job-family-source-file family) buffer-file-name
                  (ende-job-family-source-line family) (line-number-at-pos position t)
                  (ende-job-family-question-id family) (plist-get instruction :question-id)
                  (ende-job-family-request-id family) (plist-get instruction :request-id))))
        (add-hook 'kill-buffer-hook #'ende-queue--source-killed -60 t)))))

(defun ende-queue-evict-family (family)
  "Release the complete terminal FAMILY, without touching user-owned sources."
  (unless (ende-job-terminal-p (ende-job-family-root family))
    (error "Cannot evict an active family"))
  (let ((buffer (ende-job-family-origin family))
        (ids (mapcar #'ende-job-id (ende-job-family-members family))))
    (setq ende--finished-families (delq family ende--finished-families))
    (dolist (job (ende-job-family-members family))
      (remhash (ende-job-id job) ende--retained-jobs)
      (ende--job-release-owned-buffer job)
      (when (buffer-live-p (ende-job-work-buffer job))
        (kill-buffer (ende-job-work-buffer job)))
      (setf (ende-job-work-buffer job) nil
            (ende-job-target job) nil (ende-job-overlay job) nil
            (ende-job-family job) nil (ende-job-parent job) nil
            (ende-job-children job) nil (ende-job-profile job) nil
            (ende-job-inference-template job) nil (ende-job-access-policy job) nil
            (ende-job-instruction job) nil (ende-job-result job) nil
            (ende-job-error job) nil (ende-job-structured-state job) nil
            (ende-job-cancel-function job) nil (ende-job-resume-function job) nil
            (ende-job-delegated-settlement job) nil (ende-job-active-spawn-batch job) nil
            (ende-job-pending-guidance job) nil (ende-job-latest-user job) nil
            (ende-job-last-usage job) nil (ende-job-metadata job) nil
            (ende-job-reasoning-summary-text job) nil
            (ende-job-reasoning-summary-item job) nil
            (ende-job-activity-title job) nil (ende-job-activity-status job) nil))
    (when (markerp (ende-job-family-source-marker family))
      (set-marker (ende-job-family-source-marker family) nil))
    (setf (ende-job-family-members family) nil (ende-job-family-root family) nil
          (ende-job-family-origin family) nil (ende-job-family-source-marker family) nil
          (ende-job-family-inference-template family) nil
          (ende-job-family-registry family) nil
          (ende-job-family-queue-marker family) nil (ende-job-family-queue-tail family) nil)
    (when (buffer-live-p buffer)
      (unless (cl-some (lambda (job) (eq (ende-job-origin-buffer job) buffer))
                       (hash-table-values ende--retained-jobs))
        (with-current-buffer buffer
          (remove-hook 'kill-buffer-hook #'ende-queue--source-killed t))))
    (ende-queue-emit-event (list :type :evicted :ids ids))))

(defun ende-queue--finalize-retention ()
  "Archive terminal families once, then trim whole oldest trees."
  (let ((families (prog1 ende--settled-families
                    (setq ende--settled-families nil))))
    (dolist (family families)
      (when (and family (ende-job-terminal-p (ende-job-family-root family))
                 (not (ende-job-family-archived-p family)))
        (setf (ende-job-family-archived-p family) t)
        (push family ende--finished-families))))
  (while (> (length ende--finished-families) (max 0 ende-queue-finished-limit))
    (ende-queue-evict-family (car (last ende--finished-families)))))

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
  (let ((buffer (and (null (ende-job-parent job))
                     (ende-job-family job)
                     (eq job (ende-job-family-root (ende-job-family job)))
                     (ende-job-origin-buffer job))))
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

(defun ende-job-live-p (job)
  "Return non-nil when JOB is in a live scheduler state."
  (and (ende-job-p job)
       (memq (ende-job-state job)
             '(queued running suspended completed failed cancelled skipped))
       (not (ende-job-terminal-p job))))

(defun ende-job-current-p (job)
  "Non-nil when JOB is the active, still-settleable job."
  (and (memq job ende--active-jobs)
       (eq (ende-job-state job) 'running)
       (not ende--aborting)))

(defun ende-job-execution-current-p (job generation)
  "Return non-nil when JOB is running in worker GENERATION."
  (and (ende-job-current-p job)
       (= (or (ende-job-execution-generation job) 0) generation)))

(defun ende-job-cancellation-set (job cancel-function &optional generation expected)
  "Associate CANCEL-FUNCTION with JOB in GENERATION.
When EXPECTED is non-nil, update only if it is still JOB's cancellation."
  (let ((current (ende-job-cancel-function job)))
    (if (and (ende-job-execution-current-p
              job (or generation (ende-job-execution-generation job) 0))
             (or (null expected) (eq current expected)))
        (setf (ende-job-cancel-function job) cancel-function)
      (when cancel-function
        (ende--invoke-cancellation cancel-function))))
  cancel-function)

(defun ende-job-can-accept-guidance-p (job)
  "Return non-nil when live, started chat JOB can accept user guidance."
  (and (eq (ende-job-type job) 'chat)
       (or (ende-job-current-p job)
           (and (ende-job-live-p job)
                (ende-job-started-at job)
                (memq (ende-job-state job) '(suspended queued))))))

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

(defun ende--job-release-worker (job)
  "Release JOB's provider worker without releasing its target or source."
  (setq ende--active-jobs (delq job ende--active-jobs)))

(defun ende--job-register-origin (job)
  "Register root JOB for cancellation when its origin buffer is killed."
  (let ((buffer (and (null (ende-job-parent job))
                     (ende-job-family job)
                     (eq job (ende-job-family-root (ende-job-family job)))
                     (ende-job-origin-buffer job))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (cl-pushnew job ende--source-buffer-jobs :test #'eq)
        (add-hook 'kill-buffer-hook #'ende--source-buffer-killed -50 t))))
  job)

(defun ende--job-release-current (job)
  "Release JOB from the active set and release its target."
  (ende--job-release-worker job)
  (ende--unregister-source-job job)
  (ende-ignore-errors #'ende-target-release (ende-job-target job)))

;;;; Queue state

(defvar ende-queue nil
  "Pending jobs and fence entries in their authoritative scheduler order.")

(defvar ende-queue-paused nil
  "Non-nil when user-requested pause prevents new jobs from starting.")

(defun ende-running-jobs ()
  "Return a snapshot of jobs currently consuming workers."
  (copy-sequence ende--active-jobs))

(defun ende-active-jobs ()
  "Return a snapshot of currently active jobs."
  (ende-running-jobs))

(defun ende-queued-jobs ()
  "Return a snapshot of queued jobs awaiting scheduler dispatch."
  (copy-sequence
   (cl-remove-if-not
    (lambda (job) (eq (ende-job-state job) 'queued))
    (cl-remove-if-not #'ende-job-p ende-queue))))

(defun ende-physical-waiting-jobs ()
  "Return queued and suspended jobs in physical scheduler order."
  (copy-sequence
   (cl-remove-if-not
    (lambda (job) (memq (ende-job-state job) '(queued suspended)))
    (cl-remove-if-not #'ende-job-p ende-queue))))

(defun ende-pending-jobs ()
  "Return a snapshot of queued jobs, excluding structural entries."
  (ende-queued-jobs))

(defun ende-queue-awaiting-dispatch ()
  "Return queued jobs currently eligible for scheduler consideration."
  (ende-queued-jobs))

(defun ende-queue-suspended-jobs ()
  "Return suspended jobs in physical scheduler order."
  (cl-remove-if-not
   (lambda (job) (eq (ende-job-state job) 'suspended))
   (ende-physical-waiting-jobs)))

(defun ende-all-live-jobs ()
  "Return a snapshot of all live jobs in stable active and waiting order."
  (cl-delete-duplicates
   (append (ende-running-jobs) (ende-physical-waiting-jobs)) :test #'eq))

(defun ende-job-family-roots ()
  "Return live top-level job representatives in scheduler order."
  (let (roots)
    (dolist (job (ende-all-live-jobs))
      (let ((root (or (and (ende-job-family job)
                           (ende-job-family-root (ende-job-family job)))
                      job)))
        (cl-pushnew root roots :test #'eq)))
    (nreverse roots)))

(defun ende-job-origin-buffer (job)
  "Return JOB's user-facing family origin buffer."
  (or (and (ende-job-family job) (ende-job-family-origin (ende-job-family job)))
      (ende-target-buffer (ende-job-target job))))

(defun ende-queue-family-markers ()
  "Return family markers in global scheduler order."
  (cl-remove-if-not #'ende-job-family-p ende-queue))

(defun ende-queue-entries ()
  "Return queued jobs and fences in scheduler order, excluding family markers."
  (copy-sequence (cl-remove-if #'ende-job-family-p ende-queue)))

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
                       (ende-physical-waiting-jobs)))))

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

(defun ende-job-report-activity (job title status &optional generation)
  "Report validated TITLE and STATUS for the originating agent JOB.
Return non-nil while JOB is current, including for an unchanged report.
Stale reports do nothing.  Transport status and scheduler state are untouched."
  (when (ende-job-execution-current-p
         job (or generation (ende-job-execution-generation job) 0))
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
    (:reasoning-summary
     (let* ((item (plist-get event :item))
            (text (plist-get event :text))
            (previous
             (and (equal item (ende-job-reasoning-summary-item job))
                  (ende-job-reasoning-summary-text job))))
       (when (stringp text)
         (setf (ende-job-reasoning-summary-item job) item
               (ende-job-reasoning-summary-text job)
               (if (plist-get event :replace)
                   text
                 (concat previous text))))))
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
    (setq ende--queue-tail cell)
    (when (ende-job-family-p entry)
      (setf (ende-job-family-queue-marker entry) cell
            (ende-job-family-queue-tail entry) cell))))

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
        (when (ende-job-family-p entry)
          (setf (ende-job-family-queue-marker entry) nil
                (ende-job-family-queue-tail entry) nil))
        (when (ende-job-p entry)
          (let ((family (ende-job-family entry)))
            (when (and family (eq (ende-job-family-queue-tail family) cell))
              (setf (ende-job-family-queue-tail family)
                    previous))))
        (when (eq cell ende--queue-tail)
          (setq ende--queue-tail previous))
        entry))))

(defun ende--queue-sync-tail ()
  "Refresh the queue tail after a structural queue mutation."
  (setq ende--queue-tail (last ende-queue)))

(defun ende--queue-refresh-family-hints ()
  "Refresh family marker and tail cells after rebuilding queue conses."
  (let ((family nil))
    (cl-loop for cell on ende-queue do
      (let ((entry (car cell)))
        (cond
         ((ende-job-family-p entry)
          (setq family entry)
          (setf (ende-job-family-queue-marker family) cell
                (ende-job-family-queue-tail family) cell))
         ((and family (ende-job-p entry)
               (eq (ende-job-family entry) family))
          (setf (ende-job-family-queue-tail family) cell))
         (t (setq family nil)))))))

(defun ende--schedule-path-before-p (left right)
  "Return non-nil when numeric path LEFT precedes RIGHT lexicographically."
  (let ((left-tail left)
        (right-tail right)
        result)
    (while (and left-tail right-tail (null result))
      (cond
       ((< (car left-tail) (car right-tail)) (setq result t))
       ((> (car left-tail) (car right-tail)) (setq result 'after))
       (t (setq left-tail (cdr left-tail)
                right-tail (cdr right-tail)))))
    (cond
     ((eq result 'after) nil)
     (result t)
     ((null left-tail) right-tail)
     (t nil))))

(defun ende--queue-insert-family-member (job)
  "Insert queued JOB into its family's contiguous path-ordered region."
  (let* ((family (ende-job-family job))
         (marker (and family
                      (or (ende-job-family-queue-marker family)
                          (memq family ende-queue))))
         (tail (and family (ende-job-family-queue-tail family)))
         (previous marker)
         (cell (and marker (cdr marker)))
         (path (ende-job-schedule-path job)))
    (unless marker
      (error "job family marker is not present"))
    (setf (ende-job-family-queue-marker family) marker)
    (if (and tail (ende-job-p (car tail))
             (not (ende--schedule-path-before-p
                   path (ende-job-schedule-path (car tail)))))
        (setq previous tail cell (cdr tail))
      (while (and cell
                  (ende-job-p (car cell))
                  (eq (ende-job-family (car cell)) family)
                  (not (ende--schedule-path-before-p
                        path (ende-job-schedule-path (car cell)))))
        (setq previous cell
              cell (cdr cell))))
    (let ((inserted (cons job cell)))
      (setcdr previous inserted)
      (when (or (eq tail previous) (null cell))
        (setf (ende-job-family-queue-tail family) inserted))
      (unless cell
        (setq ende--queue-tail inserted)))
    job))

(defun ende--queue-maintain-families ()
  "Remove markers for settled roots with no live family members."
  (let ((changed nil)
        (live (ende-all-live-jobs)))
    (setq ende-queue
          (cl-remove-if
           (lambda (entry)
             (when (and (ende-job-family-p entry)
                        (let ((root (ende-job-family-root entry)))
                          (and (ende-job-terminal-p root)
                               (not (cl-some
                                     (lambda (job)
                                       (eq (ende-job-family job) entry))
                                     live)))))
               (setq changed t)))
           ende-queue))
    (ende--queue-refresh-family-hints)
    (when changed
      (ende--queue-sync-tail)
      (ende-queue-emit-event '(:type :families)))))

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
      (ende--queue-refresh-family-hints)
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

(defun ende--family-scheduling-policy (job)
  "Return JOB's effective family scheduling policy without freezing it."
  (let* ((family (ende-job-family job))
         (profile (ende-job-profile job))
         (overrides (ende-job-scheduling-overrides job))
         (parallel-default
          (let ((runtime (ende-provider--runtime-value
                          (plist-get profile :name) :parallel)))
            (if (eq runtime :unset)
                (ende-provider-parallel-enabled-p profile)
              (eq runtime t))))
         (limit-default (ende-provider-max-concurrency profile)))
    (if (and family (ende-job-family-policy-frozen-p family))
        (list :parallel (ende-job-family-parallel family)
              :max-concurrency (ende-job-family-max-concurrency family))
      (list :parallel
            (if (plist-member overrides :parallel)
                (plist-get overrides :parallel)
              parallel-default)
            :max-concurrency
            (if (plist-member overrides :max-concurrency)
                (plist-get overrides :max-concurrency)
              limit-default)))))

(defun ende--freeze-family-policy (job)
  "Freeze JOB's effective scheduling policy on its family's first start."
  (let ((family (ende-job-family job)))
    (when (and family (not (ende-job-family-policy-frozen-p family)))
      (let ((policy (ende--family-scheduling-policy job)))
        (setf (ende-job-family-parallel family) (plist-get policy :parallel)
              (ende-job-family-max-concurrency family)
              (plist-get policy :max-concurrency)
              (ende-job-family-policy-frozen-p family) t)))
    family))

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
    (when (eq (ende-job-state job) 'suspended)
      (push (list :kind 'suspended) blockers))
    (when (eq (ende-job-state job) 'queued)
      (dolist (entry ende-queue)
        (cond
         ((eq entry job) (setq before nil))
         ((and before (ende-job-p entry)
               (eq (ende-job-state entry) 'queued)
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
             (policy (ende--family-scheduling-policy job))
             (parallel (eq (plist-get policy :parallel) t))
             (limit (plist-get policy :max-concurrency)))
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
                 (when (and (ende-job-p job)
                            (eq (ende-job-state job) 'queued))
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
         (target (plist-get data :target))
         (prepare (plist-get data :prepare)))
    (unless (or (ende-target-p target)
                (and (eq type 'agent) (functionp prepare)))
      (error "ende request is missing a target"))
    (let* ((buffer (and (ende-target-p target)
                        (ende-target-buffer target)))
           (profile
            (copy-tree
             (or (plist-get data :profile)
                 (and (plist-member data :provider)
                      (ende-provider-profile-snapshot
                       (plist-get data :provider)))
                 (and (buffer-live-p buffer)
                      (ende-provider-profile-for-buffer buffer t)))))
           (now (float-time))
           (overrides
            (append (when (plist-member data :parallel)
                      (list :parallel (plist-get data :parallel)))
                    (when (plist-member data :max-concurrency)
                      (list :max-concurrency
                            (plist-get data :max-concurrency)))))
           (template (copy-tree profile))
           (job (ende--make-job
                 :inference-template template
                 :scheduling-overrides overrides
                 :id (cl-incf ende--next-job-id)
                 :type type
                 :state 'queued
                 :overlay (plist-get data :overlay)
                 :target target
                 :profile profile
                 :access-policy (copy-tree (plist-get data :access-policy))
                 :instruction (plist-get data :instruction)
                 :enqueued-at now)))
      (setf (ende-job-title job)
            (ende-queue--fallback-title type target (ende-job-instruction job)))
      (unless profile
        (error "ende request is missing a provider profile"))
      (condition-case caught
          (progn
            (when prepare
              (funcall prepare job))
            (unless (and (ende-target-p (ende-job-target job))
                         (ende-target-live-p (ende-job-target job)))
              (error "ende request preparation did not create a live target"))
            (let ((profile
                   (plist-put (copy-tree (ende-job-profile job))
                              :job-id (ende-job-id job))))
              (when (and (eq (plist-get profile :wire-api) 'responses)
                         (not (eq (plist-get profile :type) 'openai)))
                (setq profile
                      (plist-put
                       profile :codex-session-id
                       (or (plist-get profile :codex-session-id)
                           (ende-codex--new-session-id)))))
              (setf (ende-job-profile job) profile))
            job)
        ((error quit)
         (ende--job-cleanup-unpublished job)
         (signal (car caught) (cdr caught)))))))

(defun ende-queue--fallback-title (type target instruction)
  "Return a short local label for TYPE, TARGET, and INSTRUCTION."
  (let ((text (or (plist-get instruction :task) (symbol-name type))))
    (when (and (eq type 'chat) (ende-target-p target)
               (buffer-live-p (ende-target-buffer target)))
      (with-current-buffer (ende-target-buffer target)
        (save-excursion
          (goto-char (or (plist-get instruction :question-marker)
                         (ende-target-cursor target) (point-min)))
          (let ((heading (and (derived-mode-p 'org-mode)
                              (not (org-before-first-heading-p))
                              (org-get-heading t t t t))))
            (if (and heading (not (string-blank-p heading)))
                (setq text heading)
              (when heading (org-end-of-meta-data t))
              (setq text (buffer-substring-no-properties
                          (point) (min (point-max) (+ (point) 240)))))))))
    (truncate-string-to-width
     (string-trim (replace-regexp-in-string "[[:space:]]+" " " text))
     60 nil nil "…")))

(defun ende--job-cleanup-unpublished (job)
  "Release target and private buffer resources for unpublished JOB."
  (when (ende-target-p (ende-job-target job))
    (ende-ignore-errors #'ende-target-release (ende-job-target job)))
  (when-let* ((buffer (ende--job-release-owned-buffer job)))
    (kill-buffer buffer))
  (when (buffer-live-p (ende-job-work-buffer job))
    (kill-buffer (ende-job-work-buffer job)))
  (setf (ende-job-work-buffer job) nil)
  job)

(defun ende--job-release-owned-buffer (job)
  "Detach JOB's private-buffer ownership and return its surviving buffer."
  (let ((buffer (ende-job-owned-buffer job))
        (hook (ende-job-owned-buffer-hook job)))
    (setf (ende-job-owned-buffer job) nil
          (ende-job-owned-buffer-hook job) nil)
    (when (and hook (buffer-live-p buffer))
      (with-current-buffer buffer
        (remove-hook 'kill-buffer-hook hook t)))
    (and (buffer-live-p buffer) buffer)))

(defun ende-enqueue (request)
  "Add REQUEST to the queue and trigger processing if idle.
Return the newly created `ende-job'."
  (let* ((job (ende--job-from-request request))
         (family (ende--make-job-family
                  :root job
                  :inference-template (ende-job-inference-template job)
                  :origin (ende-target-buffer (ende-job-target job))
                  :queue-marker nil :queue-tail nil))
         (published nil))
    (unwind-protect
        (progn
    (setf (ende-job-family job) family
          (ende-job-family-queue-marker family) nil
          (ende-job-family-queue-tail family) nil
          (ende-job-schedule-path job) nil
          (ende-job-next-child-index job) 0)
    (ende--job-register-origin job)
    (ende--queue-push family)
    (ende--queue-insert-family-member job)
    (ende--retain-published-job job)
    (ende-queue--track-source family)
    (setq published t)
    (ende-job-emit-event
     job
     (list :type :queued
           :client (ende-provider-client-name (ende-job-profile job))
           :model (plist-get (ende-job-profile job) :model)))
    (ende-process-next)
    job)
      (unless published
        (ende--unregister-source-job job)
        (setq ende-queue (delq family (delq job ende-queue)))
        (ende--queue-refresh-family-hints)
        (ende--queue-sync-tail)
        (remhash (ende-job-id job) ende--retained-jobs)
        (when (markerp (ende-job-family-source-marker family))
          (set-marker (ende-job-family-source-marker family) nil))
        (setf (ende-job-family-members family) nil)
        (ende--job-cleanup-unpublished job)))))

(defun ende-job-next-child-path (parent)
  "Reserve and return the next never-reused numeric child path for PARENT."
  (unless (and (ende-job-p parent)
               (ende-job-family parent)
               (ende-job-live-p parent)
               (ende-job-family-policy-frozen-p (ende-job-family parent)))
    (error "agent children require a live job in a started family"))
  (setf (ende-job-next-child-index parent)
        (1+ (or (ende-job-next-child-index parent) 0)))
  (append (copy-sequence (ende-job-schedule-path parent))
          (list (ende-job-next-child-index parent))))

(defun ende-job-publish-child (parent request schedule-path)
  "Publish prepared REQUEST as PARENT's child at SCHEDULE-PATH.

The family marker, queue region, and child ownership are updated before
scheduler callbacks are permitted to observe the child."
  (unless (and (ende-job-p parent) (ende-job-live-p parent)
               (ende-job-family parent)
               (equal (butlast schedule-path)
                      (ende-job-schedule-path parent))
               (integerp (car (last schedule-path)))
               (= (car (last schedule-path))
                  (ende-job-next-child-index parent))
               (> (car (last schedule-path)) 0)
               (ende-job-family-policy-frozen-p (ende-job-family parent)))
    (error "invalid parent or scheduling path for delegated child"))
  (let* ((job (ende--job-from-request request))
         (family (ende-job-family parent))
         (published nil))
    (unwind-protect
        (progn
          (setf (ende-job-family job) family
                (ende-job-family-queue-marker family)
                (or (ende-job-family-queue-marker family)
                    (memq family ende-queue))
                (ende-job-parent job) parent
                (ende-job-schedule-path job) (copy-sequence schedule-path)
                (ende-job-next-child-index job) 0
                (ende-job-access-policy job)
                (copy-tree (plist-get (cdr request) :access-policy))
                (ende-job-delegated-settlement job)
                (plist-get (cdr request) :delegated-settlement)
                (ende-job-children parent)
                (append (ende-job-children parent) (list job)))
          (ende--queue-insert-family-member job)
          (ende--retain-published-job job)
          (ende-job-emit-event
           job (list :type :queued
                     :client (ende-provider-client-name (ende-job-profile job))
                     :model (plist-get (ende-job-profile job) :model)))
          (setq published t)
          job)
      (unless published
        (remhash (ende-job-id job) ende--retained-jobs)
        (setf (ende-job-family-members family)
              (delq job (ende-job-family-members family)))
        (setq ende-queue (delq job ende-queue))
        (ende--queue-sync-tail)
        (setf (ende-job-children parent)
              (delq job (ende-job-children parent)))
        (ende--job-cleanup-unpublished job)))))

;;;; Settlement and dispatch

(defun ende--job-live-subtree (roots)
  "Return live ROOTS and descendants in parent-before-child order."
  (let ((pending (copy-sequence roots))
        (seen nil)
        result)
    (while pending
      (let ((job (pop pending)))
        (when (and (ende-job-live-p job) (not (memq job seen)))
          (push job seen)
          (push job result)
          (setq pending (nconc (copy-sequence (ende-job-children job))
                               pending)))))
    (nreverse result)))

(defun ende--job-terminal-event (job state result error)
  "Emit lifecycle events for JOB's terminal STATE."
  (pcase state
    ('completed
     (ende-job-emit-event
      job (list :type :done :result result
                :content (if (stringp result) result
                           (and (listp result) (plist-get result :content))))))
    ('failed
     (ende-job-emit-event
      job (append (list :type :error :error error)
                  (when result (list :result result)))))
    ('cancelled (ende-job-emit-event job '(:type :cancelled)))
    ('skipped
     (ende-job-emit-event job (list :type :skipped :error error))))
  (ende-job-emit-event job (list :type :settled :state state)))

(defun ende--job-terminalize (roots state result error)
  "Terminalize ROOTS and their owned descendants atomically."
  (unless (memq state '(completed failed cancelled skipped))
    (error "invalid terminal job state: %s" state))
  (let* ((was-suspended ende--queue-suspended)
         (ende--settlement-depth (1+ ende--settlement-depth))
         (selected (cl-delete-duplicates (copy-sequence roots) :test #'eq))
         (jobs (ende--job-live-subtree selected))
         (now (float-time))
         cancellations)
    (when (and (eq state 'completed)
               (cl-some (lambda (job) (ende-job-children job)) selected))
      (error "job cannot complete while it owns live descendants"))
    (when jobs
      (let ((ende--queue-suspended t))
        (dolist (job jobs)
          (push (cons job (ende-job-cancel-function job)) cancellations)
          (setf (ende-job-state job)
                (if (memq job selected) state 'cancelled)
                (ende-job-result job) (and (memq job selected) result)
                (ende-job-error job)
                (if (memq job selected) error
                  (ende-error-create :kind 'cancelled
                                     :message "owning job settled"))
                (ende-job-finished-at job) now
                (ende-job-resume-function job) nil
                (ende-job-cancel-function job) nil
                (ende-job-active-spawn-batch job) nil))
        (setq ende--active-jobs
              (cl-set-difference ende--active-jobs jobs :test #'eq)
              ende-queue
              (cl-remove-if
               (lambda (entry) (and (ende-job-p entry) (memq entry jobs)))
               ende-queue))
        (ende--queue-refresh-family-hints)
        (dolist (job jobs)
          (when-let* ((parent (ende-job-parent job)))
            (setf (ende-job-children parent)
                  (delq job (ende-job-children parent))))
          (setf (ende-job-children job) nil))
        (ende--queue-sync-tail)
        (dolist (job jobs)
          (ende--unregister-source-job job)
          (ende-ignore-errors #'ende-target-release (ende-job-target job)))
        (let (settlement-failures)
          (dolist (job jobs)
            (let ((settlement (ende-job-delegated-settlement job)))
              (setf (ende-job-delegated-settlement job) nil)
              (when settlement
                (condition-case caught
                    (funcall settlement job)
                  ((error quit)
                   (when-let* ((parent (ende-job-parent job)))
                     (when (ende-job-live-p parent)
                       (push (cons parent caught) settlement-failures))))))))
          (dolist (failure settlement-failures)
            (let ((failure-job (car failure)))
              (when (ende-job-live-p failure-job)
                (ende--job-terminalize
                 (list failure-job) 'failed nil
                 (ende-error-create
                  :kind 'ownership
                  :message (format "delegated settlement failed: %s"
                                   (error-message-string (cdr failure)))))))))
        (ende--queue-maintain-families)
        (ende--queue-maintain-fences)
        (dolist (job jobs)
          (ende--job-terminal-event
           job (ende-job-state job) (ende-job-result job) (ende-job-error job))
          (ende--job-release-owned-buffer job)
          (cl-pushnew (ende-job-family job) ende--settled-families :test #'eq))
        (ende-queue-emit-event (list :type state))
        (dolist (entry cancellations)
          (ende--invoke-cancellation (cdr entry))))
      (when (= ende--settlement-depth 1)
        (ende-queue--finalize-retention))
      (unless (or ende--aborting was-suspended)
        (ende-process-next)))
    jobs))

(defun ende--job-settle (job state &optional result error)
  "Settle live JOB exactly once with STATE, RESULT, and ERROR."
  (ende--job-terminalize (list job) state result error))

(defun ende-job-suspend (job resume-function)
  "Release JOB's worker and queue it as suspended with RESUME-FUNCTION."
  (unless (and (ende-job-current-p job) (functionp resume-function))
    (error "cannot suspend job outside a running execution"))
  (let ((was-suspended ende--queue-suspended))
    (let ((ende--queue-suspended t))
      (setf (ende-job-resume-function job) resume-function
            (ende-job-state job) 'suspended
            (ende-job-status job) "Suspended")
      (ende--job-release-worker job)
      (ende--queue-insert-family-member job)
      (ende-job-emit-event job '(:type :suspended))
      (ende-queue-emit-event '(:type :suspended)))
    (unless (or was-suspended ende--aborting)
      (ende-process-next)))
  job)

(defun ende-job-resume-queued (job)
  "Make suspended JOB queued at the same family/path position."
  (when (and (ende-job-live-p job)
             (eq (ende-job-state job) 'suspended))
    (setf (ende-job-state job) 'queued
          (ende-job-status job) "Waiting to resume")
    (ende-job-emit-event job '(:type :queued-for-resumption))
    (ende-queue-emit-event '(:type :resumption-ready))
    (unless (or ende--queue-suspended ende--aborting)
      (ende-process-next)))
  job)

(defun ende-job-finish (job &optional result)
  "Complete running JOB with RESULT and advance the queue."
  (when (ende-job-current-p job)
    (condition-case caught
        (ende--job-settle job 'completed result nil)
      (error
       (ende-job-fail
        job (ende-error-create :kind 'ownership
                               :message (error-message-string caught)))))))

(defun ende-job-fail (job error &optional result)
  "Fail live JOB with ERROR and advance the queue.
When RESULT is non-nil, retain it for history and job inspection."
  (when (ende-job-live-p job)
    (let ((id (ende-job-id job)))
      (ende--job-settle job 'failed result error)
      (condition-case nil
          (message "ende: job %d failed: %s"
                   id (ende-error-display-message error))
        ((error quit) nil)))))

(defun ende-job-skip (job message)
  "Skip live JOB with MESSAGE and advance the queue."
  (when (ende-job-live-p job)
    (let ((id (ende-job-id job)))
      (ende--job-settle job 'skipped nil
                        (ende-error-create :kind 'skipped :message message))
      (message "ende: skipped job %d (%s)" id message))))

(defun ende--dispatch-job (job)
  "Start or resume JOB and route scheduler failures to settlement."
  (let* ((type (ende-job-type job))
         (handler (alist-get type ende--job-handlers))
         (target (ende-job-target job))
         (resuming (and (ende-job-started-at job) t))
         (continuation (and resuming (ende-job-resume-function job)))
         (generation (1+ (or (ende-job-execution-generation job) 0))))
    (ende--freeze-family-policy job)
    (push job ende--active-jobs)
    (setf (ende-job-state job) 'running
          (ende-job-started-at job) (or (ende-job-started-at job) (float-time))
          (ende-job-dispatch-order job) (cl-incf ende--next-dispatch-order)
          (ende-job-execution-generation job) generation
          (ende-job-status job) "Running")
    (ende-job-emit-event job (if resuming '(:type :resumed) '(:type :started)))
    (cond
     ((and resuming (not (functionp continuation)))
      (ende-job-fail
       job (ende-error-create :kind 'scheduler
                              :message "started job has no resume continuation")))
     ((not (ende-target-live-p target))
      (ende-job-skip
       job (format "%s: target buffer no longer exists" type)))
     ((and (not resuming) (null handler))
      (ende-job-fail
       job (ende-error-create
            :kind 'scheduler
            :message (format "unknown job handler: %s" type))))
     (t
      (condition-case caught
          (if resuming
              (progn
                (setf (ende-job-resume-function job) nil)
                (funcall continuation))
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
         (jobs (let (roots)
                 (dolist (job (append ende--active-jobs
                                      (ende-physical-waiting-jobs)))
                   (when (ende--fence-applies-p fence job)
                     (let ((root (or (and (ende-job-family job)
                                          (ende-job-family-root
                                           (ende-job-family job)))
                                     job)))
                       (cl-pushnew root roots :test #'eq))))
                 (nreverse roots)))
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
  (cl-find id (ende-all-live-jobs) :key #'ende-job-id :test #'eql))

(defun ende-queue-change-policy (ids policy)
  "Set queued jobs named by IDS to POLICY, then dispatch once if allowed.

POLICY is `parallel' or `sequential'.  All IDs are validated before any profile
is changed, making a batch update atomic with respect to scheduler dispatch."
  (unless (memq policy '(parallel sequential))
    (user-error "Unsupported concurrency policy: %s" policy))
  (let ((jobs (mapcar #'ende-queue-live-job-by-id ids)))
    (unless (and jobs
                 (cl-every #'identity jobs)
                 (cl-every
                  (lambda (job)
                    (let ((family (ende-job-family job)))
                      (and (eq (ende-job-state job) 'queued)
                           (null (ende-job-started-at job))
                           (null (ende-job-parent job))
                           (or (null family)
                               (and (eq job (ende-job-family-root family))
                                    (not (ende-job-family-policy-frozen-p
                                          family)))))))
                  jobs))
      (user-error "Selected jobs changed; policy was not updated"))
    (dolist (job jobs)
      (setf (ende-job-profile job)
            (plist-put (copy-tree (ende-job-profile job))
                       :parallel (eq policy 'parallel))
            (ende-job-scheduling-overrides job)
            (plist-put (copy-sequence (ende-job-scheduling-overrides job))
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

(defun ende--queue-segments ()
  "Return the queue as ordered family, fence, and orphan-job segments."
  (let (segments current)
    (dolist (entry ende-queue)
      (cond
       ((ende-queue-fence-p entry)
        (when current (push (nreverse current) segments) (setq current nil))
        (push (list entry) segments))
       ((ende-job-family-p entry)
        (when current (push (nreverse current) segments))
        (setq current (list entry)))
       ((and current (ende-job-p entry))
        (push entry current))
       (t
        (when current (push (nreverse current) segments) (setq current nil))
        (push (list entry) segments))))
    (when current (push (nreverse current) segments))
    (nreverse segments)))

(defun ende-queue-move (id direction)
  "Move never-started top-level job ID across one family or fence."
  (unless (memq direction '(-1 1))
    (error "Direction must be -1 or 1"))
  (let* ((job (ende-queue-live-job-by-id id))
         (family (and job (ende-job-family job)))
         (segments (ende--queue-segments))
         (index (cl-position-if (lambda (segment)
                                  (eq (car segment) family))
                                segments))
         (other-index (and index (+ index direction))))
    (unless (and job family (eq job (ende-job-family-root family))
                 (null (ende-job-parent job))
                 (null (ende-job-started-at job))
                 (eq (ende-job-state job) 'queued)
                 (memq family ende-queue))
      (user-error "Only never-started top-level jobs can be moved"))
    (unless (and other-index (>= other-index 0)
                 (< other-index (length segments)))
      (user-error "Job is already at that end of the queue"))
    (let ((segment (nth index segments))
          (other (nth other-index segments)))
      (setf (nth index segments) other
            (nth other-index segments) segment))
    (setq ende-queue (apply #'append segments))
    (ende--queue-update-moved-job-fences job)
    (ende--queue-sync-tail)
    (ende--queue-refresh-family-hints)
    (ende--queue-maintain-fences)
    (ende-queue-emit-event '(:type :reordered))
    (ende-process-next)
    job))

;;;; Cancellation and status

(defun ende--job-cancel (job)
  "Cancel JOB through the atomic scheduler cancellation path."
  (ende--cancel-live-jobs (list job)))

(defun ende--cancel-live-jobs (jobs)
  "Cancel JOBS and their owned descendants in one scheduler transaction."
  (ende--job-terminalize jobs 'cancelled nil
                         (ende-error-create :kind 'cancelled
                                            :message "job cancelled")))

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
  (let ((was-aborting ende--aborting))
    (setq ende--aborting t)
    (unwind-protect
        (progn
          (ende--job-terminalize
           (ende-all-live-jobs) 'cancelled nil
           (ende-error-create :kind 'cancelled :message "queue aborted"))
          (dolist (function ende-queue-abort-functions)
            (ende-ignore-errors function)))
      (setq ende--aborting was-aborting)
      (ende-queue-emit-event '(:type :aborted))
      (message "ende: queue aborted"))))

(defun ende-queue-status ()
  "Display and return a nonvisual snapshot of queue state."
  (interactive)
  (let* ((active (car ende--active-jobs))
         (status (list :running (and ende--active-jobs t)
                      :active (and active (ende-job-id active))
                      :active-jobs (mapcar #'ende-job-id ende--active-jobs)
                      :pending (length (ende-queued-jobs))
                      :suspended (length (ende-queue-suspended-jobs))
                      :fences (length (ende-queue-fences))
                      :paused ende-queue-paused)))
    (message "ende: active=%s pending=%d fences=%d"
             (or (plist-get status :active) "none")
             (plist-get status :pending)
             (plist-get status :fences))
    status))

(provide 'ende-queue)

;;; ende-queue.el ends here
