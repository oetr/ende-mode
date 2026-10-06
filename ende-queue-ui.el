;;; ende-queue-ui.el --- Live scheduler dashboard for ende -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools

;;; Commentary:

;; Read-only Jobs and Buffers views over the scheduler's authoritative queue.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'ende-queue)

(defface ende-queue-ui-heading-face
  '((t :inherit font-lock-keyword-face))
  "Face for dashboard headings."
  :group 'ende)

(defface ende-queue-ui-fence-face
  '((t :inherit font-lock-constant-face))
  "Face for compact fence entries."
  :group 'ende)

(defface ende-queue-ui-mark-face
  '((t :inherit font-lock-warning-face))
  "Face for marked jobs."
  :group 'ende)

(defconst ende-queue-ui-buffer-name "*ende-queue*"
  "Name of the live scheduler dashboard buffer.")

(defvar ende-queue-ui--buffer nil
  "The current dashboard buffer, if it exists.")

(defvar ende-queue-ui--marked-ids (make-hash-table :test #'eql)
  "Job IDs marked for batch dashboard operations.")

(defvar ende-queue-ui--expanded-jobs (make-hash-table :test #'eql)
  "Job IDs whose additional details are expanded.")

(defvar ende-queue-ui--expanded-fences (make-hash-table :test #'eql)
  "Fence IDs whose predecessor lists are expanded.")

(defvar ende-queue-ui--folded-buffers (make-hash-table :test #'eq)
  "Source buffers whose groups are folded.")

(defvar ende-queue-ui--buffer-order nil
  "Stable order in which source buffers first appeared in the dashboard.")

(defvar-local ende-queue-ui-view 'jobs
  "Current view in the dashboard buffer: `jobs' or `buffers'.")

(defvar-local ende-queue-ui--refresh-timer nil
  "Coalesced dashboard refresh timer.")

(defvar-local ende-queue-ui--clock-timer nil
  "Periodic elapsed-time refresh timer.")

(defvar-local ende-queue-ui--rendering nil
  "Non-nil while the dashboard buffer is being rebuilt.")

(defun ende-queue-ui--live-jobs ()
  "Return all current jobs in stable active and actual pending order."
  (append
   (sort (ende-active-jobs)
         (lambda (left right)
           (< (or (ende-job-dispatch-order left) (ende-job-id left))
              (or (ende-job-dispatch-order right) (ende-job-id right)))))
   (ende-pending-jobs)))

(defun ende-queue-ui--prune-ui-state ()
  "Drop marks and expansions for jobs that left the live scheduler."
  (let ((live (mapcar #'ende-job-id (ende-queue-ui--live-jobs)))
        (live-fences (mapcar #'ende-queue-fence-id (ende-queue-fences)))
        stale)
    (maphash (lambda (id _value)
               (unless (memq id live)
                 (push id stale)))
             ende-queue-ui--marked-ids)
    (dolist (id stale)
      (remhash id ende-queue-ui--marked-ids)
      (remhash id ende-queue-ui--expanded-jobs))
    (setq stale nil)
    (maphash (lambda (id _value)
               (unless (memq id live)
                 (push id stale)))
             ende-queue-ui--expanded-jobs)
    (dolist (id stale)
      (remhash id ende-queue-ui--expanded-jobs))
    (setq stale nil)
    (maphash (lambda (id _value)
               (unless (memq id live-fences)
                 (push id stale)))
             ende-queue-ui--expanded-fences)
    (dolist (id stale)
      (remhash id ende-queue-ui--expanded-fences))))

(defun ende-queue-ui--marked-count ()
  "Return the number of currently live marked jobs."
  (hash-table-count ende-queue-ui--marked-ids))

(defun ende-queue-ui--queue-event (_event)
  "Schedule a coalesced refresh after a queue-wide scheduler change."
  (ende-queue-ui--schedule-refresh))

(defun ende-queue-ui--job-event (_job event)
  "Schedule a dashboard refresh for relevant lifecycle EVENT on JOB."
  (unless (memq (plist-get event :type) '(:delta :usage))
    (ende-queue-ui--schedule-refresh)))

(defun ende-queue-ui--schedule-refresh ()
  "Schedule one refresh for the live dashboard, coalescing event bursts."
  (let ((buffer (and (buffer-live-p ende-queue-ui--buffer)
                     ende-queue-ui--buffer)))
    (when (and buffer
               (not (buffer-local-value 'ende-queue-ui--rendering buffer))
               (not (timerp
                     (buffer-local-value 'ende-queue-ui--refresh-timer buffer))))
      (with-current-buffer buffer
        (setq ende-queue-ui--refresh-timer
              (run-at-time 0.08 nil #'ende-queue-ui--refresh-callback buffer))))))

(defun ende-queue-ui--refresh-callback (buffer)
  "Refresh BUFFER if it still displays the dashboard."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq ende-queue-ui--refresh-timer nil)
      (when (derived-mode-p 'ende-queue-ui-mode)
        (if (get-buffer-window buffer t)
            (ende-queue-ui--render)
          (ende-queue-ui--detach-observers))))))

(defun ende-queue-ui--clock (buffer)
  "Refresh elapsed times in visible dashboard BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (if (get-buffer-window buffer t)
          (ende-queue-ui--render)
        (when (timerp ende-queue-ui--clock-timer)
          (cancel-timer ende-queue-ui--clock-timer)
          (setq ende-queue-ui--clock-timer nil)
          (ende-queue-ui--detach-observers))))))

(defun ende-queue-ui--attach-observers ()
  "Subscribe this dashboard to scheduler and lifecycle notifications."
  (add-hook 'ende-job-event-functions #'ende-queue-ui--job-event)
  (add-hook 'ende-queue-event-functions #'ende-queue-ui--queue-event))

(defun ende-queue-ui--detach-observers ()
  "Unsubscribe this dashboard from scheduler notifications."
  (remove-hook 'ende-job-event-functions #'ende-queue-ui--job-event)
  (remove-hook 'ende-queue-event-functions #'ende-queue-ui--queue-event))

(defun ende-queue-ui--start-clock ()
  "Start a modest periodic refresh while the dashboard is visible."
  (unless (timerp ende-queue-ui--clock-timer)
    (setq ende-queue-ui--clock-timer
          (run-with-timer 5 5 #'ende-queue-ui--clock (current-buffer)))))

(defun ende-queue-ui--cleanup ()
  "Cancel timers and unregister lifecycle observers when the buffer dies."
  (dolist (timer (list ende-queue-ui--refresh-timer ende-queue-ui--clock-timer))
    (when (timerp timer) (cancel-timer timer)))
  (setq ende-queue-ui--refresh-timer nil
        ende-queue-ui--clock-timer nil)
  (ende-queue-ui--detach-observers)
  (when (eq ende-queue-ui--buffer (current-buffer))
    (setq ende-queue-ui--buffer nil)))

(defun ende-queue-ui--line (text &optional properties face)
  "Insert single-line TEXT, attach PROPERTIES, and optionally FACE."
  (let ((start (point)))
    (insert (string-remove-suffix "\n" text))
    (when face
      (add-face-text-property start (point) face t))
    (when properties
      (add-text-properties start (point) properties))
    (insert "\n")))

(defun ende-queue-ui--safe-name (buffer)
  "Return a source BUFFER name suitable for a one-line summary."
  (if (buffer-live-p buffer) (buffer-name buffer) "*killed*"))

(defun ende-queue-ui--provider-name (job)
  "Return JOB's concise provider display name."
  (ende-provider-client-name (ende-job-profile job) (ende-job-metadata job)))

(defun ende-queue-ui--policy (job)
  "Return JOB's editable queue concurrency policy label."
  (if (eq (plist-get (ende-job-profile job) :parallel) t)
      "parallel"
    "wait-idle"))

(defun ende-queue-ui--operation (job)
  "Return JOB's concise operation label."
  (pcase (ende-job-type job)
    ('chat (if (plist-get (ende-job-profile job) :tool-definitions)
               "chat/tools"
             "chat"))
    ('chat-compact "compact")
    (type (symbol-name type))))

(defun ende-queue-ui--duration (seconds)
  "Format elapsed SECONDS as a compact duration."
  (let ((seconds (max 0 (floor (or seconds 0)))))
    (cond
     ((< seconds 60) (format "%ds" seconds))
     ((< seconds 3600) (format "%dm" (/ seconds 60)))
     ((< seconds 86400) (format "%dh" (/ seconds 3600)))
     (t (format "%dd" (/ seconds 86400))))))

(defun ende-queue-ui--elapsed (job state)
  "Return JOB's run or wait duration label for STATE."
  (let* ((start (if (eq state 'running)
                    (ende-job-started-at job)
                  (ende-job-enqueued-at job)))
         (duration (ende-queue-ui--duration (- (float-time) (or start (float-time)))))
         (label (if (eq state 'running) "run" "wait")))
    (format "%s %s" duration label)))

(defun ende-queue-ui--blocker-label (blocker)
  "Return concise text for readiness BLOCKER."
  (pcase (plist-get blocker :kind)
    ('fence (format "Fence F%d"
                    (ende-queue-fence-id (plist-get blocker :fence))))
    ('behind (format "Behind #%d"
                     (ende-job-id (plist-get blocker :job))))
    ('buffer "Buffer busy")
    ('provider "Provider busy")
    (_ "Blocked")))

(defun ende-queue-ui--job-status (job state)
  "Return JOB's concise state/status label for STATE."
  (if (eq state 'running)
      (or (ende-job-status job) "Running")
    (let* ((explanation (ende-job-explanation job))
           (blockers (plist-get explanation :blockers)))
      (if blockers
          (ende-queue-ui--blocker-label (car blockers))
        "Ready"))))

(defun ende-queue-ui--window-width ()
  "Return available dashboard width in display columns."
  (max 48
       (condition-case nil
           (window-body-width (get-buffer-window (current-buffer) t))
         (error (frame-width)))))

(defun ende-queue-ui--columns (view)
  "Return the adaptive column layout for VIEW."
  (let ((width (ende-queue-ui--window-width)))
    (if (>= width 95)
        (if (eq view 'jobs)
            '((id 4 "ID") (buffer 18 "Buffer") (operation 12 "Operation")
              (policy 10 "Policy") (provider 12 "Provider")
              (status 20 "Status") (time 10 "Time"))
          '((state 9 "State") (id 4 "ID") (operation 12 "Operation")
            (policy 10 "Policy") (provider 12 "Provider")
            (status 20 "Status") (time 10 "Time")))
      (if (eq view 'jobs)
          (list (list 'id 4 "ID")
                (list 'buffer (max 8 (min 22 (- width 60))) "Buffer")
                (list 'operation 12 "Operation")
                (list 'status (max 13 (min 24 (- width 48))) "Status")
                (list 'time 9 "Time"))
        (list (list 'state 9 "State") (list 'id 4 "ID")
              (list 'operation 12 "Operation")
              (list 'status (max 13 (min 24 (- width 48))) "Status")
              (list 'time 9 "Time"))))))

(defun ende-queue-ui--fit (text width)
  "Truncate TEXT to display WIDTH and pad it to that width."
  (let* ((plain (replace-regexp-in-string "[\n\r\t]+" " " (or text "")))
         (fitted (truncate-string-to-width plain width 0 nil "…")))
    (concat fitted (make-string (max 0 (- width (string-width fitted))) ?\s))))

(defun ende-queue-ui--column-header (view)
  "Return the fixed-width column header for VIEW."
  (let ((columns (ende-queue-ui--columns view)))
    (concat "   "
            (mapconcat (lambda (column)
                         (ende-queue-ui--fit (nth 2 column) (nth 1 column)))
                       columns " "))))

(defun ende-queue-ui--job-row (job state view)
  "Render JOB in STATE for VIEW and return its one-line summary."
  (let* ((marked (gethash (ende-job-id job) ende-queue-ui--marked-ids))
         (expanded (gethash (ende-job-id job) ende-queue-ui--expanded-jobs))
         (buffer (ende-target-buffer (ende-job-target job)))
         (values (list (cons 'id (number-to-string (ende-job-id job)))
                       (cons 'buffer (ende-queue-ui--safe-name buffer))
                       (cons 'operation (ende-queue-ui--operation job))
                       (cons 'policy (ende-queue-ui--policy job))
                       (cons 'provider (ende-queue-ui--provider-name job))
                       (cons 'state (if (eq state 'running) "Running" "Queued"))
                       (cons 'status (ende-queue-ui--job-status job state))
                       (cons 'time (ende-queue-ui--elapsed job state))))
          (prefix (concat (if marked
                             (propertize "*" 'face 'ende-queue-ui-mark-face)
                           " ")
                         (if expanded "▾ " "▸ ")))
         (summary
          (concat prefix
                  (mapconcat
                   (lambda (column)
                     (ende-queue-ui--fit
                      (alist-get (car column) values) (nth 1 column)))
                   (ende-queue-ui--columns view) " "))))
    (ende-queue-ui--line
      summary
      (list 'ende-queue-ui-kind 'job
            'ende-queue-ui-owner job
            'ende-queue-ui-job-id (ende-job-id job)
            'ende-queue-ui-state state))
    (when expanded
      (ende-queue-ui--job-details job state))))

(defun ende-queue-ui--display-blockers (job)
  "Render useful readiness blockers for queued JOB."
  (let ((blockers (cdr (plist-get (ende-job-explanation job) :blockers))))
    (when blockers
      (ende-queue-ui--detail-line
       (concat "Also blocked by: "
               (mapconcat #'ende-queue-ui--blocker-label blockers ", "))
       job))))

(defun ende-queue-ui--detail-line (text owner)
  "Insert indented TEXT as a detail line owned by OWNER."
  (ende-queue-ui--line
   (concat "    " text)
   (list 'ende-queue-ui-kind 'detail 'ende-queue-ui-owner owner
         'ende-queue-ui-job-id (and (ende-job-p owner) (ende-job-id owner)))))

(defun ende-queue-ui--job-details (job state)
  "Render non-redundant inert details for JOB in STATE."
  (let* ((profile (ende-job-profile job))
         (columns (mapcar #'car (ende-queue-ui--columns ende-queue-ui-view)))
         (model (plist-get profile :model))
         (latest (ende-job-latest-user job)))
    (unless (memq 'policy columns)
      (ende-queue-ui--detail-line
       (format "Policy: %s" (ende-queue-ui--policy job)) job))
    (unless (memq 'provider columns)
      (ende-queue-ui--detail-line
       (format "Provider: %s" (ende-queue-ui--provider-name job)) job))
    (when (and (stringp model) (not (string-empty-p model)))
      (ende-queue-ui--detail-line (format "Model: %s" model) job))
    (when latest
      (let ((lines (seq-take (split-string latest "\n") 3)))
        (when lines
          (ende-queue-ui--detail-line
           (concat "Latest user: " (car lines)) job)
          (dolist (line (cdr lines))
            (ende-queue-ui--detail-line line job)))))
    (when (and (eq state 'queued) (null latest))
      (ende-queue-ui--detail-line
       "Request preview: live source, captured when started" job))
    (when (eq state 'queued)
      (ende-queue-ui--display-blockers job))))

(defun ende-queue-ui--fence-row (fence)
  "Render FENCE at its actual scheduler position."
  (let* ((jobs (ende-queue-fence-unfinished-predecessors fence))
         (ids (mapcar (lambda (job) (format "#%d" (ende-job-id job))) jobs))
         (waiting (if (> (length ids) 3)
                      (format "%d unfinished jobs" (length ids))
                    (if ids (mapconcat #'identity ids ", ") "none")))
         (scope (or (ende-queue-fence-provider fence) "all"))
         (expanded (gethash (ende-queue-fence-id fence)
                            ende-queue-ui--expanded-fences)))
    (ende-queue-ui--line
     (format "#  F%d  %s · waiting for %s"
             (ende-queue-fence-id fence) scope waiting)
     (list 'ende-queue-ui-kind 'fence 'ende-queue-ui-owner fence
           'ende-queue-ui-fence-id (ende-queue-fence-id fence))
     'ende-queue-ui-fence-face)
    (when expanded
      (ende-queue-ui--detail-line
       (format "All unfinished predecessors: %s"
               (if ids (mapconcat #'identity ids ", ") "none"))
       fence))))

(defun ende-queue-ui--section (title entries renderer)
  "Render one section with TITLE, ENTRIES, and entry RENDERER."
  (ende-queue-ui--line title nil 'ende-queue-ui-heading-face)
  (ende-queue-ui--line (ende-queue-ui--column-header ende-queue-ui-view))
  (dolist (entry entries)
    (funcall renderer entry)))

(defun ende-queue-ui--render-jobs ()
  "Render the Jobs view using scheduler order and stable running order."
  (let ((running (cl-remove-if-not
                  (lambda (job) (eq (ende-job-state job) 'running))
                  (ende-queue-ui--live-jobs)))
        (entries (ende-queue-entries)))
    (ende-queue-ui--section
     (format "Running · %d" (length running)) running
     (lambda (job) (ende-queue-ui--job-row job 'running 'jobs)))
    (ende-queue-ui--section
     (format "Queued · %d" (length (ende-pending-jobs))) entries
     (lambda (entry)
       (if (ende-queue-fence-p entry)
           (ende-queue-ui--fence-row entry)
         (ende-queue-ui--job-row entry 'queued 'jobs))))))

(defun ende-queue-ui--buffer-key (job)
  "Return JOB's source buffer key, including a shared dead-buffer sentinel."
  (or (ende-target-buffer (ende-job-target job)) 'ende-queue-ui--killed-buffer))

(defun ende-queue-ui--buffer-jobs ()
  "Return current job groups in a persistent source-buffer order."
  (let ((groups nil))
    (dolist (job (ende-queue-ui--live-jobs))
      (let* ((buffer (ende-queue-ui--buffer-key job))
             (group (assq buffer groups)))
        (unless group
          (push (cons buffer nil) groups)
          (unless (memq buffer ende-queue-ui--buffer-order)
            (setq ende-queue-ui--buffer-order
                  (append ende-queue-ui--buffer-order (list buffer))))
          (setq group (assq buffer groups)))
        (setcdr group (cons job (cdr group)))))
    (dolist (group groups)
      (setcdr group (nreverse (cdr group))))
    (setq ende-queue-ui--buffer-order
          (cl-remove-if
            (lambda (buffer)
              (and (bufferp buffer)
                   (not (buffer-live-p buffer))
                   (not (assq buffer groups))))
           ende-queue-ui--buffer-order))
    (let (stale)
      (maphash (lambda (buffer _value)
                 (unless (assq buffer groups)
                   (push buffer stale)))
               ende-queue-ui--folded-buffers)
      (dolist (buffer stale)
        (remhash buffer ende-queue-ui--folded-buffers)))
    (mapcar (lambda (buffer) (assq buffer groups))
            (cl-remove-if-not (lambda (buffer) (assq buffer groups))
                              ende-queue-ui--buffer-order))))

(defun ende-queue-ui--buffer-group (group)
  "Render one source-buffer GROUP in the Buffers view."
  (let* ((buffer (car group))
         (jobs (cdr group))
         (running (cl-remove-if-not
                   (lambda (job) (eq (ende-job-state job) 'running)) jobs))
         (queued (cl-remove-if-not
                  (lambda (job) (eq (ende-job-state job) 'queued)) jobs))
         (folded (gethash buffer ende-queue-ui--folded-buffers)))
    (ende-queue-ui--line
     (format "Buffer %s · %d running · %d queued"
             (ende-queue-ui--safe-name
              (unless (eq buffer 'ende-queue-ui--killed-buffer) buffer))
             (length running) (length queued))
     (list 'ende-queue-ui-kind 'buffer 'ende-queue-ui-buffer buffer
           'ende-queue-ui-owner buffer)
     'ende-queue-ui-heading-face)
    (ende-queue-ui--line (ende-queue-ui--column-header 'buffers))
    (unless folded
      (dolist (job running)
        (ende-queue-ui--job-row job 'running 'buffers))
      (dolist (job queued)
        (ende-queue-ui--job-row job 'queued 'buffers)))))

(defun ende-queue-ui--render-buffers ()
  "Render jobs grouped by source buffer and a shared fence section."
  (dolist (group (ende-queue-ui--buffer-jobs))
    (ende-queue-ui--buffer-group group))
  (ende-queue-ui--line
   (format "Fences · %d" (length (ende-queue-fences)))
   (list 'ende-queue-ui-kind 'fences)
   'ende-queue-ui-heading-face)
  (dolist (fence (ende-queue-fences))
    (ende-queue-ui--fence-row fence)))

(defun ende-queue-ui--entry-key-at (position)
  "Return the stable entry key at POSITION, or nil for section text."
  (let ((kind (get-text-property position 'ende-queue-ui-kind))
        (owner (get-text-property position 'ende-queue-ui-owner)))
    (pcase kind
      ('job (cons 'job (ende-job-id owner)))
      ('detail (and (ende-job-p owner)
                    (cons 'job (ende-job-id owner))))
      ('fence (cons 'fence (ende-queue-fence-id owner)))
      ('buffer (cons 'buffer (get-text-property position 'ende-queue-ui-buffer)))
      (_ nil))))

(defun ende-queue-ui--position-for-key (key)
  "Return the rendered position for stable entry KEY, or nil."
  (let ((position (point-min)) found)
    (while (and (< position (point-max)) (not found))
      (when (equal key (ende-queue-ui--entry-key-at position))
        (setq found position))
      (setq position (or (next-single-property-change
                          position 'ende-queue-ui-kind nil (point-max))
                         (point-max))))
    found))

(defun ende-queue-ui--entry-positions ()
  "Return positions of rendered job and fence entries."
  (let ((position (point-min)) positions)
    (while (< position (point-max))
      (when (memq (get-text-property position 'ende-queue-ui-kind)
                  '(job fence))
        (push position positions))
      (setq position (or (next-single-property-change
                          position 'ende-queue-ui-kind nil (point-max))
                         (point-max))))
    (nreverse positions)))

(defun ende-queue-ui--entry-index (key position)
  "Return KEY's old entry index, or the number of entries before POSITION."
  (let* ((positions (ende-queue-ui--entry-positions))
         (matching (and key
                        (cl-position-if
                         (lambda (entry-position)
                           (equal key (ende-queue-ui--entry-key-at entry-position)))
                         positions))))
    (or matching
        (cl-count-if (lambda (entry-position) (< entry-position position))
                     positions))))

(defun ende-queue-ui--restore-point (key old-index)
  "Restore point to KEY, or a nearby surviving entry around OLD-INDEX."
  (let ((position (and key (ende-queue-ui--position-for-key key))))
    (unless position
      (let* ((positions (ende-queue-ui--entry-positions))
             (target (max 0 old-index)))
        (setq position
              (or (nth (min target (max 0 (1- (length positions)))) positions)
                  (point-min)))))
    (goto-char position)))

(defun ende-queue-ui--render ()
  "Rebuild the live view while preserving stable selection and window starts."
  (interactive)
  (unless ende-queue-ui--rendering
    (let* ((ende-queue-ui--rendering t)
           (selected-key (ende-queue-ui--entry-key-at (point)))
           (old-index (ende-queue-ui--entry-index selected-key (point)))
           (windows (get-buffer-window-list (current-buffer) nil t))
           (starts (mapcar (lambda (window)
                             (cons window
                                   (with-current-buffer (window-buffer window)
                                     (line-number-at-pos (window-start window)))))
                           windows)))
      (ende-queue-ui--prune-ui-state)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (ende-queue-ui--line
         (format "Queue [%s]  %s  %s%s  v buffers  ? help"
                 (if (eq ende-queue-ui-view 'jobs) "Jobs" "Buffers")
                 (if ende-queue-paused "PAUSED (no new starts)" "ACTIVE")
                 (if ende-queue-paused "R resume" "P pause")
                 (if (> (ende-queue-ui--marked-count) 0)
                     (format "  · %d marked" (ende-queue-ui--marked-count))
                   ""))
         (list 'ende-queue-ui-kind 'header)
         'ende-queue-ui-heading-face)
        (if (eq ende-queue-ui-view 'jobs)
            (ende-queue-ui--render-jobs)
          (ende-queue-ui--render-buffers)))
      (ende-queue-ui--restore-point selected-key old-index)
      (dolist (entry starts)
        (when (window-live-p (car entry))
          (set-window-start
           (car entry)
           (save-excursion
             (goto-char (point-min))
             (forward-line (max 0 (1- (cdr entry))))
             (point))
           t))))))

(defun ende-queue-ui--entry-at-point ()
  "Return (KIND . OWNER) for the current entry, or nil."
  (let* ((position (line-beginning-position))
         (kind (get-text-property position 'ende-queue-ui-kind))
         (owner (get-text-property position 'ende-queue-ui-owner)))
    (when (eq kind 'detail)
      (setq kind (if (ende-job-p owner) 'job 'fence)))
    (and (memq kind '(job fence buffer)) (cons kind owner))))

(defun ende-queue-ui--current-job ()
  "Return the current job owner or signal a user-facing error."
  (let ((entry (ende-queue-ui--entry-at-point)))
    (unless (and entry (eq (car entry) 'job)
                 (ende-job-p (cdr entry)))
      (user-error "Point is not on a job"))
    (cdr entry)))

(defun ende-queue-ui--selected-jobs ()
  "Return marked jobs, or the current job when there are no marks."
  (let ((marked (cl-remove-if-not
                 (lambda (job) (gethash (ende-job-id job)
                                        ende-queue-ui--marked-ids))
                 (ende-queue-ui--live-jobs))))
    (if marked marked (list (ende-queue-ui--current-job)))))

(defun ende-queue-ui--job-counts (jobs)
  "Return (RUNNING . QUEUED) counts for JOBS."
  (cons (cl-count-if (lambda (job) (eq (ende-job-state job) 'running)) jobs)
        (cl-count-if (lambda (job) (eq (ende-job-state job) 'queued)) jobs)))

(defun ende-queue-ui--state-snapshot (jobs)
  "Return stable ID/state pairs for JOBS."
  (mapcar (lambda (job) (cons (ende-job-id job) (ende-job-state job))) jobs))

(defun ende-queue-ui--assert-states-current (snapshot)
  "Reject SNAPSHOT if any selected ID disappeared or changed state."
  (let ((current
         (mapcar (lambda (entry)
                   (let ((job (ende-queue-live-job-by-id (car entry))))
                     (and job (cons (car entry) (ende-job-state job)))))
                 snapshot)))
    (unless (equal snapshot current)
      (user-error "Queue changed; review the selected jobs and try again"))))

(defun ende-queue-ui--confirm-cancel (jobs &optional all)
  "Confirm cancelling JOBS when required, labeling broad cancellation ALL."
  (let* ((counts (ende-queue-ui--job-counts jobs))
         (running (car counts))
         (queued (cdr counts))
         (plural (> (+ running queued) 1)))
    (or (and (= (+ running queued) 0) t)
        (and (not all) (= queued 1) (= running 0) t)
        (yes-or-no-p
         (format "Cancel%s %d running and %d queued job%s? "
                 (if all " all" "") running queued
                 (if plural "s" ""))))))

(defun ende-queue-ui-cancel ()
  "Cancel marked jobs, or the job at point, after any required confirmation."
  (interactive)
  (let* ((jobs (ende-queue-ui--selected-jobs))
         (snapshot (ende-queue-ui--state-snapshot jobs))
         (ids (mapcar #'car snapshot)))
    (when (ende-queue-ui--confirm-cancel jobs)
      (ende-queue-ui--assert-states-current snapshot)
      (ende-queue-cancel-ids ids))))

(defun ende-queue-ui-cancel-all ()
  "Cancel every job through the existing broad abort behavior."
  (interactive)
  (let* ((jobs (ende-queue-ui--live-jobs))
         (snapshot (ende-queue-ui--state-snapshot jobs)))
    (when (ende-queue-ui--confirm-cancel jobs t)
      (let* ((current (ende-queue-ui--state-snapshot
                       (ende-queue-ui--live-jobs)))
             (expected (sort snapshot (lambda (left right)
                                        (< (car left) (car right))))))
        (setq current (sort current (lambda (left right)
                                      (< (car left) (car right)))))
        (unless (equal expected current)
          (user-error "Queue changed; review it and try again")))
      (ende-abort-queue))))

(defun ende-queue-ui-change-policy ()
  "Change concurrency policy for marked jobs or the queued job at point."
  (interactive)
  (let* ((jobs (ende-queue-ui--selected-jobs))
         (ids (mapcar #'ende-job-id jobs)))
    (unless (cl-every (lambda (job) (eq (ende-job-state job) 'queued)) jobs)
      (user-error "Concurrency policy can only change for queued jobs"))
    (let* ((choice (completing-read "Policy: " '("parallel" "wait-idle")
                                   nil t))
           (policy (intern choice)))
      (ende-queue-change-policy ids policy))))

(defun ende-queue-ui-move-up ()
  "Move the queued job at point one scheduler entry earlier."
  (interactive)
  (unless (eq ende-queue-ui-view 'jobs)
    (user-error "Reordering is available only in Jobs view"))
  (ende-queue-move (ende-job-id (ende-queue-ui--current-job)) -1))

(defun ende-queue-ui-move-down ()
  "Move the queued job at point one scheduler entry later."
  (interactive)
  (unless (eq ende-queue-ui-view 'jobs)
    (user-error "Reordering is available only in Jobs view"))
  (ende-queue-move (ende-job-id (ende-queue-ui--current-job)) 1))

(defun ende-queue-ui-mark ()
  "Mark the job at point for batch operations."
  (interactive)
  (let ((job (ende-queue-ui--current-job)))
    (puthash (ende-job-id job) t ende-queue-ui--marked-ids)
    (ende-queue-ui--render)))

(defun ende-queue-ui-unmark ()
  "Unmark the job at point for batch operations."
  (interactive)
  (let ((job (ende-queue-ui--current-job)))
    (remhash (ende-job-id job) ende-queue-ui--marked-ids)
    (ende-queue-ui--render)))

(defun ende-queue-ui-toggle-entry ()
  "Expand or fold the job, fence, or buffer group at point."
  (interactive)
  (let ((entry (ende-queue-ui--entry-at-point)))
    (unless entry (user-error "Point is not on a job, fence, or buffer heading"))
    (pcase (car entry)
      ('job
       (let ((id (ende-job-id (cdr entry))))
         (if (gethash id ende-queue-ui--expanded-jobs)
             (remhash id ende-queue-ui--expanded-jobs)
           (puthash id t ende-queue-ui--expanded-jobs))))
      ('fence
       (let ((id (ende-queue-fence-id (cdr entry))))
         (if (gethash id ende-queue-ui--expanded-fences)
             (remhash id ende-queue-ui--expanded-fences)
           (puthash id t ende-queue-ui--expanded-fences))))
      ('buffer
       (let ((buffer (cdr entry)))
          (if (gethash buffer ende-queue-ui--folded-buffers)
              (remhash buffer ende-queue-ui--folded-buffers)
            (puthash buffer t ende-queue-ui--folded-buffers)))))
    (ende-queue-ui--render)))

(defun ende-queue-ui-visit-source ()
  "Visit the source location of the job at point."
  (interactive)
  (let* ((job (ende-queue-ui--current-job))
         (target (ende-job-target job))
         (buffer (ende-target-buffer target))
         (position (or (ende-target-cursor target)
                       (ende-target-start target))))
    (unless (buffer-live-p buffer)
      (user-error "The source buffer no longer exists"))
    (pop-to-buffer buffer)
    (when position
      (goto-char (min (point-max) (max (point-min) position))))))

(defun ende-queue-ui-next-entry (&optional direction)
  "Move to the next or previous job/fence entry by DIRECTION."
  (interactive)
  (let* ((direction (or direction 1))
         (positions (ende-queue-ui--entry-positions))
         (current (point))
         (candidate
          (if (> direction 0)
              (cl-find-if (lambda (position) (> position current)) positions)
            (car (last (cl-remove-if-not
                        (lambda (position) (< position current)) positions))))))
    (if candidate
        (goto-char candidate)
      (user-error "No more job or fence entries"))))

(defun ende-queue-ui-next-job ()
  "Move to the next job or fence entry."
  (interactive)
  (ende-queue-ui-next-entry 1))

(defun ende-queue-ui-previous-job ()
  "Move to the previous job or fence entry."
  (interactive)
  (ende-queue-ui-next-entry -1))

(defun ende-queue-ui-switch-view ()
  "Switch between Jobs and Buffers without changing scheduler state."
  (interactive)
  (setq ende-queue-ui-view (if (eq ende-queue-ui-view 'jobs) 'buffers 'jobs))
  (ende-queue-ui--render))

(defun ende-queue-ui-help ()
  "Display concise dashboard key help."
  (interactive)
  (with-help-window "*ende-queue help*"
    (princ "Ende queue dashboard\n\n")
    (princ "C-n/C-p, arrows  Move by line    n/p  Next/previous job or fence\n")
    (princ "TAB             Expand/fold     v    Switch Jobs/Buffers\n")
    (princ "j/RET           Visit source    q    Quit window\n")
    (princ "P/R             Pause/resume    k/K  Cancel selected/all\n")
    (princ "c               Change queued policy\n")
    (princ "M-<up/down>     Reorder queued job (Jobs view only)\n")
    (princ "m/u             Mark/unmark     g    Refresh\n")))

(defun ende-queue-ui-quit ()
  "Quit the dashboard window without changing scheduling."
  (interactive)
  (let ((buffer (current-buffer)))
    (quit-window)
    (when (and (buffer-live-p buffer) (not (get-buffer-window buffer t)))
      (with-current-buffer buffer
        (when (timerp ende-queue-ui--clock-timer)
          (cancel-timer ende-queue-ui--clock-timer)
          (setq ende-queue-ui--clock-timer nil))
        (when (timerp ende-queue-ui--refresh-timer)
          (cancel-timer ende-queue-ui--refresh-timer)
          (setq ende-queue-ui--refresh-timer nil))
        (ende-queue-ui--detach-observers)))))

(defvar ende-queue-ui-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "C-n") #'next-line)
    (define-key map (kbd "C-p") #'previous-line)
    (define-key map (kbd "<down>") #'next-line)
    (define-key map (kbd "<up>") #'previous-line)
    (define-key map (kbd "n") #'ende-queue-ui-next-job)
    (define-key map (kbd "p") #'ende-queue-ui-previous-job)
    (define-key map (kbd "TAB") #'ende-queue-ui-toggle-entry)
    (define-key map (kbd "v") #'ende-queue-ui-switch-view)
    (define-key map (kbd "q") #'ende-queue-ui-quit)
    (define-key map (kbd "j") #'ende-queue-ui-visit-source)
    (define-key map (kbd "RET") #'ende-queue-ui-visit-source)
    (define-key map (kbd "P") #'ende-queue-pause)
    (define-key map (kbd "R") #'ende-queue-resume)
    (define-key map (kbd "k") #'ende-queue-ui-cancel)
    (define-key map (kbd "K") #'ende-queue-ui-cancel-all)
    (define-key map (kbd "c") #'ende-queue-ui-change-policy)
    (define-key map (kbd "M-<up>") #'ende-queue-ui-move-up)
    (define-key map (kbd "M-<down>") #'ende-queue-ui-move-down)
    (define-key map (kbd "m") #'ende-queue-ui-mark)
    (define-key map (kbd "u") #'ende-queue-ui-unmark)
    (define-key map (kbd "g") #'ende-queue-ui--render)
    (define-key map (kbd "?") #'ende-queue-ui-help)
    map)
  "Keymap for the Ende scheduler dashboard.")

(define-derived-mode ende-queue-ui-mode special-mode "ende-queue"
  "Read-only live views of Ende's scheduler queue."
  (setq-local truncate-lines t)
  (setq-local ende-queue-ui-view 'jobs)
  (setq ende-queue-ui--buffer (current-buffer))
  (add-hook 'kill-buffer-hook #'ende-queue-ui--cleanup nil t)
  (ende-queue-ui--attach-observers)
  (ende-queue-ui--start-clock))

;;;###autoload
(defun ende-queue-open ()
  "Open the live Ende scheduler dashboard."
  (interactive)
  (let ((buffer (get-buffer-create ende-queue-ui-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'ende-queue-ui-mode)
        (ende-queue-ui-mode))
      (setq ende-queue-ui--buffer buffer)
      (ende-queue-ui--attach-observers)
      (ende-queue-ui--start-clock)
      (ende-queue-ui--render))
    (pop-to-buffer buffer)
    buffer))

(provide 'ende-queue-ui)

;;; ende-queue-ui.el ends here
