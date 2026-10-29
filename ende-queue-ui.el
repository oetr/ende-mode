;;; ende-queue-ui.el --- Global task tree dashboard -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ende-queue)
(require 'ende-queue-source)
(require 'ende-history)

(defface ende-queue-ui-heading-face
  '((t :inherit font-lock-keyword-face))
  "Face for dashboard headings." :group 'ende)

(defface ende-queue-ui-mark-face
  '((t :inherit font-lock-warning-face))
  "Face for marked live jobs." :group 'ende)

(defconst ende-queue-ui-buffer-name "*ende-queue*")
(defvar ende-queue-ui--buffer nil)
(defvar ende-queue-ui--marked-ids (make-hash-table :test #'eql))
(defvar ende-queue-ui--folds (make-hash-table :test #'eql)
  "Explicit folding choices keyed by stable job ID: expanded or collapsed.")
(defvar-local ende-queue-ui--refresh-timer nil)
(defvar-local ende-queue-ui--clock-timer nil)
(defvar-local ende-queue-ui--rendering nil)

(defun ende-queue-ui--live-jobs ()
  "Return live scheduler jobs for actionable commands."
  (ende-all-live-jobs))

(defun ende-queue-ui--queue-event (event)
  "Forget evicted IDs and coalesce a refresh for EVENT."
  (when (eq (plist-get event :type) :evicted)
    (dolist (id (plist-get event :ids))
      (remhash id ende-queue-ui--marked-ids)
      (remhash id ende-queue-ui--folds)))
  (ende-queue-ui--schedule-refresh))

(defun ende-queue-ui--job-event (job event)
  "Coalesce a refresh for a relevant lifecycle EVENT."
  (when (eq (plist-get event :type) :settled)
    (remhash (ende-job-id job) ende-queue-ui--marked-ids))
  (unless (eq (plist-get event :type) :delta)
    (ende-queue-ui--schedule-refresh)))

(defun ende-queue-ui--schedule-refresh ()
  "Schedule one refresh without retaining any job objects."
  (when (buffer-live-p ende-queue-ui--buffer)
    (with-current-buffer ende-queue-ui--buffer
      (unless (or ende-queue-ui--rendering (timerp ende-queue-ui--refresh-timer))
        (setq ende-queue-ui--refresh-timer
              (run-at-time 0.08 nil #'ende-queue-ui--refresh-callback
                           (current-buffer)))))))

(defun ende-queue-ui--refresh-callback (buffer)
  "Refresh visible dashboard BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq ende-queue-ui--refresh-timer nil)
      (when (get-buffer-window buffer t) (ende-queue-ui--render)))))

(defun ende-queue-ui--clock (buffer)
  "Refresh visible BUFFER's elapsed times once per second."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (if (get-buffer-window buffer t)
          (ende-queue-ui--render)
        (when (timerp ende-queue-ui--clock-timer)
          (cancel-timer ende-queue-ui--clock-timer)
          (setq ende-queue-ui--clock-timer nil))))))

(defun ende-queue-ui--start-clock ()
  "Start the visible-dashboard clock once."
  (unless (timerp ende-queue-ui--clock-timer)
    (setq ende-queue-ui--clock-timer
          (run-with-timer 1 1 #'ende-queue-ui--clock (current-buffer)))))

(defun ende-queue-ui--window-buffer-changed (frame)
  "Restart refreshes when the dashboard becomes visible on FRAME."
  (when (and (buffer-live-p ende-queue-ui--buffer)
             (get-buffer-window ende-queue-ui--buffer frame))
    (with-current-buffer ende-queue-ui--buffer
      (ende-queue-ui--start-clock)
      (ende-queue-ui--schedule-refresh))))

(add-hook 'ende-job-event-functions #'ende-queue-ui--job-event)
(add-hook 'ende-queue-event-functions #'ende-queue-ui--queue-event)
(add-hook 'window-buffer-change-functions #'ende-queue-ui--window-buffer-changed)

(defun ende-queue-ui--cleanup ()
  "Cancel dashboard timers, leaving family retention untouched."
  (dolist (timer (list ende-queue-ui--refresh-timer ende-queue-ui--clock-timer))
    (when (timerp timer) (cancel-timer timer)))
  (setq ende-queue-ui--refresh-timer nil ende-queue-ui--clock-timer nil)
  (when (eq ende-queue-ui--buffer (current-buffer))
    (setq ende-queue-ui--buffer nil)))

(defun ende-queue-ui--line (text &optional id face)
  "Insert TEXT on one line with optional stable job ID and FACE."
  (let ((start (point)))
    (insert text)
    (when id (put-text-property start (point) 'ende-queue-ui-job-id id))
    (when face (add-face-text-property start (point) face t))
    (insert "\n")))

(defun ende-queue-ui--duration (seconds)
  "Format nonnegative elapsed SECONDS compactly, including seconds."
  (let* ((seconds (max 0 (floor seconds)))
         (minutes (/ seconds 60)) (hours (/ minutes 60)))
    (cond ((> hours 0) (format "%dh%02dm%02ds" hours (% minutes 60) (% seconds 60)))
          ((> minutes 0) (format "%dm%02ds" minutes (% seconds 60)))
          (t (format "%ds" seconds)))))

(defun ende-queue-ui--policy (job)
  "Return JOB's effective family policy label."
  (if (plist-get (ende--family-scheduling-policy
                 (ende-job-family-root (ende-job-family job))) :parallel)
      "parallel" "sequential"))

(defun ende-queue-ui--job-status (job)
  "Return scheduler or execution status, never an activity title."
  (pcase (ende-job-state job)
    ('queued "Queued") ('suspended "Waiting")
    ('completed "Done (ok)") ('failed "Done (error)")
    ('cancelled "Cancelled") ('skipped "Skipped")
    (_ (or (ende-job-status job) "Thinking"))))

(defun ende-queue-ui--cells (job now)
  "Return JOB's first five cells using a common render time NOW."
  (let* ((family (ende-job-family job))
         (buffer (ende-job-family-origin family)))
    (when (buffer-live-p buffer)
      (setf (ende-job-family-origin-name family) (buffer-name buffer)))
    (list (if (ende-job-parent job) "" (or (ende-job-family-origin-name family) ""))
          (ende-queue-ui--policy job) (ende-queue-ui--job-status job)
          (ende-history--format-token-count
           (or (plist-get (ende--usage-token-counts (ende-job-last-usage job)) :input) 0))
          (ende-queue-ui--duration
           (- (or (ende-job-finished-at job) now)
              (or (ende-job-started-at job) (ende-job-enqueued-at job) now))))))

(defun ende-queue-ui--fit (text width &optional right)
  "Fit TEXT in display WIDTH, padding on the left when RIGHT is non-nil."
  (let* ((text (truncate-string-to-width text width nil nil "…"))
         (padding (make-string (max 0 (- width (string-width text))) ?\s)))
    (if right (concat padding text) (concat text padding))))

(defun ende-queue-ui--columns-text (cells widths)
  "Align CELLS with display WIDTHS, right-aligning the numeric columns."
  (let ((index -1))
    (mapconcat (lambda (cell)
                 (cl-incf index)
                 (ende-queue-ui--fit cell (nth index widths) (>= index 3)))
               cells "  ")))

(defun ende-queue-ui--expanded-p (job)
  "Return JOB's explicit folding choice or its section default."
  (eq (gethash (ende-job-id job) ende-queue-ui--folds
               (if (ende-job-family-archived-p (ende-job-family job))
                   'collapsed 'expanded))
      'expanded))

(defun ende-queue-ui--entry-key-at (position)
  "Return the stable job key at POSITION."
  (when-let* ((id (get-text-property position 'ende-queue-ui-job-id)))
    (cons 'job id)))

(defun ende-queue-ui--entry-positions ()
  "Return visible job row positions."
  (save-excursion
    (goto-char (point-min))
    (let (positions)
      (while (< (point) (point-max))
        (when (get-text-property (point) 'ende-queue-ui-job-id) (push (point) positions))
        (forward-line 1))
      (nreverse positions))))

(defun ende-queue-ui--position-for-key (key)
  "Resolve stable KEY to its visible row."
  (cl-find-if (lambda (position) (equal key (ende-queue-ui--entry-key-at position)))
              (ende-queue-ui--entry-positions)))

(defun ende-queue-ui--render ()
  "Render one global tree, retaining identity and sensible window positions."
  (interactive)
  (unless ende-queue-ui--rendering
    (let* ((ende-queue-ui--rendering t)
           (key (ende-queue-ui--entry-key-at (line-beginning-position)))
           (old-positions (ende-queue-ui--entry-positions))
           (old-index (or (cl-position (line-beginning-position) old-positions) 0))
           (starts (mapcar (lambda (window)
                             (cons window (line-number-at-pos (window-start window))))
                           (get-buffer-window-list (current-buffer) nil t)))
           (active (mapcar #'ende-job-family-root (ende-queue-family-markers)))
           (finished (mapcar #'ende-job-family-root ende--finished-families))
           (roots (append active finished))
           (children (make-hash-table :test #'eql))
           (cells (make-hash-table :test #'eql))
           (headers '("Origin" "Policy" "Status" "Context" "Elapsed"))
           (widths (mapcar #'string-width headers))
           (now (float-time)))
      (maphash (lambda (id _value)
                 (unless (ende-queue-live-job-by-id id)
                   (remhash id ende-queue-ui--marked-ids))) ende-queue-ui--marked-ids)
      (dolist (root roots)
        (dolist (job (ende-job-family-members (ende-job-family root)))
          (let ((values (ende-queue-ui--cells job now)))
            (puthash (ende-job-id job) values cells)
            (setq widths (cl-mapcar #'max widths (mapcar #'string-width values))))
          (when-let* ((parent (ende-job-parent job)))
            (push job (gethash (ende-job-id parent) children)))))
      (maphash (lambda (id jobs)
                 (puthash id (sort jobs (lambda (a b)
                                         (ende--schedule-path-before-p
                                          (ende-job-schedule-path a)
                                          (ende-job-schedule-path b)))) children)) children)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (ende-queue-ui--line
         (format "Queue [Jobs]  %s  %s  c: change  w: see agent's work  k: kill agent  q: quit"
                 (if ende-queue-paused "PAUSED" "ACTIVE")
                 (if ende-queue-paused "R: resume" "P: pause")) nil 'ende-queue-ui-heading-face)
        (insert "\n")
        (ende-queue-ui--line (concat (ende-queue-ui--columns-text headers widths) "  Task"))
        (cl-labels
            ((tree (job prefix branch)
               (let* ((id (ende-job-id job)) (descendants (gethash id children))
                      (expanded (ende-queue-ui--expanded-p job))
                      (triangle (cond (descendants (if expanded "▼ " "▶ "))
                                      ((string-empty-p branch) "  ")
                                      (t ""))))
                 (ende-queue-ui--line
                  (concat (ende-queue-ui--columns-text (gethash id cells) widths)
                          "  " prefix branch triangle
                          (replace-regexp-in-string
                           "[[:cntrl:]]" " "
                           (if (string-blank-p (or (ende-job-title job) ""))
                               "tbd." (ende-job-title job))))
                  id (and (gethash id ende-queue-ui--marked-ids) 'ende-queue-ui-mark-face))
                 (when expanded
                   (let ((remaining descendants))
                     (dolist (child descendants)
                       (setq remaining (cdr remaining))
                       (tree child (concat prefix (pcase branch ("├─ " "│  ") ("└─ " "   ") (_ "")))
                             (if remaining "├─ " "└─ "))))))))
          (dolist (section (list (cons "Active" active) (cons "Finished" finished)))
            (ende-queue-ui--line (format "%s (%d)" (car section) (length (cdr section)))
                                 nil 'ende-queue-ui-heading-face)
            (dolist (root (cdr section)) (tree root "" "")))))
      (let ((position (and key (ende-queue-ui--position-for-key key)))
            (job (and key (ende-queue-retained-job-by-id (cdr key)))))
        (while (and (not position) job (ende-job-parent job))
          (setq job (ende-job-parent job)
                position (ende-queue-ui--position-for-key (cons 'job (ende-job-id job)))))
        (goto-char (or position (nth (min old-index (max 0 (1- (length (ende-queue-ui--entry-positions)))))
                                    (ende-queue-ui--entry-positions)) (point-min))))
      (dolist (entry starts)
        (when (window-live-p (car entry))
          (set-window-start
           (car entry)
           (save-excursion
             (goto-char (point-min)) (forward-line (1- (cdr entry))) (point)) t))))))

(defun ende-queue-ui--current-job (&optional live)
  "Resolve the current row through the retained index, or scheduler if LIVE."
  (let* ((id (get-text-property (line-beginning-position) 'ende-queue-ui-job-id))
         (job (and id (if live (ende-queue-live-job-by-id id)
                        (ende-queue-retained-job-by-id id)))))
    (or job (user-error (if live "Select an eligible live job" "No retained job at point")))))

(defun ende-queue-ui--selected-jobs ()
  "Resolve marked IDs through the live scheduler, or select the current live job."
  (if (= (hash-table-count ende-queue-ui--marked-ids) 0)
      (list (ende-queue-ui--current-job t))
    (mapcar (lambda (id) (or (ende-queue-live-job-by-id id)
                             (user-error "Marked jobs changed; refresh first")))
            (hash-table-keys ende-queue-ui--marked-ids))))

(defun ende-queue-ui--state-snapshot (jobs)
  "Return stable ID/state pairs for JOBS."
  (mapcar (lambda (job) (cons (ende-job-id job) (ende-job-state job))) jobs))

(defun ende-queue-ui--assert-states-current (snapshot)
  "Reject SNAPSHOT if a selected live ID disappeared or changed state."
  (unless (equal snapshot
                 (mapcar (lambda (entry)
                           (when-let* ((job (ende-queue-live-job-by-id (car entry))))
                             (cons (car entry) (ende-job-state job)))) snapshot))
    (user-error "Queue changed; review the selection and try again")))

(defun ende-queue-ui--confirm-cancel (jobs &optional all)
  "Confirm broad or started JOBS cancellation, labeling ALL explicitly."
  (let ((running (cl-count 'running jobs :key #'ende-job-state))
        (queued (cl-count 'queued jobs :key #'ende-job-state))
        (waiting (cl-count 'suspended jobs :key #'ende-job-state))
        (owns-children (cl-some #'ende-job-children jobs)))
    (or (null jobs)
        (and (not all) (not owns-children) (= queued 1) (= running 0) (= waiting 0))
        (yes-or-no-p (format "Cancel%s %d running, %d queued, and %d waiting jobs? "
                             (if all " all" "") running queued waiting)))))

(defun ende-queue-ui-cancel ()
  "Cancel selected live subtrees with the existing confirmation safeguards."
  (interactive)
  (let* ((jobs (ende-queue-ui--selected-jobs))
         (snapshot (ende-queue-ui--state-snapshot jobs)))
    (when (ende-queue-ui--confirm-cancel jobs)
      (ende-queue-ui--assert-states-current snapshot)
      (ende-queue-cancel-ids (mapcar #'car snapshot)))))

(defun ende-queue-ui-cancel-all ()
  "Cancel all live work without clearing retained history."
  (interactive)
  (let* ((jobs (ende-queue-ui--live-jobs))
         (snapshot (ende-queue-ui--state-snapshot jobs)))
    (when (ende-queue-ui--confirm-cancel jobs t)
      (unless (equal snapshot (ende-queue-ui--state-snapshot (ende-queue-ui--live-jobs)))
        (user-error "Queue changed; review it and try again"))
      (ende-abort-queue))))

(defun ende-queue-ui-change-policy ()
  "Change only queued, never-started, unfrozen top-level family policies."
  (interactive)
  (let ((ids (mapcar #'ende-job-id (ende-queue-ui--selected-jobs))))
    (ende-queue-change-policy ids (intern (completing-read "Policy: "
                                                         '("parallel" "sequential") nil t)))))

(defun ende-queue-ui-inject-guidance ()
  "Inject guidance into an eligible live chat."
  (interactive)
  (let ((job (ende-queue-ui--current-job t)))
    (unless (ende-job-can-accept-guidance-p job) (user-error "Selected job cannot accept guidance"))
    (ende-queue-add-guidance job (read-string "Inject prompt: "))))

(defun ende-queue-ui-move-up ()
  "Move an eligible queued root earlier under existing scheduler semantics."
  (interactive) (ende-queue-move (ende-job-id (ende-queue-ui--current-job t)) -1))
(defun ende-queue-ui-move-down ()
  "Move an eligible queued root later under existing scheduler semantics."
  (interactive) (ende-queue-move (ende-job-id (ende-queue-ui--current-job t)) 1))

(defun ende-queue-ui-mark ()
  "Mark an eligible live job by ID."
  (interactive)
  (puthash (ende-job-id (ende-queue-ui--current-job t)) t ende-queue-ui--marked-ids)
  (ende-queue-ui--render))
(defun ende-queue-ui-unmark ()
  "Unmark the selected job ID."
  (interactive)
  (remhash (ende-job-id (ende-queue-ui--current-job)) ende-queue-ui--marked-ids)
  (ende-queue-ui--render))

(defun ende-queue-ui-toggle-entry ()
  "Toggle descendants only; leaves do not expand details."
  (interactive)
  (let ((job (ende-queue-ui--current-job)))
    (when (cl-some (lambda (member) (eq (ende-job-parent member) job))
                    (ende-job-family-members (ende-job-family job)))
      (puthash (ende-job-id job) (if (ende-queue-ui--expanded-p job) 'collapsed 'expanded)
               ende-queue-ui--folds)
      (ende-queue-ui--render))))

(defun ende-queue-ui-visit-source ()
  "Visit the originating source of the selected retained job."
  (interactive) (ende-queue-visit-source (ende-queue-ui--current-job)))
(defun ende-queue-ui-visit-agent-work ()
  "Inspect retained private work without creating a replacement."
  (interactive)
  (let ((buffer (ende-job-work-buffer (ende-queue-ui--current-job))))
    (unless (buffer-live-p buffer) (user-error "Work buffer is unavailable"))
    (pop-to-buffer buffer) (goto-char (point-max))))

(defun ende-queue-ui-next-entry (&optional direction)
  "Move DIRECTION through visible job rows."
  (let* ((positions (ende-queue-ui--entry-positions))
         (current (line-beginning-position))
         (position (if (> (or direction 1) 0)
                       (cl-find-if (lambda (p) (> p current)) positions)
                     (car (last (cl-remove-if-not (lambda (p) (< p current)) positions))))))
    (if position (goto-char position) (user-error "No more visible jobs"))))
(defun ende-queue-ui-next-job ()
  "Select the next visible job." (interactive) (ende-queue-ui-next-entry 1))
(defun ende-queue-ui-previous-job ()
  "Select the previous visible job." (interactive) (ende-queue-ui-next-entry -1))

(defun ende-queue-ui-help ()
  "Show available dashboard commands."
  (interactive)
  (with-help-window "*ende-queue help*"
    (princ "TAB Fold descendants   n/p Next/previous job   j/RET Visit source\nw Inspect work   P/R Pause/resume   c Change eligible queued policy\nk/K Cancel selected/all live work   m/u Mark/unmark live jobs\ni Inject guidance   M-<up/down> Reorder queued roots   g Refresh   q Quit\n")))
(defun ende-queue-ui-quit ()
  "Quit without cancelling jobs or clearing retained history."
  (interactive) (quit-window))

(defvar ende-queue-ui-mode-map)
(setq ende-queue-ui-mode-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map special-mode-map)
        (dolist (entry '(("n" . ende-queue-ui-next-job) ("p" . ende-queue-ui-previous-job)
                         ("TAB" . ende-queue-ui-toggle-entry) ("q" . ende-queue-ui-quit)
                         ("j" . ende-queue-ui-visit-source) ("RET" . ende-queue-ui-visit-source)
                         ("w" . ende-queue-ui-visit-agent-work) ("i" . ende-queue-ui-inject-guidance)
                         ("P" . ende-queue-pause) ("R" . ende-queue-resume)
                         ("k" . ende-queue-ui-cancel) ("K" . ende-queue-ui-cancel-all)
                         ("c" . ende-queue-ui-change-policy) ("m" . ende-queue-ui-mark)
                         ("u" . ende-queue-ui-unmark) ("g" . ende-queue-ui--render)
                         ("?" . ende-queue-ui-help) ("M-<up>" . ende-queue-ui-move-up)
                         ("M-<down>" . ende-queue-ui-move-down)))
          (define-key map (kbd (car entry)) (cdr entry)))
        map))

(define-derived-mode ende-queue-ui-mode special-mode "ende-queue"
  "Inspect active and retained task families in one global tree."
  (setq-local truncate-lines t)
  (setq-local buffer-face-mode-face 'fixed-pitch)
  (buffer-face-mode 1)
  (buffer-disable-undo)
  (setq ende-queue-ui--buffer (current-buffer))
  (add-hook 'kill-buffer-hook #'ende-queue-ui--cleanup nil t))

(when (buffer-live-p ende-queue-ui--buffer)
  (with-current-buffer ende-queue-ui--buffer
    (use-local-map ende-queue-ui-mode-map)
    (ende-queue-ui--schedule-refresh)))

;;;###autoload
(defun ende-queue-open ()
  "Open the global task dashboard."
  (interactive)
  (let ((buffer (get-buffer-create ende-queue-ui-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'ende-queue-ui-mode) (ende-queue-ui-mode))
      (ende-queue-ui--render)
      (ende-queue-ui--start-clock))
    (pop-to-buffer buffer)
    buffer))

(provide 'ende-queue-ui)
;;; ende-queue-ui.el ends here
