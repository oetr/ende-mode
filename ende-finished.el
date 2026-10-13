;;; ende-finished.el --- Navigation list for finished Ende jobs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools

;;; Commentary:

;; Keep bounded navigation snapshots for terminal jobs without retaining jobs.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'subr-x)
(require 'tabulated-list)
(require 'ende-queue)

(defcustom ende-finished-limit 100
  "Maximum number of finished jobs retained in memory."
  :type 'integer
  :group 'ende)

(defconst ende-finished-buffer-name "*ende-finished*"
  "Name of the finished-jobs buffer.")

(cl-defstruct (ende-finished--live (:constructor ende-finished--make-live))
  id operation buffer buffer-name marker file line title request-id)

(cl-defstruct (ende-finished-job (:constructor ende-finished--make-job))
  id operation state finished-at buffer buffer-name file line title request-id)

(defvar ende-finished-jobs nil
  "Finished-job navigation snapshots, newest first.")

(defvar ende-finished--live-jobs (make-hash-table :test #'eql)
  "Temporary source navigation state for queued and running jobs.")

(defun ende-finished--source-title (position)
  "Return the Org heading title containing POSITION, or nil."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char position)
      (when (and (derived-mode-p 'org-mode)
                 (org-back-to-heading t))
        (let ((title (org-get-heading t t t t)))
          (unless (string-empty-p title)
            title))))))

(defun ende-finished--track (job)
  "Begin lightweight source tracking for JOB while its target is live."
  (let* ((target (ende-job-target job))
         (buffer (ende-target-buffer target)))
    (when (and (ende-target-live-p target)
               (not (gethash (ende-job-id job) ende-finished--live-jobs)))
      (with-current-buffer buffer
        (let* ((position (or (ende-target-cursor target) (point-min)))
               (instruction (ende-job-instruction job))
               (live
                (ende-finished--make-live
                 :id (ende-job-id job)
                 :operation (ende-job-type job)
                 :buffer buffer
                 :buffer-name (buffer-name buffer)
                 :marker (copy-marker position)
                 :file (and buffer-file-name
                            (expand-file-name buffer-file-name))
                 :title (ende-finished--source-title position)
                 :request-id (and (memq (ende-job-type job)
                                        '(chat chat-compact))
                                  (plist-get instruction :request-id)))))
          (puthash (ende-job-id job) live ende-finished--live-jobs)
          (add-hook 'kill-buffer-hook
                    #'ende-finished--source-buffer-killed -60 t))))))

(defun ende-finished--line-at-marker (marker buffer)
  "Return the line at MARKER in BUFFER, or nil if unavailable."
  (when (and (markerp marker)
             (buffer-live-p buffer)
             (eq (marker-buffer marker) buffer)
             (marker-position marker))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (line-number-at-pos (marker-position marker) t)))))

(defun ende-finished--source-buffer-killed ()
  "Save line numbers for tracked jobs before the source buffer is killed."
  (let ((buffer (current-buffer)))
    (maphash
     (lambda (_id live)
       (when (eq (ende-finished--live-buffer live) buffer)
         (setf (ende-finished--live-line live)
               (ende-finished--line-at-marker
                (ende-finished--live-marker live) buffer))))
     ende-finished--live-jobs)))

(defun ende-finished--untrack (live)
  "Release temporary marker state LIVE and its source-buffer hook if unused."
  (let ((buffer (ende-finished--live-buffer live)))
    (remhash (ende-finished--live-id live) ende-finished--live-jobs)
    (when (markerp (ende-finished--live-marker live))
      (set-marker (ende-finished--live-marker live) nil))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (unless (cl-some (lambda (other)
                           (eq (ende-finished--live-buffer other) buffer))
                         (hash-table-values ende-finished--live-jobs))
          (remove-hook 'kill-buffer-hook
                       #'ende-finished--source-buffer-killed t)))))
  nil)

(defun ende-finished--record (job state)
  "Create and retain a lightweight terminal snapshot of JOB in STATE."
  (let* ((id (ende-job-id job))
         (live (or (gethash id ende-finished--live-jobs)
                   (progn (ende-finished--track job)
                          (gethash id ende-finished--live-jobs))))
         (target (ende-job-target job))
         (buffer (or (and live (ende-finished--live-buffer live))
                     (ende-target-buffer target)))
         (line (or (and live
                        (ende-finished--line-at-marker
                         (ende-finished--live-marker live) buffer))
                   (and live (ende-finished--live-line live))))
         (snapshot
          (ende-finished--make-job
           :id id
           :operation (or (and live (ende-finished--live-operation live))
                          (ende-job-type job))
           :state state
           :finished-at (or (ende-job-finished-at job) (float-time))
           :buffer (and (buffer-live-p buffer) buffer)
           :buffer-name (or (and live (ende-finished--live-buffer-name live))
                            (and (buffer-live-p buffer) (buffer-name buffer)))
           :file (and live (ende-finished--live-file live))
           :line line
           :title (and live (ende-finished--live-title live))
           :request-id (and live (ende-finished--live-request-id live)))))
    (when live
      (ende-finished--untrack live))
    (setq ende-finished-jobs
          (cl-subseq (cons snapshot ende-finished-jobs) 0
                     (min (max 0 ende-finished-limit)
                          (1+ (length ende-finished-jobs)))))
    snapshot))

(defun ende-finished--job-event (job event)
  "Track JOB from its queued event and snapshot it when EVENT settles."
  (pcase (plist-get event :type)
    (:queued (ende-finished--track job))
    (:settled (ende-finished--record job (plist-get event :state)))))

(add-hook 'ende-job-event-functions #'ende-finished--job-event)

(defun ende-finished--inside-work-section-p ()
  "Return non-nil when the current heading is inside an agent work section."
  (save-excursion
    (let ((inside nil)
          (continue t))
      (while (and continue (org-up-heading-safe))
        (when (equal (org-entry-get nil "ENDE_CHAT_WORK" nil) "t")
          (setq inside t
                continue nil)))
      inside)))

(defun ende-finished--find-chat-reply (request-id)
  "Return the position of REQUEST-ID's actual assistant reply or context fence."
  (when (and request-id (derived-mode-p 'org-mode))
    (save-excursion
      (goto-char (point-min))
      (catch 'found
        (while (re-search-forward org-heading-regexp nil t)
          (let ((position (match-beginning 0)))
            (goto-char position)
            (let ((title (org-get-heading t t t t)))
              (when (and (member title '("Agent: Reply" "Agent: Context Fence"))
                         (equal (org-entry-get nil "ENDE_CHAT_REQUEST_ID" nil)
                                request-id)
                         (equal (org-entry-get nil "ENDE_ROLE" nil) "assistant")
                         (not (ende-finished--inside-work-section-p))
                         (or (not (equal title "Agent: Context Fence"))
                             (equal (org-entry-get nil
                                                   "ENDE_CHAT_CONTEXT_FENCE" nil)
                                    "t")))
                (throw 'found position))))
          (forward-line 1))
        nil))))

(defun ende-finished--find-heading (title)
  "Return the first Org heading whose exact title is TITLE."
  (when (and title (derived-mode-p 'org-mode))
    (save-excursion
      (goto-char (point-min))
      (catch 'found
        (while (re-search-forward org-heading-regexp nil t)
          (let ((position (match-beginning 0)))
            (goto-char position)
            (when (equal (org-get-heading t t t t) title)
              (throw 'found position)))
          (forward-line 1))
        nil))))

(defun ende-finished--resolve-buffer (job)
  "Resolve JOB's original source buffer, opening its file if needed."
  (let ((buffer (ende-finished-job-buffer job))
        (file (ende-finished-job-file job)))
    (cond
     ((buffer-live-p buffer) buffer)
     ((and file (file-exists-p file)) (find-file-noselect file))
     (file (message "ende: source file no longer exists: %s" file) nil)
     (t (message "ende: source buffer no longer exists: %s"
                 (or (ende-finished-job-buffer-name job) "unknown"))
        nil))))

(defun ende-finished-visit-job (&optional job)
  "Visit JOB's best-known source location, or the selected finished job."
  (interactive)
  (let* ((job (or job
                  (let ((id (tabulated-list-get-id)))
                    (cl-find id ende-finished-jobs
                             :key #'ende-finished-job-id :test #'eql))))
         (buffer (and job (ende-finished--resolve-buffer job)))
         position)
    (unless job
      (user-error "No finished job selected"))
    (when buffer
      (with-current-buffer buffer
        (widen)
        (setq position
              (or (ende-finished--find-chat-reply
                   (ende-finished-job-request-id job))
                  (ende-finished--find-heading
                   (ende-finished-job-title job))
                  (let ((line (ende-finished-job-line job)))
                    (and (integerp line) (> line 0)
                         (save-excursion
                           (goto-char (point-min))
                           (and (zerop (forward-line (1- line)))
                                (point)))))))
        (when position
          (goto-char position)))
      (pop-to-buffer buffer))))

(defun ende-finished--entries ()
  "Return tabulated-list rows for the retained finished jobs."
  (mapcar
   (lambda (job)
     (list (ende-finished-job-id job)
           (vector
            (number-to-string (ende-finished-job-id job))
            (or (ende-finished-job-buffer-name job) "")
            (symbol-name (ende-finished-job-operation job))
            (symbol-name (ende-finished-job-state job))
            (format-time-string "%H:%M"
                                (seconds-to-time
                                 (ende-finished-job-finished-at job)))
            (or (ende-finished-job-title job) ""))))
   ende-finished-jobs))

(defun ende-finished-refresh ()
  "Refresh the finished-jobs list."
  (interactive)
  (tabulated-list-print t))

(defvar ende-finished-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'ende-finished-visit-job)
    (define-key map (kbd "g") #'ende-finished-refresh)
    map)
  "Keymap for the finished-jobs list.")

(define-derived-mode ende-finished-mode tabulated-list-mode "ende-finished"
  "Tabulated navigation list of recently finished Ende jobs."
  (setq tabulated-list-format
        [("ID" 6 t)
         ("Buffer" 24 t)
         ("Operation" 18 t)
         ("State" 12 t)
         ("Finished" 9 t)
         ("Title" 0 t)])
  (setq tabulated-list-padding 2
        tabulated-list-entries #'ende-finished--entries)
  (tabulated-list-init-header))

;;;###autoload
(defun ende-finished-open ()
  "Open the list of recently finished Ende jobs."
  (interactive)
  (let ((buffer (get-buffer-create ende-finished-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'ende-finished-mode)
        (ende-finished-mode))
      (tabulated-list-print t))
    (pop-to-buffer buffer)))

(provide 'ende-finished)

;;; ende-finished.el ends here
