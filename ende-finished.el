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
  id operation buffer buffer-name marker file line title request-id question-id)

(cl-defstruct (ende-finished-job (:constructor ende-finished--make-job))
  id operation state finished-at buffer buffer-name marker file line title
  request-id question-id)

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

(defun ende-finished--normalize-title (text)
  "Collapse whitespace in TEXT and truncate it to a short display title."
  (when (and (stringp text) (not (string-blank-p text)))
    (let ((title (string-trim
                  (replace-regexp-in-string "[[:space:]]+" " " text))))
      (if (> (length title) 80)
          (concat (substring title 0 77) "…")
        title))))

(defun ende-finished--find-chat-question (question-id)
  "Return the position of the user question with QUESTION-ID."
  (when (and question-id (derived-mode-p 'org-mode))
    (save-excursion
      (goto-char (point-min))
      (catch 'found
        (while (re-search-forward org-heading-regexp nil t)
          (let ((position (match-beginning 0)))
            (goto-char position)
            (when (and (equal (org-entry-get nil "ENDE_CHAT_ID" nil)
                              question-id)
                       (equal (org-entry-get nil "ENDE_ROLE" nil) "user"))
              (throw 'found position)))
          (forward-line 1))
        nil))))

(defun ende-finished--chat-title (question-id)
  "Return QUESTION-ID's actual heading, or a short excerpt of its body."
  (when-let* ((position (ende-finished--find-chat-question question-id)))
    (save-excursion
      (goto-char position)
      (let ((heading (ende-finished--normalize-title
                      (org-get-heading t t t t))))
        (or heading
            (progn
              (org-end-of-meta-data t)
              (let ((begin (point))
                    (end (save-excursion
                           (org-end-of-subtree t t))))
                (ende-finished--normalize-title
                 (buffer-substring-no-properties begin end)))))))))

(defun ende-finished--track (job)
  "Begin lightweight source tracking for JOB while its origin is live."
  (let* ((root (or (and (ende-job-family job)
                        (ende-job-family-root (ende-job-family job)))
                   job))
         (target (ende-job-target root))
         (buffer (ende-job-origin-buffer job)))
    (when (and (ende-target-live-p target)
               (not (gethash (ende-job-id job) ende-finished--live-jobs)))
      (with-current-buffer buffer
        (let* ((position (or (ende-target-cursor target) (point-min)))
               (instruction (ende-job-instruction job))
               (chat-job (memq (ende-job-type job) '(chat chat-compact)))
               (live
                (ende-finished--make-live
                 :id (ende-job-id job)
                 :operation (ende-job-type job)
                 :buffer buffer
                 :buffer-name (buffer-name buffer)
                 :marker (copy-marker position t)
                 :file (and buffer-file-name
                            (expand-file-name buffer-file-name))
                 :title (ende-finished--source-title position)
                 :request-id (and chat-job
                                  (plist-get instruction :request-id))
                 :question-id (and chat-job
                                   (plist-get instruction :question-id)))))
          (puthash (ende-job-id job) live ende-finished--live-jobs)
          (add-hook 'kill-buffer-hook
                    #'ende-finished--source-buffer-killed -60 t))))))

(defun ende-finished--buffer-navigation-p (buffer)
  "Return non-nil when BUFFER has live or retained navigation markers."
  (or (cl-some (lambda (live)
                 (eq (ende-finished--live-buffer live) buffer))
               (hash-table-values ende-finished--live-jobs))
      (cl-some (lambda (job)
                 (let ((marker (ende-finished-job-marker job)))
                   (and (markerp marker)
                        (eq (marker-buffer marker) buffer))))
               ende-finished-jobs)))

(defun ende-finished--update-buffer-hook (buffer)
  "Keep the source kill hook on BUFFER while navigation state needs it."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (if (ende-finished--buffer-navigation-p buffer)
          (add-hook 'kill-buffer-hook #'ende-finished--source-buffer-killed
                    -60 t)
        (remove-hook 'kill-buffer-hook #'ende-finished--source-buffer-killed
                     t)))))

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
  "Save line numbers for tracked and retained jobs before BUFFER is killed."
  (let ((buffer (current-buffer)))
    (maphash
     (lambda (_id live)
       (when (eq (ende-finished--live-buffer live) buffer)
         (setf (ende-finished--live-line live)
               (ende-finished--line-at-marker
                (ende-finished--live-marker live) buffer))))
     ende-finished--live-jobs)
    (dolist (job ende-finished-jobs)
      (let ((marker (ende-finished-job-marker job)))
        (when (and (markerp marker) (eq (marker-buffer marker) buffer))
          (setf (ende-finished-job-line job)
                (ende-finished--line-at-marker marker buffer)))))))

(defun ende-finished--untrack (live)
  "Release temporary marker state LIVE and its source-buffer hook if unused."
  (let ((buffer (ende-finished--live-buffer live)))
    (remhash (ende-finished--live-id live) ende-finished--live-jobs)
    (when (markerp (ende-finished--live-marker live))
      (set-marker (ende-finished--live-marker live) nil))
    (ende-finished--update-buffer-hook buffer))
  nil)

(defun ende-finished--record (job state)
  "Create and retain a lightweight terminal snapshot of JOB in STATE."
  (let* ((id (ende-job-id job))
         (live (or (gethash id ende-finished--live-jobs)
                   (progn (ende-finished--track job)
                          (gethash id ende-finished--live-jobs))))
         (target (ende-job-target job))
         (instruction (ende-job-instruction job))
         (chat-job (memq (ende-job-type job) '(chat chat-compact)))
         (request-id (and chat-job (plist-get instruction :request-id)))
         (question-id (and chat-job (plist-get instruction :question-id)))
         (buffer (or (and live (ende-finished--live-buffer live))
                     (ende-target-buffer target)))
         (line (or (and live
                        (ende-finished--line-at-marker
                         (ende-finished--live-marker live) buffer))
                   (and live (ende-finished--live-line live))))
         (title
          (if (and question-id (buffer-live-p buffer))
              (with-current-buffer buffer
                (save-restriction
                  (widen)
                  (or (ende-finished--chat-title question-id)
                      (and live (ende-finished--live-title live)))))
            (and live (ende-finished--live-title live))))
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
           :marker (and live (markerp (ende-finished--live-marker live))
                        (copy-marker (ende-finished--live-marker live)))
           :file (and live (ende-finished--live-file live))
           :line line
           :title title
           :request-id request-id
           :question-id question-id)))
    (when live
      (ende-finished--untrack live))
    (let* ((all (cons snapshot ende-finished-jobs))
           (retained (cl-subseq all 0
                                (min (max 0 ende-finished-limit)
                                     (length all)))))
      (dolist (evicted (nthcdr (length retained) all))
        (let ((marker (ende-finished-job-marker evicted)))
          (when (markerp marker)
            (let ((marker-buffer (marker-buffer marker)))
              (set-marker marker nil)
              (ende-finished--update-buffer-hook marker-buffer)))))
      (setq ende-finished-jobs retained)
      (ende-finished--update-buffer-hook buffer))
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

(defun ende-finished--find-chat-reply (request-id &optional allow-context-fence)
  "Return REQUEST-ID's assistant reply, or its context fence when allowed."
  (when (and request-id (derived-mode-p 'org-mode))
    (save-excursion
      (goto-char (point-min))
      (catch 'found
        (while (re-search-forward org-heading-regexp nil t)
          (let ((position (match-beginning 0)))
            (goto-char position)
            (let ((context-fence
                   (org-entry-get nil "ENDE_CHAT_CONTEXT_FENCE" nil)))
              (when (and (equal (org-entry-get nil "ENDE_CHAT_REQUEST_ID" nil)
                                request-id)
                         (equal (org-entry-get nil "ENDE_ROLE" nil) "assistant")
                         (not (org-entry-get nil "ENDE_CHAT_REASONING_FOR" nil))
                         (not (equal (org-entry-get nil "ENDE_CHAT_WORK" nil)
                                     "t"))
                         (not (ende-finished--inside-work-section-p))
                         (or (null context-fence)
                             (and allow-context-fence
                                  (equal context-fence "t"))))
                (throw 'found position))))
          (forward-line 1))
        nil))))

(defun ende-finished--recover-legacy-chat-identity (job)
  "Recover JOB's question identity and generic title from its request ID."
  (when (and (memq (ende-finished-job-operation job) '(chat chat-compact))
             (ende-finished-job-request-id job))
    (let* ((buffer (or (and (buffer-live-p (ende-finished-job-buffer job))
                            (ende-finished-job-buffer job))
                       (and (ende-finished-job-file job)
                            (file-exists-p (ende-finished-job-file job))
                            (find-file-noselect
                             (ende-finished-job-file job))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (derived-mode-p 'org-mode)
            (save-restriction
              (widen)
              (when-let* ((reply
                           (ende-finished--find-chat-reply
                            (ende-finished-job-request-id job)
                            (eq (ende-finished-job-operation job) 'chat-compact))))
                (goto-char reply)
                (let ((question-id
                       (org-entry-get nil "ENDE_CHAT_REPLY_TO" nil)))
                  (when (and question-id
                             (ende-finished--find-chat-question question-id))
                    (setf (ende-finished-job-question-id job) question-id)
                    (when (or (null (ende-finished-job-title job))
                              (member (ende-finished-job-title job)
                                      '("Agent: Reply" "Agent: Context Fence")))
                      (setf (ende-finished-job-title job)
                            (ende-finished--chat-title question-id)))))))))))))

(defun ende-finished--upgrade-legacy-jobs ()
  "Convert finished-job vectors from layouts predating question identity."
  (setq ende-finished-jobs
        (mapcar
         (lambda (entry)
           (if (and (vectorp entry)
                    (> (length entry) 0)
                    (eq (aref entry 0) 'ende-finished-job)
                    (memq (length entry) '(11 12)))
               (let* ((has-marker (= (length entry) 12))
                      (file-slot (if has-marker 8 7))
                      (job
                       (ende-finished--make-job
                        :id (aref entry 1)
                        :operation (aref entry 2)
                        :state (aref entry 3)
                        :finished-at (aref entry 4)
                        :buffer (aref entry 5)
                        :buffer-name (aref entry 6)
                        :marker (and has-marker (aref entry 7))
                        :file (aref entry file-slot)
                        :line (aref entry (1+ file-slot))
                        :title (aref entry (+ file-slot 2))
                        :request-id (aref entry (+ file-slot 3)))))
                 (ende-finished--recover-legacy-chat-identity job)
                 job)
             entry))
         ende-finished-jobs)))
;; `ende-reload' re-evaluates this file while preserving the old history value.
(ende-finished--upgrade-legacy-jobs)


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

(defun ende-finished--recorded-position (job buffer)
  "Return JOB's live source marker in BUFFER, or its saved line position."
  (let ((marker (ende-finished-job-marker job))
        (line (ende-finished-job-line job)))
    (or (and (markerp marker)
             (eq (marker-buffer marker) buffer)
             (marker-position marker))
        (and (integerp line) (> line 0)
             (save-excursion
               (goto-char (point-min))
               (and (zerop (forward-line (1- line)))
                    (point)))))))

(defun ende-finished-visit-job (&optional job)
  "Visit JOB's best-known source location, or the selected finished job."
  (interactive)
  (let* ((job (or job
                  (let ((id (tabulated-list-get-id)))
                    (cl-find id ende-finished-jobs
                             :key #'ende-finished-job-id :test #'eql))))
         (buffer (and job (ende-finished--resolve-buffer job)))
         (chat-job (and job
                        (or (memq (ende-finished-job-operation job)
                                  '(chat chat-compact))
                            (ende-finished-job-request-id job)
                            (ende-finished-job-question-id job))))
         position)
    (unless job
      (user-error "No finished job selected"))
    (when buffer
      (with-current-buffer buffer
        (widen)
        (setq position
              (if chat-job
                  (or (ende-finished--find-chat-reply
                       (ende-finished-job-request-id job)
                       (eq (ende-finished-job-operation job) 'chat-compact))
                      (ende-finished--find-chat-question
                       (ende-finished-job-question-id job))
                      (ende-finished--recorded-position job buffer))
                (ende-finished--recorded-position job buffer))))
      (let ((window (progn (pop-to-buffer buffer) (selected-window))))
        (when position
          (with-selected-window window
            (goto-char position)
            (when (derived-mode-p 'org-mode)
              (org-reveal))
            (set-window-point window position)))))))

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
