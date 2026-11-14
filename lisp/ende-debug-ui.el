;;; ende-debug-ui.el --- Local diagnostic viewer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter Samarin
;; SPDX-License-Identifier: AGPL-3.0-only

;;; Commentary:

;; One disposable metadata view, fed by the existing job event hook.

;;; Code:

(require 'cl-lib)
(require 'ende-debug)
(require 'ende-queue)

(defvar ende-debug-ui--jobs (make-hash-table :test #'eq :weakness 'key))
(defvar ende-debug-ui--buffer nil)
(defvar-local ende-debug-ui--owned-buffer nil)
(defvar-local ende-debug-ui--job nil)
(defvar-local ende-debug-ui--expanded nil)
(defvar-local ende-debug-ui--detail-event nil)

(defun ende-debug-ui--job-event (job event)
  "Record the known lifecycle EVENT of JOB without retaining its contents."
  (when ende-diagnostics-enabled
    (condition-case nil
        (let* ((type (plist-get event :type))
               (context (if (eq type :queued)
                            (puthash job (ende-diagnostics-context-create (ende-job-id job))
                                     ende-debug-ui--jobs)
                          (gethash job ende-debug-ui--jobs)))
               (parent (ende-job-parent job))
               (kind (pcase type
                       (:queued 'job-queued) (:started 'job-started)
                       (:resumed 'job-resumed) (:suspended 'job-waiting)
                       (:settled
                        (pcase (plist-get event :state)
                          ('completed 'job-done) ('failed 'job-failed)
                          ('cancelled 'job-cancelled) ('skipped 'job-skipped))))))
          (when (and context kind)
            (ende-diagnostics-record kind context :parent (and parent (ende-job-id parent)))
            (when (and parent (eq type :queued))
              (ende-diagnostics-record 'agent-spawned context :parent (ende-job-id parent)))
            (when (and (eq kind 'job-failed) (ende-error-p (ende-job-error job)))
              (ende-diagnostics-fail context (ende-job-error job))))
          (when (eq type :settled) (remhash job ende-debug-ui--jobs)))
      ((error quit) nil))))

(add-hook 'ende-job-event-functions #'ende-debug-ui--job-event)

(defun ende-debug-ui--details (event)
  "Format only typed metadata fields of EVENT."
  (mapconcat
   #'identity
   (cl-loop for spec in ende-diagnostics--fields
            for key = (car spec)
            when (plist-member event key)
            collect (format "%s=%s" (nth 2 spec) (plist-get event key)))
   " "))

(defun ende-debug-ui-refresh ()
  "Refresh the current metadata view without changing the retained events."
  (interactive)
  (let ((inhibit-read-only t)
        (position (point)))
    (erase-buffer)
    (insert (format "Ende diagnostics · session %d · %s · %s · %d retained · %d overwritten\n"
                    ende-diagnostics-session
                    (if ende-diagnostics-enabled "recording" "stopped")
                    (if ende-debug-ui--expanded "timeline" "summary")
                    (if ende-diagnostics--ring (ring-length ende-diagnostics--ring) 0)
                    ende-diagnostics-dropped))
    (insert (format "Scope: %s   TAB timeline  RET fields  g refresh  e export  a all jobs  q quit\n\n"
                    (if ende-debug-ui--job (format "job %d" ende-debug-ui--job) "all jobs")))
    (insert (propertize (format "%-12s %-4s %8s %8s %-22s %s\n"
                                "TIME" "MARK" "JOB" "REQUEST" "EVENT" "DETAILS")
                        'face 'bold))
    (dolist (event (ende-diagnostics-events ende-debug-ui--job))
      (let* ((level (plist-get event :level))
             (mark (pcase level ('error "x") ('warning "!") ('outcome "+") (_ ".")))
             (face (pcase level ('error 'error) ('warning 'warning)
                          ('outcome 'success) (_ 'shadow))))
        (when (or ende-debug-ui--expanded (not (eq level 'detail)))
          (let ((start (point)))
            (insert (format "%s %-4s %8s %8s %-22s %s\n"
                            (format-time-string "%H:%M:%S.%3N" (plist-get event :time))
                            mark (or (plist-get event :job) "-")
                            (or (plist-get event :request) "-") (plist-get event :kind)
                            (ende-debug-ui--details event)))
            (add-text-properties start (point) (list 'ende-debug-event event 'face face))))))
    (when ende-debug-ui--detail-event
      (insert "\nSelected event fields\n")
      (let ((fields ende-debug-ui--detail-event))
        (while fields
          (insert (format "%-20s %s\n" (pop fields) (pop fields))))))
    (goto-char (min position (point-max)))))

(defun ende-debug-ui-toggle-timeline ()
  "Toggle expanded start/detail events without altering recording."
  (interactive)
  (setq ende-debug-ui--expanded (not ende-debug-ui--expanded))
  (ende-debug-ui-refresh))

(defun ende-debug-ui-event-details ()
  "Toggle the selected row's retained scalar fields in this viewer."
  (interactive)
  (let ((event (get-text-property (line-beginning-position) 'ende-debug-event)))
    (unless event (user-error "No diagnostic event on this line"))
    (setq ende-debug-ui--detail-event
          (unless (equal event ende-debug-ui--detail-event) event))
    (ende-debug-ui-refresh)))

(defun ende-debug-ui-all-jobs ()
  "Show events for all jobs in the retained session."
  (interactive)
  (setq ende-debug-ui--job nil ende-debug-ui--detail-event nil)
  (ende-debug-ui-refresh))

(defun ende-debug-ui-export (file)
  "Export the previewed job scope as metadata-only JSON to local FILE.
Both summary and timeline export all retained events for the selected scope."
  (interactive (list (read-file-name "Export local diagnostic metadata: "
                                     user-emacs-directory nil nil "ende-diagnostics.json")))
  (when (file-remote-p file) (user-error "Diagnostics exports must stay local"))
  (when (and (file-exists-p file) (called-interactively-p 'interactive)
             (not (yes-or-no-p "Replace existing export? ")))
    (user-error "Export cancelled"))
  (let ((saved (ende-debug-save-diagnostics file ende-debug-ui--job)))
    (unless saved (user-error "Could not save diagnostic metadata"))
    (ende-diagnostics-record 'bundle-saved (ende-diagnostics-context-create ende-debug-ui--job))
    (ende-debug-ui-refresh)
    (message "Saved local metadata: %s" saved)
    saved))

(defvar ende-debug-ui-mode-map)
(setq ende-debug-ui-mode-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map special-mode-map)
        (define-key map (kbd "g") #'ende-debug-ui-refresh)
        (define-key map (kbd "TAB") #'ende-debug-ui-toggle-timeline)
        (define-key map (kbd "RET") #'ende-debug-ui-event-details)
        (define-key map (kbd "e") #'ende-debug-ui-export)
        (define-key map (kbd "a") #'ende-debug-ui-all-jobs)
        map))

(define-derived-mode ende-debug-ui-mode special-mode "Ende diagnostics"
  "Inspect bounded local metadata, never request content."
  (setq-local truncate-lines t)
  (buffer-disable-undo)
  (setq ende-debug-ui--buffer (current-buffer)))

(when (buffer-live-p ende-debug-ui--buffer)
  (with-current-buffer ende-debug-ui--buffer
    (setq ende-debug-ui--detail-event nil)
    (use-local-map ende-debug-ui-mode-map)
    (ende-debug-ui-refresh)))

;;;###autoload
(defun ende-debug-open (&optional job)
  "Open the metadata viewer, optionally scoped to numeric JOB."
  (interactive)
  (let ((buffer (ende-owned-buffer ende-debug-ui--buffer "*ende-diagnostics*"
                                   'ende-debug-ui--owned-buffer #'ende-debug-ui-mode)))
    (with-current-buffer buffer
      (setq ende-debug-ui--job job ende-debug-ui--detail-event nil)
      (ende-debug-ui-refresh))
    (pop-to-buffer buffer)
    buffer))

(provide 'ende-debug-ui)
;;; ende-debug-ui.el ends here
