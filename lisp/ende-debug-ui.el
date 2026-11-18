;;; ende-debug-ui.el --- Local diagnostic viewer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter Samarin
;; SPDX-License-Identifier: AGPL-3.0-only

;;; Commentary:

;; One disposable metadata view, fed by the existing job event hook.

;;; Code:

(require 'cl-lib)
(require 'ende-debug)
(require 'ende-queue)

(defvar ende-debug-ui--buffer nil)
(defvar-local ende-debug-ui--owned-buffer nil)
(defvar-local ende-debug-ui--job nil)
(defvar-local ende-debug-ui--expanded t)
(defvar-local ende-debug-ui--detail-event nil)

(defun ende-debug-ui--job-event (job event)
  "Record the known lifecycle EVENT of JOB without retaining its contents."
  (when ende-diagnostics-enabled
    (condition-case nil
        (let* ((type (plist-get event :type))
               (context (if (eq type :queued)
                            (setf (ende-job-diagnostics-context job)
                                  (ende-diagnostics-context-create (ende-job-id job)))
                          (ende-job-diagnostics-context job)))
               (parent (ende-job-parent job))
               (kind (pcase type
                       (:queued 'job-queued) (:started 'job-started)
                       (:resumed 'job-resumed) (:suspended 'job-waiting)
                       (:work-checkpoint 'work-checkpoint)
                       (:chat-saved 'chat-saved) (:chat-save-failed 'chat-save-failed)
                       (:work-retained 'work-retained)
                       (:settled
                        (pcase (plist-get event :state)
                          ('completed 'job-done) ('failed 'job-failed)
                          ('cancelled 'job-cancelled) ('skipped 'job-skipped))))))
          (when (and context kind)
            (apply #'ende-diagnostics-record kind context
                   :parent (and parent (ende-job-id parent))
                   :state (plist-get event :state)
                   (when (eq type :settled)
                     (list :error-kind (if (ende-error-p (ende-job-error job))
                                           (ende-error-kind (ende-job-error job))
                                         (and (eq kind 'job-failed) 'internal))
                           :tools (or (ende-job-tool-call-count job) 0)
                           :elapsed (when (and (ende-job-started-at job)
                                               (ende-job-finished-at job))
                                      (- (ende-job-finished-at job)
                                         (ende-job-started-at job))))))
            (when (and parent (eq type :queued))
              (ende-diagnostics-record 'agent-spawned context :parent (ende-job-id parent)))))
      ((error quit) nil))))

(add-hook 'ende-job-event-functions #'ende-debug-ui--job-event)

(defun ende-debug-ui--appearance (event)
  "Return EVENT's distinct mark and theme face."
  (let ((kind (plist-get event :kind)))
    (cond
     ((memq kind '(job-cancelled tool-cancelled request-cancelled debug-off)) '("■" shadow))
     ((eq kind 'retry-scheduled) '("↻" warning))
     ((eq (plist-get event :level) 'error) '("✗" error))
     ((eq (plist-get event :level) 'warning) '("!" warning))
     ((memq kind '(debug-on job-started job-resumed agent-spawned auth-started
                    prep-started ws-connecting request-offered request-writable tool-started))
      '("▶" font-lock-function-name-face))
     ((memq kind '(capture-saved bundle-saved captures-pruned job-done auth-ready
                    prep-ready ws-open response-done tool-done work-checkpoint chat-saved))
      '("✓" success))
     (t '("·" shadow)))))

(defun ende-debug-ui--brief-keys (kind)
  "Return the few fields worth scanning on KIND's row."
  (pcase kind
    ('ws-connecting '(:connection :timeout-limit))
    ((or 'ws-open 'ws-reused) '(:connection :connection-age))
    ((or 'ws-closed 'ws-interrupted) '(:connection :close-code :cause))
    ('retry-scheduled '(:delay :reason :retry-limit))
    ('retries-exhausted '(:retries :retry-limit))
    ('timeout '(:deadline :timeout-limit :elapsed))
    ('limit-exceeded '(:bytes :byte-limit :event-limit))
    ('response-first '(:last-event :elapsed))
    ('response-done '(:elapsed :messages :bytes))
    ('request-accepted '(:scope))
    ((or 'tool-started 'tool-done 'tool-failed 'tool-cancelled) '(:tool :call :elapsed))
    ((or 'job-done 'job-failed 'job-cancelled 'job-skipped) '(:error-kind :elapsed :tools))
    ((or 'job-queued 'agent-spawned) '(:parent))
    ((or 'debug-on 'bundle-saved) '(:content))
    ('content-enabled '(:scope))
    ('debug-failed '(:operation))
    ('chat-saved '(:state))
    (_ '(:error-kind :bytes :elapsed :count))))

(defun ende-debug-ui--value (key value)
  "Format typed VALUE of KEY without serialized Lisp or long decimals."
  (let ((type (cadr (assq key ende-diagnostics--fields))))
    (cond
     ((eq type 'boolean) (if (eq value t) "yes" "no"))
     ((eq type 'number)
      (cond ((>= value 1) (format "%.3gs" value))
            ((>= value 0.001) (format "%.3gms" (* value 1000)))
            ((> value 0) (format "%.3gµs" (* value 1000000)))
            (t "0s")))
     (t (format "%s" value)))))

(defun ende-debug-ui--details (event)
  "Format EVENT's primary fields; all fields remain available on RET."
  (mapconcat
   #'identity
   (cl-loop for key in (ende-debug-ui--brief-keys (plist-get event :kind))
            when (plist-member event key)
            collect (format "%s=%s" (nth 2 (assq key ende-diagnostics--fields))
                            (ende-debug-ui--value key (plist-get event key))))
   " "))

(defun ende-debug-ui-refresh ()
  "Refresh the current metadata view without changing the retained events."
  (interactive)
  (let ((inhibit-read-only t)
        (position (point))
        (selected (get-text-property (line-beginning-position) 'ende-debug-event))
        (column (current-column))
        selected-position)
    (erase-buffer)
    (insert (format "SESSION %d   %s   %s   JOB %s\n"
                    ende-diagnostics-session
                    (if ende-diagnostics-enabled "RECORDING" "STOPPED")
                    (if ende-debug-ui--expanded "TIMELINE" "SUMMARY")
                    (or ende-debug-ui--job "ALL")))
    (insert (propertize "TAB timeline   RET fields   g refresh   e export   a all jobs   q quit\n"
                        'face 'shadow))
    (when (> ende-diagnostics-dropped 0)
      (insert (propertize (format "! CAPTURE DROPPED   events=%d\n" ende-diagnostics-dropped)
                          'face 'warning)))
    (insert "\n" (propertize (format "%-12s  %s  %4s  %7s  %-19s %s\n"
                                        "TIME" " " "JOB" "REQ/TRY" "EVENT" "DETAILS")
                                'face 'bold))
    (dolist (event (ende-diagnostics-events ende-debug-ui--job))
      (when (or ende-debug-ui--expanded (not (eq (plist-get event :level) 'detail)))
        (pcase-let* ((`(,mark ,face) (ende-debug-ui--appearance event))
                     (kind (plist-get event :kind))
                     (request (plist-get event :request))
                     (start (point)))
          (insert (format "%s  %s  %4s  %7s  %s %s\n"
                          (propertize (format-time-string "%H:%M:%S.%3N" (plist-get event :time))
                                      'face 'shadow)
                          (propertize mark 'face face)
                          (or (plist-get event :job) "—")
                          (if request (format "%s/%s" request (plist-get event :attempt)) "—")
                          (propertize (format "%-19s" (upcase (replace-regexp-in-string "-" " " (symbol-name kind))))
                                      'face face)
                          (ende-debug-ui--details event)))
          (when (equal event selected)
            (setq selected-position start))
          (when (equal event ende-debug-ui--detail-event)
            (let ((fields event))
              (while fields
                (let ((key (pop fields)) (value (pop fields)))
                  (unless (memq key '(:time :level :kind))
                    (insert (format "    │ %-18s %s\n"
                                    (propertize (upcase (substring (symbol-name key) 1)) 'face 'shadow)
                                    (ende-debug-ui--value key value))))))))
          (add-text-properties start (point) (list 'ende-debug-event event)))))
    (goto-char (or selected-position (min position (point-max))))
    (when selected-position (move-to-column column))))

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
    (ende-diagnostics-record 'bundle-saved (ende-diagnostics-context-create ende-debug-ui--job)
                             :content 'excluded)
    (ende-debug-ui-refresh)
    (message "✓ BUNDLE SAVED  %s" saved)
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
  "Inspect local metadata, never request content."
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
