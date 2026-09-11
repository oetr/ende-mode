;;; deslop.el --- Local-LLM text assistance for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Version: 0.3
;; Keywords: convenience, tools, llm
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; deslop is a minor mode that queues requests to an OpenAI-compatible LLM.
;;
;;   C-c e i - deslop-insert-at-cursor
;;   C-c e m - deslop-improve-at-cursor
;;   C-c e s - deslop-summarize
;;   C-c e a - deslop-abort-queue

;;; Code:

(require 'cl-lib)
(require 'deslop-common)
(require 'deslop-ui)
(require 'deslop-llm)
(require 'deslop-queue)
(require 'deslop-history)

;;;; Anchor and callback helpers

(defun deslop--make-marker (buffer position insertion-type)
  "Create a marker in BUFFER at POSITION with INSERTION-TYPE."
  (let ((marker (make-marker)))
    (set-marker marker position buffer)
    (set-marker-insertion-type marker insertion-type)
    marker))

(defun deslop--marker-position (marker)
  "Return MARKER's position, or nil when it no longer has a buffer."
  (and (markerp marker) (marker-position marker)))

(defun deslop--target-error (kind message)
  "Create a target error of KIND with MESSAGE."
  (deslop-error-create :kind kind :message message))

(defun deslop--job-error (job error)
  "Record ERROR for JOB and advance its queue."
  (deslop-history-event job (list :type :error :error error))
  (deslop-job-fail job error))

(defun deslop--job-event (job event)
  "Forward EVENT to the job history sink."
  (deslop-history-event job event))

(defun deslop--call-with-job-error (job function)
  "Call FUNCTION and settle JOB if it signals an error."
  (condition-case caught
      (funcall function)
    (error
     (deslop--job-error job caught))))

(defun deslop--result-content (result)
  "Extract completion content from RESULT or a legacy string result."
  (if (stringp result)
      result
    (plist-get result :content)))

(defun deslop--job-restriction-valid-p (job start end)
  "Return non-nil when START and END of JOB are accessible now."
  (ignore job)
  (and (number-or-marker-p start)
       (number-or-marker-p end)
       (<= (point-min) start)
       (<= end (point-max))
       (<= start end)))

(defun deslop--job-selected-source (job start end)
  "Return the current selected source for JOB, or signal a target error."
  (unless (deslop--job-restriction-valid-p job start end)
    (signal 'error (list "target is outside the current restriction")))
  (let ((source (buffer-substring-no-properties start end)))
    (unless (or (null (deslop-job-source-snapshot job))
                (string= source (deslop-job-source-snapshot job)))
      (signal 'error (list "selected source changed before dispatch")))
    source))

(defun deslop--advance-queued-anchors (buffer position new-position)
  "Advance pending anchors at POSITION to NEW-POSITION in BUFFER.
This preserves FIFO ordering for jobs queued at the same location while the
anchors remain stationary for edits made directly by the user."
  (dolist (queued-job deslop-queue)
    (let ((marker (deslop-job-cursor-marker queued-job)))
      (when (and (markerp marker)
                 (eq (marker-buffer marker) buffer)
                 (= (marker-position marker) position))
        (set-marker marker new-position)
        (let ((overlay (deslop-job-overlay queued-job)))
          (when (and (overlayp overlay)
                     (= (overlay-start overlay) position)
                     (= (overlay-end overlay) position))
            (move-overlay overlay new-position new-position buffer)))))))

(defun deslop--apply-insert-result (job result)
  "Apply RESULT for insert JOB atomically and settle JOB."
  (when (deslop-job-current-p job)
    (condition-case failure
        (let ((content (deslop-trim-trailing (or (deslop--result-content result) "")))
              (buffer (deslop-job-buffer job))
              (insertion-start nil)
              (insertion-end nil)
              (review-start nil)
              (review-end nil))
          (when (string-empty-p content)
            (signal 'error (list "empty completion")))
          (unless (buffer-live-p buffer)
            (signal 'error (list "target buffer no longer exists")))
          (with-current-buffer buffer
            (barf-if-buffer-read-only)
            (let* ((selected (deslop-job-get job :selected))
                   (start (deslop--marker-position
                           (deslop-job-start-marker job)))
                   (end (deslop--marker-position
                         (deslop-job-end-marker job)))
                   (cursor (deslop--marker-position
                            (deslop-job-cursor-marker job))))
              (unless cursor
                (signal 'error (list "cursor anchor no longer exists")))
              (when (and selected
                         (or (null start) (null end)
                             (> start end)))
                (signal 'error (list "selection anchors no longer exist")))
              (unless (deslop--job-restriction-valid-p
                       job (or start cursor) (or end cursor))
                (signal 'error (list "target is outside the current restriction")))
              (when selected
                (deslop--job-selected-source job start end))
              (setq insertion-start cursor)
              (atomic-change-group
                (save-excursion
                  (goto-char cursor)
                  (pcase (deslop-job-placement job)
                    (:before
                     (setq review-start (point))
                     (insert content)
                     (setq review-end (point))
                     (insert "\n"))
                    (:after
                     (insert "\n")
                     (setq review-start (point))
                     (insert content)
                     (setq review-end (point)))
                    (:inside
                     (insert "\n")
                     (setq review-start (point))
                     (insert content)
                     (setq review-end (point))
                     (insert "\n"))
                    (_
                     (setq review-start (point))
                     (insert content)
                     (setq review-end (point))))
                  (setq insertion-end (point)))))
          (deslop--advance-queued-anchors
           buffer insertion-start insertion-end)
          (when (and review-start review-end (< review-start review-end))
            (deslop-ui-create-review-overlay
             buffer review-start review-end
             (plist-get (deslop-job-profile job) :model)
             (deslop-provider-client-name (deslop-job-profile job))))
          (deslop--job-event
           job (list :type :done :result result
                     :content (deslop--result-content result)))
          (message "deslop: inserted %d chars in %s"
                   (length content) (buffer-name buffer))
           (deslop-job-finish job result)))
      (error
          (deslop--job-error job failure)))))

(defun deslop--insert-messages (_job context)
  "Build insertion messages for CONTEXT."
  (list (cons "system" deslop-insert-system)
        (cons "user" (format "Task: %s\n\nDocument:\n%s"
                              deslop-insert-instruction context))))

(defun deslop--run-insert (job)
  "Execute an insert JOB."
  (deslop--call-with-job-error
   job (lambda ()
      (let* ((buffer (deslop-job-buffer job))
             (selected (deslop-job-get job :selected))
             (start (deslop--marker-position (deslop-job-start-marker job)))
             (end (deslop--marker-position (deslop-job-end-marker job)))
             (cursor (deslop--marker-position (deslop-job-cursor-marker job)))
             (context nil))
        (unless (and cursor (buffer-live-p buffer))
          (signal 'error (list "target anchor no longer exists")))
        (with-current-buffer buffer
          (barf-if-buffer-read-only)
          (if selected
              (let* ((source (deslop--job-selected-source job start end))
                     (offset (plist-get (deslop-job-data job) :offset)))
                (setq context
                      (deslop-marker-context source (or offset 0))))
            (unless (deslop--job-restriction-valid-p job cursor cursor)
              (signal 'error (list "cursor is outside the current restriction")))
            (setq context
                  (deslop-marker-context
                   (buffer-substring-no-properties (point-min) (point-max))
                   (- cursor (point-min)))))
        (let ((messages (deslop--insert-messages job context)))
          (deslop-history-begin job messages)
          (deslop--job-event job (list :type :status :status "Thinking"))
          (when (deslop-job-overlay job)
            (deslop-ui-update-overlay (deslop-job-overlay job) :status "Thinking"))
          (funcall deslop-llm-function
                   messages
                   (list
                    :on-start
                    (lambda (proc)
                      (deslop-job-process-set job proc))
                    :on-status
                    (lambda (status)
                      (when (deslop-job-current-p job)
                        (when (deslop-job-overlay job)
                          (deslop-ui-update-overlay
                           (deslop-job-overlay job) :status status))))
                    :on-delta
                    (lambda (_type _text) nil)
                    :on-usage
                    (lambda (tokens speed &optional estimated)
                      (when (deslop-job-current-p job)
                        (when (deslop-job-overlay job)
                          (deslop-ui-update-overlay
                           (deslop-job-overlay job)
                           :tokens tokens :speed speed :estimated estimated))))
                    :on-event
                    (lambda (event)
                      (when (deslop-job-current-p job)
                        (deslop--job-event job event)))
                    :on-done
                    (lambda (result)
                      (deslop--apply-insert-result job result))
                    :on-error
                    (lambda (error)
                      (deslop--job-error job error)))
                     (deslop-job-profile job))))))))

(deslop-register-job-handler 'insert #'deslop--run-insert)

;;;; Improve and summarize

(defun deslop--replacement-messages (job instruction)
  "Build improvement messages for JOB and INSTRUCTION."
  (list (cons "system" deslop-improve-system)
        (cons "user"
              (format "Instruction: %s\n\nText to improve:\n%s"
                      instruction (deslop-job-source-snapshot job)))))

(defun deslop--run-improve (job)
  "Execute an improvement JOB that replaces its target atomically."
  (deslop--call-with-job-error
   job (lambda ()
      (let* ((buffer (deslop-job-buffer job))
             (start (deslop--marker-position (deslop-job-start-marker job)))
             (end (deslop--marker-position (deslop-job-end-marker job)))
             (instruction (deslop-job-get job :instruction))
             (review-start nil)
             (review-end nil))
        (unless (and (buffer-live-p buffer) start end)
          (signal 'error (list "improvement target no longer exists")))
        (with-current-buffer buffer
          (barf-if-buffer-read-only)
          (deslop--job-selected-source job start end))
        (let ((messages (deslop--replacement-messages job instruction)))
          (deslop-history-begin job messages)
          (when (deslop-job-overlay job)
            (deslop-ui-update-overlay (deslop-job-overlay job) :status "Thinking"))
          (funcall deslop-llm-function
                   messages
                   (list
                    :on-start (lambda (proc) (deslop-job-process-set job proc))
                    :on-status
                    (lambda (status)
                      (when (deslop-job-current-p job)
                        (when (deslop-job-overlay job)
                          (deslop-ui-update-overlay
                           (deslop-job-overlay job) :status status))))
                    :on-delta
                    (lambda (_type _text) nil)
                    :on-usage
                    (lambda (tokens speed &optional estimated)
                      (when (deslop-job-current-p job)
                        (when (deslop-job-overlay job)
                          (deslop-ui-update-overlay
                           (deslop-job-overlay job)
                           :tokens tokens :speed speed :estimated estimated))))
                    :on-event
                    (lambda (event)
                      (when (deslop-job-current-p job)
                        (deslop--job-event job event)))
                    :on-done
                    (lambda (result)
                      (when (deslop-job-current-p job)
                        (condition-case apply-error
                            (let* ((content
                                    (deslop-trim-trailing
                                     (or (deslop--result-content result) "")))
                                   (live-start
                                    (deslop--marker-position
                                     (deslop-job-start-marker job)))
                                   (live-end
                                    (deslop--marker-position
                                     (deslop-job-end-marker job))))
                              (when (string-empty-p content)
                                (signal 'error (list "empty improvement")))
                              (unless (and live-start live-end)
                                (signal 'error
                                        (list "improvement target no longer exists")))
                              (with-current-buffer buffer
                                (barf-if-buffer-read-only)
                                (deslop--job-selected-source
                                 job live-start live-end)
                                (atomic-change-group
                                  (delete-region live-start live-end)
                                  (goto-char live-start)
                                  (setq review-start (point))
                                  (insert content)
                                  (setq review-end (point))))
                              (deslop-ui-create-review-overlay
                               buffer review-start review-end
                               (plist-get (deslop-job-profile job) :model)
                               (deslop-provider-client-name
                                (deslop-job-profile job)))
                              (deslop--job-event
                               job (list :type :done :result result
                                         :content content))
                              (deslop-job-finish job result))
                          (error
                           (deslop--job-error
                            job (deslop--target-error
                                 'application
                                 (error-message-string apply-error)))))))
                    :on-error (lambda (error) (deslop--job-error job error)))
                    (deslop-job-profile job)))))))
(deslop-register-job-handler 'improve #'deslop--run-improve)

(defun deslop--run-summarize (job)
  "Execute a summarize JOB and write its result to a separate buffer."
  (deslop--call-with-job-error
   job (lambda ()
      (let* ((buffer (deslop-job-buffer job))
             (start (deslop--marker-position (deslop-job-start-marker job)))
             (end (deslop--marker-position (deslop-job-end-marker job)))
             (source nil)
             (review-buffer nil)
             (review-start nil)
             (review-end nil))
        (unless (and (buffer-live-p buffer) start end)
          (signal 'error (list "summary target no longer exists")))
        (with-current-buffer buffer
          (setq source (deslop--job-selected-source job start end)))
        (let ((messages (list (cons "system" deslop-summarize-system)
                              (cons "user" (format "Text to summarize:\n%s"
                                                   source)))))
          (deslop-history-begin job messages)
          (funcall deslop-llm-function
                   messages
                   (list
                    :on-start (lambda (proc) (deslop-job-process-set job proc))
                    :on-status
                    (lambda (status)
                      (ignore status))
                    :on-delta
                    (lambda (_type _text) nil)
                    :on-usage
                    (lambda (tokens speed &optional estimated)
                      (ignore tokens speed estimated))
                    :on-event
                    (lambda (event)
                      (when (deslop-job-current-p job)
                        (deslop--job-event job event)))
                    :on-done
                    (lambda (result)
                      (when (deslop-job-current-p job)
                        (condition-case apply-error
                            (let ((content
                                   (deslop-trim-trailing
                                    (or (deslop--result-content result) ""))))
                              (when (string-empty-p content)
                                (signal 'error (list "empty summary")))
                              (with-current-buffer
                                  (setq review-buffer
                                        (get-buffer-create
                                         deslop-summary-buffer-name))
                                (goto-char (point-max))
                                (unless (bobp) (insert "\n\n"))
                                (insert (format "Summary for %s:\n"
                                                (buffer-name buffer)))
                                (setq review-start (point))
                                (insert content)
                                (setq review-end (point)))
                              (deslop-ui-create-review-overlay
                               review-buffer review-start review-end
                               (plist-get (deslop-job-profile job) :model)
                               (deslop-provider-client-name
                                (deslop-job-profile job)))
                              (deslop--job-event
                               job (list :type :done :result result
                                         :content content))
                              (deslop-job-finish job result))
                          (error
                           (deslop--job-error
                            job (deslop--target-error
                                 'application
                                 (error-message-string apply-error)))))))
                    :on-error (lambda (error) (deslop--job-error job error)))
                    (deslop-job-profile job)))))))

(deslop-register-job-handler 'summarize #'deslop--run-summarize)

;;;; Interactive commands

(defun deslop--enqueue-insert-like (type &optional instruction)
  "Queue TYPE for the current region/cursor, using INSTRUCTION if supplied."
  (let* ((buffer (current-buffer))
         (region (deslop-current-region))
         (point-position (point))
         (selected (and region t))
         (start (if region (car region) point-position))
         (end (if region (cdr region) point-position))
         (placement (if (not region)
                        nil
                      (cond ((= point-position start) :before)
                            ((= point-position end) :after)
                            (t :inside))))
         (source (and region
                      (buffer-substring-no-properties start end)))
         (overlay (deslop-ui-create-overlay buffer start end "Queued"))
         (data (list :buffer buffer
                     :selected selected
                     :placement placement
                     :offset (and region (- point-position start))
                     :source-snapshot source
                     :overlay overlay
                     :start-marker
                     (deslop--make-marker buffer start (and region t))
                      :end-marker
                      (deslop--make-marker buffer end nil)
                      :cursor-marker
                      (deslop--make-marker buffer point-position nil)
                     :restriction-beg (point-min)
                     :restriction-end (point-max))))
    (when instruction
      (setq data (plist-put data :instruction instruction)))
    (let ((job (deslop-enqueue (cons type data))))
      (message "deslop: queued %s job %d" type (deslop-job-id job))
      job)))

(defun deslop-insert-at-cursor ()
  "Queue a request to insert LLM text at cursor position."
  (interactive)
  (deslop--enqueue-insert-like 'insert))

(defun deslop-improve-at-cursor (instruction)
  "Queue an improvement of the active region or accessible buffer.
INSTRUCTION describes the desired improvement."
  (interactive (list (read-string "How should the text be improved? ")))
  (let* ((buffer (current-buffer))
         (region (deslop-current-region))
         (start (if region (car region) (point-min)))
         (end (if region (cdr region) (point-max)))
         (source (buffer-substring-no-properties start end))
         (overlay (deslop-ui-create-overlay buffer start end "Queued"))
         (data (list :buffer buffer
                     :selected t
                     :source-snapshot source
                     :instruction instruction
                     :overlay overlay
                     :start-marker (deslop--make-marker buffer start t)
                     :end-marker (deslop--make-marker buffer end nil)
                     :cursor-marker (deslop--make-marker buffer start t)
                     :restriction-beg (point-min)
                     :restriction-end (point-max))))
    (let ((job (deslop-enqueue (cons 'improve data))))
      (message "deslop: queued improve job %d" (deslop-job-id job))
      job)))

(defun deslop-summarize ()
  "Queue a summary of the active region or accessible buffer."
  (interactive)
  (let* ((buffer (current-buffer))
         (region (deslop-current-region))
         (start (if region (car region) (point-min)))
         (end (if region (cdr region) (point-max)))
         (source (buffer-substring-no-properties start end))
         (overlay (deslop-ui-create-overlay buffer start end "Queued"))
         (data (list :buffer buffer
                     :selected t
                     :source-snapshot source
                     :overlay overlay
                     :start-marker (deslop--make-marker buffer start t)
                     :end-marker (deslop--make-marker buffer end nil)
                     :cursor-marker (deslop--make-marker buffer start t)
                     :restriction-beg (point-min)
                     :restriction-end (point-max))))
    (let ((job (deslop-enqueue (cons 'summarize data))))
      (message "deslop: queued summarize job %d" (deslop-job-id job))
      job)))

(defun deslop--review-overlays-at-point ()
  "Return review overlays touching point or the character before it."
  (let ((positions (delq nil
                         (list (point)
                               (and (> (point) (point-min))
                                    (1- (point)))))))
    (delete-dups
     (cl-remove-if-not
      #'deslop-ui-review-overlay-p
      (apply #'append (mapcar #'overlays-at positions))))))

(defun deslop-approve-output ()
  "Approve generated output touching point and remove its review marking.
With an active region, approve all review overlays intersecting the region."
  (interactive)
  (let ((overlays
         (if (use-region-p)
             (cl-remove-if-not
              #'deslop-ui-review-overlay-p
              (overlays-in (region-beginning) (region-end)))
           (deslop--review-overlays-at-point))))
    (unless overlays
      (user-error "No deslop output awaiting approval here"))
    (dolist (overlay overlays)
      (deslop-ui-delete-review-overlay overlay))
    (message "deslop: approved %d generated region%s"
             (length overlays)
             (if (= (length overlays) 1) "" "s"))))

;;;; Keymap and minor mode

(defun deslop--make-mode-map ()
  "Build the keymap for `deslop-mode'."
  (let ((map (make-sparse-keymap))
        (prefix (make-sparse-keymap)))
    (define-key prefix "i" #'deslop-insert-at-cursor)
    (define-key prefix "m" #'deslop-improve-at-cursor)
    (define-key prefix "s" #'deslop-summarize)
    (define-key prefix "q" #'deslop-queue-status)
    (define-key prefix "a" #'deslop-abort-queue)
    (define-key map (kbd "C-c e") prefix)
    map))

(defvar deslop-mode-map
  (deslop--make-mode-map)
  "Keymap for `deslop-mode'.")

;;;###autoload
(define-minor-mode deslop-mode
  "Minor mode for queueing LLM requests."
  :lighter " deslop"
  :keymap deslop-mode-map)

;;;###autoload
(defun deslop-reload ()
  "Abort active jobs and reload all deslop files in dependency order."
  (interactive)
  (deslop-abort-queue)
  (let* ((dir (or (file-name-directory (locate-library "deslop.el"))
                  (user-error "deslop.el not found on load-path")))
         (files '("deslop-common.el" "deslop-ui.el" "deslop-llm.el"
                  "deslop-queue.el" "deslop-history.el" "deslop.el")))
    (dolist (file files)
      (load (expand-file-name file dir)))
    (setq deslop-mode-map (deslop--make-mode-map))
    (message "deslop: reloaded from %s" dir)))

(provide 'deslop)

;;; deslop.el ends here
