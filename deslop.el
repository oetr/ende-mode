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
;;   C-c e p - deslop-approve-output
;;   C-c e r - deslop-reject-output

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

(defun deslop--anchor-position (anchor)
  "Return ANCHOR as a live marker position or numeric position."
  (cond
   ((markerp anchor) (marker-position anchor))
   ((integerp anchor) anchor)))

(defun deslop--target-error (kind message)
  "Create a target error of KIND with MESSAGE."
  (deslop-error-create :kind kind :message message))

(defun deslop--job-error (job error)
  "Record ERROR for JOB and advance its queue."
  (deslop-job-fail job error))

(defun deslop--job-event (job event)
  "Forward EVENT to the job history sink."
  (deslop-history-event job event))

(defun deslop--apply-job-metadata (job metadata)
  "Apply endpoint METADATA to JOB's overlay and history."
  (when (deslop-job-current-p job)
    (let* ((source (plist-get metadata :source))
           (rank (pcase source (:response 2) (:discovery 1) (_ 0)))
           (current-rank (or (deslop-job-get job :metadata-rank) -1)))
      (when (>= rank current-rank)
        (deslop-job-put job :metadata-rank rank)
        (deslop-job-put job :metadata metadata)
        (let ((display (deslop-provider-display-metadata
                        (deslop-job-profile job) metadata)))
          (when (deslop-job-overlay job)
            (deslop-ui-update-overlay
             (deslop-job-overlay job)
             :client (plist-get display :client)
             :model (plist-get display :model)))
          (deslop--job-event
           job (list :type :metadata :metadata display)))))))

(defun deslop--job-display-metadata (job result)
  "Return display metadata for JOB using RESULT and cached discovery."
  (let* ((profile (deslop-job-profile job))
         (metadata (copy-sequence
                    (or (deslop-job-get job :metadata)
                        (deslop-provider-cached-metadata profile)
                        nil)))
         (reported-model (and (listp result)
                              (plist-get result :reported-model)))
         (reported-provider (and (listp result)
                                 (plist-get result :reported-provider))))
    (when (stringp reported-model)
      (setq metadata (plist-put metadata :model reported-model)))
    (when (stringp reported-provider)
      (setq metadata (plist-put metadata :provider reported-provider))
      (setq metadata (plist-put metadata :client reported-provider)))
    (deslop-provider-display-metadata profile metadata)))

(defun deslop--call-with-job-error (job function)
  "Call FUNCTION and settle JOB if it signals an error."
  (condition-case caught
      (funcall function)
    ((error quit)
     (deslop--job-error job caught))))

(defun deslop--result-content (result)
  "Extract and validate completion content from RESULT or a legacy string."
  (let ((content (if (stringp result)
                     result
                   (plist-get result :content))))
    (when (and (stringp content)
               (> (length content) deslop-max-output-chars))
      (error "completion exceeds deslop-max-output-chars"))
    content))

(defun deslop--validate-messages (messages)
  "Return MESSAGES after enforcing the final request character budget."
  (unless (and (integerp deslop-max-request-chars)
               (> deslop-max-request-chars 0))
    (error "deslop-max-request-chars must be a positive integer"))
  (let ((characters
         (cl-loop for message in messages
                  sum (+ (length (format "%s" (car message)))
                         (length (format "%s" (cdr message)))))))
    (when (> characters deslop-max-request-chars)
      (error "request is %d characters; deslop-max-request-chars is %d"
             characters deslop-max-request-chars)))
  messages)

(defun deslop--job-restriction-valid-p (job start end)
  "Return non-nil when START and END of JOB are accessible now."
  (let ((restriction-start
         (deslop--anchor-position (deslop-job-restriction-beg job)))
        (restriction-end
         (deslop--anchor-position (deslop-job-restriction-end job))))
    (and (number-or-marker-p start)
         (number-or-marker-p end)
         restriction-start restriction-end
         (<= (point-min) restriction-start start)
         (<= start end restriction-end (point-max)))))

(defmacro deslop--with-job-restriction (job &rest body)
  "Evaluate BODY within both JOB's original and current restrictions."
  (declare (indent 1) (debug t))
  `(let ((current-start (point-min))
         (current-end (point-max)))
     (save-restriction
       (widen)
       (let* ((original-start
               (deslop--anchor-position (deslop-job-restriction-beg ,job)))
              (original-end
               (deslop--anchor-position (deslop-job-restriction-end ,job)))
              (restriction-start
               (and original-start (max original-start current-start)))
              (restriction-end
               (and original-end (min original-end current-end))))
         (unless (and restriction-start restriction-end
                      (<= (point-min) restriction-start restriction-end)
                      (<= restriction-end (point-max)))
           (error "original buffer restriction no longer exists"))
         (narrow-to-region restriction-start restriction-end)
         ,@body))))

(defun deslop--region-digest (start end)
  "Return a stable digest of the current buffer between START and END."
  (secure-hash 'sha256 (current-buffer) start end))

(defun deslop--validate-selected-source (job start end)
  "Signal when JOB's selected source at START..END has changed."
  (unless (deslop--job-restriction-valid-p job start end)
    (error "target is outside the original restriction"))
  (let ((digest (deslop-job-source-digest job))
        (snapshot (deslop-job-source-snapshot job)))
    (cond
     (digest
      (unless (string= digest (deslop--region-digest start end))
        (error "selected source changed before dispatch")))
     (snapshot
      (unless (string= snapshot (buffer-substring-no-properties start end))
        (error "selected source changed before dispatch"))))))

(defun deslop--job-selected-source (job start end)
  "Return the current selected source for JOB, or signal a target error."
  (deslop--validate-selected-source job start end)
  (buffer-substring-no-properties start end))

(defun deslop--buffer-marker-context (start end offset)
  "Return bounded buffer context from START to END with marker at OFFSET."
  (setq offset (max start (min end offset)))
  (let* ((length (- end start))
         (relative-offset (- offset start))
         (bounds (deslop-context-window length relative-offset))
         (window-start (+ start (car bounds)))
         (window-end (+ start (cdr bounds))))
    (deslop-format-marker-context
     (buffer-substring-no-properties window-start offset)
     (buffer-substring-no-properties offset window-end)
     (> window-start start)
     (< window-end end))))

(defun deslop--ensure-target-size (start end operation)
  "Reject an OPERATION target larger than the configured context limit."
  (when (> (- end start) deslop-max-context-chars)
    (user-error "%s target is %d characters; limit is %d"
                operation (- end start) deslop-max-context-chars)))

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
                      (overlay-buffer overlay)
                      (numberp (overlay-start overlay))
                      (numberp (overlay-end overlay))
                      (= (overlay-start overlay) position)
                      (= (overlay-end overlay) position))
             (move-overlay overlay new-position new-position buffer)))))))

(defun deslop--apply-insert-result (job result)
  "Apply RESULT for insert JOB atomically and settle JOB."
  (when (deslop-job-current-p job)
    (condition-case failure
        (let* ((content
                (deslop-trim-trailing
                 (or (deslop--result-content result) "")))
              (buffer (deslop-job-buffer job))
              (display (deslop--job-display-metadata job result))
              (insertion-start nil)
              (insertion-end nil)
              (review-overlay nil))
          (when (string-empty-p content)
            (signal 'error (list "empty completion")))
          (unless (buffer-live-p buffer)
            (signal 'error (list "target buffer no longer exists")))
          (with-current-buffer buffer
            (deslop--with-job-restriction job
              (barf-if-buffer-read-only)
              (let* ((selected (deslop-job-get job :selected))
                     (start (deslop--marker-position
                             (deslop-job-start-marker job)))
                     (end (deslop--marker-position
                           (deslop-job-end-marker job)))
                     (cursor (deslop--marker-position
                              (deslop-job-cursor-marker job))))
                (unless cursor
                  (error "cursor anchor no longer exists"))
                (when (and selected
                           (or (null start) (null end) (> start end)))
                  (error "selection anchors no longer exist"))
                (unless (deslop--job-restriction-valid-p
                         job (or start cursor) (or end cursor))
                  (error "target is outside the original restriction"))
                (when selected
                  (deslop--validate-selected-source job start end))
                (setq insertion-start cursor)
                (deslop-job-put job :applying t)
                (unwind-protect
                    (atomic-change-group
                      (save-excursion
                        (goto-char cursor)
                        (pcase (deslop-job-placement job)
                          (:before (insert content "\n"))
                          (:after (insert "\n" content))
                          (:inside (insert "\n" content "\n"))
                          (_ (insert content)))
                        (setq insertion-end (point))
                        (setq review-overlay
                              (deslop-ui-create-review-overlay
                               buffer insertion-start insertion-end
                               (plist-get display :model)
                               (plist-get display :client) ""))))
                  (deslop-job-put job :applying nil)))))
          (condition-case nil
              (deslop--advance-queued-anchors
               buffer insertion-start insertion-end)
            ((error quit) nil))
          (unless (deslop-ui-review-overlay-p review-overlay)
            (error "generated output could not be marked for review"))
          (deslop--job-event
           job (list :type :done :result result
                     :content (deslop--result-content result)))
          (condition-case nil
              (message "deslop: inserted %d chars in %s"
                       (length content) (buffer-name buffer))
            ((error quit) nil))
          (deslop-job-finish job result))
      ((error quit)
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
          (deslop--with-job-restriction job
            (barf-if-buffer-read-only)
            (if selected
                (let ((offset (or (deslop-job-get job :offset) 0)))
                  (deslop--validate-selected-source job start end)
                  (setq context
                        (deslop--buffer-marker-context
                         start end (+ start offset))))
              (unless (deslop--job-restriction-valid-p job cursor cursor)
                (error "cursor is outside the original restriction"))
              (setq context
                    (deslop--buffer-marker-context
                     (point-min) (point-max) cursor)))))
        (let ((messages (deslop--insert-messages job context)))
          (deslop--validate-messages messages)
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
                     :on-metadata
                     (lambda (metadata)
                       (deslop--apply-job-metadata job metadata))
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
                     (deslop-job-profile job)))))))

(deslop-register-job-handler 'insert #'deslop--run-insert)

;;;; Improve and summarize

(defun deslop--replacement-messages (source instruction)
  "Build improvement messages for SOURCE and INSTRUCTION."
  (list (cons "system" deslop-improve-system)
        (cons "user"
              (format "Instruction: %s\n\nText to improve:\n%s"
                      instruction source))))

(defun deslop--run-improve (job)
  "Execute an improvement JOB that replaces its target atomically."
  (deslop--call-with-job-error
   job (lambda ()
      (let* ((buffer (deslop-job-buffer job))
             (start (deslop--marker-position (deslop-job-start-marker job)))
             (end (deslop--marker-position (deslop-job-end-marker job)))
             (instruction (deslop-job-get job :instruction))
              (source nil))
        (unless (and (buffer-live-p buffer) start end)
          (signal 'error (list "improvement target no longer exists")))
        (with-current-buffer buffer
          (deslop--with-job-restriction job
            (barf-if-buffer-read-only)
            (deslop--ensure-target-size start end "Improve")
            (setq source (deslop--job-selected-source job start end))))
        (let ((messages (deslop--replacement-messages source instruction)))
          (deslop--validate-messages messages)
          (deslop-history-begin job messages)
          (deslop--job-event job (list :type :status :status "Thinking"))
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
                     :on-metadata
                     (lambda (metadata)
                       (deslop--apply-job-metadata job metadata))
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
                                      (deslop-job-end-marker job)))
                                    (display
                                     (deslop--job-display-metadata job result))
                                    (live-source nil)
                                    (original-source nil)
                                    (review-overlay nil))
                              (when (string-empty-p content)
                                (signal 'error (list "empty improvement")))
                              (unless (and live-start live-end)
                                (signal 'error
                                        (list "improvement target no longer exists")))
                               (with-current-buffer buffer
                                 (deslop--with-job-restriction job
                                   (barf-if-buffer-read-only)
                                   (setq live-source
                                         (deslop--job-selected-source
                                          job live-start live-end))
                                   (setq original-source
                                         (buffer-substring live-start live-end))
                                   (deslop-job-put job :applying t)
                                   (unwind-protect
                                       (atomic-change-group
                                         (save-excursion
                                           (delete-region live-start live-end)
                                           (goto-char live-start)
                                           (let ((review-start (point)))
                                             (insert content)
                                             (setq review-overlay
                                                   (deslop-ui-create-review-overlay
                                                    buffer review-start (point)
                                                    (plist-get display :model)
                                                    (plist-get display :client)
                                                    original-source)))))
                                     (deslop-job-put job :applying nil))))
                               (unless (deslop-ui-review-overlay-p review-overlay)
                                 (error "improvement could not be marked for review"))
                               (deslop--job-event
                               job (list :type :done :result result
                                         :content content))
                              (deslop-job-finish job result))
                           ((error quit)
                            (deslop--job-error
                            job (deslop--target-error
                                 'application
                                 (error-message-string apply-error)))))))
                    :on-error (lambda (error) (deslop--job-error job error)))
                    (deslop-job-profile job)))))))
(deslop-register-job-handler 'improve #'deslop--run-improve)

(defvar deslop--summary-buffer-object nil
  "Summary buffer owned by deslop, when one exists.")

(defvar-local deslop--summary-owned-buffer nil
  "Non-nil in a summary buffer created by deslop.")

(defun deslop--summary-buffer ()
  "Return a non-file-visiting summary buffer owned by deslop."
  (if (buffer-live-p deslop--summary-buffer-object)
      deslop--summary-buffer-object
    (let* ((existing (get-buffer deslop-summary-buffer-name))
           (buffer
            (if (and existing
                     (not (buffer-local-value
                           'deslop--summary-owned-buffer existing)))
                (generate-new-buffer deslop-summary-buffer-name)
              (get-buffer-create deslop-summary-buffer-name))))
      (with-current-buffer buffer
        (text-mode)
        (setq-local deslop--summary-owned-buffer t))
      (setq deslop--summary-buffer-object buffer)
      buffer)))

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
          (deslop--with-job-restriction job
            (deslop--ensure-target-size start end "Summarize")
            (setq source (deslop--job-selected-source job start end))))
        (let ((messages (list (cons "system" deslop-summarize-system)
                               (cons "user" (format "Text to summarize:\n%s"
                                                    source)))))
          (deslop--validate-messages messages)
          (deslop-history-begin job messages)
          (deslop--job-event job (list :type :status :status "Thinking"))
          (when (deslop-job-overlay job)
            (deslop-ui-update-overlay
             (deslop-job-overlay job) :status "Thinking"))
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
                     :on-metadata
                     (lambda (metadata)
                       (deslop--apply-job-metadata job metadata))
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
                                    (display
                                     (deslop--job-display-metadata job result))
                                    (review-overlay nil))
                              (when (string-empty-p content)
                                (signal 'error (list "empty summary")))
                               (with-current-buffer
                                  (setq review-buffer
                                         (deslop--summary-buffer))
                                  (let ((inhibit-read-only t))
                                    (save-restriction
                                      (widen)
                                      (atomic-change-group
                                        (save-excursion
                                          (goto-char (point-max))
                                          (let ((review-start (point)))
                                            (unless (bobp) (insert "\n\n"))
                                            (insert (format "Summary for %s:\n"
                                                            (buffer-name buffer)))
                                          (insert content)
                                          (setq review-overlay
                                                (deslop-ui-create-review-overlay
                                                 review-buffer review-start (point)
                                                 (plist-get display :model)
                                                 (plist-get display :client)
                                                       ""))))))))
                               (unless (deslop-ui-review-overlay-p review-overlay)
                                 (error "summary could not be marked for review"))
                               (deslop--job-event
                                job (list :type :done :result result
                                          :content content))
                               (deslop-job-finish job result)
                               (condition-case nil
                                   (display-buffer review-buffer)
                                 ((error quit) nil)))
                           ((error quit)
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
         (source-digest (and region (deslop--region-digest start end)))
         (overlay (deslop-ui-create-overlay buffer start end "Queued"))
         (data (list :buffer buffer
                     :selected selected
                     :placement placement
                     :offset (and region (- point-position start))
                      :source-digest source-digest
                     :overlay overlay
                     :start-marker
                     (deslop--make-marker buffer start (and region t))
                      :end-marker
                      (deslop--make-marker buffer end nil)
                      :cursor-marker
                      (deslop--make-marker buffer point-position nil)
                      :restriction-beg
                      (deslop--make-marker buffer (point-min) nil)
                      :restriction-end
                      (deslop--make-marker buffer (point-max) t))))
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
         (_size-check (deslop--ensure-target-size start end "Improve"))
         (source-digest (deslop--region-digest start end))
         (overlay (deslop-ui-create-overlay buffer start end "Queued"))
         (data (list :buffer buffer
                     :selected t
                      :source-digest source-digest
                     :instruction instruction
                     :overlay overlay
                     :start-marker (deslop--make-marker buffer start t)
                     :end-marker (deslop--make-marker buffer end nil)
                     :cursor-marker (deslop--make-marker buffer start t)
                      :restriction-beg
                      (deslop--make-marker buffer (point-min) nil)
                      :restriction-end
                      (deslop--make-marker buffer (point-max) t))))
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
         (_size-check (deslop--ensure-target-size start end "Summarize"))
         (source-digest (deslop--region-digest start end))
         (overlay (deslop-ui-create-overlay buffer start end "Queued"))
         (data (list :buffer buffer
                     :selected t
                      :source-digest source-digest
                     :overlay overlay
                     :start-marker (deslop--make-marker buffer start t)
                     :end-marker (deslop--make-marker buffer end nil)
                     :cursor-marker (deslop--make-marker buffer start t)
                      :restriction-beg
                      (deslop--make-marker buffer (point-min) nil)
                      :restriction-end
                      (deslop--make-marker buffer (point-max) t))))
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
      (deslop-ui-delete-review-overlay overlay t))
    (message "deslop: approved %d generated region%s"
              (length overlays)
              (if (= (length overlays) 1) "" "s"))))

(defun deslop-reject-output ()
  "Reject generated output touching point and restore replaced source text.
With an active region, reject all review overlays intersecting the region."
  (interactive)
  (let ((overlays
         (if (use-region-p)
             (cl-remove-if-not
              #'deslop-ui-review-overlay-p
              (overlays-in (region-beginning) (region-end)))
           (deslop--review-overlays-at-point))))
    (unless overlays
      (user-error "No deslop output awaiting review here"))
    (setq overlays
          (sort overlays
                (lambda (left right)
                  (> (or (overlay-start left) 0)
                     (or (overlay-start right) 0)))))
    (let ((buffer (overlay-buffer (car overlays))))
      (unless (cl-every (lambda (overlay)
                          (eq (overlay-buffer overlay) buffer))
                        overlays)
        (error "Cannot reject reviews from multiple buffers at once"))
      (with-current-buffer buffer
        (barf-if-buffer-read-only)
        (save-restriction
          (widen)
          (atomic-change-group
            (save-excursion
              (dolist (overlay overlays)
                (unless (deslop-ui-review-overlay-p overlay)
                  (error "Review overlay is no longer live"))
                (let ((start (overlay-start overlay))
                      (end (overlay-end overlay))
                      (original
                       (or (overlay-get overlay 'deslop-original-text) "")))
                  (delete-region start end)
                  (goto-char start)
                  (insert original)))))))
      (dolist (overlay overlays)
        (deslop-ui-delete-review-overlay overlay)))
    (message "deslop: rejected %d generated region%s"
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
    (define-key prefix "p" #'deslop-approve-output)
    (define-key prefix "r" #'deslop-reject-output)
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
;; How do transformers work? Briefly explain in 1 sentence
;; Transformers use self-attention to let each token attend to all other tokens, building context-aware representations that are then processed by feed-forward networks.
