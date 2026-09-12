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

;;;; Anchor and Marker Helpers

(defun deslop--make-marker (buffer position insertion-type)
  "Create a marker in BUFFER at POSITION with INSERTION-TYPE."
  (let ((m (make-marker)))
    (set-marker m position buffer)
    (set-marker-insertion-type m insertion-type)
    m))

(defun deslop--marker-position (marker)
  "Return MARKER's position, or nil when it no longer has a buffer."
  (and (markerp marker) (marker-position marker)))

(defun deslop--anchor-position (anchor)
  "Return ANCHOR as a live marker position or numeric position."
  (if (markerp anchor) (marker-position anchor) anchor))

(defun deslop--call-with-job-error (job function)
  "Call FUNCTION and settle JOB if it signals an error."
  (condition-case caught
      (funcall function)
    ((error quit) (deslop-job-fail job caught))))

;;;; LLM Execution Helpers

(defun deslop--llm-callbacks (job &optional on-done)
  "Return standardized LLM callback plist for JOB."
  (list
   :on-start (lambda (proc) (deslop-job-process-set job proc))
   :on-status (lambda (st)
                (when (and (deslop-job-current-p job) (deslop-job-overlay job))
                  (deslop-ui-update-overlay (deslop-job-overlay job) :status st)))
   :on-metadata (lambda (meta) (deslop--apply-job-metadata job meta))
   :on-delta (lambda (_t _x) nil)
   :on-usage (lambda (tok spd &optional est)
               (when (and (deslop-job-current-p job) (deslop-job-overlay job))
                 (deslop-ui-update-overlay (deslop-job-overlay job)
                                           :tokens tok :speed spd :estimated est)))
   :on-event (lambda (ev) (when (deslop-job-current-p job) (deslop-history-event job ev)))
   :on-done (or on-done (lambda (res) (deslop-job-finish job res)))
   :on-error (lambda (err) (deslop-job-fail job err))))

(defun deslop--dispatch-llm (job messages &optional on-done)
  "Validate MESSAGES, record history, and invoke LLM handler for JOB."
  (deslop--validate-messages messages)
  (deslop-history-begin job messages)
  (deslop-history-event job '(:type :status :status "Thinking"))
  (when (deslop-job-overlay job)
    (deslop-ui-update-overlay (deslop-job-overlay job) :status "Thinking"))
  (funcall deslop-llm-function
           messages
           (deslop--llm-callbacks job on-done)
           (deslop-job-profile job)))

(defun deslop--apply-job-metadata (job metadata)
  "Apply endpoint METADATA to JOB's overlay and history."
  (when (deslop-job-current-p job)
    (let* ((source (plist-get metadata :source))
           (rank (pcase source (:response 2) (:discovery 1) (_ 0)))
           (current-rank (or (deslop-job-get job :metadata-rank) -1)))
      (when (>= rank current-rank)
        (deslop-job-put job :metadata-rank rank)
        (deslop-job-put job :metadata metadata)
        (let ((display (deslop-provider-display-metadata (deslop-job-profile job) metadata)))
          (when (deslop-job-overlay job)
            (deslop-ui-update-overlay (deslop-job-overlay job)
                                      :client (plist-get display :client)
                                      :model (plist-get display :model)))
          (deslop-history-event job (list :type :metadata :metadata display)))))))

(defun deslop--job-display-metadata (job result)
  "Return display metadata for JOB using RESULT and cached discovery."
  (let* ((profile (deslop-job-profile job))
         (metadata (copy-sequence (or (deslop-job-get job :metadata)
                                     (deslop-provider-cached-metadata profile))))
         (rep-m (and (listp result) (plist-get result :reported-model)))
         (rep-p (and (listp result) (plist-get result :reported-provider))))
    (when (stringp rep-m) (setq metadata (plist-put metadata :model rep-m)))
    (when (stringp rep-p)
      (setq metadata (plist-put metadata :provider rep-p))
      (setq metadata (plist-put metadata :client rep-p)))
    (deslop-provider-display-metadata profile metadata)))

(defun deslop--result-content (result)
  "Extract and validate completion content from RESULT or a legacy string."
  (let ((content (if (stringp result) result (plist-get result :content))))
    (when (and (stringp content) (> (length content) deslop-max-output-chars))
      (error "completion exceeds deslop-max-output-chars"))
    content))

(defun deslop--validate-messages (messages)
  "Return MESSAGES after enforcing character limits."
  (unless (and (integerp deslop-max-request-chars) (> deslop-max-request-chars 0))
    (error "deslop-max-request-chars must be a positive integer"))
  (let ((chars (cl-loop for m in messages sum (+ (length (format "%s" (car m)))
                                                 (length (format "%s" (cdr m)))))))
    (when (> chars deslop-max-request-chars)
      (error "request is %d characters; deslop-max-request-chars is %d" chars deslop-max-request-chars)))
  messages)

;;;; Target Validation and Restrictions

(defun deslop--job-restriction-valid-p (job start end)
  "Return non-nil when START and END of JOB are accessible."
  (let ((r-start (deslop--anchor-position (deslop-job-restriction-beg job)))
        (r-end   (deslop--anchor-position (deslop-job-restriction-end job))))
    (and (number-or-marker-p start) (number-or-marker-p end)
         r-start r-end
         (<= (point-min) r-start start)
         (<= start end r-end (point-max)))))

(defmacro deslop--with-job-restriction (job &rest body)
  "Evaluate BODY within both JOB's original and current restrictions."
  (declare (indent 1) (debug t))
  `(let ((cur-start (point-min)) (cur-end (point-max)))
     (save-restriction
       (widen)
       (let* ((orig-start (deslop--anchor-position (deslop-job-restriction-beg ,job)))
              (orig-end   (deslop--anchor-position (deslop-job-restriction-end ,job)))
              (r-start (and orig-start (max orig-start cur-start)))
              (r-end   (and orig-end (min orig-end cur-end))))
         (unless (and r-start r-end (<= (point-min) r-start r-end (point-max)))
           (error "original buffer restriction no longer exists"))
         (narrow-to-region r-start r-end)
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
     (digest (unless (string= digest (deslop--region-digest start end))
               (error "selected source changed before dispatch")))
     (snapshot (unless (string= snapshot (buffer-substring-no-properties start end))
                 (error "selected source changed before dispatch"))))))

(defun deslop--job-selected-source (job start end)
  "Return the current selected source for JOB, or signal a target error."
  (deslop--validate-selected-source job start end)
  (buffer-substring-no-properties start end))

(defun deslop--buffer-marker-context (start end offset)
  "Return bounded buffer context from START to END with marker at OFFSET."
  (setq offset (max start (min end offset)))
  (let* ((len (- end start))
         (rel (- offset start))
         (bounds (deslop-context-window len rel))
         (w-start (+ start (car bounds)))
         (w-end (+ start (cdr bounds))))
    (deslop-format-marker-context
     (buffer-substring-no-properties w-start offset)
     (buffer-substring-no-properties offset w-end)
     (> w-start start) (< w-end end))))

(defun deslop--ensure-target-size (start end operation)
  "Reject an OPERATION target larger than the configured context limit."
  (when (> (- end start) deslop-max-context-chars)
    (user-error "%s target is %d characters; limit is %d"
                operation (- end start) deslop-max-context-chars)))

(defun deslop--advance-queued-anchors (buffer position new-position)
  "Advance pending anchors at POSITION to NEW-POSITION in BUFFER."
  (dolist (job deslop-queue)
    (let ((m (deslop-job-cursor-marker job)))
      (when (and (markerp m) (eq (marker-buffer m) buffer) (= (marker-position m) position))
        (set-marker m new-position)
        (let ((ov (deslop-job-overlay job)))
          (when (and (overlayp ov) (overlay-buffer ov)
                     (numberp (overlay-start ov)) (numberp (overlay-end ov))
                     (= (overlay-start ov) position) (= (overlay-end ov) position))
            (move-overlay ov new-position new-position buffer)))))))

;;;; Job Handlers

(defun deslop--apply-insert-result (job result)
  "Apply RESULT for insert JOB atomically and settle JOB."
  (when (deslop-job-current-p job)
    (condition-case failure
        (let* ((content (deslop-trim-trailing (or (deslop--result-content result) "")))
               (buffer (deslop-job-buffer job))
               (display (deslop--job-display-metadata job result))
               ins-start ins-end review-ov)
          (when (string-empty-p content) (signal 'error '("empty completion")))
          (unless (buffer-live-p buffer) (signal 'error '("target buffer no longer exists")))
          (with-current-buffer buffer
            (deslop--with-job-restriction job
              (barf-if-buffer-read-only)
              (let* ((selected (deslop-job-get job :selected))
                     (start (deslop--marker-position (deslop-job-start-marker job)))
                     (end (deslop--marker-position (deslop-job-end-marker job)))
                     (cursor (deslop--marker-position (deslop-job-cursor-marker job))))
                (unless cursor (error "cursor anchor no longer exists"))
                (when (and selected (or (null start) (null end) (> start end)))
                  (error "selection anchors no longer exist"))
                (unless (deslop--job-restriction-valid-p job (or start cursor) (or end cursor))
                  (error "target is outside the original restriction"))
                (when selected (deslop--validate-selected-source job start end))
                (setq ins-start cursor)
                (deslop-job-put job :applying t)
                (unwind-protect
                    (atomic-change-group
                      (save-excursion
                        (goto-char cursor)
                        (pcase (deslop-job-placement job)
                          (:before (insert content "\n"))
                          (:after  (insert "\n" content))
                          (:inside (insert "\n" content "\n"))
                          (_       (insert content)))
                        (setq ins-end (point)
                              review-ov (deslop-ui-create-review-overlay
                                         buffer ins-start ins-end
                                         (plist-get display :model)
                                         (plist-get display :client) ""))))
                  (deslop-job-put job :applying nil)))))
          (ignore-errors (deslop--advance-queued-anchors buffer ins-start ins-end))
          (unless (deslop-ui-review-overlay-p review-ov)
            (error "generated output could not be marked for review"))
          (deslop-history-event job (list :type :done :result result :content (deslop--result-content result)))
          (ignore-errors (message "deslop: inserted %d chars in %s" (length content) (buffer-name buffer)))
          (deslop-job-finish job result))
      ((error quit) (deslop-job-fail job failure)))))

(defun deslop--run-insert (job)
  "Execute an insert JOB."
  (deslop--call-with-job-error
   job (lambda ()
         (let* ((buffer (deslop-job-buffer job))
                (selected (deslop-job-get job :selected))
                (start (deslop--marker-position (deslop-job-start-marker job)))
                (end (deslop--marker-position (deslop-job-end-marker job)))
                (cursor (deslop--marker-position (deslop-job-cursor-marker job)))
                context)
           (unless (and cursor (buffer-live-p buffer))
             (signal 'error '("target anchor no longer exists")))
           (with-current-buffer buffer
             (deslop--with-job-restriction job
               (barf-if-buffer-read-only)
               (if selected
                   (let ((offset (or (deslop-job-get job :offset) 0)))
                     (deslop--validate-selected-source job start end)
                     (setq context (deslop--buffer-marker-context start end (+ start offset))))
                 (unless (deslop--job-restriction-valid-p job cursor cursor)
                   (error "cursor is outside the original restriction"))
                 (setq context (deslop--buffer-marker-context (point-min) (point-max) cursor)))))
           (deslop--dispatch-llm
            job
            (list (cons "system" deslop-insert-system)
                  (cons "user" (format "Task: %s\n\nDocument:\n%s" deslop-insert-instruction context)))
            (lambda (res) (deslop--apply-insert-result job res)))))))

(defun deslop--run-improve (job)
  "Execute an improvement JOB that replaces its target atomically."
  (deslop--call-with-job-error
   job (lambda ()
         (let* ((buffer (deslop-job-buffer job))
                (start (deslop--marker-position (deslop-job-start-marker job)))
                (end (deslop--marker-position (deslop-job-end-marker job)))
                (instruction (deslop-job-get job :instruction))
                source)
           (unless (and (buffer-live-p buffer) start end)
             (signal 'error '("improvement target no longer exists")))
           (with-current-buffer buffer
             (deslop--with-job-restriction job
               (barf-if-buffer-read-only)
               (deslop--ensure-target-size start end "Improve")
               (setq source (deslop--job-selected-source job start end))))
           (deslop--dispatch-llm
            job
            (list (cons "system" deslop-improve-system)
                  (cons "user" (format "Instruction: %s\n\nText to improve:\n%s" instruction source)))
            (lambda (result)
              (when (deslop-job-current-p job)
                (condition-case apply-error
                    (let* ((content (deslop-trim-trailing (or (deslop--result-content result) "")))
                           (l-start (deslop--marker-position (deslop-job-start-marker job)))
                           (l-end   (deslop--marker-position (deslop-job-end-marker job)))
                           (display (deslop--job-display-metadata job result))
                           orig-source review-ov)
                      (when (string-empty-p content) (signal 'error '("empty improvement")))
                      (unless (and l-start l-end) (signal 'error '("improvement target no longer exists")))
                      (with-current-buffer buffer
                        (deslop--with-job-restriction job
                          (barf-if-buffer-read-only)
                          (deslop--job-selected-source job l-start l-end)
                          (setq orig-source (buffer-substring l-start l-end))
                          (deslop-job-put job :applying t)
                          (unwind-protect
                              (atomic-change-group
                                (save-excursion
                                  (delete-region l-start l-end)
                                  (goto-char l-start)
                                  (let ((r-start (point)))
                                    (insert content)
                                    (setq review-ov (deslop-ui-create-review-overlay
                                                     buffer r-start (point)
                                                     (plist-get display :model)
                                                     (plist-get display :client)
                                                     orig-source)))))
                            (deslop-job-put job :applying nil))))
                      (unless (deslop-ui-review-overlay-p review-ov)
                        (error "improvement could not be marked for review"))
                      (deslop-history-event job (list :type :done :result result :content content))
                      (deslop-job-finish job result))
                  ((error quit)
                   (deslop-job-fail job (deslop-error-create :kind 'application :message (error-message-string apply-error))))))))))))

(defvar deslop--summary-buffer-object nil
  "Summary buffer owned by deslop, when one exists.")

(defvar-local deslop--summary-owned-buffer nil
  "Non-nil in a summary buffer created by deslop.")

(defun deslop--summary-buffer ()
  "Return a non-file-visiting summary buffer owned by deslop."
  (if (buffer-live-p deslop--summary-buffer-object)
      deslop--summary-buffer-object
    (let* ((existing (get-buffer deslop-summary-buffer-name))
           (buf (if (and existing (not (buffer-local-value 'deslop--summary-owned-buffer existing)))
                    (generate-new-buffer deslop-summary-buffer-name)
                  (get-buffer-create deslop-summary-buffer-name))))
      (with-current-buffer buf
        (text-mode)
        (setq-local deslop--summary-owned-buffer t))
      (setq deslop--summary-buffer-object buf))))

(defun deslop--run-summarize (job)
  "Execute a summarize JOB and write its result to a separate buffer."
  (deslop--call-with-job-error
   job (lambda ()
         (let* ((buffer (deslop-job-buffer job))
                (start (deslop--marker-position (deslop-job-start-marker job)))
                (end (deslop--marker-position (deslop-job-end-marker job)))
                source)
           (unless (and (buffer-live-p buffer) start end)
             (signal 'error '("summary target no longer exists")))
           (with-current-buffer buffer
             (deslop--with-job-restriction job
               (deslop--ensure-target-size start end "Summarize")
               (setq source (deslop--job-selected-source job start end))))
           (deslop--dispatch-llm
            job
            (list (cons "system" deslop-summarize-system)
                  (cons "user" (format "Text to summarize:\n%s" source)))
            (lambda (result)
              (when (deslop-job-current-p job)
                (condition-case apply-error
                    (let* ((content (deslop-trim-trailing (or (deslop--result-content result) "")))
                           (display (deslop--job-display-metadata job result))
                           (rev-buf (deslop--summary-buffer))
                           review-ov)
                      (when (string-empty-p content) (signal 'error '("empty summary")))
                      (with-current-buffer rev-buf
                        (let ((inhibit-read-only t))
                          (save-restriction
                            (widen)
                            (atomic-change-group
                              (save-excursion
                                (goto-char (point-max))
                                (let ((r-start (point)))
                                  (unless (bobp) (insert "\n\n"))
                                  (insert (format "Summary for %s:\n" (buffer-name buffer)))
                                  (insert content)
                                  (setq review-ov (deslop-ui-create-review-overlay
                                                   rev-buf r-start (point)
                                                   (plist-get display :model)
                                                   (plist-get display :client) ""))))))))
                      (unless (deslop-ui-review-overlay-p review-ov)
                        (error "summary could not be marked for review"))
                      (deslop-history-event job (list :type :done :result result :content content))
                      (deslop-job-finish job result)
                      (ignore-errors (display-buffer rev-buf)))
                  ((error quit)
                   (deslop-job-fail job (deslop-error-create :kind 'application :message (error-message-string apply-error))))))))))))

(deslop-register-job-handler 'insert #'deslop--run-insert)
(deslop-register-job-handler 'improve #'deslop--run-improve)
(deslop-register-job-handler 'summarize #'deslop--run-summarize)

;;;; Interactive Commands

(defun deslop--enqueue-job (type &optional instruction)
  "Enqueue a job of TYPE (insert, improve, summarize) with optional INSTRUCTION."
  (let* ((buffer (current-buffer))
         (region (deslop-current-region))
         (pt (point))
         (selected (if (eq type 'insert) (and region t) t))
         (start (if region (car region) (if (eq type 'insert) pt (point-min))))
         (end (if region (cdr region) (if (eq type 'insert) pt (point-max)))))
    (unless (eq type 'insert)
      (deslop--ensure-target-size start end (capitalize (symbol-name type))))
    (let ((data (list :buffer buffer
                      :selected selected
                      :placement (and region (cond ((= pt start) :before)
                                                   ((= pt end) :after)
                                                   (t :inside)))
                      :offset (and region (- pt start))
                      :source-digest (and selected (deslop--region-digest start end))
                      :overlay (deslop-ui-create-overlay buffer start end "Queued")
                      :start-marker (deslop--make-marker buffer start (and selected t))
                      :end-marker (deslop--make-marker buffer end nil)
                      :cursor-marker (deslop--make-marker buffer pt nil)
                      :restriction-beg (deslop--make-marker buffer (point-min) nil)
                      :restriction-end (deslop--make-marker buffer (point-max) t))))
      (when instruction (setq data (plist-put data :instruction instruction)))
      (let ((job (deslop-enqueue (cons type data))))
        (message "deslop: queued %s job %d" type (deslop-job-id job))
        job))))

(defun deslop-insert-at-cursor ()
  "Queue a request to insert LLM text at cursor position."
  (interactive)
  (deslop--enqueue-job 'insert))

(defun deslop-improve-at-cursor (instruction)
  "Queue an improvement of the active region or accessible buffer."
  (interactive (list (read-string "How should the text be improved? ")))
  (deslop--enqueue-job 'improve instruction))

(defun deslop-summarize ()
  "Queue a summary of the active region or accessible buffer."
  (interactive)
  (deslop--enqueue-job 'summarize))

(defun deslop--review-overlays-in-target ()
  "Return active review overlays intersecting region or point."
  (let ((ovs (if (use-region-p)
                 (overlays-in (region-beginning) (region-end))
               (apply #'append (mapcar #'overlays-at
                                       (delq nil (list (point) (and (> (point) (point-min)) (1- (point))))))))))
    (delete-dups (cl-remove-if-not #'deslop-ui-review-overlay-p ovs))))

(defun deslop-approve-output ()
  "Approve generated output touching point or active region."
  (interactive)
  (let ((overlays (deslop--review-overlays-in-target)))
    (unless overlays (user-error "No deslop output awaiting approval here"))
    (dolist (ov overlays) (deslop-ui-delete-review-overlay ov t))
    (message "deslop: approved %d generated region%s"
             (length overlays) (if (= (length overlays) 1) "" "s"))))

(defun deslop-reject-output ()
  "Reject generated output touching point or active region."
  (interactive)
  (let ((overlays (deslop--review-overlays-in-target)))
    (unless overlays (user-error "No deslop output awaiting review here"))
    (setq overlays (sort overlays (lambda (l r) (> (or (overlay-start l) 0) (or (overlay-start r) 0)))))
    (let ((buf (overlay-buffer (car overlays))))
      (unless (cl-every (lambda (ov) (eq (overlay-buffer ov) buf)) overlays)
        (error "Cannot reject reviews from multiple buffers at once"))
      (with-current-buffer buf
        (barf-if-buffer-read-only)
        (save-restriction
          (widen)
          (atomic-change-group
            (save-excursion
              (dolist (ov overlays)
                (unless (deslop-ui-review-overlay-p ov) (error "Review overlay is no longer live"))
                (let ((st (overlay-start ov)) (nd (overlay-end ov))
                      (orig (or (overlay-get ov 'deslop-original-text) "")))
                  (delete-region st nd)
                  (goto-char st)
                  (insert orig))))))))
    (dolist (ov overlays) (deslop-ui-delete-review-overlay ov))
    (message "deslop: rejected %d generated region%s"
             (length overlays) (if (= (length overlays) 1) "" "s"))))

;;;; Keymap and Minor Mode

(defun deslop--make-mode-map ()
  "Build the keymap for `deslop-mode'."
  (let ((map (make-sparse-keymap))
        (prefix (make-sparse-keymap)))
    (pcase-dolist (`(,key . ,cmd) '(("i" . deslop-insert-at-cursor)
                                  ("m" . deslop-improve-at-cursor)
                                  ("s" . deslop-summarize)
                                  ("q" . deslop-queue-status)
                                  ("a" . deslop-abort-queue)
                                  ("p" . deslop-approve-output)
                                  ("r" . deslop-reject-output)))
      (define-key prefix key cmd))
    (define-key map (kbd "C-c e") prefix)
    map))

(defvar deslop-mode-map (deslop--make-mode-map) "Keymap for `deslop-mode'.")

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
    (dolist (f files) (load (expand-file-name f dir)))
    (setq deslop-mode-map (deslop--make-mode-map))
    (message "deslop: reloaded from %s" dir)))

(provide 'deslop)

;;; deslop.el ends here
