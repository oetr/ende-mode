;;; deslop-operations.el --- LLM operation orchestration for deslop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools, llm

;;; Commentary:

;; Request construction, execution, result application, and interactive
;; commands for deslop's insert, improve, and summarize operations.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'deslop-core)
(require 'deslop-provider)
(require 'deslop-review)
(require 'deslop-target)
(require 'deslop-ui)
(require 'deslop-llm)
(require 'deslop-queue)

(defcustom deslop-insert-instruction
  "Write the most natural continuation of the text at the cursor position."
  "Instruction given to the LLM for deslop-insert-at-cursor."
  :type 'string
  :group 'deslop)

(defcustom deslop-summary-buffer-name "*deslop-summary*"
  "Name of the buffer used for summaries."
  :type 'string
  :group 'deslop)

(defun deslop-trim-trailing (s)
  "Remove trailing newlines from S, preserving inline trailing spaces."
  (let ((end (length s)))
    (while (and (> end 0) (memq (aref s (1- end)) '(?\n ?\r)))
      (setq end (1- end)))
    (substring s 0 end)))

(defun deslop-current-region ()
  "Return (START . END) of the active region, or nil."
  (when (use-region-p)
    (cons (region-beginning) (region-end))))

(defconst deslop-insert-system
  "You are an inline code and text completion engine. You will receive a document containing a <CURSOR> marker indicating where your output will be inserted.

### Output Constraints
- Output ONLY the exact text to be inserted at <CURSOR>.\040
- Never wrap your response in markdown code blocks (```) or add explanatory commentary.
- Output text VERBATIM. Use real line breaks for multi-line completions, never literal \"\\n\" strings.

### Completion Rules
1. No Duplication: Do not repeat words, code, or symbols that already exist immediately before or after <CURSOR>.
2. Comment Awareness: If <CURSOR> sits directly after a comment marker (e.g., `;;`, `//`, `#`), do NOT start your output with another comment marker. If your completion spans multiple lines inside a comment block, prefix only the new subsequent lines with the appropriate comment symbol.
3. No Empty Responses: Do not return an empty string. If the cursor sits at a natural boundary, insert a single space or a logical continuation phrase.

### Examples

Input:
;; <CURSOR>refactor this later
Output:
TODO:\040

Input:
// <CURSOR>
// next line
Output:
first line

Input:
def calculate_total(price, tax):
    <CURSOR>
Output:
return price * (1 + tax)"
  "System prompt instructions for cursor completion.")

(defconst deslop-improve-system
  "You improve text while preserving its intent and useful information. Return ONLY the improved text, without markdown fences or commentary."
  "System prompt instructions for improving a target region.")

(defconst deslop-summarize-system
  "You summarize the supplied text accurately and concisely. Return ONLY the summary, without markdown fences or commentary."
  "System prompt instructions for summarizing a target region.")

;;;; LLM execution

(defun deslop--call-with-job-error (job function)
  "Call FUNCTION and settle JOB if it signals an error."
  (condition-case caught
      (funcall function)
    ((error quit) (deslop-job-fail job caught))))

(defun deslop--llm-callbacks (job &optional on-done)
  "Return standardized LLM callback plist for JOB."
  (list
   :on-start (lambda (cancel-function)
               (deslop-job-cancellation-set job cancel-function))
   :on-event (lambda (event) (deslop--handle-llm-event job event))
   :on-done (or on-done (lambda (res) (deslop-job-finish job res)))
   :on-error (lambda (err) (deslop-job-fail job err))))

(defun deslop--handle-llm-event (job event)
  "Route normalized transport EVENT for JOB to interested subsystems."
  (when (deslop-job-current-p job)
    (pcase (plist-get event :type)
      (:status
       (when (deslop-job-overlay job)
         (deslop-ui-update-overlay
          (deslop-job-overlay job) :status (plist-get event :status))))
      (:metadata
       (deslop--apply-job-metadata job (plist-get event :metadata)))
      (:usage
       (when (deslop-job-overlay job)
         (deslop-ui-update-overlay
          (deslop-job-overlay job)
          :tokens (plist-get event :tokens)
          :speed (plist-get event :speed)
          :estimated (plist-get event :estimated)))))
    (deslop-job-emit-event job event)))

(defun deslop--dispatch-llm (job messages &optional on-done)
  "Validate MESSAGES, record history, and invoke LLM handler for JOB."
  (deslop--validate-messages messages)
  (deslop-job-emit-event job (list :type :begin :messages messages))
  (deslop--handle-llm-event job '(:type :status :status "Thinking"))
  (funcall deslop-llm-function
           messages
           (deslop--llm-callbacks job on-done)
           (deslop-job-profile job)))

(defun deslop--validate-messages (messages)
  "Return MESSAGES after enforcing character limits."
  (unless (and (integerp deslop-max-request-chars)
               (> deslop-max-request-chars 0))
    (error "deslop-max-request-chars must be a positive integer"))
  (let ((chars (cl-loop for message in messages
                        sum (+ (length (format "%s" (car message)))
                               (length (format "%s" (cdr message)))))))
    (when (> chars deslop-max-request-chars)
      (error "request is %d characters; deslop-max-request-chars is %d"
             chars deslop-max-request-chars)))
  messages)

;;;; Result application

(defun deslop--apply-job-metadata (job metadata)
  "Apply endpoint METADATA to JOB's overlay and history."
  (when (deslop-job-current-p job)
    (let* ((source (plist-get metadata :source))
           (rank (pcase source (:response 2) (:discovery 1) (_ 0)))
           (current-rank (or (deslop-job-metadata-rank job) -1)))
      (when (>= rank current-rank)
        (setf (deslop-job-metadata-rank job) rank
              (deslop-job-metadata job) metadata)
        (let ((display
               (deslop-provider-display-metadata
                (deslop-job-profile job) metadata)))
          (when (deslop-job-overlay job)
            (deslop-ui-update-overlay
             (deslop-job-overlay job)
             :client (plist-get display :client)
             :model (plist-get display :model)))
          display)))))

(defun deslop--job-display-metadata (job result)
  "Return display metadata for JOB using RESULT and cached discovery."
  (let* ((profile (deslop-job-profile job))
         (metadata (copy-sequence
                    (or (deslop-job-metadata job)
                        (deslop-provider-cached-metadata profile))))
         (reported-model (and (listp result) (plist-get result :model)))
         (reported-provider (and (listp result)
                                 (plist-get result :provider))))
    (when (stringp reported-model)
      (setq metadata (plist-put metadata :model reported-model)))
    (when (stringp reported-provider)
      (setq metadata (plist-put metadata :provider reported-provider))
      (setq metadata (plist-put metadata :client reported-provider)))
    (deslop-provider-display-metadata profile metadata)))

(defun deslop--result-content (result)
  "Extract and validate completion content from RESULT or a legacy string."
  (let ((content (if (stringp result) result (plist-get result :content))))
    (when (and (stringp content)
               (> (length content) deslop-max-output-chars))
      (error "completion exceeds deslop-max-output-chars"))
    content))

(defun deslop--application-error (error)
  "Return ERROR as an application error suitable for job settlement."
  (if (deslop-error-p error)
      error
    (deslop-error-create :kind 'application
                         :message (error-message-string error))))

(defun deslop--apply-result (job result apply-function)
  "Apply RESULT for JOB with APPLY-FUNCTION and settle JOB.
APPLY-FUNCTION receives trimmed content and display metadata and must return
the review overlay it creates."
  (when (deslop-job-current-p job)
    (condition-case failure
        (let* ((content (deslop-trim-trailing
                         (or (deslop--result-content result) "")))
               (display (deslop--job-display-metadata job result))
               (review-overlay nil))
          (when (string-empty-p content)
            (error "empty completion"))
          (setq review-overlay (funcall apply-function content display))
          (unless (deslop-review-overlay-p review-overlay)
            (error "generated output could not be marked for review"))
          (deslop-job-finish job result))
      ((error quit)
       (deslop-job-fail job (deslop--application-error failure))))))

(defun deslop--apply-insert-result (job result)
  "Apply RESULT for insert JOB atomically and settle JOB."
  (deslop--apply-result
   job result
   (lambda (content display)
     (let* ((target (deslop-job-target job))
            (buffer (deslop-target-buffer target))
            (model (plist-get display :model))
            (client (plist-get display :client))
            ins-start ins-end review-overlay)
       (unless (buffer-live-p buffer)
         (error "target buffer no longer exists"))
       (with-current-buffer buffer
         (deslop-target-with-restriction target
           (barf-if-buffer-read-only)
           (let* ((selected (deslop-target-selected target))
                  (start (deslop-target-start target))
                  (end (deslop-target-end target))
                  (cursor (deslop-target-cursor target)))
             (unless cursor
               (error "cursor anchor no longer exists"))
             (when (and selected (or (null start) (null end) (> start end)))
               (error "selection anchors no longer exist"))
             (unless (deslop-target-restriction-valid-p
                      target (or start cursor) (or end cursor))
               (error "target is outside the original restriction"))
             (when selected
               (deslop-target-validate-source target start end))
             (setq ins-start cursor)
             (deslop-target-with-application target
               (atomic-change-group
                 (save-excursion
                   (goto-char cursor)
                   (pcase (deslop-target-placement target)
                     (:before (insert content "\n"))
                     (:after  (insert "\n" content))
                     (:inside (insert "\n" content "\n"))
                     (_       (insert content)))
                   (setq ins-end (point)
                         review-overlay
                         (deslop-review-create-overlay
                          buffer ins-start ins-end model client ""))))))))
       (dolist (queued-job deslop-queue)
         (deslop--ignore-errors
          #'deslop-target-advance-queued-anchors
          (list (deslop-job-target queued-job)) buffer ins-start ins-end
          (lambda (_target)
            (let ((overlay (deslop-job-overlay queued-job)))
              (when (and (overlayp overlay) (overlay-buffer overlay)
                         (numberp (overlay-start overlay))
                         (numberp (overlay-end overlay))
                         (= (overlay-start overlay) ins-start)
                         (= (overlay-end overlay) ins-start))
                (move-overlay overlay ins-end ins-end buffer))))))
       (deslop--ignore-errors
        #'message "deslop: inserted %d chars in %s"
        (length content) (buffer-name buffer))
       review-overlay))))

;;;; Operation handlers

(defun deslop--run-insert (job)
  "Execute an insert JOB."
  (deslop--call-with-job-error
   job
   (lambda ()
     (let* ((target (deslop-job-target job))
            (buffer (deslop-target-buffer target))
            (selected (deslop-target-selected target))
            (start (deslop-target-start target))
            (end (deslop-target-end target))
            (cursor (deslop-target-cursor target))
            context)
       (unless (and cursor (buffer-live-p buffer))
         (signal 'error '("target anchor no longer exists")))
       (with-current-buffer buffer
         (deslop-target-with-restriction target
           (barf-if-buffer-read-only)
           (if selected
               (let ((offset (or (deslop-target-offset target) 0)))
                 (deslop-target-validate-source target start end)
                 (setq context
                        (deslop-target-buffer-marker-context
                         start end (+ start offset))))
             (unless (deslop-target-restriction-valid-p target cursor cursor)
               (error "cursor is outside the original restriction"))
             (setq context
                    (deslop-target-buffer-marker-context
                     (point-min) (point-max) cursor)))))
       (deslop--dispatch-llm
        job
        (list (cons "system" deslop-insert-system)
              (cons "user"
                    (format "Task: %s\n\nDocument:\n%s"
                            deslop-insert-instruction context)))
        (lambda (result) (deslop--apply-insert-result job result)))))))

(defun deslop--run-improve (job)
  "Execute an improvement JOB that replaces its target atomically."
  (deslop--call-with-job-error
   job
   (lambda ()
     (let* ((target (deslop-job-target job))
            (buffer (deslop-target-buffer target))
            (start (deslop-target-start target))
            (end (deslop-target-end target))
            (instruction (deslop-job-instruction job))
            source)
       (unless (and (buffer-live-p buffer) start end)
         (signal 'error '("improvement target no longer exists")))
       (with-current-buffer buffer
         (deslop-target-with-restriction target
           (barf-if-buffer-read-only)
           (deslop-target-ensure-size start end "Improve")
           (setq source (deslop-target-selected-source target start end))))
       (deslop--dispatch-llm
        job
        (list (cons "system" deslop-improve-system)
              (cons "user"
                    (format "Instruction: %s\n\nText to improve:\n%s"
                            instruction source)))
        (lambda (result)
          (deslop--apply-result
           job result
           (lambda (content display)
             (let* ((latest-start (deslop-target-start target))
                    (latest-end (deslop-target-end target))
                    (model (plist-get display :model))
                    (client (plist-get display :client))
                    original-source review-overlay)
               (unless (and latest-start latest-end)
                 (error "improvement target no longer exists"))
               (with-current-buffer buffer
                 (deslop-target-with-restriction target
                   (barf-if-buffer-read-only)
                   (deslop-target-selected-source
                    target latest-start latest-end)
                   (setq original-source
                         (buffer-substring latest-start latest-end))
                   (deslop-target-with-application target
                     (atomic-change-group
                       (save-excursion
                         (delete-region latest-start latest-end)
                         (goto-char latest-start)
                         (let ((review-start (point)))
                           (insert content)
                           (setq review-overlay
                                 (deslop-review-create-overlay
                                  buffer review-start (point)
                                  model client original-source))))))))
               review-overlay)))))))))

(defvar deslop--summary-buffer-object nil
  "Summary buffer owned by deslop, when one exists.")

(defvar-local deslop--summary-owned-buffer nil
  "Non-nil in a summary buffer created by deslop.")

(defun deslop--summary-buffer ()
  "Return a non-file-visiting summary buffer owned by deslop."
  (setq deslop--summary-buffer-object
        (deslop--owned-buffer deslop--summary-buffer-object
                              deslop-summary-buffer-name
                              'deslop--summary-owned-buffer
                              #'text-mode)))

(defun deslop--run-summarize (job)
  "Execute a summarize JOB and write its result to a separate buffer."
  (deslop--call-with-job-error
   job
   (lambda ()
     (let* ((target (deslop-job-target job))
            (buffer (deslop-target-buffer target))
            (start (deslop-target-start target))
            (end (deslop-target-end target))
            source)
       (unless (and (buffer-live-p buffer) start end)
         (signal 'error '("summary target no longer exists")))
       (with-current-buffer buffer
         (deslop-target-with-restriction target
           (deslop-target-ensure-size start end "Summarize")
           (setq source (deslop-target-selected-source target start end))))
       (deslop--dispatch-llm
        job
        (list (cons "system" deslop-summarize-system)
              (cons "user" (format "Text to summarize:\n%s" source)))
        (lambda (result)
          (deslop--apply-result
           job result
           (lambda (content display)
             (let* ((review-buffer (deslop--summary-buffer))
                    (model (plist-get display :model))
                    (client (plist-get display :client))
                    review-overlay)
               (with-current-buffer review-buffer
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
                                 (deslop-review-create-overlay
                                  review-buffer review-start (point)
                                  model client ""))))))))
               (deslop--ignore-errors #'display-buffer review-buffer)
               review-overlay)))))))))

(deslop-register-job-handler 'insert #'deslop--run-insert)
(deslop-register-job-handler 'improve #'deslop--run-improve)
(deslop-register-job-handler 'summarize #'deslop--run-summarize)

;;;; Interactive commands

(defun deslop--enqueue-job (type &optional instruction)
  "Enqueue a job of TYPE (insert, improve, summarize) with optional INSTRUCTION."
  (let* ((buffer (current-buffer))
         (region (deslop-current-region))
         (point (point))
         (selected (if (eq type 'insert) (and region t) t))
         (start (if region (car region) (if (eq type 'insert) point (point-min))))
         (end (if region (cdr region) (if (eq type 'insert) point (point-max)))))
    (unless (eq type 'insert)
      (deslop-target-ensure-size start end (capitalize (symbol-name type))))
    (let* ((target
            (deslop-target-create
             :buffer buffer :start start :end end :cursor point
             :selected selected
             :placement (and region (cond ((= point start) :before)
                                          ((= point end) :after)
                                          (t :inside)))
             :offset (and region (- point start))))
           (data (list :target target
                       :overlay
                       (deslop-ui-create-overlay buffer start end "Queued"))))
      (when instruction
        (setq data (plist-put data :instruction instruction)))
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

(provide 'deslop-operations)

;;; deslop-operations.el ends here
