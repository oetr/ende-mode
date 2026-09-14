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
(require 'deslop-operation)
(require 'deslop-edit)
(require 'deslop-summary)
(require 'deslop-provider)
(require 'deslop-review)
(require 'deslop-target)
(require 'deslop-ui)
(require 'deslop-llm)
(require 'deslop-queue)

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

(defconst deslop-write-anywhere-system
  "You edit a document by returning a strict JSON edit list. Inspect the entire
document and make every edit needed to fulfill the user's task. When the task
applies at multiple positions, return a separate edit for each position rather
than placing all generated text together in one location.

Choose semantically appropriate code boundaries, normally line starts, line
ends, or positions adjacent to complete syntactic forms. Do not place an edit
inside an identifier, keyword, string, or expression unless the user's task
explicitly requires an inline change.

### Output Format
Return ONLY one JSON object with this exact top-level shape:
{\"version\":1,\"edits\":[{\"anchor\":\"exact existing text\",\"position\":\"replace\",\"text\":\"replacement text\"}]}

Every edit object must contain `anchor`, `position`, and `text`. `anchor` must be
an exact non-empty string copied from the document. `position` must be `before`,
`after`, or `replace`; it identifies where `text` goes relative to the anchor.
Use `replace` to replace the anchor itself. The anchor must match exactly once
unless the edit includes an integer `occurrence` (one-based), or `all` set to
true. `text` must not be empty. Never invent an anchor, duplicate a JSON key,
or use absolute character offsets. Return at least one edit.
Do not use markdown fences, comments, or any keys other than those shown."
  "System prompt instructions for arbitrary document edits.")

(defconst deslop-insert-anywhere-system
  "You perform an INSERT-ANYWHERE operation by returning a JSON insertion list.
Inspect the entire document and find every position where the user's task calls
for an insertion. Multiple insertions at different positions are expected when
appropriate; do not put all inserted text at one arbitrary location.

Choose semantically appropriate code boundaries, normally line starts, line
ends, or positions adjacent to complete syntactic forms. Do not place an
insertion inside an identifier, keyword, string, or expression unless the
user's task explicitly requires an inline insertion.

### Output Format
Return ONLY one JSON object with this exact top-level shape:
{\"version\":1,\"edits\":[{\"anchor\":\"exact existing text\",\"position\":\"before\",\"text\":\"inserted text\"}]}

Each edit object must contain `anchor`, `position`, and `text`. `anchor` must be
an exact non-empty string copied from the document. `position` must be either
`before` or `after`; it identifies where the insertion goes relative to the
anchor. The anchor must match exactly once unless the edit includes an integer
`occurrence` (one-based), or `all` set to true. `text` must not be empty. Never
invent an anchor, duplicate a JSON key, or use absolute character offsets.
Do not use markdown fences, comments, or any keys other than those shown."
  "System prompt instructions for arbitrary document insertions.")

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
    (let ((type (plist-get event :type)))
      (unless (eq type :error)
        (pcase type
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
        (deslop-job-emit-event job event)))))

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
  (unless (or (null deslop-max-request-chars)
              (and (integerp deslop-max-request-chars)
                   (> deslop-max-request-chars 0)))
    (error "deslop-max-request-chars must be nil or a positive integer"))
  (let ((chars (cl-loop for message in messages
                        sum (+ (length (format "%s" (car message)))
                               (length (format "%s" (cdr message)))))))
    (when (and deslop-max-request-chars
               (> chars deslop-max-request-chars))
      (error "request is %d characters; deslop-max-request-chars is %d"
             chars deslop-max-request-chars)))
  messages)

(defun deslop--selected-job-source (job operation &optional writable)
  "Return the validated selected source for JOB.
OPERATION names the operation for size diagnostics.  When WRITABLE is
non-nil, also reject read-only targets before reading the source."
  (let* ((target (deslop-job-target job))
         (buffer (deslop-target-buffer target))
         (start (deslop-target-start target))
         (end (deslop-target-end target)))
    (unless (and (buffer-live-p buffer) start end)
      (error "%s target no longer exists" operation))
    (with-current-buffer buffer
      (deslop-target-with-restriction target
        (when writable
          (barf-if-buffer-read-only))
        (deslop-target-ensure-size start end operation)
        (deslop-target-selected-source target start end)))))

(defun deslop--run-selected-operation
    (job system-prompt prompt-function on-done &optional writable)
  "Run a selected-text operation for JOB.
SYSTEM-PROMPT is sent as the system message.  PROMPT-FUNCTION receives the
validated source and returns the user message.  ON-DONE receives the raw LLM
result.  When WRITABLE is non-nil, the target must be writable while its
source is captured."
  (deslop--call-with-job-error
   job
   (lambda ()
     (let ((source (deslop--selected-job-source
                    job (if writable "Improve" "Summarize") writable)))
       (deslop--dispatch-llm
        job
        (list (cons "system" system-prompt)
              (cons "user" (funcall prompt-function source)))
        on-done)))))

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
  (deslop-provider-result-display-metadata
   (deslop-job-profile job)
   (deslop-job-metadata job)
   result))

(defun deslop--create-review-overlay (buffer start end display original)
  "Mark generated text from START to END in BUFFER for review.
DISPLAY supplies the provider label and ORIGINAL is restored on rejection."
  (deslop-review-create-overlay
   buffer start end
   (plist-get display :model)
   (plist-get display :client)
   original))

(defun deslop--mutate-target (target mutation)
  "Run MUTATION as one writable, atomic change to TARGET.
MUTATION runs in TARGET's buffer and returns the generated review overlay."
  (let ((buffer (deslop-target-buffer target)))
    (unless (buffer-live-p buffer)
      (error "target buffer no longer exists"))
    (with-current-buffer buffer
      (deslop-target-with-restriction target
        (barf-if-buffer-read-only)
        (deslop-target-with-application target
          (atomic-change-group
            (funcall mutation)))))))

(defun deslop--result-content (result)
  "Extract and validate completion content from RESULT or a legacy string."
  (let ((content (if (stringp result) result (plist-get result :content))))
     (when (and deslop-max-output-chars
                (stringp content)
                (> (length content) deslop-max-output-chars))
      (error "completion exceeds deslop-max-output-chars"))
    content))

(defun deslop--application-error (error)
  "Return ERROR as an application error suitable for job settlement."
  (if (deslop-error-p error)
      error
    (deslop-error-create :kind 'application
                         :message (if (stringp error)
                                      error
                                    (error-message-string error)))))

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
             (let ((finish-reason (and (listp result)
                                       (plist-get result :finish-reason))))
               (if finish-reason
                   (error "empty completion (provider stopped: %s)"
                          finish-reason)
                 (error "empty completion"))))
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
             ins-start ins-end review-overlay)
        (setq review-overlay
              (deslop--mutate-target
               target
               (lambda ()
                 (let* ((selected (deslop-target-selected target))
                        (start (deslop-target-start target))
                        (end (deslop-target-end target))
                        (cursor (deslop-target-cursor target)))
                   (unless cursor
                     (error "cursor anchor no longer exists"))
                   (when (and selected
                              (or (null start) (null end) (> start end)))
                     (error "selection anchors no longer exist"))
                   (unless (deslop-target-restriction-valid-p
                            target (or start cursor) (or end cursor))
                     (error "target is outside the original restriction"))
                   (when selected
                     (deslop-target-validate-source target start end))
                   (setq ins-start cursor)
                   (save-excursion
                     (goto-char cursor)
                     (pcase (deslop-target-placement target)
                       (:before (insert content "\n"))
                       (:after  (insert "\n" content))
                       (:inside (insert "\n" content "\n"))
                       (_       (insert content)))
                     (setq ins-end (point))
                     (deslop--create-review-overlay
                      buffer ins-start ins-end display ""))))))
         (dolist (other-job (append deslop--active-jobs deslop-queue))
           (let ((other-target (deslop-job-target other-job)))
             (when (and (not (eq other-job job))
                        (> (deslop-job-id other-job) (deslop-job-id job))
                         (eq (deslop-job-type other-job) 'insert)
                        (eq (deslop-target-buffer other-target) buffer))
               (deslop--ignore-errors
                #'deslop-target-advance-queued-anchors
                (list other-target) buffer ins-start ins-end
                (lambda (_target)
                  (let ((overlay (deslop-job-overlay other-job)))
                    (when (and (overlayp overlay) (overlay-buffer overlay)
                               (numberp (overlay-start overlay))
                               (numberp (overlay-end overlay))
                               (= (overlay-start overlay) ins-start)
                               (= (overlay-end overlay) ins-start))
                      (move-overlay overlay ins-end ins-end buffer))))))))
       (deslop--ignore-errors
        #'message "deslop: inserted %d chars in %s"
        (length content) (buffer-name buffer))
       review-overlay))))

(defun deslop--apply-write-anywhere-result (job result)
  "Apply valid anchor-based edits from RESULT while skipping invalid edits."
  (when (deslop-job-current-p job)
    (condition-case failure
        (let* ((target (deslop-job-target job))
               (buffer (deslop-target-buffer target))
               start source parsed edits failures application-failures
               applied display)
          (unless (buffer-live-p buffer)
            (error "write-anywhere target no longer exists"))
          (with-current-buffer buffer
            (deslop-target-with-restriction target
              (setq start (point-min)
                    source (buffer-substring-no-properties
                            (point-min) (point-max)))))
          (setq parsed
                (deslop-edit-parse-replacements
                 (deslop-trim-trailing
                  (or (deslop--result-content result) ""))
                 source)
                edits (plist-get parsed :edits)
                failures (plist-get parsed :errors)
                display (deslop--job-display-metadata job result))
          (with-current-buffer buffer
            (let ((buffer-read-only nil))
              (dolist (edit
                       (sort (copy-sequence edits)
                             (lambda (left right)
                               (or (> (plist-get left :start)
                                      (plist-get right :start))
                                   (and (= (plist-get left :start)
                                           (plist-get right :start))
                                        (> (plist-get left :index)
                                           (plist-get right :index)))))))
                (condition-case caught
                    (progn
                      (deslop--apply-anchor-edit
                       target buffer start edit display)
                      (setq applied (1+ (or applied 0))))
                  ((error quit)
                   (push (list :index (plist-get edit :index)
                               :message (error-message-string caught)
                               :raw (plist-get edit :raw))
                         application-failures))))))
          (setq failures (append failures (nreverse application-failures)))
          (dolist (failure failures)
            (message "deslop: skipped write-anywhere edit %d: %s"
                     (1+ (plist-get failure :index))
                     (plist-get failure :message)))
          (let ((annotated
                 (deslop-edit-add-replacement-failures result failures)))
            (if (and applied (> applied 0))
                (deslop-job-finish job annotated)
              (deslop-job-fail
               job
               (deslop--application-error
                (format "write-anywhere produced no applicable edits: %s"
                        (deslop-edit-format-failures failures "; ")))
               annotated))))
      ((error quit)
       (deslop-job-fail job (deslop--application-error failure))))))

(defun deslop--apply-anchor-edit (target buffer source-start edit display)
  "Apply anchor-based EDIT relative to SOURCE-START in TARGET."
  (let* ((start (+ source-start (plist-get edit :start)))
         (end (+ source-start (plist-get edit :end)))
         (replace (equal (plist-get edit :position) "replace"))
         (text (plist-get edit :text)))
    (deslop--mutate-target
     target
     (lambda ()
       (let ((original (if replace
                          (buffer-substring-no-properties start end)
                        "")))
         (goto-char start)
         (when replace
           (delete-region start end))
         (let ((review-start (point)))
           (insert text)
           (deslop--create-review-overlay
            buffer review-start (point) display original)))))))

(defun deslop--apply-insertion-edit (target buffer start edit display)
  "Apply one insertion EDIT to TARGET and return its review overlay."
  (deslop--apply-anchor-edit target buffer start edit display))

(defun deslop--apply-insert-anywhere-result (job result)
  "Apply valid INSERT-ANYWHERE edits while skipping invalid edits."
  (when (deslop-job-current-p job)
    (condition-case failure
        (let* ((target (deslop-job-target job))
               (buffer (deslop-target-buffer target))
               (start (deslop-target-start target))
               (end (deslop-target-end target))
               source parsed edits failures applied display)
          (unless (buffer-live-p buffer)
            (error "insert-anywhere target no longer exists"))
          (with-current-buffer buffer
            (deslop-target-with-restriction target
              (setq start (point-min)
                    end (point-max)
                    source (buffer-substring-no-properties start end))))
          (setq parsed
                (deslop-edit-parse-insertions
                 (deslop-trim-trailing
                  (or (deslop--result-content result) ""))
                 source)
                edits (plist-get parsed :edits)
                failures (plist-get parsed :errors)
                display (deslop--job-display-metadata job result))
          (with-current-buffer buffer
            (let ((buffer-read-only nil))
              (dolist (edit
                       (sort (copy-sequence edits)
                             (lambda (left right)
                               (or (> (plist-get left :offset)
                                      (plist-get right :offset))
                                   (and (= (plist-get left :offset)
                                           (plist-get right :offset))
                                        (> (plist-get left :index)
                                           (plist-get right :index)))))))
                (condition-case caught
                    (progn
                      (deslop--apply-insertion-edit
                       target buffer start edit display)
                      (setq applied (1+ (or applied 0))))
                  ((error quit)
                   (push (list :index (plist-get edit :index)
                               :message (error-message-string caught))
                         failures))))))
          (dolist (failure failures)
            (message "deslop: skipped insert-anywhere edit %d: %s"
                     (1+ (plist-get failure :index))
                     (plist-get failure :message)))
          (if (and applied (> applied 0))
              (deslop-job-finish
               job (deslop-edit-add-insertion-failures result failures))
            (error "insert-anywhere produced no applicable insertions: %s"
                   (deslop-edit-format-insertion-failures failures "; "))))
      ((error quit)
       (deslop-job-fail job (deslop--application-error failure))))))

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

(defun deslop-operations--run-structured-edit (job)
  "Execute a structured-edit JOB using its operation specification."
  (deslop--call-with-job-error
   job
   (lambda ()
      (let* ((type (deslop-job-type job))
             (target (deslop-job-target job))
             (buffer (deslop-target-buffer target))
             (start (deslop-target-start target))
             (end (deslop-target-end target))
             (insertion-only
              (eq (deslop-operation-structured-result type) 'insertion))
             (instruction (or (deslop-job-instruction job)
                              (pcase type
                                ('write-anywhere deslop-write-anywhere-instruction)
                                ('insert-anywhere deslop-insert-anywhere-instruction))))
             (system-prompt (pcase type
                               ('write-anywhere deslop-write-anywhere-system)
                               ('insert-anywhere deslop-insert-anywhere-system)))
             source)
        (unless (and (buffer-live-p buffer) start end)
          (error "write-anywhere target no longer exists"))
        (when (deslop-operation-freezes-buffer-p type)
          (deslop-target-freeze-buffer target))
        (with-current-buffer buffer
          (deslop-target-with-restriction target
            (deslop-target-validate-source target start end)
            (setq source (buffer-substring-no-properties start end))))
        (deslop--dispatch-llm
         job
         (list (cons "system" system-prompt)
               (cons "user"
                     (format "Task: %s\n\nOperation: %s\n%s\nDocument:\n%s"
                             instruction
                             (if insertion-only
                                 "INSERT-ANYWHERE"
                               "WRITE-ANYWHERE")
                             (if insertion-only
                                 "Find every appropriate insertion point across the entire document. Multiple separate insertions are expected when the task applies in multiple places."
                               "Make all appropriate insertions and replacements across the entire document.")
                             source)))
          (lambda (result)
            (if insertion-only
                (deslop--apply-insert-anywhere-result job result)
              (deslop--apply-write-anywhere-result job result))))))))

(defun deslop--run-improve (job)
  "Execute an improvement JOB that replaces its target atomically."
  (let* ((target (deslop-job-target job))
         (buffer (deslop-target-buffer target))
         (instruction (deslop-job-instruction job)))
    (deslop--run-selected-operation
     job
     deslop-improve-system
     (lambda (source)
       (format "Instruction: %s\n\nText to improve:\n%s"
               instruction source))
     (lambda (result)
       (deslop--apply-result
        job result
        (lambda (content display)
          (let* ((latest-start (deslop-target-start target))
                 (latest-end (deslop-target-end target))
                  original-source review-overlay)
             (setq review-overlay
                   (deslop--mutate-target
                    target
                    (lambda ()
                      (unless (and latest-start latest-end)
                        (error "improvement target no longer exists"))
                      (deslop-target-validate-source
                       target latest-start latest-end)
                      (setq original-source
                            (buffer-substring latest-start latest-end))
                      (save-excursion
                        (delete-region latest-start latest-end)
                        (goto-char latest-start)
                        (let ((review-start (point)))
                          (insert content)
                          (deslop--create-review-overlay
                           buffer review-start (point)
                           display original-source))))))
             review-overlay))))
      t)))

(defun deslop--run-summarize (job)
  "Execute a summarize JOB and write its result to a separate buffer."
  (let* ((target (deslop-job-target job))
         (buffer (deslop-target-buffer target)))
    (deslop--run-selected-operation
     job
     deslop-summarize-system
     (lambda (source) (format "Text to summarize:\n%s" source))
     (lambda (result)
       (deslop--apply-result
        job result
         (lambda (content display)
           (deslop-summary-append buffer content display)))))))

(deslop-register-job-handler 'insert #'deslop--run-insert)
(deslop-register-job-handler
 'write-anywhere #'deslop-operations--run-structured-edit)
(deslop-register-job-handler
 'insert-anywhere #'deslop-operations--run-structured-edit)
(deslop-register-job-handler 'improve #'deslop--run-improve)
(deslop-register-job-handler 'summarize #'deslop--run-summarize)

(provide 'deslop-operations)

;;; deslop-operations.el ends here
