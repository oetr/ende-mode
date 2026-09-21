;;; ende-operations.el --- LLM operation orchestration for ende -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools, llm

;;; Commentary:

;; Request construction, execution, result application, and interactive
;; commands for ende's insert, improve, and summarize operations.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ende-core)
(require 'ende-operation)
(require 'ende-edit)
(require 'ende-summary)
(require 'ende-provider)
(require 'ende-review)
(require 'ende-target)
(require 'ende-ui)
(require 'ende-llm)
(require 'ende-queue)

(defconst ende-insert-system
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

(defconst ende-improve-system
  "You improve text while preserving its intent and useful information. Return ONLY the improved text, without markdown fences or commentary."
  "System prompt instructions for improving a target region.")

(defconst ende-summarize-system
  "Summarize the supplied text accurately and concisely using Org mode syntax. Use not more than one sentence per line, unless the sentences are short. Prefer using vertical space over horizontal space. Give the user the best possible experience by structuring your output into sections, paragraphs, lists, code blocks. Be creative! Generate elegant, and readable text.

Know your audience---given your input, what does the user most likely want? If it is pure source code from a pure source code file, what does the user want? If it is a text file, what does the user want? (Don't tell me, just structure and focus your summary accordingly). Understand the combination of provided user input with the fact that the user wants a summary, and try to figure out what the user actually wants.

THIS IS ORG MODE.
Return only the summary in org-mode syntax. Do not add a title or 1-level headings. Never use Markdown syntax.

Use Org conventions:
- Use *bold* and /italic/ for emphasis. Using double **bold text** for bold is wrong, use *bold*.
- Use ~code~ for inline code and =verbatim text= for literal text.
- Org lists begin with - or 1, and can be nested. Use where approprate.
- Use >1 level headings to structure your output like this:  `** <section title>` or if necessary `*** <deeper section title>`. Though, if your summary is short, prefer a flat file.
- Put multiline code in:
  #+begin_src LANGUAGE
  code
  #+end_src
- Put quoted prose in:
  #+begin_quote
  quoted text
  #+end_quote

Preserve identifiers, commands, paths, and code exactly.

The source text is prefixed with its absolute line numbers in the form `LINE | text`.
These prefixes are metadata, not part of the source.
Cite relevant concrete claims and excerpts using exactly `[[ende-source:LINE][DESCRIPTION]]`, where LINE is one of the supplied source lines and DESCRIPTION is a concise label for the cited claim or excerpt.
An exceptionally readable DESCRIPTION can also be the buffer/file name and line number. Here is a good example:
`[[ende-source:10][growing-neural-gas in gng.rkt:10]]`
This example is great because the user can easier remember this than some generic \"source\" link.
Do not invent another citation target or link type.

When quoting Org source inside an Org source block, escape lines that Org could interpret structurally. Prefix a comma immediately before every quoted line beginning with *, ,*, #+, or ,#+.

Example:

#+begin_src org
,* Escaped heading
,#+TITLE: Escaped keyword
#+end_src

Apply this escaping only to quoted Org source. Do not escape ordinary summary markup or the #+begin_src and #+end_src delimiters themselves."
  "System prompt instructions for summarizing a target region.")

(defconst ende-write-anywhere-system
  "You edit a document by returning a strict JSON edit list. Inspect the provided
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

(defconst ende-insert-anywhere-system
  "You perform an INSERT-ANYWHERE operation by returning a JSON insertion list.
Inspect the provided document and find every position where the user's task calls
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

(defcustom ende-structured-edit-repair-attempts 1
  "Number of model feedback rounds for failed structured edits."
  :type 'integer
  :group 'ende)

(defcustom ende-structured-edit-move-point nil
  "Non-nil to move point to the final applied structured edit.
When nil, structured edits preserve point at the position current when their
result is applied."
  :type 'boolean
  :group 'ende)

(defun ende--validate-structured-repair-attempts ()
  "Validate the configured number of structured-edit repair rounds."
  (unless (and (integerp ende-structured-edit-repair-attempts)
               (>= ende-structured-edit-repair-attempts 0))
    (error "ende-structured-edit-repair-attempts must be nonnegative"))
  ende-structured-edit-repair-attempts)

(defun ende--call-with-job-error (job function)
  "Call FUNCTION and settle JOB if it signals an error."
  (condition-case caught
      (funcall function)
    ((error quit) (ende-job-fail job caught))))

(defun ende--llm-callbacks (job &optional on-done on-error)
  "Return standardized LLM callback plist for JOB."
  (list
   :on-start (lambda (cancel-function)
               (ende-job-cancellation-set job cancel-function))
   :on-event (lambda (event) (ende--handle-llm-event job event))
   :on-done (or on-done (lambda (res) (ende-job-finish job res)))
   :on-error (or on-error (lambda (err) (ende-job-fail job err)))))

(defun ende--handle-llm-event (job event)
  "Route normalized transport EVENT for JOB to interested subsystems."
  (when (ende-job-current-p job)
    (let ((type (plist-get event :type)))
      (unless (eq type :error)
        (pcase type
          (:status
           (when (ende-job-overlay job)
             (ende-ui-update-overlay
              (ende-job-overlay job) :status (plist-get event :status))))
          (:metadata
           (ende--apply-job-metadata job (plist-get event :metadata)))
          (:usage
           (when (ende-job-overlay job)
             (ende-ui-update-overlay
              (ende-job-overlay job)
              :tokens (plist-get event :tokens)
              :speed (plist-get event :speed)
              :estimated (plist-get event :estimated)))))
        (ende-job-emit-event job event)))))

(defun ende-operations--source-metadata-block (job)
  "Return the source metadata prompt block for JOB, or nil."
  (let ((metadata (ende-target-source-metadata
                   (ende-job-target job))))
    (when metadata
      (let ((fields
             (list
              (and (ende-provider-source-name-disclosure-p
                    (ende-job-profile job))
                   (cons "Name" (plist-get metadata :name)))
              (cons "Type" (plist-get metadata :type))
              (cons "Major mode" (plist-get metadata :major-mode)))))
        (concat
         "Source metadata:\n"
         (mapconcat (lambda (field)
                      (format "%s: %s" (car field) (cdr field)))
                    (delq nil fields)
                    "\n")
         "\n\n")))))

(defun ende-operations--add-source-metadata (job messages)
  "Prepend JOB's source metadata to the first user message in MESSAGES."
  (let ((block (ende-operations--source-metadata-block job)))
    (if (null block)
        messages
      (let ((added nil))
        (mapcar
         (lambda (message)
           (if (and (not added)
                    (consp message)
                    (equal (car message) "user"))
               (progn
                 (setq added t)
                 (cons (car message)
                       (concat block
                               (if (stringp (cdr message))
                                   (cdr message)
                                 (format "%s" (cdr message))))))
             message))
         messages)))))

(defun ende--dispatch-llm
    (job messages &optional on-done on-error no-begin)
  "Add source metadata, validate MESSAGES, record history, and invoke LLM."
  (let ((messages (ende-operations--add-source-metadata job messages)))
    (ende--validate-messages messages)
    (unless no-begin
      (ende-job-emit-event job (list :type :begin :messages messages)))
    (ende--handle-llm-event job '(:type :status :status "Thinking"))
    (funcall ende-llm-function
             messages
             (ende--llm-callbacks job on-done on-error)
             (ende-job-profile job))))

(defun ende--validate-messages (messages)
  "Return MESSAGES after enforcing character limits."
  (unless (or (null ende-max-request-chars)
              (and (integerp ende-max-request-chars)
                   (> ende-max-request-chars 0)))
    (error "ende-max-request-chars must be nil or a positive integer"))
  (let ((chars (cl-loop for message in messages
                        sum (+ (length (format "%s" (car message)))
                               (length (format "%s" (cdr message)))))))
    (when (and ende-max-request-chars
               (> chars ende-max-request-chars))
      (error "request is %d characters; ende-max-request-chars is %d"
             chars ende-max-request-chars)))
  messages)

(defun ende--selected-job-source (job operation &optional writable)
  "Return the validated selected source for JOB.
OPERATION names the operation for size diagnostics.  When WRITABLE is
non-nil, also reject read-only targets before reading the source."
  (let* ((target (ende-job-target job))
         (buffer (ende-target-buffer target))
         (start (ende-target-start target))
         (end (ende-target-end target)))
    (unless (and (buffer-live-p buffer) start end)
      (error "%s target no longer exists" operation))
    (with-current-buffer buffer
      (ende-target-with-restriction target
        (when writable
          (barf-if-buffer-read-only))
        (ende-target-ensure-size start end operation)
        (ende-target-selected-source target start end)))))

(defun ende--run-selected-operation
    (job system-prompt prompt-function on-done &optional writable)
  "Run a selected-text operation for JOB.
SYSTEM-PROMPT is sent as the system message.  PROMPT-FUNCTION receives the
validated source and returns the user message.  ON-DONE receives the raw LLM
result.  When WRITABLE is non-nil, the target must be writable while its
source is captured."
  (ende--call-with-job-error
   job
   (lambda ()
     (let ((source (ende--selected-job-source
                    job (if writable "Improve" "Summarize") writable)))
       (ende--dispatch-llm
        job
        (list (cons "system" system-prompt)
              (cons "user" (funcall prompt-function source)))
        on-done)))))

;;;; Result application

(defun ende--apply-job-metadata (job metadata)
  "Apply endpoint METADATA to JOB's overlay and history."
  (when (ende-job-current-p job)
    (let* ((source (plist-get metadata :source))
           (rank (pcase source (:response 2) (:discovery 1) (_ 0)))
           (current-rank (or (ende-job-metadata-rank job) -1)))
      (when (>= rank current-rank)
        (setf (ende-job-metadata-rank job) rank
              (ende-job-metadata job) metadata)
        (let ((display
               (ende-provider-display-metadata
                (ende-job-profile job) metadata)))
          (when (ende-job-overlay job)
            (ende-ui-update-overlay
             (ende-job-overlay job)
             :client (plist-get display :client)
             :model (plist-get display :model)))
          display)))))

(defun ende--job-display-metadata (job result)
  "Return display metadata for JOB using RESULT and cached discovery."
  (ende-provider-result-display-metadata
   (ende-job-profile job)
   (ende-job-metadata job)
   result))

(defun ende--create-review-overlay (buffer start end display original)
  "Mark generated text from START to END in BUFFER for review.
DISPLAY supplies the provider label and ORIGINAL is restored on rejection."
  (ende-review-create-overlay
   buffer start end
   (plist-get display :model)
   (plist-get display :client)
   original))

(defun ende--mutate-target (target mutation)
  "Run MUTATION as one writable, atomic change to TARGET.
MUTATION runs in TARGET's buffer and returns the generated review overlay."
  (let ((buffer (ende-target-buffer target)))
    (unless (buffer-live-p buffer)
      (error "target buffer no longer exists"))
    (with-current-buffer buffer
      (ende-target-with-restriction target
        (barf-if-buffer-read-only)
        (ende-target-with-application target
          (atomic-change-group
            (funcall mutation)))))))

(defun ende--result-content (result)
  "Extract and validate completion content from RESULT or a legacy string."
  (let ((content (if (stringp result) result (plist-get result :content))))
     (when (and ende-max-output-chars
                (stringp content)
                (> (length content) ende-max-output-chars))
      (error "completion exceeds ende-max-output-chars"))
    content))

(defun ende--application-error (error)
  "Return ERROR as an application error suitable for job settlement."
  (if (ende-error-p error)
      error
    (ende-error-create :kind 'application
                         :message (if (stringp error)
                                      error
                                    (error-message-string error)))))

(defun ende--apply-result (job result apply-function &optional skip-review)
  "Apply RESULT for JOB with APPLY-FUNCTION and settle JOB.
APPLY-FUNCTION receives trimmed content and display metadata.  Unless
SKIP-REVIEW is non-nil, it must return the review overlay it creates."
  (when (ende-job-current-p job)
    (condition-case failure
        (let* ((content (ende-trim-trailing
                         (or (ende--result-content result) "")))
               (display (ende--job-display-metadata job result))
               (review-overlay nil))
           (when (string-empty-p content)
             (let ((finish-reason (and (listp result)
                                       (plist-get result :finish-reason))))
               (if finish-reason
                   (error "empty completion (provider stopped: %s)"
                          finish-reason)
                 (error "empty completion"))))
          (setq review-overlay (funcall apply-function content display))
          (unless (or skip-review
                      (ende-review-overlay-p review-overlay))
            (error "generated output could not be marked for review"))
          (ende-job-finish job result))
       ((error quit)
        (ende-job-fail job (ende--application-error failure))))))

(defun ende--apply-insert-result (job result)
  "Apply RESULT for insert JOB atomically and settle JOB."
  (ende--apply-result
   job result
   (lambda (content display)
      (let* ((target (ende-job-target job))
             (buffer (ende-target-buffer target))
             ins-start ins-end review-overlay)
        (setq review-overlay
              (ende--mutate-target
               target
               (lambda ()
                 (let* ((selected (ende-target-selected target))
                        (start (ende-target-start target))
                        (end (ende-target-end target))
                        (cursor (ende-target-cursor target)))
                   (unless cursor
                     (error "cursor anchor no longer exists"))
                   (when (and selected
                              (or (null start) (null end) (> start end)))
                     (error "selection anchors no longer exist"))
                   (unless (ende-target-restriction-valid-p
                            target (or start cursor) (or end cursor))
                     (error "target is outside the original restriction"))
                   (when selected
                     (ende-target-validate-source target start end))
                   (setq ins-start cursor)
                   (save-excursion
                     (goto-char cursor)
                     (pcase (ende-target-placement target)
                       (:before (insert content "\n"))
                       (:after  (insert "\n" content))
                       (:inside (insert "\n" content "\n"))
                       (_       (insert content)))
                     (setq ins-end (point))
                     (ende--create-review-overlay
                      buffer ins-start ins-end display ""))))))
         (dolist (other-job (append (ende-active-jobs)
                                    (ende-pending-jobs)))
           (let ((other-target (ende-job-target other-job)))
             (when (and (not (eq other-job job))
                        (> (ende-job-id other-job) (ende-job-id job))
                         (eq (ende-job-type other-job) 'insert)
                        (eq (ende-target-buffer other-target) buffer))
               (ende-ignore-errors
                #'ende-target-advance-queued-anchors
                (list other-target) buffer ins-start ins-end
                (lambda (_target)
                  (let ((overlay (ende-job-overlay other-job)))
                    (when (and (overlayp overlay) (overlay-buffer overlay)
                               (numberp (overlay-start overlay))
                               (numberp (overlay-end overlay))
                               (= (overlay-start overlay) ins-start)
                               (= (overlay-end overlay) ins-start))
                      (move-overlay overlay ins-end ins-end buffer))))))))
       (ende-ignore-errors
        #'message "ende: inserted %d chars in %s"
        (length content) (buffer-name buffer))
       review-overlay))))

(defun ende--apply-anchor-edit (target buffer source-start edit display)
  "Apply anchor-based EDIT relative to SOURCE-START in TARGET."
  (let* ((start (+ source-start (plist-get edit :start)))
         (end (+ source-start (plist-get edit :end)))
         (replace (equal (plist-get edit :position) "replace"))
         (text (plist-get edit :text)))
    (ende--mutate-target
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
           (ende--create-review-overlay
            buffer review-start (point) display original)))))))

(defun ende--apply-insertion-edit (target buffer start edit display)
  "Apply one insertion EDIT to TARGET and return its review overlay."
  (ende--apply-anchor-edit target buffer start edit display))

(defun ende--structured-state (job)
  "Return or initialize the structured-edit state for JOB."
  (or (ende-job-structured-state job)
      (setf (ende-job-structured-state job)
            (list :type (ende-job-type job)
                  :instruction nil
                  :system-prompt
                  (pcase (ende-job-type job)
                    ('write-anywhere ende-write-anywhere-system)
                    ('insert-anywhere ende-insert-anywhere-system))
                  :pass-count 0
                  :requested nil
                  :first-pass-applied 0
                  :repaired 0
                  :repair-attempts 0
                  :failures nil
                  :last-result nil
                  :source nil))))

(defun ende--structured-edit-descending-p (left right key)
  "Return non-nil when LEFT should be applied after RIGHT using KEY."
  (or (> (plist-get left key) (plist-get right key))
      (and (= (plist-get left key) (plist-get right key))
           (let ((left-replace (equal (plist-get left :position) "replace"))
                 (right-replace (equal (plist-get right :position) "replace")))
             (or (and left-replace (not right-replace))
                 (and (eq left-replace right-replace)
                      (> (plist-get left :index)
                         (plist-get right :index))))))))

(defun ende--structured-source (job)
  "Return the current restricted source data for structured JOB."
  (let* ((target (ende-job-target job))
         (buffer (ende-target-buffer target)))
    (unless (buffer-live-p buffer)
      (error "%s target no longer exists" (ende-job-type job)))
    (with-current-buffer buffer
      (save-excursion
        (ende-target-with-restriction target
          (list :buffer buffer
                :start (point-min)
                :source (buffer-substring-no-properties
                         (point-min) (point-max))))))))

(defun ende--structured-edit-pass (job result)
  "Parse and apply one structured RESULT for JOB.
Return pass data without settling JOB."
  (let* ((type (ende-job-type job))
         (target (ende-job-target job))
         (source-data (ende--structured-source job))
         (buffer (plist-get source-data :buffer))
         (source-start (plist-get source-data :start))
         (source (plist-get source-data :source))
         (content (ende-trim-trailing
                   (or (ende--result-content result) "")))
         (parsed (if (eq type 'insert-anywhere)
                     (ende-edit-parse-insertions content source)
                   (ende-edit-parse-replacements content source)))
         (edits (plist-get parsed :edits))
         (failures (plist-get parsed :errors))
         (application-failures nil)
         (display (ende--job-display-metadata job result))
         (key (if (eq type 'insert-anywhere) :offset :start)))
    (with-current-buffer buffer
      (let ((apply-edits
             (lambda ()
               (ende-target-with-restriction target
                 (barf-if-buffer-read-only)
                 (dolist (edit
                          (sort (copy-sequence edits)
                                (lambda (left right)
                                  (ende--structured-edit-descending-p
                                   left right key))))
                   (condition-case caught
                       (ende--apply-anchor-edit
                        target buffer source-start edit display)
                     ((error quit)
                      (push (list :index (plist-get edit :index)
                                  :message (error-message-string caught)
                                  :raw (plist-get edit :raw))
                            application-failures))))))))
        (if ende-structured-edit-move-point
            (funcall apply-edits)
          (save-excursion
            (funcall apply-edits)))))
    (setq failures (append failures (nreverse application-failures)))
    (list :source source
          :requested (+ (length edits) (length failures))
          :applied (- (length edits) (length application-failures))
          :failures failures)))

(defun ende--structured-summary (state)
  "Return durable counters and failures from structured STATE."
  (let ((failures (plist-get state :failures)))
    (list :requested (or (plist-get state :requested) 0)
          :first-pass-applied (or (plist-get state :first-pass-applied) 0)
          :repaired (or (plist-get state :repaired) 0)
          :skipped (length failures)
          :repair-attempts (or (plist-get state :repair-attempts) 0)
          :final-failures failures)))

(defun ende--structured-preserve-failure-indices (failures previous)
  "Keep the original edit indices for repaired FAILURES.
PREVIOUS contains failures from the preceding structured pass."
  (let ((remaining (copy-sequence previous)))
    (mapcar
     (lambda (failure)
       (if (null remaining)
           failure
         (let ((annotated (copy-sequence failure))
               (origin (pop remaining)))
            (plist-put annotated :index (plist-get origin :index)))))
      failures)))

(defun ende--structured-annotate-result (result state)
  "Return RESULT annotated with structured STATE summary and failures."
  (let* ((annotated (if (listp result)
                        (copy-sequence result)
                      (list :content result)))
         (failures (plist-get state :failures))
         (type (plist-get state :type)))
    (when failures
      (setq annotated
            (if (eq type 'insert-anywhere)
                (ende-edit-add-insertion-failures annotated failures)
              (ende-edit-add-replacement-failures annotated failures))))
    (plist-put annotated :structured-summary
               (ende--structured-summary state))))

(defun ende--structured-report (state)
  "Report the aggregate outcome represented by structured STATE."
  (let* ((summary (ende--structured-summary state))
         (applied (+ (plist-get summary :first-pass-applied)
                     (plist-get summary :repaired))))
    (message "ende: applied %d of %d edits; %d repaired; %d skipped"
             applied
             (plist-get summary :requested)
             (plist-get summary :repaired)
             (plist-get summary :skipped))))

(defun ende--structured-finish (job result)
  "Settle JOB with the final structured RESULT and its aggregate state."
  (let* ((state (ende--structured-state job))
         (summary (ende--structured-summary state))
         (annotated (ende--structured-annotate-result result state))
         (applied (+ (plist-get summary :first-pass-applied)
                     (plist-get summary :repaired))))
    (ende--structured-report state)
    (if (> applied 0)
        (ende-job-finish job annotated)
      (ende-job-fail
       job
       (ende--application-error
        (format "%s produced no applicable edits: %s"
                (if (eq (plist-get state :type) 'insert-anywhere)
                    "insert-anywhere"
                  "write-anywhere")
                (ende-edit-format-failures
                 (plist-get state :failures) "; ")))
       annotated))))

(defun ende--structured-transport-error (job error)
  "Finish structured JOB after a transport ERROR during repair."
  (when (ende-job-current-p job)
    (let* ((state (ende--structured-state job))
           (previous (plist-get state :last-result))
           (annotated (and previous
                           (ende--structured-annotate-result
                            previous state))))
      (ende--structured-report state)
      (ende-job-fail job error annotated))))

(defun ende--structured-repair-prompt (state)
  "Build a feedback prompt for the remaining failures in STATE."
  (format
   "Task: %s\n\nOperation: %s\n\nThe previous response applied %d edits successfully. Return only edits that address the remaining failures below. Do not repeat any successful edit. Use the current document as the source of truth.\n\nFailed edits and exact diagnostics:\n%s\n\nCurrent document:\n%s"
   (or (plist-get state :instruction) "complete the requested task")
   (if (eq (plist-get state :type) 'insert-anywhere)
       "INSERT-ANYWHERE"
     "WRITE-ANYWHERE")
   (+ (plist-get state :first-pass-applied)
      (plist-get state :repaired))
   (ende-edit-format-failures (plist-get state :failures) "\n")
   (or (plist-get state :source) "")))

(defun ende--dispatch-structured-repair (job)
  "Dispatch one feedback request for the remaining edits in JOB."
  (let* ((state (ende--structured-state job))
         (attempts (1+ (or (plist-get state :repair-attempts) 0))))
    (setf (ende-job-structured-state job)
          (plist-put state :repair-attempts attempts))
    (ende--dispatch-llm
     job
     (list (cons "system" (plist-get state :system-prompt))
           (cons "user" (ende--structured-repair-prompt state)))
     (lambda (repair-result)
       (ende--handle-structured-result job repair-result))
     (lambda (error)
       (ende--structured-transport-error job error))
     t)))

(defun ende--handle-structured-result (job result)
  "Apply one structured RESULT and repair or settle JOB as appropriate."
  (when (ende-job-current-p job)
    (let ((state (ende--structured-state job)))
      (condition-case failure
          (let* ((repair-p (> (or (plist-get state :pass-count) 0) 0))
                 (pass (ende--structured-edit-pass job result))
                 (failures (plist-get pass :failures))
                 (failures (if repair-p
                               (ende--structured-preserve-failure-indices
                                failures (plist-get state :failures))
                             failures))
                 (applied (plist-get pass :applied)))
            (setf state (plist-put state :pass-count
                                   (1+ (or (plist-get state :pass-count) 0))))
            (when (null (plist-get state :requested))
              (setf state (plist-put state :requested
                                     (plist-get pass :requested))))
            (setf state (plist-put state :failures failures)
                  state (plist-put state :last-result result))
            (if repair-p
                (setf state (plist-put state :repaired
                                       (+ (or (plist-get state :repaired) 0)
                                          applied)))
              (setf state (plist-put state :first-pass-applied applied)))
            (setf (ende-job-structured-state job) state)
            (if (and failures
                     (< (plist-get state :repair-attempts)
                        ende-structured-edit-repair-attempts))
                (progn
                  (setf state
                        (plist-put
                         state :source
                         (plist-get (ende--structured-source job) :source)))
                  (setf (ende-job-structured-state job) state)
                  (ende--dispatch-structured-repair job))
              (ende--structured-finish job result)))
        (ende-edit-resource-error
         (ende-job-fail
          job
          (ende--application-error failure)
          (ende--structured-annotate-result result state)))
        ((error quit)
         (ende-job-fail
          job
          (ende--application-error failure)
          (ende--structured-annotate-result result state)))))))

(defun ende--apply-write-anywhere-result (job result)
  "Apply a WRITE-ANYWHERE RESULT and repair or settle JOB."
  (ende--handle-structured-result job result))

(defun ende--apply-insert-anywhere-result (job result)
  "Apply an INSERT-ANYWHERE RESULT and repair or settle JOB."
  (ende--handle-structured-result job result))

;;;; Operation handlers

(defun ende--run-insert (job)
  "Execute an insert JOB."
  (ende--call-with-job-error
   job
   (lambda ()
     (let* ((target (ende-job-target job))
            (buffer (ende-target-buffer target))
            (selected (ende-target-selected target))
            (start (ende-target-start target))
            (end (ende-target-end target))
            (cursor (ende-target-cursor target))
            context)
       (unless (and cursor (buffer-live-p buffer))
         (signal 'error '("target anchor no longer exists")))
       (with-current-buffer buffer
         (ende-target-with-restriction target
           (barf-if-buffer-read-only)
           (if selected
               (let ((offset (or (ende-target-offset target) 0)))
                 (ende-target-validate-source target start end)
                 (setq context
                        (ende-target-buffer-marker-context
                         start end (+ start offset))))
             (unless (ende-target-restriction-valid-p target cursor cursor)
               (error "cursor is outside the original restriction"))
             (setq context
                    (ende-target-buffer-marker-context
                     (point-min) (point-max) cursor)))))
       (ende--dispatch-llm
        job
        (list (cons "system" ende-insert-system)
              (cons "user"
                    (format "Task: %s\n\nDocument:\n%s"
                            ende-insert-instruction context)))
         (lambda (result) (ende--apply-insert-result job result)))))))

(defun ende-operations--run-structured-edit (job)
  "Execute a structured-edit JOB using its operation specification."
  (ende--call-with-job-error
   job
   (lambda ()
      (let* ((type (ende-job-type job))
             (target (ende-job-target job))
             (buffer (ende-target-buffer target))
             (start (ende-target-start target))
             (end (ende-target-end target))
             (insertion-only
              (eq (ende-operation-structured-result type) 'insertion))
             (instruction (or (ende-job-instruction job)
                              (pcase type
                                ('write-anywhere ende-write-anywhere-instruction)
                                ('insert-anywhere ende-insert-anywhere-instruction))))
              (system-prompt (pcase type
                                ('write-anywhere ende-write-anywhere-system)
                                ('insert-anywhere ende-insert-anywhere-system)))
              source)
         (ende--validate-structured-repair-attempts)
         (unless (and (buffer-live-p buffer) start end)
           (error "write-anywhere target no longer exists"))
         (with-current-buffer buffer
           (save-excursion
             (ende-target-with-restriction target
               (barf-if-buffer-read-only)
               (ende-target-validate-source target start end)
               (setq source (buffer-substring-no-properties start end)))))
         (setf (ende-job-structured-state job)
               (list :type type
                     :instruction instruction
                     :system-prompt system-prompt
                     :pass-count 0
                     :requested nil
                     :first-pass-applied 0
                     :repaired 0
                     :repair-attempts 0
                     :failures nil
                     :last-result nil
                     :source nil))
         (ende--dispatch-llm
         job
         (list (cons "system" system-prompt)
               (cons "user"
                     (format "Task: %s\n\nOperation: %s\n%s\nDocument:\n%s"
                             instruction
                             (if insertion-only
                                 "INSERT-ANYWHERE"
                               "WRITE-ANYWHERE")
                             (if insertion-only
                                 "Find every appropriate insertion point within the provided document. Multiple separate insertions are expected when the task applies in multiple places."
                               "Make all appropriate insertions and replacements within the provided document.")
                             source)))
          (lambda (result)
            (if insertion-only
                (ende--apply-insert-anywhere-result job result)
              (ende--apply-write-anywhere-result job result))))))))

(defun ende--run-improve (job)
  "Execute an improvement JOB that replaces its target atomically."
  (let* ((target (ende-job-target job))
         (buffer (ende-target-buffer target))
         (instruction (ende-job-instruction job)))
    (ende--run-selected-operation
     job
     ende-improve-system
     (lambda (source)
       (format "Instruction: %s\n\nText to improve:\n%s"
               instruction source))
     (lambda (result)
       (ende--apply-result
        job result
        (lambda (content display)
          (let* ((latest-start (ende-target-start target))
                 (latest-end (ende-target-end target))
                  original-source review-overlay)
             (setq review-overlay
                   (ende--mutate-target
                    target
                    (lambda ()
                      (unless (and latest-start latest-end)
                        (error "improvement target no longer exists"))
                      (ende-target-validate-source
                       target latest-start latest-end)
                      (setq original-source
                            (buffer-substring latest-start latest-end))
                      (save-excursion
                        (delete-region latest-start latest-end)
                        (goto-char latest-start)
                        (let ((review-start (point)))
                          (insert content)
                          (ende--create-review-overlay
                           buffer review-start (point)
                           display original-source))))))
             review-overlay))))
      t)))

(defun ende--run-summarize (job)
  "Execute a summarize JOB and write its result to a separate buffer."
  (let* ((target (ende-job-target job))
         (buffer (ende-target-buffer target))
         (summary-source nil))
    (ende--run-selected-operation
     job
     ende-summarize-system
     (lambda (source)
       (setq summary-source
             (ende-summary-source-capture
              buffer (ende-target-start target) (ende-target-end target)))
       (format "Source lines: %d-%d\n\nLine-numbered text to summarize:\n%s"
               (ende-summary-source-start-line summary-source)
               (ende-summary-source-end-line summary-source)
               (ende-summary-number-source
                source (ende-summary-source-start-line summary-source))))
     (lambda (result)
       (ende--apply-result
        job result
        (lambda (content display)
          (ende-summary-append summary-source content display))
        t)))))

(ende-register-job-handler 'insert #'ende--run-insert)
(ende-register-job-handler
 'write-anywhere #'ende-operations--run-structured-edit)
(ende-register-job-handler
 'insert-anywhere #'ende-operations--run-structured-edit)
(ende-register-job-handler 'improve #'ende--run-improve)
(ende-register-job-handler 'summarize #'ende--run-summarize)

(provide 'ende-operations)

;;; ende-operations.el ends here
