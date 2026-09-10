;; -*- lexical-binding: t; -*-

(defgroup deslop nil
  "Local-LLM text assistance: insert at cursor, improve, summarize."
  :prefix "deslop-"
  :group 'convenience)

(defvar deslop-queue nil
  "Queue of pending jobs.  Each job is (TYPE . PROPERTIES).")

(defvar deslop-running nil
  "Non-nil while a job is being processed.")

(defvar deslop--current-job nil
  "The job currently being processed.")

(defvar deslop--current-job-overlay nil
  "Dynamic reference to the overlay of the active job for stream updates.")

(defvar deslop--managed-overlays nil
  "List of all active job overlays across queue and execution.")

(defvar deslop--spinner-frames ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
  "Frames for the dynamic overlay spinner animation.")

(defvar deslop--spinner-index 0)
(defvar deslop--spinner-timer nil)

(defface deslop-working-face
  '((t :background "#1e293b" :underline "#38bdf8"))
  "Face applied to buffer text currently being processed by LLM."
  :group 'deslop)

(defcustom deslop-url "http://localhost:8080/v1/chat/completions"
  "OpenAI-compatible chat completions endpoint."
  :type 'string
  :group 'deslop)

(defcustom deslop-model "qwen3.8-27b"
  "Model name sent to the LLM server."
  :type 'string
  :group 'deslop)

(defcustom deslop-temperature 0.2
  "Sampling temperature for LLM requests."
  :type 'number
  :group 'deslop)

(defcustom deslop-max-context-chars 24000
  "Maximum number of characters sent to the LLM as context.
Tokens are estimated as chars/4."
  :type 'integer
  :group 'deslop)

(defcustom deslop-window-chars 8000
  "Characters kept on each side of the cursor when context is elided."
  :type 'integer
  :group 'deslop)

(defcustom deslop-anchor-chars 120
  "Characters captured on each side of the cursor as an insert anchor."
  :type 'integer
  :group 'deslop)

(defcustom deslop-summary-chars 4000
  "Approximate target length in characters for generated summaries."
  :type 'integer
  :group 'deslop)

(defvar deslop-llm-function #'deslop-llm-default
  "Function used to talk to the LLM.
Called as (FN MESSAGES DONE).  MESSAGES is a list of (ROLE . CONTENT)
pairs.  DONE is called as (DONE CONTENT ERR): CONTENT is the response
string on success, ERR describes the problem on failure.")

(defcustom deslop-insert-instruction
  "Write the most natural continuation of the text at the cursor position."
  "Instruction given to the LLM for deslop-insert-at-cursor."
  :type 'string
  :group 'deslop)

(setq deslop-queue nil)
(setq deslop-running nil)

(defun deslop--refresh-all-overlays ()
  "Refresh all active overlays. Queued items remain static; active items spin."
  (if (null deslop--managed-overlays)
      (deslop--stop-spinner)
    (setq deslop--spinner-index
          (% (1+ deslop--spinner-index) (length deslop--spinner-frames)))
    (let ((spinner (aref deslop--spinner-frames deslop--spinner-index)))
      (dolist (ov deslop--managed-overlays)
        (when (and (overlayp ov) (buffer-live-p (overlay-buffer ov)))
          (let* ((status (or (overlay-get ov 'status) "Queued"))
                 (is-queued (string= status "Queued"))
                 (icon (if is-queued "⏳" spinner))
                 (tokens (or (overlay-get ov 'tokens) 0))
                 (tok-str (if (> tokens 0) (format " (%d tok)" tokens) ""))
                 (badge (format " [%s %s%s] " icon status tok-str)))
            (overlay-put ov 'before-string
                         (propertize badge 'face '(:foreground "#facc15" :weight bold)))))))))

(defun deslop--start-spinner ()
  "Start spinner animation timer if not currently running."
  (unless (timerp deslop--spinner-timer)
    (setq deslop--spinner-index 0)
    (setq deslop--spinner-timer
          (run-with-timer 0 0.1 #'deslop--refresh-all-overlays))))

(defun deslop--stop-spinner ()
  "Stop the overlay spinner timer."
  (when (timerp deslop--spinner-timer)
    (cancel-timer deslop--spinner-timer)
    (setq deslop--spinner-timer nil)))

(defun deslop--create-overlay (buf beg end label)
  "Create an indicator overlay in BUF from BEG to END with LABEL."
  (with-current-buffer buf
    (let* ((ov-end (if (= beg end) (min (point-max) (1+ beg)) end))
           (ov (make-overlay beg ov-end)))
      (overlay-put ov 'face 'deslop-working-face)
      (overlay-put ov 'status label)
      (overlay-put ov 'tokens 0)
      (push ov deslop--managed-overlays)
      (deslop--start-spinner)
      ov)))

(defun deslop--delete-overlay (ov)
  "Delete overlay OV and remove it from managed tracking."
  (when (overlayp ov)
    (setq deslop--managed-overlays (delq ov deslop--managed-overlays))
    (delete-overlay ov)
    (when (null deslop--managed-overlays)
      (deslop--stop-spinner))))

(defun deslop--pick-closest (candidates)
  "CANDIDATES is a list of (POS . DIST).
Return the POS with the smallest DIST, or nil."
  (let* ((best nil))
    (dolist (c candidates)
      (when (or (null best) (< (cdr c) (cdr best)))
        (setq best c)))
    (and best (car best))))

(defun deslop--find-string (text start)
  "Case-sensitively find TEXT in the current buffer at or after START."
  (let* ((n (length text))
         (limit (- (point-max) n)))
    (and (> n 0)
         (let ((pos start) (found nil))
           (while (and (null found) (<= pos limit))
             (when (string= text (buffer-substring-no-properties pos (+ pos n)))
               (setq found pos))
             (setq pos (1+ pos)))
           found))))

(defun deslop--find-text-at (text orig-pos)
  "Find the occurrence of TEXT closest to ORIG-POS in current buffer."
  (let* ((cands nil)
         (s (deslop--find-string text (point-min))))
    (while s
      (push (cons s (abs (- s orig-pos))) cands)
      (setq s (deslop--find-string text (1+ s))))
    (deslop--pick-closest cands)))

(defun deslop--find-insert-point (before after orig-pos)
  "Find the insertion position for a (BEFORE . AFTER) cursor anchor."
  (if (string= after "")
      (if (string= before "") (point-min) (point-max))
    (let* ((exact nil) (any nil)
           (s (deslop--find-string after (point-min))))
      (while s
        (let* ((d (abs (- s orig-pos)))
               (ok (or (string= before "")
                       (and (>= s (+ (point-min) (length before)))
                            (string= before
                                     (buffer-substring-no-properties
                                      (- s (length before)) s))))))
          (push (cons s d) any)
          (when ok (push (cons s d) exact)))
        (setq s (deslop--find-string after (1+ s))))
      (let* ((r (deslop--pick-closest exact)))
        (or r (deslop--pick-closest any))))))

(defun deslop--live-buffer (p)
  "Return the job's buffer if it is still alive, else nil."
  (let* ((buf (plist-get p :buffer)))
    (and (buffer-live-p buf) buf)))

(defun deslop--current-region ()
  "Return (START . END) of the active region, or nil."
  (when (use-region-p)
    (cons (region-beginning) (region-end))))

(defun deslop--whitespacep (c)
  "Non-nil if C is a whitespace character."
  (memq c '(32 9 10 13 11 12)))

(defun deslop--trim-trailing (s)
  "Remove trailing whitespace from S, keeping leading whitespace intact."
  (let* ((end (length s)))
    (while (and (> end 0) (deslop--whitespacep (aref s (1- end))))
      (setq end (1- end)))
    (substring s 0 end)))

(defun deslop--insert-llm (buf ins-pos region context instruction ov)
  "Ask LLM for insert text and apply it to BUF using OV's live position."
  (let ((deslop--current-job-overlay ov))
    (funcall deslop-llm-function
             (list (cons "system" deslop-insert-system)
                   (cons "user" (format "Task: %s\n\nDocument:\n%s" instruction context)))
             (lambda (content err)
               (if (null content)
                   (deslop--skip (format "insert: LLM error: %s" err))
                 (let* ((text (deslop--trim-trailing content)))
                   (with-current-buffer buf
                     (save-excursion
                       ;; Read live overlay position at millisecond of insertion
                       (let ((target-pos (if (and (overlayp ov) (overlay-buffer ov))
                                             (overlay-start ov)
                                           ins-pos)))
                         (goto-char target-pos)
                         (cond
                          ((null region) (insert text))
                          ((= target-pos (car region)) (insert text "\n"))
                          ((= target-pos (cdr region)) (insert "\n" text))
                          (t (insert "\n" text "\n"))))))
                   (message "insert: inserted %d chars in %s"
                            (length text) (buffer-name buf))
                   (deslop--job-done)))))))

(defun deslop-process-next ()
  "Process the next queued job if the queue is not idle."
  (unless (or deslop-running (null deslop-queue))
    (setq deslop-running t)
    (let* ((job (pop deslop-queue)))
      (setq deslop--current-job job)
      (condition-case err
          (deslop-run-job job)
        (error
         (message "error in job %s: %s" (car job) (error-message-string err))
         (deslop--job-done))))))

(defun deslop-enqueue (job)
  "Add JOB to the queue and start processing if idle."
  (setq deslop-queue (nconc deslop-queue (list job)))
  (deslop-process-next))

(defun deslop--job-done ()
  "Finish current job, clean its overlay, and continue queue."
  (when deslop--current-job
    (let ((ov (plist-get (cdr deslop--current-job) :overlay)))
      (deslop--delete-overlay ov))
    (setq deslop--current-job nil))
  (setq deslop-running nil)
  (deslop-process-next))

(defun deslop-run-job (job)
  "Execute JOB (async) and finish it via deslop--job-done."
  (pcase job
    (`(insert . ,p) (deslop--run-insert p))
    (_ (progn
         (message "unknown job: %S" job)
         (deslop--job-done)))))

(defun deslop--skip (msg)
  "Skip current job with logged reason."
  (message "deslop: skipped (%s)" msg)
  (deslop--job-done))

(defun deslop--marker-context (text offset)
  "Return TEXT with a <CURSOR> marker at OFFSET."
  (setq offset (max 0 (min (length text) offset)))
  (if (<= (length text) deslop-max-context-chars)
      (concat (substring text 0 offset) "<CURSOR>" (substring text offset))
    (let* ((w deslop-window-chars)
           (bs (max 0 (- offset w)))
           (be (min (length text) (+ offset w)))
           (win (substring text bs be))
           (co (- offset bs)))
      (concat
       (when (> bs 0) (format "...[%d characters elided]...\n" bs))
       (substring win 0 co)
       "<CURSOR>"
       (substring win co)
       (when (> (- (length text) be) 0)
         (format "\n...[%d characters elided]..." (- (length text) be)))))))

(defun deslop--run-insert (p)
  "Execute an insert job."
  (let* ((buf (deslop--live-buffer p))
         (ov (plist-get p :overlay)))
    (if (null buf)
        (deslop--skip "insert: buffer no longer exists")
      (with-current-buffer buf
        (let* ((region (plist-get p :region))
               (offset (plist-get p :offset))
               (anchor (plist-get p :anchor))
               (live-orig (if (and (overlayp ov) (overlay-buffer ov))
                              (overlay-start ov)
                            (plist-get p :orig))))
          (if (plist-get p :selected)
              (let* ((pos (deslop--find-text-at anchor live-orig)))
                (unless pos (setq pos live-orig))
                (let* ((len (length anchor))
                       (off (max 0 (min len offset)))
                       ins)
                  (cond
                   ((<= offset 0) (setq ins pos))
                   ((>= offset len) (setq ins (+ pos len)))
                   (t (setq ins (+ pos off))))
                  (when (overlayp ov)
                    (move-overlay ov pos (+ pos len))
                    (overlay-put ov 'status "Thinking"))
                  (deslop--insert-llm
                   buf ins (cons pos (+ pos len))
                   (deslop--marker-context anchor off)
                   deslop-insert-instruction ov)))
            (let* ((pos (deslop--find-insert-point (car anchor) (cdr anchor) live-orig))
                   (ctx (buffer-substring-no-properties (point-min) (point-max))))
              (unless pos (setq pos live-orig))
              (when (overlayp ov)
                (move-overlay ov pos (min (point-max) (1+ pos)))
                (overlay-put ov 'status "Thinking"))
              (deslop--insert-llm
               buf pos nil
               (deslop--marker-context ctx (- pos (point-min)))
               deslop-insert-instruction ov))))))))

(defun deslop-insert-at-cursor ()
  "Queue a request to insert LLM text at the cursor."
  (interactive)
  (let* ((buf (current-buffer))
         (region (deslop--current-region))
         (pt (point))
         (ov (if region
                 (deslop--create-overlay buf (car region) (cdr region) "Queued")
               (deslop--create-overlay buf pt pt "Queued"))))
    (if region
        (let* ((s (car region)) (e (cdr region)))
          (deslop-enqueue
           (list 'insert :buffer buf :region (cons s e)
                 :offset (- pt s)
                 :anchor (buffer-substring-no-properties s e)
                 :orig s :selected t :overlay ov)))
      (let* ((from (min pt (max (point-min) (- pt deslop-anchor-chars))))
             (to (max pt (min (point-max) (+ pt deslop-anchor-chars)))))
        (deslop-enqueue
         (list 'insert :buffer buf
               :region (cons (point-min) (point-max))
               :offset (- pt (point-min))
               :anchor (cons (buffer-substring-no-properties from pt)
                             (buffer-substring-no-properties pt to))
               :orig pt :selected nil :overlay ov))))
    (message "deslop: queued insert-at-cursor")))

(defun deslop-llm-default (messages done)
  "Call LLM asynchronously via curl SSE stream to update overlay metrics live."
  (let* ((body `((:model . ,deslop-model)
                 (:messages . ,(mapcar (lambda (m)
                                         `((:role . ,(car m))
                                           (:content . ,(cdr m))))
                                       messages))
                 (:temperature . ,deslop-temperature)
                 (:thinking_budget_tokens . 1000)
                 (:reasoning_effort . "high")
                 (:stream . t)
                 (:stream_options . ((:include_usage . t)))))
         (json-data (encode-coding-string (json-encode body) 'utf-8))
         (accumulated-content "")
         (proc-buffer (generate-new-buffer " *deslop-curl*"))
         (target-ov deslop--current-job-overlay))
    (make-process
     :name "deslop-curl"
     :buffer proc-buffer
     :command (list "curl" "-s" "-N"
                    "-X" "POST" deslop-url
                    "-H" "Content-Type: application/json"
                    "-d" json-data)
     :filter
     (lambda (_proc string)
       (when (buffer-live-p proc-buffer)
         (with-current-buffer proc-buffer
           (goto-char (point-max))
           (insert string)
           (goto-char (point-min))
           (while (re-search-forward "^data: \\(.*\\)\r?\n" nil t)
             (let ((raw (match-string 1)))
               (delete-region (point-min) (match-end 0))
               (unless (string= (string-trim raw) "[DONE]")
                 (ignore-errors
                   (let* ((json (json-read-from-string raw))
                          (choices (alist-get 'choices json))
                          (delta (and choices (> (length choices) 0)
                                      (alist-get 'delta (aref choices 0))))
                          (usage (alist-get 'usage json))
                          (reasoning (or (alist-get 'reasoning_content delta)
                                         (alist-get 'reasoning delta)))
                          (content (alist-get 'content delta)))
                     
                     (when (overlayp target-ov)
                       (when usage
                         (let ((total-tok (alist-get 'completion_tokens usage)))
                           (when total-tok (overlay-put target-ov 'tokens total-tok))))
                       
                       (when (and reasoning (> (length reasoning) 0))
                         (overlay-put target-ov 'status "Thinking")
                         (overlay-put target-ov 'tokens (1+ (or (overlay-get target-ov 'tokens) 0))))
                       
                       (when (and content (> (length content) 0))
                         (overlay-put target-ov 'status "Generating")
                         (overlay-put target-ov 'tokens (1+ (or (overlay-get target-ov 'tokens) 0)))
                         (setq accumulated-content (concat accumulated-content content))))))))))))
     :sentinel
     (lambda (_proc event)
       (when (buffer-live-p proc-buffer)
         (kill-buffer proc-buffer))
       (if (string-match-p "finished" event)
           (funcall done accumulated-content nil)
         (funcall done nil (format "Process error: %s" event)))))))

(defun deslop-abort-queue ()
  "Abort all pending deslop jobs and clear active overlays."
  (interactive)
  (dolist (ov deslop--managed-overlays)
    (when (overlayp ov) (delete-overlay ov)))
  (setq deslop--managed-overlays nil)
  (deslop--stop-spinner)
  (setq deslop-queue nil)
  (setq deslop--current-job nil)
  (message "deslop: queue aborted"))

(defconst deslop-insert-system
  "You are an inline code and text completion engine. You will receive a document containing a <CURSOR> marker indicating where your output will be inserted.

### Output Constraints
- Output ONLY the exact text to be inserted at <CURSOR>. 
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
TODO: 

Input:
// <CURSOR>
// next line
Output:
first line

Input:
def calculate_total(price, tax):
    <CURSOR>
Output:
return price * (1 + tax)")

(defvar deslop-mode-map
  (let* ((map (make-sparse-keymap))
         (prefix (make-sparse-keymap)))
    (define-key prefix "i" #'deslop-insert-at-cursor)
    (define-key prefix "a" #'deslop-abort-queue)
    (define-key map (kbd "C-c e") prefix)
    map)
  "Prefix map for deslop-mode.")

(define-minor-mode deslop-mode
  "Minor mode for queueing local LLM requests."
  :lighter " deslop"
  :keymap deslop-mode-map)

(provide 'deslop)

