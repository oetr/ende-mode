;; -*- lexical-binding: t; -*-

(defgroup deslop nil
  "Local-LLM text assistance: insert at cursor, improve, summarize."
  :prefix "deslop-"
  :group 'convenience)

(defvar deslop-queue nil
  "Queue of pending jobs.  Each job is (TYPE . PROPERTIES).")

(defvar deslop-running nil
  "Non-nil while a job is being processed.")

(defvar deslop--active-overlay nil
  "Active visual indicator overlay for the currently processing job.")

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

;; Tell me a joke: Why did the Lisp programmer break up with their partner? They had no cons.

(defun deslop--create-overlay (buf beg end label)
  "Create indicator overlay in BUF from BEG to END with LABEL."
  (when deslop--active-overlay
    (delete-overlay deslop--active-overlay))
  (with-current-buffer buf
    (setq deslop--active-overlay (make-overlay beg (max beg end)))
    (overlay-put deslop--active-overlay 'face 'deslop-working-face)
    (overlay-put deslop--active-overlay 'before-string
                 (propertize (format " [%s ⏳] " label)
                             'face '(:foreground "yellow" :weight bold)))))

(defun deslop--clear-overlay ()
  "Remove active visual indicator overlay."
  (when (overlayp deslop--active-overlay)
    (delete-overlay deslop--active-overlay)
    (setq deslop--active-overlay nil)))

(defun deslop--pick-closest (candidates)
  "CANDIDATES is a list of (POS . DIST).
Return the POS with the smallest DIST, or nil."
  (let* ((best nil))
    (dolist (c candidates)
      (when (or (null best) (< (cdr c) (cdr best)))
        (setq best c)))
    (and best (car best))))

(defun deslop--find-string (text start)
  "Case-sensitively find TEXT in the current buffer at or after START.
Return the position where the occurrence starts, or nil.
This Emacs build's search primitives ignore case unconditionally,
so the comparison is done explicitly."
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
  "Find the occurrence of TEXT closest to ORIG-POS in the current buffer
(case-sensitively).  Return the start position, or nil if TEXT is not found."
  (let* ((cands nil)
         (s (deslop--find-string text (point-min))))
    (while s
      (push (cons s (abs (- s orig-pos))) cands)
      (setq s (deslop--find-string text (1+ s))))
    (deslop--pick-closest cands)))

(defun deslop--find-insert-point (before after orig-pos)
  "Find the insertion position for a (BEFORE . AFTER) cursor anchor.
Prefer occurrences of AFTER immediately preceded by BEFORE; otherwise fall
back to the occurrence of AFTER closest to ORIG-POS.  If AFTER is empty,
use the buffer end (buffer start if BEFORE is also empty).
Return the position, or nil if AFTER is non-empty and not found."
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
  "Non-nil if C is a whitespace character (space, tab, newline, ...)."
  (memq c '(32 9 10 13 11 12)))

(defun deslop--clean (s)
  "Trim whitespace at both ends of S."
  (let* ((start 0) (end (length s)))
    (while (and (< start end) (deslop--whitespacep (aref s start)))
      (setq start (1+ start)))
    (while (and (> end start) (deslop--whitespacep (aref s (1- end))))
      (setq end (1- end)))
    (substring s start end)))

(defun deslop--trim-trailing (s)
  "Remove trailing whitespace from S, keeping leading whitespace intact."
  (let* ((end (length s)))
    (while (and (> end 0) (deslop--whitespacep (aref s (1- end))))
      (setq end (1- end)))
    (substring s 0 end)))

(defun deslop--insert-llm (buf ins-pos region context instruction)
  "Ask the LLM for insert text and apply it to BUF at INS-POS."
  (funcall deslop-llm-function
	   (let ((messages 
		  (list (cons "system" deslop--insert-system)
			(cons "user" (format "Task: %s\n\nDocument:\n%s" instruction context)))))
	     (print (format "MESSAGES: %S" messages))
	     messages)
	   (lambda (content err)
	     (print "LAMBDA deslop insert llm")
      (if (null content)
          (deslop--skip (format "insert: LLM error: %s" err))
        (let* ((text (deslop--trim-trailing content)))
          (with-current-buffer buf
            (save-excursion
              (goto-char ins-pos)
             (cond
              ((null region) (insert text))
              ((= ins-pos (car region)) (insert text "\n"))
              ((= ins-pos (cdr region)) (insert "\n" text))
              (t (insert "\n" text "\n")))))
         (message "insert: inserted %d chars at %d in %s"
                     (length text) ins-pos (buffer-name buf))
         (deslop--job-done))))))

(defun deslop-process-next ()
  "Process the next queued job if the queue is not idle."
  ;;(print deslop-queue)
  (unless (or deslop-running (null deslop-queue))
    (setq deslop-running t)
    (print (format "QUEUE BEFORE: %s" deslop-queue))
    (let* ((job (pop deslop-queue)))
      (print (format "QUEUE AFTER: %s" deslop-queue))
      ;;(deslop-log "processing: %s" (car job))
      (print (format "PROCESSING: %s" job))
      
      (condition-case err
          (deslop-run-job job)
        (error
         ;;(deslop-log "error in job %s: %s" (car job) (error-message-string err))
	 ;;(error-message-string err)
	 (message "test error in job %s: %s" (car job) (error-message-string err))
         (deslop--job-done)
	 )))))

(defun deslop-enqueue (job)
  "Add JOB to the queue and start processing if idle."
  (setq deslop-queue (nconc deslop-queue (list job)))
  (print (format "QUEUED: ---- D"))
  ;;(deslop-log "queued: %s" (car job))
  (deslop-process-next)
  )

(defun deslop--job-done ()
  "Finish the current job and continue the queue."
  (deslop--clear-overlay)
  (setq deslop-running nil)
  (deslop-process-next))

(defun deslop-run-job (job)
  "Execute JOB (async) and finish it via deslop--job-done."
  (print "RUNNING deslop run job!")
  (pcase job
    (`(insert . ,p) (deslop--run-insert p))
    ;; (`(improve . ,p) (deslop--run-improve p))
    ;; (`(summarize . ,p) (deslop--run-summarize p))
    (_ (progn
         (message "unknown job: %S" job)
         (deslop--job-done)))))

(defun deslop--skip (msg)
  "Skip the current job with a logged reason."
  ;;(deslop-log "SKIP: %s" msg)
  (message "deslop: skipped (%s)" msg)
  (deslop--job-done))

(defun deslop--marker-context (text offset)
  "Return TEXT with a <CURSOR> marker at OFFSET.
If TEXT exceeds deslop-max-context-chars, keep a window of
deslop-window-chars characters on each side of OFFSET and mark the cut
points with elision markers."
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
  (print "RUNNING deslop--run-insert")
  (let* ((buf (deslop--live-buffer p)))
    (if (null buf)
        (deslop--skip "insert: buffer no longer exists")
      (with-current-buffer buf
        (let* ((region (plist-get p :region))
              (offset (plist-get p :offset))
              (anchor (plist-get p :anchor))
              (orig (plist-get p :orig)))
          (if (plist-get p :selected)
              (let* ((pos (deslop--find-text-at anchor orig)))
                (if (null pos)
                    (deslop--skip
                     "insert: highlighted region not found (modified by an earlier job?)")
                  (let* ((len (length anchor))
                        (off (max 0 (min len offset)))
                        ins)
                    (cond
                     ((<= offset 0) (setq ins pos))
                     ((>= offset len) (setq ins (+ pos len)))
                     (t (setq ins (+ pos off))))
		    (deslop--create-overlay buf pos (+ pos len) "LLM Working")
                     (deslop--insert-llm
                      buf ins (cons pos (+ pos len))
                      (deslop--marker-context anchor off)
                      deslop-insert-instruction))))
             (let* ((pos (deslop--find-insert-point (car anchor) (cdr anchor) orig))
                    (ctx (buffer-substring-no-properties (point-min) (point-max))))
               (if (null pos)
                   (deslop--skip
                    "insert: cursor anchor not found (modified by an earlier job?)")
		 (deslop--create-overlay buf pos pos "LLM Working")
                 (deslop--insert-llm
                  buf pos nil
                  (deslop--marker-context ctx (- pos (point-min)))
                  deslop-insert-instruction)))))))))

(defun deslop-insert-at-cursor ()
  "Queue a request to insert LLM text at the cursor.
If a region is active, only that region is sent as context and the text
is inserted relative to the region (start: TEXT+newline before it, end:
newline+TEXT after it, middle: newline+TEXT+newline at the point).
Without a region, the whole buffer is sent as context and the text is
inserted raw at the cursor.  See refine.org for details."
  (interactive)
  (let* ((buf (current-buffer))
         (region (deslop--current-region))
         (pt (point)))
    (if region
        (let* ((s (car region)) (e (cdr region)))
          (deslop-enqueue
           (list 'insert :buffer buf :region (cons s e)
                 :offset (- pt s)
                 :anchor (buffer-substring-no-properties s e)
                 :orig s :selected t)))
      (let* ((from (min pt (max (point-min) (- pt deslop-anchor-chars))))
            (to (max pt (min (point-max) (+ pt deslop-anchor-chars)))))
        (deslop-enqueue
         (list 'insert :buffer buf
               :region (cons (point-min) (point-max))
               :offset (- pt (point-min))
               :anchor (cons (buffer-substring-no-properties from pt)
                             (buffer-substring-no-properties pt to))
               :orig pt :selected nil))))
    (message "deslop: queued insert-at-cursor")))

(defun deslop-improve-at-cursor ()
  "Queue a request to improve the text at the cursor or in the region.
If a region is active, only that region is sent as context and the
improved text replaces it.  Without a region, the whole buffer is sent
as context and the improved text is inserted at the cursor."
  (interactive)
  (let* ((buf (current-buffer))
         (region (deslop--current-region))
         (pt (point)))
    (if region
        (let* ((s (car region)) (e (cdr region)))
          (deslop-enqueue
           (list 'improve :buffer buf :region (cons s e)
                 :offset (- pt s)
                 :anchor (buffer-substring-no-properties s e)
                 :orig s :selected t)))
      (let* ((from (min pt (max (point-min) (- pt deslop-anchor-chars))))
            (to (max pt (min (point-max) (+ pt deslop-anchor-chars)))))
        (deslop-enqueue
         (list 'improve :buffer buf
               :region (cons (point-min) (point-max))
               :offset (- pt (point-min))
               :anchor (cons (buffer-substring-no-properties from pt)
                             (buffer-substring-no-properties pt to))
               :orig pt :selected nil))))
    (message "deslop: queued improve-at-cursor")))

(defun deslop-summarize ()
  "Queue a request to summarize the current buffer."
  (interactive)
  (let ((buf (current-buffer)))
    (deslop-enqueue
     (list 'summarize :buffer buf
           :region (cons (point-min) (point-max))
           :offset 0
           :anchor (cons "" "")
           :orig (point-min) :selected nil))
    (message "deslop: queued summarize")))

(defun deslop-show-log ()
  "Show the deslop log buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*deslop-log*")
    (switch-to-buffer (current-buffer))
    (view-mode 1)))

(defun deslop-show-queue ()
  "Show the current deslop queue."
  (interactive)
  (with-current-buffer (get-buffer-create "*deslop-queue*")
    (switch-to-buffer (current-buffer))
    (erase-buffer)
    (if (null deslop-queue)
        (insert "Queue is empty.\n")
      (dolist (job deslop-queue)
        (insert (format "%s\n" job))))
    (view-mode 1)))

(defun deslop-abort-queue ()
  "Abort all pending deslop jobs."
  (interactive)
  (setq deslop-queue nil)
  (message "deslop: queue aborted"))
;;\n
(defun deslop-log (fmt &rest args)
  "Append a formatted log message to the deslop log buffer."
  (with-current-buffer (get-buffer-create "*deslop-log*")
    (goto-char (point-max))
    (insert (format (concat fmt "\n") args))))

(defun deslop-llm-default (messages done)
  "Call the LLM asynchronously.
MESSAGES is a list of (ROLE . CONTENT) pairs.  DONE is called as
(DONE CONTENT ERR)."
  (let* ((body `((:model . ,deslop-model)
                 (:messages . ,(mapcar (lambda (m)
                                         `((:role . ,(car m))
                                           (:content . ,(cdr m))))
                                       messages))
                 (:temperature . ,deslop-temperature)
		 (:thinking_budget_tokens . 1000)
		 (:reasoning_effort . "high")
		 ))
	 (url-request-method "POST")
	 (url-request-extra-headers '(("Content-Type" . "application/json")))
	 (url-request-data (encode-coding-string (json-encode body) 'utf-8)))
    (condition-case err
        (url-retrieve
         deslop-url
         (lambda (&optional success)
           (let* ((buf (current-buffer)))
             (condition-case e
                 (let* ((resp (with-current-buffer buf
                                (goto-char (point-min))
				(re-search-forward "\r?\n\r?\n" nil t)
                                (ignore-errors (json-read))))
                       (choices (alist-get 'choices resp))
                       (msg (and choices
				 (> (length choices) 0)
                                 (aref choices 0)))
		       (message (alist-get 'message msg))
                       (content (alist-get 'content message)))
		   (print content)
                   (if (stringp content)
                       (funcall done content nil)
                     (funcall done nil
                              (format "malformed LLM response: %s"
                                      (with-current-buffer buf (buffer-string))))))
	       
               (error (message "ERROR: %s" (error-message-string e)) (funcall done nil (error-message-string e))))
             (kill-buffer buf))))
      (error
       (funcall done nil (error-message-string err))))))

;; System prompts
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
    (define-key prefix "p" #'deslop-improve-at-cursor)
    (define-key prefix "s" #'deslop-summarize)
    ;; (define-key prefix "l" #'deslop-show-log)
    ;; (define-key prefix "q" #'deslop-show-queue)
    ;; (define-key prefix "a" #'deslop-abort-queue)
    (define-key map (kbd "C-c e") prefix)
    map)
   "Prefix map for deslop-mode.
Prefix C-c e: i insert, p improve, s summarize, l log, q queue, a abort.")

(define-minor-mode deslop-mode
  "Minor mode for queueing local LLM requests.
\\{deslop-mode-map}"
  :lighter " deslop"
  :keymap deslop-mode-map)

(provide 'deslop)

;; I like to add a function to "warp" content---the user will move the cursor to the where he thinks the changes should be made, and the AI replaces a bunch of text before and after the cursor (at most N lines)
;; I want you to work out the concept here in how this should be implemented with your own words
;; Use about 10 lines of comments, don't implement anything yet:
;; ;; The idea: user parks the cursor where the edit should happen, we grab N lines
;; above and N lines below (configurable via deslop-warp-lines, default maybe 15),
;; send that whole block to the LLM with an instruction like "rewrite this section
;; to be clearer / fix the bug / restructure it", and the LLM returns a replacement
;; block.  We then delete the original N+N lines and insert the LLM's output in
;; their place.  The anchor is the full warp zone (not just a small snippet), so
;; deslop--find-text-at locates it by the first and last few lines as a fingerprint.
;; If the buffer is shorter than 2*N lines we just send the whole buffer.  The
;; key difference from insert is that we REPLACE a region rather than INSERT at a
;; point, so the job type would be (warp . PROPS) with :region set to the warp
;; boundaries and :selected t so the existing region-replacement path in
;; deslop--insert-llm handles the splice.

;; The warp approach described above has several flaws:
;; 1) it might not always be code,
;; 2) the AI should rewrite the region around cursor, but consider whole document
;; 3) marker inside the warp zone tells the LLM where the user's focus is, so the rewrite should be centered on that area rather than uniformly rewriting everything.  We'd embed <CURSOR> at the user's point within the warp block, and the system prompt would say "rewrite the text around the cursor, keeping the rest of the block intact unless it needs adjusting for coherence."
;; 4) The AI will have trouble counting the exact region, so we need to mark it again, like above as well (@LLM: please comment now. LLM-answer: We'd wrap the warp zone in explicit delimiters, e.g. <WARP> ... </WARP>, so the LLM sees exactly which text to produce as the replacement.  The system prompt would say: "The text between <WARP> and </WARP> is the region to rewrite.  Return ONLY the replacement text for that region---no delimiters, no preamble.  Use the <CURSOR> marker inside the region to understand where the user's focus is, and center your rewrite there while keeping surrounding lines coherent."  On the Emacs side, after the LLM responds we simply delete the original warp zone (we already know its start/end from the plist) and insert the response.  No fuzzy matching needed for the splice itself---the region boundaries are exact positions we computed at enqueue time.  The anchor/fingerprint is only needed to re-locate the zone if an earlier job shifted the buffer, same as insert.  The full document is still sent as context (via deslop--marker-context) so the LLM has global awareness, but the <WARP> block is what it actually rewrites.  If the buffer is shorter than 2*N lines the warp zone is just the whole buffer and the markers still work.  One more detail: we should strip a single leading/trailing newline from the LLM response before splicing, since models tend to add one, and we don't want to introduce blank lines at the seam.
