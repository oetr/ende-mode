;;; enslop.el --- minor mode for local LLM text assistance

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Version: 0.1
;; Keywords: convenience, tools, lisp
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; enslop is a minor mode that queues requests to a local LLM served by
;; any OpenAI-compatible chat completions endpoint (LM Studio, llama.cpp
;; server, Ollama):
;;
;;   C-c e i - enslop-insert-at-cursor : insert LLM text at the cursor
;;   C-c e p - enslop-improve-at-cursor: improve region or whole buffer
;;   C-c e s - enslop-summarize        : summarize region or whole buffer
;;   C-c e l - show the request log
;;   C-c e q - show queue status
;;   C-c e a - clear the queue
;;
;; See refine.org for the full specification of queue semantics, anchor
;; resolution and context management.

;;; Code:

(defgroup enslop nil
  "Local-LLM text assistance: insert at cursor, improve, summarize."
  :prefix "enslop-"
  :group 'convenience)

;;;; User options

(defcustom enslop-url "http://localhost:8080/v1/chat/completions"
  "OpenAI-compatible chat completions endpoint."
  :type 'string
  :group 'enslop)

(defcustom enslop-model "qwen3.8-27b"
  "Model name sent to the LLM server."
  :type 'string
  :group 'enslop)

(defcustom enslop-temperature 0.2
  "Sampling temperature for LLM requests."
  :type 'number
  :group 'enslop)

(defcustom enslop-max-context-chars 24000
  "Maximum number of characters sent to the LLM as context.
Tokens are estimated as chars/4."
  :type 'integer
  :group 'enslop)

(defcustom enslop-window-chars 8000
  "Characters kept on each side of the cursor when context is elided."
  :type 'integer
  :group 'enslop)

(defcustom enslop-anchor-chars 120
  "Characters captured on each side of the cursor as an insert anchor."
  :type 'integer
  :group 'enslop)

(defcustom enslop-summary-chars 4000
  "Approximate target length in characters for generated summaries."
  :type 'integer
  :group 'enslop)

(defcustom enslop-insert-instruction
  "Write the most natural continuation of the text at the cursor position."
  "Instruction given to the LLM for enslop-insert-at-cursor."
  :type 'string
  :group 'enslop)

;;;; Internals

(defvar enslop-queue nil
  "Queue of pending jobs.  Each job is (TYPE . PROPERTIES).")

(defvar enslop-running nil
  "Non-nil while a job is being processed.")

(defvar enslop-log-buffer-name "*enslop-log*")

(defvar enslop-llm-function #'enslop-llm-default
  "Function used to talk to the LLM.
Called as (FN MESSAGES DONE).  MESSAGES is a list of (ROLE . CONTENT)
pairs.  DONE is called as (DONE CONTENT ERR): CONTENT is the response
string on success, ERR describes the problem on failure.")

;;;; Logging

(defun enslop-log (format &rest args)
  "Append a timestamped message to the enslop log buffer."
  (with-current-buffer (get-buffer-create enslop-log-buffer-name)
    (goto-char (point-max))
    (insert (format-time-string "[%Y-%m-%d %H:%M:%S] ")
            (apply #'format format args)
            "\n")))

;;;; LLM client

(defconst enslop--insert-system
  "You are an expert writing assistant embedded in a text editor. You receive a document with a <CURSOR> marker indicating where text will be inserted. Respond with ONLY the text to insert at that position. No explanations, no quotes, no markdown code fences, no preamble.")

(defconst enslop--improve-system
  "You are an expert text improver embedded in a text editor. Apply the user's instructions to the marked region. Respond in EXACTLY one of these two formats, with no other text:\n<<<REGION>>>\n<improved text that replaces the marked region>\n<<<BUFFER>>>\n<improved entire document>\nUse <<<REGION>>> unless the instructions require changes outside the region.")

(defun enslop-llm-default (messages done)
  "Call the LLM asynchronously.
MESSAGES is a list of (ROLE . CONTENT) pairs.  DONE is called as
(DONE CONTENT ERR)."
  (let* ((body (json-encode
               `((:model . ,enslop-model)
                 (:messages . ,(mapcar (lambda (m)
                                         `((:role . ,(car m))
                                           (:content . ,(cdr m))))
                                       messages))
                 (:temperature . ,enslop-temperature)))))
    (print (format "HERE1"))
    (condition-case err
        (url-retrieve
         enslop-url
         (lambda (&optional success)
           (let* ((buf (current-buffer)))
	     (print (format "HERE2"))
             (condition-case e
                 (let* ((resp (with-current-buffer buf
                                (goto-char (url-retrieve-start))
                                (ignore-errors (json-read))))
                       (choices (and (listp resp) (gethash "choices" resp)))
                       (msg (and (listp choices) (>= (length choices) 1)
                                 (gethash "message" (elt choices 0))))
                       (content (and (listp msg) (gethash "content" msg))))
		   (print (format "HERE3"))
                   (if (stringp content)
                       (funcall #'done content nil)
                     (funcall #'done nil
                              (format "malformed LLM response: %s"
                                      (with-current-buffer buf (buffer-string))))))
               (error (funcall #'done nil (error-message-string e))))
             (kill-buffer buf))))
      (error
       (funcall #'done nil (error-message-string err))))))

;;;; Queue

(defun enslop-enqueue (job)
  "Add JOB to the queue and start processing if idle."
  (setq enslop-queue (nconc enslop-queue (list job)))
  (enslop-log "queued: %s" (car job))
  (enslop-process-next))

(defun enslop-process-next ()
  "Process the next queued job if the queue is not idle."
  (unless (or enslop-running (null enslop-queue))
    (setq enslop-running t)
    (let* ((job (pop enslop-queue)))
      (enslop-log "processing: %s" (car job))
      (condition-case err
          (enslop-run-job job)
        (error
         (enslop-log "error in job %s: %s" (car job) (error-message-string err))
         (enslop--job-done))))))

(defun enslop--job-done ()
  "Finish the current job and continue the queue."
  (setq enslop-running nil)
  (enslop-process-next))

(defun enslop-run-job (job)
  "Execute JOB (async) and finish it via enslop--job-done."
  (pcase job
    (`(insert . ,p) (enslop--run-insert p))
    (`(improve . ,p) (enslop--run-improve p))
    (`(summarize . ,p) (enslop--run-summarize p))
    (_ (progn
         (enslop-log "unknown job: %S" job)
         (enslop--job-done)))))

(defun enslop--skip (msg)
  "Skip the current job with a logged reason."
  (enslop-log "SKIP: %s" msg)
  (message "enslop: skipped (%s)" msg)
  (enslop--job-done))

(defun enslop--live-buffer (p)
  "Return the job's buffer if it is still alive, else nil."
  (let* ((buf (plist-get p :buffer)))
    (and (buffer-live-p buf) buf)))

;;;; Anchor resolution

(defun enslop--current-region ()
  "Return (START . END) of the active region, or nil."
  (when (use-region-p)
    (cons (region-beginning) (region-end))))

(defun enslop--pick-closest (candidates)
  "CANDIDATES is a list of (POS . DIST).
Return the POS with the smallest DIST, or nil."
  (let* ((best nil))
    (dolist (c candidates)
      (when (or (null best) (< (cdr c) (cdr best)))
        (setq best c)))
    (and best (car best))))

(defun enslop--find-string (text start)
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

(defun enslop--find-text-at (text orig-pos)
  "Find the occurrence of TEXT closest to ORIG-POS in the current buffer
(case-sensitively).  Return the start position, or nil if TEXT is not found."
  (let* ((cands nil)
         (s (enslop--find-string text (point-min))))
    (while s
      (push (cons s (abs (- s orig-pos))) cands)
      (setq s (enslop--find-string text (1+ s))))
    (enslop--pick-closest cands)))

(defun enslop--find-insert-point (before after orig-pos)
  "Find the insertion position for a (BEFORE . AFTER) cursor anchor.
Prefer occurrences of AFTER immediately preceded by BEFORE; otherwise fall
back to the occurrence of AFTER closest to ORIG-POS.  If AFTER is empty,
use the buffer end (buffer start if BEFORE is also empty).
Return the position, or nil if AFTER is non-empty and not found."
  (if (string= after "")
      (if (string= before "") (point-min) (point-max))
    (let* ((exact nil) (any nil)
           (s (enslop--find-string after (point-min))))
      (while s
        (let* ((d (abs (- s orig-pos)))
               (ok (or (string= before "")
                       (and (>= s (+ (point-min) (length before)))
                            (string= before
                                     (buffer-substring-no-properties
                                      (- s (length before)) s))))))
          (push (cons s d) any)
          (when ok (push (cons s d) exact)))
        (setq s (enslop--find-string after (1+ s))))
      (let* ((r (enslop--pick-closest exact)))
        (or r (enslop--pick-closest any))))))

;;;; Context building

(defun enslop--whitespacep (c)
  "Non-nil if C is a whitespace character (space, tab, newline, ...)."
  (memq c '(32 9 10 13 11 12)))

(defun enslop--clean (s)
  "Trim whitespace at both ends of S."
  (let* ((start 0) (end (length s)))
    (while (and (< start end) (enslop--whitespacep (aref s start)))
      (setq start (1+ start)))
    (while (and (> end start) (enslop--whitespacep (aref s (1- end))))
      (setq end (1- end)))
    (substring s start end)))

(defun enslop--trim-trailing (s)
  "Remove trailing whitespace from S, keeping leading whitespace intact."
  (let* ((end (length s)))
    (while (and (> end 0) (enslop--whitespacep (aref s (1- end))))
      (setq end (1- end)))
    (substring s 0 end)))

(defun enslop--marker-context (text offset)
  "Return TEXT with a <CURSOR> marker at OFFSET.
If TEXT exceeds enslop-max-context-chars, keep a window of
enslop-window-chars characters on each side of OFFSET and mark the cut
points with elision markers."
  (setq offset (max 0 (min (length text) offset)))
  (if (<= (length text) enslop-max-context-chars)
      (concat (substring text 0 offset) "<CURSOR>" (substring text offset))
     (let* ((w enslop-window-chars)
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

(defun enslop--chunk-split (text size)
  "Split TEXT into consecutive chunks of at most SIZE characters."
  (let* ((chunks nil) (i 0) (n (length text)))
    (while (< i n)
      (push (substring text i (min n (+ i size))) chunks)
      (setq i (min n (+ i size))))
    (nreverse chunks)))

;;;; Job: insert at cursor

(defun enslop-insert-at-cursor ()
  "Queue a request to insert LLM text at the cursor.
If a region is active, only that region is sent as context and the text
is inserted relative to the region (start: TEXT+newline before it, end:
newline+TEXT after it, middle: newline+TEXT+newline at the point).
Without a region, the whole buffer is sent as context and the text is
inserted raw at the cursor.  See refine.org for details."
  (interactive)
  (let* ((buf (current-buffer))
         (region (enslop--current-region))
         (pt (point)))
    (if region
        (let* ((s (car region)) (e (cdr region)))
          (enslop-enqueue
           (list 'insert :buffer buf :region (cons s e)
                 :offset (- pt s)
                 :anchor (buffer-substring-no-properties s e)
                 :orig s :selected t)))
      (let* ((from (min pt (max (point-min) (- pt enslop-anchor-chars))))
            (to (max pt (min (point-max) (+ pt enslop-anchor-chars)))))
        (enslop-enqueue
         (list 'insert :buffer buf
               :region (cons (point-min) (point-max))
               :offset (- pt (point-min))
               :anchor (cons (buffer-substring-no-properties from pt)
                             (buffer-substring-no-properties pt to))
               :orig pt :selected nil))))
    (message "enslop: queued insert-at-cursor")))

(defun enslop--run-insert (p)
  "Execute an insert job."
  (let* ((buf (enslop--live-buffer p)))
    (if (null buf)
        (enslop--skip "insert: buffer no longer exists")
      (with-current-buffer buf
        (let* ((region (plist-get p :region))
              (offset (plist-get p :offset))
              (anchor (plist-get p :anchor))
              (orig (plist-get p :orig)))
          (if (plist-get p :selected)
              (let* ((pos (enslop--find-text-at anchor orig)))
                (if (null pos)
                    (enslop--skip
                     "insert: highlighted region not found (modified by an earlier job?)")
                  (let* ((len (length anchor))
                        (off (max 0 (min len offset)))
                        ins)
                    (cond
                     ((<= offset 0) (setq ins pos))
                     ((>= offset len) (setq ins (+ pos len)))
                     (t (setq ins (+ pos off))))
                     (enslop--insert-llm
                      buf ins (cons pos (+ pos len))
                      (enslop--marker-context anchor off)
                      enslop-insert-instruction))))
             (let* ((pos (enslop--find-insert-point (car anchor) (cdr anchor) orig))
                    (ctx (buffer-substring-no-properties (point-min) (point-max))))
               (if (null pos)
                   (enslop--skip
                    "insert: cursor anchor not found (modified by an earlier job?)")
                 (enslop--insert-llm
                  buf pos nil
                  (enslop--marker-context ctx (- pos (point-min)))
                  enslop-insert-instruction)))))))))

(defun enslop--insert-llm (buf ins-pos region context instruction)
  "Ask the LLM for insert text and apply it to BUF at INS-POS."
  (funcall enslop-llm-function
	   (let ((messages 
		  (list (cons "system" enslop--insert-system)
			(cons "user" (format "Task: %s\n\nDocument:\n%s" instruction context)))))
	     (print (format "MESSAGES: %S" messages))
	     messages)
   (lambda (content err)
      (if (null content)
          (enslop--skip (format "insert: LLM error: %s" err))
        (let* ((text (enslop--trim-trailing content)))
          (with-current-buffer buf
            (save-excursion
              (goto-char ins-pos)
             (cond
              ((null region) (insert text))
              ((= ins-pos (car region)) (insert text "\n"))
              ((= ins-pos (cdr region)) (insert "\n" text))
              (t (insert "\n" text "\n")))))
         (enslop-log "insert: inserted %d chars at %d in %s"
                     (length text) ins-pos (buffer-name buf))
         (enslop--job-done))))))

;;;; Job: improve

(defun enslop-improve-at-cursor ()
  "Queue a request to improve the active region (or the whole buffer).
The user is asked for instructions in the *enslop-prompt* side window.
The LLM may either replace the region or rewrite the whole buffer; see
refine.org for the response protocol."
  (interactive)
  (let* ((buf (current-buffer))
         (region (enslop--current-region))
         (r (or region (cons (point-min) (point-max))))
         (region-text (buffer-substring-no-properties (car r) (cdr r))))
    (enslop-prompt
     (lambda (instructions)
       (enslop-enqueue
        (list 'improve :buffer buf :region r
              :region-text region-text
              :instructions instructions
              :whole (null region)))
       (message "enslop: queued improve")))))

(defun enslop-queue-improve (buf region instructions)
  "Queue an improve job for REGION (a cons of start and end) in BUF.
Programmatic entry point (also used by tests); the region is captured
now, at call time."
  (enslop-enqueue
   (list 'improve :buffer buf :region region
         :region-text (with-current-buffer buf
                        (buffer-substring-no-properties (car region) (cdr region)))
         :instructions instructions
         :whole (and (= (car region) (with-current-buffer buf (point-min)))
                     (= (cdr region) (with-current-buffer buf (point-max)))))))

(defun enslop--run-improve (p)
  "Execute an improve job."
  (let* ((buf (enslop--live-buffer p)))
    (if (null buf)
        (enslop--skip "improve: buffer no longer exists")
      (with-current-buffer buf
        (let* ((region (plist-get p :region))
              (s (car region)) (e (cdr region))
              (anchor (plist-get p :region-text))
              (instructions (plist-get p :instructions)))
          (cond
           ((plist-get p :whole)
            (enslop--improve-range buf (point-min) (point-max) instructions))
           ((string= anchor "")
            (enslop--skip "improve: empty region"))
           (t
            (let* ((pos (enslop--find-text-at anchor s)))
              (if (null pos)
                  (enslop--skip
                   "improve: region not found (modified by an earlier job?)")
                (enslop--improve-range buf pos (+ pos (length anchor))
                                       instructions))))))))))

(defun enslop--improve-range (buf s e instructions)
  "Improve BUF's range S..E according to INSTRUCTIONS."
  (let* ((text (buffer-substring-no-properties s e))
         (buf-size (buffer-size buf)))
    (cond
      ((> (length text) enslop-max-context-chars)
       (enslop--improve-chunk-loop
        buf s e (enslop--chunk-split text enslop-max-context-chars)
        nil instructions))
     ((<= buf-size enslop-max-context-chars)
      (funcall enslop-llm-function
       (enslop--improve-whole-messages buf s e instructions)
       (lambda (content err)
         (if (null content)
             (enslop--skip (format "improve: LLM error: %s" err))
           (let* ((parsed (enslop-parse-improve (enslop--clean content))))
             (if (eq (car parsed) 'buffer)
                 (progn
                   (enslop--replace-buffer buf (cdr parsed))
                   (enslop-log "improve: replaced whole buffer of %s"
                               (buffer-name buf)))
               (enslop--replace-range buf s e (cdr parsed))
               (enslop-log "improve: replaced region in %s"
                           (buffer-name buf)))
             (enslop--job-done))))))
     (t
      (funcall enslop-llm-function
       (enslop--improve-local-messages buf s e instructions)
       (lambda (content err)
         (if (null content)
             (enslop--skip (format "improve: LLM error: %s" err))
           (let* ((parsed (enslop-parse-improve (enslop--clean content))))
             (if (eq (car parsed) 'buffer)
                 (progn
                   (enslop--replace-buffer buf (cdr parsed))
                   (enslop-log "improve: LLM chose full-buffer rewrite (best effort) in %s"
                               (buffer-name buf)))
               (enslop--replace-range buf s e (cdr parsed))
               (enslop-log "improve: replaced region in %s" (buffer-name buf)))
             (enslop--job-done)))))))))

(defun enslop--improve-chunk-loop (buf s e chunks results instructions)
  "Improve CHUNKS sequentially; RESULTS collects improved chunks (last first).
When all chunks are done, S..E is replaced once with their concatenation,
so intermediate length changes never shift pending chunk ranges."
  (if (null chunks)
      (progn
        (enslop--replace-range buf s e (apply #'concat (nreverse results)))
        (enslop-log "improve: all chunks done in %s" (buffer-name buf))
        (enslop--job-done))
    (let* ((chunk (car chunks)))
      (funcall enslop-llm-function
       (enslop--improve-chunk-messages chunk instructions)
       (lambda (content err)
         (if (null content)
             (enslop--skip
              (format "improve: chunk failed: %s (remaining chunks aborted)" err))
           (let* ((parsed (enslop-parse-improve (enslop--clean content))))
             (if (eq (car parsed) 'buffer)
                 (progn
                   (enslop--replace-buffer buf (cdr parsed))
                   (enslop-log "improve: LLM chose full-buffer rewrite, chunking aborted")
                   (enslop--job-done))
               (enslop--improve-chunk-loop
                buf s e (cdr chunks)
                (nconc (list (cdr parsed)) results)
                instructions)))))))))

(defun enslop--improve-whole-messages (buf s e instructions)
  "Messages for improving a region when the whole buffer fits in context."
  (let* ((whole (buffer-substring-no-properties (point-min) (point-max)))
         (s0 (- s (point-min)))
         (e0 (- e (point-min)))
         (ctx (concat (substring whole 0 s0)
                      "<region>"
                      (substring whole s0 e0)
                      "</region>"
                      (substring whole e0))))
    (list (cons "system" enslop--improve-system)
          (cons "user" (format "Instructions: %s\n\nDocument:\n%s"
                               instructions ctx)))))

(defun enslop--improve-local-messages (buf s e instructions)
  "Messages for improving a region when only the region fits in context."
  (list (cons "system"
              (concat enslop--improve-system
                      "\nThe document is too large to send in full, so only the region and a little surrounding context are provided. You MUST respond with <<<REGION>>> only."))
        (cons "user"
              (format "Instructions: %s\n\n%s<region>\n%s\n</region>%s"
                      instructions
                      (buffer-substring-no-properties (max (point-min) (- s 500)) s)
                      (buffer-substring-no-properties s e)
                      (buffer-substring-no-properties e (min (point-max) (+ e 500)))))))

(defun enslop--improve-chunk-messages (chunk instructions)
  "Messages for improving one chunk of a too-large region."
  (list (cons "system"
              (concat enslop--improve-system
                      "\nThe region is too large to send in full, so it is processed chunk by chunk. You MUST respond with <<<REGION>>> containing the improved chunk only."))
        (cons "user"
              (format "Instructions: %s\n\n<region-chunk>\n%s\n</region-chunk>"
                      instructions chunk))))

(defun enslop-parse-improve (text)
  "Parse an improve response into (TYPE . CONTENT).
TYPE is region or buffer; untagged replies are treated as region."
  (let* ((str (enslop--clean text)))
    (cond
     ((string-prefix-p "<<<BUFFER>>>" str)
      (cons 'buffer (enslop--clean (substring str (length "<<<BUFFER>>>")))))
     ((string-prefix-p "<<<REGION>>>" str)
      (cons 'region (enslop--clean (substring str (length "<<<REGION>>>")))))
     (t (cons 'region str)))))

(defun enslop--replace-range (buf s e text)
  "Replace BUF's range S..E with TEXT."
  (with-current-buffer buf
    (delete-region s e)
    (goto-char s)
    (insert text)))

(defun enslop--replace-buffer (buf text)
  "Replace the entire content of BUF with TEXT."
  (with-current-buffer buf
    (delete-region (point-min) (point-max))
    (insert text)))

;;;; Job: summarize

(defun enslop-summarize ()
  "Queue a request to summarize the active region (or the whole buffer).
The summary is appended to the *enslop-summary* buffer; the source
buffer is never modified."
  (interactive)
  (let* ((buf (current-buffer))
         (region (enslop--current-region))
         (r (or region (cons (point-min) (point-max))))
         (region-text (buffer-substring-no-properties (car r) (cdr r))))
    (enslop-enqueue
     (list 'summarize :buffer buf :region r
           :region-text region-text
           :whole (null region)))
    (message "enslop: queued summarize")))

(defun enslop--run-summarize (p)
  "Execute a summarize job."
  (let* ((buf (enslop--live-buffer p)))
    (if (null buf)
        (enslop--skip "summarize: buffer no longer exists")
      (with-current-buffer buf
        (let* ((region (plist-get p :region))
              (s (car region)) (e (cdr region))
              (anchor (plist-get p :region-text)))
          (cond
           ((plist-get p :whole)
            (enslop--summarize-range buf (point-min) (point-max) "whole buffer"))
           ((string= anchor "")
            (enslop--skip "summarize: empty region"))
           (t
            (let* ((pos (enslop--find-text-at anchor s)))
              (if (null pos)
                  (enslop--skip
                   "summarize: region not found (modified by an earlier job?)")
                (enslop--summarize-range buf pos (+ pos (length anchor))
                                         "region"))))))))))

(defun enslop--summarize-range (buf s e label)
  "Summarize BUF's range S..E and record the result."
  (let* ((text (buffer-substring-no-properties s e))
         (name (buffer-name buf)))
    (if (<= (length text) enslop-max-context-chars)
        (funcall enslop-llm-function
         (enslop--summarize-messages text nil)
         (lambda (content err)
           (if (null content)
               (enslop--skip (format "summarize: LLM error: %s" err))
             (enslop--record-summary name label (enslop--clean content))
             (enslop--job-done))))
      (enslop--summarize-chunk-loop name label
                                    (enslop--chunk-split text enslop-max-context-chars)
                                    nil))))

(defun enslop--summarize-chunk-loop (name label chunks running)
  "Sequentially summarize CHUNKS, carrying RUNNING as the running summary."
  (if (null chunks)
      (progn
        (enslop--record-summary name label (enslop--clean running))
        (enslop--job-done))
    (let* ((chunk (car chunks)))
      (funcall enslop-llm-function
       (enslop--summarize-messages chunk running)
       (lambda (content err)
         (if (null content)
             (progn
               (enslop-log "summarize: chunk failed (%s), continuing with running summary" err)
               (enslop--summarize-chunk-loop name label (cdr chunks) running))
           (enslop--summarize-chunk-loop name label (cdr chunks) content)))))))

(defun enslop--summarize-messages (chunk running)
  "Messages for summarizing CHUNK, given the previous RUNNING summary (or nil)."
  (if (null running)
      (list (cons "system"
                  "You are a summarization assistant. Summarize the given content faithfully and concisely. Respond with only the summary, no preamble, no quotes.")
            (cons "user"
                  (format "Summarize the following content (target length about %d characters):\n\n%s"
                          enslop-summary-chars chunk)))
    (list (cons "system"
                "You are a summarization assistant maintaining a running summary over a stream of content chunks. Given the previous summary and the next chunk, produce an updated summary that faithfully covers everything seen so far. Respond with only the updated summary.")
          (cons "user"
                (format "Previous summary:\n%s\n\nNext content:\n%s\n\nUpdated summary:"
                        running chunk)))))

(defun enslop--record-summary (name label summary)
  "Append SUMMARY for buffer NAME (LABEL) to *enslop-summary* and show it."
  (with-current-buffer (get-buffer-create "*enslop-summary*")
    (goto-char (point-max))
    (insert (format "=== %s (%s) @ %s ===\n%s\n\n"
                    name label (format-time-string "%Y-%m-%d %H:%M:%S") summary)))
  (enslop-log "summarize: recorded summary for %s (%s)" name label)
  (condition-case nil
      (display-buffer (get-buffer "*enslop-summary*")
                      '((display-buffer-in-side-window . (side-position right window-width 0.4))))
    (error nil)))

;;;; Prompt window

(defvar enslop-prompt-mode-map
  (let* ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'enslop-prompt-submit)
    (define-key map (kbd "C-c C-k") #'enslop-prompt-cancel)
    map)
  "Keymap for the *enslop-prompt* buffer.")

(defvar enslop-prompt-continue nil)
(make-variable-buffer-local 'enslop-prompt-continue)

(defvar enslop-prompt-prev-window nil)
(make-variable-buffer-local 'enslop-prompt-prev-window)

(defvar enslop-prompt-header-len 0)
(make-variable-buffer-local 'enslop-prompt-header-len)

(defun enslop-prompt-mode ()
  "Major mode for the *enslop-prompt* buffer."
  (kill-all-local-variables)
  (use-local-map enslop-prompt-mode-map)
  (setq major-mode 'enslop-prompt-mode)
  (setq mode-name "enslop-prompt"))

(defun enslop-prompt (continue-fn)
  "Open the prompt window and run CONTINUE-FN with the entered instructions.
If *enslop-prompt* already holds a draft, it is reused as-is."
  (let* ((prev (ignore-errors (selected-window)))
        (header "How should the text be improved?\n(C-c C-c) submit, (C-c C-k) cancel\n\n")
        (buf (get-buffer-create "*enslop-prompt*")))
    (with-current-buffer buf
      (enslop-prompt-mode)
      (setq-local enslop-prompt-continue continue-fn)
      (setq-local enslop-prompt-prev-window prev)
      (unless (buffer-modified-p)
        (erase-buffer)
        (insert header)
        (setq-local enslop-prompt-header-len (length header))
        (goto-char (point-max))))
    (when prev
      (let* ((win (condition-case nil
                     (display-buffer buf
                                     '((display-buffer-in-side-window . (side-position right window-width 0.35))))
                   (error nil))))
        (when win (select-window win))))))

(defun enslop-prompt-submit ()
  "Submit the prompt: queue the improve job and close the prompt window."
  (interactive)
  ;; NB: the local buffer var is named pb (not buf) so that user
  ;; continue-functions called below never see a dead buffer through
  ;; dynamic lookup.
  (let* ((pb (current-buffer))
         (text (buffer-string))
         (prev (buffer-local-value 'enslop-prompt-prev-window pb))
         (cont (buffer-local-value 'enslop-prompt-continue pb))
         (win (ignore-errors (selected-window)))
         (instructions (enslop--clean
                        (if (> (length text) enslop-prompt-header-len)
                            (substring text enslop-prompt-header-len)
                          ""))))
    (if (string= instructions "")
        (message "enslop: nothing to submit")
      (when (and prev (window-live-p prev)) (select-window prev))
      (when (and (window-live-p win) (> (length (window-list)) 1))
        (delete-window win))
      (kill-buffer pb)
      (funcall cont instructions))))

(defun enslop-prompt-cancel ()
  "Cancel the prompt: close the prompt window without queueing anything."
  (interactive)
  (let* ((pb (current-buffer))
         (prev (buffer-local-value 'enslop-prompt-prev-window pb))
         (win (ignore-errors (selected-window))))
    (when (and prev (window-live-p prev)) (select-window prev))
    (when (and (window-live-p win) (> (length (window-list)) 1))
      (delete-window win))
    (kill-buffer pb)
    (message "enslop: cancelled")))

;;;; Mode

(defvar enslop-mode-map
  (let* ((map (make-sparse-keymap))
        (prefix (make-sparse-keymap)))
    (define-key prefix "i" #'enslop-insert-at-cursor)
    (define-key prefix "p" #'enslop-improve-at-cursor)
    (define-key prefix "s" #'enslop-summarize)
    (define-key prefix "l" #'enslop-show-log)
    (define-key prefix "q" #'enslop-show-queue)
    (define-key prefix "a" #'enslop-abort-queue)
    (define-key map (kbd "C-c e") prefix)
    map)
   "Prefix map for enslop-mode.
Prefix C-c e: i insert, p improve, s summarize, l log, q queue, a abort.")

(define-minor-mode enslop-mode
  "Minor mode for queueing local LLM requests.
\\{enslop-mode-map}"
  :lighter " enslop"
  :keymap enslop-mode-map)

(define-globalized-minor-mode global-enslop-mode enslop-mode enslop-on-all)

;;;; Utility commands

(defun enslop-show-log ()
  "Show the enslop request log."
  (interactive)
  (let* ((buf (get-buffer-create enslop-log-buffer-name)))
    (switch-to-buffer buf)))

(defun enslop-show-queue ()
  "Show the current queue status."
  (interactive)
  (message "enslop: %d job(s) queued, %s"
           (length enslop-queue)
           (if enslop-running "processing" "idle")))

(defun enslop-abort-queue ()
  "Clear the queue.  A job already in flight finishes and is applied."
  (interactive)
  (setq enslop-queue nil)
  (message "enslop: queue cleared"))

(provide 'enslop)

;;; enslop.el ends here
