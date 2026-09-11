;;; deslop-test.el --- ERT tests for deslop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools, llm

;;; Commentary:

;; ERT coverage for synchronous operations, asynchronous lifecycle behavior,
;; target conflicts, and the incremental streaming parsers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'deslop)

;;;; Test harness

(defvar deslop-test--prompts nil)
(defvar deslop-test--calls nil)
(defvar deslop-test--fake-responses nil)

(defun deslop-test--callback (call name &rest args)
  "Invoke callback NAME from CALL with ARGS when it exists."
  (let ((callback (plist-get (plist-get call :callbacks) name)))
    (when callback
      (apply callback args))))

(defun deslop-test--complete-call (call)
  "Complete asynchronous fake CALL using its queued response."
  (let ((response (plist-get call :response)))
    (cond
     ((eq response :stall) nil)
     ((eq response :error)
      (deslop-test--callback
       call :on-error
       (deslop-error-create :kind 'mock :message "mock failure")))
     ((stringp response)
      (deslop-test--callback call :on-status "Generating")
      (deslop-test--callback call :on-event
                             (list :type :status :status "Generating"))
      (deslop-test--callback call :on-delta :content response)
      (deslop-test--callback call :on-event
                             (list :type :delta :channel :content
                                   :text response))
      (deslop-test--callback call :on-usage 10 50.0 nil)
      (deslop-test--callback call :on-event
                             (list :type :usage :tokens 10 :speed 50.0))
      (deslop-test--callback call :on-done response))
     ((listp response)
      (deslop-test--callback call :on-done response))
     (t
      (deslop-test--callback call :on-error
                             (format "unknown mock response: %S" response))))))

(defun deslop-test--fake-llm (responses &optional asynchronous process-factory)
  "Return a mock LLM dispatcher for RESPONSES.
When ASYNCHRONOUS is non-nil, calls remain in `deslop-test--calls' until
`deslop-test--complete-call' is invoked.  PROCESS-FACTORY can provide a real
process for cancellation tests."
  (setq deslop-test--fake-responses (copy-sequence responses))
  (lambda (messages callbacks &optional options)
    (push messages deslop-test--prompts)
    (let ((call (list :messages messages
                      :callbacks callbacks
                      :options options
                      :response (pop deslop-test--fake-responses))))
      (when process-factory
        (setq call
              (plist-put call :process (funcall process-factory)))
        (deslop-test--callback call :on-start (plist-get call :process)))
      (setq deslop-test--calls
            (append deslop-test--calls (list call)))
      (unless asynchronous
        (deslop-test--complete-call call))
      call)))

(defun deslop-test--sleep-process ()
  "Create a process that remains live until explicitly terminated."
  (make-process :name (generate-new-buffer-name "deslop-test-sleep")
                :command (list "sleep" "30")
                :connection-type 'pipe
                :noquery t))

(defun deslop-test--select (start end point)
  "Activate the region START..END with point at POINT."
  (setq transient-mark-mode t)
  (goto-char point)
  (set-mark (if (= point end) start end))
  (setq mark-active t))

(defun deslop-test--reset ()
  "Reset global state and clean test-created processes and buffers."
  (deslop-abort-queue)
  (dolist (call deslop-test--calls)
    (let ((process (plist-get call :process)))
      (when (and (processp process) (process-live-p process))
        (delete-process process))))
  (setq deslop-test--prompts nil
        deslop-test--calls nil
        deslop-test--fake-responses nil
        deslop-queue nil
        deslop--queue-tail nil
        deslop-running nil
        deslop--current-job nil
        deslop--aborting nil
        deslop--next-job-id 0)
  (clrhash deslop-history--entries)
  (when (get-buffer deslop-history-buffer-name)
    (kill-buffer deslop-history-buffer-name))
  (when (get-buffer deslop-summary-buffer-name)
    (kill-buffer deslop-summary-buffer-name))
  (deslop-ui-clear-all))

(defmacro deslop-test--with-buffer (contents &rest body)
  "Evaluate BODY in a temporary buffer containing CONTENTS."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,contents)
     ,@body))

;;;; Normal operations

(ert-deftest deslop-test/insert-no-selection ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" there"))))
    (deslop-test--with-buffer "hello world"
      (goto-char 6)
      (let ((job (deslop-insert-at-cursor)))
        (should (eq (deslop-job-state job) 'completed))
        (should (string= (buffer-string) "hello there world"))
        (let ((user-msg (cdr (cadr (car deslop-test--prompts)))))
          (should (string-match "hello<CURSOR> world" user-msg)))))))

(ert-deftest deslop-test/insert-at-buffer-boundaries ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '("START " " END"))))
    (deslop-test--with-buffer "middle"
      (goto-char (point-min))
      (deslop-insert-at-cursor)
      (goto-char (point-max))
      (deslop-insert-at-cursor)
      (should (string= (buffer-string) "START middle END")))))

(ert-deftest deslop-test/insert-selected-placements ()
  (deslop-test--reset)
  (let ((deslop-llm-function
         (deslop-test--fake-llm '("HEADER" "FOOTER") t)))
    (deslop-test--with-buffer "alpha beta"
      (deslop-test--select 1 11 1)
      (setq deslop-running t)
      (deslop-insert-at-cursor)
      (deslop-test--select 1 11 11)
      (deslop-insert-at-cursor)
      (setq deslop-running nil)
      (deslop-process-next)
      (deslop-test--complete-call (nth 0 deslop-test--calls))
      (deslop-test--complete-call (nth 1 deslop-test--calls))
      (should (string= (buffer-string)
                       "HEADER\nalpha beta\nFOOTER")))))

(ert-deftest deslop-test/insert-does-not-mutate-selected-characters ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '("MIDDLE"))))
    (deslop-test--with-buffer "foobar"
      (let ((overlay (deslop-ui-create-overlay (current-buffer) 1 7 "Queued"))
            (request nil))
        (setq request
              (list 'insert :buffer (current-buffer) :selected t
                    :placement :inside :offset 3
                    :source-snapshot "foobar" :overlay overlay
                    :start-marker (deslop--make-marker (current-buffer) 1 t)
                    :end-marker (deslop--make-marker (current-buffer) 7 nil)
                    :cursor-marker (deslop--make-marker (current-buffer) 4 t)
                    :restriction-beg (point-min)
                    :restriction-end (point-max)))
      (deslop-enqueue request))
      (should (string= (buffer-string) "foo\nMIDDLE\nbar")))))

(ert-deftest deslop-test/insert-large-context-elision ()
  (deslop-test--reset)
  (let ((deslop-max-context-chars 30)
        (deslop-window-chars 10)
        (deslop-llm-function (deslop-test--fake-llm '("X"))))
    (deslop-test--with-buffer
        (concat (make-string 20 ?A) (make-string 20 ?B))
      (goto-char 21)
      (deslop-insert-at-cursor)
      (let ((user-msg (cdr (cadr (car deslop-test--prompts)))))
        (should (string-match-p "characters elided" user-msg))
        (should (string-match "AAAA<CURSOR>BBBB" user-msg))))))

(ert-deftest deslop-test/improve-replaces-selection ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '("improved"))))
    (deslop-test--with-buffer "old text"
      (deslop-test--select 1 9 9)
      (let ((job (deslop-improve-at-cursor "make it better")))
        (should (eq (deslop-job-state job) 'completed))
        (should (string= (buffer-string) "improved"))
        (should (cl-some #'deslop-ui-review-overlay-p
                         (overlays-in (point-min) (point-max))))))))

(ert-deftest deslop-test/summarize-opens-buffer ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '("short summary"))))
    (deslop-test--with-buffer "long source"
      (let ((job (deslop-summarize)))
        (should (eq (deslop-job-state job) 'completed))
        (should (string-match-p "short summary"
                                (with-current-buffer deslop-summary-buffer-name
                                  (buffer-string))))
        (with-current-buffer deslop-summary-buffer-name
          (should (cl-some #'deslop-ui-review-overlay-p
                           (overlays-in (point-min) (point-max)))))))))

(ert-deftest deslop-test/output-remains-marked-until-approved ()
  (deslop-test--reset)
  (let ((deslop-model "test-model")
        (deslop-llm-function (deslop-test--fake-llm '(" generated") t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (let* ((job (deslop-insert-at-cursor))
             (queued-overlay (deslop-job-overlay job)))
        (should (string-match-p
                 "test-model"
                 (substring-no-properties
                  (or (overlay-get queued-overlay 'before-string)
                      (overlay-get queued-overlay 'after-string)
                      ""))))
        (should (string-match-p
                 "localhost"
                 (substring-no-properties
                  (or (overlay-get queued-overlay 'before-string)
                      (overlay-get queued-overlay 'after-string)
                      ""))))
        (deslop-test--complete-call (car deslop-test--calls))
        (let ((review (cl-find-if
                     #'deslop-ui-review-overlay-p
                     (overlays-in (point-min) (point-max)))))
          (should review)
          (should (string= (overlay-get review 'model) "test-model"))
          (should (string= (overlay-get review 'client) "localhost"))
          (should (string-match-p
                   "test-model"
                   (substring-no-properties
                    (overlay-get review 'before-string))))
          (should (string-match-p
                   "localhost"
                   (substring-no-properties
                    (overlay-get review 'before-string))))
          (goto-char (1+ (overlay-start review)))
          (insert "edited ")
          (should (deslop-ui-review-overlay-p review))
          (deslop-approve-output)
          (should-not (deslop-ui-review-overlay-p review)))))))

;;;; Queue lifecycle

(ert-deftest deslop-test/queue-fifo-ordering-async ()
  (deslop-test--reset)
  (let ((deslop-llm-function
         (deslop-test--fake-llm '(" 1" " 2" " 3") t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (setq deslop-running t)
      (deslop-insert-at-cursor)
      (deslop-insert-at-cursor)
      (deslop-insert-at-cursor)
      (setq deslop-running nil)
      (deslop-process-next)
      (should (= (length deslop-test--calls) 1))
      (deslop-test--complete-call (nth 0 deslop-test--calls))
      (deslop-test--complete-call (nth 1 deslop-test--calls))
      (deslop-test--complete-call (nth 2 deslop-test--calls))
      (should (string= (buffer-string) "base 1 2 3"))
      (should (null deslop-queue))
      (should (null deslop-running)))))

(ert-deftest deslop-test/abort-invalidates-before-kill ()
  (deslop-test--reset)
  (let ((deslop-llm-function
         (deslop-test--fake-llm '(:stall "never") t
                                #'deslop-test--sleep-process)))
    (deslop-test--with-buffer "content"
      (let ((job (deslop-insert-at-cursor)))
        (deslop-insert-at-cursor)
        (let ((call (car deslop-test--calls)))
          (should deslop-running)
          (deslop-abort-queue)
          (should (eq (deslop-job-state job) 'cancelled))
          (should-not deslop-running)
          (should (null deslop-queue))
          (should-not (process-live-p (plist-get call :process)))
          ;; A late transport callback must be inert.
          (deslop-test--callback call :on-done "orphan")
          (should (string= (buffer-string) "content")))))))

(ert-deftest deslop-test/stale-callback-cannot-settle-next-job ()
  (deslop-test--reset)
  (let ((deslop-llm-function
         (deslop-test--fake-llm '(" A" " B") t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (setq deslop-running t)
      (deslop-insert-at-cursor)
      (deslop-insert-at-cursor)
      (setq deslop-running nil)
      (deslop-process-next)
      (let ((first (nth 0 deslop-test--calls)))
        (deslop-test--complete-call first)
        (let ((second (nth 1 deslop-test--calls)))
          (should second)
          (should (eq (deslop-job-state deslop--current-job) 'running))
          (deslop-test--callback first :on-done " stale")
          (deslop-test--callback first :on-error "stale error")
          (should (eq (deslop-job-state deslop--current-job) 'running))
          (deslop-test--complete-call second)))
      (should (string= (buffer-string) "base A B")))))

(ert-deftest deslop-test/duplicate-completion-is-idempotent ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" X") t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (let ((job (deslop-insert-at-cursor))
            (call nil))
        (setq call (car deslop-test--calls))
        (deslop-test--complete-call call)
        (deslop-test--callback call :on-done " X")
        (should (eq (deslop-job-state job) 'completed))
        (should (string= (buffer-string) "base X"))
        (should-not deslop-running)))))

;;;; Target failure paths

(ert-deftest deslop-test/killed-buffer-settles-job ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" X") t))
        (buffer (generate-new-buffer " *deslop-killed*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "base")
            (goto-char (point-max))
            (deslop-insert-at-cursor))
          (let ((job (car (last deslop-test--calls))))
            (kill-buffer buffer)
            (deslop-test--complete-call job)
            (should-not deslop-running)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest deslop-test/read-only-target-settles-job ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" X") t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (let ((job (deslop-insert-at-cursor))
            (call nil))
        (setq call (car deslop-test--calls))
        (setq buffer-read-only t)
        (deslop-test--complete-call call)
        (should (eq (deslop-job-state job) 'failed))
        (should-not deslop-running)
        (should (string= (buffer-string) "base"))))))

(ert-deftest deslop-test/narrowed-target-settles-job ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" X") t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (let ((job (deslop-insert-at-cursor))
            (call nil))
        (setq call (car deslop-test--calls))
        (narrow-to-region (point-min) (1- (point-max)))
        (deslop-test--complete-call call)
        (should (eq (deslop-job-state job) 'failed))
        (should-not deslop-running)))))

(ert-deftest deslop-test/modification-hook-settles-job ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" X") t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (let ((job (deslop-insert-at-cursor))
            (call nil))
        (setq call (car deslop-test--calls))
        (add-hook 'before-change-functions
                  (lambda (&rest _) (error "blocked")) nil t)
        (deslop-test--complete-call call)
        (should (eq (deslop-job-state job) 'failed))
        (should (string= (buffer-string) "base"))
        (should-not deslop-running)))))

(ert-deftest deslop-test/empty-result-does-not-insert-separators ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '("\n") t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (let ((job (deslop-insert-at-cursor))
            (call nil))
        (setq call (car deslop-test--calls))
        (deslop-test--complete-call call)
        (should (eq (deslop-job-state job) 'failed))
        (should (string= (buffer-string) "base"))))))

;;;; Anchor ordering and conflicts

(ert-deftest deslop-test/same-endpoint-preserves-fifo ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '("A" "B") t)))
    (deslop-test--with-buffer "alpha"
      (deslop-test--select 1 6 6)
      (setq deslop-running t)
      (deslop-insert-at-cursor)
      (deslop-insert-at-cursor)
      (setq deslop-running nil)
      (deslop-process-next)
      (deslop-test--complete-call (nth 0 deslop-test--calls))
      (deslop-test--complete-call (nth 1 deslop-test--calls))
      (should (string= (buffer-string) "alpha\nA\nB")))))

(ert-deftest deslop-test/changed-selection-is-skipped ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '("X") t)))
    (deslop-test--with-buffer "original"
      (deslop-test--select 1 9 9)
      (let ((job (deslop-insert-at-cursor))
            (call nil))
        (setq call (car deslop-test--calls))
        (goto-char 3)
        (delete-char 1)
        (insert "X")
        (deslop-test--complete-call call)
        (should (eq (deslop-job-state job) 'failed))
        (should-not deslop-running)))))

;;;; Incremental reasoning parser

(defun deslop-test--reasoning-callbacks (thinking content)
  "Return callbacks appending thinking and content into two symbols."
  (list :on-delta
        (lambda (type text)
          (if (eq type :thinking)
              (set thinking (concat (symbol-value thinking) text))
            (set content (concat (symbol-value content) text))))
        :on-status #'ignore))

(ert-deftest deslop-test/thinking-parser-separation ()
  (let ((thinking-symbol (make-symbol "thinking"))
        (content-symbol (make-symbol "content")))
    (set thinking-symbol "")
    (set content-symbol "")
    (let* ((parser (deslop-llm--make-reasoning :mode 'content :pending ""))
           (callbacks (deslop-test--reasoning-callbacks
                       thinking-symbol content-symbol)))
      (deslop-llm--reasoning-feed
       parser "Intro <think>Plan here" callbacks)
      (deslop-llm--reasoning-feed
       parser "...done</think>Final answer" callbacks)
      (should-not (deslop-llm--reasoning-finish parser callbacks))
      (should (string= (symbol-value thinking-symbol) "Plan here...done"))
      (should (string= (symbol-value content-symbol) "Intro Final answer")))))

(ert-deftest deslop-test/thinking-parser-all-delimiter-splits ()
  (dolist (split (number-sequence 0 (length "<think>secret</think>answer")))
    (let ((thinking "") (content "")
          (parser (deslop-llm--make-reasoning :mode 'content :pending "")))
      (let ((callbacks
             (list :on-delta
                   (lambda (type text)
                     (if (eq type :thinking)
                         (setq thinking (concat thinking text))
                       (setq content (concat content text)))))))
        (deslop-llm--reasoning-feed
         parser (substring "<think>secret</think>answer" 0 split) callbacks)
        (deslop-llm--reasoning-feed
         parser (substring "<think>secret</think>answer" split) callbacks)
        (should-not (deslop-llm--reasoning-finish parser callbacks))
        (should (string= thinking "secret"))
        (should (string= content "answer"))))))

(ert-deftest deslop-test/thinking-parser-unclosed-is-error ()
  (let ((parser (deslop-llm--make-reasoning :mode 'content :pending "")))
    (deslop-llm--reasoning-feed parser "<think>secret" (list :on-delta #'ignore))
    (should (deslop-error-p
             (deslop-llm--reasoning-finish parser (list :on-delta #'ignore))))))

;;;; SSE parser

(ert-deftest deslop-test/sse-parser-fragmented-and-done ()
  (let ((parser (deslop-llm--make-sse :buffer "" :data-lines nil :done nil))
        (events nil))
    (setq events
          (append events
                  (deslop-llm--sse-feed
                   parser "data: {\"choices\":[{\"delta\":{\"content\":\"hi")))
    (setq events
          (append events
                  (deslop-llm--sse-feed
                   parser " there\"}}]}\r\n\r\ndata: [DONE]\r\n\r\n")))
    (should (= (length events) 2))
    (should (eq (plist-get (nth 0 events) :type) :json))
    (should (string= (alist-get 'content
                                (alist-get 'delta
                                           (aref (alist-get 'choices
                                                            (plist-get (nth 0 events)
                                                                       :value))
                                                 0)))
                     "hi there"))
    (should (eq (plist-get (nth 1 events) :type) :done))
     (should-not (deslop-llm--sse-finish parser))))

(ert-deftest deslop-test/sse-parser-rejects-malformed-and-late-data ()
  (let ((parser (deslop-llm--make-sse :buffer "" :data-lines nil :done nil)))
    (should-error
     (deslop-llm--sse-feed parser "data: {bad}\n\n"))
    (let ((done-parser (deslop-llm--make-sse :buffer "" :data-lines nil :done nil)))
      (deslop-llm--sse-feed done-parser "data: [DONE]\n\n")
      (should-error
       (deslop-llm--sse-feed
        done-parser "data: {\"choices\":[]}\n\n")))))

(ert-deftest deslop-test/sse-parser-requires-done ()
  (let ((parser (deslop-llm--make-sse :buffer "" :data-lines nil :done nil)))
    (deslop-llm--sse-feed parser "data: {\"choices\":[]}\n\n")
    (should-error (deslop-llm--sse-finish parser))))

(defun deslop-test--run-default-transport (header response)
  "Run the default transport with a mocked process and HEADER/RESPONSE.
Return (RESULT ERROR COMMAND)."
  (let ((captured nil)
        (process nil)
        (result nil)
        (failure nil)
        (real-make-process (symbol-function 'make-process))
        (profile (list :name 'test
                       :job-id 1
                       :url "http://example.test/v1"
                       :model "test-model"
                       :temperature 0.2
                       :require-sse t)))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq captured args)
                 (setq process
                       (funcall real-make-process
                        :name
                        (plist-get args :name)
                        :buffer
                        (plist-get args :buffer)
                        :command (list "cat")
                        :connection-type 'pipe)))))
      (unwind-protect
          (progn
            (deslop-llm-default
             (list (cons "user" "body"))
             (list :on-done (lambda (value) (setq result value))
                   :on-error (lambda (value) (setq failure value)))
             profile)
            (let* ((command (plist-get captured :command))
                   (headers-file (nth (1+ (cl-position "--dump-header"
                                                      command :test #'string=))
                                      command))
                   (filter (plist-get captured :filter))
                   (sentinel (plist-get captured :sentinel)))
              (with-temp-file headers-file
                (insert header))
              (funcall filter process response)
              (when (process-live-p process)
                (process-send-eof process))
              (while (process-live-p process)
                (accept-process-output process 0.05))
              (funcall sentinel process "finished\n")
              (list result failure command)))
        (when (and (processp process) (process-live-p process))
          (delete-process process))))))

(ert-deftest deslop-test/default-transport-completes-valid-sse ()
  (let ((out (deslop-test--run-default-transport
              "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n"
              "data: {\"choices\":[{\"delta\":{\"content\":\"hello\"}}]}\n\ndata: [DONE]\n\n")))
    (should-not (nth 1 out))
    (should (string= (plist-get (nth 0 out) :content) "hello"))
    (should-not (member "hello" (nth 2 out)))
    (should (member "@-" (nth 2 out)))))

(ert-deftest deslop-test/default-transport-rejects-http-errors ()
  (let ((out (deslop-test--run-default-transport
              "HTTP/1.1 401 Unauthorized\r\nContent-Type: text/event-stream\r\n\r\n"
              "data: {\"error\":{\"message\":\"bad key\"}}\n\ndata: [DONE]\n\n")))
    (should-not (nth 0 out))
    (should (deslop-error-p (nth 1 out)))
    (should (eq (deslop-error-kind (nth 1 out)) 'http))))

;;;; Profiles, history, and transport command safety

(ert-deftest deslop-test/profile-is-snapshotted-at-enqueue ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(:stall) t))
        (deslop-url "http://one.example/v1"))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (let ((job (deslop-insert-at-cursor)))
        (setq deslop-url "http://two.example/v1")
        (should (string= (plist-get (deslop-job-profile job) :url)
                         "http://one.example/v1"))))))

(ert-deftest deslop-test/auth-and-body-are-not-command-arguments ()
  (let* ((token "secret-token")
         (profile (list :name 'remote
                        :url "https://example.test/v1"
                        :model "model"
                        :temperature 0.2
                        :auth-source-host "example.test"
                        :auth-source-user "deslop"))
         (config nil))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _) (list (list :secret token)))))
      (setq config (deslop-llm--curl-config profile))
      (unwind-protect
          (let ((command (deslop-llm--curl-command
                          profile "/tmp/headers" config)))
            (should-not (member token command))
            (should-not (member "body" command))
            (should-not (member "--fail-with-body" command)))
        (when (file-exists-p config) (delete-file config))))))

(ert-deftest deslop-test/provider-client-display-names ()
  (should (string= (deslop-provider-client-name
                    '(:url "http://localhost:8080/v1"))
                   "localhost"))
  (should (string= (deslop-provider-client-name
                    '(:url "http://192.168.1.20:8080/v1"))
                   "192.168.1.20"))
  (should (string= (deslop-provider-client-name
                    '(:url "https://api.openai.com/v1/chat/completions"))
                   "OpenAI")))

(ert-deftest deslop-test/history-retains-terminal-result ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" result"))))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (deslop-insert-at-cursor)
      (with-current-buffer deslop-history-buffer-name
        (should (string-match-p "Output" (buffer-string)))
        (should (string-match-p "result" (buffer-string)))))))

;;;; Overlay lifecycle

(ert-deftest deslop-test/insert-anchor-does-not-follow-edits-at-point ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" X") t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (let* ((job (deslop-insert-at-cursor))
             (call (car deslop-test--calls))
             (overlay (deslop-job-overlay job)))
        (dotimes (_ 5)
          (insert "\n"))
        (should (= (overlay-start overlay) 5))
        (should (= (deslop--marker-position
                    (deslop-job-cursor-marker job))
                   5))
        (deslop-test--complete-call call)
        (should (string= (buffer-string) "base X\n\n\n\n\n"))))))

(ert-deftest deslop-test/zero-width-overlay-boundary-behavior ()
  (deslop-test--reset)
  (deslop-test--with-buffer "abc"
    (goto-char (point-max))
    (let ((ov (deslop-ui-create-overlay (current-buffer) (point) (point)
                                        "Queued")))
      (insert "\n")
      (should (= (overlay-start ov) 4))
      (should (= (overlay-end ov) 4))
      (should-not (overlay-get ov 'before-string))
      (should (overlay-get ov 'after-string))
      (should (= (point) 5))
      (goto-char (point-min))
      (insert "x")
      (should (= (overlay-start ov) 5))
      (should (= (overlay-end ov) 5)))))

(ert-deftest deslop-test/overlay-lifecycle ()
  (deslop-test--reset)
  (deslop-test--with-buffer "hello"
    (let ((ov (deslop-ui-create-overlay (current-buffer) 1 3 "Queued")))
      (should (overlayp ov))
      (should (= (length deslop-ui--managed-overlays) 1))
      (deslop-ui-delete-overlay ov)
      (should (null deslop-ui--managed-overlays))
      (should (null deslop-ui--spinner-timer)))))

(provide 'deslop-test)

;;; deslop-test.el ends here
