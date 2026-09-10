;;; enslop-test.el --- ERT tests for enslop

(require 'ert)
(require 'enslop)

(defvar enslop-test--prompts nil
  "Message lists handed to the fake LLM, newest first.")

(defvar enslop-test--fake-responses nil
  "Pending fake responses for enslop-test--fake-llm.")

(defun enslop-test--fake-llm (responses)
  "Return a fake LLM function that pops RESPONSES in order.
Each response is a string (success) or a non-string sentinel (failure).
Every call's MESSAGES is recorded in enslop-test--prompts."
  (setq enslop-test--fake-responses responses)
  (lambda (messages done)
    (push messages enslop-test--prompts)
    (let* ((r (if enslop-test--fake-responses
                  (pop enslop-test--fake-responses)
                :no-fake-response)))
      (if (stringp r)
          (funcall done r nil)
        (funcall done nil (format "fake LLM: %S" r))))))

(defun enslop-test--reset ()
  (setq enslop-test--prompts nil)
  (setq enslop-test--fake-responses nil)
  (setq enslop-queue nil)
  (setq enslop-running nil)
  (when (get-buffer "*enslop-summary*") (kill-buffer "*enslop-summary*"))
  (when (get-buffer "*enslop-log*") (kill-buffer "*enslop-log*")))

(defun enslop-test--log-string ()
  (with-current-buffer (get-buffer-create "*enslop-log*") (buffer-string)))

(defun enslop-test--summary-string ()
  (with-current-buffer (get-buffer-create "*enslop-summary*") (buffer-string)))

;;;; insert-at-cursor

(ert-deftest enslop-test/insert-at-cursor-no-selection ()
  (enslop-test--reset)
  (let* ((enslop-llm-function (enslop-test--fake-llm '(" there"))))
    (with-temp-buffer
      (insert "hello world")
      (goto-char 6)
      (enslop-insert-at-cursor)
      (should (string= (buffer-string) "hello there world"))
      (let* ((user-msg (cdr (cadr (car enslop-test--prompts)))))
        (should (string-match "<CURSOR>" user-msg))
        (should (string-match "hello<CURSOR> world" user-msg))))))

(ert-deftest enslop-test/insert-three-at-same-cursor ()
  "Three queued inserts at one unselected cursor stack in queue order.
The queue is held while all three jobs are enqueued so that, as with a
real (slow) LLM, every anchor is captured against the unchanged buffer."
  (enslop-test--reset)
  (let* ((enslop-llm-function (enslop-test--fake-llm '(" one" " two" " three"))))
    (with-temp-buffer
      (insert "hello world")
      (goto-char 6)
      (let ((enslop-running t))
        (enslop-insert-at-cursor)
        (enslop-insert-at-cursor)
        (enslop-insert-at-cursor))
      (enslop-process-next)
      (should (string= (buffer-string) "hello one two three world")))))

(ert-deftest enslop-test/insert-at-buffer-start ()
  (enslop-test--reset)
  (let* ((enslop-llm-function (enslop-test--fake-llm '("X"))))
    (with-temp-buffer
      (insert "world")
      (goto-char 1)
      (enslop-insert-at-cursor)
      (should (string= (buffer-string) "Xworld")))))

(ert-deftest enslop-test/insert-at-buffer-end ()
  (enslop-test--reset)
  (let* ((enslop-llm-function (enslop-test--fake-llm '("X"))))
    (with-temp-buffer
      (insert "world")
      (goto-char (point-max))
      (enslop-insert-at-cursor)
      (should (string= (buffer-string) "worldX")))))

(ert-deftest enslop-test/insert-region-cursor-at-start ()
  "Cursor at the region start: TEXT + newline is inserted before the region."
  (enslop-test--reset)
  (let* ((enslop-llm-function (enslop-test--fake-llm '("X"))))
    (with-temp-buffer
      (setq transient-mark-mode t)
      (insert "foo bar")
      (set-mark (point-max))
      (goto-char 0)
      (enslop-insert-at-cursor)
      (should (string= (buffer-string) "X\nfoo bar"))
      (should (string-match "<CURSOR>foo"
                            (cdr (cadr (car enslop-test--prompts))))))))

(ert-deftest enslop-test/insert-region-cursor-at-end ()
  "Cursor at the region end: newline + TEXT is inserted after the region."
  (enslop-test--reset)
  (let* ((enslop-llm-function (enslop-test--fake-llm '("X"))))
    (with-temp-buffer
      (setq transient-mark-mode t)
      (insert "foo bar")
      (goto-char (point-max))
      (set-mark 0)
      (enslop-insert-at-cursor)
      (should (string= (buffer-string) "foo bar\nX"))
      (should (string-match "bar<CURSOR>"
                            (cdr (cadr (car enslop-test--prompts))))))))

(ert-deftest enslop-test/insert-region-cursor-in-middle ()
  "Cursor inside the region: newline + TEXT + newline at the point.
A live region always puts the point at a region endpoint, so the
middle case is exercised by enqueuing the job directly with an
interior offset."
  (enslop-test--reset)
  (let* ((enslop-llm-function (enslop-test--fake-llm '("X"))))
    (with-temp-buffer
      (insert "foobar")
      (enslop-enqueue
       (list 'insert :buffer (current-buffer) :region (cons 1 7)
             :offset 3 :anchor "foobar" :orig 1 :selected t))
      (should (string= (buffer-string) "foo\nX\nbar"))
      (should (string-match "foo<CURSOR>bar"
                            (cdr (cadr (car enslop-test--prompts))))))))

(ert-deftest enslop-test/insert-does-not-mutate-region ()
  "The highlighted text survives an insert verbatim."
  (enslop-test--reset)
  (let* ((enslop-llm-function (enslop-test--fake-llm '("X"))))
    (with-temp-buffer
      (setq transient-mark-mode t)
      (insert "alpha beta gamma")
      (set-mark 6)
      (goto-char 1)
      (enslop-insert-at-cursor)
      (should (string-match-p "alpha beta gamma" (buffer-string))))))

(ert-deftest enslop-test/insert-elides-large-context ()
  "Context above the budget is windowed around the cursor with elision markers."
  (enslop-test--reset)
  (let* ((enslop-max-context-chars 30)
        (enslop-window-chars 10)
        (enslop-llm-function (enslop-test--fake-llm '("X"))))
    (with-temp-buffer
      (insert (concat (make-string 20 ?A) (make-string 20 ?B)))
      (goto-char 21)
      (enslop-insert-at-cursor)
      (let* ((user-msg (cdr (cadr (car enslop-test--prompts)))))
        (should (string-match-p "characters elided" user-msg))
        ;; the window straddles the A/B boundary, split by the marker
        (should (string-match-p "AAAA" user-msg))
        (should (string-match-p "BBBB" user-msg))
        (should (string-match "AAAA<CURSOR>BBBB" user-msg)))
      (should (string= (buffer-string)
                       (concat (make-string 20 ?A) "X" (make-string 20 ?B)))))))

(ert-deftest enslop-test/insert-stale-anchor-skipped ()
  "An insert whose anchor was destroyed by an earlier job is skipped."
  (enslop-test--reset)
  (let* ((enslop-llm-function (enslop-test--fake-llm
                               '("<<<BUFFER>>>\nZZZ" "INS"))))
    (with-temp-buffer
      (insert "aaa bbb")
      ;; Hold the queue so the insert's anchor is captured against the
      ;; original buffer, before the improve job rewrites it.
      (let ((enslop-running t))
        (enslop-queue-improve (current-buffer) (cons 1 7) "rewrite")
        (goto-char 2)
        (enslop-insert-at-cursor))
      (enslop-process-next)
      (should (string= (buffer-string) "ZZZ"))
      (should (string-match-p "SKIP: insert: cursor anchor not found"
                               (enslop-test--log-string))))))

;;;; improve

(ert-deftest enslop-test/improve-region ()
  (enslop-test--reset)
  (let* ((enslop-llm-function (enslop-test--fake-llm '("<<<REGION>>>\nAAA"))))
    (with-temp-buffer
      (insert "aaa bbb ccc")
      (enslop-queue-improve (current-buffer) (cons 1 4) "shout")
      (should (string= (buffer-string) "AAA bbb ccc"))
      (let* ((user-msg (cdr (cadr (car enslop-test--prompts)))))
        (should (string-match "shout" user-msg))
        (should (string-match "<region>aaa</region>" user-msg))))))

(ert-deftest enslop-test/improve-whole-buffer-untagged ()
  "An untagged reply is treated as <<<REGION>>> (here region == whole buffer)."
  (enslop-test--reset)
  (let* ((enslop-llm-function (enslop-test--fake-llm '("NEW STUFF"))))
    (with-temp-buffer
      (insert "aaa bbb")
      (enslop-queue-improve (current-buffer) (cons 1 8) "rewrite")
      (should (string= (buffer-string) "NEW STUFF")))))

(ert-deftest enslop-test/improve-whole-buffer-tag ()
  "The LLM may choose to rewrite the whole buffer via <<<BUFFER>>>."
  (enslop-test--reset)
  (let* ((enslop-llm-function (enslop-test--fake-llm '("<<<BUFFER>>>\nNEW BUFFER"))))
    (with-temp-buffer
      (insert "aaa bbb")
      (enslop-queue-improve (current-buffer) (cons 1 8) "rewrite")
      (should (string= (buffer-string) "NEW BUFFER")))))

(ert-deftest enslop-test/improve-large-buffer-forces-region-tag ()
  "When the buffer exceeds the budget, only region + surroundings are sent
and the prompt forces the <<<REGION>>> tag."
  (enslop-test--reset)
  (let* ((enslop-max-context-chars 30)
        (enslop-llm-function (enslop-test--fake-llm '("<<<REGION>>>\nA10"))))
    (with-temp-buffer
      (insert "aaaaaaaaaa bbbbbbbbbbbb cccccccccc")
      (enslop-queue-improve (current-buffer) (cons 1 11) "fix")
      (should (string= (buffer-string) "A10 bbbbbbbbbbbb cccccccccc"))
      (should (string-match-p "You MUST respond with <<<REGION>>> only"
                               (cdr (car (car enslop-test--prompts))))))))

(ert-deftest enslop-test/improve-chunked-region ()
  "A region above the budget is improved chunk by chunk, in order."
  (enslop-test--reset)
  (let* ((enslop-max-context-chars 10)
        (enslop-llm-function
         (enslop-test--fake-llm
          '("<<<REGION>>>\nC1" "<<<REGION>>>\nC2" "<<<REGION>>>\nC3"))))
    (with-temp-buffer
      (insert (concat "HEAD" (make-string 25 ?1) "TAIL"))
      (enslop-queue-improve (current-buffer) (cons 5 30) "polish")
      (should (string= (buffer-string) "HEADC1C2C3TAIL"))
      (should (= (length enslop-test--prompts) 3))
      ;; the first chunk sent must be the first 10 chars of the region
      (should (string-match
               "<region-chunk>\n1111111111\n</region-chunk>"
               (cdr (cadr (cadr enslop-test--prompts))))))))

(ert-deftest enslop-test/improve-twice-same-region-second-skipped ()
  (enslop-test--reset)
  (let* ((enslop-llm-function (enslop-test--fake-llm
                              '("<<<REGION>>>\nAAA" "<<<REGION>>>\nSHOULD NOT RUN"))))
    (with-temp-buffer
      (insert "aaa bbb")
      ;; Hold the queue so both anchors are captured against the
      ;; original buffer, as with a real (slow) LLM.
      (let ((enslop-running t))
        (enslop-queue-improve (current-buffer) (cons 1 4) "x")
        (enslop-queue-improve (current-buffer) (cons 1 4) "y"))
      (enslop-process-next)
      (should (string= (buffer-string) "AAA bbb"))
      (should (string-match-p "SKIP: improve: region not found"
                               (enslop-test--log-string))))))

;;;; summarize

(ert-deftest enslop-test/summarize-region ()
  (enslop-test--reset)
  (let* ((enslop-llm-function (enslop-test--fake-llm '("A fox summary."))))
    (with-temp-buffer
      (insert "The quick brown fox. The lazy dog.")
      (let* ((job (list 'summarize :buffer (current-buffer)
                       :region (cons 1 20)
                       :region-text "The quick brown fox."
                       :whole nil)))
        (enslop-enqueue job))
      (should (string= (buffer-string) "The quick brown fox. The lazy dog."))
      (should (string-match-p "A fox summary." (enslop-test--summary-string)))
      (should (string-match-p "(region)" (enslop-test--summary-string))))))

(ert-deftest enslop-test/summarize-whole-buffer-chunked ()
  "Content above the budget is summarized chunk-wise with a running summary."
  (enslop-test--reset)
  (let* ((enslop-max-context-chars 10)
        (enslop-llm-function
         (enslop-test--fake-llm '("S1" "S12" "S123"))))
    (with-temp-buffer
      (insert (make-string 30 ?x))
      (let* ((job (list 'summarize :buffer (current-buffer)
                       :region (cons 1 31) :region-text (make-string 30 ?x)
                       :whole t)))
        (enslop-enqueue job))
      (should (string= (buffer-string) (make-string 30 ?x)))
      (should (string-match "S123" (enslop-test--summary-string)))
      (should (= (length enslop-test--prompts) 3))
       ;; second call carries the running summary
       (should (string-match "Previous summary:\nS1"
                             (cdr (cadr (cadr enslop-test--prompts))))))))

;;;; queue

(ert-deftest enslop-test/queue-disjoint-improves-in-order ()
  (enslop-test--reset)
  (let* ((enslop-llm-function (enslop-test--fake-llm
                              '("<<<REGION>>>\nAAA" "<<<REGION>>>\nCCC"))))
    (with-temp-buffer
      (insert "aaa mmm ccc")
      (enslop-queue-improve (current-buffer) (cons 1 4) "x")
      (enslop-queue-improve (current-buffer) (cons 9 12) "y")
      (should (string= (buffer-string) "AAA mmm CCC"))
      (should (null enslop-queue))
      (should (null enslop-running)))))

(ert-deftest enslop-test/queue-status-and-abort ()
  "With a request in flight, further jobs accumulate and abort clears them."
  (enslop-test--reset)
  (let* ((stalled nil)
        (enslop-llm-function
         (lambda (messages done)
           (push messages enslop-test--prompts)
           (setq stalled t)
           nil)))
    (with-temp-buffer
      (insert "abc")
      (enslop-queue-improve (current-buffer) (cons 1 2) "x")
      (enslop-queue-improve (current-buffer) (cons 1 2) "y")
      (should stalled)
      (should enslop-running)
      (should (= (length enslop-queue) 1))
      (enslop-abort-queue)
      (should (null enslop-queue)))))

;;;; parsing

(ert-deftest enslop-test/parse-improve ()
  (should (equal (enslop-parse-improve "<<<REGION>>>\nfoo") '(region . "foo")))
  (should (equal (enslop-parse-improve "<<<REGION>>>\nfoo\n") '(region . "foo")))
  (should (equal (enslop-parse-improve "<<<BUFFER>>>\nbar") '(buffer . "bar")))
  (should (equal (enslop-parse-improve "plain text") '(region . "plain text"))))

;;;; prompt window (headless-safe)

(ert-deftest enslop-test/prompt-submit-queues-improve ()
  (enslop-test--reset)
  (let* ((submitted nil)
        (enslop-llm-function (enslop-test--fake-llm '("<<<REGION>>>\nAAA"))))
    (with-temp-buffer
      (insert "aaa bbb")
      (let* ((buf (current-buffer))
             (prompt-buf (get-buffer-create "*enslop-prompt*")))
        (with-current-buffer prompt-buf
          (enslop-prompt-mode)
          (setq-local enslop-prompt-continue
                      (lambda (instructions)
                        (setq submitted instructions)
                        (enslop-queue-improve buf (cons 1 4) instructions)))
          (setq-local enslop-prompt-prev-window nil)
          (setq-local enslop-prompt-header-len 0)
          (insert "shout louder")
          (enslop-prompt-submit))
      (should (equal submitted "shout louder"))
      (should (null (get-buffer "*enslop-prompt*")))
      (should (string= (buffer-string) "AAA bbb"))))))

(ert-deftest enslop-test/prompt-empty-submit-keeps-buffer ()
  (enslop-test--reset)
  (let* ((submitted nil)
        (prompt-buf (get-buffer-create "*enslop-prompt*")))
    (with-current-buffer prompt-buf
      (enslop-prompt-mode)
      (setq-local enslop-prompt-continue
                  (lambda (instructions) (setq submitted instructions)))
      (setq-local enslop-prompt-prev-window nil)
      (setq-local enslop-prompt-header-len 0)
      (enslop-prompt-submit))
    (should (null submitted))
    (should (get-buffer "*enslop-prompt*"))
    (kill-buffer prompt-buf)))

;;;; mode

(ert-deftest enslop-test/mode-keybindings ()
  (with-temp-buffer
    (enslop-mode 1)
    (should (eq (lookup-key enslop-mode-map (kbd "C-c e i"))
                #'enslop-insert-at-cursor))
    (should (eq (lookup-key enslop-mode-map (kbd "C-c e p"))
                #'enslop-improve-at-cursor))
    (should (eq (lookup-key enslop-mode-map (kbd "C-c e s"))
                #'enslop-summarize))
    (enslop-mode -1)))

(ert-run-tests-batch)
