;;; deslop-test.el --- ERT tests for deslop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools, llm

;;; Commentary:

;; ERT test suite for deslop.

;;; Code:

(require 'ert)
(require 'deslop)

;;;; Test Harness & Mocks

(defvar deslop-test--prompts nil
  "List of prompt message lists sent to fake LLM.")

(defvar deslop-test--fake-responses nil
  "Queue of responses for fake LLM.")

(defun deslop-test--fake-llm (responses)
  "Return a mock LLM dispatch function yielding RESPONSES in sequence."
  (setq deslop-test--fake-responses responses)
  (lambda (messages done &optional callbacks _options)
    (push messages deslop-test--prompts)
    (let ((r (if deslop-test--fake-responses
                 (pop deslop-test--fake-responses)
               :no-fake-response)))
      (if (stringp r)
          (progn
            (when (plist-get callbacks :on-status)
              (funcall (plist-get callbacks :on-status) "Generating"))
            (when (plist-get callbacks :on-delta)
              (funcall (plist-get callbacks :on-delta) :content r))
            (when (plist-get callbacks :on-usage)
              (funcall (plist-get callbacks :on-usage) 10 50.0))
            (if (functionp done)
                (funcall done r nil)
              (when (plist-get callbacks :on-done)
                (funcall (plist-get callbacks :on-done) r))))
        (if (functionp done)
            (funcall done nil (format "mock error: %S" r))
          (when (plist-get callbacks :on-error)
            (funcall (plist-get callbacks :on-error) (format "mock error: %S" r))))))))

(defun deslop-test--reset ()
  "Reset all internal state before running tests."
  (deslop-abort-queue)
  (setq deslop-test--prompts nil)
  (setq deslop-test--fake-responses nil)
  (setq deslop-queue nil)
  (setq deslop-running nil)
  (deslop-ui-clear-all))

;;;; Unit Tests: Insert at Cursor

(ert-deftest deslop-test/insert-no-selection ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" there"))))
    (with-temp-buffer
      (insert "hello world")
      (goto-char 6)
      (deslop-insert-at-cursor)
      (should (string= (buffer-string) "hello there world"))
      (let ((user-msg (cdr (cadr (car deslop-test--prompts)))))
        (should (string-match "<CURSOR>" user-msg))
        (should (string-match "hello<CURSOR> world" user-msg))))))

(ert-deftest deslop-test/insert-at-buffer-start ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '("START "))))
    (with-temp-buffer
      (insert "middle end")
      (goto-char (point-min))
      (deslop-insert-at-cursor)
      (should (string= (buffer-string) "START middle end")))))

(ert-deftest deslop-test/insert-at-buffer-end ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" END"))))
    (with-temp-buffer
      (insert "beginning")
      (goto-char (point-max))
      (deslop-insert-at-cursor)
      (should (string= (buffer-string) "beginning END")))))

(ert-deftest deslop-test/insert-region-cursor-at-start ()
  "Cursor at region start: text + newline is inserted BEFORE the region."
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '("HEADER"))))
    (with-temp-buffer
      (setq transient-mark-mode t)
      (insert "alpha beta")
      (set-mark (point-max))
      (goto-char (point-min))
      (deslop-insert-at-cursor)
      (should (string= (buffer-string) "HEADER\nalpha beta"))
      (let ((user-msg (cdr (cadr (car deslop-test--prompts)))))
        (should (string-match "<CURSOR>alpha beta" user-msg))))))

(ert-deftest deslop-test/insert-region-cursor-at-end ()
  "Cursor at region end: newline + text is inserted AFTER the region."
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '("FOOTER"))))
    (with-temp-buffer
      (setq transient-mark-mode t)
      (insert "alpha beta")
      (set-mark (point-min))
      (goto-char (point-max))
      (deslop-insert-at-cursor)
      (should (string= (buffer-string) "alpha beta\nFOOTER"))
      (let ((user-msg (cdr (cadr (car deslop-test--prompts)))))
        (should (string-match "alpha beta<CURSOR>" user-msg))))))

(ert-deftest deslop-test/insert-region-cursor-in-middle ()
  "Cursor inside region: newline + text + newline is inserted at offset."
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '("MIDDLE"))))
    (with-temp-buffer
      (insert "foobar")
      (let ((ov (deslop-ui-create-overlay (current-buffer) 1 7 "Queued")))
        (deslop-enqueue
         (list 'insert :buffer (current-buffer)
                       :selected t
                       :offset 3
                       :overlay ov)))
      (should (string= (buffer-string) "foo\nMIDDLE\nbar"))
      (let ((user-msg (cdr (cadr (car deslop-test--prompts)))))
        (should (string-match "foo<CURSOR>bar" user-msg))))))

(ert-deftest deslop-test/insert-does-not-mutate-region ()
  "The original selected region text survives unaltered."
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '("EXTRA"))))
    (with-temp-buffer
      (setq transient-mark-mode t)
      (insert "preserve this text")
      (set-mark (point-min))
      (goto-char (point-max))
      (deslop-insert-at-cursor)
      (should (string-match-p "preserve this text" (buffer-string))))))

(ert-deftest deslop-test/insert-large-context-elision ()
  "Context exceeding budget is windowed with elision markers."
  (deslop-test--reset)
  (let ((deslop-max-context-chars 30)
        (deslop-window-chars 10)
        (deslop-llm-function (deslop-test--fake-llm '("X"))))
    (with-temp-buffer
      (insert (concat (make-string 20 ?A) (make-string 20 ?B)))
      (goto-char 21)
      (deslop-insert-at-cursor)
      (let ((user-msg (cdr (cadr (car deslop-test--prompts)))))
        (should (string-match-p "characters elided" user-msg))
        (should (string-match "AAAA<CURSOR>BBBB" user-msg))))))

;;;; Unit Tests: Queue Mechanics & Abortion

(ert-deftest deslop-test/queue-fifo-ordering ()
  "Multiple queued inserts execute sequentially in FIFO order."
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" 1" " 2" " 3"))))
    (with-temp-buffer
      (insert "base")
      (goto-char (point-max))
      ;; Enqueue three items while paused
      (let ((deslop-running t))
        (deslop-insert-at-cursor)
        (deslop-insert-at-cursor)
        (deslop-insert-at-cursor))
      (deslop-process-next)
      (should (string= (buffer-string) "base 1 2 3"))
      (should (null deslop-queue))
      (should (null deslop-running)))))

(ert-deftest deslop-test/abort-queue-clears-all ()
  "Aborting queue clears active jobs and deletes overlays."
  (deslop-test--reset)
  (let ((deslop-llm-function
         (lambda (_msg _cb &optional _opt)
           ;; Stall without calling on-done
           nil)))
    (with-temp-buffer
      (insert "content")
      (deslop-insert-at-cursor)
      (deslop-insert-at-cursor)
      (should deslop-running)
      (should (= (length deslop-queue) 1))
      (deslop-abort-queue)
      (should (null deslop-queue))
      (should (null deslop-running))
      (should (null deslop-ui--managed-overlays)))))

;;;; Unit Tests: Streaming Thinking Parser

(ert-deftest deslop-test/thinking-parser-separation ()
  "Verify <think> tag extraction cleanly separates thinking and response content."
  (let ((thinking-deltas nil)
        (content-deltas nil)
        (statuses nil)
        (callbacks nil))
    (setq callbacks
          (list
           :on-status (lambda (s) (push s statuses))
           :on-delta (lambda (type text)
                       (if (eq type :thinking)
                           (push text thinking-deltas)
                         (push text content-deltas)))))
    ;; Process chunk containing thinking and final output
    (let* ((res1 (deslop-llm--process-content "Intro <think>Plan here" nil callbacks))
           (in-think1 (car res1))
           (out1 (cdr res1)))
      (should in-think1)
      (should (string= out1 "Intro "))
      (let* ((res2 (deslop-llm--process-content "...done</think>Final answer" in-think1 callbacks))
             (in-think2 (car res2))
             (out2 (cdr res2)))
        (should (null in-think2))
        (should (string= out2 "Final answer"))
        (should (string= (apply #'concat (nreverse thinking-deltas)) "Plan here...done"))
        (should (string= (apply #'concat (nreverse content-deltas)) "Intro Final answer"))))))

;;;; Unit Tests: Overlay Lifecycle

(ert-deftest deslop-test/overlay-lifecycle ()
  "Overlay creation starts timer; clearing stops timer and empties list."
  (deslop-test--reset)
  (with-temp-buffer
    (insert "hello")
    (let ((ov (deslop-ui-create-overlay (current-buffer) 1 3 "Queued")))
      (should (overlayp ov))
      (should (= (length deslop-ui--managed-overlays) 1))
      (should (timerp deslop-ui--spinner-timer))
      (deslop-ui-delete-overlay ov)
      (should (null deslop-ui--managed-overlays))
      (should (null deslop-ui--spinner-timer)))))

(ert-deftest deslop-test/legacy-2arg-llm-function ()
  "Verify compatibility with legacy 2-argument (messages done) LLM functions."
  (deslop-test--reset)
  (let ((deslop-llm-function
         (lambda (_messages done)
           (funcall done "LEGACY" nil))))
    (with-temp-buffer
      (insert "hello ")
      (goto-char (point-max))
      (deslop-insert-at-cursor)
      (should (string= (buffer-string) "hello LEGACY")))))

(provide 'deslop-test)

;;; deslop-test.el ends here
