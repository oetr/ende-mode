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

(defun deslop-test--overlay-label (overlay)
  "Return the display label attached to OVERLAY."
  (substring-no-properties
   (or (overlay-get overlay 'before-string)
       (overlay-get overlay 'after-string)
       "")))

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
      (when (or (plist-get response :reported-model)
                (plist-get response :reported-provider))
        (deslop-test--callback
         call :on-metadata
         (list :source :response
               :model (plist-get response :reported-model)
               :provider (plist-get response :reported-provider))))
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

(defun deslop-test--count-matches (regexp text)
  "Return the number of nonoverlapping REGEXP matches in TEXT."
  (let ((start 0)
        (count 0))
    (while (string-match regexp text start)
      (setq count (1+ count))
      (setq start (match-end 0)))
    count))

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
         deslop--dispatching nil
         deslop--next-job-id 0)
  (clrhash deslop-provider--discovery-cache)
  (clrhash deslop-provider--discovery-inflight)
  (clrhash deslop-history--entries)
  (when (get-buffer deslop-history-buffer-name)
    (kill-buffer deslop-history-buffer-name))
  (when (buffer-live-p deslop-history--buffer-object)
    (kill-buffer deslop-history--buffer-object))
  (setq deslop-history--buffer-object nil)
  (when (get-buffer deslop-summary-buffer-name)
    (kill-buffer deslop-summary-buffer-name))
  (when (buffer-live-p deslop--summary-buffer-object)
    (kill-buffer deslop--summary-buffer-object))
  (setq deslop--summary-buffer-object nil)
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

(ert-deftest deslop-test/context-window-enforces-source-budget ()
  (let* ((deslop-max-context-chars 30)
         (deslop-window-chars 100)
         (context (deslop-marker-context
                   (concat (make-string 50 ?A) (make-string 50 ?B)) 50))
         (source-chars (+ (cl-count ?A context) (cl-count ?B context))))
    (should (<= source-chars deslop-max-context-chars))
    (should (string-match-p "<CURSOR>" context))
    (should (string-match-p "characters elided" context))))

(ert-deftest deslop-test/large-replacement-targets-are-rejected-before-queueing ()
  (deslop-test--reset)
  (let ((deslop-max-context-chars 4))
    (deslop-test--with-buffer "oversized"
      (should-error (deslop-improve-at-cursor "rewrite") :type 'user-error)
      (should-error (deslop-summarize) :type 'user-error)
      (should-not deslop-queue)
      (should-not deslop-running)
      (should-not deslop-ui--managed-overlays))))

(ert-deftest deslop-test/final-request-budget-includes-instructions ()
  (deslop-test--reset)
  (let ((deslop-max-request-chars 100)
        (deslop-insert-instruction (make-string 200 ?X))
        (deslop-llm-function (deslop-test--fake-llm '("never"))))
    (deslop-test--with-buffer "base"
      (let ((job (deslop-insert-at-cursor)))
        (should (eq (deslop-job-state job) 'failed))
        (should-not deslop-test--calls)
        (should (string= (buffer-string) "base"))))))

(ert-deftest deslop-test/custom-transport-output-is-bounded ()
  (deslop-test--reset)
  (let ((deslop-max-output-chars 3)
        (deslop-llm-function (deslop-test--fake-llm '("four"))))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (let ((job (deslop-insert-at-cursor)))
        (should (eq (deslop-job-state job) 'failed))
        (should (string= (buffer-string) "base"))))))

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
  (let ((deslop-llm-function (deslop-test--fake-llm '("short summary")))
        (displayed nil))
    (cl-letf (((symbol-function 'display-buffer)
               (lambda (buffer &rest _)
                 (setq displayed buffer))))
      (deslop-test--with-buffer "long source"
        (let ((job (deslop-summarize)))
          (should (eq (deslop-job-state job) 'completed))
          (should (eq displayed (get-buffer deslop-summary-buffer-name)))
          (should (string-match-p
                   "short summary"
                   (with-current-buffer deslop-summary-buffer-name
                     (buffer-string))))
          (with-current-buffer deslop-summary-buffer-name
            (should (cl-some #'deslop-ui-review-overlay-p
                             (overlays-in (point-min) (point-max))))))))))

(ert-deftest deslop-test/reject-summary-removes-entire-appended-block ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '("short summary"))))
    (deslop-test--with-buffer "source"
      (deslop-summarize))
    (with-current-buffer deslop-summary-buffer-name
      (goto-char (point-max))
      (deslop-reject-output)
      (should (string-empty-p (buffer-string))))))

(ert-deftest deslop-test/summarize-reports-progress-and-preserves-buffer-point ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '("summary") t)))
    (with-current-buffer (deslop--summary-buffer)
      (insert "existing")
      (goto-char (point-min)))
    (deslop-test--with-buffer "source"
      (let* ((job (deslop-summarize))
             (call (car deslop-test--calls))
             (overlay (deslop-job-overlay job)))
        (deslop-test--callback call :on-status "Generating")
        (deslop-test--callback call :on-usage 12 34.0 nil)
        (should (string= (overlay-get overlay 'status) "Generating"))
        (should (= (overlay-get overlay 'tokens) 12))
        (deslop-test--complete-call call)))
    (with-current-buffer deslop-summary-buffer-name
      (should (= (point) (point-min))))))

(ert-deftest deslop-test/output-remains-marked-until-approved ()
  (deslop-test--reset)
  (let ((deslop-model "test-model")
        (deslop-llm-function (deslop-test--fake-llm '(" generated") t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (let* ((job (deslop-insert-at-cursor))
             (queued-overlay (deslop-job-overlay job)))
        (should (string-match-p "test-model"
                                (deslop-test--overlay-label queued-overlay)))
        (should (string-match-p "localhost"
                                (deslop-test--overlay-label queued-overlay)))
        (deslop-test--complete-call (car deslop-test--calls))
        (let ((review (cl-find-if
                       #'deslop-ui-review-overlay-p
                       (overlays-in (point-min) (point-max)))))
          (should review)
          (should (string= (overlay-get review 'model) "test-model"))
          (should (string= (overlay-get review 'client) "localhost"))
          (should (string-match-p "test-model"
                                  (deslop-test--overlay-label review)))
          (should (string-match-p "localhost"
                                  (deslop-test--overlay-label review)))
          (goto-char (1+ (overlay-start review)))
          (insert "edited ")
          (should (deslop-ui-review-overlay-p review))
          (deslop-approve-output)
          (should-not (deslop-ui-review-overlay-p review)))))))

(ert-deftest deslop-test/response-metadata-wins-for-review-label ()
  (deslop-test--reset)
  (let ((deslop-model "configured-model")
        (deslop-llm-function
         (deslop-test--fake-llm
          '((:content " generated"
             :reported-model "actual-model"
             :reported-provider "llama.cpp"))
          t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (let ((job (deslop-insert-at-cursor)))
        (deslop-test--complete-call (car deslop-test--calls))
        (let ((review (cl-find-if
                       #'deslop-ui-review-overlay-p
                       (overlays-in (point-min) (point-max)))))
          (should review)
          (should (string= (overlay-get review 'model) "actual-model"))
          (should (string= (overlay-get review 'client) "llama.cpp"))
          (should (string-match-p "actual-model"
                                  (deslop-test--overlay-label review)))
          (should-not (string-match-p "configured-model"
                                      (deslop-test--overlay-label review)))
          (should (eq (deslop-job-state job) 'completed)))))))

(ert-deftest deslop-test/response-metadata-cannot-be-overwritten-by-discovery ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" X") t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (let* ((job (deslop-insert-at-cursor))
             (call (car deslop-test--calls)))
        (deslop-test--callback
         call :on-metadata
         '(:source :response :model "response-model" :provider "llama.cpp"))
        (deslop-test--callback
         call :on-metadata
         '(:source :discovery :model "stale-model" :provider "stale"))
        (should (string= (overlay-get (deslop-job-overlay job) 'model)
                         "response-model"))
        (should (string= (overlay-get (deslop-job-overlay job) 'client)
                         "llama.cpp"))))))

(ert-deftest deslop-test/reject-restores-properties-and-save-is-guarded ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '("new"))))
    (deslop-test--with-buffer "old"
      (add-text-properties 1 4 '(deslop-test-property original))
      (deslop-test--select 1 4 4)
      (deslop-improve-at-cursor "rewrite")
      (should-error (run-hooks 'before-save-hook) :type 'user-error)
      (goto-char 2)
      (deslop-reject-output)
      (should (string= (buffer-string) "old"))
      (should (eq (get-text-property 1 'deslop-test-property) 'original))
      (should-not (deslop-ui--review-overlays-in-buffer))
      (should-not (memq #'deslop-ui--prevent-unreviewed-save
                        before-save-hook)))))

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

(ert-deftest deslop-test/deleted-pending-overlay-cannot-fail-committed-insert ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" A" " B") t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (setq deslop-running t)
      (let ((first (deslop-insert-at-cursor))
            (second (deslop-insert-at-cursor)))
        (delete-overlay (deslop-job-overlay second))
        (setq deslop-running nil)
        (deslop-process-next)
        (deslop-test--complete-call (nth 0 deslop-test--calls))
        (should (eq (deslop-job-state first) 'completed))
        (deslop-test--complete-call (nth 1 deslop-test--calls))
        (should (eq (deslop-job-state second) 'completed))
        (should (string= (buffer-string) "base A B"))))))

(ert-deftest deslop-test/malformed-structured-error-cannot-wedge-queue ()
  (deslop-test--reset)
  (let ((deslop-llm-function
         (lambda (_messages callbacks &optional _options)
           (funcall (plist-get callbacks :on-error)
                    (deslop-error-create :kind 'provider :message 123)))))
    (deslop-test--with-buffer "base"
      (let ((job (deslop-insert-at-cursor)))
        (should (eq (deslop-job-state job) 'failed))
        (should-not deslop-running)
        (should-not deslop--current-job)))))

(ert-deftest deslop-test/synchronous-failure-backlog-is-iterative ()
  (deslop-test--reset)
  (deslop-test--with-buffer "base"
    (let ((deslop-running t)
          (max-lisp-eval-depth 100))
      (dotimes (_ 500)
        (deslop-enqueue (list 'unknown :buffer (current-buffer))))
      (setq deslop-running nil)
      (cl-letf (((symbol-function 'message) #'ignore))
        (deslop-process-next))
      (should-not deslop-running)
      (should-not deslop-queue))))

(ert-deftest deslop-test/quit-and-cleanup-errors-still-settle-jobs ()
  (deslop-test--reset)
  (let ((deslop--job-handlers (copy-tree deslop--job-handlers)))
    (deslop-register-job-handler
     'quit-test (lambda (_job) (signal 'quit nil)))
    (deslop-test--with-buffer "base"
      (let ((job (deslop-enqueue
                  (list 'quit-test :buffer (current-buffer)))))
        (should (eq (deslop-job-state job) 'failed))
        (should-not deslop-running))))
  (deslop-test--reset)
  (let ((deslop--job-handlers (copy-tree deslop--job-handlers)))
    (deslop-register-job-handler
     'cleanup-test (lambda (job) (deslop-job-finish job "ok")))
    (deslop-test--with-buffer "base"
      (let* ((overlay (deslop-ui-create-overlay
                       (current-buffer) 1 1 "Queued"))
             (job nil))
        (cl-letf (((symbol-function 'deslop-ui-delete-overlay)
                   (lambda (_) (error "cleanup failed"))))
          (setq job
                (deslop-enqueue
                 (list 'cleanup-test :buffer (current-buffer)
                       :overlay overlay))))
        (should (eq (deslop-job-state job) 'completed))
        (should-not deslop-running)
        (delete-overlay overlay)))))

;;;; Target failure paths

(ert-deftest deslop-test/killed-buffer-settles-job ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" X") t))
        (buffer (generate-new-buffer " *deslop-killed*"))
        (job nil))
    (unwind-protect
        (progn
          (with-current-buffer buffer
             (insert "base")
             (goto-char (point-max))
             (setq job (deslop-insert-at-cursor)))
          (kill-buffer buffer)
          (should (eq (deslop-job-state job) 'failed))
          (should-not deslop-running)
          (deslop-test--complete-call (car deslop-test--calls))
          (should (eq (deslop-job-state job) 'failed)))
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

(ert-deftest deslop-test/widening-does-not-expand-queued-context ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" X") t)))
    (deslop-test--with-buffer "SECRET safe SECRET"
      (narrow-to-region 8 12)
      (goto-char 10)
      (setq deslop-running t)
      (deslop-insert-at-cursor)
      (widen)
      (setq deslop-running nil)
      (deslop-process-next)
      (let ((user-message
             (cdr (cadr (plist-get (car deslop-test--calls) :messages)))))
        (should (string-match-p "sa<CURSOR>fe" user-message))
        (should-not (string-match-p "SECRET" user-message))))))

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
    (should (string= (deslop-llm--json-get
                      "content"
                      (deslop-llm--json-get
                       "delta"
                       (aref (deslop-llm--json-get
                              "choices"
                              (plist-get (nth 0 events) :value))
                             0)))
                     "hi there"))
    (should (eq (plist-get (nth 1 events) :type) :done))
     (should-not (deslop-llm--sse-finish parser))))

(ert-deftest deslop-test/sse-parser-rejects-malformed-and-ignores-late-data ()
  (let ((parser (deslop-llm--make-sse :buffer "" :data-lines nil :done nil)))
    (should-error
     (deslop-llm--sse-feed parser "data: {bad}\n\n"))
    (let ((done-parser (deslop-llm--make-sse :buffer "" :data-lines nil :done nil)))
      (deslop-llm--sse-feed done-parser "data: [DONE]\n\n")
      (should-not
       (deslop-llm--sse-feed
        done-parser "data: {\"choices\":[]}\n\n")))))

(ert-deftest deslop-test/sse-parser-requires-done ()
  (let ((parser (deslop-llm--make-sse :buffer "" :data-lines nil :done nil)))
    (deslop-llm--sse-feed parser "data: {\"choices\":[]}\n\n")
    (should-error (deslop-llm--sse-finish parser))))

(defun deslop-test--run-default-transport (header response)
  "Run the default transport with a mocked process and HEADER/RESPONSE.
Return (RESULT ERROR COMMAND DONE-COUNT ERROR-COUNT)."
  (let ((captured nil)
        (process nil)
        (result nil)
        (failure nil)
        (done-count 0)
        (error-count 0)
        (deslop-provider-discovery-enabled nil)
        (real-make-process (symbol-function 'make-process))
         (profile (list :name 'test
                        :job-id 1
                        :url "https://example.test/v1"
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
              (list :on-done (lambda (value)
                               (setq done-count (1+ done-count))
                               (setq result value))
                    :on-error (lambda (value)
                                (setq error-count (1+ error-count))
                                (setq failure value)))
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
               (list result failure command done-count error-count)))
        (when (and (processp process) (process-live-p process))
          (delete-process process))))))

(ert-deftest deslop-test/default-transport-completes-valid-sse ()
  (let ((out (deslop-test--run-default-transport
               "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n"
               "data: {\"model\":\"actual-model\",\"provider\":\"llama.cpp\",\"choices\":[{\"delta\":{\"content\":\"hello\"}}]}\n\ndata: [DONE]\n\n")))
    (should-not (nth 1 out))
    (should (string= (plist-get (nth 0 out) :content) "hello"))
    (should (string= (plist-get (nth 0 out) :model) "actual-model"))
    (should (string= (plist-get (nth 0 out) :provider) "llama.cpp"))
    (should (string= (plist-get (nth 0 out) :client) "llama.cpp"))
    (should (= (nth 3 out) 1))
    (should (= (nth 4 out) 0))
    (should-not (member "hello" (nth 2 out)))
    (should (member "@-" (nth 2 out)))))

(ert-deftest deslop-test/default-transport-rejects-http-errors ()
  (let ((out (deslop-test--run-default-transport
              "HTTP/1.1 401 Unauthorized\r\nContent-Type: text/event-stream\r\n\r\n"
              "data: {\"error\":{\"message\":\"bad key\"}}\n\ndata: [DONE]\n\n")))
    (should-not (nth 0 out))
    (should (deslop-error-p (nth 1 out)))
    (should (eq (deslop-error-kind (nth 1 out)) 'http))
    (should (= (nth 3 out) 0))
    (should (= (nth 4 out) 1))))

(ert-deftest deslop-test/default-transport-settles-empty-provider-error ()
  (let ((out (deslop-test--run-default-transport
              "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n"
              "data: {\"error\":{}}\n\ndata: [DONE]\n\n")))
    (should-not (nth 0 out))
    (should (eq (deslop-error-kind (nth 1 out)) 'provider))
    (should (= (nth 4 out) 1))))

(ert-deftest deslop-test/default-transport-done-ignores-same-chunk-trailing-data ()
  (let ((out (deslop-test--run-default-transport
              "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n"
              (concat
               "data: {\"choices\":[{\"delta\":{\"content\":\"ok\"}}]}\n\n"
               "data: [DONE]\n\n"
               "data: malformed trailing bytes\n\n"))))
    (should (string= (plist-get (nth 0 out) :content) "ok"))
    (should-not (nth 1 out))
    (should (= (nth 3 out) 1))))

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
                         :auth-source-user "deslop"
                         :auth-source-port 443))
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

(ert-deftest deslop-test/curl-config-escapes-and-rejects-unsafe-headers ()
  (let* ((profile '(:url "https://example.test/v1"
                    :request-headers ("X-Title: a\"b\\c")))
         (config (deslop-llm--curl-config profile)))
    (unwind-protect
        (with-temp-buffer
          (should (= (logand (file-modes config) #o777) #o600))
          (insert-file-contents config)
          (should (string=
                   (buffer-string)
                   "header = \"X-Title: a\\\"b\\\\c\"\n")))
      (when (file-exists-p config)
        (delete-file config))))
  (dolist (header '("X-Test: ok\nurl = https://attacker.test"
                    "Host: attacker.test"
                    "X-Auth-Token: secret"
                    "Content-Length: 0"))
    (should-error
     (deslop-llm--curl-config
      (list :url "https://example.test/v1"
            :request-headers (list header))))))

(ert-deftest deslop-test/provider-url-and-curl-command-are-hardened ()
  (should-error
   (deslop-llm--validated-url '(:url "http://127.attacker.test/v1")))
  (should-error
   (deslop-llm--validated-url '(:url "https://example.test/v1?key=secret")))
  (should-error
   (deslop-llm--validated-url
    '(:url "https://other.test/v1"
      :auth-source-host "example.test"
      :auth-source-user "deslop"
      :auth-source-port 443)))
  (let ((command
         (deslop-llm--curl-command
          '(:url "https://example.test/v1") "/tmp/headers" nil)))
    (should (string= (nth 1 command) "--disable"))
    (should (member "--globoff" command))
    (should (member "--noproxy" command))
    (should (member "--url" command))))

(ert-deftest deslop-test/json-parser-does-not-intern-keys-or-accept-garbage ()
  (let ((key (format "deslop-untrusted-key-%d" (random most-positive-fixnum))))
    (should-not (intern-soft key))
    (let ((payload (deslop-llm--json-read-string
                    (format "{\"%s\":1}" key))))
      (should (hash-table-p payload))
      (should (= (deslop-llm--json-get key payload) 1))
      (should-not (intern-soft key))))
  (should-error (deslop-llm--json-read-string "{\"ok\":1} garbage")))

(ert-deftest deslop-test/default-transport-enforces-response-byte-limit ()
  (let* ((deslop-max-response-bytes 16)
         (out (deslop-test--run-default-transport
               "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n"
               (make-string 100 ?X))))
    (should-not (nth 0 out))
    (should (eq (deslop-error-kind (nth 1 out)) 'resource))
    (should (= (nth 4 out) 1))))

(ert-deftest deslop-test/sse-parser-enforces-line-and-event-limits ()
  (let ((deslop-max-sse-event-bytes 8)
        (parser (deslop-llm--make-sse :buffer "" :data-lines nil
                                      :data-bytes 0 :done nil)))
    (should-error (deslop-llm--sse-feed parser "123456789")))
  (let ((deslop-max-sse-event-bytes 8)
        (parser (deslop-llm--make-sse :buffer "" :data-lines nil
                                      :data-bytes 0 :done nil)))
    (should-error
     (deslop-llm--sse-feed
      parser (mapconcat (lambda (_) "data:\n") (number-sequence 1 10) "")))))

(ert-deftest deslop-test/provider-discovery-cache-is-bounded ()
  (let ((deslop-provider--discovery-cache (make-hash-table :test #'equal))
        (deslop-provider--discovery-inflight (make-hash-table :test #'equal))
        (deslop-provider-discovery-cache-size 2))
    (dotimes (index 5)
      (deslop-provider--finish-discovery (list index) (list :model index)))
    (should (= (hash-table-count deslop-provider--discovery-cache) 2))))

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

(ert-deftest deslop-test/provider-discovery-parses-model-metadata ()
  (let* ((profile '(:url "http://localhost:8080/v1/chat/completions"
                    :model "llama3"))
         (metadata
          (deslop-provider--discovery-metadata
           profile
           (deslop-llm--json-read-string
            "{\"object\":\"list\",\"data\":[{\"id\":\"llama3\",\"owned_by\":\"llama.cpp\"}]}"))))
    (should (string= (deslop-provider--models-url profile)
                     "http://localhost:8080/v1/models"))
    (should (eq (plist-get metadata :source) :discovery))
    (should (string= (plist-get metadata :model) "llama3"))
    (should (string= (plist-get metadata :owner) "llama.cpp"))
    (should (string= (plist-get metadata :client) "llama.cpp"))))

(ert-deftest deslop-test/provider-discovery-does-not-borrow-unrelated-owner ()
  (let* ((profile '(:url "https://example.test/v1/chat/completions"
                    :model "requested"))
         (metadata
          (deslop-provider--discovery-metadata
           profile
           (deslop-llm--json-read-string
            "{\"data\":[{\"id\":\"other\",\"owned_by\":\"wrong-owner\"}]}"))))
    (should-not (plist-get metadata :owner))
    (should-not (plist-get metadata :client))))

(ert-deftest deslop-test/provider-discovery-is-async-and-cached ()
  (let ((profile '(:name local
                   :url "http://localhost:8080/v1/chat/completions"
                   :model "llama3"))
        (captured nil)
        (process nil)
        (values nil)
        (make-process-count 0)
        (real-make-process (symbol-function 'make-process)))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq captured args)
                 (setq make-process-count (1+ make-process-count))
                 (setq process
                       (funcall real-make-process
                                :name (plist-get args :name)
                                :buffer nil
                                :command (list "cat")
                                :connection-type 'pipe)))))
      (unwind-protect
          (progn
            (deslop-provider-discover
             profile (lambda (metadata) (push metadata values)))
            (should (process-live-p process))
            (let* ((command (plist-get captured :command))
                   (header-position
                    (cl-position "--dump-header" command :test #'string=))
                   (headers-file (nth (1+ header-position) command)))
              (should (member "GET" command))
              (should (member "http://localhost:8080/v1/models" command))
              (with-temp-file headers-file
                (insert "HTTP/1.1 200 OK\r\n"
                        "Content-Type: application/json\r\n\r\n"))
              (funcall
               (plist-get captured :filter) process
               "{\"data\":[{\"id\":\"llama3\",\"owned_by\":\"llama.cpp\"}]}")
              (should (string-match-p
                       "llama3"
                       (with-current-buffer (plist-get captured :buffer)
                         (buffer-string))))
              (should (plist-get
                       (deslop-provider--parse-discovery-response
                        profile (plist-get captured :buffer) headers-file)
                       :model))
              (process-send-eof process)
              (while (process-live-p process)
                (accept-process-output process 0.05))
              (funcall (plist-get captured :sentinel) process "finished\n"))
            (should (= make-process-count 1))
            (should (= (length values) 1))
            (should (string= (plist-get (car values) :model) "llama3"))
            (deslop-provider-discover
             profile (lambda (metadata) (push metadata values)))
            (should (= make-process-count 1))
            (should (= (length values) 2))
            (should (string= (plist-get (car values) :model) "llama3")))
        (when (and (processp process) (process-live-p process))
          (delete-process process))
        (clrhash deslop-provider--discovery-cache)
        (clrhash deslop-provider--discovery-inflight)))))

(ert-deftest deslop-test/provider-discovery-response-is-bounded ()
  (let ((profile '(:name local
                   :url "http://localhost:8080/v1/chat/completions"
                   :model "llama3"))
        (deslop-max-discovery-bytes 4)
        (captured nil)
        (process nil)
        (values nil)
        (real-make-process (symbol-function 'make-process)))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq captured args)
                 (setq process
                       (funcall real-make-process
                                :name (plist-get args :name)
                                :buffer (plist-get args :buffer)
                                :command (list "cat")
                                :connection-type 'pipe)))))
      (unwind-protect
          (progn
            (deslop-provider-discover
             profile (lambda (metadata) (push metadata values)))
            (funcall (plist-get captured :filter) process "oversized")
            (should (equal values '(nil)))
            (should-not (process-live-p process))
            (should (= (hash-table-count
                        deslop-provider--discovery-inflight) 0)))
        (when (and (processp process) (process-live-p process))
          (delete-process process))
        (clrhash deslop-provider--discovery-cache)
        (clrhash deslop-provider--discovery-inflight)))))

(ert-deftest deslop-test/history-retains-terminal-result ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" result"))))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (deslop-insert-at-cursor)
      (with-current-buffer deslop-history-buffer-name
        (let ((text (buffer-string)))
          (should (string-match-p "Output" text))
          (should (= (deslop-test--count-matches "result" text) 1))
          (should-not (string-match-p "base" text))
          (should (eq buffer-undo-list t)))))))

(ert-deftest deslop-test/history-terminal-cleanup-survives-disable-and-write-errors ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" result") t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (deslop-insert-at-cursor)
      (should (= (hash-table-count deslop-history--entries) 1))
      (let ((deslop-history-enabled nil))
        (deslop-test--complete-call (car deslop-test--calls)))
      (should (= (hash-table-count deslop-history--entries) 0))))
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" result") t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (deslop-insert-at-cursor)
      (with-current-buffer (deslop-history--buffer)
        (add-hook 'before-change-functions
                  (lambda (&rest _) (error "history blocked")) nil t))
      (deslop-test--complete-call (car deslop-test--calls))
      (should (= (hash-table-count deslop-history--entries) 0)))))

(ert-deftest deslop-test/history-is-bounded-and-body-cannot-create-headings ()
  (deslop-test--reset)
  (let ((deslop-history-max-entries 2)
        (deslop-llm-function
         (deslop-test--fake-llm '(" one" " two" "\n* fake heading"))))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (dotimes (_ 3)
        (deslop-insert-at-cursor))
      (with-current-buffer deslop-history-buffer-name
        (let ((text (buffer-string)))
          (should (= (deslop-test--count-matches "^\\* Job " text) 2))
          (should-not (string-match-p "^\\* fake heading" text)))))))

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
      (should-not deslop-ui--spinning-overlays)
      (should-not deslop-ui--spinner-timer)
      (deslop-ui-update-overlay ov :status "Thinking")
      (should (memq ov deslop-ui--spinning-overlays))
      (should (timerp deslop-ui--spinner-timer))
      (deslop-ui-delete-overlay ov)
      (should (null deslop-ui--managed-overlays))
      (should (null deslop-ui--spinner-timer)))))

(ert-deftest deslop-test/narrowing-cannot-hide-review-from-save-guard ()
  (deslop-test--reset)
  (deslop-test--with-buffer "first second"
    (let ((first (deslop-ui-create-review-overlay
                  (current-buffer) 1 6 nil nil ""))
          (second (deslop-ui-create-review-overlay
                   (current-buffer) 7 13 nil nil "")))
      (narrow-to-region 1 6)
      (deslop-ui-delete-review-overlay first)
      (should (memq #'deslop-ui--prevent-unreviewed-save before-save-hook))
      (should-error (run-hooks 'before-save-hook) :type 'user-error)
      (widen)
      (deslop-ui-delete-review-overlay second)
      (should-not (memq #'deslop-ui--prevent-unreviewed-save
                        before-save-hook)))))

;;;; Review highlight

(ert-deftest deslop-test/review-highlight-blend-color-math ()
  (should (string= (deslop-ui--blend-color "#ffffff" "#0000ff" 0.15) "#d9d9ff"))
  (should (string= (deslop-ui--blend-color "#000000" "#ffffff" 0.5) "#808080"))
  (should (string= (deslop-ui--blend-color "#333333" "#ffffff" 0.0) "#333333"))
  (should (string= (deslop-ui--blend-color "#333333" "#ffffff" 1.0) "#ffffff"))
  (should (null (deslop-ui--blend-color "#abc" "#ffffff" 0.5)))
  (should (null (deslop-ui--blend-color "#ffffff" "junk" 0.5))))

(ert-deftest deslop-test/review-highlight-hex-round-trip ()
  (should (equal (deslop-ui--hex-components "#a1b2c3") '(161 178 195)))
  (should (string= (deslop-ui--hex-join '(161 178 195)) "#a1b2c3"))
  (should (null (deslop-ui--hex-components "red")))
  (should (null (deslop-ui--hex-components "#12345")))
  (should (null (deslop-ui--hex-components "#1234567")))
  (should (null (deslop-ui--hex-join '(1 2)))))

(ert-deftest deslop-test/review-highlight-face-attr-resolution ()
  (set-face-foreground 'font-lock-keyword-face "#0000ff")
  (set-face-background 'font-lock-keyword-face "#ffffff")
  (should (string= (deslop-ui--face-attr 'font-lock-keyword-face :foreground)
                   "#0000ff"))
  (should (string= (deslop-ui--face-attr 'font-lock-keyword-face :background)
                   "#ffffff"))
  (should (string= (deslop-ui--face-attr
                    '(:foreground "red" :background "blue") :foreground)
                   "red"))
  (should (string= (deslop-ui--face-attr
                    '((:foreground "cyan") (:weight bold)) :foreground)
                   "cyan"))
  (should (string= (deslop-ui--face-attr
                    '((:foreground "cyan") (:background "magenta")) :background)
                   "magenta"))
  (should (null (deslop-ui--face-attr '(:weight bold) :foreground))))

(ert-deftest deslop-test/review-highlight-style-from-adjacent-face ()
  (set-face-foreground 'font-lock-keyword-face "#0000ff")
  (set-face-background 'font-lock-keyword-face "#ffffff")
  (deslop-test--with-buffer "defun x"
    (add-text-properties 1 6 '(face font-lock-keyword-face))
    (should (equal (deslop-ui--review-style (current-buffer) 6)
                    '("#0000ff" . "#8080ff")))))

(ert-deftest deslop-test/review-highlight-style-default-fallback ()
  (set-face-foreground 'default "#111111")
  (set-face-background 'default "#222222")
  (deslop-test--with-buffer "plain x"
    (should (equal (deslop-ui--review-style (current-buffer) 6)
                   '("#111111" . "#1a1a1a")))
    (should (equal (deslop-ui--review-style (current-buffer) 1)
                    '("#111111" . "#1a1a1a")))))

(ert-deftest deslop-test/review-highlight-overlay-sets-char-properties ()
  (deslop-test--reset)
  (set-face-foreground 'font-lock-keyword-face "#0000ff")
  (set-face-background 'font-lock-keyword-face "#ffffff")
  (deslop-test--with-buffer "defunLLM"
    (add-text-properties 1 6 '(face font-lock-keyword-face))
    (let ((overlay (deslop-ui-create-review-overlay
                    (current-buffer) 6 9 nil nil "")))
      (should (overlayp overlay))
      (should (not (overlay-get overlay 'face)))
      (should (string= (get-char-property 6 'foreground-color) "#0000ff"))
       (should (string= (get-char-property 8 'background-color) "#8080ff"))
      (should (eq (get-char-property 3 'face) 'font-lock-keyword-face))
      (should (null (get-char-property 3 'foreground-color)))
      (should (memq #'deslop-ui--prevent-unreviewed-save before-save-hook))
      (deslop-ui-delete-review-overlay overlay t)
      (should (null (get-char-property 6 'foreground-color)))
      (should (null (get-char-property 8 'background-color)))
      (should (eq (get-char-property 3 'face) 'font-lock-keyword-face)))))

(ert-deftest deslop-test/review-highlight-disabled-sets-no-properties ()
  (deslop-test--reset)
  (deslop-test--with-buffer "defunLLM"
    (add-text-properties 1 6 '(face font-lock-keyword-face))
    (let* ((deslop-review-highlight nil)
           (overlay (deslop-ui-create-review-overlay
                     (current-buffer) 6 9 nil nil "")))
      (should (null (get-char-property 6 'foreground-color)))
      (should (null (get-char-property 8 'background-color))))))

(ert-deftest deslop-test/review-highlight-approval-cleans-properties ()
  (deslop-test--reset)
  (set-face-foreground 'font-lock-keyword-face "#0000ff")
  (set-face-background 'font-lock-keyword-face "#ffffff")
  (let ((deslop-llm-function (deslop-test--fake-llm '("LLM"))))
    (deslop-test--with-buffer "kw rest"
      (add-text-properties 1 3 '(face font-lock-keyword-face))
      (goto-char 3)
      (deslop-insert-at-cursor)
      (should (string= (buffer-string) "kwLLM rest"))
      (let ((review (cl-find-if #'deslop-ui-review-overlay-p
                                (overlays-in (point-min) (point-max)))))
        (should review)
        (should (string= (get-char-property 3 'foreground-color) "#0000ff"))
         (should (string= (get-char-property 5 'background-color) "#8080ff"))
        (goto-char 4)
        (deslop-approve-output)
        (should (string= (buffer-string) "kwLLM rest"))
        (should (null (get-char-property 3 'foreground-color)))
        (should (null (get-char-property 3 'background-color)))))))

(ert-deftest deslop-test/review-highlight-rejection-preserves-original-properties ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '("new"))))
    (deslop-test--with-buffer "old text"
      (add-text-properties 1 4 '(face font-lock-string-face))
      (add-text-properties 1 4 '(foreground-color "#123456"))
      (deslop-test--select 1 9 1)
      (deslop-improve-at-cursor "rewrite")
      (should (string= (buffer-string) "new"))
      (let ((review (cl-find-if #'deslop-ui-review-overlay-p
                                (overlays-in (point-min) (point-max)))))
        (should review)
        (goto-char 2)
        (deslop-reject-output)
        (should (string= (buffer-string) "old text"))
        (should (eq (get-char-property 1 'face) 'font-lock-string-face))
        (should (string= (get-char-property 1 'foreground-color) "#123456"))
        (should (null (get-char-property 1 'background-color)))))))

(provide 'deslop-test)

;;; deslop-test.el ends here
