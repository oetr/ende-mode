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
(require 'deslop-provider)
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
       (deslop-test--callback call :on-event
                              (list :type :status :status "Generating"))
       (deslop-test--callback call :on-event
                              (list :type :delta :channel :content
                                    :text response))
       (deslop-test--callback call :on-event
                              (list :type :usage :tokens 10 :speed 50.0))
       (deslop-test--callback call :on-done response))
      ((listp response)
       (when (or (plist-get response :reported-model)
                 (plist-get response :reported-provider))
         (deslop-test--callback
          call :on-event
          (list :type :metadata
                :metadata
                (list :source :response
                      :model (plist-get response :reported-model)
                      :provider (plist-get response :reported-provider)))))
       (when (stringp (plist-get response :thinking))
         (deslop-test--callback
          call :on-event
          (list :type :delta :channel :thinking
                :text (plist-get response :thinking))))
       (when (stringp (plist-get response :content))
         (deslop-test--callback
          call :on-event
          (list :type :delta :channel :content
                :text (plist-get response :content))))
       (when (plist-member response :usage)
         (deslop-test--callback
          call :on-event
          (list :type :usage
                :tokens (plist-get response :tokens)
                :speed (plist-get response :speed)
                :usage (plist-get response :usage))))
       (when (listp (plist-get response :partial))
         (deslop-test--callback
          call :on-event
          (list :type :partial-result :result (plist-get response :partial))))
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
        (deslop-test--callback
         call :on-start
         (lambda ()
           (let ((process (plist-get call :process)))
             (when (and (processp process) (process-live-p process))
               (process-put process :deslop-cancelled t)
               (delete-process process))))))
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
         deslop--queue-suspended nil
         deslop--current-job nil
         deslop--aborting nil
         deslop--dispatching nil
         deslop--next-job-id 0)
  (clrhash deslop-provider--discovery-cache)
  (clrhash deslop-provider--discovery-inflight)
  (clrhash deslop-history--entries)
  (when (buffer-live-p deslop-history--buffer-object)
    (kill-buffer deslop-history--buffer-object))
  (setq deslop-history--buffer-object nil)
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

(ert-deftest deslop-test/reset-does-not-kill-unowned-named-buffers ()
  (let* ((deslop-history-buffer-name " *deslop-unowned-history*")
         (deslop-summary-buffer-name " *deslop-unowned-summary*")
         (history (get-buffer-create deslop-history-buffer-name))
         (summary (get-buffer-create deslop-summary-buffer-name)))
    (unwind-protect
        (progn
          (setq deslop-history--buffer-object nil
                deslop--summary-buffer-object nil)
          (deslop-test--reset)
          (should (buffer-live-p history))
          (should (buffer-live-p summary)))
      (when (buffer-live-p history)
        (kill-buffer history))
      (when (buffer-live-p summary)
        (kill-buffer summary)))))

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
      (setq deslop--queue-suspended t)
      (deslop-insert-at-cursor)
      (deslop-test--select 1 11 11)
      (deslop-insert-at-cursor)
      (setq deslop--queue-suspended nil)
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
              (list 'insert
                    :target
                    (deslop-target-create
                     :buffer (current-buffer) :start 1 :end 7 :cursor 4
                     :selected t :placement :inside :offset 3
                     :source-snapshot "foobar")
                    :overlay overlay))
      (deslop-enqueue request))
      (should (string= (buffer-string) "foo\nMIDDLE\nbar")))))

(ert-deftest deslop-test/insert-large-context-elision ()
  (deslop-test--reset)
  (let ((deslop-target-max-context-chars 30)
        (deslop-target-window-chars 10)
        (deslop-llm-function (deslop-test--fake-llm '("X"))))
    (deslop-test--with-buffer
        (concat (make-string 20 ?A) (make-string 20 ?B))
      (goto-char 21)
      (deslop-insert-at-cursor)
      (let ((user-msg (cdr (cadr (car deslop-test--prompts)))))
        (should (string-match-p "characters elided" user-msg))
        (should (string-match "AAAA<CURSOR>BBBB" user-msg))))))

(ert-deftest deslop-test/target-context-window-enforces-source-budget ()
  (let* ((deslop-target-max-context-chars 30)
         (deslop-target-window-chars 100)
         (context (deslop-target-string-marker-context
                    (concat (make-string 50 ?A) (make-string 50 ?B)) 50))
         (source-chars (+ (cl-count ?A context) (cl-count ?B context))))
    (should (<= source-chars deslop-target-max-context-chars))
    (should (string-match-p "<CURSOR>" context))
    (should (string-match-p "characters elided" context))))

(ert-deftest deslop-test/large-replacement-targets-are-rejected-before-queueing ()
  (deslop-test--reset)
  (let ((deslop-target-max-context-chars 4))
    (deslop-test--with-buffer "oversized"
      (should-error (deslop-improve-at-cursor "rewrite") :type 'user-error)
      (should-error (deslop-summarize) :type 'user-error)
      (should-not deslop-queue)
      (should-not deslop--current-job)
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

(ert-deftest deslop-test/custom-transport-output-can-be-unbounded ()
  (deslop-test--reset)
  (let ((deslop-max-output-chars nil)
        (deslop-llm-function (deslop-test--fake-llm '("four"))))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (let ((job (deslop-insert-at-cursor)))
        (should (eq (deslop-job-state job) 'completed))
        (should (string= (buffer-string) "basefour"))))))

(ert-deftest deslop-test/improve-replaces-selection ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '("improved"))))
    (deslop-test--with-buffer "old text"
      (deslop-test--select 1 9 9)
      (let ((job (deslop-improve-at-cursor "make it better")))
        (should (eq (deslop-job-state job) 'completed))
        (should (string= (buffer-string) "improved"))
        (should (cl-some #'deslop-review-overlay-p
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
            (should (cl-some #'deslop-review-overlay-p
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
        (deslop-test--callback
         call :on-event '(:type :status :status "Generating"))
        (deslop-test--callback
         call :on-event '(:type :usage :tokens 12 :speed 34.0))
        (should (string= (overlay-get overlay 'status) "Generating"))
        (should (= (overlay-get overlay 'tokens) 12))
        (deslop-test--complete-call call)))
    (with-current-buffer deslop-summary-buffer-name
      (should (= (point) (point-min))))))

(ert-deftest deslop-test/output-remains-marked-until-approved ()
  (deslop-test--reset)
  (let ((deslop-provider-model "test-model")
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
                       #'deslop-review-overlay-p
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
          (should (deslop-review-overlay-p review))
          (deslop-approve-output)
          (should-not (deslop-review-overlay-p review)))))))

(ert-deftest deslop-test/task-cannot-start-inside-pending-review ()
  (deslop-test--reset)
  (deslop-test--with-buffer "generated text"
    (let ((review (deslop-review-create-overlay
                   (current-buffer) 1 (point-max) nil nil "")))
      (goto-char 5)
      (should-error (deslop-insert-at-cursor) :type 'user-error)
      (deslop-test--select 2 6 6)
      (should-error (deslop-improve-at-cursor "rewrite") :type 'user-error)
      (should-error (deslop-summarize) :type 'user-error)
      (should-not deslop-queue)
      (deslop-review-delete-overlay review))))

(ert-deftest deslop-test/queued-task-cannot-dispatch-into-new-review ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" never") t)))
    (deslop-test--with-buffer "base"
      (goto-char 3)
      (setq deslop--queue-suspended t)
      (let ((job (deslop-insert-at-cursor))
            (review (deslop-review-create-overlay
                     (current-buffer) 1 (point-max) nil nil "")))
        (setq deslop--queue-suspended nil)
        (deslop-process-next)
        (should (eq (deslop-job-state job) 'failed))
        (should-not deslop-test--calls)
        (deslop-review-delete-overlay review)))))

(ert-deftest deslop-test/review-creation-failure-leaves-no-overlay ()
  (deslop-test--reset)
  (deslop-test--with-buffer "text"
    (let ((deslop-review-highlight-blend 'invalid))
      (should-error
       (deslop-review-create-overlay (current-buffer) 1 3 nil nil ""))
      (should-not (deslop-review--overlays-in-buffer))
      (should-not (memq #'deslop-review--prevent-save
                        before-save-hook)))))

(ert-deftest deslop-test/response-metadata-wins-for-review-label ()
  (deslop-test--reset)
  (let ((deslop-provider-model "configured-model")
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
                       #'deslop-review-overlay-p
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
         call :on-event
         '(:type :metadata :metadata
           (:source :response :model "response-model" :provider "llama.cpp")))
        (deslop-test--callback
         call :on-event
         '(:type :metadata :metadata
           (:source :discovery :model "stale-model" :provider "stale")))
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
      (should-not (deslop-review--overlays-in-buffer))
      (should-not (memq #'deslop-review--prevent-save
                        before-save-hook)))))

;;;; Queue lifecycle

(ert-deftest deslop-test/queue-fifo-ordering-async ()
  (deslop-test--reset)
  (let ((deslop-llm-function
         (deslop-test--fake-llm '(" 1" " 2" " 3") t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (setq deslop--queue-suspended t)
      (deslop-insert-at-cursor)
      (deslop-insert-at-cursor)
      (deslop-insert-at-cursor)
      (setq deslop--queue-suspended nil)
      (deslop-process-next)
      (should (= (length deslop-test--calls) 1))
      (deslop-test--complete-call (nth 0 deslop-test--calls))
      (deslop-test--complete-call (nth 1 deslop-test--calls))
      (deslop-test--complete-call (nth 2 deslop-test--calls))
      (should (string= (buffer-string) "base 1 2 3"))
      (should (null deslop-queue))
      (should (null deslop--current-job)))))

(ert-deftest deslop-test/abort-invalidates-before-kill ()
  (deslop-test--reset)
  (let ((deslop-llm-function
         (deslop-test--fake-llm '(:stall "never") t
                                #'deslop-test--sleep-process)))
    (deslop-test--with-buffer "content"
      (let ((job (deslop-insert-at-cursor)))
        (deslop-insert-at-cursor)
        (let ((call (car deslop-test--calls)))
          (should deslop--current-job)
          (deslop-abort-queue)
          (should (eq (deslop-job-state job) 'cancelled))
          (should-not deslop--current-job)
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
      (setq deslop--queue-suspended t)
      (deslop-insert-at-cursor)
      (deslop-insert-at-cursor)
      (setq deslop--queue-suspended nil)
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
        (should-not deslop--current-job)))))

(ert-deftest deslop-test/deleted-pending-overlay-cannot-fail-committed-insert ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" A" " B") t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (setq deslop--queue-suspended t)
      (let ((first (deslop-insert-at-cursor))
            (second (deslop-insert-at-cursor)))
        (delete-overlay (deslop-job-overlay second))
        (setq deslop--queue-suspended nil)
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
        (should-not deslop--current-job)
        (should-not deslop--current-job)))))

(ert-deftest deslop-test/synchronous-failure-backlog-is-iterative ()
  (deslop-test--reset)
  (deslop-test--with-buffer "base"
    (let ((deslop--queue-suspended t)
          (max-lisp-eval-depth 100))
      (dotimes (_ 500)
        (deslop-enqueue
         (list 'unknown
               :target (deslop-target-create
                        :buffer (current-buffer) :start 1 :end 1 :cursor 1))))
      (setq deslop--queue-suspended nil)
      (cl-letf (((symbol-function 'message) #'ignore))
        (deslop-process-next))
      (should-not deslop--current-job)
      (should-not deslop-queue))))

(ert-deftest deslop-test/quit-and-cleanup-errors-still-settle-jobs ()
  (deslop-test--reset)
  (let ((deslop--job-handlers (copy-tree deslop--job-handlers)))
    (deslop-register-job-handler
     'quit-test (lambda (_job) (signal 'quit nil)))
    (deslop-test--with-buffer "base"
      (let ((job (deslop-enqueue
                  (list 'quit-test
                        :target (deslop-target-create
                                 :buffer (current-buffer)
                                 :start 1 :end 1 :cursor 1)))))
        (should (eq (deslop-job-state job) 'failed))
        (should-not deslop--current-job))))
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
                  (list 'cleanup-test
                        :target (deslop-target-create
                                 :buffer (current-buffer)
                                 :start 1 :end 1 :cursor 1)
                        :overlay overlay))))
        (should (eq (deslop-job-state job) 'completed))
        (should-not deslop--current-job)
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
          (should-not deslop--current-job)
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
        (should-not deslop--current-job)
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
         (should-not deslop--current-job)))))

(ert-deftest deslop-test/widening-does-not-expand-queued-context ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" X") t)))
    (deslop-test--with-buffer "SECRET safe SECRET"
      (narrow-to-region 8 12)
      (goto-char 10)
      (setq deslop--queue-suspended t)
      (deslop-insert-at-cursor)
      (widen)
      (setq deslop--queue-suspended nil)
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
        (should-not deslop--current-job)))))

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
      (setq deslop--queue-suspended t)
      (deslop-insert-at-cursor)
      (deslop-insert-at-cursor)
      (setq deslop--queue-suspended nil)
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
        (should-not deslop--current-job)))))

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
    (let* ((parser (deslop-protocol--make-reasoning :mode 'content :pending ""))
           (callbacks (deslop-test--reasoning-callbacks
                       thinking-symbol content-symbol)))
      (deslop-protocol--reasoning-feed
       parser "Intro <think>Plan here" callbacks)
      (deslop-protocol--reasoning-feed
       parser "...done</think>Final answer" callbacks)
      (should-not (deslop-protocol--reasoning-finish parser callbacks))
      (should (string= (symbol-value thinking-symbol) "Plan here...done"))
      (should (string= (symbol-value content-symbol) "Intro Final answer")))))

(ert-deftest deslop-test/thinking-parser-all-delimiter-splits ()
  (dolist (split (number-sequence 0 (length "<think>secret</think>answer")))
    (let ((thinking "") (content "")
          (parser (deslop-protocol--make-reasoning :mode 'content :pending "")))
      (let ((callbacks
             (list :on-delta
                   (lambda (type text)
                     (if (eq type :thinking)
                         (setq thinking (concat thinking text))
                       (setq content (concat content text)))))))
        (deslop-protocol--reasoning-feed
         parser (substring "<think>secret</think>answer" 0 split) callbacks)
        (deslop-protocol--reasoning-feed
         parser (substring "<think>secret</think>answer" split) callbacks)
        (should-not (deslop-protocol--reasoning-finish parser callbacks))
        (should (string= thinking "secret"))
        (should (string= content "answer"))))))

(ert-deftest deslop-test/thinking-parser-unclosed-is-error ()
  (let ((parser (deslop-protocol--make-reasoning :mode 'content :pending "")))
    (deslop-protocol--reasoning-feed parser "<think>secret" (list :on-delta #'ignore))
    (should (deslop-error-p
              (deslop-protocol--reasoning-finish parser (list :on-delta #'ignore))))))

;;;; SSE parser

(ert-deftest deslop-test/sse-parser-fragmented-and-done ()
  (let ((parser (deslop-protocol--make-sse
                 :buffer-parts nil :data-lines nil :done nil))
        (events nil))
    (setq events
          (append events
                   (deslop-protocol--sse-feed
                   parser "data: {\"choices\":[{\"delta\":{\"content\":\"hi")))
    (setq events
          (append events
                   (deslop-protocol--sse-feed
                   parser " there\"}}]}\r\n\r\ndata: [DONE]\r\n\r\n")))
    (should (= (length events) 2))
    (should (eq (plist-get (nth 0 events) :type) :json))
    (should (string= (deslop-protocol--json-get
                      "content"
                      (deslop-protocol--json-get
                       "delta"
                       (aref (deslop-protocol--json-get
                              "choices"
                              (plist-get (nth 0 events) :value))
                             0)))
                     "hi there"))
    (should (eq (plist-get (nth 1 events) :type) :done))
     (should-not (deslop-protocol--sse-finish parser))))

(ert-deftest deslop-test/sse-parser-rejects-malformed-and-ignores-late-data ()
  (let ((parser (deslop-protocol--make-sse
                 :buffer-parts nil :data-lines nil :done nil)))
    (should-error
     (deslop-protocol--sse-feed parser "data: {bad}\n\n"))
    (let ((done-parser (deslop-protocol--make-sse :buffer-parts nil)))
      (deslop-protocol--sse-feed done-parser "data: [DONE]\n\n")
      (should-not
       (deslop-protocol--sse-feed
        done-parser "data: {\"choices\":[]}\n\n")))))

(ert-deftest deslop-test/sse-parser-requires-done ()
  (let ((parser (deslop-protocol--make-sse :buffer-parts nil)))
    (deslop-protocol--sse-feed parser "data: {\"choices\":[]}\n\n")
    (should-error (deslop-protocol--sse-finish parser))))

(ert-deftest deslop-test/transport-session-settles-once-and-cleans-resources ()
  (let* ((process-buffer (generate-new-buffer " *deslop-test-process*"))
         (stderr-buffer (generate-new-buffer " *deslop-test-stderr*"))
         (headers-file (make-temp-file "deslop-test-headers-"))
         (config-file (make-temp-file "deslop-test-config-"))
         (events 0)
         (done 0)
         (errors 0)
         (session
          (deslop-llm--make-session
           :callbacks
           (list :on-event (lambda (_event)
                             (setq events (1+ events))
                             (error "callback failed"))
                 :on-done (lambda (_result) (setq done (1+ done)))
                 :on-error (lambda (_error) (setq errors (1+ errors))))
           :process-buffer process-buffer :stderr-buffer stderr-buffer
           :headers-file headers-file :config-file config-file)))
    (deslop-llm--fail
     session (deslop-error-create :kind 'transport :message "failed"))
    (deslop-llm--fail
     session (deslop-error-create :kind 'transport :message "late"))
    (deslop-llm--settle session 'success "late")
    (should (eq (deslop-llm--session-terminal session) 'failure))
    (should (= events 1))
    (should (= done 0))
    (should (= errors 1))
    (should-not (deslop-llm--session-callbacks session))
    (should-not (buffer-live-p process-buffer))
    (should-not (buffer-live-p stderr-buffer))
    (should-not (file-exists-p headers-file))
    (should-not (file-exists-p config-file))))

(defun deslop-test--run-default-transport (header response)
  "Run the default transport with a mocked process and HEADER/RESPONSE.
Return (RESULT ERROR COMMAND DONE-COUNT ERROR-COUNT CANCEL-FUNCTION EVENTS)."
  (let ((captured nil)
        (process nil)
        (result nil)
         (failure nil)
         (done-count 0)
         (error-count 0)
         (cancel-function nil)
         (events nil)
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
               (list :on-start (lambda (cancel)
                                 (setq cancel-function cancel))
                     :on-event (lambda (event)
                                 (push event events))
                     :on-done (lambda (value)
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
                (list result failure command done-count error-count
                      cancel-function events)))
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
    (should (= (nth 3 out) 1))
    (should (= (nth 4 out) 0))
    (should (functionp (nth 5 out)))
    (should-not (member "hello" (nth 2 out)))
    (should (member "@-" (nth 2 out)))))

(ert-deftest deslop-test/default-transport-accepts-text-completion-deltas ()
  (let ((out (deslop-test--run-default-transport
               "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n"
               "data: {\"choices\":[{\"text\":\"legacy\"}]}\n\ndata: [DONE]\n\n")))
    (should-not (nth 1 out))
    (should (string= (plist-get (nth 0 out) :content) "legacy"))))

(ert-deftest deslop-test/default-transport-retains-joined-partial-result ()
  (let* ((first
          "data: {\"choices\":[{\"finish_reason\":null,\"delta\":{\"reasoning_content\":\"Sub\"}}]}\n\n")
         (second
          "data: {\"choices\":[{\"finish_reason\":\"length\",\"delta\":{\"reasoning_content\":\"headings\"}}]}\n\n")
         (out (deslop-test--run-default-transport
               "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n"
               (concat first second
                       "data: {\"choices\":[],\"usage\":{\"completion_tokens\":4096}}\n\n"
                       "data: [DONE]\n\n")))
         (partial (cl-find-if
                   (lambda (event)
                     (eq (plist-get event :type) :partial-result))
                   (nth 6 out))))
    (should partial)
    (should (string= (plist-get (plist-get partial :result) :thinking)
                     "Subheadings"))
    (should (string= (plist-get (plist-get partial :result) :finish-reason)
                     "length"))))

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

(ert-deftest deslop-test/openai-profile-uses-opencode-oauth ()
  (let ((profile (deslop-provider-profile 'openai)))
    (should (eq (plist-get profile :wire-api) 'responses))
    (should (eq (plist-get profile :auth-method) 'opencode-oauth))
    (should (string= (plist-get profile :model) "gpt-5.6-luna"))
    (should (string= (plist-get profile :url)
                     "https://chatgpt.com/backend-api/codex/responses"))
    (should-not (plist-get (deslop-provider-profile-snapshot 'openai) :access))
    (should-not (plist-get (deslop-provider-profile-snapshot 'openai) :refresh))))

(ert-deftest deslop-test/provider-list-and-configured-models-are-selectable ()
  (let ((deslop-providers '(openai local))
        (deslop-provider-profiles
         '((local :models ("local-one" "local-two")))))
    (should (equal (deslop-provider-names) '(openai local)))
    (should (equal (deslop-provider-models
                    (deslop-provider-profile 'local))
                   '("local-one" "local-two")))))

(ert-deftest deslop-test/openai-model-probe-parses-official-slugs ()
  (should (equal
           (deslop-codex--parse-models
            (concat "{\"models\":[{\"slug\":\"gpt-5.6-luna\"},"
                    "{\"slug\":\"gpt-5.5\"},{\"title\":\"ignored\"}]}"))
           '("gpt-5.6-luna" "gpt-5.5"))))

(ert-deftest deslop-test/selector-runs-from-source-with-selected-profile ()
  (let* ((source (generate-new-buffer " *deslop-selector-source*"))
         (selector (generate-new-buffer " *deslop-selector-test*"))
         (deslop-providers '(local))
         (deslop-provider-profiles '((local :models ("one" "two"))))
         (queued nil))
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert "source"))
          (with-current-buffer selector
            (deslop-selector-mode)
            (setq deslop-selector--source-buffer source
                  deslop-selector--providers '(local)
                  deslop-selector--provider-index 0
                  deslop-selector--models '("one" "two")
                  deslop-selector--model-index 0)
            (deslop-selector-next-model)
            (cl-letf (((symbol-function 'deslop--enqueue-job)
                       (lambda (type instruction profile)
                         (setq queued
                               (list type instruction profile (current-buffer))))))
               (deslop-selector-insert))))
          (should (eq (nth 0 queued) 'insert))
          (should (string= (plist-get (nth 2 queued) :model) "two"))
          (should (eq (nth 3 queued) source)))
      (dolist (buffer (list source selector))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))))

(ert-deftest deslop-test/openai-request-body-matches-codex-responses-shape ()
  (let* ((body (deslop-protocol--json-read-string
                (json-encode
                 (deslop-codex--request-body
                  (list (cons "system" "rules")
                        (cons "user" "question")
                        (cons "assistant" "answer"))
                  '(:model "gpt-test")))))
         (input (deslop-protocol--json-get "input" body))
         (user (aref input 1))
         (user-content (aref (deslop-protocol--json-get "content" user) 0))
         (assistant (aref input 2))
         (assistant-content
          (aref (deslop-protocol--json-get "content" assistant) 0)))
    (should (eq (deslop-protocol--json-get "store" body) :false))
    (should (eq (deslop-protocol--json-get "stream" body) t))
    (should (string= (deslop-protocol--json-get "text" user-content)
                     "question"))
    (should (string= (deslop-protocol--json-get "type" user-content)
                     "input_text"))
    (should (string= (deslop-protocol--json-get "text" assistant-content)
                     "answer"))
    (should (string= (deslop-protocol--json-get "type" assistant-content)
                     "output_text"))))

(ert-deftest deslop-test/openai-http-error-keeps-provider-detail ()
  (let ((headers-file (make-temp-file "deslop-openai-error-")))
    (unwind-protect
        (progn
          (with-temp-file headers-file
            (insert "HTTP/1.1 400 Bad Request\r\n\r\n"))
          (let* ((session
                  (deslop-codex--make-session
                   :headers-file headers-file
                   :preview-parts '("{\"detail\":\"invalid request body\"}")))
                 (error (deslop-codex--http-error session)))
            (should (string= (deslop-error-message error)
                             "HTTP status 400: invalid request body"))))
      (when (file-exists-p headers-file)
        (delete-file headers-file)))))

(ert-deftest deslop-test/openai-auth-entry-is-read-from-opencode-file ()
  (let* ((file (make-temp-file "deslop-openai-auth-test-"))
         (deslop-codex-auth-file file))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "{\"openai\":{\"type\":\"oauth\","
                    "\"access\":\"access-token\","
                    "\"refresh\":\"refresh-token\","
                    "\"expires\":9999999999999,"
                    "\"accountId\":\"account-id\"}}"))
          (let ((auth (deslop-codex--auth-entry)))
            (should (string= (plist-get auth :access) "access-token"))
            (should (string= (plist-get auth :refresh) "refresh-token"))
            (should (string= (plist-get auth :account-id) "account-id"))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest deslop-test/openai-transport-keeps-token-out-of-command-arguments ()
  (let* ((auth-file (make-temp-file "deslop-openai-auth-test-"))
         (deslop-codex-auth-file auth-file)
         (captured nil)
         (process nil)
         (result nil)
         (real-make-process (symbol-function 'make-process)))
    (unwind-protect
        (progn
          (with-temp-file auth-file
            (insert "{\"openai\":{\"type\":\"oauth\","
                    "\"access\":\"access-token\","
                    "\"refresh\":\"refresh-token\","
                    "\"expires\":9999999999999,"
                    "\"accountId\":\"account-id\"}}"))
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest args)
                       (setq captured args)
                       (setq process
                             (funcall real-make-process
                                      :name (plist-get args :name)
                                      :buffer (plist-get args :buffer)
                                      :command (list "cat")
                                      :connection-type 'pipe)))))
            (deslop-codex-default
             (list (cons "user" "body"))
             (list :on-done (lambda (value) (setq result value))
                   :on-error (lambda (value) (setq result value)))
             (deslop-provider-profile 'openai))
            (let* ((command (plist-get captured :command))
                   (config (cadr (member "--config" command))))
              (should (not (member "access-token" command)))
              (should (file-exists-p config))
               (with-temp-buffer
                 (insert-file-contents config)
                 (should (string-match-p "Authorization: Bearer access-token"
                                         (buffer-string))))
               (with-temp-buffer
                 (insert-file-contents config)
                 (should (string-match-p "User-Agent: deslop"
                                         (buffer-string))))
              (funcall
               (plist-get captured :filter)
               process
               (concat "data: {\"type\":\"response.output_text.delta\","
                       "\"delta\":\"answer\"}\n\n"
                       "data: {\"type\":\"response.completed\","
                       "\"response\":{\"model\":\"gpt-test\"}}\n\n"))
              (should (string= (plist-get result :content) "answer"))
              (should-not (file-exists-p config)))))
      (when (and (processp process) (process-live-p process))
        (delete-process process))
      (when (file-exists-p auth-file)
        (delete-file auth-file)))))

(ert-deftest deslop-test/openai-refresh-rotates-opencode-credentials-safely ()
  (let* ((auth-file (make-temp-file "deslop-openai-auth-test-"))
         (deslop-codex-auth-file auth-file)
         (deslop-codex-issuer "https://example.test")
         (captured nil)
         (process nil)
         (refreshed nil)
         (real-make-process (symbol-function 'make-process))
         (auth '(:access "old-access" :refresh "old-refresh"
                 :expires 0 :account-id "account-id")))
    (unwind-protect
        (progn
          (with-temp-file auth-file
            (insert "{\"openai\":{\"type\":\"oauth\","
                    "\"access\":\"old-access\","
                    "\"refresh\":\"old-refresh\","
                    "\"expires\":0,"
                    "\"accountId\":\"account-id\"}}"))
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest args)
                       (setq captured args)
                       (setq process
                             (funcall real-make-process
                                      :name (plist-get args :name)
                                      :buffer (plist-get args :buffer)
                                      :stderr (plist-get args :stderr)
                                      :command (list "cat")
                                      :connection-type 'pipe
                                      :noquery t)))))
            (deslop-codex--refresh-auth auth
                                        (lambda (value) (setq refreshed value)))
            (let* ((command (plist-get captured :command))
                   (headers-file
                    (cadr (member "--dump-header" command)))
                   (filter (plist-get captured :filter))
                   (sentinel (plist-get captured :sentinel)))
              (should-not (member "old-refresh" command))
              (with-temp-file headers-file
                (insert "HTTP/1.1 200 OK\r\n\r\n"))
              (with-current-buffer (plist-get captured :buffer)
                (erase-buffer))
              (when (process-live-p process)
                (process-send-eof process))
              (while (process-live-p process)
                (accept-process-output process 0.05))
              (with-current-buffer (plist-get captured :buffer)
                (erase-buffer))
              (funcall filter process
                       (concat "{\"access_token\":\"new-access\","
                               "\"refresh_token\":\"new-refresh\","
                               "\"expires_in\":3600}"))
              (funcall sentinel process "finished\n")
              (should (string= (plist-get refreshed :access) "new-access"))
              (should (string= (plist-get refreshed :refresh) "new-refresh"))
              (should (string= (plist-get (deslop-codex--auth-entry) :refresh)
                               "new-refresh")))))
      (when (and (processp process) (process-live-p process))
        (delete-process process))
      (when (file-exists-p auth-file)
        (delete-file auth-file)))))

(ert-deftest deslop-test/openai-responses-events-normalize-to-deslop-result ()
  (let* ((events nil)
         (result nil)
         (failure nil)
         (session
         (deslop-codex--make-session
          :profile '(:model "gpt-test")
          :callbacks (list :on-event (lambda (event) (push event events))
                           :on-done (lambda (value) (setq result value))
                           :on-error (lambda (value) (setq failure value)))
          :sse (deslop-protocol--make-sse :buffer-parts nil :data-lines nil)
          :estimated-chars 0)))
    (deslop-codex--handle-event
     session
     (deslop-protocol--json-read-string
      "{\"type\":\"response.created\",\"response\":{\"model\":\"gpt-actual\"}}"))
    (deslop-codex--handle-event
     session
     (deslop-protocol--json-read-string
      "{\"type\":\"response.reasoning_summary_text.delta\",\"delta\":\"thinking\"}"))
    (deslop-codex--handle-event
     session
     (deslop-protocol--json-read-string
      "{\"type\":\"response.output_text.delta\",\"delta\":\"answer\"}"))
    (deslop-codex--handle-event
     session
     (deslop-protocol--json-read-string
      "{\"type\":\"response.completed\",\"response\":{\"model\":\"gpt-actual\",\"usage\":{\"output_tokens\":7}}}"))
    (should-not failure)
    (should (string= (plist-get result :content) "answer"))
    (should (string= (plist-get result :thinking) "thinking"))
    (should (string= (plist-get result :model) "gpt-actual"))
    (should (= (plist-get result :tokens) 7))
    (should (eq (deslop-codex--session-terminal session) 'success))))

;;;; Profiles, history, and transport command safety

(ert-deftest deslop-test/profile-is-snapshotted-at-enqueue ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(:stall) t))
        (deslop-provider-endpoint "http://one.example/v1"))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (let ((job (deslop-insert-at-cursor)))
        (setq deslop-provider-endpoint "http://two.example/v1")
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
      (setq config (deslop-curl--config profile))
      (unwind-protect
          (let ((command (deslop-curl--command
                          profile "/tmp/headers" config)))
            (should-not (member token command))
            (should-not (member "body" command))
            (should-not (member "--fail-with-body" command)))
         (when (file-exists-p config) (delete-file config))))))

(ert-deftest deslop-test/unbounded-output-omits-provider-token-limit ()
  (let ((deslop-max-output-tokens nil))
    (let ((body (deslop-llm--request-body
                 (list (cons "user" "body"))
                 '(:model "test-model" :temperature 0.2))))
      (should-not (alist-get :max_tokens body))
      (should-not (string-match-p "max_tokens" (json-encode body))))))

(ert-deftest deslop-test/curl-config-escapes-and-rejects-unsafe-headers ()
  (let* ((profile '(:url "https://example.test/v1"
                    :request-headers ("X-Title: a\"b\\c")))
         (config (deslop-curl--config profile)))
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
      (deslop-curl--config
      (list :url "https://example.test/v1"
            :request-headers (list header))))))

(ert-deftest deslop-test/provider-url-and-curl-command-are-hardened ()
  (should-error
    (deslop-curl--validated-url '(:url "http://127.attacker.test/v1")))
  (should-error
    (deslop-curl--validated-url '(:url "https://example.test/v1?key=secret")))
  (should-error
    (deslop-curl--validated-url
    '(:url "https://other.test/v1"
      :auth-source-host "example.test"
      :auth-source-user "deslop"
      :auth-source-port 443)))
  (let ((command
         (deslop-curl--command
          '(:url "https://example.test/v1") "/tmp/headers" nil)))
    (should (string= (nth 1 command) "--disable"))
    (should (member "--globoff" command))
    (should (member "--noproxy" command))
     (should (member "--url" command))))

(ert-deftest deslop-test/curl-command-uses-idle-not-total-timeout-by-default ()
  (let ((deslop-request-timeout nil)
        (deslop-response-idle-timeout 60))
    (let ((command (deslop-curl--command
                    '(:url "https://example.test/v1") "/tmp/headers" nil)))
      (should-not (member "--max-time" command))
      (should (equal (cadr (member "--speed-time" command)) "60"))
      (should (equal (cadr (member "--speed-limit" command)) "1")))))

(ert-deftest deslop-test/json-parser-does-not-intern-keys-or-accept-garbage ()
  (let ((key (format "deslop-untrusted-key-%d" (random most-positive-fixnum))))
    (should-not (intern-soft key))
    (let ((payload (deslop-protocol--json-read-string
                    (format "{\"%s\":1}" key))))
      (should (hash-table-p payload))
      (should (= (deslop-protocol--json-get key payload) 1))
      (should-not (intern-soft key))))
  (should-error (deslop-protocol--json-read-string "{\"ok\":1} garbage")))

(ert-deftest deslop-test/default-transport-enforces-response-byte-limit ()
  (let* ((deslop-max-response-bytes 16)
         (out (deslop-test--run-default-transport
               "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n"
               (make-string 100 ?X))))
    (should-not (nth 0 out))
    (should (eq (deslop-error-kind (nth 1 out)) 'resource))
    (should (= (nth 4 out) 1))))

(ert-deftest deslop-test/response-byte-limit-retains-safe-prefix ()
  (let* ((prefix
          "data: {\"choices\":[{\"delta\":{\"content\":\"kept\"}}]}\n\n")
         (deslop-max-response-bytes (string-bytes prefix))
         (out (deslop-test--run-default-transport
               "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n"
               (concat prefix (make-string 100 ?X))))
         (partial (cl-find-if
                   (lambda (event)
                     (eq (plist-get event :type) :partial-result))
                   (nth 6 out))))
    (should-not (nth 0 out))
    (should (eq (deslop-error-kind (nth 1 out)) 'resource))
    (should partial)
    (should (string= (plist-get (plist-get partial :result) :content)
                     "kept"))))

(ert-deftest deslop-test/sse-parser-enforces-line-and-event-limits ()
  (let ((deslop-max-sse-event-bytes 8)
        (parser (deslop-protocol--make-sse :buffer-parts nil :data-lines nil
                                      :data-bytes 0 :done nil)))
    (should-error (deslop-protocol--sse-feed parser "123456789")))
  (let ((deslop-max-sse-event-bytes 8)
        (parser (deslop-protocol--make-sse :buffer-parts nil :data-lines nil
                                      :data-bytes 0 :done nil)))
    (should-error
     (deslop-protocol--sse-feed
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
           (deslop-protocol--json-read-string
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
           (deslop-protocol--json-read-string
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
        (deslop-provider-max-discovery-bytes 4)
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
          (should (string-match-p "base" text))
          (should (eq buffer-undo-list t)))))))

(ert-deftest deslop-test/history-retains-failed-stream-and-raw-usage ()
  (deslop-test--reset)
  (let ((deslop-llm-function
         (deslop-test--fake-llm
          '((:thinking "reasoning before failure"
             :content ""
             :tokens 4096
             :speed 58.4
             :usage (:prompt_tokens 12
                     :completion_tokens 4096
                     :reasoning_tokens 4000
                     :total_tokens 4108))))))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (let ((job (deslop-insert-at-cursor)))
        (should (eq (deslop-job-state job) 'failed)))
      (with-current-buffer deslop-history-buffer-name
        (let ((text (buffer-string)))
          (should (string-match-p "Prompt" text))
          (should (string-match-p "base" text))
          (should (string-match-p "Thinking" text))
          (should (string-match-p "reasoning before failure" text))
          (should (string-match-p "reasoning_tokens" text))
          (should (string-match-p "empty completion" text))
          (should buffer-read-only)
          (should-error (insert "user edit") :type 'buffer-read-only)
          (let ((buffer-read-only nil))
            (should-error (insert "user edit") :type 'user-error)))))))

(ert-deftest deslop-test/history-retains-joined-partial-result-on-failure ()
  (deslop-test--reset)
  (let ((deslop-llm-function
         (deslop-test--fake-llm
          '((:content ""
             :partial (:content ""
                       :thinking "Subheadings: ** TODO"
                       :tokens 4096
                       :speed 57.1
                       :finish-reason "length"))))))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (let ((job (deslop-insert-at-cursor)))
        (should (eq (deslop-job-state job) 'failed)))
      (with-current-buffer deslop-history-buffer-name
        (let ((text (buffer-string)))
          (should (string-match-p "Thinking" text))
          (should (string-match-p (regexp-quote "Subheadings: ** TODO")
                                  text))
          (should (string-match-p "Tokens: 4096" text))
          (should (string-match-p "Finish reason: length" text))
          (should-not (string-match-p "data:" text)))))))

(ert-deftest deslop-test/history-command-displays-buffer ()
  (deslop-test--reset)
  (let ((displayed nil))
    (cl-letf (((symbol-function 'display-buffer)
               (lambda (buffer &rest _) (setq displayed buffer))))
      (deslop-history)
      (should (eq displayed (get-buffer deslop-history-buffer-name)))))
  (should (eq (lookup-key deslop-mode-map (kbd "C-c e"))
              #'deslop-selector-open)))

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
        (deslop-insert-at-cursor)
        (deslop-review-delete-overlay
         (cl-find-if #'deslop-review-overlay-p
                     (overlays-in (point-min) (point-max)))))
      (with-current-buffer deslop-history-buffer-name
        (let ((text (buffer-string)))
           (should (= (deslop-test--count-matches "^\\* Job " text) 2))
           (should-not (string-match-p "^\\* fake heading" text)))))))

(ert-deftest deslop-test/history-replaces-oldest-entries-at-default-limit ()
  (deslop-test--reset)
  (let ((deslop-history-max-entries 100)
        (deslop-llm-function
         (deslop-test--fake-llm
          (cl-loop for index from 0 to 102
                   collect (format " item-%d" index)))))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (dotimes (_ 103)
        (deslop-insert-at-cursor)
        (deslop-review-delete-overlay
         (cl-find-if #'deslop-review-overlay-p
                     (overlays-in (point-min) (point-max)))))
      (with-current-buffer deslop-history-buffer-name
        (let ((text (buffer-string)))
          (should (= (deslop-test--count-matches "^\\* Job " text) 100))
          (should-not (string-match-p "^\\* Job 1:" text))
          (should-not (string-match-p "^\\* Job 3:" text))
          (should (string-match-p "^\\* Job 103:" text)))))))

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
        (should (= (deslop-target-cursor (deslop-job-target job))
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
    (let ((first (deslop-review-create-overlay
                  (current-buffer) 1 6 nil nil ""))
          (second (deslop-review-create-overlay
                   (current-buffer) 7 13 nil nil "")))
      (narrow-to-region 1 6)
      (deslop-review-delete-overlay first)
      (should (memq #'deslop-review--prevent-save before-save-hook))
      (should-error (run-hooks 'before-save-hook) :type 'user-error)
      (widen)
      (deslop-review-delete-overlay second)
      (should-not (memq #'deslop-review--prevent-save
                        before-save-hook)))))

;;;; Review highlight

(ert-deftest deslop-test/review-highlight-blend-color-math ()
  (should (string= (deslop-review--blend-color "#ffffff" "#0000ff" 0.15) "#d9d9ff"))
  (should (string= (deslop-review--blend-color "#000000" "#ffffff" 0.5) "#808080"))
  (should (string= (deslop-review--blend-color "#333333" "#ffffff" 0.0) "#333333"))
  (should (string= (deslop-review--blend-color "#333333" "#ffffff" 1.0) "#ffffff"))
  (should (null (deslop-review--blend-color "#abc" "#ffffff" 0.5)))
  (should (null (deslop-review--blend-color "#ffffff" "junk" 0.5))))

(ert-deftest deslop-test/review-highlight-hex-round-trip ()
  (should (equal (deslop-review--hex-components "#a1b2c3") '(161 178 195)))
  (should (string= (deslop-review--hex-join '(161 178 195)) "#a1b2c3"))
  (should (null (deslop-review--hex-components "red")))
  (should (null (deslop-review--hex-components "#12345")))
  (should (null (deslop-review--hex-components "#1234567")))
  (should (null (deslop-review--hex-join '(1 2)))))

(ert-deftest deslop-test/review-highlight-face-attr-resolution ()
  (set-face-foreground 'font-lock-keyword-face "#0000ff")
  (set-face-background 'font-lock-keyword-face "#ffffff")
  (should (string= (deslop-review--face-attr 'font-lock-keyword-face :foreground)
                   "#0000ff"))
  (should (string= (deslop-review--face-attr 'font-lock-keyword-face :background)
                   "#ffffff"))
  (should (string= (deslop-review--face-attr
                    '(:foreground "red" :background "blue") :foreground)
                   "red"))
  (should (string= (deslop-review--face-attr
                    '((:foreground "cyan") (:weight bold)) :foreground)
                   "cyan"))
  (should (string= (deslop-review--face-attr
                    '((:foreground "cyan") (:background "magenta")) :background)
                   "magenta"))
  (should (null (deslop-review--face-attr '(:weight bold) :foreground))))

(ert-deftest deslop-test/review-highlight-style-from-adjacent-face ()
  (set-face-foreground 'font-lock-keyword-face "#0000ff")
  (set-face-background 'font-lock-keyword-face "#ffffff")
  (deslop-test--with-buffer "defun x"
    (add-text-properties 1 6 '(face font-lock-keyword-face))
    (should (equal (deslop-review--style (current-buffer) 6)
                   '("#0000ff" . "#d9d9ff")))))

(ert-deftest deslop-test/review-highlight-style-default-fallback ()
  (set-face-foreground 'default "#111111")
  (set-face-background 'default "#222222")
  (deslop-test--with-buffer "plain x"
    (should (equal (deslop-review--style (current-buffer) 6)
                   '("#111111" . "#1f1f1f")))
    (should (equal (deslop-review--style (current-buffer) 1)
                   '("#111111" . "#1f1f1f")))))

(ert-deftest deslop-test/review-highlight-dark-background-light-foreground ()
  (set-face-foreground 'default "#e6e6e6")
  (set-face-background 'default "#1e1e1e")
  (deslop-test--with-buffer "plain text"
    (should (equal (deslop-review--style (current-buffer) 6)
                   '("#e6e6e6" . "#3c3c3c")))))

(ert-deftest deslop-test/review-highlight-overlay-sets-face ()
  (deslop-test--reset)
  (set-face-foreground 'font-lock-keyword-face "#0000ff")
  (set-face-background 'font-lock-keyword-face "#ffffff")
  (deslop-test--with-buffer "defunLLM"
    (add-text-properties 1 6 '(face font-lock-keyword-face))
    (let ((overlay (deslop-review-create-overlay
                    (current-buffer) 6 9 nil nil "")))
      (should (overlayp overlay))
      (should (equal (overlay-get overlay 'face)
                     '(:foreground "#0000ff" :background "#d9d9ff")))
      (should (eq (get-char-property 3 'face) 'font-lock-keyword-face))
      (should (null (get-char-property 3 'foreground-color)))
      (should (memq #'deslop-review--prevent-save before-save-hook))
      (deslop-review-delete-overlay overlay)
      (should (eq (get-char-property 3 'face) 'font-lock-keyword-face)))))

(ert-deftest deslop-test/review-highlight-disabled-sets-no-properties ()
  (deslop-test--reset)
  (deslop-test--with-buffer "defunLLM"
    (add-text-properties 1 6 '(face font-lock-keyword-face))
    (let ((deslop-review-highlight nil))
      (let ((overlay (deslop-review-create-overlay
                      (current-buffer) 6 9 nil nil "")))
        (should (null (overlay-get overlay 'face)))))))

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
      (let ((review (cl-find-if #'deslop-review-overlay-p
                                (overlays-in (point-min) (point-max)))))
        (should review)
        (should (equal (overlay-get review 'face)
                       '(:foreground "#0000ff" :background "#d9d9ff")))
        (goto-char 4)
        (deslop-approve-output)
        (should (string= (buffer-string) "kwLLM rest"))
        (should-not (deslop-review-overlay-p review))))))

(ert-deftest deslop-test/review-highlight-rejection-preserves-original-properties ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '("new"))))
    (deslop-test--with-buffer "old text"
      (add-text-properties 1 4 '(face font-lock-string-face))
      (add-text-properties 1 4 '(foreground-color "#123456"))
      (deslop-test--select 1 9 1)
      (deslop-improve-at-cursor "rewrite")
      (should (string= (buffer-string) "new"))
      (let ((review (cl-find-if #'deslop-review-overlay-p
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
