;;; ende-queue-test.el --- Queue lifecycle tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Coverage for FIFO ordering, cancellation, stale callbacks, and settlement.

;;; Code:

(require 'ert)
(require 'ende)
(require 'ende-test-helpers)
(require 'cl-lib)



(defun ende-test--sleep-process ()
  "Create a process that remains live until explicitly terminated."
  (make-process :name (generate-new-buffer-name "ende-test-sleep")
                :command (list "sleep" "30")
                :connection-type 'pipe
                :noquery t))



(ert-deftest ende-test/queue-snapshots-do-not-expose-list-state ()
  (let* ((active-job (ende--make-job :id 1 :state 'running))
         (pending-job (ende--make-job :id 2 :state 'queued))
         (ende--active-jobs (list active-job))
         (ende-queue (list pending-job)))
    (let ((active (ende-active-jobs))
          (pending (ende-pending-jobs)))
      (setcar active :changed)
      (setcar pending :changed)
      (should (eq (car ende--active-jobs) active-job))
      (should (eq (car ende-queue) pending-job))
      (should (cl-every #'ende-job-p (ende-active-jobs)))
      (should (cl-every #'ende-job-p (ende-pending-jobs))))))

;;;; Queue lifecycle

(ert-deftest ende-test/queue-fifo-ordering-async ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm '(" 1" " 2" " 3") t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (setq ende--queue-suspended t)
			    (ende-insert-at-cursor)
			    (ende-insert-at-cursor)
			    (ende-insert-at-cursor)
			    (setq ende--queue-suspended nil)
			    (ende-process-next)
			    (should (= (length ende-test--calls) 1))
			    (ende-test--complete-call (nth 0 ende-test--calls))
			    (ende-test--complete-call (nth 1 ende-test--calls))
			    (ende-test--complete-call (nth 2 ende-test--calls))
			    (should (string= (buffer-string) "base 1 2 3"))
			    (should (null ende-queue))
			    (should (null ende--current-job)))))



(ert-deftest ende-test/abort-invalidates-before-kill ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm '(:stall "never") t
                              #'ende-test--sleep-process)))
    (ende-test--with-buffer "content"
			    (let ((job (ende-insert-at-cursor)))
			      (ende-insert-at-cursor)
			      (let ((call (car ende-test--calls)))
				(should ende--current-job)
				(ende-abort-queue)
				(should (eq (ende-job-state job) 'cancelled))
				(should-not ende--current-job)
				(should (null ende-queue))
				(should-not (process-live-p (plist-get call :process)))
				;; A late transport callback must be inert.
				(ende-test--callback call :on-done '(:content "orphan"))
				(should (string= (buffer-string) "content")))))))



(ert-deftest ende-test/stale-callback-cannot-settle-next-job ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm '(" A" " B") t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (setq ende--queue-suspended t)
			    (ende-insert-at-cursor)
			    (ende-insert-at-cursor)
			    (setq ende--queue-suspended nil)
			    (ende-process-next)
			    (let ((first (nth 0 ende-test--calls)))
			      (ende-test--complete-call first)
			      (let ((second (nth 1 ende-test--calls)))
				(should second)
				(should (eq (ende-job-state ende--current-job) 'running))
				(ende-test--callback first :on-done '(:content " stale"))
				(ende-test--callback first :on-error "stale error")
				(should (eq (ende-job-state ende--current-job) 'running))
				(ende-test--complete-call second)))
			    (should (string= (buffer-string) "base A B")))))



(ert-deftest ende-test/duplicate-completion-is-idempotent ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" X") t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((job (ende-insert-at-cursor))
				  (call nil))
			      (setq call (car ende-test--calls))
			      (ende-test--complete-call call)
			      (ende-test--callback call :on-done '(:content " X"))
			      (should (eq (ende-job-state job) 'completed))
			      (should (string= (buffer-string) "base X"))
			      (should-not ende--current-job)))))



(ert-deftest ende-test/malformed-structured-error-cannot-wedge-queue ()
  (ende-test--reset)
  (let ((ende-llm-function
         (lambda (_messages callbacks &optional _options)
           (funcall (plist-get callbacks :on-error)
                    (ende-error-create :kind 'provider :message 123)))))
    (ende-test--with-buffer "base"
			    (let ((job (ende-insert-at-cursor)))
			      (should (eq (ende-job-state job) 'failed))
			      (should-not ende--current-job)
			      (should-not ende--current-job)))))



(ert-deftest ende-test/synchronous-failure-backlog-is-iterative ()
  (ende-test--reset)
  (ende-test--with-buffer "base"
			  (let ((ende--queue-suspended t)
				(max-lisp-eval-depth 100))
			    (dotimes (_ 500)
			      (ende-enqueue
			       (list 'unknown
				     :target (ende-target-create
					      :buffer (current-buffer) :start 1 :end 1 :cursor 1))))
			    (setq ende--queue-suspended nil)
			    (cl-letf (((symbol-function 'message) #'ignore))
			      (ende-process-next))
			    (should-not ende--current-job)
			    (should-not ende-queue))))



(ert-deftest ende-test/quit-and-cleanup-errors-still-settle-jobs ()
  (ende-test--reset)
  (let ((ende--job-handlers (copy-tree ende--job-handlers)))
    (ende-register-job-handler
     'quit-test (lambda (_job) (signal 'quit nil)))
    (ende-test--with-buffer "base"
			    (let ((job (ende-enqueue
					(list 'quit-test
					      :target (ende-target-create
						       :buffer (current-buffer)
						       :start 1 :end 1 :cursor 1)))))
			      (should (eq (ende-job-state job) 'failed))
			      (should-not ende--current-job))))
  (ende-test--reset)
  (let ((ende--job-handlers (copy-tree ende--job-handlers)))
    (ende-register-job-handler
     'cleanup-test (lambda (job) (ende-job-finish job "ok")))
    (ende-test--with-buffer "base"
			    (let* ((overlay (ende-ui-create-overlay
					     (current-buffer) 1 1 "Queued"))
				   (job nil))
			      (cl-letf (((symbol-function 'ende-ui-delete-overlay)
					 (lambda (_) (error "cleanup failed"))))
				(setq job
				      (ende-enqueue
				       (list 'cleanup-test
					     :target (ende-target-create
						      :buffer (current-buffer)
						      :start 1 :end 1 :cursor 1)
					     :overlay overlay))))
			      (should (eq (ende-job-state job) 'completed))
			      (should-not ende--current-job)
			      (delete-overlay overlay)))))

;;;; Target failure paths

(ert-deftest ende-test/killed-source-buffer-cancels-active-and-queued-jobs ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" X") t))
        (buffer (generate-new-buffer " *ende-killed*"))
        (active nil)
        (pending nil))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "base")
            (goto-char (point-max))
            (setq active (ende-insert-at-cursor)))
          (let ((ende--queue-suspended t))
            (with-current-buffer buffer
              (setq pending (ende-insert-at-cursor))))
          (should (eq (ende-job-state active) 'running))
          (should (eq (ende-job-state pending) 'queued))
          (kill-buffer buffer)
          (should (eq (ende-job-state active) 'cancelled))
          (should (eq (ende-job-state pending) 'cancelled))
          (should-not ende--current-job)
          (should-not ende-queue)
          (ende-test--callback (car ende-test--calls) :on-done '(:content "late"))
          (should (eq (ende-job-state active) 'cancelled)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))



(ert-deftest ende-test/source-buffer-cancellation-preserves-outer-abort ()
  (ende-test--reset)
  (let ((first (generate-new-buffer " *ende-abort-first*"))
        (second (generate-new-buffer " *ende-abort-second*"))
        (observed nil)
        (first-job nil)
        (second-job nil))
    (unwind-protect
        (progn
          (with-current-buffer first
            (insert "first")
            (setq first-job
                  (ende--make-job
                   :id 1 :state 'running
                   :target (ende-target-create
                            :buffer first :start 1 :end 1 :cursor 1)
                   :cancel-function
                   (lambda ()
                     (kill-buffer second)
                     (setq observed ende--aborting)))
                  ende--source-buffer-jobs (list first-job))
            (add-hook 'kill-buffer-hook #'ende--source-buffer-killed nil t))
          (with-current-buffer second
            (insert "second")
            (setq second-job
                  (ende--make-job
                   :id 2 :state 'queued
                   :target (ende-target-create
                            :buffer second :start 1 :end 1 :cursor 1)))
            (setq ende--source-buffer-jobs (list second-job))
            (add-hook 'kill-buffer-hook #'ende--source-buffer-killed nil t))
          (setq ende--active-jobs (list first-job)
                ende--current-job first-job
                ende-queue (list second-job)
                ende--queue-tail ende-queue)
          (ende-abort-queue)
          (should observed)
          (should-not ende--aborting)
          (should (eq (ende-job-state first-job) 'cancelled))
          (should (eq (ende-job-state second-job) 'cancelled)))
      (when (buffer-live-p first) (kill-buffer first))
      (when (buffer-live-p second) (kill-buffer second)))))



(ert-deftest ende-test/read-only-target-settles-job ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" X") t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((job (ende-insert-at-cursor))
				  (call nil))
			      (setq call (car ende-test--calls))
			      (setq buffer-read-only t)
			      (ende-test--complete-call call)
			      (should (eq (ende-job-state job) 'failed))
			      (should-not ende--current-job)
			      (should (string= (buffer-string) "base"))))))



(ert-deftest ende-test/narrowed-target-settles-job ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" X") t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((job (ende-insert-at-cursor))
				  (call nil))
			      (setq call (car ende-test--calls))
			      (narrow-to-region (point-min) (1- (point-max)))
			      (ende-test--complete-call call)
			      (should (eq (ende-job-state job) 'failed))
			      (should-not ende--current-job)))))



(ert-deftest ende-test/widening-does-not-expand-queued-context ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" X") t)))
    (ende-test--with-buffer "SECRET safe SECRET"
			    (narrow-to-region 8 12)
			    (goto-char 10)
			    (setq ende--queue-suspended t)
			    (ende-insert-at-cursor)
			    (widen)
			    (setq ende--queue-suspended nil)
			    (ende-process-next)
			    (let ((user-message
				   (cdr (cadr (plist-get (car ende-test--calls) :messages)))))
			      (should (string-match-p "sa<CURSOR>fe" user-message))
			      (should-not (string-match-p "SECRET" user-message))))))



(ert-deftest ende-test/modification-hook-settles-job ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" X") t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((job (ende-insert-at-cursor))
				  (call nil))
			      (setq call (car ende-test--calls))
			      (add-hook 'before-change-functions
					(lambda (&rest _) (error "blocked")) nil t)
			      (ende-test--complete-call call)
			      (should (eq (ende-job-state job) 'failed))
			      (should (string= (buffer-string) "base"))
			      (should-not ende--current-job)))))

(ert-deftest ende-test/same-endpoint-preserves-fifo ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("A" "B") t)))
    (ende-test--with-buffer "alpha"
			    (ende-test--select 1 6 6)
			    (setq ende--queue-suspended t)
			    (ende-insert-at-cursor)
			    (ende-insert-at-cursor)
			    (setq ende--queue-suspended nil)
			    (ende-process-next)
			    (ende-test--complete-call (nth 0 ende-test--calls))
			    (ende-test--complete-call (nth 1 ende-test--calls))
			    (should (string= (buffer-string) "alpha\nA\nB")))))

;;;; Profiles, history, and transport command safety

(ert-deftest ende-test/profile-is-snapshotted-at-enqueue ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(:stall) t))
        (ende-provider-endpoint "http://one.example/v1")
        (ende-provider-profiles '((local :temperature 0.2))))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((job (ende-insert-at-cursor)))
			      (setq ende-provider-endpoint "http://two.example/v1"
				    ende-provider-profiles
				    '((local :temperature 0.9
					     :controls ((:temperature :path ("changed"))))))
			      (should (string= (plist-get (ende-job-profile job) :url)
					       "http://one.example/v1"))
			      (should (= (plist-get (ende-job-profile job) :temperature) 0.2))
			      (should (equal (plist-get (ende-job-profile job) :controls)
					     '((:temperature :path ("temperature")))))))))

(provide 'ende-queue-test)

;;; ende-queue-test.el ends here
