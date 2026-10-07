;;; ende-queue-scheduler-test.el --- Scheduler control behavior -*- lexical-binding: t; -*-

;;; Commentary:

;; Deterministic coverage for queue mutations and scheduler readiness.

;;; Code:

(require 'ert)
(require 'ende-test-helpers)

(ert-deftest ende-test/queue-readiness-is-pure-and-respects-provider-order ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm nil t)))
    (ende-test--with-buffer "base"
			    (ende-queue-pause)
			    (let* ((first (ende-enqueue
					   (ende-test--dashboard-request
					    ende-test--dashboard-profile)))
				   (second (ende-enqueue
					    (ende-test--dashboard-request
					     ende-test--dashboard-profile)))
				   (entries (ende-queue-entries))
				   (explanation-one (ende-job-explanation first))
				   (explanation-two (ende-job-explanation second)))
			      (should (plist-get explanation-one :ready))
			      (should-not (plist-get explanation-two :ready))
			      (should (eq (plist-get (car (plist-get explanation-two :blockers)) :kind)
					  'behind))
			      (should (eq (ende-job-id
					   (plist-get (car (plist-get explanation-two :blockers)) :job))
					  (ende-job-id first)))
			      (ende-queue-status)
			      (should (equal entries (ende-queue-entries)))
			      (should-not ende--active-jobs)
			      (should (= (length ende-test--calls) 0))))))

(ert-deftest ende-test/queued-targeted-cancel-does-not-start-neighbor ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm nil t)))
    (ende-test--with-buffer "base"
			    (ende-queue-pause)
			    (let* ((cancelled (ende-enqueue
					       (ende-test--dashboard-request
						ende-test--dashboard-profile)))
				   (remaining (ende-enqueue
					       (ende-test--dashboard-request
						ende-test--dashboard-profile))))
			      (ende-queue-cancel-ids (list (ende-job-id cancelled)))
			      (should (eq (ende-job-state cancelled) 'cancelled))
			      (should (eq (ende-job-state remaining) 'queued))
			      (should (equal (ende-pending-jobs) (list remaining)))
			      (should (= (length ende-test--calls) 0))))))

(ert-deftest ende-test/targeted-batch-cancel-invalidates-before-callbacks ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" A" " B" " C") t)))
    (ende-test--with-buffer "base"
			    (let* ((first (ende-enqueue
					   (ende-test--dashboard-request
					    ende-test--dashboard-profile)))
				   (second (ende-enqueue
					    (ende-test--dashboard-request
					     ende-test--dashboard-profile)))
				   (third (ende-enqueue
					   (ende-test--dashboard-request
					    ende-test--dashboard-profile)))
				   callback-call-count)
			      (ende-job-cancellation-set
			       first
			       (lambda ()
				 (ende-job-finish second "late sibling completion")
				 (ende-job-finish third "late unrelated completion")
				 (ende-process-next)
				 (setq callback-call-count (length ende-test--calls))))
			      (ende-queue-cancel-ids (list (ende-job-id first) (ende-job-id second)))
			      (should (eq (ende-job-state first) 'cancelled))
			      (should (eq (ende-job-state second) 'cancelled))
			      (should (eq (ende-job-state third) 'running))
			      (should (= callback-call-count 1))
			      (should (= (length ende-test--calls) 2))
			      (should (equal ende--source-buffer-jobs (list third)))
			      (ende-test--complete-call (car ende-test--calls))
			      (should (eq (ende-job-state third) 'running))
			      (ende-test--complete-call (nth 1 ende-test--calls))
			      (should (eq (ende-job-state third) 'completed))))))

(ert-deftest ende-test/fences-follow-queue-position-across-multiple-moves ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm nil t)))
    (ende-test--with-buffer "base"
			    (ende-queue-pause)
			    (let* ((first (ende-enqueue
					   (ende-test--dashboard-request
					    ende-test--dashboard-profile)))
				   (second (ende-enqueue
					    (ende-test--dashboard-request
					     ende-test--dashboard-profile)))
				   (fence-one (ende-fence-all))
				   (third (ende-enqueue
					   (ende-test--dashboard-request
					    ende-test--dashboard-profile)))
				   (fence-two (ende-fence-all)))
			      (should (eq (car (ende-queue-entries)) first))
			      (should (eq (nth 2 (ende-queue-entries)) fence-one))
			      (should (memq third (ende-queue-fence-jobs fence-two)))
			      ;; Moving across F1 makes THIRD a predecessor of F1, then moving back
			      ;; removes that membership without changing either job's stable ID.
			      (ende-queue-move (ende-job-id third) -1)
			      (should (eq (nth 2 (ende-queue-entries)) third))
			      (should (memq third (ende-queue-fence-jobs fence-one)))
			      (ende-queue-move (ende-job-id third) 1)
			      (should-not (memq third (ende-queue-fence-jobs fence-one)))
			      (should (memq third (ende-queue-fence-jobs fence-two)))
			      (should (eq (plist-get (car (plist-get
							   (ende-job-explanation third) :blockers))
						     :fence)
					  fence-one))
			      (should (equal (mapcar #'ende-job-id
						     (ende-queue-fence-unfinished-predecessors fence-one))
					     (list (ende-job-id first) (ende-job-id second))))))))

(ert-deftest ende-test/reordered-inserts-advance-older-queued-anchor ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" B" " A") t)))
    (ende-test--with-buffer "base"
			    (ende-queue-pause)
			    (let* ((first (ende-enqueue
					   (ende-test--dashboard-request
					    ende-test--dashboard-profile)))
				   (second (ende-enqueue
					    (ende-test--dashboard-request
					     ende-test--dashboard-profile))))
			      (ende-queue-move (ende-job-id second) -1)
			      (ende-queue-resume)
			      (should (eq (ende-job-state second) 'running))
			      (ende-test--complete-call (car ende-test--calls))
			      (should (eq (ende-job-state first) 'running))
			      (ende-test--complete-call (nth 1 ende-test--calls))
			      (should (equal (buffer-string) "base B A"))))))

(ert-deftest ende-test/moving-before-global-fence-starts-and-remains-predecessor ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" A" " C" " D") t))
        (local-profile '(:name local :model "local" :max-concurrency 2
                               :parallel t)))
    (ende-test--with-buffer "base"
			    (let* ((active (ende-enqueue
					    (ende-test--dashboard-request
					     ende-test--dashboard-profile)))
				   (fence (ende-fence-all))
				   (moved (ende-enqueue
					   (ende-test--dashboard-request local-profile)))
				   (blocked nil))
			      (should (eq (ende-job-state active) 'running))
			      (should (eq (ende-job-state moved) 'queued))
			      (ende-queue-move (ende-job-id moved) -1)
			      (should (eq (ende-job-state moved) 'running))
			      (should (memq moved (ende-queue-fence-jobs fence)))
			      (setq blocked
				    (ende-enqueue
				     (ende-test--dashboard-request ende-test--dashboard-profile)))
			      (should (eq (ende-job-state blocked) 'queued))
			      (ende-test--complete-call (nth 1 ende-test--calls))
			      (should (memq fence (ende-queue-fences)))
			      (should (eq (ende-job-state blocked) 'queued))
			      (ende-test--complete-call (car ende-test--calls))
			      (should-not (memq fence (ende-queue-fences)))
			      (should (eq (ende-job-state blocked) 'running))))))

(ert-deftest ende-test/provider-fence-is-scoped-and-policy-is-job-local ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" R" " L" " Q") t))
        (local-profile '(:name local :model "local" :max-concurrency 2
                               :parallel t))
        (default-profile '(:name remote :model "remote" :max-concurrency 2
				 :parallel nil)))
    (ende-test--with-buffer "base"
			    (let* ((remote (ende-enqueue
					    (ende-test--dashboard-request default-profile)))
				   (fence (ende-fence-provider 'remote))
				   (remote-waiter (ende-enqueue
						   (ende-test--dashboard-request default-profile)))
				   (local (ende-enqueue
					   (ende-test--dashboard-request local-profile))))
			      (should (eq (ende-job-state remote) 'running))
			      (should (eq (ende-job-state remote-waiter) 'queued))
			      (should (eq (ende-job-state local) 'running))
			      (should (eq (plist-get (car (plist-get
							   (ende-job-explanation remote-waiter)
							   :blockers)) :fence)
					  fence))
			      (should (eq (plist-get (ende-job-explanation local) :ready) t))
			      (should (plist-get (ende-job-explanation remote-waiter) :blockers))))))

(ert-deftest ende-test/policy-batches-respect-fences-and-queued-only-rule ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" A" " B" " C") t))
        (profile '(:name remote :model "remote" :max-concurrency 2
			 :parallel nil)))
    (ende-test--with-buffer "base"
			    (ende-queue-pause)
			    (let* ((first (ende-enqueue (ende-test--dashboard-request profile)))
				   (second (ende-enqueue (ende-test--dashboard-request profile)))
				   (fence (ende-fence-all))
				   (third (ende-enqueue (ende-test--dashboard-request profile))))
			      (ende-queue-change-policy (list (ende-job-id first)) 'parallel)
			      (should (eq (plist-get (ende-job-profile first) :parallel) t))
			      (should-not (plist-get profile :parallel))
			      (should (eq (plist-get (ende-job-profile second) :parallel) nil))
			      (ende-queue-change-policy (list (ende-job-id third)) 'parallel)
			      (should (eq (plist-get (car (plist-get
							   (ende-job-explanation third) :blockers))
						     :fence)
					  fence))
			      (should (= (length ende-test--calls) 0))
			      (ende-queue-resume)
			      (should (eq (ende-job-state first) 'running))
			      (should-error (ende-queue-change-policy
					     (list (ende-job-id first)) 'wait-idle)
					    :type 'user-error)
			      (should-error (ende-queue-change-policy
					     (list (ende-job-id first) (ende-job-id second)) 'parallel)
					    :type 'user-error)
			      (should-not (plist-get (ende-job-profile second) :parallel))
			      (should (eq (ende-job-state third) 'queued))))))

(provide 'ende-queue-scheduler-test)

;;; ende-queue-scheduler-test.el ends here
