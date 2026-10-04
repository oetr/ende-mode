;;; ende-queue-parallel-test.el --- Parallel queue tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Coverage for concurrency limits, scheduling, and provider fences.

;;; Code:

(require 'ert)
(require 'ende)
(require 'ende-test-helpers)
(require 'cl-lib)

(ert-deftest ende-test/parallel-same-endpoint-preserves-enqueue-order ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm '("A" "B") t))
        (profile '(:name remote :model "remote-model"
			 :max-concurrency 2 :parallel t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((ende--queue-suspended t)
				  (target (ende-target-create
					   :buffer (current-buffer) :start 5 :end 5 :cursor 5)))
			      (ende-enqueue (list 'insert :target target :profile profile))
			      (ende-enqueue
			       (list 'insert
				     :target (ende-target-create
					      :buffer (current-buffer) :start 5 :end 5 :cursor 5)
				     :profile profile))
			      (setq ende--queue-suspended nil)
			      (ende-process-next))
			    (should (= (length ende--active-jobs) 2))
			    (ende-test--complete-call (nth 1 ende-test--calls))
			    (ende-test--complete-call (nth 0 ende-test--calls))
			    (should (string= (buffer-string) "baseAB")))))



(ert-deftest ende-test/parallel-job-at-provider-limit-keeps-policy ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm '("A" "B") t))
        (profile '(:name remote :model "remote-model"
			 :max-concurrency 1 :parallel t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((first (ende-enqueue
					  (list 'insert
						:target (ende-target-create
							 :buffer (current-buffer)
							 :start 5 :end 5 :cursor 5)
						:profile profile)))
				  (second nil))
			      (setq second
				    (ende-enqueue
				     (list 'insert
					   :target (ende-target-create
						    :buffer (current-buffer)
						    :start 5 :end 5 :cursor 5)
					   :profile profile)))
			      (should (eq (ende-job-state first) 'running))
			      (should (eq (ende-job-state second) 'queued))
			      (should (plist-get (ende-job-profile second) :parallel))
			      (ende-test--complete-call (nth 0 ende-test--calls))
			      (should (eq (ende-job-state second) 'running))
			      (ende-test--complete-call (nth 1 ende-test--calls))
			      (should (string= (buffer-string) "baseAB"))))))



(ert-deftest ende-test/parallel-job-uses-free-slot-alongside-serial-job ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm '("A" "B") t))
        (serial-profile '(:name remote :model "serial"
				:max-concurrency 4 :parallel nil))
        (parallel-profile '(:name remote :model "parallel"
				  :max-concurrency 4 :parallel t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((first
				   (ende-enqueue
				    (list 'insert
					  :target (ende-target-create
						   :buffer (current-buffer)
						   :start 5 :end 5 :cursor 5)
					  :profile serial-profile)))
				  (second nil))
			      (let ((ende--queue-suspended t))
				(setq second
				      (ende-enqueue
				       (list 'insert
					     :target (ende-target-create
						      :buffer (current-buffer)
						      :start 5 :end 5 :cursor 5)
					     :profile parallel-profile))))
			      (should (eq (ende-job-state first) 'running))
			      (should (eq (ende-job-state second) 'queued))
			      (ende-process-next)
			      (should (eq (ende-job-state second) 'running))
			      (should (= (length ende--active-jobs) 2))
			      (ende-test--complete-call (nth 0 ende-test--calls))
			      (ende-test--complete-call (nth 1 ende-test--calls))
			      (should (string= (buffer-string) "baseAB"))))))



(ert-deftest ende-test/global-fence-waits-for-existing-jobs ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm '("A" "B" "C") t))
        (profile '(:name remote :model "remote-model"
			 :max-concurrency 4 :parallel t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((first (ende-enqueue
					  (list 'insert
						:target (ende-target-create
							 :buffer (current-buffer)
							 :start 5 :end 5 :cursor 5)
						:profile profile)))
				  (second (ende-enqueue
					   (list 'insert
						 :target (ende-target-create
							  :buffer (current-buffer)
							  :start 5 :end 5 :cursor 5)
						 :profile profile)))
				  (third nil))
			      (ende-fence-all)
			      (setq third
				    (ende-enqueue
				     (list 'insert
					   :target (ende-target-create
						    :buffer (current-buffer)
						    :start 5 :end 5 :cursor 5)
					   :profile profile)))
			      (should (eq (ende-job-state first) 'running))
			      (should (eq (ende-job-state second) 'running))
			      (should (eq (ende-job-state third) 'queued))
			      (should (= (length ende--fences) 1))
			      (ende-test--complete-call (nth 0 ende-test--calls))
			      (should (eq (ende-job-state third) 'queued))
			      (ende-test--complete-call (nth 1 ende-test--calls))
			      (should (eq (ende-job-state third) 'running))
			      (should-not ende--fences)
			      (ende-test--complete-call (nth 2 ende-test--calls))
			      (should (string= (buffer-string) "baseABC"))))))



(ert-deftest ende-test/provider-fence-does-not-block-other-providers ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm '("A1" "B1" "A2" "B2") t))
        (first-profile '(:name first :model "first-model"
                               :max-concurrency 4 :parallel t))
        (second-profile '(:name second :model "second-model"
				:max-concurrency 4 :parallel t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((first (ende-enqueue
					  (list 'insert
						:target (ende-target-create
							 :buffer (current-buffer)
							 :start 5 :end 5 :cursor 5)
						:profile first-profile)))
				  (second (ende-enqueue
					   (list 'insert
						 :target (ende-target-create
							  :buffer (current-buffer)
							  :start 5 :end 5 :cursor 5)
						 :profile second-profile)))
				  (third nil)
				  (fourth nil))
			      (ende-fence-provider 'first)
			      (setq third
				    (ende-enqueue
				     (list 'insert
					   :target (ende-target-create
						    :buffer (current-buffer)
						    :start 5 :end 5 :cursor 5)
					   :profile first-profile)))
			      (setq fourth
				    (ende-enqueue
				     (list 'insert
					   :target (ende-target-create
						    :buffer (current-buffer)
						    :start 5 :end 5 :cursor 5)
					   :profile second-profile)))
			      (should (eq (ende-job-state first) 'running))
			      (should (eq (ende-job-state second) 'running))
			      (should (eq (ende-job-state third) 'queued))
			      (should (eq (ende-job-state fourth) 'running))
			      (ende-test--complete-call (nth 1 ende-test--calls))
			      (ende-test--complete-call (nth 0 ende-test--calls))
			      (should (eq (ende-job-state third) 'running))
			      (ende-test--complete-call (nth 3 ende-test--calls))
			      (ende-test--complete-call (nth 2 ende-test--calls))
			      (should (string= (buffer-string) "baseA1B1B2A2"))))))



(ert-deftest ende-test/equal-provider-identities-share-free-slots ()
  (ende-test--reset)
  (let* ((ende-llm-function
          (ende-test--fake-llm '("X" "A" "B") t))
         (url "http://same.example/v1/chat/completions")
         (active-profile (list :url (copy-sequence url)
                               :model "active" :max-concurrency 2
                               :parallel t))
         (serial-profile (list :url (copy-sequence url)
                               :model "serial" :max-concurrency 2
                               :parallel nil))
         (parallel-profile (list :url (copy-sequence url)
                                 :model "parallel" :max-concurrency 2
                                 :parallel t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((active (ende-enqueue
					   (list 'insert
						 :target (ende-target-create
							  :buffer (current-buffer)
							  :start 5 :end 5 :cursor 5)
						 :profile active-profile)))
				  (serial nil)
				  (parallel nil))
			      (let ((ende--queue-suspended t))
				(setq serial
				      (ende-enqueue
				       (list 'insert
					     :target (ende-target-create
						      :buffer (current-buffer)
						      :start 5 :end 5 :cursor 5)
					     :profile serial-profile))
				      parallel
				      (ende-enqueue
				       (list 'insert
					     :target (ende-target-create
						      :buffer (current-buffer)
						      :start 5 :end 5 :cursor 5)
					     :profile parallel-profile))))
			      (should (eq (ende-job-state active) 'running))
			      (should (eq (ende-job-state serial) 'queued))
			      (should (eq (ende-job-state parallel) 'queued))
			      (should (= (length ende-test--calls) 1))
			      (ende-test--complete-call (car ende-test--calls))
			      (should (eq (ende-job-state serial) 'running))
			      (should (eq (ende-job-state parallel) 'running))
			      (ende-test--complete-call (nth 1 ende-test--calls))
			      (ende-test--complete-call (nth 2 ende-test--calls))
			      (should (string= (buffer-string) "baseXAB"))))))



(ert-deftest ende-test/parallel-slots-are-scoped-per-provider ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm '("A1" "B1" "A2") t))
        (first-profile '(:name first :model "first-model"
                               :max-concurrency 1 :parallel t))
        (second-profile '(:name second :model "second-model"
				:max-concurrency 1 :parallel t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((first (ende-enqueue
					  (list 'insert
						:target (ende-target-create
							 :buffer (current-buffer)
							 :start 5 :end 5 :cursor 5)
						:profile first-profile)))
				  (second nil)
				  (third nil))
			      (setq second
				    (ende-enqueue
				     (list 'insert
					   :target (ende-target-create
						    :buffer (current-buffer)
						    :start 5 :end 5 :cursor 5)
					   :profile first-profile)))
			      (setq third
				    (ende-enqueue
				     (list 'insert
					   :target (ende-target-create
						    :buffer (current-buffer)
						    :start 5 :end 5 :cursor 5)
					   :profile second-profile)))
			      (should (eq (ende-job-state first) 'running))
			      (should (eq (ende-job-state second) 'queued))
			      (should (eq (ende-job-state third) 'running))
			      (should (= (length ende--active-jobs) 2))
			      (ende-abort-queue)))))

(provide 'ende-queue-parallel-test)

;;; ende-queue-parallel-test.el ends here
