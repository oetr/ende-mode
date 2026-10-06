;;; ende-queue-dashboard-test.el --- Queue dashboard behavior -*- lexical-binding: t; -*-

;;; Commentary:

;; Behavioral coverage for dashboard state, views, and actions using
;; deterministic fake asynchronous providers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ende-test-helpers)
(require 'ende-queue-ui)

(defun ende-test--dashboard-reset ()
  "Reset scheduler state and tear down dashboard-only state."
  (ende-test--reset)
  (when (buffer-live-p ende-queue-ui--buffer)
    (kill-buffer ende-queue-ui--buffer))
  (clrhash ende-queue-ui--marked-ids)
  (clrhash ende-queue-ui--expanded-jobs)
  (clrhash ende-queue-ui--expanded-fences)
  (clrhash ende-queue-ui--folded-buffers)
  (setq ende-queue-ui--buffer-order nil))

(ert-deftest ende-test/queue-pause-is-independent-of-dashboard-lifecycle ()
  (ende-test--dashboard-reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" A" " B" " C") t)))
    (ende-test--with-buffer "base"
      (let* ((first (ende-enqueue
                     (ende-test--dashboard-request
                      ende-test--dashboard-profile)))
             (second nil)
             (third nil))
        (ende-queue-pause)
        (setq second
              (ende-enqueue (ende-test--dashboard-request
                             ende-test--dashboard-profile)))
        (ende-test--complete-call (car ende-test--calls))
        (setq third
              (ende-enqueue (ende-test--dashboard-request
                             ende-test--dashboard-profile)))
        (should (eq (ende-job-state first) 'completed))
        (should (eq (ende-job-state second) 'queued))
        (should (eq (ende-job-state third) 'queued))
        (should (= (length ende-test--calls) 1))
        (let ((dashboard (ende-queue-open)))
          (with-current-buffer dashboard
            (ende-queue-ui-quit))
          (should ende-queue-paused))
        (ende-queue-resume)
        (should-not ende-queue-paused)
        (should (eq (ende-job-state second) 'running))
        (should (= (length ende-test--calls) 2))
        (ende-test--complete-call (nth 1 ende-test--calls))
        (should (eq (ende-job-state third) 'running))
        (ende-test--complete-call (nth 2 ende-test--calls))
        (should (null (ende-pending-jobs)))))))

(ert-deftest ende-test/dashboard-views-preserve-job-identity-and-ui-state ()
  (ende-test--dashboard-reset)
  (let ((ende-llm-function (ende-test--fake-llm nil t)))
    (ende-test--with-buffer "base"
      (ende-queue-pause)
      (let* ((first (ende-enqueue
                     (ende-test--dashboard-request
                      ende-test--dashboard-profile)))
             (second (ende-enqueue
                      (ende-test--dashboard-request
                       ende-test--dashboard-profile)))
             (dashboard (ende-queue-open))
             (id (ende-job-id first)))
        (with-current-buffer dashboard
          (goto-char (ende-queue-ui--position-for-key (cons 'job id)))
          (ende-queue-ui-mark)
          (ende-queue-ui-toggle-entry)
          (should (gethash id ende-queue-ui--expanded-jobs))
          (forward-line 1)
          (should (eq (ende-job-id (ende-queue-ui--current-job)) id))
          (ende-queue-ui--render)
          (should (eq (ende-job-id (ende-queue-ui--current-job)) id))
          (ende-queue-ui-switch-view)
          (should (equal (get-text-property
                          (ende-queue-ui--position-for-key (cons 'job id))
                          'ende-queue-ui-job-id)
                         id))
          (should (= (ende-queue-ui--marked-count) 1))
          (should (gethash id ende-queue-ui--expanded-jobs))
          (should (string-match-p "State" (buffer-string)))
          (goto-char (point-min))
          (search-forward "Buffer " nil t)
          (beginning-of-line)
          (let ((source (ende-target-buffer (ende-job-target first))))
            (ende-queue-ui-toggle-entry)
            (should (gethash source ende-queue-ui--folded-buffers))
            (should-not (ende-queue-ui--position-for-key (cons 'job id)))
            (ende-queue-ui-toggle-entry)
            (should-not (gethash source ende-queue-ui--folded-buffers))
            (should (ende-queue-ui--position-for-key (cons 'job id))))
          (ende-queue-ui-switch-view)
          (goto-char (ende-queue-ui--position-for-key (cons 'job id)))
          (ende-queue-ui-cancel))
        (should (eq (ende-job-state first) 'cancelled))
        (should (eq (ende-job-state second) 'queued))
        (should (= (length ende-test--calls) 0))))))

(ert-deftest ende-test/dashboard-inspection-is-read-only-for-scheduler-state ()
  (ende-test--dashboard-reset)
  (let ((ende-llm-function (ende-test--fake-llm nil t)))
    (ende-test--with-buffer "base"
      (ende-queue-pause)
      (let* ((first (ende-enqueue
                     (ende-test--dashboard-request
                      ende-test--dashboard-profile)))
             (second (ende-enqueue
                      (ende-test--dashboard-request
                       ende-test--dashboard-profile)))
             (fence (ende-fence-all))
             (before (ende-queue-entries))
             (membership (copy-sequence (ende-queue-fence-jobs fence)))
             (dashboard (ende-queue-open)))
        (ende-job-explanation second)
        (with-current-buffer dashboard
          (goto-char (ende-queue-ui--position-for-key
                      (cons 'job (ende-job-id second))))
          (ende-queue-ui-toggle-entry)
          (ende-queue-ui--render))
        (should (equal before (ende-queue-entries)))
        (should (equal membership (ende-queue-fence-jobs fence)))
        (should (eq (ende-job-state first) 'queued))
        (should-not ende--active-jobs)
        (should (= (length ende-test--calls) 0))))))

(ert-deftest ende-test/dashboard-batch-cancel-keeps-captured-job-ids ()
  (ende-test--dashboard-reset)
  (let ((ende-llm-function (ende-test--fake-llm nil t)))
    (ende-test--with-buffer "base"
      (ende-queue-pause)
      (let* ((first (ende-enqueue
                     (ende-test--dashboard-request
                      ende-test--dashboard-profile)))
             (second (ende-enqueue
                      (ende-test--dashboard-request
                       ende-test--dashboard-profile)))
             (third (ende-enqueue
                     (ende-test--dashboard-request
                      ende-test--dashboard-profile)))
             (dashboard (ende-queue-open)))
        (with-current-buffer dashboard
          (goto-char (ende-queue-ui--position-for-key
                      (cons 'job (ende-job-id first))))
          (ende-queue-ui-mark)
          (goto-char (ende-queue-ui--position-for-key
                      (cons 'job (ende-job-id second))))
          (ende-queue-ui-mark)
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (prompt)
                       (should (string-match-p "0 running and 2 queued" prompt))
                       (ende-queue-move (ende-job-id third) -1)
                       t)))
            (ende-queue-ui-cancel)))
        (should (eq (ende-job-state first) 'cancelled))
        (should (eq (ende-job-state second) 'cancelled))
        (should (eq (ende-job-state third) 'queued))
        (should (equal (ende-pending-jobs) (list third)))))))

(ert-deftest ende-test/dashboard-cancel-all-preserves-broad-abort-hooks ()
  (ende-test--dashboard-reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" A" " B") t))
        (abort-hook-ran nil)
        prompt)
    (let ((ende-queue-abort-functions
           (cons (lambda () (setq abort-hook-ran t))
                 ende-queue-abort-functions)))
      (ende-test--with-buffer "base"
        (let* ((running (ende-enqueue
                         (ende-test--dashboard-request
                          ende-test--dashboard-profile)))
               (queued (ende-enqueue
                        (ende-test--dashboard-request
                         ende-test--dashboard-profile)))
               (dashboard (ende-queue-open)))
          (with-current-buffer dashboard
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (text) (setq prompt text) t)))
              (ende-queue-ui-cancel-all)))
          (should (string-match-p "1 running and 1 queued" prompt))
          (should abort-hook-ran)
          (should (eq (ende-job-state running) 'cancelled))
          (should (eq (ende-job-state queued) 'cancelled))
          (should-not ende--active-jobs)
          (should-not (ende-pending-jobs)))))))

(provide 'ende-queue-dashboard-test)

;;; ende-queue-dashboard-test.el ends here
