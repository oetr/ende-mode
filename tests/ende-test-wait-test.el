;;; ende-test-wait-test.el --- Tests for bounded waits -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise event pumping and useful timeout failures.

;;; Code:

(require 'ende-test-wait)

(ert-deftest ende-test/wait-returns-value-and-pumps-events ()
  (let* ((value nil)
         (expected (list 'ready))
         (timer (run-at-time 0 nil (lambda () (setq value expected)))))
    (unwind-protect
        (should (eq (ende-test--wait-for (lambda () value)
                                       :description "timer callback")
                    expected))
      (cancel-timer timer))))

(ert-deftest ende-test/wait-timeout-describes-condition ()
  (let ((failure (should-error
                  (ende-test--wait-for (lambda () nil) :timeout 0
                                       :description "missing callback")
                  :type 'ert-test-failed)))
    (should (equal (cdr failure)
                   '((:waiting-for "missing callback" :timeout 0))))))

(provide 'ende-test-wait-test)
;;; ende-test-wait-test.el ends here
