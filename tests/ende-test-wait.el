;;; ende-test-wait.el --- Bounded ERT waits -*- lexical-binding: t; -*-

;;; Commentary:
;; Lightweight event-loop waits for asynchronous tests.

;;; Code:

(require 'cl-lib)
(require 'ert)

(cl-defun ende-test--wait-for (predicate &key (timeout 10) description)
  "Return PREDICATE's truthy value, pumping events for at most TIMEOUT seconds.
Fail with DESCRIPTION when the deadline expires."
  (let ((deadline (+ (float-time) timeout))
        value)
    (while (not (setq value (funcall predicate)))
      (when (>= (float-time) deadline)
        (ert-fail (list :waiting-for description :timeout timeout)))
      (accept-process-output nil 0.01))
    value))

(provide 'ende-test-wait)
;;; ende-test-wait.el ends here
