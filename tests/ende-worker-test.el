;;; ende-worker-test.el --- Worker lifecycle tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'ende-worker)

(defun ende-worker-test--wait (predicate)
  "Wait briefly for PREDICATE to become non-nil."
  (let ((deadline (+ (float-time) 10)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (should (funcall predicate))))

(ert-deftest ende-worker/round-trip-keeps-the-event-loop-free ()
  (let* ((tick nil)
         (done nil)
         (value nil)
         (failure nil)
         (cancel nil)
         (timer (run-at-time 0 nil (lambda () (setq tick t)))))
    (unwind-protect
        (progn
          (setq cancel
                (ende-worker-run
                 'cl-lib 'eval
                 '((progn (sleep-for 0.05)
                          (list (emacs-pid) case-fold-search "λ\nquoted \"text\"")))
                 '((case-fold-search . nil))
                 (lambda (result error)
                   (setq value result failure error done t))))
          (should-not done)
          (ende-worker-test--wait (lambda () done))
          (should tick)
          (should-not failure)
          (should-not (= (car value) (emacs-pid)))
          (should (equal (cdr value) '(nil "λ\nquoted \"text\""))))
      (when cancel (funcall cancel))
      (cancel-timer timer))))

(ert-deftest ende-worker/errors-and-cancellation-settle-once ()
  (dolist (cancel-p '(nil t))
    (let ((done nil)
          (calls 0)
          (failure nil)
          (cancel nil)
          (registered nil))
      (unwind-protect
          (progn
            (setq cancel
                  (ende-worker-run
                   'cl-lib (if cancel-p 'sleep-for 'error)
                   (if cancel-p '(60) '("worker error")) nil
                   (lambda (_value error)
                     (setq failure error done t calls (1+ calls)))
                   (lambda (function) (setq registered function))))
            (should (functionp registered))
            (when cancel-p (funcall registered))
            (ende-worker-test--wait (lambda () done))
            (should (stringp failure))
            (should-not registered)
            (funcall cancel)
            (accept-process-output nil 0.05)
            (should (= calls 1)))
        (when cancel (funcall cancel))))))

(provide 'ende-worker-test)
