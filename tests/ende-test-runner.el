;;; ende-test-runner.el --- Concise batch ERT reporting -*- lexical-binding: t; -*-

(require 'ert)

(defun ende-test-runner--condition (result)
  (let* ((condition (ert-test-result-with-condition-condition result))
         (message (or (cadr condition) (car condition)))
         (summary (replace-regexp-in-string
                   "[\n\r]+" " " (format "%s" message))))
    (truncate-string-to-width summary 160 nil nil "…")))

(defun ende-test-runner--event (event &rest arguments)
  (pcase event
    ('run-started
     (message "Running %d tests" (ert-stats-total (car arguments))))
    ('test-ended
     (pcase-let ((`(_stats ,test ,result) arguments))
       (unless (ert-test-result-expected-p test result)
         (message "TEST-FAILED: %s%s: %s"
                  (ert-test-name test)
                  (if-let ((location (ert-test-location test)))
                      (format " (%s)" location)
                    "")
                  (if (ert-test-result-with-condition-p result)
                      (ende-test-runner--condition result)
                    (if (ert-test-p result)
                        "passed unexpectedly"
                      "aborted with non-local exit")))
         (when (ert-test-result-with-condition-p result)
           (message "TEST-BACKTRACE: %s"
                    (backtrace-to-string
                     (ert-test-result-with-condition-backtrace result)))))))
    ('run-ended
     (pcase-let ((`(,stats ,aborted) arguments))
       (message "TEST-SUMMARY: %d tests: %d passed, %d unexpected, %d skipped%s"
                (ert-stats-total stats)
                (ert--stats-passed-expected stats)
                (ert-stats-completed-unexpected stats)
                (ert-stats-skipped stats)
                (if aborted " (aborted)" ""))))))

(defun ende-test-runner ()
  "Run ERT quietly and exit with a status reflecting its results."
  (let ((ert-quiet t)
        (status 2))
    (condition-case error-data
        (let ((stats (ert-run-tests t #'ende-test-runner--event)))
          (setq status
                (cond
                 ((ert--stats-aborted-p stats) 2)
                 ((zerop (ert-stats-completed-unexpected stats)) 0)
                 (t 1))))
      (error
       (message "TEST-RUNNER-ERROR: %s" (error-message-string error-data))
       (setq status 2)))
    (kill-emacs status)))

(provide 'ende-test-runner)
