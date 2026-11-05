;;; ende-test-runner.el --- Concise batch ERT reporting -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter Samarin
;;
;; Author: Peter Samarin <peter.samarin@gmail.com>
;; Maintainer: Peter Samarin <peter.samarin@gmail.com>
;; Keywords: lisp, tools
;; URL: https://github.com/oetr/ende-mode
;; SPDX-License-Identifier: AGPL-3.0-only

;; This file is part of Ende.

;; Ende is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, version 3 of the License.
;;
;; Ende is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with Ende.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Concise batch ERT reporting for the Ende test suite.

;;; Code:

(defvar ende-test-runner--load-start nil
  "Time immediately before loading test files, supplied by the Makefile.")

(defvar ende-test-runner--load-duration
  (and ende-test-runner--load-start
       (float-time (time-subtract (current-time) ende-test-runner--load-start)))
  "Elapsed test-file loading time captured on entry to this runner.")

(defvar ende-test-runner--timing-count nil
  "Number of slow tests to report for the current run, or nil.")

(defvar ende-test-runner--durations nil
  "Test names and durations collected for the current run.")

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
       (when ende-test-runner--timing-count
         (push (cons (ert-test-name test) (ert-test-result-duration result))
               ende-test-runner--durations))
       (unless (ert-test-result-expected-p test result)
         (message "TEST-FAILED: %s%s: %s"
                  (ert-test-name test)
                  (if-let* ((location (ert-test-location test)))
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
                (if aborted " (aborted)" ""))
       (when ende-test-runner--timing-count
         (let ((durations (sort ende-test-runner--durations
                                (lambda (a b) (> (cdr a) (cdr b))))))
           (dotimes (_ (min ende-test-runner--timing-count (length durations)))
             (let ((entry (pop durations)))
               (message "TEST-SLOW: %.3fs %s" (cdr entry) (car entry))))))))))

(defun ende-test-runner ()
  "Run ERT quietly and exit with a status reflecting its results."
  (let ((ert-quiet t)
        (ende-test-runner--timing-count
         (let ((value (getenv "ENDE_TEST_TIMINGS")))
           (and value
                (string-match-p "\\`[0-9]+\\'" value)
                (> (string-to-number value) 0)
                (string-to-number value))))
        (ende-test-runner--durations nil)
        (status 2))
    (when (and ende-test-runner--timing-count ende-test-runner--load-duration)
      (message "TEST-LOAD: %.3fs" ende-test-runner--load-duration))
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

;;; ende-test-runner.el ends here
