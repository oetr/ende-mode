;;; ende-mode-line-test.el --- Mode-line tests -*- lexical-binding: t; -*-

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

;; Coverage for buffer-local job and pending-review counts.

;;; Code:

(require 'ert)
(require 'ende)
(require 'ende-test-helpers)

(ert-deftest ende-test/mode-line-reports-buffer-job-count ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm (list :stall :stall) t)))
    (ende-test--with-buffer "test"
			    (ende-mode 1)
			    (should (string= ende--mode-line-string " ende[0]"))
			    (let ((first (ende-write-anywhere))
				  (second nil))
			      (should (eq (ende-job-state first) 'running))
			      (should (string= ende--mode-line-string " ende[1]"))
			      (setq second (ende-write-anywhere))
			      (should (eq (ende-job-state second) 'queued))
			      (should (string= ende--mode-line-string " ende[2]"))
			      (should (string-match-p
				       "Thinking"
				       (ende-test--overlay-label (ende-job-overlay first))))
			      (should (string-match-p
				       "Queued"
				       (ende-test--overlay-label (ende-job-overlay second))))
			      (ende-abort-queue)
			      (should-not (ende-job-overlay first))
			      (should-not (ende-job-overlay second))
			      (should (string= ende--mode-line-string " ende[0]"))))))

(ert-deftest ende-test/mode-line-reports-pending-review-count ()
  (ende-test--reset)
  (ende-test--with-buffer "test"
			  (ende-mode 1)
			  (should (string= ende--mode-line-string " ende[0]"))
			  (let ((review (ende-review-create-overlay
					 (current-buffer) 1 2 nil nil "t")))
			    (should (string= ende--mode-line-string " ende[0|1]"))
			    (ende-approve-all-output)
			    (should (string= ende--mode-line-string " ende[0]"))
			    (ende-review-create-overlay
			     (current-buffer) 1 2 nil nil "t")
			    (should (string= ende--mode-line-string " ende[0|1]"))
			    (ende-reject-all-output)
			    (should (string= ende--mode-line-string " ende[0]"))
			    (should-not (ende-review-overlay-p review)))))

(provide 'ende-mode-line-test)

;;; ende-mode-line-test.el ends here
