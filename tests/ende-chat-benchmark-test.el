;;; ende-chat-benchmark-test.el --- Chat benchmark invariants -*- lexical-binding: t; -*-

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

;; Startup benchmark smoke test.

;;; Code:

(require 'ert)
(require 'ende-test-helpers)
(require 'ende-chat-benchmark)

(ert-deftest ende-chat-benchmark-startup-invariants ()
  (let ((ende-test--title-prelude t))
    (ende-chat-benchmark--call-isolated
     (lambda (_buffer)
       (insert "* Existing job\nLeave this running.\n")
       (goto-char (point-min))
       (let* ((ende-llm-function #'ignore)
              (job (ende-ask ende-chat-benchmark--profile))
              (report (let ((standard-output #'ignore))
                        (ende-chat-benchmark-startup-run '(2) 1)))
              (cases (plist-get report :cases)))
         (should (ende-job-current-p job))
         (should (equal (mapcar (lambda (case) (plist-get case :scenario)) cases)
                        ende-chat-benchmark-startup-scenarios))
         (dolist (case cases)
           (let ((sample (car (plist-get case :samples))))
             (should (= (plist-get sample :dispatches) 1))
             (should (>= (plist-get sample :elapsed) 0)))))))))

(provide 'ende-chat-benchmark-test)

;;; ende-chat-benchmark-test.el ends here
