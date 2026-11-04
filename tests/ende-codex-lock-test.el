;;; ende-codex-lock-test.el --- Credential mutex process tests -*- lexical-binding: t; -*-

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

;; Tests for credential mutex behavior across processes.

;;; Code:

(require 'ert)
(require 'package)
(require 'ende-codex-auth)
(require 'ende-codex-auth-test)
(require 'ende-lock-test-helpers)

(defun ende-codex-lock-test--start (form)
  "Start an independent Emacs sharing the current credential store."
  (ende-lock-test--start
   `(progn
      (require 'package)
      (setq package-user-dir ,package-user-dir load-prefer-newer t)
      (package-initialize)
      (add-to-list 'load-path
                   ,(file-name-directory (locate-library "ende-codex-auth")))
      (add-to-list 'load-path
                   ,(file-name-directory (locate-library "ende-codex-auth-test")))
      (require 'ende-codex-auth)
      (require 'ende-codex-auth-test)
      (let ((ende-codex-auth--directory ,ende-codex-auth--directory))
        ,form))))

(ert-deftest ende-codex-lock-sqlite-contention-and-crash-release ()
  (should (sqlite-available-p))
  (let* ((directory (make-temp-file "ende-codex-lock-" t))
         (ende-codex-auth--directory directory)
         (mutex (expand-file-name ".credentials.lock.sqlite" directory))
         process)
    (ende-lock-call mutex #'ignore)
    (unwind-protect
        (progn
          (setq process
                (ende-codex-lock-test--start
                 `(let ((db (sqlite-open ,mutex nil t)))
                    (sqlite-execute db "BEGIN IMMEDIATE")
                    (princ "held\n")
                    (read)
                    (sqlite-close db))))
          (ende-lock-test--await process "held")
          (should-error (ende-lock-call mutex (lambda () 'unexpected)))
          (should (file-regular-p mutex))
          (kill-process process)
          (let ((deadline (+ (float-time) 15)))
            (while (and (process-live-p process) (< (float-time) deadline))
              (accept-process-output process 0.05)))
          (should-not (process-live-p process))
          (should (eq (ende-lock-call mutex (lambda () 'released)) 'released))
          (should (file-regular-p mutex)))
      (ende-lock-test--stop process)
      (delete-directory directory t))))

(ert-deftest ende-codex-lock-independent-process-writes-preserve-records ()
  (should (sqlite-available-p))
  (let* ((directory (make-temp-file "ende-codex-lock-" t))
         (ende-codex-auth--directory directory)
         (credentials (ende-codex-auth-test--credentials))
         (first (ende-codex-auth--register 'chatgpt-plan-oauth 'first credentials))
         (child
          (ende-codex-lock-test--start
           `(let* ((credentials (ende-codex-auth-test--credentials "child"))
                   (record (ende-codex-auth--register
                            'chatgpt-plan-oauth 'second credentials)))
              (princ (if (ende-codex-auth--id-p (plist-get record :revision))
                         "written\n" "failed\n")))))
         (credentials (plist-put credentials :refresh-token "rotated-parent")))
    (unwind-protect
        (progn
          (should (ende-codex-auth--rotate first credentials))
          (ende-lock-test--await child "written")
          (should (equal "rotated-parent"
                         (plist-get (ende-codex-auth-read 'chatgpt-plan-oauth 'first)
                                    :refresh-token)))
          (should (ende-codex-auth-read 'chatgpt-plan-oauth 'second))
          (with-temp-buffer
            (insert-file-contents (expand-file-name "connections.json" directory))
            (should (json-parse-string (buffer-string))))
          (should (file-regular-p (expand-file-name ".credentials.lock.sqlite" directory))))
      (ende-lock-test--stop child)
      (delete-directory directory t))))

(provide 'ende-codex-lock-test)
;;; ende-codex-lock-test.el ends here
