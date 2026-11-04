;;; ende-lock-test.el --- SQLite mutex tests -*- lexical-binding: t; -*-

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

;; Tests for SQLite mutexes.

;;; Code:

(require 'ert)
(require 'ende-lock)
(require 'ende-lock-test-helpers)

(ert-deftest ende-lock-sqlite-crash-releases-mutex ()
  (should (sqlite-available-p))
  (let* ((directory (make-temp-file "ende-sqlite-" t))
         (file (expand-file-name "guard.sqlite" directory)) process)
    (ende-lock-call file #'ignore)
    (unwind-protect
        (progn
          (setq process
                (ende-lock-test--start
                 `(let ((db (sqlite-open ,file nil t)))
                   (sqlite-execute db "BEGIN IMMEDIATE")
                   (princ "held\n")
                   (read)
                   (sqlite-close db))))
          (ende-lock-test--await process "held")
          (should-error (ende-lock-call file (lambda () 'unexpected)))
          (kill-process process)
          (let ((deadline (+ (float-time) 15)))
            (while (and (process-live-p process) (< (float-time) deadline))
              (accept-process-output process 0.05)))
          (should-not (process-live-p process))
          (should (eq (ende-lock-call file (lambda () 'released)) 'released))
          (should (file-regular-p file)))
      (ende-lock-test--stop process)
      (delete-directory directory t))))

(ert-deftest ende-lock-refuses-symlinks-and-preserves-database ()
  (let* ((directory (make-temp-file "ende-lock-" t))
         (file (expand-file-name "guard.sqlite" directory)))
    (unwind-protect
        (progn
          (should-error (ende-lock-call file (lambda () (error "callback"))))
          (let ((inode (file-attribute-inode-number (file-attributes file))))
            (should (eq (ende-lock-call file (lambda () 'released)) 'released))
            (should (= inode (file-attribute-inode-number (file-attributes file)))))
          (delete-file file)
          (make-symbolic-link (expand-file-name "missing" directory) file)
          (should-error (ende-lock-call file (lambda () (ert-fail "Entered symlink mutex")))
                        :type 'ende-lock-error))
      (delete-directory directory t))))

(provide 'ende-lock-test)
;;; ende-lock-test.el ends here
