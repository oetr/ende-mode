;;; ende-file-test.el --- File publication tests -*- lexical-binding: t; -*-

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

;; Tests for file publication.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ende-file)

(ert-deftest ende-test/new-files-are-complete-and-exclusive ()
  (let* ((directory (make-temp-file "ende-file-" t))
         (path (expand-file-name "new" directory))
         (write (symbol-function 'write-region)))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'write-region)
                     (lambda (_content _end stage &rest _)
                       (funcall write "partial" nil stage nil 'silent)
                       (error "Write failed"))))
            (should-error (ende-file-write-new path "complete")))
          (should-not (directory-files directory nil directory-files-no-dot-files-regexp))
          (ende-file-write-new path "complete")
          (should-error (ende-file-write-new path "replacement") :type 'file-already-exists)
          (with-temp-buffer
            (insert-file-contents path)
            (should (equal (buffer-string) "complete")))
          (should (equal '("new")
                         (directory-files directory nil directory-files-no-dot-files-regexp))))
      (delete-directory directory t))))

(ert-deftest ende-test/cleanup-errors-preserve-publication-outcome ()
  (let* ((directory (make-temp-file "ende-file-" t))
         (path (expand-file-name "new" directory)))
    (unwind-protect
        (cl-letf (((symbol-function 'delete-file)
                   (lambda (&rest _)
                     (signal 'file-error '("Cleanup failed")))))
          (should (equal path (ende-file-write-new path "complete")))
          (should-error (ende-file-write-new path "replacement")
                        :type 'file-already-exists)
          (with-temp-buffer
            (insert-file-contents path)
            (should (equal (buffer-string) "complete"))))
      (delete-directory directory t))))

(provide 'ende-file-test)

;;; ende-file-test.el ends here
