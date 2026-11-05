;;; ende-file.el --- Atomic file publication -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter Samarin
;;
;; Author: Peter Samarin <peter.samarin@gmail.com>
;; Maintainer: Peter Samarin <peter.samarin@gmail.com>
;; Keywords: convenience, tools
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

;; Atomic file publication for Ende.

;;; Code:

(defun ende-file-write-new (path content &optional coding-system mode)
  "Publish complete CONTENT at new PATH without replacing an existing file."
  (let ((stage (make-temp-file
                (expand-file-name ".ende-stage-" (file-name-directory path)))))
    (unwind-protect
        (progn
          (let ((coding-system-for-write (or coding-system 'utf-8-unix)))
            (write-region content nil stage nil 'silent))
          (set-file-modes stage (or mode (logand #o666 (default-file-modes))))
          (add-name-to-file stage path)
          path)
      (ignore-errors (delete-file stage)))))

(provide 'ende-file)

;;; ende-file.el ends here
