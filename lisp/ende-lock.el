;;; ende-lock.el --- SQLite mutex for credential transactions -*- lexical-binding: t; -*-

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

;; SQLite mutex for Ende credential transactions.

;;; Code:

(require 'sqlite)

(define-error 'ende-lock-error "SQLite mutex is unavailable or unsafe")
(define-error 'ende-lock-busy "SQLite mutex is busy" 'ende-lock-error)

(defun ende-lock--fail ()
  "Signal a value-free mutex error."
  (signal 'ende-lock-error nil))

(defun ende-lock--safe-file (file &optional required)
  "Require FILE to be a private, owned regular file if it exists."
  (when (file-symlink-p file) (ende-lock--fail))
  (let ((attributes (file-attributes file 'integer)))
    (if attributes
        (unless (and (null (file-attribute-type attributes))
                     (= (file-modes file) #o600)
                     (= (file-attribute-link-number attributes) 1)
                     (= (file-attribute-user-id attributes) (user-uid)))
          (ende-lock--fail))
      (when required (ende-lock--fail)))
    attributes))

(defun ende-lock--safe-directory (directory)
  "Require DIRECTORY to be a private directory owned by this user."
  (when (file-symlink-p directory) (ende-lock--fail))
  (let ((attributes (file-attributes directory 'integer)))
    (unless (and attributes (eq (file-attribute-type attributes) t)
                 (= (file-modes directory) #o700)
                 (= (file-attribute-user-id attributes) (user-uid)))
      (ende-lock--fail))))

(defun ende-lock--check-files (file &optional required)
  "Validate FILE and SQLite's possible sidecars."
  (ende-lock--safe-file file required)
  (dolist (suffix '("-journal" "-wal" "-shm"))
    (ende-lock--safe-file (concat file suffix))))

(defun ende-lock--open (file)
  "Open or safely create SQLite mutex FILE."
  (unless (and (sqlite-available-p) (stringp file)
               (file-name-absolute-p file) (not (file-remote-p file)))
    (ende-lock--fail))
  (let* ((file (expand-file-name file))
         (directory (file-name-directory file))
         (modes (default-file-modes))
         db complete)
    (ende-lock--safe-directory directory)
    (ende-lock--check-files file)
    (unwind-protect
        (progn
          (set-default-file-modes #o600)
          (let ((inhibit-quit t))
            (setq db (sqlite-open file nil t)))
          (ende-lock--check-files file t)
          (setq complete t)
          db)
      (set-default-file-modes modes)
      (unless complete
        (when db (ignore-errors (sqlite-close db)))))))

(defun ende-lock--begin (db)
  "Acquire DB's write reservation with bounded contention retries."
  (sqlite-execute db "PRAGMA busy_timeout = 100")
  (let ((attempts 0) acquired)
    (while (and (< attempts 5) (not acquired))
      (setq attempts (1+ attempts))
      (condition-case nil
          (progn (sqlite-execute db "BEGIN IMMEDIATE")
                 (setq acquired t))
        (sqlite-locked-error
         (if (< attempts 5) (sleep-for 0.05)
           (setq acquired :busy)))
        ((error quit) (signal 'ende-lock-error nil))))
    (when (eq acquired :busy) (signal 'ende-lock-busy nil))
    acquired))

(defun ende-lock-call (file function)
  "Call FUNCTION while holding FILE's permanent SQLite mutex.
FILE and its parent must be in a private local directory.  The mutex
database is never removed or replaced and contains no application tables.
Callback errors and quits are re-signaled after releasing SQLite resources."
  (let (db held result failure callback-stage cleanup-failed)
    (condition-case error-data
        (progn
          (setq db (ende-lock--open file))
          (setq held (ende-lock--begin db))
          (unless held (ende-lock--fail))
          (when (sqlite-select
                 db "SELECT 1 FROM sqlite_schema WHERE type = 'table' AND name NOT GLOB 'sqlite_*' LIMIT 1")
            (ende-lock--fail))
          (ende-lock--check-files (expand-file-name file) t)
          (setq callback-stage t
                result (funcall function)))
      ((error quit) (setq failure error-data)))
    (let ((inhibit-quit t))
      (when db
        (when held
          (condition-case nil
              (sqlite-execute db "ROLLBACK")
            ((error quit) (setq cleanup-failed t))))
        (condition-case nil
            (sqlite-close db)
          ((error quit) (setq cleanup-failed t)))))
    (cond
     (callback-stage
      (when failure (signal (car failure) (cdr failure)))
      (when cleanup-failed (ende-lock--fail))
      result)
     ((and failure (eq (car failure) 'ende-lock-busy))
      (signal 'ende-lock-busy nil))
     ((or failure cleanup-failed) (ende-lock--fail))
     (t result))))

(provide 'ende-lock)
;;; ende-lock.el ends here
