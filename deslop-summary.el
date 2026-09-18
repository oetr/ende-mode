;;; deslop-summary.el --- Summary buffer for deslop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools, llm

;;; Commentary:

;; Ownership and rendering of the summary buffer.

;;; Code:

(require 'deslop-core)
(require 'org)

(defcustom deslop-summary-buffer-name "*deslop-summary*"
  "Name of the buffer used for summaries."
  :type 'string
  :group 'deslop)

(defvar deslop-summary--buffer-object nil
  "Summary buffer owned by deslop, when one exists.")

(defvar-local deslop-summary--owned-buffer nil
  "Non-nil in a summary buffer created by deslop.")

(defun deslop-summary-buffer ()
  "Return a non-file-visiting summary buffer owned by deslop."
  (setq deslop-summary--buffer-object
        (deslop-owned-buffer deslop-summary--buffer-object
                              deslop-summary-buffer-name
                              'deslop-summary--owned-buffer
                              #'org-mode)))

(defun deslop-summary-append (source-name content display)
  "Append CONTENT for SOURCE-NAME to the summary buffer.
DISPLAY supplies the model label."
  (let ((summary-buffer (deslop-summary-buffer)))
    (with-current-buffer summary-buffer
      (let ((inhibit-read-only t))
        (save-restriction
          (widen)
          (save-excursion
            (goto-char (point-max))
            (atomic-change-group
              (unless (bobp) (insert "\n\n"))
              (insert (format "* %s - %s - %s\n"
                              source-name
                              (format-time-string "%Y-%m-%d %H:%M:%S")
                              (or (plist-get display :model) "unknown")))
              (insert content))))))
    (deslop-ignore-errors #'display-buffer summary-buffer)
    t))

(provide 'deslop-summary)

;;; deslop-summary.el ends here
