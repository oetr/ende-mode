;;; deslop-summary.el --- Summary buffer for deslop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools, llm

;;; Commentary:

;; Ownership and rendering of the summary buffer.

;;; Code:

(require 'deslop-core)
(require 'deslop-review)

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
        (deslop--owned-buffer deslop-summary--buffer-object
                              deslop-summary-buffer-name
                              'deslop-summary--owned-buffer
                              #'text-mode)))

(defun deslop-summary-append (source-buffer content display)
  "Append CONTENT for SOURCE-BUFFER to the summary buffer.
DISPLAY supplies the provider label.  Return the review overlay."
  (let ((summary-buffer (deslop-summary-buffer))
        (review-overlay nil))
    (with-current-buffer summary-buffer
      (let ((inhibit-read-only t))
        (save-restriction
          (widen)
          (save-excursion
            (goto-char (point-max))
            (let ((review-start (point)))
              (unless (bobp) (insert "\n\n"))
              (insert (format "Summary for %s:\n"
                              (buffer-name source-buffer)))
              (insert content)
              (setq review-overlay
                    (deslop-review-create-overlay
                     summary-buffer review-start (point)
                     (plist-get display :model)
                     (plist-get display :client)
                     "")))))))
    (deslop--ignore-errors #'display-buffer summary-buffer)
    review-overlay))

(provide 'deslop-summary)

;;; deslop-summary.el ends here
