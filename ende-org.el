;;; ende-org.el --- Shared Org literal-region safety -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)

(defun ende-org--block-directive (line)
  "Return the indented, case-insensitive block directive in LINE, or nil."
  (let ((case-fold-search t))
    (cond
     ((string-match
       "\\`[ \t]*#\\+begin_\\([[:alnum:]_-]+\\)\\(?:[ \t].*\\)?[ \t\r]*\\'" line)
      (cons 'begin (downcase (match-string 1 line))))
     ((string-match "\\`[ \t]*#\\+end_\\([[:alnum:]_-]+\\)[ \t\r]*\\'" line)
      (cons 'end (downcase (match-string 1 line)))))))

(defun ende-org--mask-ranges (text ranges)
  "Mask buffer-position RANGES in TEXT, preserving newlines and coordinates."
  (let ((offset 0) parts)
    (dolist (range (sort (copy-sequence ranges)
                         (lambda (left right) (< (car left) (car right)))))
      (let ((start (1- (car range))) (end (1- (cdr range))))
        (push (substring text offset start) parts)
        (push (replace-regexp-in-string "[^\n]" " " (substring text start end)) parts)
        (setq offset end)))
    (push (substring text offset) parts)
    (apply #'concat (nreverse parts))))

(defun ende-org--blocks (text &optional repair)
  "Classify literal blocks in TEXT, optionally REPAIR assistant presentation.
Read-only analysis retains source coordinates, including for unfinished blocks.
REPAIR escapes block bodies and closes open blocks at this text's boundary."
  (with-temp-buffer
    (insert (or text ""))
    (let (open-block block-start synthetic-newline ranges)
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let* ((line-start (point))
               (line-end (line-end-position))
               (directive (ende-org--block-directive
                           (buffer-substring-no-properties line-start line-end))))
          (if open-block
              (if (and directive (eq (car directive) 'end)
                       (equal (cdr directive) open-block))
                  (progn
                    (push (cons block-start (min (point-max) (1+ line-end))) ranges)
                    (setq open-block nil block-start nil))
                (when repair
                  (skip-chars-forward " \t")
                  (when (looking-at "\\(?:\\*\\|#\\+\\|,\\(?:\\*\\|#\\+\\|,\\)\\)")
                    (insert ","))))
            (when (and directive (eq (car directive) 'begin))
              (setq open-block (cdr directive) block-start line-start)))
          (forward-line 1)))
      (when open-block
        (goto-char (point-max))
        (when repair
          (unless (bolp) (insert "\n") (setq synthetic-newline t))
          (insert (format "#+end_%s\n" open-block)))
        (push (cons block-start (point-max)) ranges))
      (let ((content (buffer-string)))
        (list :content content :ranges (reverse ranges)
              :synthetic-end (and repair open-block)
              :synthetic-newline synthetic-newline
              :parse-content (ende-org--mask-ranges content ranges))))))

(defun ende-org--apply-edits (text edits)
  "Apply (BEGIN END REPLACEMENT) EDITS in TEXT's source coordinate space."
  (with-temp-buffer
    (insert text)
    (dolist (edit (sort (copy-sequence edits)
                        (lambda (left right) (> (car left) (car right)))))
      (delete-region (nth 0 edit) (nth 1 edit))
      (goto-char (nth 0 edit))
      (insert (nth 2 edit)))
    (buffer-string)))

(provide 'ende-org)
;;; ende-org.el ends here
