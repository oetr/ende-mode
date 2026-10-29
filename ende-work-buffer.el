;;; ende-work-buffer.el --- Read-only work inspection -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'org)

(declare-function ende-queue-open "ende-queue-ui" ())

(define-minor-mode ende-work-buffer-mode
  "Inspect private work using Org navigation without editing it."
  :lighter " Work"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "q") #'ende-work-buffer-quit)
            map)
  (when ende-work-buffer-mode
    (setq-local font-lock-mode t)
    (font-lock-ensure))
  (read-only-mode (if ende-work-buffer-mode 1 -1)))

(defun ende-work-buffer-at-end (buffer function)
  "Run FUNCTION at BUFFER's end, advancing windows that followed its old end."
  (let* ((end (with-current-buffer buffer (point-max)))
         (windows (get-buffer-window-list buffer nil t))
         (following (cl-remove-if-not
                     (lambda (window)
                       (= (window-point window) end))
                     windows)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (funcall function))))
    (dolist (window following)
      (when (window-live-p window)
        (set-window-point window
                          (with-current-buffer buffer (point-max)))))))

(defun ende-work-buffer-quit ()
  "Quit the current inspection window and return to the queue dashboard."
  (interactive)
  (quit-window)
  (require 'ende-queue-ui)
  (ende-queue-open))

(provide 'ende-work-buffer)

;;; ende-work-buffer.el ends here
