;;; ende-work-buffer.el --- Read-only work inspection -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'org)

(declare-function ende-queue-open "ende-queue-ui" ())

(defvar ende-work-buffer--retired nil
  "Finished work buffers awaiting the closure of their last view.")

(defvar ende-work-buffer--timer nil
  "Coalesced timer for collecting undisplayed retired buffers.")

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

(defun ende-work-buffer--killed ()
  "Forget the current buffer when it is explicitly or automatically killed."
  (setq ende-work-buffer--retired
        (delq (current-buffer) ende-work-buffer--retired)))

(defun ende-work-buffer--collect ()
  "Dispose of retired buffers whose last inspection view has closed."
  (when (timerp ende-work-buffer--timer)
    (cancel-timer ende-work-buffer--timer))
  (setq ende-work-buffer--timer nil)
  (dolist (buffer (copy-sequence ende-work-buffer--retired))
    (when (and (buffer-live-p buffer)
               (not (get-buffer-window buffer t)))
      (kill-buffer buffer)))
  (setq ende-work-buffer--retired
        (cl-delete-if-not #'buffer-live-p ende-work-buffer--retired)))

(defun ende-work-buffer-retire (buffer)
  "Release finished BUFFER as soon as no window on any frame displays it."
  (when (buffer-live-p buffer)
    (cl-pushnew buffer ende-work-buffer--retired :test #'eq)
    (with-current-buffer buffer
      (add-hook 'kill-buffer-hook #'ende-work-buffer--killed nil t))
    (ende-work-buffer--collect)))

(defun ende-work-buffer--windows-changed (_frame)
  "Schedule collection after a window buffer or frame change."
  (when (and ende-work-buffer--retired
             (not (timerp ende-work-buffer--timer)))
    (setq ende-work-buffer--timer
          (run-at-time 0 nil #'ende-work-buffer--collect))))

(add-hook 'window-buffer-change-functions #'ende-work-buffer--windows-changed)
(add-hook 'delete-frame-functions #'ende-work-buffer--windows-changed)

(defun ende-work-buffer-quit ()
  "Quit the current inspection window and return to the queue dashboard."
  (interactive)
  (quit-window)
  (require 'ende-queue-ui)
  (ende-queue-open))

(provide 'ende-work-buffer)

;;; ende-work-buffer.el ends here
