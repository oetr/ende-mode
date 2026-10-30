;;; ende-file.el --- Atomic file publication -*- lexical-binding: t; -*-

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
      (delete-file stage))))

(provide 'ende-file)

;;; ende-file.el ends here
