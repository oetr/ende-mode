;;; ende-lock-test-helpers.el --- Independent Emacs helpers -*- lexical-binding: t; -*-

(require 'ert)

(defun ende-lock-test--start (form)
  "Start an independent Emacs evaluating FORM."
  (make-process
   :name "ende-lock-test" :buffer (generate-new-buffer " *ende-lock-test*")
   :connection-type 'pipe :noquery t
   :command (list (expand-file-name invocation-name invocation-directory)
                  "-Q" "--batch" "--eval" (prin1-to-string form))))

(defun ende-lock-test--await (process marker)
  "Wait for PROCESS to emit MARKER, with a bounded failure deadline."
  (let ((deadline (+ (float-time) 15)))
    (while (and (not (with-current-buffer (process-buffer process)
                      (goto-char (point-min))
                      (search-forward marker nil t)))
                (process-live-p process) (< (float-time) deadline))
      (accept-process-output process 0.05))
    (with-current-buffer (process-buffer process)
      (goto-char (point-min))
      (unless (search-forward marker nil t)
        (ert-fail (buffer-string))))))

(defun ende-lock-test--stop (process)
  "Dispose of PROCESS and its output buffer."
  (when process
    (when (process-live-p process) (delete-process process))
    (kill-buffer (process-buffer process))))

(provide 'ende-lock-test-helpers)
;;; ende-lock-test-helpers.el ends here
