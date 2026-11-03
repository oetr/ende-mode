;;; ende-lock-test.el --- Cross-process locking tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

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

(ert-deftest ende-lock-sqlite-crash-releases-mutex ()
  (should (sqlite-available-p))
  (let* ((directory (make-temp-file "ende-sqlite-" t))
         (file (expand-file-name "guard.sqlite" directory)) a b)
    (unwind-protect
        (progn
          (setq a (ende-lock-test--start
                   `(let ((db (sqlite-open ,file)))
                      (sqlite-execute db "BEGIN IMMEDIATE")
                      (princ "held\n")
                      (read)
                      (sqlite-close db))))
          (ende-lock-test--await a "held")
          (setq b (ende-lock-test--start
                   `(let ((db (sqlite-open ,file)))
                      (condition-case nil
                          (progn (sqlite-execute db "BEGIN IMMEDIATE")
                                 (error "Acquired a held mutex"))
                        (sqlite-locked-error (princ "contended\n")))
                      (read)
                      (sqlite-execute db "BEGIN IMMEDIATE")
                      (princ "acquired\n")
                      (sqlite-close db))))
          (ende-lock-test--await b "contended")
          (kill-process a)
          (while (process-live-p a) (accept-process-output a 0.05))
          (process-send-string b "continue\n")
          (ende-lock-test--await b "acquired"))
      (ende-lock-test--stop a)
      (ende-lock-test--stop b)
      (delete-directory directory t))))

(provide 'ende-lock-test)
;;; ende-lock-test.el ends here
