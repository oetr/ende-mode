;;; ende-lock-test.el --- Cross-process locking tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'ende-lock-test-helpers)
(require 'ende-lock)

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

(ert-deftest ende-lock-fails-closed-and-retains-database ()
  (let* ((directory (make-temp-file "ende-lock-" t))
         (file (expand-file-name "guard.sqlite" directory)))
    (unwind-protect
        (progn
          (should-error (ende-lock-call file (lambda () (error "callback"))))
          (let ((identity (ende-lock--identity (file-attributes file))))
            (should (eq (ende-lock-call file (lambda () 'released)) 'released))
            (should (equal identity (ende-lock--identity (file-attributes file)))))
          (dolist (path (list file (concat file "-journal")))
            (when (file-exists-p path) (delete-file path))
            (make-symbolic-link (expand-file-name "missing" directory) path)
            (should-error (ende-lock-call file (lambda () (ert-fail "Entered unsafe mutex")))
                          :type 'ende-lock-error)
            (delete-file path)))
      (delete-directory directory t))))

(provide 'ende-lock-test)
;;; ende-lock-test.el ends here
