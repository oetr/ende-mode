;;; ende-lock-test.el --- SQLite mutex tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'ende-lock)
(require 'ende-lock-test-helpers)

(ert-deftest ende-lock-sqlite-crash-releases-mutex ()
  (should (sqlite-available-p))
  (let* ((directory (make-temp-file "ende-sqlite-" t))
         (file (expand-file-name "guard.sqlite" directory)) process)
    (ende-lock-call file #'ignore)
    (unwind-protect
        (progn
          (setq process
                (ende-lock-test--start
                 `(let ((db (sqlite-open ,file nil t)))
                   (sqlite-execute db "BEGIN IMMEDIATE")
                   (princ "held\n")
                   (read)
                   (sqlite-close db))))
          (ende-lock-test--await process "held")
          (should-error (ende-lock-call file (lambda () 'unexpected)))
          (kill-process process)
          (let ((deadline (+ (float-time) 15)))
            (while (and (process-live-p process) (< (float-time) deadline))
              (accept-process-output process 0.05)))
          (should-not (process-live-p process))
          (should (eq (ende-lock-call file (lambda () 'released)) 'released))
          (should (file-regular-p file)))
      (ende-lock-test--stop process)
      (delete-directory directory t))))

(ert-deftest ende-lock-refuses-symlinks-and-preserves-database ()
  (let* ((directory (make-temp-file "ende-lock-" t))
         (file (expand-file-name "guard.sqlite" directory)))
    (unwind-protect
        (progn
          (should-error (ende-lock-call file (lambda () (error "callback"))))
          (let ((inode (file-attribute-inode-number (file-attributes file))))
            (should (eq (ende-lock-call file (lambda () 'released)) 'released))
            (should (= inode (file-attribute-inode-number (file-attributes file)))))
          (delete-file file)
          (make-symbolic-link (expand-file-name "missing" directory) file)
          (should-error (ende-lock-call file (lambda () (ert-fail "Entered symlink mutex")))
                        :type 'ende-lock-error))
      (delete-directory directory t))))

(provide 'ende-lock-test)
;;; ende-lock-test.el ends here
