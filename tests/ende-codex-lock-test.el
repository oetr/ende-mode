;;; ende-codex-lock-test.el --- Codex cross-process locks -*- lexical-binding: t; -*-

(require 'ert)
(require 'package)
(require 'ende-codex-auth)
(require 'ende-lock-test-helpers)

(defun ende-codex-lock-test--start (form)
  "Start FORM in an independent Emacs sharing the current credential store."
  (ende-lock-test--start
   `(progn
      (require 'package)
      (setq package-user-dir ,package-user-dir load-prefer-newer t)
      (package-initialize)
      (add-to-list 'load-path
                   ,(file-name-directory (locate-library "ende-codex-auth")))
      (require 'ende-codex-auth)
      (require 'ert)
      (let ((ende-codex-auth--directory ,ende-codex-auth--directory))
        ,form))))

(ert-deftest ende-codex-lock-contention-and-writer-crash ()
  (should (sqlite-available-p))
  (let* ((directory (make-temp-file "ende-codex-lock-" t))
         (ende-codex-auth--directory directory)
         (lock (expand-file-name ".writer-lock" directory)) process)
    (unwind-protect
        (progn
          (setq process
                (ende-codex-lock-test--start
                 '(progn
                    (ende-codex-auth--with-lock-guard
                     (lambda () (princ "guard-held\n") (read)))
                    (ende-codex-auth--transaction
                     (lambda (_state) (princ "writer-held\n") (read))))))
          (ende-lock-test--await process "guard-held")
          (should-error (ende-codex-auth--transaction #'ignore)
                        :type 'ende-codex-auth-busy)
          (should-not (file-exists-p lock))
          (process-send-string process "continue\n")
          (ende-lock-test--await process "writer-held")
          (should (file-directory-p lock))
          (should-error (ende-codex-auth--recover-lock ".writer-lock")
                        :type 'user-error)
          (kill-process process)
          (let ((deadline (+ (float-time) 15)))
            (while (and (process-live-p process) (< (float-time) deadline))
              (accept-process-output process 0.05)))
          (should-not (process-live-p process))
          (should (file-directory-p lock))
          (should (ende-codex-auth--recover-lock ".writer-lock"))
          (should-not (file-exists-p lock))
          (should (ende-codex-auth--transaction (lambda (_state) t))))
      (ende-lock-test--stop process)
      (delete-directory directory t))))

(ert-deftest ende-codex-lock-stale-release-and-confirmed-recovery ()
  (should (sqlite-available-p))
  (let* ((directory (make-temp-file "ende-codex-lock-" t))
         (ende-codex-auth--directory directory)
         (lock (expand-file-name ".writer-lock" directory)) process)
    (unwind-protect
        (progn
          (setq process
                (ende-codex-lock-test--start
                 `(let ((identity (ende-codex-auth--make-lock ,lock)))
                    (princ "owned\n")
                    (read)
                    (ende-codex-auth--delete-lock ,lock identity)
                    (cl-letf (((symbol-function 'yes-or-no-p)
                               (lambda (&rest _)
                                 (princ "confirming\n") (read) t)))
                      (should-error (ende-codex-auth--recover-lock ".writer-lock")
                                    :type 'user-error))
                    (princ "replacement-preserved\n"))))
          (ende-lock-test--await process "owned")
          (ende-codex-auth--delete-lock lock (ende-codex-auth--lock-identity lock))
          (ende-codex-auth--make-lock lock)
          ;; An unrecognized owner requires explicit confirmation.
          (with-temp-file (expand-file-name "owner" lock) (insert "{}\n"))
          (let ((identity (ende-codex-auth--lock-identity lock)))
            (process-send-string process "continue\n")
            (ende-lock-test--await process "confirming")
            (should (equal identity (ende-codex-auth--lock-identity lock)))
            ;; Confirmation must not retain the cross-process mutex.
            (should (eq (ende-codex-auth--with-lock-guard (lambda () 'available))
                        'available))
            (ende-codex-auth--delete-lock lock identity))
          (ende-codex-auth--make-lock lock)
          ;; Keep the replacement unverifiable so only identity prevents removal.
          (with-temp-file (expand-file-name "owner" lock) (insert "{\"new\":true}\n"))
          (let ((replacement (ende-codex-auth--lock-identity lock)))
            (process-send-string process "confirm\n")
            (ende-lock-test--await process "replacement-preserved")
            (should (equal replacement (ende-codex-auth--lock-identity lock)))))
      (ende-lock-test--stop process)
      (delete-directory directory t))))

(provide 'ende-codex-lock-test)
;;; ende-codex-lock-test.el ends here
