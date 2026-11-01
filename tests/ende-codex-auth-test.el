;;; ende-codex-auth-test.el --- Private Codex account tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'ende-codex-auth)

(defmacro ende-codex-auth-test--with-store (&rest body)
  "Run BODY with an isolated private credential store."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "ende-codex-accounts-" t))
          (ende-codex-auth--directory (expand-file-name "private" root)))
     (unwind-protect (progn ,@body)
       (delete-directory root t))))

(defun ende-codex-auth-test--credentials (&optional suffix)
  "Return synthetic credentials for a registration named SUFFIX."
  (let ((suffix (or suffix "one")))
    (list :client-id (concat "issued-client-" suffix)
          :subject (concat "validated-subject-" suffix)
          :access-token (concat "synthetic-access-" suffix)
          :refresh-token (concat "synthetic-refresh-" suffix)
          :id-token (concat "synthetic-id-" suffix)
          :expires-at (+ (floor (float-time)) 3600))))

(ert-deftest ende-codex-auth-storage-independent-records-and-host ()
  (ende-codex-auth-test--with-store
    (let* ((personal (ende-codex-auth--register
                      'personal (ende-codex-auth-test--credentials)))
           (work (ende-codex-auth--register
                  'work (ende-codex-auth-test--credentials "two"))))
      (should (equal personal (ende-codex-auth-read 'personal)))
      (should (equal work (ende-codex-auth-read 'work)))
      (should (equal (plist-get personal :host-id) (plist-get work :host-id)))
      (should-not (equal (plist-get personal :registration-id)
                         (plist-get work :registration-id)))
      (should (= (logand (file-modes ende-codex-auth--directory) #o777) #o700))
      (should (= (logand (file-modes (expand-file-name "accounts.json"
                                                       ende-codex-auth--directory))
                        #o777) #o600))
      ;; Reads are detached, and there is no persistent current-account field.
      (setf (plist-get personal :access-token) "mutated")
      (should-not (equal personal (ende-codex-auth-read 'personal)))
      (with-temp-buffer
        (insert-file-contents (expand-file-name "accounts.json" ende-codex-auth--directory))
        (should-not (string-match-p "current" (buffer-string)))))))

(ert-deftest ende-codex-auth-storage-replacement-requires-explicit-cas ()
  (ende-codex-auth-test--with-store
    (let* ((first (ende-codex-auth--register 'personal (ende-codex-auth-test--credentials)))
           (other (ende-codex-auth-test--credentials "two")))
      (should-error (ende-codex-auth--register 'personal other)
                    :type 'ende-codex-auth-conflict)
      (should-error (ende-codex-auth--register 'personal other nil t)
                    :type 'ende-codex-auth-conflict)
      (let ((second (ende-codex-auth--register 'personal other (plist-get first :revision) t)))
        (should-not (ende-codex-auth--same-registration-p first second))
        (should-error (ende-codex-auth--register 'personal other (plist-get first :revision) t)
                      :type 'ende-codex-auth-conflict)
        (should (equal second (ende-codex-auth-read 'personal)))))))

(ert-deftest ende-codex-auth-storage-rotation-preserves-registration-not-revision ()
  (ende-codex-auth-test--with-store
    (let* ((credentials (ende-codex-auth-test--credentials))
           (personal (ende-codex-auth--register 'personal credentials))
           (work (ende-codex-auth--register 'work credentials)))
      ;; Identical subjects/client IDs still need independent registrations.
      (should-not (ende-codex-auth--same-registration-p personal work))
      (setq credentials (plist-put credentials :refresh-token "synthetic-rotated"))
      (let ((rotated (ende-codex-auth--rotate personal credentials)))
        (should (ende-codex-auth--same-registration-p personal rotated))
        (should-not (equal (plist-get personal :revision) (plist-get rotated :revision)))
        (should-error (ende-codex-auth--rotate personal credentials)
                      :type 'ende-codex-auth-conflict)
        (should (equal work (ende-codex-auth-read 'work)))
        (setq credentials (plist-put credentials :subject "another-identity"))
        (should-error (ende-codex-auth--rotate rotated credentials)
                      :type 'ende-codex-auth-conflict)))))

(ert-deftest ende-codex-auth-storage-rejects-remote-symlink-and-insecure-paths ()
  (ende-codex-auth-test--with-store
    (let ((ende-codex-auth--directory "/ssh:unreachable:/credentials/"))
      (should-error (ende-codex-auth-read 'personal) :type 'ende-codex-auth-error))
    (make-directory ende-codex-auth--directory)
    (set-file-modes ende-codex-auth--directory #o755)
    (should-error (ende-codex-auth--register 'personal (ende-codex-auth-test--credentials))
                  :type 'ende-codex-auth-error)
    (set-file-modes ende-codex-auth--directory #o700)
    (ende-codex-auth--register 'personal (ende-codex-auth-test--credentials))
    (let ((file (expand-file-name "accounts.json" ende-codex-auth--directory)))
      (set-file-modes file #o644)
      (should-error (ende-codex-auth-read 'personal) :type 'ende-codex-auth-error)
      (set-file-modes file #o600)
      (rename-file file (concat file ".real"))
      (make-symbolic-link (concat file ".real") file)
      (should-error (ende-codex-auth-read 'personal) :type 'ende-codex-auth-error)
      (delete-file file)
      (make-symbolic-link (concat file ".missing") file)
      (should-error (ende-codex-auth-read 'personal) :type 'ende-codex-auth-error))
    (let ((link (expand-file-name "link" root)))
      (make-symbolic-link ende-codex-auth--directory link)
      (let ((ende-codex-auth--directory (expand-file-name "child/" link)))
        (should-error (ende-codex-auth--register 'personal (ende-codex-auth-test--credentials))
                      :type 'ende-codex-auth-error)))))

(ert-deftest ende-codex-auth-storage-rejects-hardlinks-and-untrusted-ancestors ()
  (ende-codex-auth-test--with-store
    (ende-codex-auth--register 'personal (ende-codex-auth-test--credentials))
    (let ((file (expand-file-name "accounts.json" ende-codex-auth--directory)))
      (add-name-to-file file (concat file ".link"))
      (should-error (ende-codex-auth-read 'personal) :type 'ende-codex-auth-error)
      (delete-file (concat file ".link")))
    (set-file-modes root #o777)
    (unwind-protect
        (should-error (ende-codex-auth-read 'personal) :type 'ende-codex-auth-error)
      (set-file-modes root #o700))))

(ert-deftest ende-codex-auth-storage-atomic-failure-leaves-old-record ()
  (ende-codex-auth-test--with-store
    (let* ((credentials (ende-codex-auth-test--credentials))
           (old (ende-codex-auth--register 'personal credentials)))
      (cl-letf (((symbol-function 'rename-file) (lambda (&rest _) (error "write failed"))))
        (should-error (ende-codex-auth--rotate old credentials) :type 'ende-codex-auth-error))
      (should (equal old (ende-codex-auth-read 'personal)))
      (should-not (directory-files ende-codex-auth--directory nil "\\`\\.accounts-"))
      (should-not (file-exists-p (expand-file-name ".writer-lock" ende-codex-auth--directory))))))

(ert-deftest ende-codex-auth-storage-lock-never-overwrites-another-writer ()
  (ende-codex-auth-test--with-store
    (let* ((credentials (ende-codex-auth-test--credentials))
           (old (ende-codex-auth--register 'personal credentials))
           (lock (expand-file-name ".writer-lock" ende-codex-auth--directory)))
      (make-directory lock)
      (should-error (ende-codex-auth--rotate old credentials) :type 'ende-codex-auth-busy)
      (should (file-directory-p lock))
      (should (equal old (ende-codex-auth-read 'personal)))
      (delete-directory lock))))

(ert-deftest ende-codex-auth-storage-errors-never-print-credentials ()
  (ende-codex-auth-test--with-store
    (let* ((credentials (ende-codex-auth-test--credentials))
           (secret (plist-get credentials :access-token)))
      (setq credentials (plist-put credentials :expires-at secret))
      (let ((failure (should-error (ende-codex-auth--register 'personal credentials))))
        (should-not (string-match-p (regexp-quote secret) (prin1-to-string failure))))
      (should-not (custom-variable-p 'ende-codex-auth--directory))
      (should-not (file-exists-p ende-codex-auth--directory)))))

(provide 'ende-codex-auth-test)
;;; ende-codex-auth-test.el ends here
