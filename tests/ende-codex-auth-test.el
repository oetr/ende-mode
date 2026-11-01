;;; ende-codex-auth-test.el --- Private Codex account tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'ende-codex-auth)
(require 'ende-test-helpers)

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

(defun ende-codex-auth-test--profile (account)
  "Return an internal request profile bound to Ende ACCOUNT."
  (ende-codex--account-profile
   (list :name 'codex-test :service 'openai-codex :account account
         :api 'openai-responses :auth-method 'codex-oauth :model "synthetic-model"
         :url "https://chatgpt.com/backend-api/codex/responses")))

(ert-deftest ende-codex-auth-binding-two-queued-accounts-reject-replacement ()
  (ende-codex-auth-test--with-store
    (let* ((personal (ende-codex-auth--register 'personal (ende-codex-auth-test--credentials)))
           (work (ende-codex-auth--register 'work (ende-codex-auth-test--credentials "two")))
           (ende-credentials--bindings (make-hash-table :test #'eq)))
      (with-temp-buffer
        (let* ((target (ende-target-create :buffer (current-buffer) :start 1 :end 1 :cursor 1))
               (personal-job (ende--job-from-request
                              (list 'insert :target target
                                    :profile (ende-codex-auth-test--profile 'personal))))
               (work-job (ende--job-from-request
                          (list 'insert :target target
                                :profile (ende-codex-auth-test--profile 'work)))))
          (should-not (ende-codex--check-binding (ende-job-profile personal-job) personal))
          (should-not (ende-codex--check-binding (ende-job-profile work-job) work))
          (ende-codex-auth--register 'personal (ende-codex-auth-test--credentials "replacement")
                                     (plist-get personal :revision) t)
          (should-error (ende-codex--check-binding
                         (ende-job-profile personal-job) (ende-codex-auth-read 'personal)))
          (should-not (ende-codex--check-binding
                       (ende-job-profile work-job) (ende-codex-auth-read 'work)))
          (dolist (job (list personal-job work-job))
            (let ((printed (prin1-to-string (list (ende-job-profile job)
                                                   (ende-job-inference-template job)))))
              (dolist (secret '("synthetic-access" "synthetic-refresh" "synthetic-id"))
                (should-not (string-match-p secret printed)))))
          (ende-target-release target))))))

(ert-deftest ende-codex-auth-binding-rotation-survives-and-capture-stays-failed-closed ()
  (ende-codex-auth-test--with-store
    (let* ((credentials (ende-codex-auth-test--credentials))
           (record (ende-codex-auth--register 'personal credentials))
           (profile (ende-codex-capture-binding (ende-codex-auth-test--profile 'personal)))
           (missing (ende-codex-capture-binding (ende-codex-auth-test--profile 'missing))))
      (setq credentials (plist-put credentials :refresh-token "synthetic-rotation"))
      (should-not (ende-codex--check-binding profile (ende-codex-auth--rotate record credentials)))
      (let ((new (ende-codex-auth--register 'missing (ende-codex-auth-test--credentials))))
        (should-error (ende-codex--check-binding missing new))
        (should-error (ende-codex--check-binding (ende-codex-capture-binding missing) new)))
      ;; Changing an alias, store, or opaque binding cannot rebind an old job.
      (should-error (ende-codex--check-binding
                     (plist-put (copy-sequence profile) :account 'work)
                     (ende-codex-auth-read 'personal)))
      (should-error (ende-codex--check-binding
                     (plist-put (copy-sequence profile) :codex-binding (make-symbol "lost"))
                     (ende-codex-auth-read 'personal)))
      (let ((ende-codex-auth--directory (expand-file-name "another" root)))
        (should-error (ende-codex--check-binding profile record))))))

(ert-deftest ende-codex-auth-refresh-is-account-and-registration-scoped ()
  (ende-codex-auth-test--with-store
    (let* ((credentials (plist-put (ende-codex-auth-test--credentials) :expires-at 0))
           (personal (ende-codex-auth--register 'personal credentials))
           (work (ende-codex-auth--register 'work credentials))
           (ende-codex--refresh-flights (make-hash-table :test #'equal))
           flights cancellations)
      (cl-letf (((symbol-function 'ende-codex--start-refresh-flight)
                 (lambda (flight) (push flight flights))))
        (unwind-protect
            (progn
              (push (ende-codex--refresh-auth personal #'ignore) cancellations)
              (push (ende-codex--refresh-auth personal #'ignore) cancellations)
              (should (= (length flights) 1))
              (push (ende-codex--refresh-auth work #'ignore) cancellations)
              (should (= (length flights) 2))
              (let ((replacement (ende-codex-auth--register
                                  'personal credentials (plist-get personal :revision) t)))
                (push (ende-codex--refresh-auth replacement #'ignore) cancellations)
                (should (= (length flights) 3))))
          (mapc #'funcall cancellations))))))

(ert-deftest ende-codex-auth-refresh-uses-issued-client-and-private-stdin ()
  (ende-codex-auth-test--with-store
    (let* ((credentials (plist-put (ende-codex-auth-test--credentials) :expires-at 0))
           (old (ende-codex-auth--register 'personal credentials))
           (profile (ende-codex-capture-binding (ende-codex-auth-test--profile 'personal)))
           (ende-codex--refresh-flights (make-hash-table :test #'equal))
           (ende-codex--refresh-processes nil)
           (real-make-process (symbol-function 'make-process))
           process command sent result cancel)
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest args)
                   (setq command (plist-get args :command)
                         process (funcall real-make-process :name "ende-test-refresh"
                                          :command '("cat") :connection-type 'pipe
                                          :noquery t :filter #'ignore :sentinel #'ignore))))
                ((symbol-function 'process-send-string)
                 (lambda (_process text) (setq sent text)))
                ((symbol-function 'process-send-eof) #'ignore))
        (unwind-protect
            (progn
              (setq cancel (ende-codex--resolve-auth (lambda (value) (setq result value)) profile))
              (should (string-match-p "client_id=issued-client-one" sent))
              (should (string-match-p "refresh_token=synthetic-refresh-one" sent))
              (should-not (string-match-p "synthetic-" (prin1-to-string command)))
              (let ((flight (gethash (ende-codex--refresh-key old) ende-codex--refresh-flights)))
                (with-temp-file (ende-codex--refresh-flight-headers-file flight)
                  (insert "HTTP/1.1 200 OK\r\n\r\n"))
                (ende-codex--refresh-filter
                 flight process
                 "{\"access_token\":\"synthetic-new-access\",\"refresh_token\":\"synthetic-new-refresh\",\"expires_in\":3600}")
                (cl-letf (((symbol-function 'process-status) (lambda (_) 'exit))
                          ((symbol-function 'process-exit-status) (lambda (_) 0)))
                  (ende-codex--refresh-sentinel flight process "finished\n")))
              (should-not (ende-error-p result))
              (should (equal (plist-get result :refresh-token) "synthetic-new-refresh"))
              (should (ende-codex-auth--same-registration-p old result))
              (should-not (ende-codex--check-binding profile result)))
          (when cancel (funcall cancel))
          (when (and process (process-live-p process)) (delete-process process)))))))

(ert-deftest ende-codex-auth-refresh-stale-winner-must-have-the-same-registration ()
  (ende-codex-auth-test--with-store
    (let* ((credentials (ende-codex-auth-test--credentials))
           (old (ende-codex-auth--register 'personal credentials))
           (rotated (ende-codex-auth--rotate old credentials)))
      (should-error (ende-codex--store-refresh old "stale-access" "stale-refresh" 3600 nil)
                    :type 'ende-codex-auth-conflict)
      (should (equal rotated (ende-codex--refresh-current old)))
      (ende-codex-auth--register 'personal credentials (plist-get rotated :revision) t)
      (should-not (ende-codex--refresh-current old)))))

(ert-deftest ende-codex-auth-resolve-never-falls-back-or-sends-replaced-account ()
  (ende-codex-auth-test--with-store
    (cl-letf (((symbol-function 'ende-codex--auth-entry)
               (lambda () (ert-fail "Ende account path accessed foreign credentials"))))
      (let (result)
        (ende-codex--resolve-auth (lambda (value) (setq result value))
                                 (ende-codex-auth-test--profile 'missing))
        (should (ende-error-p result)))
      (let* ((credentials (ende-codex-auth-test--credentials))
             (old (ende-codex-auth--register 'personal credentials))
             (profile (ende-codex-capture-binding (ende-codex-auth-test--profile 'personal)))
             result)
        (ende-codex--resolve-auth (lambda (value) (setq result value)) profile)
        (should (equal old result))
        (ende-codex-auth--register 'personal credentials (plist-get old :revision) t)
        (ende-codex--resolve-auth (lambda (value) (setq result value)) profile)
        (should (ende-error-p result))))))

(ert-deftest ende-codex-auth-validation-precedes-all-queue-credential-access ()
  (let ((base (ende-codex-auth-test--profile 'personal))
        (credential-accesses 0))
    (cl-letf (((symbol-function 'ende-codex--profile-auth)
               (lambda (&rest _) (cl-incf credential-accesses) (error "No credentials")))
              ((symbol-function 'ende-credentials--resolve)
               (lambda (&rest _) (cl-incf credential-accesses) (error "No credentials"))))
      (with-temp-buffer
        (let ((target (ende-target-create :buffer (current-buffer) :start 1 :end 1 :cursor 1)))
          (dolist (overrides '((:model nil) (:api unsupported) (:account nil)
                              (:auth-method none) (:stream-usage invalid)
                              (:url "https://attacker.test/responses")
                              (:parameters (:access_token "synthetic-secret"))
                              (:access-token "synthetic-secret")))
            (should-error (ende--job-from-request
                           (list 'insert :target target :profile (ende-plist-merge base overrides)))))
          (should (= credential-accesses 0))
          (ende-target-release target))))))

(ert-deftest ende-codex-auth-validation-retained-transport-profile-before-network ()
  (let ((base (ende-codex-auth-test--profile 'personal))
        (accesses 0))
    (cl-letf (((symbol-function 'ende-codex--profile-auth)
               (lambda (&rest _) (cl-incf accesses) (error "Credential access")))
              ((symbol-function 'make-process)
               (lambda (&rest _) (cl-incf accesses) (error "Network access"))))
      (dolist (overrides '((:model nil) (:api unsupported) (:account nil)
                          (:auth-method none) (:stream-usage invalid)
                          (:url "https://attacker.test/responses")
                          (:access-token "synthetic-secret")))
        (let (failure)
          (ende-codex-default '(("user" . "test"))
                              (list :on-error (lambda (value) (setq failure value)))
                              (ende-plist-merge base overrides))
          (should (ende-error-p failure))))
      (should (= accesses 0)))))

(provide 'ende-codex-auth-test)
;;; ende-codex-auth-test.el ends here
