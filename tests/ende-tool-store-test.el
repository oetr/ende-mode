;;; ende-tool-store-test.el --- Tool payload store tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'sqlite)
(require 'ende-tool-store)

(ert-deftest ende-test/tool-store-roundtrips-immutable-records-and-switches-files ()
  (let* ((directory (make-temp-file "ende-tool-store-" t))
         (first (expand-file-name "first.sqlite3" directory))
         (second (expand-file-name "nested/second.sqlite3" directory))
         (ende-tool-store-file first)
         (request (concat "{\"arguments\":\"" (make-string 250000 ?x) "\"}"))
         (result "{\"content\":\"exact\\nresult\"}"))
    (unwind-protect
        (progn
          (ende-tool-store-put "execution-1" 'request request)
          (ende-tool-store-put "execution-1" 'result result)
          (ende-tool-store-put "execution-1" 'request request)
          (should (equal (ende-tool-store-get "execution-1" 'request) request))
          (should (equal (ende-tool-store-get "execution-1" 'result) result))
          (should-error (ende-tool-store-put "execution-1" 'request "different"))
          (let ((ende-tool-store-file second))
            (should-not (ende-tool-store-get "execution-1" 'request))
            (ende-tool-store-put "execution-2" 'request "{}")
            (should (equal (ende-tool-store-get "execution-2" 'request) "{}")))
          (should (equal (ende-tool-store-get "execution-1" 'request) request)))
      (delete-directory directory t))))

(ert-deftest ende-test/tool-store-semantic-executions-preserve-payloads-and-presence ()
  (let* ((directory (make-temp-file "ende-tool-store-semantic-" t))
         (ende-tool-store-file (expand-file-name "store.sqlite3" directory))
         (arguments "{ \"path\" : \"a.el\", \"exact\" : true }")
         (content "{\"ok\":true,\"text\":\"exact result\"}")
         (execution (ende-tool-store-create-execution
                     "provider-call-unchanged" "read_file" arguments))
         (execution-id (plist-get execution :execution-id)))
    (unwind-protect
        (progn
          (should (plist-get execution :request-present-p))
          (should-not (plist-get execution :result-present-p))
          (should (equal (plist-get execution :provider-call-id)
                         "provider-call-unchanged"))
          (should (equal (plist-get execution :arguments) arguments))
          (ende-tool-store-store-result execution-id "provider-call-unchanged"
                                        content)
          (let ((loaded (ende-tool-store-load-execution execution-id)))
            (should (plist-get loaded :request-present-p))
            (should (plist-get loaded :result-present-p))
            (should (equal (plist-get loaded :provider-call-id)
                           "provider-call-unchanged"))
            (should (equal (plist-get loaded :arguments) arguments))
            (should (equal (plist-get loaded :content) content)))
          (let ((result-only-id "result-only"))
            (ende-tool-store-put
             result-only-id 'result
             "{\"provider_call_id\":\"archived-call\",\"content\":\"archived\"}")
            (let ((loaded (ende-tool-store-load-execution result-only-id)))
              (should-not (plist-get loaded :request-present-p))
              (should (plist-get loaded :result-present-p))
              (should (equal (plist-get loaded :provider-call-id)
                             "archived-call"))
              (should (equal (plist-get loaded :content) "archived"))))
          (let ((mismatched-id "mismatched-ids"))
            (ende-tool-store-put
             mismatched-id 'request
             "{\"provider_call_id\":\"request-call\",\"tool_name\":\"read_file\",\"arguments\":\"{}\"}")
            (ende-tool-store-put
             mismatched-id 'result
             "{\"provider_call_id\":\"result-call\",\"content\":\"done\"}")
            (should-error (ende-tool-store-load-execution mismatched-id)))
          (let ((malformed-id "malformed-payload"))
            (ende-tool-store-put
             malformed-id 'result
             "{\"provider_call_id\":\"call\",\"content\":5}")
            (should-error
             (ende-tool-store-load-execution malformed-id))))
      (delete-directory directory t))))

(ert-deftest ende-test/tool-store-rejects-unsupported-schema-version ()
  (let* ((directory (make-temp-file "ende-tool-store-version-" t))
         (file (expand-file-name "store.sqlite3" directory))
         (ende-tool-store-file file)
         (database (sqlite-open file nil)))
    (unwind-protect
        (progn
          (sqlite-execute database "PRAGMA user_version = 42")
          (sqlite-close database)
          (setq database nil)
          (let ((failure (should-error
                          (ende-tool-store-put "execution" 'request "{}"))))
            (should (string-match-p "use a fresh database"
                                    (error-message-string failure)))))
      (when database (sqlite-close database))
      (delete-directory directory t))))

(ert-deftest ende-test/tool-store-requires-built-in-sqlite-support ()
  (let* ((directory (make-temp-file "ende-tool-store-unavailable-" t))
         (ende-tool-store-file (expand-file-name "store.sqlite3" directory)))
    (unwind-protect
        (cl-letf (((symbol-function 'sqlite-available-p) (lambda () nil)))
          (let ((failure (should-error
                          (ende-tool-store-put "execution" 'request "{}"))))
            (should (string-match-p "Emacs build with SQLite support"
                                    (error-message-string failure)))
            (should-not (file-exists-p ende-tool-store-file))))
      (delete-directory directory t))))

(ert-deftest ende-test/tool-store-size-does-not-create-and-counts-sidecars ()
  (let* ((directory (make-temp-file "ende-tool-store-size-" t))
         (file (expand-file-name "store.sqlite3" directory))
         (ende-tool-store-file file))
    (unwind-protect
        (progn
          (should-not (ende-tool-store-size))
          (should-not (file-exists-p file))
          (ende-tool-store-put "execution" 'request "{}")
          (with-temp-file (concat file "-wal") (insert "wal"))
          (with-temp-file (concat file "-shm") (insert "shared"))
          (let* ((footprint (ende-tool-store--footprint))
                 (files (cadr footprint))
                 (reported (ende-tool-store-size)))
            (should (equal (mapcar #'car files)
                           (list file (concat file "-wal")
                                 (concat file "-shm"))))
            (should (= reported (apply #'+ (mapcar #'cdr files)))))
          (with-temp-file (concat file "-journal") (insert "journal"))
          (should (member (concat file "-journal")
                          (mapcar #'car (cadr (ende-tool-store--footprint))))))
      (delete-directory directory t))))

(ert-deftest ende-test/tool-store-keeps-databases-and-live-sidecars-private ()
  (skip-unless (not (memq system-type '(windows-nt ms-dos))))
  (let* ((directory (make-temp-file "ende-tool-store-private-" t))
         (user-emacs-directory (file-name-as-directory directory)))
    (unwind-protect
        (with-file-modes #o777
          (dolist (relative '("ende/tool-payloads.sqlite3" "shared/custom.sqlite3"))
            (let* ((ende-tool-store-file (expand-file-name relative directory))
                   (parent (file-name-directory ende-tool-store-file))
                   (dedicated (string-prefix-p "ende/" relative)))
              (make-directory parent t)
              (set-file-modes parent #o755)
              (ende-tool-store-put "private" 'request "exact payload")
              (ende-tool-store--call-with-database
               t
               (lambda (database)
                 (sqlite-execute database "PRAGMA journal_mode = WAL")
                 (sqlite-execute database "UPDATE tool_payloads SET payload = payload")
                 (dolist (file (ende-tool-store--files ende-tool-store-file))
                   (when (file-exists-p file)
                     (should (= (logand (file-modes file) #o777) #o600))
                     (set-file-modes file #o666)))
                 (when dedicated (set-file-modes parent #o777))
                 (should (equal (ende-tool-store-get "private" 'request)
                                "exact payload"))
                 (should (= (logand (file-modes parent) #o777)
                            (if dedicated #o700 #o755)))
                 (dolist (file (ende-tool-store--files ende-tool-store-file))
                   (when (file-exists-p file)
                     (should (= (logand (file-modes file) #o777) #o600))))))
              (should (= (logand (file-modes ende-tool-store-file) #o777) #o600))
              (delete-file ende-tool-store-file)
              (let ((unrelated (expand-file-name "unrelated" directory)))
                (with-temp-file unrelated (insert "not a database"))
                (set-file-modes unrelated #o644)
                (dolist (file (list ende-tool-store-file
                                    (concat ende-tool-store-file "-wal")))
                  (make-symbolic-link unrelated file)
                  (should-error (ende-tool-store-get "private" 'request))
                  (should (= (logand (file-modes unrelated) #o777) #o644))
                  (delete-file file))))))
      (delete-directory directory t))))

(provide 'ende-tool-store-test)

;;; ende-tool-store-test.el ends here
