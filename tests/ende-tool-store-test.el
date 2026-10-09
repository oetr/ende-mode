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

(provide 'ende-tool-store-test)

;;; ende-tool-store-test.el ends here
