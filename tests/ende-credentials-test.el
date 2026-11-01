;;; ende-credentials-test.el --- Exact credentials -*- lexical-binding: t; -*-

;;; Code:
(require 'ert)
(require 'ende-curl)

(ert-deftest ende-credentials-native-binding-and-ambiguity ()
  (let* ((file (make-temp-file "ende-authinfo-"))
         (auth-sources (list file))
         (profile '(:url "https://example.test/v1" :auth-method auth-source
                    :auth-source-host "example.test" :auth-source-port 443
                    :auth-source-user "work" :auth-scheme x-api-key)))
    (unwind-protect
        (progn
          (set-file-modes file #o600)
          (with-temp-file file (insert "machine example.test port 443 login work password test-key\n"))
          (should (equal (ende-curl--auth-secret profile) "test-key"))
          (let ((snapshot (ende-credentials-capture profile)))
            (should-not (string-match-p "test-key" (prin1-to-string snapshot)))
            (should (equal (ende-curl--auth-secret snapshot) "test-key"))
            (with-temp-file file (insert "machine example.test port 443 login work password replacement\n"))
            (should-error (ende-curl--auth-secret snapshot)))
          (with-temp-file file (insert "machine example.test port 443 login work password test-key\n"))
          (let ((config (ende-curl--config profile)))
            (unwind-protect
                (with-temp-buffer
                  (insert-file-contents config)
                  (should (string-match-p "x-api-key: test-key" (buffer-string))))
              (delete-file config)))
          (with-temp-file file
            (insert "machine example.test port 443 login work password first\n"
                    "machine example.test port 0443 login work password second\n"))
          (should-error (ende-curl--auth-secret profile))
          (with-temp-file file (insert "machine example.test login work password wildcard\n"))
          (should-error (ende-curl--auth-secret profile))
          (should-error (ende-curl--auth-secret (plist-put (copy-sequence profile) :auth-source-user "other")))
          (should-error (ende-curl--validated-url (plist-put (copy-sequence profile) :url "http://localhost/v1"))))
      (delete-file file))))

(ert-deftest ende-credentials-skips-ignored-sources ()
  (let* ((file (make-temp-file "ende-authinfo-"))
         (auth-sources (list (concat file ".missing") file))
         (profile '(:auth-method auth-source :auth-source-host "example.test"
                    :auth-source-port 443 :auth-source-user "work")))
    (unwind-protect
        (progn
          (set-file-modes file #o600)
          (with-temp-file file
            (insert "machine example.test port 443 login work password test-key\n"))
          (should (equal (ende-credentials-secret profile) "test-key")))
      (delete-file file))))

(provide 'ende-credentials-test)
;;; ende-credentials-test.el ends here
