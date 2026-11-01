;;; ende-codex-test.el --- Codex contracts -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'ende-test-helpers)
(require 'ende-codex-auth-test)

(ert-deftest ende-codex-account-isolation-and-refresh ()
  (ende-codex-auth-test--with-store
   (let* ((personal (ende-codex-auth--register 'personal (ende-codex-auth-test--credentials)))
          (work (ende-codex-auth--register 'work (ende-codex-auth-test--credentials "work")))
          (profile (ende-codex-capture-binding (ende-codex-auth-test--profile 'personal)))
          (payload (ende-codex-auth--json
                    "{\"access_token\":\"rotated-access\",\"refresh_token\":\"rotated-refresh\",\"token_type\":\"Bearer\",\"expires_in\":3600}"))
          resolved)
     (ende-codex--resolve-auth (lambda (auth) (setq resolved auth)) profile)
     (should (equal personal resolved))
     (let ((next (ende-codex--store-refresh personal "rotated-access" "rotated-refresh" 3600 nil payload)))
       (should (ende-codex-auth--same-registration-p personal next))
       (should (equal (plist-get personal :scope) (plist-get next :scope)))
       (should-error (ende-codex--store-refresh personal "stale" "stale" 3600 nil payload)
                     :type 'ende-codex-auth-conflict)
       (should-not (ende-codex--check-binding profile next))
       (should (equal work (ende-codex-auth-read 'work)))
       (puthash "scope" "openid offline_access" payload)
       (setq next (ende-codex--store-refresh next "limited" "limited-refresh" 3600 nil payload))
       (ende-codex--resolve-auth (lambda (auth) (setq resolved auth)) profile)
       (should (ende-error-p resolved))
       (setq next (ende-codex-auth--require-login next))
       (should (plist-get next :needs-login))
       (should-not (plist-get next :access-token))
       (should-not (plist-get next :refresh-token))
       (should (ende-codex-auth--same-registration-p personal next)))
     (should-not (string-match-p "scope=" (ende-codex--refresh-body "secret" "issued")))
     (should (string-match-p "resource=" (ende-codex--refresh-body "secret" "issued")))
     (should (equal (ende-codex--validated-refresh-url) ende-codex--token-url)))))

(ert-deftest ende-codex-stream-contract ()
  (let* ((stream (concat "data: {\"type\":\"response.output_text.delta\",\"delta\":\"answer\"}\n\n"
                         "data: {\"type\":\"response.completed\",\"response\":{\"usage\":{\"output_tokens\":7}}}\n\n"))
         (out (ende-test--run-stream-request 'codex (list stream))))
    (should-not (plist-get out :error))
    (should (equal (plist-get (plist-get out :result) :content) "answer"))
    (should-not (string-match-p "transport-test-token" (prin1-to-string (plist-get out :command))))
    (should (string-match-p "Authorization: Bearer transport-test-token" (plist-get out :config-text)))))

(provide 'ende-codex-test)
;;; ende-codex-test.el ends here
