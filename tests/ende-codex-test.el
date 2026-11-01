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
    (should (string-match-p "Authorization: Bearer transport-test-token" (plist-get out :config-text))))
  (let* ((definition
          (list (cons :type "function")
                (cons :function (list (cons :name "read_file")
                                      (cons :description "Read a file")
                                      (cons :parameters '((:type . "object")))))))
         (body (ende-codex--request-body
                (list '("system" . "Be brief") '("user" . "Read it")
                      (list :tool-call-id "call_1" :content "File contents"))
                (list :model "model-a" :tool-definitions (list definition))))
         (input (alist-get :input body))
         (tools (alist-get :tools body)))
    (should (eq (alist-get :store body) json-false))
    (should (eq (alist-get :stream body) t))
    (should (equal (alist-get :model body) "model-a"))
    (should (equal (alist-get :role (car input)) "developer"))
    (should (equal (alist-get :call_id (car (last input))) "call_1"))
    (should (equal (alist-get :type (car tools)) "namespace"))
    (should (equal (alist-get :name (car (alist-get :tools (car tools)))) "read_file"))
    (should-not (alist-get :max_output_tokens body))))

(ert-deftest ende-codex-catalog-keeps-listed-metadata ()
  (let* ((models (json-parse-string
                  (json-encode
                   (list (cons 'models
                               (vector
                                (list (cons 'slug "model-a")
                                      (cons 'display_name "Model A")
                                      (cons 'visibility "list")
                                      (cons 'default_reasoning_level "high")
                                      (cons 'supported_reasoning_levels
                                            (vector (list (cons 'effort "low"))
                                                    (list (cons 'effort "high")))))
                                (list (cons 'slug "internal")
                                      (cons 'visibility "hidden"))))))))
         (entries (delq nil (mapcar #'ende-codex--catalog-entry
                                    (append (gethash "models" models) nil))))
         (catalog (ende-provider--catalog-from-models entries)))
    (should (equal (plist-get catalog :models) (list "model-a")))
    (should (equal (plist-get (car (plist-get catalog :details)) :name) "Model A"))
    (should (equal (plist-get (car entries) :efforts) (list "low" "high")))
    (should (equal (plist-get (car entries) :default) "high"))))

(ert-deftest ende-codex-cache-key-follows-registration ()
  (let* ((base (list :name 'codex :service 'openai-codex :api 'openai-responses
                     :account 'personal :model-discovery-function #'ignore))
         (first (ende-provider--catalog-key
                 (plist-put (copy-sequence base) :model-discovery-key '("registration-a"))))
         (second (ende-provider--catalog-key
                  (plist-put (copy-sequence base) :model-discovery-key '("registration-b")))))
    (should first)
    (should-not (equal first second))))

(provide 'ende-codex-test)
;;; ende-codex-test.el ends here
