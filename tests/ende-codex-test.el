;;; ende-codex-test.el --- Codex contracts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter Samarin
;;
;; Author: Peter Samarin <peter.samarin@gmail.com>
;; Maintainer: Peter Samarin <peter.samarin@gmail.com>
;; Keywords: lisp, tools
;; URL: https://github.com/oetr/ende-mode
;; SPDX-License-Identifier: AGPL-3.0-only

;; This file is part of Ende.

;; Ende is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, version 3 of the License.
;;
;; Ende is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with Ende.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for Codex contracts.

;;; Code:

(require 'ert)
(require 'ende-test-helpers)
(require 'ende-codex-auth-test)

(ert-deftest ende-codex-arguments-and-summaries-use-fragments ()
  (let* ((transcript (ende-transcript-create))
         (session (ende-codex--make-session
                   :callbacks (list :on-event
                                    (lambda (event) (ende-transcript-event transcript event)))))
         (stream (ende-codex--session-stream session)))
    (ende-codex--observe-output
     session (ende-protocol-json-read-string
              "{\"type\":\"function_call\",\"call_id\":\"c\",\"name\":\"read\",\"arguments\":\"\"}") 0)
    (cl-letf (((symbol-function 'ende-stream-text-string)
               (lambda (&rest _) (ert-fail "Materialized a streaming argument or summary"))))
      (dolist (fragment '("{\"a\":" "1}"))
        (ende-codex--update-tool-call-arguments session 0 fragment t)
        (ende-codex--emit-reasoning-summary session "s" fragment)))
    (should (equal (ende-transcript-entry-arguments
                    (gethash '(tool-call . 0) (ende-transcript-sources transcript)))
                   "{\"a\":1}"))
    (let ((call (car (ende-codex--tool-call-list session))))
      (should (equal (plist-get call :arguments) "{\"a\":1}"))
      (aset (plist-get call :arguments) 0 ?x)
      (should (equal (plist-get (car (ende-codex--tool-call-list session)) :arguments)
                     "{\"a\":1}")))
    (ende-stream-complete-reasoning-summary stream "s" "replaced")
    (ende-codex--update-tool-call-arguments session 0 "{}")
    (should (equal (plist-get (car (ende-codex--tool-call-list session)) :arguments) "{}"))
    (ende-codex--cancel session)))

(ert-deftest ende-codex-account-isolation-and-refresh ()
  (ende-codex-auth-test--with-store
   (let* ((personal (ende-codex-auth--register 'chatgpt-plan-oauth 'personal (ende-codex-auth-test--credentials)))
          (work (ende-codex-auth--register 'chatgpt-plan-oauth 'work (ende-codex-auth-test--credentials "work")))
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
       (should (equal work (ende-codex-auth-read 'chatgpt-plan-oauth 'work)))
       (puthash "scope" "openid offline_access" payload)
       (setq next (ende-codex--store-refresh next "limited" "limited-refresh" 3600 nil payload))
       (ende-codex--resolve-auth (lambda (auth) (setq resolved auth)) profile)
       (should (ende-error-p resolved))
       (setq next (ende-codex-auth--require-login next))
       (should (plist-get next :needs-login))
       (should-not (plist-get next :access-token))
       (should-not (plist-get next :refresh-token))
       (should (ende-codex-auth--same-registration-p personal next)))
     (should-not (string-match-p "scope=" (ende-codex--refresh-body 'chatgpt-plan-oauth personal)))
     (should (string-match-p "resource=" (ende-codex--refresh-body 'chatgpt-plan-oauth personal)))
     (should (equal (ende-codex--validated-refresh-url 'chatgpt-plan-oauth) (ende-codex--auth-property 'chatgpt-plan-oauth :token-url))))))

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

(ert-deftest ende-codex-endpoints-are-pinned ()
  (dolist (url (list (ende-codex--auth-property 'chatgpt-plan-oauth :authorize-url) (ende-codex--auth-property 'chatgpt-plan-oauth :token-url)
                     ende-codex--jwks-url (ende-codex--auth-property 'chatgpt-plan-oauth :models-url)
                     (ende-codex--auth-property 'chatgpt-plan-oauth :responses-url)))
    (should (equal (ende-codex-auth--endpoint url url) url)))
  (should-error (ende-codex-auth--endpoint
                 "https://auth.openai.com:8443/api/accounts/authorize"
                 (ende-codex--auth-property 'chatgpt-plan-oauth :authorize-url)))
  (should-error (ende-codex-auth--endpoint
                 (ende-codex--auth-property 'chatgpt-plan-oauth :models-url) (ende-codex--auth-property 'chatgpt-plan-oauth :responses-url)))
  (should-error (ende-codex-auth--endpoint
                 "https://attacker.test/api/accounts/authorize"
                 (ende-codex--auth-property 'chatgpt-plan-oauth :authorize-url)))
  (dolist (url '("http://auth.openai.com/api/accounts/authorize"
                 "https://auth.openai.com/api/accounts/authorize?x=1"
                 "https://auth.openai.com/api/accounts/authorize#x"
                 "https://auth.openai.com/api/accounts/token"))
    (should-error (ende-codex-auth--endpoint url (ende-codex--auth-property 'chatgpt-plan-oauth :authorize-url)))))

(ert-deftest ende-codex-requires-completion-event ()
  (let ((completed (ende-test--run-stream-request
                    'codex
                    (list "data: {\"type\":\"response.completed\",\"response\":{\"output\":[]}}\n\n")))
        (done (ende-test--run-stream-request
               'codex
               (list "data: {\"type\":\"response.done\",\"response\":{\"output\":[]}}\n\n")))
        (missing (ende-test--run-stream-request
                  'codex
                  (list "data: {\"type\":\"response.output_text.delta\",\"delta\":\"partial\"}\n\n"))))
    (should-not (plist-get completed :error))
    (should (plist-get completed :result))
    (should (ende-error-p (plist-get done :error)))
    (should (ende-error-p (plist-get missing :error)))))

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
