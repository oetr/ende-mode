;;; ende-codex-methods-test.el --- OAuth endpoint tests -*- lexical-binding: t; -*-

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

;; Tests for OAuth endpoints.

;;; Code:

(require 'ert)
(require 'ende-codex-methods)

(ert-deftest ende-codex-methods-fixed-protocols ()
  (should (equal (mapcar #'car ende-codex--auth-methods)
                 '(codex-oauth chatgpt-plan-oauth)))
  (dolist (pair '((chatgpt-plan-oauth "https://api.openai.com/v1")
                  (codex-oauth "https://chatgpt.com/backend-api/codex")))
    (should (equal (ende-codex--auth-property (car pair) :responses-url)
                   (concat (cadr pair) "/responses")))
    (should (equal (ende-codex--auth-property (car pair) :models-url)
                   (concat (cadr pair) "/models"))))
  (should-error (ende-codex--auth-spec 'unknown)))

(require 'ende-provider-config)

(ert-deftest ende-codex-methods-resolution ()
  (let ((ende-providers '(openai-codex))
        (ende-custom-providers nil)
        (ende-builtin-provider-settings nil))
    (should (eq (plist-get (ende-provider-config-resolve 'openai-codex) :auth-method)
                'codex-oauth))
    (dolist (method '(chatgpt-plan-oauth codex-oauth))
      (let ((profile (ende-provider-config-resolve
                      'openai-codex nil nil (list :auth-method method))))
        (should (equal (plist-get profile :url)
                       (ende-codex--auth-property method :responses-url)))
        (should (equal (plist-get profile :models-url)
                       (ende-codex--auth-property method :models-url)))
        (should (equal (ende-provider-config-supported-auth profile)
                       '(codex-oauth chatgpt-plan-oauth)))))
    (let ((ende-builtin-provider-settings '((openai-codex :auth-method codex-oauth))))
      (should (eq (plist-get (ende-provider-config-resolve 'openai-codex) :auth-method)
                  'codex-oauth))
      (should (eq (plist-get (ende-provider-config-resolve
                              'openai-codex nil nil '(:auth-method chatgpt-plan-oauth))
                             :auth-method) 'chatgpt-plan-oauth)))
    (should-error (ende-provider-config-resolve 'openai-codex nil nil '(:auth-method unknown)))
    (dolist (key '(:base-url :url :models-url :endpoints))
      (let ((ende-builtin-provider-settings
             (list (list 'openai-codex key "https://example.org"))))
        (should-error (ende-provider-config-resolve 'openai-codex))))))

(ert-deftest ende-codex-methods-native-account-claim ()
  (let ((claims (make-hash-table :test #'equal))
        (auth (make-hash-table :test #'equal)))
    (should-error (ende-codex-native-account-id-from-claims claims))
    (puthash "https://api.openai.com/auth" auth claims)
    (puthash "chatgpt_account_id" "workspace" auth)
    (should (equal "workspace" (ende-codex-native-account-id-from-claims claims)))))

(require 'ende-codex-login)

(ert-deftest ende-codex-methods-login-protocols ()
  (dolist (method '(chatgpt-plan-oauth codex-oauth))
    (let* ((native (eq method 'codex-oauth))
           (attempt (ende-codex--make-login
                     :method method :account 'personal :state "state" :nonce "nonce"
                     :verifier "verifier" :host-id "host"
                     :client-id (if native ende-codex-native-client-id "dynamic_agent_client")
                     :redirect (if native "http://localhost:1455/auth/callback"
                                 "http://127.0.0.1:1234/auth/callback")))
           (url (ende-codex--login-authorize-url attempt))
           (body (ende-codex--code-exchange-body method attempt "code")))
      (should (string-prefix-p (ende-codex--auth-property method :authorize-url) url))
      (should (string-match-p "code_challenge_method=S256" url))
      (should (eq (not (null (string-match-p "resource=" url))) (not native)))
      (should (eq (not (null (string-match-p "resource=" body))) (not native)))
      (when native
        (should (string-match-p "codex_cli_simplified_flow=true" url))
        (should-not (string-match-p "nonce=" url))
        (let ((ende-codex--pending-login attempt) captured)
          (cl-letf (((symbol-function 'ende-codex-auth--request)
                     (lambda (endpoint form _limit _callback)
                       (setq captured (list endpoint form)) #'ignore)))
            (should-error (ende-codex--login-receive-callback attempt '(("state" . "wrong"))))
            (ende-codex--login-receive-callback attempt '(("state" . "state") ("code" . "code")))
            (should (equal (car captured) "https://auth.openai.com/oauth/token"))
            (should-error (ende-codex--login-receive-callback attempt '(("state" . "state") ("code" . "code")))))
          (ende-codex--login-finish attempt nil nil)))))
  (should-error (ende-codex--login-query "state=a&state=a")))

(require 'ende-codex)

(ert-deftest ende-codex-methods-transport-binding ()
  (dolist (method '(chatgpt-plan-oauth codex-oauth))
    (let* ((profile (append (list :codex-session-id "stable")
                            (ende-provider-config-resolve 'openai-codex nil nil (list :auth-method method))))
           (auth (list :auth-method method :account 'default :access-token "token"
                       :refresh-token "refresh" :client-id (if (eq method 'codex-oauth)
                                                              ende-codex-native-client-id "issued")
                       :chatgpt-account-id (and (eq method 'codex-oauth) "workspace")
                       :scope (ende-codex--auth-property method :scopes)))
           (headers (ende-codex--inference-headers method auth profile)))
      (should (member "session-id: stable" headers))
      (should (eq (not (null (member "ChatGPT-Account-Id: workspace" headers)))
                  (eq method 'codex-oauth)))
      (should (equal (ende-codex--validated-generation-url profile)
                     (ende-codex--auth-property method :responses-url)))
      (should (equal (ende-codex--catalog-url profile)
                     (ende-codex--auth-property method :models-url)))
      (should (equal (ende-codex--catalog-request-url profile)
                     (if (eq method 'codex-oauth)
                         (concat (ende-codex--auth-property method :models-url)
                                 "?client_version=" (url-hexify-string ende-client-version))
                       (ende-codex--auth-property method :models-url))))
      (should (equal (ende-codex--validated-refresh-url method)
                     (ende-codex--auth-property method :token-url)))
      (should (eq (not (null (string-match-p "resource=" (ende-codex--refresh-body method auth))))
                  (eq method 'chatgpt-plan-oauth)))
      (let ((other (if (eq method 'codex-oauth) 'chatgpt-plan-oauth 'codex-oauth)))
        (should-error (ende-codex--validated-generation-url
                       (plist-put (copy-sequence profile) :url (ende-codex--auth-property other :responses-url))))
        (should-error (ende-codex--inference-headers other auth profile))))))

(provide 'ende-codex-methods-test)

(require 'ende-codex-auth)

(ert-deftest ende-codex-methods-storage-isolation ()
  (let* ((root (make-temp-file "ende-methods-" t))
         (ende-codex-auth--directory (expand-file-name "private" root))
         (credentials (list :client-id "issued-client" :subject "verified-subject"
                            :access-token "access" :refresh-token "refresh"
                            :id-token "id" :expires-at 2000000000)))
    (unwind-protect
        (let* ((siwc (ende-codex-auth--register 'chatgpt-plan-oauth 'personal credentials))
               (native-credentials (append (list :chatgpt-account-id "workspace")
                                           (plist-put (copy-sequence credentials) :client-id
                                                      ende-codex-native-client-id)))
               (native (ende-codex-auth--register 'codex-oauth 'personal native-credentials)))
          (should (equal siwc (ende-codex-auth-read 'chatgpt-plan-oauth 'personal)))
          (should (equal native (ende-codex-auth-read 'codex-oauth 'personal)))
          (should-not (ende-codex-auth-lookup 'codex-oauth 'work))
          (should-not (ende-codex-auth--same-registration-p siwc native))
          (ende-codex-auth--rotate native (plist-put native-credentials :refresh-token "rotated"))
          (should (equal siwc (ende-codex-auth-read 'chatgpt-plan-oauth 'personal)))
          (should-error (ende-codex-auth--register 'codex-oauth 'work credentials))
          (should (= #o600 (file-modes (expand-file-name "connections.json" ende-codex-auth--directory)))))
      (delete-directory root t))))

;;; ende-codex-methods-test.el ends here
