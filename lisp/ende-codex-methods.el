;;; ende-codex-methods.el --- Trusted Codex OAuth protocols -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter Samarin
;;
;; Author: Peter Samarin <peter.samarin@gmail.com>
;; Maintainer: Peter Samarin <peter.samarin@gmail.com>
;; Keywords: convenience, tools
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

;; Trusted Codex OAuth protocols for Ende.

;;; Code:

(defconst ende-codex-native-client-id "app_EMoamEEZ73f0CkXaXp7hrann")

(defconst ende-codex--auth-methods
  '((codex-oauth
     :authorize-url "https://auth.openai.com/oauth/authorize"
     :token-url "https://auth.openai.com/oauth/token"
     :responses-url "https://chatgpt.com/backend-api/codex/responses"
     :models-url "https://chatgpt.com/backend-api/codex/models"
     :usage-url "https://chatgpt.com/backend-api/wham/usage"
     :scopes "openid profile email offline_access"
     :client-id "app_EMoamEEZ73f0CkXaXp7hrann"
     :redirect-uri "http://localhost:1455/auth/callback"
     :callback-port 1455 :client-kind fixed
     :account-header "ChatGPT-Account-Id" :catalog-schema native
     :websocket (:stream t :headers ("OpenAI-Beta: responses_websockets=2026-02-06"))
     :models-query ("client_version"))
    (chatgpt-plan-oauth
     :authorize-url "https://auth.openai.com/api/accounts/authorize"
     :token-url "https://auth.openai.com/api/accounts/oauth/token"
     :responses-url "https://api.openai.com/v1/responses"
     :models-url "https://api.openai.com/v1/models"
     :scopes "openid profile email offline_access resource.invoke chatgpt.tokens.use.direct"
     :resource "https://api.openai.com/v1" :client-kind dynamic :nonce t
     :catalog-schema public :websocket nil))
  "Single authority for the two trusted OAuth protocols.
Native Codex authorization and backend access remain experimental until live
verification; the public client registration does not imply approval for Ende.")

(defun ende-codex--auth-spec (method)
  "Return trusted configuration for METHOD, failing for unknown methods."
  (or (cdr (assq method ende-codex--auth-methods))
      (error "Unsupported Codex OAuth method: %S" method)))

(defun ende-codex--auth-property (method property)
  "Return trusted PROPERTY for METHOD."
  (plist-get (ende-codex--auth-spec method) property))

(provide 'ende-codex-methods)
;;; ende-codex-methods.el ends here
