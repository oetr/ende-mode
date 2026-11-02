;;; ende-codex-methods.el --- Trusted Codex OAuth protocols -*- lexical-binding: t; -*-

;;; Code:

(defconst ende-codex-native-client-id "app_EMoamEEZ73f0CkXaXp7hrann")

(defconst ende-codex--auth-methods
  '((chatgpt-plan-oauth
     :authorize-url "https://auth.openai.com/api/accounts/authorize"
     :token-url "https://auth.openai.com/api/accounts/oauth/token"
     :responses-url "https://api.openai.com/v1/responses"
     :models-url "https://api.openai.com/v1/models"
     :scopes "openid profile email offline_access resource.invoke chatgpt.tokens.use.direct"
     :resource "https://api.openai.com/v1" :client-kind dynamic :nonce t
     :catalog-schema public)
    (codex-oauth
     :authorize-url "https://auth.openai.com/oauth/authorize"
     :token-url "https://auth.openai.com/oauth/token"
     :responses-url "https://chatgpt.com/backend-api/codex/responses"
     :models-url "https://chatgpt.com/backend-api/codex/models"
     :scopes "openid profile email offline_access"
     :client-id "app_EMoamEEZ73f0CkXaXp7hrann"
     :redirect-uri "http://localhost:1455/auth/callback"
     :callback-port 1455 :client-kind fixed
     :account-header "ChatGPT-Account-Id" :catalog-schema native
     :models-query ("client_version")))
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
