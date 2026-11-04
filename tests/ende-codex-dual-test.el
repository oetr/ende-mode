;;; ende-codex-dual-test.el --- Dual OAuth integration -*- lexical-binding: t; -*-

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

;; Tests for dual OAuth integration.

;;; Code:

(require 'ert)
(require 'ende-test-helpers)
(require 'ende-codex-auth-test)
(require 'ende-selector)
(require 'ende-agent)

(defun ende-codex-dual-test--credentials (method)
  "Return synthetic credentials specific to METHOD."
  (let ((credentials (ende-codex-auth-test--credentials)))
    (setq credentials (plist-put credentials :scope (ende-codex--auth-property method :scopes)))
    (if (eq method 'codex-oauth)
        (append (list :chatgpt-account-id "workspace")
                (plist-put credentials :client-id ende-codex-native-client-id))
      credentials)))

(ert-deftest ende-codex-dual-wire-body-and-affinity ()
  (let ((ende-builtin-provider-settings nil) bodies)
    (dolist (method '(chatgpt-plan-oauth codex-oauth))
      (let* ((profile (ende-codex--account-profile
                       (ende-provider-config-resolve 'openai-codex "model" nil (list :auth-method method))))
             (profile (plist-put profile :codex-session-id "stable-session"))
             (out (ende-test--run-stream-request
                   'codex '("data: {\"type\":\"response.completed\",\"response\":{\"output\":[]}}\n\n")
                   nil nil profile)))
        (should-not (plist-get out :error))
        (should (member (ende-codex--auth-property method :responses-url) (plist-get out :command)))
        (should (string-match-p "session-id: stable-session" (plist-get out :config-text)))
        (should (eq (not (null (string-match-p "ChatGPT-Account-Id:" (plist-get out :config-text))))
                    (eq method 'codex-oauth)))
        (push (plist-get out :request-body) bodies)))
    (should (equal (car bodies) (cadr bodies)))))

(ert-deftest ende-codex-dual-queue-reconnect-and-delegation ()
  (ende-codex-auth-test--with-store
    (let* ((ende-builtin-provider-settings nil)
           (ende-credentials--bindings (make-hash-table :test #'eq))
           (siwc (ende-codex-auth--register 'chatgpt-plan-oauth 'personal
                                          (ende-codex-dual-test--credentials 'chatgpt-plan-oauth)))
           (native (ende-codex-auth--register 'codex-oauth 'personal
                                            (ende-codex-dual-test--credentials 'codex-oauth)))
           (profile (ende-codex-capture-binding
                     (ende-provider--compose-profile 'openai-codex "model" nil
                                                     '(:auth-method codex-oauth :account personal))))
           (other (ende-codex-capture-binding
                   (ende-provider--compose-profile 'openai-codex "model" nil
                                                   '(:auth-method chatgpt-plan-oauth :account personal))))
           (family (ende--make-job-family :inference-template profile))
           (child (ende-agent-profile-for-policy family nil)))
      (should (eq (plist-get child :auth-method) 'codex-oauth))
      (should (eq (plist-get child :codex-binding) (plist-get profile :codex-binding)))
      (should-not (ende-codex--check-binding child native))
      (should-error (ende-codex--check-binding profile siwc))
      (let ((replacement (ende-codex-auth--register
                          'codex-oauth 'personal (ende-codex-dual-test--credentials 'codex-oauth)
                          (plist-get native :revision) t)))
        (should-error (ende-codex--check-binding child replacement))
        (should-not (ende-codex--check-binding other siwc)))
      (should-error (ende-codex--profile-auth
                     (plist-put (copy-tree profile) :account 'missing))))))

(ert-deftest ende-codex-dual-refresh-isolation-and-rotation ()
  (ende-codex-auth-test--with-store
    (let* ((ende-codex--refresh-flights (make-hash-table :test #'equal))
           (siwc (ende-codex-auth--register 'chatgpt-plan-oauth 'personal
                                          (ende-codex-dual-test--credentials 'chatgpt-plan-oauth)))
           (native (ende-codex-auth--register 'codex-oauth 'personal
                                            (ende-codex-dual-test--credentials 'codex-oauth)))
           flights results cancels)
      (cl-letf (((symbol-function 'ende-codex--refresh-flight-begin)
                 (lambda (flight) (cl-pushnew flight flights))))
        (dolist (record (list native native siwc))
          (push (ende-codex--refresh-auth record (lambda (result) (push result results))) cancels))
        (should (= 2 (length flights)))
        (should (= 2 (hash-table-count ende-codex--refresh-flights)))
        (dolist (flight flights)
          (let ((method (plist-get (ende-codex--refresh-flight-auth flight) :auth-method)))
            (ende-codex--refresh-response
             flight 200 (ende-codex-auth--json
                         (format "{\"access_token\":\"new-%s\",\"refresh_token\":\"rotated-%s\",\"token_type\":\"Bearer\",\"expires_in\":1200}" method method)))))
        (should (= 3 (length results)))
        (should (= 0 (hash-table-count ende-codex--refresh-flights)))
        (dolist (method '(chatgpt-plan-oauth codex-oauth))
          (let ((record (ende-codex-auth-read method 'personal)))
            (should (equal (plist-get record :refresh-token) (format "rotated-%s" method)))
            (should (ende-codex-auth--same-registration-p
                     record (if (eq method 'codex-oauth) native siwc)))))
        (mapc #'funcall cancels)))))

(ert-deftest ende-codex-dual-native-identity-continuity ()
  (let* ((claims (ende-codex-auth--json
                  (format "{\"iss\":\"https://auth.openai.com\",\"aud\":\"%s\",\"sub\":\"subject\",\"iat\":%d,\"exp\":%d,\"https://api.openai.com/auth\":{\"chatgpt_account_id\":\"workspace\"}}"
                          ende-codex-native-client-id (- (floor (float-time)) 10) (+ (floor (float-time)) 3600))))
         (record (list :auth-method 'codex-oauth :client-id ende-codex-native-client-id
                       :subject "subject" :chatgpt-account-id "workspace")))
    (cl-letf (((symbol-function 'ende-codex-auth--signed-claims) (lambda (&rest _) claims)))
      (should (eq claims (ende-codex--verify-id-token 'codex-oauth "synthetic" ende-codex-native-client-id nil)))
      (should (eq claims (ende-codex-auth--refresh-claims "synthetic" record nil)))
      (dolist (pair '((:subject . "changed") (:chatgpt-account-id . "changed") (:client-id . "issued-siwc")))
        (should-error (ende-codex-auth--refresh-claims
                       "synthetic" (plist-put (copy-tree record) (car pair) (cdr pair)) nil)))
      (dolist (pair '(("iss" . "https://attacker.test") ("aud" . "wrong") ("exp" . 1) ("sub" . "")))
        (let ((original (gethash (car pair) claims)))
          (puthash (car pair) (cdr pair) claims)
          (should-error (ende-codex--verify-id-token 'codex-oauth "synthetic" ende-codex-native-client-id nil))
          (puthash (car pair) original claims))))))

(ert-deftest ende-codex-dual-listener-cancellation ()
  (let ((ende-codex--pending-login nil) listener)
    (unwind-protect
        (cl-letf (((symbol-function 'browse-url) #'ignore)
                  ((symbol-function 'ende-codex-auth-lookup) (lambda (&rest _) nil))
                  ((symbol-function 'ende-codex-auth-host-id) (lambda () "host")))
          (ende-codex--begin-login 'codex-oauth 'personal #'ignore)
          (setq listener (ende-codex--login-listener ende-codex--pending-login))
          (should (= 1455 (process-contact listener :service)))
          (should (equal "127.0.0.1" (process-contact listener :host)))
          (should (equal "http://localhost:1455/auth/callback"
                         (ende-codex--login-redirect ende-codex--pending-login)))
          (ende-codex-cancel-login)
          (should-not ende-codex--pending-login)
          (should-not (process-live-p listener)))
      (ende-codex-cancel-login))))

(ert-deftest ende-codex-dual-recognized-catalogs ()
  (should (equal (ende-codex--parse-catalog "{\"data\":[{\"id\":\"model\"}]}" 'chatgpt-plan-oauth)
                 '((:id "model"))))
  (should (equal (plist-get (car (ende-codex--parse-catalog
                                "{\"models\":[{\"slug\":\"model\",\"visibility\":\"list\"}]}" 'codex-oauth)) :id) "model"))
  (dolist (method '(chatgpt-plan-oauth codex-oauth))
    (should-error (ende-codex--parse-catalog "{\"error\":\"denied\"}" method))))

(ert-deftest ende-codex-dual-catalog-command-keeps-query-out-of-profile-validation ()
  (let* ((native (ende-provider-config-resolve 'openai-codex nil nil
                                                '(:auth-method codex-oauth)))
         (url (ende-codex--catalog-request-url native))
         (command (ende-codex--catalog-command url "headers" "config")))
    (should (equal (ende-codex--catalog-url native)
                   "https://chatgpt.com/backend-api/codex/models"))
    (should (equal (ende-curl--validated-url (list :url (ende-codex--catalog-url native)))
                   "https://chatgpt.com/backend-api/codex/models"))
    (should (equal (cadr (member "--url" command)) url))
    (should (string-match-p "client_version=1.0.0" (cadr (member "--url" command))))))

(ert-deftest ende-codex-dual-siwc-model-catalog-recognized-shapes ()
  (should (equal (ende-codex--parse-catalog
                  "{\"data\":[{\"id\":\"gpt-public\"}]}" 'chatgpt-plan-oauth)
                 '((:id "gpt-public"))))
  (should (equal (mapcar (lambda (entry) (plist-get entry :id))
                         (ende-codex--parse-catalog
                          "{\"models\":[{\"slug\":\"gpt-plan\",\"visibility\":\"list\",\"display_name\":\"GPT Plan\"}]}"
                          'chatgpt-plan-oauth))
                 '("gpt-plan")))
  (should-error (ende-codex--parse-catalog "{\"error\":\"denied\"}" 'chatgpt-plan-oauth)))

(ert-deftest ende-codex-dual-catalog-url-advertises-ende-version ()
  (let ((siwc (ende-provider-config-resolve 'openai-codex nil nil
                                             '(:auth-method chatgpt-plan-oauth)))
        (native (ende-provider-config-resolve 'openai-codex nil nil
                                               '(:auth-method codex-oauth))))
    (should (equal (ende-codex--catalog-request-url siwc)
                   "https://api.openai.com/v1/models"))
    (should (equal (ende-codex--catalog-request-url native)
                   (concat "https://chatgpt.com/backend-api/codex/models?client_version="
                           (url-hexify-string ende-client-version))))
    (should (string= ende-client-version "1.0.0"))))

(ert-deftest ende-codex-dual-usage-is-native-only-and-account-bound ()
  (let* ((ende-builtin-provider-settings nil)
         (native (ende-provider-config-resolve
                  'openai-codex "model" nil '(:auth-method codex-oauth :account personal)))
         (siwc (ende-provider-config-resolve
                'openai-codex "model" nil '(:auth-method chatgpt-plan-oauth :account personal)))
         (auth '(:auth-method codex-oauth :account personal
                 :client-id "app_EMoamEEZ73f0CkXaXp7hrann"
                 :access-token "native-token" :chatgpt-account-id "workspace"
                 :scope "openid offline_access"))
         (headers nil) (request nil))
    (should (equal (ende-codex--usage-url native)
                   "https://chatgpt.com/backend-api/wham/usage"))
    (should-error (ende-codex--usage-url siwc))
    (should (equal (ende-codex--usage-headers native auth)
                   '("Authorization: Bearer native-token" "originator: opencode"
                     "ChatGPT-Account-Id: workspace")))
    (should-error (ende-codex--usage-headers siwc auth))
    (cl-letf (((symbol-function 'ende-codex--resolve-auth)
               (lambda (callback _profile) (funcall callback auth) nil))
              ((symbol-function 'ende-curl-start-request)
               (lambda (&rest arguments)
                 (setq request arguments)
                 (list :process nil :response-buffer (generate-new-buffer " *usage-probe*")))))
      (let ((cancel (ende-codex-probe-usage
                     native (lambda (&rest _) nil))))
        (setq headers (plist-get request :internal-headers))
        (should (member "originator: opencode" headers))
        (should (member "Authorization: Bearer native-token" headers))
        (should (member "ChatGPT-Account-Id: workspace" headers))
        (should (member "--request" (funcall (plist-get request :command) "headers" nil)))
        (funcall cancel)))))

(ert-deftest ende-codex-dual-usage-parser-and-string ()
  (let ((quota (ende-codex--parse-usage
                "{\"rate_limit\":{\"primary_window\":{\"used_percent\":25,\"reset_at\":2000003600,\"limit_window_seconds\":18000},\"secondary_window\":{\"used_percent\":50,\"reset_at\":2000010000,\"limit_window_seconds\":604800}}}")))
    (should (equal (ende-codex-usage-string quota 2000000000)
                   "1h: 75% left · weekly: 50% left · reset in 1h"))
    (should-error (ende-codex--parse-usage "{\"rate_limit\":{}}"))))

(ert-deftest ende-codex-dual-selector-method-draft-and-apply ()
  (save-window-excursion
    (let* ((source (generate-new-buffer " *dual-selector-source*"))
           (ende-providers '(openai-codex))
           (ende-builtin-provider-settings '((openai-codex :auth-method chatgpt-plan-oauth :models ("model"))))
           (ende-model-selection--last-used nil)
           (ende-model-selection--buffer-selections (make-hash-table :test #'equal))
           (ende-model-selection--provider-selections (make-hash-table :test #'eq))
           (ende-model-selection--buffer-provider-selections (make-hash-table :test #'equal))
           (ende-provider--catalog-cache (make-hash-table :test #'equal))
           registered connected login-callback (prompts 0) (usage-refreshes 0))
      (unwind-protect
          (cl-letf (((symbol-function 'ende-codex-auth-lookup)
                     (lambda (_method _account) registered))
                    ((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) (cl-incf prompts) t))
                    ((symbol-function 'ende-codex--begin-login)
                     (lambda (method account callback)
                       (setq connected (list method account)
                             login-callback callback)))
                    ((symbol-function 'ende-codex--login-report) #'ignore)
                    ((symbol-function 'ende-codex-probe-usage)
                     (lambda (_profile callback)
                       (cl-incf usage-refreshes)
                       (funcall callback nil nil)
                       #'ignore)))
            (switch-to-buffer source)
            (ende-selector-open)
            (let* ((selector (current-buffer))
                   (old-key (plist-get (ende-selector--provider-profile) :model-discovery-key)))
              (ende-selector-configure-connection)
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (_prompt choices &rest _)
                           (should (equal choices '("codex-oauth" "chatgpt-plan-oauth")))
                           "codex-oauth")))
                (ende-selector-connection-auth))
              (should-not connected)
              (let ((profile (ende-selector-connection--profile)))
                (should (equal (plist-get profile :url) "https://chatgpt.com/backend-api/codex/responses"))
                (should-not (equal old-key (plist-get profile :model-discovery-key))))
              (should (string-match-p "not connected" (buffer-string)))
              (should (eq (lookup-key ende-selector-connection-mode-map (kbd "l"))
                          #'ende-selector-connection-login))
              (ende-selector-connection-apply)
              (should-not connected)
              (should (eq (current-buffer) selector))
              (should (eq (plist-get (ende-selector--provider-profile) :auth-method) 'codex-oauth))
              (ende-selector-configure-connection)
              (should (eq (plist-get (ende-selector-connection--profile) :auth-method) 'codex-oauth))
              (should (string-match-p "codex-oauth" (buffer-string)))
              (ende-selector-connection-login)
              (should (equal connected '(codex-oauth default)))
              (should (= prompts 0))
              (setq registered '(:auth-method codex-oauth :account default
                                 :registration-id "new-registration" :revision "revision"))
              (funcall login-callback
                       '(:auth-method codex-oauth :account default :scope "offline_access") nil)
              (should (= usage-refreshes 2))
              (with-current-buffer selector
                (should (eq ende-selector--model-state :ready)))
              (ende-selector-connection-cancel)
              (ende-selector-configure-connection)
              (should (string-match-p "connected" (buffer-string)))
              (ende-selector-connection-login)
              (should (= prompts 1))
              (let ((credentials (copy-tree registered)))
                (funcall login-callback nil (ende-error-create :kind 'auth :message "failed"))
                (should (equal registered credentials)))
              (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) (cl-incf prompts) nil)))
                (ende-selector-connection-login))
              (should (= prompts 2))
              (should (= usage-refreshes 2))
              (ende-selector-connection-cancel)))
        (when-let* ((selector (get-buffer ende-selector-buffer-name)))
          (with-current-buffer selector (ende-selector-close)))
        (kill-buffer source)))))

(ert-deftest ende-codex-dual-selector-login-keeps-draft-separate ()
  (save-window-excursion
    (let ((source (generate-new-buffer " *dual-selector-source*"))
          (ende-providers '(openai-codex))
          (ende-builtin-provider-settings '((openai-codex :models ("model"))))
          (ende-model-selection--last-used nil)
          (ende-model-selection--buffer-selections (make-hash-table :test #'equal))
          (ende-model-selection--provider-selections (make-hash-table :test #'eq))
          (ende-model-selection--buffer-provider-selections (make-hash-table :test #'equal))
          (ende-provider--catalog-cache (make-hash-table :test #'equal))
          registered callback (reports 0))
      (unwind-protect
          (cl-letf (((symbol-function 'ende-codex-auth-lookup)
                     (lambda (method account)
                       (and (eq method 'chatgpt-plan-oauth)
                            (eq account 'personal) registered)))
                    ((symbol-function 'ende-codex--begin-login)
                     (lambda (_method _account finished) (setq callback finished)))
                    ((symbol-function 'ende-codex--login-report)
                     (lambda (&rest _) (cl-incf reports))))
            (switch-to-buffer source)
            (ende-selector-open)
            (ende-selector-configure-connection)
            (ende-selector-connection--set :auth-method 'chatgpt-plan-oauth)
            (ende-selector-connection-apply)
            (let* ((selector (current-buffer))
                   (connection (copy-tree ende-selector--connection))
                   (selector-tick (buffer-chars-modified-tick)))
              (ende-selector-configure-connection)
              (ende-selector-connection--set :account 'personal)
              (should (string-match-p "OAuth: +not connected\n" (buffer-string)))
              (ende-selector-connection-login)
              (setq registered '(:auth-method chatgpt-plan-oauth :account personal))
              (funcall callback registered nil)
              (should (string-match-p "OAuth: +connected\n" (buffer-string)))
              (dolist (change '((:account . other) (:auth-method . codex-oauth)))
                (ende-selector-connection--set :account 'personal)
                (ende-selector-connection--set (car change) (cdr change))
                (let ((tick (buffer-chars-modified-tick)))
                  (funcall callback registered nil)
                  (should (= tick (buffer-chars-modified-tick)))))
              (ende-selector-connection-cancel)
              (funcall callback registered nil)
              (with-current-buffer selector
                (should (equal connection ende-selector--connection))
                (should (= selector-tick (buffer-chars-modified-tick))))
              (should (= reports 4))))
        (when-let* ((selector (get-buffer ende-selector-buffer-name)))
          (with-current-buffer selector (ende-selector-close)))
        (kill-buffer source)))))

(ert-deftest ende-codex-dual-native-login-saves-verified-account ()
  (ende-codex-auth-test--with-store
    (let* ((ende-codex--pending-login nil)
           (claims (ende-codex-auth--json
                    (format "{\"iss\":\"https://auth.openai.com\",\"aud\":\"%s\",\"sub\":\"verified\",\"iat\":%d,\"exp\":%d,\"https://api.openai.com/auth\":{\"chatgpt_account_id\":\"workspace\"}}"
                            ende-codex-native-client-id (- (floor (float-time)) 10) (+ (floor (float-time)) 3600))))
           (payload (ende-codex-auth--json
                     "{\"access_token\":\"native-access\",\"refresh_token\":\"native-refresh\",\"id_token\":\"signed-token\",\"token_type\":\"Bearer\",\"expires_in\":1234}"))
           result failure requests listener)
      (cl-letf (((symbol-function 'browse-url) #'ignore)
                ((symbol-function 'ende-codex-auth--signed-claims) (lambda (&rest _) claims))
                ((symbol-function 'ende-codex-auth--jwks-async)
                 (lambda (callback) (funcall callback 'trusted-keys nil) #'ignore))
                ((symbol-function 'ende-codex-auth--request)
                 (lambda (url form _limit callback)
                   (push (list url form) requests)
                   (funcall callback payload nil) #'ignore)))
        (unwind-protect
            (progn
              (ende-codex--begin-login 'codex-oauth 'personal
                                       (lambda (record error) (setq result record failure error)))
              (let ((attempt ende-codex--pending-login))
                (setq listener (ende-codex--login-listener attempt))
                (ende-codex--login-receive-callback
                 attempt (list (cons "state" (ende-codex--login-state attempt)) '("code" . "authorization-code")))
                (should-error (ende-codex--login-receive-callback attempt '(("code" . "authorization-code")))))
              (should-not failure)
              (should-not (process-live-p listener))
              (should (eq (plist-get result :auth-method) 'codex-oauth))
              (should (equal (plist-get result :chatgpt-account-id) "workspace"))
              (should (equal (plist-get result :client-id) ende-codex-native-client-id))
              (should (<= (abs (- (plist-get result :expires-at) (+ (floor (float-time)) 1234))) 1))
              (should (equal result (ende-codex-auth-read 'codex-oauth 'personal)))
              (should-not (ende-codex-auth-lookup 'chatgpt-plan-oauth 'personal))
              (should (= 1 (length requests)))
              (should (equal (caar requests) "https://auth.openai.com/oauth/token"))
              (should-not (string-match-p "resource=" (cadar requests))))
          (ende-codex-cancel-login))))))

(ert-deftest ende-codex-dual-refresh-http-boundary ()
  (let ((ende-codex--refresh-flights (make-hash-table :test #'equal))
        (ende-codex--refresh-processes nil)
        (real-make-process (symbol-function 'make-process))
        command sent result cancel)
    (dolist (method '(chatgpt-plan-oauth codex-oauth))
      (let ((auth (append (list :auth-method method :account 'personal)
                          (ende-codex-dual-test--credentials method))))
        (unwind-protect
            (cl-letf (((symbol-function 'ende-codex--refresh-flight-begin) #'ende-codex--start-refresh-flight)
                      ((symbol-function 'make-process)
                       (lambda (&rest args)
                         (setq command (plist-get args :command))
                         (funcall real-make-process :name "ende-refresh-mock" :command '("cat")
                                  :noquery t :connection-type 'pipe :filter #'ignore :sentinel #'ignore)))
                      ((symbol-function 'process-send-string) (lambda (_process body) (setq sent body)))
                      ((symbol-function 'process-send-eof) #'ignore)
                      ((symbol-function 'ende-codex-auth--rotate)
                       (lambda (expected credentials) (append (list :auth-method (plist-get expected :auth-method)) credentials))))
              (setq result nil cancel (ende-codex--refresh-auth auth (lambda (value) (setq result value))))
              (should (member (ende-codex--auth-property method :token-url) command))
              (should (equal sent (ende-codex--refresh-body method auth)))
              (let* ((flight (gethash (ende-codex--refresh-key auth) ende-codex--refresh-flights))
                     (process (ende-codex--refresh-flight-process flight)))
                (with-temp-file (ende-codex--refresh-flight-headers-file flight)
                  (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n"))
                (ende-codex--refresh-filter
                 flight process "{\"access_token\":\"new-access\",\"token_type\":\"Bearer\",\"expires_in\":1800}")
                (cl-letf (((symbol-function 'process-status) (lambda (_) 'exit))
                          ((symbol-function 'process-exit-status) (lambda (_) 0)))
                  (ende-codex--refresh-sentinel flight process "finished")))
              (should (equal (plist-get result :access-token) "new-access"))
              (should (equal (plist-get result :refresh-token) (plist-get auth :refresh-token))))
          (when cancel (funcall cancel)))))))

(ert-deftest ende-codex-dual-profile-and-header-injection-rejected ()
  (let ((ende-builtin-provider-settings nil))
    (dolist (method '(chatgpt-plan-oauth codex-oauth))
      (let* ((profile (ende-provider--compose-profile 'openai-codex "model" nil (list :auth-method method)))
             (other (if (eq method 'codex-oauth) 'chatgpt-plan-oauth 'codex-oauth)))
        (should (ende-provider-validate-profile profile))
        (dolist (pair '((:url . :responses-url) (:models-url . :models-url)))
          (should-error
           (ende-provider-validate-profile
            (plist-put (copy-tree profile) (car pair) (ende-codex--auth-property other (cdr pair))))))
        (should-error (ende-provider-validate-profile (plist-put (copy-tree profile) :base-url "https://example.org"))))))
  (dolist (header '("ChatGPT-Account-Id: attacker" "originator: opencode"))
    (should-error (ende-provider-config--headers (list header)))))

(provide 'ende-codex-dual-test)
;;; ende-codex-dual-test.el ends here
