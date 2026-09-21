;;; deslop-codex.el --- ChatGPT/Codex transport for deslop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools, llm

;;; Commentary:

;; Reuse the OAuth credentials maintained by OpenCode for ChatGPT Plus/Pro.
;; This deliberately does not implement a second login flow: users authenticate
;; OpenCode once, then deslop reads and refreshes its openai OAuth record.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url-util)
(require 'deslop-core)
(require 'deslop-curl)
(require 'deslop-protocol)
(require 'deslop-transport)

(defcustom deslop-codex-auth-file
  (expand-file-name "~/.local/share/opencode/auth.json")
  "OpenCode credential file containing the `openai' OAuth record."
  :type 'file
  :group 'deslop)

(defcustom deslop-codex-issuer "https://auth.openai.com"
  "OAuth issuer used to refresh OpenCode's ChatGPT credentials."
  :type 'string
  :group 'deslop)

(defcustom deslop-codex-client-id "app_EMoamEEZ73f0CkXaXp7hrann"
  "OpenAI OAuth client id used by OpenCode's ChatGPT integration."
  :type 'string
  :group 'deslop)

(defcustom deslop-codex-model "gpt-5.6-luna"
  "Default model used by the ChatGPT/Codex profile."
  :type 'string
  :group 'deslop)

(defcustom deslop-codex-endpoint "https://chatgpt.com/backend-api/codex/responses"
  "ChatGPT/Codex Responses endpoint."
  :type 'string
  :group 'deslop)

(defcustom deslop-codex-models-endpoint "https://chatgpt.com/backend-api/codex/models"
  "Account-scoped ChatGPT/Codex model discovery endpoint."
  :type 'string
  :group 'deslop)

(defcustom deslop-codex-client-version "1.0.0"
  "Client version sent to the ChatGPT/Codex model discovery endpoint."
  :type 'string
  :group 'deslop)

(defconst deslop-codex--generation-host "chatgpt.com"
  "Official host allowed for Codex generation and model discovery.")

(defconst deslop-codex--issuer-host "auth.openai.com"
  "Official host allowed for Codex OAuth refreshes.")

(defconst deslop-codex--generation-path "/backend-api/codex/responses"
  "Official Codex generation path.")

(defconst deslop-codex--models-path "/backend-api/codex/models"
  "Official Codex model discovery path.")

(defconst deslop-codex--refresh-path "/oauth/token"
  "Official Codex OAuth refresh path.")

(defvar deslop-codex--refresh-processes nil
  "In-flight OpenAI OAuth refresh processes.")

(cl-defstruct (deslop-codex--refresh-waiter
               (:constructor deslop-codex--make-refresh-waiter))
  "One callback waiting for a shared OAuth refresh flight."
  callback cancelled)

(cl-defstruct (deslop-codex--refresh-flight
               (:constructor deslop-codex--make-refresh-flight))
  "One coalesced OAuth refresh and its waiting callbacks."
  key auth waiters process response-buffer stderr-buffer stderr-process
  headers-file
  received-bytes finished)

(defvar deslop-codex--refresh-flights (make-hash-table :test #'equal)
  "In-flight OAuth refreshes keyed by credential source and client identity.")

(defvar deslop-codex--model-probe-processes nil
  "In-flight ChatGPT/Codex model discovery processes.")

(cl-defstruct (deslop-codex--session
               (:constructor deslop-codex--make-session))
  profile callbacks lifecycle process process-buffer stderr-buffer stderr-process
  headers-file
  config-file auth-cancel sse content-parts thinking-parts usage actual-model
   actual-provider finish-reason estimated-chars received-bytes preview-parts
   preview-bytes generation-start terminal terminal-event cancelled)

(defun deslop-codex--lifecycle (session)
  "Return the shared lifecycle object for SESSION, creating it if needed."
  (or (deslop-codex--session-lifecycle session)
      (let ((lifecycle
             (deslop-transport-session-create
              (deslop-codex--session-callbacks session)
              (apply-partially #'deslop-codex--fail session))))
        (deslop-transport-session-set-resources
         lifecycle
         (list :process (deslop-codex--session-process session)
               :response-buffer (deslop-codex--session-process-buffer session)
               :stderr-buffer (deslop-codex--session-stderr-buffer session)
               :stderr-process (deslop-codex--session-stderr-process session)
               :headers-file (deslop-codex--session-headers-file session)
               :config-file (deslop-codex--session-config-file session)))
        (setf (deslop-codex--session-lifecycle session) lifecycle))))

;;;; Credential handling

(defun deslop-codex--auth-version (auth)
  "Return the credential version represented by AUTH."
  (list (plist-get auth :access)
        (plist-get auth :refresh)
        (plist-get auth :expires)
        (plist-get auth :account-id)))

(defun deslop-codex--entry-version (entry)
  "Return the credential version represented by JSON ENTRY."
  (list (deslop-protocol-json-get "access" entry)
        (deslop-protocol-json-get "refresh" entry)
        (deslop-protocol-json-get "expires" entry)
        (deslop-protocol-json-get "accountId" entry)))

(defun deslop-codex--validated-official-url (url host path)
  "Return URL when it has the exact official HTTPS origin and PATH."
  (let* ((parsed (and (stringp url) (url-generic-parse-url url)))
         (scheme (and parsed (url-type parsed)))
         (actual-host (and parsed (url-host parsed)))
         (port (and parsed (url-port parsed)))
         (actual-path (and parsed (url-filename parsed))))
    (unless (and (stringp url)
                 (string= (downcase (or scheme "")) "https")
                 (string= (downcase (or actual-host "")) host)
                 (= (or port 443) 443)
                 (null (and parsed (url-user parsed)))
                 (null (and parsed (url-password parsed)))
                 (not (string-match-p "[?#]" url))
                 (string= (or actual-path "/") path))
      (error "Codex credentials may only be sent to the official %s endpoint"
             host))
    url))

(defun deslop-codex--validated-generation-url (url)
  "Validate the credential-bearing Codex generation URL URL."
  (deslop-codex--validated-official-url
   url deslop-codex--generation-host deslop-codex--generation-path))

(defun deslop-codex--validated-models-url ()
  "Validate and return the credential-bearing Codex models URL."
  (deslop-codex--validated-official-url
   deslop-codex-models-endpoint
   deslop-codex--generation-host
   deslop-codex--models-path))

(defun deslop-codex--validated-refresh-url ()
  "Validate and return the official Codex OAuth refresh URL."
  (deslop-codex--validated-official-url
   (concat (string-remove-suffix "/" deslop-codex-issuer)
           deslop-codex--refresh-path)
   deslop-codex--issuer-host
   deslop-codex--refresh-path))

(defun deslop-codex--auth-fresh-p (auth)
  "Return non-nil when AUTH has enough lifetime for a new request."
  (and (stringp (plist-get auth :access))
       (numberp (plist-get auth :expires))
       (> (plist-get auth :expires)
          (+ (* 1000 (floor (float-time))) 30000))))

(defun deslop-codex--read-json-file (file)
  "Read FILE as a JSON hash table without interning its keys."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (deslop-protocol-json-read-string (buffer-string))))

(defun deslop-codex--auth-entry ()
  "Return OpenCode's OAuth entry for the `openai' provider."
  (let* ((default-directory (deslop-curl-local-directory))
         (data (deslop-codex--read-json-file deslop-codex-auth-file))
         (entry (deslop-protocol-json-get "openai" data)))
    (unless (and (hash-table-p entry)
                 (string= (format "%s"
                                  (deslop-protocol-json-get "type" entry))
                          "oauth"))
      (error "OpenCode has no ChatGPT OAuth credentials for openai"))
    (let ((access (deslop-protocol-json-get "access" entry))
          (refresh (deslop-protocol-json-get "refresh" entry))
          (expires (deslop-protocol-json-get "expires" entry))
          (account-id (deslop-protocol-json-get "accountId" entry)))
      (unless (and (stringp access) (stringp refresh) (numberp expires))
        (error "OpenCode's openai OAuth credentials are incomplete"))
      (list :access access :refresh refresh :expires expires
            :account-id account-id))))

(defun deslop-codex--jwt-claims (token)
  "Decode the unverified claims from TOKEN, or return nil.
The claims are used only for routing metadata such as account and residency."
  (when (stringp token)
    (let ((parts (split-string token "\.")))
      (when (= (length parts) 3)
        (condition-case nil
            (let* ((encoded (replace-regexp-in-string
                             "-" "+"
                             (replace-regexp-in-string "_" "/"
                                                        (nth 1 parts))))
                   (padding (mod (- 4 (mod (length encoded) 4)) 4))
                   (decoded (base64-decode-string
                             (concat encoded (make-string padding ?=)))))
              (deslop-protocol-json-read-string
               (decode-coding-string decoded 'utf-8)))
          ((error quit) nil))))))

(defun deslop-codex--claim (claims key)
  "Return claim KEY from CLAIMS, including namespaced auth claims."
  (or (deslop-protocol-json-get key claims)
      (deslop-protocol-json-get key
                                 (deslop-protocol-json-get
                                  "https://api.openai.com/auth" claims))))

(defun deslop-codex--account-id (auth)
  "Return the ChatGPT account id in AUTH or its access token."
  (or (plist-get auth :account-id)
      (let ((claims (deslop-codex--jwt-claims (plist-get auth :access))))
        (or (deslop-codex--claim claims "chatgpt_account_id")
            (let ((organizations
                   (deslop-protocol-json-get "organizations" claims)))
              (and (vectorp organizations)
                   (> (length organizations) 0)
                    (deslop-protocol-json-get
                    "id" (aref organizations 0))))))))

(defun deslop-codex--residency (auth)
  "Return the compute residency claim in AUTH, if constrained."
  (let ((residency
         (deslop-codex--claim
          (deslop-codex--jwt-claims (plist-get auth :access))
          "chatgpt_compute_residency")))
    (and (stringp residency)
         (not (string-empty-p residency))
         (not (string= residency "no_constraint"))
         residency)))

(defun deslop-codex--form-field (value)
  "Encode VALUE for an OAuth form body."
  (url-hexify-string (format "%s" value)))

(defun deslop-codex--refresh-body (refresh-token)
  "Return an OAuth refresh body for REFRESH-TOKEN."
  (format "grant_type=refresh_token&refresh_token=%s&client_id=%s"
          (deslop-codex--form-field refresh-token)
          (deslop-codex--form-field deslop-codex-client-id)))

(defun deslop-codex--store-auth (auth &optional expected-version)
  "Write refreshed AUTH back to OpenCode's credential file atomically.
When EXPECTED-VERSION is non-nil, refuse to replace credentials that changed
since the refresh began."
  (let* ((default-directory (deslop-curl-local-directory))
         (file (expand-file-name deslop-codex-auth-file))
         (data (progn
                 (when (file-remote-p file)
                   (user-error "Deslop requires a local Codex credential file"))
                 (deslop-codex--read-json-file file)))
          (old (deslop-protocol-json-get "openai" data))
         (entry (if (hash-table-p old)
                    (copy-hash-table old)
          (make-hash-table :test #'equal)))
          (temporary nil)
          (complete nil))
    (when (and expected-version
               (not (equal expected-version
                           (deslop-codex--entry-version old))))
      (error "OpenCode credentials changed during refresh"))
    (puthash "type" "oauth" entry)
    (puthash "access" (plist-get auth :access) entry)
    (puthash "refresh" (plist-get auth :refresh) entry)
    (puthash "expires" (plist-get auth :expires) entry)
    (when (plist-get auth :account-id)
      (puthash "accountId" (plist-get auth :account-id) entry))
    (puthash "openai" entry data)
    (unwind-protect
        (progn
           (setq temporary
                 (make-temp-file
                  (expand-file-name ".deslop-openai-auth-"
                                    (or (file-name-directory file)
                                        default-directory))))
          (set-file-modes temporary #o600)
          (with-temp-file temporary
            (insert (json-encode data) "\n"))
          (set-file-modes temporary #o600)
          (rename-file temporary file t)
          (setq complete t))
      (unless complete
        (when (and temporary (file-exists-p temporary))
          (deslop-ignore-errors #'delete-file temporary))))))

(defun deslop-codex--refresh-command (url headers-file)
  "Build the curl command for an OAuth refresh request to URL."
  (append
   (deslop-curl-base-command url)
   (deslop-curl-response-options headers-file)
    (list "--request" "POST"
          "--header" "Content-Type: application/x-www-form-urlencoded"
          "--data-binary" "@-")
    (deslop-curl-timeout-options deslop-request-timeout
                                  deslop-max-oauth-refresh-bytes)
    (list "--url" url)))

(defun deslop-codex--refresh-finish (flight result &optional cancelled)
  "Finish FLIGHT and notify its remaining waiters with RESULT."
  (unless (deslop-codex--refresh-flight-finished flight)
    (setf (deslop-codex--refresh-flight-finished flight) t)
    (when (eq (gethash (deslop-codex--refresh-flight-key flight)
                       deslop-codex--refresh-flights)
              flight)
      (remhash (deslop-codex--refresh-flight-key flight)
               deslop-codex--refresh-flights))
    (let ((process (deslop-codex--refresh-flight-process flight))
          (waiters (deslop-codex--refresh-flight-waiters flight)))
      (setf (deslop-codex--refresh-flight-waiters flight) nil
            deslop-codex--refresh-processes
            (delq process deslop-codex--refresh-processes))
      (when (and (processp process) (process-live-p process))
        (deslop-ignore-errors #'delete-process process))
      (deslop-curl-cleanup-resources
       (list (deslop-codex--refresh-flight-response-buffer flight)
             (deslop-codex--refresh-flight-stderr-buffer flight))
        (list (deslop-codex--refresh-flight-headers-file flight))
        (list (deslop-codex--refresh-flight-stderr-process flight)))
      (unless cancelled
        (dolist (waiter waiters)
          (unless (deslop-codex--refresh-waiter-cancelled waiter)
            (condition-case nil
                (funcall (deslop-codex--refresh-waiter-callback waiter)
                         result)
              ((error quit) nil))))))))

(defun deslop-codex--refresh-cancel-waiter (flight waiter)
  "Cancel WAITER from FLIGHT without disturbing other waiters."
  (unless (deslop-codex--refresh-waiter-cancelled waiter)
    (setf (deslop-codex--refresh-waiter-cancelled waiter) t)
    (when (and (not (deslop-codex--refresh-flight-finished flight))
               (eq (gethash (deslop-codex--refresh-flight-key flight)
                            deslop-codex--refresh-flights)
                   flight))
      (setf (deslop-codex--refresh-flight-waiters flight)
            (delq waiter (deslop-codex--refresh-flight-waiters flight)))
      (when (null (deslop-codex--refresh-flight-waiters flight))
        (deslop-codex--refresh-finish flight nil t)))))

(defun deslop-codex--refresh-filter (flight _process chunk)
  "Consume refresh response CHUNK for FLIGHT within its byte budget."
  (unless (deslop-codex--refresh-flight-finished flight)
    (let* ((received (deslop-codex--refresh-flight-received-bytes flight))
           (total (+ received (string-bytes chunk))))
      (setf (deslop-codex--refresh-flight-received-bytes flight) total)
      (if (and deslop-max-oauth-refresh-bytes
               (> total deslop-max-oauth-refresh-bytes))
          (deslop-codex--refresh-finish
           flight
           (deslop-error-create
            :kind 'resource
            :message
            "OAuth refresh response exceeds deslop-max-oauth-refresh-bytes"))
        (let ((buffer (deslop-codex--refresh-flight-response-buffer flight)))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert chunk))))))))

(defun deslop-codex--refresh-sentinel (flight process event)
  "Handle terminal PROCESS EVENT for refresh FLIGHT."
  (when (and (not (deslop-codex--refresh-flight-finished flight))
             (memq (process-status process) '(exit signal closed)))
    (if (/= (process-exit-status process) 0)
        (deslop-codex--refresh-finish
         flight
         (or (process-get process :deslop-idle-error)
             (deslop-error-create
              :kind 'auth
              :message
              (format "OpenAI token refresh failed: %s" (string-trim event)))))
      (condition-case parse-error
          (let* ((headers
                  (deslop-curl-response-headers
                   (deslop-codex--refresh-flight-headers-file flight)))
                 (status (car headers))
                 (buffer (deslop-codex--refresh-flight-response-buffer flight))
                 (payload
                  (with-current-buffer buffer
                   (deslop-protocol-json-read-string (buffer-string))))
                  (access (deslop-protocol-json-get "access_token" payload))
                  (refresh (deslop-protocol-json-get "refresh_token" payload))
                  (expires-in (deslop-protocol-json-get "expires_in" payload)))
            (if (or (null status) (< status 200) (>= status 300))
                (deslop-codex--refresh-finish
                 flight
                 (deslop-error-create
                  :kind 'auth
                  :message
                  (format "OpenAI token refresh returned HTTP %s"
                          (or status "unknown"))))
              (unless (and (stringp access) (stringp refresh)
                           (numberp expires-in))
                (error "token refresh response is incomplete"))
              (let ((refreshed
                     (list :access access
                           :refresh refresh
                           :expires (+ (* 1000 (floor (float-time)))
                                       (* 1000 expires-in))
                           :account-id
                           (or (plist-get (deslop-codex--refresh-flight-auth flight)
                                          :account-id)
                               (deslop-codex--claim
                                (deslop-codex--jwt-claims access)
                                "chatgpt_account_id")))))
                (condition-case store-error
                    (progn
                      (deslop-codex--store-auth
                       refreshed
                       (deslop-codex--auth-version
                        (deslop-codex--refresh-flight-auth flight)))
                      (deslop-codex--refresh-finish flight refreshed))
                  ((error quit)
                   (let ((current
                          (condition-case nil
                              (deslop-codex--auth-entry)
                            ((error quit) nil))))
                     (if (and (string-match-p
                               "credentials changed during refresh"
                               (error-message-string store-error))
                              (deslop-codex--auth-fresh-p current))
                         (deslop-codex--refresh-finish flight current)
                       (deslop-codex--refresh-finish
                        flight
                        (deslop-error-create
                         :kind 'auth
                         :message (error-message-string store-error))))))))))
        ((error quit)
         (deslop-codex--refresh-finish
          flight
          (deslop-error-create
            :kind 'auth
            :message (error-message-string parse-error)))))))
  )

(defun deslop-codex--start-refresh-flight (flight)
  "Start the process belonging to refresh FLIGHT."
  (let ((response-buffer nil)
        (stderr-buffer nil)
        (stderr-process nil)
        (headers-file nil)
        (process nil))
    (condition-case startup-error
        (let ((default-directory (deslop-curl-local-directory)))
          (deslop-validate-resource-options)
          (setq response-buffer (generate-new-buffer " *deslop-openai-refresh*"))
          (setf (deslop-codex--refresh-flight-response-buffer flight)
                response-buffer)
          (setq stderr-buffer
                (generate-new-buffer " *deslop-openai-refresh-stderr*"))
          (setf (deslop-codex--refresh-flight-stderr-buffer flight) stderr-buffer)
          (setq headers-file (make-temp-file "deslop-openai-headers-"))
          (setf (deslop-codex--refresh-flight-headers-file flight) headers-file)
          (set-file-modes headers-file #o600)
          (setq stderr-process
                (make-pipe-process
                 :name (generate-new-buffer-name
                        "deslop-openai-refresh-stderr")
                 :filter (apply-partially #'deslop-curl-stderr-filter
                                          stderr-buffer)
                 :noquery t))
          (setf (deslop-codex--refresh-flight-stderr-process flight)
                stderr-process)
          (let ((url (deslop-codex--validated-refresh-url)))
            (setq process
                  (make-process
                   :name "deslop-openai-refresh"
                   :buffer response-buffer
                   :stderr stderr-process
                   :connection-type 'pipe
                   :noquery t
                   :command (deslop-codex--refresh-command url headers-file)
                   :filter (apply-partially
                            #'deslop-codex--refresh-filter flight)
                   :sentinel (apply-partially
                              #'deslop-codex--refresh-sentinel flight)))
            (setf (deslop-codex--refresh-flight-process flight) process)
            (deslop-curl-watch-idle process)
            (push process deslop-codex--refresh-processes)
            (process-send-string
             process
             (deslop-codex--refresh-body
              (plist-get (deslop-codex--refresh-flight-auth flight) :refresh)))
            (process-send-eof process)))
      ((error quit)
       (deslop-codex--refresh-finish
        flight
        (deslop-error-create
         :kind 'auth
         :message (error-message-string startup-error)))))))

(defun deslop-codex--refresh-auth (auth callback)
  "Coalesce a refresh for AUTH and register CALLBACK as a waiter.
Return a cancellation function that only removes this callback."
  (let* ((default-directory (deslop-curl-local-directory))
         (key (list (expand-file-name deslop-codex-auth-file)
                    deslop-codex-issuer
                    deslop-codex-client-id))
         (flight (gethash key deslop-codex--refresh-flights))
         (new-flight (null flight))
         (waiter (deslop-codex--make-refresh-waiter :callback callback)))
    (when new-flight
      (setq flight
            (deslop-codex--make-refresh-flight
             :key key :auth auth :waiters nil :received-bytes 0))
      (puthash key flight deslop-codex--refresh-flights))
    (setf (deslop-codex--refresh-flight-waiters flight)
          (cons waiter (deslop-codex--refresh-flight-waiters flight)))
    (when new-flight
      (deslop-codex--start-refresh-flight flight))
    (apply-partially #'deslop-codex--refresh-cancel-waiter flight waiter)))

(defun deslop-codex--resolve-auth (callback)
  "Load OpenCode's credentials and call CALLBACK with AUTH or an error.
Return a cancellation function for an asynchronous refresh, or nil."
  (condition-case load-error
      (let ((auth (deslop-codex--auth-entry)))
        (if (deslop-codex--auth-fresh-p auth)
            (progn (funcall callback auth) nil)
          (deslop-codex--refresh-auth auth callback)))
    ((error quit)
     (funcall callback
              (deslop-error-create
               :kind 'auth
               :message (error-message-string load-error)))
      nil)))

;;;; Model discovery

(defun deslop-codex--models-command (url headers-file config-file)
  "Build a curl GET command for the account-scoped model list at URL."
  (append
   (deslop-curl-base-command url)
   (deslop-curl-response-options headers-file)
   (list "--get"
         "--data-urlencode"
         (concat "client_version=" deslop-codex-client-version))
    (when config-file (list "--config" config-file))
    (deslop-curl-timeout-options deslop-request-timeout
                                  deslop-provider-max-discovery-bytes)
    (list "--url" url)))

(defun deslop-codex--parse-models (text)
  "Return model slugs parsed from a Codex model-list response TEXT."
   (let* ((payload (deslop-protocol-json-read-string text))
          (models (deslop-protocol-json-get "models" payload)))
    (unless (vectorp models)
      (error "Codex model response has no models array"))
    (cl-loop for model across models
              for slug = (deslop-protocol-json-get "slug" model)
             when (and (stringp slug) (not (string-empty-p slug)))
             collect slug)))

(defun deslop-codex--response-detail (preview)
  "Return a useful error detail parsed from response PREVIEW."
  (condition-case nil
       (let* ((payload (deslop-protocol-json-read-string preview))
              (error-payload (deslop-protocol-json-get "error" payload)))
         (or (deslop-protocol-json-get "message" error-payload)
             (deslop-protocol-json-get "detail" error-payload)
             (deslop-protocol-json-get "message" payload)
             (deslop-protocol-json-get "detail" payload)
            (and (stringp error-payload) error-payload)))
    (error nil)))

(defun deslop-codex-probe-models (callback)
  "Probe the authenticated Codex endpoint and call CALLBACK with model slugs.
CALLBACK receives (MODELS ERROR).  Return a cancellation function."
  (let ((response-buffer nil)
        (stderr-buffer nil)
        (stderr-process nil)
        (headers-file nil)
         (config-file nil)
          (process nil)
          (auth-cancel nil)
          (models-url nil)
          (received-bytes 0)
          (finished nil))
    (cl-labels
        ((finish (models &optional error cancelled)
           (unless finished
             (setq finished t)
             (setq deslop-codex--model-probe-processes
                   (delq process deslop-codex--model-probe-processes))
             (when (and (processp process) (process-live-p process))
               (delete-process process))
              (deslop-curl-cleanup-resources
               (list response-buffer stderr-buffer)
               (list headers-file config-file)
               (list stderr-process))
             (unless cancelled
               (funcall callback models error)))))
       (condition-case startup-error
           (progn
             (deslop-validate-resource-options)
             (setq models-url (deslop-codex--validated-models-url))
             (setq auth-cancel
                   (deslop-codex--resolve-auth
                    (lambda (auth-or-error)
                      (unless finished
                        (if (deslop-error-p auth-or-error)
                            (finish nil auth-or-error)
                          (condition-case request-error
                              (let* ((account-id
                                      (deslop-codex--account-id auth-or-error))
                                   (headers
                                    (append
                                    (list
                                     (concat "Authorization: Bearer "
                                             (plist-get auth-or-error :access))
                                     "originator: opencode")
                                    (when account-id
                                      (list (concat "ChatGPT-Account-Id: "
                                                    account-id)))))
                                  (request
                                     (deslop-curl-start-request
                                     :profile (list :url models-url)
                                     :name "deslop-codex-model-probe"
                                    :internal-headers headers
                                    :command
                                    (lambda (url-headers url-config)
                                       (deslop-codex--models-command
                                        models-url
                                        url-headers url-config))
                                      :filter
                                      (lambda (_probe-process chunk)
                                        (unless finished
                                          (setq received-bytes
                                                (+ received-bytes
                                                   (string-bytes chunk)))
                                          (if (and deslop-provider-max-discovery-bytes
                                                   (> received-bytes
                                                      deslop-provider-max-discovery-bytes))
                                              (finish
                                               nil
                                               (deslop-error-create
                                                :kind 'resource
                                                :message
                                                "Codex model response exceeds deslop-provider-max-discovery-bytes"))
                                            (when (buffer-live-p response-buffer)
                                              (with-current-buffer response-buffer
                                                (goto-char (point-max))
                                                (insert chunk))))))
                                    :sentinel
                                    (lambda (probe-process event)
                                      (when (memq (process-status probe-process)
                                                  '(exit signal closed))
                                        (if (/= (process-exit-status probe-process) 0)
                                            (finish
                                             nil
                                             (or (process-get
                                                  probe-process :deslop-idle-error)
                                                 (deslop-error-create
                                                  :kind 'transport
                                                  :message
                                                  (format "Codex model probe failed: %s"
                                                          (string-trim event)))))
                                          (let* ((status
                                                  (car (deslop-curl-response-headers
                                                        headers-file)))
                                                 (text
                                                  (with-current-buffer response-buffer
                                                    (buffer-string)))
                                                 (detail
                                                  (deslop-codex--response-detail
                                                   text)))
                                            (if (or (null status) (< status 200)
                                                    (>= status 300))
                                                (finish
                                                 nil
                                                 (deslop-error-create
                                                  :kind 'http
                                                  :message
                                                  (format "HTTP status %s: %s"
                                                          (or status "unknown")
                                                          (or detail
                                                              "Codex model probe failed"))))
                                              (condition-case parse-error
                                                  (finish (deslop-codex--parse-models text))
                                                ((error quit)
                                                 (finish
                                                  nil
                                                  (deslop-error-create
                                                   :kind 'protocol
                                                   :message
                                                   (error-message-string
                                                    parse-error)))))))))))))
                                 (setq process (plist-get request :process))
                                 (setq response-buffer
                                       (plist-get request :response-buffer)
                                       stderr-buffer
                                       (plist-get request :stderr-buffer)
                                       stderr-process
                                       (plist-get request :stderr-process)
                                       headers-file
                                       (plist-get request :headers-file)
                                       config-file
                                       (plist-get request :config-file))
                                 (push process deslop-codex--model-probe-processes))
                             ((error quit)
                              (finish
                               nil
                               (deslop-error-create
                                :kind 'startup
                                :message (error-message-string request-error))))))))))
            )
         ((error quit)
         (finish
          nil
          (deslop-error-create
           :kind 'startup
           :message (error-message-string startup-error))))
      (lambda ()
       (when auth-cancel (funcall auth-cancel))
         (finish nil nil t))))))

;;;; Responses transport

(defun deslop-codex--invoke-callback (session name &rest arguments)
  "Invoke callback NAME for SESSION safely."
  (apply #'deslop-transport-session-invoke
         (deslop-codex--lifecycle session) name arguments))

(defun deslop-codex--cleanup (session)
  "Release resources owned by SESSION."
  (when (deslop-codex--session-lifecycle session)
    (deslop-transport-session-cleanup
     (deslop-codex--session-lifecycle session)))
  (setf (deslop-codex--session-process session) nil
        (deslop-codex--session-process-buffer session) nil
        (deslop-codex--session-stderr-buffer session) nil
        (deslop-codex--session-stderr-process session) nil
        (deslop-codex--session-headers-file session) nil
        (deslop-codex--session-config-file session) nil
        (deslop-codex--session-auth-cancel session) nil
        (deslop-codex--session-lifecycle session) nil))

(defun deslop-codex--result (session)
  "Build a normalized result plist for SESSION."
  (let* ((usage (deslop-codex--session-usage session))
         (reported (and usage
                         (deslop-protocol-json-get "output_tokens" usage)))
         (tokens (or reported
                     (max 0 (floor
                             (/ (deslop-codex--session-estimated-chars session)
                                4.0)))))
         (started (or (deslop-codex--session-generation-start session)
                      (float-time))))
    (list :content (mapconcat #'identity
                              (reverse
                               (deslop-codex--session-content-parts session)) "")
          :thinking (mapconcat #'identity
                               (reverse
                                (deslop-codex--session-thinking-parts session)) "")
          :usage usage :tokens tokens :estimated (null reported)
          :speed (/ (float tokens)
                    (max 0.001 (- (float-time) started)))
          :finish-reason (deslop-codex--session-finish-reason session)
          :provider (or (deslop-codex--session-actual-provider session)
                        "OpenAI")
          :model (or (deslop-codex--session-actual-model session)
                     (plist-get (deslop-codex--session-profile session)
                                :model)))))

(defun deslop-codex--settle (session outcome value)
  "Settle SESSION once with OUTCOME and VALUE."
  (unless (deslop-codex--session-terminal session)
    (setf (deslop-codex--session-terminal session) outcome)
    (unwind-protect
        (deslop-transport-session-settle
         (deslop-codex--lifecycle session)
         outcome value
         (when (or (deslop-codex--session-content-parts session)
                   (deslop-codex--session-thinking-parts session)
                   (deslop-codex--session-usage session))
           (deslop-codex--result session)))
      (deslop-codex--cleanup session)
      (setf (deslop-codex--session-callbacks session) nil))))

(defun deslop-codex--fail (session error)
  "Settle SESSION with structured ERROR."
  (deslop-codex--settle session 'failure error))

(defun deslop-codex--cancel (session)
  "Cancel SESSION."
  (setf (deslop-codex--session-cancelled session) t)
  (let ((cancel (deslop-codex--session-auth-cancel session)))
    (setf (deslop-codex--session-auth-cancel session) nil)
    (when cancel (funcall cancel)))
  (deslop-codex--fail
   session (deslop-error-create :kind 'cancelled :message "Process cancelled")))

(defun deslop-codex--emit (session event)
  "Emit EVENT unless SESSION has settled."
  (unless (deslop-codex--session-terminal session)
    (deslop-codex--invoke-callback session :on-event event)))

(defun deslop-codex--emit-delta (session channel text)
  "Record and emit TEXT on CHANNEL."
  (when (and (stringp text) (> (length text) 0)
             (not (deslop-codex--session-terminal session)))
    (if (and deslop-max-output-chars
             (> (+ (deslop-codex--session-estimated-chars session)
                   (length text))
                deslop-max-output-chars))
        (deslop-codex--fail
         session
         (deslop-error-create
          :kind 'resource
          :message "generated output exceeds deslop-max-output-chars"))
      (if (eq channel :content)
          (push text (deslop-codex--session-content-parts session))
        (push text (deslop-codex--session-thinking-parts session)))
      (cl-incf (deslop-codex--session-estimated-chars session) (length text))
      (unless (deslop-codex--session-generation-start session)
        (setf (deslop-codex--session-generation-start session)
              (float-time)))
      (deslop-codex--emit
       session (list :type :delta :channel channel :text text)))))

(defun deslop-codex--handle-usage (session usage)
  "Record and emit Responses API USAGE."
  (when (hash-table-p usage)
    (setf (deslop-codex--session-usage session) usage)
    (let* ((tokens (deslop-protocol-json-get "output_tokens" usage))
           (started (or (deslop-codex--session-generation-start session)
                        (float-time))))
      (deslop-codex--emit
       session (list :type :usage :tokens (if (numberp tokens) tokens 0)
                     :speed (if (numberp tokens)
                                (/ (float tokens)
                                   (max 0.001 (- (float-time) started)))
                              0.0)
                     :usage usage)))))

(defun deslop-codex--handle-event (session json)
  "Handle one parsed Responses API event JSON object."
  (let* ((type (deslop-protocol-json-get "type" json))
         (response (deslop-protocol-json-get "response" json))
         (metadata (or response json))
          (model (deslop-protocol-json-get "model" metadata)))
    (when (stringp model)
      (setf (deslop-codex--session-actual-model session) model))
    (when (or (stringp model)
              (member type '("response.created" "response.completed")))
      (deslop-codex--emit
       session
       (list :type :metadata
             :metadata (list :source :response
                             :model (deslop-codex--session-actual-model session)
                             :provider "OpenAI"))))
    (cond
     ((string= type "response.output_text.delta")
      (deslop-codex--emit
       session '(:type :status :status "Generating"))
      (deslop-codex--emit-delta
        session :content (deslop-protocol-json-get "delta" json)))
     ((string= type "response.reasoning_summary_text.delta")
      (deslop-codex--emit
       session '(:type :status :status "Thinking"))
      (deslop-codex--emit-delta
        session :thinking (deslop-protocol-json-get "delta" json)))
      ((member type '("response.completed" "response.done"))
       (deslop-codex--handle-usage
         session (deslop-protocol-json-get "usage" response))
       (setf (deslop-codex--session-finish-reason session) "stop")
       (setf (deslop-codex--session-terminal-event session) json))
      ((string= type "response.incomplete")
       (deslop-codex--handle-usage
         session (deslop-protocol-json-get "usage" response))
       (setf (deslop-codex--session-finish-reason session) "incomplete")
       (setf (deslop-codex--session-terminal-event session) json))
     ((member type '("response.failed" "error"))
       (let ((error (or (deslop-protocol-json-get "error" json) json)))
        (deslop-codex--fail
         session
         (deslop-error-create
          :kind 'provider
          :message (format "%s"
                           (or (deslop-protocol-json-get "message" error)
                               (deslop-protocol-json-get "code" error)
                              "ChatGPT request failed"))
           :details error)))))))

(defun deslop-codex--append-preview (session chunk)
  "Append a bounded prefix of CHUNK to SESSION's error preview."
  (let* ((used (or (deslop-codex--session-preview-bytes session) 0))
         (remaining (and deslop-max-error-body-bytes
                         (- deslop-max-error-body-bytes used)))
         (prefix (if remaining
                     (and (> remaining 0)
                           (deslop-transport-byte-prefix chunk remaining))
                   chunk)))
    (when prefix
      (push prefix (deslop-codex--session-preview-parts session))
      (setf (deslop-codex--session-preview-bytes session)
            (+ used (string-bytes prefix))))))

(defun deslop-codex--preview-text (session)
  "Return SESSION's bounded error preview."
  (mapconcat #'identity
             (nreverse (deslop-codex--session-preview-parts session)) ""))

(defun deslop-codex--http-error (session)
  "Return a structured HTTP error for SESSION."
  (let* ((status (condition-case nil
                      (car (deslop-curl-response-headers
                            (deslop-codex--session-headers-file session)))
                    (error nil)))
          (preview (deslop-codex--preview-text session))
          (detail (deslop-transport-error-detail preview)))
    (deslop-error-create
     :kind 'http
     :message (format "HTTP status %s: %s"
                       (or status "unknown")
                       (or detail
                           (and (not (string-empty-p (string-trim preview)))
                                (string-trim preview))
                           "ChatGPT request failed")))))

(defun deslop-codex--filter (session _process chunk)
  "Consume curl output CHUNK for SESSION."
  (unless (deslop-codex--session-terminal session)
    (let* ((received (deslop-codex--session-received-bytes session))
           (chunk-bytes (string-bytes chunk))
           (total (+ received chunk-bytes))
           (remaining (and deslop-max-response-bytes
                           (- deslop-max-response-bytes received)))
           (processable (if (and deslop-max-response-bytes
                                  (> total deslop-max-response-bytes))
                             (deslop-transport-byte-prefix chunk (max 0 remaining))
                          chunk)))
      (setf (deslop-codex--session-received-bytes session) total)
      (deslop-codex--append-preview session chunk)
      (condition-case filter-error
          (when (> (length processable) 0)
            (dolist (event
                     (deslop-protocol-sse-feed
                      (deslop-codex--session-sse session) processable))
              (unless (deslop-codex--session-terminal session)
                (when (eq (plist-get event :type) :json)
                  (deslop-codex--handle-event
                   session (plist-get event :value))))))
        ((error quit)
         (deslop-codex--fail
          session
          (deslop-error-create :kind 'protocol
                                :message (error-message-string filter-error)))))
        (when (and deslop-max-response-bytes
                   (> total deslop-max-response-bytes)
                   (not (deslop-codex--session-terminal session)))
          (deslop-codex--fail
           session
           (deslop-error-create
            :kind 'resource
             :message "response exceeds deslop-max-response-bytes"))))))

(defun deslop-codex--sentinel (session process event)
  "Handle terminal PROCESS EVENT for SESSION."
  (when (and (not (deslop-codex--session-terminal session))
             (memq (process-status process) '(exit signal closed)))
     (if (/= (process-exit-status process) 0)
        (deslop-codex--fail
         session
         (or (process-get process :deslop-idle-error)
             (deslop-error-create
              :kind (if (= (process-exit-status process) 28) 'timeout 'transport)
              :message (format "curl exited with status %d (%s)"
                               (process-exit-status process)
                               (string-trim event)))))
       (condition-case finish-error
           (let* ((headers
                   (deslop-curl-response-headers
                    (deslop-codex--session-headers-file session)))
                  (status (car headers))
                  (content-type (cdr headers)))
             (cond
              ((or (null status) (< status 200) (>= status 300))
               (deslop-codex--fail session (deslop-codex--http-error session)))
              ((and content-type
                    (not (deslop-curl-event-stream-p content-type)))
               (deslop-codex--fail
                session
                (deslop-error-create
                 :kind 'protocol
                 :message (format "expected text/event-stream, got %s"
                                  (or content-type "unknown")))))
              (t
               (dolist (event
                        (deslop-protocol-sse-flush
                         (deslop-codex--session-sse session)))
                 (when (and (not (deslop-codex--session-terminal session))
                            (eq (plist-get event :type) :json))
                   (deslop-codex--handle-event
                    session (plist-get event :value))))
               (unless (deslop-codex--session-terminal session)
                 (if (deslop-codex--session-terminal-event session)
                     (deslop-codex--settle
                      session 'success (deslop-codex--result session))
                   (deslop-codex--fail
                    session
                    (deslop-error-create
                     :kind 'protocol
                     :message "Codex stream ended without a terminal response event")))))))
         ((error quit)
          (deslop-codex--fail
           session
           (deslop-error-create
            :kind 'protocol
            :message (error-message-string finish-error))))))))

(defun deslop-codex--message-content (role content)
  "Return CONTENT in the Responses shape required for ROLE."
  (if (string= role "system")
      content
    (list (list (cons :type (if (string= role "assistant")
                                "output_text"
                              "input_text"))
                (cons :text content)))))

(defun deslop-codex--request-body (messages profile)
  "Build a Responses API request body for MESSAGES and PROFILE."
  (let ((body
         (list (cons :model (plist-get profile :model))
               (cons :input
                     (mapcar (lambda (message)
                               (list (cons :role (car message))
                                     (cons :content
                                           (deslop-codex--message-content
                                            (car message) (cdr message)))))
                             messages))
               (cons :store json-false)
               (cons :stream t))))
    (if deslop-max-output-tokens
        (append body (list (cons :max_output_tokens
                                 deslop-max-output-tokens)))
      body)))

(defun deslop-codex--start-generation (session messages auth)
  "Start a Codex generation for SESSION using AUTH."
  (let* ((default-directory (deslop-curl-local-directory))
         (profile (deslop-codex--session-profile session))
         (url (deslop-codex--validated-generation-url
               (or (plist-get profile :url) deslop-codex-endpoint)))
         (account-id (deslop-codex--account-id auth))
         (residency (deslop-codex--residency auth))
          (headers (append
                    (list (concat "Authorization: Bearer "
                                  (plist-get auth :access))
                          "originator: opencode"
                         (concat "session-id: "
                                 (secure-hash 'sha256
                                              (format "%s-%s"
                                                      (float-time) (random)))))
                   (when account-id
                     (list (concat "ChatGPT-Account-Id: " account-id)))
                   (when residency
                     (list (concat "x-openai-internal-codex-residency: "
                                   residency)))))
         (body (encode-coding-string
                (json-encode (deslop-codex--request-body messages profile))
                'utf-8))
         (request
          (deslop-curl-start-request
            :profile profile
            :url url
           :internal-headers headers
           :name (format "deslop-codex-%s"
                         (or (plist-get profile :job-id) "request"))
           :command (lambda (headers-file config-file)
                      (deslop-curl-command profile headers-file config-file))
           :filter (apply-partially #'deslop-codex--filter session)
           :sentinel (apply-partially #'deslop-codex--sentinel session))))
     (setf (deslop-codex--session-process session) (plist-get request :process)
          (deslop-codex--session-process-buffer session)
          (plist-get request :response-buffer)
          (deslop-codex--session-stderr-buffer session)
          (plist-get request :stderr-buffer)
          (deslop-codex--session-headers-file session)
          (plist-get request :headers-file)
           (deslop-codex--session-config-file session)
           (plist-get request :config-file))
     (deslop-transport-session-set-resources
      (deslop-codex--lifecycle session) request)
     (process-send-string (deslop-codex--session-process session) body)
    (process-send-eof (deslop-codex--session-process session))))

(defun deslop-codex--auth-ready (session messages auth)
  "Start generation for SESSION when AUTH is valid, or fail it."
  (unless (deslop-codex--session-terminal session)
    (if (deslop-error-p auth)
        (deslop-codex--fail session auth)
      (condition-case startup-error
          (deslop-codex--start-generation session messages auth)
        ((error quit)
         (deslop-codex--fail
          session
          (deslop-error-create
           :kind 'startup
           :message (error-message-string startup-error))))))))

(defun deslop-codex-default (messages callbacks &optional options)
  "Send MESSAGES through the ChatGPT/Codex endpoint using OpenCode auth."
  (let* ((profile
          (deslop-plist-merge
           (list :url deslop-codex-endpoint :model deslop-codex-model)
           options))
         (session
          (deslop-codex--make-session
           :profile profile :callbacks callbacks
           :sse (deslop-protocol-make-sse)
           :estimated-chars 0 :received-bytes 0 :preview-parts nil
           :preview-bytes 0)))
    (condition-case startup-error
        (progn
           (deslop-validate-resource-options)
           (deslop-codex--validated-generation-url
            (plist-get profile :url))
          (deslop-codex--invoke-callback
           session :on-start (apply-partially #'deslop-codex--cancel session))
          (unless (deslop-codex--session-terminal session)
            (setf (deslop-codex--session-auth-cancel session)
                  (deslop-codex--resolve-auth
                   (apply-partially #'deslop-codex--auth-ready
                                    session messages)))))
      ((error quit)
       (deslop-codex--fail
        session
        (deslop-error-create
         :kind 'startup
         :message (error-message-string startup-error)))))
    (deslop-codex--session-process session)))

(provide 'deslop-codex)

;;; deslop-codex.el ends here
