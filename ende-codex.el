;;; ende-codex.el --- ChatGPT/Codex transport for ende -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools, llm

;;; Commentary:

;; Reuse the OAuth credentials maintained by OpenCode for ChatGPT Plus/Pro.
;; This deliberately does not implement a second login flow: users authenticate
;; OpenCode once, then ende reads and refreshes its openai OAuth record.

;;; Code:

(require 'cl-lib)
(require 'ende-content)
(require 'json)
(require 'subr-x)
(require 'url-util)
(require 'ende-core)
(require 'ende-controls)
(require 'ende-curl)
(require 'ende-protocol)
(require 'ende-transport)
(require 'ende-stream)
(require 'ende-debug)
(require 'ende-request)
(require 'ende-provider-config)

(defcustom ende-codex-auth-file
  (expand-file-name "~/.local/share/opencode/auth.json")
  "OpenCode credential file containing the `openai' OAuth record."
  :type 'file
  :group 'ende)

(defcustom ende-codex-issuer "https://auth.openai.com"
  "OAuth issuer used to refresh OpenCode's ChatGPT credentials."
  :type 'string
  :group 'ende)

(defcustom ende-codex-client-id "app_EMoamEEZ73f0CkXaXp7hrann"
  "OpenAI OAuth client id used by OpenCode's ChatGPT integration."
  :type 'string
  :group 'ende)

(defcustom ende-codex-model "gpt-5.6-luna"
  "Default model used by the ChatGPT/Codex profile."
  :type 'string
  :group 'ende)

(defcustom ende-codex-endpoint "https://chatgpt.com/backend-api/codex/responses"
  "ChatGPT/Codex Responses endpoint."
  :type 'string
  :group 'ende)

(defcustom ende-codex-models-endpoint "https://chatgpt.com/backend-api/codex/models"
  "Account-scoped ChatGPT/Codex model discovery endpoint."
  :type 'string
  :group 'ende)

(defcustom ende-codex-usage-endpoint
  "https://chatgpt.com/backend-api/wham/usage"
  "Account-scoped ChatGPT/Codex quota endpoint."
  :type 'string
  :group 'ende)

(defcustom ende-codex-client-version "1.0.0"
  "Client version sent to the ChatGPT/Codex model discovery endpoint."
  :type 'string
  :group 'ende)

(defconst ende-codex--generation-host "chatgpt.com"
  "Official host allowed for Codex generation and model discovery.")

(defconst ende-codex--issuer-host "auth.openai.com"
  "Official host allowed for Codex OAuth refreshes.")

(defconst ende-codex--generation-path "/backend-api/codex/responses"
  "Official Codex generation path.")

(defconst ende-codex--models-path "/backend-api/codex/models"
  "Official Codex model discovery path.")

(defun ende-codex--session-id-for-identity (identity)
  "Return an opaque Codex session ID derived from stable IDENTITY."
  (secure-hash 'sha256 (format "ende-codex:%s" identity)))

(defun ende-codex--new-session-id ()
  "Return a fresh Codex session ID for a job without a stable identity."
  (ende-codex--session-id-for-identity
   (format "job:%s:%s:%s" (emacs-pid) (float-time)
           (random most-positive-fixnum))))

(defconst ende-codex--usage-path "/backend-api/wham/usage"
  "Official ChatGPT/Codex quota path.")

(defconst ende-codex--refresh-path "/oauth/token"
  "Official Codex OAuth refresh path.")

(defvar ende-codex--refresh-processes nil
  "In-flight OpenAI OAuth refresh processes.")

(cl-defstruct (ende-codex--refresh-waiter
               (:constructor ende-codex--make-refresh-waiter))
  "One callback waiting for a shared OAuth refresh flight."
  callback cancelled)

(cl-defstruct (ende-codex--refresh-flight
               (:constructor ende-codex--make-refresh-flight))
  "One coalesced OAuth refresh and its waiting callbacks."
  key auth waiters process response-buffer stderr-buffer stderr-process
  headers-file
  received-bytes finished)

(defvar ende-codex--refresh-flights (make-hash-table :test #'equal)
  "In-flight OAuth refreshes keyed by credential source and client identity.")

(defvar ende-codex--model-probe-processes nil
  "In-flight ChatGPT/Codex model discovery processes.")

(defvar ende-codex--quota-probe-processes nil
  "In-flight ChatGPT/Codex quota discovery processes.")

(cl-defstruct (ende-codex--session
               (:constructor ende-codex--allocate-session))
  profile stream tool-calls terminal-event debug-record strict-completion replay-items)

(defun ende-codex--make-session (&rest arguments)
  "Create a Codex protocol session from ARGUMENTS.
The :callbacks argument initializes the composed transport lifecycle."
  (let* ((callbacks (plist-get arguments :callbacks))
         (session
          (apply #'ende-codex--allocate-session
                 (cl-loop for (key value) on arguments by #'cddr
                          unless (eq key :callbacks)
                          append (list key value)))))
    (let ((lifecycle
           (ende-transport-session-create
            callbacks (apply-partially #'ende-codex--fail session))))
      (setf (ende-codex--session-stream session)
            (ende-stream-create lifecycle (ende-protocol-make-sse)))
      session)))

(defun ende-codex--lifecycle (session)
  "Return the composed transport lifecycle for SESSION."
  (ende-stream-lifecycle (ende-codex--session-stream session)))

(defun ende-codex--terminal-p (session)
  "Return non-nil when SESSION's transport has settled."
  (ende-stream-terminal-p (ende-codex--session-stream session)))

;;;; Credential handling

(defun ende-codex--auth-version (auth)
  "Return the credential version represented by AUTH."
  (list (plist-get auth :access)
        (plist-get auth :refresh)
        (plist-get auth :expires)
        (plist-get auth :account-id)))

(defun ende-codex--entry-version (entry)
  "Return the credential version represented by JSON ENTRY."
  (list (ende-protocol-json-get "access" entry)
        (ende-protocol-json-get "refresh" entry)
        (ende-protocol-json-get "expires" entry)
        (ende-protocol-json-get "accountId" entry)))

(defun ende-codex--validated-official-url (url host path)
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

(defun ende-codex--validated-generation-url (url)
  "Validate the credential-bearing Codex generation URL URL."
  (ende-codex--validated-official-url
   url ende-codex--generation-host ende-codex--generation-path))

(defun ende-codex--validated-models-url ()
  "Validate and return the credential-bearing Codex models URL."
  (ende-codex--validated-official-url
   ende-codex-models-endpoint
   ende-codex--generation-host
   ende-codex--models-path))

(defun ende-codex--validated-usage-url ()
  "Validate and return the credential-bearing Codex quota URL."
  (ende-codex--validated-official-url
   ende-codex-usage-endpoint
   ende-codex--generation-host
   ende-codex--usage-path))

(defun ende-codex--validated-refresh-url ()
  "Validate and return the official Codex OAuth refresh URL."
  (ende-codex--validated-official-url
   (concat (string-remove-suffix "/" ende-codex-issuer)
           ende-codex--refresh-path)
   ende-codex--issuer-host
   ende-codex--refresh-path))

(defun ende-codex--auth-fresh-p (auth)
  "Return non-nil when AUTH has enough lifetime for a new request."
  (and (stringp (plist-get auth :access))
       (numberp (plist-get auth :expires))
       (> (plist-get auth :expires)
          (+ (* 1000 (floor (float-time))) 30000))))

(defun ende-codex--read-json-file (file)
  "Read FILE as a JSON hash table without interning its keys."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (ende-protocol-json-read-string (buffer-string))))

(defun ende-codex--auth-entry ()
  "Return OpenCode's OAuth entry for the `openai' provider."
  (let* ((default-directory (ende-curl-local-directory))
         (data (ende-codex--read-json-file ende-codex-auth-file))
         (entry (ende-protocol-json-get "openai" data)))
    (unless (and (hash-table-p entry)
                 (string= (format "%s"
                                  (ende-protocol-json-get "type" entry))
                          "oauth"))
      (error "OpenCode has no ChatGPT OAuth credentials for openai"))
    (let ((access (ende-protocol-json-get "access" entry))
          (refresh (ende-protocol-json-get "refresh" entry))
          (expires (ende-protocol-json-get "expires" entry))
          (account-id (ende-protocol-json-get "accountId" entry)))
      (unless (and (stringp access) (stringp refresh) (numberp expires))
        (error "OpenCode's openai OAuth credentials are incomplete"))
      (list :access access :refresh refresh :expires expires
            :account-id account-id))))

(defun ende-codex--jwt-claims (token)
  "Decode the unverified claims from TOKEN, or return nil.
The claims are used only for routing metadata such as account and residency."
  (when (stringp token)
    (let ((parts (split-string token "\\.")))
      (when (= (length parts) 3)
        (condition-case nil
            (let* ((encoded (replace-regexp-in-string
                             "-" "+"
                             (replace-regexp-in-string "_" "/"
                                                       (nth 1 parts))))
                   (padding (mod (- 4 (mod (length encoded) 4)) 4))
                   (decoded (base64-decode-string
                             (concat encoded (make-string padding ?=)))))
              (ende-protocol-json-read-string
               (decode-coding-string decoded 'utf-8)))
          ((error quit) nil))))))

(defun ende-codex--claim (claims key)
  "Return claim KEY from CLAIMS, including namespaced auth claims."
  (or (ende-protocol-json-get key claims)
      (ende-protocol-json-get key
                              (ende-protocol-json-get
                               "https://api.openai.com/auth" claims))))

(defun ende-codex--account-id (auth)
  "Return the ChatGPT account id in AUTH or its access token."
  (or (plist-get auth :account-id)
      (let ((claims (ende-codex--jwt-claims (plist-get auth :access))))
        (or (ende-codex--claim claims "chatgpt_account_id")
            (let ((organizations
                   (ende-protocol-json-get "organizations" claims)))
              (and (vectorp organizations)
                   (> (length organizations) 0)
                   (ende-protocol-json-get
                    "id" (aref organizations 0))))))))

(defun ende-codex--residency (auth)
  "Return the compute residency claim in AUTH, if constrained."
  (let ((residency
         (ende-codex--claim
          (ende-codex--jwt-claims (plist-get auth :access))
          "chatgpt_compute_residency")))
    (and (stringp residency)
         (not (string-empty-p residency))
         (not (string= residency "no_constraint"))
         residency)))

(defun ende-codex--form-field (value)
  "Encode VALUE for an OAuth form body."
  (url-hexify-string (format "%s" value)))

(defun ende-codex--refresh-body (refresh-token)
  "Return an OAuth refresh body for REFRESH-TOKEN."
  (format "grant_type=refresh_token&refresh_token=%s&client_id=%s"
          (ende-codex--form-field refresh-token)
          (ende-codex--form-field ende-codex-client-id)))

(defun ende-codex--store-auth (auth &optional expected-version)
  "Write refreshed AUTH back to OpenCode's credential file atomically.
When EXPECTED-VERSION is non-nil, refuse to replace credentials that changed
since the refresh began."
  (let* ((default-directory (ende-curl-local-directory))
         (file (expand-file-name ende-codex-auth-file))
         (data (progn
                 (when (file-remote-p file)
                   (user-error "Ende requires a local Codex credential file"))
                 (ende-codex--read-json-file file)))
         (old (ende-protocol-json-get "openai" data))
         (entry (if (hash-table-p old)
                    (copy-hash-table old)
		  (make-hash-table :test #'equal)))
         (temporary nil)
         (complete nil))
    (when (and expected-version
               (not (equal expected-version
                           (ende-codex--entry-version old))))
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
                 (expand-file-name ".ende-openai-auth-"
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
          (ende-ignore-errors #'delete-file temporary))))))

(defun ende-codex--refresh-command (url headers-file)
  "Build the curl command for an OAuth refresh request to URL."
  (append
   (ende-curl-base-command url)
   (ende-curl-response-options headers-file)
   (list "--request" "POST"
         "--header" "Content-Type: application/x-www-form-urlencoded"
         "--data-binary" "@-")
   (ende-curl-timeout-options ende-request-timeout
                              ende-max-oauth-refresh-bytes)
   (list "--url" url)))

(defun ende-codex--refresh-finish (flight result &optional cancelled)
  "Finish FLIGHT and notify its remaining waiters with RESULT."
  (unless (ende-codex--refresh-flight-finished flight)
    (setf (ende-codex--refresh-flight-finished flight) t)
    (when (eq (gethash (ende-codex--refresh-flight-key flight)
                       ende-codex--refresh-flights)
              flight)
      (remhash (ende-codex--refresh-flight-key flight)
               ende-codex--refresh-flights))
    (let ((process (ende-codex--refresh-flight-process flight))
          (waiters (ende-codex--refresh-flight-waiters flight)))
      (setf (ende-codex--refresh-flight-waiters flight) nil
            ende-codex--refresh-processes
            (delq process ende-codex--refresh-processes))
      (when (and (processp process) (process-live-p process))
        (ende-ignore-errors #'delete-process process))
      (ende-curl-cleanup-resources
       (list (ende-codex--refresh-flight-response-buffer flight)
             (ende-codex--refresh-flight-stderr-buffer flight))
       (list (ende-codex--refresh-flight-headers-file flight))
       (list (ende-codex--refresh-flight-stderr-process flight)))
      (unless cancelled
        (dolist (waiter waiters)
          (unless (ende-codex--refresh-waiter-cancelled waiter)
            (condition-case nil
                (funcall (ende-codex--refresh-waiter-callback waiter)
                         result)
              ((error quit) nil))))))))

(defun ende-codex--refresh-cancel-waiter (flight waiter)
  "Cancel WAITER from FLIGHT without disturbing other waiters."
  (unless (ende-codex--refresh-waiter-cancelled waiter)
    (setf (ende-codex--refresh-waiter-cancelled waiter) t)
    (when (and (not (ende-codex--refresh-flight-finished flight))
               (eq (gethash (ende-codex--refresh-flight-key flight)
                            ende-codex--refresh-flights)
                   flight))
      (setf (ende-codex--refresh-flight-waiters flight)
            (delq waiter (ende-codex--refresh-flight-waiters flight)))
      (when (null (ende-codex--refresh-flight-waiters flight))
        (ende-codex--refresh-finish flight nil t)))))

(defun ende-codex--refresh-filter (flight _process chunk)
  "Consume refresh response CHUNK for FLIGHT within its byte budget."
  (unless (ende-codex--refresh-flight-finished flight)
    (let* ((received (ende-codex--refresh-flight-received-bytes flight))
           (total (+ received (string-bytes chunk))))
      (setf (ende-codex--refresh-flight-received-bytes flight) total)
      (if (and ende-max-oauth-refresh-bytes
               (> total ende-max-oauth-refresh-bytes))
          (ende-codex--refresh-finish
           flight
           (ende-error-create
            :kind 'resource
            :message
            "OAuth refresh response exceeds ende-max-oauth-refresh-bytes"))
        (let ((buffer (ende-codex--refresh-flight-response-buffer flight)))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert chunk))))))))

(defun ende-codex--refresh-sentinel (flight process event)
  "Handle terminal PROCESS EVENT for refresh FLIGHT."
  (when (and (not (ende-codex--refresh-flight-finished flight))
             (memq (process-status process) '(exit signal closed)))
    (if (/= (process-exit-status process) 0)
        (ende-codex--refresh-finish
         flight
         (or (process-get process :ende-idle-error)
             (ende-error-create
              :kind 'auth
              :message
              (format "OpenAI token refresh failed: %s" (string-trim event)))))
      (condition-case parse-error
          (let* ((headers
                  (ende-curl-response-headers
                   (ende-codex--refresh-flight-headers-file flight)))
                 (status (car headers))
                 (buffer (ende-codex--refresh-flight-response-buffer flight))
                 (payload
                  (with-current-buffer buffer
                    (ende-protocol-json-read-string (buffer-string))))
                 (access (ende-protocol-json-get "access_token" payload))
                 (refresh (ende-protocol-json-get "refresh_token" payload))
                 (expires-in (ende-protocol-json-get "expires_in" payload)))
            (if (or (null status) (< status 200) (>= status 300))
                (ende-codex--refresh-finish
                 flight
                 (ende-error-create
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
                           (or (plist-get (ende-codex--refresh-flight-auth flight)
                                          :account-id)
                               (ende-codex--claim
                                (ende-codex--jwt-claims access)
                                "chatgpt_account_id")))))
                (condition-case store-error
                    (progn
                      (ende-codex--store-auth
                       refreshed
                       (ende-codex--auth-version
                        (ende-codex--refresh-flight-auth flight)))
                      (ende-codex--refresh-finish flight refreshed))
                  ((error quit)
                   (let ((current
                          (condition-case nil
                              (ende-codex--auth-entry)
                            ((error quit) nil))))
                     (if (and (string-match-p
                               "credentials changed during refresh"
                               (error-message-string store-error))
                              (ende-codex--auth-fresh-p current))
                         (ende-codex--refresh-finish flight current)
                       (ende-codex--refresh-finish
                        flight
                        (ende-error-create
                         :kind 'auth
                         :message (error-message-string store-error))))))))))
        ((error quit)
         (ende-codex--refresh-finish
          flight
          (ende-error-create
           :kind 'auth
           :message (error-message-string parse-error)))))))
  )

(defun ende-codex--start-refresh-flight (flight)
  "Start the process belonging to refresh FLIGHT."
  (let ((response-buffer nil)
        (stderr-buffer nil)
        (stderr-process nil)
        (headers-file nil)
        (process nil))
    (condition-case startup-error
        (let ((default-directory (ende-curl-local-directory)))
          (ende-validate-resource-options)
          (setq response-buffer (generate-new-buffer " *ende-openai-refresh*"))
          (setf (ende-codex--refresh-flight-response-buffer flight)
                response-buffer)
          (setq stderr-buffer
                (generate-new-buffer " *ende-openai-refresh-stderr*"))
          (setf (ende-codex--refresh-flight-stderr-buffer flight) stderr-buffer)
          (setq headers-file (make-temp-file "ende-openai-headers-"))
          (setf (ende-codex--refresh-flight-headers-file flight) headers-file)
          (set-file-modes headers-file #o600)
          (setq stderr-process
                (make-pipe-process
                 :name (generate-new-buffer-name
                        "ende-openai-refresh-stderr")
                 :filter (apply-partially #'ende-curl-stderr-filter
                                          stderr-buffer)
                 :noquery t))
          (setf (ende-codex--refresh-flight-stderr-process flight)
                stderr-process)
          (let ((url (ende-codex--validated-refresh-url)))
            (setq process
                  (make-process
                   :name "ende-openai-refresh"
                   :buffer response-buffer
                   :stderr stderr-process
                   :connection-type 'pipe
                   :noquery t
                   :command (ende-codex--refresh-command url headers-file)
                   :filter (apply-partially
                            #'ende-codex--refresh-filter flight)
                   :sentinel (apply-partially
                              #'ende-codex--refresh-sentinel flight)))
            (setf (ende-codex--refresh-flight-process flight) process)
            (ende-curl-watch-idle process)
            (push process ende-codex--refresh-processes)
            (process-send-string
             process
             (ende-codex--refresh-body
              (plist-get (ende-codex--refresh-flight-auth flight) :refresh)))
            (process-send-eof process)))
      ((error quit)
       (ende-codex--refresh-finish
        flight
        (ende-error-create
         :kind 'auth
         :message (error-message-string startup-error)))))))

(defun ende-codex--refresh-auth (auth callback)
  "Coalesce a refresh for AUTH and register CALLBACK as a waiter.
Return a cancellation function that only removes this callback."
  (let* ((default-directory (ende-curl-local-directory))
         (key (list (expand-file-name ende-codex-auth-file)
                    ende-codex-issuer
                    ende-codex-client-id))
         (flight (gethash key ende-codex--refresh-flights))
         (new-flight (null flight))
         (waiter (ende-codex--make-refresh-waiter :callback callback)))
    (when new-flight
      (setq flight
            (ende-codex--make-refresh-flight
             :key key :auth auth :waiters nil :received-bytes 0))
      (puthash key flight ende-codex--refresh-flights))
    (setf (ende-codex--refresh-flight-waiters flight)
          (cons waiter (ende-codex--refresh-flight-waiters flight)))
    (when new-flight
      (ende-codex--start-refresh-flight flight))
    (apply-partially #'ende-codex--refresh-cancel-waiter flight waiter)))

(defun ende-codex--resolve-auth (callback)
  "Load OpenCode's credentials and call CALLBACK with AUTH or an error.
Return a cancellation function for an asynchronous refresh, or nil."
  (condition-case load-error
      (let ((auth (ende-codex--auth-entry)))
        (if (ende-codex--auth-fresh-p auth)
            (progn (funcall callback auth) nil)
          (ende-codex--refresh-auth auth callback)))
    ((error quit)
     (funcall callback
              (ende-error-create
               :kind 'auth
               :message (error-message-string load-error)))
     nil)))

;;;; Model discovery

(defun ende-codex--models-command (url headers-file config-file)
  "Build a curl GET command for the account-scoped model list at URL."
  (append
   (ende-curl-base-command url)
   (ende-curl-response-options headers-file)
   (list "--get"
         "--data-urlencode"
         (concat "client_version=" ende-codex-client-version))
   (when config-file (list "--config" config-file))
   (ende-curl-timeout-options ende-request-timeout
                              ende-provider-max-discovery-bytes)
   (list "--url" url)))

(defun ende-codex--parse-model-entries (text)
  "Return model objects parsed from a Codex model-list response TEXT."
  (let* ((payload (ende-protocol-json-read-string text))
         (models (ende-protocol-json-get "models" payload)))
    (unless (vectorp models)
      (error "Codex model response has no models array"))
    (append models nil)))

(defun ende-codex--parse-models (text)
  "Return model slugs parsed from a Codex model-list response TEXT."
  (cl-loop for model in (ende-codex--parse-model-entries text)
           for slug = (ende-protocol-json-get "slug" model)
           when (and (stringp slug) (not (string-empty-p slug)))
           collect slug))

(defun ende-codex--usage-command (url headers-file config-file)
  "Build a curl GET command for the account-scoped quota URL URL."
  (append
   (ende-curl-base-command url)
   (ende-curl-response-options headers-file)
   (list "--request" "GET"
         "--header" "Accept: application/json")
   (when config-file (list "--config" config-file))
   (ende-curl-timeout-options ende-request-timeout
                              ende-provider-max-discovery-bytes)
   (list "--url" url)))

(defun ende-codex--quota-window (entry)
  "Return a normalized quota window from JSON object ENTRY, or nil."
  (when (hash-table-p entry)
    (let ((used (ende-protocol-json-get "used_percent" entry))
          (reset (ende-protocol-json-get "reset_at" entry))
          (window (ende-protocol-json-get "limit_window_seconds" entry)))
      (when (numberp used)
        (list :used-percent (max 0 (min 100 used))
              :reset-at (and (numberp reset) reset)
              :window-seconds
              (and (numberp window) (> window 0) window))))))

(defun ende-codex--parse-quota (text)
  "Return normalized quota windows parsed from usage response TEXT."
  (let* ((payload (ende-protocol-json-read-string text))
         (rate-limit (ende-protocol-json-get "rate_limit" payload))
         (primary (and (hash-table-p rate-limit)
                       (ende-codex--quota-window
                        (ende-protocol-json-get "primary_window" rate-limit))))
         (secondary (and (hash-table-p rate-limit)
                         (ende-codex--quota-window
                          (ende-protocol-json-get
                           "secondary_window" rate-limit)))))
    (unless (or primary secondary)
      (error "Codex usage response has no quota windows"))
    (list :primary primary :secondary secondary)))

(defun ende-codex--duration-string (seconds)
  "Return a compact human-readable duration for SECONDS."
  (let* ((seconds (max 0 (floor seconds)))
         (days (/ seconds 86400))
         (hours (/ (% seconds 86400) 3600))
         (minutes (/ (% seconds 3600) 60)))
    (cond
     ((> days 0) (format "%dd %dh" days hours))
     ((> hours 0) (format "%dh%s" hours
                          (if (> minutes 0) (format " %dm" minutes) "")))
     ((> minutes 0) (format "%dm" minutes))
     (t "now"))))

(defun ende-codex--quota-window-label (window now &optional until-reset)
  "Return a display label for quota WINDOW at NOW.
When UNTIL-RESET is non-nil, prefer the window's reset countdown."
  (let ((seconds (if (and until-reset
                          (numberp (plist-get window :reset-at)))
                     (- (plist-get window :reset-at) now)
                   (plist-get window :window-seconds))))
    (cond
     ((and (numberp seconds) (>= seconds (* 7 86400))) "weekly")
     ((numberp seconds) (ende-codex--duration-string seconds))
     (t "quota"))))

(defun ende-codex-quota-string (quota &optional now)
  "Return a compact display string for normalized QUOTA.
NOW is the current time as a Unix timestamp and is injectable for tests."
  (let* ((now (or now (float-time)))
         (segments nil)
         (secondary-reset-at
          (plist-get (plist-get quota :secondary) :reset-at)))
    (dolist (key '(:primary :secondary))
      (let ((window (plist-get quota key)))
        (when window
          (push (format "%s: %d%% left"
                        (ende-codex--quota-window-label
                         window now (eq key :primary))
                        (round (- 100 (plist-get window :used-percent))))
                segments))))
    (setq segments (nreverse segments))
    (when (numberp secondary-reset-at)
      (setq segments
            (append segments
                    (list (format "reset in %s"
                                  (ende-codex--duration-string
                                   (- secondary-reset-at now)))))))
    (mapconcat #'identity segments " · ")))

(defun ende-codex--probe-json
    (url request-name command parser callback &optional process-slot)
  "Probe authenticated JSON URL and call CALLBACK with parsed data.
COMMAND receives URL, headers-file, and config-file.  PARSER receives the
complete response body.  CALLBACK receives (VALUE ERROR).  When PROCESS-SLOT
is non-nil, it names a variable tracking the live request process.  Return a
cancellation function."
  (let ((response-buffer nil)
        (stderr-buffer nil)
        (stderr-process nil)
        (headers-file nil)
        (config-file nil)
        (process nil)
        (auth-cancel nil)
        (received-bytes 0)
        (finished nil))
    (cl-labels
        ((remove-process ()
           (when (and process process-slot)
             (set process-slot
                  (delq process (symbol-value process-slot)))))
         (finish (value &optional error cancelled)
           (unless finished
             (setq finished t)
             (remove-process)
             (when (and (processp process) (process-live-p process))
               (delete-process process))
             (ende-curl-cleanup-resources
              (list response-buffer stderr-buffer)
              (list headers-file config-file)
              (list stderr-process))
             (unless cancelled
               (funcall callback value error))))
         (start (auth)
           (let* ((account-id (ende-codex--account-id auth))
                  (headers
                   (append
                    (list (concat "Authorization: Bearer "
                                  (plist-get auth :access))
                          "originator: opencode")
                    (when account-id
                      (list (concat "ChatGPT-Account-Id: " account-id)))))
                  (request
                   (ende-curl-start-request
                    :profile (list :url url)
                    :name request-name
                    :internal-headers headers
                    :command
                    (lambda (url-headers url-config)
                      (funcall command url url-headers url-config))
                    :filter
                    (lambda (_probe-process chunk)
                      (unless finished
                        (setq received-bytes
                              (+ received-bytes (string-bytes chunk)))
                        (if (and ende-provider-max-discovery-bytes
                                 (> received-bytes
                                    ende-provider-max-discovery-bytes))
                            (finish
                             nil
                             (ende-error-create
                              :kind 'resource
                              :message
                              (format
                               "%s response exceeds ende-provider-max-discovery-bytes"
                               request-name)))
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
                             (or (process-get probe-process :ende-idle-error)
                                 (ende-error-create
                                  :kind 'transport
                                  :message
                                  (format "%s failed: %s"
                                          request-name
                                          (string-trim event)))))
                          (condition-case parse-error
                              (let* ((status
                                      (car (ende-curl-response-headers
                                            headers-file)))
                                     (text
                                      (with-current-buffer response-buffer
                                        (buffer-string)))
                                     (detail
                                      (ende-transport-error-detail text)))
                                (if (or (null status) (< status 200)
                                        (>= status 300))
                                    (finish
                                     nil
                                     (ende-error-create
                                      :kind 'http
                                      :message
                                      (format "HTTP status %s: %s"
                                              (or status "unknown")
                                              (or detail
                                                  (format "%s failed"
                                                          request-name)))))
                                  (finish (funcall parser text))))
                            ((error quit)
                             (finish
                              nil
                              (ende-error-create
                               :kind 'protocol
                               :message
                               (error-message-string parse-error)))))))))))
             (setq process (plist-get request :process)
                   response-buffer (plist-get request :response-buffer)
                   stderr-buffer (plist-get request :stderr-buffer)
                   stderr-process (plist-get request :stderr-process)
                   headers-file (plist-get request :headers-file)
                   config-file (plist-get request :config-file))
             (when process-slot
               (set process-slot
                    (cons process (symbol-value process-slot)))))))
      (condition-case startup-error
          (progn
            (ende-validate-resource-options)
            (setq auth-cancel
                  (ende-codex--resolve-auth
                   (lambda (auth-or-error)
                     (unless finished
                       (if (ende-error-p auth-or-error)
                           (finish nil auth-or-error)
                         (condition-case request-error
                             (start auth-or-error)
                           ((error quit)
                            (finish
                             nil
                             (ende-error-create
                              :kind 'startup
                              :message
                              (error-message-string request-error)))))))))))
        ((error quit)
         (finish
          nil
          (ende-error-create
           :kind 'startup
           :message (error-message-string startup-error)))))
      (lambda ()
        (when auth-cancel
          (funcall auth-cancel))
        (finish nil nil t)))))

(defun ende-codex--probe-model-entries (callback)
  "Probe the authenticated Codex endpoint and call CALLBACK with model objects.
CALLBACK receives (ENTRIES ERROR).  Return a cancellation function."
  (condition-case validation-error
      (ende-codex--probe-json
       (ende-codex--validated-models-url)
       "ende-codex-model-probe"
       #'ende-codex--models-command
       #'ende-codex--parse-model-entries
       callback
       'ende-codex--model-probe-processes)
    ((error quit)
     (funcall callback
              nil
              (ende-error-create
               :kind 'startup
               :message (error-message-string validation-error)))
     nil)))

(defun ende-codex-probe-quota (callback)
  "Probe OpenAI's authenticated quota endpoint and call CALLBACK.
CALLBACK receives (QUOTA ERROR).  Return a cancellation function."
  (condition-case validation-error
      (ende-codex--probe-json
       (ende-codex--validated-usage-url)
       "ende-codex-quota-probe"
       #'ende-codex--usage-command
       #'ende-codex--parse-quota
       callback
       'ende-codex--quota-probe-processes)
    ((error quit)
     (funcall callback
              nil
              (ende-error-create
               :kind 'startup
               :message (error-message-string validation-error)))
     nil)))

(defun ende-codex-probe-models (callback)
  "Probe the authenticated Codex endpoint and call CALLBACK with model slugs.
CALLBACK receives (MODELS ERROR).  Return a cancellation function."
  (ende-codex--probe-model-entries
   (lambda (entries error)
     (funcall callback
              (and entries
                   (cl-loop for entry in entries
                            for slug = (ende-protocol-json-get "slug" entry)
                            when (and (stringp slug) (not (string-empty-p slug)))
                            collect slug))
              error))))

(defun ende-codex--reasoning-effort-options (entry)
  "Return explicit reasoning effort metadata from Codex model ENTRY."
  (let ((levels (ende-protocol-json-get "supported_reasoning_levels" entry))
        (seen nil)
        (efforts nil))
    (dolist (level (cond ((vectorp levels) (append levels nil))
                         ((listp levels) levels)))
      (let ((effort (ende-protocol-json-get "effort" level)))
        (when (and (stringp effort) (not (string-empty-p effort))
                   (not (member effort seen)))
          (push effort seen)
          (push effort efforts))))
    (setq efforts (nreverse efforts))
    (list :efforts efforts
          :default
          (let ((default (ende-protocol-json-get
                          "default_reasoning_level" entry)))
            (and (stringp default) (member default efforts) default)))))

(defun ende-codex-probe-reasoning-efforts (_profile model callback)
  "Query the current Codex account for MODEL's reasoning efforts.
CALLBACK receives (EFFORTS DEFAULT ERROR).  The account-scoped model endpoint
is queried for every call; it does not share the model catalog cache."
  (ende-codex--probe-model-entries
   (lambda (entries error)
     (if error
         (funcall callback nil nil error)
       (let ((entry (cl-find model entries
                             :key (lambda (value)
                                    (ende-protocol-json-get "slug" value))
                             :test #'equal)))
         (if (not entry)
             (funcall
              callback nil nil
              (ende-error-create
               :kind 'capability
               :message (format "Codex does not advertise model %s" model)))
           (let* ((options (ende-codex--reasoning-effort-options entry))
                  (efforts (plist-get options :efforts)))
             (if efforts
                 (funcall callback efforts (plist-get options :default) nil)
               (funcall
                callback nil nil
                (ende-error-create
                 :kind 'capability
                 :message
                 (format "Codex does not advertise explicit reasoning efforts for %s"
                         model)))))))))))

;;;; Responses transport

(defun ende-codex--tool-call-list (session)
  "Return collected function calls from SESSION in output order."
  (let (calls)
    (when (hash-table-p (ende-codex--session-tool-calls session))
      (maphash (lambda (index call)
                 (push (cons index call) calls))
               (ende-codex--session-tool-calls session)))
    (mapcar #'cdr
            (sort calls (lambda (left right) (< (car left) (car right)))))))

(defun ende-codex--history (session)
  "Return completed replayable output in SESSION's wire order."
  (let ((items (sort (copy-sequence (ende-codex--session-replay-items session))
                     (lambda (a b) (< (car a) (car b))))))
    (when items (cons 'openai-responses (mapcar #'cdr items)))))

(defun ende-codex--remember-output (session item index)
  "Capture completed replay ITEM at output INDEX for SESSION."
  (when (and (integerp index) (>= index 0)
             (member (ende-protocol-json-get "type" item) '("message" "reasoning" "function_call")))
    (setf (alist-get index (ende-codex--session-replay-items session))
          (ende-provider-config-copy item))))

(defun ende-codex--result (session)
  "Build a normalized result plist for SESSION."
  (ende-stream-result
   (ende-codex--session-stream session) "output_tokens"
   (plist-get (ende-codex--session-profile session) :model) "OpenAI"
   (list :tool-calls (ende-codex--tool-call-list session)
         :protocol-history (ende-codex--history session))))

(defun ende-codex--response-status (session)
  "Return SESSION's HTTP response status, or nil."
  (condition-case nil
      (car (ende-stream-response-headers
            (ende-codex--session-stream session)))
    (error nil)))

(defun ende-codex--settle (session outcome value)
  "Settle SESSION once with OUTCOME and VALUE."
  (when (ende-codex--session-debug-record session)
    (ende-debug-record-completion
     (ende-codex--session-debug-record session) outcome
     (ende-stream-session-usage (ende-codex--session-stream session))
     (ende-codex--response-status session) value))
  (let ((tool-calls (ende-codex--tool-call-list session)))
    (ende-stream-settle-generation
     (ende-codex--session-stream session) outcome value "output_tokens"
     (plist-get (ende-codex--session-profile session) :model) "OpenAI"
     (list :tool-calls tool-calls :protocol-history (ende-codex--history session))
     tool-calls nil t)))

(defun ende-codex--fail (session error)
  "Settle SESSION with structured ERROR."
  (ende-codex--settle session 'failure error))

(defun ende-codex--cancel (session)
  "Cancel SESSION."
  (ende-codex--fail
   session (ende-error-create :kind 'cancelled :message "Process cancelled")))

(defun ende-codex--emit (session event)
  "Emit EVENT unless SESSION has settled."
  (ende-stream-emit (ende-codex--session-stream session) event))

(defun ende-codex--emit-delta (session channel text)
  "Record and emit TEXT on CHANNEL."
  (ende-stream-record-delta
   (ende-codex--session-stream session) channel text
   (apply-partially #'ende-codex--fail session)))

(defun ende-codex--emit-reasoning-summary (session item text)
  "Record and emit summary TEXT for provider ITEM."
  (ende-stream-record-reasoning-summary
   (ende-codex--session-stream session) item text))

(defun ende-codex--handle-usage (session usage)
  "Record and emit Responses API USAGE."
  (when (hash-table-p usage)
    (ende-stream-record-usage
     (ende-codex--session-stream session) usage "output_tokens")))

(defun ende-codex--tool-call-table (session)
  "Return the function-call table for SESSION, creating it if needed."
  (or (ende-codex--session-tool-calls session)
      (let ((table (make-hash-table :test #'eql)))
        (setf (ende-codex--session-tool-calls session) table)
        table)))

(defun ende-codex--collect-tool-call-item (session item output-index)
  "Collect function-call ITEM for SESSION at OUTPUT-INDEX."
  (when (equal (ende-protocol-json-get "type" item) "function_call")
    (let* ((table (ende-codex--tool-call-table session))
           (old (gethash output-index table))
           (arguments (ende-protocol-json-get "arguments" item)))
      (puthash
       output-index
       (list :id (or (ende-protocol-json-get "call_id" item)
                     (plist-get old :id))
             :name (or (ende-protocol-json-get "name" item)
                       (plist-get old :name))
             :arguments (if (and (stringp arguments)
                                 (not (string-empty-p arguments)))
                            arguments
                          (plist-get old :arguments)))
       table))))

(defun ende-codex--collect-tool-calls (session items)
  "Collect Responses API function-call ITEMS for SESSION."
  (let ((index 0))
    (dolist (item (ende-codex--output-items items))
      (ende-codex--collect-tool-call-item
       session item (or (ende-protocol-json-get "output_index" item) index))
      (cl-incf index))))

(defun ende-codex--update-tool-call-arguments
    (session output-index arguments &optional append)
  "Update the call at OUTPUT-INDEX with ARGUMENTS.
When APPEND is non-nil, append the argument delta to the stored value."
  (let* ((table (ende-codex--session-tool-calls session))
         (call (and (hash-table-p table) (gethash output-index table))))
    (when (and call (stringp arguments))
      (setq call
            (plist-put call :arguments
                       (if append
                           (concat (or (plist-get call :arguments) "")
                                   arguments)
                         arguments)))
      (puthash output-index call table))))

(defun ende-codex--output-items (items)
  "Return ITEMS as a list from a Responses API JSON array."
  (cond
   ((and (vectorp items) (not (stringp items))) (append items nil))
   ((listp items) items)))

(defun ende-codex--diagnostic-tool-names (tools)
  "Return a safe summary of tool NAMES from Responses API TOOLS."
  (if tools
      (mapconcat (lambda (tool)
                   (or (alist-get :name tool) "<unknown>"))
                 tools ", ")
    "(none)"))

(defun ende-codex--diagnostic-output-items (items)
  "Return a safe summary of output-item TYPES in ITEMS."
  (let ((items (ende-codex--output-items items)))
    (if items
        (mapconcat
         (lambda (item)
           (let ((type (or (ende-protocol-json-get "type" item) "<unknown>"))
                 (name (ende-protocol-json-get "name" item)))
             (if (and (equal type "function_call") name)
                 (format "%s(%s)" type name)
               type)))
         items ", ")
      "(none)")))

(defun ende-codex--emit-diagnostic (session text)
  "Emit diagnostic TEXT for SESSION when diagnostics are enabled."
  (when ende-tool-diagnostics
    (ende-codex--emit session (list :type :diagnostic :text text))))

(defun ende-codex--diagnose-request (session body)
  "Record the tool names in Responses request BODY for SESSION."
  (ende-codex--emit-diagnostic
   session
   (format "OpenAI Responses tools sent: %s"
           (ende-codex--diagnostic-tool-names (alist-get :tools body)))))

(defun ende-codex--diagnose-output-item (session event-type item)
  "Record the safe summary of ITEM received in EVENT-TYPE."
  (ende-codex--emit-diagnostic
   session
   (format "OpenAI Responses %s: %s"
           event-type
           (ende-codex--diagnostic-output-items (list item)))))

(defun ende-codex--handle-event (session json)
  "Handle one parsed Responses API event JSON object."
  (let* ((type (ende-protocol-json-get "type" json))
         (response (ende-protocol-json-get "response" json))
         (output (ende-protocol-json-get "output" response))
         (item (ende-protocol-json-get "item" json))
         (output-index (ende-protocol-json-get "output_index" json))
         (metadata (or response json))
         (model (ende-protocol-json-get "model" metadata)))
    (when (equal type "response.output_item.done")
      (ende-codex--remember-output session item output-index)
      (when (equal (ende-protocol-json-get "type" item) "image_generation_call")
        (ende-stream-record-image
         (ende-codex--session-stream session)
         (ende-content-generated-image (ende-protocol-json-get "result" item)))))
    (when (member type '("response.completed" "response.done"))
      (cl-loop for entry in (ende-codex--output-items output) for index from 0
               do (ende-codex--remember-output session entry index))
      (when (cl-some (lambda (entry)
                      (equal (ende-protocol-json-get "type" entry) "image_generation_call"))
                    (ende-codex--output-items output))
        (setf (ende-stream-session-output-parts (ende-codex--session-stream session))
              (reverse
               (cl-mapcan
                (lambda (entry)
                  (pcase (ende-protocol-json-get "type" entry)
                    ("message" (ende-content-output-parts
                                (ende-protocol-json-get "content" entry) nil))
                    ("image_generation_call"
                     (list (ende-content-generated-image (ende-protocol-json-get "result" entry))))))
                (ende-codex--output-items output)))))
      (unless (ende-codex--session-terminal-event session)
        (ende-codex--emit-diagnostic
         session
         (format "OpenAI Responses output items: %s"
                 (ende-codex--diagnostic-output-items output))))
      (ende-codex--collect-tool-calls
       session output))
    (when (and item
               (member type '("response.output_item.added"
                              "response.output_item.done")))
      (ende-codex--collect-tool-call-item session item output-index)
      (ende-codex--diagnose-output-item session type item))
    (when (member type '("response.function_call_arguments.delta"
                         "response.function_call_arguments.done"))
      (let ((arguments (ende-protocol-json-get
                        (if (string-suffix-p ".delta" type) "delta"
                          "arguments")
                        json)))
        (when (and (string-suffix-p ".delta" type)
                   (stringp arguments) (> (length arguments) 0))
          (ende-stream-note-generation-start
           (ende-codex--session-stream session)))
        (ende-codex--update-tool-call-arguments
         session output-index arguments
         (string-suffix-p ".delta" type)))
      (when (string-suffix-p ".done" type)
        (ende-codex--emit-diagnostic
         session
         (format "OpenAI Responses %s at output index %s"
                 type (or output-index "<unknown>")))))
    (when (stringp model)
      (setf (ende-stream-session-actual-model
             (ende-codex--session-stream session)) model))
    (when (or (stringp model)
              (member type '("response.created" "response.completed")))
      (ende-codex--emit
       session
       (list :type :metadata
             :metadata (list :source :response
                             :model (ende-stream-session-actual-model
                                     (ende-codex--session-stream session))
                             :provider "OpenAI"))))
    (cond
     ((string= type "response.output_text.delta")
      (ende-codex--emit
       session '(:type :status :status "Generating"))
      (ende-codex--emit-delta
       session :content (ende-protocol-json-get "delta" json)))
     ((string= type "response.reasoning_summary_text.delta")
      (ende-codex--emit
       session '(:type :status :status "Thinking"))
      (ende-codex--emit-reasoning-summary
       session (ende-codex--reasoning-summary-key json)
       (ende-protocol-json-get "delta" json)))
     ((string= type "response.reasoning_summary_text.done")
      (ende-stream-complete-reasoning-summary
       (ende-codex--session-stream session)
       (ende-codex--reasoning-summary-key json)
       (ende-protocol-json-get "text" json)))
     ((member type '("response.completed" "response.done" "response.incomplete"))
      (ende-codex--handle-usage
       session (ende-protocol-json-get "usage" response))
      (setf (ende-stream-session-finish-reason
             (ende-codex--session-stream session))
            (if (equal type "response.incomplete") "incomplete" "stop"))
      (setf (ende-codex--session-terminal-event session) json))
     ((member type '("response.failed" "error"))
      (let ((error (or (ende-protocol-json-get "error" response)
                       (ende-protocol-json-get "error" json) json)))
        (ende-codex--fail
         session
         (ende-error-create
          :kind 'provider
          :message (ende-transport-safe-error-text
                    (format "%s"
                            (or (ende-protocol-json-get "message" error)
                                (ende-protocol-json-get "code" error)
                                "ChatGPT request failed")))
          :details (ende-content-redact error))))))))

(defun ende-codex--http-error (session &optional response-status)
  "Return a structured HTTP error for SESSION."
  (let* ((status (or response-status
                     (condition-case nil
                         (car (ende-stream-response-headers
                               (ende-codex--session-stream session)))
                       (error nil))))
         (preview (ende-stream-preview-text
                   (ende-codex--session-stream session)))
         (detail (ende-transport-error-detail preview)))
    (ende-error-create
     :kind 'http
     :message (format "HTTP status %s: %s"
                      (or status "unknown")
                      (or detail
                          (and (not (string-empty-p (string-trim preview)))
                               (string-trim preview))
                          "ChatGPT request failed")))))

(defun ende-codex--handle-sse-events (session events)
  "Decode Codex JSON EVENTS for SESSION, ignoring Chat Completions markers."
  (dolist (event events)
    (unless (ende-codex--terminal-p session)
      (when (eq (plist-get event :type) :json)
        (ende-codex--handle-event session (plist-get event :value))))))

(defun ende-codex--filter-error (session error _over-limit)
  "Settle SESSION for SSE parser ERROR."
  (ende-codex--fail
   session (ende-error-create :kind 'protocol
                              :message (error-message-string error))))

(defun ende-codex--filter (session _process chunk)
  "Consume curl output CHUNK for SESSION."
  (ende-stream-filter
   (ende-codex--session-stream session) chunk
   (apply-partially #'ende-codex--handle-sse-events session)
   (apply-partially #'ende-codex--fail session)
   (apply-partially #'ende-codex--filter-error session)))

(defun ende-codex--process-error (session process event idle-error)
  "Settle SESSION for failed PROCESS with sentinel EVENT or IDLE-ERROR."
  (ende-codex--fail
   session
   (or idle-error
       (ende-error-create
        :kind (if (= (process-exit-status process) 28) 'timeout 'transport)
        :message (format "curl exited with status %d (%s)"
                         (process-exit-status process)
                         (string-trim event))))))

(defun ende-codex--header-error (session error)
  "Settle SESSION for response-header parsing ERROR."
  (ende-codex--fail
   session (ende-error-create :kind 'protocol
                              :message (error-message-string error))))

(defun ende-codex--finish-response (session headers)
  "Validate response HEADERS and finalize Codex's terminal Responses event."
  (condition-case finish-error
      (let* ((status (car headers))
             (content-type (cdr headers))
             (content-type-error
              (ende-stream-content-type-error content-type 'if-present)))
        (cond
         ((not (ende-stream-http-success-p status))
          (ende-codex--fail session (ende-codex--http-error session status)))
         (content-type-error
          (ende-codex--fail session content-type-error))
         (t
          (ende-stream-dispatch-events
           (ende-codex--session-stream session)
           (ende-protocol-sse-flush
            (ende-stream-session-parser (ende-codex--session-stream session)))
           (apply-partially #'ende-codex--handle-sse-events session))
          (unless (ende-codex--terminal-p session)
            (if (and (ende-codex--session-terminal-event session)
                     (not (equal (ende-stream-session-finish-reason
                                  (ende-codex--session-stream session)) "incomplete"))
                     (or (not (ende-codex--session-strict-completion session))
                         (equal (ende-protocol-json-get
                                 "type" (ende-codex--session-terminal-event session))
                                "response.completed")))
                (ende-codex--settle
                 session 'success (ende-codex--result session))
              (ende-codex--fail
               session
               (ende-error-create
                :kind (if (equal (ende-stream-session-finish-reason
                                  (ende-codex--session-stream session)) "incomplete") 'incomplete 'protocol)
                :message "Responses generation is incomplete or missing its completion event")))))))
    ((error quit)
     (ende-codex--fail
      session
      (ende-error-create :kind 'protocol
                         :message (error-message-string finish-error))))))

(defun ende-codex--sentinel (session process event)
  "Handle terminal PROCESS EVENT for SESSION."
  (ende-stream-sentinel
   (ende-codex--session-stream session) process event
   (apply-partially #'ende-codex--process-error session)
   (apply-partially #'ende-codex--finish-response session)
   (apply-partially #'ende-codex--header-error session)))

(defun ende-codex--message-content (role content &optional profile)
  "Return CONTENT in the Responses shape required for ROLE."
  (ende-content-provider-content role content 'responses profile))

(defun ende-codex--request-messages (messages &optional profile)
  "Convert internal MESSAGES to Responses API input objects."
  (cl-mapcan
   (lambda (message)
     (if (and (consp message) (stringp (car message)))
         (list (list (cons :role (car message))
                     (cons :content
                           (ende-codex--message-content
                            (car message) (cdr message) profile))))
       (let ((history (plist-get message :protocol-history))
             (calls (plist-get message :tool-calls))
             (content (plist-get message :content))
             (call-id (plist-get message :tool-call-id)))
         (cond
          ((eq (car-safe history) 'openai-responses) (ende-provider-config-copy (cdr history)))
          (calls
           (append
            (when (and (stringp content) (not (string-empty-p content)))
              (list (list (cons :role "assistant")
                          (cons :content
                                (ende-codex--message-content
                                 "assistant" content profile)))))
            (mapcar (lambda (call)
                      (list (cons :type "function_call")
                            (cons :call_id (plist-get call :id))
                            (cons :name (plist-get call :name))
                            (cons :arguments (plist-get call :arguments))))
                    calls)))
          (call-id
           (list (list (cons :type "function_call_output")
                       (cons :call_id call-id)
                       (cons :output (plist-get message :content)))))
          (t
           (let ((role (plist-get message :role)))
             (list (list (cons :role role)
                         (cons :content
                               (ende-codex--message-content
                                role (plist-get message :content) profile))))))))))
   (ende-content-provider-messages messages)))

(defun ende-codex--tool-definitions (definitions)
  "Convert Chat Completions DEFINITIONS to Responses API tool definitions."
  (mapcar
   (lambda (definition)
     (let ((function (alist-get :function definition)))
       (list (cons :type "function")
             (cons :name (alist-get :name function))
             (cons :description (alist-get :description function))
             (cons :parameters (alist-get :parameters function)))))
   definitions))

(defun ende-codex--reasoning-summary-key (json)
  "Return the identity of a reasoning-summary part in JSON."
  (cons (or (ende-protocol-json-get "item_id" json)
            (ende-protocol-json-get "output_index" json))
        (or (ende-protocol-json-get "summary_index" json) 0)))

(defun ende-responses--request-body (messages profile)
  "Build a Responses API request body for MESSAGES and PROFILE."
  (let ((body
         (list (cons :model (plist-get profile :model))
               (cons :input (ende-codex--request-messages messages profile))
               (cons :store json-false)
               (cons :stream t))))
    (when (plist-get profile :tool-definitions)
      (setq body
            (append body
                    (list (cons :tools
                                (ende-codex--tool-definitions
                                 (plist-get profile :tool-definitions)))
                          (cons :tool_choice "auto")))))
    (let ((budget (plist-get profile :output-tokens)))
      (when budget
        (setq body (append body (list (cons :max_output_tokens budget))))))
    (ende-provider-config-encode-parameters
     (ende-controls-encode body (plist-get profile :controls) profile) profile)))

(defun ende-codex--request-body (messages profile)
  "Build the retained Codex request for MESSAGES and PROFILE."
  (let ((body (ende-responses--request-body messages profile)))
    (unless (or (eq (plist-get profile :thinking) 'off)
                (equal (plist-get profile :reasoning-effort) "none"))
      (setq body
            (ende-controls--set-path
             body '("reasoning" "summary") "auto")))
    body))

(defun ende-responses--start-prepared (session body _request-body record)
  "Start a public Responses SESSION with prepared BODY and debug RECORD."
  (let* ((profile (ende-codex--session-profile session))
         (request
          (ende-curl-start-request
           :profile profile :name "ende-responses"
           :command (lambda (headers config)
                      (ende-curl-command profile headers config
                                         (and (listp body) (plist-get body :file))))
           :filter (apply-partially #'ende-codex--filter session)
           :sentinel (apply-partially #'ende-codex--sentinel session))))
    (ende-transport-session-set-resources (ende-codex--lifecycle session) request)
    (setf (ende-codex--session-debug-record session) record)
    (ende-request-send (plist-get request :process) body)))

(defun ende-responses-default (messages callbacks profile)
  "Send MESSAGES using public Responses and PROFILE's explicit authentication."
  (let ((session (ende-codex--make-session
                  :profile profile :callbacks callbacks :strict-completion t
                  :tool-calls (make-hash-table :test #'eql))))
    (condition-case failure
        (progn
          (ende-transport-session-invoke
           (ende-codex--lifecycle session) :on-start
           (apply-partially #'ende-codex--cancel session))
          (unless (ende-codex--terminal-p session)
            (ende-request-prepare
             (ende-codex--lifecycle session) 'ende-codex #'ende-responses--request-body
             messages profile (list "openai-responses" (plist-get profile :url))
             (apply-partially #'ende-responses--start-prepared session))))
      ((error quit)
       (ende-codex--fail session (ende-error-create
                                 :kind 'startup :message (error-message-string failure)))))
    (ende-transport-session-process (ende-codex--lifecycle session))))

(defun ende-codex--start-prepared (session auth session-id body request-body record)
  "Start SESSION using AUTH, SESSION-ID and a prepared BODY."
  (let* ((default-directory (ende-curl-local-directory))
         (profile (ende-codex--session-profile session))
         (url (ende-codex--validated-generation-url
               (or (plist-get profile :url) ende-codex-endpoint)))
         (account-id (ende-codex--account-id auth))
         (residency (ende-codex--residency auth))
          (headers
           (append
            (list (concat "Authorization: Bearer "
                          (plist-get auth :access))
                  "originator: opencode"
                  (concat "session-id: " session-id))
            (when account-id
              (list (concat "ChatGPT-Account-Id: " account-id)))
           (when residency
             (list (concat "x-openai-internal-codex-residency: "
                           residency)))))
         (request
          (ende-curl-start-request
           :profile profile
           :url url
           :internal-headers headers
           :name (format "ende-codex-%s"
                         (or (plist-get profile :job-id) "request"))
           :command (lambda (headers-file config-file)
                      (ende-curl-command profile headers-file config-file
                                         (and (listp body) (plist-get body :file))))
           :filter (apply-partially #'ende-codex--filter session)
           :sentinel (apply-partially #'ende-codex--sentinel session))))
    (ende-transport-session-set-resources
     (ende-codex--lifecycle session) request)
    (ende-codex--diagnose-request session request-body)
    (setf (ende-codex--session-debug-record session)
          record)
    (ende-request-send
     (ende-transport-session-process (ende-codex--lifecycle session)) body)))

(defun ende-codex--start-generation (session messages auth)
  "Prepare and start a Codex generation for SESSION using AUTH."
  (let* ((profile (ende-codex--session-profile session))
         (session-id (or (plist-get profile :codex-session-id)
                         (ende-codex--new-session-id))))
    (ende-request-prepare
     (ende-codex--lifecycle session) 'ende-codex #'ende-codex--request-body
     messages profile (list "codex-responses" (plist-get profile :url) session-id)
     (apply-partially #'ende-codex--start-prepared session auth session-id))))

(defun ende-codex--auth-ready (session messages auth)
  "Start generation for SESSION when AUTH is valid, or fail it."
  (unless (ende-codex--terminal-p session)
    (if (ende-error-p auth)
        (ende-codex--fail session auth)
      (condition-case startup-error
          (ende-codex--start-generation session messages auth)
        ((error quit)
         (ende-codex--fail
          session
          (ende-error-create
           :kind 'startup
           :message (error-message-string startup-error))))))))

(defun ende-codex-default (messages callbacks &optional options)
  "Send MESSAGES through the ChatGPT/Codex endpoint using OpenCode auth."
  (let* ((profile
          (ende-plist-merge
           (list :url ende-codex-endpoint :model ende-codex-model)
           options))
         (session
          (ende-codex--make-session
           :profile profile :callbacks callbacks
           :tool-calls (make-hash-table :test #'eql))))
    (condition-case startup-error
        (progn
          (ende-validate-resource-options)
          (ende-codex--validated-generation-url
           (plist-get profile :url))
          (ende-transport-session-invoke
           (ende-codex--lifecycle session) :on-start
           (apply-partially #'ende-codex--cancel session))
          (unless (ende-codex--terminal-p session)
            (let ((cancel
                   (ende-codex--resolve-auth
                    (apply-partially #'ende-codex--auth-ready
                                     session messages))))
              (ende-transport-session-add-canceller
               (ende-codex--lifecycle session) cancel))))
      ((error quit)
       (ende-codex--fail
        session
        (ende-error-create
         :kind 'startup
         :message (error-message-string startup-error)))))
    (ende-transport-session-process
     (ende-codex--lifecycle session))))

(provide 'ende-codex)

;;; ende-codex.el ends here
