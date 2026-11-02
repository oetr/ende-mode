;;; ende-codex.el --- ChatGPT/Codex transport for ende -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools, llm

;;; Commentary:

;; Two isolated OAuth protocols sharing the Responses transport.

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
(require 'ende-codex-auth)
(require 'ende-codex-login)

(declare-function ende-provider-validate-profile "ende-provider" (profile))

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
  received-bytes finished lock lock-id timer verification (attempts 0))

(defvar ende-codex--refresh-flights (make-hash-table :test #'equal)
  "In-flight OAuth refreshes keyed by credential source and client identity.")

(defvar ende-codex--model-probe-processes nil
  "In-flight ChatGPT/Codex model discovery processes.")

(defvar ende-codex--usage-probe-processes nil
  "In-flight Codex OAuth quota discovery processes.")

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

(defun ende-codex--session-id-for-identity (identity &optional account)
  "Return an opaque session ID for stable IDENTITY, isolated by ACCOUNT."
  (secure-hash 'sha256 (prin1-to-string (list "ende-codex-session" account identity))))

(defun ende-codex--new-session-id (&optional account)
  "Return a fresh job-scoped session ID for ACCOUNT."
  (ende-codex--session-id-for-identity
   (format "job:%s:%s:%s" (emacs-pid) (float-time)
           (random most-positive-fixnum)) account))

(defun ende-codex--ensure-session-id (profile)
  "Return detached PROFILE with one account-bound session ID.
Retain an existing ID unless its recorded account has changed."
  (let* ((profile (ende-provider-config-copy profile))
         (account (plist-get profile :account)))
    (when (or (not (plist-get profile :codex-session-id))
              (and (plist-member profile :codex-session-account)
                   (not (eq account (plist-get profile :codex-session-account)))))
      (setq profile
            (plist-put profile :codex-session-id
                       (ende-codex--new-session-id account))))
    (plist-put profile :codex-session-account account)))

(defun ende-codex--validated-generation-url (profile)
  "Require PROFILE's endpoint to match its selected OAuth method."
  (let ((expected (ende-codex--auth-property (plist-get profile :auth-method) :responses-url)))
    (unless (equal (plist-get profile :url) expected)
      (error "Codex endpoint does not match selected OAuth method"))
    (ende-codex-auth--endpoint expected expected)))

(defun ende-codex--catalog-url (profile)
  "Return PROFILE's fixed model endpoint, rejecting mismatches."
  (let ((expected (ende-codex--auth-property (plist-get profile :auth-method) :models-url)))
    (unless (equal (plist-get profile :models-url) expected)
      (error "Codex catalog does not match selected OAuth method"))
    (ende-codex-auth--endpoint expected expected)))

(defun ende-codex--catalog-request-url (profile)
  "Build the trusted discovery URL for PROFILE with method-owned parameters."
  (let* ((method (plist-get profile :auth-method))
         (base (ende-codex--catalog-url profile))
         (parameters (ende-codex--auth-property method :models-query)))
    (if (not parameters)
        base
      (concat base "?"
              (ende-codex-auth--form
               (mapcar (lambda (key)
                         (cons key
                               (if (equal key "client_version")
                                   ende-client-version
                                 (error "Unsupported catalog query parameter"))))
                       parameters))))))

(defun ende-codex--usage-url (profile)
  "Return PROFILE's fixed OAuth usage endpoint, refusing other auth methods."
  (unless (eq (plist-get profile :auth-method) 'codex-oauth)
    (error "Codex quota is available only for codex-oauth"))
  (let ((url (ende-codex--auth-property 'codex-oauth :usage-url)))
    (ende-codex-auth--endpoint url url)))

(defun ende-codex--validated-refresh-url (method)
  "Return METHOD's fixed official OAuth token endpoint."
  (let ((url (ende-codex--auth-property method :token-url)))
    (ende-codex-auth--endpoint url url)))

(defun ende-codex--access-token (auth)
  "Return the access token from private AUTH, never from a request profile."
  (plist-get auth :access-token))

(defun ende-codex--auth-fresh-p (auth)
  "Return non-nil when AUTH can authenticate a request without refresh."
  (let ((expires (plist-get auth :expires-at))
        (earliest (plist-get auth :earliest-refresh-at))
        (now (floor (float-time))))
    (and (not (plist-get auth :needs-login))
         (ende-codex-auth--text-p (ende-codex--access-token auth))
         (integerp expires)
         (> expires now)
         (or (> expires (+ now 30))
             (and earliest (> earliest now))))))

(defun ende-codex--account-profile (profile)
  "Return a detached PROFILE bound exclusively to Ende-owned account storage."
  (ende-codex-auth--alias (plist-get profile :account))
  (plist-put (ende-provider-config-copy profile) :codex-auth-source 'ende))

(defun ende-codex--account-binding-state (auth)
  "Return private registration continuity for AUTH, without tokens."
  (list 'ende (expand-file-name ende-codex-auth--directory)
        (plist-get auth :auth-method) (plist-get auth :account) (plist-get auth :host-id)
        (plist-get auth :registration-id) (plist-get auth :client-id)
        (plist-get auth :subject) (plist-get auth :chatgpt-account-id)))

(defun ende-codex--profile-auth (profile)
  "Read the Ende-owned registration selected by PROFILE."
  (unless (eq (plist-get profile :codex-auth-source) 'ende)
    (error "Codex requires Ende-owned account storage"))
  (or (ende-codex-auth-lookup (plist-get profile :auth-method) (plist-get profile :account))
      (user-error "Codex account is not connected; use the connection selector")))

(defun ende-codex-capture-binding (profile)
  "Bind queued PROFILE to its account registration without copying tokens.
An inherited binding, including an unusable one, survives delegation."
  (if (plist-member profile :codex-binding) profile
    (let ((id (make-symbol "ende-codex-binding"))
          state)
      (condition-case nil
          (setq state (ende-codex--account-binding-state
                       (ende-codex--profile-auth profile)))
        (error nil))
      (puthash id state ende-credentials--bindings)
      (plist-put profile :codex-binding id))))

(defun ende-codex--check-binding (profile auth)
  "Require captured PROFILE continuity before using AUTH."
  (when (plist-member profile :codex-binding)
    (let ((captured (gethash (plist-get profile :codex-binding)
                             ende-credentials--bindings))
          (current (and (eq (plist-get profile :codex-auth-source) 'ende)
                        (eq (plist-get profile :account) (plist-get auth :account))
                        (eq (plist-get profile :auth-method) (plist-get auth :auth-method))
                        (ende-codex--account-binding-state auth))))
      (unless (and captured current (equal captured current))
        (error "Queued Codex credentials changed or cannot be verified; submit a new request")))))

(defun ende-codex--refresh-body (method auth)
  "Return METHOD's refresh form for private AUTH."
  (unless (eq method (plist-get auth :auth-method)) (error "OAuth method mismatch"))
  (ende-codex-auth--validate-method method auth)
  (when (equal (plist-get auth :client-id) "dynamic_agent_client")
    (error "An issued Codex client ID is required"))
  (ende-codex-auth--form
   (append `(("grant_type" . "refresh_token") ("client_id" . ,(plist-get auth :client-id))
             ("refresh_token" . ,(plist-get auth :refresh-token)))
           (when-let* ((resource (ende-codex--auth-property method :resource)))
             (list (cons "resource" resource))))))

(defun ende-codex--refresh-command (url headers-file)
  "Build the curl command for an OAuth refresh request to URL."
  (append
   (ende-curl-base-command url)
   (ende-curl-response-options headers-file)
   (list "--request" "POST"
         "--header" "Content-Type: application/x-www-form-urlencoded"
         "--header" "Accept: application/json"
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
      (when-let* ((timer (ende-codex--refresh-flight-timer flight))) (cancel-timer timer))
      (when-let* ((verification (ende-codex--refresh-flight-verification flight)))
        (setf (ende-codex--refresh-flight-verification flight) nil)
        (funcall verification))
      (when-let* ((lock (ende-codex--refresh-flight-lock flight)))
        (ende-ignore-errors
         (ende-codex-auth--delete-lock
          lock (ende-codex--refresh-flight-lock-id flight))))
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

(defun ende-codex--store-refresh (auth access refresh expires-in id-token &optional payload)
  "Rotate AUTH atomically using verified ID-TOKEN and its registration identity.
PAYLOAD supplies optional updated scopes and earliest refresh time."
  (let ((credentials (ende-codex-auth--credentials auth)))
    (dolist (key '(:needs-login :earliest-refresh-at)) (cl-remf credentials key))
    (setq credentials (ende-plist-merge
                       credentials (list :access-token access :refresh-token (or refresh (plist-get auth :refresh-token))
                                         :expires-at (+ (floor (float-time)) expires-in)
                                         :id-token (or id-token (plist-get auth :id-token)))))
    (when (ende-protocol-json-has-key-p "scope" payload)
      (setq credentials (plist-put credentials :scope (gethash "scope" payload))))
    (when (and payload (gethash "earliest_refresh_at" payload))
      (setq credentials (plist-put credentials :earliest-refresh-at (gethash "earliest_refresh_at" payload))))
    (ende-codex-auth--rotate auth credentials)))

(defun ende-codex--refresh-refusal ()
  "Return the error for refreshed credentials that could not be verified."
  (ende-error-create
   :kind 'auth
   :message "Codex refresh could not be verified; connect through the connection selector"))

(defun ende-codex--refresh-rotate (flight payload id-token)
  "Rotate FLIGHT's credentials from PAYLOAD carrying verified ID-TOKEN."
  (let ((auth (ende-codex--refresh-flight-auth flight)))
    (condition-case nil
        (ende-codex--refresh-finish
         flight
         (ende-codex--store-refresh
          auth (gethash "access_token" payload) (gethash "refresh_token" payload)
          (gethash "expires_in" payload) id-token payload))
      (ende-codex-auth-conflict
       (ende-codex--refresh-finish flight (ende-codex--refresh-current auth)))
      ((error quit)
       (ende-codex--refresh-finish flight (ende-codex--refresh-refusal))))))

(defun ende-codex--refresh-verified (flight payload id-token _claims failure)
  "Finish FLIGHT after verifying ID-TOKEN from PAYLOAD, or refuse FAILURE."
  (unless (ende-codex--refresh-flight-finished flight)
    (if failure
        (ende-codex--refresh-finish flight (ende-codex--refresh-refusal))
      (ende-codex--refresh-rotate flight payload id-token))))

(defun ende-codex--refresh-current (auth)
  "Return a fresh concurrent winner only for AUTH's retained registration."
  (condition-case nil
      (let ((current (ende-codex-auth-read (plist-get auth :auth-method) (plist-get auth :account))))
        (and (ende-codex--auth-fresh-p current)
             (ende-codex-auth--same-registration-p auth current) current))
    ((error quit) nil)))

(defun ende-codex--refresh-response (flight status payload)
  "Apply token PAYLOAD with HTTP STATUS to FLIGHT."
  (let* ((auth (ende-codex--refresh-flight-auth flight))
         (code (gethash "error" payload))
         (terminal (member code '("invalid_grant" "invalid_refresh_token" "token_expired"
                                  "refresh_token_expired" "refresh_token_invalidated" "refresh_token_reused"))))
    (if (equal status 200)
        (progn
          (ende-codex-auth--token-payload (plist-get auth :auth-method) payload t)
          (let ((id-token (gethash "id_token" payload)))
            (if id-token
                (setf (ende-codex--refresh-flight-verification flight)
                      (ende-codex-auth--verify-refresh
                       id-token auth
                       (apply-partially #'ende-codex--refresh-verified
                                        flight payload id-token)))
              (ende-codex--refresh-rotate flight payload nil))))
      (let ((winner (ende-codex--refresh-current auth)))
        (if winner (ende-codex--refresh-finish flight winner)
          (when terminal (ende-codex-auth--require-login auth))
          (ende-codex--refresh-finish
           flight (ende-error-create
                   :kind (if terminal 'auth 'transport)
                   :message (if terminal "Codex requires sign-in; connect through the connection selector"
                              "OpenAI refresh was rejected; credentials retained")
                   :details (list :status status
                                  :code (and (stringp code) (<= (length code) 100)
                                             (string-match-p "\\`[a-z_]+\\'" code) code)))))))))

(defun ende-codex--refresh-sentinel (flight process _event)
  "Handle terminal PROCESS for refresh FLIGHT without leaking token responses."
  (when (and (not (ende-codex--refresh-flight-finished flight))
             (memq (process-status process) '(exit signal closed)))
    (let ((debug-on-error nil) (debug-on-quit nil) (debug-on-signal nil)
          (auth (ende-codex--refresh-flight-auth flight)))
      (condition-case failure
          (if (/= (process-exit-status process) 0)
              (ende-codex--refresh-finish
               flight (or (process-get process :ende-idle-error)
                          (ende-error-create :kind 'transport :message "OpenAI token refresh failed; credentials retained")))
            (ende-codex--refresh-response
             flight (car (ende-curl-response-headers (ende-codex--refresh-flight-headers-file flight)))
             (with-current-buffer (ende-codex--refresh-flight-response-buffer flight)
               (ende-codex-auth--json (buffer-string)))))
        ((error quit)
         (ende-codex--refresh-finish
          flight (or (and (eq (car failure) 'ende-codex-auth-conflict) (ende-codex--refresh-current auth))
                     (ende-codex--refresh-refusal))))))))

(defun ende-codex--start-refresh-flight (flight)
  "Start the process belonging to refresh FLIGHT."
  (let ((debug-on-error nil) (debug-on-quit nil) (debug-on-signal nil)
        (response-buffer nil)
        (stderr-buffer nil)
        (stderr-process nil)
        (headers-file nil)
        (process nil))
    (condition-case nil
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
          (let ((url (ende-codex--validated-refresh-url
                      (plist-get (ende-codex--refresh-flight-auth flight) :auth-method))))
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
             (let ((auth (ende-codex--refresh-flight-auth flight)))
               (ende-codex--refresh-body (plist-get auth :auth-method) auth)))
            (process-send-eof process)))
      ((error quit)
       (ende-codex--refresh-finish
        flight
        (ende-error-create
         :kind 'auth
         :message "Codex credential refresh could not be started"))))))

(defun ende-codex--refresh-key (auth)
  "Return the private coalescing key for AUTH's exact account/registration."
  (append (ende-codex--account-binding-state auth) (list (plist-get auth :revision))))

(defun ende-codex--lock-refresh (flight)
  "Serialize rotating token use across Emacs instances before starting FLIGHT."
  (unless (ende-codex--refresh-flight-finished flight)
    (condition-case nil
        (let* ((auth (ende-codex--refresh-flight-auth flight))
               (directory (ende-codex-auth--path nil))
               (lock (expand-file-name
                      (concat ".refresh-" (symbol-name (plist-get auth :auth-method))
                              "-" (symbol-name (plist-get auth :account))) directory)))
          (condition-case nil
              (setf (ende-codex--refresh-flight-lock-id flight)
                    (ende-codex-auth--make-lock lock)
                    (ende-codex--refresh-flight-lock flight) lock)
            (file-already-exists (ende-codex-auth--attributes lock t t)))
          (let ((current (ende-codex-auth-read (plist-get auth :auth-method) (plist-get auth :account))))
            (unless (ende-codex-auth--same-registration-p auth current)
              (signal 'ende-codex-auth-conflict nil))
            (cond
             ((ende-codex--auth-fresh-p current) (ende-codex--refresh-finish flight current))
             ((not (equal (plist-get auth :revision) (plist-get current :revision)))
              (error "Refusing to reuse changed refresh credentials"))
             ((ende-codex--refresh-flight-lock flight) (ende-codex--start-refresh-flight flight))
             ((< (cl-incf (ende-codex--refresh-flight-attempts flight)) 40)
              (setf (ende-codex--refresh-flight-timer flight)
                    (run-at-time 0.5 nil #'ende-codex--lock-refresh flight)))
             (t (error "Another instance is refreshing this account")))))
      ((error quit)
       (ende-codex--refresh-finish
        flight (ende-error-create :kind 'auth
                                  :message "Codex refresh is locked or credentials changed; retry or connect through the connection selector"))))))

(defun ende-codex--refresh-auth (auth callback)
  "Coalesce a refresh for AUTH and register CALLBACK as a waiter.
Return a cancellation function that only removes this callback."
  (let* ((default-directory (ende-curl-local-directory))
         (key (ende-codex--refresh-key auth))
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
      (ende-codex--lock-refresh flight))
    (apply-partially #'ende-codex--refresh-cancel-waiter flight waiter)))

(defun ende-codex--scopes (auth)
  "Return AUTH's granted scope tokens."
  (let ((scope (plist-get auth :scope)))
    (and scope (ende-codex-auth--scopes scope))))

(defun ende-codex--require-permission (method auth)
  "Require METHOD's renewable inference permission and exact AUTH binding."
  (unless (eq method (plist-get auth :auth-method)) (error "OAuth method mismatch"))
  (ende-codex-auth--validate-method method auth)
  (when (plist-get auth :needs-login)
    (user-error "Codex account requires sign-in through the connection selector"))
  (let ((scopes (ende-codex--scopes auth)))
    (unless (member "offline_access" scopes)
      (user-error "Renewable OAuth access was not authorized"))
    (when (ende-codex--auth-property method :resource)
      (unless (member "chatgpt.tokens.use.direct" scopes)
        (user-error "ChatGPT-plan inference was not authorized")))))

(defun ende-codex--resolved-account (profile callback auth)
  "Check retained PROFILE again before delivering private AUTH to CALLBACK."
  (unless (ende-error-p auth)
    (setq auth (condition-case nil
                   (let ((current (ende-codex--profile-auth profile)))
                     (unless (ende-codex-auth--same-registration-p auth current)
                       (error "Codex registration changed"))
                     (ende-codex--check-binding profile current)
                     (ende-codex--require-permission (plist-get profile :auth-method) current)
                     current)
                 ((error quit)
                  (ende-error-create :kind 'auth :message "Queued Codex registration changed or cannot be verified")))))
  (funcall callback auth))

(defun ende-codex--resolve-auth (callback profile)
  "Resolve PROFILE's exact Ende account and deliver private AUTH to CALLBACK."
  (let ((debug-on-error nil) (debug-on-quit nil) (debug-on-signal nil))
    (condition-case failure
        (let* ((profile (ende-codex-capture-binding profile))
               (auth (ende-codex--profile-auth profile))
               (deliver (apply-partially #'ende-codex--resolved-account profile callback)))
          (ende-codex--check-binding profile auth)
          (ende-codex--require-permission (plist-get profile :auth-method) auth)
          (if (ende-codex--auth-fresh-p auth)
              (progn (funcall deliver auth) nil)
            (ende-codex--refresh-auth auth deliver)))
      ((error quit)
       (funcall callback
                (ende-error-create
                 :kind 'auth
                 :message (if (eq (car failure) 'user-error)
                              (error-message-string failure)
                            "Codex account is unavailable or changed; connect through the connection selector")))
       nil))))

;;;; Model discovery

(defun ende-codex--catalog-command (url headers-file config-file)
  "Build the curl GET command for the account model catalog at URL."
  (append
   (ende-curl-base-command url)
   (ende-curl-response-options headers-file)
   (list "--request" "GET" "--header" "Accept: application/json")
   (when config-file (list "--config" config-file))
   (ende-curl-timeout-options ende-request-timeout
                              ende-provider-max-discovery-bytes)
   (list "--url" url)))

(defun ende-codex--usage-command (url headers-file config-file)
  "Build the curl GET command for Codex usage URL."
  (append
   (ende-curl-base-command url)
   (ende-curl-response-options headers-file)
   (list "--request" "GET" "--header" "Accept: application/json")
   (when config-file (list "--config" config-file))
   (ende-curl-timeout-options ende-request-timeout
                              ende-provider-max-discovery-bytes)
   (list "--url" url)))

(defun ende-codex--catalog-efforts (entry)
  "Return advertised reasoning EFFORTS and DEFAULT from catalog ENTRY."
  (let ((levels (ende-protocol-json-get "supported_reasoning_levels" entry))
        (seen nil) (efforts nil))
    (dolist (level (cond ((vectorp levels) (append levels nil))
                         ((listp levels) levels)))
      (let ((effort (ende-protocol-json-get "effort" level)))
        (when (and (stringp effort) (not (string-empty-p effort))
                   (not (member effort seen)))
          (push effort seen)
          (push effort efforts))))
    (setq efforts (nreverse efforts))
    (let ((default (ende-protocol-json-get "default_reasoning_level" entry)))
      (list :efforts efforts
            :default (and (stringp default) (member default efforts) default)))))

(defun ende-codex--catalog-entry (entry)
  "Return normalized listed model metadata from catalog ENTRY, or nil."
  (when (and (hash-table-p entry)
             (equal (ende-protocol-json-get "visibility" entry) "list"))
    (let ((slug (ende-protocol-json-get "slug" entry))
          (name (ende-protocol-json-get "display_name" entry)))
      (when (and (stringp slug) (not (string-empty-p slug)))
        (let ((options (ende-codex--catalog-efforts entry)))
          (list :id slug
                :name (and (stringp name) (not (string-empty-p name)) name)
                :efforts (plist-get options :efforts)
                :default (plist-get options :default)))))))

(defun ende-codex--parse-catalog (text method)
  "Decode a recognized model catalog schema for METHOD from TEXT."
  (let* ((payload (ende-protocol-json-read-string text))
         (public (eq (ende-codex--auth-property method :catalog-schema) 'public))
         (data (ende-protocol-json-get "data" payload))
         (models (ende-protocol-json-get "models" payload)))
    (cond
     ((and public (vectorp data))
      (unless (cl-every (lambda (entry)
                          (and (hash-table-p entry)
                               (ende-codex-auth--text-p (gethash "id" entry)))) data)
        (error "Public model catalog data is missing valid model IDs"))
      (mapcar (lambda (entry) (list :id (gethash "id" entry)))
              (append data nil)))
     ;; Some ChatGPT-plan catalog responses use the documented slug/visibility
     ;; shape even when reached through the public models URL. Accept it only
     ;; when every entry has that recognized shape; do not guess field names.
     ((and (vectorp models)
           (cl-every (lambda (entry)
                       (and (hash-table-p entry)
                            (ende-codex-auth--text-p (gethash "slug" entry))
                            (stringp (gethash "visibility" entry)))) models))
      (delq nil (mapcar #'ende-codex--catalog-entry (append models nil))))
     (t
      (error (if public
                 "Unrecognized public model catalog schema (expected data IDs or models slugs)"
               "Unrecognized native model catalog schema"))))))

(defun ende-codex--usage-window (entry)
  "Normalize one quota WINDOW JSON object, or return nil."
  (when (hash-table-p entry)
    (let ((used (ende-protocol-json-get "used_percent" entry))
          (reset (ende-protocol-json-get "reset_at" entry))
          (duration (ende-protocol-json-get "limit_window_seconds" entry)))
      (when (numberp used)
        (list :used-percent (max 0 (min 100 used))
              :reset-at (and (numberp reset) reset)
              :window-seconds (and (numberp duration) (> duration 0) duration))))))

(defun ende-codex--parse-usage (text)
  "Parse TEXT into primary and secondary quota windows."
  (let* ((payload (ende-protocol-json-read-string text))
         (rate (ende-protocol-json-get "rate_limit" payload))
         (primary (and (hash-table-p rate)
                       (ende-codex--usage-window
                        (ende-protocol-json-get "primary_window" rate))))
         (secondary (and (hash-table-p rate)
                         (ende-codex--usage-window
                          (ende-protocol-json-get "secondary_window" rate)))))
    (unless (or primary secondary)
      (error "Usage response has no quota windows"))
    (list :primary primary :secondary secondary)))

(defun ende-codex--usage-duration (seconds)
  "Format SECONDS as a compact duration."
  (let* ((seconds (max 0 (floor seconds)))
         (days (/ seconds 86400))
         (hours (/ (% seconds 86400) 3600))
         (minutes (/ (% seconds 3600) 60)))
    (cond ((> days 0) (format "%dd %dh" days hours))
          ((> hours 0) (format "%dh%s" hours
                              (if (> minutes 0) (format " %dm" minutes) "")))
          ((> minutes 0) (format "%dm" minutes))
          (t "now"))))

(defun ende-codex-usage-string (quota &optional now)
  "Format normalized Codex OAuth QUOTA for the selector."
  (let ((now (or now (float-time))) segments reset-at)
    (dolist (key '(:primary :secondary))
      (let ((window (plist-get quota key)))
        (when window
          (let ((seconds (if (and (eq key :primary)
                                  (numberp (plist-get window :reset-at)))
                             (- (plist-get window :reset-at) now)
                           (plist-get window :window-seconds))))
            (push (format "%s: %d%% left"
                          (cond ((and (numberp seconds) (>= seconds (* 7 86400))) "weekly")
                                ((numberp seconds) (ende-codex--usage-duration seconds))
                                (t "quota"))
                          (round (- 100 (plist-get window :used-percent))))
                  segments))
          (when (numberp (plist-get window :reset-at))
            (setq reset-at (if reset-at
                               (min reset-at (plist-get window :reset-at))
                             (plist-get window :reset-at)))))))
    (setq segments (nreverse segments))
    (when reset-at
      (setq segments
            (append segments
                    (list (format "reset in %s"
                                  (ende-codex--usage-duration (- reset-at now)))))))
    (mapconcat #'identity segments " · ")))

(defun ende-codex--probe-catalog
    (profile callback &optional request-url command parser request-name process-slot usage-p)
  "Probe PROFILE and call CALLBACK with parsed account-bound JSON.
CALLBACK receives (VALUE ERROR).  Return a cancellation function.  Optional
REQUEST-URL, COMMAND, PARSER, REQUEST-NAME, PROCESS-SLOT and USAGE-P customize
the request while preserving the shared Codex OAuth lifecycle."
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
        ((finish (value &optional error cancelled)
           (unless finished
             (setq finished t)
             (if process-slot
                 (set process-slot (delq process (symbol-value process-slot)))
               (setq ende-codex--model-probe-processes
                     (delq process ende-codex--model-probe-processes)))
             (when (and process (process-live-p process))
               (delete-process process))
             (ende-curl-cleanup-resources
              (list response-buffer stderr-buffer)
              (list headers-file config-file)
              (list stderr-process))
             (unless cancelled
               (funcall callback value error))))
         (start (auth)
           (let* ((default-directory (ende-curl-local-directory))
                  (url (or request-url (ende-codex--catalog-request-url profile)))
                  (request
                   (ende-curl-start-request
                    :profile (list :url (if usage-p url (ende-codex--catalog-url profile)))
                    :name (or request-name "ende-codex-model-probe")
                    :internal-headers
                    (if usage-p
                        (ende-codex--usage-headers profile auth)
                      (ende-codex--inference-headers
                       (plist-get profile :auth-method) auth profile))
                    :command (lambda (headers config)
                               (funcall (or command #'ende-codex--catalog-command)
                                        url headers config))
                    :filter
                    (lambda (_probe-process chunk)
                      (unless finished
                        (cl-incf received-bytes (string-bytes chunk))
                        (if (and ende-provider-max-discovery-bytes
                                 (> received-bytes
                                    ende-provider-max-discovery-bytes))
                            (finish
                             nil
                             (ende-error-create
                              :kind 'resource
                              :message
                              "Codex model response exceeds ende-provider-max-discovery-bytes"))
                          (with-current-buffer response-buffer
                            (goto-char (point-max))
                            (insert chunk)))))
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
                                  (format "Codex model discovery failed: %s"
                                          (string-trim event)))))
                          (condition-case parse-error
                              (let* ((status
                                      (car (ende-curl-response-headers
                                            headers-file)))
                                     (text
                                      (with-current-buffer response-buffer
                                        (buffer-string))))
                                (if (equal status 200)
                                    (finish (if parser
                                              (funcall parser text)
                                            (ende-codex--parse-catalog
                                             text (plist-get profile :auth-method))))
                                  (finish
                                   nil
                                   (ende-error-create
                                    :kind 'http
                                    :message
                                    (format "HTTP status %s: %s"
                                            (or status "unknown")
                                            (if usage-p "Codex usage request failed"
                                              (or (ende-transport-error-detail text)
                                                  "Codex model discovery failed")))))))
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
             (if process-slot
                 (set process-slot (cons process (symbol-value process-slot)))
               (push process ende-codex--model-probe-processes)))))
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
                              (error-message-string request-error))))))))
                   profile)))
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

(defun ende-codex-probe-models (profile callback)
  "Probe PROFILE's ChatGPT account catalog and call CALLBACK with model details.
CALLBACK receives (MODELS ERROR), where each model is an `:id'/`:name' plist."
  (ende-codex--probe-catalog
   profile
   (lambda (entries error)
     (funcall callback
              (and entries
                   (mapcar (lambda (entry)
                             (list :id (plist-get entry :id)
                                   :name (plist-get entry :name)))
                           entries))
              error))))

(defun ende-codex-probe-reasoning-efforts (profile model callback)
  "Query PROFILE's account for MODEL's advertised reasoning efforts.
CALLBACK receives (EFFORTS DEFAULT ERROR).  A model that advertises no
explicit efforts yields no choices instead of an error."
  (ende-codex--probe-catalog
   profile
   (lambda (entries error)
     (if error
         (funcall callback nil nil error)
       (let ((entry (cl-find model entries
                             :key (lambda (value) (plist-get value :id))
                             :test #'equal)))
         (if entry
             (funcall callback (plist-get entry :efforts)
                      (plist-get entry :default) nil)
           (funcall callback nil nil
                    (ende-error-create
                     :kind 'capability
                     :message
                     (format "ChatGPT does not advertise model %s" model)))))))))

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
             (member (ende-protocol-json-get "type" item)
                     '("reasoning" "message" "function_call")))
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
             :namespace (or (ende-protocol-json-get "namespace" item)
                            (plist-get old :namespace))
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
    (ende-debug-record-response (ende-codex--session-debug-record session) response)
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
                             :service-tier (ende-protocol-json-get "service_tier" metadata)
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
                    (format "%s" (ende-codex--error-message error)))
          :details (ende-content-redact error))))))))

(defconst ende-codex--error-hints
  '(("subscription_sharing_user_not_eligible" .
     "ChatGPT-plan use is unavailable for this account")
    ("subscription_sharing_usage_limit_exceeded" .
     "ChatGPT-plan usage limit reached")
    ("subscription_sharing_usage_unavailable" .
     "ChatGPT-plan usage is temporarily unavailable; retry later")
    ("subscription_sharing_unsupported_capability" .
     "ChatGPT-plan does not support this request")
    ("subscription_sharing_invalid_user" .
     "ChatGPT authorization is no longer valid; connect through the connection selector")
    ("chatpass_v2_scope_not_authorized" .
     "ChatGPT-plan permission is insufficient; connect through the connection selector")
    ("chatpass_v2_invalid_authorization_context" .
     "ChatGPT authorization context is invalid; connect through the connection selector"))
  "Known ChatGPT-plan error codes and their safe descriptions.")

(defun ende-codex--error-message (error)
  "Return a safe human-readable message for provider ERROR metadata."
  (let ((code (ende-protocol-json-get "code" error)))
    (or (cdr (assoc code ende-codex--error-hints))
        (ende-protocol-json-get "message" error)
        code
        "ChatGPT request failed")))

(defun ende-codex--error-payload (session)
  "Return safe provider error metadata parsed from SESSION's response."
  (condition-case nil
      (let* ((preview (ende-stream-preview-text
                       (ende-codex--session-stream session)))
             (payload (ende-protocol-json-read-string
                       (ende-transport-safe-error-text preview)))
             (error (or (ende-protocol-json-get "error" payload) payload)))
        (when (hash-table-p error)
          (list :code (ende-protocol-json-get "code" error)
                :param (ende-protocol-json-get "param" error)
                :request-id (ende-protocol-json-get "request_id" payload))))
    (error nil)))

(defun ende-codex--http-error (session &optional response-status)
  "Return a structured, token-free HTTP error for SESSION."
  (let* ((status (or response-status
                     (condition-case nil
                         (car (ende-stream-response-headers
                               (ende-codex--session-stream session)))
                       (error nil))))
         (payload (ende-codex--error-payload session))
         (code (plist-get payload :code))
         (preview (ende-stream-preview-text
                   (ende-codex--session-stream session)))
         (detail (ende-transport-error-detail preview)))
    (ende-error-create
     :kind 'http
     :message
     (ende-transport-safe-error-text
      (format "HTTP status %s%s: %s"
              (or status "unknown")
              (if (and (stringp code) (not (string-empty-p code)))
                  (format " %s" code) "")
              (or (and (stringp code)
                       (cdr (assoc code ende-codex--error-hints)))
                  detail
                  (and (not (string-empty-p (string-trim preview)))
                       (string-trim preview))
                  "ChatGPT request failed")))
     :details (list :status status :code code
                    :param (plist-get payload :param)
                    :request-id (plist-get payload :request-id)))))

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

(defun ende-codex--message-role (role)
  "Return the Responses role for internal ROLE."
  (if (equal role "system") "developer" role))

(defun ende-codex--function-call-item (call)
  "Return a replayed Responses function_call item for CALL."
  (append
   (list (cons :type "function_call")
         (cons :call_id (plist-get call :id)))
   (when (plist-get call :namespace)
     (list (cons :namespace (plist-get call :namespace))))
   (list (cons :name (plist-get call :name))
         (cons :arguments (plist-get call :arguments)))))

(defun ende-codex--request-messages (messages &optional profile)
  "Convert internal MESSAGES to Responses API input objects."
  (cl-mapcan
   (lambda (message)
     (if (and (consp message) (stringp (car message)))
         (let ((role (ende-codex--message-role (car message))))
           (list (list (cons :role role)
                       (cons :content
                             (ende-codex--message-content
                              role (cdr message) profile)))))
       (let ((history (plist-get message :protocol-history))
             (calls (plist-get message :tool-calls))
             (content (plist-get message :content))
             (call-id (plist-get message :tool-call-id)))
         (cond
          ((eq (car-safe history) 'openai-responses)
           (append (ende-provider-config-copy (cdr history))
                   (mapcar #'ende-codex--function-call-item
                           (cl-remove-if
                            (lambda (call)
                              (cl-some (lambda (item)
                                         (equal (ende-protocol-json-get "call_id" item)
                                                (plist-get call :id))) (cdr history))) calls))))
          (calls
           (append
            (when (and (stringp content) (not (string-empty-p content)))
              (list (list (cons :role "assistant")
                          (cons :content
                                (ende-codex--message-content
                                 "assistant" content profile)))))
            (mapcar #'ende-codex--function-call-item calls)))
          (call-id
           (list (list (cons :type "function_call_output")
                       (cons :call_id call-id)
                       (cons :output (plist-get message :content)))))
          (t
           (let ((role (ende-codex--message-role (plist-get message :role))))
             (list (list (cons :role role)
                         (cons :content
                               (ende-codex--message-content
                                role (plist-get message :content) profile))))))))))
   (ende-content-provider-messages messages)))

(defun ende-codex--reasoning-summary-key (json)
  "Return the identity of a reasoning-summary part in JSON."
  (cons (or (ende-protocol-json-get "item_id" json)
            (ende-protocol-json-get "output_index" json))
        (or (ende-protocol-json-get "summary_index" json) 0)))

(defun ende-responses--tool-definitions (definitions)
  "Convert Chat Completions DEFINITIONS to flat Responses API tools."
  (mapcar
   (lambda (definition)
     (let ((function (alist-get :function definition)))
       (list (cons :type "function")
             (cons :name (alist-get :name function))
             (cons :description (alist-get :description function))
             (cons :parameters (alist-get :parameters function)))))
   definitions))

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
                                (ende-responses--tool-definitions
                                 (plist-get profile :tool-definitions)))
                          (cons :tool_choice "auto")))))
    (let ((budget (plist-get profile :output-tokens)))
      (when budget
        (setq body (append body (list (cons :max_output_tokens budget))))))
    (ende-provider-config-encode-parameters
     (ende-controls-encode body (plist-get profile :controls) profile) profile)))

(defconst ende-codex--forbidden-body-fields
  '("background" "conversation" "previous_response_id" "max_output_tokens" "max_tool_calls" "metadata"
    "moderation" "multi_agent" "prompt" "prompt_cache_retention"
    "safety_identifier" "temperature" "top_logprobs" "top_p" "truncation"
    "user")
  "Request fields the ChatGPT-plan Responses API does not accept.")

(defun ende-codex--json-name (key)
  "Return the JSON member name for plist or alist KEY."
  (if (symbolp key) (substring (symbol-name key) 1) (format "%s" key)))

(defun ende-codex--validate-body (body)
  "Reject unsupported ChatGPT-plan request fields in BODY."
  (dolist (entry body)
    (when (member (ende-codex--json-name (car entry))
                  ende-codex--forbidden-body-fields)
      (error "ChatGPT-plan requests do not support %s; remove it from the profile"
             (ende-codex--json-name (car entry)))))
  body)

(defun ende-codex--tool-definitions (definitions)
  "Wrap Chat Completions DEFINITIONS in Ende's local tool namespace."
  (when definitions
    (list (list (cons :type "namespace")
                (cons :name "ende")
                (cons :description "Local coding and agent tools")
                (cons :tools
                      (ende-responses--tool-definitions definitions))))))

(defun ende-codex--request-body (messages profile)
  "Build the shared Codex OAuth Responses request for MESSAGES and PROFILE."
  (let ((provenance (plist-get profile :provenance)))
    (dolist (key '(:temperature :output-tokens))
      (when (plist-get provenance key)
        (error "ChatGPT-plan requests do not support %s; remove it from this profile"
               (ende-codex--json-name key)))))
  (let* ((body (list (cons :model (plist-get profile :model))
                     (cons :input
                           (ende-codex--request-messages messages profile))
                     (cons :include ["reasoning.encrypted_content"])
                     (cons :store json-false)
                     (cons :stream t)))
         (tools (ende-codex--tool-definitions
                 (plist-get profile :tool-definitions))))
    (when tools
      (setq body
            (append body
                    (list (cons :tools tools)
                          (cons :tool_choice "auto")))))
    (unless (or (eq (plist-get profile :thinking) 'off)
                (equal (plist-get profile :reasoning-effort) "none"))
      (setq body
            (ende-controls--set-path body '("reasoning" "summary") "auto")))
    (setq body
          (ende-provider-config-encode-parameters
           (ende-controls-encode body (plist-get profile :controls) profile)
           profile))
    (let ((include (cdr (cl-find "include" body
                                :key (lambda (entry) (ende-controls--json-key-name (car entry)))
                                :test #'equal))))
      (unless (and (vectorp include) (not (stringp include))
                   (cl-every #'stringp include))
        (error "ChatGPT-plan include must be an array of field names"))
      (setq body (ende-controls--set-path
                  body '("include")
                  (vconcat (delete-dups (append include '("reasoning.encrypted_content")))))))
    (ende-codex--validate-body body)))

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

(defun ende-codex--usage-headers (profile auth)
  "Build usage headers only for PROFILE's exact native Codex registration."
  (unless (and (eq (plist-get profile :service) 'openai-codex)
               (eq (plist-get profile :auth-method) 'codex-oauth)
               (eq (plist-get auth :auth-method) 'codex-oauth)
               (eq (plist-get profile :account) (plist-get auth :account)))
    (error "Codex quota requires its exact codex-oauth account"))
  (ende-codex--require-permission 'codex-oauth auth)
  (list (concat "Authorization: Bearer " (ende-codex--access-token auth))
        "originator: opencode"
        (concat "ChatGPT-Account-Id: " (plist-get auth :chatgpt-account-id))))

(defun ende-codex-probe-usage (profile callback)
  "Probe PROFILE's Codex OAuth quota and call CALLBACK with (QUOTA ERROR).
Return a cancellation function.  ChatGPT-plan OAuth is deliberately refused."
  (condition-case failure
      (progn
        (unless (and (eq (plist-get profile :service) 'openai-codex)
                     (eq (plist-get profile :auth-method) 'codex-oauth))
          (error "Codex quota is available only for codex-oauth"))
        (ende-codex--probe-catalog
         profile callback
         (ende-codex--usage-url profile)
         #'ende-codex--usage-command
         #'ende-codex--parse-usage
         "ende-codex-usage-probe"
         'ende-codex--usage-probe-processes t))
    ((error quit)
     (funcall callback nil
              (ende-error-create :kind 'startup
                                 :message (error-message-string failure)))
     nil)))

(defun ende-codex--inference-headers (method auth profile)
  "Build headers only for the exact METHOD, AUTH and PROFILE connection."
  (unless (and (eq method (plist-get profile :auth-method))
               (eq method (plist-get auth :auth-method))
               (eq (plist-get profile :account) (plist-get auth :account)))
    (error "OAuth connection binding mismatch"))
  (ende-codex--require-permission method auth)
  (let ((headers (list (concat "Authorization: Bearer " (ende-codex--access-token auth)))))
    (when-let* ((session (plist-get profile :codex-session-id)))
      (push (concat "session-id: " session) headers))
    (when-let* ((header (ende-codex--auth-property method :account-header)))
      (push (concat header ": " (plist-get auth :chatgpt-account-id)) headers))
    headers))

(defun ende-codex--start-prepared (session auth data body record)
  "Start SESSION with AUTH, prepared DATA, BODY and debug RECORD."
  (let* ((default-directory (ende-curl-local-directory))
         (profile (ende-codex--session-profile session))
         (_ (let ((case-fold-search t))
              (when (cl-some (lambda (header)
                              (string-match-p "\\`session-id:" header))
                            (plist-get profile :request-headers))
                (error "Codex session-id is transport-owned; remove the profile header"))))
         (url (ende-codex--validated-generation-url profile))
         (request
          (ende-curl-start-request
           :profile profile
           :url url
           :internal-headers
           (ende-codex--inference-headers (plist-get profile :auth-method) auth profile)
           :name (format "ende-codex-%s"
                         (or (plist-get profile :job-id) "request"))
           :command (lambda (headers config)
                      (ende-curl-command profile headers config
                                         (and (listp data)
                                              (plist-get data :file))))
           :filter (apply-partially #'ende-codex--filter session)
           :sentinel (apply-partially #'ende-codex--sentinel session))))
    (ende-transport-session-set-resources
     (ende-codex--lifecycle session) request)
    (ende-codex--diagnose-request session body)
    (setf (ende-codex--session-debug-record session) record)
    (ende-request-send
     (ende-transport-session-process (ende-codex--lifecycle session)) data)))

(defun ende-codex--start-generation (session messages auth)
  "Prepare the shared OAuth Responses generation for SESSION using AUTH."
  (ende-request-prepare
   (ende-codex--lifecycle session) 'ende-codex #'ende-codex--request-body
   messages (ende-codex--session-profile session)
   (list "openai-responses" (ende-codex--validated-generation-url (ende-codex--session-profile session)))
   (apply-partially #'ende-codex--start-prepared session auth)))

(defun ende-codex--auth-ready (session messages auth)
  "Start generation for SESSION when AUTH is valid, or fail it."
  (unless (ende-codex--terminal-p session)
    (if (ende-error-p auth)
        (ende-codex--fail session auth)
      (condition-case startup-error
          (progn
            (ende-codex--check-binding
             (ende-codex--session-profile session) auth)
            (ende-codex--require-permission (plist-get auth :auth-method) auth)
            (ende-codex--start-generation session messages auth))
        ((error quit)
         (ende-codex--fail
          session
          (ende-error-create
           :kind 'startup
           :message (error-message-string startup-error))))))))

(defun ende-codex-default (messages callbacks &optional options)
  "Send MESSAGES through the official Responses API for an Ende account."
  (let* ((profile (ende-codex--ensure-session-id options))
         (session
          (ende-codex--make-session
           :profile profile :callbacks callbacks :strict-completion t
           :tool-calls (make-hash-table :test #'eql))))
    (condition-case startup-error
        (progn
          (ende-validate-resource-options)
          (ende-provider-validate-profile profile)
          (ende-transport-session-invoke
           (ende-codex--lifecycle session) :on-start
           (apply-partially #'ende-codex--cancel session))
          (unless (ende-codex--terminal-p session)
            (when (plist-member profile :codex-binding)
              (ende-codex--check-binding
               profile (ende-codex--profile-auth profile)))
            (ende-transport-session-add-canceller
             (ende-codex--lifecycle session)
             (ende-codex--resolve-auth
              (apply-partially #'ende-codex--auth-ready session messages)
              profile))))
      ((error quit)
       (ende-codex--fail
        session
        (ende-error-create
         :kind 'startup
         :message (error-message-string startup-error)))))
    (ende-transport-session-process (ende-codex--lifecycle session))))

(defun ende-codex-abort-background ()
  "Cancel login, token refresh, model and usage discovery before reload."
  (ende-codex-cancel-login)
  (let (flights)
    (maphash (lambda (_key flight) (push flight flights))
             ende-codex--refresh-flights)
    (dolist (flight flights) (ende-codex--refresh-finish flight nil t)))
  (dolist (process (append ende-codex--model-probe-processes
                           ende-codex--usage-probe-processes))
    (when (and (processp process) (process-live-p process))
      (delete-process process))))

(add-hook 'ende-queue-abort-functions #'ende-codex-abort-background)

(provide 'ende-codex)

;;; ende-codex.el ends here
