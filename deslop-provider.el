;;; deslop-provider.el --- Provider profiles and discovery for deslop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools, llm

;;; Commentary:

;; Provider configuration, profile snapshots, display policy, and asynchronous
;; metadata discovery for OpenAI-compatible endpoints.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'deslop-core)
(require 'deslop-curl)
(require 'deslop-protocol)
(require 'deslop-codex)

;;;; User options

(defcustom deslop-provider-endpoint "http://localhost:8080/v1/chat/completions"
  "OpenAI-compatible chat completions endpoint."
  :type 'string
  :group 'deslop)

(defcustom deslop-provider-model "qwen3.8-27b"
  "Model name sent to the LLM server."
  :type 'string
  :group 'deslop)

(defcustom deslop-provider-temperature 0.2
  "Sampling temperature for LLM requests."
  :type 'number
  :group 'deslop)

(defcustom deslop-providers '(local)
  "Provider profiles offered by the interactive selector.

Each item is a profile symbol understood by `deslop-provider-profile'."
  :type '(repeat symbol)
  :group 'deslop)

(defcustom deslop-provider-default-profile 'local
  "Fallback provider profile used when `deslop-providers' is empty."
  :type 'symbol
  :group 'deslop)

(defcustom deslop-provider-profiles nil
  "Additional provider profiles keyed by symbol.

Each value is a plist.  Secrets must not be stored here; use auth-source
fields :auth-source-host, :auth-source-user, and :auth-source-port instead.
An optional :display-name overrides endpoint-reported provider names for the
UI."
  :type 'sexp
  :group 'deslop)

(defcustom deslop-provider-discovery-enabled t
  "Non-nil to discover metadata from OpenAI-compatible model endpoints."
  :type 'boolean
  :group 'deslop)

(defcustom deslop-provider-discovery-ttl nil
  "Seconds to retain successful or failed provider discovery results."
  :type '(choice (const :tag "Never expire" nil)
                 (number :tag "Seconds"))
  :group 'deslop)

(defcustom deslop-provider-discovery-timeout nil
  "Maximum total seconds allowed for one provider discovery request.

When nil, use only the response inactivity timeout."
  :type '(choice (const :tag "No total deadline" nil)
                 (number :tag "Maximum seconds"))
  :group 'deslop)

(defcustom deslop-provider-discovery-cache-size nil
  "Maximum number of provider discovery results retained in memory."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Maximum entries"))
  :group 'deslop)

(defcustom deslop-provider-max-discovery-bytes nil
  "Maximum response-body bytes accepted from provider discovery.

When nil, do not impose a discovery response-size limit."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Maximum bytes"))
  :group 'deslop)

;;;; Profiles and display policy

(defun deslop-provider-profile (&optional name)
  "Return a non-secret provider profile for NAME.

The URL/model customizations remain the local-provider defaults.  OpenRouter
is available as a profile template and requires an auth-source entry before it
can be dispatched."
   (let* ((name (or name (car deslop-providers)
                    deslop-provider-default-profile))
         (base
          (pcase name
            ('openai
             (list :url deslop-codex-endpoint
                   :model deslop-codex-model
                   :wire-api 'responses
                   :auth-method 'opencode-oauth
                   :require-sse t
                   :display-name "OpenAI"))
            ('openrouter
             '(:url "https://openrouter.ai/api/v1/chat/completions"
               :model "openai/gpt-4o-mini"
               :temperature 0.2
               :auth-source-host "openrouter.ai"
               :auth-source-user "deslop"
               :auth-source-port 443
               :require-sse t))
            (_
             (list :url deslop-provider-endpoint
                   :model deslop-provider-model
                   :temperature deslop-provider-temperature
                   :require-sse t))))
         (custom (alist-get name deslop-provider-profiles)))
    (deslop--plist-merge
     (plist-put (copy-sequence base) :name name)
      custom)))

(defun deslop-provider-names ()
  "Return configured provider profile names for the selector."
  (or (cl-remove-if-not #'symbolp deslop-providers)
      (list deslop-provider-default-profile)))

(defun deslop-provider-models (profile)
  "Return configured model names for PROFILE."
  (let ((models (plist-get profile :models))
        (model (plist-get profile :model)))
    (cond
     ((and (listp models) models)
      (cl-remove-if-not #'stringp models))
     ((stringp model) (list model))
     (t nil))))

(defun deslop-provider-profile-snapshot (&optional name)
  "Return a detached, non-secret snapshot of provider profile NAME."
  (let ((snapshot (copy-tree (deslop-provider-profile name))))
    (dolist (key '(:token :api-key :secret :authorization :auth-token))
      (setq snapshot (plist-put snapshot key nil)))
    snapshot))

(defun deslop-provider--friendly-name (name)
  "Return a friendly display spelling for provider NAME."
  (if (not (stringp name))
      (format "%s" name)
    (let ((downcased (downcase name)))
      (cond
       ((member downcased '("openai" "openai.com")) "OpenAI")
       ((member downcased '("openrouter" "openrouter.ai")) "OpenRouter")
       ((member downcased '("llama.cpp" "llama-cpp")) "llama.cpp")
       (t name)))))

(defun deslop-provider-client-name (profile &optional metadata)
  "Return a concise display name for provider PROFILE.
METADATA may contain a provider/client reported by the endpoint.  Explicit
profile names and known service hosts take precedence over that report; local
hosts and IP addresses otherwise retain their host spelling."
  (let* ((url (plist-get profile :url))
         (parsed (and (stringp url) (url-generic-parse-url url)))
         (host (and parsed (url-host parsed)))
         (downcased (and host (downcase host)))
         (reported (and metadata
                        (or (plist-get metadata :client)
                            (plist-get metadata :provider)
                            (plist-get metadata :owner))))
         (explicit (plist-get profile :display-name)))
    (cond
     ((and (stringp explicit) (not (string-empty-p explicit)))
      (deslop-provider--friendly-name explicit))
     ((and downcased (string= downcased "openrouter.ai")) "OpenRouter")
     ((and downcased
           (string-match-p "\\(?:\\`\\|\\.\\)openai\\.com\\'" downcased))
      "OpenAI")
     ((and (stringp reported) (not (string-empty-p reported)))
      (deslop-provider--friendly-name reported))
     ((null host)
      (deslop-provider--friendly-name
       (format "%s" (or (plist-get profile :name) "provider"))))
     (t host))))

(defun deslop-provider-display-metadata (profile &optional metadata)
  "Return display CLIENT and MODEL values for PROFILE and METADATA."
  (list :client (deslop-provider-client-name profile metadata)
        :model (or (plist-get metadata :model)
                   (plist-get profile :model))))

(defun deslop-provider-result-display-metadata (profile metadata result)
  "Return display metadata for PROFILE, cached METADATA, and RESULT.
Provider and model values reported by RESULT take precedence over discovery
metadata and configured profile values."
  (let ((metadata (copy-sequence
                   (or metadata
                       (deslop-provider-cached-metadata profile))))
        (reported-model (and (listp result) (plist-get result :model)))
        (reported-provider (and (listp result)
                                (plist-get result :provider))))
    (when (stringp reported-model)
      (setq metadata (plist-put metadata :model reported-model)))
    (when (stringp reported-provider)
      (setq metadata (plist-put metadata :provider reported-provider))
      (setq metadata (plist-put metadata :client reported-provider)))
    (deslop-provider-display-metadata profile metadata)))

;;;; Provider discovery

(defvar deslop-provider--discovery-cache (make-hash-table :test #'equal)
  "Cached provider discovery results keyed by endpoint identity.")

(defvar deslop-provider--discovery-inflight (make-hash-table :test #'equal)
  "Callbacks waiting for an in-flight provider discovery request.")

(defvar deslop-provider--discovery-processes nil
  "Live provider discovery processes available for explicit cancellation.")

(defun deslop-provider--discovery-key (profile)
  "Return a non-secret cache key for provider PROFILE."
  (list (plist-get profile :url)
        (plist-get profile :name)
        (plist-get profile :auth-source-host)
        (plist-get profile :auth-source-user)
        (plist-get profile :model)
        (plist-get profile :display-name)
        (secure-hash 'sha256
                     (prin1-to-string (plist-get profile :request-headers)))))

(defun deslop-provider--models-url (profile)
  "Return the OpenAI-compatible model-list URL for PROFILE, or nil."
  (let ((url (plist-get profile :url)))
    (cond
     ((and (stringp url)
           (string-match
            "\\`\\(.+\\)/chat/completions\\(?:/\\)?\\(?:[?#].*\\)?\\'"
            url))
      (concat (match-string 1 url) "/models"))
     ((and (stringp url)
           (string-match "\\`\\(.+\\)/v[0-9]+/?\\'" url))
      (concat (replace-regexp-in-string "/\\'" "" url) "/models")))))

(defun deslop-provider--model-entries (payload)
  "Return model entries from discovery PAYLOAD as a list."
  (let ((data (deslop-protocol--json-get "data" payload)))
    (cond
     ((vectorp data) (append data nil))
     ((listp data) data))))

(defun deslop-provider--discovery-metadata (profile payload)
  "Extract display metadata for PROFILE from discovery PAYLOAD."
  (let* ((entries (deslop-provider--model-entries payload))
         (requested (plist-get profile :model))
         (match (cl-find-if
                 (lambda (entry)
                   (or (null requested)
                       (equal (deslop-protocol--json-get "id" entry)
                              requested)))
                 entries))
         (provider (or (deslop-protocol--json-get "provider" payload)
                       (deslop-protocol--json-get "server" payload)))
         (model (or (and match (deslop-protocol--json-get "id" match))
                    requested))
         (metadata (list :source :discovery :model model)))
    (when (stringp provider)
      (setq metadata (plist-put metadata :provider provider)))
    (when (and match
               (stringp (deslop-protocol--json-get "owned_by" match)))
      (setq metadata
            (plist-put metadata :owner
                       (deslop-protocol--json-get "owned_by" match))))
    (when (or match (stringp provider))
      (plist-put metadata :client
                 (deslop-provider-client-name profile metadata)))))

(defun deslop-provider--parse-discovery-response (profile body headers)
  "Parse a discovery response in BODY using response HEADERS."
  (let ((status (car (deslop-curl--response-headers headers))))
    (when (and status (>= status 200) (< status 300)
               (buffer-live-p body))
      (with-current-buffer body
        (condition-case nil
            (deslop-provider--discovery-metadata
             profile (deslop-protocol--json-read-string (buffer-string)))
          ((error quit) nil))))))

(defun deslop-provider--curl-discovery-command
    (url headers-file config-file)
  "Build a GET command for provider discovery at URL."
  (append
   (list "curl" "--disable")
   (deslop-curl--network-options url)
   (list "--silent" "--show-error" "--no-buffer"
         "--request" "GET"
         "--speed-limit" "1"
         "--speed-time" (number-to-string deslop-response-idle-timeout)
         "--dump-header" headers-file
         "--header" "Accept: application/json")
   (when config-file (list "--config" config-file))
   (append
    (when deslop-connect-timeout
      (list "--connect-timeout" (number-to-string deslop-connect-timeout)))
    (when deslop-provider-discovery-timeout
      (list "--max-time"
            (number-to-string deslop-provider-discovery-timeout)))
    (when deslop-provider-max-discovery-bytes
      (list "--max-filesize"
            (number-to-string deslop-provider-max-discovery-bytes)))
    (list "--url" url))))

(defun deslop-provider--invoke-discovery-callback (callback metadata)
  "Invoke discovery CALLBACK with METADATA without propagating its errors."
  (when callback
    (condition-case nil
        (funcall callback metadata)
      ((error quit) nil))))

(defun deslop-provider--prune-discovery-cache ()
  "Remove expired and excess provider discovery cache entries."
  (let ((now (float-time))
        (expired nil))
    (when deslop-provider-discovery-ttl
      (maphash
       (lambda (key entry)
         (when (>= (- now (plist-get entry :time))
                    deslop-provider-discovery-ttl)
           (push key expired)))
       deslop-provider--discovery-cache))
    (dolist (key expired)
      (remhash key deslop-provider--discovery-cache))
    (when deslop-provider-discovery-cache-size
      (while (>= (hash-table-count deslop-provider--discovery-cache)
                 (max 1 deslop-provider-discovery-cache-size))
        (let ((oldest nil))
          (maphash
           (lambda (key entry)
             (when (or (null oldest)
                       (< (plist-get entry :time)
                          (plist-get (cdr oldest) :time)))
               (setq oldest (cons key entry))))
           deslop-provider--discovery-cache)
          (if oldest
              (remhash (car oldest) deslop-provider--discovery-cache)
            (clrhash deslop-provider--discovery-cache)))))))

(defun deslop-provider--finish-discovery (key metadata &optional no-cache)
  "Notify callbacks waiting on KEY and optionally cache METADATA.
NO-CACHE is used for cancellation, which must not suppress a later retry."
  (unless no-cache
    (deslop-provider--prune-discovery-cache)
    (puthash key (list :time (float-time) :metadata metadata)
             deslop-provider--discovery-cache))
  (let ((callbacks (gethash key deslop-provider--discovery-inflight)))
    (remhash key deslop-provider--discovery-inflight)
    (dolist (callback callbacks)
      (deslop-provider--invoke-discovery-callback callback metadata))))

(defun deslop-provider-cached-metadata (profile)
  "Return unexpired discovery metadata for PROFILE, or nil."
  (let ((entry (deslop-provider--cached-entry
                (deslop-provider--discovery-key profile))))
    (and entry (plist-get entry :metadata))))

(defun deslop-provider--cached-entry (key)
  "Return the live discovery cache entry for KEY, or nil when expired."
  (let* ((entry (gethash key deslop-provider--discovery-cache))
         (age (and entry (- (float-time) (plist-get entry :time)))))
    (if (and entry
             (or (null deslop-provider-discovery-ttl)
                 (< age deslop-provider-discovery-ttl)))
        entry
      (when entry
        (remhash key deslop-provider--discovery-cache))
      nil)))

(defun deslop-provider--start-discovery (profile key url)
  "Start an asynchronous discovery request for PROFILE at URL."
  (let ((response-buf nil)
        (stderr-buf nil)
        (headers-file nil)
        (config-file nil)
        (process nil)
        (timer nil)
        (received-bytes 0)
        (finished nil)
        (finish nil))
    (setq finish
          (lambda (metadata &optional no-cache)
            (unless finished
              (setq finished t)
              (setq deslop-provider--discovery-processes
                    (delq process deslop-provider--discovery-processes))
              (when (timerp timer)
                (deslop--ignore-errors #'cancel-timer timer))
              (when (and (processp process) (process-live-p process))
                (process-put process :deslop-cancelled t)
                (deslop--ignore-errors #'delete-process process))
              (deslop-curl--cleanup-resources
               (list response-buf stderr-buf)
               (list headers-file config-file))
              (deslop-provider--finish-discovery key metadata no-cache))))
    (condition-case _startup
        (let ((request
               (deslop-curl--start-request
                :profile profile
                :url url
                :name (format "deslop-provider-discovery-%s"
                              (or (plist-get profile :name) "request"))
                :command (lambda (request-headers request-config)
                           (deslop-provider--curl-discovery-command
                            url request-headers request-config))
                :filter
                (lambda (proc string)
                  (unless finished
                    (setq received-bytes
                          (+ received-bytes (string-bytes string)))
                    (if (and deslop-provider-max-discovery-bytes
                             (> received-bytes
                                deslop-provider-max-discovery-bytes))
                        (funcall finish nil)
                      (let ((response (or (process-get
                                           proc :deslop-response-buffer)
                                          (process-buffer proc))))
                        (when (buffer-live-p response)
                          (with-current-buffer response
                            (goto-char (point-max))
                            (insert string)))))))
                :sentinel
                (lambda (proc _event)
                  (when (memq (process-status proc) '(exit signal closed))
                    (if (= (process-exit-status proc) 0)
                        (condition-case nil
                            (funcall
                             finish
                             (deslop-provider--parse-discovery-response
                              profile response-buf headers-file))
                          ((error quit) (funcall finish nil)))
                      (funcall finish nil)))))))
          (setq process (plist-get request :process)
                response-buf (plist-get request :response-buffer)
                stderr-buf (plist-get request :stderr-buffer)
                headers-file (plist-get request :headers-file)
                config-file (plist-get request :config-file))
          (process-put process :deslop-discovery-finish finish)
          (push process deslop-provider--discovery-processes)
          (unless finished
           (when deslop-provider-discovery-timeout
             (setq timer
                   (run-at-time deslop-provider-discovery-timeout nil
                                (lambda () (funcall finish nil)))))))
      ((error quit)
       (funcall finish nil)))))

(defun deslop-provider-discover (profile &optional callback)
  "Discover PROFILE metadata asynchronously and invoke CALLBACK with it.
Results, including failed lookups, are cached for
`deslop-provider-discovery-ttl' seconds.  CALLBACK receives nil when the
endpoint does not expose compatible metadata."
  (let ((callback (or callback #'ignore)))
    (if (not deslop-provider-discovery-enabled)
        (deslop-provider--invoke-discovery-callback callback nil)
      (let* ((key (deslop-provider--discovery-key profile))
             (entry (deslop-provider--cached-entry key))
             (url (deslop-provider--models-url profile)))
        (if entry
            (deslop-provider--invoke-discovery-callback
             callback (plist-get entry :metadata))
          (let ((waiting (gethash key deslop-provider--discovery-inflight)))
            (if waiting
                (puthash key (cons callback waiting)
                         deslop-provider--discovery-inflight)
              (puthash key (list callback)
                       deslop-provider--discovery-inflight)
              (if url
                  (deslop-provider--start-discovery profile key url)
                (deslop-provider--finish-discovery key nil)))))))))

(defun deslop-provider-clear-discovery-cache ()
  "Clear cached provider discovery results."
  (interactive)
  (deslop-provider-abort-discovery)
  (clrhash deslop-provider--discovery-cache)
  (message "deslop: provider discovery cache cleared"))

(defun deslop-provider-abort-discovery ()
  "Cancel all in-flight provider discovery requests."
  (dolist (process (copy-sequence deslop-provider--discovery-processes))
    (let ((finish (and (processp process)
                       (process-get process :deslop-discovery-finish))))
      (if finish
          (funcall finish nil t)
        (when (and (processp process) (process-live-p process))
          (deslop--ignore-errors #'delete-process process)))))
  (setq deslop-provider--discovery-processes nil))

(provide 'deslop-provider)

;;; deslop-provider.el ends here
