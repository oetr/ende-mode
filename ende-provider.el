;;; ende-provider.el --- Provider profiles and discovery for ende -*- lexical-binding: t; -*-

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
(require 'ende-core)
(require 'ende-curl)
(require 'ende-protocol)
(require 'ende-codex)

;;;; User options

(defconst ende-provider--default-model "qwen3.8-27b"
  "Built-in model for the local provider profile.")

(defconst ende-provider--default-openrouter-model "openai/gpt-4o-mini"
  "Built-in model for the OpenRouter profiles.")

(defconst ende-provider--default-codex-model "gpt-5.6-luna"
  "Built-in model for the OpenAI/Codex profile.")

(defcustom ende-provider-endpoint "http://localhost:8080/v1/chat/completions"
  "OpenAI-compatible chat completions endpoint."
  :type 'string
  :group 'ende)

(defcustom ende-provider-model ende-provider--default-model
  "Model name sent to the LLM server."
  :type 'string
  :group 'ende)

(defcustom ende-provider-temperature 0.8
  "Sampling temperature for LLM requests."
  :type 'number
  :group 'ende)

(defcustom ende-providers '(local)
  "Provider profiles offered by the interactive selector.

Each item is a profile symbol understood by `ende-provider-profile'."
  :type '(repeat symbol)
  :group 'ende)

(defcustom ende-provider-default-profile 'local
  "Fallback provider profile used when `ende-providers' is empty."
  :type 'symbol
  :group 'ende)

(defcustom ende-provider-profiles nil
  "Additional provider profiles keyed by symbol.

Each value is a plist.  Secrets must not be stored here; use auth-source
fields :auth-source-host, :auth-source-user, and :auth-source-port instead.
An optional :display-name overrides endpoint-reported provider names for the
UI.  :max-concurrency limits parallel jobs for the provider, and :parallel
enables parallel jobs by default.  :source-name-disclosure overrides
`ende-source-name-disclosure' for operation prompts."
  :type 'sexp
  :group 'ende)

(defcustom ende-provider-discovery-enabled t
  "Non-nil to discover metadata from OpenAI-compatible model endpoints."
  :type 'boolean
  :group 'ende)

(defcustom ende-provider-discovery-ttl nil
  "Seconds to retain successful or failed provider discovery results."
  :type '(choice (const :tag "Never expire" nil)
                 (number :tag "Seconds"))
  :group 'ende)

(defcustom ende-provider-discovery-timeout nil
  "Maximum total seconds allowed for one provider discovery request.

When nil, use only the response inactivity timeout."
  :type '(choice (const :tag "No total deadline" nil)
                 (number :tag "Maximum seconds"))
  :group 'ende)

(defcustom ende-provider-discovery-cache-size nil
  "Maximum number of provider discovery results retained in memory."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Maximum entries"))
  :group 'ende)

(defconst ende-provider--openrouter-request-headers
  '("HTTP-Referer: https://github.com/oetr/ende-mode"
    "X-OpenRouter-Title: ende-mode")
  "OpenRouter app-attribution headers for the built-in profiles.")

;;;; Profiles and display policy

(defvar ende-provider--runtime-settings (make-hash-table :test #'eq)
  "Session-level provider settings changed from the selector.")

(defun ende-provider--runtime-value (name key)
  "Return runtime setting KEY for provider NAME, or `:unset'."
  (let ((settings (gethash name ende-provider--runtime-settings)))
    (if (plist-member settings key)
        (plist-get settings key)
      :unset)))

(defun ende-provider-set-runtime-setting (name key value)
  "Set runtime provider setting KEY to VALUE for NAME.
Supported settings are `:max-concurrency' and `:parallel'."
  (pcase key
    (:max-concurrency
     (unless (and (integerp value) (> value 0))
       (user-error "Provider concurrency must be a positive integer")))
    (:parallel
     (unless (memq value '(nil t))
       (error "Provider parallel setting must be boolean")))
    (_ (error "Unsupported provider runtime setting: %s" key)))
  (let ((settings (copy-sequence
                   (gethash name ende-provider--runtime-settings))))
    (puthash name (plist-put settings key value)
             ende-provider--runtime-settings))
  value)

(defun ende-provider-clear-runtime-settings ()
  "Clear all session-level provider settings."
  (clrhash ende-provider--runtime-settings))

(defun ende-provider-profile (&optional name)
  "Return a non-secret provider profile for NAME.

 The URL/model customizations remain the local-provider defaults.  OpenRouter
 provides both synchronous and Batch API profile templates and requires an
 auth-source entry before it can be dispatched."
  (let* ((name (or name (car ende-providers)
                   ende-provider-default-profile))
         (base
          (pcase name
            ('openai
             (list :url ende-codex-endpoint
                   :model ende-codex-model
                   :model-discovery-key
                   (list ende-codex-models-endpoint
                         ende-codex-client-version
                         (expand-file-name ende-codex-auth-file))
                   :max-concurrency 4
                    :parallel nil
                    :wire-api 'responses
                    :auth-method 'opencode-oauth
                    :model-discovery-function #'ende-codex-probe-models
                    :reasoning-effort-discovery-function
                    #'ende-codex-probe-reasoning-efforts
                    :reasoning-effort-wire-api 'responses
                    :require-sse t
                    :display-name "OpenAI"))
            ('openrouter
             (list :url "https://openrouter.ai/api/v1/chat/completions"
                   :batch-url "https://openrouter.ai/api/v1/batches"
                   :batch-endpoint "/v1/chat/completions"
                   :transport 'auto
                   :model ende-provider--default-openrouter-model
                   :max-concurrency 4
                   :parallel nil
                   :temperature 0.2
                   :auth-source-host "openrouter.ai"
                   :auth-source-user "ende"
                   :auth-source-port 443
                   :request-headers
                   (copy-sequence ende-provider--openrouter-request-headers)
                   :reasoning-effort-discovery-function
                   #'ende-provider--probe-openrouter-reasoning-efforts
                   :reasoning-effort-wire-api 'openrouter
                   :require-sse t))
            ('openrouter-batch
             (list :url "https://openrouter.ai/api/v1/chat/completions"
                   :batch-url "https://openrouter.ai/api/v1/batches"
                   :batch-endpoint "/v1/chat/completions"
                   :transport 'openrouter-batch
                   :model ende-provider--default-openrouter-model
                   :max-concurrency 16
                   :parallel t
                   :temperature 0.2
                   :auth-source-host "openrouter.ai"
                   :auth-source-user "ende"
                   :auth-source-port 443
                   :request-headers
                   (copy-sequence ende-provider--openrouter-request-headers)
                   :reasoning-effort-discovery-function
                   #'ende-provider--probe-openrouter-reasoning-efforts
                   :reasoning-effort-wire-api 'openrouter
                   :require-sse nil))
            (_
             (list :url ende-provider-endpoint
                   :model ende-provider-model
                   :max-concurrency 1
                   :parallel nil
                   :temperature ende-provider-temperature
                   :require-sse t))))
         (custom (alist-get name ende-provider-profiles)))
    (let ((profile (ende-plist-merge
                    (plist-put (copy-sequence base) :name name)
                    custom)))
      (setq profile
            (plist-put
             profile :model-explicit-p
             (or (and (plist-member custom :model) t)
                 (pcase name
                   ('openai
                    (not (equal ende-codex-model
                                ende-provider--default-codex-model)))
                   ('openrouter nil)
                   ('openrouter-batch nil)
                   (_ (not (equal ende-provider-model
                                  ende-provider--default-model)))))))
      (setq profile
            (plist-put profile :models-explicit-p
                       (and (plist-member custom :models) t)))
      (dolist (key '(:max-concurrency :parallel))
        (let ((value (ende-provider--runtime-value name key)))
          (unless (eq value :unset)
            (setq profile (plist-put profile key value)))))
      profile)))

(defun ende-provider-max-concurrency (profile)
  "Return the positive maximum concurrent jobs allowed by PROFILE."
  (let* ((runtime (ende-provider--runtime-value
                   (plist-get profile :name) :max-concurrency))
         (value (if (eq runtime :unset)
                    (or (plist-get profile :max-concurrency) 1)
                  runtime)))
    (unless (and (integerp value) (> value 0))
      (error "Provider max concurrency must be a positive integer"))
    value))

(defun ende-provider-parallel-enabled-p (profile)
  "Return non-nil when PROFILE enables parallel jobs by default."
  (eq (plist-get profile :parallel) t))

(defun ende-provider-reasoning-effort (profile wire-api)
  "Return PROFILE's selected effort when WIRE-API supports it.
The selector is the only code that sets `:reasoning-effort', after a live
capability check."
  (let ((effort (plist-get profile :reasoning-effort)))
    (and (eq (plist-get profile :reasoning-effort-wire-api) wire-api)
         (stringp effort)
         (not (string-empty-p effort))
         effort)))

(defun ende-provider-names ()
  "Return configured provider profile names for the selector."
  (or (cl-remove-if-not #'symbolp ende-providers)
      (list ende-provider-default-profile)))

(defun ende-provider-models (profile)
  "Return configured model names for PROFILE."
  (let ((models (plist-get profile :models))
        (model (plist-get profile :model)))
    (cond
     ((and (listp models) models)
      (cl-remove-if-not #'stringp models))
     ((stringp model) (list model))
     (t nil))))

(defun ende-provider-configured-model (profile)
  "Return PROFILE's explicitly configured model, or nil.

Built-in profile defaults are deliberately not preferences."
  (and (plist-get profile :model-explicit-p)
       (plist-get profile :model)))

(defun ende-provider-configured-models-explicit-p (profile)
  "Return non-nil when PROFILE has an explicitly configured model list."
  (plist-get profile :models-explicit-p))

(defun ende-provider-profile-snapshot (&optional name)
  "Return a detached, non-secret snapshot of provider profile NAME."
  (let ((snapshot (copy-tree (ende-provider-profile name))))
    (dolist (key '(:token :api-key :secret :authorization :auth-token))
      (setq snapshot (plist-put snapshot key nil)))
    snapshot))

(defun ende-provider-source-name-disclosure-p (profile)
  "Return non-nil when source names may be included for PROFILE."
  (let ((value (if (plist-member profile :source-name-disclosure)
                   (plist-get profile :source-name-disclosure)
                 ende-source-name-disclosure)))
    (unless (memq value '(nil t))
      (error "Source name disclosure must be boolean"))
    value))

(defun ende-provider--friendly-name (name)
  "Return a friendly display spelling for provider NAME."
  (if (not (stringp name))
      (format "%s" name)
    (let ((downcased (downcase name)))
      (cond
       ((member downcased '("openai" "openai.com")) "OpenAI")
       ((member downcased '("openrouter" "openrouter.ai" "openrouter-batch"))
        "OpenRouter")
       ((member downcased '("llama.cpp" "llama-cpp")) "llama.cpp")
       (t name)))))

(defun ende-provider-client-name (profile &optional metadata)
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
      (ende-provider--friendly-name explicit))
     ((and downcased (string= downcased "openrouter.ai")) "OpenRouter")
     ((and downcased
           (string-match-p "\\(?:\\`\\|\\.\\)openai\\.com\\'" downcased))
      "OpenAI")
     ((and (stringp reported) (not (string-empty-p reported)))
      (ende-provider--friendly-name reported))
     ((null host)
      (ende-provider--friendly-name
       (format "%s" (or (plist-get profile :name) "provider"))))
     (t host))))

(defun ende-provider-display-metadata (profile &optional metadata)
  "Return display CLIENT and MODEL values for PROFILE and METADATA."
  (list :client (ende-provider-client-name profile metadata)
        :model (or (plist-get metadata :model)
                   (plist-get profile :model))))

(defun ende-provider-result-display-metadata (profile metadata result)
  "Return display metadata for PROFILE, cached METADATA, and RESULT.
Provider and model values reported by RESULT take precedence over discovery
metadata and configured profile values."
  (let ((metadata (copy-sequence
                   (or metadata
                       (ende-provider-cached-metadata profile))))
        (reported-model (and (listp result) (plist-get result :model)))
        (reported-provider (and (listp result)
                                (plist-get result :provider))))
    (when (stringp reported-model)
      (setq metadata (plist-put metadata :model reported-model)))
    (when (stringp reported-provider)
      (setq metadata (plist-put metadata :provider reported-provider))
      (setq metadata (plist-put metadata :client reported-provider)))
    (ende-provider-display-metadata profile metadata)))

;;;; Provider discovery

(defvar ende-provider--discovery-cache (make-hash-table :test #'equal)
  "Cached provider discovery results keyed by endpoint identity.")

(defvar ende-provider--model-cache (make-hash-table :test #'equal)
  "Cached provider model catalogs keyed by model-list endpoint identity.")

(defvar ende-provider--discovery-inflight (make-hash-table :test #'equal)
  "Callbacks waiting for an in-flight provider discovery request.")

(defvar ende-provider--model-inflight (make-hash-table :test #'equal)
  "Callbacks waiting for an in-flight model catalog request.")

(defvar ende-provider--model-cancellations (make-hash-table :test #'equal)
  "Underlying cancellation functions for in-flight model catalog requests.")

(defvar ende-provider--discovery-processes nil
  "Live provider discovery processes available for explicit cancellation.")

(defvar ende-provider--model-generation 0
  "Monotonic generation for cached provider model catalogs.")

(defconst ende-provider--model-pricing-fields
  '(("prompt" . :prompt)
    ("completion" . :completion)
    ("internal_reasoning" . :internal-reasoning)
    ("input_cache_read" . :input-cache-read)
    ("input_cache_write" . :input-cache-write)
    ("input_cache_write_1h" . :input-cache-write-1h))
  "JSON pricing fields retained for discovered models.")

(defun ende-provider--discovery-key (profile)
  "Return a non-secret cache key for provider PROFILE."
  (list (plist-get profile :url)
        (plist-get profile :name)
        (plist-get profile :auth-source-host)
        (plist-get profile :auth-source-user)
        (plist-get profile :model)
        (plist-get profile :display-name)
        (secure-hash 'sha256
                     (prin1-to-string (plist-get profile :request-headers)))))

(defun ende-provider--validate-discovery-options ()
  "Validate provider discovery cache and resource options."
  (dolist (option
           (list (cons 'ende-provider-discovery-ttl
                       ende-provider-discovery-ttl)
                 (cons 'ende-provider-discovery-timeout
                       ende-provider-discovery-timeout)))
    (unless (or (null (cdr option))
                (and (numberp (cdr option)) (> (cdr option) 0)))
      (error "%s must be nil or a positive number" (car option))))
  (dolist (option
           (list (cons 'ende-provider-discovery-cache-size
                       ende-provider-discovery-cache-size)
                 (cons 'ende-provider-max-discovery-bytes
                       ende-provider-max-discovery-bytes)))
    (unless (or (null (cdr option))
                (and (integerp (cdr option)) (> (cdr option) 0)))
      (error "%s must be nil or a positive integer" (car option))))
  t)

(defun ende-provider-models-url (profile)
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

(defun ende-provider-model-discovery-available-p (profile)
  "Return non-nil when PROFILE has dynamic model discovery."
  (or (functionp (plist-get profile :model-discovery-function))
      (ende-provider-models-url profile)))

(defun ende-provider--model-cache-key (profile)
  "Return the model catalog cache key for PROFILE.

The selected model is intentionally absent.  Changing it must revalidate the
selection without starting another model-list request."
  (let ((url (ende-provider-models-url profile))
        (probe (plist-get profile :model-discovery-function)))
    (when (or url (functionp probe))
      (list (plist-get profile :name)
            (or (plist-get profile :model-discovery-key) url)
            probe
            (plist-get profile :auth-source-host)
            (plist-get profile :auth-source-user)
            (plist-get profile :auth-source-port)
            (secure-hash
             'sha256
             (prin1-to-string (plist-get profile :request-headers)))))))

(defun ende-provider--model-entries (payload)
  "Return model entries from discovery PAYLOAD as a list."
  (let ((data (ende-protocol-json-get "data" payload)))
    (cond
     ((vectorp data) (append data nil))
     ((listp data) data))))

(defun ende-provider--model-pricing (entry)
  "Return supported pricing fields from model ENTRY."
  (let ((pricing (ende-protocol-json-get "pricing" entry))
        (result nil))
    (when (hash-table-p pricing)
      (dolist (field ende-provider--model-pricing-fields)
        (let ((value (gethash (car field) pricing)))
          (when (or (stringp value) (numberp value))
            (setq result
                  (plist-put result (cdr field) (format "%s" value)))))))
    result))

(defun ende-provider--model-detail (entry)
  "Return display metadata for model ENTRY, or nil without a valid ID."
  (let ((id (ende-protocol-json-get "id" entry))
        (name (ende-protocol-json-get "name" entry)))
    (when (and (stringp id) (not (string-empty-p id)))
      (list :id id
            :name (and (stringp name) (not (string-empty-p name)) name)
            :pricing (ende-provider--model-pricing entry)))))

(defun ende-provider--discovery-metadata (profile payload)
  "Extract display metadata for PROFILE from discovery PAYLOAD."
  (let* ((entries (ende-provider--model-entries payload))
         (details (delq nil (mapcar #'ende-provider--model-detail entries)))
         (models (mapcar (lambda (detail) (plist-get detail :id)) details))
         (requested (plist-get profile :model))
         (match (cl-find-if
                 (lambda (entry)
                    (or (null requested)
                        (equal (ende-protocol-json-get "id" entry)
                               requested)))
                 entries))
          (provider (or (ende-protocol-json-get "provider" payload)
                        (ende-protocol-json-get "server" payload)))
          (model (or (and match (ende-protocol-json-get "id" match))
                    requested))
          (metadata (list :source :discovery
                          :model model
                          :models models
                          :model-details details)))
    (when (or match (stringp provider) models)
      (when (stringp provider)
        (setq metadata (plist-put metadata :provider provider)))
      (when (and match
                  (stringp (ende-protocol-json-get "owned_by" match)))
        (setq metadata
              (plist-put metadata :owner
                          (ende-protocol-json-get "owned_by" match))))
      (when (or match (stringp provider))
        (setq metadata
              (plist-put metadata :client
                         (ende-provider-client-name profile metadata))))
      metadata)))

(defun ende-provider--parse-discovery-response (profile body headers)
  "Parse a discovery response in BODY using response HEADERS."
  (let ((status (car (ende-curl-response-headers headers))))
    (when (and status (>= status 200) (< status 300)
               (buffer-live-p body))
      (with-current-buffer body
        (condition-case nil
            (ende-provider--discovery-metadata
             profile (ende-protocol-json-read-string (buffer-string)))
          ((error quit) nil))))))

(defun ende-provider--curl-discovery-command
    (url headers-file config-file)
  "Build a GET command for provider discovery at URL."
  (append
   (ende-curl-base-command url)
   (ende-curl-response-options headers-file)
   (list "--request" "GET"
         "--header" "Accept: application/json")
   (when config-file (list "--config" config-file))
   (ende-curl-timeout-options
    ende-provider-discovery-timeout ende-provider-max-discovery-bytes)
   (list "--url" url)))

(defun ende-provider--invoke-discovery-callback (callback metadata)
  "Invoke discovery CALLBACK with METADATA without propagating its errors."
  (when callback
    (condition-case nil
        (funcall callback metadata)
      ((error quit) nil))))

(defun ende-provider--discovery-process (key)
  "Return the in-flight discovery process for KEY, or nil."
  (cl-find-if
   (lambda (process)
     (and (processp process)
          (equal (process-get process :ende-discovery-key) key)))
   ende-provider--discovery-processes))

(defun ende-provider--cancel-discovery-waiter (key callback)
  "Remove CALLBACK from discovery KEY and cancel an unused request."
  (let ((callbacks (gethash key ende-provider--discovery-inflight))
        (removed nil)
        (remaining nil))
    (dolist (waiting callbacks)
      (if (and (not removed) (eq waiting callback))
          (setq removed t)
        (push waiting remaining)))
    (when removed
      (setq remaining (nreverse remaining))
      (if remaining
          (puthash key remaining ende-provider--discovery-inflight)
        (remhash key ende-provider--discovery-inflight)
        (let* ((process (ende-provider--discovery-process key))
               (finish (and (processp process)
                            (process-get process :ende-discovery-finish))))
          (when finish
            (funcall finish nil nil t)))))))

(defun ende-provider--prune-cache (cache)
  "Remove expired and excess entries from CACHE."
  (let ((now (float-time))
        (expired nil))
    (when ende-provider-discovery-ttl
      (maphash
       (lambda (key entry)
         (when (>= (- now (plist-get entry :time))
                    ende-provider-discovery-ttl)
            (push key expired)))
        cache))
    (dolist (key expired)
      (remhash key cache))
    (when ende-provider-discovery-cache-size
      (while (>= (hash-table-count cache)
                 (max 1 ende-provider-discovery-cache-size))
        (let ((oldest nil))
          (maphash
           (lambda (key entry)
             (when (or (null oldest)
                       (< (plist-get entry :time)
                          (plist-get (cdr oldest) :time)))
               (setq oldest (cons key entry))))
           cache)
          (if oldest
              (remhash (car oldest) cache)
            (clrhash cache)))))))

(defun ende-provider--prune-discovery-cache ()
  "Remove expired and excess provider discovery cache entries."
  (ende-provider--prune-cache ende-provider--discovery-cache))

(defun ende-provider--cached-model-entry (key)
  "Return the usable model catalog entry for KEY, or nil when unavailable."
  (let* ((entry (gethash key ende-provider--model-cache))
         (age (and entry (- (float-time) (plist-get entry :time)))))
    (if (and entry
             (null (plist-get entry :error))
             (consp (plist-get entry :models))
             (or (null ende-provider-discovery-ttl)
                 (< age ende-provider-discovery-ttl)))
        entry
      (when entry
        (remhash key ende-provider--model-cache))
      nil)))

(defun ende-provider--model-entry (profile)
  "Return the cached model catalog entry for PROFILE, or nil."
  (let ((key (ende-provider--model-cache-key profile)))
    (and key (ende-provider--cached-model-entry key))))

(defun ende-provider-cached-models (profile)
  "Return a copy of PROFILE's cached model catalog, or nil.

The returned plist contains `:models', `:details', `:error', and
`:generation'.  This function never starts discovery."
  (let ((entry (ende-provider--model-entry profile)))
    (when entry
      (list :models (copy-sequence (plist-get entry :models))
            :details (copy-tree (plist-get entry :details))
            :error (plist-get entry :error)
            :generation (plist-get entry :generation)))))

(defun ende-provider-models-generation (profile)
  "Return the cached model catalog generation for PROFILE, or zero."
  (or (plist-get (ende-provider--model-entry profile) :generation)
      ende-provider--model-generation))

(defun ende-provider--finish-discovery (key metadata &optional no-cache)
  "Notify callbacks waiting on KEY and optionally cache METADATA.
NO-CACHE is used for cancellation, which must not suppress a later retry."
  (unless no-cache
    (ende-provider--prune-discovery-cache)
    (puthash key (list :time (float-time) :metadata metadata)
             ende-provider--discovery-cache))
  (let ((callbacks (gethash key ende-provider--discovery-inflight)))
    (remhash key ende-provider--discovery-inflight)
    (dolist (callback callbacks)
      (ende-provider--invoke-discovery-callback callback metadata))))

(defun ende-provider--invoke-model-callback (callback entry)
  "Invoke model catalog CALLBACK with ENTRY without propagating errors."
  (condition-case nil
      (funcall callback
               (and entry (copy-sequence (plist-get entry :models)))
               (and entry (plist-get entry :error)))
    ((error quit) nil)))

(defun ende-provider--finish-model-discovery
    (key models details error &optional no-cache)
  "Notify model catalog waiters for KEY and cache only successful results."
  (setq models (cl-remove-if-not #'stringp models))
  (when (and (null error) (null models))
    (setq error
          (ende-error-create
           :kind 'discovery
           :message "Provider returned no models")))
  (let ((entry (and (not no-cache)
                    (list :models (copy-sequence models)
                          :details (copy-tree details)
                          :error error))))
    (when (and entry (null error))
      (ende-provider--prune-cache ende-provider--model-cache)
      (setq entry
            (plist-put entry :time (float-time)))
      (setq entry
            (plist-put entry :generation
                       (cl-incf ende-provider--model-generation)))
      (puthash key entry ende-provider--model-cache))
    (remhash key ende-provider--model-cancellations)
    (let ((callbacks (gethash key ende-provider--model-inflight)))
      (remhash key ende-provider--model-inflight)
      (dolist (callback callbacks)
        (ende-provider--invoke-model-callback callback entry)))))

(defun ende-provider--cancel-model-waiter (key callback)
  "Remove model catalog CALLBACK from KEY and cancel an unused request."
  (let ((callbacks (gethash key ende-provider--model-inflight))
        (removed nil)
        (remaining nil))
    (dolist (waiting callbacks)
      (if (and (not removed) (eq waiting callback))
          (setq removed t)
        (push waiting remaining)))
    (when removed
      (setq remaining (nreverse remaining))
      (if remaining
          (puthash key remaining ende-provider--model-inflight)
        (remhash key ende-provider--model-inflight)
        (let ((cancel (gethash key ende-provider--model-cancellations)))
          (remhash key ende-provider--model-cancellations)
          (when cancel
            (ende-ignore-errors cancel)))))))

(defun ende-provider--start-model-discovery (profile key)
  "Start the uncached model discovery request for PROFILE and KEY."
  (let ((probe (plist-get profile :model-discovery-function))
        (completed nil)
        (cancelled nil)
        (underlying-cancel nil))
    (cl-labels
        ((finish (models details error)
           (unless cancelled
             (setq completed t)
             (ende-provider--finish-model-discovery
              key models details error)))
         (finish-error (error)
           (finish nil nil error)))
      (condition-case startup-error
          (setq underlying-cancel
                (if (functionp probe)
                    (funcall
                     probe
                     (lambda (models error)
                       (unless cancelled
                         (if error
                             (finish-error error)
                           (finish (cl-remove-if-not #'stringp models)
                                   nil nil)))))
                  (ende-provider-discover
                   profile
                   (lambda (metadata)
                     (unless cancelled
                       (let ((models (and metadata
                                          (plist-get metadata :models))))
                         (if (and (listp models) models)
                             (finish models
                                     (plist-get metadata :model-details)
                                     nil)
                           (remhash (ende-provider--discovery-key profile)
                                    ende-provider--discovery-cache)
                           (finish-error
                            (ende-error-create
                             :kind 'discovery
                             :message "Provider returned no models")))))))))
        ((error quit)
         (finish-error
          (ende-error-create
           :kind 'startup
           :message (error-message-string startup-error))))))
    (unless completed
      (puthash
       key
       (lambda ()
         (setq cancelled t)
         (when underlying-cancel
           (ende-ignore-errors underlying-cancel)
           (setq underlying-cancel nil)))
       ende-provider--model-cancellations))))

(defun ende-provider-cached-metadata (profile)
  "Return unexpired discovery metadata for PROFILE, or nil."
  (let ((entry (ende-provider--cached-entry
                (ende-provider--discovery-key profile))))
    (and entry (plist-get entry :metadata))))

(defun ende-provider-model-details (profile)
  "Return copied cached model details for PROFILE, or nil."
  (let ((catalog (ende-provider-cached-models profile)))
    (copy-tree
     (or (plist-get catalog :details)
         (plist-get (ende-provider-cached-metadata profile)
                    :model-details)))))

(defun ende-provider--price-per-million (value)
  "Return numeric VALUE converted from dollars per token to dollars per million.
Return nil when VALUE is not a nonnegative decimal price string."
  (when (and (stringp value)
             (string-match-p
              "\\`[0-9]+\\(?:\\.[0-9]*\\)?\\(?:[eE][+-]?[0-9]+\\)?\\'"
              value))
    (* 1000000.0 (string-to-number value))))

(defun ende-provider--format-price (value)
  "Format a dollars-per-token VALUE for a completion annotation."
  (let ((per-million (ende-provider--price-per-million value)))
    (cond
     ((null per-million) nil)
     ((zerop per-million) "free")
     ((< per-million 0.1) (format "$%.3g" per-million))
     ((< per-million 1) (format "$%.2f" per-million))
     (t (format "$%.3g" per-million)))))

(defun ende-provider-model-price-annotation (detail &optional part-function)
  "Return a completion annotation for pricing in model DETAIL.
When PART-FUNCTION is non-nil, call it with each pricing field and its
formatted text before joining the parts."
  (let ((pricing (plist-get detail :pricing))
        (parts nil))
    (dolist (field '((:prompt . "in")
                     (:completion . "out")
                     (:input-cache-read . "cache-read")
                     (:input-cache-write . "cache-write")
                     (:input-cache-write-1h . "cache-write-1h")
                     (:internal-reasoning . "reasoning")))
      (let ((price (ende-provider--format-price
                    (plist-get pricing (car field)))))
        (when price
          (let ((part (format "%s: %s" (cdr field) price)))
            (push (if part-function
                      (funcall part-function (car field) part)
                    part)
                  parts)))))
    (when parts
      (concat "  " (mapconcat #'identity (nreverse parts) "  ")))))

(defun ende-provider--cached-entry (key)
  "Return the live discovery cache entry for KEY, or nil when expired."
  (let* ((entry (gethash key ende-provider--discovery-cache))
         (age (and entry (- (float-time) (plist-get entry :time)))))
    (if (and entry
             (or (null ende-provider-discovery-ttl)
                 (< age ende-provider-discovery-ttl)))
        entry
      (when entry
        (remhash key ende-provider--discovery-cache))
      nil)))

(defun ende-provider--start-live-model-request
    (profile url callback &optional discovery-key)
  "Request a model list from URL and call CALLBACK with (PAYLOAD ERROR).

This bypasses provider discovery caches.  DISCOVERY-KEY identifies a regular
cached-discovery request so its waiters can cancel it."
  (let ((response-buf nil)
        (stderr-buf nil)
        (stderr-process nil)
        (headers-file nil)
        (config-file nil)
        (process nil)
        (timer nil)
        (received-bytes 0)
        (finished nil)
        (finish nil))
    (setq finish
          (lambda (payload &optional error cancelled)
            (unless finished
              (setq finished t)
              (setq ende-provider--discovery-processes
                    (delq process ende-provider--discovery-processes))
              (when (timerp timer)
                (ende-ignore-errors #'cancel-timer timer))
              (when (and (processp process) (process-live-p process))
                (process-put process :ende-cancelled t)
                (ende-ignore-errors #'delete-process process))
              (ende-curl-cleanup-resources
               (list response-buf stderr-buf)
               (list headers-file config-file)
               (list stderr-process))
              (unless cancelled
                (funcall callback payload error)))))
    (condition-case _startup
        (let ((request
               (ende-curl-start-request
                :profile profile
                :url url
                :name (format "ende-provider-discovery-%s"
                              (or (plist-get profile :name) "request"))
                :command (lambda (request-headers request-config)
                           (ende-provider--curl-discovery-command
                            url request-headers request-config))
                :filter
                (lambda (proc string)
                  (unless finished
                    (setq received-bytes
                          (+ received-bytes (string-bytes string)))
                    (if (and ende-provider-max-discovery-bytes
                             (> received-bytes
                                ende-provider-max-discovery-bytes))
                        (funcall
                         finish nil
                         (ende-error-create
                          :kind 'resource
                          :message
                          "Provider model response exceeds ende-provider-max-discovery-bytes"))
                      (let ((response (or (process-get
                                           proc :ende-response-buffer)
                                          (process-buffer proc))))
                        (when (buffer-live-p response)
                          (with-current-buffer response
                            (goto-char (point-max))
                            (insert string)))))))
                :sentinel
                (lambda (proc event)
                  (when (memq (process-status proc) '(exit signal closed))
                    (if (/= (process-exit-status proc) 0)
                        (funcall
                         finish nil
                         (or (process-get proc :ende-idle-error)
                             (ende-error-create
                              :kind 'transport
                              :message
                              (format "Provider model request failed: %s"
                                      (string-trim event)))))
                      (condition-case parse-error
                          (let ((status
                                 (car (ende-curl-response-headers headers-file)))
                                (text
                                 (with-current-buffer response-buf
                                   (buffer-string))))
                            (if (and status (>= status 200) (< status 300))
                                (funcall finish
                                         (ende-protocol-json-read-string text))
                              (funcall
                               finish nil
                               (ende-error-create
                                :kind 'http
                                :message
                                (format "Provider model request returned HTTP status %s"
                                        (or status "unknown"))))))
                        ((error quit)
                         (funcall
                          finish nil
                          (ende-error-create
                           :kind 'protocol
                           :message (error-message-string parse-error)))))))))))
          (setq process (plist-get request :process)
                response-buf (plist-get request :response-buffer)
                stderr-buf (plist-get request :stderr-buffer)
                stderr-process (plist-get request :stderr-process)
                headers-file (plist-get request :headers-file)
                config-file (plist-get request :config-file))
          (process-put process :ende-discovery-finish finish)
          (when discovery-key
            (process-put process :ende-discovery-key discovery-key))
          (push process ende-provider--discovery-processes)
          (unless finished
            (when ende-provider-discovery-timeout
              (setq timer
                    (run-at-time ende-provider-discovery-timeout nil
                                 (lambda () (funcall finish nil)))))))
      ((error quit)
       (funcall
        finish nil
        (ende-error-create :kind 'startup
                           :message (error-message-string _startup)))))
    (lambda () (funcall finish nil nil t))))

(defun ende-provider--start-discovery (profile key url)
  "Start an asynchronous cached discovery request for PROFILE at URL."
  (ende-provider--start-live-model-request
   profile url
   (lambda (payload error)
     (ende-provider--finish-discovery
      key
      (and (not error)
           (condition-case nil
               (ende-provider--discovery-metadata profile payload)
             ((error quit) nil)))))
   key))

(defun ende-provider-discover (profile &optional callback)
  "Discover PROFILE metadata asynchronously and invoke CALLBACK with it.
Results, including failed lookups, are cached for
`ende-provider-discovery-ttl' seconds.  CALLBACK receives nil when the
endpoint does not expose compatible metadata."
  (let ((callback (or callback #'ignore)))
    (ende-validate-resource-options)
    (ende-provider--validate-discovery-options)
    (if (not ende-provider-discovery-enabled)
        (progn
          (ende-provider--invoke-discovery-callback callback nil)
          nil)
      (let* ((key (ende-provider--discovery-key profile))
             (entry (ende-provider--cached-entry key))
             (url (ende-provider-models-url profile)))
        (if entry
            (progn
              (ende-provider--invoke-discovery-callback
               callback (plist-get entry :metadata))
              nil)
          (let ((waiting (gethash key ende-provider--discovery-inflight)))
            (if waiting
                (puthash key (cons callback waiting)
                         ende-provider--discovery-inflight)
              (puthash key (list callback)
                       ende-provider--discovery-inflight)
              (if url
                  (ende-provider--start-discovery profile key url)
                (ende-provider--finish-discovery key nil)))
            (apply-partially #'ende-provider--cancel-discovery-waiter
                             key callback)))))))

(defun ende-provider-probe-models (profile callback)
  "Probe PROFILE's model-list endpoint and invoke CALLBACK with its models.
CALLBACK receives (MODELS ERROR).  Return a cancellation function that removes
this callback; an unused shared request is cancelled."
  (let ((key (ende-provider--model-cache-key profile))
        (probe (plist-get profile :model-discovery-function)))
    (if (or (not key)
            (and (null probe) (not ende-provider-discovery-enabled)))
        (progn
          (ende-provider--invoke-model-callback
           callback
           (list :models nil
                 :error
                 (ende-error-create
                  :kind 'discovery
                  :message (if (not key)
                               "Provider has no compatible model-list endpoint"
                             "Provider model discovery is disabled"))))
          (lambda () nil))
      (let ((entry (ende-provider--cached-model-entry key)))
        (if entry
            (progn
              (ende-provider--invoke-model-callback callback entry)
              (lambda () nil))
          (let ((waiting (gethash key ende-provider--model-inflight)))
            (puthash key
                     (cons callback waiting)
                     ende-provider--model-inflight)
            (unless waiting
              (ende-provider--start-model-discovery profile key))
            (apply-partially #'ende-provider--cancel-model-waiter
                             key callback)))))))

(defun ende-provider--invoke-reasoning-effort-callback
    (callback efforts default error)
  "Invoke effort CALLBACK without propagating callback errors."
  (condition-case nil
      (funcall callback efforts default error)
    ((error quit) nil)))

(defun ende-provider--json-string-list (value)
  "Return ordered, distinct nonempty strings from JSON array VALUE."
  (let ((values (cond ((vectorp value) (append value nil))
                      ((listp value) value)))
        (seen nil)
        (result nil))
    (dolist (value values)
      (when (and (stringp value) (not (string-empty-p value))
                 (not (member value seen)))
        (push value seen)
        (push value result)))
    (nreverse result)))

(defun ende-provider--probe-openrouter-reasoning-efforts
    (profile model callback)
  "Query OpenRouter live for MODEL's advertised reasoning efforts.
CALLBACK receives (EFFORTS DEFAULT ERROR).  This request deliberately bypasses
the model catalog cache so the returned choices are current."
  (let ((url (ende-provider-models-url profile)))
    (if (not url)
        (progn
          (ende-provider--invoke-reasoning-effort-callback
           callback nil nil
           (ende-error-create
            :kind 'discovery
            :message "OpenRouter profile has no model-list endpoint"))
          (lambda () nil))
      (ende-provider--start-live-model-request
       profile url
       (lambda (payload request-error)
         (if request-error
             (ende-provider--invoke-reasoning-effort-callback
              callback nil nil request-error)
           (condition-case parse-error
               (let* ((entry
                       (cl-find model (ende-provider--model-entries payload)
                                :key (lambda (value)
                                       (ende-protocol-json-get "id" value))
                                :test #'equal))
                      (reasoning (and entry
                                      (ende-protocol-json-get
                                       "reasoning" entry)))
                      (efforts
                       (ende-provider--json-string-list
                        (and reasoning
                             (ende-protocol-json-get
                              "supported_efforts" reasoning))))
                      (default (and reasoning
                                    (ende-protocol-json-get
                                     "default_effort" reasoning))))
                 (if efforts
                     (ende-provider--invoke-reasoning-effort-callback
                      callback efforts
                      (and (stringp default) (member default efforts) default)
                      nil)
                   (ende-provider--invoke-reasoning-effort-callback
                    callback nil nil
                    (ende-error-create
                     :kind 'discovery
                     :message
                     (format "OpenRouter does not advertise explicit reasoning efforts for %s"
                             model)))))
             ((error quit)
              (ende-provider--invoke-reasoning-effort-callback
               callback nil nil
               (ende-error-create
                :kind 'protocol
                :message (error-message-string parse-error)))))))))))

(defun ende-provider-probe-reasoning-efforts (profile model callback)
  "Query PROFILE live for MODEL's supported reasoning efforts.
CALLBACK receives (EFFORTS DEFAULT ERROR).  Unlike model discovery, this never
uses or updates a cache."
  (let ((probe (plist-get profile :reasoning-effort-discovery-function)))
    (cond
     ((not ende-provider-discovery-enabled)
      (ende-provider--invoke-reasoning-effort-callback
       callback nil nil
       (ende-error-create :kind 'discovery
                          :message "Provider model discovery is disabled"))
      (lambda () nil))
     ((not (functionp probe))
      (ende-provider--invoke-reasoning-effort-callback
       callback nil nil
       (ende-error-create
        :kind 'discovery
        :message "Provider has no live reasoning-effort endpoint"))
      (lambda () nil))
     (t
      (condition-case probe-error
          (or (funcall
               probe profile model
               (lambda (efforts default error)
                 (ende-provider--invoke-reasoning-effort-callback
                  callback efforts default error)))
              (lambda () nil))
        ((error quit)
         (ende-provider--invoke-reasoning-effort-callback
          callback nil nil
          (ende-error-create :kind 'startup
                             :message (error-message-string probe-error)))
         (lambda () nil)))))))

(defun ende-provider-clear-discovery-cache ()
  "Clear cached provider discovery results."
  (interactive)
  (ende-provider-abort-discovery)
  (clrhash ende-provider--discovery-cache)
  (clrhash ende-provider--model-cache)
  (cl-incf ende-provider--model-generation)
  (message "ende: provider discovery cache cleared"))

(defun ende-provider-abort-discovery ()
  "Cancel all in-flight provider discovery requests."
  (let (keys)
    (maphash (lambda (key _cancel) (push key keys))
             ende-provider--model-cancellations)
    (dolist (key keys)
      (let ((cancel (gethash key ende-provider--model-cancellations))
            (callbacks (gethash key ende-provider--model-inflight)))
        (remhash key ende-provider--model-cancellations)
        (remhash key ende-provider--model-inflight)
        (when cancel
          (ende-ignore-errors cancel))
        (dolist (callback callbacks)
          (ende-provider--invoke-model-callback callback nil)))))
  (dolist (process (copy-sequence ende-provider--discovery-processes))
    (let ((finish (and (processp process)
                       (process-get process :ende-discovery-finish))))
      (if finish
          (funcall finish nil nil t)
        (when (and (processp process) (process-live-p process))
          (ende-ignore-errors #'delete-process process)))))
  (setq ende-provider--discovery-processes nil))

(provide 'ende-provider)

;;; ende-provider.el ends here
