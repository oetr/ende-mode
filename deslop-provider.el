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

(defconst deslop-provider--default-model "qwen3.8-27b"
  "Built-in model for the local provider profile.")

(defconst deslop-provider--default-openrouter-model "openai/gpt-4o-mini"
  "Built-in model for the OpenRouter profiles.")

(defconst deslop-provider--default-codex-model "gpt-5.6-luna"
  "Built-in model for the OpenAI/Codex profile.")

(defcustom deslop-provider-endpoint "http://localhost:8080/v1/chat/completions"
  "OpenAI-compatible chat completions endpoint."
  :type 'string
  :group 'deslop)

(defcustom deslop-provider-model deslop-provider--default-model
  "Model name sent to the LLM server."
  :type 'string
  :group 'deslop)

(defcustom deslop-provider-temperature 0.8
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
UI.  :max-concurrency limits parallel jobs for the provider, and :parallel
enables parallel jobs by default.  :source-name-disclosure overrides
`deslop-source-name-disclosure' for operation prompts."
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

;;;; Profiles and display policy

(defvar deslop-provider--runtime-settings (make-hash-table :test #'eq)
  "Session-level provider settings changed from the selector.")

(defun deslop-provider--runtime-value (name key)
  "Return runtime setting KEY for provider NAME, or `:unset'."
  (let ((settings (gethash name deslop-provider--runtime-settings)))
    (if (plist-member settings key)
        (plist-get settings key)
      :unset)))

(defun deslop-provider-set-runtime-setting (name key value)
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
                   (gethash name deslop-provider--runtime-settings))))
    (puthash name (plist-put settings key value)
             deslop-provider--runtime-settings))
  value)

(defun deslop-provider-clear-runtime-settings ()
  "Clear all session-level provider settings."
  (clrhash deslop-provider--runtime-settings))

(defun deslop-provider-profile (&optional name)
  "Return a non-secret provider profile for NAME.

 The URL/model customizations remain the local-provider defaults.  OpenRouter
 provides both synchronous and Batch API profile templates and requires an
 auth-source entry before it can be dispatched."
  (let* ((name (or name (car deslop-providers)
                   deslop-provider-default-profile))
         (base
          (pcase name
            ('openai
             (list :url deslop-codex-endpoint
                   :model deslop-codex-model
                   :model-discovery-key
                   (list deslop-codex-models-endpoint
                         deslop-codex-client-version
                         (expand-file-name deslop-codex-auth-file))
                   :max-concurrency 4
                    :parallel nil
                    :wire-api 'responses
                    :auth-method 'opencode-oauth
                    :model-discovery-function #'deslop-codex-probe-models
                    :require-sse t
                   :display-name "OpenAI"))
             ('openrouter
              '(:url "https://openrouter.ai/api/v1/chat/completions"
                :batch-url "https://openrouter.ai/api/v1/batches"
                :batch-endpoint "/v1/chat/completions"
                :transport auto
                :model "openai/gpt-4o-mini"
                :max-concurrency 4
                :parallel nil
                :temperature 0.2
                :auth-source-host "openrouter.ai"
                :auth-source-user "deslop"
                :auth-source-port 443
                :require-sse t))
             ('openrouter-batch
              '(:url "https://openrouter.ai/api/v1/chat/completions"
                :batch-url "https://openrouter.ai/api/v1/batches"
                :batch-endpoint "/v1/chat/completions"
                :transport openrouter-batch
                :model "openai/gpt-4o-mini"
                :max-concurrency 16
                :parallel t
                :temperature 0.2
                :auth-source-host "openrouter.ai"
                :auth-source-user "deslop"
                :auth-source-port 443
                :require-sse nil))
            (_
             (list :url deslop-provider-endpoint
                   :model deslop-provider-model
                   :max-concurrency 1
                   :parallel nil
                   :temperature deslop-provider-temperature
                   :require-sse t))))
         (custom (alist-get name deslop-provider-profiles)))
    (let ((profile (deslop-plist-merge
                    (plist-put (copy-sequence base) :name name)
                    custom)))
      (setq profile
            (plist-put
             profile :model-explicit-p
             (or (and (plist-member custom :model) t)
                 (pcase name
                   ('openai
                    (not (equal deslop-codex-model
                                deslop-provider--default-codex-model)))
                   ('openrouter nil)
                   ('openrouter-batch nil)
                   (_ (not (equal deslop-provider-model
                                  deslop-provider--default-model)))))))
      (setq profile
            (plist-put profile :models-explicit-p
                       (and (plist-member custom :models) t)))
      (dolist (key '(:max-concurrency :parallel))
        (let ((value (deslop-provider--runtime-value name key)))
          (unless (eq value :unset)
            (setq profile (plist-put profile key value)))))
      profile)))

(defun deslop-provider-max-concurrency (profile)
  "Return the positive maximum concurrent jobs allowed by PROFILE."
  (let* ((runtime (deslop-provider--runtime-value
                   (plist-get profile :name) :max-concurrency))
         (value (if (eq runtime :unset)
                    (or (plist-get profile :max-concurrency) 1)
                  runtime)))
    (unless (and (integerp value) (> value 0))
      (error "Provider max concurrency must be a positive integer"))
    value))

(defun deslop-provider-parallel-enabled-p (profile)
  "Return non-nil when PROFILE enables parallel jobs by default."
  (eq (plist-get profile :parallel) t))

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

(defun deslop-provider-configured-model (profile)
  "Return PROFILE's explicitly configured model, or nil.

Built-in profile defaults are deliberately not preferences."
  (and (plist-get profile :model-explicit-p)
       (plist-get profile :model)))

(defun deslop-provider-configured-models-explicit-p (profile)
  "Return non-nil when PROFILE has an explicitly configured model list."
  (plist-get profile :models-explicit-p))

(defun deslop-provider-profile-snapshot (&optional name)
  "Return a detached, non-secret snapshot of provider profile NAME."
  (let ((snapshot (copy-tree (deslop-provider-profile name))))
    (dolist (key '(:token :api-key :secret :authorization :auth-token))
      (setq snapshot (plist-put snapshot key nil)))
    snapshot))

(defun deslop-provider-source-name-disclosure-p (profile)
  "Return non-nil when source names may be included for PROFILE."
  (let ((value (if (plist-member profile :source-name-disclosure)
                   (plist-get profile :source-name-disclosure)
                 deslop-source-name-disclosure)))
    (unless (memq value '(nil t))
      (error "Source name disclosure must be boolean"))
    value))

(defun deslop-provider--friendly-name (name)
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

(defvar deslop-provider--model-cache (make-hash-table :test #'equal)
  "Cached provider model catalogs keyed by model-list endpoint identity.")

(defvar deslop-provider--discovery-inflight (make-hash-table :test #'equal)
  "Callbacks waiting for an in-flight provider discovery request.")

(defvar deslop-provider--model-inflight (make-hash-table :test #'equal)
  "Callbacks waiting for an in-flight model catalog request.")

(defvar deslop-provider--model-cancellations (make-hash-table :test #'equal)
  "Underlying cancellation functions for in-flight model catalog requests.")

(defvar deslop-provider--discovery-processes nil
  "Live provider discovery processes available for explicit cancellation.")

(defvar deslop-provider--model-generation 0
  "Monotonic generation for cached provider model catalogs.")

(defconst deslop-provider--model-pricing-fields
  '(("prompt" . :prompt)
    ("completion" . :completion)
    ("internal_reasoning" . :internal-reasoning)
    ("input_cache_read" . :input-cache-read)
    ("input_cache_write" . :input-cache-write)
    ("input_cache_write_1h" . :input-cache-write-1h))
  "JSON pricing fields retained for discovered models.")

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

(defun deslop-provider--validate-discovery-options ()
  "Validate provider discovery cache and resource options."
  (dolist (option
           (list (cons 'deslop-provider-discovery-ttl
                       deslop-provider-discovery-ttl)
                 (cons 'deslop-provider-discovery-timeout
                       deslop-provider-discovery-timeout)))
    (unless (or (null (cdr option))
                (and (numberp (cdr option)) (> (cdr option) 0)))
      (error "%s must be nil or a positive number" (car option))))
  (dolist (option
           (list (cons 'deslop-provider-discovery-cache-size
                       deslop-provider-discovery-cache-size)
                 (cons 'deslop-provider-max-discovery-bytes
                       deslop-provider-max-discovery-bytes)))
    (unless (or (null (cdr option))
                (and (integerp (cdr option)) (> (cdr option) 0)))
      (error "%s must be nil or a positive integer" (car option))))
  t)

(defun deslop-provider-models-url (profile)
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

(defun deslop-provider-model-discovery-available-p (profile)
  "Return non-nil when PROFILE has dynamic model discovery."
  (or (functionp (plist-get profile :model-discovery-function))
      (deslop-provider-models-url profile)))

(defun deslop-provider--model-cache-key (profile)
  "Return the model catalog cache key for PROFILE.

The selected model is intentionally absent.  Changing it must revalidate the
selection without starting another model-list request."
  (let ((url (deslop-provider-models-url profile))
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

(defun deslop-provider--model-entries (payload)
  "Return model entries from discovery PAYLOAD as a list."
  (let ((data (deslop-protocol-json-get "data" payload)))
    (cond
     ((vectorp data) (append data nil))
     ((listp data) data))))

(defun deslop-provider--model-pricing (entry)
  "Return supported pricing fields from model ENTRY."
  (let ((pricing (deslop-protocol-json-get "pricing" entry))
        (result nil))
    (when (hash-table-p pricing)
      (dolist (field deslop-provider--model-pricing-fields)
        (let ((value (gethash (car field) pricing)))
          (when (or (stringp value) (numberp value))
            (setq result
                  (plist-put result (cdr field) (format "%s" value)))))))
    result))

(defun deslop-provider--model-detail (entry)
  "Return display metadata for model ENTRY, or nil without a valid ID."
  (let ((id (deslop-protocol-json-get "id" entry))
        (name (deslop-protocol-json-get "name" entry)))
    (when (and (stringp id) (not (string-empty-p id)))
      (list :id id
            :name (and (stringp name) (not (string-empty-p name)) name)
            :pricing (deslop-provider--model-pricing entry)))))

(defun deslop-provider--discovery-metadata (profile payload)
  "Extract display metadata for PROFILE from discovery PAYLOAD."
  (let* ((entries (deslop-provider--model-entries payload))
         (details (delq nil (mapcar #'deslop-provider--model-detail entries)))
         (models (mapcar (lambda (detail) (plist-get detail :id)) details))
         (requested (plist-get profile :model))
         (match (cl-find-if
                 (lambda (entry)
                    (or (null requested)
                        (equal (deslop-protocol-json-get "id" entry)
                               requested)))
                 entries))
          (provider (or (deslop-protocol-json-get "provider" payload)
                        (deslop-protocol-json-get "server" payload)))
          (model (or (and match (deslop-protocol-json-get "id" match))
                    requested))
          (metadata (list :source :discovery
                          :model model
                          :models models
                          :model-details details)))
    (when (or match (stringp provider) models)
      (when (stringp provider)
        (setq metadata (plist-put metadata :provider provider)))
      (when (and match
                  (stringp (deslop-protocol-json-get "owned_by" match)))
        (setq metadata
              (plist-put metadata :owner
                          (deslop-protocol-json-get "owned_by" match))))
      (when (or match (stringp provider))
        (setq metadata
              (plist-put metadata :client
                         (deslop-provider-client-name profile metadata))))
      metadata)))

(defun deslop-provider--parse-discovery-response (profile body headers)
  "Parse a discovery response in BODY using response HEADERS."
  (let ((status (car (deslop-curl-response-headers headers))))
    (when (and status (>= status 200) (< status 300)
               (buffer-live-p body))
      (with-current-buffer body
        (condition-case nil
            (deslop-provider--discovery-metadata
             profile (deslop-protocol-json-read-string (buffer-string)))
          ((error quit) nil))))))

(defun deslop-provider--curl-discovery-command
    (url headers-file config-file)
  "Build a GET command for provider discovery at URL."
  (append
   (deslop-curl-base-command url)
   (deslop-curl-response-options headers-file)
   (list "--request" "GET"
         "--header" "Accept: application/json")
   (when config-file (list "--config" config-file))
   (deslop-curl-timeout-options
    deslop-provider-discovery-timeout deslop-provider-max-discovery-bytes)
   (list "--url" url)))

(defun deslop-provider--invoke-discovery-callback (callback metadata)
  "Invoke discovery CALLBACK with METADATA without propagating its errors."
  (when callback
    (condition-case nil
        (funcall callback metadata)
      ((error quit) nil))))

(defun deslop-provider--discovery-process (key)
  "Return the in-flight discovery process for KEY, or nil."
  (cl-find-if
   (lambda (process)
     (and (processp process)
          (equal (process-get process :deslop-discovery-key) key)))
   deslop-provider--discovery-processes))

(defun deslop-provider--cancel-discovery-waiter (key callback)
  "Remove CALLBACK from discovery KEY and cancel an unused request."
  (let ((callbacks (gethash key deslop-provider--discovery-inflight))
        (removed nil)
        (remaining nil))
    (dolist (waiting callbacks)
      (if (and (not removed) (eq waiting callback))
          (setq removed t)
        (push waiting remaining)))
    (when removed
      (setq remaining (nreverse remaining))
      (if remaining
          (puthash key remaining deslop-provider--discovery-inflight)
        (remhash key deslop-provider--discovery-inflight)
        (let* ((process (deslop-provider--discovery-process key))
               (finish (and (processp process)
                            (process-get process :deslop-discovery-finish))))
          (when finish
            (funcall finish nil t)))))))

(defun deslop-provider--prune-cache (cache)
  "Remove expired and excess entries from CACHE."
  (let ((now (float-time))
        (expired nil))
    (when deslop-provider-discovery-ttl
      (maphash
       (lambda (key entry)
         (when (>= (- now (plist-get entry :time))
                    deslop-provider-discovery-ttl)
            (push key expired)))
        cache))
    (dolist (key expired)
      (remhash key cache))
    (when deslop-provider-discovery-cache-size
      (while (>= (hash-table-count cache)
                 (max 1 deslop-provider-discovery-cache-size))
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

(defun deslop-provider--prune-discovery-cache ()
  "Remove expired and excess provider discovery cache entries."
  (deslop-provider--prune-cache deslop-provider--discovery-cache))

(defun deslop-provider--cached-model-entry (key)
  "Return the usable model catalog entry for KEY, or nil when unavailable."
  (let* ((entry (gethash key deslop-provider--model-cache))
         (age (and entry (- (float-time) (plist-get entry :time)))))
    (if (and entry
             (null (plist-get entry :error))
             (consp (plist-get entry :models))
             (or (null deslop-provider-discovery-ttl)
                 (< age deslop-provider-discovery-ttl)))
        entry
      (when entry
        (remhash key deslop-provider--model-cache))
      nil)))

(defun deslop-provider--model-entry (profile)
  "Return the cached model catalog entry for PROFILE, or nil."
  (let ((key (deslop-provider--model-cache-key profile)))
    (and key (deslop-provider--cached-model-entry key))))

(defun deslop-provider-cached-models (profile)
  "Return a copy of PROFILE's cached model catalog, or nil.

The returned plist contains `:models', `:details', `:error', and
`:generation'.  This function never starts discovery."
  (let ((entry (deslop-provider--model-entry profile)))
    (when entry
      (list :models (copy-sequence (plist-get entry :models))
            :details (copy-tree (plist-get entry :details))
            :error (plist-get entry :error)
            :generation (plist-get entry :generation)))))

(defun deslop-provider-models-generation (profile)
  "Return the cached model catalog generation for PROFILE, or zero."
  (or (plist-get (deslop-provider--model-entry profile) :generation)
      deslop-provider--model-generation))

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

(defun deslop-provider--invoke-model-callback (callback entry)
  "Invoke model catalog CALLBACK with ENTRY without propagating errors."
  (condition-case nil
      (funcall callback
               (and entry (copy-sequence (plist-get entry :models)))
               (and entry (plist-get entry :error)))
    ((error quit) nil)))

(defun deslop-provider--finish-model-discovery
    (key models details error &optional no-cache)
  "Notify model catalog waiters for KEY and cache only successful results."
  (setq models (cl-remove-if-not #'stringp models))
  (when (and (null error) (null models))
    (setq error
          (deslop-error-create
           :kind 'discovery
           :message "Provider returned no models")))
  (let ((entry (and (not no-cache)
                    (list :models (copy-sequence models)
                          :details (copy-tree details)
                          :error error))))
    (when (and entry (null error))
      (deslop-provider--prune-cache deslop-provider--model-cache)
      (setq entry
            (plist-put entry :time (float-time)))
      (setq entry
            (plist-put entry :generation
                       (cl-incf deslop-provider--model-generation)))
      (puthash key entry deslop-provider--model-cache))
    (remhash key deslop-provider--model-cancellations)
    (let ((callbacks (gethash key deslop-provider--model-inflight)))
      (remhash key deslop-provider--model-inflight)
      (dolist (callback callbacks)
        (deslop-provider--invoke-model-callback callback entry)))))

(defun deslop-provider--cancel-model-waiter (key callback)
  "Remove model catalog CALLBACK from KEY and cancel an unused request."
  (let ((callbacks (gethash key deslop-provider--model-inflight))
        (removed nil)
        (remaining nil))
    (dolist (waiting callbacks)
      (if (and (not removed) (eq waiting callback))
          (setq removed t)
        (push waiting remaining)))
    (when removed
      (setq remaining (nreverse remaining))
      (if remaining
          (puthash key remaining deslop-provider--model-inflight)
        (remhash key deslop-provider--model-inflight)
        (let ((cancel (gethash key deslop-provider--model-cancellations)))
          (remhash key deslop-provider--model-cancellations)
          (when cancel
            (deslop-ignore-errors cancel)))))))

(defun deslop-provider--start-model-discovery (profile key)
  "Start the uncached model discovery request for PROFILE and KEY."
  (let ((probe (plist-get profile :model-discovery-function))
        (completed nil)
        (cancelled nil)
        (underlying-cancel nil))
    (cl-labels
        ((finish (models details error)
           (unless cancelled
             (setq completed t)
             (deslop-provider--finish-model-discovery
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
                  (deslop-provider-discover
                   profile
                   (lambda (metadata)
                     (unless cancelled
                       (let ((models (and metadata
                                          (plist-get metadata :models))))
                         (if (and (listp models) models)
                             (finish models
                                     (plist-get metadata :model-details)
                                     nil)
                           (remhash (deslop-provider--discovery-key profile)
                                    deslop-provider--discovery-cache)
                           (finish-error
                            (deslop-error-create
                             :kind 'discovery
                             :message "Provider returned no models")))))))))
        ((error quit)
         (finish-error
          (deslop-error-create
           :kind 'startup
           :message (error-message-string startup-error))))))
    (unless completed
      (puthash
       key
       (lambda ()
         (setq cancelled t)
         (when underlying-cancel
           (deslop-ignore-errors underlying-cancel)
           (setq underlying-cancel nil)))
       deslop-provider--model-cancellations))))

(defun deslop-provider-cached-metadata (profile)
  "Return unexpired discovery metadata for PROFILE, or nil."
  (let ((entry (deslop-provider--cached-entry
                (deslop-provider--discovery-key profile))))
    (and entry (plist-get entry :metadata))))

(defun deslop-provider-model-details (profile)
  "Return copied cached model details for PROFILE, or nil."
  (let ((catalog (deslop-provider-cached-models profile)))
    (copy-tree
     (or (plist-get catalog :details)
         (plist-get (deslop-provider-cached-metadata profile)
                    :model-details)))))

(defun deslop-provider--price-per-million (value)
  "Return numeric VALUE converted from dollars per token to dollars per million.
Return nil when VALUE is not a nonnegative decimal price string."
  (when (and (stringp value)
             (string-match-p
              "\\`[0-9]+\\(?:\\.[0-9]*\\)?\\(?:[eE][+-]?[0-9]+\\)?\\'"
              value))
    (* 1000000.0 (string-to-number value))))

(defun deslop-provider--format-price (value)
  "Format a dollars-per-token VALUE for a completion annotation."
  (let ((per-million (deslop-provider--price-per-million value)))
    (cond
     ((null per-million) nil)
     ((zerop per-million) "free")
     ((< per-million 0.1) (format "$%.3g" per-million))
     ((< per-million 1) (format "$%.2f" per-million))
     (t (format "$%.3g" per-million)))))

(defun deslop-provider-model-price-annotation (detail &optional part-function)
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
      (let ((price (deslop-provider--format-price
                    (plist-get pricing (car field)))))
        (when price
          (let ((part (format "%s: %s" (cdr field) price)))
            (push (if part-function
                      (funcall part-function (car field) part)
                    part)
                  parts)))))
    (when parts
      (concat "  " (mapconcat #'identity (nreverse parts) "  ")))))

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
        (stderr-process nil)
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
                (deslop-ignore-errors #'cancel-timer timer))
              (when (and (processp process) (process-live-p process))
                (process-put process :deslop-cancelled t)
                (deslop-ignore-errors #'delete-process process))
              (deslop-curl-cleanup-resources
               (list response-buf stderr-buf)
               (list headers-file config-file)
               (list stderr-process))
              (deslop-provider--finish-discovery key metadata no-cache))))
    (condition-case _startup
        (let ((request
               (deslop-curl-start-request
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
                 stderr-process (plist-get request :stderr-process)
                 headers-file (plist-get request :headers-file)
                 config-file (plist-get request :config-file))
           (process-put process :deslop-discovery-finish finish)
           (process-put process :deslop-discovery-key key)
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
    (deslop-validate-resource-options)
    (deslop-provider--validate-discovery-options)
    (if (not deslop-provider-discovery-enabled)
        (progn
          (deslop-provider--invoke-discovery-callback callback nil)
          nil)
      (let* ((key (deslop-provider--discovery-key profile))
             (entry (deslop-provider--cached-entry key))
             (url (deslop-provider-models-url profile)))
        (if entry
            (progn
              (deslop-provider--invoke-discovery-callback
               callback (plist-get entry :metadata))
              nil)
          (let ((waiting (gethash key deslop-provider--discovery-inflight)))
            (if waiting
                (puthash key (cons callback waiting)
                         deslop-provider--discovery-inflight)
              (puthash key (list callback)
                       deslop-provider--discovery-inflight)
              (if url
                  (deslop-provider--start-discovery profile key url)
                (deslop-provider--finish-discovery key nil)))
            (apply-partially #'deslop-provider--cancel-discovery-waiter
                             key callback)))))))

(defun deslop-provider-probe-models (profile callback)
  "Probe PROFILE's model-list endpoint and invoke CALLBACK with its models.
CALLBACK receives (MODELS ERROR).  Return a cancellation function that removes
this callback; an unused shared request is cancelled."
  (let ((key (deslop-provider--model-cache-key profile))
        (probe (plist-get profile :model-discovery-function)))
    (if (or (not key)
            (and (null probe) (not deslop-provider-discovery-enabled)))
        (progn
          (deslop-provider--invoke-model-callback
           callback
           (list :models nil
                 :error
                  (deslop-error-create
                   :kind 'discovery
                   :message (if (not key)
                                "Provider has no compatible model-list endpoint"
                              "Provider model discovery is disabled"))))
          (lambda () nil))
      (let ((entry (deslop-provider--cached-model-entry key)))
        (if entry
            (progn
              (deslop-provider--invoke-model-callback callback entry)
              (lambda () nil))
          (let ((waiting (gethash key deslop-provider--model-inflight)))
            (puthash key
                     (cons callback waiting)
                     deslop-provider--model-inflight)
            (unless waiting
              (deslop-provider--start-model-discovery profile key))
            (apply-partially #'deslop-provider--cancel-model-waiter
                             key callback)))))))

(defun deslop-provider-clear-discovery-cache ()
  "Clear cached provider discovery results."
  (interactive)
  (deslop-provider-abort-discovery)
  (clrhash deslop-provider--discovery-cache)
  (clrhash deslop-provider--model-cache)
  (cl-incf deslop-provider--model-generation)
  (message "deslop: provider discovery cache cleared"))

(defun deslop-provider-abort-discovery ()
  "Cancel all in-flight provider discovery requests."
  (let (keys)
    (maphash (lambda (key _cancel) (push key keys))
             deslop-provider--model-cancellations)
    (dolist (key keys)
      (let ((cancel (gethash key deslop-provider--model-cancellations))
            (callbacks (gethash key deslop-provider--model-inflight)))
        (remhash key deslop-provider--model-cancellations)
        (remhash key deslop-provider--model-inflight)
        (when cancel
          (deslop-ignore-errors cancel))
        (dolist (callback callbacks)
          (deslop-provider--invoke-model-callback callback nil)))))
  (dolist (process (copy-sequence deslop-provider--discovery-processes))
    (let ((finish (and (processp process)
                       (process-get process :deslop-discovery-finish))))
      (if finish
          (funcall finish nil t)
        (when (and (processp process) (process-live-p process))
          (deslop-ignore-errors #'delete-process process)))))
  (setq deslop-provider--discovery-processes nil))

(provide 'deslop-provider)

;;; deslop-provider.el ends here
