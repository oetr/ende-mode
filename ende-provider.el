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
(require 'ende-controls)
(require 'ende-provider-config)
(require 'savehist)

;;;; User options

(defconst ende-provider--deepseek-verified-models
  '("deepseek-flash" "deepseek-v4-pro")
  "DeepSeek models currently documented for the verified chat controls.")

(defun ende-provider-deepseek-model-verified-p (profile)
  "Return non-nil when PROFILE is the DeepSeek preset with a verified model."
  (and (eq (plist-get profile :service) 'deepseek)
       (member (plist-get profile :model)
               ende-provider--deepseek-verified-models)))

(defcustom ende-model-selection-scope 'buffer
  "Scope used to remember the selected provider, model, and control preferences.

When `buffer', selections are associated with the current buffer name and
visited file and are remembered for this Emacs session.  When `global', one
selection is shared by all buffers."
  :type '(choice (const :tag "Per buffer" buffer)
                 (const :tag "Global" global))
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

(defvar ende-model-selection--last-used nil
  "Most recently used provider, model, and optional control-preferences plist.")

(defvar ende-model-selection--buffer-selections (make-hash-table :test #'equal)
  "Selections keyed by buffer name and visited file for this Emacs session.")

(defvar ende-model-selection--shared-control-preferences
  (make-hash-table :test #'equal)
  "Control preferences keyed by provider/model/binding for this Emacs session.")

(defvar ende-model-selection--buffer-control-preferences
  (make-hash-table :test #'equal)
  "Per-buffer control-preference tables keyed by buffer identity.")

(defvar ende-model-selection--provenance (make-hash-table :test #'equal)
  "Nonsecret bindings captured when explicit selections are saved.")

(dolist (variable '(ende-model-selection--last-used ende-model-selection--buffer-selections
                    ende-model-selection--shared-control-preferences
                    ende-model-selection--buffer-control-preferences ende-model-selection--provenance))
  (add-to-list 'savehist-additional-variables variable))

(defun ende-provider-selection-provenance (profile)
  "Return PROFILE's nonsecret credential, endpoint, API and catalog identity."
  (mapcar (lambda (key) (cons key (ende-provider-config-copy (plist-get profile key))))
          '(:service :api :url :auth-method :account :auth-source-host
            :auth-source-port :auth-source-user :models)))

(defun ende-model-selection--provenance-key (selection)
  "Return the stable lookup identity of SELECTION."
  (list (plist-get selection :provider) (plist-get selection :api)
        (plist-get selection :model)))

(defvar-local ende-model-selection--buffer-selection nil
  "Selection currently associated with this live buffer.")

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

(defun ende-provider--compose-profile (name model-override &optional api)
  "Resolve NAME and MODEL-OVERRIDE before capturing a request snapshot."
  (let* ((name (or name (car ende-providers)))
         (profile (ende-provider-config-resolve name model-override api)))
    (pcase (plist-get profile :service)
      ('openai-codex
       (unless (eq (plist-get profile :account) 'default)
         (user-error "Independent Codex accounts require an authorized login registration"))
       (setq profile (ende-plist-merge
                      profile
                      (list :model-discovery-function #'ende-codex-probe-models
                            :model-discovery-key
                            (list ende-codex-models-endpoint ende-codex-client-version
                                  (expand-file-name ende-codex-auth-file))
                            :reasoning-effort-probe-function #'ende-codex-probe-reasoning-efforts))))
      ('openrouter
       (setq profile (ende-plist-merge
                      profile
                      (list :batch-url "https://openrouter.ai/api/v1/batches"
                            :batch-endpoint "/v1/chat/completions"
                            :transport (or (plist-get profile :transport) 'auto)
                            :request-headers (copy-sequence ende-provider--openrouter-request-headers)
                            :reasoning-effort-probe-function #'ende-provider--probe-openrouter-reasoning-efforts)))))
    (dolist (key '(:max-concurrency :parallel))
      (let ((value (ende-provider--runtime-value name key)))
        (unless (eq value :unset)
          (setq profile (plist-put profile key value)))))
    profile))

(defun ende-provider-profile (&optional name)
  "Return a non-secret, composed provider profile for NAME."
  (ende-provider--compose-profile name nil))

(defun ende-provider-profile-for-model (name model &optional api)
  "Return composed provider NAME specialized for selected MODEL."
  (ende-provider--compose-profile name model api))

(defun ende-provider-control-descriptor (profile control)
  "Return PROFILE's descriptor for CONTROL, or nil."
  (cdr (assq control (plist-get profile :controls))))

(defun ende-provider--validate-control-preferences (preferences)
  "Validate and return explicit control PREFERENCES."
  (unless (ende-controls--proper-plist-p preferences)
    (error "Control preferences must be a plist"))
  (cl-loop for (control value) on preferences by #'cddr do
           (unless (memq control ende-controls--names)
             (error "Unknown control preference: %S" control))
           (ende-controls--validate-canonical-value
            control value "Control preference"))
  preferences)

(defun ende-provider-profile-with-control-preferences (profile preferences)
  "Return PROFILE with explicit control PREFERENCES applied.
Present preference keys override PROFILE even when their value is nil."
  (ende-provider--validate-control-preferences preferences)
  (let ((resolved (copy-tree profile)))
    (dolist (control ende-controls--names)
      (when (plist-member preferences control)
        (setq resolved
              (plist-put resolved control (plist-get preferences control))
              resolved (plist-put resolved :provenance
                                  (plist-put (plist-get resolved :provenance) control 'selector)))))
    resolved))

(defun ende-provider-profile-for-selection (name model preferences &optional api)
  "Resolve provider NAME and MODEL with explicit control PREFERENCES last."
  (ende-provider-profile-with-control-preferences
   (ende-provider-profile-for-model name model api) preferences))

(defun ende-provider-control-setting (profile preferences control)
  "Return effective CONTROL value, source, and documented provider default."
  (unless (memq control ende-controls--names)
    (error "Unknown control: %S" control))
  (let* ((explicit (plist-member preferences control))
         (inherited (plist-member profile control))
         (defaults (plist-get profile :control-defaults))
         (default-known (plist-member defaults control)))
    (list :value (cond
                  (explicit (plist-get preferences control))
                  (inherited (plist-get profile control)))
          :source (cond
                   (explicit 'explicit)
                   (inherited 'profile)
                   (t 'provider-default))
          :provider-default (and default-known
                                 (plist-get defaults control))
          :provider-default-known default-known)))

(defun ende-provider-active-reasoning-effort (profile)
  "Return PROFILE's active effort, or nil when thinking is off."
  (and (not (eq (plist-get profile :thinking) 'off))
       (plist-get profile :reasoning-effort)))

(defun ende-provider-control-compatibility-error (profile)
  "Return a message when PROFILE requests a verified incompatible control pair."
  (when (and (eq (plist-get profile :service) 'deepseek)
             (member (plist-get profile :model)
                     ende-provider--deepseek-verified-models)
             (eq (plist-get profile :thinking) 'on)
             (equal (plist-get profile :reasoning-effort) "none"))
    "DeepSeek reasoning-effort none disables thinking and conflicts with :thinking on"))

(defun ende-provider-validate-controls (profile)
  "Validate the active control values and mappings in PROFILE."
  (ende-controls-validate-descriptors (plist-get profile :controls))
  (when-let* ((message (ende-provider-control-compatibility-error profile)))
    (error "%s" message))
  (ende-controls-encode nil (plist-get profile :controls) profile)
  t)

(defun ende-provider-validate-profile (profile)
  "Validate request configuration in PROFILE before network startup."
  (unless (assq (plist-get profile :api) ende-provider-config--apis)
    (error "Select a supported provider :api"))
  (let ((model (plist-get profile :model)))
    (unless (and (stringp model) (not (string-empty-p model)))
      (error "Provider profile must select a nonempty model")))
  (when (and (plist-member profile :stream-usage)
             (not (memq (plist-get profile :stream-usage) '(nil t))))
    (error ":stream-usage must be boolean"))
  (when (and (plist-member profile :image-input)
             (not (memq (plist-get profile :image-input) '(nil t))))
    (error ":image-input must be boolean"))
  (ende-provider-config-parameters (plist-get profile :parameters)
                                    ende-provider-config--protected-paths)
  (ende-provider-validate-controls profile)
  (let ((url (plist-get profile :url)))
    (unless (and (stringp url) (not (string-empty-p url)))
      (error "Provider profile must include a nonempty :url"))
    (if (eq (plist-get profile :service) 'openai-codex)
        (ende-codex--validated-generation-url url)
      (ende-curl--validated-url profile)))
  t)

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

(defun ende-provider-names ()
  "Return configured provider profile names for the selector."
  (ende-provider-config-validate)
  (copy-sequence ende-providers))

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
  (car (plist-get profile :models)))

(defun ende-provider-configured-models-explicit-p (profile)
  "Return non-nil when PROFILE has an explicitly configured model list."
  (plist-get profile :models-explicit-p))

(defun ende-provider-profile-snapshot (&optional name)
  "Return a detached, non-secret snapshot of provider profile NAME."
  (ende-provider-config-copy (ende-provider-profile name)))

(defun ende-model-selection--file-identity (file)
  "Return a stable lookup spelling for visited FILE, or nil.

Local paths are resolved through symlinks when possible.  Remote paths are
kept lexical so recording a preference never initiates a TRAMP connection."
  (when file
    (if (file-remote-p file)
        file
      (condition-case nil
          (file-truename file)
        (error (expand-file-name file))))))

(defun ende-model-selection--buffer-key (&optional buffer)
  "Return the preference key for BUFFER, or nil if it is not live."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (list (buffer-name)
              (ende-model-selection--file-identity buffer-file-name))))))

(defun ende-model-selection--normalize (selection)
  "Return a detached provider/model/control selection, or nil.

SELECTION must contain a provider symbol and a nonempty model string."
  (when (listp selection)
    (let* ((provider (plist-get selection :provider))
           (model (plist-get selection :model))
           (preferences-present
            (plist-member selection :control-preferences))
           (preferences
            (and preferences-present
                 (copy-tree
                  (ende-provider--validate-control-preferences
                   (plist-get selection :control-preferences))))))
      (when (and (symbolp provider) provider
                 (stringp model) (not (string-empty-p model)))
        (append (list :provider provider :model model)
                (when (plist-get selection :api) (list :api (plist-get selection :api)))
                (when preferences-present
                  (list :control-preferences preferences)))))))

(defun ende-model-selection--pair-key (provider model api)
  "Return the preference-history key for PROVIDER, MODEL and API, or nil.
Nil API resolves to PROVIDER's configured default API."
  (and (symbolp provider) provider
       (stringp model) (not (string-empty-p model))
       (let* ((profile (condition-case nil (ende-provider-profile-for-model provider model api)
                         (error nil)))
              (binding (when profile
                         (cl-remove-if (lambda (entry) (eq (car entry) :models))
                                       (ende-provider-selection-provenance profile)))))
         (list provider (copy-sequence model) binding))))

(defun ende-model-selection--buffer-preference-table (buffer &optional create)
  "Return BUFFER's pair-preference table, creating it when CREATE is non-nil."
  (let ((identity (ende-model-selection--buffer-key buffer)))
    (when identity
      (or (gethash identity ende-model-selection--buffer-control-preferences)
          (when create
            (let ((table (make-hash-table :test #'equal)))
              (puthash identity table
                       ende-model-selection--buffer-control-preferences)
              table))))))

(defun ende-model-selection--control-preference-entry
    (provider model api &optional buffer)
  "Return (FOUND . PREFERENCES) for PROVIDER, MODEL and API.
Nil API resolves to PROVIDER's configured default API.  An empty preference
plist is a found entry.  BUFFER defaults to the current buffer."
  (let* ((pair (ende-model-selection--pair-key provider model api))
         (buffer (or buffer (current-buffer)))
         (missing (make-symbol "missing"))
         entry)
    (when pair
      (pcase ende-model-selection-scope
        ('global
         (let ((preferences
                (gethash pair ende-model-selection--shared-control-preferences
                         missing)))
           (unless (eq preferences missing)
             (setq entry (cons t preferences)))))
        ('buffer
         (let* ((table (ende-model-selection--buffer-preference-table buffer))
                (preferences (if table (gethash pair table missing) missing)))
           (setq entry
                 (if (not (eq preferences missing))
                     (cons t preferences)
                   (let ((shared
                          (gethash pair
                                   ende-model-selection--shared-control-preferences
                                   missing)))
                     (unless (eq shared missing)
                       (cons t shared)))))))
        (_ (error "Invalid `ende-model-selection-scope': %s"
                  ende-model-selection-scope))))
    (when entry
      (cons t (copy-tree (cdr entry))))))

(defun ende-model-selection-control-preferences (provider model api &optional buffer)
  "Return remembered controls for PROVIDER, MODEL and API, or nil.
Nil API resolves to PROVIDER's configured default API.
BUFFER defaults to the current buffer."
  (cdr (ende-model-selection--control-preference-entry
        provider model api buffer)))

(defun ende-model-selection-save-control-preferences
    (provider model api preferences &optional buffer)
  "Save explicit control PREFERENCES for PROVIDER, MODEL and API.
Nil API resolves to PROVIDER's configured default API.
In buffer scope save a detached copy for BUFFER and also update shared history.
In global scope update only shared history."
  (let* ((pair (ende-model-selection--pair-key provider model api))
         (buffer (or buffer (current-buffer)))
         (preferences
          (copy-tree (ende-provider--validate-control-preferences preferences))))
    (unless pair
      (error "Control preferences require a provider and model"))
    (unless (memq ende-model-selection-scope '(buffer global))
      (error "Invalid `ende-model-selection-scope': %s"
             ende-model-selection-scope))
    (puthash pair (copy-tree preferences)
             ende-model-selection--shared-control-preferences)
    (when (and (eq ende-model-selection-scope 'buffer)
               (buffer-live-p buffer))
      (puthash pair (copy-tree preferences)
               (ende-model-selection--buffer-preference-table buffer t)))
    (copy-tree preferences)))

(defun ende-model-selection-remember-pair (provider model api &optional buffer)
  "Record PROVIDER, MODEL and API as selected and return its preferences.
Nil API resolves to PROVIDER's configured default API.
On first selection, snapshot the inherited settings into BUFFER's history.
BUFFER defaults to the current buffer."
  (let* ((pair (ende-model-selection--pair-key provider model api))
         (buffer (or buffer (current-buffer)))
         (entry (ende-model-selection--control-preference-entry
                 provider model api buffer))
         (preferences (copy-tree (cdr entry))))
    (unless pair
      (error "A model selection requires a provider and model"))
    (unless entry
      (setq preferences nil))
    (ende-model-selection-save-control-preferences
     provider model api preferences buffer)
    (copy-tree preferences)))

(defun ende-model-selection-initialize-buffer (&optional buffer)
  "Initialize BUFFER's per-buffer selection when one can be inherited.

An exact identity match takes precedence over the most recently used
selection.  With neither available, leave the buffer unset so normal provider
defaults remain authoritative."
  (let ((buffer (or buffer (current-buffer))))
    (when (and (buffer-live-p buffer)
               (eq ende-model-selection-scope 'buffer))
      (with-current-buffer buffer
        (let* ((key (ende-model-selection--buffer-key buffer))
               (selection
                (or ende-model-selection--buffer-selection
                    (gethash key ende-model-selection--buffer-selections)
                    ende-model-selection--last-used)))
          (when (setq ende-model-selection--buffer-selection
                      (ende-model-selection--normalize selection))
            (puthash key
                     (copy-tree ende-model-selection--buffer-selection)
                     ende-model-selection--buffer-selections)))))
    (ende-model-selection-for-buffer buffer)))

(defun ende-model-selection-for-buffer (&optional buffer)
  "Return the remembered selection applicable to BUFFER, or nil."
  (let ((buffer (or buffer (current-buffer))))
    (pcase ende-model-selection-scope
      ('global (copy-tree
                (ende-model-selection--normalize
                 ende-model-selection--last-used)))
      ('buffer
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (or (copy-tree ende-model-selection--buffer-selection)
               (copy-tree
                (gethash (ende-model-selection--buffer-key buffer)
                         ende-model-selection--buffer-selections))
               (copy-tree
                (ende-model-selection--normalize
                 ende-model-selection--last-used))))))
      (_ (error "Invalid `ende-model-selection-scope': %s"
                ende-model-selection-scope)))))

(defun ende-model-selection-save (selection &optional buffer skip-preference-history)
  "Save SELECTION as the most recently used choice for BUFFER.

In per-buffer scope also associate it with BUFFER's name and visited file.
When SKIP-PREFERENCE-HISTORY is non-nil, save only the active selection."
  (let* ((selection (ende-model-selection--normalize selection))
         (buffer (or buffer (current-buffer))))
    (unless selection
      (error "A model selection requires a provider and model"))
    (unless (memq ende-model-selection-scope '(buffer global))
      (error "Invalid `ende-model-selection-scope': %s"
             ende-model-selection-scope))
    (unless skip-preference-history
      (if (plist-member selection :control-preferences)
          (ende-model-selection-save-control-preferences
           (plist-get selection :provider) (plist-get selection :model)
           (plist-get selection :api)
           (plist-get selection :control-preferences) buffer)
        (ende-model-selection-remember-pair
         (plist-get selection :provider) (plist-get selection :model)
         (plist-get selection :api) buffer)))
    (let ((profile (condition-case nil
                       (ende-provider--compose-profile
                        (plist-get selection :provider) (plist-get selection :model)
                        (plist-get selection :api))
                     (error nil))))
      (when profile
        (puthash (ende-model-selection--provenance-key selection)
                 (ende-provider-selection-provenance profile) ende-model-selection--provenance)))
    (setq ende-model-selection--last-used (copy-tree selection))
    (pcase ende-model-selection-scope
      ('global nil)
      ('buffer
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (setq ende-model-selection--buffer-selection
                 (copy-tree selection))
           (puthash (ende-model-selection--buffer-key buffer)
                    (copy-tree selection)
                    ende-model-selection--buffer-selections))))
      (_ (error "Invalid `ende-model-selection-scope': %s"
                ende-model-selection-scope)))
    (copy-tree selection)))

(defun ende-provider-profile-for-buffer (&optional buffer remember)
  "Return BUFFER's selected provider profile snapshot.

When REMEMBER is non-nil, record the selected provider/model and explicit
control preferences, not inherited values.
An unset buffer follows the current configured provider defaults."
  (let* ((buffer (or buffer (current-buffer)))
         (_ (ende-model-selection-initialize-buffer buffer))
         (selection (ende-model-selection-for-buffer buffer))
         (provider (or (plist-get selection :provider)
                       (car ende-providers)))
         (api (plist-get selection :api))
         (configured-profile (ende-provider--compose-profile provider nil api))
         (model (or (plist-get selection :model)
                    (plist-get configured-profile :model)))
         (matching-selection-p
          (and (eq provider (plist-get selection :provider))
               (equal model (plist-get selection :model))))
         (selection-preferences-p
          (and matching-selection-p
               (plist-member selection :control-preferences)))
         (preferences
          (ende-model-selection-control-preferences provider model api buffer))
         (profile (ende-provider-profile-for-selection
                   provider model preferences api)))
    (let ((provenance (gethash (ende-model-selection--provenance-key selection)
                               ende-model-selection--provenance)))
      (when (and provenance (not (equal provenance (ende-provider-selection-provenance profile))))
        (user-error "Provider binding or catalog changed; select the provider and model again")))
    (when (and matching-selection-p
               (not (ende-provider-configured-models-explicit-p
                     configured-profile)))
      (when-let* ((catalog (ende-provider-cached-models configured-profile))
                  (models (plist-get catalog :models)))
        (unless (member model models)
          (user-error "Model %s is no longer available; select a model again"
                      model))))
    (unless model
      (let* ((catalog (ende-provider-cached-models configured-profile))
             (models (plist-get catalog :models)))
        (if (= (length models) 1)
            (setq model (car models)
                  profile (ende-provider-profile-for-selection provider model preferences api))
          (user-error "Model selection required for %s; open ende-selector-open" provider))))
    (when (and remember (stringp model))
      (ende-model-selection-save
       (append (list :provider provider :model model)
               (when api (list :api api))
               (when selection-preferences-p
                 (list :control-preferences
                       (copy-tree preferences))))
       buffer))
    profile))

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

(defvar ende-provider--catalog-cache (make-hash-table :test #'equal)
  "Successful normalized model catalogs keyed by endpoint/provider identity.")

(defvar ende-provider--discovery-failures (make-hash-table :test #'equal)
  "Runtime-only discovery failure timestamps keyed by catalog identity.")

(defvar ende-provider--catalog-inflight (make-hash-table :test #'equal)
  "Shared catalog requests and their consumer waiters.")

(defvar ende-provider--discovery-processes nil
  "Live provider discovery processes available for explicit cancellation.")

(defvar ende-provider--model-generation 0
  "Monotonic generation for cached provider model catalogs.")

(cl-defstruct (ende-provider--catalog-flight
               (:constructor ende-provider--make-catalog-flight))
  "One in-flight model-list request and all its independent consumers."
  waiters cancel)

(defconst ende-provider--model-pricing-fields
  '(("prompt" . :prompt)
    ("completion" . :completion)
    ("internal_reasoning" . :internal-reasoning)
    ("input_cache_read" . :input-cache-read)
    ("input_cache_write" . :input-cache-write)
    ("input_cache_write_1h" . :input-cache-write-1h))
  "JSON pricing fields retained for discovered models.")

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
  (plist-get profile :models-url))

(defun ende-provider-model-discovery-available-p (profile)
  "Return non-nil when PROFILE has dynamic model discovery."
  (or (functionp (plist-get profile :model-discovery-function))
      (ende-provider-models-url profile)))

(defun ende-provider--catalog-key (profile)
  "Return the model catalog key for PROFILE.

The selected model is intentionally absent.  Changing it must revalidate the
selection without starting another model-list request."
  (let ((url (ende-provider-models-url profile))
        (probe (plist-get profile :model-discovery-function)))
    (when (or url (functionp probe))
      (list (plist-get profile :name) (plist-get profile :service)
            (plist-get profile :api) (plist-get profile :account)
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

(defun ende-provider--prune-cache (cache)
  "Remove expired and excess entries from CACHE."
  (let ((now (float-time))
        (expired nil))
    (when (and ende-provider-discovery-ttl (not (eq cache ende-provider--catalog-cache)))
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

(defun ende-provider--cached-catalog (key)
  "Return the unexpired successful catalog for KEY, or nil."
  (let* ((entry (gethash key ende-provider--catalog-cache))
         (age (and entry (- (float-time) (plist-get entry :time))))
         (models (and entry (plist-get entry :models))))
    (if (and entry (consp models)
             (or (null ende-provider-discovery-ttl)
                 (< age ende-provider-discovery-ttl)))
        entry
      nil)))

(defun ende-provider--catalog-for-profile (profile)
  "Return PROFILE's unexpired successful catalog, or nil."
  (let ((key (ende-provider--catalog-key profile)))
    (and key (ende-provider--cached-catalog key))))

(defun ende-provider--catalog-metadata (profile catalog)
  "Project runtime discovery metadata for PROFILE from CATALOG."
  (when catalog
    (let* ((model (plist-get profile :model))
           (models (plist-get catalog :models))
           (details (plist-get catalog :details))
           (match (if model
                      (cl-find model details :key (lambda (detail)
                                                    (plist-get detail :id))
                               :test #'equal)
                    (car details)))
           (provider (plist-get catalog :provider))
           (metadata (list :source :discovery
                           :model (or (and match (plist-get match :id)) model)
                           :models (copy-sequence models)
                           :model-details (copy-tree details))))
      (when (stringp provider)
        (setq metadata (plist-put metadata :provider provider)))
      (when (and match (stringp (plist-get match :owner)))
        (setq metadata (plist-put metadata :owner
                                  (plist-get match :owner))))
      (when (or match (stringp provider))
        (setq metadata
              (plist-put metadata :client
                         (ende-provider-client-name profile metadata))))
      metadata)))

(defun ende-provider-cached-models (profile)
  "Return a copy of PROFILE's cached model catalog, or nil.

The returned plist contains `:models', `:details', `:error', and
`:generation'.  This function never starts discovery."
  (let ((entry (ende-provider--catalog-for-profile profile)))
    (when entry
      (list :models (copy-sequence (plist-get entry :models))
            :details (copy-tree (plist-get entry :details))
            :error nil
            :generation (plist-get entry :generation)))))

(defun ende-provider-stale-models (profile)
  "Return the last successful PROFILE catalog, explicitly marked stale."
  (let ((entry (gethash (ende-provider--catalog-key profile) ende-provider--catalog-cache)))
    (when entry
      (list :models (ende-provider-config-copy (plist-get entry :models)) :stale t
            :generation (plist-get entry :generation)))))

(defun ende-provider-models-generation (profile)
  "Return the cached model catalog generation for PROFILE, or zero."
  (or (plist-get (ende-provider--catalog-for-profile profile) :generation)
      ende-provider--model-generation))

(defun ende-provider-cached-metadata (profile)
  "Return projected runtime metadata from PROFILE's cached catalog, or nil."
  (ende-provider--catalog-metadata
   profile (ende-provider--catalog-for-profile profile)))

(defun ende-provider-model-details (profile)
  "Return copied model details from PROFILE's cached catalog, or nil."
  (copy-tree (plist-get (ende-provider--catalog-for-profile profile) :details)))

(defun ende-provider--catalog-from-models (models)
  "Normalize model IDs MODELS into a catalog without endpoint details."
  (setq models (cl-remove-if-not #'stringp models))
  (when models
    (list :models (copy-sequence models)
          :details (mapcar (lambda (model) (list :id model)) models))))

(defun ende-provider--catalog-from-payload (payload)
  "Normalize endpoint model-list PAYLOAD into one catalog."
  (let* ((entries (ende-provider--model-entries payload))
         (details
          (delq nil
                (mapcar
                 (lambda (entry)
                   (let ((detail (ende-provider--model-detail entry))
                         (owner (ende-protocol-json-get "owned_by" entry)))
                     (when (and detail (stringp owner))
                       (setq detail (plist-put detail :owner owner)))
                     detail))
                 entries)))
         (provider (or (ende-protocol-json-get "provider" payload)
                       (ende-protocol-json-get "server" payload))))
    (when details
      (list :models (mapcar (lambda (detail) (plist-get detail :id)) details)
            :details details
            :provider (and (stringp provider) provider)))))

(defun ende-provider--curl-discovery-command
    (url headers-file config-file)
  "Build a GET command for model discovery at URL."
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

(defun ende-provider--cached-failure-p (key)
  "Return non-nil when a runtime failure for KEY remains throttled."
  (let* ((entry (gethash key ende-provider--discovery-failures))
         (age (and entry (- (float-time) (plist-get entry :time)))))
    (if (and entry
             (or (null ende-provider-discovery-ttl)
                 (< age ende-provider-discovery-ttl)))
        t
      (when entry
        (remhash key ende-provider--discovery-failures))
      nil)))

(defun ende-provider--invoke-catalog-waiter
    (profile type callback catalog error)
  "Notify CALLBACK of consumer TYPE with its CATALOG projection."
  (condition-case nil
      (if (eq type 'runtime)
          (funcall callback
                   (and (null error)
                        (ende-provider--catalog-metadata profile catalog)))
        (funcall callback
                 (and catalog (copy-sequence (plist-get catalog :models)))
                 error))
    ((error quit) nil)))

(defun ende-provider--finish-catalog-flight
    (key flight catalog error &optional no-cache)
  "Complete FLIGHT, caching successful CATALOG and notifying its waiters."
  (when (eq flight (gethash key ende-provider--catalog-inflight))
    (when (and (null error)
               (not (and catalog (consp (plist-get catalog :models)))))
      (setq error
            (ende-error-create :kind 'discovery
                               :message "Provider returned no models")))
    (when (and (null error) catalog (not no-cache))
      (ende-provider--prune-cache ende-provider--catalog-cache)
      (setq catalog (plist-put catalog :time (float-time)))
      (setq catalog
            (plist-put catalog :generation
                       (cl-incf ende-provider--model-generation)))
      (puthash key catalog ende-provider--catalog-cache)
      (remhash key ende-provider--discovery-failures))
    (when (and error
               (not no-cache)
               (cl-some (lambda (waiter)
                          (eq (plist-get waiter :type) 'runtime))
                        (ende-provider--catalog-flight-waiters flight)))
      (ende-provider--prune-cache ende-provider--discovery-failures)
      (puthash key (list :time (float-time))
               ende-provider--discovery-failures))
    (remhash key ende-provider--catalog-inflight)
    (let ((waiters (ende-provider--catalog-flight-waiters flight)))
      (setf (ende-provider--catalog-flight-waiters flight) nil)
      (dolist (waiter waiters)
        (ende-provider--invoke-catalog-waiter
         (plist-get waiter :profile)
         (plist-get waiter :type)
         (plist-get waiter :callback)
         catalog error)))))

(defun ende-provider--cancel-catalog-waiter (key waiter)
  "Remove WAITER from KEY and cancel its request when no consumer remains."
  (let ((flight (gethash key ende-provider--catalog-inflight)))
    (when flight
      (setf (ende-provider--catalog-flight-waiters flight)
            (cl-remove-if (lambda (entry) (eq (plist-get entry :id) waiter))
                          (ende-provider--catalog-flight-waiters flight)))
      (unless (ende-provider--catalog-flight-waiters flight)
        (remhash key ende-provider--catalog-inflight)
        (let ((cancel (ende-provider--catalog-flight-cancel flight)))
          (when cancel
            (ende-ignore-errors cancel)))))))

(defun ende-provider--start-catalog-flight (profile key flight)
  "Start the one underlying model-list request for PROFILE and FLIGHT."
  (let ((probe (plist-get profile :model-discovery-function))
        (url (ende-provider-models-url profile))
        (completed nil)
        (cancelled nil)
        (underlying-cancel nil))
    (cl-labels
        ((finish (catalog error)
           (unless cancelled
             (setq completed t)
             (ende-provider--finish-catalog-flight key flight catalog error))))
      (condition-case startup-error
          (setq underlying-cancel
                (cond
                 ((functionp probe)
                  (funcall
                   probe
                   (lambda (models error)
                     (unless cancelled
                       (finish (unless error
                                 (ende-provider--catalog-from-models models))
                               error)))))
                 (url
                  (ende-provider--start-live-model-request
                   profile url
                   (lambda (payload error)
                     (unless cancelled
                       (finish (unless error
                                 (ende-provider--catalog-from-payload payload))
                               error)))))
                 (t
                  (finish
                   nil
                   (ende-error-create
                    :kind 'discovery
                    :message
                    "Provider has no compatible model-list endpoint")))))
        ((error quit)
         (finish
          nil
          (ende-error-create :kind 'startup
                             :message (error-message-string startup-error)))))
      (unless completed
        (setf (ende-provider--catalog-flight-cancel flight)
              (lambda ()
                (setq cancelled t)
                (when underlying-cancel
                  (ende-ignore-errors underlying-cancel)
                  (setq underlying-cancel nil))))))))

(defun ende-provider--join-catalog-flight (profile key type callback)
  "Join or start PROFILE's catalog request for consumer TYPE and CALLBACK."
  (let* ((flight (gethash key ende-provider--catalog-inflight))
         (start (null flight))
         (flight (or flight (ende-provider--make-catalog-flight)))
         (waiter (list :profile profile :type type :callback callback
                       :id (make-symbol "catalog-waiter"))))
    (setf (ende-provider--catalog-flight-waiters flight)
          (cons waiter (ende-provider--catalog-flight-waiters flight)))
    (when start
      (puthash key flight ende-provider--catalog-inflight)
      (ende-provider--start-catalog-flight profile key flight))
    (apply-partially #'ende-provider--cancel-catalog-waiter
                     key (plist-get waiter :id))))

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

(defun ende-provider--start-live-model-request (profile url callback)
  "Request a model list from URL and call CALLBACK with (PAYLOAD ERROR).

This is the uncached request primitive used by catalog and capability probes."
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
    (condition-case startup-error
        (let ((request
               (ende-curl-start-request
                :profile profile
                :url url
                :internal-headers (when (eq (plist-get profile :api) 'anthropic-messages)
                                    '("anthropic-version: 2023-06-01"))
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
                           :message (error-message-string startup-error)))))
    (lambda () (funcall finish nil nil t))))

(defun ende-provider-discover (profile &optional callback)
  "Discover PROFILE metadata asynchronously and invoke CALLBACK with it.
Successful endpoint catalogs are cached; runtime failures are throttled for
`ende-provider-discovery-ttl' seconds.  CALLBACK receives nil on failure."
  (let* ((callback (or callback #'ignore))
         (key (ende-provider--catalog-key profile)))
    (ende-validate-resource-options)
    (ende-provider--validate-discovery-options)
    (cond
     ((or (ende-provider-configured-models-explicit-p profile)
          (not ende-provider-discovery-enabled))
      (ende-provider--invoke-discovery-callback callback nil)
      nil)
     ((not key)
      (ende-provider--invoke-discovery-callback callback nil)
      nil)
     ((ende-provider--catalog-for-profile profile)
      (ende-provider--invoke-discovery-callback
       callback (ende-provider-cached-metadata profile))
      nil)
     ((ende-provider--cached-failure-p key)
      (ende-provider--invoke-discovery-callback callback nil)
      nil)
     (t
      (ende-provider--join-catalog-flight profile key 'runtime callback)))))

(defun ende-provider-probe-models (profile callback)
  "Probe PROFILE's model-list endpoint and invoke CALLBACK with its models.
CALLBACK receives (MODELS ERROR).  Return a cancellation function that removes
this consumer; an unused shared request is cancelled.  Failures are retried."
  (let ((key (ende-provider--catalog-key profile))
        (probe (plist-get profile :model-discovery-function)))
    (cond
     ((ende-provider-configured-models-explicit-p profile)
      (funcall callback (ende-provider-config-copy (plist-get profile :models)) nil)
      #'ignore)
     ((or (not key)
          (and (null probe) (not ende-provider-discovery-enabled)))
      (condition-case nil
          (funcall callback nil
                   (ende-error-create
                    :kind 'discovery
                    :message (if (not key)
                                 "Provider has no compatible model-list endpoint"
                               "Provider model discovery is disabled")))
        ((error quit) nil))
      (lambda () nil))
     ((ende-provider--catalog-for-profile profile)
      (let ((catalog (ende-provider--catalog-for-profile profile)))
        (condition-case nil
            (funcall callback (copy-sequence (plist-get catalog :models)) nil)
          ((error quit) nil)))
      (lambda () nil))
     (t
      (ende-provider--join-catalog-flight profile key 'models callback)))))

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
            :kind 'capability
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
                     :kind 'capability
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
  "Resolve PROFILE's reasoning-effort choices for MODEL.
CALLBACK receives (EFFORTS DEFAULT ERROR).  Configured choices take precedence
over PROFILE's live probe function.  Live probes bypass model metadata caches."
  (let* ((descriptor
          (ende-provider-control-descriptor profile :reasoning-effort))
         (static (and descriptor (plist-member descriptor :choices)))
         (probe (plist-get profile :reasoning-effort-probe-function)))
    (cond
     (static
      (ende-provider--invoke-reasoning-effort-callback
       callback (plist-get descriptor :choices) nil nil)
      (lambda () nil))
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
        :kind 'capability
        :message (concat "Reasoning-effort support is unknown; configure "
                         ":choices or :reasoning-effort-probe-function")))
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
  (clrhash ende-provider--catalog-cache)
  (clrhash ende-provider--discovery-failures)
  (cl-incf ende-provider--model-generation)
  (message "ende: provider discovery cache cleared"))

(defun ende-provider-abort-discovery ()
  "Cancel all in-flight provider discovery requests."
  (let (keys)
    (maphash (lambda (key _flight) (push key keys))
             ende-provider--catalog-inflight)
    (dolist (key keys)
      (let* ((flight (gethash key ende-provider--catalog-inflight))
             (cancel (and flight
                          (ende-provider--catalog-flight-cancel flight))))
        (remhash key ende-provider--catalog-inflight)
        (when cancel
          (ende-ignore-errors cancel))))
  (dolist (process (copy-sequence ende-provider--discovery-processes))
    (let ((finish (and (processp process)
                       (process-get process :ende-discovery-finish))))
      (if finish
          (funcall finish nil nil t)
        (when (and (processp process) (process-live-p process))
          (ende-ignore-errors #'delete-process process)))))
  (setq ende-provider--discovery-processes nil)))

;;;###autoload
(defun ende-provider-check (&optional probe)
  "Check configured providers locally without resolving credentials.
With explicit prefix PROBE, discover only an unauthenticated provider catalog.
Never issue generation requests or prompt for credentials."
  (interactive "P")
  (ende-provider-config-validate)
  (let ((names (ende-provider-names)) reports)
    (dolist (name names)
      (let ((profile (ende-provider-profile name)))
        (ende-provider-validate-controls profile)
        (push (list :profile name :service (or (plist-get profile :service) 'custom)
                    :api (plist-get profile :api) :account (plist-get profile :account)
                    :endpoint (plist-get profile :url)
                    :model (or (plist-get profile :model) 'selection-required)
                    :credentials (if (eq (plist-get profile :auth-method) 'none) 'none 'unchecked))
              reports)))
    (when probe
      (let* ((name (intern (completing-read "Probe unauthenticated provider: " names nil t)))
             (profile (ende-provider-profile name)))
        (unless (eq (plist-get profile :auth-method) 'none)
          (user-error "Authenticated probes are not part of the local configuration check"))
        (ende-provider-probe-models
         profile (lambda (models error)
                   (message "Ende discovery: %s" (if error "unavailable" (format "%d models" (length models))))))))
    (when (called-interactively-p 'interactive)
      (with-help-window "*Ende Provider Check*"
        (if reports
            (dolist (report (nreverse reports))
              (princ (format "%S\n" report)))
          (princ "No selectable providers. Configure ende-providers and a provider definition.\n"))))
    reports))

(provide 'ende-provider)

;;; ende-provider.el ends here
