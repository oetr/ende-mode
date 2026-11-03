;;; ende-provider-config.el --- Provider configuration values -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'ende-core)
(require 'ende-controls)
(require 'ende-curl)
(require 'ende-codex-methods)

(defcustom ende-providers nil
  "Ordered profile symbols offered by the provider selector.
An empty list disables provider selection; it does not imply a local server."
  :type '(repeat symbol)
  :group 'ende)

(defcustom ende-builtin-provider-settings nil
  "Settings for built-in services and independently named instances.
Entries have the form (NAME KEY VALUE ...).  Named instances require
:provider.  API keys belong in auth-source, never in these settings."
  :type 'sexp
  :group 'ende)

(defcustom ende-custom-providers nil
  "Custom HTTP profiles as (NAME KEY VALUE ...) entries.
Specify :api, exactly one of :base-url or :endpoints, and an explicit
:auth-method of none or auth-source.  :models is an ordered static catalog;
its first entry is the initial model.  Without it, select a discovered or
manually entered model.  :model-settings refines shared model defaults."
  :type 'sexp
  :group 'ende)

(defconst ende-provider-config--apis
  '((openai-chat-completions . "/chat/completions")
    (openai-responses . "/responses")
    (anthropic-messages . "/messages")))

(defconst ende-provider-config--services
  `((openai-codex
     :api openai-responses :max-concurrency 4
     :models-url ,(ende-codex--auth-property 'chatgpt-plan-oauth :models-url)
     :endpoints ((openai-responses . ,(ende-codex--auth-property 'chatgpt-plan-oauth :responses-url))))
    (openai
     :api openai-responses :max-concurrency 4
     :models-url "https://api.openai.com/v1/models"
     :endpoints ((openai-responses . "https://api.openai.com/v1/responses")
                 (openai-chat-completions . "https://api.openai.com/v1/chat/completions")))
    (deepseek
     :api openai-chat-completions :max-concurrency 1
     :models-url "https://api.deepseek.com/models"
     :endpoints ((openai-chat-completions . "https://api.deepseek.com/chat/completions")))
    (openrouter
     :api openai-chat-completions :max-concurrency 4
     :models-url "https://openrouter.ai/api/v1/models"
     :endpoints ((openai-chat-completions . "https://openrouter.ai/api/v1/chat/completions")))
    (anthropic
     :api anthropic-messages :max-concurrency 4
     :models-url "https://api.anthropic.com/v1/models"
     :endpoints ((anthropic-messages . "https://api.anthropic.com/v1/messages")))))

(defconst ende-provider-config--model-fields
  '(:context-window :max-output-tokens :output-tokens :temperature
		    :thinking :reasoning-effort :parameters))

(defconst ende-provider-config--common-fields
  (append '(:display-name :api :models :model-settings :request-timeout
			  :max-concurrency :parallel :source-name-disclosure)
          ende-provider-config--model-fields))

(defconst ende-provider-config--protected-paths
  (mapcar #'list
          '("model" "messages" "input" "stream" "stream_options" "tools"
            "tool_choice" "system" "instructions" "previous_response_id" "conversation"
            "max_tokens" "max_completion_tokens" "max_output_tokens" "store"
            "temperature" "thinking" "reasoning_effort" "reasoning")))

(defun ende-provider-config-name-p (name)
  "Return non-nil for a valid profile NAME."
  (and (symbolp name) name (not (eq name t)) (not (keywordp name))))

(defun ende-provider-config--string (value field)
  "Require a nonempty string VALUE for FIELD."
  (unless (and (stringp value) (not (string-blank-p value)))
    (error "%s: supply a nonempty string" field)))

(defun ende-provider-config--positive (value field &optional real)
  "Validate optional positive VALUE for FIELD; permit finite REAL values."
  (unless (or (null value)
              (and (if real (ende-provider-config--finite-p value) (integerp value))
                   (> value 0)))
    (error "%s: use nil or a positive %s" field (if real "finite number" "integer"))))

(defun ende-provider-config--models (models)
  "Validate an explicit ordered catalog MODELS."
  (unless (and (proper-list-p models) models)
    (error ":models: supply a nonempty proper list of unique model IDs"))
  (let (seen)
    (dolist (model models)
      (ende-provider-config--string model ":models")
      (when (member model seen)
        (error ":models: remove duplicate model ID %s" model))
      (push model seen))))

(defun ende-provider-config--settings (settings fields context)
  "Validate SETTINGS against FIELDS in CONTEXT."
  (ende-provider-config--plist settings context)
  (cl-loop
   for (key value) on settings by #'cddr do
   (unless (memq key fields)
     (error "%s: remove unsupported field %s" context key))
   (pcase key
     ((or :context-window :max-output-tokens :output-tokens)
      (ende-provider-config--positive value key))
     (:request-timeout (ende-provider-config--positive value key t))
     (:max-concurrency
      (unless (and (integerp value) (> value 0))
        (error ":max-concurrency: supply a positive integer")))
     ((or :parallel :source-name-disclosure)
      (unless (memq value '(nil t)) (error "%s: use t or nil" key)))
     ((or :display-name :name :base-url :models-url :auth-source-user)
      (ende-provider-config--string value key))
     (:api
      (unless (assq value ende-provider-config--apis)
        (error ":api: choose openai-chat-completions, openai-responses, or anthropic-messages")))
     (:models (ende-provider-config--models value))
     (:parameters (ende-provider-config-parameters value))
     ((or :temperature :thinking :reasoning-effort)
      (ende-controls--validate-canonical-value key value context)
      (when (and (eq key :temperature) value
                 (not (ende-provider-config--finite-p value)))
        (error ":temperature: use a finite number or nil")))
     (:controls (ende-controls--validate-descriptor-list value t))
     (:auth-method
      (unless (memq value '(none auth-source chatgpt-plan-oauth codex-oauth))
        (error ":auth-method: unsupported authentication method")))
     (:auth-scheme
      (unless (memq value '(bearer x-api-key))
        (error ":auth-scheme: choose bearer or x-api-key")))
     (:account
      (unless (and (ende-provider-config-name-p value)
                   (let ((case-fold-search nil))
                     (string-match-p "\\`[a-z0-9][a-z0-9_-]*\\'" (symbol-name value))))
        (error ":account: use a lowercase ASCII symbol with letters, digits, underscores, or hyphens")))
     (:provider
      (unless (assq value ende-provider-config--services)
        (error ":provider: choose a canonical built-in service")))
     (:transport
      (unless (memq value '(auto openrouter-batch))
        (error ":transport: choose auto or openrouter-batch"))))))

(defun ende-provider-config--headers (headers)
  "Validate nonsecret custom HEADERS."
  (unless (proper-list-p headers) (error ":request-headers: use a proper list"))
  (let (seen)
    (dolist (header headers)
      (unless (and (ende-curl--valid-header-p header)
                   (not (ende-curl--unsafe-profile-header-p header)))
        (error ":request-headers: remove invalid, secret, or transport-owned header"))
      (let ((name (downcase (car (split-string header ":")))))
        (when (member name seen)
          (error ":request-headers: remove duplicate header %s" name))
        (push name seen)))))

(defun ende-provider-config--endpoints (endpoints)
  "Validate API endpoint mapping ENDPOINTS."
  (unless (and (proper-list-p endpoints) endpoints)
    (error ":endpoints: supply a nonempty API-to-URL alist"))
  (let (seen)
    (dolist (entry endpoints)
      (unless (and (consp entry) (assq (car entry) ende-provider-config--apis)
                   (stringp (cdr entry)))
        (error ":endpoints: map implemented API symbols to complete URL strings"))
      (when (memq (car entry) seen)
        (error ":endpoints: remove duplicate API %s" (car entry)))
      (push (car entry) seen))))

(defun ende-provider-config--definition (name settings custom)
  "Validate profile NAME's SETTINGS; CUSTOM distinguishes endpoint profiles."
  (condition-case failure
      (let* ((fields (append ende-provider-config--common-fields
                             (if custom
                                 '(:base-url :endpoints :models-url :auth-method
					     :auth-source-user :auth-scheme :request-headers :controls)
                               '(:provider :account :transport))))
             (service (unless custom (or (plist-get settings :provider) name)))
             (integration (cdr (assq service ende-provider-config--services)))
             (api (or (plist-get settings :api) (plist-get integration :api)))
             (models (plist-get settings :models))
             (model-settings (plist-get settings :model-settings))
             seen)
        (when (eq service 'openai-codex)
          (push :auth-method fields))
        (ende-provider-config--settings settings fields "Profile")
        (ende-provider-config-parameters (plist-get settings :parameters)
                                         ende-provider-config--protected-paths)
        (unless (proper-list-p model-settings)
          (error ":model-settings: use a proper list"))
        (dolist (entry model-settings)
          (unless (and (consp entry) (proper-list-p entry))
            (error ":model-settings: use (MODEL KEY VALUE ...) entries"))
          (ende-provider-config--string (car entry) ":model-settings")
          (when (member (car entry) seen)
            (error ":model-settings: remove duplicate model %s" (car entry)))
          (when (and models (not (member (car entry) models)))
            (error ":model-settings: model %s is outside :models" (car entry)))
          (push (car entry) seen)
          (ende-provider-config--settings
           (cdr entry) (cons :name ende-provider-config--model-fields)
           (format "Model %s" (car entry)))
          (ende-provider-config-parameters (plist-get (cdr entry) :parameters)
                                           ende-provider-config--protected-paths))
        (if custom
            (progn
              (when (assq name ende-provider-config--services)
                (error "name is reserved; choose a different custom profile name"))
              (unless (plist-member settings :api)
                (error "supply :api"))
              (unless (plist-member settings :auth-method)
                (error "supply :auth-method none or auth-source"))
              (unless (memq (plist-get settings :auth-method) '(none auth-source))
                (error "custom profiles require none or auth-source"))
              (when (eq (not (null (plist-member settings :base-url)))
                        (not (null (plist-member settings :endpoints))))
                (error "supply exactly one of :base-url or :endpoints"))
              (when (plist-member settings :endpoints)
                (ende-provider-config--endpoints (plist-get settings :endpoints))
                (unless (assq api (plist-get settings :endpoints))
                  (error ":endpoints: add an entry for selected :api")))
              (when (eq (plist-get settings :auth-method) 'none)
                (when (or (plist-member settings :auth-source-user)
                          (plist-member settings :auth-scheme))
                  (error ":auth-method none: remove authentication fields")))
              (let ((controls (ende-controls-merge-descriptors
                               (ende-provider-config--controls nil api nil)
                               (plist-get settings :controls))))
                (dolist (entry controls)
                  (dolist (mapping (plist-get (cdr entry) :values))
                    (ende-provider-config--json-value (cdr mapping) nil))))
              (ende-provider-config--headers (plist-get settings :request-headers)))
          (unless integration
            (error "named built-in instances require :provider with a canonical service"))
          (when (and (assq name ende-provider-config--services) (not (eq name service)))
            (error ":provider: canonical profile must use its own service"))
          (unless (assq api (plist-get integration :endpoints))
            (error ":api: choose a verified API for %s" service))
          (when (eq service 'openai-codex)
            (ende-codex--auth-spec (or (plist-get settings :auth-method) 'codex-oauth)))
          (when (and (plist-member settings :transport) (not (eq service 'openrouter)))
            (error ":transport: only OpenRouter supports transport overrides")))
        (when custom
          (dolist (url (append (when (plist-get settings :base-url)
                                 (list (plist-get settings :base-url)))
                               (mapcar #'cdr (plist-get settings :endpoints))
                               (when (plist-get settings :models-url)
                                 (list (plist-get settings :models-url)))))
            (ende-curl--validated-url settings url)
            (when (and (eq (plist-get settings :auth-method) 'auth-source)
                       (not (equal (plist-get (ende-url-normalize url) :scheme) "https")))
              (error "authenticated destinations require HTTPS")))))
    (error (error "Profile %s: %s" name (error-message-string failure)))))

(defun ende-provider-config-validate ()
  "Validate all provider definitions without resolving credentials or networking."
  (dolist (table (list ende-builtin-provider-settings ende-custom-providers))
    (ende-provider-config-copy table)
    (unless (proper-list-p table) (error "Provider definitions must be proper lists")))
  (let (names selected)
    (dolist (table (list ende-builtin-provider-settings ende-custom-providers))
      (dolist (entry table)
        (unless (and (consp entry) (ende-provider-config-name-p (car entry)))
          (error "Provider definitions require non-nil, non-keyword profile symbols other than t"))
        (when (memq (car entry) names)
          (error "Profile %s: remove duplicate definition" (car entry)))
        (push (car entry) names)
        (ende-provider-config--definition
         (car entry) (cdr entry) (eq table ende-custom-providers))))
    (unless (proper-list-p ende-providers)
      (error "ende-providers: use a proper ordered list"))
    (dolist (name ende-providers)
      (unless (ende-provider-config-name-p name)
        (error "ende-providers: use valid profile symbols"))
      (when (memq name selected)
        (error "Profile %s: remove duplicate selector entry" name))
      (unless (or (memq name names) (assq name ende-provider-config--services))
        (error "Profile %s: add a built-in or custom definition" name))
      (push name selected)))
  t)

(defun ende-provider-config--finite-p (value)
  "Return non-nil when VALUE is a finite real number."
  (and (numberp value)
       (or (integerp value)
           (and (= value value) (< (abs value) 1.0e+INF)))))

(defun ende-provider-config--plist (value context &optional json)
  "Validate VALUE as a unique-key plist in CONTEXT.
When JSON is non-nil, compare keyword and string keys by JSON spelling."
  (unless (ende-controls--proper-plist-p value)
    (error "%s: use a proper, even-length plist" context))
  (let ((seen (make-hash-table :test #'equal)))
    (cl-loop for (key _value) on value by #'cddr
             for name = (if json
                            (cond ((stringp key) key)
                                  ((keywordp key)
                                   (substring (symbol-name key) 1)))
                          (and (keywordp key) key))
             do
             (unless name
               (error "%s: use %s keys" context
                      (if json "keyword or string" "keyword")))
             (when (gethash name seen)
               (error "%s: remove duplicate key %s" context name))
             (puthash name t seen)))
  value)

(defun ende-provider-config-copy (value &optional ancestors)
  "Return a detached copy of configuration VALUE; reject cycles.
ANCESTORS contains the objects on the current traversal path."
  (when (memq value ancestors)
    (error "Provider configuration must not contain cycles"))
  (cond
   ((stringp value) (substring-no-properties value))
   ((consp value)
    (let (result tail (cursor value) (path ancestors))
      (while (consp cursor)
        (when (memq cursor path) (error "Provider configuration must not contain cycles"))
        (push cursor path)
        (let ((cell (list (ende-provider-config-copy (car cursor) path))))
          (if tail (setcdr tail cell) (setq result cell))
          (setq tail cell cursor (cdr cursor))))
      (setcdr tail (ende-provider-config-copy cursor path))
      result))
   ((vectorp value)
    (apply #'vector
           (mapcar (lambda (element)
                     (ende-provider-config-copy element (cons value ancestors)))
                   value)))
   ((hash-table-p value)
    (let ((copy (make-hash-table :test (hash-table-test value))))
      (maphash (lambda (key element)
                 (puthash (ende-provider-config-copy key (cons value ancestors))
                          (ende-provider-config-copy element (cons value ancestors))
                          copy))
               value)
      copy))
   (t value)))

(defun ende-provider-config--json-value (value ancestors)
  "Encode raw JSON VALUE, rejecting cycles through ANCESTORS."
  (when (memq value ancestors)
    (error "Raw parameters must not contain cycles"))
  (cond
   ((or (null value) (eq value t) (stringp value))
    (if (stringp value) (substring-no-properties value) value))
   ((eq value :json-false) json-false)
   ((ende-provider-config--finite-p value) value)
   ((vectorp value)
    (apply #'vector
           (mapcar (lambda (element)
                     (ende-provider-config--json-value
                      element (cons value ancestors)))
                   value)))
   ((and (hash-table-p value) (zerop (hash-table-count value)))
    (make-hash-table :test #'equal))
   ((consp value)
    (ende-provider-config--json-object value ancestors))
   (t (error "Raw parameter value is not a supported JSON value"))))

(defun ende-provider-config--json-object (parameters ancestors)
  "Encode raw object PARAMETERS, rejecting cycles through ANCESTORS."
  (ende-provider-config--plist parameters "Raw parameters" t)
  (let ((object (make-hash-table :test #'equal)))
    (cl-loop for (key value) on parameters by #'cddr
             for name = (if (keywordp key) (substring (symbol-name key) 1) key)
             do (puthash (copy-sequence name)
                         (ende-provider-config--json-value
                          value (cons parameters ancestors))
                         object))
    object))

(defun ende-provider-config-parameters (parameters &optional protected)
  "Validate PARAMETERS and return a detached JSON object.
PROTECTED contains adapter-owned JSON paths.  Raw objects cannot replace
protected paths or their ancestors.  Recognized credential fields are rejected
at every nesting level; arbitrary values are not guaranteed nonsecret."
  (let ((object (ende-provider-config--json-object parameters nil)))
    (cl-labels
        ((check (value prefix)
           (when (hash-table-p value)
             (maphash
              (lambda (key child)
                (let ((path (append prefix (list key))))
                  (when (string-match-p
                         "\\(?:authorization\\|api[_-]?key\\|\\`\\(?:auth[_-]?token\\|bearer[_-]?token\\|token\\)\\'\\|access[_-]?token\\|refresh[_-]?token\\|id[_-]?token\\|password\\|secret\\|cookie\\|credential\\)"
                         (downcase key))
                    (error "Raw parameters: remove credential-bearing field"))
                  (when (cl-some
                         (lambda (owned)
                           (ende-controls--path-conflict-p path owned))
                         protected)
                    (error "Raw parameters: %s is adapter-owned" key))
                  (check child path)))
              value))
           (when (vectorp value)
             (mapc (lambda (child) (check child prefix)) value))))
      (check object nil))
    object))

(defun ende-provider-config-merge-parameters (base overrides)
  "Merge raw parameter maps BASE and OVERRIDES by normalized top-level key.
Nil OVERRIDES clears BASE.  Nested objects and arrays replace whole values."
  (ende-provider-config-parameters base)
  (ende-provider-config-parameters overrides)
  (when overrides
    (let ((result (ende-provider-config-copy base)))
      (cl-loop for (key value) on overrides by #'cddr
               for name = (ende-controls--json-key-name key)
               do (setq result
                        (cl-loop for (old-key old-value) on result by #'cddr
                                 unless (equal name (ende-controls--json-key-name old-key))
                                 append (list old-key old-value)))
               do (setq result
                        (append result (list (ende-provider-config-copy key)
                                             (ende-provider-config-copy value)))))
      result)))

(defun ende-provider-config-encode-parameters (body profile)
  "Return adapter BODY extended with PROFILE's validated raw parameters."
  (let ((parameters
         (ende-provider-config-parameters
          (plist-get profile :parameters)
          (append ende-provider-config--protected-paths
                  (mapcar (lambda (entry) (plist-get (cdr entry) :path))
                          (plist-get profile :controls)))))
        (result (copy-tree body)))
    (maphash (lambda (key value) (push (cons key value) result)) parameters)
    result))

(defun ende-provider-config--controls (service api model)
  "Return verified control mappings for SERVICE, API and MODEL."
  (pcase service
    ('openai-codex '((:reasoning-effort :path ("reasoning" "effort"))))
    ('deepseek
     (when (member model '("deepseek-flash" "deepseek-v4-pro"))
       '((:temperature :path ("temperature"))
         (:thinking :path ("thinking" "type") :choices (on off)
                    :values ((on . "enabled") (off . "disabled")))
         (:reasoning-effort :path ("reasoning_effort")
                            :choices ("none" "low" "high" "max")))))
    ('openrouter
     '((:temperature :path ("temperature"))
       (:reasoning-effort :path ("reasoning" "effort"))))
    ('openai
     (if (and model (string-match-p "\\`\\(?:o[134]\\|gpt-5\\)" model))
         (list (list :reasoning-effort :path
                     (if (eq api 'openai-responses)
                         '("reasoning" "effort") '("reasoning_effort"))))
       '((:temperature :path ("temperature")))))
    (_ '((:temperature :path ("temperature"))))))

(defun ende-provider-config--inherit (base settings source)
  "Return BASE with SETTINGS inherited and provenance marked SOURCE."
  (let ((result (ende-provider-config-copy base))
        (provenance (ende-provider-config-copy (plist-get base :provenance))))
    (cl-loop for (key value) on settings by #'cddr do
             (setq result
                   (plist-put result key
                              (if (eq key :parameters)
                                  (ende-provider-config-merge-parameters
                                   (plist-get result key) value)
                                (ende-provider-config-copy value))))
             (setq provenance (plist-put provenance key source)))
    (plist-put result :provenance provenance)))

(defun ende-provider-config--origin (url)
  "Return URL's normalized scheme, host and effective port."
  (let ((parsed (ende-url-normalize url)))
    (list (plist-get parsed :scheme) (plist-get parsed :host)
          (plist-get parsed :port))))

(defun ende-provider-config-supported-apis (profile)
  "Return APIs with configured generation endpoints for PROFILE."
  (if (plist-get profile :base-url)
      (list (plist-get profile :api))
    (mapcar #'car (plist-get profile :endpoints))))

(defun ende-provider-config-supported-auth (profile)
  "Return authentication methods implemented for PROFILE's connection."
  (if (eq (plist-get profile :service) 'openai-codex)
      '(codex-oauth chatgpt-plan-oauth)
    (list (plist-get profile :auth-method))))

(defun ende-provider-config-resolve (name &optional model api connection)
  "Resolve profile NAME into detached nonsecret request settings.
MODEL is a runtime selection, never a configuration property.  Optional API
selects another explicitly available wire endpoint.  No network is started."
  (ende-provider-config-validate)
  (unless (ende-provider-config-name-p name)
    (user-error "No provider selected; configure ende-providers and open the selector"))
  (let* ((custom (assq name ende-custom-providers))
         (configured (cdr (or custom (assq name ende-builtin-provider-settings))))
         (service (unless custom (or (plist-get configured :provider) name)))
         (integration (cdr (assq service ende-provider-config--services)))
         (api (or (plist-get connection :api) api
                  (plist-get configured :api) (plist-get integration :api)))
         (endpoints (if custom (plist-get configured :endpoints)
                      (plist-get integration :endpoints)))
         (base-url (plist-get configured :base-url))
         (url (if base-url
                  (and (eq api (plist-get configured :api))
                       (concat (string-trim-right base-url "/+")
                               (alist-get api ende-provider-config--apis)))
                (alist-get api endpoints)))
         (model (or model (car (plist-get configured :models))))
         (model-settings (cdr (assoc model (plist-get configured :model-settings))))
         (account (or (plist-get connection :account)
                      (if custom
                          (and (eq (plist-get configured :auth-method) 'auth-source)
                               (or (plist-get configured :auth-source-user) "ende"))
                        (or (plist-get configured :account) 'default))))
         (auth (if custom (plist-get configured :auth-method)
                 (if (eq service 'openai-codex)
                     (or (plist-get connection :auth-method)
                         (plist-get configured :auth-method) 'codex-oauth)
                   'auth-source)))
         (controls (ende-provider-config--controls service api model))
         (profile
          (list :name name :service service :api api :url url :model model
                :account account :auth-method auth
                :endpoints (ende-provider-config-copy endpoints)
                :max-concurrency (or (plist-get integration :max-concurrency) 1)
                :parallel nil :require-sse t :stream-usage t
                :request-timeout ende-request-timeout
                :source-name-disclosure ende-source-name-disclosure
                :output-tokens ende-default-output-tokens)))
    (when connection
      (ende-provider-config--plist connection "Connection selection")
      (cl-loop for (key _value) on connection by #'cddr do
               (unless (memq key '(:account :auth-method :api))
                 (error "Unsupported connection field: %s" key)))
      (when (and (plist-member connection :auth-method)
                 (not (eq (plist-get connection :auth-method) auth)))
        (error "Unsupported authentication method for %s" name))
      (pcase auth
        ((or 'chatgpt-plan-oauth 'codex-oauth)
         (ende-provider-config--settings (list :account account) '(:account) "Connection"))
        ('auth-source
         (unless (and (or (stringp account) (ende-provider-config-name-p account))
                      (string-match-p "\\`[[:alnum:]_.@+-]+\\'" (format "%s" account)))
           (error "Account: supply a nonempty authentication alias")))
        ('none
         (when account (error "This provider does not use an account")))))
    (when model (ende-provider-config--string model ":model selection"))
    (unless (and (or custom integration) url)
      (error "Profile %s: define the profile and a supported endpoint for :api %s" name api))
    (when (and model (plist-member configured :models)
               (not (member model (plist-get configured :models))))
      (error "Profile %s: model %s is outside :models; select a configured model" name model))
    (setq controls (if custom
                       (ende-controls-merge-descriptors controls (plist-get configured :controls))
                     controls))
    (let ((protected (append ende-provider-config--protected-paths
                             (mapcar (lambda (entry) (plist-get (cdr entry) :path)) controls))))
      (ende-provider-config-parameters (plist-get configured :parameters) protected)
      (ende-provider-config-parameters (plist-get model-settings :parameters) protected))
    (setq profile (ende-provider-config--inherit profile configured 'profile)
          profile (ende-provider-config--inherit profile model-settings 'model))
    (setq profile (plist-put profile :name name)
          profile (plist-put profile :api api)
          profile (plist-put profile :account account)
          profile (plist-put profile :model-name
                             (when (plist-get model-settings :name)
                               (ende-bounded-single-line (plist-get model-settings :name) 80 t)))
          profile (plist-put profile :controls (ende-provider-config-copy controls))
          profile (plist-put profile :models-explicit-p (and (plist-member configured :models) t))
          profile (plist-put profile :models-url
                             (or (plist-get configured :models-url)
                                 (plist-get integration :models-url)
                                 (and base-url (not (eq api 'anthropic-messages))
                                      (concat (string-trim-right base-url "/+") "/models")))))
    (when (and (eq service 'openrouter) (eq (plist-get profile :transport) 'openrouter-batch))
      (unless (plist-member configured :max-concurrency)
        (setq profile (plist-put profile :max-concurrency 16)))
      (unless (plist-member configured :parallel)
        (setq profile (plist-put profile :parallel t))))
    (when (eq service 'openai-codex)
      (let ((responses (ende-codex--auth-property auth :responses-url)))
        (setq url responses
              profile (plist-put profile :auth-method auth)
              profile (plist-put profile :url responses)
              profile (plist-put profile :endpoints (list (cons 'openai-responses responses)))
              profile (plist-put profile :models-url (ende-codex--auth-property auth :models-url)))))
    (when (eq auth 'auth-source)
      (let* ((parsed (url-generic-parse-url url))
             (user (format "%s" account)))
        (setq profile (plist-put profile :auth-source-host (downcase (url-host parsed)))
              profile (plist-put profile :auth-source-port (url-port parsed))
              profile (plist-put profile :auth-source-user user)
              profile (plist-put profile :auth-scheme
                                 (or (plist-get configured :auth-scheme)
                                     (if (eq api 'anthropic-messages) 'x-api-key 'bearer)))))
      (when-let* ((models-url (plist-get profile :models-url)))
        (unless (equal (ende-provider-config--origin url)
                       (ende-provider-config--origin models-url))
          (error "Profile %s: authenticated discovery must share the generation origin" name))))
    (when (eq service 'deepseek)
      (when (member model '("deepseek-flash" "deepseek-v4-pro"))
        (setq profile (plist-put profile :reasoning-history-field 'reasoning_content)
              profile (plist-put profile :control-defaults
                                 '(:temperature 1 :thinking on :reasoning-effort "high")))))
    (when-let* ((label (plist-get profile :display-name)))
      (setq profile (plist-put profile :display-name (ende-bounded-single-line label 80 t))))
    (let ((budget (plist-get profile :output-tokens))
          (capacity (plist-get profile :max-output-tokens)))
      (when (and budget capacity (> budget capacity))
        (error "Profile %s model %s: reduce :output-tokens below :max-output-tokens" name model)))
    (setq profile (plist-put profile :url (ende-curl--validated-url profile)))
    (when (plist-get profile :models-url)
      (setq profile (plist-put profile :models-url
                               (ende-curl--validated-url profile (plist-get profile :models-url)))))
    (ende-provider-config-copy profile)))

(provide 'ende-provider-config)
;;; ende-provider-config.el ends here
