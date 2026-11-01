;;; ende-llm.el --- LLM completion transport for ende -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools

;;; Commentary:

;; Asynchronous OpenAI-compatible completion transport and session lifecycle.
;; The transport reports one terminal result or one error.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'ende-core)
(require 'ende-content)
(require 'ende-controls)
(require 'ende-curl)
(require 'ende-protocol)
(require 'ende-transport)
(require 'ende-stream)
(require 'ende-debug)
(require 'ende-request)
(require 'ende-provider)
(require 'ende-messages)
(require 'ende-openrouter-batch)
(require 'ende-codex)

;;;; Hookable LLM caller

(defvar ende-llm-function #'ende-llm-default
  "Function used to dispatch requests to the LLM.
Called with (MESSAGES CALLBACKS &optional OPTIONS).
CALLBACKS is a plist with keys :on-start, :on-event, :on-done, and :on-error.
OPTIONS may include :tool-definitions; successful results may then include
normalized :tool-calls for the chat tool loop.")

(defcustom ende-usage-update-interval 0.1
  "Minimum seconds between estimated streaming usage updates."
  :type 'number
  :group 'ende)

;;;; Client implementation

(cl-defstruct (ende-llm--session
               (:constructor ende-llm--allocate-session))
  "State owned by one asynchronous LLM transport request."
  profile stream model messages json-data reasoning reasoning-content-parts
  tool-calls provider-error last-status last-usage-update debug-record)

(defun ende-llm--make-session (&rest arguments)
  "Create a Chat Completions session from protocol ARGUMENTS.
The :callbacks argument initializes the composed transport lifecycle."
  (let* ((callbacks (plist-get arguments :callbacks))
         (session
          (apply #'ende-llm--allocate-session
                 (cl-loop for (key value) on arguments by #'cddr
                          unless (eq key :callbacks)
                          append (list key value)))))
    (let ((lifecycle
           (ende-transport-session-create
            callbacks (apply-partially #'ende-llm--fail session))))
      (setf (ende-llm--session-stream session)
            (ende-stream-create lifecycle (ende-protocol-make-sse)))
      session)))

(defun ende-llm--lifecycle (session)
  "Return the composed transport lifecycle for SESSION."
  (ende-stream-lifecycle (ende-llm--session-stream session)))

(defun ende-llm--terminal-p (session)
  "Return non-nil when SESSION's transport has settled."
  (ende-stream-terminal-p (ende-llm--session-stream session)))

(defun ende-llm--settle (session outcome value)
  "Settle SESSION exactly once with OUTCOME and VALUE."
  (let* ((record (ende-llm--session-debug-record session))
         (tool-calls (ende-llm--tool-call-list session))
        (extra-fields
         (list :reasoning-content
               (and (ende-llm--session-reasoning-content-parts session)
                    (mapconcat
                     #'identity
                     (reverse
                      (ende-llm--session-reasoning-content-parts session))
                     ""))
               :tool-calls tool-calls)))
    (when record
      (ende-debug-record-completion
       record outcome
       (ende-stream-session-usage (ende-llm--session-stream session))
       (ende-llm--response-status session) value))
    (ende-stream-settle-generation
     (ende-llm--session-stream session) outcome value "completion_tokens"
     (ende-llm--session-model session) nil extra-fields
     (or (ende-llm--session-reasoning-content-parts session)
         tool-calls) nil nil)))

(defun ende-llm--fail (session error)
  "Settle SESSION with structured ERROR."
  (ende-llm--settle session 'failure error))

(defun ende-llm--cancel (session)
  "Cancel SESSION."
  (ende-llm--fail
   session (ende-error-create :kind 'cancelled :message "Process cancelled")))

(defun ende-llm--emit-event (session event)
  "Emit EVENT for SESSION unless it has settled."
  (ende-stream-emit (ende-llm--session-stream session) event))

(defun ende-llm--emit-diagnostic (session text)
  "Emit diagnostic TEXT for SESSION when tool diagnostics are enabled."
  (when ende-tool-diagnostics
    (ende-llm--emit-event session (list :type :diagnostic :text text))))

(defun ende-llm--diagnostic-tool-names (tools)
  "Return tool names from Chat Completions TOOLS."
  (if tools
      (mapconcat (lambda (tool)
                   (or (alist-get :name (alist-get :function tool))
                       "<unknown>"))
                 tools ", ")
    "(none)"))

(defun ende-llm--diagnose-request (session body)
  "Record the tools in Chat Completions request BODY for SESSION."
  (ende-llm--emit-diagnostic
   session
   (format "Chat Completions tools sent: %s"
           (ende-llm--diagnostic-tool-names (alist-get :tools body)))))

(defun ende-llm--diagnostic-output-items (session)
  "Return a safe summary of SESSION's Chat Completions output items."
  (let ((items (when (ende-stream-session-content-parts
                     (ende-llm--session-stream session))
                 (list "message"))))
    (dolist (call (ende-llm--tool-call-list session))
      (push (format "tool_call(%s)" (or (plist-get call :name) "<unknown>"))
            items))
    (if items
        (mapconcat #'identity (nreverse items) ", ")
      "(none)")))

(defun ende-llm--diagnose-result (session)
  "Record the response item types collected for SESSION."
  (ende-llm--emit-diagnostic
   session
   (format "Chat Completions output items: %s"
           (ende-llm--diagnostic-output-items session))))

(defun ende-llm--emit-status (session status)
  "Emit a changed STATUS for SESSION."
  (unless (equal status (ende-llm--session-last-status session))
    (setf (ende-llm--session-last-status session) status)
    (ende-llm--emit-event session (list :type :status :status status))))

(defun ende-llm--emit-delta (session channel text)
  "Record and emit nonempty TEXT on CHANNEL for SESSION."
  (when (ende-stream-record-delta
         (ende-llm--session-stream session) channel text
         (apply-partially #'ende-llm--fail session))
    (let ((now (float-time)))
      (when (or (null (ende-llm--session-last-usage-update session))
                (>= (- now (ende-llm--session-last-usage-update session))
                    ende-usage-update-interval))
        (setf (ende-llm--session-last-usage-update session) now)
        (ende-stream-emit-estimated-usage
         (ende-llm--session-stream session))))))

(defun ende-llm--parser-callbacks (session)
  "Return reasoning parser callbacks for SESSION."
  (list :on-status (apply-partially #'ende-llm--emit-status session)
        :on-delta (apply-partially #'ende-llm--emit-delta session)))

(defun ende-llm--response-status (session)
  "Return SESSION's HTTP response status, or nil."
  (condition-case nil
      (car (ende-stream-response-headers
            (ende-llm--session-stream session)))
    (error nil)))

(defun ende-llm--handle-provider-error (session json-error)
  "Settle SESSION from a provider JSON-ERROR value."
  (let* ((reported-message
          (ende-transport-safe-error-text
           (format "%s" (or (ende-protocol-json-get "message" json-error)
                             "Provider request failed"))))
         (status (ende-llm--response-status session))
         (http-error (and status (or (< status 200) (>= status 300))))
         (error (ende-error-create
                 :kind (if http-error 'http 'provider)
                 :message (if http-error
                              (format "HTTP status %s: %s"
                                      status reported-message)
                            reported-message)
                 :details (ende-content-redact json-error))))
    (setf (ende-llm--session-provider-error session) error)
    (ende-llm--fail session error)))

(defun ende-llm--handle-metadata (session json)
  "Record and emit provider metadata in JSON for SESSION."
  (let ((model (ende-protocol-json-get "model" json))
        (provider (or (ende-protocol-json-get "provider" json)
                      (ende-protocol-json-get "server" json)
                      (ende-protocol-json-get "owned_by" json))))
    (when (or (and (stringp model)
                   (not (string= model
                                 (or (ende-stream-session-actual-model
                                      (ende-llm--session-stream session))
                                     ""))))
              (and (stringp provider)
                   (not (string= provider
                                 (or (ende-stream-session-actual-provider
                                      (ende-llm--session-stream session))
                                     "")))))
      (when (stringp model)
        (setf (ende-stream-session-actual-model
               (ende-llm--session-stream session)) model))
      (when (stringp provider)
        (setf (ende-stream-session-actual-provider
               (ende-llm--session-stream session)) provider))
      (ende-llm--emit-event
       session
       (list :type :metadata
             :metadata (list :source :response
                             :model (ende-stream-session-actual-model
                                     (ende-llm--session-stream session))
                             :provider
                             (ende-stream-session-actual-provider
                              (ende-llm--session-stream session))))))))

(defun ende-llm--handle-usage (session usage)
  "Record and emit provider USAGE for SESSION."
  (when usage
    (ende-stream-record-usage
     (ende-llm--session-stream session) usage "completion_tokens")))

(defun ende-llm--first-choice (choices)
  "Return the first choice from parsed JSON CHOICES."
  (cond
   ((and (vectorp choices) (> (length choices) 0))
    (aref choices 0))
   ((consp choices) (car choices))))

(defun ende-llm--first-json-value (objects keys)
  "Return the first non-nil value for KEYS across JSON OBJECTS."
  (cl-loop for object in objects
           thereis (cl-loop for key in keys
                            for value = (ende-protocol-json-get key object)
                            when value return value)))

(defun ende-llm--response-event (json)
  "Normalize completion fields from one parsed provider JSON object."
  (let* ((choice (ende-llm--first-choice
                  (ende-protocol-json-get "choices" json)))
         (delta (ende-protocol-json-get "delta" choice))
         (message (ende-protocol-json-get "message" choice)))
    (list
     :finish-reason (ende-protocol-json-get "finish_reason" choice)
     :reasoning (ende-llm--first-json-value
                 (list delta choice)
                 '("reasoning_content" "reasoning"))
     :reasoning-content (ende-llm--first-json-value
                         (list delta choice message)
                         '("reasoning_content"))
     :content (or (ende-llm--first-json-value
                   (list delta choice)
                   '("content" "text"))
                  (ende-protocol-json-get "content" message))
     :images (or (ende-protocol-json-get "images" delta)
                 (ende-protocol-json-get "images" message))
     :tool-calls (or (ende-protocol-json-get "tool_calls" delta)
                     (ende-protocol-json-get "tool_calls" message)))))

(defun ende-llm--sequence (value)
  "Return JSON array VALUE as a list."
  (cond
   ((vectorp value) (append value nil))
   ((listp value) value)))

(defun ende-llm--collect-tool-calls (session calls)
  "Add streaming OpenAI-compatible CALLS to SESSION."
  (let ((table (or (ende-llm--session-tool-calls session)
                   (let ((table (make-hash-table :test #'eql)))
                     (setf (ende-llm--session-tool-calls session) table)
                     table))))
    (dolist (call (ende-llm--sequence calls))
      (let* ((function (ende-protocol-json-get "function" call))
             (index (or (ende-protocol-json-get "index" call)
                        (hash-table-count table)))
             (previous (gethash index table))
             (id (or (ende-protocol-json-get "id" call)
                     (plist-get previous :id)))
             (name (ende-protocol-json-get "name" function))
             (arguments (ende-protocol-json-get "arguments" function)))
        (when (or (and (stringp name) (> (length name) 0))
                  (and (stringp arguments) (> (length arguments) 0)))
          (ende-stream-note-generation-start
           (ende-llm--session-stream session)))
        (puthash index
                 (list :id id
                       :name (concat (or (plist-get previous :name) "")
                                     (or name ""))
                       :arguments
                       (concat (or (plist-get previous :arguments) "")
                               (or arguments "")))
                 table)))))

(defun ende-llm--tool-call-list (session)
  "Return collected calls from SESSION in provider index order."
  (let (calls)
    (when (hash-table-p (ende-llm--session-tool-calls session))
      (maphash (lambda (index call)
                 (push (cons index call) calls))
               (ende-llm--session-tool-calls session)))
    (mapcar #'cdr
            (sort calls (lambda (left right) (< (car left) (car right)))))))

(defun ende-llm--handle-json (session json)
  "Handle one parsed streaming JSON object for SESSION."
  (if (ende-protocol-json-has-key-p "error" json)
      (ende-llm--handle-provider-error
       session (ende-protocol-json-get "error" json))
    (ende-llm--handle-metadata session json)
    (unless (ende-llm--terminal-p session)
      (let* ((event (ende-llm--response-event json))
             (finish-reason (plist-get event :finish-reason))
             (reasoning (plist-get event :reasoning))
             (reasoning-content (plist-get event :reasoning-content))
             (content (plist-get event :content))
             (tool-calls (plist-get event :tool-calls)))
        (when tool-calls
          (ende-llm--collect-tool-calls session tool-calls))
        (when (and content (not (stringp content)))
          (dolist (part (ende-content-output-parts content nil))
            (if (eq (plist-get part :type) :text)
                (ende-llm--emit-delta session :content (plist-get part :text))
              (ende-stream-record-image (ende-llm--session-stream session) part))))
        (when (stringp finish-reason)
          (setf (ende-stream-session-finish-reason
                 (ende-llm--session-stream session)) finish-reason))
        (when (stringp reasoning-content)
          (push reasoning-content
                (ende-llm--session-reasoning-content-parts session)))
        (ende-llm--handle-usage
         session (ende-protocol-json-get "usage" json))
        (when (and (not (ende-llm--terminal-p session))
                   (stringp reasoning) (> (length reasoning) 0))
          (ende-llm--emit-status session "Thinking")
          (ende-llm--emit-delta session :thinking reasoning))
        (when (and (not (ende-llm--terminal-p session))
                   (stringp content) (> (length content) 0))
          (ende-protocol-reasoning-feed
           (ende-llm--session-reasoning session) content
           (ende-llm--parser-callbacks session)))
        (dolist (part (ende-content-output-parts nil (plist-get event :images)))
          (ende-stream-record-image (ende-llm--session-stream session) part))))))

(defun ende-llm--handle-events (session events)
  "Handle parsed SSE EVENTS for SESSION."
  (dolist (event events)
    (unless (ende-llm--terminal-p session)
      (pcase (plist-get event :type)
        (:done (ende-llm--finish-success session))
        (:json (ende-llm--handle-json session (plist-get event :value)))))))

(defun ende-llm--http-error-detail (session)
  "Return a provider error detail parsed from SESSION's response preview."
  (ende-transport-error-detail
   (ende-stream-preview-text (ende-llm--session-stream session))))

(defun ende-llm--result (session)
  "Build the normalized result plist for SESSION."
  (ende-stream-result
   (ende-llm--session-stream session) "completion_tokens"
   (ende-llm--session-model session) nil
   (list :reasoning-content
         (and (ende-llm--session-reasoning-content-parts session)
              (mapconcat #'identity
                         (reverse
                          (ende-llm--session-reasoning-content-parts session))
                         ""))
         :tool-calls (ende-llm--tool-call-list session))))

(defun ende-llm--handoff-to-batch (session)
  "Retry SESSION through OpenRouter Batch after its exact Batch-only error."
  (let ((error
         (ende-openrouter-batch-handoff
          (ende-llm--session-messages session)
          (ende-transport-session-callbacks (ende-llm--lifecycle session))
          (ende-llm--session-profile session))))
    (if error
        (ende-llm--fail session error)
      (when (ende-llm--session-debug-record session)
        (ende-debug-record-completion
         (ende-llm--session-debug-record session) 'failure
         (ende-stream-session-usage (ende-llm--session-stream session))
         (ende-llm--response-status session)
         (ende-error-create :kind 'http
                            :message "streaming request handed off to Batch"))
        (setf (ende-llm--session-debug-record session) nil))
      (ende-transport-session-transfer (ende-llm--lifecycle session))
      (ende-transport-session-cleanup (ende-llm--lifecycle session)))))

(defun ende-llm--finish-success (session &optional headers)
  "Validate and successfully finish SESSION, or settle it with an error."
  (unless (ende-llm--terminal-p session)
    (condition-case finish-error
        (let* ((headers (or headers
                            (ende-stream-response-headers
                             (ende-llm--session-stream session))))
               (status (car headers))
               (content-type (cdr headers))
               (detail (and (or (null status) (< status 200) (>= status 300))
                            (ende-llm--http-error-detail session)))
               (content-type-error
                (and (plist-get (ende-llm--session-profile session) :require-sse)
                     (ende-stream-content-type-error
                      content-type 'required))))
          (cond
           ((ende-openrouter-batch-auto-fallback-p
             (ende-llm--session-profile session) status detail)
            (ende-llm--handoff-to-batch session))
           ((not (ende-stream-http-success-p status))
            (ende-llm--fail
             session
             (ende-error-create
              :kind 'http
              :message (if detail
                           (format "HTTP status %s: %s" status detail)
                         (format "unexpected HTTP status %s" status)))))
           (content-type-error
            (ende-llm--fail session content-type-error))
           (t
            (ende-llm--handle-events
             session
             (ende-protocol-sse-finish
              (ende-stream-session-parser
               (ende-llm--session-stream session))))
            (unless (ende-llm--terminal-p session)
              (let ((reasoning-error
                     (ende-protocol-reasoning-finish
                      (ende-llm--session-reasoning session)
                      (ende-llm--parser-callbacks session))))
                (cond
                 (reasoning-error
                  (ende-llm--fail session reasoning-error))
                 ((ende-llm--session-provider-error session)
                  (ende-llm--fail
                   session (ende-llm--session-provider-error session)))
                 (t
                  (ende-llm--diagnose-result session)
                  (ende-llm--settle
                   session 'success (ende-llm--result session)))))))))
      ((error quit)
       (ende-llm--fail
        session
        (ende-error-create :kind 'protocol
                           :message (error-message-string finish-error)))))))

(defun ende-llm--finish-process-error (session process event &optional idle-error)
  "Settle SESSION for failed PROCESS with sentinel EVENT."
  (if idle-error
      (ende-llm--fail session idle-error)
    (let ((status (process-exit-status process))
          (stderr-buffer
           (ende-transport-session-stderr-buffer
            (ende-llm--lifecycle session))))
      (ende-llm--fail
       session
       (ende-error-create
        :kind (if (= status 28) 'timeout 'transport)
        :message
        (or (and (buffer-live-p stderr-buffer)
                 (with-current-buffer stderr-buffer
                   (let ((text (string-trim
                                (substring (buffer-string) 0
                                           (if ende-max-error-body-bytes
                                               (min (buffer-size)
                                                    ende-max-error-body-bytes)
                                             (buffer-size))))))
                     (unless (string-empty-p text) text))))
            (format "curl exited with status %d (%s)"
                    status (string-trim event))))))))

(defun ende-llm--filter-error (session error over-limit)
  "Settle SESSION for SSE parser ERROR and whether the response OVER-LIMIT."
  (ende-llm--fail
   session
   (if over-limit
       (ende-error-create
        :kind 'resource
        :message "response exceeds ende-max-response-bytes")
     (ende-error-create :kind 'protocol
                        :message (error-message-string error)))))

(defun ende-llm--header-error (session error)
  "Settle SESSION for response-header parsing ERROR."
  (ende-llm--fail
   session (ende-error-create :kind 'protocol
                              :message (error-message-string error))))

(defun ende-llm--process-filter (session _process chunk)
  "Consume one curl output CHUNK for SESSION."
  (ende-stream-filter
   (ende-llm--session-stream session) chunk
   (apply-partially #'ende-llm--handle-events session)
   (apply-partially #'ende-llm--fail session)
   (apply-partially #'ende-llm--filter-error session)))

(defun ende-llm--process-sentinel (session process event)
  "Handle PROCESS terminal EVENT for SESSION."
  (ende-stream-sentinel
   (ende-llm--session-stream session) process event
   (apply-partially #'ende-llm--finish-process-error session)
   (apply-partially #'ende-llm--finish-success session)
   (apply-partially #'ende-llm--header-error session)
   t))

(defun ende-llm--discovery-result (session metadata)
  "Emit discovered METADATA for a live SESSION."
  (when metadata
    (ende-llm--emit-event
     session (list :type :metadata :metadata metadata))))

(defun ende-llm--tool-call-json (call)
  "Encode normalized tool CALL for a Chat Completions request."
  `((:id . ,(plist-get call :id))
    (:type . "function")
    (:function . ((:name . ,(plist-get call :name))
                  (:arguments . ,(plist-get call :arguments))))))

(defun ende-llm--message-json (message &optional profile)
  "Encode internal MESSAGE for a Chat Completions request under PROFILE."
  (if (and (consp message) (stringp (car message)))
      `((:role . ,(car message))
        (:content . ,(ende-content-provider-content (car message) (cdr message) 'chat profile)))
    (let ((role (plist-get message :role))
          (content (plist-get message :content))
          (calls (plist-get message :tool-calls))
          (call-id (plist-get message :tool-call-id)))
      (append (list (cons :role role)
                          (cons :content (ende-content-provider-content role content 'chat profile)))
              (when (and profile
                         (eq (plist-get profile :reasoning-history-field)
                             'reasoning_content)
                         (plist-get profile :tool-definitions)
                         (equal role "assistant")
                         (stringp (plist-get message :reasoning-content)))
                (list (cons :reasoning_content
                            (plist-get message :reasoning-content))))
              (when calls
                (list (cons :tool_calls
                            (mapcar #'ende-llm--tool-call-json calls))))
              (when call-id
                (list (cons :tool_call_id call-id)))))))

(defun ende-llm--request-body (messages profile)
  "Return the JSON request object for MESSAGES and PROFILE."
  (let* ((stream-usage (if (plist-member profile :stream-usage)
                           (plist-get profile :stream-usage)
                         t))
         (body `((:model . ,(plist-get profile :model))
                 (:messages . ,(mapcar
                                (lambda (message)
                                  (ende-llm--message-json message profile))
                                (ende-content-provider-messages messages)))
                 (:stream . t)
                 ,@(when stream-usage
                     '((:stream_options . ((:include_usage . t))))))))
    (when (plist-get profile :tool-definitions)
      (setq body
            (append body
                    (list (cons :tools (plist-get profile :tool-definitions))
                          (cons :tool_choice "auto")))))
    (when (plist-get profile :output-tokens)
      (setq body (append body (list (cons :max_tokens (plist-get profile :output-tokens))))))
    (ende-provider-config-encode-parameters
     (ende-controls-encode body (plist-get profile :controls) profile) profile)))

(defun ende-llm--start-prepared (session messages json-data request-body record)
  "Start SESSION with prepared JSON-DATA, REQUEST-BODY and debug RECORD."
  (let* ((default-directory (ende-curl-local-directory))
         (profile (ende-llm--session-profile session)))
    (setf (ende-llm--session-messages session) messages)
    (setf (ende-llm--session-json-data session) json-data)
    (let ((request
           (ende-curl-start-request
            :profile profile
            :name (format "ende-curl-%s-%s"
                          (or (plist-get profile :name) "request")
                          (or (plist-get profile :job-id) "unknown"))
            :command (lambda (headers-file config-file)
                       (ende-curl-command
                        profile headers-file config-file
                        (and (listp json-data) (plist-get json-data :file))))
            :filter (apply-partially #'ende-llm--process-filter session)
            :sentinel (apply-partially #'ende-llm--process-sentinel session))))
      (ende-transport-session-set-resources
       (ende-llm--lifecycle session) request))
    (setf (ende-llm--session-debug-record session) record)
    (ende-llm--diagnose-request session request-body)
    (when (and (not (ende-llm--terminal-p session))
               ende-provider-discovery-enabled)
      (ende-transport-session-add-canceller
       (ende-llm--lifecycle session)
       (ende-provider-discover
        profile (apply-partially #'ende-llm--discovery-result session))))
    (unless (ende-llm--terminal-p session)
      (ende-request-send
       (ende-transport-session-process (ende-llm--lifecycle session)) json-data))))

(defun ende-llm--start-session (session messages)
  "Allocate and start SESSION for MESSAGES."
  (ende-validate-resource-options)
  (ende-transport-session-invoke
   (ende-llm--lifecycle session) :on-start
   (apply-partially #'ende-llm--cancel session))
  (unless (ende-llm--terminal-p session)
    (ende-request-prepare
     (ende-llm--lifecycle session) 'ende-llm #'ende-llm--request-body
     messages (ende-llm--session-profile session)
     (list "openai-compatible" (plist-get (ende-llm--session-profile session) :url))
     (apply-partially #'ende-llm--start-prepared session messages))))

(defun ende-llm--dispatch-error (callbacks caught)
  "Deliver dispatch error CAUGHT through CALLBACKS."
  (let ((session (ende-llm--make-session :callbacks callbacks)))
    (ende-llm--fail
     session
     (ende-error-create :kind 'startup
                        :message (error-message-string caught)))))

(defun ende-llm-default (messages callbacks &optional options)
  "Send MESSAGES asynchronously using a callback-safe curl transport."
  (let* ((profile (ende-provider-config-copy options))
         (validation-error
          (condition-case caught
              (progn
                (ende-provider-validate-profile profile)
                (when (eq (plist-get profile :transport) 'openrouter-batch)
                  (ende-openrouter-batch--validate-options profile))
                nil)
            ((error quit) caught))))
    (if validation-error
        (ende-llm--dispatch-error callbacks validation-error)
      (cond
       ((eq (plist-get profile :transport) 'openrouter-batch)
        (ende-openrouter-batch-default messages callbacks profile))
       ((eq (plist-get profile :service) 'openai-codex)
        (ende-codex-default messages callbacks profile))
       ((eq (plist-get profile :api) 'openai-responses)
        (ende-responses-default messages callbacks profile))
       ((eq (plist-get profile :api) 'anthropic-messages)
        (ende-messages-default messages callbacks profile))
       (t
        (let* ((session
                (ende-llm--make-session
                 :profile profile :callbacks callbacks
                 :model (plist-get profile :model)
                 :reasoning (ende-protocol-make-reasoning)
                 :tool-calls (make-hash-table :test #'eql))))
          (condition-case startup-error
              (ende-llm--start-session session messages)
            ((error quit)
             (ende-llm--fail
              session
              (ende-error-create :kind 'startup
                                 :message
                                 (error-message-string startup-error)))))
          (ende-transport-session-process
           (ende-llm--lifecycle session))))))))

(provide 'ende-llm)

;;; ende-llm.el ends here
