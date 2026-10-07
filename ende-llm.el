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
(require 'ende-curl)
(require 'ende-protocol)
(require 'ende-transport)
(require 'ende-provider)
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
  profile lifecycle discovery-cancel model messages json-data sse reasoning
  content-parts thinking-parts usage
  tool-calls
  provider-error actual-model actual-provider estimated-chars received-bytes
  response-preview response-preview-parts response-preview-bytes finish-reason
  generation-start last-status last-usage-update)

(defun ende-llm--make-session (&rest arguments)
  "Create a Chat Completions session from protocol ARGUMENTS.
The :callbacks argument initializes the composed transport lifecycle."
  (let* ((callbacks (plist-get arguments :callbacks))
         (session
          (apply #'ende-llm--allocate-session
                 (cl-loop for (key value) on arguments by #'cddr
                          unless (eq key :callbacks)
                          append (list key value)))))
    (setf (ende-llm--session-lifecycle session)
          (ende-transport-session-create
           callbacks (apply-partially #'ende-llm--fail session)))
    session))

(defun ende-llm--lifecycle (session)
  "Return the composed transport lifecycle for SESSION."
  (ende-llm--session-lifecycle session))

(defun ende-llm--terminal-p (session)
  "Return non-nil when SESSION's transport has settled."
  (ende-transport-session-terminal (ende-llm--lifecycle session)))

(defun ende-llm--invoke-callback (session name &rest arguments)
  "Invoke callback NAME for SESSION with ARGUMENTS safely."
  (apply #'ende-transport-session-invoke
         (ende-llm--lifecycle session) name arguments))

(defun ende-llm--cancel-discovery-waiter (session)
  "Cancel SESSION's discovery waiter without disturbing other callers."
  (let ((cancel (ende-llm--session-discovery-cancel session)))
    (setf (ende-llm--session-discovery-cancel session) nil)
    (when cancel (ende-ignore-errors cancel))))

(defun ende-llm--cleanup-session (session)
  "Cancel auxiliary waiters and clean resources owned by SESSION."
  (ende-llm--cancel-discovery-waiter session)
  (ende-transport-session-cleanup (ende-llm--lifecycle session)))

(defun ende-llm--settle (session outcome value)
  "Settle SESSION exactly once with OUTCOME and VALUE."
  (unless (ende-llm--terminal-p session)
    (ende-llm--cancel-discovery-waiter session)
    (unwind-protect
        (ende-transport-session-settle
         (ende-llm--lifecycle session)
         outcome value
         (when (or (ende-llm--session-content-parts session)
                   (ende-llm--session-thinking-parts session)
                   (ende-llm--session-usage session)
                   (ende-llm--session-finish-reason session))
           (ende-llm--result session)))
      (ende-llm--cleanup-session session))))

(defun ende-llm--fail (session error)
  "Settle SESSION with structured ERROR."
  (ende-llm--settle session 'failure error))

(defun ende-llm--cancel (session)
  "Cancel SESSION."
  (ende-llm--fail
   session (ende-error-create :kind 'cancelled :message "Process cancelled")))

(defun ende-llm--emit-event (session event)
  "Emit EVENT for SESSION unless it has settled."
  (unless (ende-llm--terminal-p session)
    (ende-llm--invoke-callback session :on-event event)))

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
  (let ((items (when (ende-llm--session-content-parts session)
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
  (when (and (not (ende-llm--terminal-p session))
             (stringp text) (> (length text) 0))
    (if (and ende-max-output-chars
             (> (+ (ende-llm--session-estimated-chars session) (length text))
                ende-max-output-chars))
        (ende-llm--fail
         session
         (ende-error-create
          :kind 'resource
          :message "generated output exceeds ende-max-output-chars"))
      (if (eq channel :content)
          (push text (ende-llm--session-content-parts session))
        (push text (ende-llm--session-thinking-parts session)))
      (cl-incf (ende-llm--session-estimated-chars session) (length text))
      (unless (ende-llm--session-generation-start session)
        (setf (ende-llm--session-generation-start session) (float-time)))
      (ende-llm--emit-event
       session (list :type :delta :channel channel :text text))
      (let ((now (float-time)))
        (when (and (not (ende-llm--terminal-p session))
                   (or (null (ende-llm--session-last-usage-update session))
                       (>= (- now
                              (ende-llm--session-last-usage-update session))
                           ende-usage-update-interval)))
          (setf (ende-llm--session-last-usage-update session) now)
          (let* ((tokens (max 1 (floor
                                 (/ (ende-llm--session-estimated-chars session)
                                    4.0))))
                 (elapsed (max 0.001
                               (- now
                                  (ende-llm--session-generation-start session)))))
            (ende-llm--emit-event
             session (list :type :usage :tokens tokens
                           :speed (/ (float tokens) elapsed)
                           :estimated t))))))))

(defun ende-llm--parser-callbacks (session)
  "Return reasoning parser callbacks for SESSION."
  (list :on-status (apply-partially #'ende-llm--emit-status session)
        :on-delta (apply-partially #'ende-llm--emit-delta session)))

(defun ende-llm--response-status (session)
  "Return SESSION's HTTP response status, or nil."
  (condition-case nil
      (car (ende-curl-response-headers
            (ende-transport-session-headers-file
             (ende-llm--lifecycle session))))
    (error nil)))

(defun ende-llm--handle-provider-error (session json-error)
  "Settle SESSION from a provider JSON-ERROR value."
  (let* ((reported-message
          (format "%s" (or (ende-protocol-json-get "message" json-error)
                           json-error)))
         (status (ende-llm--response-status session))
         (http-error (and status (or (< status 200) (>= status 300))))
         (error (ende-error-create
                 :kind (if http-error 'http 'provider)
                 :message (if http-error
                              (format "HTTP status %s: %s"
                                      status reported-message)
                            reported-message)
                 :details json-error)))
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
                                 (or (ende-llm--session-actual-model session)
                                     ""))))
              (and (stringp provider)
                   (not (string= provider
                                 (or (ende-llm--session-actual-provider session)
                                     "")))))
      (when (stringp model)
        (setf (ende-llm--session-actual-model session) model))
      (when (stringp provider)
        (setf (ende-llm--session-actual-provider session) provider))
      (ende-llm--emit-event
       session
       (list :type :metadata
             :metadata (list :source :response
                             :model (ende-llm--session-actual-model session)
                             :provider
                             (ende-llm--session-actual-provider session)))))))

(defun ende-llm--handle-usage (session usage)
  "Record and emit provider USAGE for SESSION."
  (when usage
    (setf (ende-llm--session-usage session) usage)
    (let* ((reported-tokens
            (ende-protocol-json-get "completion_tokens" usage))
           (tokens (if (numberp reported-tokens) reported-tokens 0))
           (elapsed (max 0.001
                         (- (float-time)
                            (or (ende-llm--session-generation-start session)
                                (float-time))))))
      (ende-llm--emit-event
       session (list :type :usage :tokens tokens
                     :speed (if (numberp reported-tokens)
                                (/ (float tokens) elapsed)
                              0.0)
                     :usage usage)))))

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
     :content (or (ende-llm--first-json-value
                   (list delta choice)
                   '("content" "text"))
                  (ende-protocol-json-get "content" message))
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
             (content (plist-get event :content))
             (tool-calls (plist-get event :tool-calls)))
        (when tool-calls
          (ende-llm--collect-tool-calls session tool-calls))
        (when (stringp finish-reason)
          (setf (ende-llm--session-finish-reason session) finish-reason))
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
           (ende-llm--parser-callbacks session)))))))

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
   (ende-llm--response-preview-text session)))

(defun ende-llm--result (session)
  "Build the normalized result plist for SESSION."
  (let* ((usage (ende-llm--session-usage session))
         (reported (and usage
                        (ende-protocol-json-get "completion_tokens" usage)))
         (tokens (or reported
                     (max 0 (floor
                             (/ (ende-llm--session-estimated-chars session)
                                4.0)))))
         (started (or (ende-llm--session-generation-start session)
                      (float-time))))
    (list :content (mapconcat #'identity
                              (reverse
                               (ende-llm--session-content-parts session)) "")
          :thinking (mapconcat #'identity
                               (reverse
                                (ende-llm--session-thinking-parts session)) "")
          :usage usage :tokens tokens :estimated (null reported)
          :speed (/ (float tokens)
                    (max 0.001 (- (float-time) started)))
          :finish-reason (ende-llm--session-finish-reason session)
          :tool-calls (ende-llm--tool-call-list session)
          :provider (ende-llm--session-actual-provider session)
          :model (or (ende-llm--session-actual-model session)
                     (ende-llm--session-model session)))))

(defun ende-llm--handoff-to-batch (session)
  "Retry SESSION through OpenRouter Batch after its exact Batch-only error."
  (if (plist-get (ende-llm--session-profile session) :tool-definitions)
      (ende-llm--fail
       session
       (ende-error-create
        :kind 'unsupported
        :message "Tool calls are unavailable with the OpenRouter batch transport"))
    (let ((callbacks
           (ende-transport-session-callbacks (ende-llm--lifecycle session)))
          (profile (copy-tree (ende-llm--session-profile session)))
          (messages (ende-llm--session-messages session)))
      (setq profile (plist-put profile :transport 'openrouter-batch))
      (condition-case caught
        (progn
          (ende-openrouter-batch-default messages callbacks profile)
          (ende-transport-session-transfer (ende-llm--lifecycle session))
          (ende-llm--cleanup-session session))
        ((error quit)
         (ende-llm--fail
          session
          (ende-error-create
           :kind 'startup :message (error-message-string caught))))))))

(defun ende-llm--finish-success (session)
  "Validate and successfully finish SESSION, or settle it with an error."
  (unless (ende-llm--terminal-p session)
    (condition-case finish-error
        (let* ((headers (ende-curl-response-headers
                         (ende-transport-session-headers-file
                          (ende-llm--lifecycle session))))
               (status (car headers))
               (content-type (cdr headers))
               (detail (and (or (null status) (< status 200) (>= status 300))
                            (ende-llm--http-error-detail session))))
          (cond
           ((and (eq (plist-get (ende-llm--session-profile session)
                                :transport)
                     'auto)
                 (numberp status)
                 (= status 404)
                 (string-match-p "[Bb]atch API" (or detail "")))
            (ende-llm--handoff-to-batch session))
           ((or (null status) (< status 200) (>= status 300))
            (ende-llm--fail
             session
             (ende-error-create
              :kind 'http
              :message (if detail
                           (format "HTTP status %s: %s" status detail)
                         (format "unexpected HTTP status %s" status)))))
           ((and (plist-get (ende-llm--session-profile session) :require-sse)
                 (or (null content-type)
                     (not (ende-curl-event-stream-p content-type))))
            (ende-llm--fail
             session
             (ende-error-create
              :kind 'protocol
              :message (format "expected text/event-stream, got %s"
                               (or content-type "unknown")))))
           (t
            (ende-llm--handle-events
             session
             (ende-protocol-sse-finish
              (ende-llm--session-sse session)))
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

(defun ende-llm--finish-process-error (session process event)
  "Settle SESSION for failed PROCESS with sentinel EVENT."
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
                  status (string-trim event)))))))

(defun ende-llm--response-preview-text (session)
  "Return SESSION's bounded response preview as a string."
  (let ((parts (ende-llm--session-response-preview-parts session)))
    (if parts
        (mapconcat #'identity (nreverse parts) "")
      (or (ende-llm--session-response-preview session) ""))))

(defun ende-llm--append-response-preview (session chunk)
  "Append a bounded, byte-safe prefix of CHUNK to SESSION's preview."
  (let* ((used (or (ende-llm--session-response-preview-bytes session) 0))
         (remaining (and ende-max-error-body-bytes
                         (- ende-max-error-body-bytes used)))
         (prefix (if remaining
                     (and (> remaining 0)
                          (ende-transport-byte-prefix chunk remaining))
                   chunk)))
    (when prefix
      (push prefix (ende-llm--session-response-preview-parts session))
      (setf (ende-llm--session-response-preview-bytes session)
            (+ used (string-bytes prefix))))))

(defun ende-llm--process-filter (session _process chunk)
  "Consume one curl output CHUNK for SESSION."
  (unless (ende-llm--terminal-p session)
    (let* ((received (ende-llm--session-received-bytes session))
           (chunk-bytes (string-bytes chunk))
           (total (+ received chunk-bytes))
           (remaining (and ende-max-response-bytes
                           (- ende-max-response-bytes received)))
           (processable (if (and ende-max-response-bytes
                                 (> total ende-max-response-bytes))
                            (ende-transport-byte-prefix chunk (max 0 remaining))
                          chunk)))
      (setf (ende-llm--session-received-bytes session) total)
      (ende-llm--append-response-preview session chunk)
      (condition-case filter-error
          (when (> (length processable) 0)
            (ende-llm--handle-events
             session
             (ende-protocol-sse-feed
              (ende-llm--session-sse session) processable)))
        ((error quit)
         (if (and ende-max-response-bytes
                  (> total ende-max-response-bytes))
             (ende-llm--fail
              session
              (ende-error-create
               :kind 'resource
               :message "response exceeds ende-max-response-bytes"))
           (ende-llm--fail
            session
            (ende-error-create :kind 'protocol
                               :message
                               (error-message-string filter-error))))))
      (when (and ende-max-response-bytes
                 (> total ende-max-response-bytes)
                 (not (ende-llm--terminal-p session)))
        (ende-llm--fail
         session
         (ende-error-create
          :kind 'resource
          :message "response exceeds ende-max-response-bytes"))))))

(defun ende-llm--process-sentinel (session process event)
  "Handle PROCESS terminal EVENT for SESSION."
  (when (and (not (ende-llm--terminal-p session))
             (memq (process-status process) '(exit signal closed)))
    (cond
     ((process-get process :ende-idle-error)
      (ende-llm--fail session (process-get process :ende-idle-error)))
     ((= (process-exit-status process) 0)
      (ende-llm--finish-success session))
     (t (ende-llm--finish-process-error session process event)))))

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

(defun ende-llm--message-json (message)
  "Encode internal MESSAGE for a Chat Completions request."
  (if (and (consp message) (stringp (car message)))
      `((:role . ,(car message)) (:content . ,(cdr message)))
    (let ((role (plist-get message :role))
          (content (plist-get message :content))
          (calls (plist-get message :tool-calls))
          (call-id (plist-get message :tool-call-id)))
      (append (list (cons :role role) (cons :content content))
              (when calls
                (list (cons :tool_calls
                            (mapcar #'ende-llm--tool-call-json calls))))
              (when call-id
                (list (cons :tool_call_id call-id)))))))

(defun ende-llm--request-body (messages profile)
  "Return the JSON request object for MESSAGES and PROFILE."
  (let ((body `((:model . ,(plist-get profile :model))
                (:messages . ,(mapcar #'ende-llm--message-json messages))
                (:temperature . ,(plist-get profile :temperature))
                (:stream . t)
                (:stream_options . ((:include_usage . t))))))
    (let ((effort (ende-provider-reasoning-effort profile 'openrouter)))
      (when effort
        (setq body
              (append body
                      (list (cons :reasoning
                                  (list (cons :effort effort))))))))
    (when (plist-get profile :tool-definitions)
      (setq body
            (append body
                    (list (cons :tools (plist-get profile :tool-definitions))
                          (cons :tool_choice "auto")))))
    (if (integerp ende-max-output-tokens)
        (append body (list (cons :max_tokens ende-max-output-tokens)))
      body)))

(defun ende-llm--start-session (session messages)
  "Allocate and start SESSION for MESSAGES."
  (ende-validate-resource-options)
  (let* ((default-directory (ende-curl-local-directory))
         (profile (ende-llm--session-profile session))
         (request-body (ende-llm--request-body messages profile))
         (json-data (encode-coding-string
                     (json-encode request-body) 'utf-8)))
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
                        profile headers-file config-file))
            :filter (apply-partially #'ende-llm--process-filter session)
            :sentinel (apply-partially #'ende-llm--process-sentinel session))))
      (ende-transport-session-set-resources
       (ende-llm--lifecycle session) request))
    (ende-llm--invoke-callback
     session :on-start (apply-partially #'ende-llm--cancel session))
    (ende-llm--diagnose-request session request-body)
    (when (and (not (ende-llm--terminal-p session))
               ende-provider-discovery-enabled)
      (setf (ende-llm--session-discovery-cancel session)
            (ende-provider-discover
             profile (apply-partially #'ende-llm--discovery-result session))))
    (unless (ende-llm--terminal-p session)
      (process-send-string
       (ende-transport-session-process (ende-llm--lifecycle session))
       (ende-llm--session-json-data session))
      (process-send-eof
       (ende-transport-session-process (ende-llm--lifecycle session))))))

(defun ende-llm-default (messages callbacks &optional options)
  "Send MESSAGES asynchronously using a callback-safe curl transport."
  (cond
   ((eq (plist-get options :transport) 'openrouter-batch)
    (ende-openrouter-batch-default messages callbacks options))
   ((eq (plist-get options :wire-api) 'responses)
    (ende-codex-default messages callbacks options))
   (t
    (let* ((profile (if (plist-get options :name)
                        options
                      (ende-plist-merge (ende-provider-profile) options)))
           (session
            (ende-llm--make-session
             :profile profile :callbacks callbacks
             :model (plist-get profile :model)
             :sse (ende-protocol-make-sse)
             :reasoning (ende-protocol-make-reasoning)
             :tool-calls (make-hash-table :test #'eql)
             :estimated-chars 0 :received-bytes 0
             :response-preview-parts nil :response-preview-bytes 0)))
      (condition-case startup-error
          (ende-llm--start-session session messages)
        ((error quit)
         (ende-llm--fail
          session
          (ende-error-create :kind 'startup
                             :message (error-message-string startup-error)))))
      (ende-transport-session-process
       (ende-llm--lifecycle session))))))

(provide 'ende-llm)

;;; ende-llm.el ends here
