;;; deslop-llm.el --- LLM completion transport for deslop -*- lexical-binding: t; -*-

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
(require 'deslop-core)
(require 'deslop-curl)
(require 'deslop-protocol)
(require 'deslop-transport)
(require 'deslop-provider)
(require 'deslop-codex)

;;;; Hookable LLM caller

(defvar deslop-llm-function #'deslop-llm-default
  "Function used to dispatch requests to the LLM.
Called with (MESSAGES CALLBACKS &optional OPTIONS).
CALLBACKS is a plist with keys :on-start, :on-event, :on-done, and :on-error.")

(defcustom deslop-usage-update-interval 0.1
  "Minimum seconds between estimated streaming usage updates."
  :type 'number
  :group 'deslop)

;;;; Client implementation

(cl-defstruct (deslop-llm--session
               (:constructor deslop-llm--make-session))
  "State owned by one asynchronous LLM transport request."
  profile callbacks lifecycle discovery-cancel model json-data process process-buffer stderr-buffer
  stderr-process
  headers-file config-file sse reasoning content-parts thinking-parts usage
  provider-error actual-model actual-provider estimated-chars received-bytes
  response-preview response-preview-parts response-preview-bytes finish-reason
  generation-start last-status last-usage-update terminal)

(defun deslop-llm--lifecycle (session)
  "Return the shared lifecycle object for SESSION, creating it if needed."
  (or (deslop-llm--session-lifecycle session)
      (let ((lifecycle
             (deslop-transport-session-create
              (deslop-llm--session-callbacks session)
              (apply-partially #'deslop-llm--fail session))))
        (deslop-transport-session-set-resources
         lifecycle
         (list :process (deslop-llm--session-process session)
               :response-buffer (deslop-llm--session-process-buffer session)
               :stderr-buffer (deslop-llm--session-stderr-buffer session)
               :stderr-process (deslop-llm--session-stderr-process session)
               :headers-file (deslop-llm--session-headers-file session)
               :config-file (deslop-llm--session-config-file session)))
        (setf (deslop-llm--session-lifecycle session) lifecycle))))

(defun deslop-llm--invoke-callback (session name &rest arguments)
  "Invoke callback NAME for SESSION with ARGUMENTS safely."
  (apply #'deslop-transport-session-invoke
         (deslop-llm--lifecycle session) name arguments))

(defun deslop-llm--cleanup-session (session)
  "Release all process resources owned by SESSION."
  (when (deslop-llm--session-lifecycle session)
    (deslop-transport-session-cleanup
     (deslop-llm--session-lifecycle session)))
  (setf (deslop-llm--session-process session) nil
        (deslop-llm--session-process-buffer session) nil
        (deslop-llm--session-stderr-buffer session) nil
        (deslop-llm--session-stderr-process session) nil
        (deslop-llm--session-headers-file session) nil
        (deslop-llm--session-config-file session) nil
        (deslop-llm--session-lifecycle session) nil))

(defun deslop-llm--settle (session outcome value)
  "Settle SESSION exactly once with OUTCOME and VALUE."
  (unless (deslop-llm--session-terminal session)
    (setf (deslop-llm--session-terminal session) outcome)
    (let ((cancel (deslop-llm--session-discovery-cancel session)))
      (setf (deslop-llm--session-discovery-cancel session) nil)
      (when cancel
        (deslop-ignore-errors cancel)))
    (unwind-protect
        (deslop-transport-session-settle
         (deslop-llm--lifecycle session)
         outcome value
         (when (or (deslop-llm--session-content-parts session)
                   (deslop-llm--session-thinking-parts session)
                   (deslop-llm--session-usage session)
                   (deslop-llm--session-finish-reason session))
           (deslop-llm--result session)))
      (deslop-llm--cleanup-session session)
      (setf (deslop-llm--session-callbacks session) nil))))

(defun deslop-llm--fail (session error)
  "Settle SESSION with structured ERROR."
  (deslop-llm--settle session 'failure error))

(defun deslop-llm--cancel (session)
  "Cancel SESSION."
  (deslop-llm--fail
   session (deslop-error-create :kind 'cancelled :message "Process cancelled")))

(defun deslop-llm--emit-event (session event)
  "Emit EVENT for SESSION unless it has settled."
  (unless (deslop-llm--session-terminal session)
    (deslop-llm--invoke-callback session :on-event event)))

(defun deslop-llm--emit-status (session status)
  "Emit a changed STATUS for SESSION."
  (unless (equal status (deslop-llm--session-last-status session))
    (setf (deslop-llm--session-last-status session) status)
    (deslop-llm--emit-event session (list :type :status :status status))))

(defun deslop-llm--emit-delta (session channel text)
  "Record and emit nonempty TEXT on CHANNEL for SESSION."
  (when (and (not (deslop-llm--session-terminal session))
             (stringp text) (> (length text) 0))
    (if (and deslop-max-output-chars
             (> (+ (deslop-llm--session-estimated-chars session) (length text))
                deslop-max-output-chars))
        (deslop-llm--fail
         session
         (deslop-error-create
          :kind 'resource
          :message "generated output exceeds deslop-max-output-chars"))
      (if (eq channel :content)
          (push text (deslop-llm--session-content-parts session))
        (push text (deslop-llm--session-thinking-parts session)))
      (cl-incf (deslop-llm--session-estimated-chars session) (length text))
      (unless (deslop-llm--session-generation-start session)
        (setf (deslop-llm--session-generation-start session) (float-time)))
      (deslop-llm--emit-event
       session (list :type :delta :channel channel :text text))
      (let ((now (float-time)))
        (when (and (not (deslop-llm--session-terminal session))
                   (or (null (deslop-llm--session-last-usage-update session))
                       (>= (- now
                              (deslop-llm--session-last-usage-update session))
                           deslop-usage-update-interval)))
          (setf (deslop-llm--session-last-usage-update session) now)
          (let* ((tokens (max 1 (floor
                                 (/ (deslop-llm--session-estimated-chars session)
                                    4.0))))
                 (elapsed (max 0.001
                               (- now
                                  (deslop-llm--session-generation-start session)))))
            (deslop-llm--emit-event
             session (list :type :usage :tokens tokens
                           :speed (/ (float tokens) elapsed)
                           :estimated t))))))))

(defun deslop-llm--parser-callbacks (session)
  "Return reasoning parser callbacks for SESSION."
  (list :on-status (apply-partially #'deslop-llm--emit-status session)
        :on-delta (apply-partially #'deslop-llm--emit-delta session)))

(defun deslop-llm--response-status (session)
  "Return SESSION's HTTP response status, or nil."
  (condition-case nil
      (car (deslop-curl-response-headers
            (deslop-llm--session-headers-file session)))
    (error nil)))

(defun deslop-llm--handle-provider-error (session json-error)
  "Settle SESSION from a provider JSON-ERROR value."
  (let* ((reported-message
          (format "%s" (or (deslop-protocol-json-get "message" json-error)
                           json-error)))
         (status (deslop-llm--response-status session))
         (http-error (and status (or (< status 200) (>= status 300))))
         (error (deslop-error-create
                 :kind (if http-error 'http 'provider)
                 :message (if http-error
                              (format "HTTP status %s: %s"
                                      status reported-message)
                            reported-message)
                 :details json-error)))
    (setf (deslop-llm--session-provider-error session) error)
    (deslop-llm--fail session error)))

(defun deslop-llm--handle-metadata (session json)
  "Record and emit provider metadata in JSON for SESSION."
  (let ((model (deslop-protocol-json-get "model" json))
        (provider (or (deslop-protocol-json-get "provider" json)
                      (deslop-protocol-json-get "server" json)
                      (deslop-protocol-json-get "owned_by" json))))
    (when (or (and (stringp model)
                   (not (string= model
                                 (or (deslop-llm--session-actual-model session)
                                     ""))))
              (and (stringp provider)
                   (not (string= provider
                                 (or (deslop-llm--session-actual-provider session)
                                     "")))))
      (when (stringp model)
        (setf (deslop-llm--session-actual-model session) model))
      (when (stringp provider)
        (setf (deslop-llm--session-actual-provider session) provider))
      (deslop-llm--emit-event
       session
       (list :type :metadata
             :metadata (list :source :response
                             :model (deslop-llm--session-actual-model session)
                             :provider
                             (deslop-llm--session-actual-provider session)))))))

(defun deslop-llm--handle-usage (session usage)
  "Record and emit provider USAGE for SESSION."
  (when usage
    (setf (deslop-llm--session-usage session) usage)
    (let* ((reported-tokens
            (deslop-protocol-json-get "completion_tokens" usage))
           (tokens (if (numberp reported-tokens) reported-tokens 0))
           (elapsed (max 0.001
                         (- (float-time)
                            (or (deslop-llm--session-generation-start session)
                                (float-time))))))
      (deslop-llm--emit-event
       session (list :type :usage :tokens tokens
                     :speed (if (numberp reported-tokens)
                                (/ (float tokens) elapsed)
                              0.0)
                      :usage usage)))))

(defun deslop-llm--first-choice (choices)
  "Return the first choice from parsed JSON CHOICES."
  (cond
   ((and (vectorp choices) (> (length choices) 0))
    (aref choices 0))
   ((consp choices) (car choices))))

(defun deslop-llm--first-json-value (objects keys)
  "Return the first non-nil value for KEYS across JSON OBJECTS."
  (cl-loop for object in objects
           thereis (cl-loop for key in keys
                             for value = (deslop-protocol-json-get key object)
                            when value return value)))

(defun deslop-llm--response-event (json)
  "Normalize completion fields from one parsed provider JSON object."
  (let* ((choice (deslop-llm--first-choice
                  (deslop-protocol-json-get "choices" json)))
         (delta (deslop-protocol-json-get "delta" choice))
         (message (deslop-protocol-json-get "message" choice)))
    (list
      :finish-reason (deslop-protocol-json-get "finish_reason" choice)
     :reasoning (deslop-llm--first-json-value
                 (list delta choice)
                 '("reasoning_content" "reasoning"))
     :content (or (deslop-llm--first-json-value
                   (list delta choice)
                   '("content" "text"))
                   (deslop-protocol-json-get "content" message)))))

(defun deslop-llm--handle-json (session json)
  "Handle one parsed streaming JSON object for SESSION."
  (if (deslop-protocol-json-has-key-p "error" json)
      (deslop-llm--handle-provider-error
        session (deslop-protocol-json-get "error" json))
    (deslop-llm--handle-metadata session json)
    (unless (deslop-llm--session-terminal session)
      (let* ((event (deslop-llm--response-event json))
             (finish-reason (plist-get event :finish-reason))
             (reasoning (plist-get event :reasoning))
             (content (plist-get event :content)))
        (when (stringp finish-reason)
          (setf (deslop-llm--session-finish-reason session) finish-reason))
        (deslop-llm--handle-usage
         session (deslop-protocol-json-get "usage" json))
        (when (and (not (deslop-llm--session-terminal session))
                   (stringp reasoning) (> (length reasoning) 0))
          (deslop-llm--emit-status session "Thinking")
          (deslop-llm--emit-delta session :thinking reasoning))
        (when (and (not (deslop-llm--session-terminal session))
                   (stringp content) (> (length content) 0))
          (deslop-protocol-reasoning-feed
           (deslop-llm--session-reasoning session) content
           (deslop-llm--parser-callbacks session)))))))

(defun deslop-llm--handle-events (session events)
  "Handle parsed SSE EVENTS for SESSION."
  (dolist (event events)
    (unless (deslop-llm--session-terminal session)
      (pcase (plist-get event :type)
        (:done (deslop-llm--finish-success session))
        (:json (deslop-llm--handle-json session (plist-get event :value)))))))

(defun deslop-llm--http-error-detail (session)
  "Return a provider error detail parsed from SESSION's response preview."
  (deslop-transport-error-detail
   (deslop-llm--response-preview-text session)))

(defun deslop-llm--result (session)
  "Build the normalized result plist for SESSION."
  (let* ((usage (deslop-llm--session-usage session))
         (reported (and usage
                         (deslop-protocol-json-get "completion_tokens" usage)))
         (tokens (or reported
                     (max 0 (floor
                             (/ (deslop-llm--session-estimated-chars session)
                                4.0)))))
         (started (or (deslop-llm--session-generation-start session)
                      (float-time))))
     (list :content (mapconcat #'identity
                               (reverse
                                (deslop-llm--session-content-parts session)) "")
           :thinking (mapconcat #'identity
                                (reverse
                                 (deslop-llm--session-thinking-parts session)) "")
          :usage usage :tokens tokens :estimated (null reported)
           :speed (/ (float tokens)
                     (max 0.001 (- (float-time) started)))
           :finish-reason (deslop-llm--session-finish-reason session)
           :provider (deslop-llm--session-actual-provider session)
          :model (or (deslop-llm--session-actual-model session)
                     (deslop-llm--session-model session)))))

(defun deslop-llm--finish-success (session)
  "Validate and successfully finish SESSION, or settle it with an error."
  (unless (deslop-llm--session-terminal session)
    (condition-case finish-error
        (let* ((headers (deslop-curl-response-headers
                         (deslop-llm--session-headers-file session)))
               (status (car headers))
               (content-type (cdr headers)))
          (cond
           ((or (null status) (< status 200) (>= status 300))
            (let ((detail (deslop-llm--http-error-detail session)))
              (deslop-llm--fail
               session
               (deslop-error-create
                :kind 'http
                :message (if detail
                             (format "HTTP status %s: %s" status detail)
                           (format "unexpected HTTP status %s" status))))))
           ((and (plist-get (deslop-llm--session-profile session) :require-sse)
                 (or (null content-type)
                     (let ((case-fold-search t))
                       (not (string-match-p "text/event-stream"
                                            content-type)))))
            (deslop-llm--fail
             session
             (deslop-error-create
              :kind 'protocol
              :message (format "expected text/event-stream, got %s"
                               (or content-type "unknown")))))
           (t
            (deslop-llm--handle-events
             session
             (deslop-protocol-sse-finish
              (deslop-llm--session-sse session)))
            (unless (deslop-llm--session-terminal session)
              (let ((reasoning-error
                     (deslop-protocol-reasoning-finish
                      (deslop-llm--session-reasoning session)
                      (deslop-llm--parser-callbacks session))))
                (cond
                 (reasoning-error
                  (deslop-llm--fail session reasoning-error))
                 ((deslop-llm--session-provider-error session)
                  (deslop-llm--fail
                   session (deslop-llm--session-provider-error session)))
                 (t
                  (deslop-llm--settle
                   session 'success (deslop-llm--result session)))))))))
      ((error quit)
       (deslop-llm--fail
        session
        (deslop-error-create :kind 'protocol
                             :message (error-message-string finish-error)))))))

(defun deslop-llm--finish-process-error (session process event)
  "Settle SESSION for failed PROCESS with sentinel EVENT."
  (let ((status (process-exit-status process))
        (stderr-buffer (deslop-llm--session-stderr-buffer session)))
    (deslop-llm--fail
     session
     (deslop-error-create
      :kind (if (= status 28) 'timeout 'transport)
      :message
      (or (and (buffer-live-p stderr-buffer)
                (with-current-buffer stderr-buffer
                  (let ((text (string-trim
                               (substring (buffer-string) 0
                                          (if deslop-max-error-body-bytes
                                              (min (buffer-size)
                                                   deslop-max-error-body-bytes)
                                            (buffer-size))))))
                   (unless (string-empty-p text) text))))
          (format "curl exited with status %d (%s)"
                  status (string-trim event)))))))

(defun deslop-llm--response-preview-text (session)
  "Return SESSION's bounded response preview as a string."
  (let ((parts (deslop-llm--session-response-preview-parts session)))
    (if parts
        (mapconcat #'identity (nreverse parts) "")
      (or (deslop-llm--session-response-preview session) ""))))

(defun deslop-llm--append-response-preview (session chunk)
  "Append a bounded, byte-safe prefix of CHUNK to SESSION's preview."
  (let* ((used (or (deslop-llm--session-response-preview-bytes session) 0))
         (remaining (and deslop-max-error-body-bytes
                         (- deslop-max-error-body-bytes used)))
         (prefix (if remaining
                     (and (> remaining 0)
                           (deslop-transport-byte-prefix chunk remaining))
                   chunk)))
    (when prefix
      (push prefix (deslop-llm--session-response-preview-parts session))
      (setf (deslop-llm--session-response-preview-bytes session)
            (+ used (string-bytes prefix))))))

(defun deslop-llm--process-filter (session _process chunk)
  "Consume one curl output CHUNK for SESSION."
  (unless (deslop-llm--session-terminal session)
    (let* ((received (deslop-llm--session-received-bytes session))
           (chunk-bytes (string-bytes chunk))
           (total (+ received chunk-bytes))
           (remaining (and deslop-max-response-bytes
                           (- deslop-max-response-bytes received)))
           (processable (if (and deslop-max-response-bytes
                                  (> total deslop-max-response-bytes))
                             (deslop-transport-byte-prefix chunk (max 0 remaining))
                          chunk)))
      (setf (deslop-llm--session-received-bytes session) total)
      (deslop-llm--append-response-preview session chunk)
      (condition-case filter-error
          (when (> (length processable) 0)
            (deslop-llm--handle-events
             session
             (deslop-protocol-sse-feed
              (deslop-llm--session-sse session) processable)))
        ((error quit)
          (if (and deslop-max-response-bytes
                   (> total deslop-max-response-bytes))
             (deslop-llm--fail
              session
              (deslop-error-create
               :kind 'resource
               :message "response exceeds deslop-max-response-bytes"))
           (deslop-llm--fail
            session
            (deslop-error-create :kind 'protocol
                                 :message
                                 (error-message-string filter-error))))))
       (when (and deslop-max-response-bytes
                  (> total deslop-max-response-bytes)
                  (not (deslop-llm--session-terminal session)))
        (deslop-llm--fail
          session
          (deslop-error-create
           :kind 'resource
           :message "response exceeds deslop-max-response-bytes"))))))

(defun deslop-llm--process-sentinel (session process event)
  "Handle PROCESS terminal EVENT for SESSION."
  (when (and (not (deslop-llm--session-terminal session))
             (memq (process-status process) '(exit signal closed)))
    (if (= (process-exit-status process) 0)
        (deslop-llm--finish-success session)
      (deslop-llm--finish-process-error session process event))))

(defun deslop-llm--discovery-result (session metadata)
  "Emit discovered METADATA for a live SESSION."
  (when metadata
    (deslop-llm--emit-event
     session (list :type :metadata :metadata metadata))))

(defun deslop-llm--request-body (messages profile)
  "Return the JSON request object for MESSAGES and PROFILE."
  (let ((body `((:model . ,(plist-get profile :model))
                (:messages . ,(mapcar (lambda (message)
                                       `((:role . ,(car message))
                                         (:content . ,(cdr message))))
                                     messages))
                (:temperature . ,(plist-get profile :temperature))
                (:stream . t)
                (:stream_options . ((:include_usage . t))))))
    (if (integerp deslop-max-output-tokens)
        (append body (list (cons :max_tokens deslop-max-output-tokens)))
      body)))

(defun deslop-llm--start-session (session messages)
  "Allocate and start SESSION for MESSAGES."
  (deslop-validate-resource-options)
  (let ((profile (deslop-llm--session-profile session)))
    (setf (deslop-llm--session-json-data session)
          (encode-coding-string
           (json-encode (deslop-llm--request-body messages profile)) 'utf-8))
    (let ((request
           (deslop-curl-start-request
            :profile profile
            :name (format "deslop-curl-%s-%s"
                          (or (plist-get profile :name) "request")
                          (or (plist-get profile :job-id) "unknown"))
            :command (lambda (headers-file config-file)
                       (deslop-curl-command
                        profile headers-file config-file))
            :filter (apply-partially #'deslop-llm--process-filter session)
            :sentinel (apply-partially #'deslop-llm--process-sentinel session))))
     (setf (deslop-llm--session-process session)
            (plist-get request :process)
            (deslop-llm--session-process-buffer session)
            (plist-get request :response-buffer)
             (deslop-llm--session-stderr-buffer session)
             (plist-get request :stderr-buffer)
             (deslop-llm--session-stderr-process session)
             (plist-get request :stderr-process)
             (deslop-llm--session-headers-file session)
             (plist-get request :headers-file)
             (deslop-llm--session-config-file session)
             (plist-get request :config-file))
       (deslop-transport-session-set-resources
        (deslop-llm--lifecycle session) request))
    (deslop-llm--invoke-callback
     session :on-start (apply-partially #'deslop-llm--cancel session))
    (when (and (not (deslop-llm--session-terminal session))
               deslop-provider-discovery-enabled)
       (setf (deslop-llm--session-discovery-cancel session)
             (deslop-provider-discover
              profile (apply-partially #'deslop-llm--discovery-result session))))
    (unless (deslop-llm--session-terminal session)
      (process-send-string (deslop-llm--session-process session)
                           (deslop-llm--session-json-data session))
      (process-send-eof (deslop-llm--session-process session)))))

(defun deslop-llm-default (messages callbacks &optional options)
  "Send MESSAGES asynchronously using a callback-safe curl transport."
  (if (eq (plist-get options :wire-api) 'responses)
      (deslop-codex-default messages callbacks options)
  (let* ((profile (if (plist-get options :name)
                      options
                    (deslop-plist-merge (deslop-provider-profile) options)))
         (session
          (deslop-llm--make-session
            :profile profile :callbacks callbacks :model (plist-get profile :model)
            :sse (deslop-protocol-make-sse)
            :reasoning (deslop-protocol-make-reasoning)
            :estimated-chars 0 :received-bytes 0
            :response-preview-parts nil :response-preview-bytes 0)))
    (condition-case startup-error
        (deslop-llm--start-session session messages)
     ((error quit)
        (deslop-llm--fail
         session
         (deslop-error-create :kind 'startup
                              :message (error-message-string startup-error)))))
     (deslop-llm--session-process session))))

(provide 'deslop-llm)

;;; deslop-llm.el ends here
