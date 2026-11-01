;;; ende-messages.el --- Anthropic Messages adapter -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'ende-content)
(require 'ende-request)
(require 'ende-stream)
(require 'ende-provider-config)

(cl-defstruct (ende-messages--session (:constructor ende-messages--allocate))
  profile stream blocks stopped debug-record)

(defun ende-messages--lifecycle (session)
  "Return SESSION's lifecycle."
  (ende-stream-lifecycle (ende-messages--session-stream session)))

(defun ende-messages--ordered-blocks (session)
  "Return SESSION's blocks in protocol order."
  (let (entries)
    (maphash (lambda (index block) (push (cons index block) entries))
             (ende-messages--session-blocks session))
    (mapcar #'cdr (sort entries (lambda (a b) (< (car a) (car b)))))))

(defun ende-messages--calls (session)
  "Return completed tool calls from SESSION."
  (cl-loop for block in (ende-messages--ordered-blocks session)
           when (and (equal (gethash "type" block) "tool_use")
                     (gethash "ende_complete" block))
           collect (list :id (gethash "id" block) :name (gethash "name" block)
                         :arguments (json-encode (gethash "input" block)))))

(defun ende-messages--history (session)
  "Return SESSION's exact replayable content blocks without local state."
  (mapcar (lambda (block)
            (let ((copy (copy-hash-table block)))
              (remhash "ende_arguments" copy)
              (remhash "ende_complete" copy)
              copy))
          (ende-messages--ordered-blocks session)))

(defun ende-messages--result (session)
  "Return normalized output and protocol history for SESSION."
  (ende-stream-result
   (ende-messages--session-stream session) "output_tokens"
   (plist-get (ende-messages--session-profile session) :model) "Anthropic"
   (list :tool-calls (ende-messages--calls session)
         :protocol-history (cons 'anthropic-messages (ende-messages--history session)))))

(defun ende-messages--settle (session outcome value)
  "Settle SESSION with OUTCOME and VALUE, preserving partial output."
  (let* ((stream (ende-messages--session-stream session))
         (partial (ende-messages--result session)))
    (when (ende-messages--session-debug-record session)
      (ende-debug-record-completion
       (ende-messages--session-debug-record session) outcome
       (ende-stream-session-usage stream)
       (condition-case nil (car (ende-stream-response-headers stream)) (error nil)) value))
    (ende-stream-settle stream outcome value
                        (when (ende-stream-has-result-p stream (plist-get partial :tool-calls))
                          partial))))

(defun ende-messages--fail (session error)
  "Fail SESSION with structured ERROR."
  (ende-messages--settle session 'failure error))

(defun ende-messages--cancel (session)
  "Cancel SESSION."
  (ende-messages--fail session (ende-error-create :kind 'cancelled :message "Process cancelled")))

(defun ende-messages--usage (session usage)
  "Merge cumulative USAGE into SESSION."
  (when (hash-table-p usage)
    (let* ((stream (ende-messages--session-stream session))
           (merged (copy-hash-table (or (ende-stream-session-usage stream)
                                        (make-hash-table :test #'equal)))))
      (maphash (lambda (key value) (puthash key value merged)) usage)
      (ende-stream-record-usage stream merged "output_tokens"))))

(defun ende-messages--event (session json)
  "Consume one Messages event JSON for SESSION."
  (let* ((stream (ende-messages--session-stream session))
         (index (gethash "index" json))
         (blocks (ende-messages--session-blocks session))
         (block (gethash index blocks))
         (delta (gethash "delta" json)))
    (pcase (gethash "type" json)
      ("message_start"
       (let ((message (gethash "message" json)))
         (setf (ende-stream-session-actual-model stream) (gethash "model" message))
         (ende-messages--usage session (gethash "usage" message))))
      ("content_block_start"
       (when (or (not (integerp index)) (gethash index blocks))
         (error "Invalid or duplicate Messages content index"))
       (puthash index (copy-hash-table (gethash "content_block" json)) blocks))
      ("content_block_delta"
       (unless block (error "Messages delta has no content block"))
       (pcase (gethash "type" delta)
         ("text_delta"
          (let ((text (gethash "text" delta)))
            (puthash "text" (concat (gethash "text" block "") text) block)
            (ende-stream-record-delta stream :content text
                                      (apply-partially #'ende-messages--fail session))))
         ("thinking_delta"
          (let ((text (gethash "thinking" delta)))
            (puthash "thinking" (concat (gethash "thinking" block "") text) block)
            (ende-stream-record-delta stream :thinking text
                                      (apply-partially #'ende-messages--fail session))))
         ("signature_delta"
          (puthash "signature" (concat (gethash "signature" block "")
                                       (gethash "signature" delta)) block))
         ("input_json_delta"
          (puthash "ende_arguments" (concat (gethash "ende_arguments" block "")
                                            (gethash "partial_json" delta)) block))))
      ("content_block_stop"
       (unless block (error "Messages stop has no content block"))
       (when (equal (gethash "type" block) "tool_use")
         (let* ((text (gethash "ende_arguments" block))
                (input (if text (ende-protocol-json-read-string text) (gethash "input" block))))
           (unless (and (hash-table-p input) (stringp (gethash "id" block))
                        (stringp (gethash "name" block)))
             (error "Incomplete Messages tool call"))
           (puthash "input" input block)))
       (puthash "ende_complete" t block))
      ("message_delta"
       (setf (ende-stream-session-finish-reason stream) (gethash "stop_reason" delta))
       (ende-messages--usage session (gethash "usage" json)))
      ("message_stop" (setf (ende-messages--session-stopped session) t))
      ("error"
       (ende-messages--fail
        session (ende-error-create :kind 'provider
                                   :message (ende-transport-safe-error-text
                                             (gethash "message" (gethash "error" json)))))))))

(defun ende-messages--events (session events)
  "Consume SSE EVENTS while SESSION is active."
  (dolist (event events)
    (unless (ende-stream-terminal-p (ende-messages--session-stream session))
      (when (eq (plist-get event :type) :json)
        (ende-messages--event session (plist-get event :value))))))

(defun ende-messages--parse-error (session error &optional _over-limit)
  "Fail SESSION for parser ERROR."
  (ende-messages--fail session (ende-error-create :kind 'protocol :message (error-message-string error))))

(defun ende-messages--filter (session _process chunk)
  "Consume CHUNK for SESSION."
  (ende-stream-filter
   (ende-messages--session-stream session) chunk
   (apply-partially #'ende-messages--events session)
   (apply-partially #'ende-messages--fail session)
   (apply-partially #'ende-messages--parse-error session)))

(defun ende-messages--finish (session headers)
  "Validate HEADERS and terminal state for SESSION."
  (let* ((stream (ende-messages--session-stream session))
         (type-error (ende-stream-content-type-error (cdr headers) 'if-present)))
    (cond
     ((not (ende-stream-http-success-p (car headers)))
      (ende-messages--fail session (ende-error-create :kind 'http
                                                      :message (format "Messages HTTP status %s" (car headers)))))
     (type-error (ende-messages--fail session type-error))
     (t
      (ende-messages--events session (ende-protocol-sse-flush (ende-stream-session-parser stream)))
      (unless (ende-stream-terminal-p stream)
        (if (and (ende-messages--session-stopped session)
                 (member (ende-stream-session-finish-reason stream) '("end_turn" "tool_use" "stop_sequence"))
                 (cl-every (lambda (block) (gethash "ende_complete" block))
                           (ende-messages--ordered-blocks session)))
            (ende-messages--settle session 'success (ende-messages--result session))
          (ende-messages--fail session (ende-error-create :kind 'incomplete
                                                          :message "Messages generation is incomplete"))))))))

(defun ende-messages--process-error (session process event idle-error)
  "Fail SESSION for PROCESS terminal EVENT or IDLE-ERROR."
  (ende-messages--fail session (or idle-error (ende-error-create
                                              :kind (if (= (process-exit-status process) 28) 'timeout 'transport)
                                              :message (string-trim event)))))

(defun ende-messages--sentinel (session process event)
  "Handle terminal PROCESS EVENT for SESSION."
  (ende-stream-sentinel
   (ende-messages--session-stream session) process event
   (apply-partially #'ende-messages--process-error session)
   (apply-partially #'ende-messages--finish session)
   (apply-partially #'ende-messages--parse-error session)))

(defun ende-messages--content (content)
  "Encode internal CONTENT into Messages blocks."
  (if (stringp content)
      (unless (string-empty-p content) (list (list (cons :type "text") (cons :text content))))
    (mapcar (lambda (part)
              (pcase (plist-get part :type)
                (:text (list (cons :type "text") (cons :text (plist-get part :text))))
                (:image
                 (let ((data (ende-content-image-data part)))
                   (list (cons :type "image")
                         (cons :source (list (cons :type "base64") (cons :media_type (car data))
                                             (cons :data (base64-encode-string (cdr data) t)))))))
                (_ (error "Unsupported Messages content")))) content)))

(defun ende-messages--request-body (messages profile)
  "Encode MESSAGES and PROFILE for Messages."
  (let (system encoded)
    (unless (plist-get profile :output-tokens)
      (error "Messages requires :output-tokens; configure a requested output budget"))
    (dolist (message (ende-content-provider-messages messages))
      (let* ((role (ende-content-role message))
             (content (ende-messages--content (ende-content-body message)))
             (history (plist-get (unless (stringp (car message)) message) :protocol-history))
             (calls (plist-get (unless (stringp (car message)) message) :tool-calls))
             (id (plist-get (unless (stringp (car message)) message) :tool-call-id)))
        (if (equal role "system")
            (setq system (append system content))
          (when (and history (eq (car history) 'anthropic-messages))
            (setq content (cdr history) calls nil))
          (dolist (call calls)
            (let ((input (ende-protocol-json-read-string (plist-get call :arguments))))
              (unless (hash-table-p input) (error "Messages tool arguments must be an object"))
              (setq content (append content (list (list (cons :type "tool_use")
                                                        (cons :id (plist-get call :id))
                                                        (cons :name (plist-get call :name)) (cons :input input)))))))
          (when id
            (setq role "user" content (list (list (cons :type "tool_result")
                                                  (cons :tool_use_id id)
                                                  (cons :content (ende-content-body message))))))
          (if (and encoded (equal role (alist-get :role (car encoded))))
              (setcdr (assq :content (car encoded))
                      (append (alist-get :content (car encoded)) content))
            (push (list (cons :role role) (cons :content content)) encoded)))))
    (ende-provider-config-encode-parameters
     (ende-controls-encode
      (append (list (cons :model (plist-get profile :model)) (cons :messages (nreverse encoded))
                    (cons :max_tokens (plist-get profile :output-tokens)) (cons :stream t))
              (when system (list (cons :system system)))
              (when (plist-get profile :tool-definitions)
                (list (cons :tools
                            (mapcar (lambda (definition)
                                      (let ((function (alist-get :function definition)))
                                        (list (cons :name (alist-get :name function))
                                              (cons :description (alist-get :description function))
                                              (cons :input_schema (alist-get :parameters function)))))
                                    (plist-get profile :tool-definitions))))))
      (plist-get profile :controls) profile) profile)))

(defun ende-messages--start (session data _body record)
  "Start SESSION with prepared DATA and debug RECORD."
  (let* ((profile (ende-messages--session-profile session))
         (request (ende-curl-start-request
                   :profile profile :name "ende-messages"
                   :internal-headers '("anthropic-version: 2023-06-01")
                   :command (lambda (headers config)
                              (ende-curl-command profile headers config
                                                 (and (listp data) (plist-get data :file))))
                   :filter (apply-partially #'ende-messages--filter session)
                   :sentinel (apply-partially #'ende-messages--sentinel session))))
    (ende-transport-session-set-resources (ende-messages--lifecycle session) request)
    (setf (ende-messages--session-debug-record session) record)
    (ende-request-send (plist-get request :process) data)))

(defun ende-messages-default (messages callbacks profile)
  "Send MESSAGES with CALLBACKS using resolved PROFILE."
  (let* ((session (ende-messages--allocate :profile profile :blocks (make-hash-table :test #'eql)))
         (lifecycle (ende-transport-session-create callbacks (apply-partially #'ende-messages--fail session))))
    (setf (ende-messages--session-stream session) (ende-stream-create lifecycle (ende-protocol-make-sse)))
    (condition-case failure
        (progn
          (ende-transport-session-invoke lifecycle :on-start (apply-partially #'ende-messages--cancel session))
          (unless (ende-stream-terminal-p (ende-messages--session-stream session))
            (ende-request-prepare lifecycle 'ende-messages #'ende-messages--request-body messages profile
                                  (list "anthropic-messages" (plist-get profile :url))
                                  (apply-partially #'ende-messages--start session))))
      ((error quit)
       (ende-messages--fail session (ende-error-create :kind 'startup :message (error-message-string failure)))))
    (ende-transport-session-process lifecycle)))

(provide 'ende-messages)
;;; ende-messages.el ends here
