;;; ende-openrouter-batch.el --- OpenRouter Batch transport for ende -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools, llm

;;; Commentary:

;; Asynchronous OpenRouter Batch API transport.  A batch group collects compatible
;; logical requests, submits them together, polls the remote batch, and routes
;; each completed result by its custom id.  The transport deliberately does not
;; use the streaming Chat Completions endpoint.

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
(require 'ende-provider)

;;;; User options

(defcustom ende-openrouter-batch-window 0.05
  "Seconds to collect compatible requests before submitting a batch."
  :type 'number
  :group 'ende)

(defcustom ende-openrouter-batch-max-requests 16
  "Maximum number of logical requests submitted in one batch."
  :type 'integer
  :group 'ende)

(defcustom ende-openrouter-batch-max-request-bytes (* 16 1024 1024)
  "Maximum encoded request body size for one OpenRouter batch."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Maximum bytes"))
  :group 'ende)

(defcustom ende-openrouter-batch-poll-initial-delay 2.0
  "Seconds to wait before the first Batch API poll."
  :type 'number
  :group 'ende)

(defcustom ende-openrouter-batch-poll-not-found-max-age (* 5 60)
  "Maximum seconds to retry an initially invisible batch."
  :type 'number
  :group 'ende)

(defcustom ende-openrouter-batch-poll-max-delay 60.0
  "Maximum delay between Batch API polls."
  :type 'number
  :group 'ende)

(defcustom ende-openrouter-batch-max-age (* 24 60 60)
  "Maximum age of a submitted batch before local polling gives up."
  :type 'number
  :group 'ende)

(defcustom ende-openrouter-batch-poll-jitter 0.15
  "Fractional random jitter applied to Batch API poll delays."
  :type 'number
  :group 'ende)

;;;; State

(cl-defstruct (ende-openrouter-batch--request
               (:constructor ende-openrouter-batch--make-request))
  "One logical ende request waiting for or belonging to a batch."
  custom-id messages callbacks profile cancelled settled)

(cl-defstruct (ende-openrouter-batch--group
               (:constructor ende-openrouter-batch--make-group))
  "A group of compatible logical requests submitted as one remote batch."
  key profile requests timer stage body batch-id status poll-attempt
  not-found-attempt batch-visible-p poll-timer submitted-at deadline terminal)

(defvar ende-openrouter-batch--groups (make-hash-table :test #'equal)
  "Pending OpenRouter Batch groups keyed by their request shape.")

(defvar ende-openrouter-batch--known-groups nil
  "Batch groups with pending local timers or remote work.")

(defvar ende-openrouter-batch--next-request-id 0
  "Next local identifier used for Batch API custom ids.")

(defun ende-openrouter-batch--register-group (group)
  "Track GROUP until its local lifecycle is terminal."
  (cl-pushnew group ende-openrouter-batch--known-groups :test #'eq)
  group)

(defun ende-openrouter-batch--unregister-group (group)
  "Stop tracking terminal GROUP."
  (setq ende-openrouter-batch--known-groups
        (delq group ende-openrouter-batch--known-groups))
  group)

(defun ende-openrouter-batch--profile-value (profile key default)
  "Return PROFILE's KEY, or DEFAULT when KEY is absent."
  (if (plist-member profile key)
      (plist-get profile key)
    default))

(defun ende-openrouter-batch--validate-options (profile)
  "Validate Batch options in PROFILE and global defaults."
  (dolist (option
           (list (cons 'ende-openrouter-batch-window
                       (ende-openrouter-batch--profile-value
                        profile :batch-window
                        ende-openrouter-batch-window))
                 (cons 'ende-openrouter-batch-poll-initial-delay
                       (ende-openrouter-batch--profile-value
                        profile :batch-poll-initial-delay
                        ende-openrouter-batch-poll-initial-delay))
                 (cons 'ende-openrouter-batch-poll-not-found-max-age
                       (ende-openrouter-batch--profile-value
                        profile :batch-poll-not-found-max-age
                        ende-openrouter-batch-poll-not-found-max-age))
                 (cons 'ende-openrouter-batch-poll-max-delay
                       (ende-openrouter-batch--profile-value
                        profile :batch-poll-max-delay
                        ende-openrouter-batch-poll-max-delay))
                 (cons 'ende-openrouter-batch-max-age
                       (ende-openrouter-batch--profile-value
                        profile :batch-max-age
                        ende-openrouter-batch-max-age))
                 (cons 'ende-openrouter-batch-poll-jitter
                       (ende-openrouter-batch--profile-value
                        profile :batch-poll-jitter
                        ende-openrouter-batch-poll-jitter))))
    (unless (and (numberp (cdr option)) (>= (cdr option) 0)
                 (or (not (eq (car option)
                              'ende-openrouter-batch-poll-jitter))
                     (<= (cdr option) 1)))
      (error "%s must be a nonnegative number" (car option))))
  (let ((max-requests
         (ende-openrouter-batch--profile-value
          profile :batch-max-requests ende-openrouter-batch-max-requests))
        (max-bytes
         (ende-openrouter-batch--profile-value
          profile :batch-max-request-bytes
          ende-openrouter-batch-max-request-bytes)))
    (unless (and (integerp max-requests) (> max-requests 0))
      (error "Batch max requests must be a positive integer"))
    (unless (or (null max-bytes) (and (integerp max-bytes) (> max-bytes 0)))
      (error "Batch max request bytes must be nil or a positive integer")))
  (let* ((url (plist-get profile :batch-url))
         (parsed (and (stringp url) (url-generic-parse-url url)))
         (path (and parsed (url-filename parsed))))
    (unless (and (stringp url)
                 (not (string-empty-p url))
                 (string= (downcase (or (and parsed (url-type parsed)) ""))
                          "https")
                 (string-match-p "\\`/api/v1/batches/?\\'" (or path "")))
      (error "Batch profile must use an HTTPS /api/v1/batches endpoint"))
    (ende-curl--validated-url profile url))
  (unless (and (stringp (plist-get profile :batch-endpoint))
               (string-prefix-p "/v" (plist-get profile :batch-endpoint)))
    (error "Batch profile has an invalid :batch-endpoint"))
  t)

(defun ende-openrouter-batch--profile-key (profile)
  "Return a stable key for compatible request PROFILE values."
  (secure-hash
   'sha256
   (prin1-to-string
    (list (plist-get profile :name)
          (plist-get profile :batch-url)
          (plist-get profile :batch-endpoint)
          (plist-get profile :auth-source-host)
          (plist-get profile :auth-source-user)
          (plist-get profile :auth-source-port)
          (plist-get profile :model)
          (plist-get profile :max-tokens)
          (ende-controls-encode nil (plist-get profile :controls) profile)
          ende-default-output-tokens
          (plist-get profile :request-headers)))))

(defun ende-openrouter-batch--active-requests (group)
  "Return unsettled requests in GROUP."
  (cl-remove-if
   (lambda (request)
     (or (ende-openrouter-batch--request-cancelled request)
         (ende-openrouter-batch--request-settled request)))
   (ende-openrouter-batch--group-requests group)))

(defun ende-openrouter-batch--invoke (request name &rest arguments)
  "Invoke callback NAME for REQUEST without leaking callback failures."
  (let ((callback (plist-get (ende-openrouter-batch--request-callbacks request)
                             name)))
    (when callback
      (condition-case callback-error
          (apply callback arguments)
        ((error quit)
         (unless (eq name :on-error)
           (let ((error-callback
                  (plist-get
                   (ende-openrouter-batch--request-callbacks request)
                   :on-error)))
             (when error-callback
               (ende-ignore-errors
                error-callback
                (ende-error-create
                 :kind 'callback
                 :message (error-message-string callback-error)))))))))))

(defun ende-openrouter-batch--emit (request event)
  "Emit EVENT for REQUEST while it remains live."
  (unless (or (ende-openrouter-batch--request-cancelled request)
              (ende-openrouter-batch--request-settled request))
    (ende-openrouter-batch--invoke request :on-event event)))

(defun ende-openrouter-batch--diagnostic (request text)
  "Emit diagnostic TEXT for REQUEST when tool diagnostics are enabled."
  (when ende-tool-diagnostics
    (ende-openrouter-batch--emit
     request (list :type :diagnostic :text text))))

(defun ende-openrouter-batch--diagnose-request (request)
  "Record the tools present in REQUEST's Batch API body."
  (ende-openrouter-batch--diagnostic
   request "OpenRouter Batch request tools: (none)"))

(defun ende-openrouter-batch--diagnostic-output-items (body)
  "Return a safe summary of completion output item types in BODY."
  (let* ((choice (ende-openrouter-batch--choice body))
         (message (ende-protocol-json-get "message" choice))
         (calls (ende-protocol-json-get "tool_calls" message))
         (calls (cond
                 ((and (vectorp calls) (not (stringp calls)))
                  (append calls nil))
                 ((listp calls) calls)))
         (items (when (stringp (ende-protocol-json-get "content" message))
                  (list "message"))))
    (dolist (call calls)
      (let* ((function (ende-protocol-json-get "function" call))
             (name (ende-protocol-json-get "name" function)))
        (push (if name (format "tool_call(%s)" name) "tool_call") items)))
    (if items
        (mapconcat #'identity (nreverse items) ", ")
      "(none)")))

(defun ende-openrouter-batch--diagnose-result (request body)
  "Record the output item types in Batch completion BODY for REQUEST."
  (ende-openrouter-batch--diagnostic
   request
   (format "OpenRouter Batch output items: %s"
           (ende-openrouter-batch--diagnostic-output-items body))))

(defun ende-openrouter-batch--status (group status)
  "Emit STATUS to all live requests in GROUP."
  (setf (ende-openrouter-batch--group-status group) status)
  (dolist (request (ende-openrouter-batch--active-requests group))
    (ende-openrouter-batch--emit
     request (list :type :status :status status))))

(defun ende-openrouter-batch--settle-request
    (request outcome &optional result error)
  "Settle REQUEST with OUTCOME, RESULT, and ERROR exactly once."
  (unless (or (ende-openrouter-batch--request-cancelled request)
              (ende-openrouter-batch--request-settled request))
    (setf (ende-openrouter-batch--request-settled request) outcome)
    (if (eq outcome 'success)
        (ende-openrouter-batch--invoke request :on-done result)
      (ende-openrouter-batch--invoke
       request :on-event
       (list :type :error :error error :result result))
      (ende-openrouter-batch--invoke request :on-error error))))

(defun ende-openrouter-batch--cancel-timer (timer)
  "Cancel TIMER when it is a live timer."
  (when (timerp timer)
    (ende-ignore-errors #'cancel-timer timer)))

(defun ende-openrouter-batch--cleanup-stage (stage)
  "Release resources owned by STAGE."
  (when stage
    (let ((request (plist-get stage :request)))
      (ende-curl-cleanup-resources
       (list (plist-get request :response-buffer)
             (plist-get request :stderr-buffer))
       (list (plist-get request :headers-file)
             (plist-get request :config-file))
       (list (plist-get request :process)
             (plist-get request :stderr-process))))))

(defun ende-openrouter-batch--group-cleanup (group)
  "Cancel timers and release the active HTTP stage for GROUP."
  (ende-openrouter-batch--cancel-timer
   (ende-openrouter-batch--group-timer group))
  (ende-openrouter-batch--cancel-timer
   (ende-openrouter-batch--group-poll-timer group))
  (setf (ende-openrouter-batch--group-timer group) nil
        (ende-openrouter-batch--group-poll-timer group) nil)
  (ende-openrouter-batch--cleanup-stage
   (ende-openrouter-batch--group-stage group))
  (setf (ende-openrouter-batch--group-stage group) nil))

(defun ende-openrouter-batch--group-fail (group error)
  "Fail all live requests in GROUP with structured ERROR."
  (unless (ende-openrouter-batch--group-terminal group)
    (setf (ende-openrouter-batch--group-terminal group) t)
    (ende-openrouter-batch--group-cleanup group)
    (ende-openrouter-batch--unregister-group group)
    (dolist (request (ende-openrouter-batch--active-requests group))
      (ende-openrouter-batch--settle-request request 'failure nil error))))

(defun ende-openrouter-batch--response-error
    (status body &optional stage-error)
  "Build an error for HTTP STATUS, BODY, or STAGE-ERROR."
  (or stage-error
      (let* ((detail (and (stringp body)
                          (ende-transport-error-detail body)))
             (message (or detail
                          (and (stringp body) (not (string-empty-p body)) body)
                          (format "HTTP status %s" status))))
        (ende-error-create
         :kind (if (and status (>= status 400)) 'http 'transport)
         :message (if status
                      (format "HTTP status %s: %s" status message)
                    message)
         :details body))))

(defun ende-openrouter-batch--request-body (messages profile custom-id)
  "Build one OpenRouter Batch request for MESSAGES and PROFILE."
  (let ((body
         `((:messages . ,(mapcar
                          (lambda (message)
                            (when (plist-get message :tool-calls)
                              (error "Batch image context cannot contain tool calls"))
                            (let ((role (ende-content-role message)))
                              (list (cons :role role)
                                    (cons :content
                                          (ende-content-provider-content
                                           role (ende-content-body message) 'chat profile)))))
                          (ende-content-provider-messages messages))))))
    (setq body (ende-controls-encode body (plist-get profile :controls) profile))
    (when (integerp ende-default-output-tokens)
      (setq body (append body (list (cons :max_tokens
                                          ende-default-output-tokens)))))
    (list (cons :custom_id custom-id)
          (cons :body body))))

(defun ende-openrouter-batch--body (group)
  "Build the ordered JSON object submitted for GROUP."
  (let* ((profile (ende-openrouter-batch--group-profile group))
         (requests
          (mapcar
           (lambda (request)
             (ende-openrouter-batch--request-body
              (ende-openrouter-batch--request-messages request)
              profile
              (ende-openrouter-batch--request-custom-id request)))
           (ende-openrouter-batch--active-requests group)))
         (body `((:endpoint . ,(plist-get profile :batch-endpoint))
                 (:model . ,(plist-get profile :model))
                 (:requests . ,requests)))
         (encoded (encode-coding-string (json-encode body) 'utf-8))
         (limit (ende-openrouter-batch--profile-value
                 profile :batch-max-request-bytes
                 ende-openrouter-batch-max-request-bytes)))
    (when (and limit (> (string-bytes encoded) limit))
      (error "Batch request exceeds ende-openrouter-batch-max-request-bytes"))
    encoded))

(defun ende-openrouter-batch--append-response (process chunk)
  "Append CHUNK to PROCESS's bounded JSON response buffer."
  (let* ((buffer (process-get process :ende-response-buffer))
         (used (or (process-get process :ende-batch-response-bytes) 0))
         (total (+ used (string-bytes chunk)))
         (remaining (and ende-max-response-bytes
                         (- ende-max-response-bytes used))))
    (if (and ende-max-response-bytes
             (> total ende-max-response-bytes))
        (progn
          (process-put
           process :ende-batch-response-error
           (ende-error-create
            :kind 'resource
            :message "Batch API response exceeds ende-max-response-bytes"))
          (when (process-live-p process)
            (delete-process process)))
      (when (and buffer (> (length chunk) 0))
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert (if remaining
                      (ende-transport-byte-prefix chunk remaining)
                    chunk))))
      (process-put process :ende-batch-response-bytes total))))

(defun ende-openrouter-batch--stage-body (stage)
  "Return the accumulated response body for STAGE."
  (let ((buffer (plist-get (plist-get stage :request) :response-buffer)))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer (buffer-string))
      "")))

(defun ende-openrouter-batch--stage-sentinel
    (group stage process event)
  "Finish one HTTP STAGE for GROUP."
  (when (and (not (ende-openrouter-batch--group-terminal group))
             (eq stage (ende-openrouter-batch--group-stage group))
             (memq (process-status process) '(exit signal closed failed)))
    (setf (ende-openrouter-batch--group-stage group) nil)
    (let* ((request (plist-get stage :request))
           (status (condition-case nil
                       (car (ende-curl-response-headers
                             (plist-get request :headers-file)))
                     (error nil)))
           (body (ende-openrouter-batch--stage-body stage))
           (stage-error
            (or (process-get process :ende-batch-response-error)
                (process-get process :ende-idle-error)
                (unless (= (process-exit-status process) 0)
                  (ende-error-create
                   :kind 'transport
                   :message
                   (format "curl exited with status %d (%s)"
                           (process-exit-status process)
                           (string-trim event)))))))
      (ende-openrouter-batch--cleanup-stage stage)
      (funcall (plist-get stage :callback) status body stage-error))))

(cl-defun ende-openrouter-batch--start-stage
    (group method url body callback)
  "Start one JSON HTTP STAGE for GROUP.
CALLBACK receives STATUS, BODY, and a structured transport error or nil."
  (when (ende-openrouter-batch--group-stage group)
    (error "Batch group already has an active HTTP stage"))
  (let* ((stage nil)
         (request
          (ende-curl-start-request
           :profile (ende-openrouter-batch--group-profile group)
           :url url
           :name (format "ende-openrouter-batch-%s"
                         (or (plist-get
                              (ende-openrouter-batch--group-profile group)
                              :job-id)
                             "request"))
           :command
           (lambda (headers-file config-file)
             (ende-curl-json-command
              (ende-openrouter-batch--group-profile group)
              headers-file config-file method "application/json"
              (not (null body)) url))
           :filter #'ende-openrouter-batch--append-response
           :sentinel
           (lambda (process event)
             (ende-openrouter-batch--stage-sentinel
              group stage process event)))))
    (setq stage (list :request request :callback callback))
    (setf (ende-openrouter-batch--group-stage group) stage)
    (when (and body
               (not (ende-openrouter-batch--group-terminal group)))
      (process-send-string (plist-get request :process) body))
    (unless (ende-openrouter-batch--group-terminal group)
      (process-send-eof (plist-get request :process)))
    request))

(defun ende-openrouter-batch--batch-url (group)
  "Return the status URL for GROUP's remote batch."
  (concat (string-remove-suffix "/"
                                (plist-get
                                 (ende-openrouter-batch--group-profile group)
                                 :batch-url))
          "/"
          (url-hexify-string
           (ende-openrouter-batch--group-batch-id group))))

(defun ende-openrouter-batch--retryable-status-p (status)
  "Return non-nil when HTTP STATUS is safe to retry while polling."
  (memq status '(408 425 429 500 502 503 504 524 529)))

(defun ende-openrouter-batch--jittered-delay (group &optional attempt)
  "Return the poll delay for GROUP and retry ATTEMPT."
  (let* ((profile (ende-openrouter-batch--group-profile group))
         (initial (ende-openrouter-batch--profile-value
                   profile :batch-poll-initial-delay
                   ende-openrouter-batch-poll-initial-delay))
         (maximum (ende-openrouter-batch--profile-value
                   profile :batch-poll-max-delay
                   ende-openrouter-batch-poll-max-delay))
         (attempt (or attempt
                      (ende-openrouter-batch--group-poll-attempt group)))
         (base (min maximum (* initial (expt 2 attempt))))
         (jitter (ende-openrouter-batch--profile-value
                  profile :batch-poll-jitter
                  ende-openrouter-batch-poll-jitter)))
    (if (zerop jitter)
        base
      (* base (+ 1.0 (* jitter (- (random 2001) 1000) 0.001))))))

(defun ende-openrouter-batch--initial-not-found-retry-p (group)
  "Return non-nil while GROUP may still be propagating remotely."
  (let* ((profile (ende-openrouter-batch--group-profile group))
         (max-age (ende-openrouter-batch--profile-value
                   profile :batch-poll-not-found-max-age
                   ende-openrouter-batch-poll-not-found-max-age))
         (submitted-at (ende-openrouter-batch--group-submitted-at group)))
    (and (not (ende-openrouter-batch--group-batch-visible-p group))
         (numberp submitted-at)
         (< (- (float-time) submitted-at) max-age))))

(defun ende-openrouter-batch--not-found-error (group body)
  "Build a terminal not-found error for GROUP and response BODY."
  (let* ((batch-id (ende-openrouter-batch--group-batch-id group))
         (url (ende-openrouter-batch--batch-url group))
         (visibility
          (if (ende-openrouter-batch--group-batch-visible-p group)
              "after it was previously visible"
            "without ever being observed; the initial retry window expired")))
    (ende-error-create
     :kind 'http
     :message (format "OpenRouter batch %s was not found at %s; %s"
                      batch-id url visibility)
     :details body)))

(defun ende-openrouter-batch--retry-not-found (group body)
  "Retry an initially invisible GROUP, or fail it with BODY."
  (if (ende-openrouter-batch--initial-not-found-retry-p group)
      (let* ((profile (ende-openrouter-batch--group-profile group))
             (max-age (ende-openrouter-batch--profile-value
                       profile :batch-poll-not-found-max-age
                       ende-openrouter-batch-poll-not-found-max-age))
             (submitted-at (ende-openrouter-batch--group-submitted-at group))
             (remaining (- max-age (- (float-time) submitted-at)))
             (delay
              (ende-openrouter-batch--jittered-delay
               group
               (ende-openrouter-batch--group-not-found-attempt group))))
        (if (< delay remaining)
            (progn
              (cl-incf
               (ende-openrouter-batch--group-not-found-attempt group))
              (ende-openrouter-batch--schedule-poll group delay))
          (ende-openrouter-batch--group-fail
           group (ende-openrouter-batch--not-found-error group body))))
    (ende-openrouter-batch--group-fail
     group (ende-openrouter-batch--not-found-error group body))))

(defun ende-openrouter-batch--schedule-poll (group &optional delay)
  "Schedule a status poll for GROUP after DELAY seconds."
  (ende-openrouter-batch--cancel-timer
   (ende-openrouter-batch--group-poll-timer group))
  (let* ((profile (ende-openrouter-batch--group-profile group))
         (max-age (ende-openrouter-batch--profile-value
                   profile :batch-max-age ende-openrouter-batch-max-age))
         (deadline (ende-openrouter-batch--group-deadline group))
         (now (float-time)))
    (unless (ende-openrouter-batch--group-terminal group)
      (if (or (>= now deadline)
              (> (- now (ende-openrouter-batch--group-submitted-at group))
                 max-age))
          (ende-openrouter-batch--group-fail
           group (ende-error-create
                  :kind 'timeout :message "OpenRouter batch exceeded its deadline"))
        (setf (ende-openrouter-batch--group-poll-timer group)
              (run-at-time (or delay (ende-openrouter-batch--jittered-delay group))
                           nil #'ende-openrouter-batch--poll group))))))

(defun ende-openrouter-batch--terminal-status-p (status)
  "Return non-nil when remote STATUS is terminal."
  (member status '("completed" "failed" "expired" "cancelled")))

(defun ende-openrouter-batch--status-label (status)
  "Return a concise UI status for remote STATUS."
  (pcase status
    ("validating" "Batch validating")
    ("in_progress" "Batch processing")
    ("finalizing" "Batch finalizing")
    ("cancelling" "Batch cancelling")
    ("cancelled" "Batch cancelled")
    ("completed" "Retrieving batch results")
    (_ "Batch processing")))

(defun ende-openrouter-batch--parse-object (body)
  "Parse nonempty JSON BODY or return a protocol error."
  (condition-case caught
      (if (and (stringp body) (not (string-empty-p (string-trim body))))
          (ende-protocol-json-read-string body)
        (ende-error-create :kind 'protocol :message "Batch API returned an empty body"))
    ((error quit)
     (ende-error-create
      :kind 'protocol
      :message (format "Malformed Batch API JSON: %s"
                       (error-message-string caught))))))

(defun ende-openrouter-batch--json-error-p (value)
  "Return non-nil when VALUE is a structured parsing error."
  (ende-error-p value))

(defun ende-openrouter-batch--submit-finished
    (group status body stage-error)
  "Handle the response to GROUP's batch submission."
  (cond
   (stage-error
    (ende-openrouter-batch--group-fail
     group (ende-error-create
            :kind 'submission-unknown
            :message "OpenRouter batch submission outcome is unknown"
            :details stage-error)))
   ((not (= status 202))
    (ende-openrouter-batch--group-fail
     group (ende-openrouter-batch--response-error status body)))
   (t
    (let* ((payload (ende-openrouter-batch--parse-object body))
           (batch-id (and (not (ende-openrouter-batch--json-error-p payload))
                          (ende-protocol-json-get "id" payload)))
           (status-name (and (not (ende-openrouter-batch--json-error-p payload))
                             (ende-protocol-json-get "status" payload))))
      (cond
       ((ende-openrouter-batch--json-error-p payload)
        (ende-openrouter-batch--group-fail group payload))
       ((or (not (stringp batch-id)) (string-empty-p batch-id))
        (ende-openrouter-batch--group-fail
         group (ende-error-create
                :kind 'protocol :message "Batch submission returned no id"
                :details payload)))
       ((not (stringp status-name))
        (ende-openrouter-batch--group-fail
         group (ende-error-create
                :kind 'protocol :message "Batch submission returned no status"
                :details payload)))
       (t
        (setf (ende-openrouter-batch--group-batch-id group) batch-id
              (ende-openrouter-batch--group-status group) status-name
              (ende-openrouter-batch--group-submitted-at group) (float-time)
              (ende-openrouter-batch--group-deadline group)
              (+ (float-time)
                 (ende-openrouter-batch--profile-value
                  (ende-openrouter-batch--group-profile group)
                  :batch-max-age ende-openrouter-batch-max-age)))
        (ende-openrouter-batch--status
         group (ende-openrouter-batch--status-label status-name))
        (ende-openrouter-batch--schedule-poll
         group
         (ende-openrouter-batch--profile-value
          (ende-openrouter-batch--group-profile group)
          :batch-poll-initial-delay
          ende-openrouter-batch-poll-initial-delay))))))))

(defun ende-openrouter-batch--submit (group)
  "Submit GROUP to OpenRouter."
  (unless (ende-openrouter-batch--group-terminal group)
    (condition-case caught
        (progn
          (ende-openrouter-batch--status group "Submitting batch")
          (setf (ende-openrouter-batch--group-body group)
                (ende-openrouter-batch--body group))
          (dolist (request (ende-openrouter-batch--active-requests group))
            (ende-openrouter-batch--diagnose-request request))
          (ende-openrouter-batch--start-stage
           group "POST"
           (plist-get (ende-openrouter-batch--group-profile group)
                      :batch-url)
           (ende-openrouter-batch--group-body group)
           (apply-partially #'ende-openrouter-batch--submit-finished group)))
      ((error quit)
       (ende-openrouter-batch--group-fail
        group (ende-error-create
               :kind 'startup :message (error-message-string caught)))))))

(defun ende-openrouter-batch--flush (group)
  "Submit pending requests in GROUP."
  (when (and (not (ende-openrouter-batch--group-terminal group))
             (gethash (ende-openrouter-batch--group-key group)
                      ende-openrouter-batch--groups))
    (remhash (ende-openrouter-batch--group-key group)
             ende-openrouter-batch--groups)
    (setf (ende-openrouter-batch--group-timer group) nil)
    (if (ende-openrouter-batch--active-requests group)
        (ende-openrouter-batch--submit group)
      (ende-openrouter-batch--group-cleanup group)
      (ende-openrouter-batch--unregister-group group))))

(defun ende-openrouter-batch--poll-finished
    (group status body stage-error)
  "Handle one status response for GROUP."
  (cond
   (stage-error
    (if (and (ende-error-p stage-error)
             (memq (ende-error-kind stage-error) '(timeout transport))
             (< (ende-openrouter-batch--group-poll-attempt group) 8))
        (progn
          (cl-incf (ende-openrouter-batch--group-poll-attempt group))
          (ende-openrouter-batch--schedule-poll group))
      (ende-openrouter-batch--group-fail group stage-error)))
   ((not (= status 200))
    (cond
     ((= status 404)
      (ende-openrouter-batch--retry-not-found group body))
     ((ende-openrouter-batch--retryable-status-p status)
      (cl-incf (ende-openrouter-batch--group-poll-attempt group))
      (ende-openrouter-batch--schedule-poll group))
     (t
      (ende-openrouter-batch--group-fail
       group (ende-openrouter-batch--response-error status body)))))
   (t
    (setf (ende-openrouter-batch--group-batch-visible-p group) t
          (ende-openrouter-batch--group-not-found-attempt group) 0)
    (let* ((payload (ende-openrouter-batch--parse-object body))
           (remote-status
            (and (not (ende-openrouter-batch--json-error-p payload))
                 (ende-protocol-json-get "status" payload))))
      (cond
       ((ende-openrouter-batch--json-error-p payload)
        (ende-openrouter-batch--group-fail group payload))
       ((not (stringp remote-status))
        (ende-openrouter-batch--group-fail
         group (ende-error-create
                :kind 'protocol :message "Batch status returned no status"
                :details payload)))
       ((string= remote-status "completed")
        (ende-openrouter-batch--finish-completed group payload))
       ((ende-openrouter-batch--terminal-status-p remote-status)
        (ende-openrouter-batch--group-fail
         group (ende-error-create
                :kind (if (string= remote-status "cancelled")
                          'cancelled 'provider)
                :message (format "OpenRouter batch ended with status %s"
                                 remote-status)
                :details payload)))
       (t
        (setf (ende-openrouter-batch--group-poll-attempt group) 0)
        (ende-openrouter-batch--status
         group (ende-openrouter-batch--status-label remote-status))
        (ende-openrouter-batch--schedule-poll group)))))))

(defun ende-openrouter-batch--poll (group)
  "Poll GROUP's remote batch."
  (when (and (not (ende-openrouter-batch--group-terminal group))
             (null (ende-openrouter-batch--group-stage group)))
    (if (>= (float-time) (ende-openrouter-batch--group-deadline group))
        (ende-openrouter-batch--group-fail
         group (ende-error-create
                :kind 'timeout :message "OpenRouter batch exceeded its deadline"))
      (ende-openrouter-batch--status
       group (ende-openrouter-batch--status-label
              (ende-openrouter-batch--group-status group)))
      (ende-openrouter-batch--start-stage
       group "GET" (ende-openrouter-batch--batch-url group) nil
       (apply-partially #'ende-openrouter-batch--poll-finished group)))))

(defun ende-openrouter-batch--choice (body)
  "Return the first completion choice from BODY."
  (let ((choices (ende-protocol-json-get "choices" body)))
    (cond
     ((and (vectorp choices) (> (length choices) 0)) (aref choices 0))
     ((and (listp choices) choices) (car choices)))))

(defun ende-openrouter-batch--normalize-result (body profile batch-usage)
  "Normalize one non-streaming completion BODY."
  (let* ((choice (ende-openrouter-batch--choice body))
         (message (ende-protocol-json-get "message" choice))
         (content (or (ende-protocol-json-get "content" message)
                      (ende-protocol-json-get "text" choice)))
         (reasoning (or (ende-protocol-json-get "reasoning_content" message)
                        (ende-protocol-json-get "reasoning" message)))
         (usage (ende-protocol-json-get "usage" body))
         (reported-model (ende-protocol-json-get "model" body))
         (finish-reason (ende-protocol-json-get "finish_reason" choice)))
    (let* ((parts (ende-content-output-parts content (ende-protocol-json-get "images" message)))
           (text (mapconcat (lambda (p) (if (eq (plist-get p :type) :text)
                                           (plist-get p :text) "")) parts "")))
     (list :content text :output-parts parts
          :thinking (and (stringp reasoning) reasoning)
          :usage (or usage batch-usage)
          :batch-usage batch-usage
          :tokens (or (ende-protocol-json-get
                       "completion_tokens" (or usage batch-usage))
                      (ende-protocol-json-get
                       "output_tokens" (or usage batch-usage))
                      0)
          :estimated nil
          :speed 0.0
          :finish-reason finish-reason
          :provider (ende-provider-client-name profile)
          :model (or reported-model (plist-get profile :model))))))

(defun ende-openrouter-batch--result-error (result)
  "Return the structured error represented by RESULT, or nil."
  (let ((error-payload (ende-protocol-json-get "error" result))
        (response (ende-protocol-json-get "response" result)))
    (cond
     (error-payload
      (ende-error-create
       :kind 'provider
       :message (format "%s"
                        (or (ende-protocol-json-get "message" error-payload)
                            error-payload))
       :details error-payload))
     ((and response
           (numberp (ende-protocol-json-get "status_code" response))
           (not (= 200 (ende-protocol-json-get "status_code" response))))
      (ende-error-create
       :kind 'provider
       :message (format "Batch request returned status %s"
                        (ende-protocol-json-get "status_code" response))
       :details response)))))

(defun ende-openrouter-batch--finish-completed (group payload)
  "Route completed PAYLOAD results to GROUP requests."
  (let* ((results (ende-protocol-json-get "results" payload))
         (usage (ende-protocol-json-get "usage" payload))
         (by-id (make-hash-table :test #'equal))
         (valid (or (vectorp results) (listp results)))
         (failure nil))
    (unless valid
      (setq failure
            (ende-error-create
             :kind 'protocol :message "Completed batch returned no results"
             :details payload)))
    (when valid
      (dolist (result (if (vectorp results) (append results nil) results))
        (let ((custom-id (ende-protocol-json-get "custom_id" result)))
          (if (or (not (stringp custom-id))
                  (gethash custom-id by-id)
                  (not (cl-some
                        (lambda (request)
                          (equal custom-id
                                 (ende-openrouter-batch--request-custom-id
                                  request)))
                        (ende-openrouter-batch--group-requests group))))
              (setq failure
                    (ende-error-create
                     :kind 'protocol
                     :message "Batch results contain an unknown, missing, or duplicate custom id"
                     :details result))
            (puthash custom-id result by-id)))))
    (dolist (request (ende-openrouter-batch--group-requests group))
      (unless (or failure
                  (ende-openrouter-batch--request-cancelled request)
                  (ende-openrouter-batch--request-settled request))
        (unless (gethash (ende-openrouter-batch--request-custom-id request) by-id)
          (setq failure
                (ende-error-create
                 :kind 'protocol
                 :message "Completed batch omitted a requested custom id")))))
    (if failure
        (ende-openrouter-batch--group-fail group failure)
      (setf (ende-openrouter-batch--group-terminal group) t)
      (ende-openrouter-batch--group-cleanup group)
      (ende-openrouter-batch--unregister-group group)
      (dolist (request (ende-openrouter-batch--group-requests group))
        (unless (ende-openrouter-batch--request-cancelled request)
          (let* ((result (gethash
                          (ende-openrouter-batch--request-custom-id request)
                          by-id))
                 (error (ende-openrouter-batch--result-error result))
                 (response (ende-protocol-json-get "response" result))
                 (body (and response
                            (ende-protocol-json-get "body" response))))
            (if error
                (ende-openrouter-batch--settle-request
                 request 'failure nil error)
              (ende-openrouter-batch--diagnose-result request body)
              (condition-case caught
                  (let ((normalized
                         (ende-openrouter-batch--normalize-result
                          body
                          (ende-openrouter-batch--request-profile request)
                          usage)))
                    (ende-openrouter-batch--emit
                     request
                     (list :type :metadata
                           :metadata
                           (list :source :response
                                 :model (plist-get normalized :model)
                                 :provider (plist-get normalized :provider))))
                    (ende-openrouter-batch--emit
                     request
                     (list :type :usage
                           :tokens (plist-get normalized :tokens)
                           :speed 0.0
                           :estimated nil
                           :usage (plist-get normalized :usage)))
                    (ende-openrouter-batch--settle-request
                     request 'success normalized nil))
                ((error quit)
                 (ende-openrouter-batch--settle-request
                  request 'failure nil
                  (ende-error-create
                   :kind 'protocol
                   :message (error-message-string caught))))))))))))

(defun ende-openrouter-batch--cancel-request (request)
  "Cancel REQUEST locally and stop an unsubmitted empty group."
  (unless (or (ende-openrouter-batch--request-cancelled request)
              (ende-openrouter-batch--request-settled request))
    (setf (ende-openrouter-batch--request-cancelled request) t)
    (let* ((profile (ende-openrouter-batch--request-profile request))
           (group (gethash (ende-openrouter-batch--profile-key profile)
                           ende-openrouter-batch--groups)))
      (when (and group
                 (null (ende-openrouter-batch--active-requests group)))
        (remhash (ende-openrouter-batch--group-key group)
                 ende-openrouter-batch--groups)
        (ende-openrouter-batch--group-cleanup group)
        (ende-openrouter-batch--unregister-group group))))
  nil)

(defun ende-openrouter-batch-auto-fallback-p (profile status detail)
  "Return non-nil when PROFILE's STATUS and DETAIL request Batch fallback."
  (and (eq (plist-get profile :service) 'openrouter)
       (eq (plist-get profile :transport) 'auto)
       (numberp status)
       (= status 404)
       (string-match-p "[Bb]atch API" (or detail ""))))

(defun ende-openrouter-batch-handoff (messages callbacks profile)
  "Handoff MESSAGES and CALLBACKS through OpenRouter Batch for PROFILE.
Return a structured error when the request cannot be transferred."
  (if (plist-get profile :tool-definitions)
      (ende-error-create
       :kind 'unsupported
       :message "Tool calls are unavailable with the OpenRouter batch transport")
    (let ((batch-profile (copy-tree profile)))
      (setq batch-profile
            (plist-put batch-profile :transport 'openrouter-batch))
      (condition-case caught
          (progn
            (ende-openrouter-batch-default
             messages callbacks batch-profile)
            nil)
        ((error quit)
         (ende-error-create
          :kind 'startup :message (error-message-string caught)))))))

(defun ende-openrouter-batch-default (messages callbacks &optional options)
  "Submit MESSAGES through OpenRouter's asynchronous Batch API.
CALLBACKS uses the standard ende transport callback contract.  OPTIONS is a
provider profile snapshot.  Compatible calls are collected and submitted in
one remote batch."
  (let* ((profile (copy-tree options))
         (key nil)
         (request nil)
         (group nil))
    (ende-openrouter-batch--validate-options profile)
    (setq key (ende-openrouter-batch--profile-key profile)
          request
          (ende-openrouter-batch--make-request
           :custom-id
           (format "ende-job-%s-%d"
                   (or (plist-get profile :job-id) "request")
                   (cl-incf ende-openrouter-batch--next-request-id))
           :messages messages
           :callbacks callbacks
           :profile profile))
    (setq group
          (or (gethash key ende-openrouter-batch--groups)
              (let ((new-group
                     (ende-openrouter-batch--make-group
                      :key key :profile profile :requests nil
                      :poll-attempt 0 :not-found-attempt 0)))
                (puthash key new-group ende-openrouter-batch--groups)
                (ende-openrouter-batch--register-group new-group))))
    (push request (ende-openrouter-batch--group-requests group))
    (ende-openrouter-batch--invoke
     request :on-start
     (apply-partially #'ende-openrouter-batch--cancel-request request))
    (ende-openrouter-batch--emit
     request (list :type :status :status "Batch queued"))
    (let ((limit (ende-openrouter-batch--profile-value
                  profile :batch-max-requests
                  ende-openrouter-batch-max-requests)))
      (if (>= (length (ende-openrouter-batch--group-requests group)) limit)
          (ende-openrouter-batch--flush group)
        (unless (ende-openrouter-batch--group-timer group)
          (setf (ende-openrouter-batch--group-timer group)
                (run-at-time
                 (ende-openrouter-batch--profile-value
                  profile :batch-window ende-openrouter-batch-window)
                 nil #'ende-openrouter-batch--flush group)))))
    request))

(defun ende-openrouter-batch-abort ()
  "Cancel pending Batch groups and all local Batch timers."
  (interactive)
  (let ((groups (copy-sequence ende-openrouter-batch--known-groups)))
    (clrhash ende-openrouter-batch--groups)
    (dolist (group groups)
      (setf (ende-openrouter-batch--group-terminal group) t)
      (ende-openrouter-batch--group-cleanup group)
      (ende-openrouter-batch--unregister-group group)
      (dolist (request (ende-openrouter-batch--group-requests group))
        (setf (ende-openrouter-batch--request-cancelled request) t)))))

(provide 'ende-openrouter-batch)

;;; ende-openrouter-batch.el ends here
