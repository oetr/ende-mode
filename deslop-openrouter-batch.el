;;; deslop-openrouter-batch.el --- OpenRouter Batch transport for deslop -*- lexical-binding: t; -*-

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
(require 'json)
(require 'subr-x)
(require 'url-util)
(require 'deslop-core)
(require 'deslop-curl)
(require 'deslop-protocol)
(require 'deslop-provider)

;;;; User options

(defcustom deslop-openrouter-batch-window 0.05
  "Seconds to collect compatible requests before submitting a batch."
  :type 'number
  :group 'deslop)

(defcustom deslop-openrouter-batch-max-requests 16
  "Maximum number of logical requests submitted in one batch."
  :type 'integer
  :group 'deslop)

(defcustom deslop-openrouter-batch-max-request-bytes (* 16 1024 1024)
  "Maximum encoded request body size for one OpenRouter batch."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Maximum bytes"))
  :group 'deslop)

(defcustom deslop-openrouter-batch-poll-initial-delay 2.0
  "Seconds to wait before the first Batch API poll."
  :type 'number
  :group 'deslop)

(defcustom deslop-openrouter-batch-poll-not-found-max-age (* 5 60)
  "Maximum seconds to retry an initially invisible batch."
  :type 'number
  :group 'deslop)

(defcustom deslop-openrouter-batch-poll-max-delay 60.0
  "Maximum delay between Batch API polls."
  :type 'number
  :group 'deslop)

(defcustom deslop-openrouter-batch-max-age (* 24 60 60)
  "Maximum age of a submitted batch before local polling gives up."
  :type 'number
  :group 'deslop)

(defcustom deslop-openrouter-batch-poll-jitter 0.15
  "Fractional random jitter applied to Batch API poll delays."
  :type 'number
  :group 'deslop)

;;;; State

(cl-defstruct (deslop-openrouter-batch--request
               (:constructor deslop-openrouter-batch--make-request))
  "One logical deslop request waiting for or belonging to a batch."
  custom-id messages callbacks profile cancelled settled)

(cl-defstruct (deslop-openrouter-batch--group
               (:constructor deslop-openrouter-batch--make-group))
  "A group of compatible logical requests submitted as one remote batch."
  key profile requests timer stage body batch-id status poll-attempt
  not-found-attempt batch-visible-p poll-timer submitted-at deadline terminal)

(defvar deslop-openrouter-batch--groups (make-hash-table :test #'equal)
  "Pending OpenRouter Batch groups keyed by their request shape.")

(defvar deslop-openrouter-batch--known-groups nil
  "Batch groups with pending local timers or remote work.")

(defvar deslop-openrouter-batch--next-request-id 0
  "Next local identifier used for Batch API custom ids.")

(defun deslop-openrouter-batch--register-group (group)
  "Track GROUP until its local lifecycle is terminal."
  (cl-pushnew group deslop-openrouter-batch--known-groups :test #'eq)
  group)

(defun deslop-openrouter-batch--unregister-group (group)
  "Stop tracking terminal GROUP."
  (setq deslop-openrouter-batch--known-groups
        (delq group deslop-openrouter-batch--known-groups))
  group)

(defun deslop-openrouter-batch--profile-value (profile key default)
  "Return PROFILE's KEY, or DEFAULT when KEY is absent."
  (if (plist-member profile key)
      (plist-get profile key)
    default))

(defun deslop-openrouter-batch--validate-options (profile)
  "Validate Batch options in PROFILE and global defaults."
  (dolist (option
           (list (cons 'deslop-openrouter-batch-window
                       (deslop-openrouter-batch--profile-value
                        profile :batch-window
                        deslop-openrouter-batch-window))
                 (cons 'deslop-openrouter-batch-poll-initial-delay
                       (deslop-openrouter-batch--profile-value
                        profile :batch-poll-initial-delay
                        deslop-openrouter-batch-poll-initial-delay))
                 (cons 'deslop-openrouter-batch-poll-not-found-max-age
                       (deslop-openrouter-batch--profile-value
                        profile :batch-poll-not-found-max-age
                        deslop-openrouter-batch-poll-not-found-max-age))
                 (cons 'deslop-openrouter-batch-poll-max-delay
                       (deslop-openrouter-batch--profile-value
                        profile :batch-poll-max-delay
                        deslop-openrouter-batch-poll-max-delay))
                 (cons 'deslop-openrouter-batch-max-age
                       (deslop-openrouter-batch--profile-value
                        profile :batch-max-age
                        deslop-openrouter-batch-max-age))
                 (cons 'deslop-openrouter-batch-poll-jitter
                       (deslop-openrouter-batch--profile-value
                        profile :batch-poll-jitter
                        deslop-openrouter-batch-poll-jitter))))
    (unless (and (numberp (cdr option)) (>= (cdr option) 0)
                 (or (not (eq (car option)
                              'deslop-openrouter-batch-poll-jitter))
                     (<= (cdr option) 1)))
      (error "%s must be a nonnegative number" (car option))))
  (let ((max-requests
         (deslop-openrouter-batch--profile-value
          profile :batch-max-requests deslop-openrouter-batch-max-requests))
        (max-bytes
         (deslop-openrouter-batch--profile-value
          profile :batch-max-request-bytes
          deslop-openrouter-batch-max-request-bytes)))
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
    (deslop-curl--validated-url profile url))
  (unless (and (stringp (plist-get profile :batch-endpoint))
               (string-prefix-p "/v" (plist-get profile :batch-endpoint)))
    (error "Batch profile has an invalid :batch-endpoint"))
  t)

(defun deslop-openrouter-batch--profile-key (profile)
  "Return a stable key for compatible request PROFILE values."
  (secure-hash
   'sha256
   (prin1-to-string
    (list (plist-get profile :name)
          (plist-get profile :batch-url)
          (plist-get profile :batch-endpoint)
          (plist-get profile :model)
          (plist-get profile :temperature)
          (plist-get profile :max-tokens)
          deslop-max-output-tokens
          (plist-get profile :request-headers)))))

(defun deslop-openrouter-batch--active-requests (group)
  "Return unsettled requests in GROUP."
  (cl-remove-if
   (lambda (request)
     (or (deslop-openrouter-batch--request-cancelled request)
         (deslop-openrouter-batch--request-settled request)))
   (deslop-openrouter-batch--group-requests group)))

(defun deslop-openrouter-batch--invoke (request name &rest arguments)
  "Invoke callback NAME for REQUEST without leaking callback failures."
  (let ((callback (plist-get (deslop-openrouter-batch--request-callbacks request)
                             name)))
    (when callback
      (condition-case callback-error
          (apply callback arguments)
        ((error quit)
         (unless (eq name :on-error)
           (let ((error-callback
                  (plist-get
                   (deslop-openrouter-batch--request-callbacks request)
                   :on-error)))
             (when error-callback
               (deslop-ignore-errors
                error-callback
                (deslop-error-create
                 :kind 'callback
                 :message (error-message-string callback-error)))))))))))

(defun deslop-openrouter-batch--emit (request event)
  "Emit EVENT for REQUEST while it remains live."
  (unless (or (deslop-openrouter-batch--request-cancelled request)
              (deslop-openrouter-batch--request-settled request))
    (deslop-openrouter-batch--invoke request :on-event event)))

(defun deslop-openrouter-batch--status (group status)
  "Emit STATUS to all live requests in GROUP."
  (setf (deslop-openrouter-batch--group-status group) status)
  (dolist (request (deslop-openrouter-batch--active-requests group))
    (deslop-openrouter-batch--emit
     request (list :type :status :status status))))

(defun deslop-openrouter-batch--settle-request
    (request outcome &optional result error)
  "Settle REQUEST with OUTCOME, RESULT, and ERROR exactly once."
  (unless (or (deslop-openrouter-batch--request-cancelled request)
              (deslop-openrouter-batch--request-settled request))
    (setf (deslop-openrouter-batch--request-settled request) outcome)
    (if (eq outcome 'success)
        (deslop-openrouter-batch--invoke request :on-done result)
      (deslop-openrouter-batch--invoke
       request :on-event
       (list :type :error :error error :result result))
      (deslop-openrouter-batch--invoke request :on-error error))))

(defun deslop-openrouter-batch--cancel-timer (timer)
  "Cancel TIMER when it is a live timer."
  (when (timerp timer)
    (deslop-ignore-errors #'cancel-timer timer)))

(defun deslop-openrouter-batch--cleanup-stage (stage)
  "Release resources owned by STAGE."
  (when stage
    (let ((request (plist-get stage :request)))
      (deslop-curl-cleanup-resources
       (list (plist-get request :response-buffer)
             (plist-get request :stderr-buffer))
       (list (plist-get request :headers-file)
             (plist-get request :config-file))
       (list (plist-get request :process)
             (plist-get request :stderr-process))))))

(defun deslop-openrouter-batch--group-cleanup (group)
  "Cancel timers and release the active HTTP stage for GROUP."
  (deslop-openrouter-batch--cancel-timer
   (deslop-openrouter-batch--group-timer group))
  (deslop-openrouter-batch--cancel-timer
   (deslop-openrouter-batch--group-poll-timer group))
  (setf (deslop-openrouter-batch--group-timer group) nil
        (deslop-openrouter-batch--group-poll-timer group) nil)
  (deslop-openrouter-batch--cleanup-stage
   (deslop-openrouter-batch--group-stage group))
  (setf (deslop-openrouter-batch--group-stage group) nil))

(defun deslop-openrouter-batch--group-fail (group error)
  "Fail all live requests in GROUP with structured ERROR."
  (unless (deslop-openrouter-batch--group-terminal group)
    (setf (deslop-openrouter-batch--group-terminal group) t)
    (deslop-openrouter-batch--group-cleanup group)
    (deslop-openrouter-batch--unregister-group group)
    (dolist (request (deslop-openrouter-batch--active-requests group))
      (deslop-openrouter-batch--settle-request request 'failure nil error))))

(defun deslop-openrouter-batch--response-error
    (status body &optional stage-error)
  "Build an error for HTTP STATUS, BODY, or STAGE-ERROR."
  (or stage-error
      (let* ((detail (and (stringp body)
                          (deslop-transport-error-detail body)))
             (message (or detail
                          (and (stringp body) (not (string-empty-p body)) body)
                          (format "HTTP status %s" status))))
        (deslop-error-create
         :kind (if (and status (>= status 400)) 'http 'transport)
         :message (if status
                      (format "HTTP status %s: %s" status message)
                    message)
         :details body))))

(defun deslop-openrouter-batch--request-body (messages profile custom-id)
  "Build one OpenRouter Batch request for MESSAGES and PROFILE."
  `((:custom_id . ,custom-id)
    (:body . ((:messages . ,(mapcar
                             (lambda (message)
                               `((:role . ,(car message))
                                 (:content . ,(cdr message))))
                             messages))
              ,@(when (numberp (plist-get profile :temperature))
                  (list (cons :temperature
                              (plist-get profile :temperature))))
              ,@(when (integerp deslop-max-output-tokens)
                  (list (cons :max_tokens deslop-max-output-tokens)))))))

(defun deslop-openrouter-batch--body (group)
  "Build the ordered JSON object submitted for GROUP."
  (let* ((profile (deslop-openrouter-batch--group-profile group))
         (requests
          (mapcar
           (lambda (request)
             (deslop-openrouter-batch--request-body
              (deslop-openrouter-batch--request-messages request)
              profile
              (deslop-openrouter-batch--request-custom-id request)))
           (deslop-openrouter-batch--group-requests group)))
         (body `((:endpoint . ,(plist-get profile :batch-endpoint))
                 (:model . ,(plist-get profile :model))
                 (:requests . ,requests)))
         (encoded (encode-coding-string (json-encode body) 'utf-8))
         (limit (deslop-openrouter-batch--profile-value
                 profile :batch-max-request-bytes
                 deslop-openrouter-batch-max-request-bytes)))
    (when (and limit (> (string-bytes encoded) limit))
      (error "Batch request exceeds deslop-openrouter-batch-max-request-bytes"))
    encoded))

(defun deslop-openrouter-batch--append-response (process chunk)
  "Append CHUNK to PROCESS's bounded JSON response buffer."
  (let* ((buffer (process-get process :deslop-response-buffer))
         (used (or (process-get process :deslop-batch-response-bytes) 0))
         (total (+ used (string-bytes chunk)))
         (remaining (and deslop-max-response-bytes
                         (- deslop-max-response-bytes used))))
    (if (and deslop-max-response-bytes
             (> total deslop-max-response-bytes))
        (progn
          (process-put
           process :deslop-batch-response-error
           (deslop-error-create
            :kind 'resource
            :message "Batch API response exceeds deslop-max-response-bytes"))
          (when (process-live-p process)
            (delete-process process)))
      (when (and buffer (> (length chunk) 0))
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert (if remaining
                      (deslop-transport-byte-prefix chunk remaining)
                    chunk))))
      (process-put process :deslop-batch-response-bytes total))))

(defun deslop-openrouter-batch--stage-body (stage)
  "Return the accumulated response body for STAGE."
  (let ((buffer (plist-get (plist-get stage :request) :response-buffer)))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer (buffer-string))
      "")))

(defun deslop-openrouter-batch--stage-sentinel
    (group stage process event)
  "Finish one HTTP STAGE for GROUP."
  (when (and (not (deslop-openrouter-batch--group-terminal group))
             (eq stage (deslop-openrouter-batch--group-stage group))
             (memq (process-status process) '(exit signal closed failed)))
    (setf (deslop-openrouter-batch--group-stage group) nil)
    (let* ((request (plist-get stage :request))
           (status (condition-case nil
                       (car (deslop-curl-response-headers
                             (plist-get request :headers-file)))
                     (error nil)))
           (body (deslop-openrouter-batch--stage-body stage))
           (stage-error
            (or (process-get process :deslop-batch-response-error)
                (process-get process :deslop-idle-error)
                (unless (= (process-exit-status process) 0)
                  (deslop-error-create
                   :kind 'transport
                   :message
                   (format "curl exited with status %d (%s)"
                           (process-exit-status process)
                           (string-trim event)))))))
      (deslop-openrouter-batch--cleanup-stage stage)
      (funcall (plist-get stage :callback) status body stage-error))))

(cl-defun deslop-openrouter-batch--start-stage
    (group method url body callback)
  "Start one JSON HTTP STAGE for GROUP.
CALLBACK receives STATUS, BODY, and a structured transport error or nil."
  (when (deslop-openrouter-batch--group-stage group)
    (error "Batch group already has an active HTTP stage"))
  (let* ((stage nil)
         (request
          (deslop-curl-start-request
           :profile (deslop-openrouter-batch--group-profile group)
           :url url
           :name (format "deslop-openrouter-batch-%s"
                         (or (plist-get
                              (deslop-openrouter-batch--group-profile group)
                              :job-id)
                             "request"))
           :command
           (lambda (headers-file config-file)
             (deslop-curl-json-command
              (deslop-openrouter-batch--group-profile group)
              headers-file config-file method "application/json"
              (not (null body)) url))
           :filter #'deslop-openrouter-batch--append-response
           :sentinel
           (lambda (process event)
             (deslop-openrouter-batch--stage-sentinel
              group stage process event)))))
    (setq stage (list :request request :callback callback))
    (setf (deslop-openrouter-batch--group-stage group) stage)
    (when (and body
               (not (deslop-openrouter-batch--group-terminal group)))
      (process-send-string (plist-get request :process) body))
    (unless (deslop-openrouter-batch--group-terminal group)
      (process-send-eof (plist-get request :process)))
    request))

(defun deslop-openrouter-batch--batch-url (group)
  "Return the status URL for GROUP's remote batch."
  (concat (string-remove-suffix "/"
                                (plist-get
                                 (deslop-openrouter-batch--group-profile group)
                                 :batch-url))
          "/"
          (url-hexify-string
           (deslop-openrouter-batch--group-batch-id group))))

(defun deslop-openrouter-batch--retryable-status-p (status)
  "Return non-nil when HTTP STATUS is safe to retry while polling."
  (memq status '(408 425 429 500 502 503 504 524 529)))

(defun deslop-openrouter-batch--jittered-delay (group &optional attempt)
  "Return the poll delay for GROUP and retry ATTEMPT."
  (let* ((profile (deslop-openrouter-batch--group-profile group))
         (initial (deslop-openrouter-batch--profile-value
                   profile :batch-poll-initial-delay
                   deslop-openrouter-batch-poll-initial-delay))
         (maximum (deslop-openrouter-batch--profile-value
                   profile :batch-poll-max-delay
                   deslop-openrouter-batch-poll-max-delay))
         (attempt (or attempt
                       (deslop-openrouter-batch--group-poll-attempt group)))
         (base (min maximum (* initial (expt 2 attempt))))
         (jitter (deslop-openrouter-batch--profile-value
                  profile :batch-poll-jitter
                  deslop-openrouter-batch-poll-jitter)))
    (if (zerop jitter)
        base
      (* base (+ 1.0 (* jitter (- (random 2001) 1000) 0.001))))))

(defun deslop-openrouter-batch--initial-not-found-retry-p (group)
  "Return non-nil while GROUP may still be propagating remotely."
  (let* ((profile (deslop-openrouter-batch--group-profile group))
         (max-age (deslop-openrouter-batch--profile-value
                   profile :batch-poll-not-found-max-age
                   deslop-openrouter-batch-poll-not-found-max-age))
         (submitted-at (deslop-openrouter-batch--group-submitted-at group)))
    (and (not (deslop-openrouter-batch--group-batch-visible-p group))
         (numberp submitted-at)
         (< (- (float-time) submitted-at) max-age))))

(defun deslop-openrouter-batch--not-found-error (group body)
  "Build a terminal not-found error for GROUP and response BODY."
  (let* ((batch-id (deslop-openrouter-batch--group-batch-id group))
         (url (deslop-openrouter-batch--batch-url group))
         (visibility
          (if (deslop-openrouter-batch--group-batch-visible-p group)
              "after it was previously visible"
            "before it became visible and after the initial retry window")))
    (deslop-error-create
     :kind 'http
     :message (format "OpenRouter batch %s was not found at %s; %s"
                      batch-id url visibility)
     :details body)))

(defun deslop-openrouter-batch--retry-not-found (group body)
  "Retry an initially invisible GROUP, or fail it with BODY."
  (if (deslop-openrouter-batch--initial-not-found-retry-p group)
      (let* ((profile (deslop-openrouter-batch--group-profile group))
             (max-age (deslop-openrouter-batch--profile-value
                       profile :batch-poll-not-found-max-age
                       deslop-openrouter-batch-poll-not-found-max-age))
             (submitted-at (deslop-openrouter-batch--group-submitted-at group))
             (remaining (- max-age (- (float-time) submitted-at)))
             (delay
              (deslop-openrouter-batch--jittered-delay
               group
               (deslop-openrouter-batch--group-not-found-attempt group))))
        (if (< delay remaining)
            (progn
              (cl-incf
               (deslop-openrouter-batch--group-not-found-attempt group))
              (deslop-openrouter-batch--schedule-poll group delay))
          (deslop-openrouter-batch--group-fail
           group (deslop-openrouter-batch--not-found-error group body))))
    (deslop-openrouter-batch--group-fail
     group (deslop-openrouter-batch--not-found-error group body))))

(defun deslop-openrouter-batch--schedule-poll (group &optional delay)
  "Schedule a status poll for GROUP after DELAY seconds."
  (deslop-openrouter-batch--cancel-timer
   (deslop-openrouter-batch--group-poll-timer group))
  (let* ((profile (deslop-openrouter-batch--group-profile group))
         (max-age (deslop-openrouter-batch--profile-value
                   profile :batch-max-age deslop-openrouter-batch-max-age))
         (deadline (deslop-openrouter-batch--group-deadline group))
         (delay (or delay (deslop-openrouter-batch--jittered-delay group))))
    (when (and (not (deslop-openrouter-batch--group-terminal group))
               (< (float-time) deadline)
               (<= (- (float-time) (deslop-openrouter-batch--group-submitted-at group))
                   max-age))
      (setf (deslop-openrouter-batch--group-poll-timer group)
            (run-at-time delay nil
                         #'deslop-openrouter-batch--poll group)))))

(defun deslop-openrouter-batch--terminal-status-p (status)
  "Return non-nil when remote STATUS is terminal."
  (member status '("completed" "failed" "expired" "cancelled")))

(defun deslop-openrouter-batch--status-label (status)
  "Return a concise UI status for remote STATUS."
  (pcase status
    ("validating" "Batch validating")
    ("in_progress" "Batch processing")
    ("finalizing" "Batch finalizing")
    ("cancelling" "Batch cancelling")
    ("cancelled" "Batch cancelled")
    ("completed" "Retrieving batch results")
    (_ "Batch processing")))

(defun deslop-openrouter-batch--parse-object (body)
  "Parse nonempty JSON BODY or return a protocol error."
  (condition-case caught
      (if (and (stringp body) (not (string-empty-p (string-trim body))))
          (deslop-protocol-json-read-string body)
        (deslop-error-create :kind 'protocol :message "Batch API returned an empty body"))
    ((error quit)
     (deslop-error-create
      :kind 'protocol
      :message (format "Malformed Batch API JSON: %s"
                       (error-message-string caught))))))

(defun deslop-openrouter-batch--json-error-p (value)
  "Return non-nil when VALUE is a structured parsing error."
  (deslop-error-p value))

(defun deslop-openrouter-batch--submit-finished
    (group status body stage-error)
  "Handle the response to GROUP's batch submission."
  (cond
   (stage-error
    (deslop-openrouter-batch--group-fail
     group (deslop-error-create
            :kind 'submission-unknown
            :message "OpenRouter batch submission outcome is unknown"
            :details stage-error)))
   ((not (= status 202))
    (deslop-openrouter-batch--group-fail
     group (deslop-openrouter-batch--response-error status body)))
   (t
    (let* ((payload (deslop-openrouter-batch--parse-object body))
           (batch-id (and (not (deslop-openrouter-batch--json-error-p payload))
                          (deslop-protocol-json-get "id" payload)))
           (status-name (and (not (deslop-openrouter-batch--json-error-p payload))
                             (deslop-protocol-json-get "status" payload))))
      (cond
       ((deslop-openrouter-batch--json-error-p payload)
        (deslop-openrouter-batch--group-fail group payload))
       ((or (not (stringp batch-id)) (string-empty-p batch-id))
        (deslop-openrouter-batch--group-fail
         group (deslop-error-create
                :kind 'protocol :message "Batch submission returned no id"
                :details payload)))
       ((not (stringp status-name))
        (deslop-openrouter-batch--group-fail
         group (deslop-error-create
                :kind 'protocol :message "Batch submission returned no status"
                :details payload)))
       (t
        (setf (deslop-openrouter-batch--group-batch-id group) batch-id
              (deslop-openrouter-batch--group-status group) status-name
              (deslop-openrouter-batch--group-submitted-at group) (float-time)
              (deslop-openrouter-batch--group-deadline group)
              (+ (float-time)
                 (deslop-openrouter-batch--profile-value
                  (deslop-openrouter-batch--group-profile group)
                  :batch-max-age deslop-openrouter-batch-max-age)))
        (deslop-openrouter-batch--status
         group (deslop-openrouter-batch--status-label status-name))
        (deslop-openrouter-batch--schedule-poll
         group
         (deslop-openrouter-batch--profile-value
          (deslop-openrouter-batch--group-profile group)
          :batch-poll-initial-delay
          deslop-openrouter-batch-poll-initial-delay))))))))

(defun deslop-openrouter-batch--submit (group)
  "Submit GROUP to OpenRouter."
  (unless (deslop-openrouter-batch--group-terminal group)
    (condition-case caught
        (progn
          (deslop-openrouter-batch--status group "Submitting batch")
          (setf (deslop-openrouter-batch--group-body group)
                (deslop-openrouter-batch--body group))
          (deslop-openrouter-batch--start-stage
           group "POST"
           (plist-get (deslop-openrouter-batch--group-profile group)
                      :batch-url)
           (deslop-openrouter-batch--group-body group)
           (apply-partially #'deslop-openrouter-batch--submit-finished group)))
      ((error quit)
       (deslop-openrouter-batch--group-fail
        group (deslop-error-create
               :kind 'startup :message (error-message-string caught)))))))

(defun deslop-openrouter-batch--flush (group)
  "Submit pending requests in GROUP."
  (when (and (not (deslop-openrouter-batch--group-terminal group))
             (gethash (deslop-openrouter-batch--group-key group)
                      deslop-openrouter-batch--groups))
    (remhash (deslop-openrouter-batch--group-key group)
             deslop-openrouter-batch--groups)
    (setf (deslop-openrouter-batch--group-timer group) nil)
     (if (deslop-openrouter-batch--active-requests group)
        (deslop-openrouter-batch--submit group)
      (deslop-openrouter-batch--group-cleanup group)
      (deslop-openrouter-batch--unregister-group group))))

(defun deslop-openrouter-batch--poll-finished
    (group status body stage-error)
  "Handle one status response for GROUP."
  (cond
   (stage-error
    (if (and (deslop-error-p stage-error)
             (memq (deslop-error-kind stage-error) '(timeout transport))
             (< (deslop-openrouter-batch--group-poll-attempt group) 8))
        (progn
          (cl-incf (deslop-openrouter-batch--group-poll-attempt group))
          (deslop-openrouter-batch--schedule-poll group))
      (deslop-openrouter-batch--group-fail group stage-error)))
   ((not (= status 200))
    (cond
     ((= status 404)
      (deslop-openrouter-batch--retry-not-found group body))
     ((deslop-openrouter-batch--retryable-status-p status)
      (cl-incf (deslop-openrouter-batch--group-poll-attempt group))
      (deslop-openrouter-batch--schedule-poll group))
     (t
      (deslop-openrouter-batch--group-fail
       group (deslop-openrouter-batch--response-error status body)))))
   (t
    (setf (deslop-openrouter-batch--group-batch-visible-p group) t
          (deslop-openrouter-batch--group-not-found-attempt group) 0)
    (let* ((payload (deslop-openrouter-batch--parse-object body))
           (remote-status
            (and (not (deslop-openrouter-batch--json-error-p payload))
                 (deslop-protocol-json-get "status" payload))))
      (cond
       ((deslop-openrouter-batch--json-error-p payload)
        (deslop-openrouter-batch--group-fail group payload))
       ((not (stringp remote-status))
        (deslop-openrouter-batch--group-fail
         group (deslop-error-create
                :kind 'protocol :message "Batch status returned no status"
                :details payload)))
       ((string= remote-status "completed")
        (deslop-openrouter-batch--finish-completed group payload))
       ((deslop-openrouter-batch--terminal-status-p remote-status)
        (deslop-openrouter-batch--group-fail
         group (deslop-error-create
                :kind (if (string= remote-status "cancelled")
                          'cancelled 'provider)
                :message (format "OpenRouter batch ended with status %s"
                                 remote-status)
                :details payload)))
       (t
        (setf (deslop-openrouter-batch--group-poll-attempt group) 0)
        (deslop-openrouter-batch--status
         group (deslop-openrouter-batch--status-label remote-status))
        (deslop-openrouter-batch--schedule-poll group)))))))

(defun deslop-openrouter-batch--poll (group)
  "Poll GROUP's remote batch."
  (when (and (not (deslop-openrouter-batch--group-terminal group))
             (null (deslop-openrouter-batch--group-stage group)))
    (if (>= (float-time) (deslop-openrouter-batch--group-deadline group))
        (deslop-openrouter-batch--group-fail
         group (deslop-error-create
                :kind 'timeout :message "OpenRouter batch exceeded its deadline"))
      (deslop-openrouter-batch--status
       group (deslop-openrouter-batch--status-label
              (deslop-openrouter-batch--group-status group)))
      (deslop-openrouter-batch--start-stage
       group "GET" (deslop-openrouter-batch--batch-url group) nil
       (apply-partially #'deslop-openrouter-batch--poll-finished group)))))

(defun deslop-openrouter-batch--choice (body)
  "Return the first completion choice from BODY."
  (let ((choices (deslop-protocol-json-get "choices" body)))
    (cond
     ((and (vectorp choices) (> (length choices) 0)) (aref choices 0))
     ((and (listp choices) choices) (car choices)))))

(defun deslop-openrouter-batch--normalize-result (body profile batch-usage)
  "Normalize one non-streaming completion BODY."
  (let* ((choice (deslop-openrouter-batch--choice body))
         (message (deslop-protocol-json-get "message" choice))
         (content (or (deslop-protocol-json-get "content" message)
                      (deslop-protocol-json-get "text" choice)))
         (reasoning (or (deslop-protocol-json-get "reasoning_content" message)
                        (deslop-protocol-json-get "reasoning" message)))
         (usage (deslop-protocol-json-get "usage" body))
         (reported-model (deslop-protocol-json-get "model" body))
         (finish-reason (deslop-protocol-json-get "finish_reason" choice)))
    (unless (stringp content)
      (error "Batch completion has no text content"))
    (list :content content
          :thinking (and (stringp reasoning) reasoning)
          :usage (or usage batch-usage)
          :batch-usage batch-usage
          :tokens (or (deslop-protocol-json-get
                       "completion_tokens" (or usage batch-usage))
                      (deslop-protocol-json-get
                       "output_tokens" (or usage batch-usage))
                      0)
          :estimated nil
          :speed 0.0
          :finish-reason finish-reason
          :provider (deslop-provider-client-name profile)
          :model (or reported-model (plist-get profile :model)))))

(defun deslop-openrouter-batch--result-error (result)
  "Return the structured error represented by RESULT, or nil."
  (let ((error-payload (deslop-protocol-json-get "error" result))
        (response (deslop-protocol-json-get "response" result)))
    (cond
     (error-payload
      (deslop-error-create
       :kind 'provider
       :message (format "%s"
                        (or (deslop-protocol-json-get "message" error-payload)
                            error-payload))
       :details error-payload))
     ((and response
           (numberp (deslop-protocol-json-get "status_code" response))
           (not (= 200 (deslop-protocol-json-get "status_code" response))))
      (deslop-error-create
       :kind 'provider
       :message (format "Batch request returned status %s"
                        (deslop-protocol-json-get "status_code" response))
       :details response)))))

(defun deslop-openrouter-batch--finish-completed (group payload)
  "Route completed PAYLOAD results to GROUP requests."
  (let* ((results (deslop-protocol-json-get "results" payload))
         (usage (deslop-protocol-json-get "usage" payload))
         (by-id (make-hash-table :test #'equal))
         (valid (or (vectorp results) (listp results)))
         (failure nil))
    (unless valid
      (setq failure
            (deslop-error-create
             :kind 'protocol :message "Completed batch returned no results"
             :details payload)))
    (when valid
      (dolist (result (if (vectorp results) (append results nil) results))
        (let ((custom-id (deslop-protocol-json-get "custom_id" result)))
          (if (or (not (stringp custom-id))
                  (gethash custom-id by-id)
                  (not (cl-some
                        (lambda (request)
                          (equal custom-id
                                 (deslop-openrouter-batch--request-custom-id
                                  request)))
                        (deslop-openrouter-batch--group-requests group))))
              (setq failure
                    (deslop-error-create
                     :kind 'protocol
                     :message "Batch results contain an unknown, missing, or duplicate custom id"
                     :details result))
            (puthash custom-id result by-id)))))
    (dolist (request (deslop-openrouter-batch--group-requests group))
      (unless (or failure
                  (deslop-openrouter-batch--request-cancelled request)
                  (deslop-openrouter-batch--request-settled request))
        (unless (gethash (deslop-openrouter-batch--request-custom-id request) by-id)
          (setq failure
                (deslop-error-create
                 :kind 'protocol
                 :message "Completed batch omitted a requested custom id")))))
    (if failure
        (deslop-openrouter-batch--group-fail group failure)
      (setf (deslop-openrouter-batch--group-terminal group) t)
      (deslop-openrouter-batch--group-cleanup group)
      (deslop-openrouter-batch--unregister-group group)
      (dolist (request (deslop-openrouter-batch--group-requests group))
        (unless (deslop-openrouter-batch--request-cancelled request)
          (let* ((result (gethash
                          (deslop-openrouter-batch--request-custom-id request)
                          by-id))
                 (error (deslop-openrouter-batch--result-error result))
                 (response (deslop-protocol-json-get "response" result))
                 (body (and response
                            (deslop-protocol-json-get "body" response))))
            (if error
                (deslop-openrouter-batch--settle-request
                 request 'failure nil error)
              (condition-case caught
                  (let ((normalized
                         (deslop-openrouter-batch--normalize-result
                          body
                          (deslop-openrouter-batch--request-profile request)
                          usage)))
                    (deslop-openrouter-batch--emit
                     request
                     (list :type :metadata
                           :metadata
                           (list :source :response
                                 :model (plist-get normalized :model)
                                 :provider (plist-get normalized :provider))))
                    (deslop-openrouter-batch--emit
                     request
                     (list :type :usage
                           :tokens (plist-get normalized :tokens)
                           :speed 0.0
                           :estimated nil
                           :usage (plist-get normalized :usage)))
                    (deslop-openrouter-batch--settle-request
                     request 'success normalized nil))
                ((error quit)
                 (deslop-openrouter-batch--settle-request
                  request 'failure nil
                  (deslop-error-create
                   :kind 'protocol
                   :message (error-message-string caught))))))))))))

(defun deslop-openrouter-batch--cancel-request (request)
  "Cancel REQUEST locally and stop an unsubmitted empty group."
  (unless (or (deslop-openrouter-batch--request-cancelled request)
              (deslop-openrouter-batch--request-settled request))
    (setf (deslop-openrouter-batch--request-cancelled request) t)
    (let* ((profile (deslop-openrouter-batch--request-profile request))
           (group (gethash (deslop-openrouter-batch--profile-key profile)
                           deslop-openrouter-batch--groups)))
      (when (and group
                 (null (deslop-openrouter-batch--active-requests group)))
        (remhash (deslop-openrouter-batch--group-key group)
                 deslop-openrouter-batch--groups)
        (deslop-openrouter-batch--group-cleanup group)
        (deslop-openrouter-batch--unregister-group group))))
  nil)

(defun deslop-openrouter-batch-default (messages callbacks &optional options)
  "Submit MESSAGES through OpenRouter's asynchronous Batch API.
CALLBACKS uses the standard deslop transport callback contract.  OPTIONS is a
provider profile snapshot.  Compatible calls are collected and submitted in
one remote batch."
  (let* ((profile (copy-tree options))
         (key nil)
         (request nil)
         (group nil))
    (deslop-openrouter-batch--validate-options profile)
    (setq key (deslop-openrouter-batch--profile-key profile)
          request
          (deslop-openrouter-batch--make-request
           :custom-id
           (format "deslop-job-%s-%d"
                   (or (plist-get profile :job-id) "request")
                   (cl-incf deslop-openrouter-batch--next-request-id))
           :messages messages
           :callbacks callbacks
           :profile profile))
    (setq group
          (or (gethash key deslop-openrouter-batch--groups)
              (let ((new-group
                     (deslop-openrouter-batch--make-group
                      :key key :profile profile :requests nil
                      :poll-attempt 0 :not-found-attempt 0)))
                (puthash key new-group deslop-openrouter-batch--groups)
                (deslop-openrouter-batch--register-group new-group))))
    (push request (deslop-openrouter-batch--group-requests group))
    (deslop-openrouter-batch--invoke
     request :on-start
     (apply-partially #'deslop-openrouter-batch--cancel-request request))
    (deslop-openrouter-batch--emit
     request (list :type :status :status "Batch queued"))
    (let ((limit (deslop-openrouter-batch--profile-value
                  profile :batch-max-requests
                  deslop-openrouter-batch-max-requests)))
      (if (>= (length (deslop-openrouter-batch--group-requests group)) limit)
          (deslop-openrouter-batch--flush group)
        (unless (deslop-openrouter-batch--group-timer group)
          (setf (deslop-openrouter-batch--group-timer group)
                (run-at-time
                 (deslop-openrouter-batch--profile-value
                  profile :batch-window deslop-openrouter-batch-window)
                 nil #'deslop-openrouter-batch--flush group)))))
    request))

(defun deslop-openrouter-batch-abort ()
  "Cancel pending Batch groups and all local Batch timers."
  (interactive)
  (let ((groups (copy-sequence deslop-openrouter-batch--known-groups)))
    (clrhash deslop-openrouter-batch--groups)
    (dolist (group groups)
      (setf (deslop-openrouter-batch--group-terminal group) t)
      (deslop-openrouter-batch--group-cleanup group)
      (deslop-openrouter-batch--unregister-group group)
      (dolist (request (deslop-openrouter-batch--group-requests group))
        (setf (deslop-openrouter-batch--request-cancelled request) t)))))

(provide 'deslop-openrouter-batch)

;;; deslop-openrouter-batch.el ends here
