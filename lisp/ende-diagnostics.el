;;; ende-diagnostics.el --- Bounded local diagnostic events -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter Samarin
;; SPDX-License-Identifier: AGPL-3.0-only

;;; Commentary:

;; Typed metadata only.  Contexts freeze session identity across callbacks.

;;; Code:

(require 'cl-lib)
(require 'ring)
(require 'json)
(require 'ende-core)

(defcustom ende-diagnostics-event-limit 512
  "Maximum retained diagnostic events, applied when starting a session."
  :type 'natnum :group 'ende)

(defconst ende-diagnostics-field-limit 64
  "Maximum retained string field length.")

(defconst ende-diagnostics--catalog
  '((debug-on . outcome) (debug-off . outcome)
    (capture-saved . outcome) (capture-dropped . warning)
    (content-enabled . warning) (bundle-saved . outcome)
    (captures-pruned . outcome) (debug-failed . error)
    (job-queued . detail) (job-started . detail) (job-waiting . outcome)
    (job-resumed . detail) (job-done . outcome) (job-failed . error)
    (job-cancelled . warning) (job-skipped . warning) (agent-spawned . outcome)
    (auth-started . detail) (auth-ready . detail)
    (prep-started . detail) (prep-ready . detail)
    (ws-connecting . detail) (ws-open . detail) (ws-reused . detail)
    (ws-closed . outcome) (ws-interrupted . warning)
    (request-offered . detail) (request-accepted . detail)
    (request-blocked . warning) (request-writable . detail)
    (response-first . detail) (response-done . outcome)
    (retry-scheduled . warning) (replay-refused . warning)
    (retries-exhausted . error) (timeout . error) (auth-failed . error)
    (provider-failed . error) (protocol-invalid . error) (limit-exceeded . error)
    (callback-failed . error) (transport-failed . error) (internal-failed . error)
    (tool-started . detail) (tool-done . outcome) (tool-failed . error)
    (tool-cancelled . warning) (work-checkpoint . detail)
    (chat-saved . outcome) (chat-save-failed . error) (work-retained . warning)))

(defconst ende-diagnostics--fields
  '((:parent natnum "parent") (:connection natnum "conn")
    (:attempt natnum "try") (:sequence natnum "capture")
    (:count natnum "count") (:call natnum "call") (:tools natnum "tools")
    (:bytes natnum "bytes") (:messages natnum "msgs")
    (:retries natnum "retries") (:http-status natnum "http")
    (:retry-limit natnum "retry-limit") (:timeout-limit number "timeout")
    (:byte-limit natnum "byte-limit") (:event-limit natnum "event-limit")
    (:close-code natnum "close") (:input-tokens natnum "in")
    (:output-tokens natnum "out") (:cached-tokens natnum "cached")
    (:elapsed number "seconds") (:connection-age number "age")
    (:delay number "delay") (:reused boolean "reused") (:clean boolean "clean")
    (:tool identifier "tool")
    (:transport (http websocket worker) "via")
    (:transport-phase (connecting upgrading preparing sending generation) "phase")
    (:delivery (unsent sending accepted) "delivery")
    (:cause (abort application close policy protocol size timeout transport utf8) "cause")
    (:deadline (handshake assembly close request idle connect) "deadline")
    (:reason (connect-timeout missing-response connection-limit unsent-replacement) "reason")
    (:last-event (response.created response.in_progress response.completed
                  response.done response.incomplete response.failed error
                  response.output_text.delta response.output_item.added
                  response.output_item.done response.reasoning_summary_text.delta
                  response.function_call_arguments.delta
                  response.function_call_arguments.done) "last")
    (:error-kind (auth timeout provider http incomplete protocol resource limit callback transport internal startup
                  cancelled skipped scheduler ownership target validation tool application) "error"))
  "Accepted field keys, types, and short display labels.")

(cl-defstruct (ende-diagnostics-context
               (:constructor ende-diagnostics--make-context))
  "Scalar correlation identifiers; SESSION must not change after creation."
  session job request attempt connection)

(defvar ende-diagnostics-enabled nil)
(defvar ende-diagnostics-session 0)
(defvar ende-diagnostics-dropped 0)
(defvar ende-diagnostics--ring nil)
(defvar ende-diagnostics--request 0)
(defvar ende-diagnostics--connection 0)
(defvar ende-diagnostics--call 0)
(defvar ende-diagnostics-failure-functions nil
  "Functions called with the session ID after an error event is recorded.")

(defun ende-diagnostics-start ()
  "Start an empty metadata session and return its numeric identity."
  (setq ende-diagnostics--ring (make-ring (max 1 ende-diagnostics-event-limit))
        ende-diagnostics-dropped 0
        ende-diagnostics--request 0
        ende-diagnostics--connection 0
        ende-diagnostics--call 0
        ende-diagnostics-session (1+ ende-diagnostics-session)
        ende-diagnostics-enabled t)
  ende-diagnostics-session)

(defun ende-diagnostics-stop ()
  "Disable recording, retaining the bounded store for inspection."
  (setq ende-diagnostics-enabled nil))

(defun ende-diagnostics-context-create (job &optional request-p)
  "Freeze the current session and numeric JOB ID, or nil for no job.
When REQUEST-P is non-nil, reserve a request ID and start attempt 1.
Return nil when recording is disabled.  Never retain a job object."
  (when ende-diagnostics-enabled
    (ende-diagnostics--make-context
     :session ende-diagnostics-session :job (and (natnump job) job)
     :request (and request-p (cl-incf ende-diagnostics--request))
     :attempt (and request-p 1))))

(defun ende-diagnostics-next-connection ()
  "Reserve a local numeric connection ID, or nil when disabled."
  (when ende-diagnostics-enabled (cl-incf ende-diagnostics--connection)))

(defun ende-diagnostics-next-call ()
  "Reserve a local numeric tool call ID, or nil when disabled."
  (when ende-diagnostics-enabled (cl-incf ende-diagnostics--call)))

(defun ende-diagnostics--scalar (value type)
  "Return a detached valid VALUE of TYPE, or nil."
  (pcase type
    ('natnum (and (fixnump value) (>= value 0) value))
    ('number (and (numberp value) (<= 0 value most-positive-fixnum) value))
    ('boolean (and (booleanp value) (if value t :false)))
    ('identifier
     (and (stringp value) (<= 1 (length value) ende-diagnostics-field-limit)
          (string-match-p "\\`[[:alnum:]_-]+\\'" value)
          (substring-no-properties value)))
    (_ (and (memq value type) value))))

(defun ende-diagnostics--copy-event (event)
  "Copy flat EVENT without sharing its strings."
  (mapcar (lambda (value) (if (stringp value) (copy-sequence value) value)) event))

(defun ende-diagnostics-record (kind context &rest fields)
  "Record known KIND for frozen CONTEXT, accepting only typed FIELDS.
Unknown events and fields, stale sessions, and malformed values are ignored.
No messages, payloads, headers, profiles, or exception text are accepted.
Return non-nil if retained; observer failures never propagate."
  (when (and ende-diagnostics-enabled (ende-diagnostics-context-p context)
             (eql (ende-diagnostics-context-session context) ende-diagnostics-session))
    (condition-case nil
        (when-let* ((level (alist-get kind ende-diagnostics--catalog)))
          (let ((event (list :time (float-time) :session ende-diagnostics-session
                             :kind kind :level level)))
            (dolist (entry (list (cons :job (ende-diagnostics-context-job context))
                                 (cons :request (ende-diagnostics-context-request context))
                                 (cons :attempt (ende-diagnostics-context-attempt context))
                                 (cons :connection (ende-diagnostics-context-connection context))))
              (when-let* ((value (ende-diagnostics--scalar (cdr entry) 'natnum)))
                (setq event (nconc event (list (car entry) value)))))
            (dolist (spec ende-diagnostics--fields)
              (when (plist-member fields (car spec))
                (when-let* ((value (ende-diagnostics--scalar
                                   (plist-get fields (car spec)) (cadr spec))))
                  (setq event (plist-put event (car spec) value)))))
            (when (= (ring-length ende-diagnostics--ring)
                     (ring-size ende-diagnostics--ring))
              (cl-incf ende-diagnostics-dropped))
            (ring-insert ende-diagnostics--ring event)
            (when (eq level 'error)
              (dolist (function ende-diagnostics-failure-functions)
                (ende-ignore-errors function ende-diagnostics-session)))
            t))
      ((error quit) nil))))

(defun ende-diagnostics-fail (context structured-error &optional fields event)
  "Record scalar classification of STRUCTURED-ERROR for CONTEXT.
FIELDS supplies a lifecycle snapshot; EVENT optionally refines classification.
Exception messages and unrecognized detail fields are never retained."
  (when (and ende-diagnostics-enabled (ende-error-p structured-error))
    (condition-case nil
        (let* ((kind (ende-error-kind structured-error))
               (details (ende-error-details structured-error))
               (fields (append (and (listp details) details) fields))
               (event (or event
                          (and (memq (plist-get fields :deadline)
                                     '(handshake assembly close request idle connect))
                               'timeout)
                          (alist-get kind '((auth . auth-failed) (timeout . timeout)
                                            (provider . provider-failed)
                                            (http . provider-failed) (incomplete . provider-failed)
                                            (protocol . protocol-invalid)
                                            (limit . limit-exceeded) (resource . limit-exceeded)
                                            (callback . callback-failed)
                                            (transport . transport-failed)))
                          'internal-failed)))
          (apply #'ende-diagnostics-record event context :error-kind kind
                 fields))
      ((error quit) nil))))

(defun ende-diagnostics-events (&optional job)
  "Return detached chronological sanitized event plists, optionally for JOB."
  (when ende-diagnostics--ring
    (cl-loop for event in (reverse (ring-elements ende-diagnostics--ring))
             when (or (null job) (eql job (plist-get event :job)))
             collect (ende-diagnostics--copy-event event))))

(defun ende-diagnostics-snapshot (&optional job)
  "Return a metadata-only JSON-ready snapshot, optionally filtered by JOB."
  (list :version 1 :emacs-version emacs-version
        :session ende-diagnostics-session :dropped ende-diagnostics-dropped
        :events (vconcat (ende-diagnostics-events job))))

(provide 'ende-diagnostics)
;;; ende-diagnostics.el ends here
