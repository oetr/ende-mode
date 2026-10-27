;;; ende-stream.el --- Shared SSE streaming driver for ende -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools, llm

;;; Commentary:

;; Shared byte, SSE, process, HTTP-header, and settlement mechanics for streams.

;;; Code:

(require 'cl-lib)
(require 'ende-curl)
(require 'ende-protocol)
(require 'ende-transport)

(cl-defstruct (ende-stream-session
               (:constructor ende-stream--make-session))
  "Protocol-independent state for one SSE response stream."
  lifecycle parser received-bytes preview-parts preview-bytes
  content-parts output-parts thinking-parts usage actual-model actual-provider
  estimated-chars generation-start finish-reason reasoning-summaries)

(defun ende-stream-create (lifecycle parser)
  "Create a stream driver composed with LIFECYCLE and using PARSER."
  (ende-stream--make-session
   :lifecycle lifecycle :parser parser :received-bytes 0
   :preview-parts nil :preview-bytes 0
   :content-parts nil :thinking-parts nil :estimated-chars 0
   :reasoning-summaries (make-hash-table :test #'equal)))

(defun ende-stream-lifecycle (stream)
  "Return STREAM's transport lifecycle."
  (ende-stream-session-lifecycle stream))

(defun ende-stream-terminal-p (stream)
  "Return non-nil when STREAM's lifecycle has settled."
  (ende-transport-session-terminal
   (ende-stream-session-lifecycle stream)))

(defun ende-stream-emit (stream event)
  "Emit EVENT for STREAM unless its transport has settled."
  (unless (ende-stream-terminal-p stream)
    (ende-transport-session-invoke
     (ende-stream-lifecycle stream) :on-event event)))

(defun ende-stream-note-generation-start (stream)
  "Record the beginning of generation for STREAM if not already recorded."
  (unless (ende-stream-session-generation-start stream)
    (setf (ende-stream-session-generation-start stream) (float-time)))
  (ende-stream-session-generation-start stream))

(defun ende-stream--speed (tokens started now)
  "Return TOKENS per second when STARTED and NOW give a useful interval."
  (let ((elapsed (and (numberp started) (- now started))))
    (when (and (numberp tokens) (> tokens 0)
               (numberp elapsed) (>= elapsed 0.1))
      (/ (float tokens) elapsed))))

(defun ende-stream-record-delta (stream channel text limit-callback)
  "Accumulate and emit TEXT on CHANNEL for STREAM.
Call LIMIT-CALLBACK with a resource error if the output limit is exceeded.
Return non-nil when the delta was accepted and STREAM remains active."
  (when (and (not (ende-stream-terminal-p stream))
             (stringp text) (> (length text) 0))
    (if (and (numberp ende-max-output-chars)
             (> (+ (ende-stream-session-estimated-chars stream)
                   (length text))
                ende-max-output-chars))
        (progn
          (funcall
           limit-callback
           (ende-error-create
            :kind 'resource
            :message "generated output exceeds ende-max-output-chars"))
          nil)
      (progn
        (if (eq channel :content)
            (progn
              (push text (ende-stream-session-content-parts stream))
              (push (list :type :text :text text)
                    (ende-stream-session-output-parts stream)))
          (push text (ende-stream-session-thinking-parts stream)))
        (cl-incf (ende-stream-session-estimated-chars stream) (length text))
        (ende-stream-note-generation-start stream)
        (ende-stream-emit
         stream (list :type :delta :channel channel :text text))
        (not (ende-stream-terminal-p stream))))))

(defun ende-stream-record-image (stream part)
  "Record generated image PART without emitting its bytes as an event."
  (ende-stream-note-generation-start stream)
  (push part (ende-stream-session-output-parts stream)))

(defun ende-stream-record-usage (stream usage token-key)
  "Record USAGE on STREAM and emit its normalized usage event.
TOKEN-KEY names the provider-specific output-token field."
  (setf (ende-stream-session-usage stream) usage)
  (let* ((reported (ende-protocol-json-get token-key usage))
         (tokens (if (numberp reported) reported 0))
         (now (float-time)))
    (ende-stream-emit
     stream (list :type :usage :tokens tokens
                  :speed (ende-stream--speed
                          tokens (ende-stream-session-generation-start stream)
                          now)
                  :usage usage))))

(defun ende-stream-record-reasoning-summary (stream item text)
  "Emit summary TEXT for provider ITEM in STREAM."
  (when (and (not (ende-stream-terminal-p stream))
             (stringp text) (> (length text) 0))
    (let ((summaries (ende-stream-session-reasoning-summaries stream)))
      (puthash item (concat (gethash item summaries "") text) summaries)
      (ende-stream-note-generation-start stream)
      (ende-stream-emit
       stream (list :type :reasoning-summary :item item :text text))
      (not (ende-stream-terminal-p stream)))))

(defun ende-stream-complete-reasoning-summary (stream item text)
  "Emit completed summary TEXT for ITEM unless its deltas already did so."
  (when (and (not (ende-stream-terminal-p stream))
             (stringp text) (> (length text) 0))
    (let* ((summaries (ende-stream-session-reasoning-summaries stream))
           (existing (gethash item summaries)))
      (unless (equal existing text)
        (ende-stream-emit
         stream (list :type :reasoning-summary :item item :text text
                      :replace t))
        (puthash item text summaries)))))

(defun ende-stream-emit-estimated-usage (stream)
  "Emit a character-based usage estimate for STREAM."
  (let* ((now (float-time))
         (tokens (max 1 (floor
                         (/ (ende-stream-session-estimated-chars stream)
                            4.0))))
         (started (ende-stream-session-generation-start stream)))
    (ende-stream-emit
     stream (list :type :usage :tokens tokens
                  :speed (ende-stream--speed tokens started now)
                  :estimated t))))

(defun ende-stream-result
    (stream token-key model-fallback provider-fallback &optional extra-fields)
  "Build a normalized result from STREAM, extended with EXTRA-FIELDS.
TOKEN-KEY is the provider-specific output-token field."
  (let* ((usage (ende-stream-session-usage stream))
         (reported (and usage (ende-protocol-json-get token-key usage)))
         (tokens (or reported
                     (max 0 (floor
                             (/ (ende-stream-session-estimated-chars stream)
                                4.0)))))
         (started (ende-stream-session-generation-start stream)))
    (append
     (list :content (mapconcat #'identity
                               (reverse
                                (ende-stream-session-content-parts stream)) "")
           :thinking (mapconcat #'identity
                                (reverse
                                 (ende-stream-session-thinking-parts stream)) "")
           :usage usage :tokens tokens :estimated (null reported)
           :speed (ende-stream--speed tokens started (float-time))
           :finish-reason (ende-stream-session-finish-reason stream)
           :provider (or (ende-stream-session-actual-provider stream)
                         provider-fallback)
           :model (or (ende-stream-session-actual-model stream)
                      model-fallback))
     (when (cl-some (lambda (part) (eq (plist-get part :type) :generated-image))
                    (ende-stream-session-output-parts stream))
       (list :output-parts (reverse (ende-stream-session-output-parts stream))))
     extra-fields)))

(defun ende-stream-has-result-p (stream &optional protocol-state allow-finish)
  "Return non-nil if STREAM or PROTOCOL-STATE has partial result data."
  (or (ende-stream-session-output-parts stream)
      (ende-stream-session-content-parts stream)
      (ende-stream-session-thinking-parts stream)
      (ende-stream-session-usage stream)
      (and allow-finish
           (ende-stream-session-finish-reason stream))
      protocol-state))

(defun ende-stream-append-preview (stream chunk)
  "Append a bounded, byte-safe prefix of CHUNK to STREAM's preview."
  (let* ((used (or (ende-stream-session-preview-bytes stream) 0))
         (remaining (and ende-max-error-body-bytes
                         (- ende-max-error-body-bytes used)))
         (prefix (if remaining
                     (and (> remaining 0)
                          (ende-transport-byte-prefix chunk remaining))
                   chunk)))
    (when prefix
      (push prefix (ende-stream-session-preview-parts stream))
      (setf (ende-stream-session-preview-bytes stream)
            (+ used (string-bytes prefix))))))

(defun ende-stream-preview-text (stream)
  "Return STREAM's bounded response preview."
  (mapconcat #'identity
             (reverse (ende-stream-session-preview-parts stream)) ""))

(defun ende-stream-dispatch-events (stream events callback)
  "Pass parsed SSE EVENTS to CALLBACK while STREAM remains active."
  (unless (ende-stream-terminal-p stream)
    (funcall callback events)))

(defun ende-stream-response-headers (stream)
  "Return the final response headers for STREAM."
  (ende-curl-response-headers
   (ende-transport-session-headers-file
    (ende-stream-session-lifecycle stream))))

(defun ende-stream-filter
    (stream chunk event-callback error-callback filter-error-callback)
  "Consume SSE CHUNK for STREAM.
EVENT-CALLBACK receives parsed events.  ERROR-CALLBACK receives a structured
resource error; FILTER-ERROR-CALLBACK receives a parser error and whether the
same chunk exceeded the total response limit."
  (unless (ende-stream-terminal-p stream)
    (let* ((received (ende-stream-session-received-bytes stream))
           (chunk-bytes (string-bytes chunk))
           (total (+ received chunk-bytes))
           (remaining (and ende-max-response-bytes
                           (- ende-max-response-bytes received)))
           (over-limit (and ende-max-response-bytes
                            (> total ende-max-response-bytes)))
           (processable (if over-limit
                            (ende-transport-byte-prefix chunk
                                                        (max 0 remaining))
                          chunk)))
      (setf (ende-stream-session-received-bytes stream) total)
      (ende-stream-append-preview stream chunk)
      (condition-case filter-error
          (when (> (length processable) 0)
            (ende-stream-dispatch-events
             stream
             (ende-protocol-sse-feed
              (ende-stream-session-parser stream) processable)
             event-callback))
        ((error quit)
         (funcall filter-error-callback filter-error over-limit)))
      (when (and over-limit (not (ende-stream-terminal-p stream)))
        (funcall
         error-callback
         (ende-error-create
          :kind 'resource
          :message "response exceeds ende-max-response-bytes"))))))

(defun ende-stream-sentinel
    (stream process event process-error-callback response-callback
            header-error-callback &optional idle-error-first)
  "Handle terminal PROCESS EVENT for STREAM.
PROCESS-ERROR-CALLBACK receives PROCESS, EVENT, and any idle error.  On a
successful exit RESPONSE-CALLBACK receives parsed response headers.  Header
parsing failures are sent to HEADER-ERROR-CALLBACK.  When IDLE-ERROR-FIRST is
non-nil, a recorded idle error takes precedence over the exit status."
  (when (and (not (ende-stream-terminal-p stream))
             (memq (process-status process) '(exit signal closed)))
    (let ((idle-error (process-get process :ende-idle-error)))
      (cond
       ((and idle-error idle-error-first)
        (funcall process-error-callback process event idle-error))
       ((/= (process-exit-status process) 0)
        (funcall process-error-callback process event idle-error))
       (t
        (condition-case header-error
            (funcall
             response-callback
             (ende-stream-response-headers stream))
          ((error quit)
           (funcall header-error-callback header-error))))))))

(defun ende-stream-http-success-p (status)
  "Return non-nil when HTTP STATUS denotes a successful response."
  (and (numberp status) (>= status 200) (< status 300)))

(defun ende-stream-content-type-error (content-type policy)
  "Return a protocol error for CONTENT-TYPE under POLICY, or nil.
POLICY is `required' to require SSE, `if-present' to reject explicit other
types, or nil to skip validation."
  (when (pcase policy
          ('required (not (ende-curl-event-stream-p content-type)))
          ('if-present (and content-type
                            (not (ende-curl-event-stream-p content-type))))
          (_ nil))
    (ende-error-create
     :kind 'protocol
     :message (format "expected text/event-stream, got %s"
                      (or content-type "unknown")))))

(defun ende-stream-settle (stream outcome value &optional partial-result)
  "Settle STREAM's lifecycle once with OUTCOME, VALUE, and PARTIAL-RESULT."
  (ende-transport-session-settle
   (ende-stream-session-lifecycle stream) outcome value partial-result))

(defun ende-stream-settle-generation
    (stream outcome value token-key model-fallback provider-fallback
            &optional extra-fields protocol-state allow-finish allow-tool-calls)
  "Settle a normalized generation and include its partial result when useful.
EXTRA-FIELDS extends the result plist; PROTOCOL-STATE can indicate additional
provider-specific partial data."
  (unless (ende-stream-terminal-p stream)
    (ende-stream-settle
     stream outcome value
     (when (or (ende-stream-has-result-p
                stream protocol-state allow-finish)
               (and allow-tool-calls
                    (plist-get extra-fields :tool-calls)))
       (ende-stream-result stream token-key model-fallback provider-fallback
                           extra-fields)))))

(provide 'ende-stream)

;;; ende-stream.el ends here
