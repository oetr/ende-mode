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
  lifecycle parser received-bytes preview-parts preview-bytes)

(defun ende-stream-create (lifecycle parser)
  "Create a stream driver composed with LIFECYCLE and using PARSER."
  (ende-stream--make-session
   :lifecycle lifecycle :parser parser :received-bytes 0
   :preview-parts nil :preview-bytes 0))

(defun ende-stream-terminal-p (stream)
  "Return non-nil when STREAM's lifecycle has settled."
  (ende-transport-session-terminal
   (ende-stream-session-lifecycle stream)))

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
             (memq (process-status process) '(exit signal closed failed)))
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

(provide 'ende-stream)

;;; ende-stream.el ends here
