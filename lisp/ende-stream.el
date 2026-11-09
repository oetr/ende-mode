;;; ende-stream.el --- Shared SSE streaming driver for ende -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter Samarin
;;
;; Author: Peter Samarin <peter.samarin@gmail.com>
;; Maintainer: Peter Samarin <peter.samarin@gmail.com>
;; Keywords: convenience, tools
;; URL: https://github.com/oetr/ende-mode
;; SPDX-License-Identifier: AGPL-3.0-only

;; This file is part of Ende.

;; Ende is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, version 3 of the License.
;;
;; Ende is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with Ende.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Shared byte, SSE, process, HTTP-header, and settlement mechanics for streams.

;;; Code:

(require 'cl-lib)
(require 'ende-curl)
(require 'ende-protocol)
(require 'ende-transport)

(cl-defstruct (ende-stream-text (:constructor ende-stream-text-create))
  "Append-only text, materialized only at an explicit consumption boundary."
  parts tail (length 0))

(defun ende-stream-text-append (text fragment)
  "Append a detached FRAGMENT to TEXT without copying its existing prefix."
  (when (and (stringp fragment) (> (length fragment) 0))
    (let ((cell (list (copy-sequence fragment))))
      (if (ende-stream-text-tail text)
          (setcdr (ende-stream-text-tail text) cell)
        (setf (ende-stream-text-parts text) cell))
      (setf (ende-stream-text-tail text) cell)
      (cl-incf (ende-stream-text-length text) (length fragment))))
  text)

(defun ende-stream-text-substring (text start &optional end)
  "Return detached TEXT between START and END, without joining other text."
  (let* ((length (ende-stream-text-length text))
         (end (or end length))
         (offset 0) parts)
    (unless (<= 0 start end length)
      (signal 'args-out-of-range (list start end length)))
    (let ((remaining (ende-stream-text-parts text)))
      (while (and remaining (< offset end))
        (let* ((part (pop remaining))
               (next (+ offset (length part))))
          (when (> next start)
            (push (substring part (max 0 (- start offset))
                             (min (length part) (- end offset))) parts))
          (setq offset next))))
    (mapconcat #'identity (nreverse parts) "")))

(defun ende-stream-text-string (text)
  "Return a detached snapshot of TEXT."
  (mapconcat #'identity (ende-stream-text-parts text) ""))

(defun ende-stream-text-prefix (text limit)
  "Return at most LIMIT characters of TEXT without joining its remainder."
  (ende-stream-text-substring text 0 (min limit (ende-stream-text-length text))))

(cl-defstruct (ende-stream-session
               (:constructor ende-stream--make-session))
  "Protocol-independent state for one SSE response stream."
  lifecycle parser received-bytes preview-parts preview-bytes
  content-parts output-parts thinking-parts usage actual-model actual-provider
  estimated-chars output-counts (output-offset 0)
  generation-start finish-reason reasoning-summaries
  input-head input-tail (input-offset 0) input-closed drain-timer draining
  event-callback error-callback filter-error-callback pending-terminal)

(defun ende-stream-create (lifecycle parser)
  "Create a stream driver composed with LIFECYCLE and using PARSER."
  (ende-stream--make-session
   :lifecycle lifecycle :parser parser :received-bytes 0
   :preview-parts nil :preview-bytes 0
   :content-parts nil :thinking-parts nil :estimated-chars 0
   :output-counts (make-hash-table :test #'equal)
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

(defun ende-stream-account-output (stream key chars limit-callback &optional snapshot)
  "Account for CHARS of output KEY in STREAM before retaining it.
CHARS changes the streamed length, or replaces the snapshot length when
SNAPSHOT is non-nil.  Charge the larger representation, not both copies.
Return non-nil if accepted.  Call LIMIT-CALLBACK if the limit is exceeded."
  (unless (ende-stream-terminal-p stream)
    (let* ((counts (ende-stream-session-output-counts stream))
           (old (gethash key counts '(0 . 0)))
           (new (if snapshot (cons (car old) chars)
                  (cons (+ (car old) chars) (cdr old))))
           (total (+ (ende-stream-session-estimated-chars stream)
                     (- (max (car new) (cdr new))
                        (max (car old) (cdr old))))))
      (if (and (numberp ende-max-output-chars)
               (> (+ (ende-stream-session-output-offset stream) total)
                  ende-max-output-chars))
          (progn
            (funcall limit-callback
                     (ende-error-create
                      :kind 'resource
                      :message "generated output exceeds ende-max-output-chars"))
            nil)
        (puthash key new counts)
        (setf (ende-stream-session-estimated-chars stream) total)
        t))))

(defun ende-stream-record-delta (stream channel text limit-callback &optional item-key)
  "Accumulate and emit TEXT on CHANNEL for STREAM.
Call LIMIT-CALLBACK with a resource error if the output limit is exceeded.
ITEM-KEY identifies the response item supplying the delta.
Return non-nil when the delta was accepted and STREAM remains active."
  (when (and (stringp text) (> (length text) 0)
             (ende-stream-account-output
              stream (cons channel item-key) (length text) limit-callback))
    (if (eq channel :content)
        (progn
          (push text (ende-stream-session-content-parts stream))
          (push (list :type :text :text text)
                (ende-stream-session-output-parts stream)))
      (push text (ende-stream-session-thinking-parts stream)))
    (ende-stream-note-generation-start stream)
    (ende-stream-emit
     stream (append (list :type :delta :channel channel :text text)
                    (when item-key (list :item-key item-key))))
    (not (ende-stream-terminal-p stream))))

(defun ende-stream-emit-item (stream key kind provider &rest fields)
  "Emit an observation of STREAM's item KEY of KIND from PROVIDER.
FIELDS supplies its raw :item, normalized :call, or full :text."
  (ende-stream-emit
   stream (append (list :type :item :key key :kind kind :provider provider)
                  fields)))

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

(defun ende-stream-record-reasoning-summary
    (stream item text limit-callback &optional item-key)
  "Emit summary TEXT for provider ITEM in STREAM, with source ITEM-KEY.
Call LIMIT-CALLBACK if the output limit is exceeded."
  (when (and (stringp text) (> (length text) 0)
             (ende-stream-account-output
              stream (cons :reasoning-summary (or item-key item))
              (length text) limit-callback))
    (let ((summaries (ende-stream-session-reasoning-summaries stream)))
      (ende-stream-text-append
       (or (gethash item summaries)
           (puthash item (ende-stream-text-create) summaries)) text)
      (ende-stream-note-generation-start stream)
      (ende-stream-emit
       stream (append (list :type :reasoning-summary :item item :text text)
                      (when item-key (list :item-key item-key))))
      (not (ende-stream-terminal-p stream)))))

(defun ende-stream-complete-reasoning-summary
    (stream item text limit-callback &optional item-key)
  "Emit completed TEXT for ITEM with source ITEM-KEY unless already emitted.
Call LIMIT-CALLBACK if STREAM's output limit is exceeded."
  (when (and (not (ende-stream-terminal-p stream))
             (stringp text) (> (length text) 0))
    (let* ((summaries (ende-stream-session-reasoning-summaries stream))
           (existing (gethash item summaries)))
      (unless (and existing (equal (ende-stream-text-string existing) text))
        (when (ende-stream-account-output
               stream (cons :reasoning-summary (or item-key item))
               (- (length text) (if existing (ende-stream-text-length existing) 0))
               limit-callback)
          (puthash item (ende-stream-text-append (ende-stream-text-create) text) summaries)
          (ende-stream-emit
           stream (append (list :type :reasoning-summary :item item :text text
                                :replace t)
                          (when item-key (list :item-key item-key)))))))))

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
  (ende-transport-safe-error-text
   (mapconcat #'identity (reverse (ende-stream-session-preview-parts stream)) "")))

(defun ende-stream-dispatch-events (stream events callback)
  "Pass parsed SSE EVENTS to CALLBACK while STREAM remains active."
  (unless (ende-stream-terminal-p stream)
    (funcall callback events)))

(defun ende-stream-response-headers (stream)
  "Return the final response headers for STREAM."
  (ende-curl-response-headers
   (ende-transport-session-headers-file
    (ende-stream-session-lifecycle stream))))

(defconst ende-stream--drain-chars 32768
  "Maximum new input characters parsed per stream continuation.")

(defconst ende-stream--drain-events 32
  "Maximum SSE events dispatched per stream continuation.")

(defun ende-stream--release-input (stream)
  "Release STREAM's queued input and invalidate its owned continuation."
  (when-let* ((timer (ende-stream-session-drain-timer stream)))
    (cancel-timer timer))
  (setf (ende-stream-session-drain-timer stream) nil
        (ende-stream-session-input-head stream) nil
        (ende-stream-session-input-tail stream) nil
        (ende-stream-session-input-offset stream) 0
        (ende-stream-session-input-closed stream) t
        (ende-stream-session-event-callback stream) nil
        (ende-stream-session-error-callback stream) nil
        (ende-stream-session-filter-error-callback stream) nil
        (ende-stream-session-pending-terminal stream) nil)
  (when-let* ((parser (ende-stream-session-parser stream)))
    (setf (ende-protocol--sse-buffer-parts parser) nil
          (ende-protocol--sse-buffer-bytes parser) 0
          (ende-protocol--sse-data-lines parser) nil
          (ende-protocol--sse-data-bytes parser) 0)))

(defun ende-stream--schedule-drain (stream)
  "Arrange one yielding continuation for STREAM's pending input."
  (when (and (not (ende-stream-terminal-p stream))
             (not (ende-stream-session-draining stream))
             (not (ende-stream-session-drain-timer stream))
             (or (ende-stream-session-input-head stream)
                 (ende-stream-session-pending-terminal stream)))
    (let (timer)
      (setq timer
            (run-at-time
             0.001 nil
             (lambda ()
               (when (eq timer (ende-stream-session-drain-timer stream))
                 (setf (ende-stream-session-drain-timer stream) nil)
                 (ende-stream--drain stream)))))
      (setf (ende-stream-session-drain-timer stream) timer))))

(defun ende-stream--drain (stream)
  "Parse and dispatch a bounded turn of STREAM's queued input."
  (unless (ende-stream-terminal-p stream)
    (setf (ende-stream-session-draining stream) t)
    (unwind-protect
        (let ((chars ende-stream--drain-chars)
              (events ende-stream--drain-events))
          (while (and (> chars 0) (> events 0)
                      (ende-stream-session-input-head stream)
                      (not (ende-stream-terminal-p stream)))
            (let* ((entry (car (ende-stream-session-input-head stream)))
                   (chunk (car entry))
                   (offset (ende-stream-session-input-offset stream))
                   (end (min (length chunk) (+ offset chars))))
              (condition-case filter-error
                  (let* ((read (ende-protocol-sse-read
                                (ende-stream-session-parser stream)
                                chunk offset end))
                         (next (car read)))
                    (cl-decf chars (- next offset))
                    (setf (ende-stream-session-input-offset stream) next)
                    (when (cdr read)
                      (cl-decf events)
                      (ende-stream-dispatch-events
                       stream (list (cdr read))
                       (ende-stream-session-event-callback stream))))
                ((error quit)
                 (funcall (ende-stream-session-filter-error-callback stream)
                          filter-error (cdr entry))))
              (when (and (not (ende-stream-terminal-p stream))
                         (= (ende-stream-session-input-offset stream)
                            (length chunk)))
                (setf (ende-stream-session-input-head stream)
                      (cdr (ende-stream-session-input-head stream))
                      (ende-stream-session-input-offset stream) 0)
                (unless (ende-stream-session-input-head stream)
                  (setf (ende-stream-session-input-tail stream) nil))
                (when (cdr entry)
                  (funcall
                   (ende-stream-session-error-callback stream)
                   (ende-error-create
                    :kind 'resource
                    :message "response exceeds ende-max-response-bytes"))))))
          (when (and (not (ende-stream-terminal-p stream))
                     (not (ende-stream-session-input-head stream))
                     (ende-stream-session-pending-terminal stream))
            (let ((terminal (ende-stream-session-pending-terminal stream)))
              (setf (ende-stream-session-pending-terminal stream) nil)
              (funcall terminal))))
      (setf (ende-stream-session-draining stream) nil)
      (ende-stream--schedule-drain stream))))

(defun ende-stream-filter
    (stream chunk event-callback error-callback filter-error-callback)
  "Queue accepted SSE CHUNK for STREAM without parsing in the process filter.
EVENT-CALLBACK receives parsed events.  ERROR-CALLBACK receives a structured
resource error; FILTER-ERROR-CALLBACK receives a parser error and whether the
same chunk exceeded the total response limit.  `ende-max-response-bytes' bounds
receipt, including queued input; nil deliberately disables that bound."
  (unless (or (ende-stream-terminal-p stream)
              (ende-stream-session-input-closed stream))
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
      (when (or over-limit (> (length processable) 0))
        (unless (ende-stream-session-event-callback stream)
          (setf (ende-stream-session-event-callback stream) event-callback
                (ende-stream-session-error-callback stream) error-callback
                (ende-stream-session-filter-error-callback stream)
                filter-error-callback)
          (ende-transport-session-add-canceller
           (ende-stream-lifecycle stream)
           (apply-partially #'ende-stream--release-input stream)))
        (ende-stream-append-preview stream processable)
        ;; Queue raw chunk references; only the head uses `input-offset'.
        (let ((cell (list (cons processable over-limit))))
          (if (ende-stream-session-input-tail stream)
              (setcdr (ende-stream-session-input-tail stream) cell)
            (setf (ende-stream-session-input-head stream) cell))
          (setf (ende-stream-session-input-tail stream) cell))
        (when over-limit
          (setf (ende-stream-session-input-closed stream) t))
        (ende-stream--schedule-drain stream)))))

(defun ende-stream-sentinel
    (stream process event process-error-callback response-callback
            header-error-callback &optional idle-error-first)
  "Handle terminal PROCESS EVENT for STREAM.
PROCESS-ERROR-CALLBACK receives PROCESS, EVENT, and any idle error.  On a
successful exit RESPONSE-CALLBACK receives parsed response headers.  Header
parsing failures are sent to HEADER-ERROR-CALLBACK.  When IDLE-ERROR-FIRST is
non-nil, a recorded idle error takes precedence over the exit status."
  (when (and (not (ende-stream-terminal-p stream))
             (not (ende-stream-session-pending-terminal stream))
             (memq (process-status process) '(exit signal closed)))
    (setf (ende-stream-session-input-closed stream) t)
    (let ((terminal
           (apply-partially #'ende-stream--finish-process
                            stream process event process-error-callback
                            response-callback header-error-callback
                            idle-error-first)))
      (if (or (ende-stream-session-input-head stream)
              (ende-stream-session-draining stream))
          (progn
            (setf (ende-stream-session-pending-terminal stream) terminal)
            (ende-stream--schedule-drain stream))
        (funcall terminal)))))

(defun ende-stream--finish-process
    (stream process event process-error-callback response-callback
            header-error-callback idle-error-first)
  "Handle PROCESS EVENT after STREAM's preceding input has drained."
  (unless (ende-stream-terminal-p stream)
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
