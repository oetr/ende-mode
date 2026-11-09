;;; ende-responses-ws.el --- Persistent Responses sockets -*- lexical-binding: t; -*-

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

;; Connection ownership and lifecycle for asynchronous Responses requests.

;;; Code:

(require 'cl-lib)
(require 'ende-websocket)
(require 'ende-curl)
(require 'ende-request)
(require 'ende-provider-config)

(defcustom ende-responses-ws-max-retries 3
  "Maximum automatic retries after an interrupted Responses attempt."
  :type 'natnum
  :group 'ende)

(cl-defstruct ende-responses-ws-connection
  owner identity socket active chain closed closing key url headers properties
  header-limit output-limit
  (generation 1) (phase 'disconnected) created close-code)

(cl-defstruct ende-responses-ws-chain
  response-id logical-request output)

(cl-defstruct ende-responses-ws-request
  connection lifecycle logical-request on-json on-complete on-prepared on-retry
  connect-timer deadline-timer idle-timer retry-timer deadline
  (retries 0) retry-limit
  payload (delivery 'unsent) (attempt 0) prepared-generation
  profile debug connect-timeout idle-timeout byte-limit event-limit
  reused last-event response-id items-by-index item-indices
  (received-bytes 0) (messages 0))

(defvar ende-responses-ws--connections (make-hash-table :test #'equal)
  "Private live connections, including ownerless ephemeral requests.")

(defvar ende-responses-ws--closing nil
  "Non-nil while shutdown prohibits allocation of replacement connections.")

(defun ende-responses-ws--eligible-p (connection)
  "Return whether CONNECTION is available for submission or reuse."
  (and connection
       (not (ende-responses-ws-connection-closed connection))
       (not (ende-responses-ws-connection-closing connection))
       (let ((socket (ende-responses-ws-connection-socket connection)))
         (or (not socket)
             (not (memq (ende-websocket-state socket) '(closing closed)))))))

(defun ende-responses-ws--live-p (request)
  "Return non-nil while REQUEST is attached and unsettled."
  (let ((connection (ende-responses-ws-request-connection request))
        (lifecycle (ende-responses-ws-request-lifecycle request)))
    (and connection lifecycle
         (not (ende-responses-ws-connection-closed connection))
         (eq request (ende-responses-ws-connection-active connection))
         (not (ende-transport-session-terminal lifecycle)))))

(defun ende-responses-ws--clear-chain (connection)
  "Detach CONNECTION's lineage and release its retained logical request."
  (when-let* ((chain (ende-responses-ws-connection-chain connection)))
    (setf (ende-responses-ws-connection-chain connection) nil)
    (ende-request-retained-release (ende-responses-ws-chain-logical-request chain) chain)))

(defun ende-responses-ws--drop-socket (connection)
  "Invalidate CONNECTION's socket and lineage before immediate abort."
  (let ((socket (ende-responses-ws-connection-socket connection)))
    (cl-incf (ende-responses-ws-connection-generation connection))
    (ende-responses-ws--clear-chain connection)
    (setf (ende-responses-ws-connection-socket connection) nil
          (ende-responses-ws-connection-phase connection) 'disconnected
          (ende-responses-ws-connection-closing connection) nil
          (ende-responses-ws-connection-created connection) nil
          (ende-responses-ws-connection-close-code connection) nil)
    ;; Cancellation and replacement must not drain an accepted request.
    (when socket (ende-websocket-abort socket))))

(defun ende-responses-ws--discard (connection)
  "Close CONNECTION and forget its private identity and lineage."
  (unless (ende-responses-ws-connection-closed connection)
    (setf (ende-responses-ws-connection-closed connection) t)
    (when (eq connection (gethash (ende-responses-ws-connection-key connection)
                                  ende-responses-ws--connections))
      (remhash (ende-responses-ws-connection-key connection) ende-responses-ws--connections))
    (setf (ende-responses-ws-connection-active connection) nil)
    (ende-responses-ws--drop-socket connection)
    (setf (ende-responses-ws-connection-phase connection) 'closed
          (ende-responses-ws-connection-headers connection) nil
          (ende-responses-ws-connection-identity connection) nil)))

(defun ende-responses-ws-finish (request outcome &optional response-id output)
  "Detach REQUEST before completion callbacks; retain a successful job socket.
OUTCOME is the transport-independent settlement outcome.  RESPONSE-ID and
replay-normalized OUTPUT commit a successful continuation before callbacks."
  (let ((connection (ende-responses-ws-request-connection request)))
    (when (and connection (eq request (ende-responses-ws-connection-active connection)))
      (ende-responses-ws--clear-chain connection)
      (when (and (eq outcome 'success) (ende-responses-ws-connection-owner connection)
                 (ende-responses-ws--eligible-p connection)
                 (stringp response-id) (not (string-empty-p response-id)))
        (let* ((retained (ende-responses-ws-request-logical-request request))
               (chain (make-ende-responses-ws-chain
                       :response-id response-id :logical-request retained
                       :output (ende-protocol-json-read-string (json-encode (vconcat output))))))
          (setf (ende-request-retained-owner retained) chain
                (ende-responses-ws-connection-chain connection) chain)))
      (setf (ende-responses-ws-connection-active connection) nil)
      (unless (and (eq outcome 'success) (ende-responses-ws-connection-owner connection)
                   (ende-responses-ws--eligible-p connection))
        (ende-responses-ws--discard connection)))
    (dolist (timer (list (ende-responses-ws-request-connect-timer request)
                         (ende-responses-ws-request-deadline-timer request)
                         (ende-responses-ws-request-idle-timer request)
                         (ende-responses-ws-request-retry-timer request)))
      (when timer (cancel-timer timer)))
    (setf (ende-responses-ws-request-connect-timer request) nil
          (ende-responses-ws-request-deadline-timer request) nil
          (ende-responses-ws-request-idle-timer request) nil
          (ende-responses-ws-request-retry-timer request) nil
          (ende-responses-ws-request-logical-request request) nil
          (ende-responses-ws-request-payload request) nil
          (ende-responses-ws-request-on-json request) nil
          (ende-responses-ws-request-on-complete request) nil
          (ende-responses-ws-request-on-prepared request) nil
          (ende-responses-ws-request-on-retry request) nil
          (ende-responses-ws-request-profile request) nil
          (ende-responses-ws-request-debug request) nil
          (ende-responses-ws-request-response-id request) nil
          (ende-responses-ws-request-items-by-index request) nil
          (ende-responses-ws-request-item-indices request) nil
          (ende-responses-ws-request-lifecycle request) nil
          (ende-responses-ws-request-connection request) nil)))

(defun ende-responses-ws--details (request)
  "Return allowlisted diagnostics for REQUEST, never dependency error data."
  (let* ((connection (ende-responses-ws-request-connection request))
         (phase (ende-responses-ws-connection-phase connection)))
    (list :transport-phase (if (eq phase 'connecting) phase
                             (pcase (ende-responses-ws-request-delivery request)
                               ('sending 'sending) ('accepted 'generation) (_ 'preparing)))
          :delivery (ende-responses-ws-request-delivery request)
          :reused (and (ende-responses-ws-request-reused request) t)
          :connection-age (when-let* ((created (ende-responses-ws-connection-created connection)))
                            (max 0 (- (float-time) created)))
          :close-code (ende-responses-ws-connection-close-code connection)
          :messages (ende-responses-ws-request-messages request)
          :retries (ende-responses-ws-request-retries request)
          :bytes (ende-responses-ws-request-received-bytes request)
          :last-event (ende-responses-ws-request-last-event request))))

(defun ende-responses-ws--fail (request kind message)
  "Fail live REQUEST once with safe KIND and MESSAGE."
  (when (ende-responses-ws--live-p request)
    (funcall (ende-transport-session-callback-error-handler
              (ende-responses-ws-request-lifecycle request))
             (ende-error-create :kind kind :message message
                                :details (ende-responses-ws--details request)))))

(defun ende-responses-ws-close-owner (owner)
  "Cancel and close the connection belonging to OWNER, if any."
  (when-let* ((connection (gethash owner ende-responses-ws--connections)))
    (when-let* ((request (ende-responses-ws-connection-active connection)))
      (ende-responses-ws--fail request 'cancelled "Responses connection cancelled"))
    (ende-responses-ws--discard connection)))

(defun ende-responses-ws-close-all ()
  "Idempotently cancel active requests and close all active and idle sockets."
  (let ((ende-responses-ws--closing t) keys)
    (maphash (lambda (key _connection) (push key keys)) ende-responses-ws--connections)
    (dolist (key keys) (ende-responses-ws-close-owner key))))

(cl-defun ende-responses-ws-create-request
    (profile headers properties lifecycle on-json on-complete on-prepared
             &optional authentication-identity on-retry)
  "Allocate a request on PROFILE's job-owned connection.
HEADERS are the effective validated handshake headers; PROPERTIES describes
the endpoint's WebSocket protocol.  LIFECYCLE owns per-request cleanup.
ON-JSON interprets provider events, ON-COMPLETE validates a terminal event,
and ON-PREPARED accepts the actual wire body and its debug capture.
AUTHENTICATION-IDENTITY privately identifies an OAuth registration.
ON-RETRY resets protocol state and supplies fresh validated handshake headers."
  (when (ende-transport-session-terminal lifecycle)
    (cl-return-from ende-responses-ws-create-request nil))
  (when ende-responses-ws--closing (error "Responses connections are shutting down"))
  (unless (natnump ende-responses-ws-max-retries)
    (error "ende-responses-ws-max-retries must be a nonnegative integer"))
  (ende-provider-config-validate-transport profile)
  (let* ((url (ende-curl--validated-url profile))
         (owner (plist-get profile :job-id))
         (key (or owner (make-symbol "ende-responses-ephemeral")))
         (identity
          (list (or (plist-get profile :request-phase) 'work) url
                (plist-get profile :service) (plist-get profile :auth-method)
                (plist-get profile :account) (plist-get profile :codex-binding)
                (plist-get profile :credential-binding) authentication-identity
                ende-max-sse-event-bytes ende-max-response-bytes ende-max-response-header-bytes))
         (connection (gethash key ende-responses-ws--connections)))
    (unless (string-prefix-p "https://" url)
      (error "Responses WebSockets require a trusted HTTPS endpoint"))
    (dolist (header headers)
      (unless (ende-curl--valid-header-p header) (error "Invalid WebSocket header")))
    (when (and connection (ende-responses-ws-connection-active connection))
      (error "A Responses connection already has an active request"))
    (when (and connection
               (or (not (ende-responses-ws--eligible-p connection))
                   (not (equal identity (ende-responses-ws-connection-identity connection)))
                   (not (equal headers (ende-responses-ws-connection-headers connection)))))
      (ende-responses-ws--discard connection)
      (setq connection nil))
    (unless connection
      (setq connection (make-ende-responses-ws-connection
                        :owner owner :key key :identity identity
                        :url (concat "wss://" (substring url 8))
                        :headers (mapcar #'copy-sequence headers) :properties properties))
      (puthash key connection ende-responses-ws--connections))
    (let ((request (make-ende-responses-ws-request
                    :connection connection :lifecycle lifecycle :profile profile
                    :reused (eq (ende-responses-ws-connection-phase connection) 'open)
                    :on-json on-json :on-complete on-complete :on-prepared on-prepared
                    :on-retry on-retry :retry-limit ende-responses-ws-max-retries
                    :connect-timeout ende-connect-timeout
                    :idle-timeout ende-response-idle-timeout
                    :byte-limit ende-max-response-bytes
                    :event-limit ende-max-sse-event-bytes))
          (timeout (if (plist-member profile :request-timeout)
                       (plist-get profile :request-timeout) ende-request-timeout)))
      (setf (ende-responses-ws-connection-active connection) request
            (ende-responses-ws-connection-header-limit connection) ende-max-response-header-bytes)
      (ende-transport-session-add-canceller
       lifecycle (lambda () (ende-responses-ws-finish request 'failure)))
      (when (and timeout (ende-responses-ws--live-p request))
        (setf (ende-responses-ws-request-deadline request) (+ (float-time) timeout)
              (ende-responses-ws-request-deadline-timer request)
              (run-at-time timeout nil #'ende-responses-ws--fail
                           request 'timeout "Responses request deadline exceeded")))
      (when (ende-responses-ws--live-p request) request))))

(defun ende-responses-ws--settings (body)
  "Copy parsed logical BODY's settings, excluding transport-owned fields."
  (let ((settings (copy-hash-table body)))
    (dolist (key '("type" "input" "previous_response_id" "stream" "background" "stream_options"))
      (remhash key settings))
    settings))

(defun ende-responses-ws-finalize (body properties &optional snapshot)
  "Select input from immutable SNAPSHOT and frame BODY using PROPERTIES."
  (let ((body (cl-remove-if
               (lambda (entry)
                 (member (ende-controls--json-key-name (car entry))
                         '("type" "previous_response_id" "stream" "background" "stream_options"))) body)))
    (let* ((full (ende-protocol-json-read-string (json-encode body)))
           (input (gethash "input" full))
           (previous (when snapshot
                       (ende-protocol-json-read-string
                        (json-encode (ende-request--retained-body (plist-get snapshot :source) nil)))))
           (expected (and previous (gethash "input" previous)))
           (output (plist-get snapshot :output))
           (prefix (+ (length expected) (length output)))
           (id (plist-get snapshot :response-id))
           (continue
            (and (stringp id) (not (string-empty-p id))
                 (ende-protocol-json-equal-p (ende-responses-ws--settings full)
                                            (ende-responses-ws--settings previous))
                 (<= prefix (length input))
                 (cl-loop for index below (length expected)
                          always (ende-protocol-json-equal-p (aref expected index) (aref input index)))
                 (cl-loop for index below (length output)
                          always (ende-protocol-json-equal-p
                                  (aref output index) (aref input (+ (length expected) index)))))))
      (setq body (cl-remove-if (lambda (entry) (equal (ende-controls--json-key-name (car entry)) "input")) body))
      (append (list (cons :type "response.create")) body
              (when continue (list (cons :previous_response_id id)))
              (list (cons :input (ende-protocol-json-for-request
                                 (if continue (substring input prefix) (vconcat input)))))
              (when (plist-get properties :stream) (list (cons :stream t)))))))

(defun ende-responses-ws-finalization (request)
  "Return serializable transport finalization arguments for REQUEST."
  (let* ((connection (ende-responses-ws-request-connection request))
         (chain (and (ende-responses-ws--eligible-p connection)
                     (ende-responses-ws-connection-chain connection))))
    (list #'ende-responses-ws-finalize
          (ende-responses-ws-connection-properties connection)
          (when chain
            (list :response-id (ende-responses-ws-chain-response-id chain)
                  :source (ende-request-retained-source (ende-responses-ws-chain-logical-request chain))
                  :output (ende-responses-ws-chain-output chain))))))

(defun ende-responses-ws-prepare (request builder messages)
  "Prepare REQUEST's MESSAGES, protecting any retained continuation baseline."
  (when (ende-responses-ws--live-p request)
    (let* ((connection (ende-responses-ws-request-connection request))
           (attempt (ende-responses-ws-request-attempt request))
           (chain (ende-responses-ws-connection-chain connection))
           (baseline (and chain (ende-responses-ws-chain-logical-request chain))))
      (ende-request-prepare
       (ende-responses-ws-request-lifecycle request) 'ende-codex builder messages
       (ende-responses-ws-request-profile request) (ende-responses-ws-request-debug request)
       (apply-partially #'ende-responses-ws--initial-prepared request
                        attempt
                        (ende-responses-ws-connection-generation connection))
       (ende-responses-ws-finalization request)
       (and baseline (plist-get (ende-request-retained-source baseline) :full-file))
       nil baseline (apply-partially #'ende-responses-ws--live-p request)))))

(defun ende-responses-ws--initial-prepared (request attempt generation data body record)
  "Publish immutable DATA even if initial wire preparation was superseded."
  (if (ende-responses-ws--preparation-live-p request attempt)
      (ende-responses-ws-send-prepared request attempt generation data body record)
    (ende-request-retained-release (plist-get data :wire-source)
                                   (ende-responses-ws-request-lifecycle request))
    (ende-ignore-errors #'ende-debug-record-completion record 'discarded nil nil nil)
    (when (ende-responses-ws--live-p request)
      (setf (ende-responses-ws-request-logical-request request) (plist-get data :retained))
      (when (eq (ende-responses-ws-connection-phase
                 (ende-responses-ws-request-connection request)) 'preparing)
        (ende-responses-ws--refinalize request)))))

(defun ende-responses-ws--preparation-live-p (request attempt)
  "Return whether preparation still belongs to REQUEST's ATTEMPT."
  (and (ende-responses-ws--live-p request)
       (= attempt (ende-responses-ws-request-attempt request))))

(defun ende-responses-ws--idle (request)
  "Reset REQUEST's captured response-idle deadline."
  (when-let* ((timer (ende-responses-ws-request-idle-timer request)))
    (cancel-timer timer))
  (setf (ende-responses-ws-request-idle-timer request)
        (run-at-time (ende-responses-ws-request-idle-timeout request) nil
                     #'ende-responses-ws--attempt-timeout request
                     (ende-responses-ws-request-attempt request) 'idle)))

(defun ende-responses-ws--attempt-timeout (request attempt kind)
  "Handle KIND's physical deadline only for REQUEST's current ATTEMPT."
  (when (ende-responses-ws--preparation-live-p request attempt)
    (if (eq kind 'connect)
        (when (eq (ende-responses-ws-connection-phase
                   (ende-responses-ws-request-connection request)) 'connecting)
          (ende-responses-ws--retry request 'connect-timeout))
      (ende-responses-ws--fail request 'timeout "Responses response-idle deadline exceeded"))))

(defun ende-responses-ws--attempt-live-p (request attempt socket)
  "Return whether ATTEMPT still owns REQUEST's accepted SOCKET."
  (and (ende-responses-ws--live-p request)
       (= attempt (ende-responses-ws-request-attempt request))
       (let ((connection (ende-responses-ws-request-connection request)))
         (and (eq socket (ende-responses-ws-connection-socket connection))
              (eq (ende-responses-ws-connection-phase connection) 'open)
              (eq (ende-websocket-state socket) 'open)
              (= (ende-responses-ws-request-prepared-generation request)
                 (ende-responses-ws-connection-generation connection))))))

(defun ende-responses-ws--send (request)
  "Offer REQUEST once; queue acceptance is an ambiguous delivery boundary."
  (when (and (ende-responses-ws--live-p request)
             (eq (ende-responses-ws-request-delivery request) 'unsent)
             (ende-responses-ws-request-payload request))
    (let* ((attempt (ende-responses-ws-request-attempt request))
           (connection (ende-responses-ws-request-connection request))
           (socket (ende-responses-ws-connection-socket connection)))
      (condition-case nil
          (let* ((data (ende-responses-ws-request-payload request))
                 (file (plist-get data :file))
                 (size (ende-responses-ws--payload-size data)))
            (when (ende-responses-ws--attempt-live-p request attempt socket)
              ;; A successful offer does not prove transmission.  Never replay
              ;; after this boundary without an explicit provider rejection.
              (if (> size (ende-responses-ws-connection-output-limit connection))
                  ;; A larger follow-up needs a fresh bounded transport and
                  ;; full preparation, without a retired previous_response_id.
                  (ende-responses-ws--retry request 'unsent-replacement)
                (setf (ende-responses-ws-request-delivery request) 'sending)
                (ende-websocket-set-receive-budget
                 socket (when-let* ((limit (ende-responses-ws-request-byte-limit request)))
                          (max 0 (- limit (ende-responses-ws-request-received-bytes request)))))
                (if (not file) (ende-websocket-send-text-bytes socket (plist-get data :data))
                  (let* ((source (plist-get data :wire-source))
                         (release (ende-request-retained-read source)))
                    (unwind-protect
                        (progn
                          (ende-websocket-send-text-file socket file release)
                          (setq release nil)
                          (ende-request-retained-release source (ende-responses-ws-request-lifecycle request)))
                      (when release (funcall release)))))
                (when (ende-responses-ws--attempt-live-p request attempt socket)
                  (setf (ende-responses-ws-request-payload request) nil
                        (ende-responses-ws-request-delivery request) 'accepted)
                  (ende-responses-ws--idle request)))))
        (ende-websocket-backpressure
         ;; Rejected atomically: retain the offer until ON-WRITABLE.
         (when (ende-responses-ws--preparation-live-p request attempt)
           (setf (ende-responses-ws-request-delivery request) 'unsent)))
        ((error quit)
         (when (ende-responses-ws--preparation-live-p request attempt)
           (ende-responses-ws--fail request 'transport "Responses request submission failed")))))))

(defun ende-responses-ws--payload-size (data)
  "Return prepared DATA's byte count without reading its payload."
  (or (plist-get data :bytes)
      (if-let* ((file (plist-get data :file)))
          (file-attribute-size (file-attributes file))
        (length (plist-get data :data)))))

(defun ende-responses-ws--release-payload (request)
  "Release REQUEST's unaccepted wire source before retiring an attempt."
  (ende-request-retained-release
   (plist-get (ende-responses-ws-request-payload request) :wire-source)
   (ende-responses-ws-request-lifecycle request)))

(defun ende-responses-ws--refinalize (request)
  "Prepare a fresh wire attempt from REQUEST's unchanged logical source."
  (let ((generation (ende-responses-ws-connection-generation
                     (ende-responses-ws-request-connection request)))
        (attempt (ende-responses-ws-request-attempt request)))
    (condition-case nil
        (ende-request-reprepare
         (ende-responses-ws-request-lifecycle request) 'ende-responses-ws
         (ende-responses-ws-request-logical-request request)
         (ende-responses-ws-request-profile request) (ende-responses-ws-request-debug request)
         (apply-partially #'ende-responses-ws-send-prepared request attempt generation)
         (ende-responses-ws-finalization request)
         (apply-partially #'ende-responses-ws--preparation-live-p request attempt))
      ((error quit)
       (when (and (ende-responses-ws--live-p request)
                  (= attempt (ende-responses-ws-request-attempt request)))
         (ende-responses-ws--fail request 'transport "Responses request preparation failed"))))))

(defun ende-responses-ws--recover (request json)
  "Route explicitly recoverable JSON errors through REQUEST's shared budget."
  (let* ((type (ende-protocol-json-get "type" json))
         (error (or (ende-protocol-json-get "error" json)
                    (ende-protocol-json-get "error" (ende-protocol-json-get "response" json))))
         (code (ende-protocol-json-get "code" error)))
    (when (and (member type '("error" "response.failed"))
               (member code '("previous_response_not_found" "websocket_connection_limit_reached")))
      (ende-responses-ws--retry request (if (equal code "previous_response_not_found")
                                          'missing-response 'connection-limit))
      t)))

(defun ende-responses-ws--deadline-expired-p (request)
  "Return non-nil after REQUEST's original logical deadline."
  (when-let* ((deadline (ende-responses-ws-request-deadline request)))
    (>= (float-time) deadline)))

(defun ende-responses-ws--retry-delay (retry)
  "Return bounded jittered exponential backoff for RETRY, starting at one."
  (min 4 (* (expt 2 (min 2 (1- retry))) (+ 0.8 (/ (random 401) 1000.0)))))

(defun ende-responses-ws--retry (request reason)
  "Retire REQUEST's attempt and recover from structured transient REASON."
  (when (ende-responses-ws--live-p request)
    (cond
     ((ende-responses-ws--deadline-expired-p request)
      (ende-responses-ws--fail request 'timeout "Responses request deadline exceeded"))
     ((>= (ende-responses-ws-request-retries request)
          (ende-responses-ws-request-retry-limit request))
      (ende-responses-ws--fail
       request 'transport
       "Responses automatic reconnection exhausted; completed work was preserved"))
     ((memq reason '(connect-timeout missing-response connection-limit unsent-replacement))
      (let ((connection (ende-responses-ws-request-connection request)))
        (ende-responses-ws--release-payload request)
        (cl-incf (ende-responses-ws-request-retries request))
        (cl-incf (ende-responses-ws-request-attempt request))
        (dolist (timer (list (ende-responses-ws-request-connect-timer request)
                            (ende-responses-ws-request-idle-timer request)))
          (when timer (cancel-timer timer)))
        (setf (ende-responses-ws-request-connect-timer request) nil
              (ende-responses-ws-request-idle-timer request) nil
              (ende-responses-ws-request-payload request) nil
              (ende-responses-ws-request-delivery request) 'unsent
              (ende-responses-ws-request-response-id request) nil
              (ende-responses-ws-request-items-by-index request) nil
              (ende-responses-ws-request-item-indices request) nil
              (ende-responses-ws-request-prepared-generation request) nil)
        (ende-responses-ws--drop-socket connection)
        (setf (ende-responses-ws-connection-phase connection) 'authenticating)
        (let ((attempt (ende-responses-ws-request-attempt request)))
          (condition-case nil
              (funcall (ende-responses-ws-request-on-retry request)
                       attempt (ende-responses-ws-request-retry-limit request)
                       (apply-partially #'ende-responses-ws--retry-ready request attempt))
            ((error quit)
             (when (ende-responses-ws--preparation-live-p request attempt)
               (ende-responses-ws--fail request 'callback "Responses retry callback failed")))))))
     (t (ende-responses-ws--fail request 'transport "Responses connection failed")))))

(defun ende-responses-ws--retry-ready (request attempt headers)
  "Accept validated HEADERS only for REQUEST's current authentication ATTEMPT."
  (when (and (ende-responses-ws--preparation-live-p request attempt)
             (eq (ende-responses-ws-connection-phase
                  (ende-responses-ws-request-connection request)) 'authenticating))
    (if (ende-responses-ws--deadline-expired-p request)
        (ende-responses-ws--fail request 'timeout "Responses request deadline exceeded")
      (let ((connection (ende-responses-ws-request-connection request)))
        (dolist (header headers)
          (unless (ende-curl--valid-header-p header) (error "Invalid WebSocket header")))
        (setf (ende-responses-ws-connection-headers connection) (mapcar #'copy-sequence headers)
              (ende-responses-ws-connection-phase connection) 'backoff
              (ende-responses-ws-request-retry-timer request)
              (run-at-time (ende-responses-ws--retry-delay (ende-responses-ws-request-retries request)) nil
                           #'ende-responses-ws--begin-attempt request attempt))))))

(defun ende-responses-ws--begin-attempt (request attempt)
  "Prepare full retained input for REQUEST after ATTEMPT's backoff."
  (when (and (ende-responses-ws--preparation-live-p request attempt)
             (eq (ende-responses-ws-connection-phase
                  (ende-responses-ws-request-connection request)) 'backoff))
    (setf (ende-responses-ws-request-retry-timer request) nil)
    (if (ende-responses-ws--deadline-expired-p request)
        (ende-responses-ws--fail request 'timeout "Responses request deadline exceeded")
      (setf (ende-responses-ws-connection-phase
             (ende-responses-ws-request-connection request)) 'preparing)
      (when (ende-responses-ws-request-logical-request request)
        (ende-responses-ws--refinalize request)))))

(defun ende-responses-ws--open (connection socket)
  "Send the prepared request when CONNECTION's current SOCKET opens."
  (when (and (ende-responses-ws--eligible-p connection)
             (eq socket (ende-responses-ws-connection-socket connection)))
    (setf (ende-responses-ws-connection-phase connection) 'open)
    (when-let* ((request (ende-responses-ws-connection-active connection)))
      (when-let* ((timer (ende-responses-ws-request-connect-timer request)))
        (cancel-timer timer)
        (setf (ende-responses-ws-request-connect-timer request) nil))
      (ende-responses-ws--send request))))

(defun ende-responses-ws--validate-item (request index id &optional introduce)
  "Validate REQUEST's association of INDEX with ID, optionally INTRODUCE it."
  (unless (and (natnump index) (stringp id) (not (string-empty-p id)))
    (error "Missing Responses item identity"))
  (let* ((items (ende-responses-ws-request-items-by-index request))
         (indices (ende-responses-ws-request-item-indices request))
         (known-id (gethash index items))
         (known-index (gethash id indices)))
    (unless (if introduce
                (and (or (null known-id) (equal known-id id))
                     (or (null known-index) (eql known-index index)))
              (and (equal known-id id) (eql known-index index)))
      (error "Responses item does not belong to this output index"))
    (when introduce
      (puthash index id items)
      (puthash id index indices))))

(defun ende-responses-ws--validate-event (request json)
  "Validate JSON's response and item ownership for REQUEST's current attempt."
  (let* ((type (ende-protocol-json-get "type" json))
         (response (ende-protocol-json-get "response" json))
         (id (ende-protocol-json-get "id" response))
         (current (ende-responses-ws-request-response-id request))
         (terminal (member type '("response.completed" "response.done"
                                  "response.incomplete" "response.failed"))))
    (if (equal type "response.created")
        (progn
          (unless (and (not current) (stringp id) (not (string-empty-p id)))
            (error "Invalid Responses creation event"))
          (setq current id)
          (setf (ende-responses-ws-request-response-id request) id
                (ende-responses-ws-request-items-by-index request) (make-hash-table :test #'eql)
                (ende-responses-ws-request-item-indices request) (make-hash-table :test #'equal)))
      (when (and (string-prefix-p "response." type) (not current))
        (error "Responses event precedes creation")))
    (when (or terminal (member type '("response.created" "response.in_progress"))
              (ende-protocol-json-has-key-p "response" json))
      (unless (and current (equal id current))
        (error "Responses snapshot belongs to another response")))
    (when (ende-protocol-json-has-key-p "response_id" json)
      (unless (and current (equal (ende-protocol-json-get "response_id" json) current))
        (error "Responses event belongs to another response")))
    (let ((index (ende-protocol-json-get "output_index" json))
          (item-id (ende-protocol-json-get "item_id" json))
          (indexed (member type '("response.output_text.delta" "response.output_text.done"
                                  "response.reasoning_summary_text.delta" "response.reasoning_summary_text.done"
                                  "response.function_call_arguments.delta" "response.function_call_arguments.done"))))
      (when (member type '("response.output_item.added" "response.output_item.done"))
        (let ((id (ende-protocol-json-get "id" (ende-protocol-json-get "item" json)))
              (added (equal type "response.output_item.added")))
          (when (and added (gethash index (ende-responses-ws-request-items-by-index request)))
            (error "Duplicate Responses output item"))
          (ende-responses-ws--validate-item request index id added)))
      (when (or indexed (ende-protocol-json-has-key-p "item_id" json)
                (ende-protocol-json-has-key-p "output_index" json))
        (ende-responses-ws--validate-item
         request
         (if (or indexed (ende-protocol-json-has-key-p "output_index" json)) index
           (gethash item-id (ende-responses-ws-request-item-indices request)))
         (if (or indexed (ende-protocol-json-has-key-p "item_id" json)) item-id
           (gethash index (ende-responses-ws-request-items-by-index request))))))
    (when (and terminal (ende-protocol-json-has-key-p "output" response))
      (let ((output (ende-protocol-json-get "output" response)))
        (unless (vectorp output) (error "Invalid Responses output snapshot"))
        (cl-loop for item across output for index from 0 do
                 (when (and (ende-protocol-json-has-key-p "output_index" item)
                            (not (eql (ende-protocol-json-get "output_index" item) index)))
                   (error "Responses snapshot reorders output items"))
                 (ende-responses-ws--validate-item
                  request index (ende-protocol-json-get "id" item) t))
        (unless (= (length output) (hash-table-count (ende-responses-ws-request-items-by-index request)))
          (error "Responses snapshot omits known output items"))))))

(defun ende-responses-ws--message (connection socket message-type text)
  "Consume a complete decoded event from CONNECTION's current SOCKET."
  (when (and (ende-responses-ws--eligible-p connection)
             (eq socket (ende-responses-ws-connection-socket connection)))
    (when-let* ((request (ende-responses-ws-connection-active connection)))
      (when (and (ende-responses-ws--live-p request)
                 (memq (ende-responses-ws-request-delivery request) '(sending accepted)))
        (let ((attempt (ende-responses-ws-request-attempt request))
              (bytes (string-bytes text)))
          (condition-case nil
              (progn
                (unless (eq message-type 'text) (error "Expected text event"))
                ;; Transport framing/UTF-8 and partial-message bounds belong to
                ;; ende-websocket.  These counters describe application input.
                (cl-incf (ende-responses-ws-request-received-bytes request) bytes)
                (if (or (and (ende-responses-ws-request-byte-limit request)
                             (> (ende-responses-ws-request-received-bytes request)
                                (ende-responses-ws-request-byte-limit request)))
                        (and (ende-responses-ws-request-event-limit request)
                             (> bytes (ende-responses-ws-request-event-limit request))))
                    (ende-responses-ws--fail request 'resource "Responses input exceeds byte limit")
                  (ende-responses-ws--idle request)
                  (let* ((json (ende-protocol-json-read-string text))
                         (type (ende-protocol-json-get "type" json)))
                    (cl-incf (ende-responses-ws-request-messages request))
                    (unless (stringp type) (error "Missing Responses event type"))
                    (when (memq (intern-soft type)
                                '(response.created response.in_progress response.completed response.done
                                  response.incomplete response.failed error response.output_text.delta
                                  response.output_item.added response.output_item.done
                                  response.reasoning_summary_text.delta response.function_call_arguments.delta
                                  response.function_call_arguments.done))
                      (setf (ende-responses-ws-request-last-event request) (intern-soft type)))
                    (ende-responses-ws--validate-event request json)
                    (unless (ende-responses-ws--recover request json)
                      (funcall (ende-responses-ws-request-on-json request) json)
                      (when (and (ende-responses-ws--attempt-live-p request attempt socket)
                                 (member type '("response.completed" "response.done" "response.incomplete")))
                        (funcall (ende-responses-ws-request-on-complete request)))))))
            ((error quit)
             (when (ende-responses-ws--preparation-live-p request attempt)
               (ende-responses-ws--fail request 'protocol "Invalid Responses WebSocket event")))))))))

(defun ende-responses-ws--closed (connection socket info)
  "Retire SOCKET using only public, structured close INFO."
  (when (and (not (ende-responses-ws-connection-closed connection))
             (eq socket (ende-responses-ws-connection-socket connection)))
    (setf (ende-responses-ws-connection-closing connection) t
          (ende-responses-ws-connection-close-code connection) (plist-get info :code))
    (let ((request (ende-responses-ws-connection-active connection))
          (cause (plist-get info :cause))
          (code (plist-get info :code)))
      (cond
       ((not request) (ende-responses-ws--discard connection))
       ((eq cause 'size)
        (ende-responses-ws--fail request 'resource "Responses transport limit exceeded"))
       ((memq cause '(protocol utf8 application))
        (ende-responses-ws--fail request 'protocol "Invalid Responses WebSocket event"))
       ((not (eq (ende-responses-ws-request-delivery request) 'unsent))
        (ende-responses-ws--fail
         request 'transport "Responses delivery interrupted; request was not replayed"))
       ((or (null code) (memq code '(1000 1001 1006 1011 1012 1013 1014)))
        (ende-responses-ws--retry request 'unsent-replacement))
       (t (ende-responses-ws--fail request 'transport "Responses connection rejected"))))))

(defun ende-responses-ws--connect (connection)
  "Start CONNECTION through the public asynchronous WebSocket API."
  (let* ((request (ende-responses-ws-connection-active connection))
         (data (ende-responses-ws-request-payload request))
         (size (ende-responses-ws--payload-size data))
         (message-limit (min (or (ende-responses-ws-request-event-limit request) most-positive-fixnum)
                             (or (ende-responses-ws-request-byte-limit request) most-positive-fixnum)))
         (output-limit (max 1 size message-limit))
         (header-limit (or (ende-responses-ws-connection-header-limit connection)
                           most-positive-fixnum))
         (limits (list :message-bytes message-limit
                       :frame-bytes message-limit
                       :write-frame-bytes (min 16384 output-limit)
                       :write-message-bytes output-limit
                       :output-bytes output-limit
                       :input-bytes (max 14 (+ 14 message-limit) (or header-limit 0))
                       :handshake-timeout (if (ende-responses-ws-request-connect-timeout request)
                                              (max 1 (ceiling (ende-responses-ws-request-connect-timeout request)))
                                            most-positive-fixnum)
                       :message-timeout (max 1 (ceiling (ende-responses-ws-request-idle-timeout request))))))
    (setq limits (append limits (list :header-bytes header-limit :line-bytes header-limit)))
    (setf (ende-responses-ws-connection-output-limit connection) output-limit
          (ende-responses-ws-connection-phase connection) 'connecting
          (ende-responses-ws-connection-created connection) (float-time))
    (when-let* ((timeout (ende-responses-ws-request-connect-timeout request)))
      (setf (ende-responses-ws-request-connect-timer request)
            (run-at-time timeout nil #'ende-responses-ws--attempt-timeout request
                         (ende-responses-ws-request-attempt request) 'connect)))
    (let ((default-directory (ende-curl-local-directory)))
      (setf (ende-responses-ws-connection-socket connection)
            (ende-websocket-connect
             (ende-responses-ws-connection-url connection)
             :headers (mapcar (lambda (header)
                                (string-match ": *" header)
                                (cons (substring header 0 (match-beginning 0))
                                      (substring header (match-end 0))))
                              (ende-responses-ws-connection-headers connection))
             :limits limits
             :on-open (apply-partially #'ende-responses-ws--open connection)
             :on-message (apply-partially #'ende-responses-ws--message connection)
             :on-close (apply-partially #'ende-responses-ws--closed connection)
             :on-writable (lambda (socket)
                            (when (eq socket (ende-responses-ws-connection-socket connection))
                              (when-let* ((active (ende-responses-ws-connection-active connection)))
                                (ende-responses-ws--send active)))))))))

(defun ende-responses-ws-send-prepared (request attempt generation data body record)
  "Open or reuse REQUEST's socket and send prepared DATA, BODY and RECORD."
  (if (not (ende-responses-ws--preparation-live-p request attempt))
      (progn
        (ende-request-retained-release (plist-get data :wire-source)
                                       (ende-responses-ws-request-lifecycle request))
        (ende-ignore-errors #'ende-debug-record-completion record 'discarded nil nil nil))
    (condition-case nil
        (let ((connection (ende-responses-ws-request-connection request)))
          (setf (ende-responses-ws-request-prepared-generation request) generation
                (ende-responses-ws-request-logical-request request) (plist-get data :retained)
                (ende-responses-ws-request-payload request) data)
          (funcall (ende-responses-ws-request-on-prepared request) body record)
          (when (ende-responses-ws--preparation-live-p request attempt)
            (if (/= generation (ende-responses-ws-connection-generation connection))
                (progn
                  (ende-ignore-errors #'ende-debug-record-completion record 'discarded nil nil nil)
                  (ende-responses-ws--retry request 'unsent-replacement))
              (ende-transport-session-start (ende-responses-ws-request-lifecycle request))
              (if (eq (ende-responses-ws-connection-phase connection) 'open)
                  (ende-responses-ws--send request)
                (unless (ende-responses-ws-connection-socket connection)
                  (ende-responses-ws--connect connection))))))
      (ende-websocket-backpressure
       (when (ende-responses-ws--preparation-live-p request attempt)
         (ende-responses-ws--retry request 'connection-limit)))
      ((error quit)
       (when (ende-responses-ws--preparation-live-p request attempt)
         (ende-responses-ws--fail request 'transport "Could not send Responses WebSocket request"))))))

(provide 'ende-responses-ws)
;;; ende-responses-ws.el ends here
