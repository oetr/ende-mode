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

;; Connection ownership and framing for asynchronous Responses requests.

;;; Code:

(require 'cl-lib)
(require 'websocket)
(require 'gnutls)
(require 'ende-curl)
(require 'ende-request)
(require 'ende-provider-config)

(defcustom ende-responses-ws-max-retries 3
  "Maximum automatic retries after an interrupted Responses attempt."
  :type 'natnum
  :group 'ende)

(cl-defstruct ende-responses-ws-connection
  owner identity socket active chain closed closing key url headers properties
  byte-limit event-limit header-limit
  opening (generation 1) (phase 'disconnected) created http-status close-code)

(cl-defstruct ende-responses-ws-opening
  generation key process upgrade-sent header)

(cl-defstruct ende-responses-ws-chain
  response-id logical-request output)

(cl-defstruct ende-responses-ws-request
  connection lifecycle logical-request on-json on-complete on-prepared on-retry
  connect-timer deadline-timer idle-timer retry-timer deadline
  (retries 0) retry-limit
  payload (delivery 'unsent) (attempt 0) prepared-generation
  profile debug connect-timeout idle-timeout byte-limit event-limit
  reused last-event
  fragment-active frame-parts (frame-bytes 0) (received-bytes 0) (messages 0))

(defvar ende-responses-ws--connections (make-hash-table :test #'equal)
  "Private live connections, including ownerless ephemeral requests.")

(defvar ende-responses-ws--closing nil
  "Non-nil while shutdown prohibits allocation of replacement connections.")

(defun ende-responses-ws--eligible-p (connection)
  "Return whether CONNECTION is available for submission or reuse."
  (and connection
       (not (ende-responses-ws-connection-closed connection))
       (not (ende-responses-ws-connection-closing connection))))

(defun ende-responses-ws--live-p (request)
  "Return non-nil while REQUEST is attached and unsettled."
  (let ((connection (ende-responses-ws-request-connection request))
        (lifecycle (ende-responses-ws-request-lifecycle request)))
    (and connection lifecycle
         (not (ende-responses-ws-connection-closed connection))
         (eq request (ende-responses-ws-connection-active connection))
         (not (ende-transport-session-terminal lifecycle)))))

(defmacro ende-responses-ws--private (&rest body)
  "Run BODY without dependency debugging or ambient cookies."
  (declare (indent 0) (debug t))
  `(let ((websocket-debug nil)
         (websocket-callback-debug-on-error nil)
         (url-cookie-storage nil)
         (url-cookie-secure-storage nil))
     ,@body))

(defun ende-responses-ws--delete-process (process)
  "Detach PROCESS before deletion, which may invoke its sentinel."
  (when (processp process)
    (set-process-filter process #'ignore)
    (set-process-sentinel process #'ignore)
    (process-put process :websocket nil)
    (ende-ignore-errors #'delete-process process)))

(defun ende-responses-ws--close-socket (socket)
  "Detach SOCKET before deleting its process, without writing a close frame."
  (ende-responses-ws--private
    (when socket
      (setf (websocket-ready-state socket) 'closed
            (websocket-on-open socket) #'ignore
            (websocket-on-message socket) #'ignore
            (websocket-on-close socket) #'ignore
            (websocket-on-error socket) #'ignore
            (websocket-client-data socket) nil
            (websocket-inflight-input socket) nil)
      (ende-responses-ws--delete-process (websocket-conn socket)))))

(defun ende-responses-ws--clear-chain (connection)
  "Detach CONNECTION's lineage and release its retained logical request."
  (when-let* ((chain (ende-responses-ws-connection-chain connection)))
    (setf (ende-responses-ws-connection-chain connection) nil)
    (ende-request-retained-release (ende-responses-ws-chain-logical-request chain) chain)))

(defun ende-responses-ws--drop-socket (connection)
  "Invalidate CONNECTION's socket and lineage before process deletion."
  (let ((socket (ende-responses-ws-connection-socket connection)))
    (cl-incf (ende-responses-ws-connection-generation connection))
    (ende-responses-ws--clear-chain connection)
    (setf (ende-responses-ws-connection-socket connection) nil
          (ende-responses-ws-connection-opening connection) nil
          (ende-responses-ws-connection-phase connection) 'disconnected
          (ende-responses-ws-connection-closing connection) nil
          (ende-responses-ws-connection-created connection) nil
          (ende-responses-ws-connection-http-status connection) nil
          (ende-responses-ws-connection-close-code connection) nil)
    (ende-responses-ws--close-socket socket)))

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
          (ende-responses-ws-request-frame-parts request) nil
          (ende-responses-ws-request-fragment-active request) nil
          (ende-responses-ws-request-frame-bytes request) 0
          (ende-responses-ws-request-payload request) nil
          (ende-responses-ws-request-on-json request) nil
          (ende-responses-ws-request-on-complete request) nil
          (ende-responses-ws-request-on-prepared request) nil
          (ende-responses-ws-request-on-retry request) nil
          (ende-responses-ws-request-profile request) nil
          (ende-responses-ws-request-debug request) nil
          (ende-responses-ws-request-lifecycle request) nil
          (ende-responses-ws-request-connection request) nil)))

(defun ende-responses-ws--details (request)
  "Return allowlisted diagnostics for REQUEST, never dependency error data."
  (let* ((connection (ende-responses-ws-request-connection request))
         (phase (ende-responses-ws-connection-phase connection)))
    (list :transport-phase (if (memq phase '(connecting upgrading)) phase
                             (pcase (ende-responses-ws-request-delivery request)
                               ('sending 'sending) ('sent 'generation) (_ 'preparing)))
          :delivery (ende-responses-ws-request-delivery request)
          :reused (and (ende-responses-ws-request-reused request) t)
          :connection-age (when-let* ((created (ende-responses-ws-connection-created connection)))
                            (max 0 (- (float-time) created)))
          :http-status (ende-responses-ws-connection-http-status connection)
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
                (let ((print-length nil) (print-level nil))
                  (secure-hash 'sha256 (prin1-to-string headers)))))
         (connection (gethash key ende-responses-ws--connections)))
    (unless (string-prefix-p "https://" url)
      (error "Responses WebSockets require a trusted HTTPS endpoint"))
    (dolist (header headers)
      (unless (ende-curl--valid-header-p header) (error "Invalid WebSocket header")))
    (when (and connection (ende-responses-ws-connection-active connection))
      (error "A Responses connection already has an active request"))
    (when (and connection
               (or (not (ende-responses-ws--eligible-p connection))
                   (not (equal identity (ende-responses-ws-connection-identity connection)))))
      (ende-responses-ws--discard connection)
      (setq connection nil))
    (unless connection
      (setq connection (make-ende-responses-ws-connection
                        :owner owner :key key :identity identity
                        :url (concat "wss://" (substring url 8))
                        :headers headers :properties properties))
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
            (ende-responses-ws-connection-byte-limit connection) ende-max-response-bytes
            (ende-responses-ws-connection-event-limit connection) ende-max-sse-event-bytes
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
        (when (memq (ende-responses-ws-connection-phase
                     (ende-responses-ws-request-connection request)) '(connecting upgrading))
          (ende-responses-ws--retry request 'connect-timeout))
      (ende-responses-ws--fail request 'timeout "Responses response-idle deadline exceeded"))))

(defun ende-responses-ws--attempt-live-p (request attempt socket)
  "Return whether ATTEMPT still owns REQUEST's accepted SOCKET."
  (and (ende-responses-ws--live-p request)
       (= attempt (ende-responses-ws-request-attempt request))
       (let ((connection (ende-responses-ws-request-connection request)))
         (and (eq socket (ende-responses-ws-connection-socket connection))
              (eq (ende-responses-ws-connection-phase connection) 'open)
              (eq (websocket-ready-state socket) 'open)
              (= (ende-responses-ws-request-prepared-generation request)
                 (ende-responses-ws-connection-generation connection))))))

(defun ende-responses-ws--send (request)
  "Materialize then submit REQUEST once; sending is an ambiguous boundary."
  (when (and (ende-responses-ws--live-p request)
             (eq (ende-responses-ws-request-delivery request) 'unsent)
             (ende-responses-ws-request-payload request))
    (let* ((attempt (ende-responses-ws-request-attempt request))
           (connection (ende-responses-ws-request-connection request))
           (socket (ende-responses-ws-connection-socket connection)))
      (ende-responses-ws--private
        (condition-case nil
            (let* ((data (ende-responses-ws-request-payload request))
                   (text (if-let* ((file (plist-get data :file)))
                             (with-temp-buffer
                               (let ((coding-system-for-read 'utf-8-unix)) (insert-file-contents file))
                               (buffer-string))
                           (decode-coding-string (plist-get data :data) 'utf-8))))
              (when (ende-responses-ws--attempt-live-p request attempt socket)
                (if (or (not (ende-responses-ws--eligible-p connection))
                        (not (eq (process-status (websocket-conn socket)) 'open)))
                    (ende-responses-ws--closed connection socket)
                  (setf (ende-responses-ws-request-payload request) nil
                        (ende-responses-ws-request-delivery request) 'sending)
                  (ende-responses-ws--idle request)
                  (process-send-string
                   (websocket-conn socket)
                   (websocket-encode-frame
                    (make-websocket-frame :opcode 'text :completep t
                                          :payload (encode-coding-string text 'utf-8)) t))
                  (when (ende-responses-ws--attempt-live-p request attempt socket)
                    (setf (ende-responses-ws-request-delivery request) 'sent)))))
          ((error quit)
           (when (and (ende-responses-ws--live-p request)
                      (= attempt (ende-responses-ws-request-attempt request)))
             (if (memq (process-status (websocket-conn socket)) '(closed failed exit signal))
                 (ende-responses-ws--closed connection socket)
               (ende-responses-ws--fail request 'transport "Responses request submission failed")))))))))

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
     ((memq reason '(connection-loss connect-timeout missing-response connection-limit unsent-replacement))
      (let ((connection (ende-responses-ws-request-connection request)))
        (cl-incf (ende-responses-ws-request-retries request))
        (cl-incf (ende-responses-ws-request-attempt request))
        (dolist (timer (list (ende-responses-ws-request-connect-timer request)
                            (ende-responses-ws-request-idle-timer request)))
          (when timer (cancel-timer timer)))
        (setf (ende-responses-ws-request-connect-timer request) nil
              (ende-responses-ws-request-idle-timer request) nil
              (ende-responses-ws-request-payload request) nil
              (ende-responses-ws-request-delivery request) 'unsent
              (ende-responses-ws-request-prepared-generation request) nil
              (ende-responses-ws-request-fragment-active request) nil
              (ende-responses-ws-request-frame-parts request) nil
              (ende-responses-ws-request-frame-bytes request) 0)
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
        (setf (ende-responses-ws-connection-headers connection) headers
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
  "Send the prepared request when CONNECTION's SOCKET opens."
  (if (ende-responses-ws-connection-closed connection)
      (ende-responses-ws--close-socket socket)
    (unless (and (ende-responses-ws-connection-socket connection)
                 (not (eq socket (ende-responses-ws-connection-socket connection))))
    (setf (ende-responses-ws-connection-socket connection) socket)
    (when-let* ((request (ende-responses-ws-connection-active connection)))
      (when-let* ((timer (ende-responses-ws-request-connect-timer request)))
        (cancel-timer timer)
        (setf (ende-responses-ws-request-connect-timer request) nil))
      (ende-responses-ws--send request)))))

(defun ende-responses-ws--message (connection socket frame)
  "Consume one JSON FRAME from CONNECTION's current SOCKET."
  (when (and (not (ende-responses-ws-connection-closed connection))
             (eq socket (ende-responses-ws-connection-socket connection)))
    (when-let* ((request (ende-responses-ws-connection-active connection)))
      (when (and (ende-responses-ws--live-p request)
                 (memq (ende-responses-ws-request-delivery request) '(sending sent)))
        (let ((attempt (ende-responses-ws-request-attempt request))
              (payload (websocket-frame-payload frame))
              (opcode (websocket-frame-opcode frame)))
          (condition-case nil
              (progn
                (unless (and (memq opcode '(text continuation))
                             (eq (eq opcode 'continuation)
                                 (ende-responses-ws-request-fragment-active request)))
                  (error "Invalid fragment sequence"))
                (cl-incf (ende-responses-ws-request-frame-bytes request) (length payload))
                (when (and (ende-responses-ws-request-event-limit request)
                           (> (ende-responses-ws-request-frame-bytes request)
                              (ende-responses-ws-request-event-limit request)))
                  (ende-responses-ws--fail request 'resource "Responses message exceeds byte limit"))
                (when (ende-responses-ws--live-p request)
                  (when (> (length payload) 0)
                    (push payload (ende-responses-ws-request-frame-parts request)))
                  (setf (ende-responses-ws-request-fragment-active request)
                        (not (websocket-frame-completep frame)))
                  (when (websocket-frame-completep frame)
                    (let* ((text (decode-coding-string
                                  (apply #'concat (nreverse (ende-responses-ws-request-frame-parts request))) 'utf-8))
                           (json (ende-protocol-json-read-string text))
                           (type (ende-protocol-json-get "type" json)))
                      (setf (ende-responses-ws-request-frame-parts request) nil
                            (ende-responses-ws-request-frame-bytes request) 0)
                      (cl-incf (ende-responses-ws-request-messages request))
                      (unless (stringp type) (error "Missing Responses event type"))
                      (when (memq (intern-soft type)
                                  '(response.created response.in_progress response.completed response.done
                                    response.incomplete response.failed error response.output_text.delta
                                    response.output_item.added response.output_item.done
                                    response.reasoning_summary_text.delta response.function_call_arguments.delta
                                    response.function_call_arguments.done))
                        (setf (ende-responses-ws-request-last-event request) (intern-soft type)))
                      (unless (ende-responses-ws--recover request json)
                        (funcall (ende-responses-ws-request-on-json request) json)
                        (when (and (ende-responses-ws--attempt-live-p request attempt socket)
                                   (member type '("response.completed" "response.done" "response.incomplete")))
                          (funcall (ende-responses-ws-request-on-complete request))))))))
            ((error quit)
             (when (and (ende-responses-ws--live-p request)
                        (= attempt (ende-responses-ws-request-attempt request)))
               (ende-responses-ws--fail request 'protocol "Invalid Responses WebSocket event")))))))))

(defun ende-responses-ws--dispatch-input (connection socket text)
  "Decode TEXT frame by frame, never letting a retired SOCKET reconnect."
  (let ((position 0) (size (length text)))
    (while (and (< position size)
                (eq socket (ende-responses-ws-connection-socket connection))
                (not (ende-responses-ws-connection-closed connection)))
      (let ((frame (websocket-read-frame (substring text position))))
        (if (not frame)
            (setf (websocket-inflight-input socket) (substring text position)
                  position size)
          (cl-incf position (websocket-frame-length frame))
          (pcase (websocket-frame-opcode frame)
            ((or 'text 'continuation)
             (ende-responses-ws--message connection socket frame))
            ('ping
             (when (and (ende-responses-ws--eligible-p connection)
                        (eq (process-status (websocket-conn socket)) 'open))
               (process-send-string
                (websocket-conn socket)
                (websocket-encode-frame
                 (make-websocket-frame :opcode 'pong :completep t
                                       :payload (websocket-frame-payload frame)) t))))))))))

(defun ende-responses-ws--accept-input (connection socket output)
  "Bound OUTPUT once on receipt, then dispatch preflighted frames."
  (catch 'rejected
    (let* ((request (ende-responses-ws-connection-active connection))
           (text (concat (websocket-inflight-input socket) output))
           (size (length text)) (position 0) close-position close-code pending-close
           (message-bytes (if request (ende-responses-ws-request-frame-bytes request) 0))
           (byte-limit (if request (ende-responses-ws-request-byte-limit request)
                         (ende-responses-ws-connection-byte-limit connection)))
           (event-limit (if request (ende-responses-ws-request-event-limit request)
                          (ende-responses-ws-connection-event-limit connection))))
      (cl-labels ((reject (kind message)
                    (when request (ende-responses-ws--fail request kind message))
                    (ende-responses-ws--discard connection)
                    (throw 'rejected nil)))
        (when request
          (cl-incf (ende-responses-ws-request-received-bytes request) (length output))
          (when (and byte-limit (> (ende-responses-ws-request-received-bytes request) byte-limit))
            (reject 'resource "Responses wire input exceeds byte limit")))
        (when (and (not request) byte-limit (> size byte-limit))
          (reject 'resource "Responses idle input exceeds byte limit"))
        (while (<= (+ position 2) size)
          (let* ((first (aref text position))
                 (opcode (logand first 15))
                 (length-byte (aref text (1+ position))))
            (when (or (not (zerop (logand first #x70)))
                      (not (zerop (logand length-byte #x80)))
                      (not (memq opcode '(0 1 8 9 10)))
                      (and (>= opcode 8)
                           (or (zerop (logand first #x80)) (> length-byte 125)))
                      (and (= opcode 8) (= length-byte 1)))
              (reject 'protocol "Invalid Responses WebSocket frame"))
            (let ((info (catch 'websocket-incomplete-frame
                          (websocket-get-payload-len
                           (substring text (1+ position) (min size (+ position 10)))))))
              (if (not info)
                  (setq position size)
                (let* ((bytes (car info))
                       (end (+ position 1 (cdr info) bytes)))
                  (when (and byte-limit
                             (> (if request
                                    (+ (ende-responses-ws-request-received-bytes request)
                                       (max 0 (- end size)))
                                  end)
                                byte-limit))
                    (reject 'resource "Responses advertised frame exceeds byte limit"))
                  (when (memq opcode '(0 1))
                    (cl-incf message-bytes bytes)
                    (when (and event-limit (> message-bytes event-limit))
                      (reject 'resource "Responses advertised message exceeds byte limit"))
                    (unless (zerop (logand first #x80)) (setq message-bytes 0)))
                  (when (and (= opcode 8) (<= end size))
                    (setq close-position position)
                    (when (>= bytes 2)
                      (setq close-code (websocket-get-bytes (substring text (+ position 2) end) 2))
                      (unless (or (memq close-code '(1000 1001 1002 1003 1007 1008 1009 1010 1011 1012 1013 1014))
                                  (<= 3000 close-code 4999))
                        (reject 'protocol "Invalid Responses WebSocket close code")))
                    (setq end size))
                  (when (and (= opcode 8) (> end size))
                    (setq pending-close position))
                  (setq position end))))))
        (when (and (= position (1- size)) (= (logand (aref text position) 15) 8))
          (setq pending-close position))
        (when (and request (ende-responses-ws--live-p request)
                   (memq (ende-responses-ws-request-delivery request) '(sending sent))
                   (> (length output) 0))
          (ende-responses-ws--idle request))
        (setf (websocket-inflight-input socket) nil)
        (when (or close-position pending-close)
          (setf (ende-responses-ws-connection-closing connection) t
                (ende-responses-ws-connection-close-code connection) close-code))
        (ende-responses-ws--dispatch-input
         connection socket (if-let* ((end (or close-position pending-close))) (substring text 0 end) text))
        (when (and pending-close (eq socket (ende-responses-ws-connection-socket connection)))
          (setf (websocket-inflight-input socket) (substring text pending-close)))
        (when (and close-position (eq socket (ende-responses-ws-connection-socket connection)))
          (ende-responses-ws--closed connection socket))))))

(defun ende-responses-ws--closed (connection socket)
  "Retire idle sockets or recover a prematurely interrupted logical request."
  (when (and (not (ende-responses-ws-connection-closed connection))
             (eq socket (ende-responses-ws-connection-socket connection)))
    (let ((request (ende-responses-ws-connection-active connection))
          (code (ende-responses-ws-connection-close-code connection)))
      (cond
       ((not request) (ende-responses-ws--discard connection))
       ((or (null code) (memq code '(1000 1001 1006 1011 1012 1013 1014)))
        (ende-responses-ws--retry
         request (if (eq (ende-responses-ws-request-delivery request) 'unsent)
                     'unsent-replacement 'connection-loss)))
       (t (ende-responses-ws--fail request 'transport "Responses connection rejected")
          (ende-responses-ws--discard connection))))))

(defun ende-responses-ws--socket-error (connection socket _callback _error)
  "Contain unknown dependency failures without exposing their private data."
  (when (eq socket (ende-responses-ws-connection-socket connection))
    (when-let* ((request (ende-responses-ws-connection-active connection)))
      (ende-responses-ws--fail request 'protocol "Responses socket callback failed"))
    (ende-responses-ws--discard connection)))

(defun ende-responses-ws--adopt (connection opening process)
  "Adopt PROCESS once for live OPENING, or delete a late process."
  (if (or (ende-responses-ws-connection-closed connection)
          (not (eq opening (ende-responses-ws-connection-opening connection)))
          (/= (ende-responses-ws-opening-generation opening)
              (ende-responses-ws-connection-generation connection))
          (and (ende-responses-ws-opening-process opening)
               (not (eq process (ende-responses-ws-opening-process opening)))))
      (progn (ende-responses-ws--delete-process process) nil)
    (unless (ende-responses-ws-connection-socket connection)
      (let ((socket
             (websocket-inner-create
              :conn process :url (ende-responses-ws-connection-url connection)
              :accept-string (websocket-calculate-accept (ende-responses-ws-opening-key opening))
              :on-open #'ignore
              :on-message (lambda (socket frame)
                            (ende-responses-ws--callback connection socket
                                                         #'ende-responses-ws--message frame))
              :on-close (lambda (socket)
                          (ende-responses-ws--callback connection socket #'ende-responses-ws--closed))
              :on-error (lambda (socket &rest _)
                          (ende-responses-ws--callback connection socket
                                                       #'ende-responses-ws--socket-error nil nil)))))
        (setf (ende-responses-ws-opening-process opening) process
              (ende-responses-ws-connection-socket connection) socket)
        (process-put process :websocket socket)))
    (ende-responses-ws-connection-socket connection)))

(defun ende-responses-ws--callback (connection socket function &rest arguments)
  "Contain FUNCTION's callback failures without disclosing ARGUMENTS."
  (ende-responses-ws--private
    (condition-case nil
        (apply function connection socket arguments)
      ((error quit) (ende-responses-ws--socket-error connection socket nil nil)))))

(defun ende-responses-ws--status (connection opening process)
  "Dispatch native PROCESS status after adoption, including immediate completion."
  (when-let* ((socket (ende-responses-ws--adopt connection opening process)))
    (pcase (process-status process)
      ('open
       (unless (ende-responses-ws-opening-upgrade-sent opening)
         (let* ((url (ende-responses-ws-connection-url connection))
                (path (url-filename (url-generic-parse-url url)))
                (headers (mapcar (lambda (header)
                                   (string-match ": *" header)
                                   (cons (substring header 0 (match-beginning 0))
                                         (substring header (match-end 0))))
                                 (ende-responses-ws-connection-headers connection)))
                (text (format "GET %s HTTP/1.1\r\n%s"
                              (if (string-empty-p path) "/" path)
                              (websocket-create-headers url (ende-responses-ws-opening-key opening)
                                                        nil nil headers))))
           (setf (ende-responses-ws-opening-upgrade-sent opening) t
                 (ende-responses-ws-connection-phase connection) 'upgrading)
           (process-send-string process text))))
      ((or 'closed 'failed 'exit 'signal)
       (if (or (eq (ende-responses-ws-connection-phase connection) 'connecting)
               (plist-get (ende-ignore-errors #'gnutls-peer-status process) :warnings))
           (progn
             (when-let* ((request (ende-responses-ws-connection-active connection)))
               (ende-responses-ws--fail request 'transport "Responses TLS or connection establishment failed"))
             (ende-responses-ws--discard connection))
         (ende-responses-ws--closed connection socket))))))

(defun ende-responses-ws--input (connection opening process output)
  "Validate a bounded Upgrade before passing OUTPUT to open-state framing."
  (when-let* ((socket (ende-responses-ws--adopt connection opening process)))
    (when (eq (ende-responses-ws-connection-phase connection) 'upgrading)
      (let* ((text (concat (ende-responses-ws-opening-header opening) output))
             (end (string-match "\r\n\r\n" text))
             (length (if end (+ end 4) (length text))))
        (when (and (ende-responses-ws-connection-header-limit connection)
                   (> length (ende-responses-ws-connection-header-limit connection)))
          (ende-responses-ws--fail (ende-responses-ws-connection-active connection)
                                   'resource "Responses handshake header exceeds byte limit")
          (error "Handshake header exceeds limit"))
        (setq output nil)
        (if (not end)
            (setf (ende-responses-ws-opening-header opening) text)
          (let ((header (substring text 0 length)))
            (when (string-match "\\`HTTP/1\\.1 \\([0-9][0-9][0-9]\\)" header)
              (setf (ende-responses-ws-connection-http-status connection)
                    (string-to-number (match-string 1 header))))
            (websocket-verify-response-code header)
            (websocket-verify-headers socket header))
          (setf (ende-responses-ws-opening-header opening) nil
                (ende-responses-ws-connection-phase connection) 'open
                (websocket-ready-state socket) 'open)
          (setq output (substring text length))
          (ende-responses-ws--open connection socket))))
    (when (and output (not (string-empty-p output))
               (eq socket (ende-responses-ws-connection-socket connection))
               (eq (ende-responses-ws-connection-phase connection) 'open))
      (ende-responses-ws--accept-input connection socket output))))

(defun ende-responses-ws--opening-error (connection opening failure)
  "Classify structured FAILURE only while CONNECTION still owns OPENING."
  (when (eq opening (ende-responses-ws-connection-opening connection))
    (when-let* ((request (ende-responses-ws-connection-active connection)))
      (if (or (eq (car-safe failure) 'network-error)
              (memq (ende-responses-ws-connection-http-status connection) '(408 500 502 503 504)))
          (ende-responses-ws--retry request 'connection-loss)
        (ende-responses-ws--fail request 'transport "Responses connection opening failed")))))

(defun ende-responses-ws--connect (connection)
  "Publish and start one owned asynchronous, verified TLS opening attempt."
  (let* ((opening (make-ende-responses-ws-opening
                   :generation (ende-responses-ws-connection-generation connection)
                   :key (websocket-genkey)))
         (url (url-generic-parse-url (ende-responses-ws-connection-url connection)))
         (host (url-host url))
         (port (if (zerop (url-port url)) 443 (url-port url)))
         (request (ende-responses-ws-connection-active connection)))
    (setf (ende-responses-ws-connection-opening connection) opening
          (ende-responses-ws-connection-phase connection) 'connecting
          (ende-responses-ws-connection-created connection) (float-time))
    (when-let* ((timeout (ende-responses-ws-request-connect-timeout request)))
      (setf (ende-responses-ws-request-connect-timer request)
            (run-at-time timeout nil #'ende-responses-ws--attempt-timeout request
                         (ende-responses-ws-request-attempt request) 'connect)))
    (ende-responses-ws--private
      (condition-case failure
          (let ((default-directory (ende-curl-local-directory)))
            (unless (gnutls-available-p) (error "Verified built-in TLS required"))
            (let* ((filter (lambda (process output)
                             (ende-responses-ws--private
                               (condition-case failure
                                   (ende-responses-ws--input connection opening process output)
                                 ((error quit)
                                  (ende-responses-ws--opening-error connection opening failure))))))
                   (sentinel (lambda (process _event)
                               (ende-responses-ws--private
                                 (condition-case failure
                                     (ende-responses-ws--status connection opening process)
                                   ((error quit)
                                    (ende-responses-ws--opening-error connection opening failure))))))
                   (process (make-network-process
                             :name "ende-responses" :buffer nil :host host :service port
                             :nowait t :noquery t :coding 'binary :filter-multibyte nil
                             :tls-parameters (cons 'gnutls-x509pki
                                                   (gnutls-boot-parameters
                                                    :type 'gnutls-x509pki :hostname host :verify-error t))
                             :filter filter :sentinel sentinel)))
              (ende-responses-ws--status connection opening process)))
        ((error quit)
         (ende-responses-ws--opening-error connection opening failure))))))

(defun ende-responses-ws-send-prepared (request attempt generation data body record)
  "Open or reuse REQUEST's socket and send prepared DATA, BODY and RECORD."
  (if (not (ende-responses-ws--preparation-live-p request attempt))
      (ende-ignore-errors #'ende-debug-record-completion record 'discarded nil nil nil)
    (ende-responses-ws--private
      (condition-case nil
          (let* ((connection (ende-responses-ws-request-connection request)))
            (setf (ende-responses-ws-request-prepared-generation request) generation
                  (ende-responses-ws-request-logical-request request) (plist-get data :retained)
                  (ende-responses-ws-request-payload request) data)
            (funcall (ende-responses-ws-request-on-prepared request) body record)
            (when (and (ende-responses-ws--live-p request)
                       (= attempt (ende-responses-ws-request-attempt request)))
              (if (/= generation (ende-responses-ws-connection-generation connection))
                  (progn
                    (ende-ignore-errors #'ende-debug-record-completion record 'discarded nil nil nil)
                    (ende-responses-ws--retry request 'unsent-replacement))
                (if (eq (ende-responses-ws-connection-phase connection) 'open)
                    (ende-responses-ws--send request)
                  (unless (ende-responses-ws-connection-opening connection)
                    (ende-responses-ws--connect connection))))))
        ((error quit)
         (when (and (ende-responses-ws--live-p request)
                    (= attempt (ende-responses-ws-request-attempt request)))
           (ende-responses-ws--fail request 'transport "Could not send Responses WebSocket request")))))))

(provide 'ende-responses-ws)
;;; ende-responses-ws.el ends here
