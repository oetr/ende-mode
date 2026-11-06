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

(cl-defstruct ende-responses-ws-connection
  owner identity socket active chain closed key url headers properties
  opening (generation 1) (phase 'disconnected) created http-status close-code)

(cl-defstruct ende-responses-ws-opening
  generation key process upgrade-sent header)

(cl-defstruct ende-responses-ws-chain
  response-id input settings)

(cl-defstruct ende-responses-ws-request
  connection lifecycle full-body on-json on-complete on-prepared
  connect-timer deadline-timer idle-timer retried observed-output
  payload (delivery 'unsent) (attempt 0) prepared-generation
  profile debug connect-timeout idle-timeout byte-limit event-limit
  reused replaced last-event
  fragment-active frame-parts (frame-bytes 0) (received-bytes 0) (messages 0))

(defvar ende-responses-ws--connections (make-hash-table :test #'equal)
  "Private live connections, including ownerless ephemeral requests.")

(defvar ende-responses-ws--closing nil
  "Non-nil while shutdown prohibits allocation of replacement connections.")

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

(defun ende-responses-ws--drop-socket (connection)
  "Invalidate CONNECTION's socket and lineage before process deletion."
  (let ((socket (ende-responses-ws-connection-socket connection)))
    (cl-incf (ende-responses-ws-connection-generation connection))
    (setf (ende-responses-ws-connection-socket connection) nil
          (ende-responses-ws-connection-opening connection) nil
          (ende-responses-ws-connection-phase connection) 'disconnected
          (ende-responses-ws-connection-chain connection) nil
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
      (setf (ende-responses-ws-connection-chain connection)
            (when (and (eq outcome 'success) (stringp response-id) (not (string-empty-p response-id)))
              (let* ((retained (ende-request--retained-body
                                (ende-responses-ws-request-full-body request) nil))
                     (full (ende-protocol-json-read-string (json-encode retained))))
                (make-ende-responses-ws-chain
                 :response-id response-id :settings (ende-responses-ws--settings full)
                 :input (vconcat (gethash "input" full)
                                 (ende-protocol-json-read-string (json-encode (vconcat output))))))))
      (setf (ende-responses-ws-connection-active connection) nil)
      (unless (and (eq outcome 'success) (ende-responses-ws-connection-owner connection))
        (ende-responses-ws--discard connection)))
    (dolist (timer (list (ende-responses-ws-request-connect-timer request)
                         (ende-responses-ws-request-deadline-timer request)
                         (ende-responses-ws-request-idle-timer request)))
      (when timer (cancel-timer timer)))
    (setf (ende-responses-ws-request-connect-timer request) nil
          (ende-responses-ws-request-deadline-timer request) nil
          (ende-responses-ws-request-idle-timer request) nil
          (ende-responses-ws-request-full-body request) nil
          (ende-responses-ws-request-frame-parts request) nil
          (ende-responses-ws-request-fragment-active request) nil
          (ende-responses-ws-request-frame-bytes request) 0
          (ende-responses-ws-request-payload request) nil
          (ende-responses-ws-request-on-json request) nil
          (ende-responses-ws-request-on-complete request) nil
          (ende-responses-ws-request-on-prepared request) nil
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

(defun ende-responses-ws-create-request
    (profile headers properties lifecycle on-json on-complete on-prepared
             &optional authentication-identity)
  "Allocate a request on PROFILE's job-owned connection.
HEADERS are the effective validated handshake headers; PROPERTIES describes
the endpoint's WebSocket protocol.  LIFECYCLE owns per-request cleanup.
ON-JSON interprets provider events, ON-COMPLETE validates a terminal event,
and ON-PREPARED accepts the actual wire body and its debug capture.
AUTHENTICATION-IDENTITY privately identifies an OAuth registration."
  (when ende-responses-ws--closing (error "Responses connections are shutting down"))
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
               (not (equal identity (ende-responses-ws-connection-identity connection))))
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
                    :connect-timeout ende-connect-timeout
                    :idle-timeout ende-response-idle-timeout
                    :byte-limit (or ende-max-response-bytes (* 256 1024 1024))
                    :event-limit (or ende-max-sse-event-bytes (* 16 1024 1024))))
          (timeout (if (plist-member profile :request-timeout)
                       (plist-get profile :request-timeout) ende-request-timeout)))
      (setf (ende-responses-ws-connection-active connection) request)
      (ende-transport-session-add-canceller
       lifecycle (lambda () (ende-responses-ws-finish request 'failure)))
      (when timeout
        (setf (ende-responses-ws-request-deadline-timer request)
              (run-at-time timeout nil #'ende-responses-ws--fail
                           request 'timeout "Responses request deadline exceeded")))
      request)))

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
           (expected (plist-get snapshot :input))
           (id (plist-get snapshot :response-id))
           (continue
            (and (stringp id) (not (string-empty-p id))
                 (ende-protocol-json-equal-p (ende-responses-ws--settings full)
                                             (plist-get snapshot :settings))
                 (<= (length expected) (length input))
                 (cl-loop for index below (length expected)
                          always (ende-protocol-json-equal-p (aref expected index) (aref input index))))))
      (setq body (cl-remove-if (lambda (entry) (equal (ende-controls--json-key-name (car entry)) "input")) body))
      (append (list (cons :type "response.create")) body
              (when continue (list (cons :previous_response_id id)))
              (list (cons :input (ende-protocol-json-for-request
                                 (if continue (substring input (length expected)) (vconcat input)))))
              (when (plist-get properties :stream) (list (cons :stream t)))))))

(defun ende-responses-ws-finalization (request)
  "Return serializable transport finalization arguments for REQUEST."
  (let* ((connection (ende-responses-ws-request-connection request))
         (chain (ende-responses-ws-connection-chain connection)))
    (list #'ende-responses-ws-finalize
          (ende-responses-ws-connection-properties connection)
          (when chain
            (ende-provider-config-copy
             (list :response-id (ende-responses-ws-chain-response-id chain)
                   :input (ende-responses-ws-chain-input chain)
                   :settings (ende-responses-ws-chain-settings chain)))))))

(defun ende-responses-ws--idle (request)
  "Reset REQUEST's captured response-idle deadline."
  (when-let* ((timer (ende-responses-ws-request-idle-timer request)))
    (cancel-timer timer))
  (setf (ende-responses-ws-request-idle-timer request)
        (run-at-time (ende-responses-ws-request-idle-timeout request) nil
                     #'ende-responses-ws--fail request 'timeout
                     "Responses response-idle deadline exceeded")))

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
                (setf (ende-responses-ws-request-payload request) nil
                      (ende-responses-ws-request-delivery request) 'sending)
                (ende-responses-ws--idle request)
                (websocket-send-text socket text)
                (when (ende-responses-ws--attempt-live-p request attempt socket)
                  (setf (ende-responses-ws-request-delivery request) 'sent))))
          ((error quit)
           (when (and (ende-responses-ws--live-p request)
                      (= attempt (ende-responses-ws-request-attempt request)))
             (ende-responses-ws--fail request 'transport "Responses request submission failed"))))))))

(defun ende-responses-ws--refinalize (request)
  "Prepare a fresh wire attempt from REQUEST's unchanged logical source."
  (cl-incf (ende-responses-ws-request-attempt request))
  (setf (ende-responses-ws-request-delivery request) 'unsent
        (ende-responses-ws-request-payload request) nil)
  (when-let* ((timer (ende-responses-ws-request-idle-timer request)))
    (cancel-timer timer)
    (setf (ende-responses-ws-request-idle-timer request) nil))
  (let ((generation (ende-responses-ws-connection-generation
                     (ende-responses-ws-request-connection request))))
    (ende-request-reprepare
     (ende-responses-ws-request-lifecycle request) 'ende-responses-ws
     (ende-responses-ws-request-full-body request)
     (ende-responses-ws-request-profile request) (ende-responses-ws-request-debug request)
     (apply-partially #'ende-responses-ws-send-prepared request
                      (ende-responses-ws-request-attempt request) generation)
     (ende-responses-ws-finalization request))))

(defun ende-responses-ws--recover (request json)
  "Intercept an explicitly recoverable JSON error for REQUEST at most once."
  (let* ((type (ende-protocol-json-get "type" json))
         (error (or (ende-protocol-json-get "error" json)
                    (ende-protocol-json-get "error" (ende-protocol-json-get "response" json))))
         (code (ende-protocol-json-get "code" error)))
    (when (and (member type '("error" "response.failed"))
               (member code '("previous_response_not_found" "websocket_connection_limit_reached"))
               (not (ende-responses-ws-request-retried request))
               (not (ende-responses-ws-request-observed-output request)))
      (setf (ende-responses-ws-request-retried request) t)
      (let ((connection (ende-responses-ws-request-connection request)))
        (setf (ende-responses-ws-connection-chain connection) nil)
        (when (equal code "websocket_connection_limit_reached")
          (ende-responses-ws--drop-socket connection)))
      (ende-responses-ws--refinalize request)
      t)))

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
                (when (> (ende-responses-ws-request-frame-bytes request)
                         (ende-responses-ws-request-event-limit request))
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
                      (when (or (string-match-p "\\`response\\.\\(?:output\\|reasoning\\|function_call\\)" type)
                                (let ((output (ende-protocol-json-get "output" (ende-protocol-json-get "response" json))))
                                  (and (vectorp output) (> (length output) 0))))
                        (setf (ende-responses-ws-request-observed-output request) t))
                      (unless (ende-responses-ws--recover request json)
                        (funcall (ende-responses-ws-request-on-json request) json)
                        (when (and (ende-responses-ws--attempt-live-p request attempt socket)
                                   (member type '("response.completed" "response.done" "response.incomplete")))
                          (funcall (ende-responses-ws-request-on-complete request))))))))
            ((error quit)
             (when (and (ende-responses-ws--live-p request)
                        (= attempt (ende-responses-ws-request-attempt request)))
               (ende-responses-ws--fail request 'protocol "Invalid Responses WebSocket event")))))))))

(defun ende-responses-ws--accept-input (connection socket output)
  "Bound OUTPUT once on receipt, then use dependency open-state framing."
  (catch 'rejected
    (let* ((request (ende-responses-ws-connection-active connection))
           (text (concat (websocket-inflight-input socket) output))
           (size (length text)) (position 0) close-position close-code pending-close
           (message-bytes (if request (ende-responses-ws-request-frame-bytes request) 0))
           (byte-limit (and request (ende-responses-ws-request-byte-limit request)))
           (event-limit (if request (ende-responses-ws-request-event-limit request)
                          (or ende-max-sse-event-bytes (* 16 1024 1024)))))
      (cl-labels ((reject (kind message)
                    (when request (ende-responses-ws--fail request kind message))
                    (ende-responses-ws--discard connection)
                    (throw 'rejected nil)))
        (when request
          (cl-incf (ende-responses-ws-request-received-bytes request) (length output))
          (when (> (ende-responses-ws-request-received-bytes request) byte-limit)
            (reject 'resource "Responses wire input exceeds byte limit")))
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
                  (when (and request (> (+ (ende-responses-ws-request-received-bytes request)
                                                           (max 0 (- end size))) byte-limit))
                    (reject 'resource "Responses advertised frame exceeds byte limit"))
                  (when (memq opcode '(0 1))
                    (cl-incf message-bytes bytes)
                    (when (> message-bytes event-limit)
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
        (websocket-process-input-on-open-ws
         socket (if-let* ((end (or close-position pending-close))) (substring text 0 end) text))
        (when (and pending-close (eq socket (ende-responses-ws-connection-socket connection)))
          (setf (websocket-inflight-input socket) (substring text pending-close)))
        (when (and close-position (eq socket (ende-responses-ws-connection-socket connection)))
          (setf (ende-responses-ws-connection-close-code connection) close-code)
          (ende-responses-ws--closed connection socket))))))

(defun ende-responses-ws--closed (connection socket)
  "Handle socket loss without replaying an ambiguously submitted request."
  (when (and (not (ende-responses-ws-connection-closed connection))
             (eq socket (ende-responses-ws-connection-socket connection)))
    (let ((request (ende-responses-ws-connection-active connection)))
      (cond
       ((not request) (ende-responses-ws--discard connection))
       ((and (ende-responses-ws--live-p request)
             (eq (ende-responses-ws-request-delivery request) 'unsent)
             (ende-responses-ws-request-reused request)
             (not (ende-responses-ws-request-replaced request))
             (eq (ende-responses-ws-connection-phase connection) 'open))
        (setf (ende-responses-ws-request-replaced request) t)
        (ende-responses-ws--drop-socket connection)
        (if (ende-responses-ws-request-full-body request)
            (ende-responses-ws--refinalize request)
          (ende-responses-ws--connect connection)))
       (t (ende-responses-ws--fail request 'transport "Responses connection closed before completion")
          (ende-responses-ws--discard connection))))))

(defun ende-responses-ws--socket-error (connection socket _callback _error)
  "Contain dependency failures without exposing their private error data."
  (ende-responses-ws--closed connection socket))

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
       (ende-responses-ws--closed connection socket)))))

(defun ende-responses-ws--input (connection opening process output)
  "Validate a bounded Upgrade before passing OUTPUT to open-state framing."
  (when-let* ((socket (ende-responses-ws--adopt connection opening process)))
    (when (eq (ende-responses-ws-connection-phase connection) 'upgrading)
      (let* ((text (concat (ende-responses-ws-opening-header opening) output))
             (end (string-match "\r\n\r\n" text))
             (length (if end (+ end 4) (length text))))
        (when (> length (or ende-max-response-header-bytes (* 1024 1024)))
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
            (run-at-time timeout nil #'ende-responses-ws--fail request 'timeout
                         "Responses connection deadline exceeded")))
    (ende-responses-ws--private
      (condition-case nil
          (let ((default-directory (ende-curl-local-directory)))
            (unless (gnutls-available-p) (error "Verified built-in TLS required"))
            (let* ((filter (lambda (process output)
                             (ende-responses-ws--private
                               (condition-case nil
                                   (ende-responses-ws--input connection opening process output)
                                 ((error quit)
                                  (when-let* ((socket (ende-responses-ws-connection-socket connection)))
                                    (ende-responses-ws--socket-error connection socket nil nil)))))))
                   (sentinel (lambda (process _event)
                               (ende-responses-ws--private
                                 (condition-case nil
                                     (ende-responses-ws--status connection opening process)
                                   ((error quit)
                                    (when-let* ((socket (ende-responses-ws-connection-socket connection)))
                                      (ende-responses-ws--socket-error connection socket nil nil)))))))
                   (process (make-network-process
                             :name "ende-responses" :buffer nil :host host :service port
                             :nowait t :noquery t :coding 'binary :filter-multibyte nil
                             :tls-parameters (cons 'gnutls-x509pki
                                                   (gnutls-boot-parameters
                                                    :type 'gnutls-x509pki :hostname host :verify-error t))
                             :filter filter :sentinel sentinel)))
              (ende-responses-ws--status connection opening process)))
        ((error quit)
         (ende-responses-ws--fail request 'transport "Responses connection opening failed"))))))

(defun ende-responses-ws-send-prepared (request attempt generation data body record)
  "Open or reuse REQUEST's socket and send prepared DATA, BODY and RECORD."
  (when (and (ende-responses-ws--live-p request)
             (= attempt (ende-responses-ws-request-attempt request)))
    (ende-responses-ws--private
      (condition-case nil
          (let* ((connection (ende-responses-ws-request-connection request)))
            (setf (ende-responses-ws-request-prepared-generation request) generation
                  (ende-responses-ws-request-full-body request)
                  (if (plist-get data :full-file) (list :full-file (plist-get data :full-file))
                    (list :full-body (plist-get data :full-body)))
                  (ende-responses-ws-request-payload request) data)
            (funcall (ende-responses-ws-request-on-prepared request) body record)
            (when (ende-responses-ws--live-p request)
              (if (/= generation (ende-responses-ws-connection-generation connection))
                  (progn
                    (ende-ignore-errors #'ende-debug-record-completion record 'discarded nil nil nil)
                    (ende-responses-ws--refinalize request))
                (if (eq (ende-responses-ws-connection-phase connection) 'open)
                    (ende-responses-ws--send request)
                  (unless (ende-responses-ws-connection-opening connection)
                    (ende-responses-ws--connect connection))))))
        ((error quit)
         (ende-responses-ws--fail request 'transport "Could not send Responses WebSocket request"))))))

(provide 'ende-responses-ws)
;;; ende-responses-ws.el ends here
