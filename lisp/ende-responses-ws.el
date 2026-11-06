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
  owner identity socket active chain closed key url headers properties)

(cl-defstruct ende-responses-ws-chain
  response-id input settings)

(cl-defstruct ende-responses-ws-request
  connection lifecycle full-body on-json on-complete on-prepared
  connect-timer deadline-timer idle-timer retried observed-output
  payload sent profile debug connect-timeout idle-timeout byte-limit event-limit
  frame-parts (frame-bytes 0) (received-bytes 0) (messages 0))

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

(defun ende-responses-ws--close-socket (socket)
  "Close SOCKET without dependency logging or propagated errors."
  (let ((websocket-debug nil))
    (ende-ignore-errors #'websocket-close socket)))

(defun ende-responses-ws--discard (connection)
  "Close CONNECTION and forget its private identity and lineage."
  (unless (ende-responses-ws-connection-closed connection)
    (setf (ende-responses-ws-connection-closed connection) t)
    (when (eq connection (gethash (ende-responses-ws-connection-key connection)
                                  ende-responses-ws--connections))
      (remhash (ende-responses-ws-connection-key connection) ende-responses-ws--connections))
    (let ((socket (ende-responses-ws-connection-socket connection)))
      (setf (ende-responses-ws-connection-socket connection) nil
            (ende-responses-ws-connection-active connection) nil
            (ende-responses-ws-connection-chain connection) nil
            (ende-responses-ws-connection-headers connection) nil
            (ende-responses-ws-connection-identity connection) nil)
      (when socket
        (ende-responses-ws--close-socket socket)
        (when (websocket-p socket)
          (setf (websocket-on-open socket) #'ignore
                (websocket-on-message socket) #'ignore
                (websocket-on-close socket) #'ignore
                (websocket-on-error socket) #'ignore
                (websocket-client-data socket) nil
                (websocket-inflight-input socket) nil)
          (let ((process (websocket-conn socket)))
            (when (processp process)
              (set-process-filter process #'ignore)
              (set-process-sentinel process #'ignore)
              (process-put process :websocket nil))))))))

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
          (ende-responses-ws-request-frame-bytes request) 0
          (ende-responses-ws-request-payload request) nil
          (ende-responses-ws-request-on-json request) nil
          (ende-responses-ws-request-on-complete request) nil
          (ende-responses-ws-request-on-prepared request) nil
          (ende-responses-ws-request-profile request) nil
          (ende-responses-ws-request-debug request) nil
          (ende-responses-ws-request-lifecycle request) nil
          (ende-responses-ws-request-connection request) nil)))

(defun ende-responses-ws--fail (request kind message)
  "Fail live REQUEST once with safe KIND and MESSAGE."
  (when (ende-responses-ws--live-p request)
    (funcall (ende-transport-session-callback-error-handler
              (ende-responses-ws-request-lifecycle request))
             (ende-error-create :kind kind :message message))))

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
                    :on-json on-json :on-complete on-complete :on-prepared on-prepared
                    :connect-timeout ende-connect-timeout
                    :idle-timeout ende-response-idle-timeout
                    :byte-limit ende-max-response-bytes :event-limit ende-max-sse-event-bytes))
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

(defun ende-responses-ws--send (request)
  "Send REQUEST's captured payload on its open socket."
  (when (and (ende-responses-ws--live-p request)
             (not (ende-responses-ws-request-sent request)))
    (let* ((websocket-debug nil)
           (data (ende-responses-ws-request-payload request))
           (text (if-let* ((file (plist-get data :file)))
                     (with-temp-buffer
                       (let ((coding-system-for-read 'utf-8-unix)) (insert-file-contents file))
                       (buffer-string))
                   (decode-coding-string (plist-get data :data) 'utf-8))))
      (setf (ende-responses-ws-request-sent request) t)
      (websocket-send-text
       (ende-responses-ws-connection-socket (ende-responses-ws-request-connection request)) text)
      (setf (ende-responses-ws-request-payload request) nil))))

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
      (setf (ende-responses-ws-request-retried request) t
            (ende-responses-ws-request-sent request) nil)
      (when-let* ((timer (ende-responses-ws-request-idle-timer request)))
        (cancel-timer timer)
        (setf (ende-responses-ws-request-idle-timer request) nil))
      (let ((connection (ende-responses-ws-request-connection request)))
        (setf (ende-responses-ws-connection-chain connection) nil)
        (when (equal code "websocket_connection_limit_reached")
          (let ((replacement (copy-ende-responses-ws-connection connection)))
            (setf (ende-responses-ws-connection-socket replacement) nil)
            (ende-responses-ws--discard connection)
            (puthash (ende-responses-ws-connection-key replacement) replacement ende-responses-ws--connections)
            (setf (ende-responses-ws-request-connection request) replacement))))
      (ende-request-reprepare
       (ende-responses-ws-request-lifecycle request) 'ende-responses-ws
       (ende-responses-ws-request-full-body request)
       (ende-responses-ws-request-profile request) (ende-responses-ws-request-debug request)
       (apply-partially #'ende-responses-ws-send-prepared request)
       (ende-responses-ws-finalization request))
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
      (when (and (ende-responses-ws--live-p request) (ende-responses-ws-request-sent request))
        (let* ((payload (websocket-frame-payload frame))
               (opcode (websocket-frame-opcode frame))
               (bytes (length payload)))
          (unless (and (memq opcode '(text continuation))
                       (eq (eq opcode 'continuation)
                           (not (null (ende-responses-ws-request-frame-parts request)))))
            (ende-responses-ws--fail request 'protocol "Invalid Responses WebSocket frame"))
          (when (ende-responses-ws--live-p request)
          (cl-incf (ende-responses-ws-request-received-bytes request) bytes)
          (cl-incf (ende-responses-ws-request-frame-bytes request) bytes)
          (if (or (and (ende-responses-ws-request-byte-limit request)
                       (> (ende-responses-ws-request-received-bytes request)
                          (ende-responses-ws-request-byte-limit request)))
                  (and (ende-responses-ws-request-event-limit request)
                       (> (ende-responses-ws-request-frame-bytes request)
                          (ende-responses-ws-request-event-limit request))))
              (ende-responses-ws--fail request 'resource "Responses WebSocket response exceeds byte limit")
            (push payload (ende-responses-ws-request-frame-parts request))
            (when (websocket-frame-completep frame)
            (cl-incf (ende-responses-ws-request-messages request))
            (condition-case nil
                (let* ((text (decode-coding-string
                              (apply #'concat (nreverse (ende-responses-ws-request-frame-parts request))) 'utf-8))
                       (json (ende-protocol-json-read-string text))
                       (type (ende-protocol-json-get "type" json)))
                  (setf (ende-responses-ws-request-frame-parts request) nil
                        (ende-responses-ws-request-frame-bytes request) 0)
                  (unless (stringp type) (error "Missing Responses event type"))
                  (when (or (string-match-p "\\`response\\.\\(?:output\\|reasoning\\|function_call\\)" type)
                            (let ((output (ende-protocol-json-get "output" (ende-protocol-json-get "response" json))))
                              (and (vectorp output) (> (length output) 0))))
                    (setf (ende-responses-ws-request-observed-output request) t))
                  (unless (ende-responses-ws--recover request json)
                    (funcall (ende-responses-ws-request-on-json request) json)
                    (when (and (ende-responses-ws--live-p request)
                               (member type '("response.completed" "response.done" "response.incomplete")))
                      (funcall (ende-responses-ws-request-on-complete request)))))
              ((error quit)
               (ende-responses-ws--fail request 'protocol "Invalid Responses WebSocket event")))))))))))

(defun ende-responses-ws--accept-input (connection socket output)
  "Validate OUTPUT before buffering it and record response-body activity."
  (catch 'rejected
    (let* ((request (ende-responses-ws-connection-active connection))
           (text (concat (websocket-inflight-input socket) output))
           (size (length text))
           (position 0)
           (body-start 0)
           (total (if request (ende-responses-ws-request-received-bytes request) 0))
           (message-bytes (if request (ende-responses-ws-request-frame-bytes request) 0))
           (byte-limit (and request (ende-responses-ws-request-byte-limit request)))
           (event-limit (if request (ende-responses-ws-request-event-limit request)
                          ende-max-sse-event-bytes)))
      (cl-labels ((reject (kind message)
                    (when request
                      (ende-responses-ws--fail request kind message))
                    (ende-responses-ws--discard connection)
                    (throw 'rejected nil)))
        (when (eq (websocket-ready-state socket) 'connecting)
          (let ((end (string-match "\r\n\r\n" text)))
            (setq position (if end (+ end 4) size))
            (when (and ende-max-response-header-bytes (> position ende-max-response-header-bytes))
              (reject 'resource "Responses WebSocket input exceeds byte limit"))))
        (setq body-start position)
        (while (<= (+ position 2) size)
          (let* ((first (aref text position))
                 (opcode (logand first 15))
                 (length-byte (aref text (1+ position))))
            (when (and (>= opcode 8)
                       (or (not (memq opcode '(8 9 10)))
                           (zerop (logand first 128))
                           (> (logand length-byte 127) 125)))
              (reject 'protocol "Invalid Responses WebSocket control frame"))
            (let ((length-info
                   (catch 'websocket-incomplete-frame
                     (websocket-get-payload-len
                      (substring text (1+ position) (min size (+ position 10)))))))
              (if (not length-info)
                  (setq position size)
                (let* ((bytes (car length-info))
                       (frame-size (+ 1 (cdr length-info) bytes
                                      (if (zerop (logand length-byte 128)) 0 4))))
                  (when (memq opcode '(0 1 2))
                    (cl-incf total bytes)
                    (cl-incf message-bytes bytes)
                    (when (or (and byte-limit (> total byte-limit))
                              (and event-limit (> message-bytes event-limit)))
                      (reject 'resource "Responses WebSocket input exceeds byte limit"))
                    (unless (zerop (logand first 128)) (setq message-bytes 0)))
                  (setq position (+ position frame-size))))))))
      (when (and request (ende-responses-ws--live-p request)
                 (> (length output) 0) (< body-start size))
        (ende-responses-ws--idle request))
      t)))

(defun ende-responses-ws--protect-socket (connection socket)
  "Isolate asynchronous SOCKET callbacks from ambient cookies and logging.
Route dependency parser failures through CONNECTION's request lifecycle."
  (when (and (websocket-p socket) (processp (websocket-conn socket)))
    (let* ((process (websocket-conn socket))
           (filter (process-filter process))
           (sentinel (process-sentinel process)))
      (set-process-filter
       process (lambda (process output)
                 (let ((websocket-debug nil)
                       (url-cookie-storage nil) (url-cookie-secure-storage nil))
                   (condition-case nil
                       (when (and (not (ende-responses-ws-connection-closed connection))
                                  (eq socket (ende-responses-ws-connection-socket connection))
                                  (ende-responses-ws--accept-input connection socket output))
                         (funcall filter process output))
                     ((error quit)
                      (ende-responses-ws--socket-error connection socket nil nil))))))
      (set-process-sentinel
       process (lambda (process event)
                 (let ((websocket-debug nil)
                       (url-cookie-storage nil) (url-cookie-secure-storage nil))
                   (condition-case nil
                       (funcall sentinel process event)
                     ((error quit)
                      (ende-responses-ws--socket-error connection socket nil nil)))))))))

(defun ende-responses-ws--closed (connection socket)
  "Discard CONNECTION when its current SOCKET closes unexpectedly."
  (when (and (not (ende-responses-ws-connection-closed connection))
             (eq socket (ende-responses-ws-connection-socket connection)))
    (when-let* ((request (ende-responses-ws-connection-active connection)))
      (ende-responses-ws--fail request 'transport "Responses WebSocket closed before completion"))
    (ende-responses-ws--discard connection)))

(defun ende-responses-ws--socket-error (connection socket _callback _error)
  "Fail CONNECTION's request without exposing SOCKET callback error payloads."
  (when (and (not (ende-responses-ws-connection-closed connection))
             (eq socket (ende-responses-ws-connection-socket connection)))
    (when-let* ((request (ende-responses-ws-connection-active connection)))
      (ende-responses-ws--fail request 'transport "Responses WebSocket transport failed"))
    (ende-responses-ws--discard connection)))

(defun ende-responses-ws-send-prepared (request data body record)
  "Open or reuse REQUEST's socket and send prepared DATA, BODY and RECORD."
  (when (ende-responses-ws--live-p request)
    (condition-case nil
        (let* ((connection (ende-responses-ws-request-connection request))
               (socket (ende-responses-ws-connection-socket connection)))
          (setf (ende-responses-ws-request-full-body request)
                (if (plist-get data :full-file) (list :full-file (plist-get data :full-file))
                  (list :full-body (plist-get data :full-body)))
                (ende-responses-ws-request-payload request) data)
          (funcall (ende-responses-ws-request-on-prepared request) body record)
          (when (ende-responses-ws--live-p request)
            (ende-responses-ws--idle request)
            (if (and socket (websocket-openp socket))
                (ende-responses-ws--send request)
              (let ((default-directory (ende-curl-local-directory))
                    (gnutls-verify-error t)
                    (websocket-debug nil)
                    (url-cookie-storage nil) (url-cookie-secure-storage nil))
                (unless (gnutls-available-p)
                  (error "Responses WebSockets require built-in verified TLS"))
                (when-let* ((timeout (ende-responses-ws-request-connect-timeout request)))
                  (setf (ende-responses-ws-request-connect-timer request)
                        (run-at-time timeout nil #'ende-responses-ws--fail request 'timeout
                                     "Responses connection deadline exceeded")))
                (let ((opened (websocket-open
                       (ende-responses-ws-connection-url connection) :nowait t
                       :custom-header-alist
                       (mapcar (lambda (header)
                                 (string-match ": *" header)
                                 (cons (substring header 0 (match-beginning 0))
                                       (substring header (match-end 0))))
                               (ende-responses-ws-connection-headers connection))
                       :on-open (apply-partially #'ende-responses-ws--open connection)
                       :on-message (apply-partially #'ende-responses-ws--message connection)
                       :on-close (apply-partially #'ende-responses-ws--closed connection)
                       :on-error (apply-partially #'ende-responses-ws--socket-error connection))))
                  (if (ende-responses-ws-connection-closed connection)
                      (ende-responses-ws--close-socket opened)
                    (setf (ende-responses-ws-connection-socket connection) opened)
                    (ende-responses-ws--protect-socket connection opened)))))))
      ((error quit)
       (ende-responses-ws--fail request 'transport "Could not send Responses WebSocket request")))))

(provide 'ende-responses-ws)
;;; ende-responses-ws.el ends here
