;;; ende-messages.el --- Anthropic Messages adapter -*- lexical-binding: t; -*-

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

;; Anthropic Messages adapter for Ende.

;;; Code:

(require 'cl-lib)
(require 'ende-content)
(require 'ende-request)
(require 'ende-stream)
(require 'ende-provider-config)

(cl-defstruct (ende-messages--session (:constructor ende-messages--allocate))
  profile stream blocks stopped debug-record)

(defun ende-messages--lifecycle (session)
  "Return SESSION's lifecycle."
  (ende-stream-lifecycle (ende-messages--session-stream session)))

(defun ende-messages--ordered-entries (session)
  "Return SESSION's indexed blocks in protocol order."
  (let (entries)
    (maphash (lambda (index block) (push (cons index block) entries))
             (ende-messages--session-blocks session))
    (sort entries (lambda (a b) (< (car a) (car b))))))

(defun ende-messages--ordered-blocks (session)
  "Return SESSION's blocks in protocol order."
  (mapcar #'cdr (ende-messages--ordered-entries session)))

(defun ende-messages--block-call (index block &optional metadata-only)
  "Return the detached tool call for BLOCK at INDEX, if any.
Omit accumulated arguments when METADATA-ONLY is non-nil."
  (when (equal (gethash "type" block) "tool_use")
    (ende-provider-config-copy
     (append
      (list :key index :id (gethash "id" block) :name (gethash "name" block))
      (unless metadata-only
        (list :arguments (if-let* ((text (gethash "ende_arguments" block)))
                             (ende-stream-text-string text)
                           (json-encode (gethash "input" block)))))))))

(defun ende-messages--calls (session)
  "Return completed tool calls from SESSION."
  (cl-loop for (index . block) in (ende-messages--ordered-entries session)
           when (and (equal (gethash "type" block) "tool_use")
                     (gethash "ende_complete" block))
           collect (ende-messages--block-call index block)))

(defun ende-messages--protocol-item (block)
  "Return detached provider BLOCK without local state."
  (let ((copy (make-hash-table :test #'equal)))
    (maphash (lambda (key value)
               (unless (member key '("ende_arguments" "ende_complete"))
                 (puthash (copy-sequence key)
                          (if (ende-stream-text-p value)
                              (ende-stream-text-string value)
                            (ende-provider-config-copy value)) copy))) block)
    copy))

(defun ende-messages--append-block-text (block field text)
  "Append TEXT to BLOCK's FIELD without copying its accumulated prefix."
  (let ((value (gethash field block)))
    (unless (ende-stream-text-p value)
      (setq value (ende-stream-text-append (ende-stream-text-create) value))
      (puthash field value block))
    (ende-stream-text-append value text)))

(defun ende-messages--protocol-items (session)
  "Return SESSION's provider content blocks without local state."
  (mapcar #'ende-messages--protocol-item
          (ende-messages--ordered-blocks session)))

(defun ende-messages--emit-block (session index &optional arguments-delta)
  "Emit SESSION's block at INDEX, or only its ARGUMENTS-DELTA."
  (let* ((block (gethash index (ende-messages--session-blocks session)))
         (type (gethash "type" block)))
    (unless (equal type "image")
      (let ((call (ende-messages--block-call index block arguments-delta))
            (item (unless arguments-delta (ende-messages--protocol-item block))))
        (apply #'ende-stream-emit-item
               (ende-messages--session-stream session) index
               (cond (call 'tool-call) ((equal type "text") 'text) (t 'opaque))
               "anthropic-messages"
               (append (when item (list :item item))
                       (when call (list :call call))
                       (when arguments-delta (list :arguments-delta arguments-delta))
                       (when (equal type "text")
                         (list :text (gethash "text" item)))))))))

(defun ende-messages--result (session)
  "Return normalized output and protocol history for SESSION."
  (ende-stream-result
   (ende-messages--session-stream session) "output_tokens"
   (plist-get (ende-messages--session-profile session) :model) "Anthropic"
   (list :tool-calls (ende-messages--calls session)
         :protocol-items (cons 'anthropic-messages (ende-messages--protocol-items session))
         :protocol-item-keys (mapcar #'car (ende-messages--ordered-entries session)))))

(defun ende-messages--settle (session outcome value)
  "Settle SESSION with OUTCOME and VALUE, preserving partial output."
  (ende-transport-session-duration (ende-stream-lifecycle (ende-messages--session-stream session)))
  (let* ((stream (ende-messages--session-stream session))
         (partial (ende-messages--result session)))
    (when (ende-messages--session-debug-record session)
      (ende-debug-record-completion
       (ende-messages--session-debug-record session) outcome
       (ende-stream-session-usage stream)
       (condition-case nil (car (ende-stream-response-headers stream)) (error nil)) value))
    (ende-stream-settle stream outcome value
                        (when (ende-stream-has-result-p stream (cdr (plist-get partial :protocol-items)))
                          partial))))

(defun ende-messages--fail (session error)
  "Fail SESSION with structured ERROR."
  (ende-messages--settle session 'failure error))

(defun ende-messages--cancel (session)
  "Cancel SESSION."
  (ende-messages--fail session (ende-error-create :kind 'cancelled :message "Process cancelled")))

(defun ende-messages--usage (session usage)
  "Merge cumulative USAGE into SESSION."
  (when (hash-table-p usage)
    (let* ((stream (ende-messages--session-stream session))
           (merged (copy-hash-table (or (ende-stream-session-usage stream)
                                        (make-hash-table :test #'equal)))))
      (maphash (lambda (key value) (puthash key value merged)) usage)
      (ende-stream-record-usage stream merged "output_tokens"))))

(defun ende-messages--event (session json)
  "Consume one Messages event JSON for SESSION."
  (let* ((stream (ende-messages--session-stream session))
         (index (gethash "index" json))
         (blocks (ende-messages--session-blocks session))
         (block (gethash index blocks))
         (delta (gethash "delta" json)))
    (pcase (gethash "type" json)
      ("message_start"
       (let ((message (gethash "message" json)))
         (setf (ende-stream-session-actual-model stream) (gethash "model" message))
         (ende-messages--usage session (gethash "usage" message))))
      ("content_block_start"
       (when (or (not (integerp index)) (gethash index blocks))
         (error "Invalid or duplicate Messages content index"))
       (let ((initial (copy-hash-table (gethash "content_block" json))))
         (puthash index initial blocks)
         (dolist (field '(("text" . :content) ("thinking" . :thinking)))
           (when-let* ((text (gethash (car field) initial)))
             (ende-stream-record-delta stream (cdr field) text
                                       (apply-partially #'ende-messages--fail session) index)))
         (ende-messages--emit-block session index)))
      ("content_block_delta"
       (unless (and block (not (gethash "ende_complete" block)))
         (error "Messages delta has no active content block"))
       (pcase (gethash "type" delta)
         ("text_delta"
          (let ((text (gethash "text" delta)))
            (ende-messages--append-block-text block "text" text)
            (ende-stream-record-delta stream :content text
                                      (apply-partially #'ende-messages--fail session) index)))
         ("thinking_delta"
          (let ((text (gethash "thinking" delta)))
            (ende-messages--append-block-text block "thinking" text)
            (ende-stream-record-delta stream :thinking text
                                      (apply-partially #'ende-messages--fail session) index)))
         ("signature_delta"
          (ende-messages--append-block-text block "signature" (gethash "signature" delta)))
         ("input_json_delta"
          (let ((text (or (gethash "partial_json" delta) "")))
            (ende-messages--append-block-text block "ende_arguments" text)
            (ende-messages--emit-block session index text)))))
      ("content_block_stop"
       (unless block (error "Messages stop has no content block"))
       (when (equal (gethash "type" block) "tool_use")
         (let* ((text (when-let* ((parts (gethash "ende_arguments" block)))
                        (ende-stream-text-string parts)))
                (input (if text (ende-protocol-json-read-string text) (gethash "input" block))))
           (unless (and (hash-table-p input)
                        (stringp (gethash "id" block)) (not (string-empty-p (gethash "id" block)))
                        (stringp (gethash "name" block)) (not (string-empty-p (gethash "name" block))))
             (error "Incomplete Messages tool call"))
           (puthash "input" input block)))
       (puthash "ende_complete" t block)
       (ende-messages--emit-block session index))
      ("message_delta"
       (setf (ende-stream-session-finish-reason stream) (gethash "stop_reason" delta))
       (ende-messages--usage session (gethash "usage" json)))
      ("message_stop" (setf (ende-messages--session-stopped session) t))
      ("error"
       (ende-messages--fail
        session (ende-error-create :kind 'provider
                                   :message (ende-transport-safe-error-text
                                             (gethash "message" (gethash "error" json)))))))))

(defun ende-messages--events (session events)
  "Consume SSE EVENTS while SESSION is active."
  (dolist (event events)
    (unless (ende-stream-terminal-p (ende-messages--session-stream session))
      (when (eq (plist-get event :type) :json)
        (ende-messages--event session (plist-get event :value))))))

(defun ende-messages--parse-error (session error &optional _over-limit)
  "Fail SESSION for parser ERROR."
  (ende-messages--fail session (ende-error-create :kind 'protocol :message (error-message-string error))))

(defun ende-messages--filter (session _process chunk)
  "Consume CHUNK for SESSION."
  (ende-stream-filter
   (ende-messages--session-stream session) chunk
   (apply-partially #'ende-messages--events session)
   (apply-partially #'ende-messages--fail session)
   (apply-partially #'ende-messages--parse-error session)))

(defun ende-messages--finish (session headers)
  "Validate HEADERS and terminal state for SESSION."
  (let* ((stream (ende-messages--session-stream session))
         (type-error (ende-stream-content-type-error (cdr headers) 'if-present)))
    (cond
     ((not (ende-stream-http-success-p (car headers)))
      (ende-messages--fail session (ende-error-create :kind 'http
                                                      :message (format "Messages HTTP status %s" (car headers)))))
     (type-error (ende-messages--fail session type-error))
     (t
      (ende-messages--events session (ende-protocol-sse-flush (ende-stream-session-parser stream)))
      (unless (ende-stream-terminal-p stream)
        (if (and (ende-messages--session-stopped session)
                 (member (ende-stream-session-finish-reason stream) '("end_turn" "tool_use" "stop_sequence"))
                 (cl-every (lambda (block) (gethash "ende_complete" block))
                           (ende-messages--ordered-blocks session)))
            (ende-messages--settle session 'success (ende-messages--result session))
          (ende-messages--fail session (ende-error-create :kind 'incomplete
                                                          :message "Messages generation is incomplete"))))))))

(defun ende-messages--process-error (session process event idle-error)
  "Fail SESSION for PROCESS terminal EVENT or IDLE-ERROR."
  (ende-messages--fail session (or idle-error (ende-error-create
                                              :kind (if (= (process-exit-status process) 28) 'timeout 'transport)
                                              :message (string-trim event)))))

(defun ende-messages--sentinel (session process event)
  "Handle terminal PROCESS EVENT for SESSION."
  (ende-stream-sentinel
   (ende-messages--session-stream session) process event
   (apply-partially #'ende-messages--process-error session)
   (apply-partially #'ende-messages--finish session)
   (apply-partially #'ende-messages--parse-error session)))

(defun ende-messages--content (content)
  "Encode internal CONTENT into Messages blocks."
  (if (stringp content)
      (unless (string-empty-p content) (list (list (cons :type "text") (cons :text content))))
    (mapcar (lambda (part)
              (pcase (plist-get part :type)
                (:text (list (cons :type "text") (cons :text (plist-get part :text))))
                (:image
                 (let ((data (ende-content-image-data part)))
                   (list (cons :type "image")
                         (cons :source (list (cons :type "base64") (cons :media_type (car data))
                                             (cons :data (base64-encode-string (cdr data) t)))))))
                (_ (error "Unsupported Messages content")))) content)))

(defun ende-messages--request-body (messages profile)
  "Encode MESSAGES and PROFILE for Messages."
  (let (system encoded)
    (unless (plist-get profile :output-tokens)
      (error "Messages requires :output-tokens; configure a requested output budget"))
    (dolist (message
             (cl-remove-if
              (lambda (entry)
                (and (consp entry) (not (stringp (car entry)))
                     (plist-get entry :protocol-opaque)
                     (not (equal (plist-get entry :protocol-provider)
                                 "anthropic-messages"))))
              (ende-content-provider-messages messages)))
      (let* ((message-data (unless (stringp (car message)) message))
             (role (ende-content-role message))
             (items (plist-get message-data :protocol-items))
             (opaque (plist-get message-data :protocol-opaque))
             (content (cond
                       (opaque (list opaque))
                       ((eq (car-safe items) 'anthropic-messages) (cdr items))
                       (t (ende-messages--content (ende-content-body message)))))
             (calls (plist-get message-data :tool-calls))
             (id (plist-get message-data :tool-call-id)))
        (if (equal role "system")
            (setq system (append system content))
          (when (eq (car-safe items) 'anthropic-messages)
            (setq content (cdr items)
                  calls (cl-remove-if
                         (lambda (call)
                           (cl-some (lambda (block)
                                      (and (equal (gethash "type" block) "tool_use")
                                           (equal (gethash "id" block) (plist-get call :id))))
                                    content)) calls)))
          (dolist (call calls)
            (let ((input (ende-protocol-json-read-string (plist-get call :arguments))))
              (unless (hash-table-p input) (error "Messages tool arguments must be an object"))
              (setq content (append content (list (list (cons :type "tool_use")
                                                        (cons :id (plist-get call :id))
                                                        (cons :name (plist-get call :name)) (cons :input input)))))))
          (when id
            (setq role "user" content (list (list (cons :type "tool_result")
                                                  (cons :tool_use_id id)
                                                  (cons :content (ende-content-body message))))))
          (if (and encoded (equal role (alist-get :role (car encoded))))
              (setcdr (assq :content (car encoded))
                      (append (alist-get :content (car encoded)) content))
            (push (list (cons :role role) (cons :content content)) encoded)))))
    (ende-provider-config-encode-parameters
     (ende-controls-encode
      (append (list (cons :model (plist-get profile :model)) (cons :messages (nreverse encoded))
                    (cons :max_tokens (plist-get profile :output-tokens)) (cons :stream t))
              (when system (list (cons :system system)))
              (when (plist-get profile :tool-definitions)
                (list (cons :tools
                            (mapcar (lambda (definition)
                                      (let ((function (alist-get :function definition)))
                                        (list (cons :name (alist-get :name function))
                                              (cons :description (alist-get :description function))
                                              (cons :input_schema (alist-get :parameters function)))))
                                    (plist-get profile :tool-definitions))))))
      (plist-get profile :controls) profile) profile)))

(defun ende-messages--start (session data _body record)
  "Start SESSION with prepared DATA and debug RECORD."
  (let* ((profile (ende-messages--session-profile session))
         (request (ende-curl-start-request
                   :profile profile :name "ende-messages"
                   :internal-headers '("anthropic-version: 2023-06-01")
                   :command (lambda (headers config)
                              (ende-curl-command profile headers config
                                                 (and (listp data) (plist-get data :file))))
                   :filter (apply-partially #'ende-messages--filter session)
                   :sentinel (apply-partially #'ende-messages--sentinel session))))
    (ende-transport-session-set-resources (ende-messages--lifecycle session) request)
    (setf (ende-messages--session-debug-record session) record)
    (ende-request-send (plist-get request :process) data)))

(defun ende-messages-default (messages callbacks profile)
  "Send MESSAGES with CALLBACKS using resolved PROFILE."
  (let* ((session (ende-messages--allocate :profile profile :blocks (make-hash-table :test #'eql)))
         (lifecycle (ende-transport-session-create callbacks (apply-partially #'ende-messages--fail session))))
    (setf (ende-messages--session-stream session) (ende-stream-create lifecycle (ende-protocol-make-sse)))
    (condition-case failure
        (progn
          (ende-transport-session-invoke lifecycle :on-start (apply-partially #'ende-messages--cancel session))
          (unless (ende-stream-terminal-p (ende-messages--session-stream session))
            (ende-request-prepare lifecycle 'ende-messages #'ende-messages--request-body messages profile
                                  (list "anthropic-messages" (plist-get profile :url))
                                  (apply-partially #'ende-messages--start session))))
      ((error quit)
       (ende-messages--fail session (ende-error-create :kind 'startup :message (error-message-string failure)))))
    (ende-transport-session-process lifecycle)))

(provide 'ende-messages)
;;; ende-messages.el ends here
