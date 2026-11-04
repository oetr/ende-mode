;;; ende-debug.el --- LLM request diagnostics for ende -*- lexical-binding: t; -*-

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

;; Opt-in recording of the exact request bodies and metadata sent by Ende's
;; streaming LLM transports.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url-parse)
(require 'ende-core)
(require 'ende-content)
(require 'ende-protocol)
(require 'ende-credentials)

(defcustom ende-debug-provider-cache-comparisons nil
  "Request provider cache comparisons while Ende debugging is enabled.
Use the previous completed response in the same job and connection.
Subscription-endpoint support is unverified; rejection follows the normal
provider-error path, without probes or retries.  This does not continue a
server-managed conversation."
  :type 'boolean
  :group 'ende)

(defvar ende-debugging-enabled nil
  "Non-nil when new streaming LLM requests should be recorded.")

(defvar ende-debug--session-directory nil
  "Directory holding the latest Ende debugging session.")

(defvar ende-debug--sequence 0
  "Next request sequence number in the active debugging session.")

(defvar ende-debug--baselines (make-hash-table :test #'equal)
  "Completed provider responses keyed by job and connection in this capture.")

(defun ende-debug--timestamp (&optional time)
  "Format TIME as a local ISO-like timestamp, or the current time."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z" time))

(defun ende-debug--with-file-modes (modes function)
  "Call FUNCTION with newly created files protected by MODES."
  (let ((original-modes (default-file-modes)))
    (unwind-protect
        (progn
          (set-default-file-modes modes)
          (funcall function))
      (set-default-file-modes original-modes))))

(defun ende-debug--new-session-directory ()
  "Create and return a private, unique debugging session directory."
  (make-directory user-emacs-directory t)
  (let* ((prefix (expand-file-name
                  (format-time-string "ende-debug-%Y%m%d-%H%M%S-")
                  user-emacs-directory))
         (path (ende-debug--with-file-modes
                #o700 (lambda () (make-temp-file prefix t)))))
    (set-file-modes path #o700)
    path))

;;;###autoload
(defun ende-debugging-on ()
  "Enable recording of subsequent LLM requests in a new private session."
  (interactive)
  (setq ende-debug--session-directory (ende-debug--new-session-directory)
        ende-debug--sequence 0
        ende-debug--baselines (make-hash-table :test #'equal)
        ende-debugging-enabled t)
  (message "ende debugging enabled; recordings: %s"
           ende-debug--session-directory))

;;;###autoload
(defun ende-debugging-off ()
  "Disable recording of new LLM requests."
  (interactive)
  (setq ende-debugging-enabled nil)
  (message "ende debugging disabled; existing recordings remain in %s"
           (or ende-debug--session-directory "(no debugging session)")))

(defun ende-debug--safe-endpoint (endpoint)
  "Return a credential-free endpoint identification for ENDPOINT."
  (when (stringp endpoint)
    (condition-case nil
        (let* ((parsed (url-generic-parse-url endpoint))
               (scheme (url-type parsed))
               (host (url-host parsed))
               (port (url-port parsed))
               (path (or (url-filename parsed) "/")))
          (setq path (car (split-string path "[?#]")))
          (when (and scheme host)
            (concat scheme "://" host
                    (if (and port
                             (not (or (and (equal scheme "https") (= port 443))
                                      (and (equal scheme "http") (= port 80)))))
                        (format ":%d" port)
                      "")
                    (if (string-empty-p path) "/" path))))
      (error nil))))

(defun ende-debug--request-settings (request-body)
  "Return non-conversation settings from REQUEST-BODY."
  (cl-loop for (key . value) in request-body
           unless (memq key '(:messages :input :instructions :tools :model))
           collect (cons key value)))

(defun ende-debug--request-field (name body)
  "Return the member named NAME from encoded request BODY.
Adapter fields use keywords; user parameters use string keys."
  (if (hash-table-p body)
      (when (ende-protocol-json-has-key-p name body) (cons name (gethash name body)))
    (cl-find name body :key (lambda (entry)
                             (ende-controls--json-key-name (car entry)))
             :test #'equal)))

(defun ende-debug--connection-fingerprint (profile transport endpoint account)
  "Identify PROFILE's connection for TRANSPORT and ENDPOINT privately.
ACCOUNT is resolved OAuth registration continuity, supplied by the transport."
  (let* ((method (plist-get profile :auth-method))
         (credential
          (pcase method
            ('none 'anonymous)
            ('auth-source (gethash (plist-get profile :credential-binding)
                                   ende-credentials--bindings))
            ((or 'codex-oauth 'chatgpt-plan-oauth)
             (and (ende-debug--usable-id-p (plist-get profile :codex-session-id)) account))))
         (provider (or (plist-get profile :name) (plist-get profile :service))))
    (when (and credential provider (stringp endpoint) (not (string-empty-p endpoint)))
      (secure-hash
       'sha256
       (prin1-to-string
        (list transport endpoint credential
              (mapcar (lambda (key) (plist-get profile key))
                      '(:name :service :api :auth-method :account :codex-session-id
                        :auth-source-host :auth-source-port :auth-source-user
                        :auth-scheme :request-headers))))))))

(defun ende-debug-prepare-request (profile transport endpoint account eligible)
  "Reserve immutable capture data for PROFILE in the main process.
TRANSPORT, ENDPOINT, and ACCOUNT scope a baseline when ELIGIBLE is non-nil."
  (let* ((job (plist-get profile :job-id))
         (connection (and eligible
                          (condition-case nil
                              (ende-debug--connection-fingerprint profile transport endpoint account)
                            ((error quit) nil))))
         (key (and connection
                   (or (integerp job) (and (stringp job) (not (string-empty-p job))))
                   (list job connection))))
    (list :directory ende-debug--session-directory :sequence (cl-incf ende-debug--sequence)
          :baseline-key key :connection connection
          :baseline (and key (copy-sequence (gethash key ende-debug--baselines)))
          :compare ende-debug-provider-cache-comparisons)))

(defun ende-debug-prepare-body (body preparation)
  "Return detached (BODY . PREPARATION) with the selected comparison applied.
An explicitly supplied comparison member, including null, is never replaced."
  (let* ((preparation (copy-sequence preparation))
         (entry (ende-debug--request-field "prompt_cache_options" body))
         (options (cdr entry))
         (explicit (and (or (hash-table-p options) (listp options))
                        (ende-debug--request-field "comparison_response_id" options)))
         (baseline (plist-get preparation :baseline))
         (identifier (plist-get baseline :response-id)))
    (cond
     (explicit
      (setq preparation (plist-put preparation :comparison-source "explicit"))
      (when (equal (cdr explicit) identifier)
        (setq preparation (plist-put preparation :comparison-sequence (plist-get baseline :sequence)))))
     ((and identifier (plist-get preparation :compare)
           (or (not entry) (hash-table-p options) (consp options)))
      (setq options (if (hash-table-p options) (copy-hash-table options)
                      (if entry (copy-tree options) (make-hash-table :test #'equal))))
      (if (hash-table-p options) (puthash "comparison_response_id" identifier options)
        (setq options (append options (list (cons :comparison_response_id identifier)))))
      (setq body (append (cl-remove "prompt_cache_options" body
                                   :key (lambda (item) (ende-controls--json-key-name (car item)))
                                   :test #'equal)
                         (list (cons :prompt_cache_options options)))
            preparation (plist-put preparation :comparison-source "automatic")
            preparation (plist-put preparation :comparison-sequence (plist-get baseline :sequence)))))
    (cons body preparation)))

(defun ende-debug--advance-baseline (record outcome)
  "Advance RECORD's job baseline only after a successful completed OUTCOME."
  (let* ((key (plist-get record :baseline-key))
         (metadata (plist-get record :metadata))
         (identifier (alist-get "provider_response_id" metadata nil nil #'equal))
         (sequence (alist-get "sequence" metadata nil nil #'equal))
         (previous (and key (gethash key ende-debug--baselines))))
    (when (and (eq outcome 'success) key (plist-get record :response-completed)
               (equal (plist-get record :directory) ende-debug--session-directory)
               (ende-debug--usable-id-p identifier)
               (> sequence (or (plist-get previous :sequence) 0)))
      (puthash key (list :response-id identifier :sequence sequence) ende-debug--baselines))))

(defun ende-debug--write-file (path contents &optional literal)
  "Write CONTENTS to PATH with owner-only permissions.
When LITERAL is non-nil, preserve CONTENTS as exact bytes."
  (ende-debug--with-file-modes
   #o600
   (lambda ()
     (let ((coding-system-for-write (if literal 'no-conversion 'utf-8-unix)))
       (write-region contents nil path nil 'silent))))
  (set-file-modes path #o600))

(defun ende-debug--write-meta (record)
  "Write RECORD's metadata to its private JSON file."
  (let* ((directory (plist-get record :directory))
         (stem (plist-get record :stem))
         (meta (plist-get record :metadata)))
    (ende-debug--write-file
     (expand-file-name (concat stem "-meta.json") directory)
     (concat (json-encode meta) "\n"))))

(defun ende-debug--redact (value)
  "Redact VALUE for request debugging, retaining encrypted reasoning."
  (let ((ende-content--debug-preserve-encrypted-content t))
    (ende-content-redact value)))

(defun ende-debug--structural-json (value)
  "Encode parsed JSON VALUE deterministically for diagnostic hashing."
  (cond
   ((hash-table-p value)
    (concat "{"
            (mapconcat (lambda (key)
                         (concat (json-encode key) ":"
                                 (ende-debug--structural-json (gethash key value))))
                       (sort (hash-table-keys value) #'string<) ",")
            "}"))
   ((and (vectorp value) (not (stringp value)))
    (concat "[" (mapconcat #'ende-debug--structural-json value ",") "]"))
   (t (let ((json-false :false)
            (json-encoding-pretty-print nil))
        (json-encode value)))))

(defun ende-debug--input-fingerprints (serialized-body)
  "Fingerprint original input values in final outgoing SERIALIZED-BODY.
Lengths describe the UTF-8 diagnostic encoding, not provider tokenization."
  (let* ((request (ende-protocol-json-read-string
                   (if (multibyte-string-p serialized-body) serialized-body
                     (decode-coding-string serialized-body 'utf-8))))
         (field (cond ((ende-protocol-json-has-key-p "input" request) "input")
                      ((ende-protocol-json-has-key-p "messages" request) "messages")))
         (items (ende-protocol-json-get field request)))
    (when (ende-debug--json-array-p items)
      `(("format" . "structural-input-sha256-v1")
        ("field" . ,field)
        ("count" . ,(length items))
        ("items" . ,(cl-map 'vector
                           (lambda (item index)
                             (let ((bytes (encode-coding-string
                                           (ende-debug--structural-json item) 'utf-8)))
                               (append
                                `(("index" . ,index)
                                  ("sha256" . ,(secure-hash 'sha256 bytes))
                                  ("byte_length" . ,(length bytes)))
                                (cl-loop for key in '("type" "role" "id" "call_id")
                                         for value = (ende-protocol-json-get key item)
                                         when (stringp value) collect (cons key value)))))
                           items (number-sequence 0 (1- (length items)))))))))

(defun ende-debug--record-field (record key value)
  "Set metadata KEY to VALUE in RECORD."
  (setf (alist-get key (plist-get record :metadata) nil nil #'equal) value))

(defun ende-debug--usable-id-p (value)
  "Return non-nil for a nonempty, bounded provider identifier VALUE."
  (and (stringp value) (< 0 (length value) 513)
       (not (string-match-p "[[:space:][:cntrl:]]" value))
       (equal value (ende-debug--redact value))))

(defun ende-debug-record-http-request-id (record identifier)
  "Retain only the selected HTTP request IDENTIFIER in RECORD."
  (when (and record (ende-debug--usable-id-p identifier))
    (ende-debug--record-field record "http_request_id" identifier)))

(defun ende-debug-record-response (record response &optional completed)
  "Retain allowlisted provider RESPONSE metadata in debug RECORD.
COMPLETED marks a completed response object, not an intermediate event.
Never capture raw output or opaque reasoning.  Completion writes this record."
  (when record
    (condition-case nil
        (progn
          (when-let* ((id (ende-protocol-json-get "id" response))
                      (_ (ende-debug--usable-id-p id)))
            (ende-debug--record-field record "provider_response_id" id))
          (dolist (key '("model" "service_tier"))
            (let ((value (ende-protocol-json-get key response)))
              (when (stringp value)
                (ende-debug--record-field record (concat "response_" key)
                                          (ende-debug--redact value)))))
          (let ((reasoning (ende-protocol-json-get "reasoning" response))
                (settings (alist-get "response_reasoning" (plist-get record :metadata)
                                     nil nil #'equal)))
            (dolist (key '("effort" "summary" "context"))
              (let ((value (ende-protocol-json-get key reasoning)))
                (when (stringp value)
                  (setf (alist-get key settings nil nil #'equal) (ende-debug--redact value)))))
            (when settings (ende-debug--record-field record "response_reasoning" settings)))
          (when completed
            (setf (plist-get record :response-completed) t)
            (let* ((diagnostics (ende-protocol-json-get "prompt_cache_diagnostics" response))
                   (selected (alist-get "prompt_cache_diagnostics" (plist-get record :metadata)
                                        nil nil #'equal))
                   (output (ende-protocol-json-get "output" response))
                   (tokens (ende-protocol-json-get
                            "reasoning_tokens"
                            (ende-protocol-json-get
                             "output_tokens_details" (ende-protocol-json-get "usage" response)))))
              (dolist (key '("type" "reason" "comparison_reusable_tokens" "cache_missed_tokens"))
                (let ((value (ende-protocol-json-get key diagnostics)))
                  (when (if (member key '("type" "reason"))
                            (member value '("cache_hit" "cache_miss" "unavailable"
                                            "comparison_response_not_found" "model_changed"
                                            "prompt_cache_key_changed" "tools_changed"
                                            "text_format_changed" "reasoning_effort_changed"
                                            "verbosity_changed" "context_compacted"
                                            "input_changed" "service_tier_changed"))
                          (and (integerp value) (>= value 0)))
                    (setf (alist-get key selected nil nil #'equal) value))))
              (when selected (ende-debug--record-field record "prompt_cache_diagnostics" selected))
              (when (ende-debug--json-array-p output)
                (ende-debug--record-field
                 record "output_reasoning_items"
                 (cl-count "reasoning" output
                           :key (lambda (item) (ende-protocol-json-get "type" item)) :test #'equal)))
              (when (and (integerp tokens) (>= tokens 0))
                (ende-debug--record-field record "response_reasoning_tokens" tokens)))))
      ((error quit) nil))))

(defun ende-debug-record-completion (record outcome usage http-status value)
  "Write RECORD's terminal OUTCOME, normalized USAGE, and HTTP-STATUS.
VALUE is the successful result or structured transport error.  Recording
errors are reported but never propagated into transport completion."
  (when (and record (not (plist-get record :finished)))
    (condition-case failure
        (let* ((counts (ende--usage-token-counts usage))
               (error-kind (and (ende-error-p value)
                                (ende-error-kind value)))
               (error-details (and (ende-error-p value)
                                   (listp (ende-error-details value))
                                   (ende-error-details value)))
               (state (cond
                       ((eq outcome 'success) "success")
                       ((eq error-kind 'cancelled) "cancelled")
                       (t "failure")))
               (completed (float-time))
               (completion
                `(("outcome" . ,state)
                  ("error_kind" . ,(and error-kind (symbol-name error-kind)))
                  ("http_status" . ,http-status)
                  ("error_code" . ,(when (stringp (plist-get error-details :code))
                                       (ende-debug--redact (plist-get error-details :code))))
                  ("error_param" . ,(when (stringp (plist-get error-details :param))
                                        (ende-debug--redact (plist-get error-details :param))))
                  ("completed_timestamp" . ,(ende-debug--timestamp))
                  ("completion_time_epoch" . ,completed)
                  ("elapsed_seconds" . ,(- completed
                                            (plist-get record :started)))
                  ("input_tokens" . ,(plist-get counts :input))
                  ("output_tokens" . ,(plist-get counts :output))
                  ("cached_input_tokens" . ,(plist-get counts :cached))
                  ("cached_tokens_reported" . ,(if (numberp
                                                    (plist-get counts :cached))
                                                   t json-false))
                  ("cache_write_tokens" . ,(plist-get counts :cache-write))
                  ("cache_write_tokens_reported" . ,(if (numberp
                                                        (plist-get counts
                                                                   :cache-write))
                                                       t json-false))))
               (metadata (plist-get record :metadata)))
          (setf (plist-get record :metadata)
                (append (cl-remove-if
                         (lambda (entry)
                           (equal (car entry) "completion"))
                         metadata)
                        (list (cons "completion" completion))))
          (setf (plist-get record :finished) t)
          (ende-debug--advance-baseline record outcome)
          (ende-debug--write-meta record))
       ((error quit)
        (message "ende debugging: could not record completion: %s"
                (error-message-string failure))))))

(defun ende-debug-record-request
    (serialized-body request-body messages profile transport endpoint &optional preparation)
  "Record redacted SERIALIZED-BODY and internal MESSAGES for a request.
REQUEST-BODY is the encoded request object used to describe its settings.
PROFILE, TRANSPORT, and ENDPOINT describe the transport; PREPARATION was
resolved in the main process before encoding.  Return an opaque
record used to associate later completion metadata, or nil when debugging is
disabled."
  (when (and ende-debugging-enabled ende-debug--session-directory)
    (condition-case failure
        (let* ((sequence (or (plist-get preparation :sequence) (cl-incf ende-debug--sequence)))
               (stem (format "%04d" sequence))
               (directory (or (plist-get preparation :directory) ende-debug--session-directory))
               (started (float-time))
               (session (plist-get profile :codex-session-id))
               (fingerprints (ende-debug--input-fingerprints serialized-body))
               (comparison (ende-debug--request-field
                            "comparison_response_id"
                            (cdr (ende-debug--request-field "prompt_cache_options" request-body))))
               (reasoning
                (cl-loop for (key . value) in request-body
                         when (memq key '(:reasoning :reasoning_effort))
                         collect (cons key value)))
               (metadata
                `(("recording_id" . ,(format "%s-%s"
                                             (file-name-nondirectory
                                              (directory-file-name directory))
                                             stem))
                  ("sequence" . ,sequence)
                  ("timestamp" . ,(ende-debug--timestamp))
                  ("start_time_epoch" . ,started)
                  ("job_id" . ,(plist-get profile :job-id))
                  ("transport" . ,transport)
                  ("provider" . ,(format "%s"
                                         (or (plist-get profile :name)
                                             transport)))
                  ("model" . ,(or (alist-get :model request-body)
                                  (plist-get profile :model)))
                  ("reasoning_effort" . ,(or (plist-get profile :reasoning-effort)
                                               (alist-get :reasoning_effort
                                                          request-body)))
                  ("reasoning_settings" . ,reasoning)
                  ("input_fingerprints" . ,fingerprints)
                  ("input_reasoning_items" . ,(when fingerprints
                                                (cl-count "reasoning" (cdr (assoc "items" fingerprints))
                                                          :key (lambda (item) (cdr (assoc "type" item)))
                                                          :test #'equal)))
                  ("connection_fingerprint" . ,(plist-get preparation :connection))
                  ("comparison_response_id" . ,(when (stringp (cdr comparison)) (cdr comparison)))
                  ("comparison_source" . ,(or (plist-get preparation :comparison-source)
                                               (and comparison "explicit")))
                  ("comparison_request_sequence" . ,(plist-get preparation :comparison-sequence))
                  ("cache_key_supplied" . ,(if (ende-debug--request-field "prompt_cache_key" request-body)
                                               t json-false))
                  ("session_fingerprint" . ,(when (and (stringp session)
                                                      (not (string-empty-p session)))
                                              (secure-hash 'sha256 session)))
                  ("request_settings" . ,(ende-debug--redact
                                           (ende-debug--request-settings request-body)))
                  ("endpoint" . ,(ende-debug--safe-endpoint endpoint))))
               (record (list :directory directory :stem stem
                             :baseline-key (plist-get preparation :baseline-key)
                             :response-completed nil :finished nil
                             :started started :metadata metadata)))
          (ende-debug--write-file
           (expand-file-name (concat stem "-request.json") directory)
           (let ((safe (json-encode (ende-debug--redact request-body))))
             (if (equal safe (json-encode request-body)) serialized-body
               safe)) t)
          (ende-debug--write-file
           (expand-file-name (concat stem "-messages.el") directory)
           (let ((print-circle t)
                 (print-level nil)
                 (print-length nil))
             (concat (prin1-to-string (ende-debug--redact messages)) "\n")))
          (setf (plist-get record :metadata) (ende-debug--redact metadata))
          (ende-debug--write-meta record)
          record)
      ((error quit)
       (message "ende debugging: could not record request: %s"
                (error-message-string failure))
       nil))))

(defun ende-debug--latest-session-directory ()
  "Return the latest available debugging session directory."
  (or (and (stringp ende-debug--session-directory)
           (file-directory-p ende-debug--session-directory)
           ende-debug--session-directory)
      (let ((directories
             (and (file-directory-p user-emacs-directory)
                  (cl-remove-if-not
                   #'file-directory-p
                   (directory-files user-emacs-directory t
                                    "^ende-debug-[0-9]" t)))))
        (car (last
              (sort directories
                    (lambda (left right)
                      (string< (file-name-nondirectory left)
                               (file-name-nondirectory right)))))))))

(defun ende-debug--read-bytes (path)
  "Read PATH literally as an unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defun ende-debug--capture-sequence (path)
  "Return the numeric sequence encoded in metadata PATH."
  (let ((name (file-name-nondirectory path)))
    (when (string-match "\\`\\([0-9]+\\)-meta\\.json\\'" name)
      (string-to-number (match-string 1 name)))))

(defun ende-debug--read-capture (directory sequence)
  "Read request SEQUENCE from DIRECTORY into a comparison record."
  (let* ((stem (format "%04d" sequence))
         (meta-path (expand-file-name (concat stem "-meta.json") directory))
         (request-path (expand-file-name (concat stem "-request.json") directory))
         (bytes (ende-debug--read-bytes request-path))
         (request (ende-protocol-json-read-string
                   (decode-coding-string bytes 'utf-8))))
    (list :sequence sequence :bytes bytes :request request
          :metadata
          (ende-protocol-json-read-string
           (with-temp-buffer
             (insert-file-contents meta-path)
             (buffer-string))))))

(defun ende-debug--read-captures (directory)
  "Return readable request captures in DIRECTORY, sorted by sequence."
  (let ((paths (directory-files directory t "^[0-9]+-meta\\.json$" t))
        captures)
    (dolist (path paths)
      (let ((sequence (ende-debug--capture-sequence path)))
        (when sequence
          (condition-case nil
              (push (ende-debug--read-capture directory sequence) captures)
            (error nil)))))
    (sort captures (lambda (left right)
                     (< (plist-get left :sequence)
                        (plist-get right :sequence))))))

(defun ende-debug--json-array-list (value)
  "Return JSON array VALUE as a list, or nil when VALUE is not an array."
  (and (ende-debug--json-array-p value) (append value nil)))

(defun ende-debug--json-array-p (value)
  "Return non-nil if VALUE is a JSON array vector."
  (and (vectorp value) (not (stringp value))))

(defun ende-debug--object-keys (object)
  "Return sorted string keys of JSON OBJECT."
  (when (hash-table-p object)
    (sort (hash-table-keys object) #'string<)))

(defun ende-debug--path-key (path key)
  "Append object KEY to diagnostic PATH."
  (if (string-empty-p (or path "")) key (concat path "." key)))

(defun ende-debug--path-index (path index)
  "Append array INDEX to diagnostic PATH."
  (format "%s[%d]" (or path "") index))

(defun ende-debug--first-difference (left right &optional path)
  "Return the first structural JSON difference between LEFT and RIGHT.
PATH is the current JSON path."
  (unless (equal left right)
    (cond
     ((and (hash-table-p left) (hash-table-p right))
      (let ((keys (sort (delete-dups
                         (append (ende-debug--object-keys left)
                                 (ende-debug--object-keys right)))
                        #'string<))
            difference)
        (while (and keys (not difference))
          (let ((key (pop keys)))
            (if (not (and (ende-protocol-json-has-key-p key left)
                          (ende-protocol-json-has-key-p key right)))
                (setq difference (ende-debug--path-key path key))
              (setq difference
                    (ende-debug--first-difference
                     (ende-protocol-json-get key left)
                     (ende-protocol-json-get key right)
                     (ende-debug--path-key path key))))))
        difference))
     ((and (ende-debug--json-array-p left)
           (ende-debug--json-array-p right))
      (let* ((shared (min (length left) (length right)))
             (index 0)
             difference)
        (while (and (< index shared) (not difference))
          (setq difference
                (ende-debug--first-difference
                 (aref left index) (aref right index)
                 (ende-debug--path-index path index)))
          (cl-incf index))
        (or difference
            (and (/= (length left) (length right))
                 (ende-debug--path-index path shared)))))
     (t (if (string-empty-p (or path "")) "$" path)))))

(defun ende-debug--json-field (object key)
  "Return JSON field KEY from OBJECT, preserving absent versus null."
  (if (ende-protocol-json-has-key-p key object)
      (cons t (ende-protocol-json-get key object))
    (cons nil nil)))

(defun ende-debug--same-json-field-p (left right key)
  "Return non-nil when JSON field KEY has the same presence and value."
  (let ((old (ende-debug--json-field left key))
        (new (ende-debug--json-field right key)))
    (and (eq (car old) (car new))
         (or (not (car old))
             (not (ende-debug--first-difference (cdr old) (cdr new)))))))

(defun ende-debug--json-equal-p (left right)
  "Return non-nil when LEFT and RIGHT are structurally equal JSON values."
  (not (ende-debug--first-difference left right)))

(defun ende-debug--instruction-content (request)
  "Return top-level and system/developer instruction fields from REQUEST."
  (let* ((key (if (ende-protocol-json-has-key-p "messages" request)
                  "messages" "input"))
         (messages (ende-debug--json-array-list
                    (ende-protocol-json-get key request)))
         (instruction-field (ende-debug--json-field request "instructions"))
         (instructions (vector (car instruction-field)
                               (cdr instruction-field)))
         (roles
          (cl-remove-if-not
           (lambda (message)
             (member (ende-protocol-json-get "role" message)
                     '("system" "developer")))
           messages)))
    (vector instructions (vconcat roles))))

(defun ende-debug--messages-prefix (previous current)
  "Compare message arrays in PREVIOUS and CURRENT request objects.
Return (PREFIX-IDENTICAL APPENDED-COUNT)."
  (let* ((key (cond
               ((ende-protocol-json-has-key-p "messages" previous) "messages")
               ((ende-protocol-json-has-key-p "input" previous) "input")
               ((ende-protocol-json-has-key-p "messages" current) "messages")
               ((ende-protocol-json-has-key-p "input" current) "input")))
         (old-value (and key (ende-protocol-json-get key previous)))
         (new-value (and key (ende-protocol-json-get key current)))
         (old-array (and key (ende-debug--json-array-p old-value)))
         (new-array (and key (ende-debug--json-array-p new-value))))
    (cond
     ((and (null key) (ende-debug--json-equal-p previous current))
      (list t 0))
     ((and key (not old-array) (not new-array)
           (ende-debug--same-json-field-p previous current key))
      (list t 0))
     ((and key old-array new-array
           (<= (length old-value) (length new-value))
           (cl-loop for old-message across old-value
                    for new-message across new-value
                    always (ende-debug--json-equal-p old-message new-message)))
      (list t (- (length new-value) (length old-value))))
     (t (list nil 0)))))

(defun ende-debug--common-byte-prefix-length (left right)
  "Return the number of equal leading bytes in LEFT and RIGHT."
  (let ((limit (min (length left) (length right)))
        (index 0))
    (while (and (< index limit) (= (aref left index) (aref right index)))
      (cl-incf index))
    index))

(defun ende-debug--valid-fingerprints (capture)
  "Return complete, supported structural input fingerprints in CAPTURE."
  (let* ((fingerprints (ende-protocol-json-get
                        "input_fingerprints" (plist-get capture :metadata)))
         (field (ende-protocol-json-get "field" fingerprints))
         (count (ende-protocol-json-get "count" fingerprints))
         (items (ende-protocol-json-get "items" fingerprints))
         (input (ende-protocol-json-get field (plist-get capture :request))))
    (when (and (equal (ende-protocol-json-get "format" fingerprints)
                      "structural-input-sha256-v1")
               (member field '("input" "messages"))
               (integerp count) (>= count 0)
               (ende-debug--json-array-p input) (= count (length input))
               (ende-debug--json-array-p items) (= count (length items))
               (cl-loop for item across items for index from 0
                        for digest = (ende-protocol-json-get "sha256" item)
                        for bytes = (ende-protocol-json-get "byte_length" item)
                        always (and (equal (ende-protocol-json-get "index" item) index)
                                    (stringp digest)
                                    (string-match-p "\\`[0-9a-f]\\{64\\}\\'" digest)
                                    (integerp bytes) (>= bytes 0))))
      fingerprints)))

(defun ende-debug--input-comparison (previous current)
  "Compare original input fingerprints in PREVIOUS and CURRENT captures.
Fall back to saved values when fingerprints cannot establish equality."
  (let ((old (ende-debug--valid-fingerprints previous))
        (new (ende-debug--valid-fingerprints current)))
    (if (not (and old new
                  (equal (gethash "field" old) (gethash "field" new))))
        (let ((saved (ende-debug--messages-prefix
                      (plist-get previous :request) (plist-get current :request))))
          (list :preserved (car saved) :appended (cadr saved)))
      (let* ((old-items (gethash "items" old))
             (new-items (gethash "items" new))
             (limit (min (length old-items) (length new-items)))
             (index 0))
        (while (and (< index limit)
                    (equal (gethash "sha256" (aref old-items index))
                           (gethash "sha256" (aref new-items index)))
                    (equal (gethash "byte_length" (aref old-items index))
                           (gethash "byte_length" (aref new-items index))))
          (cl-incf index))
        (let ((preserved (= index (length old-items))))
          (list :original t :field (gethash "field" old) :preserved preserved
                :changed-index (unless preserved index)
                :appended (if preserved (- (length new-items) index) 0)
                :appended-reasoning
                (when preserved
                  (cl-count "reasoning" new-items :start index
                            :key (lambda (item) (gethash "type" item)) :test #'equal))))))))

(defun ende-debug--history-request (request)
  "Detach REQUEST, excluding the diagnostic comparison identifier."
  (let* ((copy (copy-hash-table request))
         (options (gethash "prompt_cache_options" copy)))
    (when (hash-table-p options)
      (setq options (copy-hash-table options))
      (remhash "comparison_response_id" options)
      (if (zerop (hash-table-count options)) (remhash "prompt_cache_options" copy)
        (puthash "prompt_cache_options" options copy)))
    copy))

(defun ende-debug--cache-line (capture)
  "Return a concise provider cache utilization line for CAPTURE."
  (let* ((metadata (plist-get capture :metadata))
         (completion (ende-protocol-json-get "completion" metadata))
         (input (ende-protocol-json-get "input_tokens" completion))
         (output (ende-protocol-json-get "output_tokens" completion))
         (cached (ende-protocol-json-get "cached_input_tokens" completion))
         (cache-write
          (ende-protocol-json-get "cache_write_tokens" completion))
         (sequence (plist-get capture :sequence))
         (cache-description
          (if (numberp cached)
              (format "%d (%s)"
                      cached
                      (if (and (numberp input) (> input 0))
                          (format "%.1f%%" (* 100.0 (/ (float cached) input)))
                        "n/a"))
            "not reported")))
    (format "#%-4d Job: %s  Input: %6s  Output: %s  Cached: %s%s%s%s"
            sequence
            (or (ende-protocol-json-get "job_id" metadata) "unknown")
            (if (numberp input) (number-to-string input) "n/a")
            (if (numberp output) (number-to-string output) "n/a")
            cache-description
            (if (numberp cache-write)
                (format "  Cache write: %d" cache-write)
              "")
            (if completion
                (format "  [%s%s]"
                        (or (ende-protocol-json-get "outcome" completion)
                            "unknown")
                        (if (numberp (ende-protocol-json-get
                                     "http_status" completion))
                            (format ", HTTP %d"
                                    (ende-protocol-json-get
                                     "http_status" completion))
                          ""))
              "  [pending]")
            (ende-debug--response-lines capture))))

(defun ende-debug--response-lines (capture)
  "Describe selected provider identifiers and reasoning for CAPTURE."
  (let* ((metadata (plist-get capture :metadata))
         (diagnostics (ende-protocol-json-get "prompt_cache_diagnostics" metadata))
         (comparison (ende-protocol-json-get "comparison_response_id" metadata))
         (type (ende-protocol-json-get "type" diagnostics)))
    (concat
     (mapconcat
      (lambda (field)
        (when-let* ((value (ende-protocol-json-get (car field) metadata)))
          (format "\n  %s: %s" (cdr field) (if (hash-table-p value) (json-encode value) value))))
      '(("provider_response_id" . "Provider response ID")
        ("http_request_id" . "HTTP x-request-id")
        ("comparison_response_id" . "Comparison response ID sent")
        ("comparison_source" . "Comparison source")
        ("comparison_request_sequence" . "Comparison baseline local sequence")
        ("response_model" . "Returned model")
        ("response_service_tier" . "Returned service tier")
        ("response_reasoning" . "Effective reasoning")) "")
     (format "\n  Reasoning already sent in this request: %s items; new reasoning produced by this response: %s items; reasoning tokens: %s"
             (or (ende-protocol-json-get "input_reasoning_items" metadata) "unknown")
             (or (ende-protocol-json-get "output_reasoning_items" metadata) "unknown")
             (or (ende-protocol-json-get "response_reasoning_tokens" metadata) "not reported"))
     (format "\n  Provider diagnostic: %s%s"
             (or type (if comparison "requested, not returned" "not returned"))
             (mapconcat
              (lambda (key)
                (when-let* ((value (ende-protocol-json-get key diagnostics)))
                  (format "; %s=%s" key value)))
              '("reason" "comparison_reusable_tokens" "cache_missed_tokens") "")))))

(defun ende-debug--cache-summary (capture)
  "Return a concise cache percentage for CAPTURE."
  (let* ((completion
          (ende-protocol-json-get "completion"
                                  (plist-get capture :metadata)))
         (input (ende-protocol-json-get "input_tokens" completion))
         (cached (ende-protocol-json-get "cached_input_tokens" completion)))
    (cond
     ((null completion) "pending")
     ((not (numberp cached)) "not reported")
     ((and (numberp input) (> input 0))
      (format "%.1f%% (%d/%d)"
              (* 100.0 (/ (float cached) input)) cached input))
     (t (format "%d cached" cached)))))

(defun ende-debug--changed-label (left right key)
  "Return `changed' or `unchanged' for JSON field KEY in LEFT/RIGHT."
  (if (ende-debug--same-json-field-p left right key)
      "unchanged" "changed"))

(defun ende-debug--capture-identity (capture &optional session)
  "Return CAPTURE's known job ID, or scoped session identity if SESSION."
  (let ((metadata (plist-get capture :metadata)))
    (if session
        (let ((identity (mapcar (lambda (key)
                                 (ende-protocol-json-get key metadata))
                               '("provider" "transport" "endpoint"
                                 "session_fingerprint"))))
          (and (cl-every (lambda (value)
                          (and (stringp value) (not (string-empty-p value))))
                        identity)
               identity))
      (let ((job (ende-protocol-json-get "job_id" metadata)))
        (and (or (numberp job)
                 (and (stringp job) (not (string-empty-p job))))
             job)))))

(defun ende-debug--pair-report (previous current)
  "Return a concise structural comparison of PREVIOUS and CURRENT captures."
  (let* ((old-request (ende-debug--history-request (plist-get previous :request)))
         (new-request (ende-debug--history-request (plist-get current :request)))
         (old-meta (plist-get previous :metadata))
         (new-meta (plist-get current :metadata))
         (messages (ende-debug--input-comparison previous current))
         (original (plist-get messages :original))
         (prefix-identical (plist-get messages :preserved))
         (appended (plist-get messages :appended))
         (field (or (plist-get messages :field)
                    (if (gethash "input" old-request) "input" "messages")))
         (changed-index (plist-get messages :changed-index))
         (tools (ende-debug--changed-label old-request new-request "tools"))
         (instructions-changed
          (not (ende-debug--json-equal-p
                (ende-debug--instruction-content old-request)
                (ende-debug--instruction-content new-request))))
         (settings-changed
          (not (and (ende-debug--same-json-field-p
                     old-request new-request "reasoning")
                    (ende-debug--same-json-field-p
                     old-request new-request "reasoning_effort")
                    (equal (ende-protocol-json-get
                            "reasoning_effort" old-meta)
                           (ende-protocol-json-get
                            "reasoning_effort" new-meta)))))
         (model (ende-debug--changed-label old-request new-request "model"))
         (reasoning (if settings-changed "changed" "unchanged"))
         (difference (ende-debug--first-difference old-request new-request))
         (prefix-bytes
          (ende-debug--common-byte-prefix-length
           (plist-get previous :bytes) (plist-get current :bytes)))
         (old-length (length (plist-get previous :bytes)))
         (old-job (ende-debug--capture-identity previous))
         (new-job (ende-debug--capture-identity current))
         (old-session (ende-debug--capture-identity previous t))
         (new-session (ende-debug--capture-identity current t)))
    (concat
     (format "Request #%d -> #%d%s\n\n"
             (plist-get previous :sequence) (plist-get current :sequence)
             (cond
              ((and old-job new-job (equal old-job new-job))
               (format " (job %s)" new-job))
              ((or old-job new-job)
               (format " (job %s -> job %s)"
                       (or old-job "unknown") (or new-job "unknown")))
              (t "")))
     (format "Previous cache: %s\nCurrent cache:  %s\n\n"
             (ende-debug--cache-summary previous)
             (ende-debug--cache-summary current))
     (format "Model:           %s\nReasoning:       %s\nTools:           %s\nInstructions:    %s\n"
             model reasoning tools
             (if instructions-changed "changed" "unchanged"))
     (format "Session:         %s\n"
             (cond
              ((not (and old-session new-session)) "unknown")
              ((equal old-session new-session) "unchanged")
              (t "changed")))
     (mapconcat
      (lambda (key)
        (when (and (ende-protocol-json-has-key-p key old-meta)
                   (ende-protocol-json-has-key-p key new-meta)
                   (not (ende-debug--same-json-field-p old-meta new-meta key)))
          (format "Returned %s changed: %s -> %s\n" key
                  (json-encode (gethash key old-meta))
                  (json-encode (gethash key new-meta)))))
      '("response_model" "response_service_tier" "response_reasoning") "")
     (if prefix-identical
         (format "Local input:     %s; %d appended%s\n"
                 (if original "previous original input prefix was preserved"
                   "saved request prefix identical")
                 appended
                 (if original (format " (%d reasoning)"
                                      (plist-get messages :appended-reasoning)) ""))
       (format "Local input:     %s prefix modified, removed, or reordered\n"
               (if original "original input" "saved request")))
     (unless original
       "Limitation: original fingerprints absent, incompatible, or incomplete; saved values cannot establish equality of redacted originals.\n")
     (cond
      (changed-index
       (format "Changed input item: %s[%d]%s\n"
               field changed-index
               (if (cl-every
                    (lambda (request)
                      (let ((items (gethash field request)))
                        (and (< changed-index (length items))
                             (string-match-p "redacted"
                                             (json-encode (aref items changed-index))))))
                    (list old-request new-request))
                   " (contents redacted)" "")))
      ((and prefix-identical (> appended 0))
       (format "First appended item: %s[%d]\n" field
               (- (length (gethash field new-request)) appended)))
      (difference (format "First changed path: %s\n" difference))
      (t "First changed path: none in saved JSON\n"))
     (format "Byte common prefix: %d/%d bytes (supplementary only)\n"
             prefix-bytes old-length)
     (if (and original prefix-identical (not instructions-changed)
              (equal tools "unchanged"))
         (concat "\nObserved: no change to instructions, tools, or the prior message prefix.\n"
                 "The provider cache outcome's cause is not determined by this comparison.\n\n")
       (concat (if (and prefix-identical (not original))
                   "\nObserved: saved input prefix matches; concealed original contents remain unknown.\n"
                 "\nObserved: prompt-relevant request structure changed.\n")
               "This comparison does not establish whether that change caused a cache miss.\n\n")))))

(defun ende-debug--capture-groups (captures &optional session)
  "Group CAPTURES by known job ID, preserving sequence order.
When SESSION is non-nil, group by known provider-scoped session identity."
  (let (groups)
    (dolist (capture captures)
      (when-let* ((identity (ende-debug--capture-identity capture session)))
        (let ((group (cl-assoc identity groups :test #'equal)))
          (if group
              (setcdr group (cons capture (cdr group)))
            (push (cons identity (list capture)) groups)))))
    (dolist (group groups)
      (setcdr group (nreverse (cdr group))))
    (nreverse groups)))

(defun ende-debug--format-report (directory captures)
  "Format a readable report for CAPTURES in DIRECTORY."
  (let (pairs)
    (dolist (session '(nil t))
      (dolist (group (ende-debug--capture-groups captures session))
        (let ((requests (cdr group)))
          (while (cdr requests)
            (let ((old-job (ende-debug--capture-identity (car requests)))
                  (new-job (ende-debug--capture-identity (cadr requests))))
              (when (or (not session)
                        (and old-job new-job (not (equal old-job new-job))))
                (push (cons (car requests) (cadr requests)) pairs)))
            (setq requests (cdr requests))))))
    (concat
     (format "Ende cache report\nSession: %s\n\n"
             (abbreviate-file-name directory))
     "Provider-reported usage (missing cached-token data is not zero):\n"
     (mapconcat #'ende-debug--cache-line captures "\n")
     (let ((input 0) (cached 0) (reported 0))
       (dolist (capture captures)
         (let* ((completion (ende-protocol-json-get "completion" (plist-get capture :metadata)))
                (tokens (ende-protocol-json-get "input_tokens" completion))
                (hits (ende-protocol-json-get "cached_input_tokens" completion)))
           (when (and (numberp tokens) (numberp hits))
             (cl-incf reported)
             (cl-incf input tokens)
             (cl-incf cached hits))))
       (if (> input 0)
           (format "\nWeighted cache reuse: %.1f%% (%d/%d tokens; %d requests with both counts)"
                   (* 100.0 (/ (float cached) input)) cached input reported)
         "\nWeighted cache reuse: not reported"))
     "\n\nStructural input fingerprints compare represented values, not provider tokenization.\nA provider cache_hit does not mean all current input tokens were cached; usage measures reuse.\n\n"
     (if pairs
         (concat "Consecutive requests within each job or known provider session:\n\n"
                 (mapconcat
                  (lambda (pair)
                    (ende-debug--pair-report (car pair) (cdr pair)))
                  (nreverse pairs) ""))
       "No consecutive requests with a known job or provider session to compare.\n"))))

;;;###autoload
(defun ende-debug-cache-report ()
  "Display cache usage and structural comparisons for the latest capture."
  (interactive)
  (let* ((directory (ende-debug--latest-session-directory))
         (captures (and directory (ende-debug--read-captures directory))))
    (unless directory
      (user-error "No Ende debugging session is available"))
    (unless captures
      (user-error "No readable request captures in %s" directory))
    (let ((buffer (get-buffer-create "*Ende Cache Report*")))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (ende-debug--format-report directory captures))
          (goto-char (point-min)))
        (special-mode))
      (pop-to-buffer buffer))))

(provide 'ende-debug)

;;; ende-debug.el ends here
