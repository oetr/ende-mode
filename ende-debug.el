;;; ende-debug.el --- LLM request diagnostics for ende -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools, llm

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

(defvar ende-debugging-enabled nil
  "Non-nil when new streaming LLM requests should be recorded.")

(defvar ende-debug--session-directory nil
  "Directory holding the latest Ende debugging session.")

(defvar ende-debug--sequence 0
  "Next request sequence number in the active debugging session.")

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
  (cl-find name body :key (lambda (entry)
                           (ende-controls--json-key-name (car entry)))
           :test #'equal))

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

(defun ende-debug-record-response (record response)
  "Retain allowlisted provider RESPONSE metadata in debug RECORD.
Never capture raw output or opaque reasoning.  Completion writes this record."
  (when record
    (dolist (key '("model" "service_tier"))
      (let ((value (ende-protocol-json-get key response)))
        (when (stringp value)
          (setf (alist-get (concat "response_" key)
                           (plist-get record :metadata) nil nil #'equal)
                value))))))

(defun ende-debug-record-completion (record outcome usage http-status value)
  "Write RECORD's terminal OUTCOME, normalized USAGE, and HTTP-STATUS.
VALUE is the successful result or structured transport error.  Recording
errors are reported but never propagated into transport completion."
  (when record
    (condition-case failure
        (let* ((counts (ende--usage-token-counts usage))
               (error-kind (and (ende-error-p value)
                                (ende-error-kind value)))
               (state (cond
                       ((eq outcome 'success) "success")
                       ((eq error-kind 'cancelled) "cancelled")
                       (t "failure")))
               (completed (float-time))
               (completion
                `(("outcome" . ,state)
                  ("error_kind" . ,(and error-kind (symbol-name error-kind)))
                  ("http_status" . ,http-status)
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
          (ende-debug--write-meta record))
       ((error quit)
        (message "ende debugging: could not record completion: %s"
                (error-message-string failure))))))

(defun ende-debug-record-request
    (serialized-body request-body messages profile transport endpoint)
  "Record redacted SERIALIZED-BODY and internal MESSAGES for a request.
REQUEST-BODY is the encoded request object used to describe its settings.
PROFILE, TRANSPORT, and ENDPOINT describe the transport.  Return an opaque
record used to associate later completion metadata, or nil when debugging is
disabled."
  (when (and ende-debugging-enabled ende-debug--session-directory)
    (condition-case failure
        (let* ((sequence (cl-incf ende-debug--sequence))
               (stem (format "%04d" sequence))
               (directory ende-debug--session-directory)
               (started (float-time))
               (session (plist-get profile :codex-session-id))
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
                  ("cache_key_supplied" . ,(if (ende-debug--request-field "prompt_cache_key" request-body)
                                               t json-false))
                  ("session_fingerprint" . ,(when (and (stringp session)
                                                      (not (string-empty-p session)))
                                              (secure-hash 'sha256 session)))
                  ("request_settings" . ,(ende-debug--redact
                                           (ende-debug--request-settings request-body)))
                  ("endpoint" . ,(ende-debug--safe-endpoint endpoint))))
               (record (list :directory directory :stem stem
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

(defun ende-debug--cache-line (capture)
  "Return a concise provider cache utilization line for CAPTURE."
  (let* ((metadata (plist-get capture :metadata))
         (completion (ende-protocol-json-get "completion" metadata))
         (input (ende-protocol-json-get "input_tokens" completion))
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
    (format "#%-4d Input: %6s  Cached: %s%s%s"
            sequence
            (if (numberp input) (number-to-string input) "n/a")
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
              "  [pending]"))))

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
  (let* ((old-request (plist-get previous :request))
         (new-request (plist-get current :request))
         (old-meta (plist-get previous :metadata))
         (new-meta (plist-get current :metadata))
         (messages (ende-debug--messages-prefix old-request new-request))
         (prefix-identical (car messages))
         (appended (cadr messages))
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
     (if prefix-identical
         (format "Messages:        %d appended, previous prefix identical\n"
                 appended)
       "Messages:        previous prefix modified, removed, or reordered\n")
     (if difference
         (format "\nFirst changed path:\n  %s\n" difference)
       "\nFirst changed path: none (JSON bodies structurally identical)\n")
     (format "Byte common prefix: %d/%d bytes (supplementary only)\n"
             prefix-bytes old-length)
     (if (and prefix-identical (not instructions-changed)
              (equal tools "unchanged"))
         (concat "\nObserved: no change to instructions, tools, or the prior message prefix.\n"
                 "The provider cache outcome's cause is not determined by this comparison.\n\n")
       (concat "\nObserved: prompt-relevant request structure changed.\n"
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
     "\n\n"
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
