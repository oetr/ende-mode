;;; deslop-llm.el --- LLM streaming client and parser for deslop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools

;;; Commentary:

;; Incremental SSE/reasoning parsing and an asynchronous OpenAI-compatible
;; transport.  The transport reports one terminal result or one error.

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'deslop-common)

;;;; Hookable LLM caller

(defvar deslop-llm-function #'deslop-llm-default
  "Function used to dispatch requests to the LLM.
Called with (MESSAGES CALLBACKS &optional OPTIONS).
CALLBACKS is a plist with keys :on-start, :on-status, :on-delta,
:on-usage, :on-event, :on-done, and :on-error.")

;;;; Reasoning parser

(cl-defstruct (deslop-llm--reasoning
               (:constructor deslop-llm--make-reasoning))
  "Incremental state for XML-like reasoning tags."
  mode pending)

(defun deslop-llm--emit-delta (callbacks type text)
  "Invoke :on-delta in CALLBACKS with TYPE and nonempty TEXT."
  (let ((cb (plist-get callbacks :on-delta)))
    (when (and cb (stringp text) (> (length text) 0))
      (funcall cb type text))))

(defun deslop-llm--emit-status (callbacks status)
  "Invoke :on-status in CALLBACKS with STATUS."
  (let ((cb (plist-get callbacks :on-status)))
    (when cb
      (funcall cb status))))

(defun deslop-llm--emit-event (callbacks event)
  "Invoke :on-event in CALLBACKS with normalized EVENT."
  (let ((cb (plist-get callbacks :on-event)))
    (when cb
      (funcall cb event))))

(defun deslop-llm--longest-tag-suffix (text tag)
  "Return the longest suffix of TEXT that prefixes TAG.
The complete TAG is intentionally excluded because complete tags are handled
before this helper is called."
  (let ((limit (min (1- (length tag)) (length text)))
        (found ""))
    (cl-loop for length from limit downto 1
             for prefix = (substring tag 0 length)
             when (string-suffix-p prefix text)
             do (setq found prefix) and return found)
    found))

(defun deslop-llm--reasoning-callbacks (callbacks)
  "Return CALLBACKS with status/delta behavior suitable for tag parsing."
  callbacks)

(defun deslop-llm--reasoning-feed (parser content callbacks)
  "Feed CONTENT into incremental PARSER and return visible answer text.
Thinking and content deltas are emitted through CALLBACKS."
  (let ((remaining (concat (deslop-llm--reasoning-pending parser)
                           (or content "")))
        (visible nil)
        (open-tag "<think>")
        (close-tag "</think>"))
    (setf (deslop-llm--reasoning-pending parser) "")
    (while (> (length remaining) 0)
      (if (eq (deslop-llm--reasoning-mode parser) 'thinking)
          (let ((close (string-match (regexp-quote close-tag) remaining)))
            (if close
                (let ((thinking (substring remaining 0 close)))
                  (deslop-llm--emit-delta callbacks :thinking thinking)
                  (setq remaining
                        (substring remaining (+ close (length close-tag))))
                  (setf (deslop-llm--reasoning-mode parser) 'content)
                  (deslop-llm--emit-status callbacks "Generating"))
              (let* ((suffix (deslop-llm--longest-tag-suffix remaining close-tag))
                     (body-length (- (length remaining) (length suffix))))
                (when (> body-length 0)
                  (deslop-llm--emit-delta callbacks :thinking
                                           (substring remaining 0 body-length)))
                (setf (deslop-llm--reasoning-pending parser) suffix)
                (setq remaining ""))))
        (let ((open (string-match (regexp-quote open-tag) remaining)))
          (if open
              (let ((answer (substring remaining 0 open)))
                (when (> (length answer) 0)
                  (push answer visible)
                  (deslop-llm--emit-delta callbacks :content answer)
                  (deslop-llm--emit-status callbacks "Generating"))
                (setq remaining
                      (substring remaining (+ open (length open-tag))))
                (setf (deslop-llm--reasoning-mode parser) 'thinking)
                (deslop-llm--emit-status callbacks "Thinking"))
            (let* ((suffix (deslop-llm--longest-tag-suffix remaining open-tag))
                   (body-length (- (length remaining) (length suffix))))
              (when (> body-length 0)
                (let ((answer (substring remaining 0 body-length)))
                  (push answer visible)
                  (deslop-llm--emit-delta callbacks :content answer)
                  (deslop-llm--emit-status callbacks "Generating")))
              (setf (deslop-llm--reasoning-pending parser) suffix)
              (setq remaining ""))))))
    (apply #'concat (nreverse visible))))

(defun deslop-llm--reasoning-finish (parser callbacks)
  "Flush PARSER and return a structured error for unclosed thinking."
  (let ((pending (deslop-llm--reasoning-pending parser)))
    (when (> (length pending) 0)
      (if (eq (deslop-llm--reasoning-mode parser) 'thinking)
          (deslop-llm--emit-delta callbacks :thinking pending)
        (deslop-llm--emit-delta callbacks :content pending))
      (setf (deslop-llm--reasoning-pending parser) ""))
    (when (eq (deslop-llm--reasoning-mode parser) 'thinking)
      (deslop-error-create
       :kind 'protocol
       :message "stream ended inside a thinking section"))))

(defun deslop-llm--process-content (content in-thinking callbacks)
  "Process one complete CONTENT chunk using IN-THINKING state.
This compatibility helper retains the old boolean state API.  The transport
uses `deslop-llm--reasoning-feed' so delimiters can span chunks."
  (let ((parser (deslop-llm--make-reasoning
                 :mode (if in-thinking 'thinking 'content)
                 :pending "")))
    (let ((visible (deslop-llm--reasoning-feed parser content callbacks)))
      (cons (eq (deslop-llm--reasoning-mode parser) 'thinking)
            visible))))

;;;; Incremental SSE parser

(cl-defstruct (deslop-llm--sse (:constructor deslop-llm--make-sse))
  "Incremental SSE line/event state."
  buffer data-lines done)

(defun deslop-llm--sse-event (parser)
  "Turn the accumulated SSE data lines in PARSER into one event."
  (let ((payload (mapconcat #'identity
                            (nreverse (deslop-llm--sse-data-lines parser))
                            "\n")))
    (setf (deslop-llm--sse-data-lines parser) nil)
    (when (> (length payload) 0)
      (if (string= (string-trim payload) "[DONE]")
          (progn
            (setf (deslop-llm--sse-done parser) t)
            '(:type :done))
        (let ((json (condition-case err
                        (json-read-from-string payload)
                      (error
                       (signal 'error
                               (list (format "malformed SSE JSON: %s"
                                             (error-message-string err))))))))
          (list :type :json :value json))))))

(defun deslop-llm--sse-feed (parser chunk)
  "Feed raw SSE CHUNK into PARSER and return complete events.
Errors are signaled so the transport can turn them into protocol failures."
  (setf (deslop-llm--sse-buffer parser)
        (concat (deslop-llm--sse-buffer parser) chunk))
  (let (events position line)
    (while (string-match "\n" (deslop-llm--sse-buffer parser))
      (setq position (match-beginning 0))
      (setq line (substring (deslop-llm--sse-buffer parser) 0 position))
      (setf (deslop-llm--sse-buffer parser)
            (substring (deslop-llm--sse-buffer parser) (1+ position)))
      (when (and (> (length line) 0)
                 (= (aref line (1- (length line))) ?\r))
        (setq line (substring line 0 -1)))
      (cond
       ((string-empty-p line)
        (when (deslop-llm--sse-data-lines parser)
          (let ((event (deslop-llm--sse-event parser)))
            (when event
              (when (and (deslop-llm--sse-done parser)
                         (eq (plist-get event :type) :json))
                (signal 'error (list "SSE data received after [DONE]")))
              (push event events)))))
       ((string-prefix-p ":" line) nil)
       ((string-prefix-p "data:" line)
        (let ((value (substring line 5)))
          (when (and (> (length value) 0) (= (aref value 0) ?\s))
            (setq value (substring value 1)))
          (push value (deslop-llm--sse-data-lines parser))))
       (t
        ;; SSE permits other fields, but they do not carry model output.
        nil)))
    (nreverse events)))

(defun deslop-llm--sse-finish (parser)
  "Flush a final complete SSE event and validate PARSER state."
  (let ((events nil))
    (when (> (length (deslop-llm--sse-buffer parser)) 0)
      (setq events (deslop-llm--sse-feed parser "\n\n")))
    (when (deslop-llm--sse-data-lines parser)
      (let ((event (deslop-llm--sse-event parser)))
        (when event
          (push event events))))
    (when (> (length (deslop-llm--sse-buffer parser)) 0)
      (signal 'error (list "incomplete SSE line at end of stream")))
    (unless (deslop-llm--sse-done parser)
      (signal 'error (list "SSE stream ended without [DONE]")))
    (nreverse events)))

;;;; Curl helpers

(defun deslop-llm--auth-secret (profile)
  "Resolve PROFILE's auth-source secret, or nil for unauthenticated hosts."
  (let ((host (plist-get profile :auth-source-host)))
    (when host
      (let* ((user (plist-get profile :auth-source-user))
             (entries (auth-source-search :host host
                                           :user user
                                           :require '(:secret)
                                           :max 1))
             (entry (car entries)))
        (unless entry
          (error "No auth-source entry for %s" host))
        (let ((secret (plist-get entry :secret)))
          (if (functionp secret) (funcall secret) secret))))))

(defun deslop-llm--curl-config (profile)
  "Create a temporary curl config for PROFILE and return its path.
Return nil when PROFILE has no authentication or extra headers."
  (let ((secret (deslop-llm--auth-secret profile))
        (headers (plist-get profile :request-headers)))
    (when (or secret headers)
      (when (and secret (or (not (stringp secret))
                            (string-match-p "[\r\n]" secret)))
        (error "Invalid auth-source secret"))
      (let ((file (make-temp-file "deslop-curl-" nil ".conf")))
        (set-file-modes file #o600)
        (with-temp-file file
          (dolist (header headers)
            (insert (format "header = \"%s\"\n"
                            (replace-regexp-in-string
                             "[\\\"]" "\\\\&" header t t))))
          (when secret
            (insert (format "header = \"Authorization: Bearer %s\"\n"
                            (replace-regexp-in-string
                             "[\\\"]" "\\\\&" secret t t)))))
        (set-file-modes file #o600)
        file))))

(defun deslop-llm--curl-command (profile headers-file config-file)
  "Build curl command for PROFILE without embedding request data or secrets."
  (append
    (list "curl" "--silent" "--show-error" "--no-buffer"
          "--request" "POST"
         "--connect-timeout" (number-to-string deslop-connect-timeout)
         "--max-time" (number-to-string deslop-request-timeout)
         "--dump-header" headers-file
         "--header" "Content-Type: application/json"
         "--data-binary" "@-")
   (when config-file (list "--config" config-file))
   (list (plist-get profile :url))))

(defun deslop-llm--response-headers (file)
  "Return (STATUS . CONTENT-TYPE) from curl header FILE."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((status nil) (content-type nil))
        (while (re-search-forward "^HTTP/[0-9.]+[ \t]+\\([0-9]+\\)" nil t)
          (setq status (string-to-number (match-string 1)))
          (setq content-type nil)
          (let ((block-end (or (save-excursion
                                 (when (re-search-forward "^HTTP/" nil t)
                                   (line-beginning-position)))
                               (point-max))))
            (save-excursion
              (when (re-search-forward
                      "^[Cc]ontent-[Tt]ype:[ \t]*\\([^\r\n]+\\)"
                     block-end t)
                (setq content-type (string-trim (match-string 1)))))))
        (cons status content-type)))))

;;;; Client implementation

(defun deslop-llm-default (messages callbacks &optional options)
  "Send MESSAGES asynchronously using a callback-safe curl transport."
   (let* ((profile (if (plist-get options :name)
                       options
                     (deslop--plist-merge
                      (deslop-provider-profile)
                      options)))
          (model (plist-get profile :model))
          (temperature (plist-get profile :temperature))
          (body `((:model . ,model)
                  (:messages . ,(mapcar (lambda (message)
                                          `((:role . ,(car message))
                                            (:content . ,(cdr message))))
                                        messages))
                  (:temperature . ,temperature)
                  (:stream . t)
                  (:stream_options . ((:include_usage . t)))))
          (json-data (encode-coding-string (json-encode body) 'utf-8))
          (proc-buf (generate-new-buffer " *deslop-curl*"))
          (stderr-buf (generate-new-buffer " *deslop-curl-stderr*"))
          (headers-file (make-temp-file "deslop-headers-"))
          (config-file nil)
          (sse (deslop-llm--make-sse :buffer "" :data-lines nil :done nil))
          (reasoning (deslop-llm--make-reasoning :mode 'content :pending ""))
          (content-parts nil)
          (thinking-parts nil)
          (usage nil)
          (provider-error nil)
          (estimated-chars 0)
          (generation-start nil)
          (terminal nil)
          (proc nil))
     (let ((event-function nil)
           (fail-function nil)
           (emit-delta-function nil)
           (parser-callbacks-function nil)
           (handle-json-function nil)
           (handle-events-function nil)
           (cleanup-files-function nil)
           (finish-success-function nil)
           (finish-error-function nil))
       (setq event-function
             (lambda (name &rest args)
               (let ((function (plist-get callbacks name)))
                 (when function
                   (condition-case callback-error
                       (apply function args)
                     (error
                      (unless (eq name :on-error)
                        (funcall fail-function
                                 (deslop-error-create
                                  :kind 'callback
                                  :message (error-message-string
                                            callback-error))))))))))
       (setq fail-function
             (lambda (error)
               (unless terminal
                 (setq terminal t)
                 (when (and (processp proc) (process-live-p proc))
                   (process-put proc :deslop-cancelled t)
                   (delete-process proc))
                 (funcall event-function :on-event
                          (list :type :error :error error))
                 (funcall event-function :on-error error))))
       (setq emit-delta-function
             (lambda (type text)
               (when (and (stringp text) (> (length text) 0))
                 (if (eq type :content)
                     (push text content-parts)
                   (push text thinking-parts))
                 (setq estimated-chars (+ estimated-chars (length text)))
                 (unless generation-start
                   (setq generation-start (float-time)))
                 (funcall event-function :on-delta type text)
                 (funcall event-function :on-event
                          (list :type :delta :channel type :text text))
                 (let* ((elapsed (max 0.001
                                      (- (float-time) generation-start)))
                        (tokens (max 1 (floor (/ estimated-chars 4.0))))
                        (speed (/ (float tokens) elapsed)))
                   (funcall event-function :on-usage tokens speed t)
                   (funcall event-function :on-event
                            (list :type :usage :tokens tokens
                                  :speed speed :estimated t))))))
       (setq parser-callbacks-function
             (lambda ()
               (let ((event-function-local event-function))
                 (list :on-status
                       (lambda (status)
                         (funcall event-function-local :on-status status)
                         (funcall event-function-local :on-event
                                  (list :type :status :status status)))
                       :on-delta emit-delta-function))))
       (setq handle-json-function
              (lambda (json)
                (let ((json-error (alist-get 'error json)))
                  (if json-error
                      (setq provider-error
                            (deslop-error-create
                             :kind 'provider
                             :message (or (alist-get 'message json-error)
                                          (format "%s" json-error))
                             :details json-error))
                   (let* ((choices (alist-get 'choices json))
                          (choice (and (vectorp choices)
                                       (> (length choices) 0)
                                       (aref choices 0)))
                          (delta (and (listp choice)
                                      (alist-get 'delta choice)))
                          (reasoning-text
                           (and (listp delta)
                                (or (alist-get 'reasoning_content delta)
                                    (alist-get 'reasoning delta))))
                          (content (and (listp delta)
                                        (alist-get 'content delta)))
                          (new-usage (alist-get 'usage json)))
                     (when new-usage
                       (setq usage new-usage)
                       (let ((tokens (alist-get 'completion_tokens new-usage)))
                         (when tokens
                           (let* ((started (or generation-start
                                                (float-time)))
                                  (elapsed (max 0.001
                                                (- (float-time) started)))
                                  (speed (/ (float tokens) elapsed)))
                             (funcall event-function :on-usage tokens speed nil)
                             (funcall event-function :on-event
                                      (list :type :usage :tokens tokens
                                            :speed speed))))))
                     (when (and (stringp reasoning-text)
                                (> (length reasoning-text) 0))
                       (funcall event-function :on-status "Thinking")
                       (funcall emit-delta-function :thinking reasoning-text))
                     (when (and (stringp content) (> (length content) 0))
                       (deslop-llm--reasoning-feed
                        reasoning content
                        (funcall parser-callbacks-function))))))))
       (setq handle-events-function
             (lambda (events)
               (dolist (event events)
                 (unless terminal
                   (pcase (plist-get event :type)
                     (:done
                      (funcall event-function :on-event
                               (list :type :stream-done)))
                     (:json
                      (funcall handle-json-function
                               (plist-get event :value))))))))
       (setq cleanup-files-function
             (lambda ()
               (dolist (file (list headers-file config-file))
                 (when (and file (file-exists-p file))
                   (delete-file file)))
               (when (buffer-live-p stderr-buf)
                 (kill-buffer stderr-buf))))
       (setq finish-success-function
             (lambda (_event-string)
               (unless terminal
                 (condition-case finish-error
                     (let* ((header-info
                             (deslop-llm--response-headers headers-file))
                            (status (car header-info))
                            (content-type (cdr header-info)))
                       (cond
                        ((or (null status) (< status 200) (>= status 300))
                         (funcall fail-function
                                  (deslop-error-create
                                   :kind 'http
                                   :message
                                   (format "unexpected HTTP status %s"
                                           status))))
                        ((and (plist-get profile :require-sse)
                              (or (null content-type)
                                  (not (string-match-p "text/event-stream"
                                                       content-type))))
                         (funcall fail-function
                                  (deslop-error-create
                                   :kind 'protocol
                                   :message
                                   (format "expected text/event-stream, got %s"
                                           (or content-type "unknown")))))
                        (t
                         (funcall handle-events-function
                                  (deslop-llm--sse-finish sse))
                         (unless terminal
                           (let ((reasoning-error
                                  (deslop-llm--reasoning-finish
                                   reasoning
                                   (funcall parser-callbacks-function))))
                              (if reasoning-error
                                  (funcall fail-function reasoning-error)
                                (if provider-error
                                    (funcall fail-function provider-error)
                                  (setq terminal t)
                                  (let* ((reported
                                       (and usage
                                            (alist-get 'completion_tokens
                                                       usage)))
                                      (tokens (or reported
                                                   (max 0
                                                        (floor
                                                         (/ estimated-chars
                                                            4.0)))))
                                      (started (or generation-start
                                                    (float-time)))
                                      (speed (/ (float tokens)
                                                (max 0.001
                                                     (- (float-time)
                                                        started))))
                                      (result
                                       (list :content
                                             (mapconcat #'identity
                                                        (nreverse content-parts)
                                                        "")
                                             :thinking
                                             (mapconcat #'identity
                                                        (nreverse thinking-parts)
                                                        "")
                                             :usage usage
                                             :tokens tokens
                                             :estimated (null reported)
                                             :speed speed
                                             :provider (plist-get profile :name)
                                              :model (plist-get profile :model))))
                                    (funcall event-function :on-event
                                             (list :type :transport-done
                                                   :result result))
                                    (funcall event-function :on-done result)))))))))
                   (error
                    (funcall fail-function
                             (deslop-error-create
                              :kind 'protocol
                              :message (error-message-string finish-error))))))
               (funcall cleanup-files-function)))
       (setq finish-error-function
             (lambda (process-event)
               (unless terminal
                 (let ((status (process-exit-status proc))
                       (stderr (when (buffer-live-p stderr-buf)
                                 (with-current-buffer stderr-buf
                                   (string-trim (buffer-string))))))
                   (funcall fail-function
                            (deslop-error-create
                             :kind (if (= status 28) 'timeout 'transport)
                             :message
                             (or (and (not (string-empty-p stderr)) stderr)
                                 (format "curl exited with status %d (%s)"
                                         status
                                         (string-trim process-event)))))))
               (funcall cleanup-files-function)))
       (condition-case startup-error
           (progn
             (setq config-file (deslop-llm--curl-config profile))
             (setq proc
                   (make-process
                    :name (format "deslop-curl-%s-%s"
                                  (or (plist-get profile :name) "request")
                                  (or (plist-get profile :job-id) "unknown"))
                    :buffer proc-buf
                    :stderr stderr-buf
                    :connection-type 'pipe
                    :command (deslop-llm--curl-command
                              profile headers-file config-file)
                    :filter
                    (lambda (_process string)
                      (when (and (not terminal) (buffer-live-p proc-buf))
                        (condition-case filter-error
                            (funcall handle-events-function
                                     (deslop-llm--sse-feed sse string))
                          (error
                           (funcall fail-function
                                    (deslop-error-create
                                     :kind 'protocol
                                     :message
                                     (error-message-string filter-error)))))))
                    :sentinel
                    (lambda (process event-string)
                      (when (memq (process-status process)
                                  '(exit signal closed))
                        (when (buffer-live-p proc-buf)
                          (kill-buffer proc-buf))
                        (if (process-get process :deslop-cancelled)
                            (progn
                              (unless terminal
                                (funcall fail-function
                                         (deslop-error-create
                                          :kind 'cancelled
                                          :message "Process cancelled")))
                              (funcall cleanup-files-function))
                          (if (= (process-exit-status process) 0)
                              (funcall finish-success-function event-string)
                            (funcall finish-error-function event-string)))))))
             (funcall event-function :on-start proc)
             (unless terminal
               (process-send-string proc json-data)
               (process-send-eof proc)))
         (error
          (when (and (processp proc) (process-live-p proc))
            (process-put proc :deslop-cancelled t)
            (delete-process proc))
          (when (buffer-live-p proc-buf) (kill-buffer proc-buf))
          (funcall cleanup-files-function)
          (funcall event-function
                   :on-error
                   (deslop-error-create
                    :kind 'startup
                    :message (error-message-string startup-error)))))
       proc)))

(defalias 'deslop-llm-send #'deslop-llm-default)

(provide 'deslop-llm)

;;; deslop-llm.el ends here
