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
CALLBACKS is a plist with keys :on-start, :on-status, :on-metadata, :on-delta,
:on-usage, :on-event, :on-done, and :on-error.")

(defcustom deslop-provider-discovery-enabled t
  "Non-nil to discover metadata from OpenAI-compatible model endpoints."
  :type 'boolean
  :group 'deslop)

(defcustom deslop-provider-discovery-ttl 3600
  "Seconds to retain successful or failed provider discovery results."
  :type 'number
  :group 'deslop)

(defcustom deslop-provider-discovery-timeout 5
  "Maximum seconds allowed for one provider discovery request."
  :type 'number
  :group 'deslop)

(defcustom deslop-usage-update-interval 0.1
  "Minimum seconds between estimated streaming usage updates."
  :type 'number
  :group 'deslop)

(defvar deslop-provider--discovery-cache (make-hash-table :test #'equal)
  "Cached provider discovery results keyed by endpoint identity.")

(defvar deslop-provider--discovery-inflight (make-hash-table :test #'equal)
  "Callbacks waiting for an in-flight provider discovery request.")

(defvar deslop-provider--discovery-processes nil
  "Live provider discovery processes available for explicit cancellation.")

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
  (let* ((input (concat (deslop-llm--reasoning-pending parser)
                        (or content "")))
         (position 0)
         (size (length input))
         (visible nil)
         (open-tag "<think>")
         (close-tag "</think>"))
    (setf (deslop-llm--reasoning-pending parser) "")
    (while (< position size)
      (if (eq (deslop-llm--reasoning-mode parser) 'thinking)
          (let ((close (string-match (regexp-quote close-tag) input position)))
            (if close
                (let ((thinking (substring input position close)))
                  (deslop-llm--emit-delta callbacks :thinking thinking)
                  (setq position (+ close (length close-tag)))
                  (setf (deslop-llm--reasoning-mode parser) 'content)
                  (deslop-llm--emit-status callbacks "Generating"))
              (let* ((tail (substring input position))
                     (suffix (deslop-llm--longest-tag-suffix tail close-tag))
                     (body-end (- size (length suffix))))
                (when (> body-end position)
                  (deslop-llm--emit-delta callbacks :thinking
                                           (substring input position body-end)))
                (setf (deslop-llm--reasoning-pending parser) suffix)
                (setq position size))))
        (let ((open (string-match (regexp-quote open-tag) input position)))
          (if open
              (let ((answer (substring input position open)))
                (when (> (length answer) 0)
                  (push answer visible)
                  (deslop-llm--emit-delta callbacks :content answer)
                  (deslop-llm--emit-status callbacks "Generating"))
                (setq position (+ open (length open-tag)))
                (setf (deslop-llm--reasoning-mode parser) 'thinking)
                (deslop-llm--emit-status callbacks "Thinking"))
            (let* ((tail (substring input position))
                   (suffix (deslop-llm--longest-tag-suffix tail open-tag))
                   (body-end (- size (length suffix))))
              (when (> body-end position)
                (let ((answer (substring input position body-end)))
                  (push answer visible)
                  (deslop-llm--emit-delta callbacks :content answer)
                  (deslop-llm--emit-status callbacks "Generating")))
              (setf (deslop-llm--reasoning-pending parser) suffix)
              (setq position size))))))
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
  buffer data-lines data-bytes done)

(defun deslop-llm--sse-event (parser)
  "Turn the accumulated SSE data lines in PARSER into one event."
  (let ((payload (mapconcat #'identity
                            (nreverse (deslop-llm--sse-data-lines parser))
                            "\n")))
    (setf (deslop-llm--sse-data-lines parser) nil)
    (setf (deslop-llm--sse-data-bytes parser) 0)
    (when (> (length payload) 0)
      (if (string= (string-trim payload) "[DONE]")
          (progn
            (setf (deslop-llm--sse-done parser) t)
            '(:type :done))
        (let ((json (condition-case err
                        (deslop-llm--json-read-string payload)
                      ((error quit)
                       (signal 'error
                               (list (format "malformed SSE JSON: %s"
                                             (error-message-string err))))))))
          (list :type :json :value json))))))

(defun deslop-llm--sse-handle-line (parser line)
  "Consume one normalized SSE LINE for PARSER and return an event if complete."
  (when (> (string-bytes line) deslop-max-sse-event-bytes)
    (error "SSE line exceeds deslop-max-sse-event-bytes"))
  (cond
   ((deslop-llm--sse-done parser) nil)
   ((string-empty-p line)
    (when (deslop-llm--sse-data-lines parser)
      (deslop-llm--sse-event parser)))
   ((string-prefix-p ":" line) nil)
   ((string-prefix-p "data:" line)
    (let ((value (substring line 5)))
      (when (and (> (length value) 0) (= (aref value 0) ?\s))
        (setq value (substring value 1)))
      (let ((bytes (+ (or (deslop-llm--sse-data-bytes parser) 0)
                      (if (deslop-llm--sse-data-lines parser) 1 0)
                      (string-bytes value))))
        (when (> bytes deslop-max-sse-event-bytes)
          (error "SSE event exceeds deslop-max-sse-event-bytes"))
        (setf (deslop-llm--sse-data-bytes parser) bytes))
      (push value (deslop-llm--sse-data-lines parser)))
    nil)
   (t nil)))

(defun deslop-llm--sse-feed (parser chunk)
  "Feed raw SSE CHUNK into PARSER and return complete events.
Errors are signaled so the transport can turn them into protocol failures."
  (let* ((input (concat (deslop-llm--sse-buffer parser) chunk))
         (start 0)
         (events nil)
         position line)
    (setf (deslop-llm--sse-buffer parser) "")
    (while (string-match "\n" input start)
      (setq position (match-beginning 0))
      (setq line (substring input start position))
      (setq start (1+ position))
      (when (and (> (length line) 0)
                 (= (aref line (1- (length line))) ?\r))
        (setq line (substring line 0 -1)))
      (let ((event (deslop-llm--sse-handle-line parser line)))
        (when event
          (push event events))))
    (setf (deslop-llm--sse-buffer parser) (substring input start))
    (when (> (string-bytes (deslop-llm--sse-buffer parser))
             deslop-max-sse-event-bytes)
      (error "SSE line exceeds deslop-max-sse-event-bytes"))
    (nreverse events)))

(defun deslop-llm--sse-finish (parser)
  "Flush a final complete SSE event and validate PARSER state."
  (let ((events nil))
    (when (> (length (deslop-llm--sse-buffer parser)) 0)
      (let ((line (deslop-llm--sse-buffer parser)))
        (setf (deslop-llm--sse-buffer parser) "")
        (when (and (> (length line) 0)
                   (= (aref line (1- (length line))) ?\r))
          (setq line (substring line 0 -1)))
        (let ((event (deslop-llm--sse-handle-line parser line)))
          (when event
            (push event events)))))
    (when (deslop-llm--sse-data-lines parser)
      (let ((event (deslop-llm--sse-event parser)))
        (when event
          (push event events))))
    (unless (deslop-llm--sse-done parser)
      (signal 'error (list "SSE stream ended without [DONE]")))
    (nreverse events)))

;;;; Curl helpers

(defun deslop-llm--json-read-string (text)
  "Parse JSON TEXT without interning object keys as Emacs symbols."
  (json-parse-string text
                     :object-type 'hash-table
                     :array-type 'array
                     :null-object nil
                     :false-object :false))

(defun deslop-llm--json-get (key object)
  "Return string KEY from parsed JSON OBJECT without interning symbols."
  (cond
   ((hash-table-p object) (gethash key object))
   ((listp object)
    (cdr (cl-find-if
          (lambda (entry)
            (let ((candidate (car-safe entry)))
              (string= key (if (symbolp candidate)
                               (symbol-name candidate)
                             (format "%s" candidate)))))
          object)))))

(defun deslop-llm--json-has-key-p (key object)
  "Return non-nil when JSON alist OBJECT contains string KEY."
  (cond
   ((hash-table-p object)
    (let ((missing (make-symbol "missing")))
      (not (eq (gethash key object missing) missing))))
   ((listp object)
    (cl-find-if
     (lambda (entry)
       (let ((candidate (car-safe entry)))
         (string= key (if (symbolp candidate)
                          (symbol-name candidate)
                        (format "%s" candidate)))))
     object))))

(defun deslop-llm--loopback-host-p (host)
  "Return non-nil when HOST names an explicit loopback endpoint."
  (and (stringp host)
       (let ((name (downcase host)))
         (or (string= name "localhost")
             (string= name "::1")
             (let ((parts (split-string name "\." nil)))
               (and (= (length parts) 4)
                    (cl-every
                     (lambda (part)
                       (and (string-match-p "\\`[0-9]+\\'" part)
                            (<= (string-to-number part) 255)))
                     parts)
                    (= (string-to-number (car parts)) 127)))))))

(defun deslop-llm--validated-url (profile &optional url)
  "Return validated URL for PROFILE, optionally checking URL instead."
  (let* ((value (or url (plist-get profile :url)))
         (parsed (and (stringp value) (url-generic-parse-url value)))
         (scheme (and parsed (downcase (or (url-type parsed) ""))))
         (host (and parsed (url-host parsed)))
         (auth-host (plist-get profile :auth-source-host))
         (auth-user (plist-get profile :auth-source-user))
         (auth-port (plist-get profile :auth-source-port)))
    (unless (and (stringp value) (not (string-empty-p value))
                 (member scheme '("http" "https"))
                 (stringp host) (not (string-empty-p host)))
      (error "Provider URL must be an absolute HTTP(S) URL"))
    (when (or (url-user parsed) (url-password parsed))
      (error "Provider URL must not contain user information"))
    (when (string-match-p "[?#]" value)
      (error "Provider URL must not contain a query or fragment"))
    (when (and (string= scheme "http")
               (not (deslop-llm--loopback-host-p host)))
      (error "Plain HTTP is only allowed for explicit loopback endpoints"))
    (when (and auth-host
               (not (string= (downcase (format "%s" auth-host))
                             (downcase host))))
      (error "auth-source host %s does not match endpoint host %s"
             auth-host host))
    (when auth-host
      (unless (and (stringp auth-user) (not (string-empty-p auth-user)))
        (error "Authenticated profiles require an explicit auth-source user"))
      (unless (and (integerp auth-port)
                   (= auth-port
                      (or (url-port parsed)
                          (if (string= scheme "https") 443 80))))
        (error "Authenticated profile port does not match endpoint port")))
    value))

(defun deslop-llm--auth-secret (profile)
  "Resolve PROFILE's auth-source secret, or nil for unauthenticated hosts."
  (deslop-llm--validated-url profile)
  (let ((host (plist-get profile :auth-source-host)))
    (when host
      (let* ((user (plist-get profile :auth-source-user))
             (port (plist-get profile :auth-source-port))
             (entries (auth-source-search :host host
                                            :user user
                                            :port port
                                           :require '(:secret)
                                           :max 1))
             (entry (car entries)))
        (unless entry
          (error "No auth-source entry for %s" host))
        (let ((secret (plist-get entry :secret)))
          (setq secret (if (functionp secret) (funcall secret) secret))
          (unless (and (stringp secret)
                       (not (string-match-p "[[:cntrl:]]" secret)))
            (error "Invalid auth-source secret"))
          secret)))))

(defun deslop-llm--valid-header-p (header)
  "Return non-nil when HEADER is one complete RFC-style header line."
  (and (stringp header)
       (not (string-match-p "[[:cntrl:]]" header))
       (string-match-p
        "\\`[!#$%&'*+.^_`|~0-9A-Za-z-]+:[ \t]*.*\\'" header)))

(defun deslop-llm--unsafe-profile-header-p (header)
  "Return non-nil when HEADER can affect routing, framing, or credentials."
  (let* ((name (and (stringp header)
                    (car (split-string header ":"))))
         (downcased (and name (downcase name))))
    (and downcased
         (or (member downcased
                     '("host" "content-length" "transfer-encoding"
                       "content-type" "connection" "trailer" "upgrade"
                       "expect" "proxy-connection"))
             (string-match-p
              "\\(?:auth\\|token\\|key\\|secret\\|cookie\\|credential\\)"
              downcased)))))

(defun deslop-llm--curl-config-escape (value)
  "Escape VALUE for a double-quoted curl config argument."
  (mapconcat (lambda (character)
               (if (memq character '(?\\ ?\"))
                   (string ?\\ character)
                 (char-to-string character)))
             value ""))

(defun deslop-llm--curl-config (profile)
  "Create a temporary curl config for PROFILE and return its path.
Return nil when PROFILE has no authentication or extra headers."
  (let* ((secret (deslop-llm--auth-secret profile))
         (headers (plist-get profile :request-headers))
         (all-headers (append headers
                              (when secret
                                (list (concat "Authorization: Bearer " secret))))))
    (when (or secret headers)
      (dolist (header all-headers)
        (unless (deslop-llm--valid-header-p header)
          (error "Invalid request header"))
        (when (and (member header headers)
                   (deslop-llm--unsafe-profile-header-p header))
          (error "Unsafe request header must not be supplied by a profile")))
      (let ((contents
             (mapconcat
              (lambda (header)
                (format "header = \"%s\""
                        (deslop-llm--curl-config-escape header)))
              all-headers "\n"))
            (file nil)
            (complete nil))
        (unwind-protect
            (progn
              (setq file (make-temp-file "deslop-curl-" nil ".conf"))
              (set-file-modes file #o600)
              (with-temp-file file
                (insert contents "\n"))
              (set-file-modes file #o600)
              (setq complete t)
              file)
          (unless complete
            (when (and file (file-exists-p file))
              (condition-case nil
                  (delete-file file)
                ((error quit) nil)))))))))

(defun deslop-llm--curl-network-options (url)
  "Return hardened common curl options for URL."
  (let* ((parsed (url-generic-parse-url url))
         (host (url-host parsed)))
    (append
     (list "--globoff" "--proto" "=http,https"
           "--proto-redir" "=http,https")
     (when (or (not deslop-allow-environment-proxy)
               (deslop-llm--loopback-host-p host))
       (list "--noproxy" "*")))))

(defun deslop-llm--curl-command (profile headers-file config-file)
  "Build curl command for PROFILE without embedding request data or secrets."
  (let ((url (deslop-llm--validated-url profile)))
    (append
     (list "curl" "--disable")
     (deslop-llm--curl-network-options url)
     (list "--silent" "--show-error" "--no-buffer"
           "--request" "POST"
           "--connect-timeout" (number-to-string deslop-connect-timeout)
           "--max-time" (number-to-string deslop-request-timeout)
           "--max-filesize" (number-to-string deslop-max-response-bytes)
           "--dump-header" headers-file
           "--header" "Content-Type: application/json"
           "--data-binary" "@-")
     (when config-file (list "--config" config-file))
     (list "--url" url))))

(defun deslop-llm--response-headers (file)
  "Return (STATUS . CONTENT-TYPE) from curl header FILE."
  (when (file-readable-p file)
    (let ((size (file-attribute-size (file-attributes file))))
      (when (> size deslop-max-response-header-bytes)
        (error "Response headers exceed deslop-max-response-header-bytes")))
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

(defun deslop-provider--discovery-key (profile)
  "Return a non-secret cache key for provider PROFILE."
  (list (plist-get profile :url)
        (plist-get profile :name)
        (plist-get profile :auth-source-host)
        (plist-get profile :auth-source-user)
        (plist-get profile :model)
        (plist-get profile :display-name)
        (secure-hash 'sha256
                     (prin1-to-string (plist-get profile :request-headers)))))

(defun deslop-provider--models-url (profile)
  "Return the OpenAI-compatible model-list URL for PROFILE, or nil."
  (let ((url (plist-get profile :url)))
    (cond
     ((and (stringp url)
           (string-match
            "\\`\\(.+\\)/chat/completions\\(?:/\\)?\\(?:[?#].*\\)?\\'"
            url))
      (concat (match-string 1 url) "/models"))
     ((and (stringp url)
           (string-match "\\`\\(.+\\)/v[0-9]+/?\\'" url))
      (concat (replace-regexp-in-string "/\\'" "" url) "/models")))))

(defun deslop-provider--model-entries (payload)
  "Return model entries from discovery PAYLOAD as a list."
  (let ((data (deslop-llm--json-get "data" payload)))
    (cond
     ((vectorp data) (append data nil))
     ((listp data) data))))

(defun deslop-provider--discovery-metadata (profile payload)
  "Extract display metadata for PROFILE from discovery PAYLOAD."
  (let* ((entries (deslop-provider--model-entries payload))
         (requested (plist-get profile :model))
         (match (cl-find-if
                  (lambda (entry)
                    (or (null requested)
                        (equal (deslop-llm--json-get "id" entry)
                               requested)))
                  entries))
         (owner-entry match)
         (provider (or (deslop-llm--json-get "provider" payload)
                       (deslop-llm--json-get "server" payload)))
         (model (or (and match (deslop-llm--json-get "id" match)) requested))
         (metadata (list :source :discovery :model model)))
    (when (stringp provider)
      (setq metadata (plist-put metadata :provider provider)))
    (when (and owner-entry
               (stringp (deslop-llm--json-get "owned_by" owner-entry)))
      (setq metadata
            (plist-put metadata :owner
                       (deslop-llm--json-get "owned_by" owner-entry))))
    (when (or match (stringp provider))
      (plist-put metadata :client
                 (deslop-provider-client-name profile metadata)))))

(defun deslop-provider--parse-discovery-response (profile body headers)
  "Parse a discovery response in BODY using response HEADERS."
  (let ((status (car (deslop-llm--response-headers headers))))
    (when (and status (>= status 200) (< status 300)
               (buffer-live-p body))
      (with-current-buffer body
        (condition-case nil
            (deslop-provider--discovery-metadata
             profile (deslop-llm--json-read-string (buffer-string)))
          ((error quit) nil))))))

(defun deslop-llm--curl-discovery-command
    (url headers-file config-file)
  "Build a GET command for provider discovery at URL."
  (append
   (list "curl" "--disable")
   (deslop-llm--curl-network-options url)
   (list "--silent" "--show-error" "--no-buffer"
         "--request" "GET"
         "--connect-timeout" (number-to-string deslop-connect-timeout)
         "--max-time" (number-to-string deslop-provider-discovery-timeout)
         "--max-filesize" (number-to-string deslop-max-discovery-bytes)
         "--dump-header" headers-file
         "--header" "Accept: application/json")
   (when config-file (list "--config" config-file))
   (list "--url" url)))

(defun deslop-llm--cleanup-call (function &rest arguments)
  "Call FUNCTION with ARGUMENTS while suppressing cleanup failures and quits."
  (condition-case nil
      (apply function arguments)
    ((error quit) nil)))

(defun deslop-provider--invoke-discovery-callback (callback metadata)
  "Invoke discovery CALLBACK with METADATA without propagating its errors."
  (when callback
    (condition-case nil
        (funcall callback metadata)
      ((error quit) nil))))

(defun deslop-provider--prune-discovery-cache ()
  "Remove expired and excess provider discovery cache entries."
  (let ((now (float-time))
        (expired nil))
    (maphash
     (lambda (key entry)
       (when (>= (- now (plist-get entry :time))
                 deslop-provider-discovery-ttl)
         (push key expired)))
     deslop-provider--discovery-cache)
    (dolist (key expired)
      (remhash key deslop-provider--discovery-cache))
    (while (>= (hash-table-count deslop-provider--discovery-cache)
               (max 1 deslop-provider-discovery-cache-size))
      (let ((oldest nil))
        (maphash
         (lambda (key entry)
           (when (or (null oldest)
                     (< (plist-get entry :time)
                        (plist-get (cdr oldest) :time)))
             (setq oldest (cons key entry))))
         deslop-provider--discovery-cache)
        (if oldest
            (remhash (car oldest) deslop-provider--discovery-cache)
          (clrhash deslop-provider--discovery-cache))))))

(defun deslop-provider--finish-discovery (key metadata)
  "Cache METADATA for KEY and notify callbacks waiting on it."
  (deslop-provider--prune-discovery-cache)
  (puthash key (list :time (float-time) :metadata metadata)
           deslop-provider--discovery-cache)
  (let ((callbacks (gethash key deslop-provider--discovery-inflight)))
    (remhash key deslop-provider--discovery-inflight)
    (dolist (callback callbacks)
      (deslop-provider--invoke-discovery-callback callback metadata))))

(defun deslop-provider-cached-metadata (profile)
  "Return unexpired discovery metadata for PROFILE, or nil."
  (let* ((key (deslop-provider--discovery-key profile))
         (entry (gethash key
                          deslop-provider--discovery-cache))
         (age (and entry (- (float-time) (plist-get entry :time)))))
    (cond
     ((and entry (< age deslop-provider-discovery-ttl))
      (plist-get entry :metadata))
     (entry
      (remhash key deslop-provider--discovery-cache)
      nil))))

(defun deslop-provider--start-discovery (profile key url)
  "Start an asynchronous discovery request for PROFILE at URL."
  (let ((response-buf (generate-new-buffer " *deslop-provider-response*"))
        (stderr-buf (generate-new-buffer " *deslop-provider-stderr*"))
        (headers-file nil)
        (config-file nil)
        (process nil)
        (timer nil)
        (received-bytes 0)
        (finished nil)
        (finish nil))
    (setq finish
          (lambda (metadata)
            (unless finished
              (setq finished t)
              (setq deslop-provider--discovery-processes
                    (delq process deslop-provider--discovery-processes))
              (when (timerp timer)
                (deslop-llm--cleanup-call #'cancel-timer timer))
              (when (and (processp process) (process-live-p process))
                (process-put process :deslop-cancelled t)
                (deslop-llm--cleanup-call #'delete-process process))
              (dolist (buffer (list response-buf stderr-buf))
                (when (buffer-live-p buffer)
                  (deslop-llm--cleanup-call #'kill-buffer buffer)))
              (dolist (file (list headers-file config-file))
                (when (and file (file-exists-p file))
                  (deslop-llm--cleanup-call #'delete-file file)))
              (deslop-provider--finish-discovery key metadata))))
    (condition-case _startup
        (progn
          (deslop-llm--validated-url profile url)
          (setq headers-file (make-temp-file "deslop-provider-headers-"))
          (set-file-modes headers-file #o600)
          (setq config-file (deslop-llm--curl-config profile))
          (setq process
                (make-process
                 :name (format "deslop-provider-discovery-%s"
                               (or (plist-get profile :name) "request"))
                 :buffer response-buf
                 :stderr stderr-buf
                 :connection-type 'pipe
                  :noquery t
                  :command (deslop-llm--curl-discovery-command
                            url headers-file config-file)
                  :filter
                  (lambda (_proc string)
                    (unless finished
                      (setq received-bytes
                            (+ received-bytes (string-bytes string)))
                      (if (> received-bytes deslop-max-discovery-bytes)
                          (funcall finish nil)
                        (when (buffer-live-p response-buf)
                          (with-current-buffer response-buf
                            (goto-char (point-max))
                            (insert string))))))
                  :sentinel
                 (lambda (proc _event)
                   (when (memq (process-status proc) '(exit signal closed))
                     (if (= (process-exit-status proc) 0)
                         (condition-case nil
                             (funcall
                              finish
                              (deslop-provider--parse-discovery-response
                               profile response-buf headers-file))
                           ((error quit) (funcall finish nil)))
                        (funcall finish nil))))))
          (process-put process :deslop-discovery-finish finish)
          (push process deslop-provider--discovery-processes)
          (unless finished
            (setq timer
                  (run-at-time deslop-provider-discovery-timeout nil
                               (lambda ()
                                 (funcall finish nil))))))
      ((error quit)
       (funcall finish nil)))))

(defun deslop-provider-discover (profile &optional callback)
  "Discover PROFILE metadata asynchronously and invoke CALLBACK with it.
Results, including failed lookups, are cached for
`deslop-provider-discovery-ttl' seconds.  CALLBACK receives nil when the
endpoint does not expose compatible metadata."
  (let ((callback (or callback #'ignore)))
    (cond
     ((not deslop-provider-discovery-enabled)
      (deslop-provider--invoke-discovery-callback callback nil))
     (t
      (let* ((key (deslop-provider--discovery-key profile))
             (entry (gethash key deslop-provider--discovery-cache))
             (now (float-time))
             (url (deslop-provider--models-url profile)))
        (when (and entry
                   (>= (- now (plist-get entry :time))
                       deslop-provider-discovery-ttl))
          (remhash key deslop-provider--discovery-cache)
          (setq entry nil))
        (if entry
            (deslop-provider--invoke-discovery-callback
             callback (plist-get entry :metadata))
          (let ((waiting (gethash key deslop-provider--discovery-inflight)))
            (if waiting
                (puthash key (cons callback waiting)
                         deslop-provider--discovery-inflight)
              (puthash key (list callback)
                       deslop-provider--discovery-inflight)
              (if url
                  (deslop-provider--start-discovery profile key url)
                (deslop-provider--finish-discovery key nil))))))))))

(defun deslop-provider-clear-discovery-cache ()
  "Clear cached provider discovery results."
  (interactive)
  (clrhash deslop-provider--discovery-cache)
  (message "deslop: provider discovery cache cleared"))

(defun deslop-provider-abort-discovery ()
  "Cancel all in-flight provider discovery requests."
  (dolist (process (copy-sequence deslop-provider--discovery-processes))
    (let ((finish (and (processp process)
                       (process-get process :deslop-discovery-finish))))
      (if finish
          (funcall finish nil)
        (when (and (processp process) (process-live-p process))
          (deslop-llm--cleanup-call #'delete-process process)))))
  (setq deslop-provider--discovery-processes nil))

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
                   (:max_tokens . ,deslop-max-output-tokens)
                   (:stream . t)
                   (:stream_options . ((:include_usage . t)))))
          (json-data nil)
          (proc-buf nil)
          (stderr-buf nil)
          (headers-file nil)
          (config-file nil)
          (sse (deslop-llm--make-sse :buffer "" :data-lines nil
                                     :data-bytes 0 :done nil))
          (reasoning (deslop-llm--make-reasoning :mode 'content :pending ""))
          (content-parts nil)
           (thinking-parts nil)
           (usage nil)
           (provider-error nil)
           (actual-model nil)
            (actual-provider nil)
            (estimated-chars 0)
            (received-bytes 0)
            (response-preview "")
          (generation-start nil)
          (last-status nil)
          (last-usage-update nil)
          (cleaned nil)
          (terminal nil)
          (proc nil))
     (let ((event-function nil)
           (fail-function nil)
           (emit-delta-function nil)
           (parser-callbacks-function nil)
           (handle-json-function nil)
            (handle-events-function nil)
            (cleanup-files-function nil)
            (complete-function nil)
            (finish-success-function nil)
           (finish-error-function nil))
       (setq event-function
             (lambda (name &rest args)
               (let ((function (plist-get callbacks name)))
                 (when function
                   (condition-case callback-error
                       (apply function args)
                    ((error quit)
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
                    (process-put proc :deslop-settled t)
                    (deslop-llm--cleanup-call #'delete-process proc))
                  (funcall cleanup-files-function)
                  (funcall event-function :on-event
                           (list :type :error :error error))
                  (funcall event-function :on-error error))))
       (setq emit-delta-function
              (lambda (type text)
                (when (and (not terminal) (stringp text) (> (length text) 0))
                  (if (> (+ estimated-chars (length text))
                         deslop-max-output-chars)
                      (funcall
                       fail-function
                       (deslop-error-create
                        :kind 'resource
                        :message "generated output exceeds deslop-max-output-chars"))
                    (if (eq type :content)
                        (push text content-parts)
                      (push text thinking-parts))
                    (setq estimated-chars (+ estimated-chars (length text)))
                    (unless generation-start
                      (setq generation-start (float-time)))
                    (funcall event-function :on-delta type text)
                    (unless terminal
                      (funcall event-function :on-event
                               (list :type :delta :channel type :text text)))
                    (let ((now (float-time)))
                      (when (and (not terminal)
                                 (or (null last-usage-update)
                                     (>= (- now last-usage-update)
                                         deslop-usage-update-interval)))
                        (setq last-usage-update now)
                        (let* ((elapsed (max 0.001
                                             (- now generation-start)))
                               (tokens (max 1
                                            (floor (/ estimated-chars 4.0))))
                               (speed (/ (float tokens) elapsed)))
                          (funcall event-function :on-usage tokens speed t)
                          (unless terminal
                            (funcall event-function :on-event
                                     (list :type :usage :tokens tokens
                                           :speed speed :estimated t))))))))))
       (setq parser-callbacks-function
             (lambda ()
               (let ((event-function-local event-function))
                  (list :on-status
                        (lambda (status)
                          (unless (equal status last-status)
                            (setq last-status status)
                            (funcall event-function-local :on-status status)
                            (unless terminal
                              (funcall event-function-local :on-event
                                       (list :type :status :status status)))))
                        :on-delta emit-delta-function))))
       (setq handle-json-function
             (lambda (json)
               (let ((json-error (deslop-llm--json-get "error" json)))
                 (if (deslop-llm--json-has-key-p "error" json)
                     (progn
                       (let* ((reported-message
                               (format "%s"
                                       (or (deslop-llm--json-get
                                            "message" json-error)
                                           json-error)))
                              (status
                               (condition-case nil
                                   (car (deslop-llm--response-headers
                                         headers-file))
                                 (error nil))))
                         (setq provider-error
                               (deslop-error-create
                                :kind (if (and status
                                               (or (< status 200)
                                                   (>= status 300)))
                                          'http
                                        'provider)
                                :message (if (and status
                                                  (or (< status 200)
                                                      (>= status 300)))
                                             (format "HTTP status %s: %s"
                                                     status reported-message)
                                           reported-message)
                                :details json-error)))
                       (funcall fail-function provider-error))
                   (let ((reported-model
                          (deslop-llm--json-get "model" json))
                         (reported-provider
                          (or (deslop-llm--json-get "provider" json)
                              (deslop-llm--json-get "server" json)
                              (deslop-llm--json-get "owned_by" json))))
                     (when (or (and (stringp reported-model)
                                    (not (string= reported-model
                                                  (or actual-model ""))))
                               (and (stringp reported-provider)
                                    (not (string= reported-provider
                                                  (or actual-provider "")))))
                       (when (stringp reported-model)
                         (setq actual-model reported-model))
                       (when (stringp reported-provider)
                         (setq actual-provider reported-provider))
                       (funcall
                        event-function :on-metadata
                        (list :source :response
                              :model actual-model
                              :provider actual-provider)))
                     (unless terminal
                       (let* ((choices
                               (deslop-llm--json-get "choices" json))
                              (choice
                               (cond
                                ((and (vectorp choices)
                                      (> (length choices) 0))
                                 (aref choices 0))
                                ((consp choices) (car choices))))
                              (delta (deslop-llm--json-get "delta" choice))
                              (reasoning-text
                               (or (deslop-llm--json-get
                                    "reasoning_content" delta)
                                   (deslop-llm--json-get "reasoning" delta)))
                              (content
                               (deslop-llm--json-get "content" delta))
                              (new-usage
                               (deslop-llm--json-get "usage" json)))
                         (when new-usage
                           (setq usage new-usage)
                           (let ((tokens
                                  (deslop-llm--json-get
                                   "completion_tokens" new-usage)))
                             (when (numberp tokens)
                               (let* ((started (or generation-start
                                                   (float-time)))
                                      (elapsed (max 0.001
                                                    (- (float-time) started)))
                                      (speed (/ (float tokens) elapsed)))
                                 (funcall event-function
                                          :on-usage tokens speed nil)
                                 (unless terminal
                                   (funcall event-function :on-event
                                            (list :type :usage :tokens tokens
                                                  :speed speed)))))))
                         (when (and (not terminal)
                                    (stringp reasoning-text)
                                    (> (length reasoning-text) 0))
                           (let ((callbacks
                                  (funcall parser-callbacks-function)))
                             (deslop-llm--emit-status callbacks "Thinking"))
                           (funcall emit-delta-function
                                    :thinking reasoning-text))
                         (when (and (not terminal)
                                    (stringp content)
                                    (> (length content) 0))
                           (deslop-llm--reasoning-feed
                            reasoning content
                            (funcall parser-callbacks-function))))))))))
       (setq handle-events-function
             (lambda (events)
               (dolist (event events)
                 (unless terminal
                    (pcase (plist-get event :type)
                      (:done
                       (funcall event-function :on-event
                                (list :type :stream-done))
                       (unless terminal
                         (funcall finish-success-function "stream done")))
                     (:json
                      (funcall handle-json-function
                               (plist-get event :value))))))))
       (setq cleanup-files-function
              (lambda ()
                (unless cleaned
                  (setq cleaned t)
                  (dolist (buffer (list proc-buf stderr-buf))
                    (when (and buffer (buffer-live-p buffer))
                      (deslop-llm--cleanup-call #'kill-buffer buffer)))
                  (dolist (file (list headers-file config-file))
                    (when (and file (file-exists-p file))
                      (deslop-llm--cleanup-call #'delete-file file))))))
       (setq complete-function
             (lambda (result)
               (unless terminal
                 (setq terminal t)
                 (when (and (processp proc) (process-live-p proc))
                   (process-put proc :deslop-settled t)
                   (deslop-llm--cleanup-call #'delete-process proc))
                 (funcall cleanup-files-function)
                 (funcall event-function :on-event
                          (list :type :transport-done :result result))
                 (funcall event-function :on-done result))))
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
                                    (let ((detail
                                           (condition-case nil
                                               (let* ((payload
                                                       (deslop-llm--json-read-string
                                                        response-preview))
                                                      (json-error
                                                       (deslop-llm--json-get
                                                        "error" payload)))
                                                 (deslop-llm--json-get
                                                  "message" json-error))
                                             (error nil))))
                                      (if detail
                                          (format "HTTP status %s: %s"
                                                  status detail)
                                        (format "unexpected HTTP status %s"
                                                status))))))
                         ((and (plist-get profile :require-sse)
                               (or (null content-type)
                                   (let ((case-fold-search t))
                                     (not (string-match-p
                                           "text/event-stream"
                                           content-type)))))
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
                                   (let* ((reported
                                        (and usage
                                             (deslop-llm--json-get
                                              "completion_tokens" usage)))
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
                                              :provider (or actual-provider
                                                            (plist-get profile
                                                                       :name))
                                              :client
                                              (deslop-provider-client-name
                                               profile
                                               (list :provider actual-provider))
                                              :reported-model actual-model
                                              :reported-provider actual-provider
                                              :model (or actual-model model))))
                                     (funcall complete-function result)))))))))
                            ((error quit)
                    (funcall fail-function
                             (deslop-error-create
                              :kind 'protocol
                              :message (error-message-string finish-error))))))
                (unless terminal
                  (funcall cleanup-files-function))))
       (setq finish-error-function
              (lambda (process-event)
                (unless terminal
                  (let ((status (process-exit-status proc))
                        (stderr (when (buffer-live-p stderr-buf)
                                  (with-current-buffer stderr-buf
                                    (string-trim
                                     (substring
                                      (buffer-string) 0
                                      (min (buffer-size)
                                           deslop-max-error-body-bytes)))))))
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
              (dolist (limit (list deslop-max-output-tokens
                                   deslop-max-response-bytes
                                   deslop-max-sse-event-bytes
                                   deslop-max-output-chars))
                (unless (and (integerp limit) (> limit 0))
                  (error "deslop transport limits must be positive integers")))
              (deslop-llm--validated-url profile)
              (setq json-data
                    (encode-coding-string (json-encode body) 'utf-8))
              (setq proc-buf (generate-new-buffer " *deslop-curl*"))
              (setq stderr-buf
                    (generate-new-buffer " *deslop-curl-stderr*"))
              (setq headers-file (make-temp-file "deslop-headers-"))
              (set-file-modes headers-file #o600)
              (setq config-file (deslop-llm--curl-config profile))
              (setq proc
                   (make-process
                    :name (format "deslop-curl-%s-%s"
                                  (or (plist-get profile :name) "request")
                                  (or (plist-get profile :job-id) "unknown"))
                     :buffer proc-buf
                     :stderr stderr-buf
                     :connection-type 'pipe
                     :noquery t
                     :command (deslop-llm--curl-command
                               profile headers-file config-file)
                     :filter
                     (lambda (_process string)
                       (unless terminal
                         (setq received-bytes
                               (+ received-bytes (string-bytes string)))
                         (let ((remaining
                                (- deslop-max-error-body-bytes
                                   (length response-preview))))
                           (when (> remaining 0)
                             (setq response-preview
                                   (concat response-preview
                                           (substring string 0
                                                      (min remaining
                                                           (length string)))))
                             (while (> (string-bytes response-preview)
                                       deslop-max-error-body-bytes)
                                (setq response-preview
                                      (substring response-preview 0 -1)))))
                         (if (> received-bytes deslop-max-response-bytes)
                             (funcall
                              fail-function
                              (deslop-error-create
                               :kind 'resource
                               :message
                               "response exceeds deslop-max-response-bytes"))
                           (condition-case filter-error
                               (funcall handle-events-function
                                        (deslop-llm--sse-feed sse string))
                              ((error quit)
                              (funcall fail-function
                                       (deslop-error-create
                                        :kind 'protocol
                                        :message
                                        (error-message-string
                                         filter-error))))))))
                     :sentinel
                     (lambda (process event-string)
                       (when (memq (process-status process)
                                   '(exit signal closed))
                         (if terminal
                             (funcall cleanup-files-function)
                           (if (process-get process :deslop-cancelled)
                             (progn
                               (funcall fail-function
                                        (deslop-error-create
                                         :kind 'cancelled
                                         :message "Process cancelled")))
                             (if (= (process-exit-status process) 0)
                                 (funcall finish-success-function event-string)
                               (funcall finish-error-function event-string))))))))
              (funcall event-function :on-start proc)
              (when (and (not terminal) deslop-provider-discovery-enabled)
                (deslop-provider-discover
                 profile
                 (lambda (metadata)
                   (when (and metadata (not terminal))
                     (funcall event-function :on-metadata metadata)))))
              (unless terminal
                (process-send-string proc json-data)
               (process-send-eof proc)))
         ((error quit)
          (funcall fail-function
                    (deslop-error-create
                     :kind 'startup
                     :message (error-message-string startup-error)))))
        proc)))

(defalias 'deslop-llm-send #'deslop-llm-default)

(provide 'deslop-llm)

;;; deslop-llm.el ends here
