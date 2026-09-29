;;; ende-curl.el --- Curl resource helpers for ende -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools

;;; Commentary:

;; URL validation, authentication, headers, commands, and resource cleanup for
;; curl-based ende transports.

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'ende-core)
(require 'ende-transport)

(defun ende-curl-local-directory ()
  "Return an existing local directory for provider processes and temp files.
Reject remote or relative `temporary-file-directory' values rather than
resolving them against a possibly remote source buffer.  Provider requests
from remote buffers are supported, but their processes always run locally."
  (unless (and (stringp temporary-file-directory)
               (file-name-absolute-p temporary-file-directory)
               (not (file-remote-p temporary-file-directory))
               (file-directory-p temporary-file-directory))
    (user-error "Ende requires an existing absolute local temporary-file-directory"))
  (file-name-as-directory (expand-file-name temporary-file-directory)))

(defun ende-curl--loopback-host-p (host)
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

(defun ende-curl--validated-url (profile &optional url)
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
               (not (ende-curl--loopback-host-p host)))
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

(defun ende-curl--auth-secret (profile)
  "Resolve PROFILE's auth-source secret, or nil for unauthenticated hosts."
  (ende-curl--validated-url profile)
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

(defun ende-curl--valid-header-p (header)
  "Return non-nil when HEADER is one complete RFC-style header line."
  (and (stringp header)
       (not (string-match-p "[[:cntrl:]]" header))
       (string-match-p
        "\\`[!#$%&'*+.^_`|~0-9A-Za-z-]+:[ \t]*.*\\'" header)))

(defun ende-curl--unsafe-profile-header-p (header)
  "Return non-nil when HEADER can affect routing, framing, or credentials."
  (let* ((name (and (stringp header)
                    (car (split-string header ":"))))
         (downcased (and name (downcase name))))
    (and downcased
         (or (member downcased
                     '("host" "content-length" "transfer-encoding"
                       "content-type" "accept" "connection" "trailer"
                       "upgrade" "expect" "proxy-connection"))
             (string-match-p
              "\\(?:auth\\|token\\|key\\|secret\\|cookie\\|credential\\)"
              downcased)))))

(defun ende-curl--config-escape (value)
  "Escape VALUE for a double-quoted curl config argument."
  (mapconcat (lambda (character)
               (if (memq character '(?\\ ?\"))
                   (string ?\\ character)
                 (char-to-string character)))
             value ""))

(defun ende-curl--config (profile &optional internal-headers)
  "Create a temporary curl config for PROFILE and INTERNAL-HEADERS.
Return nil when PROFILE and INTERNAL-HEADERS have no authentication or extra
headers.  INTERNAL-HEADERS are generated by a trusted transport and are kept
separate from user-configured profile headers."
  (let* ((default-directory (ende-curl-local-directory))
         (secret (ende-curl--auth-secret profile))
         (headers (plist-get profile :request-headers))
         (all-headers (append headers internal-headers
                              (when secret
                                (list (concat "Authorization: Bearer " secret))))))
    (when (or secret headers internal-headers)
      (dolist (header all-headers)
        (unless (ende-curl--valid-header-p header)
          (error "Invalid request header"))
        (when (and (member header headers)
                   (ende-curl--unsafe-profile-header-p header))
          (error "Unsafe request header must not be supplied by a profile")))
      (let ((contents
             (mapconcat
              (lambda (header)
                (format "header = \"%s\""
                        (ende-curl--config-escape header)))
              all-headers "\n"))
            (file nil)
            (complete nil))
        (unwind-protect
            (progn
              (setq file (make-temp-file "ende-curl-" nil ".conf"))
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

(defun ende-curl--network-options (url)
  "Return hardened common curl options for URL."
  (let* ((parsed (url-generic-parse-url url))
         (host (url-host parsed)))
    (append
     (list "--globoff" "--proto" "=http,https"
           "--proto-redir" "=http,https")
     (when (or (not ende-allow-environment-proxy)
               (ende-curl--loopback-host-p host))
       (list "--noproxy" "*")))))

;;;; Shared curl command and resource APIs

(defun ende-curl-base-command (url)
  "Return the common curl command prefix for URL."
  (append (list "curl" "--disable"
                "--user-agent" ende-client-name)
          (ende-curl--network-options url)))

(defun ende-curl-response-options (headers-file)
  "Return common curl options for a streamed response."
  (list "--silent" "--show-error" "--no-buffer"
        "--dump-header" headers-file))

(defun ende-curl-stderr-filter (buffer process chunk)
  "Append CHUNK to BUFFER without exceeding the stderr resource ceiling."
  (when (and (buffer-live-p buffer) (stringp chunk))
    (let* ((used (or (and (processp process)
                          (process-get process :ende-stderr-bytes))
                     0))
           (total (+ used (string-bytes chunk)))
           (limit ende-max-stderr-bytes)
           (remaining (and limit (- limit used)))
           (prefix (if remaining
                       (and (> remaining 0)
                            (ende-transport-byte-prefix chunk remaining))
                     chunk)))
      (when (processp process)
        (process-put process :ende-stderr-bytes total))
      (when prefix
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert prefix))))))

(defun ende-curl-timeout-options (&optional total-timeout max-filesize)
  "Return configured curl timeout and response-size options."
  (append
   (when ende-connect-timeout
     (list "--connect-timeout"
           (number-to-string ende-connect-timeout)))
   (when total-timeout
     (list "--max-time" (number-to-string total-timeout)))
   (when max-filesize
     (list "--max-filesize" (number-to-string max-filesize)))))

(defun ende-curl-command (profile headers-file config-file)
  "Build curl command for PROFILE without embedding request data or secrets."
  (ende-curl-json-command
   profile headers-file config-file "POST" "text/event-stream" t))

(defun ende-curl-json-command
    (profile headers-file config-file method accept send-body &optional url)
  "Build a JSON curl command for PROFILE.
METHOD is the HTTP method, ACCEPT is the response media type, and SEND-BODY
controls whether curl reads a request body from stdin.  Request data and
secrets are deliberately kept out of the command arguments."
  (let ((url (ende-curl--validated-url profile url)))
    (append
     (ende-curl-base-command url)
     (ende-curl-response-options headers-file)
     (list "--request" method
           "--header" "Content-Type: application/json"
           "--header" (concat "Accept: " accept))
     (when send-body (list "--data-binary" "@-"))
     (when config-file (list "--config" config-file))
     (ende-curl-timeout-options
      ende-request-timeout ende-max-response-bytes)
     (list "--url" url))))

(defun ende-curl--cancel-idle-timer (process)
  "Cancel PROCESS's response inactivity timer, if any."
  (let ((timer (process-get process :ende-idle-timer)))
    (when timer
      (cancel-timer timer)
      (process-put process :ende-idle-timer nil))))

(defun ende-curl--check-idle (process)
  "Stop PROCESS if its captured response inactivity limit has expired."
  (ende-curl--cancel-idle-timer process)
  (when (and (process-live-p process)
             (not (process-get process :ende-settled)))
    (let* ((limit (process-get process :ende-idle-limit))
           (remaining (- (cdr limit)
                         (- (float-time)
                            (process-get process :ende-last-response-time)))))
      (if (> remaining 0)
          (process-put process :ende-idle-timer
                       (run-at-time remaining nil
                                    #'ende-curl--check-idle process))
        ;; Record the cause before deletion invokes the transport sentinel.
        (process-put
         process :ende-idle-error
         (ende-error-create
          :kind 'timeout
          :message (format "No response data received for %s seconds (`%s`)."
                           (cdr limit) (car limit))))
        (delete-process process)))))

(defun ende-curl-watch-idle (process)
  "Watch PROCESS for response inactivity using the current timeout setting.
Wrap its filter and sentinel, preserving their transport-specific behavior."
  (let* ((option 'ende-response-idle-timeout)
         (limit (cons option (symbol-value option)))
         (filter (process-filter process))
         (sentinel (process-sentinel process)))
    (process-put process :ende-idle-limit limit)
    (process-put process :ende-last-response-time (float-time))
    (set-process-filter
     process (lambda (proc chunk)
               (unless (string-empty-p chunk)
                 (process-put proc :ende-last-response-time (float-time)))
               (funcall filter proc chunk)))
    (set-process-sentinel
     process (lambda (proc event)
               (when (memq (process-status proc) '(exit signal closed failed))
                 (ende-curl--cancel-idle-timer proc))
               (funcall sentinel proc event)))
    (ende-curl--check-idle process)))

(cl-defun ende-curl-start-request
    (&key profile url name command filter sentinel
          internal-headers
          (buffer-name " *ende-curl*")
          (stderr-name " *ende-curl-stderr*"))
  "Start a curl request and return its owned process resources.
COMMAND receives the generated headers and config file paths.  FILTER and
SENTINEL are wrapped to track response inactivity and release its timer.
An idle failure is stored in the process property `:ende-idle-error'
before SENTINEL runs.  Startup failures clean up all resources before
being re-signaled to the caller."
  (let* ((default-directory (ende-curl-local-directory))
         (response-buffer (generate-new-buffer buffer-name))
         (stderr-buffer (generate-new-buffer stderr-name))
         (stderr-process nil)
         (headers-file nil)
         (config-file nil)
         (process nil)
         (complete nil))
    (unwind-protect
        (progn
          (ende-validate-resource-options)
          (ende-curl--validated-url profile url)
          (setq headers-file (make-temp-file "ende-headers-"))
          (set-file-modes headers-file #o600)
          (setq config-file (ende-curl--config profile internal-headers))
          (setq stderr-process
                (make-pipe-process
                 :name (generate-new-buffer-name "ende-curl-stderr")
                 :buffer nil
                 :filter (apply-partially #'ende-curl-stderr-filter
                                          stderr-buffer)
                 :noquery t))
          (setq process
                (make-process
                 :name name
                 :buffer response-buffer
                 :stderr stderr-process
                 :connection-type 'pipe
                 :noquery t
                 :command (funcall command headers-file config-file)
                 :filter filter
                 :sentinel sentinel))
          (process-put process :ende-response-buffer response-buffer)
          (process-put process :ende-stderr-buffer stderr-buffer)
          (process-put process :ende-headers-file headers-file)
          (process-put process :ende-config-file config-file)
          (process-put process :ende-stderr-process stderr-process)
          (ende-curl-watch-idle process)
          (setq complete t)
          (list :process process
                :response-buffer response-buffer
                :stderr-buffer stderr-buffer
                :stderr-process stderr-process
                :headers-file headers-file
                :config-file config-file))
      (unless complete
        (when (processp process)
          (ende-curl--cancel-idle-timer process))
        (when (and (processp process) (process-live-p process))
          (ende-ignore-errors #'delete-process process))
        (when (processp stderr-process)
          (ende-ignore-errors #'delete-process stderr-process))
        (ende-curl-cleanup-resources
         (list response-buffer stderr-buffer)
         (list headers-file config-file))))))

(defun ende-curl-event-stream-p (content-type)
  "Return non-nil when CONTENT-TYPE denotes an SSE response."
  (when (stringp content-type)
    (let ((case-fold-search t))
      (string-match-p
       "\\`[ \t]*text/event-stream\\(?:[ \t]*;\\|[ \t]*\\'\\)"
       content-type))))

(defun ende-curl-response-headers (file)
  "Return final (STATUS . CONTENT-TYPE) from curl header FILE."
  (when (file-readable-p file)
    (let ((size (file-attribute-size (file-attributes file))))
      (when (and ende-max-response-header-bytes
                 (> size ende-max-response-header-bytes))
        (error "Response headers exceed ende-max-response-header-bytes")))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((case-fold-search t)
            (status nil)
            (content-type nil))
        (while (re-search-forward
                "^HTTP/[0-9]+\\(?:\\.[0-9]+\\)?[ \t]+\\([0-9]+\\)" nil t)
          (let ((response-status (string-to-number (match-string 1)))
                (block-end (or (save-excursion
                                 (when (re-search-forward "^\r?$" nil t)
                                   (line-beginning-position)))
                               (point-max))))
            (setq status response-status
                  content-type nil)
            (save-excursion
              (when (re-search-forward
                     "^content-type:[ \t]*\\([^\r\n]+\\)" block-end t)
                (setq content-type (string-trim (match-string 1)))))))
        (cons status content-type)))))

(defun ende-curl-cleanup-resources (buffers files &optional processes)
  "Kill BUFFERS and delete FILES, ignoring cleanup failures."
  (ende-transport-cleanup-resources buffers files processes))

(provide 'ende-curl)

;;; ende-curl.el ends here
