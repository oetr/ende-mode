;;; deslop-curl.el --- Curl resource helpers for deslop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools

;;; Commentary:

;; URL validation, authentication, headers, commands, and resource cleanup for
;; curl-based deslop transports.

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'deslop-core)

(defun deslop-curl--loopback-host-p (host)
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

(defun deslop-curl--validated-url (profile &optional url)
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
               (not (deslop-curl--loopback-host-p host)))
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

(defun deslop-curl--auth-secret (profile)
  "Resolve PROFILE's auth-source secret, or nil for unauthenticated hosts."
  (deslop-curl--validated-url profile)
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

(defun deslop-curl--valid-header-p (header)
  "Return non-nil when HEADER is one complete RFC-style header line."
  (and (stringp header)
       (not (string-match-p "[[:cntrl:]]" header))
       (string-match-p
        "\\`[!#$%&'*+.^_`|~0-9A-Za-z-]+:[ \t]*.*\\'" header)))

(defun deslop-curl--unsafe-profile-header-p (header)
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

(defun deslop-curl--config-escape (value)
  "Escape VALUE for a double-quoted curl config argument."
  (mapconcat (lambda (character)
               (if (memq character '(?\\ ?\"))
                   (string ?\\ character)
                 (char-to-string character)))
             value ""))

(defun deslop-curl--config (profile &optional internal-headers)
  "Create a temporary curl config for PROFILE and INTERNAL-HEADERS.
Return nil when PROFILE and INTERNAL-HEADERS have no authentication or extra
headers.  INTERNAL-HEADERS are generated by a trusted transport and are kept
separate from user-configured profile headers."
  (let* ((secret (deslop-curl--auth-secret profile))
          (headers (plist-get profile :request-headers))
          (all-headers (append headers internal-headers
                               (when secret
                                 (list (concat "Authorization: Bearer " secret))))))
    (when (or secret headers internal-headers)
      (dolist (header all-headers)
        (unless (deslop-curl--valid-header-p header)
          (error "Invalid request header"))
        (when (and (member header headers)
                   (deslop-curl--unsafe-profile-header-p header))
          (error "Unsafe request header must not be supplied by a profile")))
      (let ((contents
             (mapconcat
              (lambda (header)
                (format "header = \"%s\""
                        (deslop-curl--config-escape header)))
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

(defun deslop-curl--network-options (url)
  "Return hardened common curl options for URL."
  (let* ((parsed (url-generic-parse-url url))
         (host (url-host parsed)))
    (append
     (list "--globoff" "--proto" "=http,https"
           "--proto-redir" "=http,https")
     (when (or (not deslop-allow-environment-proxy)
               (deslop-curl--loopback-host-p host))
       (list "--noproxy" "*")))))

(defun deslop-curl--command (profile headers-file config-file)
  "Build curl command for PROFILE without embedding request data or secrets."
  (let ((url (deslop-curl--validated-url profile)))
    (append
      (list "curl" "--disable")
      (deslop-curl--network-options url)
      (list "--silent" "--show-error" "--no-buffer"
            "--request" "POST"
            "--speed-limit" "1"
            "--speed-time" (number-to-string deslop-response-idle-timeout)
            "--dump-header" headers-file
            "--header" "Content-Type: application/json"
            "--data-binary" "@-")
      (when config-file (list "--config" config-file))
      (append
       (when deslop-connect-timeout
         (list "--connect-timeout" (number-to-string deslop-connect-timeout)))
       (when deslop-max-response-bytes
         (list "--max-filesize" (number-to-string deslop-max-response-bytes)))
       (when deslop-request-timeout
         (list "--max-time" (number-to-string deslop-request-timeout)))
        (list "--url" url)))))

(cl-defun deslop-curl--start-request
    (&key profile url name command filter sentinel
          internal-headers
          (buffer-name " *deslop-curl*")
          (stderr-name " *deslop-curl-stderr*"))
  "Start a curl request and return its owned process resources.
COMMAND receives the generated headers and config file paths.  FILTER and
SENTINEL are installed on the process.  Startup failures clean up all
resources before being re-signaled to the caller."
  (let ((response-buffer (generate-new-buffer buffer-name))
        (stderr-buffer (generate-new-buffer stderr-name))
        (headers-file nil)
        (config-file nil)
        (process nil)
        (complete nil))
    (unwind-protect
        (progn
          (deslop-curl--validated-url profile url)
          (setq headers-file (make-temp-file "deslop-headers-"))
          (set-file-modes headers-file #o600)
          (setq config-file (deslop-curl--config profile internal-headers))
          (setq process
                (make-process
                 :name name
                 :buffer response-buffer
                 :stderr stderr-buffer
                 :connection-type 'pipe
                 :noquery t
                 :command (funcall command headers-file config-file)
                 :filter filter
                 :sentinel sentinel))
          (process-put process :deslop-response-buffer response-buffer)
          (process-put process :deslop-stderr-buffer stderr-buffer)
          (process-put process :deslop-headers-file headers-file)
          (process-put process :deslop-config-file config-file)
          (setq complete t)
          (list :process process
                :response-buffer response-buffer
                :stderr-buffer stderr-buffer
                :headers-file headers-file
                :config-file config-file))
      (unless complete
        (when (and (processp process) (process-live-p process))
          (deslop--ignore-errors #'delete-process process))
        (deslop-curl--cleanup-resources
         (list response-buffer stderr-buffer)
         (list headers-file config-file))))))

(defun deslop-curl--response-headers (file)
  "Return (STATUS . CONTENT-TYPE) from curl header FILE."
  (when (file-readable-p file)
    (let ((size (file-attribute-size (file-attributes file))))
      (when (and deslop-max-response-header-bytes
                 (> size deslop-max-response-header-bytes))
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

(defun deslop-curl--cleanup-resources (buffers files)
  "Kill BUFFERS and delete FILES, ignoring cleanup failures."
  (dolist (buffer buffers)
    (when (buffer-live-p buffer)
      (deslop--ignore-errors #'kill-buffer buffer)))
  (dolist (file files)
    (when (and file (file-exists-p file))
      (deslop--ignore-errors #'delete-file file))))

(provide 'deslop-curl)

;;; deslop-curl.el ends here
