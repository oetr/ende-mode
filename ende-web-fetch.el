;;; ende-web-fetch.el --- Bounded unauthenticated web fetch -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: tools, url

;;; Commentary:

;; A synchronous, cancellable curl transport for the model-facing web_fetch
;; tool.  Every URL is authorized before DNS, all DNS answers are classified,
;; and DNS names are pinned with curl --resolve.  Provider credentials and
;; transport profiles are intentionally not involved.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-expand)
(require 'url-parse)
(require 'ende-url)

(defcustom ende-web-fetch-connect-timeout 10
  "Maximum seconds allowed for one HTTP connection attempt."
  :type 'integer
  :group 'ende)

(defcustom ende-web-fetch-call-timeout 30
  "Maximum seconds for DNS, redirects, and all response transfers combined."
  :type 'integer
  :group 'ende)

(defcustom ende-web-fetch-max-redirects 5
  "Maximum redirects followed by `web_fetch'."
  :type 'integer
  :group 'ende)

(defcustom ende-web-fetch-max-response-bytes (* 2 1024 1024)
  "Maximum cumulative response body bytes across redirects."
  :type 'integer
  :group 'ende)

(defcustom ende-web-fetch-max-response-header-bytes (* 64 1024)
  "Maximum cumulative response header bytes across redirects."
  :type 'integer
  :group 'ende)

(defcustom ende-web-fetch-max-output-chars 50000
  "Maximum characters returned by one successful `web_fetch'."
  :type 'integer
  :group 'ende)

(defconst ende-web-fetch--address-blocks
  '((#x00000000 8 unspecified)
    (#x0a000000 8 private)
    (#x64400000 10 special)
    (#x7f000000 8 loopback)
    (#xa9fe0000 16 link-local)
    (#xac100000 12 private)
    (#xc0000000 24 special)
    (#xc0000200 24 documentation)
    (#xc0586300 24 special)
    (#xc01fc400 24 special)
    (#xc034c100 24 special)
    (#xc0a80000 16 private)
    (#xc0af3000 24 special)
    (#xc6120000 15 special)
    (#xc6336400 24 documentation)
    (#xc7007100 24 documentation)
    (#xe0000000 4 multicast)
    (#xf0000000 4 reserved))
  "IPv4 non-global ranges rejected by web_fetch, as (NETWORK BITS CLASS).

The explicit private exception is limited to the RFC 1918 entries and 127/8.
Other non-global classes, including shared, link-local, documentation,
benchmarking, multicast, and reserved ranges remain denied.")

(cl-defstruct (ende-web-fetch--transfer (:constructor ende-web-fetch--make-transfer))
  "One fetch call's deadline, cancellation state, process, and byte totals."
  deadline cancel-function cancelled process header-bytes body-bytes failure)

(cl-defstruct (ende-web-fetch--hop (:constructor ende-web-fetch--make-hop))
  "One pinned HTTP response and its private buffers."
  body-buffer stderr-buffer pending-header in-body status headers)

(defun ende-web-fetch--local-directory ()
  "Return the existing local directory used to launch network helpers."
  (unless (and (stringp temporary-file-directory)
               (file-name-absolute-p temporary-file-directory)
               (not (file-remote-p temporary-file-directory))
               (file-directory-p temporary-file-directory))
    (user-error "Ende requires an existing absolute local temporary-file-directory"))
  (file-name-as-directory (expand-file-name temporary-file-directory)))

(defun ende-web-fetch--prefix-p (number network bits width)
  "Return non-nil if NUMBER is in NETWORK/BITS for address WIDTH."
  (= (ash number (- bits width)) (ash network (- bits width))))

(defun ende-web-fetch--ipv4-string (number)
  "Format numeric IPv4 NUMBER as canonical dotted decimal."
  (format "%d.%d.%d.%d"
          (logand (ash number -24) 255)
          (logand (ash number -16) 255)
          (logand (ash number -8) 255)
          (logand number 255)))

(defun ende-web-fetch--ipv4-class (number)
  "Return the fixed special-use class for IPv4 NUMBER, or `public'."
  (or (cl-loop for (network bits class) in ende-web-fetch--address-blocks
               when (ende-web-fetch--prefix-p number network bits 32)
               return class)
      'public))

(defun ende-web-fetch--ipv6-class (number groups)
  "Return the fixed special-use class for IPv6 NUMBER and GROUPS."
  (cond
   ((= number 0) 'unspecified)
   ((= number 1) 'loopback)
   ((ende-web-fetch--prefix-p number #xfc000000000000000000000000000000 7 128)
    'private)
   ((ende-web-fetch--prefix-p number #xfe800000000000000000000000000000 10 128)
    'link-local)
   ((ende-web-fetch--prefix-p number #xff000000000000000000000000000000 8 128)
    'multicast)
   ((ende-web-fetch--prefix-p number #x20010db8000000000000000000000000 32 128)
    'documentation)
   ((ende-web-fetch--prefix-p number #x20010000000000000000000000000000 23 128)
    'special)
   ((ende-web-fetch--prefix-p number #x20020000000000000000000000000000 16 128)
    'transition)
   ((ende-web-fetch--prefix-p number #x0064ff9b000000000000000000000000 96 128)
    'transition)
   ((ende-web-fetch--prefix-p number #x0064ff9b000100000000000000000000 48 128)
    'transition)
   ((ende-web-fetch--prefix-p number #x3fff0000000000000000000000000000 20 128)
    'documentation)
   ((and (memq (nth 6 groups) '(#x5efe))
         (memq (nth 5 groups) '(0 #x0200)))
    'transition)
   ((ende-web-fetch--prefix-p number #x20000000000000000000000000000000 3 128)
    'public)
   (t 'special)))

(defun ende-web-fetch-classify-address (address)
  "Parse numeric ADDRESS and return (CLASS . CANONICAL-ADDRESS).
Unsupported or malformed forms signal rather than bypassing classification."
  (unless (and (stringp address) (not (string-empty-p address)))
    (error "resolver returned an empty address"))
  (let ((ipv4 (ende-url--ipv4-number address)))
    (if ipv4
        (cons (ende-web-fetch--ipv4-class ipv4)
              (ende-web-fetch--ipv4-string ipv4))
      (when (string-match-p "[^0-9a-fA-F:.]" address)
        (error "resolver returned a non-numeric address: %s" address))
      (let* ((groups (ende-url--ipv6-groups address))
             (number (ende-url--ipv6-number groups)))
        (if (= (ash number -32) #xffff)
            (let* ((mapped (logand number #xffffffff))
                   (class (ende-web-fetch--ipv4-class mapped)))
              (cons class (ende-web-fetch--ipv4-string mapped)))
          (when (string-match-p "\\." address)
            (error "unsupported embedded IPv4 transition address"))
          (cons (ende-web-fetch--ipv6-class number groups)
                (ende-url--ipv6-string groups)))))))

(defun ende-web-fetch--address-allowed-p (class allow-private)
  "Return non-nil when address CLASS passes the selected policy."
  (or (eq class 'public)
      (and allow-private (memq class '(private loopback)))))

(defun ende-web-fetch--assert-addresses (addresses allow-private)
  "Require every parsed (CLASS . ADDRESS) in ADDRESSES to be permitted."
  (unless addresses
    (error "hostname resolved to no addresses"))
  (dolist (entry addresses)
    (unless (ende-web-fetch--address-allowed-p (car entry) allow-private)
      (error "destination address is blocked by web_fetch policy (%s): %s"
             (car entry) (cdr entry))))
  addresses)

(defun ende-web-fetch--cancelled-p (transfer)
  "Signal `quit' if TRANSFER has been cancelled."
  (when (ende-web-fetch--transfer-cancelled transfer)
    (signal 'quit nil)))

(defun ende-web-fetch--remaining-time (transfer)
  "Return TRANSFER's remaining deadline time in seconds."
  (- (ende-web-fetch--transfer-deadline transfer) (float-time)))

(defun ende-web-fetch--check-deadline (transfer)
  "Signal when TRANSFER's overall deadline has expired."
  (ende-web-fetch--cancelled-p transfer)
  (when (<= (ende-web-fetch--remaining-time transfer) 0)
    (error "web_fetch exceeded its %s-second overall deadline"
           ende-web-fetch-call-timeout)))

(defun ende-web-fetch--append-resolver-output (buffer process chunk)
  "Append bounded resolver CHUNK to BUFFER for PROCESS."
  (let ((size (+ (with-current-buffer buffer (buffer-size))
                 (string-bytes chunk))))
    (if (> size ende-web-fetch-max-response-header-bytes)
        (progn
          (process-put process :ende-web-fetch-resolver-overflow t)
          (when (process-live-p process) (delete-process process)))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert chunk)))))

(defun ende-web-fetch--wait-process (transfer process deadline)
  "Cancellably wait for PROCESS until DEADLINE under TRANSFER's deadline."
  (while (process-live-p process)
    (ende-web-fetch--check-deadline transfer)
    (let ((remaining (min deadline (ende-web-fetch--remaining-time transfer))))
      (when (<= remaining 0)
        (when (process-live-p process) (delete-process process))
        (error "web_fetch DNS or connection deadline expired"))
      (accept-process-output process (min 0.1 remaining))))
  (ende-web-fetch--check-deadline transfer))

(defun ende-web-fetch--resolve-host (transfer host)
  "Resolve DNS HOST using a bounded local helper process."
  (let* ((default-directory (ende-web-fetch--local-directory))
         (buffer (generate-new-buffer " *ende-web-fetch-dns*"))
         (process nil)
         (complete nil)
         (deadline (+ (float-time)
                      (min ende-web-fetch-connect-timeout
                           (ende-web-fetch--remaining-time transfer)))))
    (unwind-protect
        (progn
          (set-buffer-multibyte nil)
          (setq process
                (make-process
                 :name "ende-web-fetch-dns"
                 :buffer nil
                 :command (list "getent" "ahosts" host)
                 :connection-type 'pipe
                 :coding 'binary
                 :noquery t
                 :filter (apply-partially
                          #'ende-web-fetch--append-resolver-output buffer)))
          (setf (ende-web-fetch--transfer-process transfer) process)
          (ende-web-fetch--wait-process transfer process deadline)
          (when (process-get process :ende-web-fetch-resolver-overflow)
            (error "DNS resolver output exceeded its bound"))
          (unless (and (eq (process-status process) 'exit)
                       (zerop (process-exit-status process)))
            (error "DNS resolution failed for %s" host))
          (let ((addresses nil))
            (with-current-buffer buffer
              (dolist (line (split-string (buffer-string) "\n" t))
                (let ((token (car (split-string line "[ \t]+" t))))
                  (when token
                    (push (ende-web-fetch-classify-address token) addresses)))))
            (setq complete t)
            (delete-dups (nreverse addresses))))
      (when (and (processp process) (process-live-p process))
        (delete-process process))
      (when (eq (ende-web-fetch--transfer-process transfer) process)
        (setf (ende-web-fetch--transfer-process transfer) nil))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (unless complete
        (ignore-errors (ende-web-fetch--check-deadline transfer))))))

(defun ende-web-fetch--destination-addresses (transfer normalized allow-private)
  "Return all vetted destination addresses for NORMALIZED URL."
  (let* ((kind (plist-get normalized :host-kind))
         (addresses
          (pcase kind
            ('dns
             (ende-web-fetch--resolve-host
              transfer (plist-get normalized :host)))
            ('ipv4
             (list (ende-web-fetch-classify-address
                    (plist-get normalized :host))))
            ('ipv4-mapped-ipv6
             (list (ende-web-fetch-classify-address
                    (substring (plist-get normalized :host) 1 -1))))
            ('ipv6
             (list (ende-web-fetch-classify-address
                    (substring (plist-get normalized :host) 1 -1))))
            (_ (error "unsupported URL host type for web_fetch")))))
    (ende-web-fetch--assert-addresses addresses allow-private)))

(defun ende-web-fetch--resolve-option (host port address)
  "Build a curl --resolve value pinning HOST and PORT to ADDRESS."
  (format "%s:%d:%s" host port
          (if (string-match-p ":" address)
              (concat "[" address "]")
            address)))

(defun ende-web-fetch--command (normalized address deadline)
  "Build hardened curl argv for NORMALIZED URL pinned to ADDRESS until DEADLINE."
  (let* ((url (plist-get normalized :url))
         (host (plist-get normalized :host))
         (port (plist-get normalized :port))
         (remaining (max 0.1 (- deadline (float-time))))
         (dns-host (eq (plist-get normalized :host-kind) 'dns)))
    (append
     (list "curl" "--disable" "--silent" "--show-error"
           "--globoff" "--path-as-is" "--proto" "=http,https"
           "--proxy" "" "--noproxy" "*"
           "--connect-timeout"
           (number-to-string
            (min ende-web-fetch-connect-timeout remaining))
           "--max-time" (number-to-string remaining)
           "--include"
           "--header" "Accept-Encoding: identity"
           "--header"
           "Accept: text/html, text/plain, application/json, application/xml, text/xml")
     (when dns-host
       (list "--resolve"
             (ende-web-fetch--resolve-option host port address)))
     (list "--url" url))))

(defun ende-web-fetch--header-value (headers name)
  "Return the last value for NAME from response HEADERS."
  (cdr (assoc-string name headers t)))

(defun ende-web-fetch--parse-header-block (block)
  "Parse a completed raw HTTP header BLOCK to (STATUS . HEADERS)."
  (let* ((text (replace-regexp-in-string "\r" "" block t t))
         (lines (split-string text "\n" nil))
         (status-line (car lines))
         (case-fold-search t)
         (status nil)
         (headers nil))
    (unless (and status-line
                 (string-match
                  "\\`HTTP/[0-9]+\\(?:\\.[0-9]+\\)?[ \t]+\\([0-9][0-9][0-9]\\)"
                  status-line))
      (error "web_fetch received malformed HTTP response headers"))
    (setq status (string-to-number (match-string 1 status-line)))
    (dolist (line (cdr lines))
      (unless (string-empty-p line)
        (unless (string-match "\\`\\([^:]+\\):[ \t]*\\(.*\\)\\'" line)
          (error "web_fetch received a malformed HTTP header"))
        (let ((name (downcase (match-string 1 line)))
              (value (string-trim (match-string 2 line))))
          (let ((previous (assoc-string name headers t)))
            (when (and previous
                       (member name '("content-encoding" "content-type"
                                      "location"))
                       (not (string-equal (cdr previous) value)))
              (error "web_fetch received conflicting %s headers" name))
            (setq headers
                  (cons (cons name value)
                        (cl-remove-if
                         (lambda (header) (string-equal (car header) name))
                         headers)))))))
    (cons status headers)))

(defun ende-web-fetch--append-body (transfer hop bytes process)
  "Append response BYTES to HOP under TRANSFER's cumulative byte ceiling."
  (let ((total (+ (ende-web-fetch--transfer-body-bytes transfer)
                  (string-bytes bytes))))
    (if (> total ende-web-fetch-max-response-bytes)
        (progn
          (setf (ende-web-fetch--transfer-failure transfer)
                "web_fetch response bodies exceed the configured byte limit")
          (when (process-live-p process) (delete-process process)))
      (setf (ende-web-fetch--transfer-body-bytes transfer) total)
      (with-current-buffer (ende-web-fetch--hop-body-buffer hop)
        (goto-char (point-max))
        (insert bytes)))))

(defun ende-web-fetch--response-filter (transfer hop process chunk)
  "Parse HTTP response CHUNK for PROCESS, enforcing live byte ceilings."
  (unless (or (string-empty-p chunk)
              (ende-web-fetch--transfer-failure transfer))
    (condition-case caught
              (let ((pending (if (ende-web-fetch--hop-in-body hop)
                           ""
                         (concat (ende-web-fetch--hop-pending-header hop)
                                 chunk)))
              delimiter)
          (if (ende-web-fetch--hop-in-body hop)
              (ende-web-fetch--append-body transfer hop chunk process)
            (while (and pending (not (ende-web-fetch--hop-in-body hop)))
              (let ((crlf (string-match "\r\n\r\n" pending))
                    (lf (string-match "\n\n" pending)))
                (setq delimiter
                      (cond
                       ((and crlf (or (null lf) (<= crlf lf)))
                        (list crlf (+ crlf 4)))
                       (lf (list lf (+ lf 2)))))
                (if (null delimiter)
                    (progn
                      (when (> (+ (ende-web-fetch--transfer-header-bytes transfer)
                                  (string-bytes pending))
                               ende-web-fetch-max-response-header-bytes)
                        (error "web_fetch response headers exceed the configured byte limit"))
                      (setf (ende-web-fetch--hop-pending-header hop) pending)
                      (setq pending nil))
                  (let* ((start (car delimiter))
                         (end (cadr delimiter))
                         (block (substring pending 0 start))
                         (header-size (string-bytes (substring pending 0 end)))
                         (parsed (ende-web-fetch--parse-header-block block))
                         (status (car parsed)))
                    (cl-incf (ende-web-fetch--transfer-header-bytes transfer)
                             header-size)
                    (when (> (ende-web-fetch--transfer-header-bytes transfer)
                             ende-web-fetch-max-response-header-bytes)
                      (error "web_fetch response headers exceed the configured byte limit"))
                    (setf (ende-web-fetch--hop-status hop) status
                          (ende-web-fetch--hop-headers hop) (cdr parsed)
                          (ende-web-fetch--hop-pending-header hop) "")
                    (setq pending (substring pending end))
                    (if (and (>= status 100) (< status 200))
                        (when (= status 101)
                          (error "web_fetch does not support protocol upgrades"))
                      (setf (ende-web-fetch--hop-in-body hop) t)
                      (when (not (string-empty-p pending))
                        (ende-web-fetch--append-body
                          transfer hop pending process)))))))))
      ((error quit)
        (setf (ende-web-fetch--transfer-failure transfer)
              (error-message-string caught))
        (when (process-live-p process)
          (delete-process process))))))

(defun ende-web-fetch--new-hop (transfer normalized address)
  "Perform one authorized, address-pinned HTTP request for NORMALIZED."
  (let* ((default-directory (ende-web-fetch--local-directory))
         (body (generate-new-buffer " *ende-web-fetch-body*"))
         (stderr (generate-new-buffer " *ende-web-fetch-stderr*"))
         (hop (ende-web-fetch--make-hop
               :body-buffer body :stderr-buffer stderr
               :pending-header "" :in-body nil))
         (process nil)
         (success nil))
    (with-current-buffer body (set-buffer-multibyte nil))
    (with-current-buffer stderr (set-buffer-multibyte nil))
    (unwind-protect
        (progn
          (ende-web-fetch--check-deadline transfer)
          (setq process
                (make-process
                 :name "ende-web-fetch"
                 :buffer nil :stderr stderr
                 :command (ende-web-fetch--command
                           normalized address
                           (ende-web-fetch--transfer-deadline transfer))
                 :connection-type 'pipe :coding 'binary :noquery t
                 :filter (apply-partially
                          #'ende-web-fetch--response-filter transfer hop)))
          (setf (ende-web-fetch--transfer-process transfer) process)
          (while (process-live-p process)
            (ende-web-fetch--wait-process
             transfer process (ende-web-fetch--transfer-deadline transfer)))
          (when (ende-web-fetch--transfer-failure transfer)
            (error "%s" (ende-web-fetch--transfer-failure transfer)))
          (unless (eq (process-status process) 'exit)
            (error "web_fetch curl process did not exit normally"))
          (unless (zerop (process-exit-status process))
            (error "web_fetch transport failed: %s"
                   (let ((message (with-current-buffer stderr
                                    (decode-coding-string
                                     (buffer-string) 'utf-8))))
                     (if (string-empty-p (string-trim message))
                         "curl failed"
                       (string-trim message)))))
          (unless (ende-web-fetch--hop-status hop)
            (error "web_fetch received no HTTP response"))
          (setq success t)
          hop)
      (when (and (processp process) (process-live-p process))
        (delete-process process))
      (when (eq process (ende-web-fetch--transfer-process transfer))
        (setf (ende-web-fetch--transfer-process transfer) nil))
      (unless success
        (when (buffer-live-p body) (kill-buffer body))
        (when (buffer-live-p stderr) (kill-buffer stderr))))))

(defun ende-web-fetch--release-hop (hop)
  "Kill private buffers owned by completed HOP."
  (dolist (buffer (list (ende-web-fetch--hop-body-buffer hop)
                        (ende-web-fetch--hop-stderr-buffer hop)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun ende-web-fetch--text-content-type (headers)
  "Return normalized textual MIME type in HEADERS or signal."
  (let* ((content-type (ende-web-fetch--header-value headers "content-type"))
         (media-type (and content-type
                          (downcase (string-trim
                                     (car (split-string content-type ";")))))))
    (unless (and media-type
                 (or (string-prefix-p "text/" media-type)
                     (member media-type '("application/json" "application/xml"
                                          "text/xml" "application/xhtml+xml"))
                     (string-match-p
                      "\\`application/[^;]+[+]\\(?:json\\|xml\\)\\'"
                      media-type)))
      (error "web_fetch does not support content type %s"
             (or media-type "(missing)")))
    (cons media-type content-type)))

(defun ende-web-fetch--decode-body (bytes content-type)
  "Decode raw text BYTES using charset parameter of CONTENT-TYPE."
  (let* ((charset (and (string-match
                        "[;][ \t]*charset[ \t]*=[ \t]*\\(?:\"\\([^\"]+\\)\"\\|\\([^; \t]+\\)\\)"
                        content-type)
                       (or (match-string 1 content-type)
                           (match-string 2 content-type))))
         (coding (and charset (coding-system-from-name charset))))
    (when (and charset (null coding))
      (error "unsupported response character set: %s" charset))
    (decode-coding-string bytes (or coding 'utf-8))))

(defun ende-web-fetch--redirect-url (location current)
  "Resolve redirect LOCATION relative to CURRENT and normalize the result."
  (unless (and (stringp location) (not (string-empty-p location)))
    (error "web_fetch redirect has no Location header"))
  (let ((case-fold-search t))
    (ende-url-normalize
     (if (string-match-p "\\`https?://" location)
         location
       (url-expand-file-name location (plist-get current :url))))))

(defun ende-web-fetch (url authorize &optional max-chars register-cancellation)
  "Fetch URL under AUTHORIZE with bounded, unauthenticated HTTP.
AUTHORIZE receives a normalized URL plist and returns either nil or a plist
whose `:allow-private-addresses' field is non-nil.  MAX-CHARS may lower the
configured output ceiling.  REGISTER-CANCELLATION is called with a cancellation
function while work is active, then nil during cleanup."
  (unless (functionp authorize)
    (error "web_fetch requires an explicit URL authorization policy"))
  (let* ((initial (ende-url-normalize url))
         (requested (plist-get initial :url))
         (transfer (ende-web-fetch--make-transfer
                    :deadline (+ (float-time) ende-web-fetch-call-timeout)
                    :header-bytes 0 :body-bytes 0))
         (cancel (lambda ()
                   (setf (ende-web-fetch--transfer-cancelled transfer) t)
                   (let ((process (ende-web-fetch--transfer-process transfer)))
                     (when (and (processp process) (process-live-p process))
                       (delete-process process)))))
         (limit (or max-chars ende-web-fetch-max-output-chars))
         (current initial)
         (redirects 0)
         result)
    (unless (and (integerp limit) (> limit 0)
                 (<= limit ende-web-fetch-max-output-chars))
      (error "max_chars must be a positive integer no greater than %d"
             ende-web-fetch-max-output-chars))
    (unless (funcall authorize initial)
      (error "URL is not authorized by this chat's URL resources"))
    (when register-cancellation
      (funcall register-cancellation cancel))
    (unwind-protect
        (catch 'finished
          (while t
            (ende-web-fetch--check-deadline transfer)
            (let* ((permission (funcall authorize current))
                   (_ (unless permission
                        (error "redirect URL is not authorized by this chat's URL resources")))
                   (private (plist-get permission :allow-private-addresses))
                   (addresses (ende-web-fetch--destination-addresses
                               transfer current private))
                   (selected (cdar addresses))
                   (hop (ende-web-fetch--new-hop transfer current selected))
                   (status (ende-web-fetch--hop-status hop))
                   (headers (ende-web-fetch--hop-headers hop))
                   (location (ende-web-fetch--header-value headers "location")))
              (if (memq status '(301 302 303 307 308))
                  (if location
                      (progn
                        (ende-web-fetch--release-hop hop)
                        (when (>= redirects ende-web-fetch-max-redirects)
                          (error "web_fetch exceeded its redirect limit"))
                        (cl-incf redirects)
                        (setq current (ende-web-fetch--redirect-url
                                       location current)))
                    (setq result (cons hop current))
                    (throw 'finished result))
                 (setq result (cons hop current))
                (throw 'finished result)))))
      (when register-cancellation
        (ignore-errors (funcall register-cancellation nil)))
        (when (and (processp (ende-web-fetch--transfer-process transfer))
                   (process-live-p
                    (ende-web-fetch--transfer-process transfer)))
         (delete-process (ende-web-fetch--transfer-process transfer))))
    (let* ((hop (car result))
           (final (cdr result)))
      (unwind-protect
          (let* ((headers (ende-web-fetch--hop-headers hop))
                 (raw-content-type
                  (ende-web-fetch--header-value headers "content-type"))
                 (mime (ende-web-fetch--text-content-type headers))
                 (encoding
                  (ende-web-fetch--header-value headers "content-encoding")))
            (when (and encoding
                       (not (string-equal (downcase encoding) "identity")))
              (error "web_fetch does not support Content-Encoding: %s" encoding))
            (let* ((bytes (with-current-buffer
                              (ende-web-fetch--hop-body-buffer hop)
                            (buffer-string)))
                   (content (ende-web-fetch--decode-body
                             bytes (or raw-content-type (car mime))))
                   (truncated (> (length content) limit)))
              (list :requested_url requested
                    :final_url (plist-get final :url)
                    :status (ende-web-fetch--hop-status hop)
                    :content_type (cdr mime)
                    :content (if truncated (substring content 0 limit) content)
                    :truncated (if truncated t :json-false))))
        (ende-web-fetch--release-hop hop)))))

(provide 'ende-web-fetch)

;;; ende-web-fetch.el ends here
