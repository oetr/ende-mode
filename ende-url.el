;;; ende-url.el --- Strict URL policy helpers for Ende -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: tools, url

;;; Commentary:

;; URL normalization and matching shared by chat configuration and web_fetch.
;; This module deliberately does not perform network or file operations.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defun ende-url--valid-escapes-p (value)
  "Signal when VALUE contains a malformed percent escape."
  (let ((position 0))
    (while (string-match "%" value position)
      (let ((start (match-beginning 0)))
        (unless (and (<= (+ start 3) (length value))
                     (string-match-p
                      "\\`[[:xdigit:]][[:xdigit:]]\\'"
                      (substring value (1+ start) (+ start 3)))
                     (not (= (string-to-number
                              (substring value (1+ start) (+ start 3)) 16)
                             0)))
          (error "URL contains a malformed or NUL percent escape"))
        (setq position (+ start 3)))))
  value)

(defun ende-url--unreserved-p (character)
  "Return non-nil when CHARACTER is an RFC 3986 unreserved character."
  (or (and (>= character ?A) (<= character ?Z))
      (and (>= character ?a) (<= character ?z))
      (and (>= character ?0) (<= character ?9))
      (memq character '(?- ?. ?_ ?~))))

(defun ende-url--path-character-p (character)
  "Return non-nil when CHARACTER can safely remain literal in a URI path."
  (or (ende-url--unreserved-p character)
      (memq character '(?/ ?: ?@ ?! ?$ ?& ?' ?\( ?\) ?* ?+ ?, ?\; ?=))))

(defun ende-url--escape-component (value &optional query)
  "Percent-encode raw URI characters in VALUE for a path or QUERY component."
  (let ((index 0)
        (parts nil))
    (while (< index (length value))
      (let* ((character (aref value index))
             (literal (or (ende-url--path-character-p character)
                          (and query (= character ??)))))
        (if (or literal (= character ?%))
            (push (char-to-string character) parts)
          (let ((bytes (encode-coding-string
                        (char-to-string character) 'utf-8-unix t)))
            (dotimes (offset (length bytes))
              (push (format "%%%02X" (aref bytes offset)) parts))))
        (setq index (1+ index))))
    (apply #'concat (nreverse parts))))

(defun ende-url--canonicalize-escapes (value)
  (let ((position 0) parts)
    (while (string-match "%\\([[:xdigit:]][[:xdigit:]]\\)" value position)
      (let* ((start (match-beginning 0))
             (end (match-end 0))
             (byte (string-to-number (match-string 1 value) 16)))
        (push (substring value position start) parts)
        (push (if (ende-url--unreserved-p byte)
                  (char-to-string byte)
                (format "%%%02X" byte))
              parts)
        (setq position end)))
    (push (substring value position) parts)
    (apply #'concat (nreverse parts))))

(defun ende-url--normalize-percent-escapes (path)
  "Normalize unreserved escapes in PATH and reject ambiguous escaping."
  (setq path (ende-url--escape-component path))
  (ende-url--valid-escapes-p path)
  (when (or (string-match-p "%2[fF]" path)
            (string-match-p "%5[cC]" path))
    (error "URL path contains an encoded path separator"))
  (when (string-match-p "%25" path)
    (error "URL path contains nested percent escaping"))
  (ende-url--canonicalize-escapes path))

(defun ende-url--normalize-query-escapes (query)
  "Normalize percent escapes in QUERY without applying path restrictions."
  (setq query (ende-url--escape-component query t))
  (ende-url--valid-escapes-p query)
  (ende-url--canonicalize-escapes query))

(defun ende-url--path-without-dot-segments (path)
  "Return canonical PATH with literal dot segments removed."
  (let* ((segments (cdr (split-string path "/" nil)))
         (trailing (or (string-suffix-p "/" path)
                       (member (car (last segments)) '("." ".."))))
         (stack nil))
    (dolist (segment segments)
      (cond
       ((string= segment ".") nil)
       ((string= segment "..") (when stack (pop stack)))
       (t (push segment stack))))
    (setq stack (nreverse stack))
    (when (and trailing (not (equal (car (last stack)) "")))
      (setq stack (append stack '(""))))
    (concat "/" (mapconcat #'identity stack "/"))))

(defun ende-url--ipv4-number (host)
  "Parse strict dotted-decimal HOST and return its integer value, or nil."
  (when (string-match-p "\\`[0-9]+\\(?:\\.[0-9]+\\)\\{3\\}\\'" host)
    (let ((parts (split-string host "\\."))
          (value 0))
      (when (cl-every (lambda (part)
                        (and (<= (length part) 3)
                             (or (= (length part) 1)
                                 (not (= (aref part 0) ?0)))
                             (<= (string-to-number part) 255)))
                      parts)
        (dolist (part parts value)
          (setq value (+ (* value 256) (string-to-number part))))))))

(defun ende-url--numeric-host-p (host)
  "Return non-nil when every HOST label resembles a decimal or hex number."
  (and (not (string-empty-p host))
       (cl-every (lambda (label)
                   (or (string-match-p "\\`[0-9]+\\'" label)
                       (string-match-p "\\`0[xX][[:xdigit:]]+\\'" label)))
                 (split-string host "\\."))))

(defun ende-url--ipv6-groups (address)
  "Return eight 16-bit groups parsed from IPv6 ADDRESS, or signal."
  (when (string-match-p "%" address)
    (error "IPv6 zone identifiers are not supported"))
  (when (string-match-p "\\." address)
    (unless (string-match "\\`\\(.*:\\)\\([0-9]+\\(?:\\.[0-9]+\\)\\{3\\}\\)\\'"
                          address)
      (error "unsupported IPv6 transition or embedded-address form"))
    (let* ((prefix (match-string 1 address))
           (ipv4 (ende-url--ipv4-number (match-string 2 address)))
           (high (and ipv4 (logand (ash ipv4 -16) #xffff)))
           (low (and ipv4 (logand ipv4 #xffff))))
      (unless ipv4 (error "invalid embedded IPv4 address"))
      (setq address (concat prefix (format "%x:%x" high low)))))
  (let* ((double (string-match "::" address))
         (left (if double (substring address 0 double) address))
         (right (if double (substring address (+ double 2)) ""))
         (left-groups (if (string-empty-p left) nil (split-string left ":")))
         (right-groups (if (string-empty-p right) nil (split-string right ":")))
         (groups (append left-groups right-groups)))
    (when (and double (string-match "::" right))
      (error "IPv6 address contains more than one ::"))
    (when (or (string-match-p "::.*::" address)
              (cl-some (lambda (part)
                         (not (string-match-p "\\`[[:xdigit:]]\\{1,4\\}\\'"
                                              part)))
                       groups))
      (error "invalid IPv6 address"))
    (let ((count (length groups)))
      (if double
          (when (>= count 8) (error "invalid compressed IPv6 address"))
        (unless (= count 8) (error "IPv6 address must contain eight groups")))
      (let* ((zeros (if double (- 8 count) 0))
             (expanded (append (mapcar (lambda (part)
                                         (string-to-number part 16))
                                       left-groups)
                               (make-list zeros 0)
                               (mapcar (lambda (part)
                                         (string-to-number part 16))
                                       right-groups))))
        (unless (= (length expanded) 8)
          (error "invalid IPv6 address"))
        expanded))))

(defun ende-url--ipv6-number (groups)
  "Convert eight IPv6 GROUPS to an integer."
  (let ((value 0))
    (dolist (group groups value)
      (setq value (+ (ash value 16) group)))))

(defun ende-url--ipv6-string (groups)
  "Return canonical compressed text for IPv6 GROUPS."
  (let ((best-start nil)
        (best-length 1)
        (index 0))
    (while (< index 8)
      (if (/= (nth index groups) 0)
          (setq index (1+ index))
        (let ((start index))
          (while (and (< index 8) (= (nth index groups) 0))
            (setq index (1+ index)))
          (when (> (- index start) best-length)
            (setq best-start start best-length (- index start))))))
    (if (null best-start)
        (mapconcat (lambda (group) (format "%x" group)) groups ":")
      (let ((left (mapconcat (lambda (group) (format "%x" group))
                             (cl-subseq groups 0 best-start) ":"))
            (right (mapconcat (lambda (group) (format "%x" group))
                              (nthcdr (+ best-start best-length) groups) ":")))
        (concat left "::" right)))))

(defun ende-url--dns-host-p (host)
  "Return non-nil when HOST is a valid ASCII DNS name."
  (and (<= (length host) 253)
       (not (string-empty-p host))
       (cl-every
        (lambda (label)
          (and (<= 1 (length label) 63)
               (string-match-p
                "\\`[[:alnum:]]\\(?:[[:alnum:]-]*[[:alnum:]]\\)?\\'"
                label)))
        (split-string host "\\."))))

(defun ende-url--host (raw &optional pattern)
  "Normalize RAW URL host; return a plist describing it.
PATTERN permits only the specified DNS wildcard forms."
  (when (string-match-p "[^[:ascii:]]" raw)
    (error "Raw Unicode hostnames are unsupported; use an ASCII IDNA/punycode hostname"))
  (cond
   ((and pattern (string= raw "*"))
    (list :host "*" :host-kind 'any-dns))
   ((and pattern (string-prefix-p "*." raw))
    (let ((suffix (downcase (substring raw 2))))
      (when (or (string-match-p "[*:]" suffix)
                (ende-url--numeric-host-p suffix)
                (not (ende-url--dns-host-p suffix)))
        (error "invalid wildcard DNS host pattern: %s" raw))
      (list :host (concat "*." suffix) :host-kind 'subdomains)))
   ((string-prefix-p "[" raw)
    (unless (string-suffix-p "]" raw)
      (error "malformed bracketed IPv6 host"))
    (let* ((literal (substring raw 1 -1))
           (groups (ende-url--ipv6-groups literal))
           (number (ende-url--ipv6-number groups)))
      (when (and (string-match-p "\\." literal)
                 (/= (ash number -32) #xffff))
        (error "unsupported embedded IPv4 IPv6 form; use an IPv4-mapped literal"))
      (list :host (concat "[" (ende-url--ipv6-string groups) "]")
            :host-kind (if (= (ash number -32) #xffff)
                           'ipv4-mapped-ipv6
                         'ipv6)
            :address-number number :address-groups groups)))
   ((string-match-p "[\\[\\]]" raw)
    (error "malformed URL host"))
   (t
    (let* ((host (downcase (if (string-suffix-p "." raw)
                               (substring raw 0 -1) raw)))
           (ipv4 (ende-url--ipv4-number host)))
      (cond
       (ipv4 (list :host (format "%d.%d.%d.%d"
                                 (logand (ash ipv4 -24) 255)
                                 (logand (ash ipv4 -16) 255)
                                 (logand (ash ipv4 -8) 255)
                                 (logand ipv4 255))
                   :host-kind 'ipv4 :address-number ipv4))
       ((or (string-match-p "\\`[0-9.]+\\'" host)
            (ende-url--numeric-host-p host))
        (error "legacy or malformed numeric IPv4 host is not allowed: %s" raw))
       ((ende-url--dns-host-p host)
        (list :host host :host-kind 'dns))
       (t (error "invalid ASCII URL hostname: %s" raw)))))))

(defun ende-url--authority (authority &optional pattern)
  "Parse URL AUTHORITY and return normalized host and port fields."
  (when (or (string-empty-p authority)
            (string-match-p "@" authority)
            (string-match-p "[[:cntrl:]\\\\]" authority))
    (error "URL authority is empty or contains user information or invalid characters"))
  (let* ((bracketed (string-prefix-p "[" authority))
         (close (and bracketed (string-match "\\]" authority)))
         (colon (and (not bracketed) (string-match ":" authority)))
         (raw-host (if bracketed
                       (progn
                         (unless close (error "malformed bracketed URL host"))
                         (substring authority 0 (1+ close)))
                     (if colon (substring authority 0 colon) authority)))
         (suffix (if bracketed (substring authority (1+ close))
                   (if colon (substring authority colon) "")))
         (colon-count (cl-count ?: authority))
         (port-text nil)
         (host-info nil))
    (when (and (not bracketed) (> colon-count 1))
      (error "IPv6 URL hosts must be enclosed in brackets"))
    (when (and bracketed (not (or (string-empty-p suffix)
                                  (string-prefix-p ":" suffix))))
      (error "malformed URL port"))
    (when (and (not (string-empty-p suffix)) (string-prefix-p ":" suffix))
      (setq port-text (substring suffix 1))
      (unless (and (string-match-p "\\`[0-9]+\\'" port-text)
                   (<= (length port-text) 5))
        (error "URL port must be a decimal integer from 1 to 65535")))
    (setq host-info (ende-url--host raw-host pattern))
    (let ((port (and port-text (string-to-number port-text))))
      (when (and port (or (< port 1) (> port 65535)))
        (error "URL port must be from 1 to 65535"))
      (append host-info (list :explicit-port port)))))

(defun ende-url--split (value &optional allow-fragment pattern)
  "Parse VALUE into scheme, authority, path, query and fragment fields."
  (when (or (not (stringp value))
            (string-empty-p value)
            (string-match-p "[[:cntrl:]\\\\]" value))
    (error "URL must be a non-empty string without control characters or backslashes"))
  (ende-url--valid-escapes-p value)
  (when (and pattern (string-match-p "[?#]" value))
    (error "URL resource patterns must not contain a query or fragment"))
  (let ((case-fold-search t))
    (unless (string-match
             "\\`\\(https?\\)://\\([^/?#]+\\)\\([^?#]*\\)\\(?:?\\([^#]*\\)\\)?\\(?:#\\(.*\\)\\)?\\'"
             value)
      (error "URL must be an absolute HTTP(S) URL"))
    (let ((fragment (match-string 5 value)))
      (when (and fragment (not allow-fragment))
        (error "URL fragments are not allowed here"))
      (list :scheme (downcase (match-string 1 value))
            :authority (match-string 2 value)
            :path (match-string 3 value)
            :query (match-string 4 value)
            :fragment (match-string 5 value)))))

(defun ende-url-normalize (url)
  "Return normalized, fragment-free HTTP(S) URL URL as a plist.
The result includes `:url', `:scheme', `:host', `:host-kind', `:port',
`:path', and `:query'.  It is the URL to authorize and transmit."
  (let* ((parts (ende-url--split url t))
         (scheme (plist-get parts :scheme))
         (host-info (ende-url--authority (plist-get parts :authority)))
         (default-port (if (string= scheme "https") 443 80))
         (port (or (plist-get host-info :explicit-port) default-port))
         (path (or (plist-get parts :path) ""))
         (path (if (string-empty-p path) "/" path))
         (query (plist-get parts :query)))
    (unless (string-prefix-p "/" path)
      (error "HTTP URL path must be absolute"))
    (setq path (ende-url--path-without-dot-segments
                (ende-url--normalize-percent-escapes path)))
    (when query
      (setq query (ende-url--normalize-query-escapes query)))
    (let ((normalized
           (concat scheme "://" (plist-get host-info :host)
                   (if (= port default-port) "" (format ":%d" port))
                   path (if query (concat "?" query) ""))))
      (append (list :url normalized :scheme scheme :port port :path path
                    :query query :fragment (plist-get parts :fragment))
              host-info))))

(defun ende-url--pattern-glob-regexp (path)
  "Return anchored regexp for URL path glob PATH."
  (let ((index 0)
        (pieces nil))
    (while (< index (length path))
      (let ((character (aref path index)))
        (cond
         ((and (= character ?*) (< (1+ index) (length path))
               (= (aref path (1+ index)) ?*))
          (push ".*" pieces)
          (setq index (+ index 2)))
         ((= character ?*)
          (push "[^/]*" pieces)
          (setq index (1+ index)))
         (t
          (push (regexp-quote (char-to-string character)) pieces)
          (setq index (1+ index))))))
    (concat "\\`" (apply #'concat (nreverse pieces)) "\\'")))

(defun ende-url-pattern-compile (pattern)
  "Compile URL resource PATTERN to a normalized matching plist."
  (unless (and (stringp pattern) (not (string-empty-p pattern)))
    (error "URL resource pattern must be a non-empty string"))
  (let* ((case-fold-search t)
         (absolute (string-match-p "\\`https?://" pattern))
         (value (if absolute pattern
                  (if (string-match-p "[/?:#]" pattern)
                      (error "URL patterns with paths or ports must be absolute HTTP(S) URLs")
                    (concat "https://" pattern "/**"))))
         (parts (ende-url--split value nil t))
         (scheme (plist-get parts :scheme))
         (host-info (ende-url--authority (plist-get parts :authority) t))
         (default-port (if (string= scheme "https") 443 80))
         (port (or (plist-get host-info :explicit-port) default-port))
         (path (plist-get parts :path)))
    (when (and (not absolute)
               (not (memq (plist-get host-info :host-kind)
                          '(dns subdomains any-dns))))
      (error "IP literal resource patterns must use an absolute HTTP(S) URL"))
    (when (string-empty-p path)
      (setq path "/**"))
    (unless (string-prefix-p "/" path)
      (error "URL pattern path must be absolute"))
    ;; Escapes and dot segments have the same interpretation as in requests.
    (setq path (ende-url--path-without-dot-segments
                (ende-url--normalize-percent-escapes path)))
    (append (list :scheme scheme :port port :path path
                  :path-regexp (ende-url--pattern-glob-regexp path)
                  :original pattern)
            host-info)))

(defun ende-url-pattern-allows-p (pattern url)
  "Return non-nil if compiled PATTERN allows normalized request URL URL."
  (let* ((request (if (plist-get url :url) url (ende-url-normalize url)))
         (kind (plist-get pattern :host-kind))
         (host (plist-get request :host))
         (host-match
          (pcase kind
            ('any-dns (eq (plist-get request :host-kind) 'dns))
            ('subdomains
             (and (eq (plist-get request :host-kind) 'dns)
                  (let ((suffix (substring (plist-get pattern :host) 1)))
                    (and (> (length host) (length suffix))
                         (string-suffix-p suffix host)))))
            (_ (and (eq kind (plist-get request :host-kind))
                    (string= host (plist-get pattern :host)))))))
    (and (string= (plist-get pattern :scheme) (plist-get request :scheme))
         (= (plist-get pattern :port) (plist-get request :port))
         host-match
         (let ((case-fold-search nil))
           (string-match-p (plist-get pattern :path-regexp)
                           (plist-get request :path))))))

(provide 'ende-url)

;;; ende-url.el ends here
