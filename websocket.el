;;; websocket.el --- Bounded WebSocket transport -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'gnutls)

(define-error 'websocket-error "WebSocket failure")
(define-error 'websocket-protocol-error "Invalid WebSocket protocol" 'websocket-error)
(define-error 'websocket-invalid-utf8 "Invalid WebSocket UTF-8" 'websocket-error)
(define-error 'websocket-too-large "WebSocket limit exceeded" 'websocket-error)
(define-error 'websocket-backpressure "WebSocket output queue full" 'websocket-error)

(cl-defstruct (websocket--utf8 (:constructor websocket--utf8-create))
  (remaining 0) (minimum #x80) (maximum #xbf))

(defun websocket--utf8-feed (state bytes &optional final)
  (dotimes (i (length bytes))
    (let ((byte (aref bytes i)))
      (if (> (websocket--utf8-remaining state) 0)
          (progn
            (unless (<= (websocket--utf8-minimum state) byte
                        (websocket--utf8-maximum state))
              (signal 'websocket-invalid-utf8 nil))
            (cl-decf (websocket--utf8-remaining state))
            (setf (websocket--utf8-minimum state) #x80
                  (websocket--utf8-maximum state) #xbf))
        (let ((rule (cond ((<= byte #x7f) '(0 #x80 #xbf))
                          ((<= #xc2 byte #xdf) '(1 #x80 #xbf))
                          ((= byte #xe0) '(2 #xa0 #xbf))
                          ((<= #xe1 byte #xec) '(2 #x80 #xbf))
                          ((= byte #xed) '(2 #x80 #x9f))
                          ((<= #xee byte #xef) '(2 #x80 #xbf))
                          ((= byte #xf0) '(3 #x90 #xbf))
                          ((<= #xf1 byte #xf3) '(3 #x80 #xbf))
                          ((= byte #xf4) '(3 #x80 #x8f)))))
          (unless rule (signal 'websocket-invalid-utf8 nil))
          (setf (websocket--utf8-remaining state) (nth 0 rule)
                (websocket--utf8-minimum state) (nth 1 rule)
                (websocket--utf8-maximum state) (nth 2 rule))))))
  (when (and final (/= 0 (websocket--utf8-remaining state)))
    (signal 'websocket-invalid-utf8 nil))
  bytes)

(defun websocket--octets (value)
  (unless (and (stringp value) (not (multibyte-string-p value)))
    (signal 'wrong-type-argument (list 'unibyte-string value)))
  value)

(defun websocket--text (text)
  (unless (stringp text) (signal 'wrong-type-argument (list 'stringp text)))
  (dotimes (i (length text))
    (let ((char (aref text i)))
      (unless (and (<= char #x10ffff) (not (<= #xd800 char #xdfff)))
        (signal 'websocket-invalid-utf8 nil))))
  (encode-coding-string text 'utf-8-unix t))

(defun websocket--os-random-bytes (count)
  (unless (memq system-type '(gnu gnu/linux gnu/kfreebsd darwin berkeley-unix))
    (error "No supported secure random source on this platform"))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((file-name-handler-alist nil))
      (insert-file-contents-literally "/dev/urandom" nil nil count))
    (unless (= (buffer-size) count)
      (error "Secure random source returned insufficient data"))
    (buffer-string)))

(defun websocket--mask! (bytes key offset)
  (dotimes (i (length bytes))
    (aset bytes i (logxor (aref bytes i) (aref key (logand (+ offset i) 3)))))
  bytes)

(defun websocket--uint (bytes start count)
  (let ((value 0))
    (dotimes (i count)
      (setq value (+ (ash value 8) (aref bytes (+ start i)))))
    value))

(defun websocket--put-uint (bytes start count value)
  (dotimes (i count)
    (aset bytes (+ start (- count i 1)) (logand value 255))
    (setq value (ash value -8))))

(cl-defstruct (websocket--frame (:constructor websocket--frame-create))
  opcode final length remaining mask (offset 0) chunks)

(defun websocket--frame-header (bytes role maximum)
  (when (>= (length bytes) 2)
    (let* ((a (aref bytes 0)) (b (aref bytes 1))
           (opcode (logand a 15)) (final (/= 0 (logand a 128)))
           (masked (/= 0 (logand b 128))) (short (logand b 127))
           (extended (pcase short (126 2) (127 8) (_ 0)))
           (size (+ 2 extended (if masked 4 0))))
      (unless (and (= 0 (logand a 112)) (memq opcode '(0 1 2 8 9 10))
                   (eq masked (eq role 'server))
                   (or (< opcode 8) (and final (<= short 125))))
        (signal 'websocket-protocol-error nil))
      (when (>= (length bytes) size)
        (let ((length (if (= extended 0) short
                        (websocket--uint bytes 2 extended))))
          (unless (and (or (/= short 126) (>= length 126))
                       (or (/= short 127) (<= 65536 length #x7fffffffffffffff)))
            (signal 'websocket-protocol-error nil))
          (when (> length maximum) (signal 'websocket-too-large nil))
          (cons size (websocket--frame-create
                      :opcode opcode :final final :length length :remaining length
                      :mask (and masked (substring bytes (+ 2 extended) size)))))))))

(defun websocket--wire (opcode final payload role)
  (let* ((length (length payload))
         (extended (cond ((< length 126) 0) ((< length 65536) 2) (t 8)))
         (masked (eq role 'client))
         (header (make-string (+ 2 extended (if masked 4 0)) 0))
         (key (and masked (websocket--os-random-bytes 4))))
    (aset header 0 (logior opcode (if final 128 0)))
    (aset header 1 (logior (if masked 128 0)
                          (pcase extended (0 length) (2 126) (_ 127))))
    (when (> extended 0) (websocket--put-uint header 2 extended length))
    (when masked
      (store-substring header (+ 2 extended) key)
      (setq payload (websocket--mask! (copy-sequence payload) key 0)))
    (concat header payload)))

(defconst websocket--guid "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")

(defun websocket--accept (key)
  (base64-encode-string
   (secure-hash 'sha1 (concat key websocket--guid) nil nil t) t))

(defconst websocket--default-limits
  '(:header-bytes 16384 :line-bytes 4096 :header-fields 100
    :handshake-timeout 10 :frame-bytes 8388608 :message-bytes 8388608
    :input-bytes 16777216 :output-bytes 16777216 :output-items 256
    :fragment-count 4096 :message-timeout 30 :drain-bytes 65536
    :drain-frames 64 :write-frame-bytes 16384 :pending-openings 64
    :connections 128 :closing-timeout 3))

(defun websocket--limits (overrides)
  (unless (and (proper-list-p overrides) (cl-evenp (length overrides)))
    (error "Limits must be a property list"))
  (let ((limits (copy-sequence websocket--default-limits)) seen)
    (while overrides
      (let ((key (pop overrides)) (value (pop overrides)))
        (unless (and (plist-member limits key) (not (memq key seen))
                     (integerp value) (> value 0))
          (error "Invalid WebSocket limit"))
        (push key seen)
        (setq limits (plist-put limits key value))))
    (unless (and (<= (plist-get limits :line-bytes) (plist-get limits :header-bytes))
                 (<= (plist-get limits :write-frame-bytes) (plist-get limits :frame-bytes))
                 (<= (plist-get limits :frame-bytes) (plist-get limits :message-bytes))
                 (>= (plist-get limits :input-bytes) 14))
      (error "Inconsistent WebSocket limits"))
    limits))

(defconst websocket--token "[!#$%&'*+.^_`|~0-9A-Za-z-]+")

(defun websocket--token-p (value)
  (and (stringp value)
       (string-match-p (concat "\\`" websocket--token "\\'") value)))

(defun websocket--target-p (target)
  (and (stringp target) (string-prefix-p "/" target)
       (string-match-p "\\`[!$&'()*+,;=:@/?A-Za-z0-9._~%+-]*\\'" target)
       (not (string-match-p "%\\(?:[^0-9A-Fa-f]\\|[0-9A-Fa-f]\\(?:[^0-9A-Fa-f]\\|\\'\\)\\|\\'\\)" target))))

(defun websocket--authority (authority)
  (let ((case-fold-search nil))
    (unless (and (stringp authority)
                 (string-match
                  "\\`\\(\\[[0-9A-Fa-f:.]+\\]\\|[A-Za-z0-9][A-Za-z0-9.-]*\\)\\(?::\\([0-9]+\\)\\)?\\'"
                  authority))
      (error "Invalid WebSocket authority"))
    (let ((host (match-string 1 authority))
          (port (and (match-string 2 authority)
                     (string-to-number (match-string 2 authority)))))
      (when (and port (not (<= 1 port 65535))) (error "Invalid WebSocket port"))
      (list (if (string-prefix-p "[" host) (substring host 1 -1) host) port))))

(defun websocket--url (url)
  (let ((case-fold-search nil))
    (unless (and (stringp url)
                 (string-match "\\`\\(wss?\\)://\\([^/?#]+\\)\\([^#]*\\)\\'" url))
      (error "Expected a ws or wss URL without a fragment"))
    (let* ((tls (equal (match-string 1 url) "wss"))
           (authority (match-string 2 url)) (target (match-string 3 url))
           (address (websocket--authority authority)))
      (when (or (equal target "") (string-prefix-p "?" target))
        (setq target (concat "/" target)))
      (unless (websocket--target-p target) (error "Invalid escaped WebSocket target"))
      (list :host (car address) :port (or (cadr address) (if tls 443 80))
            :authority authority :target target :tls tls))))

(defun websocket--protocols (protocols)
  (unless (and (proper-list-p protocols)
               (cl-every #'websocket--token-p protocols)
               (= (length protocols) (length (delete-dups (copy-sequence protocols)))))
    (error "Invalid WebSocket subprotocols"))
  (mapcar #'copy-sequence protocols))

(defun websocket--headers (headers)
  (unless (proper-list-p headers) (error "Headers must be an alist"))
  (mapcar
   (lambda (header)
     (unless (and (consp header) (websocket--token-p (car header))
                  (stringp (cdr header))
                  (string-match-p "\\`[\t -~]*\\'" (cdr header))
                  (not (member (downcase (car header))
                               '("host" "upgrade" "connection" "content-length"
                                 "transfer-encoding" "expect" "trailer")))
                  (not (string-prefix-p "sec-websocket-" (downcase (car header)))))
       (error "Invalid or handshake-owned WebSocket header"))
     (cons (copy-sequence (car header)) (copy-sequence (cdr header))))
   headers))

(cl-defstruct (websocket--http (:constructor websocket--http-create))
  start fields (bytes 0) (count 0) (scan 1))

(defun websocket--http-line (http line limits)
  (cl-incf (websocket--http-bytes http) (+ 2 (length line)))
  (when (or (> (websocket--http-bytes http) (plist-get limits :header-bytes))
            (> (length line) (plist-get limits :line-bytes)))
    (signal 'websocket-too-large nil))
  (unless (string-match-p "\\`[\t -~\x80-\xff]*\\'" line)
    (signal 'websocket-protocol-error nil))
  (cond
   ((null (websocket--http-start http)) (setf (websocket--http-start http) line) nil)
   ((equal line "") t)
   (t
    (cl-incf (websocket--http-count http))
    (when (> (websocket--http-count http) (plist-get limits :header-fields))
      (signal 'websocket-too-large nil))
    (unless (string-match (concat "\\`\\(" websocket--token "\\):[\t ]*\\(.*\\)\\'") line)
      (signal 'websocket-protocol-error nil))
    (push (cons (downcase (match-string 1 line)) (string-trim (match-string 2 line) "[\t ]+" "[\t ]+"))
          (websocket--http-fields http))
    nil)))

(defun websocket--values (http name)
  (mapcar #'cdr (cl-remove-if-not (lambda (field) (equal (car field) name))
                                 (reverse (websocket--http-fields http)))))

(defun websocket--single (http name &optional optional)
  (let ((values (websocket--values http name)))
    (unless (or (and optional (null values)) (= (length values) 1))
      (signal 'websocket-protocol-error nil))
    (car values)))

(defun websocket--tokens (http name &optional upgrade)
  (let (tokens)
    (dolist (value (websocket--values http name))
      (dolist (token (split-string value ","))
        (setq token (string-trim token))
        (unless (if upgrade
                    (string-match-p (concat "\\`" websocket--token
                                            "\\(?:/" websocket--token "\\)?\\'") token)
                  (websocket--token-p token))
          (signal 'websocket-protocol-error nil))
        (push token tokens)))
    (nreverse tokens)))

(defun websocket--upgrade (http)
  (unless (and (member "upgrade" (mapcar #'downcase (websocket--tokens http "connection")))
               (member "websocket" (mapcar #'downcase (websocket--tokens http "upgrade" t)))
               (not (websocket--values http "transfer-encoding"))
               (not (websocket--values http "content-length")))
    (signal 'websocket-protocol-error nil)))

(defun websocket--client-response (http key protocols)
  (unless (string-match-p "\\`HTTP/1\\.1 101 [ -~]*\\'" (websocket--http-start http))
    (signal 'websocket-protocol-error nil))
  (websocket--upgrade http)
  (unless (and (equal (websocket--single http "sec-websocket-accept") (websocket--accept key))
               (not (websocket--values http "sec-websocket-extensions")))
    (signal 'websocket-protocol-error nil))
  (let ((protocol (websocket--single http "sec-websocket-protocol" t)))
    (when (and protocol (not (member protocol protocols)))
      (signal 'websocket-protocol-error nil))
    protocol))

(defun websocket--server-request (http)
  (unless (string-match "\\`GET \\([^ ]+\\) HTTP/1\\.1\\'" (websocket--http-start http))
    (signal 'websocket-protocol-error nil))
  (let ((target (match-string 1 (websocket--http-start http))))
    (unless (websocket--target-p target) (signal 'websocket-protocol-error nil))
    (websocket--upgrade http)
    (let ((authority (websocket--single http "host"))
          (key (websocket--single http "sec-websocket-key"))
          (origin (websocket--single http "origin" t))
          (protocols (websocket--tokens http "sec-websocket-protocol")))
      (condition-case nil (websocket--authority authority)
        (error (signal 'websocket-protocol-error nil)))
      (unless (and (equal (websocket--single http "sec-websocket-version") "13")
                   (string-match-p "\\`[A-Za-z0-9+/]\\{22\\}==\\'" key)
                   (condition-case nil
                       (let ((nonce (base64-decode-string key)))
                         (and (= (length nonce) 16)
                              (equal key (base64-encode-string nonce t))))
                     (error nil))
                   (= (length protocols) (length (delete-dups (copy-sequence protocols)))))
        (signal 'websocket-protocol-error nil))
      (list :target target :authority authority :origin origin :protocols protocols
            :headers (reverse (websocket--http-fields http)) :key key))))

(defun websocket--client-request (address key protocols headers)
  (concat "GET " (plist-get address :target) " HTTP/1.1\r\nHost: "
          (plist-get address :authority)
          "\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Version: 13\r\nSec-WebSocket-Key: " key "\r\n"
          (when protocols (concat "Sec-WebSocket-Protocol: " (string-join protocols ", ") "\r\n"))
          (mapconcat (lambda (field) (concat (car field) ": " (cdr field) "\r\n")) headers "")
          "\r\n"))

(provide 'websocket)
;;; websocket.el ends here
