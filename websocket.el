;;; websocket.el --- Bounded WebSocket transport -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'gnutls)
(require 'nsm)

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
      (unless (and (<= char #x10ffff) (not (<= #xd800 char #xdfff))
                   (or (multibyte-string-p text) (< char 128)))
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

(cl-defstruct (websocket--message (:constructor websocket--message-create))
  type utf8 chunks (bytes 0) (fragments 0))

(cl-defstruct (websocket--output (:constructor websocket--output-create))
  opcode bytes (offset 0) started)

(cl-defstruct (websocket--connection (:constructor websocket--connection-create))
  role process listener limits address headers protocols key protocol
  (state 'connecting) (generation 0) callbacks
  input (cursor 1) http frame message
  output output-tail (output-bytes 0) (output-items 0)
  controls (control-count 0) backpressured
  close-queued close-sent close-received peer-close failure eof
  timers work-timer busy work-pending events terminal)

(cl-defstruct (websocket--listener (:constructor websocket--listener-create))
  process limits admit callbacks connections timer (generation 0) stopped)

(defvar websocket--connections (make-hash-table :test #'eq))
(defvar websocket--listeners (make-hash-table :test #'eq))
(defvar websocket--opening-queue nil)
(defvar websocket--opening-active nil)
(defvar websocket--shutting-down nil)

(defun websocket-state (connection)
  "Return CONNECTION's state: connecting, open, closing, or closed."
  (websocket--connection-state connection))

(defun websocket-protocol (connection)
  "Return CONNECTION's selected subprotocol, or nil if none was selected."
  (websocket--connection-protocol connection))

(defun websocket--limit (connection key)
  (plist-get (websocket--connection-limits connection) key))

(defun websocket--cancel-deadline (connection kind)
  (when-let* ((entry (assq kind (websocket--connection-timers connection))))
    (cancel-timer (cdr entry))
    (setf (websocket--connection-timers connection)
          (delq entry (websocket--connection-timers connection)))))

(defun websocket--deadline (connection kind seconds function)
  (websocket--cancel-deadline connection kind)
  (let* ((generation (websocket--connection-generation connection))
         (timer
          (run-at-time seconds nil
                       (lambda ()
                         (when (and (= generation (websocket--connection-generation connection))
                                    (not (eq (websocket-state connection) 'closed)))
                           (websocket--cancel-deadline connection kind)
                           (funcall function connection))))))
    (push (cons kind timer) (websocket--connection-timers connection))))

(defun websocket--schedule (connection)
  (when (websocket--connection-busy connection)
    (setf (websocket--connection-work-pending connection) t))
  (unless (or (websocket--connection-work-timer connection)
              (websocket--connection-busy connection))
    (let ((generation (websocket--connection-generation connection)))
      (setf (websocket--connection-work-timer connection)
            (run-at-time 0 nil
                         (lambda ()
                           (when (= generation (websocket--connection-generation connection))
                             (setf (websocket--connection-work-timer connection) nil)
                             (websocket--run connection))))))))

(defun websocket--emit (connection event &rest arguments)
  (push (cons event arguments) (websocket--connection-events connection)))

(defun websocket--dispatch (connection)
  (let ((events (nreverse (websocket--connection-events connection))))
    (setf (websocket--connection-events connection) nil)
    (dolist (event events)
      (when (and (not (eq (websocket-state connection) 'closed))
                 (or (not (eq (car event) :on-message))
                     (and (eq (websocket-state connection) 'open)
                          (not (websocket--connection-close-received connection)))))
        (when-let* ((callback (plist-get (websocket--connection-callbacks connection) (car event))))
          (condition-case nil
              (apply callback connection (cdr event))
            ((error quit)
             (websocket--terminate connection
                                   '(:code 1011 :reason "" :clean nil :cause application))))))))
  (when-let* ((terminal (websocket--connection-terminal connection)))
    (setf (websocket--connection-terminal connection) nil)
    (remhash connection websocket--connections)
    (when (car terminal)
      (condition-case nil (funcall (car terminal) connection (cdr terminal))
        ((error quit) nil)))))

(defun websocket--terminate (connection info)
  (unless (eq (websocket-state connection) 'closed)
    (setf (websocket--connection-state connection) 'closed)
    (cl-incf (websocket--connection-generation connection))
    (let ((process (websocket--connection-process connection))
          (input (websocket--connection-input connection))
          (callback (plist-get (websocket--connection-callbacks connection) :on-close)))
      (dolist (entry (websocket--connection-timers connection)) (cancel-timer (cdr entry)))
      (when (websocket--connection-work-timer connection)
        (cancel-timer (websocket--connection-work-timer connection)))
      (setf (websocket--connection-process connection) nil
            (websocket--connection-input connection) nil
            (websocket--connection-timers connection) nil
            (websocket--connection-work-timer connection) nil
            (websocket--connection-callbacks connection) nil
            (websocket--connection-headers connection) nil
            (websocket--connection-address connection) nil
            (websocket--connection-key connection) nil
            (websocket--connection-http connection) nil
            (websocket--connection-frame connection) nil
            (websocket--connection-message connection) nil
            (websocket--connection-output connection) nil
            (websocket--connection-output-tail connection) nil
            (websocket--connection-output-bytes connection) 0
            (websocket--connection-output-items connection) 0
            (websocket--connection-controls connection) nil
            (websocket--connection-control-count connection) 0
            (websocket--connection-events connection) nil
            (websocket--connection-terminal connection) (cons callback info))
      (when process
        (set-process-filter process #'ignore)
        (set-process-sentinel process #'ignore)
        (set-process-plist process nil)
        (ignore-errors (delete-process process)))
      (when (buffer-live-p input) (kill-buffer input))
      (when-let* ((listener (websocket--connection-listener connection)))
        (remhash connection (websocket--listener-connections listener))
        (setf (websocket--connection-listener connection) nil))
      (setq websocket--opening-queue (delq connection websocket--opening-queue))
      (when (eq websocket--opening-active connection)
        (setq websocket--opening-active nil))
      (when (car websocket--opening-queue) (websocket--schedule (car websocket--opening-queue)))
      (websocket--schedule connection))))

(defun websocket-abort (connection)
  "Immediately dispose of CONNECTION, idempotently; notify close asynchronously."
  (websocket--terminate connection '(:code 1006 :reason "" :clean nil :cause abort))
  nil)

(defun websocket--close-code-p (code)
  (and (integerp code)
       (or (memq code '(1000 1001 1002 1003 1007 1008 1009 1010 1011 1012 1013 1014))
           (<= 3000 code 4999))))

(defun websocket--close-payload (code reason)
  (unless (websocket--close-code-p code) (error "Invalid WebSocket Close code"))
  (let ((bytes (websocket--text reason)))
    (when (> (length bytes) 123) (error "WebSocket Close reason exceeds 123 bytes"))
    (let ((payload (make-string 2 0)))
      (websocket--put-uint payload 0 2 code)
      (concat payload bytes))))

(defun websocket--read-close (payload)
  (when (= (length payload) 1) (signal 'websocket-protocol-error nil))
  (if (= (length payload) 0) '(:code 1005 :reason "")
    (let ((code (websocket--uint payload 0 2)) (reason (substring payload 2)))
      (unless (websocket--close-code-p code) (signal 'websocket-protocol-error nil))
      (websocket--utf8-feed (websocket--utf8-create) reason t)
      (list :code code :reason (decode-coding-string reason 'utf-8-unix)))))

(defun websocket--drop-output (connection)
  (setf (websocket--connection-output connection) nil
        (websocket--connection-output-tail connection) nil
        (websocket--connection-output-bytes connection) 0
        (websocket--connection-output-items connection) 0))

(defun websocket--queue-close (connection payload)
  (unless (websocket--connection-close-queued connection)
    (setf (websocket--connection-state connection) 'closing
          (websocket--connection-close-queued connection) t
          (websocket--connection-controls connection)
          (list (websocket--output-create :opcode 8 :bytes payload))
          (websocket--connection-control-count connection) 1)
    (websocket--drop-output connection)
    (websocket--deadline
     connection 'close (websocket--limit connection :closing-timeout)
     (lambda (c) (websocket--terminate c '(:code 1006 :reason "" :clean nil :cause timeout))))
    (websocket--schedule connection)))

(cl-defun websocket-close (connection &optional (code 1000) (reason ""))
  "Gracefully close CONNECTION with CODE and REASON, idempotently.
Pending application output is discarded.  Completion is asynchronous."
  (let ((payload (websocket--close-payload code reason)))
    (pcase (websocket-state connection)
      ('connecting (websocket-abort connection))
      ('open (websocket--queue-close connection payload))))
  nil)

(defun websocket--fail (connection cause)
  (unless (or (eq (websocket-state connection) 'closed)
              (websocket--connection-failure connection))
    (let* ((code (pcase cause ('utf8 1007) ('size 1009) ('policy 1008) (_ 1002)))
           (info (list :code code :reason "" :clean nil :cause cause)))
      (if (eq (websocket-state connection) 'connecting)
          (websocket--terminate connection info)
        (setf (websocket--connection-failure connection) info
              (websocket--connection-events connection) nil)
        (websocket--queue-close connection (websocket--close-payload code ""))
        (websocket--schedule connection)))))

(defun websocket--control (connection opcode payload)
  (pcase opcode
    (8
     (let ((info (websocket--read-close payload)))
       (setf (websocket--connection-peer-close connection) info
             (websocket--connection-close-received connection) t
             (websocket--connection-events connection) nil)
       (websocket--queue-close connection payload)))
    (9
     (unless (or (websocket--connection-close-received connection)
                 (websocket--connection-close-sent connection))
       (when (>= (websocket--connection-control-count connection)
                 (websocket--limit connection :output-items))
         (signal 'websocket-too-large nil))
       (setf (websocket--connection-controls connection)
             (nconc (websocket--connection-controls connection)
                    (list (websocket--output-create :opcode 10 :bytes payload))))
       (cl-incf (websocket--connection-control-count connection))))))

(defun websocket--begin-frame (connection frame)
  (let ((opcode (websocket--frame-opcode frame))
        (message (websocket--connection-message connection)))
    (when (< opcode 8)
      (unless (if (= opcode 0) message (not message))
        (signal 'websocket-protocol-error nil))
      (unless message
        (setq message (websocket--message-create
                       :type (if (= opcode 1) 'text 'binary)
                       :utf8 (and (= opcode 1) (websocket--utf8-create))))
        (setf (websocket--connection-message connection) message)
        (websocket--deadline connection 'message (websocket--limit connection :message-timeout)
                             (lambda (c) (websocket--fail c 'policy))))
      (cl-incf (websocket--message-fragments message))
      (cl-incf (websocket--message-bytes message) (websocket--frame-length frame))
      (when (or (> (websocket--message-fragments message) (websocket--limit connection :fragment-count))
                (> (websocket--message-bytes message) (websocket--limit connection :message-bytes)))
        (signal 'websocket-too-large nil)))))

(defun websocket--finish-frame (connection frame)
  (setf (websocket--connection-frame connection) nil)
  (if (>= (websocket--frame-opcode frame) 8)
      (websocket--control connection (websocket--frame-opcode frame)
                          (apply #'concat (nreverse (websocket--frame-chunks frame))))
    (when (websocket--frame-final frame)
      (let* ((message (websocket--connection-message connection))
             (utf8 (websocket--message-utf8 message)))
        (when utf8 (websocket--utf8-feed utf8 "" t))
        (setf (websocket--connection-message connection) nil)
        (websocket--cancel-deadline connection 'message)
        (when (eq (websocket-state connection) 'open)
          (let ((bytes (apply #'concat (nreverse (websocket--message-chunks message)))))
            (websocket--emit connection :on-message (websocket--message-type message)
                             (if utf8 (decode-coding-string bytes 'utf-8-unix) bytes))))))))

(defun websocket--parse-frames (connection)
  (let ((bytes 0) (frames 0) (progress t) deferred)
    (with-current-buffer (websocket--connection-input connection)
      (while (and progress
                  (< bytes (websocket--limit connection :drain-bytes))
                  (< frames (websocket--limit connection :drain-frames))
                  (not (websocket--connection-close-received connection)))
        (setq progress nil)
        (let ((cursor (websocket--connection-cursor connection)))
          (unless (websocket--connection-frame connection)
            (when-let* ((header (websocket--frame-header
                                (buffer-substring-no-properties cursor (min (point-max) (+ cursor 14)))
                                (websocket--connection-role connection)
                                (websocket--limit connection :frame-bytes))))
              (if (and (= (websocket--frame-opcode (cdr header)) 8)
                       (websocket--connection-events connection))
                  (setq deferred t)
                (websocket--begin-frame connection (cdr header))
                (cl-incf bytes (car header))
                (cl-incf cursor (car header))
                (setf (websocket--connection-cursor connection) cursor
                      (websocket--connection-frame connection) (cdr header)))))
          (when-let* ((frame (websocket--connection-frame connection)))
            (let ((count (min (websocket--frame-remaining frame) (- (point-max) cursor)
                              (max 0 (- (websocket--limit connection :drain-bytes) bytes)))))
              (when (> count 0)
                (let ((chunk (buffer-substring-no-properties cursor (+ cursor count))))
                  (when (websocket--frame-mask frame)
                    (websocket--mask! chunk (websocket--frame-mask frame) (websocket--frame-offset frame)))
                  (if (>= (websocket--frame-opcode frame) 8)
                      (push chunk (websocket--frame-chunks frame))
                    (let ((message (websocket--connection-message connection)))
                      (when (websocket--message-utf8 message)
                        (websocket--utf8-feed (websocket--message-utf8 message) chunk))
                      (push chunk (websocket--message-chunks message)))))
                (cl-incf (websocket--frame-offset frame) count)
                (cl-decf (websocket--frame-remaining frame) count)
                (cl-incf (websocket--connection-cursor connection) count)
                (cl-incf bytes count)
                (setq progress t))
              (when (= (websocket--frame-remaining frame) 0)
                (cl-incf frames)
                (websocket--finish-frame connection frame)
                (setq progress t))))))
      (delete-region (point-min) (websocket--connection-cursor connection))
      (setf (websocket--connection-cursor connection) 1)
      (and (not (websocket--connection-close-received connection))
           (> (buffer-size) 0)
           (or deferred (>= bytes (websocket--limit connection :drain-bytes))
               (>= frames (websocket--limit connection :drain-frames)))))))

(defun websocket--enqueue (connection opcode bytes)
  (unless (eq (websocket-state connection) 'open) (error "WebSocket is not open"))
  (when (> (length bytes) (websocket--limit connection :message-bytes))
    (signal 'websocket-too-large nil))
  (when (or (> (+ (websocket--connection-output-bytes connection) (length bytes))
               (websocket--limit connection :output-bytes))
            (>= (websocket--connection-output-items connection) (websocket--limit connection :output-items)))
    (setf (websocket--connection-backpressured connection) t)
    (signal 'websocket-backpressure nil))
  (let ((cell (list (websocket--output-create :opcode opcode :bytes (copy-sequence bytes)))))
    (if (websocket--connection-output-tail connection)
        (setcdr (websocket--connection-output-tail connection) cell)
      (setf (websocket--connection-output connection) cell))
    (setf (websocket--connection-output-tail connection) cell)
    (cl-incf (websocket--connection-output-bytes connection) (length bytes))
    (cl-incf (websocket--connection-output-items connection)))
  (websocket--schedule connection)
  t)

(defun websocket-send-text (connection text)
  "Queue a snapshot of Unicode scalar TEXT on CONNECTION.
Return t when accepted, not delivered.  Signal `websocket-backpressure'
without queue mutation if capacity is exhausted."
  (websocket--enqueue connection 1 (websocket--text text)))

(defun websocket-send-text-bytes (connection bytes)
  "Queue validated unibyte UTF-8 BYTES without decoding and re-encoding.
The acceptance and backpressure contract is that of `websocket-send-text'."
  (websocket--octets bytes)
  (websocket--utf8-feed (websocket--utf8-create) bytes t)
  (websocket--enqueue connection 1 bytes))

(defun websocket-send-binary (connection bytes)
  "Queue a snapshot of unibyte BYTES; acceptance does not imply delivery.
Signal `websocket-backpressure' without queue mutation when full."
  (websocket--enqueue connection 2 (websocket--octets bytes)))

(defun websocket--write (connection)
  (let ((bytes 0) (frames 0))
    (while (and (not (eq (websocket-state connection) 'closed))
                (< bytes (websocket--limit connection :drain-bytes))
                (< frames (websocket--limit connection :drain-frames))
                (or (websocket--connection-controls connection)
                    (and (not (websocket--connection-close-sent connection))
                         (websocket--connection-output connection))))
      (let* ((control (car (websocket--connection-controls connection)))
             (job (or control (car (websocket--connection-output connection))))
             (payload (websocket--output-bytes job))
             (offset (websocket--output-offset job))
             (end (if control (length payload)
                    (min (length payload) (+ offset (websocket--limit connection :write-frame-bytes)))))
             (final (= end (length payload)))
             (opcode (if (websocket--output-started job) 0 (websocket--output-opcode job)))
             (wire (websocket--wire opcode final (substring payload offset end)
                                    (websocket--connection-role connection))))
        (setf (websocket--output-started job) t
              (websocket--output-offset job) end)
        (if control
            (progn
              (pop (websocket--connection-controls connection))
              (cl-decf (websocket--connection-control-count connection))
              (when (= opcode 8) (setf (websocket--connection-close-sent connection) t)))
          (when final
            (pop (websocket--connection-output connection))
            (cl-decf (websocket--connection-output-items connection))
            (cl-decf (websocket--connection-output-bytes connection) (length payload))
            (unless (websocket--connection-output connection)
              (setf (websocket--connection-output-tail connection) nil))))
        (cl-incf bytes (length wire))
        (cl-incf frames)
        (process-send-string (websocket--connection-process connection) wire)))
    (when (and (websocket--connection-backpressured connection)
               (< (websocket--connection-output-bytes connection) (/ (websocket--limit connection :output-bytes) 2))
               (< (websocket--connection-output-items connection) (max 1 (/ (websocket--limit connection :output-items) 2))))
      (setf (websocket--connection-backpressured connection) nil)
      (websocket--emit connection :on-writable))
    (cond
     ((websocket--connection-failure connection)
      (websocket--terminate connection (websocket--connection-failure connection)))
     ((and (websocket--connection-close-sent connection)
           (websocket--connection-close-received connection)
           (eq (websocket--connection-role connection) 'server))
      (websocket--terminate connection
                            (append (websocket--connection-peer-close connection)
                                    '(:clean t :cause close)))))))

(defun websocket--nsm-verify (function process &rest arguments)
  (if (process-get process 'websocket--connection)
      (let ((nsm-noninteractive t) (inhibit-message t))
        (apply function process arguments))
    (apply function process arguments)))

(defun websocket--transport-ready (connection)
  (when (and (eq (websocket-state connection) 'connecting)
             (websocket--connection-address connection)
             (not (websocket--connection-key connection)))
    (let* ((key (base64-encode-string (websocket--os-random-bytes 16) t))
           (request (websocket--client-request
                     (websocket--connection-address connection) key
                     (websocket--connection-protocols connection)
                     (websocket--connection-headers connection))))
      (when (> (length request) (websocket--limit connection :header-bytes))
        (signal 'websocket-too-large nil))
      (setf (websocket--connection-key connection) key
            (websocket--connection-headers connection) nil)
      (process-send-string (websocket--connection-process connection) request))))

(defun websocket--sentinel (process _event)
  (when-let* ((connection (process-get process 'websocket--connection)))
    (when (= (or (process-get process 'websocket--generation) -1)
             (websocket--connection-generation connection))
      (unless (websocket--connection-process connection)
        (setf (websocket--connection-process connection) process))
      (condition-case nil
          (pcase (process-status process)
            ('open (websocket--transport-ready connection))
            ((or 'closed 'failed 'exit 'signal)
             (setf (websocket--connection-eof connection) t)
             (websocket--schedule connection)))
        ((error quit)
         (websocket--terminate connection '(:code 1006 :reason "" :clean nil :cause transport)))))))

(defun websocket--filter (process bytes)
  (when-let* ((connection (process-get process 'websocket--connection)))
    (when (and (= (or (process-get process 'websocket--generation) -1)
                  (websocket--connection-generation connection))
               (not (eq (websocket-state connection) 'closed))
               (not (websocket--connection-failure connection))
               (not (websocket--connection-close-received connection)))
      (unless (websocket--connection-process connection)
        (setf (websocket--connection-process connection) process))
      (with-current-buffer (websocket--connection-input connection)
        (if (> (+ (buffer-size) (length bytes)) (websocket--limit connection :input-bytes))
            (websocket--fail connection 'size)
          (goto-char (point-max))
          (insert bytes)))
      (websocket--schedule connection))))

(defun websocket--check-opening (connection)
  (when (eq (websocket-state connection) 'connecting)
    (if (memq (process-status (websocket--connection-process connection)) '(closed failed exit signal))
        (progn
          (setf (websocket--connection-eof connection) t)
          (websocket--schedule connection))
      (websocket--deadline connection 'transport-watch 0.1 #'websocket--check-opening))))

(defun websocket--start-client (connection)
  (setq websocket--opening-active connection
        websocket--opening-queue (delq connection websocket--opening-queue))
  (unless (advice-member-p #'websocket--nsm-verify 'nsm-verify-connection)
    (advice-add 'nsm-verify-connection :around #'websocket--nsm-verify))
  (let* ((address (websocket--connection-address connection))
         (tls (plist-get address :tls))
         (parameters
          (when tls
            (unless (gnutls-available-p) (error "GnuTLS is unavailable"))
            (cons 'gnutls-x509pki
                  (gnutls-boot-parameters :type 'gnutls-x509pki
                                          :hostname (plist-get address :host)
                                          :verify-error t :verify-flags 0
                                          :priority-string "NORMAL:-VERS-TLS1.0:-VERS-TLS1.1"))))
         (process
          (make-network-process
           :name "websocket" :host (plist-get address :host) :service (plist-get address :port)
           :buffer nil :coding 'binary :filter-multibyte nil :noquery t :nowait t
           :filter #'websocket--filter :sentinel #'websocket--sentinel
           :plist (list 'websocket--connection connection
                        'websocket--generation (websocket--connection-generation connection))
           :tls-parameters parameters)))
    (if (eq (websocket-state connection) 'closed)
        (ignore-errors (delete-process process))
      (setf (websocket--connection-process connection) process)
      (websocket--check-opening connection)
      (when (eq (process-status process) 'open) (websocket--transport-ready connection)))))

(defun websocket--opened (connection protocol)
  (setf (websocket--connection-state connection) 'open
        (websocket--connection-protocol connection) protocol
        (websocket--connection-http connection) nil
        (websocket--connection-key connection) nil
        (websocket--connection-address connection) nil)
  (websocket--cancel-deadline connection 'handshake)
  (websocket--cancel-deadline connection 'transport-watch)
  (when (eq websocket--opening-active connection)
    (setq websocket--opening-active nil)
    (when (car websocket--opening-queue) (websocket--schedule (car websocket--opening-queue))))
  (websocket--emit connection :on-open))

(defun websocket--read-http (connection)
  (let ((http (websocket--connection-http connection)) done)
    (with-current-buffer (websocket--connection-input connection)
      (goto-char (websocket--http-scan http))
      (while (and (not done) (search-forward "\r\n" nil t))
        (let ((end (point)))
          (setq done (websocket--http-line
                      http (buffer-substring-no-properties (websocket--connection-cursor connection) (- end 2))
                      (websocket--connection-limits connection)))
          (setf (websocket--connection-cursor connection) end)
          (when (and done (eq (websocket--connection-role connection) 'client)
                     (string-match-p "\\`HTTP/1\\.1 1[0-9][0-9] [ -~]*\\'" (websocket--http-start http))
                     (not (string-prefix-p "HTTP/1.1 101 " (websocket--http-start http))))
            (when (or (websocket--values http "content-length")
                      (websocket--values http "transfer-encoding"))
              (signal 'websocket-protocol-error nil))
            (setf (websocket--http-start http) nil (websocket--http-fields http) nil)
            (setq done nil))))
      (setf (websocket--http-scan http) (max (websocket--connection-cursor connection) (1- (point-max))))
      (when (or (> (+ (websocket--http-bytes http)
                       (- (point-max) (websocket--connection-cursor connection)))
                    (websocket--limit connection :header-bytes))
                (> (- (point-max) (websocket--connection-cursor connection)
                      (if (and (> (point-max) (websocket--connection-cursor connection))
                               (= (char-before (point-max)) 13)) 1 0))
                   (websocket--limit connection :line-bytes)))
        (unless done (signal 'websocket-too-large nil)))
      (when done
        (if (eq (websocket--connection-role connection) 'client)
            (websocket--opened connection
                               (websocket--client-response http (websocket--connection-key connection)
                                                           (websocket--connection-protocols connection)))
          (websocket--admit connection http))
        (when (buffer-live-p (websocket--connection-input connection))
          (delete-region (point-min) (websocket--connection-cursor connection))
          (setf (websocket--connection-cursor connection) 1))))
    done))

(defun websocket--run (connection)
  (unless (websocket--connection-busy connection)
    (let (more)
      (setf (websocket--connection-busy connection) t
            (websocket--connection-work-pending connection) nil)
      (unwind-protect
          (condition-case error
              (progn
                (when (and (eq (websocket-state connection) 'connecting)
                           (eq (websocket--connection-role connection) 'client)
                           (not (websocket--connection-process connection))
                           (not websocket--opening-active)
                           (eq connection (car websocket--opening-queue)))
                  (websocket--start-client connection))
                (when (and (eq (websocket-state connection) 'connecting)
                           (websocket--connection-process connection))
                  (websocket--read-http connection))
                (when (memq (websocket-state connection) '(open closing))
                  (unless (websocket--connection-failure connection)
                    (setq more (websocket--parse-frames connection)))
                  (websocket--write connection)))
            (websocket-invalid-utf8 (websocket--fail connection 'utf8))
            (websocket-too-large (websocket--fail connection 'size))
            (websocket-protocol-error (websocket--fail connection 'protocol))
            ((error quit)
             (ignore error)
             (websocket--terminate connection '(:code 1006 :reason "" :clean nil :cause transport))))
        (setf (websocket--connection-busy connection) nil))
      (websocket--dispatch connection)
      (when (and (websocket--connection-eof connection) (not more)
                 (not (eq (websocket-state connection) 'closed)))
        (websocket--terminate
         connection
         (if (and (websocket--connection-close-sent connection)
                  (websocket--connection-close-received connection))
             (append (websocket--connection-peer-close connection) '(:clean t :cause close))
           '(:code 1006 :reason "" :clean nil :cause transport))))
      (when (or more (websocket--connection-work-pending connection)
                (websocket--connection-failure connection)
                (websocket--connection-controls connection)
                (websocket--connection-output connection)
                (websocket--connection-terminal connection))
        (unless (and (eq (websocket-state connection) 'closed)
                     (not (websocket--connection-terminal connection)))
          (websocket--schedule connection))))))

(defun websocket--new (role limits callbacks)
  (let ((connection (websocket--connection-create
                     :role role :limits limits :callbacks callbacks
                     :http (websocket--http-create)
                     :input (generate-new-buffer " *websocket-input*"))))
    (with-current-buffer (websocket--connection-input connection) (set-buffer-multibyte nil))
    (puthash connection t websocket--connections)
    (websocket--deadline
     connection 'handshake (websocket--limit connection :handshake-timeout)
     (lambda (c) (websocket--terminate c '(:code 1006 :reason "" :clean nil :cause timeout))))
    connection))

(defun websocket--callbacks (on-open on-message on-close on-writable)
  (dolist (callback (list on-open on-message on-close on-writable))
    (unless (or (null callback) (functionp callback)) (error "Invalid WebSocket callback")))
  (list :on-open on-open :on-message on-message :on-close on-close :on-writable on-writable))

(cl-defun websocket-connect (url &key headers protocols limits on-open on-message on-close on-writable)
  "Start an asynchronous ws or certificate-verified wss connection to URL.
Return a handle immediately; callbacks never run before this call returns.
HEADERS is an ASCII name/value alist; handshake-owned fields are forbidden.
PROTOCOLS is an ordered list of unique, case-sensitive subprotocol tokens.
LIMITS overrides positive integer keys in `websocket--default-limits'.
ON-OPEN receives the connection.  ON-MESSAGE receives connection, type
(text or binary), and a complete validated message: a decoded string for
text, a unibyte string for binary.  ON-CLOSE receives connection and a plist
with :code, :reason, :clean, :cause, exactly once.  ON-WRITABLE receives
the connection once at low water after backpressure.  Callback errors
terminate with cause application; close callback errors are contained.
Synchronous argument errors signal; asynchronous failures notify ON-CLOSE.
Native writes and DNS/security operations can block despite bounded work."
  (when websocket--shutting-down (error "WebSocket shutdown in progress"))
  (let ((address (websocket--url url)) (headers (websocket--headers headers))
        (protocols (websocket--protocols protocols)) (limits (websocket--limits limits))
        (callbacks (websocket--callbacks on-open on-message on-close on-writable)))
    (when (>= (+ (length websocket--opening-queue) (if websocket--opening-active 1 0))
              (plist-get limits :pending-openings))
      (signal 'websocket-backpressure nil))
    (let ((connection (websocket--new 'client limits callbacks)))
      (setf (websocket--connection-address connection) address
            (websocket--connection-headers connection) headers
            (websocket--connection-protocols connection) protocols)
      (setq websocket--opening-queue (nconc websocket--opening-queue (list connection)))
      (websocket--schedule connection)
      connection)))

(defun websocket--admit (connection http)
  (let* ((request (websocket--server-request http))
         (key (copy-sequence (plist-get request :key)))
         (offered (mapcar #'copy-sequence (plist-get request :protocols)))
         (listener (websocket--connection-listener connection))
         (decision
          (condition-case nil
              (funcall (websocket--listener-admit listener) connection
                       (list :target (plist-get request :target)
                             :authority (plist-get request :authority)
                             :origin (plist-get request :origin)
                             :protocols (plist-get request :protocols)
                             :headers (plist-get request :headers)
                             :peer (process-contact (websocket--connection-process connection) :remote)))
            ((error quit)
             (websocket--terminate connection '(:code 1011 :reason "" :clean nil :cause application))
             nil))))
    (when (eq (websocket-state connection) 'connecting)
      (if (not (and (listp decision) (plist-get decision :accept)
                    (not (websocket--listener-stopped listener))))
          (websocket--terminate connection '(:code 1008 :reason "" :clean nil :cause policy))
        (let ((protocol (plist-get decision :protocol)))
          (if (and protocol (not (member protocol offered)))
              (websocket--terminate connection '(:code 1011 :reason "" :clean nil :cause application))
            (let ((response (concat "HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\n"
                                    "Connection: Upgrade\r\nSec-WebSocket-Accept: "
                                    (websocket--accept key) "\r\n"
                                    (when protocol (concat "Sec-WebSocket-Protocol: " protocol "\r\n"))
                                    "\r\n")))
              (process-send-string (websocket--connection-process connection) response)
              (when (eq (websocket-state connection) 'connecting)
                (websocket--opened connection (and protocol (copy-sequence protocol)))))))))))

(defun websocket--accepted (listener process)
  (or (process-get process 'websocket--connection)
      (if (or (websocket--listener-stopped listener)
              (>= (hash-table-count (websocket--listener-connections listener))
                  (plist-get (websocket--listener-limits listener) :connections)))
          (progn
            (set-process-filter process #'ignore)
            (set-process-sentinel process #'ignore)
            (set-process-plist process nil)
            (ignore-errors (delete-process process))
            nil)
        (let ((connection (websocket--new 'server (websocket--listener-limits listener)
                                          (websocket--listener-callbacks listener))))
          (setf (websocket--connection-process connection) process
                (websocket--connection-listener connection) listener)
          (set-process-plist process (list 'websocket--connection connection 'websocket--generation 0))
          (set-process-coding-system process 'binary 'binary)
          (set-process-query-on-exit-flag process nil)
          (puthash connection t (websocket--listener-connections listener))
          connection))))

(defun websocket--accept-log (server process _message)
  (when-let* ((listener (process-get server 'websocket--listener)))
    (websocket--accepted listener process)))

(cl-defun websocket-listen (&key (host "127.0.0.1") service admit limits
                                on-open on-message on-close on-writable)
  "Listen for ws connections on HOST and SERVICE; return a listener handle.
SERVICE is a TCP port or t for an OS-assigned port.  HOST defaults to
loopback.  TLS listeners are not supported; use an external TLS terminator.
ADMIT is required and receives connection and a request plist containing
:target, :authority, :origin, :protocols, :headers, and :peer.  It must
validate the target, authority, Origin policy, and authentication BEFORE
returning (:accept t), optionally with :protocol selecting one offered
token.  Every other result rejects admission.  Missing Origin is nil;
literal null Origin is the string \"null\".  Origin is not authentication.
Callbacks and LIMITS have the contracts of `websocket-connect'.
No callbacks run before this constructor returns.  Stopping the listener
disposes of all its accepted connections."
  (when websocket--shutting-down (error "WebSocket shutdown in progress"))
  (unless (and (stringp host) (not (string-empty-p host))
               (or (eq service t) (and (integerp service) (<= 1 service 65535)))
               (functionp admit))
    (error "Invalid WebSocket listener arguments"))
  (let* ((limits (websocket--limits limits))
         (callbacks (websocket--callbacks on-open on-message on-close on-writable))
         (listener (websocket--listener-create :limits limits :admit admit :callbacks callbacks
                                               :connections (make-hash-table :test #'eq))))
    (condition-case error
        (let ((process
               (make-network-process
                :name "websocket-listener" :host host :service service :server t :stop t
                :buffer nil :coding 'binary :filter-multibyte nil :noquery t
                :filter #'websocket--filter :sentinel #'websocket--sentinel
                :log #'websocket--accept-log :plist (list 'websocket--listener listener))))
          (setf (websocket--listener-process listener) process)
          (puthash listener t websocket--listeners)
          (let ((generation (websocket--listener-generation listener)))
            (setf (websocket--listener-timer listener)
                  (run-at-time 0 nil
                               (lambda ()
                                 (when (and (= generation (websocket--listener-generation listener))
                                            (not (websocket--listener-stopped listener)))
                                   (setf (websocket--listener-timer listener) nil)
                                   (continue-process process))))))
          listener)
      ((error quit)
       (websocket-stop-listener listener)
       (signal (car error) (cdr error))))))

(defun websocket-listener-port (listener)
  "Return LISTENER's bound TCP port, or nil after it has stopped."
  (when-let* ((process (websocket--listener-process listener)))
    (process-contact process :service)))

(defun websocket-stop-listener (listener)
  "Stop LISTENER and abort all its accepted connections, idempotently."
  (unless (websocket--listener-stopped listener)
    (setf (websocket--listener-stopped listener) t)
    (cl-incf (websocket--listener-generation listener))
    (when-let* ((timer (websocket--listener-timer listener))) (cancel-timer timer))
    (let ((process (websocket--listener-process listener)) connections)
      (setf (websocket--listener-process listener) nil
            (websocket--listener-timer listener) nil
            (websocket--listener-admit listener) nil
            (websocket--listener-callbacks listener) nil)
      (remhash listener websocket--listeners)
      (when process
        (set-process-plist process nil)
        (ignore-errors (delete-process process)))
      (maphash (lambda (connection _) (push connection connections))
               (websocket--listener-connections listener))
      (dolist (connection connections)
        (websocket--terminate connection '(:code 1001 :reason "" :clean nil :cause listener))))))

(defun websocket-shutdown ()
  "Stop all listeners and dispose of all connections, work, and callbacks.
Close callbacks run after resources are detached.  Safe to call repeatedly;
new connections cannot be constructed during shutdown callbacks."
  (unless websocket--shutting-down
    (let ((websocket--shutting-down t) listeners connections)
      (maphash (lambda (listener _) (push listener listeners)) websocket--listeners)
      (dolist (listener listeners) (websocket-stop-listener listener))
      (maphash (lambda (connection _) (push connection connections)) websocket--connections)
      (dolist (connection connections)
        (websocket--terminate connection '(:code 1001 :reason "" :clean nil :cause shutdown)))
      (dolist (connection connections)
        (when-let* ((timer (websocket--connection-work-timer connection))) (cancel-timer timer))
        (setf (websocket--connection-work-timer connection) nil)
        (websocket--dispatch connection))
      (setq websocket--opening-queue nil websocket--opening-active nil)
      (advice-remove 'nsm-verify-connection #'websocket--nsm-verify))))

(defun websocket-unload-function ()
  (websocket-shutdown)
  nil)


(provide 'websocket)
;;; websocket.el ends here
