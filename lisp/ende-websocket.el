;;; ende-websocket.el --- Bounded WebSocket transport -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'gnutls)
(require 'nsm)

(define-error 'ende-websocket-error "WebSocket failure")
(define-error 'ende-websocket-protocol-error "Invalid WebSocket protocol" 'ende-websocket-error)
(define-error 'ende-websocket-version-error "Unsupported WebSocket version" 'ende-websocket-protocol-error)
(define-error 'ende-websocket-invalid-utf8 "Invalid WebSocket UTF-8" 'ende-websocket-error)
(define-error 'ende-websocket-too-large "WebSocket limit exceeded" 'ende-websocket-error)
(define-error 'ende-websocket-backpressure "WebSocket output queue full" 'ende-websocket-error)

(cl-defstruct (ende-websocket--utf8 (:constructor ende-websocket--utf8-create))
  (remaining 0) (minimum #x80) (maximum #xbf))

(defun ende-websocket--utf8-feed (state bytes &optional final)
  (dotimes (i (length bytes))
    (let ((byte (aref bytes i)))
      (if (> (ende-websocket--utf8-remaining state) 0)
          (progn
            (unless (<= (ende-websocket--utf8-minimum state) byte
                        (ende-websocket--utf8-maximum state))
              (signal 'ende-websocket-invalid-utf8 nil))
            (cl-decf (ende-websocket--utf8-remaining state))
            (setf (ende-websocket--utf8-minimum state) #x80
                  (ende-websocket--utf8-maximum state) #xbf))
        (let ((rule (cond ((<= byte #x7f) '(0 #x80 #xbf))
                          ((<= #xc2 byte #xdf) '(1 #x80 #xbf))
                          ((= byte #xe0) '(2 #xa0 #xbf))
                          ((<= #xe1 byte #xec) '(2 #x80 #xbf))
                          ((= byte #xed) '(2 #x80 #x9f))
                          ((<= #xee byte #xef) '(2 #x80 #xbf))
                          ((= byte #xf0) '(3 #x90 #xbf))
                          ((<= #xf1 byte #xf3) '(3 #x80 #xbf))
                          ((= byte #xf4) '(3 #x80 #x8f)))))
          (unless rule (signal 'ende-websocket-invalid-utf8 nil))
          (setf (ende-websocket--utf8-remaining state) (nth 0 rule)
                (ende-websocket--utf8-minimum state) (nth 1 rule)
                (ende-websocket--utf8-maximum state) (nth 2 rule))))))
  (when (and final (/= 0 (ende-websocket--utf8-remaining state)))
    (signal 'ende-websocket-invalid-utf8 nil))
  bytes)

(defun ende-websocket--octets (value)
  (unless (and (stringp value) (not (multibyte-string-p value)))
    (signal 'wrong-type-argument (list 'unibyte-string value)))
  value)

(defun ende-websocket--text-size (text)
  (unless (stringp text) (signal 'wrong-type-argument (list 'stringp text)))
  (let ((size 0))
    (dotimes (i (length text))
      (let ((char (aref text i)))
        (unless (and (<= char #x10ffff) (not (<= #xd800 char #xdfff))
                     (or (multibyte-string-p text) (< char 128)))
          (signal 'ende-websocket-invalid-utf8 nil))
        (cl-incf size (cond ((< char #x80) 1) ((< char #x800) 2)
                           ((< char #x10000) 3) (t 4)))))
    size))

(defun ende-websocket--text (text)
  (ende-websocket--text-size text)
  (encode-coding-string text 'utf-8-unix t))

(defun ende-websocket--os-random-bytes (count)
  (unless (memq system-type '(gnu gnu/linux gnu/kfreebsd darwin berkeley-unix))
    (error "No supported secure random source on this platform"))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((file-name-handler-alist nil))
      (insert-file-contents-literally "/dev/urandom" nil nil count))
    (unless (= (buffer-size) count)
      (error "Secure random source returned insufficient data"))
    (buffer-string)))

(defun ende-websocket--mask! (bytes key offset)
  (dotimes (i (length bytes))
    (aset bytes i (logxor (aref bytes i) (aref key (logand (+ offset i) 3)))))
  bytes)

(defun ende-websocket--uint (bytes start count)
  (let ((value 0))
    (dotimes (i count)
      (setq value (+ (ash value 8) (aref bytes (+ start i)))))
    value))

(defun ende-websocket--put-uint (bytes start count value)
  (dotimes (i count)
    (aset bytes (+ start (- count i 1)) (logand value 255))
    (setq value (ash value -8))))

(cl-defstruct (ende-websocket--frame (:constructor ende-websocket--frame-create))
  opcode final length remaining mask (offset 0) chunks)

(defun ende-websocket--frame-header (bytes role maximum)
  (when (>= (length bytes) 2)
    (let* ((a (aref bytes 0)) (b (aref bytes 1))
           (opcode (logand a 15)) (final (/= 0 (logand a 128)))
           (masked (/= 0 (logand b 128))) (short (logand b 127))
           (extended (pcase short (126 2) (127 8) (_ 0)))
           (size (+ 2 extended (if masked 4 0))))
      (unless (and (= 0 (logand a 112)) (memq opcode '(0 1 2 8 9 10))
                   (eq masked (eq role 'server))
                   (or (< opcode 8) (and final (<= short 125))))
        (signal 'ende-websocket-protocol-error nil))
      (when (>= (length bytes) size)
        (let ((length (if (= extended 0) short
                        (ende-websocket--uint bytes 2 extended))))
          (unless (and (or (/= short 126) (>= length 126))
                       (or (/= short 127) (<= 65536 length #x7fffffffffffffff)))
            (signal 'ende-websocket-protocol-error nil))
          (when (and (< opcode 8) (> length maximum))
            (signal 'ende-websocket-too-large nil))
          (cons size (ende-websocket--frame-create
                      :opcode opcode :final final :length length :remaining length
                      :mask (and masked (substring bytes (+ 2 extended) size)))))))))

(defun ende-websocket--wire (opcode final payload role)
  (let* ((length (length payload))
         (extended (cond ((< length 126) 0) ((< length 65536) 2) (t 8)))
         (masked (eq role 'client))
         (header (make-string (+ 2 extended (if masked 4 0)) 0))
         (key (and masked (ende-websocket--os-random-bytes 4))))
    (aset header 0 (logior opcode (if final 128 0)))
    (aset header 1 (logior (if masked 128 0)
                          (pcase extended (0 length) (2 126) (_ 127))))
    (when (> extended 0) (ende-websocket--put-uint header 2 extended length))
    (when masked
      (store-substring header (+ 2 extended) key)
      (setq payload (ende-websocket--mask! (copy-sequence payload) key 0)))
    (concat header payload)))

(defconst ende-websocket--guid "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")

(defun ende-websocket--accept (key)
  (base64-encode-string
   (secure-hash 'sha1 (concat key ende-websocket--guid) nil nil t) t))

(defconst ende-websocket--default-limits
  '(:header-bytes 16384 :line-bytes 4096 :header-fields 100
    :handshake-timeout 10 :frame-bytes 8388608 :message-bytes 8388608
    :input-bytes 16777216 :output-bytes 16777216 :output-items 256
    :fragment-count 4096 :message-timeout 30 :drain-bytes 65536
    :drain-frames 64 :write-frame-bytes 16384 :write-message-bytes 8388608 :pending-openings 64
    :connections 128 :closing-timeout 3))

(defun ende-websocket--limits (overrides)
  (unless (and (proper-list-p overrides) (cl-evenp (length overrides)))
    (error "Limits must be a property list"))
  (let ((limits (copy-sequence ende-websocket--default-limits)) seen)
    (while overrides
      (let ((key (pop overrides)) (value (pop overrides)))
        (unless (and (plist-member limits key) (not (memq key seen))
                     (integerp value) (> value 0))
          (error "Invalid WebSocket limit"))
        (push key seen)
        (setq limits (plist-put limits key value))))
    (unless (and (<= (plist-get limits :line-bytes) (plist-get limits :header-bytes))
                 (<= (plist-get limits :write-frame-bytes) (plist-get limits :write-message-bytes))
                 (<= (plist-get limits :frame-bytes) (plist-get limits :message-bytes))
                 (>= (plist-get limits :input-bytes) 14))
      (error "Inconsistent WebSocket limits"))
    limits))

(defconst ende-websocket--token "[!#$%&'*+.^_`|~0-9A-Za-z-]+")

(defun ende-websocket--token-p (value)
  (and (stringp value)
       (string-match-p (concat "\\`" ende-websocket--token "\\'") value)))

(defun ende-websocket--target-p (target)
  (and (stringp target) (string-prefix-p "/" target)
       (string-match-p "\\`[!$&'()*+,;=:@/?A-Za-z0-9._~%+-]*\\'" target)
       (not (string-match-p "%\\(?:[^0-9A-Fa-f]\\|[0-9A-Fa-f]\\(?:[^0-9A-Fa-f]\\|\\'\\)\\|\\'\\)" target))))

(defun ende-websocket--ipv4-p (host)
  (let ((parts (split-string host "\\.")))
    (and (= (length parts) 4)
         (cl-every (lambda (part)
                     (and (string-match-p "\\`\\(?:0\\|[1-9][0-9]\\{0,2\\}\\)\\'" part)
                          (<= (string-to-number part) 255)))
                   parts))))

(defun ende-websocket--ipv6-p (host)
  (let ((candidate host) (valid t))
    (when (string-search "." candidate)
      (if (and (string-match "\\([0-9.]+\\)\\'" candidate)
               (> (match-beginning 1) 0)
               (= (aref candidate (1- (match-beginning 1))) ?:))
          (let ((start (match-beginning 1)) (tail (match-string 1 candidate)))
            (setq valid (ende-websocket--ipv4-p tail)
                  candidate (concat (substring candidate 0 start) "0:0")))
        (setq valid nil)))
    (let* ((sides (split-string candidate "::" nil))
           (groups (mapcan (lambda (side) (unless (equal side "") (split-string side ":" nil))) sides)))
      (and valid (<= (length sides) 2)
           (if (= (length sides) 2) (< (length groups) 8) (= (length groups) 8))
           (cl-every (lambda (group) (string-match-p "\\`[0-9A-Fa-f]\\{1,4\\}\\'" group)) groups)))))

(defun ende-websocket--hostname-p (host)
  (let ((name (string-remove-suffix "." host)))
    (and (<= (length name) 253)
         (if (and (string-search "." name) (string-match-p "\\`[0-9.]+\\'" name))
             (ende-websocket--ipv4-p name)
           (cl-every (lambda (label)
                       (and (<= 1 (length label) 63)
                            (string-match-p "\\`[A-Za-z0-9]\\(?:[A-Za-z0-9-]*[A-Za-z0-9]\\)?\\'" label)))
                     (split-string name "\\." nil))))))

(defun ende-websocket--authority (authority)
  (let ((case-fold-search nil))
    (unless (and (stringp authority)
                 (string-match
                  "\\`\\(\\[[0-9A-Fa-f:.]+\\]\\|[A-Za-z0-9][A-Za-z0-9.-]*\\)\\(?::\\([0-9]+\\)\\)?\\'"
                  authority))
      (error "Invalid WebSocket authority"))
    (let ((host (match-string 1 authority))
          (port (and (match-string 2 authority)
                     (string-to-number (match-string 2 authority)))))
      (unless (if (string-prefix-p "[" host)
                  (ende-websocket--ipv6-p (substring host 1 -1))
                (ende-websocket--hostname-p host))
        (error "Invalid WebSocket host"))
      (when (and port (not (<= 1 port 65535))) (error "Invalid WebSocket port"))
      (list (if (string-prefix-p "[" host) (substring host 1 -1) host) port))))

(defun ende-websocket--url (url)
  (let ((case-fold-search t))
    (unless (and (stringp url)
                 (string-match "\\`\\(wss?\\)://\\([^/?#]+\\)\\([^#]*\\)\\'" url))
      (error "Expected a ws or wss URL without a fragment"))
    (let* ((tls (equal (downcase (match-string 1 url)) "wss"))
           (authority (match-string 2 url)) (target (match-string 3 url))
           (address (ende-websocket--authority authority)))
      (when (or (equal target "") (string-prefix-p "?" target))
        (setq target (concat "/" target)))
      (unless (ende-websocket--target-p target) (error "Invalid escaped WebSocket target"))
      (list :host (car address) :port (or (cadr address) (if tls 443 80))
            :authority authority :target target :tls tls))))

(defun ende-websocket--protocols (protocols)
  (unless (and (proper-list-p protocols)
               (cl-every #'ende-websocket--token-p protocols)
               (= (length protocols) (length (delete-dups (copy-sequence protocols)))))
    (error "Invalid WebSocket subprotocols"))
  (mapcar #'copy-sequence protocols))

(defun ende-websocket--headers (headers)
  (unless (proper-list-p headers) (error "Headers must be an alist"))
  (mapcar
   (lambda (header)
     (unless (and (consp header) (ende-websocket--token-p (car header))
                  (stringp (cdr header))
                  (string-match-p "\\`[\t -~]*\\'" (cdr header))
                  (not (member (downcase (car header))
                               '("host" "upgrade" "connection" "content-length"
                                 "transfer-encoding" "expect" "trailer")))
                  (not (string-prefix-p "sec-websocket-" (downcase (car header)))))
       (error "Invalid or handshake-owned WebSocket header"))
     (cons (copy-sequence (car header)) (copy-sequence (cdr header))))
   headers))

(cl-defstruct (ende-websocket--http (:constructor ende-websocket--http-create))
  start fields (bytes 0) (count 0) (scan 1))

(defun ende-websocket--http-line (http line limits)
  (cl-incf (ende-websocket--http-bytes http) (+ 2 (length line)))
  (when (or (> (ende-websocket--http-bytes http) (plist-get limits :header-bytes))
            (> (length line) (plist-get limits :line-bytes)))
    (signal 'ende-websocket-too-large nil))
  (unless (string-match-p "\\`[\t -~\x80-\xff]*\\'" line)
    (signal 'ende-websocket-protocol-error nil))
  (cond
   ((null (ende-websocket--http-start http)) (setf (ende-websocket--http-start http) line) nil)
   ((equal line "") t)
   (t
    (cl-incf (ende-websocket--http-count http))
    (when (> (ende-websocket--http-count http) (plist-get limits :header-fields))
      (signal 'ende-websocket-too-large nil))
    (unless (string-match (concat "\\`\\(" ende-websocket--token "\\):[\t ]*\\(.*\\)\\'") line)
      (signal 'ende-websocket-protocol-error nil))
    (push (cons (downcase (match-string 1 line)) (string-trim (match-string 2 line) "[\t ]+" "[\t ]+"))
          (ende-websocket--http-fields http))
    nil)))

(defun ende-websocket--values (http name)
  (mapcar #'cdr (cl-remove-if-not (lambda (field) (equal (car field) name))
                                 (reverse (ende-websocket--http-fields http)))))

(defun ende-websocket--single (http name &optional optional)
  (let ((values (ende-websocket--values http name)))
    (unless (or (and optional (null values)) (= (length values) 1))
      (signal 'ende-websocket-protocol-error nil))
    (car values)))

(defun ende-websocket--tokens (http name &optional upgrade)
  (let (tokens)
    (dolist (value (ende-websocket--values http name))
      (dolist (token (split-string value ","))
        (setq token (string-trim token))
        (unless (if upgrade
                    (string-match-p (concat "\\`" ende-websocket--token
                                            "\\(?:/" ende-websocket--token "\\)?\\'") token)
                  (ende-websocket--token-p token))
          (signal 'ende-websocket-protocol-error nil))
        (push token tokens)))
    (nreverse tokens)))

(defun ende-websocket--upgrade (http)
  (unless (and (member "upgrade" (mapcar #'downcase (ende-websocket--tokens http "connection")))
               (member "websocket" (mapcar #'downcase (ende-websocket--tokens http "upgrade" t)))
               (not (ende-websocket--values http "transfer-encoding"))
               (not (ende-websocket--values http "content-length")))
    (signal 'ende-websocket-protocol-error nil)))

(defun ende-websocket--client-response (http key protocols)
  (unless (and (string-prefix-p "HTTP/1.1 101 " (ende-websocket--http-start http))
               (string-match-p "\\`HTTP/1\\.1 101 [ -~]*\\'" (ende-websocket--http-start http)))
    (signal 'ende-websocket-protocol-error nil))
  (ende-websocket--upgrade http)
  (unless (and (equal (ende-websocket--single http "sec-websocket-accept") (ende-websocket--accept key))
               (not (ende-websocket--values http "sec-websocket-extensions")))
    (signal 'ende-websocket-protocol-error nil))
  (let ((protocol (ende-websocket--single http "sec-websocket-protocol" t)))
    (when (and protocol (not (member protocol protocols)))
      (signal 'ende-websocket-protocol-error nil))
    protocol))

(defun ende-websocket--server-request (http)
  (unless (and (string-prefix-p "GET " (ende-websocket--http-start http))
               (string-match "\\`GET \\([^ ]+\\) HTTP/1\\.1\\'" (ende-websocket--http-start http))
               (string-suffix-p " HTTP/1.1" (ende-websocket--http-start http)))
    (signal 'ende-websocket-protocol-error nil))
  (let ((target (match-string 1 (ende-websocket--http-start http))))
    (unless (ende-websocket--target-p target) (signal 'ende-websocket-protocol-error nil))
    (ende-websocket--upgrade http)
    (let ((authority (ende-websocket--single http "host"))
          (key (ende-websocket--single http "sec-websocket-key"))
          (origin (ende-websocket--single http "origin" t))
          (protocols (ende-websocket--tokens http "sec-websocket-protocol")))
      (condition-case nil (ende-websocket--authority authority)
        (error (signal 'ende-websocket-protocol-error nil)))
      (unless (equal (ende-websocket--single http "sec-websocket-version") "13")
        (signal 'ende-websocket-version-error nil))
      (unless (and (string-match-p "\\`[A-Za-z0-9+/]\\{22\\}==\\'" key)
                   (condition-case nil
                       (let ((nonce (base64-decode-string key)))
                         (and (= (length nonce) 16)
                              (equal key (base64-encode-string nonce t))))
                     (error nil))
                   (= (length protocols) (length (delete-dups (copy-sequence protocols)))))
        (signal 'ende-websocket-protocol-error nil))
      (list :target target :authority authority :origin origin :protocols protocols
            :headers (reverse (ende-websocket--http-fields http)) :key key))))

(defun ende-websocket--client-request (address key protocols headers)
  (concat "GET " (plist-get address :target) " HTTP/1.1\r\nHost: "
          (plist-get address :authority)
          "\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Version: 13\r\nSec-WebSocket-Key: " key "\r\n"
          (when protocols (concat "Sec-WebSocket-Protocol: " (string-join protocols ", ") "\r\n"))
          (mapconcat (lambda (field) (concat (car field) ": " (cdr field) "\r\n")) headers "")
          "\r\n"))

(cl-defstruct (ende-websocket--message (:constructor ende-websocket--message-create))
  type utf8 chunks chunk (used 0) (bytes 0) (fragments 0))

(cl-defstruct (ende-websocket--output (:constructor ende-websocket--output-create))
  opcode bytes file signature size utf8 cleanup (offset 0) started)

(cl-defstruct (ende-websocket--connection (:constructor ende-websocket--connection-create))
  role process listener limits address headers protocols key protocol
  (state 'connecting) (generation 0) callbacks
  input (cursor 1) http frame message receive-budget
  output output-tail (output-bytes 0) (output-items 0)
  controls control-tail (control-count 0) backpressured
  close-queued draining-close close-sent close-received peer-close failure eof
  timers work-timer busy dispatching work-pending events terminal)

(cl-defstruct (ende-websocket--listener (:constructor ende-websocket--listener-create))
  process limits admit callbacks connections timer (generation 0) stopped)

(defvar ende-websocket--connections (make-hash-table :test #'eq))
(defvar ende-websocket--listeners (make-hash-table :test #'eq))
(defvar ende-websocket--openings (make-hash-table :test #'eq))
(defvar ende-websocket--shutting-down nil)

(defun ende-websocket-state (connection)
  "Return CONNECTION's state: connecting, open, closing, or closed."
  (ende-websocket--connection-state connection))

(defun ende-websocket-protocol (connection)
  "Return CONNECTION's selected subprotocol, or nil if none was selected."
  (ende-websocket--connection-protocol connection))

(defun ende-websocket--limit (connection key)
  (plist-get (ende-websocket--connection-limits connection) key))

(defun ende-websocket-set-receive-budget (connection bytes)
  "Set CONNECTION's remaining incoming payload budget to BYTES, or nil."
  (unless (or (null bytes) (natnump bytes)) (error "Invalid WebSocket receive budget"))
  (setf (ende-websocket--connection-receive-budget connection) bytes))

(defun ende-websocket--cancel-deadline (connection kind)
  (when-let* ((entry (assq kind (ende-websocket--connection-timers connection))))
    (cancel-timer (cdr entry))
    (setf (ende-websocket--connection-timers connection)
          (delq entry (ende-websocket--connection-timers connection)))))

(defun ende-websocket--deadline (connection kind seconds function)
  (ende-websocket--cancel-deadline connection kind)
  (let* ((generation (ende-websocket--connection-generation connection))
         (timer
          (run-at-time seconds nil
                       (lambda ()
                         (when (and (= generation (ende-websocket--connection-generation connection))
                                    (not (eq (ende-websocket-state connection) 'closed)))
                           (ende-websocket--cancel-deadline connection kind)
                           (funcall function connection))))))
    (push (cons kind timer) (ende-websocket--connection-timers connection))))

(defun ende-websocket--schedule (connection)
  (when (ende-websocket--connection-busy connection)
    (setf (ende-websocket--connection-work-pending connection) t))
  (unless (or (ende-websocket--connection-work-timer connection)
              (ende-websocket--connection-busy connection))
    (let ((generation (ende-websocket--connection-generation connection)))
      (setf (ende-websocket--connection-work-timer connection)
            (run-at-time 0 nil
                         (lambda ()
                           (when (= generation (ende-websocket--connection-generation connection))
                             (setf (ende-websocket--connection-work-timer connection) nil)
                             (ende-websocket--run connection))))))))

(defun ende-websocket--emit (connection event &rest arguments)
  (push (cons event arguments) (ende-websocket--connection-events connection)))

(defun ende-websocket--dispatch (connection)
  (unless (ende-websocket--connection-dispatching connection)
    (setf (ende-websocket--connection-dispatching connection) t)
    (unwind-protect
        (progn
          (let ((events (nreverse (ende-websocket--connection-events connection))))
            (setf (ende-websocket--connection-events connection) nil)
            (dolist (event events)
              (when (and (not (eq (ende-websocket-state connection) 'closed))
                         (or (not (eq (car event) :on-message))
                             (and (eq (ende-websocket-state connection) 'open)
                                  (not (ende-websocket--connection-close-received connection)))))
                (when-let* ((callback (plist-get (ende-websocket--connection-callbacks connection) (car event))))
                  (condition-case nil
                      (apply callback connection (cdr event))
                    ((error quit)
                     (ende-websocket--terminate connection
                                               '(:code 1011 :reason "" :clean nil :cause application))))))))
          (when-let* ((terminal (ende-websocket--connection-terminal connection)))
            (setf (ende-websocket--connection-terminal connection) nil)
            (remhash connection ende-websocket--connections)
            (when (car terminal)
              (condition-case nil (funcall (car terminal) connection (cdr terminal))
                ((error quit) nil)))))
      (setf (ende-websocket--connection-dispatching connection) nil))))

(defun ende-websocket--terminate (connection info)
  (unless (eq (ende-websocket-state connection) 'closed)
    (setf (ende-websocket--connection-state connection) 'closed)
    (cl-incf (ende-websocket--connection-generation connection))
    (ende-websocket--drop-output connection)
    (let ((process (ende-websocket--connection-process connection))
          (input (ende-websocket--connection-input connection))
          (callback (plist-get (ende-websocket--connection-callbacks connection) :on-close)))
      (dolist (entry (ende-websocket--connection-timers connection)) (cancel-timer (cdr entry)))
      (when (ende-websocket--connection-work-timer connection)
        (cancel-timer (ende-websocket--connection-work-timer connection)))
      (setf (ende-websocket--connection-process connection) nil
            (ende-websocket--connection-input connection) nil
            (ende-websocket--connection-timers connection) nil
            (ende-websocket--connection-work-timer connection) nil
            (ende-websocket--connection-callbacks connection) nil
            (ende-websocket--connection-headers connection) nil
            (ende-websocket--connection-address connection) nil
            (ende-websocket--connection-key connection) nil
            (ende-websocket--connection-http connection) nil
            (ende-websocket--connection-frame connection) nil
            (ende-websocket--connection-message connection) nil
            (ende-websocket--connection-output connection) nil
            (ende-websocket--connection-output-tail connection) nil
            (ende-websocket--connection-output-bytes connection) 0
            (ende-websocket--connection-output-items connection) 0
            (ende-websocket--connection-controls connection) nil
            (ende-websocket--connection-control-tail connection) nil
            (ende-websocket--connection-control-count connection) 0
            (ende-websocket--connection-draining-close connection) nil
            (ende-websocket--connection-events connection) nil
            (ende-websocket--connection-terminal connection) (cons callback info))
      (when process
        (set-process-filter process #'ignore)
        (set-process-sentinel process #'ignore)
        (set-process-plist process nil)
        (ignore-errors (delete-process process)))
      (when (buffer-live-p input) (kill-buffer input))
      (when-let* ((listener (ende-websocket--connection-listener connection)))
        (remhash connection (ende-websocket--listener-connections listener))
        (setf (ende-websocket--connection-listener connection) nil))
      (remhash connection ende-websocket--openings)
      (ende-websocket--schedule connection))))

(defun ende-websocket-abort (connection)
  "Immediately dispose of CONNECTION, idempotently; notify close asynchronously."
  (ende-websocket--terminate connection '(:code 1006 :reason "" :clean nil :cause abort))
  nil)

(defun ende-websocket--close-code-p (code)
  (and (integerp code)
       (or (memq code '(1000 1001 1002 1003 1007 1008 1009 1010 1011 1012 1013 1014))
           (<= 3000 code 4999))))

(defun ende-websocket--close-payload (code reason)
  (unless (ende-websocket--close-code-p code) (error "Invalid WebSocket Close code"))
  (when (> (ende-websocket--text-size reason) 123) (error "WebSocket Close reason exceeds 123 bytes"))
  (let ((bytes (encode-coding-string reason 'utf-8-unix t)))
    (let ((payload (make-string 2 0)))
      (ende-websocket--put-uint payload 0 2 code)
      (concat payload bytes))))

(defun ende-websocket--read-close (payload)
  (when (= (length payload) 1) (signal 'ende-websocket-protocol-error nil))
  (if (= (length payload) 0) '(:code 1005 :reason "")
    (let ((code (ende-websocket--uint payload 0 2)) (reason (substring payload 2)))
      (unless (ende-websocket--close-code-p code) (signal 'ende-websocket-protocol-error nil))
      (ende-websocket--utf8-feed (ende-websocket--utf8-create) reason t)
      (list :code code :reason (decode-coding-string reason 'utf-8-unix)))))

(defun ende-websocket--release-output (job)
  (let ((cleanup (ende-websocket--output-cleanup job)))
    (setf (ende-websocket--output-cleanup job) nil)
    (when cleanup
      (condition-case nil (funcall cleanup)
        ((error quit) nil)))))

(defun ende-websocket--drop-output (connection)
  (let ((jobs (ende-websocket--connection-output connection)))
    (setf (ende-websocket--connection-output connection) nil
          (ende-websocket--connection-output-tail connection) nil
          (ende-websocket--connection-output-bytes connection) 0
          (ende-websocket--connection-output-items connection) 0)
    (dolist (job jobs) (ende-websocket--release-output job))))

(defun ende-websocket--queue-close (connection payload &optional drain)
  (let ((first (not (ende-websocket--connection-close-queued connection)))
        (pending (ende-websocket--connection-draining-close connection)))
    (unless drain (ende-websocket--drop-output connection))
    (when first
      (setf (ende-websocket--connection-state connection) 'closing
            (ende-websocket--connection-close-queued connection) t)
      (ende-websocket--cancel-deadline connection 'message)
      (ende-websocket--deadline
       connection 'close (ende-websocket--limit connection :closing-timeout)
       (lambda (c) (ende-websocket--terminate c '(:code 1006 :reason "" :clean nil :cause timeout)))))
    (when (or first pending)
      (if (and drain (ende-websocket--connection-output connection))
          (setf (ende-websocket--connection-draining-close connection) payload)
        (setf (ende-websocket--connection-draining-close connection) nil
              (ende-websocket--connection-controls connection)
              (list (ende-websocket--output-create :opcode 8 :bytes (or pending payload)))
              (ende-websocket--connection-control-count connection) 1
              (ende-websocket--connection-control-tail connection)
              (ende-websocket--connection-controls connection))))
    (ende-websocket--schedule connection)))

(cl-defun ende-websocket-close (connection &optional (code 1000) (reason ""))
  "Gracefully close CONNECTION with CODE and REASON, idempotently.
Drain accepted application output before sending Close; reject new sends.
Completion is asynchronous and bounded by the closing timeout."
  (when (and (= code 1010) (eq (ende-websocket--connection-role connection) 'server))
    (error "Close code 1010 is client-only"))
  (let ((payload (ende-websocket--close-payload code reason)))
    (pcase (ende-websocket-state connection)
      ('connecting (ende-websocket-abort connection))
      ('open (ende-websocket--queue-close connection payload t))))
  nil)

(defun ende-websocket--reject (connection status info)
  (let ((phrase (pcase status (403 "Forbidden") (426 "Upgrade Required")
                       (431 "Request Header Fields Too Large") (500 "Internal Server Error")
                       (_ "Bad Request"))))
    (when-let* ((process (ende-websocket--connection-process connection)))
      (condition-case nil
          (process-send-string
           process (concat (format "HTTP/1.1 %d %s\r\n" status phrase)
                           "Connection: close\r\nContent-Length: 0\r\n"
                           (when (= status 426) "Sec-WebSocket-Version: 13\r\n") "\r\n"))
        ((error quit) nil)))
    (ende-websocket--terminate connection info)))

(defun ende-websocket--fail (connection cause &optional status)
  (unless (or (eq (ende-websocket-state connection) 'closed)
              (ende-websocket--connection-failure connection))
    (let* ((code (pcase cause ('utf8 1007) ('size 1009) ('policy 1008) (_ 1002)))
           (info (list :code code :reason "" :clean nil :cause cause)))
      (if (eq (ende-websocket-state connection) 'connecting)
          (if (eq (ende-websocket--connection-role connection) 'server)
              (ende-websocket--reject connection (or status (if (eq cause 'size) 431 400)) info)
            (ende-websocket--terminate connection info))
        (setf (ende-websocket--connection-failure connection) info
              (ende-websocket--connection-events connection) nil)
        (ende-websocket--queue-close connection (ende-websocket--close-payload code ""))
        (ende-websocket--schedule connection)))))

(defun ende-websocket--control (connection opcode payload)
  (pcase opcode
    (8
     (let ((info (ende-websocket--read-close payload)))
       (setf (ende-websocket--connection-message connection) nil)
       (ende-websocket--cancel-deadline connection 'message)
       (setf (ende-websocket--connection-peer-close connection) info
             (ende-websocket--connection-close-received connection) t
             (ende-websocket--connection-events connection) nil)
       (ende-websocket--queue-close connection payload)))
    (9
     (unless (or (ende-websocket--connection-close-received connection)
                 (ende-websocket--connection-close-sent connection))
       (when (>= (ende-websocket--connection-control-count connection)
                 (ende-websocket--limit connection :output-items))
         (signal 'ende-websocket-too-large nil))
       (let ((cell (list (ende-websocket--output-create :opcode 10 :bytes payload))))
         (if (ende-websocket--connection-control-tail connection)
             (setcdr (ende-websocket--connection-control-tail connection) cell)
           (setf (ende-websocket--connection-controls connection) cell))
         (setf (ende-websocket--connection-control-tail connection) cell))
       (cl-incf (ende-websocket--connection-control-count connection))))))

(defun ende-websocket--begin-frame (connection frame)
  (let ((opcode (ende-websocket--frame-opcode frame))
        (message (ende-websocket--connection-message connection)))
    (when (< opcode 8)
      (unless (if (= opcode 0) message (not message))
        (signal 'ende-websocket-protocol-error nil))
      (unless message
        (setq message (ende-websocket--message-create
                       :type (if (= opcode 1) 'text 'binary)
                       :utf8 (and (= opcode 1) (ende-websocket--utf8-create))))
        (setf (ende-websocket--connection-message connection) message)
        (when (eq (ende-websocket-state connection) 'open)
          (ende-websocket--deadline connection 'message (ende-websocket--limit connection :message-timeout)
                               (lambda (c) (ende-websocket--fail c 'policy)))))
      (cl-incf (ende-websocket--message-fragments message))
      (cl-incf (ende-websocket--message-bytes message) (ende-websocket--frame-length frame))
      (when (or (> (ende-websocket--message-fragments message) (ende-websocket--limit connection :fragment-count))
                (> (ende-websocket--message-bytes message) (ende-websocket--limit connection :message-bytes)))
        (signal 'ende-websocket-too-large nil))
      (when-let* ((budget (ende-websocket--connection-receive-budget connection)))
        (when (> (ende-websocket--frame-length frame) budget)
          (signal 'ende-websocket-too-large nil))
        (cl-decf (ende-websocket--connection-receive-budget connection)
                 (ende-websocket--frame-length frame))))))

(defun ende-websocket--collect (message bytes)
  (let ((offset 0) (length (length bytes)))
    (while (< offset length)
      (unless (ende-websocket--message-chunk message)
        ;; Cumulative frame lengths amortize tiny fragments without a large minimum.
        (setf (ende-websocket--message-chunk message)
              (make-string (min 16384 (ende-websocket--message-bytes message)) 0)))
      (let* ((chunk (ende-websocket--message-chunk message))
             (used (ende-websocket--message-used message))
             (count (min (- (length chunk) used) (- length offset))))
        (store-substring chunk used
                         (if (and (= offset 0) (= count length)) bytes
                           (substring bytes offset (+ offset count))))
        (cl-incf offset count)
        (cl-incf (ende-websocket--message-used message) count)
        (when (= (ende-websocket--message-used message) (length chunk))
          (push chunk (ende-websocket--message-chunks message))
          (setf (ende-websocket--message-chunk message) nil
                (ende-websocket--message-used message) 0))))))

(defun ende-websocket--compact-input (connection)
  (let* ((cursor (ende-websocket--connection-cursor connection))
         (consumed (- cursor (point-min))))
    (when (or (= cursor (point-max))
              (and (>= consumed 65536) (>= consumed (/ (buffer-size) 2))))
      (delete-region (point-min) cursor)
      (setf (ende-websocket--connection-cursor connection) 1))))

(defun ende-websocket--finish-frame (connection frame)
  (setf (ende-websocket--connection-frame connection) nil)
  (if (>= (ende-websocket--frame-opcode frame) 8)
      (ende-websocket--control connection (ende-websocket--frame-opcode frame)
                          (apply #'concat (nreverse (ende-websocket--frame-chunks frame))))
    (when (ende-websocket--frame-final frame)
      (let* ((message (ende-websocket--connection-message connection))
             (utf8 (ende-websocket--message-utf8 message)))
        (when utf8 (ende-websocket--utf8-feed utf8 "" t))
        (setf (ende-websocket--connection-message connection) nil)
        (ende-websocket--cancel-deadline connection 'message)
        (when (eq (ende-websocket-state connection) 'open)
          (when (> (ende-websocket--message-used message) 0)
            (push (substring (ende-websocket--message-chunk message) 0 (ende-websocket--message-used message))
                  (ende-websocket--message-chunks message)))
          (let ((bytes (apply #'concat (nreverse (ende-websocket--message-chunks message)))))
            (ende-websocket--emit connection :on-message (ende-websocket--message-type message)
                             (if utf8 (decode-coding-string bytes 'utf-8-unix) bytes))))))))

(defun ende-websocket--parse-frames (connection)
  (let ((bytes 0) (frames 0) (progress t) deferred)
    (with-current-buffer (ende-websocket--connection-input connection)
      (while (and progress
                  (< bytes (ende-websocket--limit connection :drain-bytes))
                  (< frames (ende-websocket--limit connection :drain-frames))
                  (not (ende-websocket--connection-close-received connection)))
        (setq progress nil)
        (let ((cursor (ende-websocket--connection-cursor connection)))
          (unless (ende-websocket--connection-frame connection)
            (when-let* ((header (ende-websocket--frame-header
                                (buffer-substring-no-properties cursor (min (point-max) (+ cursor 14)))
                                (ende-websocket--connection-role connection)
                                (ende-websocket--limit connection :frame-bytes))))
              (if (and (= (ende-websocket--frame-opcode (cdr header)) 8)
                       (ende-websocket--connection-events connection))
                  (setq deferred t)
                (ende-websocket--begin-frame connection (cdr header))
                (cl-incf bytes (car header))
                (cl-incf cursor (car header))
                (setf (ende-websocket--connection-cursor connection) cursor
                      (ende-websocket--connection-frame connection) (cdr header)))))
          (when-let* ((frame (ende-websocket--connection-frame connection)))
            (let ((count (min (ende-websocket--frame-remaining frame) (- (point-max) cursor)
                              (max 0 (- (ende-websocket--limit connection :drain-bytes) bytes)))))
              (when (> count 0)
                (let ((chunk (buffer-substring-no-properties cursor (+ cursor count))))
                  (when (ende-websocket--frame-mask frame)
                    (ende-websocket--mask! chunk (ende-websocket--frame-mask frame) (ende-websocket--frame-offset frame)))
                  (if (>= (ende-websocket--frame-opcode frame) 8)
                      (push chunk (ende-websocket--frame-chunks frame))
                    (let ((message (ende-websocket--connection-message connection)))
                      (when (ende-websocket--message-utf8 message)
                        (ende-websocket--utf8-feed (ende-websocket--message-utf8 message) chunk))
                      (ende-websocket--collect message chunk))))
                (cl-incf (ende-websocket--frame-offset frame) count)
                (cl-decf (ende-websocket--frame-remaining frame) count)
                (cl-incf (ende-websocket--connection-cursor connection) count)
                (cl-incf bytes count)
                (setq progress t))
              (when (= (ende-websocket--frame-remaining frame) 0)
                (cl-incf frames)
                (ende-websocket--finish-frame connection frame)
                (setq progress t))))))
      (ende-websocket--compact-input connection)
      (and (not (ende-websocket--connection-close-received connection))
           (< (ende-websocket--connection-cursor connection) (point-max))
           (or deferred (>= bytes (ende-websocket--limit connection :drain-bytes))
               (>= frames (ende-websocket--limit connection :drain-frames)))))))

(defun ende-websocket--capacity (connection size)
  (unless (eq (ende-websocket-state connection) 'open) (error "WebSocket is not open"))
  (when (> size (min (ende-websocket--limit connection :write-message-bytes)
                     (ende-websocket--limit connection :output-bytes)))
    (signal 'ende-websocket-too-large nil))
  (when (or (> (+ (ende-websocket--connection-output-bytes connection) size)
               (ende-websocket--limit connection :output-bytes))
            (>= (ende-websocket--connection-output-items connection) (ende-websocket--limit connection :output-items)))
    (setf (ende-websocket--connection-backpressured connection) t)
    (signal 'ende-websocket-backpressure nil)))

(defun ende-websocket--output-length (job)
  (or (ende-websocket--output-size job) (length (ende-websocket--output-bytes job))))

(defun ende-websocket--queue-output (connection job)
  (let ((size (ende-websocket--output-length job))
        (cell (list job)))
    (ende-websocket--capacity connection size)
    (if (ende-websocket--connection-output-tail connection)
        (setcdr (ende-websocket--connection-output-tail connection) cell)
      (setf (ende-websocket--connection-output connection) cell))
    (setf (ende-websocket--connection-output-tail connection) cell)
    (cl-incf (ende-websocket--connection-output-bytes connection) size)
    (cl-incf (ende-websocket--connection-output-items connection)))
  (ende-websocket--schedule connection)
  t)

(defun ende-websocket--enqueue (connection opcode bytes &optional owned)
  (ende-websocket--capacity connection (length bytes))
  (ende-websocket--queue-output
   connection (ende-websocket--output-create :opcode opcode
                                            :bytes (if owned bytes (copy-sequence bytes)))))

(defun ende-websocket-send-text (connection text)
  "Queue a snapshot of Unicode scalar TEXT on CONNECTION.
Return t when accepted, not delivered.  Signal `ende-websocket-backpressure'
without queue mutation if capacity is exhausted."
  (ende-websocket--capacity connection (ende-websocket--text-size text))
  (ende-websocket--enqueue connection 1 (encode-coding-string text 'utf-8-unix) t))

(defun ende-websocket-send-text-bytes (connection bytes)
  "Queue validated unibyte UTF-8 BYTES without decoding and re-encoding.
The acceptance and backpressure contract is that of `ende-websocket-send-text'."
  (ende-websocket--octets bytes)
  (ende-websocket--capacity connection (length bytes))
  (ende-websocket--utf8-feed (ende-websocket--utf8-create) bytes t)
  (ende-websocket--enqueue connection 1 bytes))

(defun ende-websocket--file-signature (file)
  (unless (and (stringp file) (file-name-absolute-p file) (not (file-remote-p file)))
    (error "WebSocket output requires a local absolute file"))
  (let ((attributes (file-attributes file 'integer)))
    (unless (and attributes (null (file-attribute-type attributes)) (file-readable-p file))
      (error "WebSocket output requires a readable regular file"))
    (list (file-attribute-file-identifier attributes)
          (file-attribute-size attributes)
          (file-attribute-modification-time attributes)
          (file-attribute-status-change-time attributes))))

(defun ende-websocket-send-text-file (connection file &optional cleanup)
  "Queue immutable local FILE as UTF-8 text, reading only bounded chunks.
Acceptance and backpressure follow `ende-websocket-send-text'.  The caller
must keep FILE unchanged until CLEANUP, called exactly once after accepted
output is sent or discarded.  Rejection does not take ownership.  Invalid
UTF-8 or file changes fail asynchronously through the connection lifecycle."
  (unless (or (null cleanup) (functionp cleanup)) (error "Invalid output cleanup"))
  (let ((signature (ende-websocket--file-signature file)))
    (ende-websocket--queue-output
     connection (ende-websocket--output-create
                 :opcode 1 :file (copy-sequence file) :signature signature
                 :size (nth 1 signature) :utf8 (ende-websocket--utf8-create)
                 :cleanup cleanup))))

(defun ende-websocket--output-chunk (job end)
  (let ((offset (ende-websocket--output-offset job))
        (file (ende-websocket--output-file job)))
    (if (not file) (substring (ende-websocket--output-bytes job) offset end)
      (let ((signature (ende-websocket--output-signature job)))
        (unless (equal signature (ende-websocket--file-signature file))
          (error "WebSocket output file changed"))
        (let ((chunk (with-temp-buffer
                       (set-buffer-multibyte nil)
                       (insert-file-contents-literally file nil offset end)
                       (buffer-string))))
          (unless (and (= (length chunk) (- end offset))
                       (equal signature (ende-websocket--file-signature file)))
            (error "WebSocket output file changed"))
          (ende-websocket--utf8-feed (ende-websocket--output-utf8 job) chunk
                                     (= end (ende-websocket--output-length job)))
          chunk)))))

(defun ende-websocket-send-binary (connection bytes)
  "Queue a snapshot of unibyte BYTES; acceptance does not imply delivery.
Signal `ende-websocket-backpressure' without queue mutation when full."
  (ende-websocket--enqueue connection 2 (ende-websocket--octets bytes)))

(defun ende-websocket--write (connection)
  (let ((bytes 0) (frames 0))
    (while (and (not (eq (ende-websocket-state connection) 'closed))
                (< bytes (ende-websocket--limit connection :drain-bytes))
                (< frames (ende-websocket--limit connection :drain-frames))
                (or (ende-websocket--connection-draining-close connection)
                    (ende-websocket--connection-controls connection)
                    (and (not (ende-websocket--connection-close-sent connection))
                         (ende-websocket--connection-output connection))))
      (when (and (ende-websocket--connection-draining-close connection)
                 (not (ende-websocket--connection-output connection)))
        (ende-websocket--queue-close connection (ende-websocket--connection-draining-close connection)))
      (let* ((control (car (ende-websocket--connection-controls connection)))
             (job (or control (car (ende-websocket--connection-output connection))))
             (size (ende-websocket--output-length job))
             (offset (ende-websocket--output-offset job))
             (end (if control size
                    (min size
                         (+ offset (ende-websocket--limit connection :write-frame-bytes))
                         (+ offset (max 1 (- (ende-websocket--limit connection :drain-bytes) bytes))))))
             (final (= end size))
             (opcode (if (ende-websocket--output-started job) 0 (ende-websocket--output-opcode job)))
             (wire (ende-websocket--wire opcode final (ende-websocket--output-chunk job end)
                                    (ende-websocket--connection-role connection))))
        (setf (ende-websocket--output-started job) t
              (ende-websocket--output-offset job) end)
        (if control
            (progn
              (pop (ende-websocket--connection-controls connection))
              (cl-decf (ende-websocket--connection-control-count connection))
              (unless (ende-websocket--connection-controls connection)
                (setf (ende-websocket--connection-control-tail connection) nil))
              (when (= opcode 8) (setf (ende-websocket--connection-close-sent connection) t)))
          (when final
            (pop (ende-websocket--connection-output connection))
            (cl-decf (ende-websocket--connection-output-items connection))
            (cl-decf (ende-websocket--connection-output-bytes connection) size)
            (unless (ende-websocket--connection-output connection)
              (setf (ende-websocket--connection-output-tail connection) nil))))
        (cl-incf bytes (length wire))
        (cl-incf frames)
        (unwind-protect
            (process-send-string (ende-websocket--connection-process connection) wire)
          (when final (ende-websocket--release-output job)))))
    (when (and (ende-websocket--connection-backpressured connection)
               (< (ende-websocket--connection-output-bytes connection)
                  (max 1 (/ (ende-websocket--limit connection :output-bytes) 2)))
               (< (ende-websocket--connection-output-items connection) (max 1 (/ (ende-websocket--limit connection :output-items) 2))))
      (setf (ende-websocket--connection-backpressured connection) nil)
      (ende-websocket--emit connection :on-writable))
    (cond
     ((ende-websocket--connection-failure connection)
      (ende-websocket--terminate connection (ende-websocket--connection-failure connection)))
     ((and (ende-websocket--connection-close-sent connection)
           (ende-websocket--connection-close-received connection)
           (eq (ende-websocket--connection-role connection) 'server))
      (ende-websocket--terminate connection
                            (append (ende-websocket--connection-peer-close connection)
                                    '(:clean t :cause close)))))))

(defun ende-websocket--nsm-verify (function process &rest arguments)
  (if (process-get process 'ende-websocket--connection)
      (let ((nsm-noninteractive t) (inhibit-message t))
        (apply function process arguments))
    (apply function process arguments)))

(defun ende-websocket--transport-ready (connection)
  (when (and (eq (ende-websocket-state connection) 'connecting)
             (ende-websocket--connection-address connection)
             (not (ende-websocket--connection-key connection)))
    (let* ((key (base64-encode-string (ende-websocket--os-random-bytes 16) t))
           (request (ende-websocket--client-request
                     (ende-websocket--connection-address connection) key
                     (ende-websocket--connection-protocols connection)
                     (ende-websocket--connection-headers connection))))
      (when (> (length request) (ende-websocket--limit connection :header-bytes))
        (signal 'ende-websocket-too-large nil))
      (setf (ende-websocket--connection-key connection) key
            (ende-websocket--connection-headers connection) nil)
      (process-send-string (ende-websocket--connection-process connection) request))))

(defun ende-websocket--sentinel (process _event)
  (when-let* ((connection (process-get process 'ende-websocket--connection)))
    (when (= (or (process-get process 'ende-websocket--generation) -1)
             (ende-websocket--connection-generation connection))
      (unless (ende-websocket--connection-process connection)
        (setf (ende-websocket--connection-process connection) process))
      (condition-case nil
          (pcase (process-status process)
            ('open (ende-websocket--transport-ready connection))
            ((or 'closed 'failed 'exit 'signal)
             (setf (ende-websocket--connection-eof connection) t)
             (ende-websocket--schedule connection)))
        ((error quit)
         (ende-websocket--terminate connection '(:code 1006 :reason "" :clean nil :cause transport)))))))

(defun ende-websocket--filter (process bytes)
  (when-let* ((connection (process-get process 'ende-websocket--connection)))
    (when (and (= (or (process-get process 'ende-websocket--generation) -1)
                  (ende-websocket--connection-generation connection))
               (not (eq (ende-websocket-state connection) 'closed))
               (not (ende-websocket--connection-failure connection))
               (not (ende-websocket--connection-close-received connection)))
      (unless (ende-websocket--connection-process connection)
        (setf (ende-websocket--connection-process connection) process))
      (with-current-buffer (ende-websocket--connection-input connection)
        (if (> (+ (- (point-max) (ende-websocket--connection-cursor connection)) (length bytes))
               (ende-websocket--limit connection :input-bytes))
            (ende-websocket--fail connection 'size)
          (goto-char (point-max))
          (insert bytes)))
      (ende-websocket--schedule connection))))

(defun ende-websocket--check-opening (connection)
  (when (eq (ende-websocket-state connection) 'connecting)
    (if (memq (process-status (ende-websocket--connection-process connection)) '(closed failed exit signal))
        (progn
          (setf (ende-websocket--connection-eof connection) t)
          (ende-websocket--schedule connection))
      (ende-websocket--deadline connection 'transport-watch 0.1 #'ende-websocket--check-opening))))

(defun ende-websocket--start-client (connection)
  (unless (advice-member-p #'ende-websocket--nsm-verify 'nsm-verify-connection)
    (advice-add 'nsm-verify-connection :around #'ende-websocket--nsm-verify))
  (let* ((address (ende-websocket--connection-address connection))
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
           :filter #'ende-websocket--filter :sentinel #'ende-websocket--sentinel
           :plist (list 'ende-websocket--connection connection
                        'ende-websocket--generation (ende-websocket--connection-generation connection))
           :tls-parameters parameters)))
    (if (eq (ende-websocket-state connection) 'closed)
        (ignore-errors (delete-process process))
      (setf (ende-websocket--connection-process connection) process)
      (ende-websocket--check-opening connection)
      (when (eq (process-status process) 'open) (ende-websocket--transport-ready connection)))))

(defun ende-websocket--opened (connection protocol)
  (setf (ende-websocket--connection-state connection) 'open
        (ende-websocket--connection-protocol connection) protocol
        (ende-websocket--connection-http connection) nil
        (ende-websocket--connection-key connection) nil
        (ende-websocket--connection-address connection) nil)
  (ende-websocket--cancel-deadline connection 'handshake)
  (ende-websocket--cancel-deadline connection 'transport-watch)
  (remhash connection ende-websocket--openings)
  (ende-websocket--emit connection :on-open))

(defun ende-websocket--read-http (connection)
  (let ((http (ende-websocket--connection-http connection)) done)
    (with-current-buffer (ende-websocket--connection-input connection)
      (goto-char (ende-websocket--http-scan http))
      (while (and (not done) (search-forward "\r\n" nil t))
        (let ((end (point)))
          (setq done (ende-websocket--http-line
                      http (buffer-substring-no-properties (ende-websocket--connection-cursor connection) (- end 2))
                      (ende-websocket--connection-limits connection)))
          (setf (ende-websocket--connection-cursor connection) end)
          (when (and done (eq (ende-websocket--connection-role connection) 'client)
                     (string-match-p "\\`HTTP/1\\.1 1[0-9][0-9] [ -~]*\\'" (ende-websocket--http-start http))
                     (not (string-prefix-p "HTTP/1.1 101 " (ende-websocket--http-start http))))
            (when (or (ende-websocket--values http "content-length")
                      (ende-websocket--values http "transfer-encoding"))
              (signal 'ende-websocket-protocol-error nil))
            (setf (ende-websocket--http-start http) nil (ende-websocket--http-fields http) nil)
            (setq done nil))))
      (setf (ende-websocket--http-scan http) (max (ende-websocket--connection-cursor connection) (1- (point-max))))
      (when (or (> (+ (ende-websocket--http-bytes http)
                       (- (point-max) (ende-websocket--connection-cursor connection)))
                    (ende-websocket--limit connection :header-bytes))
                (> (- (point-max) (ende-websocket--connection-cursor connection)
                      (if (and (> (point-max) (ende-websocket--connection-cursor connection))
                               (= (char-before (point-max)) 13)) 1 0))
                   (ende-websocket--limit connection :line-bytes)))
        (unless done (signal 'ende-websocket-too-large nil)))
      (when done
        (if (eq (ende-websocket--connection-role connection) 'client)
            (ende-websocket--opened connection
                               (ende-websocket--client-response http (ende-websocket--connection-key connection)
                                                           (ende-websocket--connection-protocols connection)))
          (ende-websocket--admit connection http))
        (when (buffer-live-p (ende-websocket--connection-input connection))
          (delete-region (point-min) (ende-websocket--connection-cursor connection))
          (setf (ende-websocket--connection-cursor connection) 1))))
    done))

(defun ende-websocket--run (connection)
  (unless (ende-websocket--connection-busy connection)
    (let (more)
      (setf (ende-websocket--connection-busy connection) t
            (ende-websocket--connection-work-pending connection) nil)
      (unwind-protect
          (progn
            (condition-case error
                (progn
                  (when (and (eq (ende-websocket-state connection) 'connecting)
                             (eq (ende-websocket--connection-role connection) 'client)
                             (not (ende-websocket--connection-process connection)))
                    (ende-websocket--start-client connection))
                  (when (and (eq (ende-websocket-state connection) 'connecting)
                             (ende-websocket--connection-process connection))
                    (ende-websocket--read-http connection)
                    (ende-websocket--dispatch connection))
                  (when (memq (ende-websocket-state connection) '(open closing))
                    (unless (ende-websocket--connection-failure connection)
                      (setq more (ende-websocket--parse-frames connection)))
                    (ende-websocket--write connection)))
              (ende-websocket-invalid-utf8 (ende-websocket--fail connection 'utf8))
              (ende-websocket-too-large (ende-websocket--fail connection 'size))
              (ende-websocket-version-error (ende-websocket--fail connection 'protocol 426))
              (ende-websocket-protocol-error (ende-websocket--fail connection 'protocol))
              ((error quit)
               (ignore error)
               (ende-websocket--terminate connection '(:code 1006 :reason "" :clean nil :cause transport))))
            (ende-websocket--dispatch connection))
        (setf (ende-websocket--connection-busy connection) nil))
      (when (and (ende-websocket--connection-eof connection) (not more)
                 (not (eq (ende-websocket-state connection) 'closed)))
        (ende-websocket--terminate
         connection
         (if (and (ende-websocket--connection-close-sent connection)
                  (ende-websocket--connection-close-received connection))
             (append (ende-websocket--connection-peer-close connection) '(:clean t :cause close))
           '(:code 1006 :reason "" :clean nil :cause transport))))
      (when (or more (ende-websocket--connection-work-pending connection)
                (ende-websocket--connection-failure connection)
                (ende-websocket--connection-draining-close connection)
                (ende-websocket--connection-controls connection)
                (ende-websocket--connection-output connection)
                (ende-websocket--connection-terminal connection))
        (unless (and (eq (ende-websocket-state connection) 'closed)
                     (not (ende-websocket--connection-terminal connection)))
          (ende-websocket--schedule connection))))))

(defun ende-websocket--new (role limits callbacks)
  (let ((connection (ende-websocket--connection-create
                     :role role :limits limits :callbacks callbacks
                     :http (ende-websocket--http-create)
                     :input (generate-new-buffer " *websocket-input*"))))
    (with-current-buffer (ende-websocket--connection-input connection) (set-buffer-multibyte nil))
    (puthash connection t ende-websocket--connections)
    (ende-websocket--deadline
     connection 'handshake (ende-websocket--limit connection :handshake-timeout)
     (lambda (c) (ende-websocket--terminate c '(:code 1006 :reason "" :clean nil :cause timeout))))
    connection))

(defun ende-websocket--callbacks (on-open on-message on-close on-writable)
  (dolist (callback (list on-open on-message on-close on-writable))
    (unless (or (null callback) (functionp callback)) (error "Invalid WebSocket callback")))
  (list :on-open on-open :on-message on-message :on-close on-close :on-writable on-writable))

(cl-defun ende-websocket-connect (url &key headers protocols limits on-open on-message on-close on-writable)
  "Start an asynchronous ws or certificate-verified wss connection to URL.
Return a handle immediately; callbacks never run before this call returns.
HEADERS is an ASCII name/value alist; handshake-owned fields are forbidden.
PROTOCOLS is an ordered list of unique, case-sensitive subprotocol tokens.
LIMITS overrides positive integer keys in `ende-websocket--default-limits'.
ON-OPEN receives the connection.  ON-MESSAGE receives connection, type
(text or binary), and a complete validated message: a decoded string for
text, a unibyte string for binary.  ON-CLOSE receives connection and a plist
with :code, :reason, :clean, :cause, exactly once.  ON-WRITABLE receives
the connection once at low water after backpressure.  Callback errors
terminate with cause application; close callback errors are contained.
Synchronous argument errors signal; asynchronous failures notify ON-CLOSE.
Native writes and DNS/security operations can block despite bounded work."
  (when ende-websocket--shutting-down (error "WebSocket shutdown in progress"))
  (let ((address (ende-websocket--url url)) (headers (ende-websocket--headers headers))
        (protocols (ende-websocket--protocols protocols)) (limits (ende-websocket--limits limits))
        (callbacks (ende-websocket--callbacks on-open on-message on-close on-writable)))
    (when (>= (hash-table-count ende-websocket--openings)
              (plist-get limits :pending-openings))
      (signal 'ende-websocket-backpressure nil))
    (let ((connection (ende-websocket--new 'client limits callbacks)))
      (setf (ende-websocket--connection-address connection) address
            (ende-websocket--connection-headers connection) headers
            (ende-websocket--connection-protocols connection) protocols)
      (puthash connection t ende-websocket--openings)
      (ende-websocket--schedule connection)
      connection)))

(defun ende-websocket--admit (connection http)
  (let* ((request (ende-websocket--server-request http))
         (key (copy-sequence (plist-get request :key)))
         (offered (mapcar #'copy-sequence (plist-get request :protocols)))
         (listener (ende-websocket--connection-listener connection))
         (decision
          (condition-case nil
              (funcall (ende-websocket--listener-admit listener) connection
                       (list :target (plist-get request :target)
                             :authority (plist-get request :authority)
                             :origin (plist-get request :origin)
                             :protocols (plist-get request :protocols)
                             :headers (plist-get request :headers)
                             :peer (process-contact (ende-websocket--connection-process connection) :remote)))
            ((error quit)
             (ende-websocket--reject connection 500 '(:code 1011 :reason "" :clean nil :cause application))
             nil))))
    (when (eq (ende-websocket-state connection) 'connecting)
      (if (not (and (proper-list-p decision) (cl-evenp (length decision))
                    (eq (plist-get decision :accept) t)
                    (not (ende-websocket--listener-stopped listener))))
          (ende-websocket--reject connection 403 '(:code 1008 :reason "" :clean nil :cause policy))
        (let ((protocol (plist-get decision :protocol)))
          (if (and protocol (not (member protocol offered)))
              (ende-websocket--reject connection 500 '(:code 1011 :reason "" :clean nil :cause application))
            (let ((response (concat "HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\n"
                                    "Connection: Upgrade\r\nSec-WebSocket-Accept: "
                                    (ende-websocket--accept key) "\r\n"
                                    (when protocol (concat "Sec-WebSocket-Protocol: " protocol "\r\n"))
                                    "\r\n")))
              (process-send-string (ende-websocket--connection-process connection) response)
              (when (eq (ende-websocket-state connection) 'connecting)
                (ende-websocket--opened connection (and protocol (copy-sequence protocol)))))))))))

(defun ende-websocket--accepted (listener process)
  (or (process-get process 'ende-websocket--connection)
      (if (or (ende-websocket--listener-stopped listener)
              (>= (hash-table-count (ende-websocket--listener-connections listener))
                  (plist-get (ende-websocket--listener-limits listener) :connections)))
          (progn
            (set-process-filter process #'ignore)
            (set-process-sentinel process #'ignore)
            (set-process-plist process nil)
            (ignore-errors (delete-process process))
            nil)
        (let ((connection (ende-websocket--new 'server (ende-websocket--listener-limits listener)
                                          (ende-websocket--listener-callbacks listener))))
          (setf (ende-websocket--connection-process connection) process
                (ende-websocket--connection-listener connection) listener)
          (set-process-plist process (list 'ende-websocket--connection connection 'ende-websocket--generation 0))
          (set-process-coding-system process 'binary 'binary)
          (set-process-query-on-exit-flag process nil)
          (puthash connection t (ende-websocket--listener-connections listener))
          connection))))

(defun ende-websocket--accept-log (server process _message)
  (when-let* ((listener (process-get server 'ende-websocket--listener)))
    (ende-websocket--accepted listener process)))

(cl-defun ende-websocket-listen (&key (host "127.0.0.1") service admit limits
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
Callbacks and LIMITS have the contracts of `ende-websocket-connect'.
No callbacks run before this constructor returns.  Stopping the listener
disposes of all its accepted connections."
  (when ende-websocket--shutting-down (error "WebSocket shutdown in progress"))
  (unless (and (stringp host) (not (string-empty-p host))
               (or (eq service t) (and (integerp service) (<= 1 service 65535)))
               (functionp admit))
    (error "Invalid WebSocket listener arguments"))
  (let* ((limits (ende-websocket--limits limits))
         (callbacks (ende-websocket--callbacks on-open on-message on-close on-writable))
         (listener (ende-websocket--listener-create :limits limits :admit admit :callbacks callbacks
                                               :connections (make-hash-table :test #'eq))))
    (condition-case error
        (let ((process
               (make-network-process
                :name "websocket-listener" :host host :service service :server t :stop t
                :buffer nil :coding 'binary :filter-multibyte nil :noquery t
                :filter #'ende-websocket--filter :sentinel #'ende-websocket--sentinel
                :log #'ende-websocket--accept-log :plist (list 'ende-websocket--listener listener))))
          (setf (ende-websocket--listener-process listener) process)
          (puthash listener t ende-websocket--listeners)
          (let ((generation (ende-websocket--listener-generation listener)))
            (setf (ende-websocket--listener-timer listener)
                  (run-at-time 0 nil
                               (lambda ()
                                 (when (and (= generation (ende-websocket--listener-generation listener))
                                            (not (ende-websocket--listener-stopped listener)))
                                   (setf (ende-websocket--listener-timer listener) nil)
                                   (continue-process process))))))
          listener)
      ((error quit)
       (ende-websocket-stop-listener listener)
       (signal (car error) (cdr error))))))

(defun ende-websocket-listener-port (listener)
  "Return LISTENER's bound TCP port, or nil after it has stopped."
  (when-let* ((process (ende-websocket--listener-process listener)))
    (process-contact process :service)))

(defun ende-websocket-stop-listener (listener)
  "Stop LISTENER and abort all its accepted connections, idempotently."
  (unless (ende-websocket--listener-stopped listener)
    (setf (ende-websocket--listener-stopped listener) t)
    (cl-incf (ende-websocket--listener-generation listener))
    (when-let* ((timer (ende-websocket--listener-timer listener))) (cancel-timer timer))
    (let ((process (ende-websocket--listener-process listener)) connections)
      (setf (ende-websocket--listener-process listener) nil
            (ende-websocket--listener-timer listener) nil
            (ende-websocket--listener-admit listener) nil
            (ende-websocket--listener-callbacks listener) nil)
      (remhash listener ende-websocket--listeners)
      (when process
        (set-process-plist process nil)
        (ignore-errors (delete-process process)))
      (maphash (lambda (connection _) (push connection connections))
               (ende-websocket--listener-connections listener))
      (dolist (connection connections)
        (ende-websocket--terminate connection '(:code 1001 :reason "" :clean nil :cause listener))))))

(defun ende-websocket-shutdown ()
  "Stop all listeners and dispose of all connections, work, and callbacks.
Close callbacks run after resources are detached.  Safe to call repeatedly;
new connections cannot be constructed during shutdown callbacks."
  (unless ende-websocket--shutting-down
    (let ((ende-websocket--shutting-down t) listeners connections)
      (maphash (lambda (listener _) (push listener listeners)) ende-websocket--listeners)
      (dolist (listener listeners) (ende-websocket-stop-listener listener))
      (maphash (lambda (connection _) (push connection connections)) ende-websocket--connections)
      (dolist (connection connections)
        (ende-websocket--terminate connection '(:code 1001 :reason "" :clean nil :cause shutdown)))
      (dolist (connection connections)
        (when-let* ((timer (ende-websocket--connection-work-timer connection))) (cancel-timer timer))
        (setf (ende-websocket--connection-work-timer connection) nil)
        (ende-websocket--dispatch connection))
      (clrhash ende-websocket--openings)
      (advice-remove 'nsm-verify-connection #'ende-websocket--nsm-verify))))

(defun ende-websocket-unload-function ()
  (ende-websocket-shutdown)
  nil)


(provide 'ende-websocket)
;;; ende-websocket.el ends here
