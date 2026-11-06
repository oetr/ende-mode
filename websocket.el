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

(provide 'websocket)
;;; websocket.el ends here
