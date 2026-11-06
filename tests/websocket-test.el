;;; websocket-test.el --- WebSocket behavioral checks -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(load (expand-file-name "../websocket.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t t)

(ert-deftest websocket-codec ()
  (should (equal (websocket--accept "dGhlIHNhbXBsZSBub25jZQ==")
                 "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="))
  (let* ((wire (unibyte-string #x81 #x85 #x37 #xfa #x21 #x3d
                               #x7f #x9f #x4d #x51 #x58))
         (header (websocket--frame-header wire 'server 1024))
         (frame (cdr header)))
    (should (= (car header) 6))
    (should (equal (websocket--mask! (substring wire 6)
                                    (websocket--frame-mask frame) 0) "Hello"))
    (should-error (websocket--frame-header wire 'client 1024)
                  :type 'websocket-protocol-error))
  (dolist (bytes (list (unibyte-string #x81 #x7e 0 1)
                       (unibyte-string #x89 #x7e 0 126)
                       (unibyte-string #xc1 0)
                       (unibyte-string #x83 0)))
    (should-error (websocket--frame-header bytes 'client 1024)
                  :type 'websocket-protocol-error))
  (dolist (text '("" "Hello" "\ufeff\r\nλ😀"))
    (let ((bytes (websocket--text text)))
      (dotimes (split (1+ (length bytes)))
        (let ((state (websocket--utf8-create)))
          (websocket--utf8-feed state (substring bytes 0 split))
          (websocket--utf8-feed state (substring bytes split) t)))
      (should (equal text (decode-coding-string bytes 'utf-8-unix)))))
  (dolist (bytes (list '(#xc0 #x80) '(#xed #xa0 #x80) '(#xf4 #x90 #x80 #x80)
                       '(#x80) '(#xe0 #x9f #xbf) '(#xf0 #x8f #xbf #xbf) '(#xc2)))
    (setq bytes (apply #'unibyte-string bytes))
    (dotimes (split (1+ (length bytes)))
      (should-error
       (let ((state (websocket--utf8-create)))
         (websocket--utf8-feed state (substring bytes 0 split))
         (websocket--utf8-feed state (substring bytes split) t))
       :type 'websocket-invalid-utf8))))

(defun websocket-test--http (wire)
  (let ((http (websocket--http-create)))
    (dolist (line (butlast (split-string wire "\r\n")))
      (websocket--http-line http line websocket--default-limits))
    http))

(ert-deftest websocket-opening-boundary ()
  (let* ((key "dGhlIHNhbXBsZSBub25jZQ==")
         (address (websocket--url "ws://[::1]:8080?x=%20"))
         (request (websocket--client-request address key '("chat") '(("Origin" . "null"))))
         (http (websocket-test--http request)))
    (should (equal (plist-get address :target) "/?x=%20"))
    (should (equal (plist-get (websocket--server-request http) :origin) "null"))
    (push '("host" . "other") (websocket--http-fields http))
    (should-error (websocket--server-request http) :type 'websocket-protocol-error)
    (dolist (extra '("" "Sec-WebSocket-Protocol: chat\r\n"))
      (should (member
               (websocket--client-response
                (websocket-test--http
                 (concat "HTTP/1.1 101 Switching Protocols\r\nConnection: keep-alive, Upgrade\r\n"
                         "Upgrade: WebSocket\r\nSec-WebSocket-Accept: " (websocket--accept key)
                         "\r\n" extra "\r\n")) key '("chat")) '(nil "chat"))))
    (dolist (extra '("Sec-WebSocket-Accept: bad\r\n" "Sec-WebSocket-Protocol: other\r\n"
                     "Sec-WebSocket-Extensions: compress\r\n"))
      (should-error
       (websocket--client-response
        (websocket-test--http
         (concat "HTTP/1.1 101 Switching Protocols\r\nConnection: Upgrade\r\nUpgrade: websocket\r\n"
                 "Sec-WebSocket-Accept: " (websocket--accept key) "\r\n" extra "\r\n")) key '("chat"))
       :type 'websocket-protocol-error)))
  (dolist (url '("http://host/" "ws://host/#fragment" "ws://user:secret@host/"
                 "ws://host:0/" "ws://host/a b" "ws://host/%2" "ws://host/%gg"))
    (should-error (websocket--url url)))
  (dolist (line '(" Host: example" "Host : example" "Host: x\rInjected: yes"))
    (should-error (websocket-test--http (concat "GET / HTTP/1.1\r\n" line "\r\n\r\n"))
                  :type 'websocket-protocol-error)))

;;; websocket-test.el ends here
