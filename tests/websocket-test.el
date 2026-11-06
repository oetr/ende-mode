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

;;; websocket-test.el ends here
