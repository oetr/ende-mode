;;; ende-rs256-test.el --- RS256 trust boundary tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'json)
(require 'ende-rs256)
(require 'ende-rs256-fixtures)
(require 'ende-codex-auth)
(require 'ende-codex)

(defun ende-rs256-test--jwks ()
  "Return a detached public key set for the OpenSSL fixtures."
  (json-parse-string
   (json-encode `(("keys" . [,(alist-get 'jwk ende-rs256-test--fixtures)])))))

(ert-deftest ende-rs256-signature-and-key-boundary ()
  (let* ((jwks (ende-rs256-test--jwks))
         (key (aref (gethash "keys" jwks) 0))
         (parts (split-string (alist-get 'valid ende-rs256-test--fixtures) "\\."))
         (input (concat (car parts) "." (cadr parts)))
         (signature (nth 2 parts)))
    (should (ende-rs256-verify input signature jwks "test-key"))
    (dolist (position (list 0 (1+ (length (car parts)))))
      (let ((tampered (copy-sequence input)))
        (aset tampered position (logxor 1 (aref tampered position)))
        (should-error (ende-rs256-verify tampered signature jwks "test-key"))))
    (dolist (bad (list (concat "!" signature) (concat signature "=")
                      (substring signature 1) (gethash "n" key)
                      (concat (if (= (aref signature 0) ?A) "B" "A")
                              (substring signature 1))
                      (alist-get 'bad-padding ende-rs256-test--fixtures)
                      (alist-get 'bad-digest-info ende-rs256-test--fixtures)))
      (should-error (ende-rs256-verify input bad jwks "test-key")))
    (dolist (change
             `(("kty" . "EC") ("use" . "enc") ("alg" . "RS512")
               ("e" . "Aw") ("e" . "AA") ("e" . "Ag") ("e" . "AAEAAQ")
               ("n" . "!") ("n" . "AA") ("n" . ,(concat (gethash "n" key) "="))
               ("n" . ,(base64url-encode-string
                         (concat (unibyte-string 0)
                                 (base64-decode-string (gethash "n" key) t)) t))
               ("n" . ,(base64url-encode-string (fillarray (make-string 128 0) 255) t))
               ("n" . ,(base64url-encode-string (fillarray (make-string 1025 0) 255) t))
               ("n" . ,(base64url-encode-string
                         (concat (unibyte-string 127)
                                 (fillarray (make-string 255 0) 255)) t))
               ("n" . ,(base64url-encode-string
                         (concat (fillarray (make-string 255 0) 255)
                                 (unibyte-string 254)) t))))
      (let ((changed (copy-hash-table key)))
        (puthash (car change) (cdr change) changed)
        (puthash "keys" (vector changed) jwks)
        (should-error (ende-rs256-verify input signature jwks "test-key"))))
    (puthash "keys" (vector key key) jwks)
    (should-error (ende-rs256-verify input signature jwks "test-key"))
    (puthash "keys" (vector key) jwks)
    (should-error (ende-rs256-verify input signature jwks "missing-key"))))

(ert-deftest ende-rs256-authenticated-identity ()
  (let ((jwks (ende-rs256-test--jwks)) (parsed-claims 0)
        (read-json (symbol-function 'ende-codex-auth--json)))
    (cl-letf (((symbol-function 'float-time) (lambda (&rest _) 1800000100.0))
              ((symbol-function 'ende-codex-auth--jwks) (lambda () jwks))
              ((symbol-function 'ende-codex-auth--json)
               (lambda (bytes)
                 (when (string-match-p "test-subject" bytes) (cl-incf parsed-claims))
                 (funcall read-json bytes))))
      (let* ((valid (alist-get 'valid ende-rs256-test--fixtures))
             (claims (ende-codex--verify-id-token valid "test-client" "test-nonce")))
        (should (equal (gethash "sub" claims) "test-subject"))
        (let* ((directory (make-temp-file "ende-verified-refresh-" t))
               (ende-codex-auth--directory (expand-file-name "private" directory)))
          (unwind-protect
              (let* ((record (ende-codex-auth--register
                              'verified (list :client-id "test-client" :subject "test-subject"
                                              :id-token valid :access-token "access"
                                              :refresh-token "refresh" :expires-at 1800000000)))
                     (next (ende-codex--store-refresh record "next-access" "next-refresh"
                                                       3600 valid)))
                (should (ende-codex-auth--same-registration-p record next))
                (should (equal (plist-get next :subject) "test-subject"))
                (should-error (ende-codex--store-refresh
                               next "bad-access" "bad-refresh" 3600
                               (alist-get 'nonce ende-rs256-test--fixtures)))
                (should (equal next (ende-codex-auth-read 'verified)))
                (let ((other (plist-put (copy-sequence next) :subject "other-subject")))
                  (should-error (ende-codex--store-refresh other "access" "refresh" 3600 valid))))
            (delete-directory directory t)))
        (dolist (scope '("openid offline_access" "offline_access openid profile"))
          (should-not (ende-codex-auth--refresh-scope
                       (json-parse-string (json-encode `(("scope" . ,scope)))))))
        (dolist (scope '("profile" "openid" "OPENID offline_access" nil 42 "openid\noffline_access"))
          (should-error (ende-codex-auth--refresh-scope
                         (json-parse-string (json-encode `(("scope" . ,scope)))))))
        (dolist (kind '(issuer audience nonce expired future issued subject azp))
          (should-error (ende-codex--verify-id-token
                         (alist-get kind ende-rs256-test--fixtures) "test-client" "test-nonce")
                        :type 'ende-codex-identity-error))
        (let ((before parsed-claims))
          (should-error (ende-codex--verify-id-token
                         (concat (substring valid 0 -2) "AA") "test-client" "test-nonce")
                        :type 'ende-codex-identity-error)
          (should (= parsed-claims before)))
        (dolist (token '("" "e30.e30" "e30..AA" "e30.e30.AA.extra"))
          (should-error (ende-codex--verify-id-token token "test-client" "test-nonce")))
        (dolist (header '("{\"alg\":\"none\",\"kid\":\"test-key\"}"
                          "{\"alg\":\"HS256\",\"kid\":\"test-key\"}"
                          "{\"alg\":\"RS256\",\"kid\":\"test-key\",\"kid\":\"other\"}"
                          "{\"alg\":\"RS256\",\"kid\":\"test-key\",\"crit\":[\"extension\"]}"
                          "{\"alg\":\"RS256\",\"kid\":\"test-key\",\"b64\":false}"
                          "{\"alg\":\"RS256\",\"kid\":\"test-key\",\"jku\":\"https://attacker.test\"}"))
          (cl-letf (((symbol-function 'ende-codex-auth--jwks)
                     (lambda () (ert-fail "Unsupported header reached network"))))
            (should-error (ende-codex--verify-id-token
                           (concat (base64url-encode-string header t) ".e30.AA")
                           "test-client" "test-nonce"))))
        (cl-letf (((symbol-function 'ende-codex-auth--jwks)
                   (lambda () (error "Unavailable"))))
          (should-error (ende-codex--verify-id-token valid "test-client" "test-nonce")
                        :type 'ende-codex-identity-error))))))

(ert-deftest ende-rs256-trusted-key-retrieval ()
  (let ((start (symbol-function 'ende-curl-start-request))
        (jwks (ende-rs256-test--jwks)))
    (dolist (response (list (list 200 (json-encode jwks) t)
                            '(302 "{}" nil) '(200 "{malformed" nil)
                            '(200 "{\"keys\":[]}" nil)
                            (list 200 (make-string 65537 ?x) nil)))
      (let (resources command)
        (cl-letf (((symbol-function 'ende-curl-start-request)
                   (lambda (&rest arguments)
                     (let* ((builder (plist-get arguments :command))
                            (headers (make-temp-file "ende-jwks-test-")))
                       (unwind-protect (setq command (funcall builder headers nil))
                         (delete-file headers)))
                     (setq arguments (plist-put arguments :command
                                                 (lambda (&rest _) '("cat"))))
                     (setq resources (apply start arguments))
                     (with-temp-file (plist-get resources :headers-file)
                       (insert (format "HTTP/1.1 %d Test\r\n\r\n" (car response))))
                     (process-send-string (plist-get resources :process) (cadr response))
                     (process-send-eof (plist-get resources :process))
                     resources)))
          (if (nth 2 response)
              (should (= (length (gethash "keys" (ende-codex-auth--jwks))) 1))
            (should-error (ende-codex-auth--jwks)))
          (should (equal (cadr (member "--url" command)) ende-codex-auth--jwks-uri))
          (should (equal (cadr (member "--max-redirs" command)) "0"))
          (should (member "--disable" command))
          (should (member "--max-time" command))
          (should-not (member "--insecure" command))
          (should-not (member "--location" command))
          (should-not (process-live-p (plist-get resources :process)))
          (should-not (buffer-live-p (plist-get resources :response-buffer)))
          (should-not (file-exists-p (plist-get resources :headers-file))))))))

(provide 'ende-rs256-test)
;;; ende-rs256-test.el ends here
