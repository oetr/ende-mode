;;; ende-rs256-test.el --- RS256 trust boundary tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'json)
(require 'ende-rs256)
(require 'ende-rs256-fixtures)

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

(provide 'ende-rs256-test)
;;; ende-rs256-test.el ends here
