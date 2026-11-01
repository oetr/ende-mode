;;; ende-rs256.el --- Native RS256 verification -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)

(define-error 'ende-rs256-error "Invalid RS256 signature or public key")

(defun ende-rs256--fail ()
  "Reject invalid input without including its contents."
  (signal 'ende-rs256-error nil))

(defun ende-rs256--decode (text)
  "Decode canonical, unpadded base64url TEXT into unibyte data."
  (unless (and (stringp text)
               (let ((case-fold-search nil))
                 (string-match-p "\\`[A-Za-z0-9_-]+\\'" text))
               (/= (mod (length text) 4) 1))
    (ende-rs256--fail))
  (let ((bytes (base64-decode-string text t)))
    (unless (equal text (base64url-encode-string bytes t))
      (ende-rs256--fail))
    bytes))

(defun ende-rs256--integer (bytes)
  "Return the unsigned integer represented by big-endian BYTES."
  (let ((value 0))
    (dotimes (index (length bytes))
      (setq value (+ (ash value 8) (aref bytes index))))
    value))

(defun ende-rs256--bytes (value size)
  "Encode nonnegative integer VALUE as exactly SIZE big-endian bytes."
  (unless (and (integerp value) (>= value 0)) (ende-rs256--fail))
  (let ((bytes (make-string size 0)))
    (dotimes (index size)
      (aset bytes (- size index 1) (logand value 255))
      (setq value (ash value -8)))
    (unless (= value 0) (ende-rs256--fail))
    bytes))

(defun ende-rs256--modexp (base exponent modulus)
  "Return BASE raised to EXPONENT modulo MODULUS."
  (let ((result 1))
    (while (> exponent 0)
      (when (= (logand exponent 1) 1)
        (setq result (mod (* result base) modulus)))
      (setq base (mod (* base base) modulus)
            exponent (ash exponent -1)))
    result))

(defun ende-rs256--kid-p (value)
  "Return non-nil for a bounded, nonblank key identifier VALUE."
  (and (stringp value) (<= 1 (length value) 1024)
       (string-match-p "[^[:space:]]" value)
       (not (string-match-p "[[:cntrl:]]" value))))

(defun ende-rs256--public-key (jwk)
  "Validate RSA JWK and return (MODULUS . BYTE-LENGTH)."
  (unless (and (hash-table-p jwk)
               (equal (gethash "kty" jwk) "RSA")
               (equal (gethash "use" jwk "sig") "sig")
               (equal (gethash "alg" jwk "RS256") "RS256")
               (ende-rs256--kid-p (gethash "kid" jwk))
               (stringp (gethash "n" jwk))
               (<= (length (gethash "n" jwk)) 1366)
               (equal (gethash "e" jwk) "AQAB"))
    (ende-rs256--fail))
  (let* ((bytes (ende-rs256--decode (gethash "n" jwk)))
         (size (length bytes))
         (modulus (ende-rs256--integer bytes)))
    (unless (and (<= 256 size 1024) (/= (aref bytes 0) 0)
                 (>= modulus (ash 1 2047))
                 (= (logand modulus 1) 1))
      (ende-rs256--fail))
    (cons modulus size)))

(defun ende-rs256--key (jwks kid)
  "Select exactly one public key from trusted JWKS matching KID."
  (let ((keys (and (hash-table-p jwks) (gethash "keys" jwks))) matches)
    (unless (and (ende-rs256--kid-p kid) (vectorp keys)
                 (<= 1 (length keys) 16))
      (ende-rs256--fail))
    (mapc (lambda (key)
            (unless (and (hash-table-p key)
                         (ende-rs256--kid-p (gethash "kid" key)))
              (ende-rs256--fail))
            (when (equal (gethash "kid" key) kid) (push key matches)))
          keys)
    (unless (= (length matches) 1) (ende-rs256--fail))
    (ende-rs256--public-key (car matches))))

(defun ende-rs256-verify (input signature jwks kid)
  "Verify RS256 INPUT and encoded SIGNATURE using KID in trusted JWKS.
Return t on an exact PKCS#1 v1.5 match; otherwise signal an error."
  (pcase-let* ((`(,modulus . ,size) (ende-rs256--key jwks kid))
               (_ (unless (and (stringp input)
                               (not (string-match-p "[^[:ascii:]]" input))
                               (stringp signature)
                               (<= (length signature) 1366))
                    (ende-rs256--fail)))
               (bytes (ende-rs256--decode signature))
               (_ (unless (= (length bytes) size) (ende-rs256--fail)))
               (value (ende-rs256--integer bytes)))
    (unless (< value modulus) (ende-rs256--fail))
    (let* ((digest-info
            (concat (unibyte-string #x30 #x31 #x30 #x0d #x06 #x09 #x60 #x86
                                    #x48 #x01 #x65 #x03 #x04 #x02 #x01 #x05
                                    #x00 #x04 #x20)
                    (secure-hash 'sha256 input nil nil t)))
           (padding (- size (length digest-info) 3)))
      (unless (and (>= padding 8)
                   (string= (ende-rs256--bytes
                             (ende-rs256--modexp value 65537 modulus) size)
                            (concat (unibyte-string 0 1)
                                    (fillarray (make-string padding 0) 255)
                                    (unibyte-string 0) digest-info)))
        (ende-rs256--fail))
      t)))

(provide 'ende-rs256)
;;; ende-rs256.el ends here
