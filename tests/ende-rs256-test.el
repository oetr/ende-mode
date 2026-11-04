;;; ende-rs256-test.el --- RS256 trust boundary tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter Samarin
;;
;; Author: Peter Samarin <peter.samarin@gmail.com>
;; Maintainer: Peter Samarin <peter.samarin@gmail.com>
;; Keywords: lisp, tools
;; URL: https://github.com/oetr/ende-mode
;; SPDX-License-Identifier: AGPL-3.0-only

;; This file is part of Ende.

;; Ende is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, version 3 of the License.
;;
;; Ende is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with Ende.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for the RS256 trust boundary.

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

(defun ende-rs256-test--refresh-claims (record token jwks)
  "Return the claims that verify TOKEN against JWKS for RECORD, or nil."
  (let (claims)
    (cl-letf (((symbol-function 'ende-codex-auth--jwks-async)
               (lambda (callback) (funcall callback jwks nil) #'ignore)))
      (ende-codex-auth--verify-refresh
       token record
       (lambda (value failure) (setq claims (and value (not failure) value)))))
    claims))

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
             (claims (ende-codex--verify-id-token 'chatgpt-plan-oauth valid "test-client" "test-nonce")))
        (should (equal (gethash "sub" claims) "test-subject"))
        (let* ((directory (make-temp-file "ende-verified-refresh-" t))
               (ende-codex-auth--directory (expand-file-name "private" directory))
               (parts (split-string valid "\\."))
               (retired (concat (base64url-encode-string
                                 "{\"alg\":\"RS256\",\"kid\":\"retired-key\"}" t)
                                "." (cadr parts) "." (nth 2 parts))))
          (unwind-protect
              (let ((record (ende-codex-auth--register 'chatgpt-plan-oauth
                             'verified (list :client-id "test-client" :subject "test-subject"
                                             :id-token retired :access-token "access"
                                             :refresh-token "refresh" :expires-at 1800000000))))
                ;; Signing-key rotation must not invalidate the stored identity:
                ;; only the replacement token is verified, against current keys.
                (should-error (ende-codex-auth--signed-claims retired (lambda () jwks)))
                (should (ende-rs256-test--refresh-claims record valid jwks))
                (should-not (ende-rs256-test--refresh-claims
                             (plist-put (copy-sequence record) :subject "other-subject")
                             valid jwks))
                (should-not (ende-rs256-test--refresh-claims
                             record (alist-get 'nonce ende-rs256-test--fixtures) jwks))
                (let ((next (ende-codex--store-refresh
                             record "next-access" "next-refresh" 3600 valid)))
                  (should (ende-codex-auth--same-registration-p record next))
                  (should (equal (plist-get next :subject) "test-subject"))
                  (should (equal next (ende-codex-auth-read 'chatgpt-plan-oauth 'verified)))))
            (delete-directory directory t)))
        (dolist (scope '("openid offline_access" "offline_access openid profile"))
          (should (equal (ende-codex-auth--scopes scope)
                         (split-string scope " " nil))))
        (dolist (scope '("" "openid  offline_access" "openid openid" nil 42
                         "openid\noffline_access"))
          (should-error (ende-codex-auth--scopes scope)))
        (dolist (kind '(issuer audience nonce expired future issued subject azp))
          (should-error (ende-codex--verify-id-token 'chatgpt-plan-oauth
                         (alist-get kind ende-rs256-test--fixtures) "test-client" "test-nonce")
                        :type 'ende-codex-identity-error))
        (let ((before parsed-claims))
          (should-error (ende-codex--verify-id-token 'chatgpt-plan-oauth
                         (concat (substring valid 0 -2) "AA") "test-client" "test-nonce")
                        :type 'ende-codex-identity-error)
          (should (= parsed-claims before)))
        (dolist (token '("" "e30.e30" "e30..AA" "e30.e30.AA.extra"))
          (should-error (ende-codex--verify-id-token 'chatgpt-plan-oauth token "test-client" "test-nonce")))
        (dolist (header '("{\"alg\":\"none\",\"kid\":\"test-key\"}"
                          "{\"alg\":\"HS256\",\"kid\":\"test-key\"}"
                          "{\"alg\":\"RS256\",\"kid\":\"test-key\",\"kid\":\"other\"}"
                          "{\"alg\":\"RS256\",\"kid\":\"test-key\",\"crit\":[\"extension\"]}"
                          "{\"alg\":\"RS256\",\"kid\":\"test-key\",\"b64\":false}"
                          "{\"alg\":\"RS256\",\"kid\":\"test-key\",\"jku\":\"https://attacker.test\"}"))
          (cl-letf (((symbol-function 'ende-codex-auth--jwks)
                     (lambda () (ert-fail "Unsupported header reached network"))))
            (should-error (ende-codex--verify-id-token 'chatgpt-plan-oauth
                           (concat (base64url-encode-string header t) ".e30.AA")
                           "test-client" "test-nonce"))))
        (cl-letf (((symbol-function 'ende-codex-auth--jwks)
                   (lambda () (error "Unavailable"))))
          (should-error (ende-codex--verify-id-token 'chatgpt-plan-oauth valid "test-client" "test-nonce")
                        :type 'ende-codex-identity-error))))))

(provide 'ende-rs256-test)
;;; ende-rs256-test.el ends here
