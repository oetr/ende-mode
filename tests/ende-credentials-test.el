;;; ende-credentials-test.el --- Exact credentials -*- lexical-binding: t; -*-

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

;; Tests for exact credential handling.

;;; Code:
(require 'ert)
(require 'ende-curl)

(ert-deftest ende-credentials-native-binding-and-ambiguity ()
  (let* ((file (make-temp-file "ende-authinfo-"))
         (auth-sources (list file))
         (profile '(:url "https://example.test/v1" :auth-method auth-source
                    :auth-source-host "example.test" :auth-source-port 443
                    :auth-source-user "work" :auth-scheme x-api-key)))
    (unwind-protect
        (progn
          (set-file-modes file #o600)
          (with-temp-file file (insert "machine example.test port 443 login work password test-key\n"))
          (should (equal (ende-curl--auth-secret profile) "test-key"))
          (let ((snapshot (ende-credentials-capture profile)))
            (should-not (string-match-p "test-key" (prin1-to-string snapshot)))
            (should (equal (ende-curl--auth-secret snapshot) "test-key"))
            (with-temp-file file (insert "machine example.test port 443 login work password replacement\n"))
            (should-error (ende-curl--auth-secret snapshot)))
          (with-temp-file file (insert "machine example.test port 443 login work password test-key\n"))
          (let ((config (ende-curl--config profile)))
            (unwind-protect
                (with-temp-buffer
                  (insert-file-contents config)
                  (should (string-match-p "x-api-key: test-key" (buffer-string))))
              (delete-file config)))
          (with-temp-file file
            (insert "machine example.test port 443 login work password first\n"
                    "machine example.test port 0443 login work password second\n"))
          (should-error (ende-curl--auth-secret profile))
          (with-temp-file file (insert "machine example.test login work password wildcard\n"))
          (should-error (ende-curl--auth-secret profile))
          (should-error (ende-curl--auth-secret (plist-put (copy-sequence profile) :auth-source-user "other")))
          (should-error (ende-curl--validated-url (plist-put (copy-sequence profile) :url "http://localhost/v1"))))
      (delete-file file))))

(ert-deftest ende-credentials-skips-ignored-sources ()
  (let* ((file (make-temp-file "ende-authinfo-"))
         (auth-sources (list (concat file ".missing") file))
         (profile '(:auth-method auth-source :auth-source-host "example.test"
                    :auth-source-port 443 :auth-source-user "work")))
    (unwind-protect
        (progn
          (set-file-modes file #o600)
          (with-temp-file file
            (insert "machine example.test port 443 login work password test-key\n"))
          (should (equal (ende-credentials-secret profile) "test-key")))
      (delete-file file))))

(ert-deftest ende-credentials-verifies-exact-stored-attributes ()
  (let* ((backend (make-instance 'auth-source-backend
                                 :type 'netrc :source "private source"
                                 :host t :user t :port t
                                 :search-function 'auth-source-netrc-search))
         (entry '(:host "example.test" :port 443 :user "work" :secret "test-key")))
    (cl-letf (((symbol-function 'ende-credentials--discover-backends)
               (lambda () (list backend)))
              ((symbol-function 'auth-source-search-backends)
               (lambda (&rest _) (list entry))))
      (should (= 1 (length (ende-credentials--candidates "example.test" 443 "work"))))
      (should-not (ende-credentials--candidates "example.test" 444 "work")))))

(ert-deftest ende-credentials-keeps-secret-service-unverifiable ()
  (let ((backend (make-instance 'auth-source-backend
                                :type 'secrets :source "Login"
                                :host t :user t :port t
                                :search-function 'auth-source-secrets-search)))
    (should-not (ende-credentials--adapter backend))))

(ert-deftest ende-credentials-keeps-pass-unverifiable ()
  (let ((backend (make-instance 'auth-source-backend
                                :type 'password-store :source "."
                                :host t :user t :port t
                                :search-function 'auth-source-pass-search)))
    (should-not (ende-credentials--adapter backend))))

(ert-deftest ende-credentials-rejects-unverifiable-and-duplicate-results ()
  (let* ((backend (make-instance 'auth-source-backend
                                 :type 'netrc :source "test"
                                 :host t :user t :port t
                                 :search-function 'auth-source-netrc-search))
         (entry '(:host "example.test" :port 443 :user "work" :secret "test-key")))
    (cl-letf (((symbol-function 'ende-credentials--discover-backends)
               (lambda () (list backend)))
              ((symbol-function 'auth-source-search-backends)
               (lambda (&rest _) (list entry entry))))
      (should-error (ende-credentials--resolve
                     '(:auth-source-host "example.test" :auth-source-port 443
                       :auth-source-user "work"))))
    (cl-letf (((symbol-function 'ende-credentials--discover-backends)
               (lambda () (list backend)))
              ((symbol-function 'auth-source-search-backends)
               (lambda (&rest _) (list '(:host t :port 443 :user "work" :secret "test-key")))))
      (should-error (ende-credentials--candidates "example.test" 443 "work")))
    (cl-letf (((symbol-function 'ende-credentials--discover-backends)
               (lambda () (list backend)))
              ((symbol-function 'auth-source-search-backends)
               (lambda (&rest _) (list '(:host "example.test" :user "work" :secret "test-key")))))
      (should-error (ende-credentials--candidates "example.test" 443 "work")))))

(ert-deftest ende-credentials-names-unsupported-backend-safely ()
  (let ((backend (make-instance 'auth-source-backend
                                :type 'password-store :source "secret backend record"
                                :host t :user t :port t
                                :search-function 'ignore)))
    (cl-letf (((symbol-function 'ende-credentials--discover-backends)
               (lambda () (list backend))))
      (let ((message (condition-case err
                         (progn (ende-credentials--candidates "example.test" 443 "work") "")
                       (error (error-message-string err)))))
        (should (string-match-p "password-store" message))
        (should-not (string-match-p "secret backend record" message))))))

(ert-deftest ende-credentials-captured-binding-rejects-replacement ()
  (let* ((file (make-temp-file "ende-authinfo-"))
         (auth-sources (list file))
         (profile '(:auth-method auth-source :auth-source-host "example.test"
                    :auth-source-port 443 :auth-source-user "work")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "machine example.test port 443 login work password first\n"))
          (let ((snapshot (ende-credentials-capture profile)))
            (with-temp-file file
              (insert "machine example.test port 443 login work password replacement\n"))
            (should-error (ende-credentials-secret snapshot))))
      (delete-file file))))

(provide 'ende-credentials-test)
;;; ende-credentials-test.el ends here
