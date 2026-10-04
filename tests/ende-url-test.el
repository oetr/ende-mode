;;; ende-url-test.el --- URL policy tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Pure normalization and URL-resource matching guarantees.

;;; Code:

(require 'ert)
(require 'ende-url)

(ert-deftest ende-test/url-policy-matches-host-scheme-port-and-path-separately ()
  (dolist (case
           '(("example.com" "https://example.com/a/b" t)
             ("example.com" "https://www.example.com/a" nil)
             ("example.com" "http://example.com/a" nil)
             ("*.example.com" "https://example.com/a" nil)
             ("*.example.com" "https://a.example.com/a" t)
             ("*.example.com" "https://a.b.example.com/a" t)
             ("*.example.com" "https://evil-example.com/a" nil)
             ("https://example.com:8443/a/*" "https://example.com:8443/a/x" t)
             ("https://example.com:8443/a/*" "https://example.com:8443/a/x/y" nil)
             ("https://example.com/a/**" "https://example.com/a/x/y" t)
             ("https://example.com/a/**" "https://example.com/A/x" nil)
             ("*" "https://service.invalid/" t)
             ("*" "https://127.0.0.1/" nil)
             ("https://127.0.0.1/**" "https://127.0.0.1/a" t)
             ("https://[::ffff:192.0.2.1]/**"
              "https://[::ffff:c000:201]/a" t)
             ("https://[2001:db8::1]/**" "https://[2001:0db8:0:0:0:0:0:1]/a" t)))
    (ert-info ((format "%s against %s" (car case) (cadr case)))
      (should (eq (not (null (ende-url-pattern-allows-p
                              (ende-url-pattern-compile (car case))
                              (cadr case))))
                  (nth 2 case))))))

(ert-deftest ende-test/url-policy-normalizes-the-authorized-request-url ()
  (let ((normalized (ende-url-normalize
                     "HTTPS://EXAMPLE.COM.:443/a/./b/../%7euser?q=one#fragment")))
    (should (equal (plist-get normalized :url)
                   "https://example.com/a/~user?q=one"))
    (should (equal (plist-get normalized :path) "/a/~user"))
    (should (equal (plist-get normalized :query) "q=one"))
    (should (equal (plist-get normalized :fragment) "fragment"))
    (should (ende-url-pattern-allows-p
             (ende-url-pattern-compile "https://example.com/a/~user")
             normalized)))
  (should (equal (plist-get (ende-url-normalize "http://example.com:80/") :url)
                 "http://example.com/"))
  (should (equal (plist-get (ende-url-normalize "https://example.com/a?q=x#f") :url)
                 "https://example.com/a?q=x"))
  (should (equal (plist-get
                  (ende-url-normalize "https://example.com/café space?q=%25")
                  :url)
                 "https://example.com/caf%C3%A9%20space?q=%25"))
  (should (ende-url-pattern-allows-p
           (ende-url-pattern-compile "https://example.com/café space")
           "https://example.com/caf%C3%A9%20space")))

(ert-deftest ende-test/url-policy-ignores-query-for-path-permission ()
  (let ((pattern (ende-url-pattern-compile "https://example.com/docs/index.html")))
    (should (ende-url-pattern-allows-p
             pattern "https://example.com/docs/index.html?token=private"))
    (should-not (ende-url-pattern-allows-p
                 pattern "https://example.com/docs/other.html?next=index.html"))))

(ert-deftest ende-test/url-policy-rejects-ambiguous-and-unsupported-inputs ()
  (dolist (url '("//example.com/path"
                 "https://user@example.com/"
                 "https://example.com/%2fsecret"
                 "https://example.com/%252e%252e/secret"
                 "https://example.com/%zz"
                 "https://example.com\\@evil.invalid/"
                 "https://127.1/"
                 "https://0x7f000001/"
                 "https://[fe80::1%eth0]/"
                 "https://☃.example/"))
    (ert-info ((format "Rejected request URL %S" url))
      (should-error (ende-url-normalize url))))
  (dolist (pattern '("example.com/path"
                    "example.com:443"
                    "https://example.com/path?q=1"
                    "https://example.com/path#fragment"
                    "https://*.127.0.0.1/**"
                    "127.0.0.1"))
    (ert-info ((format "Rejected URL policy pattern %S" pattern))
      (should-error (ende-url-pattern-compile pattern)))))

(provide 'ende-url-test)

;;; ende-url-test.el ends here
