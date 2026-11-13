;;; ende-render-test.el --- General renderer contracts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter Samarin
;; SPDX-License-Identifier: AGPL-3.0-only

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ende-render)

(ert-deftest ende-render-recognition-and-purity ()
  (let* ((raw "{\"title\":\"Result\",\"text\":\"real\\nnewline; literal \\\\n\"}")
         (before (copy-sequence raw))
         (reply (ende-render-reply raw)))
    (should (equal (plist-get reply :body) "real\nnewline; literal \\n"))
    (should (equal-including-properties raw before))
    (dolist (content (list "prose" "{\"title\":" (concat raw " trailing")
                          "{\"title\":\"T\",\"text\":\"B\",\"extra\":false}"))
      (should-not (ende-render-reply content))))
  (let* ((text (copy-sequence "new\nline"))
         (arguments (list :path "a.el" :content text))
         (snapshot (copy-tree arguments))
         (body (plist-get (ende-render-tool-request "create_file" arguments) :body)))
    (should (get-text-property 0 'ende-render-face body))
    (should-not (text-properties-at 0 text))
    (aset body 0 ?!)
    (should (equal arguments snapshot)))
  (let ((parser (symbol-function 'ende-protocol-json-read-string)) (calls 0))
    (cl-letf (((symbol-function 'ende-protocol-json-read-string)
               (lambda (text) (cl-incf calls) (funcall parser text))))
      (let ((body (plist-get (ende-render-tool-response
                             "unknown" "{\"ok\":true,\"result\":\"{}\"}") :body)))
        (should (equal body "{}"))
        (should (= calls 1))))))

(ert-deftest ende-render-outcomes-and-fallbacks ()
  (let* ((presentation (ende-render-tool-response
                        "run_host_command"
                        "{\"ok\":true,\"result\":{\"process_ok\":false,\"exit_code\":9,\"stderr\":\"diagnostic\"}}"))
         (summary (plist-get presentation :summary)))
    (should (eq (plist-get presentation :state) 'error))
    (should (string-search "failed" summary))
    (should (string-search "diagnostic" (plist-get presentation :body))))
  (dolist (case '(((:value nil) "null") ((:value :false) "false")
                 ((:value 0) "0") ((:value "") "\"\"") ((:value []) "[]")))
    (should (string-search (cadr case) (ende-render-readable (car case)))))
  (dolist (case '(((:unexpected "kept") neutral)
                 ((:ok t :result (:unexpected "kept")) success)
                 ((:ok :false :error "kept") error)))
    (let ((presentation (ende-render-tool-response "unknown" (car case))))
      (should (eq (plist-get presentation :state) (cadr case)))
      (should (string-search "kept" (plist-get presentation :body)))))
  (cl-letf (((symbol-function 'ende-render--request)
             (lambda (&rest _) (error "formatter failed"))))
    (let ((presentation (ende-render-tool-request "unknown" "{\"message\":\"kept\"}")))
      (should (string-search "kept" (plist-get presentation :body)))
      (should (eq (plist-get presentation :state) 'neutral))))
  (should (equal (ende-render-readable "{incomplete") "{incomplete")))

(provide 'ende-render-test)
;;; ende-render-test.el ends here
