;;; ende-render-test.el --- General renderer contracts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter Samarin
;; SPDX-License-Identifier: AGPL-3.0-only

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ende-render)

(ert-deftest ende-render-complete-json-and-readable-values ()
  (let ((reply "{\"title\":\"One\\nline\",\"text\":\"Body\"}"))
    (should (equal (ende-render-reply reply)
                   '(:summary "One line" :body "Body" :state neutral :folded nil)))
    (dolist (text (list (concat reply " trailing") (concat reply " {}")
                       (concat "```json\n" reply "\n```")
                       "{\"title\":\"Incomplete\"}" "{\"title\":1,\"text\":\"x\"}"))
      (should-not (ende-render-reply text))))
  (should (equal (ende-render-readable "{\"a\":null,\"b\":false,\"c\":0,\"d\":\"\",\"e\":[]}")
                 "A: null\nB: false\nC: 0\nD: \"\"\nE: []"))
  (should (equal (ende-render-readable "{} trailing") "{} trailing")))

(ert-deftest ende-render-decodes-once-and-preserves-input ()
  (let ((parse (symbol-function 'ende-protocol-json-read-string)) (calls 0))
    (cl-letf (((symbol-function 'ende-protocol-json-read-string)
               (lambda (text) (cl-incf calls) (funcall parse text))))
      (let ((presentation (ende-render-tool-response
                           "future_tool" "{\"ok\":true,\"result\":{\"text\":\"{}\"}}")))
        (should (= calls 1))
        (should (stringp (plist-get presentation :summary)))
        (should (equal (plist-get presentation :body) "Text: {}"))
        (should (plist-get presentation :folded)))))
  (let* ((source (copy-sequence "new\nline\n"))
         (args (list :path "x" :content source))
         (snapshot (copy-tree args))
         (body (plist-get (ende-render-tool-request "create_file" args) :body)))
    (should (eq (get-text-property 0 'ende-render-face body) 'ende-render-success-face))
    (should (equal args snapshot))
    (should-not (text-properties-at 0 source))
    (aset body 0 ?!)
    (should (equal source "new\nline\n"))))

(ert-deftest ende-render-failures-and-fallback-remain-presentations ()
  (dolist (content '("{\"ok\":false,\"error\":\"denied\"}"
                     "{\"ok\":true,\"result\":{\"process_ok\":false,\"exit_code\":9,\"stdout\":\"\",\"stderr\":\"failed\"}}"))
    (let ((presentation (ende-render-tool-response "run_host_command" content)))
      (should (eq (plist-get presentation :state) 'error))
      (should (plist-get presentation :folded))))
  (cl-letf (((symbol-function 'ende-render--request) (lambda (&rest _) (error "formatter failed"))))
    (let ((presentation (ende-render-tool-request "future_tool" "{\"message\":\"kept\"}")))
      (should (equal (plist-get presentation :body) "Message: kept"))
      (should (eq (plist-get presentation :state) 'neutral))))
  (let ((presentation (ende-render-tool-response "spawn_agent" "{\"ok\":true,\"result\":\"Agent failed\"}")))
    (should (eq (plist-get presentation :state) 'neutral))
    (should (equal (plist-get presentation :body) "Agent failed"))))

(provide 'ende-render-test)
;;; ende-render-test.el ends here
