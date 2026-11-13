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
                       "{\"title\":\"Incomplete\"}" "{\"title\":1,\"text\":\"x\"}"
                       "{\"title\":\"Title\",\"text\":\"Body\",\"extra\":false}"))
      (should-not (ende-render-reply text))))
  (should (equal (ende-render-readable "{\"a\":null,\"b\":false,\"c\":0,\"d\":\"\",\"e\":[]}")
                 "A: null\nB: false\nC: 0\nD: \"\"\nE: []"))
  (should (equal (ende-render-readable "{} trailing") "{} trailing")))

(ert-deftest ende-render-decodes-once-and-preserves-core-content ()
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
    (should (equal source "new\nline\n")))
  ;; Folded requests retain their core arguments, without repeating the tool
  ;; name supplied by the host.  Check facts, not entire tool snapshots.
  (dolist (case '(("search_project" (:query "needle" :glob "*.el")
                  ("query" "needle" "*.el"))
                 ("search_project" (:patterns ["first" "second"])
                  ("patterns" "first" "second"))
                 ("search_project" (:regexp "a+b") ("regexp" "a+b"))
                 ("read_file" (:path "a.el" :start_line 8 :line_count 3)
                  ("a.el" "8–10"))
                 ("read_buffer" (:buffer_id "buf-1" :start_line 8 :line_count 3)
                  ("buf-1" "8–10"))
                 ("generate_image" (:prompt "A quiet lake" :width 640 :height 480)
                  ("A quiet lake" "640×480"))
                 ("generate_image" (:prompt "A quiet lake" :width 640)
                  ("A quiet lake" "width 640"))
                 ("copy_from_sandbox" (:source "src/a.el" :destination "lisp/b.el")
                  ("src/a.el → lisp/b.el"))
                 ("create_file" (:path "a.el" :content "α\n") ("a.el" "3 bytes"))
                 ("apply_patch"
                  (:patch "*** Begin Patch\r\n*** Add File: a.el\r\n+*** Delete File: fake.el\r\n*** Update File: b.el\r\n@@\r\n-x\r\n+y\r\n*** Delete File: c.el\r\n*** End Patch\r\n")
                  ("a.el" "b.el" "c.el"))))
    (let* ((presentation (ende-render-tool-request (car case) (cadr case)))
           (summary (plist-get presentation :summary)))
      (dolist (fact (caddr case)) (should (string-search fact summary)))
      (should-not (string-search (car case) summary))
      (should-not (string-match-p "Read file\\|Search files\\|fake.el" summary))
      (should (plist-get presentation :folded))))
  (should (equal (plist-get (ende-render-tool-request "generate_image" '(:prompt "Lake"))
                            :summary) "Lake"))
  (dolist (tool '("project_info" "list_project_buffers"))
    (let ((presentation (ende-render-tool-request tool "{}")))
      (should (equal (plist-get presentation :summary) ""))
      (should (equal (plist-get presentation :body) ""))))
  (let ((body (plist-get
               (ende-render-tool-request
                "edit_file" '(:path "a.el" :edits
                              [(:anchor "old" :position "replace" :text "new" :occurrence 2)
                               (:anchor "anchor" :position "before" :text "above" :all t)
                               (:anchor "anchor" :position "after" :text "below")])) :body)))
    (dolist (fact '("occurrence 2" "all occurrences" "- old" "+ new"
                    "+ above\n\n  anchor" "  anchor\n\n+ below"))
      (should (string-search fact body))))
  (dolist (case '(("project_info" (:root "/project/" :name "project" :file_count 7 :buffer_count 2)
                  ("7 files" "2 buffers"))
                 ("file_info" (:path "a.el" :line_count 7 :byte_count 40
                               :digest "long-secret-digest" :visiting_buffer_modified t)
                  ("a.el" "disk" "7 lines" "40 bytes"))))
    (let* ((presentation (ende-render-tool-response (car case) (cadr case)))
           (summary (plist-get presentation :summary))
           (body (plist-get presentation :body)))
      (dolist (fact (caddr case)) (should (string-search fact summary)))
      (should-not (string-match-p "digest\\|visiting\\|modified" (concat summary body)))
      (when (equal (car case) "project_info") (should (string-search "/project/" body)))))
  (dolist (tool '("list_project_files" "list_project_buffers"))
    (let* ((buffers (equal tool "list_project_buffers"))
           (items [(:path "a.el" :byte_count 40 :buffer_id "buf-1" :buffer_modified t
                    :visiting_buffer_modified t)
                   (:path "b.el" :byte_count 20 :buffer_id "buf-2")])
           (body (plist-get (ende-render-tool-response
                             tool (list (if buffers :buffers :files) items)) :body)))
      (should (= (length (split-string body "\n")) 2))
      (should (string-prefix-p "a.el" body))
      (should (string-search "\nb.el" body))
      (should-not (string-search "Path:" body))
      (if buffers
          (progn (should (string-search "buf-1" body))
                 (should (string-search "modified" body)))
        (should-not (string-search "modified" body))))))

(ert-deftest ende-render-failures-and-fallback-remain-presentations ()
  (dolist (content '("{\"ok\":false,\"error\":\"denied\"}"
                     "{\"ok\":true,\"result\":{\"process_ok\":false,\"exit_code\":9,\"stdout\":\"\",\"stderr\":\"failed\"}}"))
    (let ((presentation (ende-render-tool-response "run_host_command" content)))
      (should (eq (plist-get presentation :state) 'error))
      (should (plist-get presentation :folded))))
  ;; Outcomes lead long context; empty streams and protocol bookkeeping do not
  ;; crowd out diagnostic text.  Both command transports share this contract.
  (dolist (tool '("run_host_command" "run_sandbox_command"))
    (let* ((presentation (ende-render-tool-response
                          tool '(:ok t :result (:process_ok :false :exit_code 9
                                                :stdout "output" :stderr "failure"))
                          '(:command "a very long command")))
           (body (plist-get presentation :body)))
      (should (string-prefix-p "exit 9 · failed" (plist-get presentation :summary)))
      (should (eq (plist-get presentation :state) 'error))
      (should (string-prefix-p "stderr:\nfailure" body))
      (should (string-search "stdout:\noutput" body))
      (should-not (string-match-p "command\\|Process ok\\|Exit code" body)))
    (dolist (streams '((:stdout "" :stderr "failure") (:stdout "output" :stderr "")
                       (:stdout "" :stderr "")))
      (let ((body (plist-get (ende-render-tool-response
                             tool (append '(:exit_code 0 :process_ok t) streams)) :body)))
        (dolist (key '(:stdout :stderr))
          (when (equal (plist-get streams key) "")
            (should-not (string-search (substring (symbol-name key) 1) body)))))))
  (let ((presentation (ende-render-tool-response
                       "web_fetch" '(:ok t :result (:status 404 :final_url "https://example.org/long"
                                                   :content "Missing")))))
    (should (string-prefix-p "HTTP 404" (plist-get presentation :summary)))
    (should (eq (plist-get presentation :state) 'error)))
  (dolist (tool '("read_file" "read_buffer"))
    (let* ((presentation (ende-render-tool-response
                          tool '(:ok t :result (:path "a.el" :start_line 8 :end_line 10
                                                :total_lines 20 :truncated t :content "a\nb\nc\n"
                                                :visiting_buffer_modified t))))
           (summary (plist-get presentation :summary)))
      (should (string-prefix-p (if (equal tool "read_file") "Disk" "Buffer") summary))
      (should (string-search "8–10" summary))
      (should (string-search "excerpt" summary))
      (should-not (string-search "partial" summary))
      (should (eq (plist-get presentation :state) 'success))
      (should (string-search "   8 │ a" (plist-get presentation :body)))
      (should-not (string-search "modified" (plist-get presentation :body)))))
  ;; A readable unknown or unexpected result is not proof of success.  Only
  ;; explicit envelope outcomes apply, including when a formatter falls back.
  (dolist (tool '("future_tool" "file_info" "run_host_command"))
    (dolist (case '(((:unexpected "kept") neutral)
                   ((:ok t :result (:unexpected "kept")) success)
                   ((:ok :false :error "kept") error)))
      (let ((presentation (ende-render-tool-response tool (car case))))
        (should (eq (plist-get presentation :state) (cadr case)))
        (should (string-search "kept" (plist-get presentation :body)))
        (should-not (string-search tool (plist-get presentation :summary))))))
  (cl-letf (((symbol-function 'ende-render--request) (lambda (&rest _) (error "formatter failed"))))
    (let ((presentation (ende-render-tool-request "future_tool" "{\"message\":\"kept\"}")))
      (should (equal (plist-get presentation :body) "Message: kept"))
      (should (eq (plist-get presentation :state) 'neutral))))
  (let ((presentation (ende-render-tool-response "spawn_agent" "{\"ok\":true,\"result\":\"Agent failed\"}")))
    (should (eq (plist-get presentation :state) 'neutral))
    (should (equal (plist-get presentation :body) "Agent failed"))))

(provide 'ende-render-test)
;;; ende-render-test.el ends here
