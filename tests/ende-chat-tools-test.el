;;; ende-chat-tools-test.el --- Chat tool tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'ende)
(require 'ende-test-helpers)
(require 'ende-chat-test-helpers)

(ert-deftest ende-test/chat-completions-tool-diagnostics-cover-local-and-openrouter ()
  (should (eq ende-tool-diagnostics t))
  (let* ((definition
          (cl-find-if
           (lambda (candidate)
             (equal (alist-get :name (alist-get :function candidate))
                    "read_file"))
           (ende-tool-definitions)))
         (tool-event
          (ende-protocol-json-read-string
           (concat
            "{\"choices\":[{\"delta\":{\"tool_calls\":["
            "{\"index\":0,\"id\":\"call-1\",\"type\":\"function\","
            "\"function\":{\"name\":\"read_file\","
            "\"arguments\":\"{\\\"path\\\":\\\"secret.el\\\"}\"}}]}}]}"))))
    (dolist (provider '(local openrouter))
      (let* ((profile (list :name provider :model "test-model"
                            :tool-definitions (list definition)))
             (body (ende-llm--request-body
                    (list (cons "user" "read the file")) profile))
             (events nil)
             (session
              (ende-llm--make-session
               :profile profile
               :callbacks (list :on-event
                                (lambda (event) (push event events)))
               :tool-calls (make-hash-table :test #'eql))))
        (ende-llm--diagnose-request session body)
        (ende-llm--handle-json session tool-event)
        (ende-llm--diagnose-result session)
        (let ((diagnostics
               (mapcar (lambda (event) (plist-get event :text))
                       (cl-remove-if-not
                        (lambda (event)
                          (eq (plist-get event :type) :diagnostic))
                        (nreverse events)))))
          (should (equal diagnostics
                         '("Chat Completions tools sent: read_file"
                           "Chat Completions output items: tool_call(read_file)")))
          (should-not (string-match-p
                       "secret\\.el" (mapconcat #'identity diagnostics "\n"))))))))

(ert-deftest ende-test/chat-tool-messages-adapt-to-provider-protocols ()
  (let* ((transcript
          (list (list :role "assistant" :content nil
                      :tool-calls
                      (list (list :id "call-1" :name "read_file"
                                  :arguments "{ \"path\" : \"foo.el\" }")
                            (list :id "call-2" :name "read_file"
                                  :arguments "{\"path\":\"bar.el\"}")))
                (list :role "tool" :tool-call-id "call-1"
                      :content "{\"text\":\"first result\"}")
                (list :role "tool" :tool-call-id "call-2"
                      :content "{\"text\":\"second result\"}")))
         (chat-assistant (ende-llm--message-json (car transcript)))
         (responses (ende-codex--request-messages transcript)))
    (should (equal (alist-get :arguments
                              (alist-get :function
                                         (car (alist-get :tool_calls
                                                         chat-assistant))))
                   "{ \"path\" : \"foo.el\" }"))
    (should (equal (alist-get :call_id (nth 0 responses)) "call-1"))
    (should (equal (alist-get :call_id (nth 1 responses)) "call-2"))
    (should (equal (alist-get :output (nth 2 responses))
                   (plist-get (nth 1 transcript) :content)))))

(ert-deftest ende-test/chat-tool-call-list-accepts-proper-sequences-only ()
  (let* ((calls '((:id "call-1") (:id "call-2")))
         (result (list :tool-calls calls)))
    (should (equal (ende-chat--tool-call-list result) calls))
    (should (equal (ende-chat--tool-call-list
                    (list :tool-calls (vconcat calls)))
                   calls))
    (should-not (ende-chat--tool-call-list '(:tool-calls nil)))
    (dolist (invalid '("not a call array" 7))
      (should-error (ende-chat--tool-call-list (list :tool-calls invalid))))
    (should-error
     (ende-chat--tool-call-list
      (list :tool-calls (cons (car calls) 'improper-tail))))))

(ert-deftest ende-test/chat-tool-loop-filters-search-and-authorizes-calls ()
  (let* ((root (make-temp-file "ende-chat-tools-" t))
         (public (expand-file-name "main.el" root))
         (private-directory (expand-file-name "private" root))
         (private (expand-file-name "secret.txt" private-directory))
         (visited nil)
         (job nil)
         (original-search (symbol-function #'ende-tools--search-source)))
    (ende-test--reset)
    (unwind-protect
        (progn
          (make-directory private-directory)
          (with-temp-file public (insert "public text\n"))
          (with-temp-file private (insert "classified text\n"))
          (let ((ende-llm-function
                 (ende-test--fake-llm
                  (list
                   (list :tool-calls
                         (list (list :id "call-project"
                                     :name "project_info" :arguments "{}")
                               (list :id "call-search"
                                     :name "search_project"
                                     :arguments "{\"regexp\":\"classified\"}")))
                   (list :tool-calls
                         (list (list :id "call-project-round-2"
                                     :name "project_info" :arguments "{}")))
                   "Search complete")
                  t)))
            (with-temp-buffer
              (insert
               (format
                (concat
                 "#+begin_ende\n(root %S)\n"
                 "(group inspect project_info list_project_files read_file "
                 "search_project)\n"
                 "(resource project (include \"**\") "
                 "(exclude \"private/**\"))\n"
                 "(tools inspect)\n"
                 "(set-tool-resources inspect :resources project)\n"
                 "#+end_ende\n\n* Find classified text\nSearch now\n")
                root))
              (org-mode)
              (ende-test--chat-goto "* Find classified text")
              (cl-letf (((symbol-function #'ende-tools--search-source)
                         (lambda (source regexp before after maximum)
                           (push (plist-get source :relative) visited)
                           (funcall original-search source regexp before after
                                    maximum))))
                (setq job (ende-ask))
                (goto-char (point-min))
                (should (search-forward
                         "(resource project (include \"**\")" nil t))
                (replace-match
                 "(resource project (include \"private/**\")" t t)
                (ende-test--complete-call (car ende-test--calls))
                (should (= (ende-job-tool-call-count job) 2))
                (ende-test--complete-call (nth 1 ende-test--calls))
                (should (= (ende-job-tool-call-count job) 3))
                (ende-test--complete-call (nth 2 ende-test--calls)))
              (should (= (ende-job-tool-call-count job) 3))
              (should (equal (nreverse visited) '("main.el")))
              (should (string-match-p "Search complete" (buffer-string)))
              (should (= (length ende-test--calls) 3))
              (let* ((tool-reply
                      (car (last (plist-get (nth 1 ende-test--calls) :messages))))
                     (reply (json-parse-string
                             (plist-get tool-reply :content)
                             :object-type 'hash-table)))
                (should (equal (plist-get tool-reply :role) "tool"))
                (should-not (string-match-p "classified"
                                            (plist-get tool-reply :content)))
                (should (= (gethash "files_inspected"
                                    (gethash "result" reply)) 1))))))
      (ende-test--reset)
      (delete-directory root t))))

(ert-deftest ende-test/chat-work-container-is-only-created-for-tool-loops ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("A plain answer.") t)))
    (ende-test--with-buffer
     "* Topic\n** Question\nAnswer this.\n"
     (org-mode)
     (ende-test--chat-goto "** Question")
     (let ((job (ende-ask)))
       (ende-test--complete-call (car ende-test--calls))
       (should (eq (ende-job-state job) 'completed)))
     (let* ((document (ende-chat--build-document))
            (headlines (ende-chat--document-headlines document)))
       (should-not (cl-some (lambda (headline)
                              (ende-chat--work-section-p document headline))
                            headlines))
       (should (= (cl-count-if
                   (lambda (headline)
                     (and (equal (org-element-property :raw-value headline)
                                 "Agent: Reply")
                          (equal (ende-chat--property document headline :role)
                                 "assistant")))
                   headlines)
                  1))))))

(provide 'ende-chat-tools-test)

;;; ende-chat-tools-test.el ends here
