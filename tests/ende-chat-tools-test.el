;;; ende-chat-tools-test.el --- Chat tool tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'ende)
(require 'ende-test-helpers)
(require 'ende-chat-test-helpers)

(ert-deftest ende-test/chat-and-agents-share-implementation-guidance-after-reload ()
  (let ((ende-agent--implementation-guidance "")
        (ende-agent--system-prompt "")
        (ende-chat--system ""))
    (ende-reload)
    (should-not (string-empty-p ende-agent--implementation-guidance))
    (with-temp-buffer
      (org-mode)
      (dolist (prompt (list (ende-chat--system-prompt
                            (ende-chat--build-document))
                           ende-agent--system-prompt))
        (should (string-match-p
                 (regexp-quote ende-agent--implementation-guidance) prompt))))))

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

(ert-deftest ende-test/chat-completions-tool-only-stream-starts-generation-timing ()
  (let* ((clock 5.0)
         (session
          (ende-llm--make-session
           :profile '(:model "test-model")
           :callbacks nil
           :tool-calls (make-hash-table :test #'eql))))
    (cl-letf (((symbol-function 'float-time) (lambda (&optional _) clock)))
      ;; A call ID alone announces the call but contains no generated output.
      (ende-llm--handle-json
       session
       (ende-protocol-json-read-string
        (concat
         "{\"choices\":[{\"delta\":{\"tool_calls\":[{"
         "\"index\":0,\"id\":\"call-1\",\"type\":\"function\"}]}}]}")))
      (should-not (ende-stream-session-generation-start
                   (ende-llm--session-stream session)))
      (setq clock 10.0)
      (ende-llm--handle-json
       session
       (ende-protocol-json-read-string
        (concat
         "{\"choices\":[{\"delta\":{\"tool_calls\":[{"
         "\"index\":0,\"function\":{\"arguments\":\"{}\"}}]}}]}")))
      (should (= (ende-stream-session-generation-start
                  (ende-llm--session-stream session))
                 10.0))
      (setq clock 12.0)
      (ende-llm--handle-json
       session
       (ende-protocol-json-read-string
        "{\"choices\":[],\"usage\":{\"completion_tokens\":40}}"))
      (should (= (plist-get (ende-llm--result session) :speed) 20.0)))))

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
                                     :arguments "{\"query\":\"classified\"}")))
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
                         (lambda (source regexp before after maximum accept-match)
                           (push (plist-get source :relative) visited)
                           (funcall original-search source regexp before after
                                    maximum accept-match))))
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

(ert-deftest ende-test/chat-guidance-joins-an-in-flight-final-turn-and-round-trips ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm
          (list '(:content "I was about to give the final answer.")
                "Final answer after guidance.")
          t))
        job)
    (unwind-protect
        (ende-test--with-buffer
            "* Conversation\n** Question\nConsider the request.\n"
          (org-mode)
          (ende-test--chat-goto "** Question")
          (setq job (ende-ask '(:name remote :model "fixture")))
          (should (eq (ende-job-state job) 'running))
          (ende-queue-add-guidance job "First exact guidance.\nline two")
          (ende-queue-add-guidance job "Second guidance, unchanged.")
          (ende-test--complete-call (car ende-test--calls))
          (should (eq (ende-job-state job) 'running))
          (should (= (length ende-test--calls) 2))
          (should-not (ende-job-pending-guidance job))
          (should (= (ende-job-tool-call-count job) 0))
          (let* ((call (cadr ende-test--calls))
                 (messages (ende-test--chat-protocol-messages
                            (plist-get call :messages)))
                 (assistant
                  (cl-find-if
                   (lambda (message)
                     (plist-member message :tool-calls))
                   messages))
                 (results
                  (cl-remove-if-not
                   (lambda (message)
                     (equal (plist-get message :role) "tool"))
                   messages)))
            (should-not
             (member "user_guidance"
                     (ende-test--tool-definition-names
                      (plist-get (plist-get call :options)
                                 :tool-definitions))))
            (should (equal (plist-get assistant :role) "assistant"))
            (should (equal (plist-get assistant :content)
                           "I was about to give the final answer."))
            (should (equal (mapcar (lambda (tool-call)
                                     (plist-get tool-call :name))
                                   (plist-get assistant :tool-calls))
                           '("user_guidance" "user_guidance")))
            (should (equal (mapcar (lambda (tool-call)
                                     (plist-get tool-call :arguments))
                                   (plist-get assistant :tool-calls))
                           '("{}" "{}")))
            (should (= (length (delete-dups
                                (mapcar (lambda (tool-call)
                                          (plist-get tool-call :id))
                                        (plist-get assistant :tool-calls))))
                       2))
            (should (equal (mapcar (lambda (result)
                                     (plist-get result :content))
                                   results)
                           '("First exact guidance.\nline two"
                             "Second guidance, unchanged."))))
          (should-not (string-match-p "First exact guidance" (buffer-string)))
          (should-not (string-match-p "First exact guidance"
                                      (ende-chat--staged-text job)))
          (ende-test--complete-call (cadr ende-test--calls))
          (should (eq (ende-job-state job) 'completed))
          (goto-char (point-max))
          (insert "** Follow-up\nContinue from that answer.\n")
          (ende-test--chat-goto "** Follow-up")
          (let* ((history (ende-chat-context-at-point))
                 (assistant
                  (cl-find-if
                   (lambda (message)
                     (and (equal (plist-get message :role) "assistant")
                          (cl-some
                           (lambda (tool-call)
                             (equal (plist-get tool-call :name) "user_guidance"))
                           (plist-get message :tool-calls))))
                   history))
                 (results
                  (cl-remove-if-not
                   (lambda (message)
                     (equal (plist-get message :role) "tool"))
                   history)))
            (should assistant)
            (should (equal (mapcar (lambda (tool-call)
                                     (plist-get tool-call :arguments))
                                   (plist-get assistant :tool-calls))
                           '("{}" "{}")))
            (should (equal (mapcar (lambda (result)
                                     (plist-get result :content))
                                   results)
                           '("First exact guidance.\nline two"
                             "Second guidance, unchanged.")))
            (should (string-match-p
                     "I was about to give the final answer"
                     (ende-test--chat-context-text history)))))
      (ende-test--reset))))

(ert-deftest ende-test/chat-guidance-follows-all-results-in-an-async-tool-round ()
  (ende-test--reset)
  (let* ((ende-tools--registry (copy-sequence ende-tools--registry))
         (pending nil)
         (executed-tools nil)
         (original-tool-response-async
          (symbol-function #'ende-chat--tool-response-async))
         (ende-llm-function
          (ende-test--fake-llm
           (list (list :tool-calls
                       (list '(:id "real-one" :name "ende_test_guidance_tool"
                               :arguments "{}")
                             '(:id "real-two" :name "ende_test_guidance_tool"
                               :arguments "{}")))
                 "All work is complete.")
           t))
         job)
    (unwind-protect
        (progn
          (ende-tool-register
           'ende_test_guidance_tool "Guidance ordering fixture"
           ende-tools--empty-object-schema
           (lambda (_arguments _root callback) (push callback pending))
           nil :no-resource '(:requires-root nil :async t))
          (cl-letf (((symbol-function #'ende-chat--tool-response-async)
                     (lambda (call configuration callback)
                       (push (ende-chat--tool-value call :name) executed-tools)
                       (funcall original-tool-response-async
                                call configuration callback))))
            (ende-test--with-buffer
              (concat (ende-test--chat-config-block
                       '((tools ende_test_guidance_tool)))
                      "* Conversation\n** Question\nWork on both items.\n")
            (org-mode)
            (ende-test--chat-goto "** Question")
            (setq job (ende-ask '(:name remote :model "fixture")))
            (ende-test--complete-call (car ende-test--calls))
            (should (= (length pending) 1))
            (ende-queue-add-guidance job "Wait until both tools finish.")
            (funcall (car pending) '(:first t) nil)
            (should (= (length ende-test--calls) 1))
            (should (= (length pending) 2))
            (funcall (car pending) '(:second t) nil)
            (should (= (length ende-test--calls) 2))
            (let* ((messages (ende-test--chat-protocol-messages
                              (plist-get (cadr ende-test--calls) :messages)))
                   (tool-round
                    (cl-find-if
                     (lambda (message)
                       (and (equal (plist-get message :role) "assistant")
                            (cl-some
                             (lambda (call)
                               (equal (plist-get call :name)
                                      "ende_test_guidance_tool"))
                             (plist-get message :tool-calls))))
                     messages))
                   (assistant-index (cl-position tool-round messages :test #'eq)))
              (should tool-round)
              (should (equal (mapcar (lambda (call) (plist-get call :id))
                                     (plist-get tool-round :tool-calls))
                             '("real-one" "real-two")))
              (should (equal
                       (mapcar (lambda (message)
                                 (plist-get message :tool-call-id))
                               (seq-take
                                (cl-remove-if-not
                                 (lambda (message)
                                   (equal (plist-get message :role) "tool"))
                                 messages)
                                2))
                       '("real-one" "real-two")))
              (should (equal
                       (plist-get (nth (+ assistant-index 3) messages) :role)
                       "assistant"))
              (let* ((guidance-turn (nth (+ assistant-index 3) messages))
                     (guidance-call (car (plist-get guidance-turn :tool-calls))))
                (should (equal (plist-get guidance-turn :role) "assistant"))
                (should (equal (plist-get guidance-call :name) "user_guidance"))
                (should (equal (plist-get guidance-call :arguments) "{}"))
                (should (equal (plist-get (nth (+ assistant-index 4) messages)
                                         :tool-call-id)
                               (plist-get guidance-call :id)))
                (should (equal (plist-get (nth (+ assistant-index 4) messages)
                                         :content)
                               "Wait until both tools finish."))))
              (should (equal (nreverse executed-tools)
                             '("ende_test_guidance_tool"
                               "ende_test_guidance_tool")))
              (should (= (ende-job-tool-call-count job) 2))
              (should-not (ende-job-pending-guidance job)))))
      (ende-test--reset))))

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
