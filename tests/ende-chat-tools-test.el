;;; ende-chat-tools-test.el --- Chat tool tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Coverage for tool dispatch loops and authorization integration.

;;; Code:

(require 'ert)
(require 'ende)
(require 'ende-test-helpers)
(require 'cl-lib)
(require 'json)
(require 'ende-chat-test-helpers)
(require 'ox-ascii)



(ert-deftest ende-test/chat-tool-loop-filters-search-and-authorizes-calls ()
  (let* ((root (make-temp-file "ende-chat-tools-" t))
         (public (expand-file-name "main.el" root))
         (private-directory (expand-file-name "private" root))
         (private (expand-file-name "secret.txt" private-directory))
         (visited nil)
         (job nil)
         (frozen-configuration nil)
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
                                     :name "project_info"
                                     :arguments "{}")
                               (list :id "call-search"
                                     :name "search_project"
                                     :arguments "{\"regexp\":\"classified\"}")))
                   (list :tool-calls
                         (list (list :id "call-project-round-2"
                                     :name "project_info"
                                     :arguments "{}")))
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
                (let* ((document (ende-chat--build-document))
                       (question (car (ende-chat--document-headlines document))))
                  (setq frozen-configuration
                        (ende-chat-config-resolve
                         (ende-chat--document-tree document)
                         (ende-chat--document-parent document) question))
                  (goto-char (point-min))
                  (should (search-forward
                           "(resource project (include \"**\")" nil t))
                  (replace-match
                   "(resource project (include \"private/**\")" t t)
                  (ende-test--complete-call (car ende-test--calls))
                  (should (= (ende-job-tool-call-count job) 2))
                  (ende-test--complete-call (nth 1 ende-test--calls))
                  (should (= (ende-job-tool-call-count job) 3))
                  (ende-test--complete-call (nth 2 ende-test--calls))))
              (should (= (ende-job-tool-call-count job) 3))
              (should (equal (nreverse visited) '("main.el")))
              (should (string-match-p "Search complete" (buffer-string)))
              (should (= (length ende-test--calls) 3))
              (let* ((first-call (car ende-test--calls))
                     (definitions (plist-get (plist-get first-call :options)
                                             :tool-definitions))
                     (second-messages (plist-get (nth 1 ende-test--calls)
                                                 :messages))
                     (tool-reply (car (last second-messages))))
                (should (equal (sort (mapcar (lambda (definition)
                                               (alist-get
                                                :name
                                                (alist-get :function
                                                           definition)))
                                             definitions)
                                     #'string<)
                               '("about_thinking" "list_project_files" "project_info"
                                 "read_file" "search_project")))
                (should (equal (plist-get tool-reply :role) "tool"))
                (should-not (string-match-p "classified"
                                            (plist-get tool-reply :content)))
                (let* ((reply (json-parse-string
                               (plist-get tool-reply :content)
                               :object-type 'hash-table))
                       (result (gethash "result" reply)))
                  (should (= (gethash "files_inspected" result) 1))))
              (let* ((configuration frozen-configuration)
                     (denied
                      (ende-tool-call
                       "read_file" '(("path" . "private/secret.txt"))
                       root configuration))
                     (allowed
                      (ende-tool-call "read_file" '(("path" . "main.el"))
                                      root configuration))
                     (no-resources (copy-tree configuration)))
                (should (plist-get allowed :ok))
                (should (string-match-p
                         "public text"
                         (plist-get (plist-get allowed :result) :content)))
                (should-not (plist-get denied :ok))
                (should (string-match-p "not authorized"
                                        (plist-get denied :error)))
                (setf (plist-get no-resources :tool-resources) nil)
                (should (string-match-p
                         "no assigned resources"
                         (plist-get
                          (ende-tool-call
                           "read_file" '(("path" . "main.el"))
                           root no-resources)
                          :error)))
                (should (plist-get
                         (ende-tool-call "project_info" nil root no-resources)
                         :ok))))))
      (ende-test--reset)
      (when (file-directory-p root)
        (delete-directory root t)))))



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
                       "secret\\.el"
                       (mapconcat #'identity diagnostics "\n"))))))))

;;;; Org-backed chat

(ert-deftest ende-test/chat-tool-messages-adapt-to-provider-protocols ()
  (let* ((transcript
          (list (list :role "assistant" :content nil
                      :tool-calls
                      (list (list :id "call-1" :name "read_file"
                                  :arguments "{ \"path\" : \"foo.el\" }")
                            (list :id "call-2" :name "read_file"
                                  :arguments "{\"path\":\"bar.el\"}")))
                (list :role "tool" :tool-call-id "call-1"
                      :content "{\"text\":\"first line\\nsecond line\"}")
                (list :role "tool" :tool-call-id "call-2"
                      :content "{\"text\":\"another result\"}")
                (list :role "assistant" :content "One more lookup."
                      :tool-calls
                      (list (list :id "call-3" :name "read_file"
                                  :arguments "{\"path\":\"third.el\"}")))
                (list :role "tool" :tool-call-id "call-3"
                      :content "{\"text\":\"third result\"}")))
         (chat-assistant (ende-llm--message-json (car transcript)))
         (chat-tool (ende-llm--message-json (nth 1 transcript)))
         (responses (ende-codex--request-messages transcript)))
    (should (equal (plist-get (car (plist-get (car transcript) :tool-calls))
                              :arguments)
                   "{ \"path\" : \"foo.el\" }"))
    (should (equal (alist-get :arguments
                              (alist-get :function
                                         (car (alist-get :tool_calls
                                                         chat-assistant))))
                   "{ \"path\" : \"foo.el\" }"))
    (should (equal (alist-get :tool_call_id chat-tool) "call-1"))
    (should (equal (alist-get :call_id (nth 0 responses)) "call-1"))
    (should (equal (alist-get :call_id (nth 1 responses)) "call-2"))
    (should (equal (alist-get :output (nth 2 responses))
                   (plist-get (nth 1 transcript) :content)))
    (should (equal (alist-get :call_id (nth 5 responses)) "call-3"))
    (should (equal (alist-get :output (nth 6 responses))
                   (plist-get (nth 4 transcript) :content)))))



(ert-deftest ende-test/chat-tool-call-list-accepts-proper-sequences-only ()
  (let* ((calls '((:id "call-1") (:id "call-2")))
         (result (list :tool-calls calls)))
    (should (equal (ende-chat--tool-call-list result) calls))
    (should (equal
             (ende-chat--tool-call-list
              (list :tool-calls (vconcat calls)))
             calls))
    (should-not (ende-chat--tool-call-list '(:tool-calls nil)))
    (dolist (invalid '("not a call array" 7))
      (should-error
       (ende-chat--tool-call-list (list :tool-calls invalid))))
    (should-error
     (ende-chat--tool-call-list
      (list :tool-calls (cons (car calls) 'improper-tail))))))



(ert-deftest ende-test/chat-single-tool-round-is-readable-in-history ()
  (let* ((call (list :id "read-1" :name "read_file"
                     :arguments "{ \"path\" : \"foo.el\" }"))
         (assistant
          (ende-chat--assistant-protocol-text
           2 "question-1" "request-1" ""
           '(:content "I'll inspect foo.el.") (list call)))
         (tool (ende-chat--tool-result-text
                2 "request-1" call "{\"result\":\"source text\"}"))
         (answer (ende-chat--assistant-text
                  2 "question-1" "request-1" "" '(:content "Final answer"))))
    (with-temp-buffer
      (org-mode)
      (insert "* Topic\n** Question\nRead foo.el.\n")
      (insert assistant tool answer)
      (let* ((document (ende-chat--build-document))
             (call (cl-find-if
                    (lambda (headline)
                      (ende-chat--tool-call-protocol-p document headline))
                    (ende-chat--document-headlines document))))
        (should call)
        (delete-region (org-element-property :begin call)
                       (org-element-property :end call)))
      (insert "** Follow-up\nAsk about nil.\n")
      (ende-test--chat-goto "** Follow-up")
      (let* ((messages (ende-chat-context-at-point))
             (text (ende-test--chat-context-text messages))
             (reply (cl-position-if
                     (lambda (message)
                       (and (equal (ende-test--chat-message-role message)
                                   "assistant")
                            (stringp (cdr message))
                            (string-match-p "Final answer" (cdr message))))
                     messages)))
        (should-not (ende-test--chat-protocol-messages messages))
        (should (string-match-p "I'll inspect foo.el." text))
        (should-not (string-match-p "Tool Call: read_file" text))
        (should (string-match-p "Tool Result: read_file" text))
        (should (string-match-p "Arguments:\n{ \"path\" : \"foo.el\" }"
                                text))
        (should (string-match-p "source text" text))
        (should (= reply (- (length messages) 2)))))))



(ert-deftest ende-test/chat-tool-calls-and-results-are-work-siblings ()
  (let* ((request-id "request-siblings")
         (question-id "question-siblings")
         (calls '((:id "project" :name "project_info" :arguments "{}")
                  (:id "read" :name "read_file"
                       :arguments "{\"path\":\"ende.el\",\"start_line\":1}")))
         (sections
          (concat
           (ende-chat--work-text 2 question-id request-id)
           (ende-chat--assistant-protocol-text
            3 question-id request-id "" '(:content "Checking the source.") calls)
           (ende-chat--tool-result-text 3 request-id (car calls) "Project")
           (ende-chat--tool-result-text 3 request-id (cadr calls) "Source")
           (ende-chat--assistant-protocol-text
            2 question-id request-id "" '(:content "Finished.") nil))))
    (with-temp-buffer
      (org-mode)
      (insert "* Topic\n** Question\nInspect.\n")
      (insert sections)
      (goto-char (point-min))
      (search-forward "** Agent: Work")
      (org-back-to-heading t)
      (org-entry-delete nil ende-chat-state-property)
      (let* ((document (ende-chat--build-document))
             (headlines (ende-chat--document-headlines document))
             (work (cl-find-if (lambda (headline)
                                 (ende-chat--work-section-p document headline))
                               headlines))
             (calls-in-doc
              (cl-remove-if-not
               (lambda (headline)
                 (ende-chat--tool-call-protocol-p document headline))
               headlines))
             (results
              (cl-remove-if-not
               (lambda (headline)
                 (ende-chat--tool-result-protocol-p document headline))
               headlines))
             (replies
              (cl-remove-if-not
               (lambda (headline)
                 (and (ende-chat--assistant-protocol-p document headline)
                      (equal (org-element-property :raw-value headline)
                             "Agent: Reply")))
               headlines))
             (final (car (last replies)))
             (commentary (car replies)))
        (should (= (length replies) 2))
        (should (eq (ende-chat--parent document commentary) work))
        (should (= (org-element-property :level commentary) 3))
        (should (eq (ende-chat--parent document final)
                    (ende-chat--parent document work)))
        (should (= (org-element-property :level final) 2))
        (should (equal (mapcar (lambda (headline)
                                 (org-element-property :raw-value headline))
                               calls-in-doc)
                       '("--> project_info" "--> read_file")))
        (should (equal (mapcar (lambda (headline)
                                 (org-element-property :raw-value headline))
                               results)
                       '("project_info" "read_file: path=\"ende.el\", start_line=1")))
        (dolist (headline (append calls-in-doc results))
          (should (eq (ende-chat--parent document headline) work))
          (should (= (org-element-property :level headline) 3)))
        (should (equal (mapcar (lambda (headline)
                                 (ende-chat--property
                                  document headline :tool-kind))
                               calls-in-doc)
                       '("call" "call")))
        (should (equal (mapcar (lambda (headline)
                                 (ende-chat--property
                                  document headline :tool-kind))
                               results)
                       '("result" "result")))
        (should (eq (ende-chat--validate-work-section document work) work))
        (should (< (org-element-property :begin (car calls-in-doc))
                   (org-element-property :begin (cadr calls-in-doc))))
        (should (< (org-element-property :begin (cadr calls-in-doc))
                   (org-element-property :begin (car results))))
        (should (< (org-element-property :begin (car results))
                   (org-element-property :begin (cadr results))))
        (should (< (org-element-property :begin (cadr results))
                   (org-element-property :begin final)))))
    (let ((calls '((:id "empty" :name "read_file" :arguments "{}"))))
      (with-temp-buffer
	(org-mode)
	(insert "* Topic\n** Question\n")
	(insert (ende-chat--work-text 2 "q" "r"))
	(insert (ende-chat--assistant-protocol-text
		 3 "q" "r" "" '(:content "") calls))
	(let* ((document (ende-chat--build-document))
               (headlines (ende-chat--document-headlines document)))
          (should-not
           (cl-some (lambda (headline)
                      (and (equal (org-element-property :raw-value headline)
                                  "Agent: Reply")
                           (ende-chat--work-ancestor document headline)))
                    headlines))
          (should (= (cl-count-if
                      (lambda (headline)
			(ende-chat--tool-call-protocol-p document headline))
                      headlines)
                     1)))))))



(ert-deftest ende-test/chat-work-rejects-nested-tool-call-headings ()
  (with-temp-buffer
    (org-mode)
    (insert (concat
             "* Topic\n** Question\nInspect.\n"
             "*** Agent: Work\n"
             ":PROPERTIES:\n:ENDE_CHAT_WORK: t\n"
             ":ENDE_CHAT_REPLY_TO: question\n"
             ":ENDE_CHAT_REQUEST_ID: request\n:END:\n\n"
             "**** Agent: Reply\n"
             ":PROPERTIES:\n:ENDE_ROLE: assistant\n"
             ":ENDE_CHAT_REPLY_TO: question\n"
             ":ENDE_CHAT_REQUEST_ID: request\n"
             ":ENDE_CHAT_CONTENT_FORMAT: literal\n:END:\n\n"
             "Checking the source.\n\n"
             "***** --> read_file\n"
             ":PROPERTIES:\n:ENDE_CHAT_REQUEST_ID: request\n"
             ":ENDE_TOOL_KIND: call\n:ENDE_TOOL_CALL_ID: call\n"
             ":ENDE_TOOL_NAME: read_file\n:END:\n\n{}\n"))
    (let* ((document (ende-chat--build-document))
           (work (cl-find-if
                  (lambda (headline)
                    (ende-chat--work-section-p document headline))
                  (ende-chat--document-headlines document)))
           (failure
            (condition-case caught
                (progn (ende-chat--validate-work-section document work) nil)
              (error (error-message-string caught)))))
      (should (string-match-p
               "Malformed generated protocol section outside Agent: Work"
               failure)))))



(ert-deftest ende-test/chat-tool-history-is-readable-in-source-order ()
  (let* ((first-calls
          (list (list :id "call-A/01" :name "read_file"
                      :arguments "{\n  \"path\" : \"foo.el\",\n  \"note\" : \"* literal\"\n}" )
                (list :id "call-B:02" :name "search_project"
                      :arguments "{ \"regexp\" : \"foo\\\\nbar\" }")))
         (second-call
          (list :id "call-C" :name "read_buffer"
                :arguments "{\"buffer_id\":\"buffer-1\"}"))
         (assistant-one "I'll inspect both.\n* this is still assistant content\n")
         (result-one "first line\n* raw result heading\n#+begin_example\nlast\n")
         (result-two "{ \"matches\" : [1, 2] }\n")
         (assistant-two "")
         (result-three "Result C\n")
         (final-content "Done.\n\n")
         (request-id "request-roundtrip")
         (sections
          (concat
           (ende-chat--work-text 2 "question-roundtrip" request-id)
           (ende-chat--assistant-protocol-text
            3 "question-roundtrip" request-id ""
            (list :content assistant-one) first-calls)
           (ende-chat--tool-result-text 3 request-id (nth 0 first-calls)
					result-one)
           (ende-chat--tool-result-text 3 request-id (nth 1 first-calls)
					result-two)
           (ende-chat--assistant-protocol-text
            3 "question-roundtrip" request-id ""
            (list :content assistant-two) (list second-call))
           (ende-chat--tool-result-text 3 request-id second-call result-three)
           (ende-chat--assistant-protocol-text
            2 "question-roundtrip" request-id ""
            (list :content final-content) nil))))
    (with-temp-buffer
      (org-mode)
      (insert "* Topic\n** Question\nInspect these sources.\n")
      (insert sections)
      (insert "** Follow-up\nContinue from the tool results.\n")
      (ende-test--chat-goto "** Follow-up")
      (let* ((document (ende-chat--build-document))
             (headlines (ende-chat--document-headlines document))
             (messages (ende-chat-context-at-point))
             (context (ende-test--chat-context-text messages))
             (protocol (ende-test--chat-protocol-messages messages))
             (assistant-headlines
              (cl-remove-if-not
               (lambda (headline)
                 (ende-chat--assistant-protocol-p document headline))
               headlines))
             (calls
              (cl-remove-if-not
               (lambda (headline)
                 (ende-chat--tool-call-protocol-p document headline))
               headlines))
             (results
              (cl-remove-if-not
               (lambda (headline)
                 (ende-chat--tool-result-protocol-p document headline))
               headlines))
             (work (cl-find-if
                    (lambda (headline)
                      (ende-chat--work-section-p document headline))
                    headlines))
             (ordered-text
              (list assistant-one
                    "Tool Call: read_file"
                    (plist-get (car first-calls) :arguments)
                    "Tool Call: search_project"
                    (plist-get (cadr first-calls) :arguments)
                    "Tool Result: read_file"
                    "first line"
                    "* raw result heading"
                    "#+begin_example"
                    "Tool Result: search_project"
                    "matches"
                    "Tool Call: read_buffer"
                    (plist-get second-call :arguments)
                    "Tool Result: read_buffer"
                    "Result C"
                    final-content)))
        (should-not protocol)
        (should (equal (ende-test--chat-message-role (car (last messages)))
                       "user"))
        (should-not (member "tool" (mapcar #'ende-test--chat-message-role
                                           messages)))
        (let ((position 0))
          (dolist (text ordered-text)
            (let ((next (string-match (regexp-quote text) context position)))
              (unless next
                (ert-fail (format "Missing %S after position %d"
                                  text position)))
              (setq position (+ next (length text))))))
        (should (equal (mapcar (lambda (headline)
                                 (ende-chat--property
                                  document headline :tool-call-id))
                               calls)
                       '("call-A/01" "call-B:02" "call-C")))
        (should (equal (mapcar (lambda (headline)
                                 (ende-chat--property
                                  document headline :tool-call-id))
                               results)
                       '("call-A/01" "call-B:02" "call-C")))
        (should (equal (mapcar (lambda (headline)
                                 (ende-chat--property
                                  document headline :tool-name))
                               calls)
                       '("read_file" "search_project" "read_buffer")))
        (should (equal (mapcar (lambda (headline)
                                 (ende-chat--property
                                  document headline :tool-name))
                               results)
                       '("read_file" "search_project" "read_buffer")))
        (should (equal (mapcar (lambda (headline)
                                 (ende-chat--property
                                  document headline :request-id))
                               assistant-headlines)
                       (list request-id request-id)))
        (should (equal (mapcar (lambda (headline)
                                 (org-element-property :level headline))
                               assistant-headlines)
                       '(3 2)))
        (should (equal (mapcar (lambda (headline)
                                 (org-element-property :level headline))
                               calls)
                       '(3 3 3)))
        (should (equal (mapcar (lambda (headline)
                                 (org-element-property :level headline))
                               results)
                       '(3 3 3)))
        (dolist (headline (append calls results))
          (should (equal (ende-chat--property
                          document headline :request-id)
                         request-id))
          (should (eq (ende-chat--parent document headline) work)))
        (should (eq (ende-chat--parent document (car assistant-headlines))
                    work))
        (should-not (ende-chat--work-ancestor document
                                              (car (last assistant-headlines))))))))



(ert-deftest ende-test/chat-tool-protocol-requires-local-kind-metadata ()
  (ende-test--with-buffer
   (concat
    "* Topic\n"
    "** Kind owner\n:PROPERTIES:\n:ENDE_TOOL_KIND: call\n:END:\n"
    "*** Tool Call: read_file\n"
    ":PROPERTIES:\n:ENDE_CHAT_REQUEST_ID: lookalike-request\n"
    ":ENDE_TOOL_CALL_ID: lookalike-call\n:ENDE_TOOL_NAME: read_file\n:END:\n"
    "An ordinary call-looking note.\n"
    "*** Tool Result: read_file\n"
    ":PROPERTIES:\n:ENDE_CHAT_REQUEST_ID: lookalike-request\n"
    ":ENDE_TOOL_CALL_ID: lookalike-call\n:ENDE_TOOL_NAME: read_file\n:END:\n"
    "An ordinary result-looking note.\n"
    "** Follow-up\nAsk about these notes.\n")
   (org-mode)
   (ende-test--chat-goto "** Follow-up")
   (let* ((document (ende-chat--build-document))
          (headlines (ende-chat--document-headlines document))
          (parent (cl-find-if
                   (lambda (headline)
                     (equal (org-element-property :raw-value headline)
                            "Kind owner"))
                   headlines))
          (call-lookalike
           (cl-find-if (lambda (headline)
                         (equal (org-element-property :raw-value headline)
                                "Tool Call: read_file"))
                       headlines))
          (result-lookalike
           (cl-find-if (lambda (headline)
                         (equal (org-element-property :raw-value headline)
                                "Tool Result: read_file"))
                       headlines))
          (messages (ende-chat-context-at-point))
          (text (ende-test--chat-context-text messages)))
     (should (equal (ende-chat--property document parent :tool-kind) "call"))
     (should-not (ende-chat--property document call-lookalike :tool-kind))
     (should-not (ende-chat--property document result-lookalike :tool-kind))
     (should (equal (ende-chat--property document call-lookalike :tool-call-id)
                    "lookalike-call"))
     (should (equal (ende-chat--property document result-lookalike :tool-call-id)
                    "lookalike-call"))
     (should-not (ende-chat--tool-call-protocol-p document call-lookalike))
     (should-not (ende-chat--tool-result-protocol-p document result-lookalike))
     (should-not (ende-test--chat-protocol-messages messages))
     (should (string-match-p "An ordinary call-looking note" text))
     (should (string-match-p "An ordinary result-looking note" text)))))



(ert-deftest ende-test/chat-edited-tool-history-still-produces-a-reply ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("A useful reply.") t)))
    (ende-test--with-buffer
     (concat
      "* Topic\n"
      "** Earlier question\nRead the source.\n"
      "** Edited work wrapper\n"
      ":PROPERTIES:\n:ENDE_CHAT_WORK: t\n"
      ":ENDE_CHAT_REQUEST_ID: wrapper-request\n:END:\n\n"
      "A note added while editing the history.\n"
      "*** Agent: Reply\n"
      ":PROPERTIES:\n:ENDE_ROLE: assistant\n"
      ":ENDE_CHAT_REQUEST_ID: reply-request\n"
      ":ENDE_CHAT_CONTENT_FORMAT: org\n"
      ":ENDE_CHAT_CONTENT_HEADLINE_SHIFT: stale\n"
      ":ENDE_CHAT_CONTENT_SYNTHETIC_END: example\n"
      ":ENDE_CHAT_CONTENT_SYNTHETIC_NEWLINE: t\n:END:\n\n"
      "Old answer with preserved spaces  \n"
      "*** Tool Result: read_file\n"
      ":PROPERTIES:\n"
      ":ENDE_CHAT_REQUEST_ID: unrelated-request\n"
      ":ENDE_TOOL_CALL_ID: orphan-result\n"
      ":ENDE_TOOL_NAME: read_file\n:END:\n\n"
      "Arguments:\n{\"path\":\"src/example.el\"}\n\n"
      "Output:\nThe saved source text.\n\n"
      "** Follow-up\nExplain the saved result.\n")
     (org-mode)
     (ende-test--chat-goto "** Follow-up")
     (let* ((original (buffer-string))
            (messages (ende-chat-context-at-point))
            (text (ende-test--chat-context-text messages)))
       (should (string-match-p "Edited work wrapper" text))
       (should (string-match-p "A note added while editing" text))
       (should (string-match-p "Old answer with preserved spaces  " text))
       (should (string-match-p "src/example\\.el" text))
       (should (string-match-p "The saved source text" text))
       (should-not (ende-test--chat-protocol-messages messages))
       (should (equal original (buffer-string))))
     (let ((job (ende-ask)))
       (should (eq (ende-job-state job) 'running))
       (cl-letf (((symbol-function 'ende-chat--tool-response)
                  (lambda (&rest _)
                    (error "Historical calls must not execute again"))))
         (ende-test--complete-call (car ende-test--calls)))
       (should (eq (ende-job-state job) 'completed))
       (let* ((provider-messages (plist-get (car ende-test--calls) :messages))
              (provider-text (ende-test--chat-context-text provider-messages)))
         (should-not (ende-test--chat-protocol-messages provider-messages))
         (should (string-match-p "The saved source text" provider-text))
         (should (string-match-p "src/example\\.el" provider-text)))
       (should (string-match-p "A useful reply" (buffer-string)))))))



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
       (should-not
        (cl-some (lambda (headline)
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



(ert-deftest ende-test/chat-copied-work-containers-remain-readable ()
  (let* ((request-id "request-duplicate-work")
         (question-id "question-duplicate-work")
         (first-call '(:id "call-first" :name "read_file" :arguments "{}"))
         (second-call '(:id "call-second" :name "read_file" :arguments "{}"))
         (protocol
          (concat
           (ende-chat--work-text 2 question-id request-id)
           (ende-chat--assistant-protocol-text
            3 question-id request-id "" '(:content "First round")
            (list first-call))
           (ende-chat--tool-result-text 3 request-id first-call "First result")
           (ende-chat--work-text 2 question-id request-id)
           (ende-chat--assistant-protocol-text
            3 question-id request-id "" '(:content "Second round")
            (list second-call))
           (ende-chat--tool-result-text 3 request-id second-call "Second result")
           (ende-chat--assistant-protocol-text
            2 question-id request-id "" '(:content "Final") nil nil 'org))))
    (with-temp-buffer
      (org-mode)
      (insert "* Topic\n** User\nQuestion\n")
      (insert protocol)
      (insert "** Follow-up\nContinue\n")
      (ende-test--chat-goto "** Follow-up")
      (let* ((messages (ende-chat-context-at-point))
             (text (ende-test--chat-context-text messages)))
        (should-not (ende-test--chat-protocol-messages messages))
        (dolist (content '("First round" "First result" "Second round"
                           "Second result" "Final"))
          (should (string-match-p content text)))))))



(ert-deftest ende-test/chat-tool-history-survives-follow-up-and-compaction ()
  (let ((root (make-temp-file "ende-chat-history-" t)))
    (ende-test--reset)
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "foo.el" root)
            (insert "FOO_SOURCE_TOKEN\n(defun foo (x) x)\n"))
          (with-temp-file (expand-file-name "bar.el" root)
            (insert "BAR_SOURCE_TOKEN\n(defun bar (x) x)\n"))
          (with-temp-file (expand-file-name "baz.el" root)
            (insert "BAZ_SOURCE_TOKEN\n(defun baz (x) x)\n"))
          (let ((ende-llm-function
                 (ende-test--fake-llm
                  (list
                   (list :tool-calls
                         (list (list :id "call-A" :name "read_file"
                                     :arguments "{ \"path\" : \"foo.el\" }")
                               (list :id "call-B" :name "read_file"
                                     :arguments "{\"path\":\"bar.el\"}")))
                   (list :tool-calls
                         (list (list :id "call-C" :name "read_file"
                                     :arguments "{\"path\":\"baz.el\"}")))
                   "Final answer about the source."
                   "Follow-up answer without another lookup."
                   "Compact summary only.")
                  t)))
            (ende-test--with-buffer
	     (format
	      (concat
	       "#+begin_ende\n(root %S)\n"
	       "(group inspect project_info list_project_files read_file search_project)\n"
	       "(resource project (include \"**\"))\n"
	       "(tools inspect)\n"
	       "(set-tool-resources inspect :resources project)\n"
	       "#+end_ende\n\n"
	       "* Topic\n"
	       "** Question\nRead foo.el and bar.el.\n"
	       "** Follow-up\nWhat would happen if foo received nil?\n"
	       "** Compact\nKeep the important source behavior.\n"
	       "** Later\nContinue from the summary.\n")
	      root)
             (org-mode)
             (ende-test--chat-goto "** Question")
             (let ((job (ende-ask)))
	       (ende-test--complete-call (nth 0 ende-test--calls))
	       (should (= (length ende-test--calls) 2))
	       ;; The group starts closed while first-round results are appended.
	       (let* ((document (ende-chat--build-document))
	              (request-id
	               (plist-get (ende-job-instruction job) :request-id))
	              (work (ende-chat--request-work-section document request-id))
	              (headlines (ende-chat--document-headlines document))
	              (calls
	               (cl-remove-if-not
	                (lambda (headline)
	                  (ende-chat--tool-call-protocol-p document headline))
	                headlines))
	              (results
	               (cl-remove-if-not
	                (lambda (headline)
	                  (ende-chat--tool-result-protocol-p document headline))
	                headlines)))
	         (should work)
	         (should (equal (org-element-property :tags work)
	                        '("ignore" "noexport")))
	         (should (= (length calls) 2))
	         (should (= (length results) 2))
	         (should (ende-test--chat-headline-text-hidden-p
	                  work "--> read_file"))
	         (ende-test--chat-goto "** Agent: Work")
	         ;; Reveal child headings without opening their argument/output bodies.
	         (org-fold-show-children)
	         (dolist (headline (append calls results))
	           (should-not
	            (ende-test--chat-headline-text-hidden-p
	             headline (org-element-property :raw-value headline))))
	         (dolist (entry '(("call-A" . "{ \"path\" : \"foo.el\" }")
	                          ("call-B" . "{\"path\":\"bar.el\"}")))
	           (let ((call
	                  (cl-find-if
	                   (lambda (headline)
	                     (equal (ende-chat--property
	                             document headline :tool-call-id)
	                            (car entry)))
	                   calls)))
	             (should (ende-test--chat-headline-text-hidden-p
	                      call (cdr entry)))))
	         (dolist (entry '(("call-A" . "FOO_SOURCE_TOKEN")
	                          ("call-B" . "BAR_SOURCE_TOKEN")))
	           (let ((result
	                  (cl-find-if
	                   (lambda (headline)
	                     (equal (ende-chat--property
	                             document headline :tool-call-id)
	                            (car entry)))
	                   results)))
	             (should (ende-test--chat-headline-text-hidden-p
	                      result (cdr entry))))))
	       (ende-test--complete-call (nth 1 ende-test--calls))
	       (should (= (length ende-test--calls) 3))
	       ;; The later round appends under the user's expanded Work section.
	       (let* ((document (ende-chat--build-document))
	              (request-id
	               (plist-get (ende-job-instruction job) :request-id))
	              (work (ende-chat--request-work-section document request-id))
	              (call
	               (cl-find-if
	                (lambda (headline)
	                  (equal (ende-chat--property
	                          document headline :tool-call-id)
	                         "call-C"))
	                (ende-chat--document-headlines document)))
	              (result
	               (cl-find-if
	                (lambda (headline)
	                  (and (ende-chat--tool-result-protocol-p document headline)
	                       (equal (ende-chat--property
	                               document headline :tool-call-id)
	                              "call-C")))
	                (ende-chat--document-headlines document))))
	         (should work)
	         (should call)
	         (should result)
	         (should-not
	          (ende-test--chat-headline-text-hidden-p
	           call (org-element-property :raw-value call)))
	         (should (ende-test--chat-headline-text-hidden-p
	                  call "{\"path\":\"baz.el\"}"))
	         (should (ende-test--chat-headline-text-hidden-p
	                  result "BAZ_SOURCE_TOKEN")))
	       (ende-test--complete-call (nth 2 ende-test--calls))
	       (should (eq (ende-job-state job) 'completed)))
             (ende-test--chat-goto "** Follow-up")
             (beginning-of-line)
             (insert "** Tool Result: read_file\n"
                     "This is a user-authored note.\n\n"
                     "** Agent: Work\n"
                     "This heading is user-authored.\n\n")
             (let* ((document (ende-chat--build-document))
                    (headlines (ende-chat--document-headlines document))
                    (reply
                     (cl-find-if
                      (lambda (headline)
                        (and (ende-chat--assistant-protocol-p document headline)
                             (equal (org-element-property :raw-value headline)
                                    "Agent: Reply")
                             (null (ende-chat--work-ancestor
                                    document headline))))
                      headlines))
                    (works
                     (cl-remove-if-not
                      (lambda (headline)
			(ende-chat--work-section-p document headline))
                      headlines))
                    (work (car works))
                    (protocol-headlines
                     (cl-remove-if-not
                      (lambda (headline)
                        (or (ende-chat--assistant-protocol-p
                             document headline)
                            (ende-chat--tool-call-protocol-p document headline)
                            (ende-chat--tool-result-protocol-p document headline)))
                      headlines))
                    (calls
                     (cl-remove-if-not
                      (lambda (headline)
                        (ende-chat--tool-call-protocol-p document headline))
                      headlines))
                    (results
                     (cl-remove-if-not
                      (lambda (headline)
                        (ende-chat--tool-result-protocol-p document headline))
                      headlines)))
	       (should reply)
	       (should (= (length works) 1))
	       (should (equal (ende-chat--property document work :request-id)
                              (ende-chat--property document reply :request-id)))
	       (should (eq (ende-chat--parent document work)
                           (ende-chat--parent document reply)))
	       (should (= (org-element-property :level work)
                          (org-element-property :level reply)))
	       (should (< (org-element-property :begin work)
                          (org-element-property :begin reply)))
	       (should-not
	        (ende-test--chat-headline-text-hidden-p work "--> read_file"))
	       (let ((export (org-export-as 'ascii)))
	         (should-not (string-match-p "--> read_file" export))
	         (should-not (string-match-p "FOO_SOURCE_TOKEN" export))
	         (should
	          (string-match-p "Final answer about the source" export)))
	       (should
                (eq reply
                    (cadr (memq work
                                (ende-chat--children
                                 document (ende-chat--parent document work))))))
	       (dolist (child (ende-chat--children document work))
                 (should (equal (ende-chat--property document child :request-id)
                                (ende-chat--property document work :request-id))))
	       (should
                (equal (mapcar (lambda (headline)
                                 (org-element-property :raw-value headline))
			       protocol-headlines)
		       '("--> read_file" "--> read_file"
                         "read_file: path=\"foo.el\""
                         "read_file: path=\"bar.el\""
                         "--> read_file" "read_file: path=\"baz.el\""
                         "Agent: Reply")))
	       (should (equal (mapcar (lambda (headline)
                                        (ende-chat--property
                                         document headline :tool-call-id))
                                      calls)
                              '("call-A" "call-B" "call-C")))
	       (should (equal (mapcar (lambda (headline)
                                        (ende-chat--property
                                         document headline :tool-call-id))
                                      results)
                              '("call-A" "call-B" "call-C")))
	       (dolist (headline (append calls results))
	         (should (eq (ende-chat--parent document headline) work))
	         (should (= (org-element-property :level headline) 3))
	         (should-not
	          (ende-test--chat-headline-text-hidden-p
	           headline (org-element-property :raw-value headline))))
	       (should
                (ende-test--chat-headline-text-hidden-p
                 work "FOO_SOURCE_TOKEN"))
	       (should-not
                (ende-test--chat-headline-text-hidden-p
                 reply "Final answer about the source."))
	       (let ((user-note
                      (cl-find-if
		       (lambda (headline)
                         (and (equal (org-element-property :raw-value headline)
                                     "Tool Result: read_file")
                              (null (ende-chat--property
                                     document headline :tool-call-id))))
		       headlines)))
                 (should user-note)
                 (should-not
                  (ende-test--chat-headline-text-hidden-p
                   user-note "This is a user-authored note.")))
	       (ende-test--chat-goto "** Follow-up")
               (let* ((messages (ende-chat-context-at-point))
                      (text (ende-test--chat-context-text messages))
                      (reply-index
		       (cl-position-if
			(lambda (message)
			  (and (consp message) (stringp (car message))
			       (equal (car message) "assistant")
			       (string-match-p "Final answer about the source"
					       (cdr message))))
			messages)))
		 (should-not (ende-test--chat-protocol-messages messages))
		 (should reply-index)
		 (should (string-match-p "FOO_SOURCE_TOKEN" text))
		 (should (string-match-p "Arguments:" text))
		 (should (string-match-p "Output:" text))
		 (should (equal (ende-test--chat-message-role (car (last messages)))
				"user"))
		 (should
                  (cl-some
                   (lambda (message)
                     (and (consp message) (stringp (car message))
                          (string-match-p "This heading is user-authored"
                                          (cdr message))))
                   messages))
		 (should
                  (cl-every (lambda (role)
                              (member role '("user" "assistant")))
                            (mapcar #'ende-test--chat-message-role messages)))
		 (ende-test--chat-goto "** Agent: Work")
		 (org-fold-show-subtree)
		 (ende-test--chat-goto "** Follow-up")
		 (should (equal messages (ende-chat-context-at-point)))
		 (ende-test--chat-goto "** Agent: Work")
		 (org-fold-hide-subtree)
		 (ende-test--chat-goto "** Follow-up")
		 (let ((follow-up (ende-ask)))
                   (should (eq (ende-job-state follow-up) 'running))
                   (ende-test--complete-call (nth 3 ende-test--calls))
                   (should (eq (ende-job-state follow-up) 'completed))
                   (let ((provider-messages
                          (plist-get (nth 3 ende-test--calls) :messages)))
                     (should-not
                      (ende-test--chat-protocol-messages provider-messages))
                     (should
                      (string-match-p
                       "Tool Result: read_file"
                       (ende-test--chat-context-text provider-messages)))))
		 (ende-test--chat-goto "** Compact")
		 (let ((compaction (ende-compact)))
		   (should (= (length ende-test--calls) 5))
                   (let* ((compaction-messages
                           (plist-get (nth 4 ende-test--calls) :messages))
                          (compaction-text
                           (ende-test--chat-context-text compaction-messages)))
                     (should-not
                      (ende-test--chat-protocol-messages compaction-messages))
                     (should (string-match-p "FOO_SOURCE_TOKEN" compaction-text))
                     (should (string-match-p "Arguments:" compaction-text)))
		   (ende-test--complete-call (nth 4 ende-test--calls))
		   (should (eq (ende-job-state compaction) 'completed)))
		 (ende-test--chat-goto "** Later")
		 (let* ((messages (ende-chat-context-at-point))
			(text (ende-test--chat-context-text messages)))
		   (should (string-match-p "Compact summary only" text))
		   (should-not (ende-test--chat-protocol-messages messages))
		   (should-not (string-match-p "FOO_SOURCE_TOKEN" text))
		   (should-not (string-match-p "call-A" text))))))))
      (ende-test--reset)
      (when (file-directory-p root)
	(delete-directory root t)))))



(ert-deftest ende-test/chat-tool-failure-keeps-completed-protocol-history ()
  (let ((root (make-temp-file "ende-chat-partial-tools-" t)))
    (ende-test--reset)
    (unwind-protect
        (let ((ende-llm-function
               (ende-test--fake-llm
                (list
                 (list :tool-calls
                       (list (list :id "call-A" :name "read_file"
                                   :arguments "{\"path\":\"a.el\"}")
                             (list :id "call-B" :name "read_file"
                                   :arguments "{\"path\":\"b.el\"}")))
                 "This must not be requested")
                t)))
          (ende-test--with-buffer
           (format (concat "#+begin_ende\n(root %S)\n"
                           "(group inspect read_file)\n"
                           "(resource project (include \"**\"))\n"
                           "(tools inspect)\n"
                           "(set-tool-resources inspect :resources project)\n"
                           "#+end_ende\n\n"
                           "* Topic\n** Question\nRead both files.\n"
                           "** Follow-up\nAsk about the results.\n")
                   root)
           (org-mode)
           (ende-test--chat-goto "** Question")
           (let ((job (ende-ask))
                 (completed nil))
             (cl-letf
                 (((symbol-function 'ende-chat--tool-response)
                   (lambda (call _configuration)
                     (if (equal (ende-chat--tool-value call :id) "call-A")
                         (progn (setq completed t) "result-A")
		       (error "second tool failed")))))
	       (ende-test--complete-call (car ende-test--calls)))
             (should completed)
             (should (eq (ende-job-state job) 'failed))
             (let* ((document (ende-chat--build-document))
                    (work
                     (cl-find-if
                      (lambda (headline)
                        (ende-chat--work-section-p document headline))
                      (ende-chat--document-headlines document)))
                    (reply
                     (cl-find-if
                      (lambda (headline)
                        (and (equal (org-element-property :raw-value headline)
                                    "Agent: Reply")
                             (equal (ende-chat--property document headline :request-id)
                                    (plist-get (ende-job-instruction job) :request-id))
                             (null (ende-chat--work-ancestor document headline))))
                      (ende-chat--document-headlines document)))
                    (calls
                     (cl-remove-if-not
                      (lambda (headline)
                        (ende-chat--tool-call-protocol-p document headline))
                      (ende-chat--document-headlines document)))
                    (results
                     (cl-remove-if-not
                      (lambda (headline)
                        (ende-chat--tool-result-protocol-p document headline))
                      (ende-chat--document-headlines document))))
	       (should work)
	       (should-not reply)
	       (should-not
	        (cl-some (lambda (headline)
	                   (and (ende-chat--assistant-protocol-p document headline)
	                        (ende-chat--work-ancestor document headline)))
	                 (ende-chat--document-headlines document)))
	       (should (equal (ende-chat--property document work :state)
                              "failed"))
	       (should (equal (mapcar (lambda (call)
                                        (ende-chat--property
                                         document call :tool-call-id))
                                      calls)
                              '("call-A" "call-B")))
	       (should (equal (mapcar (lambda (result)
                                        (ende-chat--property
                                         document result :tool-call-id))
                                      results)
                              '("call-A")))
	       (should (eq (ende-chat--parent document (car calls)) work))
	       (should (eq (ende-chat--parent document (cadr calls)) work))
	       (should (eq (ende-chat--parent document (car results)) work))
	       (should (< (org-element-property :begin (cadr calls))
	                  (org-element-property :begin (car results))))
	       (should (equal (mapcar (lambda (call)
					(ende-chat--protocol-body-string call nil 'literal))
	                              calls)
	                      '("{\"path\":\"a.el\"}" "{\"path\":\"b.el\"}")))
	       (should (eq (ende-chat--validate-work-section document work) work))
	       (should-not
                (string-match-p "This must not be requested" (buffer-string)))))))
      (ende-test--reset)
      (when (file-directory-p root)
        (delete-directory root t)))))



(ert-deftest ende-test/chat-tool-only-round-preserves-call-provenance ()
  (let ((root (make-temp-file "ende-chat-tool-provenance-" t)))
    (ende-test--reset)
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "one.el" root)
            (insert "SOURCE_ONE\n"))
          (with-temp-file (expand-file-name "two.el" root)
            (insert "SOURCE_TWO\n"))
          (let* ((calls
                  (list (list :id "tool-call-one" :name "read_file"
                              :arguments "{\"path\":\"one.el\"}")
                        (list :id "tool-call-two" :name "read_file"
                              :arguments "{\"path\":\"two.el\"}")))
                 (ende-llm-function
                  (ende-test--fake-llm
                   (list (list :content "" :reported-model "tool-model"
                               :tool-calls calls)
                         (list :content "Answer from another model."
                               :reported-model "answer-model"))
                   t))
                 (profile
                  '(:name local :model "requested-model"
			  :reasoning-effort "high"
			  :controls ((:temperature :path ("temperature"))
                                     (:reasoning-effort
                                      :path ("reasoning" "effort")
                                      :choices ("high"))))))
            (ende-test--with-buffer
             (format (concat "#+begin_ende\n(root %S)\n"
                             "(group inspect read_file)\n"
                             "(resource project (include \"**\"))\n"
                             "(tools inspect)\n"
                             "(set-tool-resources inspect :resources project)\n"
                             "#+end_ende\n\n"
                             "* Topic\n** Question\nRead both files.\n"
                             "** Follow-up\nContinue from those files.\n")
                     root)
             (org-mode)
             (ende-test--chat-goto "** Question")
             (let ((job (ende-ask profile)))
               (ende-test--complete-call (car ende-test--calls))
               (should (= (length ende-test--calls) 2))
               (ende-test--complete-call (cadr ende-test--calls))
               (should (eq (ende-job-state job) 'completed)))
             (let* ((document (ende-chat--build-document))
                    (headlines (ende-chat--document-headlines document))
                    (work (cl-find-if
                           (lambda (headline)
                             (ende-chat--work-section-p document headline))
                           headlines))
                    (tool-calls
                     (cl-remove-if-not
                      (lambda (headline)
                        (ende-chat--tool-call-protocol-p document headline))
                      headlines))
                    (tool-results
                     (cl-remove-if-not
                      (lambda (headline)
                        (ende-chat--tool-result-protocol-p document headline))
                      headlines))
                    (final-reply
                     (cl-find-if
                      (lambda (headline)
                        (and (ende-chat--assistant-protocol-p document headline)
                             (null (ende-chat--work-ancestor document headline))))
                      headlines)))
               (should work)
               (should (= (length tool-calls) 2))
               (should (= (length tool-results) 2))
               (should final-reply)
               (should-not
                (cl-some (lambda (headline)
                           (and (ende-chat--assistant-protocol-p document headline)
                                (ende-chat--work-ancestor document headline)))
                         headlines))
               (should (< (org-element-property :begin (cadr tool-calls))
                          (org-element-property :begin (car tool-results))))
               (should (equal
                        (mapcar (lambda (call)
                                  (ende-chat--protocol-body-string call nil 'literal))
                                tool-calls)
                        '("{\"path\":\"one.el\"}"
                          "{\"path\":\"two.el\"}")))
               (dolist (call tool-calls)
                 (should (eq (ende-chat--parent document call) work))
                 (should (equal
                          (ende-test--chat-headline-property
                           call ende-chat-model-property)
                          "tool-model"))
                 (should (equal
                          (ende-test--chat-headline-property
                           call ende-chat-reasoning-effort-property)
                          "high"))
                 (should-not
                  (string-match-p
                   "ENDE_MODEL\\|ENDE_REASONING_EFFORT\\|tool-model\\|high"
                   (ende-chat--protocol-body-string call nil 'literal))))
               (should (equal
                        (ende-test--chat-headline-property
                         final-reply ende-chat-model-property)
                        "answer-model"))
               (should (equal
                        (ende-test--chat-headline-property
                         final-reply ende-chat-reasoning-effort-property)
                        "high"))
               (should (cl-every #'ende-test--chat-control-drawer-hidden-p
                                 (ende-test--chat-control-drawers))))
             (ende-test--chat-goto "** Follow-up")
             (let ((context (ende-test--chat-context-text
                             (ende-chat-context-at-point))))
               (should (string-match-p "SOURCE_ONE" context))
               (should (string-match-p "SOURCE_TWO" context))
               (dolist (private-value
                        (list "tool-model" "answer-model" "high"
                              ende-chat-model-property
                              ende-chat-reasoning-effort-property))
                 (should-not (string-match-p (regexp-quote private-value)
                                             context)))))))
      (ende-test--reset)
      (when (file-directory-p root)
        (delete-directory root t)))))

(provide 'ende-chat-tools-test)

;;; ende-chat-tools-test.el ends here
