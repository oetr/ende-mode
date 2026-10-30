;;; ende-chat-tool-store-test.el --- External chat tool history tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'ende)
(require 'ende-test-helpers)
(require 'ende-chat-test-helpers)

(defun ende-test--stored-tool-history (entries &optional normalized)
  "Insert completed tool ENTRIES with optional NORMALIZED assistant content."
  (let* ((question-id "question-tool-store")
         (request-id "request-tool-store")
         (calls (mapcar (lambda (entry)
                          (ende-chat--prepare-tool-call (car entry)))
                        entries)))
    (insert (ende-chat--work-text 2 question-id request-id)
            (ende-chat--assistant-protocol-text
             3 question-id request-id ""
             (or normalized '(:content "Checking both sources."))
             calls))
    (cl-mapc (lambda (call entry)
               (insert (ende-chat--tool-result-text
                        3 request-id call (cdr entry))))
             calls entries)
    (insert (ende-chat--assistant-text
             2 question-id request-id "" '(:content "Final stored answer.")))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^\\*\\* Agent: Work")
      (org-back-to-heading t)
      (org-entry-delete nil ende-chat-state-property))
    calls))

(defun ende-test--tool-headlines (document kind)
  "Return tool references of KIND in DOCUMENT order."
  (cl-remove-if-not
   (if (eq kind 'call)
       (lambda (headline) (ende-chat--tool-call-protocol-p document headline))
     (lambda (headline) (ende-chat--tool-result-protocol-p document headline)))
   (ende-chat--document-headlines document)))

(defun ende-test--stored-tool-messages (messages)
  "Return assistant tool-call messages from serialized MESSAGES."
  (cl-remove-if-not
   (lambda (message)
     (and (equal (plist-get message :role) "assistant")
          (plist-member message :tool-calls)))
   messages))

(ert-deftest ende-test/chat-editable-history-preserves-roles-and-tool-turns ()
  (dolist (final-role '(nil "user"))
    (ende-test--reset)
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "* Topic\n** Question\nLook up a source.\n")
          (ende-test--stored-tool-history
           (list (cons '(:id "history-call" :name "read_file"
                         :arguments "{\"path\":\"source.el\"}")
                       "HISTORY_RESULT"))
           (ende-chat--normalize-response
            (concat "Checking the source.\n"
                    "* User note\nUser-owned reference.\n"
                    "* Private note\nHidden generated detail.\n"
                    "* Resumed assistant\nAssistant continuation.\n"
                    "#+begin_src org\n* Literal heading\n"
                    "#+ENDE_ARTIFACT: literal-not-an-artifact\n")
            3 t))
          (ende-test--chat-goto "**** User note")
          (org-entry-put nil ende-chat-role-property "user")
          (goto-char (point-min))
          (search-forward "User-owned reference.")
          (end-of-line)
          (insert "\n#+begin_example\n,* User-authored escape\n#+end_example")
          (ende-test--chat-goto "**** Private note")
          (org-entry-put nil ende-chat-visibility-property "hidden")
          (ende-test--chat-goto "** Agent: Reply")
          (if final-role
              (progn
                (org-entry-put nil ende-chat-role-property final-role)
                (forward-line 1)
                (search-forward "Final stored answer.")
                (replace-match "User correction retaining request metadata."))
            (org-cut-subtree))
          (goto-char (point-max))
          (insert "** User follow-up\n:PROPERTIES:\n:ENDE_ROLE: user\n:END:\n"
                  "User follow-up text.\n"
                  "** Later question\nWhat next?\n")
          (ende-test--chat-goto "** Later question")
          (set-buffer-modified-p nil)
          (let* ((before (buffer-string))
                 (messages (ende-chat-context-at-point))
                 (roles (mapcar #'ende-content-role messages))
                 (text (ende-chat--render-context messages))
                 (turn (car (ende-test--stored-tool-messages messages)))
                 (user (cl-find-if
                        (lambda (message)
                          (string-match-p "User-owned reference"
                                          (ende-content-readable
                                           (ende-content-body message))))
                        messages)))
            (should (equal roles
                           (append '("user" "assistant" "user" "assistant" "tool")
                                   (if final-role '("user" "user" "user")
                                     '("user" "user")))))
            (should (equal (ende-content-role user) "user"))
            (should (string-match-p "\\*\\*\\*\\* User note"
                                    (ende-content-body user)))
            (should (string-match-p ",\\* User-authored escape"
                                    (ende-content-body user)))
            (should (string-match-p "\\* Resumed assistant"
                                    (plist-get turn :content)))
            (should (string-match-p "\n\\* Literal heading"
                                    (plist-get turn :content)))
            (should (string-match-p "literal-not-an-artifact"
                                    (plist-get turn :content)))
            (should-not (string-match-p "end_src" (plist-get turn :content)))
            (should (equal (plist-get (car (plist-get turn :tool-calls)) :id)
                           "history-call"))
            (should (equal (plist-get (nth 4 messages) :tool-call-id)
                           "history-call"))
            (should (equal (plist-get (nth 4 messages) :content)
                           "HISTORY_RESULT"))
            (should-not (string-match-p "Hidden generated detail" text))
            (when final-role
              (should (equal (ende-content-role (nth 5 messages)) "user"))
              (should (string-match-p "User correction"
                                      (ende-content-body (nth 5 messages)))))
            (should (equal before (buffer-string)))
            (should-not (buffer-modified-p))))
      (ende-test--reset))))

(ert-deftest ende-test/tool-store-org-setting-initializes-and-refreshes ()
  (let* ((directory (make-temp-file "ende-tool-store-org-" t))
         (ende-tool-store-file (expand-file-name "global.sqlite3" directory)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (setq buffer-file-name (expand-file-name "chat.org" directory))
          (setq default-directory temporary-file-directory)
          (insert "* Conversation\n** Settings elsewhere\n"
                  "#+begin_src text\n#+ENDE_TOOL_STORE_FILE: ignored.sqlite3\n#+end_src\n"
                  "#+begin_example\n#+ENDE_TOOL_STORE_FILE: ignored-too.sqlite3\n#+end_example\n")
          (ende-mode 1)
          (should-not ende-tool-store--local-file)
          (should (equal (ende-tool-store--filename) ende-tool-store-file))
          (goto-char (point-max))
          (insert "#+ENDE_TOOL_STORE_FILE: selected.sqlite3\n")
          (ende-mode 1)
          (should (equal (ende-tool-store--filename)
                         (expand-file-name "selected.sqlite3" directory)))
          (goto-char (point-max))
          (forward-line -1)
          (delete-region (point) (point-max))
          (insert "#+ENDE_TOOL_STORE_FILE: refreshed.sqlite3\n")
          (forward-line -1)
          (org-ctrl-c-ctrl-c)
          (should (equal (ende-tool-store--filename)
                         (expand-file-name "refreshed.sqlite3" directory)))
          (should (= (cl-count #'ende-tool-store--initialize-buffer org-mode-hook) 1))
          (ende-mode -1)
          (should-not (memq #'ende-tool-store--initialize-buffer org-mode-hook))
          (should-not (local-variable-p 'ende-tool-store--local-file))
          (org-mode)
          (should-not ende-tool-store--local-file)
          (ende-mode 1)
          (should (equal (ende-tool-store--filename)
                         (expand-file-name "refreshed.sqlite3" directory)))
          (dotimes (_ 2) (ende-reload))
          (should ende-mode)
          (should (= (cl-count #'ende-tool-store--initialize-buffer org-mode-hook) 1))
          (should (equal (ende-tool-store--filename)
                         (expand-file-name "refreshed.sqlite3" directory)))
          (ende-mode -1)
          (should-not (memq #'ende-tool-store--initialize-buffer org-mode-hook))
          (should-not (local-variable-p 'ende-tool-store--local-file))
          (ende-mode 1)
          (should (= (cl-count #'ende-tool-store--initialize-buffer org-mode-hook) 1))
          (should (equal (ende-tool-store--filename)
                         (expand-file-name "refreshed.sqlite3" directory)))
          (goto-char (point-max))
          (let ((start (point)))
            (insert "#+ENDE_TOOL_STORE_FILE: duplicate.sqlite3\n")
            (should-error (ende-tool-store--initialize-buffer) :type 'user-error)
            (delete-region start (point-max)))
          (forward-line -1)
          (delete-region (point) (point-max))
          (insert "#+ENDE_TOOL_STORE_FILE:\n")
          (should-error (ende-tool-store--initialize-buffer) :type 'user-error)
          (forward-line -1)
          (delete-region (point) (point-max))
          (insert "#+ENDE_TOOL_STORE_FILE: relative.sqlite3\n")
          (setq buffer-file-name nil)
          (should-error (ende-tool-store--initialize-buffer) :type 'user-error))
      (delete-directory directory t))))

(ert-deftest ende-test/chat-inspection-observes-sections-without-submitting ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("Should not be called"))))
    (unwind-protect
        (save-window-excursion
          (with-temp-buffer
            (org-mode)
            (insert "Document preamble.\n* Topic\n** Question\nOriginal question.\n")
            (ende-test--stored-tool-history
             (list (cons '(:id "inspect-call" :name "read_file" :arguments "{}")
                         "INSPECT_RESULT"))
             '(:content "Checking.\n**** Work detail\nWork reference."))
            (goto-char (point-max))
            (insert "*** Reply reference\nAssistant reference.\n"
                    "*** User correction\n:PROPERTIES:\n:ENDE_ROLE: user\n:END:\n"
                    "Independent correction.\n"
                    "*** Private\n:PROPERTIES:\n:ENDE_VISIBILITY: hidden\n:END:\n"
                    "INSPECT_PRIVATE\n"
                    "** Follow-up\nSelected user body.\n"
                    "*** Reference\nSelected user reference.\n"
                    "*** Independent turn\n:PROPERTIES:\n:ENDE_ROLE: user\n:END:\n"
                    "INSPECT_LATER_CHILD\n"
                    "** Later question\nINSPECT_LATER_SIBLING\n"
                    (ende-chat--assistant-text
                     2 "later-question" "later-request" ""
                     '(:content "INSPECT_LATER_REPLY")))
            (dolist (heading '(nil "** Agent: Work" "**** Work detail"
                               "** Agent: Reply" "*** User correction"
                               "** Follow-up"))
              (if heading (ende-test--chat-goto heading)
                (goto-char (point-min)))
              (set-buffer-modified-p (equal heading "** Agent: Work"))
              (let* ((source (current-buffer))
                     (before (buffer-string))
                     (modified (buffer-modified-p))
                     (cursor (point))
                     (queue (copy-sequence ende-queue))
                     (next-id ende-chat--next-id)
                     (messages (ende-chat-context-at-point))
                     (text (ende-chat--render-context messages))
                     (system (ende-chat--system-prompt (ende-chat--build-document)))
                     (work (member heading '("** Agent: Work" "**** Work detail"))))
                (if (null heading)
                    (should-not messages)
                  (should (string-match-p "INSPECT_RESULT" text))
                  (should (equal (mapcar #'ende-content-role messages)
                                 (append '("user" "assistant" "tool")
                                         (unless work '("assistant" "user"))
                                         (when (equal heading "** Follow-up")
                                           '("user")))))
                  (if work
                      (should-not (string-match-p "Final stored answer" text))
                    (should (string-match-p "Final stored answer" text)))
                  (if (equal heading "** Follow-up")
                      (progn
                        (should (string-match-p "Selected user body" text))
                        (should (string-match-p "Selected user reference" text)))
                    (should-not (string-match-p "Selected user body" text))
                    (should-error (ende-ask) :type 'user-error)))
                (should-not (string-match-p "INSPECT_PRIVATE\\|INSPECT_LATER" text))
                (save-current-buffer
                  (ende-inspect-context-at-point))
                (with-current-buffer "*ende context*"
                  (should buffer-read-only)
                  (should (derived-mode-p 'special-mode))
                  (should (equal (buffer-string)
                                 (ende-chat--render-context
                                  (cons (cons "system" system) messages)))))
                (with-current-buffer source
                  (should (equal before (buffer-string)))
                  (should (eq modified (buffer-modified-p)))
                  (should (= cursor (point))))
                (should (= next-id ende-chat--next-id))
                (should (equal queue ende-queue))
                (should-not (ende-running-jobs))
                (should-not ende-test--calls)))))
      (when (get-buffer "*ende context*")
        (kill-buffer "*ende context*"))
      (ende-test--reset))))

(ert-deftest ende-test/chat-inspect-context-renders-persisted-tool-history ()
  (ende-test--reset)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* Topic\n"
                "** Earlier user\nEarlier user context.\n"
                "** Earlier assistant\n"
                ":PROPERTIES:\n:ENDE_ROLE: assistant\n:END:\n"
                "Earlier assistant context.\n"
                "** Question\nCurrent inspection question.\n")
        (ende-test--stored-tool-history
         (list (cons '(:id "provider-inspect-call" :name "read_file"
                       :arguments "{\"path\":\"ende-stream.el\",\"start_line\":12,\"line_count\":3}")
                     "{\"ok\":true,\"content\":\"INSPECTED_TOOL_RESULT\"}")))
        (goto-char (point-max))
        (insert "** Follow-up\nFollow-up question.\n")
        (ende-test--chat-goto "** Follow-up")
        (let ((source (current-buffer))
              (source-text (buffer-string))
              (queued-before (length ende-queue)))
          (ende-inspect-context-at-point)
          (with-current-buffer "*ende context*"
            (let ((text (buffer-string)))
              (should (derived-mode-p 'special-mode))
              (should buffer-read-only)
              (should (string-match-p
                       (regexp-quote "Answer the user's requests using the supplied role-tagged conversation.")
                       text))
              (should (< (string-match "Earlier user context" text)
                         (string-match "Earlier assistant context" text)))
              (should (< (string-match "Earlier assistant context" text)
                         (string-match "Checking both sources" text)))
              (should (< (string-match "Checking both sources" text)
                         (string-match "Tool call: read_file" text)))
              (should (< (string-match "Tool call: read_file" text)
                         (string-match "TOOL RESULT" text)))
              (should (< (string-match "TOOL RESULT" text)
                         (string-match "Final stored answer" text)))
              (should (< (string-match "Final stored answer" text)
                         (string-match "Follow-up question" text)))
              (should (string-match-p "Tool call: read_file" text))
              (should (string-match-p "provider-inspect-call" text))
              (should (string-match-p "Arguments:\n{" text))
              (should (string-match-p "\n  \\\"path\\\":" text))
              (should (string-match-p "INSPECTED_TOOL_RESULT" text))
              (should (string-match-p "Follow-up question" text))))
          (should (= queued-before (length ende-queue)))
          (should (equal source-text
                         (with-current-buffer source (buffer-string))))))
    (when (get-buffer "*ende context*")
      (kill-buffer "*ende context*"))
    (ende-test--reset)))


(ert-deftest ende-test/chat-parallel-tool-history-keeps-payloads-out-of-org ()
  (ende-test--reset)
  (unwind-protect
      (let* ((raw-a "{ \"path\" : \"alpha.el\", \"proof\" : \"RAW_ARGS_ALPHA\" }")
             (raw-b (json-encode
                     `((path . ,(concat "beta/" (make-string 240 ?b)))
                       (proof . "RAW_ARGS_BETA"))))
             (result-a "{\"body\":\"RAW_RESULT_ALPHA\",\"line\":1}")
             (result-b "{\"body\":\"RAW_RESULT_BETA\",\"line\":2}")
             (specs
              (list (cons (list :id "provider-A" :name "read_file"
                                :arguments raw-a)
                          result-a)
                    (cons (list :id "provider-B" :name "read_file"
                                :arguments raw-b)
                          result-b))))
        (with-temp-buffer
          (org-mode)
          (insert "* Topic\n** Question\nInspect both files.\n")
          (ende-test--stored-tool-history specs)
          (insert "** Follow-up\nWhat did they contain?\n")
          (let* ((document (ende-chat--build-document))
                 (calls (ende-test--tool-headlines document 'call))
                 (results (ende-test--tool-headlines document 'result))
                 (first-id (ende-chat--property
                            document (car calls) :tool-execution-id))
                 (second-id (ende-chat--property
                             document (cadr calls) :tool-execution-id))
                 (result-a
                  (cl-find-if
                   (lambda (headline)
                     (equal (ende-chat--property
                             document headline :tool-execution-id)
                            first-id))
                   results))
                 (result-b
                  (cl-find-if
                   (lambda (headline)
                     (equal (ende-chat--property
                             document headline :tool-execution-id)
                            second-id))
                   results))
                 (result-b-end
                  (copy-marker (org-element-property :end result-b)))
                 (result-a-text
                  (buffer-substring-no-properties
                   (org-element-property :begin result-a)
                   (org-element-property :end result-a))))
            (delete-region (org-element-property :begin result-a)
                           (org-element-property :end result-a))
            (goto-char result-b-end)
            (insert result-a-text)
            (set-marker result-b-end nil))
          (let* ((org-text (buffer-string))
                   (document (ende-chat--build-document))
                   (call-headlines (ende-test--tool-headlines document 'call))
                   (result-headlines (ende-test--tool-headlines document 'result))
                   (execution-ids
                    (mapcar (lambda (headline)
                              (ende-chat--property
                               document headline :tool-execution-id))
                            call-headlines)))
              (should (= (length call-headlines) 2))
              (should (= (length result-headlines) 2))
              (should (= (length (delete-dups (copy-sequence execution-ids))) 2))
              (should (string-match-p "path=\\\"alpha.el\\\"" org-text))
              (should (string-match-p "path=" org-text))
              (should-not (string-match-p (regexp-quote raw-a) org-text))
              (should-not (string-match-p (regexp-quote raw-b) org-text))
              (should-not (string-match-p (regexp-quote result-a) org-text))
              (should-not (string-match-p (regexp-quote result-b) org-text))
              (should-not (string-match-p "ENDE_TOOL_CALL_ID" org-text))
              (dolist (headline call-headlines)
                (should (<= (string-width
                             (org-element-property :raw-value headline)) 180)))
              (ende-test--chat-goto "** Follow-up")
              (let* ((messages (ende-chat-context-at-point))
                     (assistant (car (ende-test--stored-tool-messages messages)))
                     (tool-results
                      (cl-remove-if-not
                       (lambda (message)
                         (equal (plist-get message :role) "tool"))
                       messages))
                     (stored-calls (plist-get assistant :tool-calls)))
                (should (= (length (ende-test--stored-tool-messages messages)) 1))
                (should (equal (plist-get assistant :content)
                               "Checking both sources."))
                (should (equal (mapcar (lambda (call) (plist-get call :id))
                                       stored-calls)
                               '("provider-A" "provider-B")))
                (should (equal (mapcar (lambda (call)
                                        (plist-get call :arguments))
                                      stored-calls)
                               (mapcar (lambda (entry)
                                         (plist-get (car entry) :arguments))
                                       specs)))
                (should (equal (mapcar (lambda (message)
                                          (plist-get message :tool-call-id))
                                        tool-results)
                                '("provider-B" "provider-A")))
                (should (equal (mapcar (lambda (message)
                                          (plist-get message :content))
                                        tool-results)
                                (list result-b result-a)))
              (should (equal (cdr (car (last messages)))
                             "** Follow-up\nWhat did they contain?\n"))))))
    (ende-test--reset)))

(ert-deftest ende-test/chat-org-edits-control-complete-tool-exchanges ()
  (ende-test--reset)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* Topic\n** Question\nInspect both files.\n")
        (let* ((raw-a "{\"path\":\"a.el\"}")
               (raw-b "{\"path\":\"b.el\"}")
               (calls
                (ende-test--stored-tool-history
                 (list (cons (list :id "provider-A" :name "read_file"
                                   :arguments raw-a)
                             "RESULT_A")
                       (cons (list :id "provider-B" :name "read_file"
                                   :arguments raw-b)
                             "RESULT_B"))))
               (original-records
                (mapcar (lambda (call)
                          (let ((id (ende-tools-call-value call :execution-id)))
                            (cons (ende-tool-store-get id 'request)
                                  (ende-tool-store-get id 'result))))
                        calls)))
          (insert "** Follow-up\nContinue.\n")
          (buffer-enable-undo)
          (let* ((document (ende-chat--build-document))
                 (call-a
                  (cl-find-if
                   (lambda (headline)
                     (equal (ende-chat--property
                             document headline :tool-execution-id)
                            (ende-tools-call-value (car calls) :execution-id)))
                   (ende-test--tool-headlines document 'call))))
            (goto-char (org-element-property :begin call-a))
            (org-edit-headline "--> read_file: user-edited summary")
            (ende-test--chat-goto "** Follow-up")
            (let* ((messages (ende-chat-context-at-point))
                   (calls-in-history
                    (cl-loop for message in (ende-test--stored-tool-messages messages)
                             append (plist-get message :tool-calls))))
              (should (equal (mapcar (lambda (call)
                                      (plist-get call :arguments))
                                    calls-in-history)
                             (list raw-a raw-b)))))
          (let* ((document (ende-chat--build-document))
                 (result-a
                  (cl-find-if
                   (lambda (headline)
                     (equal (ende-chat--property
                             document headline :tool-execution-id)
                            (ende-tools-call-value (car calls) :execution-id)))
                   (ende-test--tool-headlines document 'result))))
            (undo-boundary)
            (delete-region (org-element-property :begin result-a)
                           (org-element-property :end result-a))
            (undo-boundary)
            (ende-test--chat-goto "** Follow-up")
            (let* ((messages (ende-chat-context-at-point))
                   (calls-in-history
                    (cl-loop for message in (ende-test--stored-tool-messages messages)
                             append (plist-get message :tool-calls))))
              (should (equal (mapcar (lambda (call) (plist-get call :id))
                                     calls-in-history)
                             '("provider-B")))
              (should (equal (mapcar (lambda (message)
                                       (plist-get message :content))
                                     (cl-remove-if-not
                                      (lambda (message)
                                        (equal (plist-get message :role) "tool"))
                                      messages))
                             '("RESULT_B"))))
            (undo)
            (ende-test--chat-goto "** Follow-up")
            (let ((messages (ende-chat-context-at-point)))
              (should (= (length (cl-loop for message in
                                           (ende-test--stored-tool-messages messages)
                                           append (plist-get message :tool-calls)))
                         2)))
            (let* ((document (ende-chat--build-document))
                   (call-b
                    (cl-find-if
                     (lambda (headline)
                       (equal (ende-chat--property
                               document headline :tool-execution-id)
                              (ende-tools-call-value (cadr calls) :execution-id)))
                     (ende-test--tool-headlines document 'call)))
                   (result-b
                    (cl-find-if
                     (lambda (headline)
                       (equal (ende-chat--property
                               document headline :tool-execution-id)
                              (ende-tools-call-value (cadr calls) :execution-id)))
                     (ende-test--tool-headlines document 'result)))
                   (call-copy (buffer-substring-no-properties
                               (org-element-property :begin call-b)
                               (org-element-property :end call-b)))
                   (result-copy (buffer-substring-no-properties
                                 (org-element-property :begin result-b)
                                 (org-element-property :end result-b)))
                   (final
                    (cl-find-if
                     (lambda (headline)
                       (and (ende-chat--assistant-protocol-p document headline)
                            (null (ende-chat--work-ancestor document headline))))
                     (ende-chat--document-headlines document))))
              (goto-char (org-element-property :begin final))
              (insert call-copy result-copy))
            (ende-test--chat-goto "** Follow-up")
            (let ((messages (ende-chat-context-at-point)))
              (should (= (length (cl-loop for message in
                                           (ende-test--stored-tool-messages messages)
                                           append (plist-get message :tool-calls)))
                         3)))
            (let* ((document (ende-chat--build-document))
                   (copied-call
                    (car (last (ende-test--tool-headlines document 'call)))))
              (undo-boundary)
              (delete-region (org-element-property :begin copied-call)
                             (org-element-property :end copied-call))
              (undo-boundary)
              (ende-test--chat-goto "** Follow-up")
              (let ((messages (ende-chat-context-at-point)))
                (should (= (length (cl-loop for message in
                                             (ende-test--stored-tool-messages messages)
                                             append (plist-get message :tool-calls)))
                           2)))
              (undo)
              (ende-test--chat-goto "** Follow-up")
              (let ((messages (ende-chat-context-at-point)))
                (should (= (length (cl-loop for message in
                                             (ende-test--stored-tool-messages messages)
                                             append (plist-get message :tool-calls)))
                           3)))
              (should (equal original-records
                             (mapcar
                              (lambda (call)
                                (let ((id (ende-tools-call-value
                                           call :execution-id)))
                                  (cons (ende-tool-store-get id 'request)
                                        (ende-tool-store-get id 'result))))
                               calls)))))))
    (ende-test--reset)))

(ert-deftest ende-test/chat-tool-pairing-stops-at-assistant-turn-boundaries ()
  (ende-test--reset)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* Topic\n** Question\nInspect these files.\n")
        (let* ((question-id "question-boundaries")
               (request-id "request-boundaries")
               (call-a (ende-chat--prepare-tool-call
                        '(:id "provider-A" :name "read_file"
                          :arguments "{\"path\":\"a.el\"}")))
               (call-b (ende-chat--prepare-tool-call
                        '(:id "provider-B" :name "read_file"
                          :arguments "{\"path\":\"b.el\"}")))
               (call-a-text (ende-chat--tool-call-text 3 request-id call-a))
               (result-a-text (ende-chat--tool-result-text
                               3 request-id call-a "RESULT_A")))
          (insert (ende-chat--work-text 2 question-id request-id)
                  (ende-chat--assistant-protocol-text
                   3 question-id request-id ""
                   '(:content "First turn prose") (list call-a))
                  call-a-text
                  result-a-text
                  (ende-chat--assistant-protocol-text
                   3 question-id request-id ""
                   '(:content "Second turn prose") (list call-b))
                  (ende-chat--tool-result-text
                   3 request-id call-b "RESULT_B")
                  (ende-chat--assistant-text
                   2 question-id request-id "" '(:content "Final answer.")))
          (save-excursion
            (goto-char (point-min))
            (re-search-forward "^\\*\\* Agent: Work")
            (org-back-to-heading t)
            (org-entry-delete nil ende-chat-state-property))
          (insert "** Follow-up\nContinue from complete history.\n")
          (let* ((document (ende-chat--build-document))
                 (result-a
                  (cl-find-if
                   (lambda (headline)
                     (equal (ende-chat--property
                             document headline :tool-execution-id)
                            (ende-tools-call-value call-a :execution-id)))
                   (ende-test--tool-headlines document 'result)))
                 (result-a-copy
                  (buffer-substring-no-properties
                   (org-element-property :begin result-a)
                   (org-element-property :end result-a))))
            (delete-region (org-element-property :begin result-a)
                           (org-element-property :end result-a))
            (let* ((updated (ende-chat--build-document))
                   (call-b
                    (cl-find-if
                     (lambda (headline)
                       (equal (ende-chat--property
                               updated headline :tool-execution-id)
                              (ende-tools-call-value call-b :execution-id)))
                     (ende-test--tool-headlines updated 'call))))
              (goto-char (org-element-property :end call-b))
              (insert result-a-copy)))
          (ende-test--chat-goto "** Follow-up")
          (let* ((messages (ende-chat-context-at-point))
                 (tool-turns (ende-test--stored-tool-messages messages))
                 (tool-results
                  (cl-remove-if-not
                   (lambda (message)
                     (equal (plist-get message :role) "tool"))
                   messages))
                 (first-prose
                  (cl-find-if
                   (lambda (message)
                     (and (consp message)
                          (equal (car message) "assistant")
                          (string-match-p "First turn prose" (cdr message))))
                   messages)))
            (should first-prose)
            (should (= (length tool-turns) 1))
            (should (equal (plist-get (car tool-turns) :content)
                           "Second turn prose"))
            (should (equal (mapcar (lambda (call)
                                    (plist-get call :id))
                                  (plist-get (car tool-turns) :tool-calls))
                           '("provider-B")))
            (should (equal (mapcar (lambda (message)
                                    (plist-get message :content))
                                  tool-results)
                           '("RESULT_B"))))))
    (ende-test--reset)))

(ert-deftest ende-test/chat-omits-only-executions-with-missing-payloads ()
  (ende-test--reset)
  (unwind-protect
      (let (notices)
        (with-temp-buffer
          (org-mode)
          (insert "* Topic\n** Question\nInspect both files.\n")
          (let* ((calls
                  (ende-test--stored-tool-history
                   (list (cons '(:id "provider-A" :name "read_file"
                                 :arguments "{\"path\":\"a.el\"}")
                               "RESULT_A")
                         (cons '(:id "provider-B" :name "read_file"
                                 :arguments "{\"path\":\"b.el\"}")
                               "RESULT_B"))))
                 (database (sqlite-open ende-tool-store-file))
                 (missing-id (ende-tools-call-value (car calls) :execution-id)))
            (unwind-protect
                (sqlite-execute
                 database
                 "DELETE FROM tool_payloads WHERE execution_id = ? AND kind = 'result'"
                 (vector missing-id))
              (sqlite-close database)))
          (insert "** Follow-up\nContinue from available history.\n")
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest arguments)
                       (let ((text (apply #'format format-string arguments)))
                          (when (string-match-p "Some tool payloads are unavailable" text)
                            (setq notices (cons text notices)))
                          text))))
            (ende-test--chat-goto "** Follow-up")
            (let* ((messages (ende-chat-context-at-point))
                   (assistant (car (ende-test--stored-tool-messages messages)))
                   (calls (plist-get assistant :tool-calls))
                   (results
                    (cl-remove-if-not
                     (lambda (message)
                       (equal (plist-get message :role) "tool"))
                     messages)))
              (should (equal (mapcar (lambda (call) (plist-get call :id)) calls)
                             '("provider-B")))
              (should (equal (mapcar (lambda (message)
                                       (plist-get message :tool-call-id))
                                     results)
                             '("provider-B")))
              (should (equal (mapcar (lambda (message)
                                       (plist-get message :content))
                                     results)
                             '("RESULT_B")))
                (should (= (length notices) 1))))))
    (ende-test--reset)))

(ert-deftest ende-test/chat-omits-empty-tool-only-turns-after-reference-deletion ()
  (ende-test--reset)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* Topic\n** Question\nRun the lookup.\n")
        (let* ((question-id "question-empty-turn")
               (request-id "request-empty-turn")
               (call (ende-chat--prepare-tool-call
                      '(:id "provider-empty" :name "read_file"
                        :arguments "{\"path\":\"empty.el\"}"))))
          (insert (ende-chat--work-text 2 question-id request-id)
                  (ende-chat--assistant-protocol-text
                   3 question-id request-id "" '(:content "") (list call))
                  (ende-chat--tool-result-text 3 request-id call "EMPTY_RESULT")
                  (ende-chat--assistant-text
                   2 question-id request-id "" '(:content "Final answer.")))
          (save-excursion
            (goto-char (point-min))
            (re-search-forward "^\\*\\* Agent: Work")
            (org-back-to-heading t)
            (org-entry-delete nil ende-chat-state-property))
          (insert "** Follow-up\nContinue.\n")
          (let* ((document (ende-chat--build-document))
                 (call-ref (car (ende-test--tool-headlines document 'call)))
                 (result-ref (car (ende-test--tool-headlines document 'result))))
            (delete-region (org-element-property :begin result-ref)
                           (org-element-property :end result-ref))
            (delete-region (org-element-property :begin call-ref)
                           (org-element-property :end call-ref)))
          (ende-test--chat-goto "** Follow-up")
          (let* ((messages (ende-chat-context-at-point))
                 (text (ende-test--chat-context-text messages)))
            (should-not (ende-test--chat-protocol-messages messages))
            (should (string-match-p "Final answer" text))
            (should (string-match-p "Continue" text)))))
    (ende-test--reset)))

(ert-deftest ende-test/chat-expired-tool-history-is-omitted-once-and-never-replayed ()
  (ende-test--reset)
  (unwind-protect
      (let (calls-to-tools notices replayed)
        (with-temp-buffer
          (org-mode)
          (insert "* Topic\n** Question\nCheck these records.\n")
          (setq calls-to-tools
                (ende-test--stored-tool-history
                 (list (cons '(:id "provider-A" :name "read_file"
                               :arguments "{\"path\":\"a.el\"}")
                             "RESULT_A")
                       (cons '(:id "provider-B" :name "read_file"
                               :arguments "{\"path\":\"b.el\"}")
                             "RESULT_B"))))
          (insert "** Follow-up\nUse the surviving prose.\n")
          (let ((file (expand-file-name ende-tool-store-file)))
            (delete-file file)
            (dolist (suffix '("-wal" "-shm" "-journal"))
              (when (file-exists-p (concat file suffix))
                (delete-file (concat file suffix))))
             (cl-letf (((symbol-function 'message)
                       (lambda (format-string &rest arguments)
                         (let ((text (apply #'format format-string arguments)))
                           (when (string-match-p "Some tool payloads are unavailable" text)
                             (push text notices))
                            text)))
                       ((symbol-function 'ende-tools-call-async)
                        (lambda (&rest _arguments)
                          (setq replayed t)
                          (error "expired tool replayed"))))
              (ende-test--chat-goto "** Follow-up")
              (let* ((messages (ende-chat-context-at-point))
                     (text (prin1-to-string messages)))
                (should-not (ende-test--chat-protocol-messages messages))
                (should (string-match-p "Checking both sources" text))
                (should (string-match-p "Final stored answer" text))
                (should (string-match-p "Use the surviving prose" text))
                (should (= (length notices) 1))
                (should-not replayed))
              (should-not (file-exists-p file))
              (let* ((new-call
                      (ende-chat--prepare-tool-call
                       '(:id "provider-A" :name "read_file"
                         :arguments "{\"path\":\"new.el\"}")))
                     (new-id (ende-tools-call-value new-call :execution-id)))
                (should (file-exists-p file))
                (should-not (member new-id
                                   (mapcar (lambda (call)
                                             (ende-tools-call-value
                                              call :execution-id))
                                           calls-to-tools)))
                (goto-char (point-max))
                (insert "\n" (ende-chat--tool-call-text
                               3 "request-new" new-call))
                (goto-char (point-min))
                (search-forward
                 (concat ende-chat-tool-execution-id-property ": " new-id))
                (org-back-to-heading t)
                (ende-chat-inspect-tool-at-point)
                (with-current-buffer "*ende tool payload*"
                  (should buffer-read-only)
                  (should (string-match-p "Provider call ID: provider-A"
                                          (buffer-string)))
                   (should (string-match-p "new.el" (buffer-string)))))))))
    (ende-test--reset)))

(defun ende-test--register-store-boundary-tool (handler)
  "Register a rootless side-effecting tool using HANDLER."
  (ende-tool-register 'ende_test_payload_boundary "Store boundary test tool"
                      ende-tools--empty-object-schema handler nil :no-resource
                      '(:requires-root nil)))

(ert-deftest ende-test/chat-switching-stores-preserves-context-and-freezes-async-storage ()
  (ende-test--reset)
  (let* ((directory (make-temp-file "ende-chat-store-switch-" t))
         (selected (expand-file-name "selected.sqlite3" directory))
         (ende-tools--registry (copy-sequence ende-tools--registry))
         (ende-llm-function
          (ende-test--fake-llm
           '((:tool-calls ((:id "new-call" :name "ende_test_payload_boundary"
                           :arguments "{}"))) "Continued answer") t))
         pending notices)
    (ende-tool-register
     'ende_test_payload_boundary "Async store test" ende-tools--empty-object-schema
     (lambda (_arguments _root callback) (setq pending callback))
     nil :no-resource '(:requires-root nil :async t))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (setq buffer-file-name (expand-file-name "chat.org" directory))
          (insert (ende-test--chat-config-block
                   '((tools ende_test_payload_boundary)))
                  "* Topic\n** Question\nOriginal question.\n")
          (let ((old-calls
                 (ende-test--stored-tool-history
                  (list (cons '(:id "old-A" :name "read_file" :arguments "{}") "A")
                        (cons '(:id "old-B" :name "read_file" :arguments "{}") "B")))))
            (insert "** Follow-up\nContinue from the prose.\n")
            (save-excursion
              (goto-char (point-min))
              (insert "#+ENDE_TOOL_STORE_FILE: selected.sqlite3\n"))
            (ende-mode 1)
            (ende-test--chat-goto "** Follow-up")
            (cl-letf (((symbol-function 'message)
                       (lambda (format-string &rest arguments)
                         (let ((text (apply #'format format-string arguments)))
                           (when (equal text "Some tool payloads are unavailable; omitting affected tool history")
                             (push text notices))
                           text))))
              (let* ((messages (ende-chat-context-at-point))
                     (text (prin1-to-string messages)))
                (should-not (ende-test--chat-protocol-messages messages))
                (dolist (prose '("Original question" "Checking both sources"
                                 "Final stored answer" "Continue from the prose"))
                  (should (string-match-p prose text)))
                (should (= (length notices) 1)))
              (setq notices nil)
              (let ((job (ende-ask)))
                (should (= (length notices) 1))
                (should (equal (plist-get (ende-job-instruction job) :tool-store-file)
                               selected))
                (with-temp-buffer
                  (ende-test--complete-call (car ende-test--calls)))
                (should pending)
                (setq ende-tool-store--local-file nil)
                (ende-queue-add-guidance job "Keep going.")
                (with-temp-buffer (funcall pending '(:ok t) nil))
                (ende-test--complete-call (nth 1 ende-test--calls))
                (should (eq (ende-job-state job) 'completed))
                (let* ((document (ende-chat--build-document))
                       (references (ende-test--tool-headlines document 'call))
                       (new-references (nthcdr (length old-calls) references)))
                  (should (= (length new-references) 2))
                  (dolist (reference new-references)
                    (let* ((id (ende-chat--property
                                document reference :tool-execution-id))
                           (execution (ende-tool-store-load-execution id selected)))
                      (should (plist-get execution :request-present-p))
                      (should (plist-get execution :result-present-p))
                      (should-not (ende-tool-store-get id 'request)))))))))
      (ende-test--reset)
      (delete-directory directory t))))

(ert-deftest ende-test/chat-tool-store-failures-respect-execution-order ()
  (dolist (failure-kind '(request result))
    (ende-test--reset)
    (let* ((ende-tools--registry (copy-sequence ende-tools--registry))
           (effects 0)
           (call '(:id "provider-boundary" :name "ende_test_payload_boundary"
                   :arguments "{\"marker\":\"BOUNDARY_ARGS\"}"))
           (ende-llm-function
            (ende-test--fake-llm
             (list (list :tool-calls (list call)) "Must never be replayed") t))
           (original-put (symbol-function 'ende-tool-store-put)))
      (ende-test--register-store-boundary-tool
       (lambda (_arguments _root)
         (setq effects (1+ effects))
         "TOOL_SIDE_EFFECT_DONE"))
      (unwind-protect
          (with-temp-buffer
            (org-mode)
            (insert (ende-test--chat-config-block
                     '((tools ende_test_payload_boundary)))
                    "* Topic\n** Question\nRun the test tool.\n")
            (ende-test--chat-goto "** Question")
            (let ((job (ende-ask)))
              (cl-letf (((symbol-function 'ende-tool-store-put)
                         (lambda (execution-id kind payload &optional filename)
                           (if (eq kind failure-kind)
                               (error "%s-store-failed" kind)
                             (funcall original-put execution-id kind payload
                                      filename)))))
                (ende-test--complete-call (car ende-test--calls)))
              (should (eq (ende-job-state job) 'failed))
              (should (string-match-p
                       (format "%s-store-failed" failure-kind)
                       (ende-error-display-message (ende-job-error job))))
              (should (= (length ende-test--calls) 1))
              (let* ((document (ende-chat--build-document))
                     (tool-calls (ende-test--tool-headlines document 'call))
                     (tool-results (ende-test--tool-headlines document 'result)))
                (if (eq failure-kind 'request)
                    (progn
                      (should (= effects 0))
                      (should-not tool-calls)
                      (should-not tool-results))
                  (should (= effects 1))
                  (should (= (length tool-calls) 1))
                  (should-not tool-results)
                  (let ((execution-id
                         (ende-chat--property
                          document (car tool-calls) :tool-execution-id)))
                    (should (ende-tool-store-get execution-id 'request))
                    (should-not (ende-tool-store-get execution-id 'result)))))))
        (ende-test--reset)))))

(ert-deftest ende-test/tool-store-purge-confirms-selected-store-and-refuses-live-jobs ()
  (ende-test--reset)
  (let* ((directory (make-temp-file "ende-tool-store-purge-" t))
         (selected (expand-file-name "selected.sqlite3" directory))
         (other (expand-file-name "other.sqlite3" directory))
         (ende-tool-store-file other)
         (ende-llm-function (ende-test--fake-llm '("Done") t)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (setq buffer-file-name (expand-file-name "chat.org" directory))
          (ende-tool-store-put "other" 'request "other payload")
          (insert "#+ENDE_TOOL_STORE_FILE: selected.sqlite3\n"
                  "* Question\nProceed.\n")
          (ende-mode 1)
          (ende-tool-store-put "selected" 'request "selected payload")
          (ende-test--chat-goto "* Question")
          (ende-queue-pause)
          (let ((job (ende-ask)))
            (should (eq (ende-job-state job) 'queued))
            (should-not (plist-get (ende-job-instruction job) :tool-store-file))
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (prompt)
                         (should (string-match-p (regexp-quote selected) prompt))
                         t)))
              (should-error (ende-tool-store-purge) :type 'user-error)
              (should (file-exists-p selected)))
            (with-temp-buffer
              (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
                (ende-tool-store-purge))
              (should-not (file-exists-p other))
              (should (file-exists-p selected))
              (ende-tool-store-put "other" 'request "other payload"))
            (ende-queue-resume)
            (should (eq (ende-job-state job) 'running))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
              (should-error (ende-tool-store-purge) :type 'user-error))
            (ende-test--complete-call (car ende-test--calls))
            (should (eq (ende-job-state job) 'completed)))
          (dolist (file (cdr (ende-tool-store--files selected)))
            (with-temp-file file (insert "sidecar")))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
            (ende-tool-store-purge)
            (should (file-exists-p selected)))
          (let ((text (buffer-string)))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
              (ende-tool-store-purge)
              (dolist (file (ende-tool-store--files selected))
                (should-not (file-exists-p file)))
              (should (equal (buffer-string) text))
              (should (equal (ende-tool-store-get "other" 'request other)
                             "other payload"))
              (should (string-match-p "does not exist"
                                      (ende-tool-store-purge))))))
      (ende-test--reset)
      (delete-directory directory t))))

(ert-deftest ende-test/chat-reexecutions-get-fresh-tool-execution-ids ()
  (ende-test--reset)
  (let* ((ende-tools--registry (copy-sequence ende-tools--registry))
         (effects 0)
         (call '(:id "provider-reused" :name "ende_test_payload_boundary"
                 :arguments "{\"marker\":\"same call\"}"))
         (ende-llm-function
          (ende-test--fake-llm
           (list (list :tool-calls (list call)) "First answer"
                 (list :tool-calls (list call)) "Second answer") t)))
    (ende-test--register-store-boundary-tool
     (lambda (_arguments _root)
       (setq effects (1+ effects))
       (format "result-%d" effects)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert (ende-test--chat-config-block
                   '((tools ende_test_payload_boundary)))
                  "* Topic\n** First\nRun the tool.\n"
                  "** Second\nRun it again.\n")
          (ende-test--chat-goto "** First")
          (let ((job (ende-ask)))
            (ende-test--complete-call (nth 0 ende-test--calls))
            (ende-test--complete-call (nth 1 ende-test--calls))
            (should (eq (ende-job-state job) 'completed)))
          (ende-test--chat-goto "** Second")
          (let ((job (ende-ask)))
            (ende-test--complete-call (nth 2 ende-test--calls))
            (ende-test--complete-call (nth 3 ende-test--calls))
            (should (eq (ende-job-state job) 'completed)))
          (let* ((document (ende-chat--build-document))
                 (references (ende-test--tool-headlines document 'call))
                 (execution-ids
                  (mapcar (lambda (headline)
                            (ende-chat--property
                             document headline :tool-execution-id))
                          references)))
            (should (= effects 2))
            (should (= (length execution-ids) 2))
            (should-not (equal (car execution-ids) (cadr execution-ids)))
            (should-not (string-match-p "provider-reused" (buffer-string)))
            (dolist (execution-id execution-ids)
              (let ((record (json-parse-string
                             (ende-tool-store-get execution-id 'request)
                             :object-type 'hash-table)))
                (should (equal (gethash "provider_call_id" record)
                               "provider-reused")))))
          (should (cl-some (lambda (message)
                             (plist-member message :tool-calls))
                           (ende-test--chat-protocol-messages
                            (plist-get (nth 2 ende-test--calls) :messages)))))
      (ende-test--reset))))

(provide 'ende-chat-tool-store-test)

;;; ende-chat-tool-store-test.el ends here
