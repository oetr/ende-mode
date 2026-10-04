;;; ende-test.el --- ERT tests for ende -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools, llm

;;; Commentary:

;; ERT coverage for synchronous operations, asynchronous lifecycle behavior,
;; target conflicts, and the incremental streaming parsers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'ende-provider)
(require 'ende)

;;;; Test harness

(require 'ende-test-helpers)
(require 'ende-chat-test-helpers)

(ert-deftest ende-test/chat-tool-config-resolves-inheritance-and-rejects-code ()
  (let* ((root (make-temp-file "ende-chat-config-" t))
         (child-root (make-temp-file "ende-chat-child-config-" t))
         (configuration
          (format
           (concat
            "#+begin_ende\n(root %S)\n"
            "(group inspect project_info list_project_files file_info read_file "
            "search_project list_project_buffers read_buffer)\n"
            "(group modify edit_file create_file)\n"
            "(resource project (include \"**\") (exclude \"private/**\"))\n"
            "(resource chat (include \"main.el\") (include \"new.el\"))\n"
            "(tools inspect)\n"
            "(set-tool-resources inspect :resources project)\n"
            "#+end_ende\n\n"
            "* Review\n** Edit\n"
            "#+begin_ende\n(root %S)\n"
            "(tools modify)\n"
            "(set-tool-resources modify :resources chat)\n"
            "(set-tool-resources read_file :resources chat)\n"
            "(add-tool-resources read_file :resources project)\n"
            "#+end_ende\n*** Inspect only\n"
            "#+begin_ende\n(clear-tools)\n(tools inspect)\n"
            "(clear-tool-resources)\n"
            "(set-tool-resources inspect :resources project)\n"
            "#+end_ende\n")
           root child-root)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "private" root))
          (make-directory (expand-file-name "private" child-root))
          (with-temp-file (expand-file-name "main.el" child-root)
            (insert "hello"))
          (with-temp-file (expand-file-name "private/secret.txt" child-root)
            (insert "secret"))
          (with-temp-buffer
            (insert configuration)
            (org-mode)
            (let* ((document (ende-chat--build-document))
                   (headlines (ende-chat--document-headlines document))
                   (edit (cl-find-if
                          (lambda (headline)
                            (equal (org-element-property :raw-value headline)
                                   "Edit"))
                          headlines))
                   (inspect-only
                    (cl-find-if
                     (lambda (headline)
                       (equal (org-element-property :raw-value headline)
                              "Inspect only"))
                     headlines))
                   (edit-config
                    (ende-chat-config-resolve
                     (ende-chat--document-tree document)
                     (ende-chat--document-parent document) edit))
                   (inspect-config
                    (ende-chat-config-resolve
                     (ende-chat--document-tree document)
                     (ende-chat--document-parent document) inspect-only)))
              (should (equal (plist-get edit-config :root)
                             (ende-project-normalize-root child-root)))
              (should (memq 'edit_file
                            (plist-get edit-config :active-tools)))
              (should (memq 'create_file
                            (plist-get edit-config :active-tools)))
              (should (equal (cdr (assq 'edit_file
                                        (plist-get edit-config
                                                   :tool-resources)))
                             '(chat)))
              (should (equal (cdr (assq 'create_file
                                        (plist-get edit-config
                                                   :tool-resources)))
                             '(chat)))
              (should (equal (cdr (assq 'read_file
                                        (plist-get edit-config
                                                   :tool-resources)))
                             '(chat project)))
              (should (equal (plist-get inspect-config :active-tools)
                             '(project_info list_project_files file_info
					    read_file search_project list_project_buffers
					    read_buffer)))
              (should (equal (plist-get inspect-config :root)
                             (ende-project-normalize-root child-root)))
              (should-not (memq 'edit_file
                                (plist-get inspect-config :active-tools)))
              (should-not (assq 'edit_file
                                (plist-get inspect-config :tool-resources)))
              (let ((allowed
                     (ende-tool-call
                      "edit_file"
                      (list (cons "path" "main.el")
                            (cons "edits"
                                  (list (ende-test--anchor-edit
                                         "hello" "replace" "updated"))))
                      child-root edit-config)))
                (should (plist-get allowed :ok))
                (should (equal (plist-get (plist-get allowed :result) :status)
                               "applied")))
              (let* ((created
                      (ende-tool-call
                       "create_file"
                       '(("path" . "new.el") ("content" . ""))
                       child-root edit-config))
                     (result (plist-get created :result)))
                (should (plist-get created :ok))
                (should (equal (plist-get result :operation) "create"))
                (should (equal (plist-get result :status) "applied"))
                (should-not (plist-member result :change_set_id))
                (should (= (file-attribute-size
                            (file-attributes
                             (expand-file-name "new.el" child-root)))
                           0)))
              (let ((denied
                     (ende-tool-call
                      "create_file"
                      '(("path" . "private/secret.txt")
                        ("content" . "changed"))
                      child-root edit-config)))
                (should-not (plist-get denied :ok))
                (should (string-match-p "not authorized"
                                        (plist-get denied :error))))))
          (let ((ran nil))
            (should-error
             (ende-chat-config--read-forms "(#.(setq ran t))"))
            (should-not ran))
          (with-temp-buffer
            (insert "#+begin_ende\n(tools read_file)\n#+end_ende\n* Q\n")
            (org-mode)
            (let* ((document (ende-chat--build-document))
                   (question (car (ende-chat--document-headlines document))))
              (should-error
               (ende-chat-config-resolve
                (ende-chat--document-tree document)
                (ende-chat--document-parent document) question))))
          (with-temp-buffer
            (insert
             (format
              (concat "#+begin_ende\n(root %S)\n(group inspect project_info)\n"
                      "(tools inspect)\n#+end_ende\n* A\n** B\n#+begin_ende\n"
                      "(group inspect project_info)\n#+end_ende\n")
              root))
            (org-mode)
            (let* ((document (ende-chat--build-document))
                   (configuration (ende-test--chat-config-at document "B"))
                   (inherited (ende-test--chat-config-at document "A")))
              (should (equal (plist-get configuration :groups)
                             (plist-get inherited :groups)))
              (should (equal (plist-get inherited :active-tools) '(project_info)))
              (should-not (plist-get configuration :active-tools))
              (should-not (plist-get configuration :tool-resources)))))
      (delete-directory root t)
      (delete-directory child-root t))))

(ert-deftest ende-test/chat-config-definitions-can-remain-unused ()
  (let ((root (make-temp-file "ende-config-library-" t))
        (cases '(("** No tools" nil)
                 ("** Inspect" (project_info list_project_files read_file))
                 ("*** Clear tools" nil)
                 ("*** Redefine active" nil)
                 ("*** Redefine unused" (project_info list_project_files read_file))
                 ("** Direct tools" (read_file))
                 ("** Direct grants" (read_file))
                 ("** Information" (project_info))
                 ("** Redefine only" nil))))
    (ende-test--reset)
    (unwind-protect
        (let ((ende-llm-function
               (ende-test--fake-llm (make-list (length cases) "Available tools described."))))
          (with-temp-buffer
            (org-mode)
            (insert
             (ende-test--chat-config-block
              `((root ,root)
                (group inspect project_info list_project_files read_file)
                (group modify edit_file create_file)
                (resource project (include "bla.org"))
                (resource unused (include "unused/**"))))
             "* Conversation\n** No tools\nWhich tools are available?\n"
             "** Inspect\n"
             (ende-test--chat-config-block
              '((tools inspect) (set-tool-resources inspect :resources project)))
             "Which tools are available?\n*** Clear tools\n"
             (ende-test--chat-config-block '((clear-tools)))
             "Which tools are available?\n*** Redefine active\n"
             (ende-test--chat-config-block '((group inspect read_file)))
             "Which tools are available?\n*** Redefine unused\n"
             (ende-test--chat-config-block
              '((group modify file_info) (resource unused (include "other/**"))))
             "Which tools are available?\n** Direct tools\n"
             (ende-test--chat-config-block
              '((tools read_file) (set-tool-resources read_file :resources project)))
             "Which tools are available?\n** Direct grants\n"
             (ende-test--chat-config-block
              '((group inspect read_file)
                (tools inspect)
                (set-tool-resources read_file :resources project)))
             "Which tools are available?\n** Information\n"
             (ende-test--chat-config-block '((tools project_info)))
             "Which tools are available?\n** Redefine only\n"
             (ende-test--chat-config-block
              '((group inspect file_info) (resource project (include "other.org"))))
             "Which tools are available?\n")
            (dolist (case cases)
              (ert-info ((car case))
                (ende-test--chat-goto (car case))
                (let ((configuration
                       (ende-test--chat-config-at
                        (ende-chat--build-document)
                        (org-get-heading t t t t))))
                  (should (equal (plist-get configuration :active-tools) (cadr case)))
                  (should (assq 'modify (plist-get configuration :groups)))
                  (should (assq 'unused (plist-get configuration :resources))))
                (should (eq (ende-job-state (ende-ask)) 'completed))
                (let* ((options (plist-get (car (last ende-test--calls)) :options))
                       (names (mapcar (lambda (definition)
                                        (alist-get :name (alist-get :function definition)))
                                      (plist-get options :tool-definitions))))
                  (should (equal (sort names #'string<)
                                 (sort (mapcar #'symbol-name (cadr case)) #'string<))))))))
      (ende-test--reset)
      (delete-directory root t))))

(ert-deftest ende-test/chat-config-redefinitions-follow-the-org-tree ()
  (let ((root (make-temp-file "ende-config-scopes-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "src/private" root) t)
          (dolist (path '("main.el" "src/public.el" "src/private/secret.el"))
            (with-temp-file (expand-file-name path root)
              (insert "source\n")))
          (with-temp-buffer
            (org-mode)
            (insert
             (ende-test--chat-config-block
              `((root ,root)
                (group inspect project_info read_file search_project)
                (resource project (include "**") (exclude "src/private/**"))
                (tools inspect)
                (set-tool-resources inspect :resources project)))
             "* Conversation\n** Original\nInspect.\n** Changed\n"
             (ende-test--chat-config-block
              '((tools inspect)
                (set-tool-resources inspect :resources project)
                (resource project (include "src/**") (exclude "src/private/**"))
                (group inspect project_info read_file)))
             "Inspect sources.\n*** Descendant\nContinue.\n"
             "*** Information only\n"
             (ende-test--chat-config-block
              '((group inspect project_info) (tools inspect)))
             "Describe the project.\n** Agent: Reply\n"
             ":PROPERTIES:\n:ENDE_ROLE: assistant\n:END:\nSaved answer.\n"
             "** Next\nContinue.\n* Other conversation\nInspect.\n")
            (let ((document (ende-chat--build-document)))
              ;; Resolve children first to catch shared-state mutation.
              (dolist (case
                       '(("Information only" (project_info) nil)
                         ("Descendant" (project_info read_file) ("src/public.el"))
                         ("Changed" (project_info read_file) ("src/public.el"))
                         ("Next" (project_info read_file search_project)
                          ("main.el" "src/public.el"))
                         ("Original" (project_info read_file search_project)
                          ("main.el" "src/public.el"))
                         ("Other conversation" (project_info read_file search_project)
                          ("main.el" "src/public.el"))))
                (ert-info ((car case))
                  (let* ((configuration
                          (ende-test--chat-config-at document (car case)))
                         (active (cadr case))
                         (allowed (caddr case)))
                    (should (equal (plist-get configuration :active-tools) active))
                    (should (equal (cdr (assq 'inspect
                                              (plist-get configuration :groups)))
                                   active))
                    (should (equal configuration
                                   (ende-test--chat-config-at document (car case))))
                    (should (plist-get
                             (ende-tool-call 'project_info nil root configuration)
                             :ok))
                    (dolist (path '("main.el" "src/public.el" "src/private/secret.el"))
                      (let ((result (ende-tool-call 'read_file (list :path path)
                                                    root configuration)))
                        (should (eq (not (null (plist-get result :ok)))
                                    (not (null (member path allowed)))))))))))))
      (delete-directory root t))))

(ert-deftest ende-test/chat-config-group-replacement-preserves-independent-grants ()
  (let ((root (make-temp-file "ende-config-origins-" t)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert
           (ende-test--chat-config-block
            `((root ,root)
              (group changing read_file search_project file_info)
              (group stable read_file list_project_files)
              (resource broad (include "**"))
              (resource safe (include "safe.el"))
              (resource direct (include "direct.el"))
              (resource replacement (include "new.el"))
              (tools changing stable search_project)
              (set-tool-resources changing :resources broad)
              (add-tool-resources stable :resources safe)
              (add-tool-resources search_project :resources direct)
              (add-tool-resources read_file :resources broad)))
           "* Conversation\n** Replace group\n"
           (ende-test--chat-config-block
            '((group changing project_info read_buffer)
              (tools changing)
              (set-tool-resources changing :resources replacement)))
           "Inspect.\n*** Revoke broad\n"
           (ende-test--chat-config-block
            '((resource broad (include "**"))))
           "Inspect safely.\n")
          (let* ((document (ende-chat--build-document))
                 (replacement (ende-test--chat-config-at document "Replace group"))
                 (revoked (ende-test--chat-config-at document "Revoke broad")))
            (dolist (configuration (list replacement revoked))
              (should (equal (plist-get configuration :active-tools)
                             '(read_file list_project_files search_project
                                         project_info read_buffer)))
              (let ((assignments (plist-get configuration :tool-resources)))
                (should-not (assq 'file_info assignments))
                (should (equal (cdr (assq 'search_project assignments)) '(direct)))
                (should (equal (cdr (assq 'list_project_files assignments)) '(safe)))
                (should (equal (cdr (assq 'read_buffer assignments)) '(replacement)))))
            (should (equal (cdr (assq 'read_file
                                      (plist-get replacement :tool-resources)))
                           '(safe broad)))
            (should (equal (cdr (assq 'read_file
                                      (plist-get revoked :tool-resources)))
                           '(safe)))
            (should-not (plist-member replacement :grants))
            (should-not (plist-member replacement :active-targets))))
      (delete-directory root t))))

(ert-deftest ende-test/chat-config-resource-replacement-requires-selective-renewal ()
  (let ((root (make-temp-file "ende-config-resources-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "private" root))
          (dolist (path '("public.el" "private/secret.el"))
            (with-temp-file (expand-file-name path root)
              (insert "TOKEN\n")))
          (with-temp-buffer
            (org-mode)
            (insert
             (ende-test--chat-config-block
              `((root ,root)
                (group readers read_file)
                (group metadata file_info)
                (resource project (include "**"))
                (resource retained (include "public.el"))
                (tools readers metadata search_project)
                (set-tool-resources readers metadata search_project :resources project)
                (add-tool-resources readers metadata search_project :resources retained)))
             "* Conversation\n** Revoke\n"
             (ende-test--chat-config-block
              '((resource project (include "private/**"))))
             "Only public access remains.\n*** Renew readers\n"
             (ende-test--chat-config-block
              '((add-tool-resources readers :resources project)
                (resource unused (include "unused/**"))))
             "Read private sources too.\n** Reset access\n"
             (ende-test--chat-config-block
              '((resource project (include "private/**"))
                (clear-tools)
                (tools readers)
                (clear-tool-resources)
                (set-tool-resources readers :resources project)))
             "Only read private sources.\n")
            (let ((document (ende-chat--build-document)))
              (dolist (case '(("Revoke" (retained) (retained) nil)
                              ("Renew readers" (retained project) (retained) t)
                              ("Reset access" (project) nil t)))
                (ert-info ((car case))
                  (let* ((configuration
                          (ende-test--chat-config-at document (car case)))
                         (assignments (plist-get configuration :tool-resources)))
                    (should (equal (cdr (assq 'read_file assignments)) (cadr case)))
                    (dolist (tool '(file_info search_project))
                      (should (equal (cdr (assq tool assignments)) (caddr case))))
                    (should (eq (plist-get
                                 (ende-tool-call 'read_file
                                                 '(:path "private/secret.el")
                                                 root configuration)
                                 :ok)
                                (nth 3 case)))
                    (should-not (plist-get
                                 (ende-tool-call 'file_info
                                                 '(:path "private/secret.el")
                                                 root configuration)
                                 :ok))
                    (when (caddr case)
                      (let* ((call (ende-tool-call 'search_project '(:regexp "TOKEN")
                                                   root configuration))
                             (matches (plist-get (plist-get call :result) :matches)))
                        (should (plist-get call :ok))
                        (should (= (length matches) 1))
                        (should (equal (plist-get (car matches) :path) "public.el"))))))))))
      (delete-directory root t))))

(ert-deftest ende-test/chat-config-assignment-operations-remain-ordered ()
  (let ((root (make-temp-file "ende-config-order-" t)))
    (unwind-protect
        (dolist (case
                 '((nil (wide) (wide) (wide))
                   (((set-tool-resources first :resources narrow)
                     (add-tool-resources second :resources extra))
                    (narrow) (narrow extra) (wide extra))
                   (((add-tool-resources second :resources extra)
                     (set-tool-resources first :resources narrow))
                    (narrow) (narrow) (wide extra))
                   (((clear-tool-resources)
                     (set-tool-resources first second :resources narrow))
                    (narrow) (narrow) (narrow))))
          (ert-info ((format "Operations: %S" (car case)))
            (with-temp-buffer
              (org-mode)
              (insert
               (ende-test--chat-config-block
                `((root ,root)
                  (group first read_file file_info)
                  (group second file_info search_project)
                  (resource wide (include "**"))
                  (resource narrow (include "main.el"))
                  (resource extra (include "other.el"))
                  (tools first second)
                  (set-tool-resources first second :resources wide)))
               "* Conversation\n** Operations\n"
               (ende-test--chat-config-block (car case))
               "Inspect.\n*** Replace second\n"
               (ende-test--chat-config-block
                '((group second project_info) (tools second)))
               "Inspect again.\n")
              (let* ((document (ende-chat--build-document))
                     (configuration (ende-test--chat-config-at document "Operations"))
                     (replacement (ende-test--chat-config-at document "Replace second")))
                (cl-mapc (lambda (tool expected)
                           (should (equal (cdr (assq tool (plist-get configuration
                                                                     :tool-resources)))
                                          expected)))
                         '(read_file file_info search_project) (cdr case))
                (should (equal (plist-get replacement :active-tools)
                               '(read_file file_info project_info)))
                (dolist (tool '(read_file file_info))
                  (should (equal (cdr (assq tool (plist-get replacement :tool-resources)))
                                 (cadr case))))
                (should-not (assq 'search_project
                                  (plist-get replacement :tool-resources)))))))
      (delete-directory root t))))

(ert-deftest ende-test/chat-config-rejects-incomplete-or-ambiguous-permissions ()
  (let ((root (make-temp-file "ende-config-invalid-" t)))
    (unwind-protect
        (dolist
            (case
             '((((group inspect read_file) (tools inspect)) "no assigned resources")
               (((group inspect read_file) (tools read_file)) "no assigned resources")
               (((group inspect read_file) (tools inspect)
                 (set-tool-resources inspect :resources project)
                 (clear-tool-resources)) "no assigned resources")
               (((resource project (include "**"))) "resource redefinition revoked project")
               (((group inspect project_info) (group inspect read_file)) "defined twice")
               (((resource project (include "**"))
                 (resource project (include "public/**"))) "defined twice")
               (((group read_file project_info)) "conflicts with registered tool")
               (((tools edit_file)) "no assigned resources")))
          (ert-info ((format "Invalid operations: %S" (car case)))
            (ende-test--reset)
            (let ((ende-llm-function (ende-test--fake-llm '("Must not be requested"))))
              (with-temp-buffer
                (org-mode)
                (insert
                 (ende-test--chat-config-block
                  `((root ,root)
                    (group inspect read_file)
                    (resource project (include "**"))
                    (tools inspect)
                    (set-tool-resources inspect :resources project)))
                 "* Question\n"
                 (ende-test--chat-config-block (car case))
                 "Inspect.\n")
                (let ((failure
                       (should-error
                        (ende-test--chat-config-at (ende-chat--build-document) "Question"))))
                  (should (string-match-p (cadr case) (error-message-string failure))))
                (ende-test--chat-goto "* Question")
                (let ((job (ende-ask)))
                  (should (eq (ende-job-state job) 'failed))
                  (should (string-match-p
                           (cadr case)
                           (ende-error-display-message (ende-job-error job)))))
                (should-not ende-test--calls)))))
      (ende-test--reset)
      (delete-directory root t))))

(ert-deftest ende-test/chat-config-dispatch-uses-live-scopes-and-freezes-tool-access ()
  (let ((root (make-temp-file "ende-config-dispatch-" t)))
    (ende-test--reset)
    (unwind-protect
        (progn
          (dolist (path '("public.el" "private.el"))
            (with-temp-file (expand-file-name path root)
              (insert path "\n")))
          (let ((ende-llm-function
                 (ende-test--fake-llm
                  (list
                   (list :tool-calls
                         '((:id "public" :name "read_file"
				:arguments "{\"path\":\"public.el\"}")
                           (:id "private" :name "read_file"
				:arguments "{\"path\":\"private.el\"}")))
                   "Scoped answer." "Follow-up answer.")
                  t)))
            (with-temp-buffer
              (org-mode)
              (insert
               (ende-test--chat-config-block
                `((root ,root)
                  (group inspect read_file)
                  (resource project (include "**"))
                  (tools inspect)
                  (set-tool-resources inspect :resources project)))
               "* Conversation\n** Question\n"
               (ende-test--chat-config-block
                '((group inspect read_file)
                  (resource project (include "public.el"))))
               "Read the files.\n** Next\nContinue.\n")
              (ende-test--chat-goto "** Question")
              (setq ende--queue-suspended t)
              (let ((job (ende-ask)))
                (should (eq (ende-job-state job) 'queued))
                (search-forward "#+end_ende")
                (beginning-of-line)
                (insert "(tools inspect)\n"
                        "(set-tool-resources inspect :resources project)\n")
                (setq ende--queue-suspended nil)
                (ende-process-next)
                (should (eq (ende-job-state job) 'running))
                (should (= (length ende-test--calls) 1))
                (ende-test--chat-goto "** Question")
                (search-forward "(include \"public.el\")")
                (replace-match "(include \"private.el\")" t t)
                (ende-test--complete-call (car ende-test--calls))
                (should (= (length ende-test--calls) 2))
                (let* ((messages (plist-get (cadr ende-test--calls) :messages))
                       (results (cl-remove-if-not
                                 (lambda (message)
                                   (equal (ende-test--chat-message-role message) "tool"))
                                 messages)))
                  (should (= (length results) 2))
                  (cl-mapc
                   (lambda (message allowed)
                     (let ((result (json-parse-string
                                    (plist-get message :content)
                                    :object-type 'plist
                                    :null-object nil :false-object nil)))
                       (should (eq (plist-get result :ok) allowed))
                       (if allowed
                           (should (equal (plist-get (plist-get result :result) :content)
                                          "public.el\n"))
                         (should (string-match-p "not authorized"
                                                 (plist-get result :error))))))
                   results '(t nil)))
                (ende-test--complete-call (cadr ende-test--calls))
                (should (eq (ende-job-state job) 'completed)))
              (ende-test--chat-goto "** Next")
              (let* ((configuration
                      (ende-test--chat-config-at (ende-chat--build-document) "Next"))
                     (job (ende-ask)))
                (dolist (path '("public.el" "private.el"))
                  (should (plist-get
                           (ende-tool-call 'read_file (list :path path) root configuration)
                           :ok)))
                (ende-test--complete-call (nth 2 ende-test--calls))
                (should (eq (ende-job-state job) 'completed))))))
      (ende-test--reset)
      (delete-directory root t))))

(ert-deftest ende-test/chat-tool-loop-filters-search-and-authorizes-calls ()
  (let* ((root (make-temp-file "ende-chat-tools-" t))
         (public (expand-file-name "main.el" root))
         (private-directory (expand-file-name "private" root))
         (private (expand-file-name "secret.txt" private-directory))
         (visited nil)
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
                         (list (list :id "call-search"
                                     :name "search_project"
                                     :arguments "{\"regexp\":\"classified\"}")))
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
                (ende-ask)
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
                  (ende-test--complete-call (nth 1 ende-test--calls))))
              (should (equal (nreverse visited) '("main.el")))
              (should (string-match-p "Search complete" (buffer-string)))
              (should (= (length ende-test--calls) 2))
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
                               '("list_project_files" "project_info"
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

(ert-deftest ende-test/openai-tool-diagnostics-are-safe-and-configurable ()
  (should (eq ende-tool-diagnostics t))
  (let* ((definition
          (cl-find-if
           (lambda (candidate)
             (equal (alist-get :name (alist-get :function candidate))
                    "read_file"))
           (ende-tool-definitions)))
         (body (ende-codex--request-body
                (list (cons "user" "question"))
                (list :model "gpt-test"
                      :tool-definitions (list definition))))
         (events nil)
         (session
          (ende-codex--make-session
           :profile '(:model "gpt-test")
           :callbacks (list :on-event
                            (lambda (event) (push event events)))
           :tool-calls (make-hash-table :test #'eql))))
    (ende-codex--diagnose-request session body)
    (ende-codex--handle-event
     session
     (ende-protocol-json-read-string
      (concat
       "{\"type\":\"response.completed\",\"response\":{\"output\":["
       "{\"type\":\"function_call\",\"call_id\":\"call-1\","
       "\"name\":\"read_file\",\"arguments\":"
       "\"{\\\"path\\\":\\\"secret.el\\\"}\"}]}}")))
    (let ((diagnostics
           (mapcar (lambda (event) (plist-get event :text))
                   (cl-remove-if-not
                    (lambda (event)
                      (eq (plist-get event :type) :diagnostic))
                    (nreverse events)))))
      (should (equal diagnostics
                     '("OpenAI Responses tools sent: read_file"
                       "OpenAI Responses output items: function_call(read_file)")))
      (should-not (string-match-p "secret\\.el"
                                  (mapconcat #'identity diagnostics "\n")))))
  (let ((ende-tool-diagnostics nil)
        (events nil))
    (let ((codex-session
           (ende-codex--make-session
            :callbacks (list :on-event
                             (lambda (event) (push event events)))))
          (chat-session
           (ende-llm--make-session
            :callbacks (list :on-event
                             (lambda (event) (push event events)))
            :tool-calls (make-hash-table :test #'eql))))
      (ende-codex--diagnose-request codex-session '((:tools . nil)))
      (ende-llm--diagnose-request chat-session '((:tools . nil)))
      (ende-codex--diagnose-output-item
       codex-session "response.output_item.done"
       (ende-protocol-json-read-string
        "{\"type\":\"function_call\",\"name\":\"read_file\"}"))
      (ende-llm--diagnose-result chat-session)
      (should-not events))))

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

(ert-deftest ende-test/openrouter-batch-tool-diagnostics-use-shared-setting ()
  (should (eq ende-tool-diagnostics t))
  (let* ((events nil)
         (request
          (ende-openrouter-batch--make-request
           :callbacks (list :on-event
                            (lambda (event) (push event events)))))
         (body
          (ende-protocol-json-read-string
           (concat
            "{\"choices\":[{\"message\":{\"content\":\"answer\","
            "\"tool_calls\":[{\"function\":{\"name\":\"read_file\","
            "\"arguments\":\"{\\\"path\\\":\\\"secret.el\\\"}\"}}]}}]}"))))
    (ende-openrouter-batch--diagnose-request request)
    (ende-openrouter-batch--diagnose-result request body)
    (let ((diagnostics (mapcar (lambda (event) (plist-get event :text))
                               (nreverse events))))
      (should (equal diagnostics
                     '("OpenRouter Batch request tools: (none)"
                       "OpenRouter Batch output items: message, tool_call(read_file)")))
      (should-not (string-match-p
                   "secret\\.el"
                   (mapconcat #'identity diagnostics "\n")))))
  (let ((ende-tool-diagnostics nil)
        (events nil))
    (let ((request
           (ende-openrouter-batch--make-request
            :callbacks (list :on-event
                             (lambda (event) (push event events))))))
      (ende-openrouter-batch--diagnose-request request)
      (ende-openrouter-batch--diagnose-result
       request (ende-protocol-json-read-string "{\"choices\":[]}"))
      (should-not events))))

(ert-deftest ende-test/openai-streamed-function-call-is-collected-without-final-output ()
  (let* ((events nil)
         (session
          (ende-codex--make-session
           :profile '(:model "gpt-test")
           :callbacks (list :on-event
                            (lambda (event) (push event events)))
           :estimated-chars 0)))
    (dolist
        (event
         (list
          (concat
           "{\"type\":\"response.output_item.added\","
           "\"output_index\":0,\"item\":{\"id\":\"fc_1\","
           "\"type\":\"function_call\",\"call_id\":\"call-1\","
           "\"name\":\"read_file\",\"arguments\":\"\"}}")
          (concat
           "{\"type\":\"response.function_call_arguments.delta\","
           "\"output_index\":0,\"item_id\":\"fc_1\","
           "\"delta\":\"{\\\"path\\\":\"}")
          (concat
           "{\"type\":\"response.function_call_arguments.delta\","
           "\"output_index\":0,\"item_id\":\"fc_1\","
           "\"delta\":\"\\\"ende.el\\\"}\"}")
          (concat
           "{\"type\":\"response.function_call_arguments.done\","
           "\"output_index\":0,\"item_id\":\"fc_1\","
           "\"arguments\":\"{\\\"path\\\":\\\"ende.el\\\"}\"}")
          (concat
           "{\"type\":\"response.output_item.done\","
           "\"output_index\":0,\"item\":{\"id\":\"fc_1\","
           "\"type\":\"function_call\",\"call_id\":\"call-1\","
           "\"name\":\"read_file\","
           "\"arguments\":\"{\\\"path\\\":\\\"ende.el\\\"}\"}}")
          (concat
           "{\"type\":\"response.completed\",\"response\":{"
           "\"output\":[],\"usage\":{\"output_tokens\":40}}}")))
      (ende-codex--handle-event
       session (ende-protocol-json-read-string event)))
    (let* ((call (car (plist-get (ende-codex--result session) :tool-calls)))
           (diagnostics
            (mapcar (lambda (event) (plist-get event :text))
                    (cl-remove-if-not
                     (lambda (event)
                       (eq (plist-get event :type) :diagnostic))
                     (nreverse events)))))
      (should (equal (plist-get call :id) "call-1"))
      (should (equal (plist-get call :name) "read_file"))
      (should (equal (plist-get call :arguments)
                     "{\"path\":\"ende.el\"}"))
      (should (member
               "OpenAI Responses response.output_item.added: function_call(read_file)"
               diagnostics))
      (should (member
               "OpenAI Responses response.output_item.done: function_call(read_file)"
               diagnostics))
      (should (member "OpenAI Responses output items: (none)" diagnostics))
      (should-not (string-match-p "ende\\.el"
                                  (mapconcat #'identity diagnostics "\n"))))))

;;;; Normal operations

;;;; Queue lifecycle

;;;; Target failure paths

;;;; Anchor ordering and conflicts

;;;; Incremental reasoning parser

;;;; SSE parser

(ert-deftest ende-test/codex-session-clears-resource-slots ()
  (let ((process-buffer (generate-new-buffer " *ende-test-codex-process*"))
        (stderr-buffer (generate-new-buffer " *ende-test-codex-stderr*"))
        (headers-file (make-temp-file "ende-test-codex-headers-"))
        (config-file (make-temp-file "ende-test-codex-config-")))
    (let ((session
           (ende-codex--make-session
            :callbacks nil
            :process-buffer process-buffer
            :stderr-buffer stderr-buffer
            :headers-file headers-file
            :config-file config-file)))
      (ende-codex--fail
       session (ende-error-create :kind 'transport :message "failed"))
      (should-not (ende-codex--session-process session))
      (should-not (ende-codex--session-process-buffer session))
      (should-not (ende-codex--session-stderr-buffer session))
      (should-not (ende-codex--session-stderr-process session))
      (should-not (ende-codex--session-headers-file session))
      (should-not (ende-codex--session-config-file session))
      (should-not (buffer-live-p process-buffer))
      (should-not (buffer-live-p stderr-buffer))
      (should-not (file-exists-p headers-file))
      (should-not (file-exists-p config-file)))))

(ert-deftest ende-test/openai-profile-uses-opencode-oauth ()
  (let ((profile (ende-provider-profile 'openai)))
    (should (eq (plist-get profile :wire-api) 'responses))
    (should (eq (plist-get profile :auth-method) 'opencode-oauth))
    (should (string= (plist-get profile :model) "gpt-5.6-luna"))
    (should (string= (plist-get profile :url)
                     "https://chatgpt.com/backend-api/codex/responses"))
    (should-not (ende-provider-configured-model profile))
    (should-not (plist-get (ende-provider-profile-snapshot 'openai) :access))
    (should-not (plist-get (ende-provider-profile-snapshot 'openai) :refresh)))
  (let ((ende-codex-model "gpt-configured"))
    (should (string= (ende-provider-configured-model
                      (ende-provider-profile 'openai))
                     "gpt-configured"))))

(ert-deftest ende-test/provider-list-and-configured-models-are-selectable ()
  (let ((ende-providers '(openai local))
        (ende-provider-profiles
         '((local :models ("local-one" "local-two")))))
    (should (equal (ende-provider-names) '(openai local)))
    (should (equal (ende-provider-models
                    (ende-provider-profile 'local))
                   '("local-one" "local-two")))))

(ert-deftest ende-test/provider-runtime-concurrency-and-parallel-settings ()
  (ende-provider-clear-runtime-settings)
  (let ((ende-provider-profiles
         '((remote :max-concurrency 3 :parallel nil))))
    (let ((profile (ende-provider-profile 'remote)))
      (should (= (ende-provider-max-concurrency profile) 3))
      (should-not (ende-provider-parallel-enabled-p profile)))
    (ende-provider-set-runtime-setting 'remote :max-concurrency 5)
    (ende-provider-set-runtime-setting 'remote :parallel t)
    (let ((profile (ende-provider-profile 'remote)))
      (should (= (ende-provider-max-concurrency profile) 5))
      (should (ende-provider-parallel-enabled-p profile)))))

(ert-deftest ende-test/openai-model-probe-parses-official-slugs ()
  (should (equal
           (ende-codex--parse-models
            (concat "{\"models\":[{\"slug\":\"gpt-5.6-luna\"},"
                    "{\"slug\":\"gpt-5.5\"},{\"title\":\"ignored\"}]}"))
           '("gpt-5.6-luna" "gpt-5.5"))))

(ert-deftest ende-test/openai-model-probe-response-is-bounded ()
  (let ((ende-provider-max-discovery-bytes 4)
        (captured nil)
        (process nil)
        (result nil)
        (real-make-process (symbol-function 'make-process)))
    (cl-letf (((symbol-function 'ende-codex--resolve-auth)
               (lambda (callback)
                 (funcall callback '(:access "access-token"))
                 nil))
              ((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq captured args
                       process
                       (funcall real-make-process
                                :name (plist-get args :name)
                                :buffer nil
                                :command (list "cat")
                                :connection-type 'pipe)))))
      (unwind-protect
          (progn
            (ende-codex-probe-models
             (lambda (models error)
               (setq result (list models error))))
            (should (process-live-p process))
            (funcall (plist-get captured :filter) process "oversized")
            (should (equal (car result) nil))
            (should (ende-error-p (cadr result)))
            (should (eq (ende-error-kind (cadr result)) 'resource))
            (should-not (process-live-p process))
            (should-not ende-codex--model-probe-processes))
        (when (and (processp process) (process-live-p process))
          (delete-process process))
        (setq ende-codex--model-probe-processes nil)))))

(ert-deftest ende-test/openai-quota-parser-and-display-are-window-aware ()
  (let* ((now 2000000000)
         (quota
          (ende-codex--parse-quota
           (concat
            "{\"rate_limit\":{"
            "\"primary_window\":{\"used_percent\":28,"
            "\"reset_at\":2000018660,\"limit_window_seconds\":18000},"
            "\"secondary_window\":{\"used_percent\":59,"
            "\"reset_at\":2000007980,\"limit_window_seconds\":604800}}}"))))
    (should (= (plist-get (plist-get quota :primary) :used-percent) 28))
    (should (= (plist-get (plist-get quota :secondary) :used-percent) 59))
    (should
     (string=
      (ende-codex-quota-string quota now)
      "5h 11m: 72% left · weekly: 41% left · reset in 2h 13m"))
    (should-error
     (ende-codex--parse-quota
      "{\"rate_limit\":{\"primary_window\":null,\"secondary_window\":null}}"))))

(ert-deftest ende-test/openai-quota-display-follows-secondary-reset ()
  (let ((now 2000000000)
        (quota
         '(:primary (:used-percent 10
				   :reset-at 2000003600
				   :window-seconds 18000)
		    :secondary (:used-percent 20
					      :reset-at 2000021600
					      :window-seconds 604800))))
    (should
     (string=
      (ende-codex-quota-string quota now)
      "1h: 90% left · weekly: 80% left · reset in 6h"))
    (should
     (string=
      (ende-codex-quota-string (list :primary (plist-get quota :primary)) now)
      "1h: 90% left"))))

(ert-deftest ende-test/openai-quota-probe-uses-account-scoped-oauth ()
  (let ((captured nil)
        (config-text nil)
        (process nil)
        (result nil)
        (failure nil)
        (real-make-process (symbol-function 'make-process)))
    (cl-letf (((symbol-function 'ende-codex--resolve-auth)
               (lambda (callback)
                 (funcall callback
                          '(:access "access-token" :account-id "account-id"))
                 nil))
              ((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq captured args)
                 (let ((config
                        (cadr (member "--config"
                                      (plist-get args :command)))))
                   (when config
                     (with-temp-buffer
                       (insert-file-contents config)
                       (setq config-text (buffer-string)))))
                 (setq process
                       (funcall real-make-process
                                :name (plist-get args :name)
                                :buffer nil
                                :command '("cat")
                                :connection-type 'pipe)))))
      (unwind-protect
          (progn
            (ende-codex-probe-quota
             (lambda (quota error)
               (setq result quota
                     failure error)))
            (let* ((command (plist-get captured :command))
                   (headers-file
                    (cadr (member "--dump-header" command))))
              (with-temp-file headers-file
                (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n"))
              (funcall
               (plist-get captured :filter)
               process
               "{\"rate_limit\":{\"primary_window\":{\"used_percent\":22}}}")
              (process-send-eof process)
              (while (process-live-p process)
                (accept-process-output process 0.05))
              (funcall (plist-get captured :sentinel) process "finished\n"))
            (should result)
            (should-not failure)
            (should (string-match-p
                     "Authorization: Bearer access-token" config-text))
            (should (string-match-p
                     "ChatGPT-Account-Id: account-id" config-text))
            (should (member "Accept: application/json" (plist-get captured :command)))
            (should-not (member "access-token" (plist-get captured :command)))
            (should-not ende-codex--quota-probe-processes))
        (when (and (processp process) (process-live-p process))
          (delete-process process))))))

(ert-deftest ende-test/selector-quota-is-scoped-and-ignores-stale-results ()
  (let ((buffer (generate-new-buffer " *ende-selector-quota-test*"))
        (quota-callback nil)
        (cancel-count 0)
        (ende-providers '(local openai))
        (ende-provider-profiles
         '((local :model "test-model" :models ("test-model"))
           (openai :model "test-model" :auth-method opencode-oauth
                   :models ("test-model"))))
        (ende-provider--runtime-settings (make-hash-table :test #'equal))
        (ende-selector--last-selection nil))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(local openai)
                ende-selector--provider-index 0)
          (cl-letf (((symbol-function 'ende-provider-probe-models)
                     (lambda (_profile callback)
                       (funcall callback '("test-model") nil)
                       (lambda () nil)))
                    ((symbol-function 'ende-codex-probe-quota)
                     (lambda (callback)
                       (setq quota-callback callback)
                       (lambda () (cl-incf cancel-count)))))
            (ende-selector--set-provider 0)
            (should-not quota-callback)
            (should (string-match-p "Quota: unavailable"
                                    (buffer-string)))
            (ende-selector--set-provider 1)
            (should quota-callback)
            (should (eq ende-selector--quota-state :loading))
            (should (string-match-p "Quota: loading\.\.\."
                                    (buffer-string)))
            (let ((now (floor (float-time))))
              (funcall
               quota-callback
               (list :primary
                     (list :used-percent 28
                           :reset-at (+ now 18660)
                           :window-seconds 18000)
                     :secondary
                     (list :used-percent 59
                           :reset-at (+ now 7980)
                           :window-seconds 604800))
               nil)
              (should (eq ende-selector--quota-state :ready))
              (should (string-match-p
                       "Quota: 5h 1[01]m: 72% left"
                       (buffer-string)))
              (ende-selector--set-provider 1)
              (let ((stale-callback quota-callback))
                (ende-selector--set-provider 0)
                (should (= cancel-count 1))
                (should (eq ende-selector--quota-state :unavailable))
                (funcall stale-callback
                         (list :primary
                               (list :used-percent 1
                                     :reset-at (+ now 18660)
                                     :window-seconds 18000))
                         nil)
                (should (eq ende-selector--quota-state :unavailable))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ende-test/model-selection-restores-by-buffer-identity ()
  (let* ((name " *ende-model-selection-identity*")
         (file "/tmp/ende-model-selection-identity.el")
         (other-file "/tmp/ende-model-selection-other.el")
         (source nil)
         (other nil)
         (reopened nil)
         (different nil)
         (ende-model-selection-scope 'buffer)
         (ende-model-selection--last-used nil)
         (ende-model-selection--buffer-selections
          (make-hash-table :test #'equal))
         (ende-provider-model "default-model"))
    (unwind-protect
        (progn
          (setq source (get-buffer-create name))
          (with-current-buffer source
            (setq buffer-file-name file)
            (ende-mode 1)
            (should-not (ende-model-selection-for-buffer source))
            (should (equal (plist-get
                            (ende-provider-profile-for-buffer source) :model)
                           "default-model"))
            (should (= (hash-table-count
                        ende-model-selection--buffer-selections)
                       0))
            (ende-model-selection-save
             '(:provider remote :model "file-model"
                         :reasoning-effort "high")
             source))
          (kill-buffer source)
          (setq source nil
                other (get-buffer-create " *ende-model-selection-latest*"))
          (with-current-buffer other
            (setq buffer-file-name other-file)
            (ende-model-selection-save
             '(:provider openrouter :model "latest-model") other))
          (kill-buffer other)
          (setq other nil
                reopened (get-buffer-create name))
          (with-current-buffer reopened
            (setq buffer-file-name file)
            (ende-mode 1)
            (should (equal (ende-model-selection-for-buffer reopened)
                           '(:provider remote :model "file-model"
                                       :reasoning-effort "high")))
            (should (equal (plist-get
                            (ende-provider-profile-for-buffer reopened) :model)
                           "file-model"))
            (should (equal
                     (plist-get
                      (ende-provider-profile-for-buffer reopened)
                      :reasoning-effort)
                     "high")))
          (kill-buffer reopened)
          (setq reopened nil
                different (get-buffer-create name))
          (with-current-buffer different
            (setq buffer-file-name other-file)
            (ende-mode 1)
            (should (equal (ende-model-selection-for-buffer different)
                           '(:provider openrouter :model "latest-model")))))
      (dolist (buffer (list source other reopened different))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest ende-test/model-selection-global-scope-shares-one-choice ()
  (let ((ende-model-selection-scope 'global)
        (ende-model-selection--last-used nil)
        (ende-model-selection--buffer-selections
         (make-hash-table :test #'equal))
        (first (generate-new-buffer " *ende-model-selection-global-one*"))
        (second (generate-new-buffer " *ende-model-selection-global-two*")))
    (unwind-protect
        (progn
          (ende-model-selection-save
           '(:provider openai :model "shared-model"
                       :reasoning-effort "medium")
           first)
          (should (equal (ende-model-selection-for-buffer second)
                         '(:provider openai :model "shared-model"
                                     :reasoning-effort "medium")))
          (should (= (hash-table-count
                      ende-model-selection--buffer-selections)
                     0)))
      (dolist (buffer (list first second))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest ende-test/direct-chat-request-uses-buffer-selection ()
  (ende-test--reset)
  (let ((ende-model-selection-scope 'buffer)
        (ende-model-selection--last-used nil)
        (ende-model-selection--buffer-selections
         (make-hash-table :test #'equal))
        (ende-llm-function (ende-test--fake-llm '("Reply"))))
    (unwind-protect
        (ende-test--with-buffer "* Root\n** Question\nAsk\n"
				(org-mode)
				(ende-test--chat-goto "** Question")
				(ende-model-selection-save
				 '(:provider openrouter :model "chat-model"))
				(let ((job (ende-ask)))
				  (should (eq (plist-get (ende-job-profile job) :name) 'openrouter))
				  (should (equal (plist-get (ende-job-profile job) :model)
						 "chat-model"))))
      (ende-test--reset))))

(ert-deftest ende-test/selector-runs-from-source-with-selected-profile ()
  (let* ((source (generate-new-buffer " *ende-selector-source*"))
         (selector (generate-new-buffer " *ende-selector-test*"))
         (ende-providers '(local))
         (ende-provider-profiles
          '((local :models ("one" "two")
                   :reasoning-effort "made-up"
                   :reasoning-effort-wire-api openrouter)))
         (ende-selector--last-selection nil)
         (queued nil))
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert "source"))
          (with-current-buffer selector
            (ende-selector-mode)
            (setq ende-selector--source-buffer source
                  ende-selector--providers '(local)
                  ende-selector--provider-index 0
                  ende-selector--models '("one" "two")
                  ende-selector--model-index 0)
            (ende-selector-next-model)
            (cl-letf (((symbol-function 'ende-enqueue-operation)
                       (lambda (type instruction profile)
                         (setq queued
                               (list type instruction profile (current-buffer))))))
              (ende-selector-insert)))
          (should (eq (nth 0 queued) 'insert))
          (should (string= (plist-get (nth 2 queued) :model) "two"))
          (should-not (plist-get (nth 2 queued) :reasoning-effort))
          (should (equal ende-selector--last-selection
                         '(:provider local :model "two")))
          (should (equal (ende-model-selection-for-buffer source)
                         '(:provider local :model "two")))
          (should (eq (nth 3 queued) source)))
      (dolist (buffer (list source selector))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest ende-test/selector-model-completion-updates-choice ()
  (let ((buffer (generate-new-buffer " *ende-selector-model-test*"))
        (completion-call nil)
        (ende-selector--last-selection nil))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(remote)
                ende-selector--provider-index 0
                ende-selector--models
                '("remote/alpha" "remote/beta" "other/gamma")
                ende-selector--model-details
                '((:id "remote/alpha" :name "Alpha Model"
                       :pricing (:prompt "0.000003" :completion "0.000009"
					 :input-cache-read "0.000001"
					 :input-cache-write "0.000004"))
                  (:id "remote/beta" :name "Beta Model")
                  (:id "other/gamma" :name "Gamma Model"))
                ende-selector--model-index 0
                ende-selector--model-state :ready)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (prompt collection predicate require-match
                                     initial-input history default &rest _)
                       (setq completion-call
                             (list :prompt prompt
                                   :collection collection
                                   :predicate predicate
                                   :require-match require-match
                                   :initial-input initial-input
                                   :history history
                                   :default default
                                   :extra-properties completion-extra-properties
                                   :styles completion-styles
                                   :ignore-case completion-ignore-case))
                       "remote/beta")))
            (ende-selector-select-model))
          (should (equal (plist-get completion-call :prompt) "Model: "))
          (should (equal (plist-get completion-call :collection)
                         '("remote/alpha" "remote/beta" "other/gamma")))
          (should-not (member "input" (plist-get completion-call :collection)))
          (should (plist-get completion-call :require-match))
          (should-not (plist-get completion-call :initial-input))
          (should (equal (plist-get completion-call :default)
                         "remote/alpha"))
          (let* ((properties (plist-get completion-call :extra-properties))
                 (affixation (plist-get properties :affixation-function))
                 (affixed (funcall affixation
                                   (plist-get completion-call :collection)))
                 (priced (car affixed))
                 (unpriced (cadr affixed))
                 (priced-model (car priced))
                 (priced-suffix (nth 2 priced)))
            (should (functionp affixation))
            (should-not (plist-get properties :annotation-function))
            (should (string= (substring-no-properties priced-model)
                             "remote/alpha"))
            (should (eq (get-text-property 0 'face priced-model)
                        'ende-selector-model-face))
            (should
             (string=
              (substring-no-properties priced-suffix)
              "  in: $3  out: $9  cache-read: $1  cache-write: $4"))
            (should (eq (get-text-property (string-match "in:" priced-suffix)
                                           'face priced-suffix)
                        'ende-selector-price-muted-face))
            (should (eq (get-text-property (string-match "out:" priced-suffix)
                                           'face priced-suffix)
                        'ende-selector-price-output-face))
            (should (eq (get-text-property
                         (string-match "cache-read:" priced-suffix)
                         'face priced-suffix)
                        'ende-selector-price-muted-face))
            (should (equal (mapcar #'substring-no-properties
                                   (list (car unpriced) (nth 2 unpriced)))
                           '("remote/beta" ""))))
          (should (equal (plist-get completion-call :styles) '(substring)))
          (should (plist-get completion-call :ignore-case))
          (should (= ende-selector--model-index 1))
          (should (string= (ende-selector--model-name) "remote/beta"))
          (should (equal ende-selector--last-selection
                         '(:provider remote :model "remote/beta"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ende-test/selector-preserves-remembered-model-after-discovery ()
  (let ((buffer (generate-new-buffer " *ende-selector-discovery-test*"))
        (probe-callback nil)
        (ende-selector--last-selection
         '(:provider remote :model "remote/remembered"))
        (ende-provider-profiles
         '((remote :url "http://remote.example/v1/chat/completions"
                   :model "remote/default"))))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(remote)
                ende-selector--provider-index 0)
          (cl-letf (((symbol-function 'ende-provider-probe-models)
                     (lambda (_profile callback)
                       (setq probe-callback callback)
                       (lambda () nil))))
            (ende-selector--set-provider 0)
            (should (string= (ende-selector--model-name)
                             "remote/remembered"))
            (should (string-match-p "validating"
                                    (buffer-string)))
            (funcall probe-callback
                     '("remote/default" "remote/remembered") nil)
            (should (string= (ende-selector--model-name)
                             "remote/remembered"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ende-test/selector-uses-cached-models-before-rendering ()
  (let* ((buffer (generate-new-buffer " *ende-selector-cached-models-test*"))
         (profile '(:name remote
			  :url "http://remote.example/v1/chat/completions"))
         (ende-providers '(remote))
         (ende-provider-profiles
          '((remote :url "http://remote.example/v1/chat/completions")))
         (ende-selector--last-selection
          '(:provider remote :model "remote/saved"))
         (ende-provider--model-cache (make-hash-table :test #'equal))
         (ende-provider--model-inflight (make-hash-table :test #'equal))
         (ende-provider--model-cancellations (make-hash-table :test #'equal))
         (ende-provider--model-generation 0))
    (unwind-protect
        (progn
          (ende-provider--finish-model-discovery
           (ende-provider--model-cache-key profile)
           '("remote/first" "remote/saved") nil nil)
          (with-current-buffer buffer
            (ende-selector-mode)
            (setq ende-selector--providers '(remote)
                  ende-selector--provider-index 0)
            (cl-letf (((symbol-function 'ende-provider-probe-models)
                       (lambda (&rest _)
                         (error "cached model catalog was not used"))))
              (ende-selector--set-provider 0)
              (should (string= (ende-selector--model-name)
                               "remote/saved"))
              (should (eq ende-selector--model-state :ready))
              (setq ende-selector--last-selection nil)
              (ende-selector--set-provider 0)
              (should (string= (ende-selector--model-name)
                               "remote/first")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ende-test/selector-validates-first-model-without-preference ()
  (let ((buffer (generate-new-buffer " *ende-selector-first-model-test*"))
        (probe-callback nil)
        (ende-providers '(remote))
        (ende-provider-profiles
         '((remote :url "http://remote.example/v1/chat/completions")))
        (ende-selector--last-selection nil))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(remote)
                ende-selector--provider-index 0)
          (cl-letf (((symbol-function 'ende-provider-probe-models)
                     (lambda (_profile callback)
                       (setq probe-callback callback)
                       (lambda () nil))))
            (ende-selector--set-provider 0)
            (should-not (ende-selector--model-name))
            (should (string-match-p "validating" (buffer-string)))
            (funcall probe-callback '("remote/first" "remote/second") nil)
            (should (string= (ende-selector--model-name)
                             "remote/first"))
            (should (eq ende-selector--model-state :ready))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ende-test/selector-validates-preferences-once-per-catalog ()
  (let ((buffer (generate-new-buffer " *ende-selector-validation-test*"))
        (messages nil)
        (ende-providers '(remote))
        (ende-provider-profiles
         '((remote :url "http://remote.example/v1/chat/completions")))
        (ende-selector--last-selection
         '(:provider remote :model "remote/missing"))
        (ende-selector--model-validations (make-hash-table :test #'equal))
        (ende-provider--model-cache (make-hash-table :test #'equal))
        (ende-provider--model-inflight (make-hash-table :test #'equal))
        (ende-provider--model-cancellations (make-hash-table :test #'equal))
        (ende-provider--model-generation 0))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(remote)
                ende-selector--provider-index 0)
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (push (apply #'format format-string args) messages)))
                    ((symbol-function 'ende-provider-probe-models)
                     (lambda (_profile callback)
                       (funcall callback '("remote/first" "remote/second") nil)
                       (lambda () nil))))
            (ende-selector--set-provider 0)
            (should (string= (ende-selector--model-name) "remote/first"))
            (should (= (cl-count-if
                        (lambda (value)
                          (string-match-p "unavailable" value))
                        messages)
                       1))
            (ende-selector--set-provider 0)
            (should (= (cl-count-if
                        (lambda (value)
                          (string-match-p "unavailable" value))
                        messages)
                       1))
            (setq ende-selector--last-selection
                  '(:provider remote :model "remote/second"))
            (ende-selector--set-provider 0)
            (should (string= (ende-selector--model-name)
                             "remote/second"))
            (should (= (cl-count-if
                        (lambda (value)
                          (string-match-p "unavailable" value))
                        messages)
                       1))
            (ende-provider-clear-discovery-cache)
            (setq ende-selector--last-selection
                  '(:provider remote :model "remote/missing"))
            (ende-selector--set-provider 0)
            (should (= (cl-count-if
                        (lambda (value)
                          (string-match-p "unavailable" value))
                        messages)
                       2))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ende-test/selector-model-completion-rejects-incomplete-discovery ()
  (let ((buffer (generate-new-buffer " *ende-selector-model-loading-test*"))
        (called nil))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(remote)
                ende-selector--provider-index 0
                ende-selector--models '("remote/default")
                ende-selector--model-index 0
                ende-selector--model-state :loading)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _)
                       (setq called t)
                       "remote/default")))
            (should-error (ende-selector-select-model) :type 'user-error))
          (should-not called))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ende-test/selector-effort-picker-uses-live-values-and-remembers-choice ()
  (let ((buffer (generate-new-buffer " *ende-selector-effort-picker-test*"))
        (calls nil)
        (ende-provider-profiles '((remote :model "remote-model")))
        (ende-model-selection-scope 'global)
        (ende-selector--effort-selections (make-hash-table :test #'equal)))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(remote)
                ende-selector--provider-index 0
                ende-selector--models '("remote-model")
                ende-selector--model-index 0
                ende-selector--model-state :ready)
          (cl-letf (((symbol-function 'ende-provider-probe-reasoning-efforts)
                     (lambda (profile model callback)
                       (push (list profile model) calls)
                       (funcall callback '("high" "low") "low" nil)
                       (lambda () nil)))
                    ((symbol-function 'completing-read)
                     (lambda (prompt collection _predicate require-match
                                     _initial _history default &rest _)
                       (should (string= prompt "Reasoning effort: "))
                       (should (equal collection '("high" "low")))
                       (should require-match)
                       (should (string= default "low"))
                       "high")))
            (ende-selector-select-effort))
          (should (= (length calls) 1))
          (should (string= (cadar calls) "remote-model"))
          (should (string= ende-selector--effort "high"))
          (should (eq ende-selector--effort-state :ready))
          (should ende-selector--effort-validated-p)
          (should (string=
                   (gethash '(remote "remote-model")
                            ende-selector--effort-selections)
                   "high"))
          (should (string-match-p "Effort:.*high" (buffer-string))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ende-test/selector-effort-is-saved-only-for-its-source-buffer ()
  (let* ((source (generate-new-buffer " *ende-effort-source-one*"))
         (other-source (generate-new-buffer " *ende-effort-source-two*"))
         (selector (generate-new-buffer " *ende-effort-source-selector*"))
         (ende-model-selection-scope 'buffer)
         (ende-model-selection--last-used nil)
         (ende-model-selection--buffer-selections
          (make-hash-table :test #'equal))
         (ende-provider-profiles '((remote :model "remote-model"))))
    (unwind-protect
        (progn
          (with-current-buffer source
            (ende-model-selection-save
             '(:provider remote :model "remote-model")))
          (with-current-buffer other-source
            (ende-model-selection-save
             '(:provider remote :model "remote-model")))
          (with-current-buffer selector
            (ende-selector-mode)
            (setq ende-selector--source-buffer source
                  ende-selector--providers '(remote)
                  ende-selector--provider-index 0
                  ende-selector--models '("remote-model")
                  ende-selector--model-index 0
                  ende-selector--model-state :ready)
            (cl-letf (((symbol-function 'ende-provider-probe-reasoning-efforts)
                       (lambda (_profile _model callback)
                         (funcall callback '("high" "low") "low" nil)
                         (lambda () nil)))
                      ((symbol-function 'completing-read)
                       (lambda (&rest _) "high")))
              (ende-selector-select-effort))
            (should (equal
                     (plist-get (ende-model-selection-for-buffer source)
                                :reasoning-effort)
                     "high"))
            (should-not
             (plist-get (ende-model-selection-for-buffer other-source)
                        :reasoning-effort))
            (setq ende-selector--source-buffer other-source)
            (ende-selector--set-effort-context)
            (should-not ende-selector--effort)))
      (dolist (buffer (list source other-source selector))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest ende-test/selector-effort-probes-cancel-on-model-change ()
  (let ((buffer (generate-new-buffer " *ende-selector-effort-cancel-test*"))
        (callback nil)
        (cancelled 0)
        (completed nil)
        (ende-provider-profiles '((remote :model "first")))
        (ende-model-selection-scope 'global)
        (ende-selector--effort-selections (make-hash-table :test #'equal)))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(remote)
                ende-selector--provider-index 0
                ende-selector--models '("first" "second")
                ende-selector--model-index 0
                ende-selector--model-state :ready)
          (cl-letf (((symbol-function 'ende-provider-probe-reasoning-efforts)
                     (lambda (_profile _model probe-callback)
                       (setq callback probe-callback)
                       (lambda () (cl-incf cancelled))))
                    ((symbol-function 'completing-read)
                     (lambda (&rest _)
                       (setq completed t)
                       "high")))
            (ende-selector-select-effort)
            (should (eq ende-selector--effort-state :loading))
            (ende-selector-next-model)
            (should (= cancelled 1))
            (should (string= (ende-selector--model-name) "second"))
            (funcall callback '("high") "high" nil)
            (should-not completed)
            (should-not ende-selector--effort)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ende-test/selector-validates-remembered-effort-before-dispatch ()
  (let ((source (generate-new-buffer " *ende-selector-effort-source*"))
        (selector (generate-new-buffer " *ende-selector-effort-dispatch*"))
        (probe-callback nil)
        (queued nil)
        (ende-provider-profiles '((remote :model "remote-model")))
        (ende-model-selection-scope 'global)
        (ende-selector--effort-selections (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (puthash '(remote "remote-model") "high"
                   ende-selector--effort-selections)
          (with-current-buffer selector
            (ende-selector-mode)
            (setq ende-selector--source-buffer source
                  ende-selector--providers '(remote)
                  ende-selector--provider-index 0
                  ende-selector--models '("remote-model")
                  ende-selector--model-index 0
                  ende-selector--model-state :ready)
            (ende-selector--set-effort-context)
            (cl-letf (((symbol-function 'ende-provider-probe-reasoning-efforts)
                       (lambda (_profile _model callback)
                         (setq probe-callback callback)
                         (lambda () nil)))
                      ((symbol-function 'ende-enqueue-operation)
                       (lambda (type instruction profile)
                         (setq queued (list type instruction profile)))))
              (ende-selector-insert)
              (should (eq ende-selector--effort-state :loading))
              (should-not queued)
              (funcall probe-callback '("high" "low") "low" nil)))
          (should (equal (take 2 queued) '(insert nil)))
          (should (string= (plist-get (nth 2 queued) :model) "remote-model"))
          (should (string= (plist-get (nth 2 queued) :reasoning-effort) "high")))
      (dolist (buffer (list source selector))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest ende-test/selector-rejects-effort-removed-by-live-validation ()
  (let ((source (generate-new-buffer " *ende-selector-effort-stale-source*"))
        (selector (generate-new-buffer " *ende-selector-effort-stale*"))
        (queued nil)
        (ende-provider-profiles '((remote :model "remote-model")))
        (ende-model-selection-scope 'global)
        (ende-selector--effort-selections (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (puthash '(remote "remote-model") "high"
                   ende-selector--effort-selections)
          (with-current-buffer selector
            (ende-selector-mode)
            (setq ende-selector--source-buffer source
                  ende-selector--providers '(remote)
                  ende-selector--provider-index 0
                  ende-selector--models '("remote-model")
                  ende-selector--model-index 0
                  ende-selector--model-state :ready)
            (ende-selector--set-effort-context)
            (cl-letf (((symbol-function 'ende-provider-probe-reasoning-efforts)
                       (lambda (_profile _model callback)
                         (funcall callback '("low") "low" nil)
                         (lambda () nil)))
                      ((symbol-function 'ende-enqueue-operation)
                       (lambda (&rest _)
                         (setq queued t))))
              (ende-selector-insert))
            (should-not queued)
            (should-not ende-selector--effort)
            (should (eq ende-selector--effort-state :error))
            (should-not (gethash '(remote "remote-model")
                                 ende-selector--effort-selections))))
      (dolist (buffer (list source selector))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest ende-test/selector-uses-bottom-window-and-quits-it-on-close ()
  (let ((source (generate-new-buffer " *ende-selector-window-source*"))
        (display-action nil)
        (quit-window-call nil)
        (probe-profile nil)
        (selector nil))
    (unwind-protect
        (with-current-buffer source
          (cl-letf (((symbol-function 'display-buffer)
                     (lambda (buffer action)
                       (setq display-action (list buffer action))
                       (selected-window)))
                    ((symbol-function 'quit-window)
                     (lambda (kill window)
                       (setq quit-window-call (list kill window))))
                    ((symbol-function 'ende-provider-probe-models)
                     (lambda (profile callback)
                       (setq probe-profile profile)
                       (funcall callback '("local-model") nil)
                       (lambda () nil))))
            (let ((ende-providers '(local))
                  (ende-provider-profiles
                   '((local :model "local-model"))))
              (ende-selector-open)
              (setq selector (get-buffer ende-selector-buffer-name))
              (should (eq (plist-get probe-profile :name) 'local))
              (should (eq (car display-action) selector))
              (should (eq (car (car (cadr display-action)))
                          #'display-buffer-at-bottom))
              (should (eq (cdr (assq 'window-height
                                     (cdr (cadr display-action))))
                          #'ende-selector--fit-window))
              (should (equal (cdr (assq 'window-min-height
                                        (cdr (cadr display-action))))
                             ende-selector-window-min-height))
              (with-current-buffer selector
                (ende-selector-close))
              (should (equal (car quit-window-call) t))
              (should (eq (cadr quit-window-call) (selected-window)))
              (should-not (buffer-live-p selector)))))
      (when (buffer-live-p selector)
        (kill-buffer selector))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(ert-deftest ende-test/selector-changes-provider-runtime-settings ()
  (ende-provider-clear-runtime-settings)
  (let ((ende-providers '(remote))
        (ende-provider-profiles
         '((remote :model "remote-model" :max-concurrency 2
                   :parallel nil))))
    (with-temp-buffer
      (ende-selector-mode)
      (setq ende-selector--providers '(remote)
            ende-selector--provider-index 0
            ende-selector--models '("remote-model")
            ende-selector--model-index 0)
      (cl-letf (((symbol-function 'read-number)
                 (lambda (&rest _) 4)))
        (ende-selector-set-concurrency))
      (should (= (ende-provider-max-concurrency
                  (ende-provider-profile 'remote))
                 4))
      (should-not (ende-selector--provider-parallel-p))
      (ende-selector-toggle-parallel)
      (should (ende-selector--provider-parallel-p))
      (ende-selector--render)
      (dolist (key '("Provider:" "Model:" "Concurrency:" "Parallel:" "Effort:"
                     "Quota: unavailable"
                     "i  - insert" "a  - ask" "I  - insert anywhere"
                     "k  - compact chat context"
                     "w  - write anywhere"
                     "m  - select/search model"
                     "e  - select reasoning effort"
                     "s  - summarize"
                     "r  - rubber duck"
                     "RET       - confirm and save selection"
                     "C-c C-c   - approve all LLM changes"
                     "C-c C-k   - reject all LLM changes"))
        (should (string-match-p (regexp-quote key) (buffer-string)))))))

(ert-deftest ende-test/selector-runtime-keybindings-survive-reload ()
  (let ((original-c (lookup-key ende-selector-mode-map (kbd "c")))
        (original-C (lookup-key ende-selector-mode-map (kbd "C")))
        (original-P (lookup-key ende-selector-mode-map (kbd "P")))
        (original-e (lookup-key ende-selector-mode-map (kbd "e")))
        (original-a (lookup-key ende-selector-mode-map (kbd "a")))
        (original-k (lookup-key ende-selector-mode-map (kbd "k"))))
    (unwind-protect
        (progn
          ;; Simulate a keymap retained from before the new bindings existed.
          (define-key ende-selector-mode-map (kbd "c") nil)
          (define-key ende-selector-mode-map (kbd "C") nil)
          (define-key ende-selector-mode-map (kbd "P") nil)
          (define-key ende-selector-mode-map (kbd "e") nil)
          (define-key ende-selector-mode-map (kbd "a") nil)
          (define-key ende-selector-mode-map (kbd "k") nil)
          (with-temp-buffer
            (ende-selector-mode)
            (should (eq (key-binding (kbd "c"))
                        #'ende-selector-set-concurrency))
            (should (eq (key-binding (kbd "C"))
                        #'ende-selector-set-concurrency))
            (should (eq (key-binding (kbd "P"))
                        #'ende-selector-toggle-parallel))
            (should (eq (key-binding (kbd "n"))
                        #'ende-selector-next-provider))
            (should (eq (key-binding (kbd "p"))
                        #'ende-selector-previous-provider))
            (should (eq (key-binding (kbd "m"))
                        #'ende-selector-select-model))
            (should (eq (key-binding (kbd "e"))
                        #'ende-selector-select-effort))
            (should (eq (key-binding (kbd "a"))
                        #'ende-selector-ask))
            (should (eq (key-binding (kbd "k"))
                        #'ende-selector-compact))
            (should (eq (key-binding (kbd "RET"))
                        #'ende-selector-confirm))
            (should (eq (key-binding (kbd "C-c C-c"))
                        #'ende-selector-approve-source-output))
            (should (eq (key-binding (kbd "C-c C-k"))
                        #'ende-selector-reject-source-output))
            (should-not (eq (key-binding (kbd "DEL"))
                            #'ende-selector-reject-source-output))
            (should-not (eq (key-binding (kbd "<backspace>"))
                            #'ende-selector-reject-source-output))
            (should (eq (key-binding (kbd "w"))
                        #'ende-selector-write-anywhere))
            (should (eq (key-binding (kbd "I"))
                        #'ende-selector-insert-anywhere))))
      (define-key ende-selector-mode-map (kbd "c") original-c)
      (define-key ende-selector-mode-map (kbd "C") original-C)
      (define-key ende-selector-mode-map (kbd "P") original-P)
      (define-key ende-selector-mode-map (kbd "e") original-e)
      (define-key ende-selector-mode-map (kbd "a") original-a)
      (define-key ende-selector-mode-map (kbd "k") original-k))))

(ert-deftest ende-test/selector-labels-are-single-line-and-display-bounded ()
  (let ((multiline (ende-selector--display-label "provider\nspoof" 24))
        (long (ende-selector--display-label
               "provider-name-that-is-longer-than-the-row" 24)))
    (should (string-match-p "provider spoof" multiline))
    (should-not (string-match-p "\n" multiline))
    (should (= (string-width multiline) 24))
    (should (string-suffix-p "..." (string-trim-right long)))
    (should (= (string-width long) 24))))

(ert-deftest ende-test/selector-probe-callbacks-are-generation-bound ()
  (let ((buffer (generate-new-buffer " *ende-selector-probe-test*"))
        (callbacks nil)
        (cancelled 0)
        (ende-model-selection-scope 'buffer)
        (ende-model-selection--last-used nil)
        (ende-model-selection--buffer-selections
         (make-hash-table :test #'equal))
        (ende-provider-profiles
         '((remote :url "http://remote.example/v1"
                   :model "configured"))))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(remote)
                ende-selector--provider-index 0
                ende-selector--models '("configured")
                ende-selector--model-index 0)
          (cl-letf (((symbol-function 'ende-provider-probe-models)
                     (lambda (_profile callback)
                       (setq callbacks (append callbacks (list callback)))
                       (lambda () (setq cancelled (1+ cancelled))))))
            (ende-selector--set-provider 0)
            (let ((stale (car callbacks)))
              (ende-selector--cancel-probe)
              (ende-selector--set-provider 0)
              (funcall stale '("stale") nil)
              (should (equal ende-selector--models '("configured")))
              (let ((current (car (last callbacks))))
                (kill-buffer buffer)
                (should (= cancelled 2))
                (funcall current '("after-kill") nil)
                (should-not (buffer-live-p buffer))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ende-test/openai-request-body-matches-codex-responses-shape ()
  (let* ((body (ende-protocol-json-read-string
                (json-encode
                 (ende-codex--request-body
                  (list (cons "system" "rules")
                        (cons "user" "question")
                        (cons "assistant" "answer"))
                  '(:model "gpt-test")))))
         (input (ende-protocol-json-get "input" body))
         (user (aref input 1))
         (user-content (aref (ende-protocol-json-get "content" user) 0))
         (assistant (aref input 2))
         (assistant-content
          (aref (ende-protocol-json-get "content" assistant) 0)))
    (should (eq (ende-protocol-json-get "store" body) :false))
    (should (eq (ende-protocol-json-get "stream" body) t))
    (should (string= (ende-protocol-json-get "text" user-content)
                     "question"))
    (should (string= (ende-protocol-json-get "type" user-content)
                     "input_text"))
    (should (string= (ende-protocol-json-get "text" assistant-content)
                     "answer"))
    (should (string= (ende-protocol-json-get "type" assistant-content)
                     "output_text"))))

(ert-deftest ende-test/openai-http-error-keeps-provider-detail ()
  (let ((headers-file (make-temp-file "ende-openai-error-")))
    (unwind-protect
        (progn
          (with-temp-file headers-file
            (insert "HTTP/1.1 400 Bad Request\r\n\r\n"))
          (let* ((session
                  (ende-codex--make-session
                   :headers-file headers-file
                   :preview-parts '("{\"detail\":\"invalid request body\"}")))
                 (error (ende-codex--http-error session)))
            (should (string= (ende-error-message error)
                             "HTTP status 400: invalid request body"))))
      (when (file-exists-p headers-file)
        (delete-file headers-file)))))

(ert-deftest ende-test/openai-auth-entry-is-read-from-opencode-file ()
  (let* ((file (make-temp-file "ende-openai-auth-test-"))
         (ende-codex-auth-file file))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "{\"openai\":{\"type\":\"oauth\","
                    "\"access\":\"access-token\","
                    "\"refresh\":\"refresh-token\","
                    "\"expires\":9999999999999,"
                    "\"accountId\":\"account-id\"}}"))
          (let ((auth (ende-codex--auth-entry)))
            (should (string= (plist-get auth :access) "access-token"))
            (should (string= (plist-get auth :refresh) "refresh-token"))
            (should (string= (plist-get auth :account-id) "account-id"))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest ende-test/openai-transport-keeps-token-out-of-command-arguments ()
  (let* ((auth-file (make-temp-file "ende-openai-auth-test-"))
         (ende-codex-auth-file auth-file)
         (captured nil)
         (process nil)
         (result nil)
         (failure nil)
         (real-make-process (symbol-function 'make-process)))
    (unwind-protect
        (progn
          (with-temp-file auth-file
            (insert "{\"openai\":{\"type\":\"oauth\","
                    "\"access\":\"access-token\","
                    "\"refresh\":\"refresh-token\","
                    "\"expires\":9999999999999,"
                    "\"accountId\":\"account-id\"}}"))
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest args)
                       (setq captured args)
                       (setq process
                             (funcall real-make-process
                                      :name (plist-get args :name)
                                      :buffer (plist-get args :buffer)
                                      :command (list "cat")
                                      :connection-type 'pipe)))))
            (ende-codex-default
             (list (cons "user" "body"))
             (list :on-done (lambda (value) (setq result value))
                   :on-error (lambda (value) (setq failure value)))
             (ende-provider-profile 'openai))
            (let* ((command (plist-get captured :command))
                   (config (cadr (member "--config" command)))
                   (headers-file (cadr (member "--dump-header" command)))
                   (filter (plist-get captured :filter))
                   (sentinel (plist-get captured :sentinel)))
              (should (not (member "access-token" command)))
              (should (member "Accept: text/event-stream" command))
              (should (file-exists-p config))
              (with-temp-file headers-file
                (insert "HTTP/1.1 200 OK\r\n\r\n"))
              (with-temp-buffer
                (insert-file-contents config)
                (should (string-match-p "Authorization: Bearer access-token"
                                        (buffer-string))))
              (should (equal (cadr (member "--user-agent" command))
                             ende-client-name))
              (funcall
               filter
               process
               (concat "data: {\"type\":\"response.output_text.delta\","
                       "\"delta\":\"answer\"}\n\n"
                       "data: {\"type\":\"response.completed\","
                       "\"response\":{\"model\":\"gpt-test\","
                       "\"usage\":{\"output_tokens\":7}}}"))
              (while (process-live-p process)
                (accept-process-output process 0.05))
              (funcall sentinel process "finished\n")
              (should-not failure)
              (should (string= (plist-get result :content) "answer"))
              (should (string= (plist-get result :model) "gpt-test"))
              (should (= (plist-get result :tokens) 7))
              (should (string= (plist-get result :finish-reason) "stop"))
              (should-not (file-exists-p config)))))
      (when (and (processp process) (process-live-p process))
        (delete-process process))
      (when (file-exists-p auth-file)
        (delete-file auth-file)))))

(ert-deftest ende-test/openai-rejects-explicit-non-sse-content-type ()
  (let ((headers-file (make-temp-file "ende-openai-headers-"))
        (process (make-process :name "ende-openai-test"
                               :command (list "true") :noquery t))
        (result nil)
        (failure nil)
        (events nil))
    (unwind-protect
        (let ((session
               (ende-codex--make-session
                :profile '(:model "gpt-test")
                :process process
                :headers-file headers-file
                :callbacks
                (list :on-event (lambda (event) (push event events))
                      :on-done (lambda (value) (setq result value))
                      :on-error (lambda (error) (setq failure error)))
                :sse (ende-protocol-make-sse)
                :estimated-chars 0
                :received-bytes 0
                :preview-bytes 0)))
          (with-temp-file headers-file
            (insert "HTTP/2 200\r\nContent-Type: application/json\r\n\r\n"))
          (ende-codex--filter
           session process
           (concat "data: {\"type\":\"response.output_text.delta\","
                   "\"delta\":\"answer\"}\n\n"
                   "data: {\"type\":\"response.completed\","
                   "\"response\":{\"model\":\"gpt-test\"}}\n\n"
                   "data: [DONE]\n\n"))
          (while (process-live-p process)
            (accept-process-output process 0.05))
          (ende-codex--sentinel session process "finished\n")
          (should-not result)
          (should (eq (ende-error-kind failure) 'protocol))
          (should (string-match-p "application/json"
                                  (ende-error-message failure)))
          (let ((partial
                 (cl-find-if
                  (lambda (event)
                    (eq (plist-get event :type) :partial-result))
                  events)))
            (should (string= (plist-get (plist-get partial :result) :content)
                             "answer"))))
      (when (process-live-p process)
        (delete-process process))
      (when (file-exists-p headers-file)
        (delete-file headers-file)))))

(ert-deftest ende-test/openai-missing-content-type-requires-terminal-event ()
  (let ((headers-file (make-temp-file "ende-openai-headers-"))
        (process (make-process :name "ende-openai-incomplete-test"
                               :command (list "true") :noquery t))
        (failure nil))
    (unwind-protect
        (let ((session
               (ende-codex--make-session
                :profile '(:model "gpt-test")
                :process process
                :headers-file headers-file
                :callbacks (list :on-error
                                 (lambda (error) (setq failure error)))
                :sse (ende-protocol-make-sse)
                :estimated-chars 0
                :received-bytes 0
                :preview-bytes 0)))
          (with-temp-file headers-file
            (insert "HTTP/2 200\r\n\r\n"))
          (ende-codex--filter session process "data: [DONE]\n\n")
          (while (process-live-p process)
            (accept-process-output process 0.05))
          (ende-codex--sentinel session process "finished\n")
          (should (eq (ende-error-kind failure) 'protocol))
          (should (string-match-p "without a terminal response event"
                                  (ende-error-message failure))))
      (when (process-live-p process)
        (delete-process process))
      (when (file-exists-p headers-file)
        (delete-file headers-file)))))

(ert-deftest ende-test/openai-refresh-rotates-opencode-credentials-safely ()
  (let* ((auth-file (make-temp-file "ende-openai-auth-test-"))
         (ende-codex-auth-file auth-file)
         (ende-codex-issuer "https://auth.openai.com:443")
         (captured nil)
         (process nil)
         (refreshed nil)
         (refreshed-again nil)
         (make-process-count 0)
         (real-make-process (symbol-function 'make-process))
         (auth '(:access "old-access" :refresh "old-refresh"
			 :expires 0 :account-id "account-id")))
    (unwind-protect
        (progn
          (with-temp-file auth-file
            (insert "{\"openai\":{\"type\":\"oauth\","
                    "\"access\":\"old-access\","
                    "\"refresh\":\"old-refresh\","
                    "\"expires\":0,"
                    "\"accountId\":\"account-id\"}}"))
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest args)
                       (setq make-process-count (1+ make-process-count))
                       (setq captured args)
                       (setq process
                             (funcall real-make-process
                                      :name (plist-get args :name)
                                      :buffer (plist-get args :buffer)
                                      :stderr (plist-get args :stderr)
                                      :command (list "cat")
                                      :connection-type 'pipe
                                      :noquery t)))))
            (ende-codex--refresh-auth auth
                                      (lambda (value) (setq refreshed value)))
            (ende-codex--refresh-auth
             auth (lambda (value) (setq refreshed-again value)))
            (should (= make-process-count 1))
            (let* ((command (plist-get captured :command))
                   (headers-file
                    (cadr (member "--dump-header" command)))
                   (filter (plist-get captured :filter))
                   (sentinel (plist-get captured :sentinel)))
              (should-not (member "old-refresh" command))
              (with-temp-file headers-file
                (insert "HTTP/1.1 200 OK\r\n\r\n"))
              (with-current-buffer (plist-get captured :buffer)
                (erase-buffer))
              (when (process-live-p process)
                (process-send-eof process))
              (while (process-live-p process)
                (accept-process-output process 0.05))
              (with-current-buffer (plist-get captured :buffer)
                (erase-buffer))
              (funcall filter process
                       (concat "{\"access_token\":\"new-access\","
                               "\"refresh_token\":\"new-refresh\","
                               "\"expires_in\":3600}"))
              (funcall sentinel process "finished\n")
              (should (string= (plist-get refreshed :access) "new-access"))
              (should (string= (plist-get refreshed :refresh) "new-refresh"))
              (should (equal refreshed-again refreshed))
              (should (string= (plist-get (ende-codex--auth-entry) :refresh)
                               "new-refresh")))))
      (when (and (processp process) (process-live-p process))
        (delete-process process))
      (when (file-exists-p auth-file)
        (delete-file auth-file)))))

(ert-deftest ende-test/openai-refresh-startup-failure-does-not-wedge-flight ()
  (let* ((key '(ende-test-refresh))
         (result nil)
         (flight
          (ende-codex--make-refresh-flight
           :key key
           :auth '(:refresh "refresh-token")
           :waiters
           (list
            (ende-codex--make-refresh-waiter
             :callback (lambda (value) (setq result value))))))
         (ende-codex--refresh-flights (make-hash-table :test #'equal))
         (ende-codex--refresh-processes nil))
    (puthash key flight ende-codex--refresh-flights)
    (cl-letf (((symbol-function 'make-temp-file)
               (lambda (&rest _)
                 (error "temporary file creation failed"))))
      (ende-codex--start-refresh-flight flight))
    (should (ende-codex--refresh-flight-finished flight))
    (should (ende-error-p result))
    (should-not (gethash key ende-codex--refresh-flights))
    (should-not ende-codex--refresh-processes)
    (should-not (buffer-live-p
                 (ende-codex--refresh-flight-response-buffer flight)))
    (should-not (buffer-live-p
                 (ende-codex--refresh-flight-stderr-buffer flight)))))

(ert-deftest ende-test/openai-credentials-use-only-official-endpoints ()
  (should (string= (ende-codex--validated-generation-url
                    "https://chatgpt.com:443/backend-api/codex/responses")
                   "https://chatgpt.com:443/backend-api/codex/responses"))
  (should (string= (ende-codex--validated-usage-url)
                   "https://chatgpt.com/backend-api/wham/usage"))
  (should-error
   (ende-codex--validated-generation-url
    "https://chatgpt.com.attacker.test/backend-api/codex/responses"))
  (should-error
   (ende-codex--validated-generation-url
    "https://chatgpt.com/backend-api/codex/responses?redirect=attacker"))
  (let ((ende-codex-usage-endpoint
         "https://chatgpt.com/backend-api/wham/usage?redirect=attacker"))
    (should-error (ende-codex--validated-usage-url)))
  (let ((ende-codex-issuer "https://auth.openai.com.attacker.test"))
    (should-error (ende-codex--validated-refresh-url))))

(ert-deftest ende-test/openai-request-body-applies-output-token-limit ()
  (let ((ende-max-output-tokens 123))
    (let ((body (ende-codex--request-body
                 (list (cons "user" "body"))
                 '(:model "test-model"))))
      (should (= (alist-get :max_output_tokens body) 123)))))

(ert-deftest ende-test/openai-credential-replacement-is-side-by-side ()
  (let* ((directory (make-temp-file "ende-openai-auth-dir-" t))
         (file (expand-file-name "auth.json" directory))
         (ende-codex-auth-file file)
         (old '(:access "old-access" :refresh "old-refresh"
			:expires 0 :account-id "account-id"))
         (new '(:access "new-access" :refresh "new-refresh"
			:expires 9999999999999 :account-id "account-id")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "{\"openai\":{\"type\":\"oauth\","
                    "\"access\":\"old-access\","
                    "\"refresh\":\"old-refresh\","
                    "\"expires\":0,\"accountId\":\"account-id\"}}"))
          (ende-codex--store-auth new (ende-codex--auth-version old))
          (should (string= (plist-get (ende-codex--auth-entry) :access)
                           "new-access"))
          (should (= (logand (file-modes file) #o777) #o600))
          (should-not
           (directory-files directory nil "\\`\\.ende-openai-auth-" t))
          (should-error
           (ende-codex--store-auth
            old (ende-codex--auth-version
                 '(:access "other" :refresh "other" :expires 0
			   :account-id "account-id")))))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest ende-test/openai-responses-events-normalize-to-ende-result ()
  (let* ((events nil)
         (result nil)
         (failure nil)
         (session
          (ende-codex--make-session
           :profile '(:model "gpt-test")
           :callbacks (list :on-event (lambda (event) (push event events))
                            :on-done (lambda (value) (setq result value))
                            :on-error (lambda (value) (setq failure value)))
           :sse (ende-protocol-make-sse)
           :estimated-chars 0)))
    (ende-codex--handle-event
     session
     (ende-protocol-json-read-string
      "{\"type\":\"response.created\",\"response\":{\"model\":\"gpt-actual\"}}"))
    (ende-codex--handle-event
     session
     (ende-protocol-json-read-string
      "{\"type\":\"response.reasoning_summary_text.delta\",\"delta\":\"thinking\"}"))
    (ende-codex--handle-event
     session
     (ende-protocol-json-read-string
      "{\"type\":\"response.output_text.delta\",\"delta\":\"answer\"}"))
    (ende-codex--handle-event
     session
     (ende-protocol-json-read-string
      "{\"type\":\"response.completed\",\"response\":{\"model\":\"gpt-actual\",\"usage\":{\"output_tokens\":7}}}"))
    (setq result (ende-codex--result session))
    (should-not failure)
    (should (string= (plist-get result :content) "answer"))
    (should (string= (plist-get result :thinking) "thinking"))
    (should (string= (plist-get result :model) "gpt-actual"))
    (should (= (plist-get result :tokens) 7))
    (should (ende-codex--session-terminal-event session))
    (should-not (ende-codex--session-terminal session))))

;;;; Profiles, history, and transport command safety

(ert-deftest ende-test/provider-discovery-cache-is-bounded ()
  (let ((ende-provider--discovery-cache (make-hash-table :test #'equal))
        (ende-provider--discovery-inflight (make-hash-table :test #'equal))
        (ende-provider-discovery-cache-size 2))
    (dotimes (index 5)
      (ende-provider--finish-discovery (list index) (list :model index)))
    (should (= (hash-table-count ende-provider--discovery-cache) 2))))

(ert-deftest ende-test/provider-client-display-names ()
  (should (string= (ende-provider-client-name
                    '(:url "http://localhost:8080/v1"))
                   "localhost"))
  (should (string= (ende-provider-client-name
                    '(:url "http://192.168.1.20:8080/v1"))
                   "192.168.1.20"))
  (should (string= (ende-provider-client-name
                    '(:url "https://api.openai.com/v1/chat/completions"))
                   "OpenAI")))

(ert-deftest ende-test/provider-discovery-parses-model-metadata ()
  (let* ((profile '(:url "http://localhost:8080/v1/chat/completions"
			 :model "llama3"))
         (metadata
          (ende-provider--discovery-metadata
           profile
           (ende-protocol-json-read-string
            (concat
             "{\"object\":\"list\",\"data\":["
             "{\"id\":\"llama3\",\"name\":\"Llama 3\","
             "\"owned_by\":\"llama.cpp\",\"pricing\":{"
             "\"prompt\":\"0.000003\",\"completion\":\"0.000015\","
             "\"input_cache_read\":\"0.000001\","
             "\"input_cache_write\":\"0.000004\","
             "\"input_cache_write_1h\":\"0.000005\","
             "\"internal_reasoning\":\"0\"}},"
             "{\"id\":\"llama4\"}]}")))))
    (should (string= (ende-provider-models-url profile)
                     "http://localhost:8080/v1/models"))
    (should (eq (plist-get metadata :source) :discovery))
    (should (equal (plist-get metadata :models) '("llama3" "llama4")))
    (let ((detail (car (plist-get metadata :model-details))))
      (should (string= (plist-get detail :id) "llama3"))
      (should (string= (plist-get detail :name) "Llama 3"))
      (should (equal (plist-get detail :pricing)
                     '(:prompt "0.000003"
			       :completion "0.000015"
			       :internal-reasoning "0"
			       :input-cache-read "0.000001"
			       :input-cache-write "0.000004"
			       :input-cache-write-1h "0.000005"))))
    (should (string= (plist-get metadata :model) "llama3"))
    (should (string= (plist-get metadata :owner) "llama.cpp"))
    (should (string= (plist-get metadata :client) "llama.cpp"))))

(ert-deftest ende-test/codex-effort-probe-uses-selected-model-capabilities ()
  (let ((calls 0)
        (results nil)
        (entries
         (ende-codex--parse-model-entries
          (concat
           "{\"models\":["
           "{\"slug\":\"other\",\"supported_reasoning_levels\":[{\"effort\":\"none\"}]},"
           "{\"slug\":\"selected\",\"default_reasoning_level\":\"low\","
           "\"supported_reasoning_levels\":[{\"effort\":\"high\"},{\"effort\":\"low\"},"
           "{\"effort\":\"\"},{\"effort\":\"low\"}]}]}"))))
    (cl-letf (((symbol-function 'ende-codex--probe-model-entries)
               (lambda (callback)
                 (cl-incf calls)
                 (funcall callback entries nil)
                 (lambda () nil))))
      (dotimes (_ 2)
        (ende-codex-probe-reasoning-efforts
         nil "selected"
         (lambda (efforts default error)
           (push (list efforts default error) results))))
      (should (= calls 2))
      (should (equal results
                     '((("high" "low") "low" nil)
                       (("high" "low") "low" nil)))))))

(ert-deftest ende-test/openrouter-effort-probe-is-live-and-cache-independent ()
  (let ((ende-provider-discovery-enabled t)
        (ende-provider--discovery-cache (make-hash-table :test #'equal))
        (ende-provider--model-cache (make-hash-table :test #'equal))
        (requests nil)
        (results nil)
        (profile (ende-provider-profile 'openrouter)))
    (puthash 'metadata 'unchanged ende-provider--discovery-cache)
    (puthash 'models 'unchanged ende-provider--model-cache)
    (cl-letf
        (((symbol-function 'ende-provider--start-live-model-request)
          (lambda (_profile url callback &optional _discovery-key)
            (push url requests)
            (funcall
             callback
             (ende-protocol-json-read-string
              (concat
               "{\"data\":["
               "{\"id\":\"other\",\"reasoning\":{\"supported_efforts\":[\"none\"]}},"
               "{\"id\":\"selected\",\"reasoning\":{\"supported_efforts\":[\"high\",\"low\",\"high\"],"
               "\"default_effort\":\"low\"}}]}"))
             nil)
            (lambda () nil))))
      (dotimes (_ 2)
        (ende-provider-probe-reasoning-efforts
         profile "selected"
         (lambda (efforts default error)
           (push (list efforts default error) results))))
      (should (equal requests
                     '("https://openrouter.ai/api/v1/models"
                       "https://openrouter.ai/api/v1/models")))
      (should (equal results
                     '((("high" "low") "low" nil)
                       (("high" "low") "low" nil))))
      (should (eq (gethash 'metadata ende-provider--discovery-cache)
                  'unchanged))
      (should (eq (gethash 'models ende-provider--model-cache) 'unchanged)))))

(ert-deftest ende-test/reasoning-effort-discovery-is-limited-to-builtins ()
  (let* ((called nil)
         (result nil)
         (ende-provider-discovery-enabled t)
         (profile
          (list :name 'remote :reasoning-effort-discovery-function
                (lambda (&rest _)
                  (setq called t)))))
    (ende-provider-probe-reasoning-efforts
     profile "remote-model"
     (lambda (efforts default error)
       (setq result (list efforts default error))))
    (should-not called)
    (should-not (nth 0 result))
    (should-not (nth 1 result))
    (should (ende-error-p (nth 2 result)))
    (should (string-match-p "no live reasoning-effort endpoint"
                            (ende-error-message (nth 2 result))))))

(ert-deftest ende-test/reasoning-effort-is-serialized-only-by-supported-apis ()
  (let* ((messages (list (cons "user" "question")))
         (openrouter
          (list :model "router-model" :temperature 0.2
                :reasoning-effort-wire-api 'openrouter
                :reasoning-effort "high"))
         (responses
          (list :model "codex-model" :reasoning-effort-wire-api 'responses
                :reasoning-effort "low"))
         (unsupported (list :model "local-model" :temperature 0.2
                            :reasoning-effort "high"))
         (router-body
          (ende-protocol-json-read-string
           (json-encode (ende-llm--request-body messages openrouter))))
         (responses-body
          (ende-protocol-json-read-string
           (json-encode (ende-codex--request-body messages responses))))
         (unsupported-body
          (ende-protocol-json-read-string
           (json-encode (ende-llm--request-body messages unsupported)))))
    (should (string=
             (ende-protocol-json-get
              "effort" (ende-protocol-json-get "reasoning" router-body))
             "high"))
    (should (string=
             (ende-protocol-json-get
              "effort" (ende-protocol-json-get "reasoning" responses-body))
             "low"))
    (should-not (ende-protocol-json-has-key-p "reasoning" unsupported-body))))

(ert-deftest ende-test/provider-model-price-annotation-omits-invalid-fields ()
  (should
   (string=
    (ende-provider-model-price-annotation
     '(:pricing (:prompt "0" :completion "0.000015"
                         :input-cache-read "not-a-price"
                         :input-cache-write-1h "0.000005")))
    "  in: free  out: $15  cache-write-1h: $5")))

(ert-deftest ende-test/provider-model-price-annotation-formats-small-values ()
  (should
   (string=
    (ende-provider-model-price-annotation
     '(:pricing (:prompt "0.000003"
			 :completion "0.000015"
			 :input-cache-read "0.0000003")))
    "  in: $3  out: $15  cache-read: $0.30")))

(ert-deftest ende-test/provider-discovery-does-not-borrow-unrelated-owner ()
  (let* ((profile '(:url "https://example.test/v1/chat/completions"
			 :model "requested"))
         (metadata
          (ende-provider--discovery-metadata
           profile
           (ende-protocol-json-read-string
            "{\"data\":[{\"id\":\"other\",\"owned_by\":\"wrong-owner\"}]}"))))
    (should (equal (plist-get metadata :models) '("other")))
    (should-not (plist-get metadata :owner))
    (should-not (plist-get metadata :client))))

(ert-deftest ende-test/provider-discovery-is-async-and-cached ()
  (let ((profile '(:name local
			 :url "http://localhost:8080/v1/chat/completions"
			 :model "llama3"))
        (captured nil)
        (process nil)
        (values nil)
        (probe-values nil)
        (make-process-count 0)
        (real-make-process (symbol-function 'make-process)))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq captured args)
                 (setq make-process-count (1+ make-process-count))
                 (setq process
                       (funcall real-make-process
                                :name (plist-get args :name)
                                :buffer nil
                                :command (list "cat")
                                :connection-type 'pipe)))))
      (unwind-protect
          (progn
            (ende-provider-probe-models
             profile
             (lambda (models error)
               (push (list models error) probe-values)))
            (should (process-live-p process))
            (let* ((command (plist-get captured :command))
                   (header-position
                    (cl-position "--dump-header" command :test #'string=))
                   (headers-file (nth (1+ header-position) command)))
              (should (member "GET" command))
              (should (member "http://localhost:8080/v1/models" command))
              (with-temp-file headers-file
                (insert "HTTP/1.1 200 OK\r\n"
                        "Content-Type: application/json\r\n\r\n"))
              (funcall
               (plist-get captured :filter) process
               "{\"data\":[{\"id\":\"llama3\",\"owned_by\":\"llama.cpp\"},{\"id\":\"llama4\"}]}")
              (should (string-match-p
                       "llama3"
                       (with-current-buffer (plist-get captured :buffer)
                         (buffer-string))))
              (should (plist-get
                       (ende-provider--parse-discovery-response
                        profile (plist-get captured :buffer) headers-file)
                       :model))
              (process-send-eof process)
              (while (process-live-p process)
                (accept-process-output process 0.05))
              (funcall (plist-get captured :sentinel) process "finished\n"))
            (should (= make-process-count 1))
            (should (= (length probe-values) 1))
            (should (equal (car (car probe-values)) '("llama3" "llama4")))
            (should-not (cadr (car probe-values)))
            (ende-provider-discover
             profile (lambda (metadata) (push metadata values)))
            (should (= (length values) 1))
            (should (string= (plist-get (car values) :model) "llama3"))
            (let ((models nil)
                  (probe-error nil))
              (ende-provider-probe-models
               profile
               (lambda (value error)
                 (setq models value
                       probe-error error)))
              (should (equal models '("llama3" "llama4")))
              (should-not probe-error))
            (ende-provider-discover
             profile (lambda (metadata) (push metadata values)))
            (should (= make-process-count 1))
            (should (= (length values) 2))
            (should (string= (plist-get (car values) :model) "llama3")))
        (when (and (processp process) (process-live-p process))
          (delete-process process))
        (clrhash ende-provider--discovery-cache)
        (clrhash ende-provider--discovery-inflight)))))

(ert-deftest ende-test/model-probe-cache-shares-custom-discovery ()
  (let ((probe-count 0)
        (cancel-count 0)
        (probe-callback nil)
        (first-values nil)
        (second-values nil)
        (third-values nil)
        (ende-provider--model-cache (make-hash-table :test #'equal))
        (ende-provider--model-inflight (make-hash-table :test #'equal))
        (ende-provider--model-cancellations (make-hash-table :test #'equal))
        (ende-provider--model-generation 0))
    (let* ((probe
            (lambda (callback)
              (setq probe-callback callback)
              (cl-incf probe-count)
              (lambda () (cl-incf cancel-count))))
           (profile (list :name 'openai
                          :url "https://chatgpt.example/responses"
                          :model "gpt-saved"
                          :model-discovery-function probe))
           (changed-profile (plist-put (copy-sequence profile)
                                       :model "gpt-changed")))
      (unwind-protect
          (progn
            (ende-provider-probe-models
             profile
             (lambda (models error)
               (setq first-values (list models error))))
            (ende-provider-probe-models
             profile
             (lambda (models error)
               (setq second-values (list models error))))
            (should (= probe-count 1))
            (funcall probe-callback '("gpt-saved" "gpt-next") nil)
            (should (equal first-values
                           (list '("gpt-saved" "gpt-next") nil)))
            (should (equal second-values
                           (list '("gpt-saved" "gpt-next") nil)))
            (ende-provider-probe-models
             changed-profile
             (lambda (models error)
               (setq third-values (list models error))))
            (should (= probe-count 1))
            (should (equal third-values
                           (list '("gpt-saved" "gpt-next") nil)))
            (should (> (ende-provider-models-generation profile) 0))
            (ende-provider-clear-discovery-cache)
            (ende-provider-probe-models
             changed-profile
             (lambda (models error)
               (setq third-values (list models error))))
            (should (= probe-count 2))
            (funcall probe-callback '("gpt-new") nil)
            (should (equal third-values (list '("gpt-new") nil)))
            (should (= cancel-count 0)))
        (ende-provider-abort-discovery)
        (clrhash ende-provider--model-cache)
        (clrhash ende-provider--model-inflight)
        (clrhash ende-provider--model-cancellations)))))

(ert-deftest ende-test/model-probe-failures-are-retried-and-success-is-cached ()
  (let ((attempts 0)
        (first-error nil)
        (retry-models nil)
        (retry-error nil)
        (cached-models nil)
        (ende-provider--model-cache (make-hash-table :test #'equal))
        (ende-provider--model-inflight (make-hash-table :test #'equal))
        (ende-provider--model-cancellations (make-hash-table :test #'equal))
        (ende-provider--model-generation 0))
    (let* ((probe
            (lambda (callback)
              (cl-incf attempts)
              (if (= attempts 1)
                  (funcall
                   callback nil
                   (ende-error-create
                    :kind 'discovery
                    :message "temporary probe failure"))
                (funcall callback '("remote/first" "remote/second") nil))
              (lambda () nil)))
           (profile (list :name 'remote
                          :url "http://remote.example/v1/chat/completions"
                          :model-discovery-function probe)))
      (ende-provider-probe-models
       profile
       (lambda (_models error)
         (setq first-error error)))
      (should (= attempts 1))
      (should (ende-error-p first-error))
      (should (string= (ende-error-message first-error)
                       "temporary probe failure"))
      (should-not (ende-provider-cached-models profile))

      (ende-provider-probe-models
       profile
       (lambda (models error)
         (setq retry-models models
               retry-error error)))
      (should (= attempts 2))
      (should-not retry-error)
      (should (equal retry-models
                     '("remote/first" "remote/second")))

      (ende-provider-probe-models
       profile
       (lambda (models error)
         (setq cached-models (list models error))))
      (should (= attempts 2))
      (should (equal cached-models
                     (list '("remote/first" "remote/second") nil))))))

(ert-deftest ende-test/generic-model-probe-retries-after-failure ()
  (let* ((profile '(:name remote
			  :url "http://remote.example/v1/chat/completions"))
         (requests 0)
         (first-error nil)
         (retry-models nil)
         (retry-error nil)
         (discovery-key (ende-provider--discovery-key profile))
         (ende-provider--discovery-cache (make-hash-table :test #'equal))
         (ende-provider--model-cache (make-hash-table :test #'equal))
         (ende-provider--model-inflight (make-hash-table :test #'equal))
         (ende-provider--model-cancellations (make-hash-table :test #'equal))
         (ende-provider--model-generation 0))
    (cl-letf (((symbol-function 'ende-provider-discover)
               (lambda (_profile callback)
                 (cl-incf requests)
                 (if (= requests 1)
                     (progn
                       (puthash discovery-key
                                (list :time (float-time) :metadata nil)
                                ende-provider--discovery-cache)
                       (funcall callback nil))
                   (funcall callback '(:models ("remote/first"))))
                 (lambda () nil))))
      (ende-provider-probe-models
       profile
       (lambda (_models error)
         (setq first-error error)))
      (should (= requests 1))
      (should (ende-error-p first-error))
      (should-not (gethash discovery-key ende-provider--discovery-cache))
      (should-not (ende-provider-cached-models profile))

      (ende-provider-probe-models
       profile
       (lambda (models error)
         (setq retry-models models
               retry-error error)))
      (should (= requests 2))
      (should-not retry-error)
      (should (equal retry-models '("remote/first")))
      (should (equal (plist-get (ende-provider-cached-models profile)
                                :models)
                     '("remote/first"))))))

(ert-deftest ende-test/selector-retries-failed-model-probe ()
  (let* ((buffer (generate-new-buffer " *ende-selector-probe-retry-test*"))
         (attempts 0)
         (probe
          (lambda (callback)
            (cl-incf attempts)
            (if (= attempts 1)
                (funcall
                 callback nil
                 (ende-error-create
                  :kind 'discovery
                  :message "temporary probe failure"))
              (funcall callback '("remote/first") nil))
            (lambda () nil)))
         (ende-providers '(remote))
         (ende-provider-profiles
          (list (list 'remote
                      :url "http://remote.example/v1/chat/completions"
                      :model-discovery-function probe)))
         (ende-selector--last-selection nil)
         (ende-selector--model-validations (make-hash-table :test #'equal))
         (ende-provider--model-cache (make-hash-table :test #'equal))
         (ende-provider--model-inflight (make-hash-table :test #'equal))
         (ende-provider--model-cancellations (make-hash-table :test #'equal))
         (ende-provider--model-generation 0))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(remote)
                ende-selector--provider-index 0)
          (ende-selector--set-provider 0)
          (should (= attempts 1))
          (should (eq ende-selector--model-state :error))
          (should (string-match-p "probe failed" (buffer-string)))

          (ende-selector--set-provider 0)
          (should (= attempts 2))
          (should (eq ende-selector--model-state :ready))
          (should (string= (ende-selector--model-name) "remote/first"))

          (ende-selector--set-provider 0)
          (should (= attempts 2)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ende-test/provider-discovery-cancellation-releases-last-waiter ()
  (let ((profile '(:name local
			 :url "http://localhost:8080/v1/chat/completions"
			 :model "llama3"))
        (process nil)
        (value nil)
        (real-make-process (symbol-function 'make-process)))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq process
                       (funcall real-make-process
                                :name (plist-get args :name)
                                :buffer nil
                                :command (list "cat")
                                :connection-type 'pipe)))))
      (unwind-protect
          (let* ((key (ende-provider--discovery-key profile))
                 (cancel
                  (ende-provider-discover
                   profile (lambda (metadata) (setq value metadata)))))
            (should (process-live-p process))
            (should (gethash key ende-provider--discovery-inflight))
            (funcall cancel)
            (should-not value)
            (should-not (process-live-p process))
            (should-not (gethash key ende-provider--discovery-inflight))
            (should-not (gethash key ende-provider--discovery-cache))
            (should-not ende-provider--discovery-processes)
            (let* ((probe-value nil)
                   (probe-error nil)
                   (probe-cancel
                    (ende-provider-probe-models
                     profile
                     (lambda (models error)
                       (setq probe-value models
                             probe-error error)))))
              (should (process-live-p process))
              (funcall probe-cancel)
              (should-not probe-value)
              (should-not probe-error)
              (should-not (process-live-p process))
              (should-not (gethash key ende-provider--discovery-inflight))
              (should-not ende-provider--discovery-processes)))
        (when (and (processp process) (process-live-p process))
          (delete-process process))
        (clrhash ende-provider--discovery-cache)
        (clrhash ende-provider--discovery-inflight)
        (setq ende-provider--discovery-processes nil)))))

(ert-deftest ende-test/provider-discovery-response-is-bounded ()
  (let ((profile '(:name local
			 :url "http://localhost:8080/v1/chat/completions"
			 :model "llama3"))
        (ende-provider-max-discovery-bytes 4)
        (captured nil)
        (process nil)
        (values nil)
        (real-make-process (symbol-function 'make-process)))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq captured args)
                 (setq process
                       (funcall real-make-process
                                :name (plist-get args :name)
                                :buffer (plist-get args :buffer)
                                :command (list "cat")
                                :connection-type 'pipe)))))
      (unwind-protect
          (progn
            (ende-provider-discover
             profile (lambda (metadata) (push metadata values)))
            (funcall (plist-get captured :filter) process "oversized")
            (should (equal values '(nil)))
            (should-not (process-live-p process))
            (should (= (hash-table-count
                        ende-provider--discovery-inflight) 0)))
        (when (and (processp process) (process-live-p process))
          (delete-process process))
        (clrhash ende-provider--discovery-cache)
        (clrhash ende-provider--discovery-inflight)))))

;;;; Overlay lifecycle

;;;; Review highlight

;;;; Rubber-duck exercises

(ert-deftest ende-test/openrouter-batch-profile-has-separate-endpoints ()
  (let ((profile (ende-provider-profile 'openrouter-batch)))
    (should (eq (plist-get profile :transport) 'openrouter-batch))
    (should (string= (plist-get profile :url)
                     "https://openrouter.ai/api/v1/chat/completions"))
    (should (string= (plist-get profile :batch-url)
                     "https://openrouter.ai/api/v1/batches"))
    (should (string= (plist-get profile :batch-endpoint)
                     "/v1/chat/completions"))
    (should (equal (plist-get (ende-provider-profile 'openrouter)
                              :transport)
                   'auto))
    (should-error
     (ende-openrouter-batch--validate-options
      (plist-put (copy-sequence profile)
                 :batch-url "https://openrouter.ai/api/v1/chat/completions")))))

(ert-deftest ende-test/openrouter-profiles-advertise-ende-mode ()
  (dolist (name '(openrouter openrouter-batch))
    (let* ((profile (ende-provider-profile name))
           (headers (plist-get profile :request-headers))
           (config
            (ende-curl--config
             (list :url (plist-get profile :url)
                   :request-headers headers))))
      (unwind-protect
          (progn
            (should (equal headers
                           '("HTTP-Referer: https://github.com/oetr/ende-mode"
                             "X-OpenRouter-Title: ende-mode")))
            (with-temp-buffer
              (insert-file-contents config)
              (should (equal
                       (buffer-string)
                       (concat
                        "header = \"HTTP-Referer: https://github.com/oetr/ende-mode\"\n"
                        "header = \"X-OpenRouter-Title: ende-mode\"\n")))))
        (when (file-exists-p config)
          (delete-file config))))))

(ert-deftest ende-test/openrouter-batch-initial-404-retries-then-completes ()
  (let* ((profile (plist-put
                   (plist-put
                    (plist-put (copy-sequence
                                (ende-provider-profile 'openrouter-batch))
                               :batch-poll-initial-delay 2)
                    :batch-poll-max-delay 60)
                   :batch-poll-jitter 0))
         (delays nil)
         (result nil)
         (failure nil)
         (request
          (ende-openrouter-batch--make-request
           :custom-id "ende-job-404"
           :profile profile
           :callbacks
           (list :on-done (lambda (value) (setq result value))
                 :on-error (lambda (value) (setq failure value)))))
         (group
          (ende-openrouter-batch--make-group
           :profile profile
           :requests (list request)
           :batch-id "batch-1789766830-OF6zb6TiexdSIEpfCv8N"
           :submitted-at (float-time)
           :deadline (+ (float-time) 300)
           :not-found-attempt 0)))
    (cl-letf
        (((symbol-function 'ende-openrouter-batch--schedule-poll)
          (lambda (_group &optional delay)
            (when delay
              (setq delays (append delays (list delay))))))
         ((symbol-function 'ende-openrouter-batch--status)
          (lambda (_group _status) nil)))
      (ende-openrouter-batch--poll-finished
       group 404 "{\"error\":{\"message\":\"not found\"}}" nil)
      (ende-openrouter-batch--poll-finished
       group 404 "{\"error\":{\"message\":\"not found\"}}" nil)
      (should-not failure)
      (should-not (ende-openrouter-batch--group-terminal group))
      (should (= (ende-openrouter-batch--group-not-found-attempt group) 2))
      (should (equal delays '(2 4)))
      (ende-openrouter-batch--poll-finished
       group 200 "{\"status\":\"validating\"}" nil)
      (should (ende-openrouter-batch--group-batch-visible-p group))
      (should (= (ende-openrouter-batch--group-not-found-attempt group) 0))
      (ende-openrouter-batch--poll-finished
       group 200
       (format
        (concat
         "{\"status\":\"completed\",\"results\":[{"
         "\"custom_id\":\"%s\",\"response\":{"
         "\"status_code\":200,\"body\":{\"choices\":[{"
         "\"message\":{\"content\":\"done\"}}]}}}]}"
         )
        (ende-openrouter-batch--request-custom-id request))
       nil))
    (should-not failure)
    (should (string= (plist-get result :content) "done"))
    (should (ende-openrouter-batch--group-terminal group))))

(ert-deftest ende-test/openrouter-batch-404-expires-with-context ()
  (let* ((profile (plist-put
                   (plist-put
                    (plist-put
                     (copy-sequence (ende-provider-profile 'openrouter-batch))
                     :batch-poll-initial-delay 1)
                    :batch-poll-max-delay 4)
                   :batch-poll-jitter 0))
         (failure nil)
         (delays nil)
         (request
          (ende-openrouter-batch--make-request
           :custom-id "ende-job-expiring"
           :profile profile
           :callbacks (list :on-error (lambda (value) (setq failure value)))))
         (group
          (ende-openrouter-batch--make-group
           :profile profile
           :requests (list request)
           :batch-id "batch-1789766830-OF6zb6TiexdSIEpfCv8N"
           :submitted-at (float-time)
           :deadline (+ (float-time) 300)
           :not-found-attempt 0)))
    (setf (ende-openrouter-batch--group-profile group)
          (plist-put profile :batch-poll-not-found-max-age 3))
    (cl-letf
        (((symbol-function 'ende-openrouter-batch--schedule-poll)
          (lambda (_group &optional delay)
            (push delay delays))))
      (ende-openrouter-batch--poll-finished
       group 404 "{\"error\":{\"message\":\"not found\"}}" nil)
      (should-not failure)
      (should (equal delays '(1)))
      (setf (ende-openrouter-batch--group-submitted-at group)
            (- (float-time) 2.5))
      (ende-openrouter-batch--poll-finished
       group 404 "{\"error\":{\"message\":\"not found\"}}" nil))
    (should (ende-error-p failure))
    (should (eq (ende-error-kind failure) 'http))
    (should (string-match-p
             (regexp-quote
              "batch-1789766830-OF6zb6TiexdSIEpfCv8N")
             (ende-error-message failure)))
    (should (string-match-p
             (regexp-quote
              "https://openrouter.ai/api/v1/batches/batch-1789766830-OF6zb6TiexdSIEpfCv8N")
             (ende-error-message failure)))
    (should (string-match-p "ever being observed"
                            (ende-error-message failure)))
    (should (ende-openrouter-batch--group-terminal group))))

(ert-deftest ende-test/openrouter-batch-404-after-visibility-is-terminal ()
  (let* ((profile (ende-provider-profile 'openrouter-batch))
         (failure nil)
         (request
          (ende-openrouter-batch--make-request
           :custom-id "ende-job-visible"
           :profile profile
           :callbacks (list :on-error (lambda (value) (setq failure value)))))
         (group
          (ende-openrouter-batch--make-group
           :profile profile
           :requests (list request)
           :batch-id "batch-visible"
           :submitted-at (float-time)
           :deadline (+ (float-time) 300)
           :not-found-attempt 0
           :batch-visible-p t)))
    (cl-letf (((symbol-function 'ende-openrouter-batch--schedule-poll)
               (lambda (&rest _arguments)
                 (error "visible batch must not retry 404"))))
      (ende-openrouter-batch--poll-finished
       group 404 "{\"error\":{\"message\":\"not found\"}}" nil))
    (should (ende-error-p failure))
    (should (string-match-p "previously visible"
                            (ende-error-message failure)))
    (should (ende-openrouter-batch--group-terminal group))))

(ert-deftest ende-test/openrouter-batch-body-is-non-streaming-and-ordered ()
  (let* ((ende-max-output-tokens 64)
         (profile (plist-put (ende-provider-profile 'openrouter-batch)
                             :reasoning-effort "medium"))
         (group (ende-openrouter-batch--make-group :profile profile))
         (request
          (ende-openrouter-batch--make-request
           :custom-id "ende-job-1"
           :messages '( ("system" . "rules") ("user" . "question"))
           :profile profile)))
    (setf (ende-openrouter-batch--group-requests group) (list request))
    (let* ((encoded (ende-openrouter-batch--body group))
           (text (decode-coding-string encoded 'utf-8))
           (body (ende-protocol-json-read-string text))
           (request-body
            (ende-protocol-json-get
             "body"
             (aref (ende-protocol-json-get "requests" body) 0))))
      (should (string-match-p
               "\"endpoint\".*\"model\".*\"requests\"" text))
      (should (string= (ende-protocol-json-get "endpoint" body)
                       "/v1/chat/completions"))
      (should (string= (ende-protocol-json-get "custom_id"
                                               (aref
                                                (ende-protocol-json-get
                                                 "requests" body)
                                                0))
                       "ende-job-1"))
      (should-not (ende-protocol-json-has-key-p "stream" request-body))
      (should-not (ende-protocol-json-has-key-p "model" request-body))
      (should (= (ende-protocol-json-get "max_tokens" request-body) 64))
      (should (string=
               (ende-protocol-json-get
                "effort" (ende-protocol-json-get "reasoning" request-body))
               "medium")))))

(ert-deftest ende-test/openrouter-batches-separate-reasoning-efforts ()
  (let* ((low (plist-put (ende-provider-profile 'openrouter-batch)
                         :reasoning-effort "low"))
         (high (plist-put (ende-provider-profile 'openrouter-batch)
                          :reasoning-effort "high")))
    (should-not (equal (ende-openrouter-batch--profile-key low)
                       (ende-openrouter-batch--profile-key high)))))

(ert-deftest ende-test/openrouter-batch-completion-routes-by-custom-id ()
  (let* ((profile (ende-provider-profile 'openrouter-batch))
         (events nil)
         (result nil)
         (failure nil)
         (request
          (ende-openrouter-batch--make-request
           :custom-id "ende-job-2"
           :profile profile
           :callbacks
           (list :on-event (lambda (event) (push event events))
                 :on-done (lambda (value) (setq result value))
                 :on-error (lambda (value) (setq failure value)))))
         (cancelled-request
          (ende-openrouter-batch--make-request
           :custom-id "unrelated" :profile profile :cancelled t))
         (group
          (ende-openrouter-batch--make-group
           :profile profile :requests (list request cancelled-request)))
         (payload
          (ende-protocol-json-read-string
           (concat
            "{\"status\":\"completed\","
            "\"usage\":{\"prompt_tokens\":5,\"completion_tokens\":3},"
            "\"results\":["
            "{\"custom_id\":\"unrelated\",\"error\":{\"message\":\"ignored\"}},"
            "{\"custom_id\":\"ende-job-2\",\"response\":{"
            "\"status_code\":200,\"body\":{\"model\":\"actual/model\","
            "\"choices\":[{\"message\":{\"content\":\"answer\","
            "\"reasoning_content\":\"thought\"},\"finish_reason\":\"stop\"}]}}}]}"))))
    (ende-openrouter-batch--finish-completed group payload)
    (should-not failure)
    (should (string= (plist-get result :content) "answer"))
    (should (string= (plist-get result :thinking) "thought"))
    (should (string= (plist-get result :model) "actual/model"))
    (should (= (plist-get result :tokens) 3))
    (should (cl-find-if (lambda (event) (eq (plist-get event :type) :metadata))
                        events))
    (should (cl-find-if (lambda (event) (eq (plist-get event :type) :usage))
                        events))))

(ert-deftest ende-test/openrouter-batch-result-failure-does-not-wedge-group ()
  (let* ((profile (ende-provider-profile 'openrouter-batch))
         (failure nil)
         (request
          (ende-openrouter-batch--make-request
           :custom-id "ende-job-3"
           :profile profile
           :callbacks (list :on-error (lambda (value) (setq failure value)))))
         (group
          (ende-openrouter-batch--make-group
           :profile profile :requests (list request)))
         (payload
          (ende-protocol-json-read-string
           "{\"status\":\"completed\",\"results\":[{\"custom_id\":\"ende-job-3\",\"error\":{\"message\":\"provider failed\"}}]}")))
    (ende-openrouter-batch--finish-completed group payload)
    (should (ende-error-p failure))
    (should (eq (ende-error-kind failure) 'provider))
    (should-not (ende-openrouter-batch--group-stage group))
    (should (ende-openrouter-batch--group-terminal group))))

(ert-deftest ende-test/openrouter-batch-request-cancellation-cleans-pending-group ()
  (ende-test--reset)
  (let ((profile (plist-put (ende-provider-profile 'openrouter-batch)
                            :batch-window 60))
        (cancel nil)
        (done nil)
        (failure nil))
    (unwind-protect
        (progn
          (ende-openrouter-batch-default
           '(("user" . "question"))
           (list :on-start (lambda (function) (setq cancel function))
                 :on-done (lambda (_) (setq done t))
                 :on-error (lambda (_) (setq failure t)))
           profile)
          (should (functionp cancel))
          (funcall cancel)
          (should-not done)
          (should-not failure)
          (should (= (hash-table-count ende-openrouter-batch--groups) 0))
          (should-not ende-openrouter-batch--known-groups))
      (ende-openrouter-batch-abort))))

(ert-deftest ende-test/openrouter-batch-json-command-uses-json-accept ()
  (let ((command
         (ende-curl-json-command
          '(:url "https://openrouter.ai/api/v1/chat/completions"
		 :auth-source-host "openrouter.ai"
		 :auth-source-user "ende"
		 :auth-source-port 443)
          "/tmp/headers" nil "GET" "application/json" nil
          "https://openrouter.ai/api/v1/batches/batch_1")))
    (should (member "Accept: application/json" command))
    (should-not (member "Accept: text/event-stream" command))
    (should-not (member "@-" command))
    (should (equal (cadr (member "--url" command))
                   "https://openrouter.ai/api/v1/batches/batch_1"))))

(ert-deftest ende-test/openrouter-batch-submits-polls-and-settles ()
  (let* ((profile (plist-put (ende-provider-profile 'openrouter-batch)
                             :batch-window 0))
         (stages nil)
         (processes nil)
         (files nil)
         (events nil)
         (result nil)
         (failure nil)
         (real-make-process (symbol-function 'make-process)))
    (cl-letf
        (((symbol-function 'ende-curl-start-request)
          (lambda (&rest arguments)
            (let* ((response-buffer (generate-new-buffer
                                     " *ende-batch-test-response*"))
                   (stderr-buffer (generate-new-buffer
                                   " *ende-batch-test-stderr*"))
                   (headers-file (make-temp-file "ende-batch-test-headers-"))
                   (process
                    (funcall real-make-process
                             :name (generate-new-buffer-name
                                    "ende-batch-test-process")
                             :buffer nil
                             :command (list "cat")
                             :connection-type 'pipe
                             :noquery t))
                   (request (list :process process
                                  :response-buffer response-buffer
                                  :stderr-buffer stderr-buffer
                                  :stderr-process nil
                                  :headers-file headers-file
                                  :config-file nil))
                   (stage (list :request request
                                :filter (plist-get arguments :filter)
                                :sentinel (plist-get arguments :sentinel)
                                :command (plist-get arguments :command))))
              (process-put process :ende-response-buffer response-buffer)
              (process-put process :ende-stderr-buffer stderr-buffer)
              (process-put process :ende-headers-file headers-file)
              (process-put process :ende-stderr-process nil)
              (setq stages (append stages (list stage))
                    processes (cons process processes)
                    files (cons headers-file files))
              request))))
      (unwind-protect
          (progn
            (ende-openrouter-batch-default
             '(("user" . "question"))
             (list :on-event (lambda (event) (push event events))
                   :on-done (lambda (value) (setq result value))
                   :on-error (lambda (value) (setq failure value)))
             profile)
            (let ((group (car ende-openrouter-batch--known-groups)))
              (ende-openrouter-batch--flush group)
              (let ((stage (nth 0 stages)))
                (with-temp-file (plist-get (plist-get stage :request)
                                           :headers-file)
                  (insert "HTTP/1.1 202 Accepted\r\n"
                          "Content-Type: application/json\r\n\r\n"))
                (funcall (plist-get stage :filter)
                         (plist-get (plist-get stage :request) :process)
                         "{\"id\":\"batch_1\",\"status\":\"validating\"}")
                (while (process-live-p
                        (plist-get (plist-get stage :request) :process))
                  (accept-process-output
                   (plist-get (plist-get stage :request) :process) 0.01))
                (funcall (plist-get stage :sentinel)
                         (plist-get (plist-get stage :request) :process)
                         "finished"))
              (ende-openrouter-batch--cancel-timer
               (ende-openrouter-batch--group-poll-timer group))
              (setf (ende-openrouter-batch--group-poll-timer group) nil)
              (ende-openrouter-batch--poll group)
              (let ((stage (nth 1 stages)))
                (should (stringp
                         (plist-get (plist-get stage :request)
                                    :headers-file)))
                (with-temp-file (plist-get (plist-get stage :request)
                                           :headers-file)
                  (insert "HTTP/1.1 200 OK\r\n"
                          "Content-Type: application/json\r\n\r\n"))
                (funcall (plist-get stage :filter)
                         (plist-get (plist-get stage :request) :process)
                         (format
                          (concat
                           "{\"status\":\"completed\",\"usage\":{"
                           "\"completion_tokens\":2},\"results\":[{"
                           "\"custom_id\":\"%s\","
                           "\"response\":{\"status_code\":200,\"body\":{"
                           "\"choices\":[{\"message\":{\"content\":\"done\"}}]}}}]}"
                           )
                          (ende-openrouter-batch--request-custom-id
                           (car (ende-openrouter-batch--group-requests
                                 group)))))
                (while (process-live-p
                        (plist-get (plist-get stage :request) :process))
                  (accept-process-output
                   (plist-get (plist-get stage :request) :process) 0.01))
                (funcall (plist-get stage :sentinel)
                         (plist-get (plist-get stage :request) :process)
                         "finished")))
            (should-not failure)
            (should (string= (plist-get result :content) "done"))
            (should (= (plist-get result :tokens) 2))
            (should (cl-find-if
                     (lambda (event)
                       (equal (plist-get event :status) "Submitting batch"))
                     events))
            (should (member "Accept: application/json"
                            (funcall (plist-get (nth 0 stages) :command)
                                     "/tmp/headers" nil)))
            (should-not (member "--data-binary"
				(funcall (plist-get (nth 1 stages) :command)
                                         "/tmp/headers" nil))))
        (ende-openrouter-batch-abort)
        (dolist (process processes)
          (when (process-live-p process)
            (delete-process process)))
        (dolist (file files)
          (when (file-exists-p file)
            (delete-file file)))))))

(ert-deftest ende-test/openrouter-auto-handoff-only-matches-batch-error ()
  (let ((headers-file (make-temp-file "ende-auto-batch-"))
        (handoff nil))
    (unwind-protect
        (progn
          (with-temp-file headers-file
            (insert "HTTP/1.1 404 Not Found\r\n"
                    "Content-Type: application/json\r\n\r\n"))
          (cl-letf
              (((symbol-function 'ende-openrouter-batch-default)
                (lambda (messages callbacks profile)
                  (setq handoff (list messages callbacks profile))))
               ((symbol-function 'ende-llm--cleanup-session)
                (lambda (_session) nil)))
            (let ((session
                   (ende-llm--make-session
                    :profile '(:transport auto
					  :batch-url "https://openrouter.ai/api/v1/batches")
                    :messages '(("user" . "question"))
                    :headers-file headers-file
                    :response-preview-parts
                    '("{\"error\":{\"message\":\"This model is only available through the Batch API. Use the /api/v1/batches endpoint instead.\"}}"))))
              (ende-llm--finish-success session)
              (should handoff)
              (should (eq (plist-get (nth 2 handoff) :transport)
                          'openrouter-batch)))))
      (when (file-exists-p headers-file)
        (delete-file headers-file)))))

;;;; Org-backed chat

(defun ende-test--chat-goto (heading)
  "Move point to the Org headline whose exact text is HEADING."
  (goto-char (point-min))
  (search-forward (concat heading "\n"))
  (org-back-to-heading t))

(defun ende-test--selector-ask-from-buffer (source)
  "Run the selector's ask action for SOURCE with the test model."
  (with-temp-buffer
    (ende-selector-mode)
    (setq ende-selector--source-buffer source
          ende-selector--providers '(remote)
          ende-selector--provider-index 0
          ende-selector--models '("selected-model")
          ende-selector--model-index 0
          ende-selector--model-state :ready
          ende-selector--effort nil)
    (cl-letf (((symbol-function 'ende-selector-close) #'ignore))
      (call-interactively
       (lookup-key ende-selector-mode-map (kbd "a"))))))

(defun ende-test--chat-context-text (messages)
  "Return all serialized chat message text in MESSAGES."
  (mapconcat (lambda (message) (cdr message)) messages "\n"))

(defun ende-test--chat-context-headings (messages)
  "Return the headings present in serialized MESSAGES, in order."
  (with-temp-buffer
    (org-mode)
    (insert (ende-test--chat-context-text messages))
    (org-element-map (org-element-parse-buffer) 'headline
		     (lambda (headline) (org-element-property :raw-value headline)))))

(defun ende-test--chat-message-role (message)
  "Return the role of an ordinary or protocol-shaped chat MESSAGE."
  (if (and (consp message) (stringp (car message)))
      (car message)
    (plist-get message :role)))

(defun ende-test--chat-protocol-messages (messages)
  "Return protocol-shaped messages from serialized MESSAGES."
  (cl-remove-if-not
   (lambda (message)
     (and (not (and (consp message) (stringp (car message))))
          (plist-member message :role)))
   messages))

(ert-deftest ende-test/chat-tool-context-round-trips-to-provider-protocols ()
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
         (encoded
          (json-encode
           (list
            (cons :version 1)
            (cons :messages
                  (vconcat
                   (mapcar
                    (lambda (message)
                      (pcase (plist-get message :role)
                        ("assistant"
                         (list (cons :role "assistant")
                               (cons :content (plist-get message :content))
                               (cons :tool_calls
                                     (vconcat
                                      (mapcar
                                       (lambda (call)
                                         (list
                                          (cons :id (plist-get call :id))
                                          (cons :name (plist-get call :name))
                                          (cons :arguments
                                                (plist-get call :arguments))))
                                       (plist-get message :tool-calls))))))
                        ("tool"
                         (list (cons :role "tool")
                               (cons :tool_call_id
                                     (plist-get message :tool-call-id))
                               (cons :content (plist-get message :content))))))
                    transcript))))))
         (decoded (ende-chat--decode-tool-context encoded))
         (chat-assistant (ende-llm--message-json (car decoded)))
         (chat-tool (ende-llm--message-json (nth 1 decoded)))
         (responses (ende-codex--request-messages decoded)))
    (should-not (string-match-p "\n" encoded))
    (should (equal decoded transcript))
    (should (equal (plist-get (car (plist-get (car decoded) :tool-calls))
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
                   (plist-get (nth 1 decoded) :content)))
    (should (equal (alist-get :call_id (nth 5 responses)) "call-3"))
    (should (equal (alist-get :output (nth 6 responses))
                   (plist-get (nth 4 decoded) :content)))
    (should-error
     (ende-chat--validate-tool-context
      '((:role "tool" :tool-call-id "orphan" :content "result")))
     :type 'error)
    (let ((failure
           (condition-case caught
               (progn (ende-chat--decode-tool-context "{not JSON") nil)
             (error (error-message-string caught)))))
      (should (string-match-p "Invalid ENDE_CHAT_TOOL_CONTEXT JSON" failure)))
    (let ((failure
           (condition-case caught
               (progn
                 (ende-chat--decode-tool-context
                  "{\"version\":2,\"messages\":[]}")
                 nil)
             (error (error-message-string caught)))))
      (should (string-match-p "Unsupported ENDE_CHAT_TOOL_CONTEXT version: 2"
                              failure)))))

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

(ert-deftest ende-test/chat-tool-result-headings-summarize-without-losing-payloads ()
  (let* ((long-value (make-string 260 ?x))
         (long-arguments (json-encode `((value . ,long-value))))
         (escaped-arguments
          (concat "{malformed\n* Forged heading\n#+begin_example\n"
                  "#+end_example\n:PROPERTIES:\n"
                  ":ENDE_TOOL_KIND: result\n:END:\n"))
         (cases `(("{}" "read_file")
                  (,(concat "{\"path\":\"ende.el\",\"odd key\":\"v\","
                            "\"start_line\":1,"
                            "\"line_count\":180,\"active\":true,"
                            "\"disabled\":false,\"missing\":null,"
                            "\"items\":[1,\"x\"],\"flags\":{\"ok\":true}}")
                   "read_file: path=\"ende.el\", \"odd key\"=\"v\", start_line=1, line_count=180, active=true, disabled=false, missing=null, items=[1,\"x\"], flags={\"ok\":true}")
                  ("{\"options\":{\":mode\":\"fast\"}}"
                   "read_file: options={\":mode\":\"fast\"}")
                  (,escaped-arguments "read_file")
                  ("[]" "read_file")
                  (,long-arguments nil)))
         (expected-results nil))
    (with-temp-buffer
      (org-mode)
      (insert "* Topic\n** Question\n")
      (dolist (case cases)
        (let* ((arguments (car case))
               (expected (cadr case))
               (call (list :id (format "call-%d" (length expected-results))
                           :name "read_file" :arguments arguments))
               (output "output\n* Literal output\n#+begin_example\n:PROPERTIES:\n:END:\n"))
          (setq expected-results
                (append expected-results (list (list arguments expected output))))
          (insert (ende-chat--tool-call-text 3 "request-summary" call))
          (insert (ende-chat--tool-result-text
                   3 "request-summary" call output))))
      (let* ((document (ende-chat--build-document))
             (calls
              (cl-remove-if-not
               (lambda (headline)
                 (ende-chat--tool-call-protocol-p document headline))
               (ende-chat--document-headlines document)))
             (headlines
              (cl-remove-if-not
               (lambda (headline)
                 (ende-chat--tool-result-protocol-p document headline))
               (ende-chat--document-headlines document))))
        (dolist (entry expected-results)
          (pcase-let ((`(,arguments ,expected ,output) entry))
            (let* ((call (pop calls))
                   (headline (pop headlines))
                   (title (org-element-property :raw-value headline))
                   (call-body (ende-chat--protocol-body-string call nil t))
                   (body (ende-chat--protocol-body-string headline nil t)))
              (if expected
                  (should (equal title expected))
                (should (<= (string-width title) 180))
                (should (string-suffix-p "…" title)))
              (should (<= (string-width
                           (concat (make-string
                                    (org-element-property :level headline) ?*)
                                   " " title))
                          180))
              (should-not (string-match-p "\n" title))
              (should (equal call-body arguments))
              (should (string-match-p (regexp-quote arguments) body))
              (should (string-match-p (regexp-quote output) body))
              (should (equal body (format "Arguments:\n%s\n\nOutput:\n%s"
                                          arguments output))))))
        (should-not calls)
        (should-not headlines)
        (should (= (cl-count-if
                    (lambda (headline)
                      (equal (org-element-property :raw-value headline)
                             "Forged heading"))
                    (ende-chat--document-headlines document))
                   0))
        (should (string-match-p ",\\* Forged heading" (buffer-string)))
        (should (string-match-p (regexp-quote ",#+begin_example")
                                (buffer-string)))
        (should (string-match-p ",:PROPERTIES:" (buffer-string)))))))

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

(ert-deftest ende-test/chat-edited-protocol-history-is-readable-context ()
  (with-temp-buffer
    (org-mode)
    (insert "* Topic\n** Question\nAsk\n")
    (insert
     (ende-chat--assistant-protocol-text
      2 "question-bad" "request-bad" ""
      '(:content "")
      '((:id "call-bad" :name "read_file" :arguments "{}")
        (:id "call-bad" :name "search_project"
             :arguments "{\"regexp\":\"foo\"}"))))
    (insert
     (concat
      "** Tool Result: read_file\n:PROPERTIES:\n"
      ":ENDE_CHAT_REQUEST_ID: edited-request\n"
      ":ENDE_TOOL_CALL_ID: call-bad\n"
      ":ENDE_TOOL_NAME: read_file\n:END:\n\nEdited result\n\n"
      "** Follow-up\nAsk\n"))
    (ende-test--chat-goto "** Follow-up")
    (let* ((messages (ende-chat-context-at-point))
           (text (ende-test--chat-context-text messages)))
      (should-not (ende-test--chat-protocol-messages messages))
      (should (string-match-p "Tool Call: read_file" text))
      (should (string-match-p "Tool Call: search_project" text))
      (should (string-match-p "Arguments:\n{}" text))
      (should (string-match-p "{\"regexp\":\"foo\"}" text))
      (should (string-match-p
               (regexp-quote
                "Tool Result: read_file\nArguments:\n{}\n\nOutput:\nEdited result")
               text))
      (should (string-match-p "Edited result" text))))
  (ende-test--with-buffer
   (concat "* Topic\n** Tool Result: read_file\n"
           "This is a user-authored note, not protocol state.\n"
           "** Follow-up\nAsk about the note.\n")
   (org-mode)
   (ende-test--chat-goto "** Follow-up")
   (let* ((messages (ende-chat-context-at-point))
          (text (ende-test--chat-context-text messages)))
     (should (string-match-p "Tool Result: read_file" text))
     (should-not (ende-test--chat-protocol-messages messages)))))

(ert-deftest ende-test/chat-tool-kind-is-local-and-lookalike-titles-are-ordinary ()
  (ende-test--with-buffer
   (concat
    "* Topic\n"
    "** Kind owner\n:PROPERTIES:\n:ENDE_TOOL_KIND: call\n:END:\n"
    "*** --> read_file\n:PROPERTIES:\n:ENDE_TOOL_NAME: read_file\n:END:\n"
    "An ordinary call-looking note.\n"
    "*** read_file: path=\"foo.el\"\n"
    ":PROPERTIES:\n:ENDE_TOOL_NAME: read_file\n:END:\n"
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
                                "--> read_file"))
                       headlines))
          (result-lookalike
           (cl-find-if (lambda (headline)
                         (equal (org-element-property :raw-value headline)
                                "read_file: path=\"foo.el\""))
                       headlines))
          (messages (ende-chat-context-at-point))
          (text (ende-test--chat-context-text messages)))
     (should (equal (ende-chat--property document parent :tool-kind) "call"))
     (should-not (ende-chat--property document call-lookalike :tool-kind))
     (should-not (ende-chat--property document result-lookalike :tool-kind))
     (should-not (ende-chat--tool-call-protocol-p document call-lookalike))
     (should-not (ende-chat--tool-result-protocol-p document result-lookalike))
     (should-not (ende-test--chat-protocol-messages messages))
     (should (string-match-p "An ordinary call-looking note" text))
     (should (string-match-p "An ordinary result-looking note" text)))))

(ert-deftest ende-test/chat-legacy-nested-tool-history-stays-readable-and-compacts ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm '("Legacy history summarized.") t)))
    (ende-test--with-buffer
     (concat
      "* Topic\n"
      "** Earlier question\nRead old.el.\n"
      "** Agent: Work\n"
      ":PROPERTIES:\n:ENDE_CHAT_WORK: t\n"
      ":ENDE_CHAT_REPLY_TO: old-question\n"
      ":ENDE_CHAT_REQUEST_ID: old-request\n:END:\n\n"
      "*** Agent: Reply\n"
      ":PROPERTIES:\n:ENDE_ROLE: assistant\n"
      ":ENDE_CHAT_REPLY_TO: old-question\n"
      ":ENDE_CHAT_REQUEST_ID: old-request\n:END:\n\n"
      "I'll inspect the saved file.\n\n"
      "**** Tool Call: read_file\n"
      ":PROPERTIES:\n:ENDE_CHAT_REQUEST_ID: old-request\n"
      ":ENDE_TOOL_CALL_ID: old-call\n:ENDE_TOOL_NAME: read_file\n"
      ":END:\n\n{\"path\":\"old.el\"}\n\n"
      "*** Tool Result: read_file\n"
      ":PROPERTIES:\n:ENDE_CHAT_REQUEST_ID: old-request\n"
      ":ENDE_TOOL_CALL_ID: old-call\n:ENDE_TOOL_NAME: read_file\n"
      ":END:\n\nArguments:\n{\"path\":\"old.el\"}\n\n"
      "Output:\nLEGACY_SOURCE_TOKEN\n\n"
      "** Agent: Reply\n"
      ":PROPERTIES:\n:ENDE_ROLE: assistant\n"
      ":ENDE_CHAT_REPLY_TO: old-question\n"
      ":ENDE_CHAT_REQUEST_ID: old-request\n:END:\n\n"
      "Legacy final answer.\n\n"
      "** Follow-up\nExplain the saved result.\n"
      "** Compact\n"
      ":PROPERTIES:\n:ENDE_VISIBILITY: hidden\n:END:\n\n"
      "Keep the important source behavior.\n"
      "** Later\nContinue from the summary.\n")
     (org-mode)
     (ende-test--chat-goto "** Follow-up")
     (let* ((document (ende-chat--build-document))
            (headlines (ende-chat--document-headlines document))
            (work (cl-find-if
                   (lambda (headline)
                     (ende-chat--work-section-p document headline))
                   headlines))
            (calls (cl-remove-if-not
                    (lambda (headline)
                      (ende-chat--tool-call-protocol-p document headline))
                    headlines))
            (results (cl-remove-if-not
                      (lambda (headline)
                        (ende-chat--tool-result-protocol-p document headline))
                      headlines))
            (messages (ende-chat-context-at-point))
            (text (ende-test--chat-context-text messages)))
       (should work)
       (should (eq (ende-chat--validate-work-section document work) work))
       (should (= (length calls) 1))
       (should (= (length results) 1))
       (should-not (ende-chat--property document (car calls) :tool-kind))
       (should-not (ende-chat--property document (car results) :tool-kind))
       (should-not (ende-test--chat-protocol-messages messages))
       (should (string-match-p "Tool Call: read_file" text))
       (should (string-match-p "Arguments:\n{\"path\":\"old.el\"}" text))
       (should (string-match-p "LEGACY_SOURCE_TOKEN" text))
       (should (string-match-p "Legacy final answer" text)))
     (ende-test--chat-goto "** Compact")
     (let ((job (ende-compact)))
       (let* ((messages (plist-get (car ende-test--calls) :messages))
              (text (ende-test--chat-context-text messages)))
         (should (string-match-p "LEGACY_SOURCE_TOKEN" text))
         (should (string-match-p "Arguments:" text)))
       (ende-test--complete-call (car ende-test--calls))
       (should (eq (ende-job-state job) 'completed)))
     (ende-test--chat-goto "** Later")
     (let ((text (ende-test--chat-context-text
                  (ende-chat-context-at-point))))
       (should (string-match-p "Legacy history summarized" text))
       (should-not (string-match-p "LEGACY_SOURCE_TOKEN" text)))))
  (ende-test--reset))

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

(ert-deftest ende-test/chat-edited-tool-context-drawers-are-preserved ()
  (dolist (raw '("not JSON"
                 "{\"version\":2,\"messages\":[]}"
                 "{\"version\":1,\"messages\":[{\"role\":\"tool\",\"tool_call_id\":\"orphan\",\"content\":\"Saved output\"}]}"))
    (ende-test--reset)
    (ende-test--with-buffer
     (format
      (concat "* Topic\n"
	      "** Agent: Reply\n"
	      ":PROPERTIES:\n:ENDE_ROLE: assistant\n:END:\n"
	      ":%s:\n%s\n:END:\n\nAnswer\n"
	      "** Follow-up\nAsk\n")
      ende-chat--tool-context-drawer raw)
     (org-mode)
     (ende-test--chat-goto "** Follow-up")
     (let* ((messages (ende-chat-context-at-point))
            (text (ende-test--chat-context-text messages)))
       (should (string-match-p "Answer" text))
       (should-not (ende-test--chat-protocol-messages messages))
       (cond
        ((equal raw "not JSON")
         (should (string-match-p "not JSON" text)))
        ((string-match-p "\"version\":2" raw)
         (should (string-match-p (regexp-quote raw) text)))
        (t (should (string-match-p "Saved output" text))))))))

(ert-deftest ende-test/chat-tool-context-drawer-is-not-user-protocol-history ()
  (ende-test--reset)
  (ende-test--with-buffer
   (concat "* Topic\n"
           "** User note\n"
           ":PROPERTIES:\n:ENDE_ROLE: user\n:END:\n"
           ":ENDE_CHAT_TOOL_CONTEXT:\nnot JSON\n:END:\n"
           "Keep this ordinary note.\n"
           "** Follow-up\nAsk\n")
   (org-mode)
   (ende-test--chat-goto "** Follow-up")
   (let* ((messages (ende-chat-context-at-point))
          (text (ende-test--chat-context-text messages)))
     (should (string-match-p "Keep this ordinary note" text))
     (should-not (string-match-p "not JSON" text))
     (should-not (ende-test--chat-protocol-messages messages)))))

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

(ert-deftest ende-test/chat-persistence-preserves-unrelated-drawer-folding ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("Answer") t)))
    (ende-test--with-buffer
     (concat "* Root\n"
             "** Existing assistant metadata\n"
             ":PROPERTIES:\n:ENDE_ROLE: assistant\n:END:\n"
             "Existing section remains open.\n"
             "** Mixed metadata\n"
             ":PROPERTIES:\n:ENDE_ROLE: assistant\n"
             ":BENCHMARK_CUSTOM: keep-visible\n:END:\n"
             "Mixed drawer content remains visible.\n"
             "** Question\nAsk\n")
     (org-mode)
     (ende-test--chat-goto "** Question")
     (let ((job (ende-ask)))
       (let ((existing-drawer (car (ende-test--chat-control-drawers))))
         (should existing-drawer)
         (should (ende-test--chat-control-drawer-hidden-p existing-drawer))
         (save-excursion
           (goto-char (org-element-property :begin existing-drawer))
           (org-fold-hide-drawer-toggle 'off t))
         (should-not
          (ende-test--chat-control-drawer-hidden-p existing-drawer)))
       (ende-test--complete-call (car ende-test--calls))
       (should (eq (ende-job-state job) 'completed)))
     (let ((control-drawers (ende-test--chat-control-drawers)))
       (should (= (length control-drawers) 3))
       (should-not (ende-test--chat-control-drawer-hidden-p
                    (car control-drawers)))
       (should (ende-test--chat-control-drawer-hidden-p
                (car (last control-drawers)))))
     (save-excursion
       (ende-test--chat-goto "** Mixed metadata")
       (forward-line 2)
       (should-not (invisible-p (point))))
     (should (string-match-p ":BENCHMARK_CUSTOM: keep-visible"
                             (buffer-string)))
     (should (string-match-p "Mixed drawer content remains visible"
                             (buffer-string))))))

(ert-deftest ende-test/chat-completion-reduces-whole-transcript-parsing ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("Answer") t)))
    (ende-test--with-buffer
     "* Root\n** Question\nAsk\n"
     (org-mode)
     (ende-test--chat-goto "** Question")
     (let* ((job (ende-ask))
            (target (current-buffer))
            (whole-transcript-parses 0)
            (scoped-target-parses 0)
            (other-buffer-parses 0)
            (parse-buffer (symbol-function 'org-element-parse-buffer)))
       (cl-letf (((symbol-function 'org-element-parse-buffer)
                  (lambda (&rest arguments)
                    (if (eq (current-buffer) target)
                        (if (and (= (point-min) 1)
                                 (= (point-max) (1+ (buffer-size))))
                            (cl-incf whole-transcript-parses)
                          (cl-incf scoped-target-parses))
                      (cl-incf other-buffer-parses))
                    (apply parse-buffer arguments))))
         (ende-test--complete-call (car ende-test--calls)))
       (should (eq (ende-job-state job) 'completed))
       (should (= whole-transcript-parses 0))
       (should (> scoped-target-parses 0))
       (should (> other-buffer-parses 0))))))

(ert-deftest ende-test/chat-completion-rejects-an-edited-destination ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("Generated answer") t)))
    (ende-test--with-buffer
     "* Root\n** Question\nAsk\n"
     (org-mode)
     (ende-test--chat-goto "** Question")
     (let ((job (ende-ask)))
       (pcase-let ((`(,_document ,placeholder)
                    (ende-chat--placeholder job)))
         (goto-char (org-element-property :begin placeholder))
         (org-edit-headline "User-edited destination"))
       (ende-test--complete-call (car ende-test--calls))
       (should (eq (ende-job-state job) 'failed))
       (should (string-match-p "User-edited destination" (buffer-string)))
       (should-not (string-match-p "Generated answer" (buffer-string)))
       (should (string-match-p "destination was edited or moved"
                               (buffer-string)))))))

(ert-deftest ende-test/chat-persistence-failure-restores-the-pending-slot ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("Answer") t)))
    (ende-test--with-buffer
     "* Root\n** Question\nAsk\n"
     (org-mode)
     (ende-test--chat-goto "** Question")
     (let* ((job (ende-ask))
            (instruction (ende-job-instruction job))
            (target-marker (ende-target-cursor-marker (ende-job-target job)))
            (position (marker-position target-marker))
            (before (buffer-string))
            (response (ende-chat--assistant-text
                       2 (plist-get instruction :question-id)
                       (plist-get instruction :request-id) ""
                       '(:content "Generated answer"))))
       (cl-letf (((symbol-function 'ende-chat--hide-control-drawers-in-region)
                  (lambda (&rest _arguments)
                    (error "Injected drawer-folding failure"))))
         (should-error
          (ende-chat--persist-request-section job response nil nil t)))
       (should (equal before (buffer-string)))
       (should (= position (marker-position target-marker)))
       (should (ende-chat--placeholder job))
       (ende-test--complete-call (car ende-test--calls))
       (should (eq (ende-job-state job) 'completed))))))

(ert-deftest ende-test/chat-request-markers-are-released-on-settlement ()
  (dolist (outcome '(completed failed cancelled))
    (ende-test--reset)
    (let ((ende-llm-function (ende-test--fake-llm '("Answer") t)))
      (ende-test--with-buffer
       "* Root\n** Question\nAsk\n"
       (org-mode)
       (ende-test--chat-goto "** Question")
       (let* ((job (ende-ask))
              (question-marker
               (plist-get (ende-job-instruction job) :question-marker)))
         (should (eq (marker-buffer question-marker) (current-buffer)))
         (pcase outcome
           ('completed (ende-test--complete-call (car ende-test--calls)))
           ('failed (ende-job-fail job "Injected failure"))
           ('cancelled (ende-abort-queue)))
         (should (eq (ende-job-state job) outcome))
         (should-not (marker-buffer question-marker))
         (should-not (plist-get (ende-job-instruction job) :question-marker))
         (should-not (plist-get (ende-job-instruction job)
                               :request-end-marker)))))))

(ert-deftest ende-test/chat-user-authored-agent-work-heading-is-ordinary-content ()
  (ende-test--reset)
  (ende-test--with-buffer
   (concat "* Topic\n"
           "** User\nA question.\n"
           "** Agent: Work\nThese are ordinary user notes.\n"
           "** Follow-up\nContinue.\n")
   (org-mode)
   (ende-test--chat-goto "** Follow-up")
   (let* ((document (ende-chat--build-document))
          (work (cl-find-if
                 (lambda (headline)
                   (equal (org-element-property :raw-value headline)
                          "Agent: Work"))
                 (ende-chat--document-headlines document)))
          (messages (ende-chat-context-at-point)))
     (should work)
     (should-not (ende-chat--work-section-p document work))
     (let ((work-message (car (last messages 2))))
       (should (string-match-p "Agent: Work"
			       (ende-test--chat-context-text
                                (list work-message))))
       (should (string-match-p "These are ordinary user notes"
			       (ende-test--chat-context-text
                                (list work-message))))))))

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
	       (ende-test--complete-call (nth 1 ende-test--calls))
	       (should (= (length ende-test--calls) 3))
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
	       (should
                (eq reply
                    (cadr (memq work
                                (ende-chat--children
                                 document (ende-chat--parent document work))))))
	       (dolist (child (ende-chat--children document work))
                 (should (equal (ende-chat--property document child :request-id)
                                (ende-chat--property document work :request-id))))
	       (should-not (cl-some
                            (lambda (drawers) drawers)
                            (hash-table-values
                             (ende-chat--document-tool-context-drawers
                              document))))
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
		 (should-not
                  (string-match-p "ENDE_CHAT_TOOL_CONTEXT" (buffer-string))))
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
		 (should-not (string-match-p "call-A" text)))))))
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
					(ende-chat--protocol-body-string call nil t))
	                              calls)
	                      '("{\"path\":\"a.el\"}" "{\"path\":\"b.el\"}")))
	       (should (eq (ende-chat--validate-work-section document work) work))
	       (should-not
                (string-match-p "This must not be requested" (buffer-string)))))))
      (ende-test--reset)
      (when (file-directory-p root)
        (delete-directory root t)))))

(defun ende-test--chat-unpartitioned-text (document question)
  "Return the selected Org content for QUESTION without message boundaries."
  (let* ((root (ende-chat--root document question))
         (root-begin (org-element-property :begin root))
         (root-end (org-element-property :end root))
         (question-begin (org-element-property :begin question))
         (prefix
          (cl-remove-if-not
           (lambda (headline)
             (let ((begin (org-element-property :begin headline)))
               (and (>= begin root-begin)
                    (< begin question-begin)
                    (< begin root-end)
                    (ende-chat--eligible-p document headline))))
           (ende-chat--document-headlines document)))
         (current (ende-chat--current-headlines document question)))
    (concat (mapconcat (lambda (headline)
                         (ende-chat--section-text document headline))
                       prefix "")
            (mapconcat (lambda (headline)
                         (ende-chat--section-text document headline))
                       current ""))))

(defun ende-test--chat-response-count ()
  "Return the number of generated Agent: Reply headlines in the buffer."
  (cl-count-if
   (lambda (headline)
     (string= (org-element-property :raw-value headline) "Agent: Reply"))
   (org-element-map (org-element-parse-buffer) 'headline #'identity)))

(defun ende-test--chat-control-drawers ()
  "Return the Ende control property drawers in the current buffer."
  (cl-remove-if-not
   (lambda (drawer)
     (cl-every #'ende-chat--reserved-property-p
               (org-element-map drawer 'node-property #'identity)))
   (org-element-map (org-element-parse-buffer) 'property-drawer #'identity)))

(defun ende-test--chat-control-drawer-hidden-p (drawer)
  "Return non-nil when DRAWER's first property line is folded."
  (save-excursion
    (goto-char (org-element-property :begin drawer))
    (forward-line 1)
    (invisible-p (point))))

(defun ende-test--chat-response-headlines ()
  "Return generated Agent: Reply headlines in document order."
  (cl-remove-if-not
   (lambda (headline)
     (string= (org-element-property :raw-value headline) "Agent: Reply"))
   (org-element-map (org-element-parse-buffer) 'headline #'identity)))

(defun ende-test--chat-response-has-separator-p (headline)
  "Return non-nil when HEADLINE has a blank line before it."
  (save-excursion
    (goto-char (org-element-property :begin headline))
    (forward-line -1)
    (looking-at-p "^[[:space:]]*$")))

(defun ende-test--chat-reasoning-headlines ()
  "Return generated Agent: Reasoning headlines in document order."
  (cl-remove-if-not
   (lambda (headline)
     (string= (org-element-property :raw-value headline) "Agent: Reasoning"))
   (org-element-map (org-element-parse-buffer) 'headline #'identity)))

(defun ende-test--chat-headline-property (headline property)
  "Return PROPERTY from HEADLINE."
  (save-excursion
    (goto-char (org-element-property :begin headline))
    (org-entry-get nil property)))

(defun ende-test--chat-headline-text-hidden-p (headline text)
  "Return non-nil when TEXT in HEADLINE's subtree is folded."
  (save-excursion
    (goto-char (org-element-property :begin headline))
    (when (search-forward text (org-element-property :end headline) t)
      (invisible-p (1- (point))))))

(ert-deftest ende-test/chat-context-isolates-level-one-roots ()
  (ende-test--reset)
  (ende-test--with-buffer
   (concat "* First root\n"
           "First root material\n"
           "** First question\n"
           "First question\n"
           "* Second root\n"
           "Second root material\n"
           "** Second question\n"
           "Second question\n")
   (org-mode)
   (ende-test--chat-goto "** Second question")
   (let* ((messages (ende-chat-context-at-point))
          (text (ende-test--chat-context-text messages)))
     (should (string-match-p "Second root material" text))
     (should (string-match-p "Second question" text))
     (should-not (string-match-p "First root material" text))
     (should-not (string-match-p "First question" text)))))

(ert-deftest ende-test/chat-hidden-and-reasoning-obey-independent-policies ()
  (ende-test--reset)
  (ende-test--with-buffer
   (concat "* Root\n"
           "** Assistant material\n"
           ":PROPERTIES:\n"
           ":ENDE_ROLE: assistant\n"
           ":END:\n"
           "Answer\n"
           "*** Hidden assistant descendant\n"
           ":PROPERTIES:\n"
           ":ENDE_VISIBILITY: hidden\n"
           ":END:\n"
           "Never send this\n"
           "*** Reasoning section\n"
           ":PROPERTIES:\n"
           ":ENDE_VISIBILITY: reasoning\n"
           ":END:\n"
           "Section thought\n"
           "**** Hidden reasoning descendant\n"
           ":PROPERTIES:\n"
           ":ENDE_VISIBILITY: hidden\n"
           ":END:\n"
           "Still never send this\n"
           "** Question\n"
           "Ask\n")
   (org-mode)
   (ende-test--chat-goto "** Question")
   (let ((without-reasoning
          (ende-test--chat-context-text
           (let ((ende-chat-include-reasoning nil))
             (ende-chat-context-at-point)))))
     (should (string-match-p "Answer" without-reasoning))
     (should-not (string-match-p "Section thought" without-reasoning))
     (should-not (string-match-p "Never send this" without-reasoning)))
   (let ((with-reasoning
          (ende-test--chat-context-text
           (let ((ende-chat-include-reasoning t))
             (ende-chat-context-at-point)))))
     (should (string-match-p "Section thought" with-reasoning))
     (should-not (string-match-p "Never send this" with-reasoning)))))

(ert-deftest ende-test/chat-role-inheritance-is-overridden-explicitly ()
  (ende-test--reset)
  (ende-test--with-buffer
   (concat "* Root\n"
           "** Assistant section\n"
           ":PROPERTIES:\n"
           ":ENDE_ROLE: assistant\n"
           ":END:\n"
           "Assistant answer\n"
           "*** Inherited child\n"
           "Inherited assistant text\n"
           "*** Explicit user child\n"
           ":PROPERTIES:\n"
           ":ENDE_ROLE: user\n"
           ":END:\n"
           "User reference text\n"
           "** Question\n"
           "Ask\n")
   (org-mode)
   (ende-test--chat-goto "** Question")
   (let* ((messages (ende-chat-context-at-point))
          (roles (mapcar #'car messages))
          (text (ende-test--chat-context-text messages)))
     (should (equal roles '("user" "assistant" "user" "user")))
     (should (string-match-p "Assistant answer" text))
     (should (string-match-p "Inherited assistant text" text))
     (should (string-match-p "User reference text" text))
     (should (string-match-p "Inherited assistant text"
                             (cdr (nth 1 messages))))
     (should (equal (car (car (last messages))) "user")))))

(ert-deftest ende-test/chat-prefix-messages-follow-explicit-role-boundaries ()
  (ende-test--reset)
  (ende-test--with-buffer
   (concat "* Topic\n"
           "Root source.\n"
           "** Reference\n"
           "Reference source.\n"
           "*** Nested reference\n"
           "Nested reference source.\n"
           "** Assistant response\n"
           ":PROPERTIES:\n"
           ":ENDE_ROLE: assistant\n"
           ":END:\n"
           "Assistant source.\n"
           "*** Inherited assistant section\n"
           "Inherited assistant source.\n"
           "**** Deeper inherited section\n"
           "Deeper assistant source.\n"
           "*** Explicit user section\n"
           ":PROPERTIES:\n"
           ":ENDE_ROLE: user\n"
           ":END:\n"
           "Nested user source.\n"
           "**** Inherited user section\n"
           "Inherited user source.\n"
           "*** Resumed assistant section\n"
           "Resumed assistant source.\n"
           "*** Explicit same-role section\n"
           ":PROPERTIES:\n"
           ":ENDE_ROLE: assistant\n"
           ":END:\n"
           "Same-role boundary source.\n"
           "**** Inherited from same-role section\n"
           "Same-role descendant source.\n"
           "*** Resumed after same-role section\n"
           "Assistant after nested section source.\n"
           "** Question\n"
           "Current question source.\n"
           "** Later sibling\n"
           "Later sibling source.\n")
   (org-mode)
   (ende-test--chat-goto "** Question")
   (let* ((document (ende-chat--build-document))
          (question (ende-chat--headline-at-point document))
          (messages (ende-chat--serialize document question))
          (roles (mapcar #'car messages))
          (contents (mapcar #'cdr messages))
          (serialized-text (mapconcat #'identity contents "")))
     (should (equal roles
                    '("user" "assistant" "user" "assistant"
                      "assistant" "assistant" "user")))
     (should (equal serialized-text
                    (ende-test--chat-unpartitioned-text document question)))
     (should (string-match-p "Root source" (nth 0 contents)))
     (should (string-match-p "Nested reference source" (nth 0 contents)))
     (should-not (string-match-p "Assistant source" (nth 0 contents)))
     (should (string-match-p "Inherited assistant source" (nth 1 contents)))
     (should (string-match-p "Deeper assistant source" (nth 1 contents)))
     (should-not (string-match-p "Nested user source" (nth 1 contents)))
     (should (string-match-p "Nested user source" (nth 2 contents)))
     (should (string-match-p "Inherited user source" (nth 2 contents)))
     (should (string-match-p "Resumed assistant source" (nth 3 contents)))
     (should (string-match-p "Same-role boundary source" (nth 4 contents)))
     (should (string-match-p "Same-role descendant source" (nth 4 contents)))
     (should (string-match-p "Assistant after nested section source"
                             (nth 5 contents)))
     (should (string-match-p "Current question source" (nth 6 contents)))
     (should-not (string-match-p "Later sibling source" serialized-text)))))

(ert-deftest ende-test/chat-current-question-skips-independent-child-subtrees ()
  (ende-test--reset)
  (ende-test--with-buffer
   (concat "* Root\n"
           "** Question\n"
           "Question body\n"
           "*** Unmarked reference\n"
           "Include this reference\n"
           "*** Explicit user child\n"
           ":PROPERTIES:\n"
           ":ENDE_ROLE: user\n"
           ":END:\n"
           "Do not include this turn\n"
           "*** Assistant child\n"
           ":PROPERTIES:\n"
           ":ENDE_ROLE: assistant\n"
           ":END:\n"
           "Do not include this answer\n"
           "*** Hidden child\n"
           ":PROPERTIES:\n"
           ":ENDE_VISIBILITY: hidden\n"
           ":END:\n"
           "Do not include this private note\n"
           "*** Later unmarked reference\n"
           "Include this later reference\n"
           "** Later sibling\n"
           "Must not enter this request\n")
   (org-mode)
   (ende-test--chat-goto "** Question")
   (let ((text (ende-test--chat-context-text
                (ende-chat-context-at-point))))
     (should (string-match-p "Question body" text))
     (should (string-match-p "Include this reference" text))
     (should (string-match-p "Include this later reference" text))
     (should-not (string-match-p "Do not include this turn" text))
     (should-not (string-match-p "Do not include this answer" text))
     (should-not (string-match-p "Do not include this private note" text))
     (should-not (string-match-p "Must not enter this request" text)))))

(ert-deftest ende-test/chat-root-prefix-policy-can-be-narrowed ()
  (ende-test--reset)
  (ende-test--with-buffer
   (concat "* Root\n"
           "Earlier material\n"
           "** Question\n"
           "Current material\n")
   (org-mode)
   (ende-test--chat-goto "** Question")
   (let ((root-prefix (ende-test--chat-context-text
		       (let ((ende-chat-context-policy 'root-prefix))
                         (ende-chat-context-at-point))))
         (current-only (ende-test--chat-context-text
                        (let ((ende-chat-context-policy
			       'current-section-only))
                          (ende-chat-context-at-point)))))
     (should (string-match-p "Earlier material" root-prefix))
     (should-not (string-match-p "Earlier material" current-only))
     (should (string-match-p "Current material" current-only)))))

(ert-deftest ende-test/chat-context-fences-substitute-history-by-scope ()
  (ende-test--reset)
  (let ((ende-chat-context-policy 'root-prefix)
        (ende-chat-include-reasoning nil))
    (ende-test--with-buffer
     (concat "* Q0\nOriginal root context\n"
	     "** R1\n:PROPERTIES:\n:ENDE_ROLE: assistant\n:END:\n"
	     "** C0\n:PROPERTIES:\n:ENDE_ROLE: assistant\n"
	     ":ENDE_CHAT_CONTEXT_FENCE: t\n:END:\n"
	     "** Q01\n:PROPERTIES:\n:ENDE_ROLE: user\n:END:\n"
	     "** R01\n:PROPERTIES:\n:ENDE_ROLE: assistant\n:END:\n"
	     "*** Q11\n:PROPERTIES:\n:ENDE_ROLE: user\n:END:\n"
	     "*** R11\n:PROPERTIES:\n:ENDE_ROLE: assistant\n:END:\n"
	     "*** C1\n:PROPERTIES:\n:ENDE_ROLE: assistant\n"
	     ":ENDE_CHAT_CONTEXT_FENCE: t\n:END:\n"
	     "**** Summary detail\nRemember this detail\n"
	     "*** Instructions\n:PROPERTIES:\n"
	     ":ENDE_VISIBILITY: hidden\n:END:\nNever send this\n"
	     "*** Q1\n:PROPERTIES:\n:ENDE_ROLE: user\n:END:\n"
	     "**** Reference\nCurrent question reference\n"
	     "*** R12\n:PROPERTIES:\n:ENDE_ROLE: assistant\n:END:\n"
	     "** Q2\n:PROPERTIES:\n:ENDE_ROLE: user\n:END:\n"
	     "** R02\n:PROPERTIES:\n:ENDE_ROLE: assistant\n:END:\n"
	     "*** Q21\n:PROPERTIES:\n:ENDE_ROLE: user\n:END:\n"
	     "**** Q211\n:PROPERTIES:\n:ENDE_ROLE: user\n:END:\n"
	     "** C2\n:PROPERTIES:\n:ENDE_ROLE: assistant\n"
	     ":ENDE_CHAT_CONTEXT_FENCE: t\n:END:\n"
	     "** Q3\n:PROPERTIES:\n:ENDE_ROLE: user\n:END:\n"
	     "* Other\n** Q4\n")
     (org-mode)
     ;; These queries exercise a local fence, an ancestor fence, an earlier
     ;; branch at the same depth, later fences, and an independent root.
     (dolist (case
	      '(("*** Q11" "C0" "Q01" "R01" "Q11")
                ("*** Q1" "C1" "Summary detail" "Q1" "Reference")
                ("** Q2" "C0" "Q01" "R01" "C1" "Summary detail"
                 "Q1" "Reference" "R12" "Q2")
                ("*** Q21" "C0" "Q01" "R01" "C1" "Summary detail"
                 "Q1" "Reference" "R12" "Q2" "R02" "Q21")
                ("**** Q211" "C0" "Q01" "R01" "C1" "Summary detail"
                 "Q1" "Reference" "R12" "Q2" "R02" "Q21" "Q211")
                ("** Q3" "C2" "Q3")
                ("** Q4" "Other" "Q4")))
       (ende-test--chat-goto (car case))
       (let* ((messages (ende-chat-context-at-point))
              (text (ende-test--chat-context-text messages)))
         (ert-info ((format "Context at %s" (car case)))
           (should (equal (ende-test--chat-context-headings messages)
                          (cdr case)))
           (should (equal (caar (last messages)) "user"))
           (should-not (string-match-p "Original root context" text))
           (should-not (string-match-p "Never send this" text))
           (should-not (string-match-p "ENDE_CHAT_CONTEXT_FENCE" text)))))
     ;; An unfinished result cannot replace history, even if it was marked.
     (dolist (state '("pending" "failed"))
       (ende-test--chat-goto "** C2")
       (org-entry-put nil ende-chat-state-property state)
       (org-entry-put nil ende-chat-visibility-property "hidden")
       (ende-test--chat-goto "** Q3")
       (should
        (equal (ende-test--chat-context-headings (ende-chat-context-at-point))
	       '("C0" "Q01" "R01" "C1" "Summary detail" "Q1" "Reference"
                 "R12" "Q2" "R02" "Q21" "Q211" "Q3"))))
     (let ((ende-chat-context-policy 'current-section-only))
       (ende-test--chat-goto "*** Q1")
       (should
        (equal (ende-test--chat-context-headings (ende-chat-context-at-point))
	       '("Q1" "Reference")))))))

(ert-deftest ende-test/chat-compaction-preserves-instructions-and-org-layout ()
  (dolist (level '(2 4))
    (dolist (input '(("" "")
                     ("" "Keep the decisions.")
                     ("My summary" "")
                     ("My summary" "Keep the decisions.")
                     ("" "" "A note added during generation.")))
      (ende-test--reset)
      (let* ((title (nth 0 input))
             (body (nth 1 input))
             (late-note (nth 2 input))
             (ende-chat-compact-instruction "Keep the essentials.")
             (ende-llm-function
              (ende-test--fake-llm
               '((:content "#+ENDE_CHAT_TITLE: Ignore this title\n* Facts\nSummary"
			   :thinking "Plan")) t)))
        (ende-test--with-buffer "* Topic\nEarlier conversation\n"
				(org-mode)
				(goto-char (point-max))
				(cl-loop for depth from 2 below level
					 do (insert (make-string depth ?*) " Ancestor\n"))
				(let ((start (point)))
				  (insert (make-string level ?*) " " title "\n" body "\n"
					  (make-string level ?*) " Next\nContinue\n")
				  (goto-char start)
				  (let* ((job (ende-compact '(:name local :model "compact-model")))
					 (question-id (plist-get (ende-job-instruction job) :question-id))
					 (call (car ende-test--calls))
					 (messages (plist-get call :messages)))
				    (should (eq (ende-job-state job) 'running))
				    (should (string-match-p
					     (regexp-quote (if (string-empty-p body)
							       ende-chat-compact-instruction body))
					     (cdar (last messages))))
				    (should-not (string-match-p "ENDE_CHAT_TITLE" (cdar messages)))
				    (when (and (not (string-empty-p title))
					       (string-empty-p body))
				      (should-not (string-match-p (regexp-quote title)
								  (cdar (last messages)))))
				    (when late-note
				      (org-end-of-meta-data t)
				      (insert late-note "\n"))
				    (ende-test--complete-call call)
				    (should (eq (ende-job-state job) 'completed))
				    (let* ((document (ende-chat--build-document))
					   (question (ende-chat--find-by-property
						      document ende-chat-id-property question-id))
					   (fence (ende-chat--find-by-property
						   document ende-chat-request-id-property
						   (plist-get (ende-job-instruction job) :request-id)))
					   (reasoning (ende-chat--find-by-property
						       document ende-chat-reasoning-for-property
						       (plist-get (ende-job-instruction job)
								  :request-id))))
				      (should (ende-chat--context-fence-p document fence))
				      (should (= (org-element-property :level fence) level))
				      (should (equal (org-element-property :raw-value fence)
						     "Agent: Context Fence"))
				      (should (eq (ende-chat--parent document reasoning) fence))
				      (should-not (ende-chat--context-fence-p document reasoning))
				      (should (equal (ende-test--chat-headline-property
						      fence ende-chat-model-property)
						     "compact-model"))
				      (if (and (string-empty-p title) (string-empty-p body)
					       (not late-note))
					  (should-not question)
					(should question)
					(should (equal (org-element-property :raw-value question)
						       (if (string-empty-p title)
							   "User: Context Fence Instructions" title)))
					(should (equal (ende-chat--property document question :visibility)
						       "hidden"))
					(should (= (org-element-property :end question)
						   (org-element-property :begin fence)))
					(should (string-match-p (regexp-quote (or late-note body))
								(buffer-string))))
				      (should-not (string-match-p "Ignore this title" (buffer-string)))
				      (should (cl-every #'ende-test--chat-control-drawer-hidden-p
							(ende-test--chat-control-drawers))))
				    (ende-test--chat-goto (concat (make-string level ?*) " Next"))
				    (let ((ende-chat-include-reasoning nil))
				      (should
				       (equal (ende-test--chat-context-headings
					       (ende-chat-context-at-point))
					      '("Agent: Context Fence" "Facts" "Next")))))))))))

(ert-deftest ende-test/chat-compaction-and-asks-share-live-context ()
  (ende-test--reset)
  (let ((ende-provider-profiles '((local :parallel t :max-concurrency 2)))
        (ende-llm-function
         (ende-test--fake-llm
          '("First summary" "New answer" "Second summary" "Final answer"
            "Third summary") t)))
    (ende-test--with-buffer
     (concat "* Topic\nOriginal history\n"
	     "** First compaction\nKeep decisions\n"
	     "** Follow up\nNew question\n"
	     "** Second compaction\nKeep commands\n"
	     "** Final question\nContinue\n")
     (org-mode)
     (ende-test--chat-goto "** First compaction")
     (let ((first (ende-compact)))
       (ende-test--chat-goto "** Follow up")
       (let ((ask (ende-ask)))
         (should (eq (ende-job-state ask) 'queued))
         (ende-test--complete-call (nth 0 ende-test--calls))
         (should (eq (ende-job-state first) 'completed))
         (should (eq (ende-job-state ask) 'running))
         (let ((prompt (ende-test--chat-context-text
                        (plist-get (nth 1 ende-test--calls) :messages))))
           (should (string-match-p "First summary" prompt))
           (should-not (string-match-p "Original history" prompt))
           (should-not (string-match-p "Keep decisions" prompt)))
         (ende-test--chat-goto "** Second compaction")
         (let ((second (ende-compact)))
           (should (eq (ende-job-state second) 'queued))
           (ende-test--chat-goto "** Follow up")
           (org-end-of-meta-data t)
           (insert "Latest detail\n")
           (ende-test--complete-call (nth 1 ende-test--calls))
           (should (eq (ende-job-state second) 'running))
           (let ((prompt (ende-test--chat-context-text
                          (plist-get (nth 2 ende-test--calls) :messages))))
             (dolist (text '("First summary" "New answer" "Latest detail"
                             "Keep commands"))
	       (should (string-match-p text prompt)))
             (should-not (string-match-p "Original history" prompt)))
           (ende-test--chat-goto "** Final question")
           (let ((final (ende-ask)))
             (should (eq (ende-job-state final) 'queued))
             (ende-test--complete-call (nth 2 ende-test--calls))
             (let ((prompt (ende-test--chat-context-text
                            (plist-get (nth 3 ende-test--calls) :messages))))
	       (should (string-match-p "Second summary" prompt))
	       (dolist (text '("First summary" "New answer" "Latest detail"
			       "Keep commands" "Original history"))
                 (should-not (string-match-p text prompt))))
             (ende-test--complete-call (nth 3 ende-test--calls))
             (should (eq (ende-job-state final) 'completed)))
           ;; Compacting an existing fence summarizes its result, not its
           ;; archived instructions or the conversation after it.
           (let* ((document (ende-chat--build-document))
                  (fence (ende-chat--find-by-property
                          document ende-chat-request-id-property
                          (plist-get (ende-job-instruction second) :request-id))))
             (goto-char (org-element-property :begin fence))
             (let ((third (ende-compact)))
	       (should
                (equal (ende-test--chat-context-headings
                        (butlast (cdr (plist-get (nth 4 ende-test--calls)
                                                 :messages))))
		       '("Agent: Context Fence")))
	       (ende-test--complete-call (nth 4 ende-test--calls))
	       (should (eq (ende-job-state third) 'completed)))))))
     ;; A fresh buffer reconstructs the same context using only durable Org.
     (let ((text (buffer-string)))
       (with-temp-buffer
         (org-mode)
         (insert text)
         (ende-test--chat-goto "** Final question")
         (let* ((messages (ende-chat-context-at-point))
                (context (ende-test--chat-context-text messages)))
           (should (equal (ende-test--chat-context-headings messages)
                          '("Agent: Context Fence" "Final question")))
           (should (string-match-p "Third summary" context))
           (should-not (string-match-p "Second summary" context))))))))

(ert-deftest ende-test/chat-compaction-failures-leave-history-usable ()
  (dolist (outcome '(:error :empty :title-only :cancelled :deleted))
    (ende-test--reset)
    (let ((ende-llm-function
           (ende-test--fake-llm
            (list (pcase outcome
                    (:error :error)
                    (:empty "")
                    (:title-only "#+ENDE_CHAT_TITLE: Not a summary\n")
                    (_ "Summary"))) t)))
      (ende-test--with-buffer
       "* Topic\nOriginal history\n** Instructions\nKeep decisions\n** Next\nAsk\n"
       (org-mode)
       (ende-test--chat-goto "** Instructions")
       (let ((job (ende-compact)))
         (pcase outcome
           (:cancelled (ende-abort-queue))
           (:deleted
            (pcase-let ((`(,_document ,headline) (ende-chat--placeholder job)))
              (delete-region (org-element-property :begin headline)
                             (org-element-property :end headline)))))
         (ende-test--complete-call (car ende-test--calls))
         (should (eq (ende-job-state job)
                     (if (eq outcome :cancelled) 'cancelled 'failed)))
         (should (string-match-p "Keep decisions" (buffer-string)))
         (let ((document (ende-chat--build-document)))
           (should-not
            (cl-some (lambda (headline)
		       (ende-chat--context-fence-p document headline))
                     (ende-chat--document-headlines document))))
         (ende-test--chat-goto "** Next")
         (let* ((messages (ende-chat-context-at-point))
                (context (ende-test--chat-context-text messages)))
           (should (equal (ende-test--chat-context-headings messages)
                          '("Topic" "Next")))
           (should (string-match-p "Original history" context))
           (should-not (string-match-p "Keep decisions" context))))))))

(ert-deftest ende-test/chat-compaction-supports-queued-repeats ()
  (dolist (body '("" "Keep the decisions"))
    (ende-test--reset)
    (let ((ende-llm-function
           (ende-test--fake-llm '("First summary" "Second summary") t)))
      (ende-test--with-buffer
       (concat "* Topic\nOriginal history\n** \n" body "\n** Next\nContinue\n")
       (org-mode)
       (ende-test--chat-goto "** ")
       (let ((first (ende-compact))
             (second (ende-compact)))
         (should (eq (ende-job-state second) 'queued))
         (ende-test--complete-call (nth 0 ende-test--calls))
         (should (eq (ende-job-state first) 'completed))
         (should (eq (ende-job-state second) 'running))
         (let ((context (ende-test--chat-context-text
                         (plist-get (nth 1 ende-test--calls) :messages))))
           (should (string-match-p "First summary" context))
           (should-not (string-match-p "Original history" context)))
         (ende-test--complete-call (nth 1 ende-test--calls))
         (should (eq (ende-job-state second) 'completed))
         (ende-test--chat-goto "** Next")
         (let* ((messages (ende-chat-context-at-point))
                (context (ende-test--chat-context-text messages)))
           (should (equal (ende-test--chat-context-headings messages)
                          '("Agent: Context Fence" "Next")))
           (should (string-match-p "Second summary" context))
           (should-not (string-match-p "First summary" context)))
         (if (string-empty-p body)
             (should-not (string-match-p "^\\*\\* *$" (buffer-string)))
           (should (string-match-p body (buffer-string)))))))))

(ert-deftest ende-test/chat-ask-marks-question-and-retries-without-sent-state ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm
          '((:content "First answer" :thinking "First plan")
            (:content "Second answer" :thinking "Second plan")))))
    (ende-test--with-buffer "* Root\n** Question\nAsk again\n"
			    (org-mode)
			    (ende-test--chat-goto "** Question")
			    (ende-ask)
			    (ende-ask)
			    (should (= (ende-test--chat-response-count) 2))
			    (should (= (length (ende-test--chat-reasoning-headlines)) 2))
			    (should (equal (org-entry-get nil ende-chat-role-property)
					   "user"))
			    (should (< (string-match-p "First answer" (buffer-string))
				       (string-match-p "Second answer" (buffer-string))))
			    (let* ((document (ende-chat--build-document))
				   (headlines (ende-chat--document-headlines document))
				   (reasoning
				    (cl-remove-if-not
				     (lambda (headline)
				       (string= (org-element-property :raw-value headline)
						"Agent: Reasoning"))
				     headlines))
				   (replies
				    (cl-remove-if-not
				     (lambda (headline)
				       (string= (org-element-property :raw-value headline)
						"Agent: Reply"))
				     headlines)))
			      (should (< (org-element-property :begin (nth 0 reasoning))
					 (org-element-property :begin (nth 0 replies))))
			      (should (< (org-element-property :begin (nth 0 replies))
					 (org-element-property :begin (nth 1 reasoning))))
			      (should (< (org-element-property :begin (nth 1 reasoning))
					 (org-element-property :begin (nth 1 replies))))
			      (dolist (pair (cl-mapcar #'cons reasoning replies))
				(let ((request-id
				       (ende-chat--property document (cdr pair) :request-id)))
				  (should (equal (ende-chat--property document (car pair)
								      :reasoning-for)
						 request-id))
				  (should (eq (ende-chat--find-by-property
					       document ende-chat-request-id-property request-id)
					      (cdr pair))))))
			    (should-not (cl-some #'ende-review-overlay-p
						 (overlays-in (point-min) (point-max)))))))

(ert-deftest ende-test/chat-title-directive-is-applied-and-removed ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm
          (list (concat "#+ENDE_CHAT_TITLE: Explain the request\n\n"
                        "#+begin_example\n"
                        "Instructions → User question — café\n"
                        "#+end_example\n\n"
                        "Answer\n* Generated")))))
    (ende-test--with-buffer "* Root\n** \nRequest body\n"
			    (org-mode)
			    (ende-test--chat-goto "** ")
			    (let ((job (ende-ask)))
			      (should (eq (ende-job-state job) 'completed)))
			    (should-not (ende-test--chat-reasoning-headlines))
			    (should (string-match-p "^\\*\\* Explain the request$"
						    (buffer-string)))
			    (should-not (string-match-p "ENDE_CHAT_TITLE" (buffer-string)))
			    (should (string-match-p "Answer" (buffer-string)))
			    (should (string-match-p
				     "Instructions → User question — café" (buffer-string)))
			    (should (string-match-p "^\\*\\*\\* Generated$"
						    (buffer-string))))))

(ert-deftest ende-test/chat-response-blocks-stay-opaque-and-close-at-boundary ()
  (let* ((source
          (concat "* First active heading\n"
                  "#+begin_src org\n"
                  "#+ENDE_CHAT_TITLE: literal text\n"
                  ":PROPERTIES:\n:ENDE_ROLE: user\n:END:\n"
                  "*** Literal source heading\n"
                  "Unicode: café → “quoted”.\n"
                  ",* Previously escaped heading\n"
                  ",#+end_src\n"
                  "#+begin_quote\n**** Nested source heading\n#+end_quote\n"
                  "#+end_src\n"
                  "** Second active heading\n"
                  "#+begin_quote\n*** Literal quote heading\n"))
         (normalized (ende-chat--normalize-response source 2)))
    (should-not (plist-get normalized :title))
    (should
     (equal (plist-get normalized :content)
            (concat "*** First active heading\n"
                    "#+begin_src org\n"
                    ",#+ENDE_CHAT_TITLE: literal text\n"
                    ":PROPERTIES:\n:ENDE_ROLE: user\n:END:\n"
                    ",*** Literal source heading\n"
                    "Unicode: café → “quoted”.\n"
                    ",,* Previously escaped heading\n"
                    ",,#+end_src\n"
                    ",#+begin_quote\n,**** Nested source heading\n,#+end_quote\n"
                    "#+end_src\n"
                    "**** Second active heading\n"
                    "#+begin_quote\n,*** Literal quote heading\n"
                    "#+end_quote")))))

(ert-deftest ende-test/chat-unclosed-response-block-does-not-swallow-later-replies ()
  (ende-test--reset)
  (let ((ende-provider-profiles '((local :parallel t :max-concurrency 2)))
        (ende-llm-function
         (ende-test--fake-llm
          '((:content "First answer\n#+begin_src org\n*** Literal heading"
		      :thinking "Plan\n#+begin_quote\n* Literal thought")
            "Second answer"
            "Third answer")
          t)))
    (ende-test--with-buffer
     (concat "* Topic\n"
	     "** First question\nFirst request\n"
	     "** Second question\nSecond request\n"
	     "** Third question\nThird request\n"
	     "** Later section\nKeep this section\n")
     (org-mode)
     (ende-test--chat-goto "** First question")
     (setq ende--queue-suspended t)
     (let ((first-job (ende-ask)))
       (ende-test--chat-goto "** Second question")
       (let ((second-job (ende-ask)))
         (setq ende--queue-suspended nil)
         (ende-process-next)
         (should (= (length ende-test--calls) 2))
         ;; The lower reply finishes while the earlier request is still open.
         (ende-test--complete-call (nth 1 ende-test--calls))
         (should (eq (ende-job-state second-job) 'completed))
         (ende-test--complete-call (nth 0 ende-test--calls))
         (should (eq (ende-job-state first-job) 'completed))
         (should (string-match-p (regexp-quote "#+end_src")
                                 (buffer-string)))
         (should (string-match-p (regexp-quote "#+end_quote")
                                 (buffer-string)))
         (should (string-match-p (regexp-quote ",* Literal thought")
                                 (buffer-string)))
         (should (string-match-p
                  (regexp-quote
                   "#+begin_src org\n,*** Literal heading\n#+end_src")
                  (buffer-string)))
         (let* ((document (ende-chat--build-document))
                (headlines (ende-chat--document-headlines document)))
           (dolist (title '("First question" "Second question"
                            "Third question" "Later section"))
             (should (cl-some
                      (lambda (headline)
                        (string= (org-element-property :raw-value headline)
                                 title))
                      headlines)))
           (should-not
            (cl-some
             (lambda (headline)
	       (member (org-element-property :raw-value headline)
		       '("Literal heading" "Literal thought")))
             headlines))
           (should (= (length (ende-test--chat-response-headlines)) 2)))
         ;; A fresh request below the repaired response must remain usable.
         (ende-test--chat-goto "** Third question")
         (let ((third-job (ende-ask)))
           (should (= (length ende-test--calls) 3))
           (ende-test--complete-call (nth 2 ende-test--calls))
           (should (eq (ende-job-state third-job) 'completed)))
         (ende-test--chat-goto "** Later section")
         (let ((context (ende-test--chat-context-text
                         (ende-chat-context-at-point))))
           (should (string-match-p "First answer" context))
           (should (string-match-p "Second answer" context))
           (should (string-match-p "Third answer" context))
           (should
            (string-match-p
             (regexp-quote
              "First answer\n#+begin_src org\n*** Literal heading")
             context))
           (should-not (string-match-p (regexp-quote "#+end_src") context))
           (should (string-match-p "Keep this section" context))))))))

(ert-deftest ende-test/chat-reasoning-is-org-structured-folded-and-configurable ()
  (ende-test--reset)
  (let* ((thinking (concat "Plan with /ordinary Org emphasis/.\n"
                           "* First thought\n"
                           "** Nested thought\n"
                           "#+begin_src emacs-lisp\n"
                           "(message \"reasoning source\")\n"
                           "#+end_src\n"))
         (ende-llm-function
          (ende-test--fake-llm
           (list (list :content "Answer\n** Reply section"
                       :thinking thinking)))))
    (ende-test--with-buffer
     (concat "* Root\n"
	     "** Question\n"
	     "Ask\n"
	     "** Next\n"
	     "Next\n")
     (org-mode)
     (ende-test--chat-goto "** Question")
     (ende-ask)
     (let* ((document (ende-chat--build-document))
            (headlines (ende-chat--document-headlines document))
            (reasoning
             (cl-find-if
              (lambda (headline)
                (string= (org-element-property :raw-value headline)
                         "Agent: Reasoning"))
              headlines))
            (reply
             (cl-find-if
              (lambda (headline)
                (string= (org-element-property :raw-value headline)
                         "Agent: Reply"))
              headlines))
            (reasoning-id
             (ende-chat--property document reasoning :reasoning-for))
            (reply-id (ende-chat--property document reply :request-id))
            (first-thought
             (cl-find-if
              (lambda (headline)
                (string= (org-element-property :raw-value headline)
                         "First thought"))
              headlines))
            (nested-thought
             (cl-find-if
              (lambda (headline)
                (string= (org-element-property :raw-value headline)
                         "Nested thought"))
              headlines))
            (reply-section
             (cl-find-if
              (lambda (headline)
                (string= (org-element-property :raw-value headline)
                         "Reply section"))
              headlines))
            (source-blocks
             (org-element-map reasoning 'src-block #'identity)))
       (should (equal (org-element-property :raw-value reasoning)
                      "Agent: Reasoning"))
       (should (equal (org-element-property :raw-value reply) "Agent: Reply"))
       (should (= (org-element-property :level reasoning) 2))
       (should (= (org-element-property :level reply) 2))
       (should (eq (ende-chat--parent document reasoning)
                   (ende-chat--parent document reply)))
       (should (< (org-element-property :begin reasoning)
                  (org-element-property :begin reply)))
       (should (eq (ende-chat--effective-role document reasoning) 'assistant))
       (should (eq (ende-chat--effective-role document reply) 'assistant))
       (should (eq (ende-chat--effective-visibility document reasoning)
                   'reasoning))
       (should (eq (ende-chat--effective-visibility document reply) 'normal))
       (should (equal reasoning-id reply-id))
       (should-not (ende-chat--property document reasoning :request-id))
       (should (eq (ende-chat--find-by-property
                    document ende-chat-request-id-property reply-id)
                   reply))
       (should (equal (org-element-property :raw-value first-thought)
                      "First thought"))
       (should (= (org-element-property :level first-thought) 3))
       (should (= (org-element-property :level nested-thought) 4))
       (should (eq (ende-chat--parent document first-thought) reasoning))
       (should (eq (ende-chat--parent document nested-thought) first-thought))
       (should (= (length source-blocks) 1))
       (should (equal (org-element-property :language (car source-blocks))
                      "emacs-lisp"))
       (should (equal (org-element-property :raw-value reply-section)
                      "Reply section"))
       (should (= (org-element-property :level reply-section) 3))
       (should-not (string-match-p ",\\*" (buffer-string)))
       (should (cl-every #'ende-test--chat-control-drawer-hidden-p
                         (ende-test--chat-control-drawers)))
       (should (ende-test--chat-headline-text-hidden-p reasoning "Plan"))
       (should-not (ende-test--chat-headline-text-hidden-p reply "Answer"))
       (save-excursion
         (goto-char (org-element-property :begin reasoning))
         (org-fold-show-subtree)
         (should-not (ende-test--chat-headline-text-hidden-p
                      reasoning "Plan"))))
     (ende-test--chat-goto "** Next")
     (let* ((without-messages
             (let ((ende-chat-include-reasoning nil))
	       (ende-chat-context-at-point)))
            (with-messages
             (let ((ende-chat-include-reasoning t))
	       (ende-chat-context-at-point)))
            (without (ende-test--chat-context-text without-messages))
            (with (ende-test--chat-context-text with-messages)))
       (should (equal (cdr (assoc "assistant" without-messages))
                      "Answer\n** Reply section"))
       (should-not (string-match-p "First thought" without))
       (should (string-match-p "Answer" without))
       (should-not (string-match-p "ENDE_CHAT_REASONING_FOR" without))
       (should (string-match-p "First thought" with))
       (should (string-match-p "reasoning source" with))
       (should (string-match-p "Answer" with))
       (should-not (string-match-p "ENDE_CHAT_REASONING_FOR" with))
       (should (equal (mapcar #'car without-messages)
                      '("user" "user" "assistant" "user")))
       (should (equal (mapcar #'car with-messages)
                      '("user" "user" "assistant" "assistant" "user")))))))

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
			  :reasoning-effort-wire-api openrouter
			  :reasoning-effort-validated-p t
			  :reasoning-effort "high")))
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
                                  (ende-chat--protocol-body-string call nil t))
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
                   (ende-chat--protocol-body-string call nil t))))
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

(ert-deftest ende-test/chat-response-provenance-is-durable-and-private ()
  (dolist (scenario
           '((:request-model "requested-model"
			     :reported-model "actual-model\n:ENDE_ROLE: user"
			     :expected-model "actual-model :ENDE_ROLE: user"
			     :effort "high")
             (:request-model "requested-fallback"
			     :expected-model "requested-fallback")))
    (ende-test--reset)
    (let* ((reported-model (plist-get scenario :reported-model))
           (effort (plist-get scenario :effort))
           (profile
            (append (list :name 'local
                          :model (plist-get scenario :request-model)
                          :reasoning-effort-wire-api 'openrouter
                          :reasoning-effort-validated-p t)
                    (when effort (list :reasoning-effort effort))))
           (response
            (append
             (list :content
                   (concat "* Answer\n"
                           ":PROPERTIES:\n"
                           ":ENDE_MODEL: forged-model\n"
                           ":ENDE_REASONING_EFFORT: forged-effort\n"
                           ":ENDE_CHAT_CONTEXT_FENCE: t\n"
                           ":END:\n\nAnswer body")
                   :thinking "Private plan")
             (when reported-model
               (list :reported-model reported-model))))
           (ende-llm-function (ende-test--fake-llm (list response))))
      (ende-test--with-buffer
       "* Root\n** Question\nAsk\n** Next\nFollow up\n"
       (org-mode)
       (ende-test--chat-goto "** Question")
       (let ((job (ende-ask profile)))
         (should (eq (ende-job-state job) 'completed)))
       (let ((reasoning (car (ende-test--chat-reasoning-headlines)))
             (reply (car (ende-test--chat-response-headlines))))
         (should reasoning)
         (should reply)
         (dolist (headline (list reasoning reply))
           (should (equal
                    (ende-test--chat-headline-property
                     headline ende-chat-model-property)
                    (plist-get scenario :expected-model)))
           (should (equal
                    (ende-test--chat-headline-property
                     headline ende-chat-reasoning-effort-property)
                    effort)))
         (should (cl-every #'ende-test--chat-control-drawer-hidden-p
                           (ende-test--chat-control-drawers)))
         (should (= 1 (ende-test--count-matches
		       "^:ENDE_ROLE: user$" (buffer-string))))
         (should-not (string-match-p "forged-model" (buffer-string)))
         (should-not (string-match-p "ENDE_CHAT_CONTEXT_FENCE" (buffer-string)))
         (should-not (string-match-p "ENDE_CHAT_TOOL_CONTEXT"
                                     (buffer-string)))
         (should-not (string-match-p "forged-effort" (buffer-string))))
       (ende-test--chat-goto "** Next")
       (let* ((ende-chat-include-reasoning t)
              (context
	       (ende-test--chat-context-text
                (ende-chat-context-at-point))))
         (should (string-match-p "Private plan" context))
         (should (string-match-p "Answer body" context))
         (dolist (private-value
                  (list (plist-get scenario :expected-model)
                        effort
                        ende-chat-model-property
                        ende-chat-reasoning-effort-property))
           (when private-value
             (should-not (string-match-p
                          (regexp-quote private-value) context)))))))))

(ert-deftest ende-test/chat-failure-keeps-question-and-reserves-slot ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(:error))))
    (ende-test--with-buffer "* Root\n** Question\nOriginal text\n"
			    (org-mode)
			    (ende-test--chat-goto "** Question")
			    (let ((job (ende-ask)))
			      (should (eq (ende-job-state job) 'failed)))
			    (should (string-match-p "Original text" (buffer-string)))
			    (should (string-match-p ":ENDE_CHAT_STATE: failed" (buffer-string)))
			    (should (string-match-p ":ENDE_VISIBILITY: hidden" (buffer-string))))))

(ert-deftest ende-test/chat-queued-context-is-built-at-dispatch-and-hides-placeholders ()
  (ende-test--reset)
  (let ((ende-provider-profiles '((local :parallel t :max-concurrency 2)))
        (ende-llm-function (ende-test--fake-llm '("First" "Second") t)))
    (ende-test--with-buffer "* Root\n** Question\nAsk\n"
			    (org-mode)
			    (ende-test--chat-goto "** Question")
			    (let ((point-before (point)))
			      (setq ende--queue-suspended t)
			      (let ((first (ende-ask))
				    (second (ende-ask)))
				(should (= (point) point-before))
				(let ((drawers (ende-test--chat-control-drawers)))
				  (should (= (length drawers) 3))
				  (should (cl-every #'ende-test--chat-control-drawer-hidden-p
						    drawers)))
				(let ((headlines (ende-test--chat-response-headlines)))
				  (should (= (length headlines) 2))
				  (should (cl-every #'ende-test--chat-response-has-separator-p
						    headlines))
				  (dolist (job (list first second))
				    (should
				     (cl-some
				      (lambda (headline)
					(= (overlay-start (ende-job-overlay job))
					   (1- (org-element-property :begin headline))))
				      headlines))))
				(setq ende--queue-suspended nil)
				(ende-process-next)
				(should (= (length ende-test--calls) 2))
				(dolist (messages ende-test--prompts)
				  (let ((users (mapcar #'cdr
						       (cl-remove-if-not
							(lambda (message)
							  (equal (car message) "user"))
							messages))))
				    (should (cl-every
					     (lambda (text)
					       (and (not (string-match-p "ENDE_CHAT_STATE" text))
						    (not (string-match-p "Agent: Reply" text))))
					     users))))
				(ende-test--complete-call (nth 1 ende-test--calls))
				(should (eq (ende-job-state second) 'completed))
				(should (eq (ende-job-state first) 'running))
				(should (string-match-p "Second" (buffer-string)))
				(should (cl-every #'ende-test--chat-control-drawer-hidden-p
						  (ende-test--chat-control-drawers)))
				(should (string-match-p "ENDE_CHAT_STATE: pending" (buffer-string)))
				(ende-test--complete-call (nth 0 ende-test--calls))
				(should (eq (ende-job-state first) 'completed))
				(should (< (string-match-p "First" (buffer-string))
					   (string-match-p "Second" (buffer-string))))
				(should (cl-every #'ende-test--chat-control-drawer-hidden-p
						  (ende-test--chat-control-drawers)))
				(should (cl-every #'ende-test--chat-response-has-separator-p
						  (ende-test--chat-response-headlines))))))))

(ert-deftest ende-test/chat-preserves-point-through-async-lifecycle ()
  (dolist (response '("Answer" :error))
    (ende-test--reset)
    (let ((ende-llm-function (ende-test--fake-llm (list response) t)))
      (ende-test--with-buffer
       "* Root\n** Question\nAsk\n** Work\nKeep editing\n"
       (org-mode)
       (ende-test--chat-goto "** Question")
       (let ((question-point (point)))
         (setq ende--queue-suspended t)
         (let ((job (ende-ask)))
           (should (= (point) question-point))
           (setq ende--queue-suspended nil)
           (ende-process-next)
           (goto-char (point-min))
           (search-forward "Keep editing")
           (let ((line (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position)))
                 (column (current-column)))
             (ende-test--complete-call (car ende-test--calls))
             (should (equal line
                            (buffer-substring-no-properties
                             (line-beginning-position)
                             (line-end-position))))
             (should (= column (current-column)))
             (should (eq (ende-job-state job)
                         (if (eq response :error) 'failed 'completed))))))))))

(ert-deftest ende-test/chat-ask-promotes-the-selected-headline-to-user ()
  (dolist (local-role '(nil "assistant"))
    (ende-test--reset)
    (let ((ende-llm-function (ende-test--fake-llm '("Answer"))))
      (ende-test--with-buffer
       (concat "* Root\n"
	       "** Agent: Reply\n"
	       ":PROPERTIES:\n:ENDE_ROLE: assistant\n:END:\n"
	       "Prior assistant answer.\n"
	       "*** Follow-up\n"
	       (if local-role
                   (format ":PROPERTIES:\n:ENDE_ROLE: %s\n:END:\n"
                           local-role)
                 "")
	       "Question from this node.\n")
       (org-mode)
       (ende-test--chat-goto "*** Follow-up")
       (let* ((job (ende-ask))
              (document (ende-chat--build-document))
              (question
	       (ende-chat--find-by-property
                document ende-chat-id-property
                (plist-get (ende-job-instruction job) :question-id)))
              (parent (ende-chat--parent document question))
              (messages (plist-get (car ende-test--calls) :messages))
              (assistant-text
	       (mapconcat
                (lambda (message)
                  (if (equal (car message) "assistant")
                      (cdr message)
                    ""))
                messages "\n"))
              (final-message (car (last messages))))
         (should (eq (ende-job-state job) 'completed))
         (should (equal (ende-chat--property document question :role)
                        "user"))
         (should (equal (ende-chat--property document parent :role)
                        "assistant"))
         (should-not (ende-chat--property document question :visibility))
         (should (string-match-p "Prior assistant answer" assistant-text))
         (should-not (string-match-p "Question from this node" assistant-text))
         (should (equal (car final-message) "user"))
         (should (string-match-p "Question from this node"
                                 (cdr final-message))))))))

(ert-deftest ende-test/selector-ask-includes-only-reasoning-ancestors ()
  (dolist (policy '(root-prefix current-section-only))
    (ende-test--reset)
    (let ((ende-chat-context-policy policy)
          (ende-chat-include-reasoning nil)
          (ende-provider-profiles '((remote :model "configured-model")))
          (ende-llm-function (ende-test--fake-llm '("Answer"))))
      (ende-test--with-buffer
       (concat "* Root\n"
	       "** Reasoning cousin\n"
	       ":PROPERTIES:\n"
	       ":ENDE_ROLE: assistant\n"
	       ":ENDE_VISIBILITY: reasoning\n"
	       ":END:\nCousin reasoning text.\n"
	       "** Agent: Reasoning\n"
	       ":PROPERTIES:\n"
	       ":ENDE_ROLE: assistant\n"
	       ":ENDE_VISIBILITY: reasoning\n"
	       ":END:\nOuter reasoning context.\n"
	       "*** Reasoning aunt\n"
	       ":PROPERTIES:\n:ENDE_VISIBILITY: reasoning\n:END:\n"
	       "Aunt reasoning text.\n"
	       "*** Nested reasoning\n"
	       "Nested reasoning context.\n"
	       "**** Reasoning sibling\n"
	       ":PROPERTIES:\n:ENDE_VISIBILITY: reasoning\n:END:\n"
	       "Sibling reasoning text.\n"
	       "**** Follow-up\nQuestion from reasoning.\n")
       (org-mode)
       (ende-test--chat-goto "**** Follow-up")
       (let ((source (current-buffer)))
         (ende-test--selector-ask-from-buffer source))
       (let* ((call (car ende-test--calls))
              (messages (plist-get call :messages))
              (assistant-text
	       (mapconcat
                (lambda (message)
                  (if (equal (car message) "assistant")
                      (cdr message)
                    ""))
                messages "\n"))
              (final-message (car (last messages)))
              (document (ende-chat--build-document)))
         (should (equal (plist-get (plist-get call :options) :model)
                        "selected-model"))
         (should (string-match-p "Answer" (buffer-string)))
         (should (string-match-p "Outer reasoning context" assistant-text))
         (should (string-match-p "Nested reasoning context" assistant-text))
         (dolist (excluded '("Cousin reasoning text"
                             "Aunt reasoning text"
                             "Sibling reasoning text"))
           (should-not (string-match-p (regexp-quote excluded)
				       assistant-text)))
         (should (equal (car final-message) "user"))
         (should (string-match-p "Question from reasoning"
                                 (cdr final-message)))
         (let* ((question (ende-chat--headline-at-point document))
                (parent (ende-chat--parent document question)))
           (should (equal (ende-chat--property document question :role)
                          "user"))
           (should-not (ende-chat--property document question :visibility))
           (should (eq (ende-chat--effective-visibility document parent)
		       'reasoning))))))))

(ert-deftest ende-test/selector-ask-rejects-hidden-ancestors ()
  (ende-test--reset)
  (let ((ende-provider-profiles '((remote :model "configured-model")))
        (ende-llm-function (ende-test--fake-llm '("Must not run"))))
    (ende-test--with-buffer
     (concat "* Root\n"
	     "** Hidden assistant context\n"
	     ":PROPERTIES:\n"
	     ":ENDE_ROLE: assistant\n"
	     ":ENDE_VISIBILITY: hidden\n"
	     ":END:\n"
	     "*** Follow-up\nQuestion inside hidden context.\n")
     (org-mode)
     (ende-test--chat-goto "*** Follow-up")
     (let ((source (current-buffer)))
       (should-error (ende-test--selector-ask-from-buffer source)
                     :type 'user-error))
     (ende-test--chat-goto "*** Follow-up")
     (should-not (org-entry-get nil ende-chat-role-property nil))
     (should-not (org-entry-get nil ende-chat-id-property nil))
     (should-not ende-test--calls))))

(ert-deftest ende-test/chat-ask-rejects-hidden-nodes-and-reasoning-roots ()
  (dolist (property
           '(":ENDE_VISIBILITY: hidden"
             ":ENDE_VISIBILITY: reasoning"))
    (ende-test--reset)
    (ende-test--with-buffer
     (format "* Root\n** Question\n:PROPERTIES:\n%s\n:END:\nAsk\n"
	     property)
     (org-mode)
     (ende-test--chat-goto "** Question")
     (should-error (ende-ask) :type 'user-error))))

(ert-deftest ende-test/selector-chat-actions-use-selected-profile-in-org-buffer ()
  (dolist (key '("a" "k"))
    (ende-test--reset)
    (let ((ende-selector--last-selection nil)
          (ende-provider-profiles '((remote :model "configured-model")))
          (ende-llm-function (ende-test--fake-llm '("Reply"))))
      (ende-test--with-buffer "* Root\nEarlier context\n** Question\nAsk\n"
			      (org-mode)
			      (ende-test--chat-goto "** Question")
			      (let ((source (current-buffer)))
				(with-temp-buffer
				  (ende-selector-mode)
				  (setq ende-selector--source-buffer source
					ende-selector--providers '(remote)
					ende-selector--provider-index 0
					ende-selector--models '("selected-model")
					ende-selector--model-index 0
					ende-selector--model-state :ready
					ende-selector--effort nil)
				  (cl-letf (((symbol-function 'ende-selector-close) #'ignore))
				    (call-interactively (lookup-key ende-selector-mode-map (kbd key)))))
				(let ((profile (plist-get (car ende-test--calls) :options)))
				  (should (eq (plist-get profile :name) 'remote))
				  (should (equal (plist-get profile :model) "selected-model"))
				  (should (string-match-p "Reply" (buffer-string)))))))))

(ert-deftest ende-test/selector-ask-prepares-chat-from-non-org-buffer ()
  (dolist (initial '("" "* Existing\nPrevious material\n"))
    (ende-test--reset)
    (let ((ende-selector--last-selection nil)
          (ende-llm-function (ende-test--fake-llm '("Reply"))))
      (ende-test--with-buffer "source text"
			      (let ((source (current-buffer))
				    (shown nil)
				    (chat (ende-chat--ensure-buffer)))
				(with-current-buffer chat
				  (insert initial))
				(with-temp-buffer
				  (ende-selector-mode)
				  (setq ende-selector--source-buffer source
					ende-selector--providers '(local)
					ende-selector--provider-index 0
					ende-selector--models '("local-model")
					ende-selector--model-index 0
					ende-selector--model-state :ready
					ende-selector--effort nil)
				  (cl-letf (((symbol-function 'ende-selector-close) #'ignore)
					    ((symbol-function 'pop-to-buffer)
					     (lambda (buffer) (setq shown buffer))))
				    (ende-selector-ask)
				    (with-current-buffer chat
				      (goto-char (point-max))
				      (insert "Question\n"))
				    (setq ende-selector--source-buffer chat)
				    (ende-selector-ask)))
				(should (eq shown chat))
				(with-current-buffer chat
				  (should (derived-mode-p 'org-mode))
				  (should (bound-and-true-p ende-mode))
				  (should (= (how-many "^\\* *$" (point-min) (point-max)) 1))
				  (should (string-match-p "Reply" (buffer-string)))
				  (should (string-match-p "^\\*\\* Agent: Reply$"
							  (buffer-string)))
				  (should (string-match-p ":ENDE_ROLE: assistant"
							  (buffer-string)))
				  (when (not (string-empty-p initial))
				    (should (string-match-p "Previous material"
							    (buffer-string))))))))))

(provide 'ende-test)

;;; ende-test.el ends here
