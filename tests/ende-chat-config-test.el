;;; ende-chat-config-test.el --- Chat configuration tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Coverage for configuration parsing, inheritance, and permissions.

;;; Code:

(require 'ert)
(require 'ende)
(require 'ende-test-helpers)
(require 'cl-lib)
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
					    read_buffer about_thinking)))
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
              (should (equal (plist-get inherited :active-tools)
                             '(project_info about_thinking)))
              (should (equal (plist-get configuration :active-tools)
                             '(about_thinking)))
              (should-not (plist-get configuration :tool-resources)))))
      (delete-directory root t)
      (delete-directory child-root t))))

(ert-deftest ende-test/chat-config-supports-rootless-typed-url-resources ()
  (let ((tool-name 'ende-test-url-policy-probe)
        (ende-test-root (make-temp-file "ende-rootless-url-" t)))
    (unwind-protect
        (progn
          ;; This fixture is deliberately test-only; production web_fetch is
          ;; registered with its implementation in the later transport step.
          (ende-tool-register
           tool-name "Test URL policy access" ende-tools--empty-object-schema
           (lambda (arguments root)
             (list :root root
                   :allowed
                   (ende-tools--url-authorized-p
                    (ende-url-normalize
                     (ende-tools--required-string arguments "url")))))
           nil nil '(:resource-kind url :requires-root nil))
          (with-temp-buffer
            (org-mode)
            (insert
             (ende-test--chat-config-block
              `((resource docs :type url
                          (include "https://*.example.com/**")
                          (exclude "https://private.example.com/**"))
                (resource exception :type url
                          (include "https://private.example.com/**"))
                (tools ,tool-name)
                (set-tool-resources ,tool-name :resources docs exception)))
             "* Conversation\n** Rootless\nFetch only policy-approved URLs.\n"
             "*** Redefined\n"
             (ende-test--chat-config-block
              '((resource docs :type url
                          (include "https://other.example/**"))))
             "The old docs grant is revoked.\n")
            (let* ((document (ende-chat--build-document))
                   (rootless (ende-test--chat-config-at document "Rootless"))
                   (redefined (ende-test--chat-config-at document "Redefined")))
              (should-not (plist-get rootless :root))
              (should (equal (plist-get rootless :active-tools)
                             (list tool-name 'about_thinking)))
              (should (equal (cdr (assq tool-name
                                        (plist-get rootless :tool-resources)))
                             '(docs exception)))
              (should (equal (cdr (assq tool-name
                                        (plist-get redefined :tool-resources)))
                             '(exception)))
              (let ((default-directory ende-test-root))
                (dolist (case '(("https://a.example.com/a" t)
                                ("https://private.example.com/a" t)
                                ("https://example.com/a" nil)
                                ("https://evil-example.com/a" nil)))
                  (let* ((call (ende-tool-call
                                tool-name (list :url (car case)) nil rootless))
                         (result (plist-get call :result)))
                    (should (plist-get call :ok))
                    (should (eq (plist-get result :allowed) (cadr case)))
                    (should-not (plist-get result :root))))
                (should-not
                 (plist-get
                  (plist-get
                   (ende-tool-call
                    tool-name '(:url "https://a.example.com/")
                    nil redefined)
                   :result)
                  :allowed))
                (should
                 (plist-get
                  (plist-get
                   (ende-tool-call
                    tool-name '(:url "https://private.example.com/")
                    nil redefined)
                   :result)
                  :allowed)))))
          (with-temp-buffer
            (org-mode)
            (insert
             (ende-test--chat-config-block
              '((resource docs :type url (include "https://example.com/**"))
                (group files read_file)
                (tools files)
                (set-tool-resources files :resources docs)))
             "* Conversation\n** Bad\n")
            (should-error
             (ende-test--chat-config-at (ende-chat--build-document) "Bad")))
          (with-temp-buffer
            (org-mode)
            (insert
             (ende-test--chat-config-block
              `((resource files (include "**"))
                (tools ,tool-name)
                (set-tool-resources ,tool-name :resources files)))
             "* Conversation\n** Bad\n")
            (should-error
             (ende-test--chat-config-at (ende-chat--build-document) "Bad")))
          (with-temp-buffer
            (org-mode)
            (insert
             (ende-test--chat-config-block
              `((resource docs :type url :allow-private-addresses t
                          (include "https://*.example.com/**"))))
             "* Conversation\n** Bad\n")
            (should-error
             (ende-test--chat-config-at (ende-chat--build-document) "Bad"))))
      (setq ende-tools--registry
            (assq-delete-all tool-name ende-tools--registry))
      (when (file-directory-p ende-test-root)
        (delete-directory ende-test-root t)))))

(ert-deftest ende-test/chat-patch-needs-patch-and-operation-grants ()
  (let* ((root (make-temp-file "ende-patch-policy-" t))
         (source (expand-file-name "src/main.txt" root))
         (added (expand-file-name "src/new.txt" root))
         (mixed-delete (expand-file-name "tmp/mixed.txt" root))
         (hidden-delete (expand-file-name "tmp/hidden.txt" root))
         (denied-delete (expand-file-name "tmp/denied.txt" root)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory source) t)
          (make-directory (file-name-directory mixed-delete) t)
          (with-temp-file source (insert "before\nold\n"))
          (dolist (path (list mixed-delete hidden-delete denied-delete))
            (with-temp-file path (insert "remove me\n")))
          (with-temp-buffer
            (org-mode)
            (insert
             (ende-test--chat-config-block
              `((root ,root)
                (resource source (include "src/**"))
                (resource disposable (include "tmp/**"))
                (set-tool-resources apply_patch :resources source disposable)
                (set-tool-resources edit_file create_file :resources source)
                (set-tool-resources delete_file :resources disposable)))
             "* Conversation\n** Mixed\n"
             (ende-test--chat-config-block '((tools apply_patch)))
             "Apply a fully granted patch.\n"
             "** Hide delete\n"
             (ende-test--chat-config-block '((tools apply_patch delete_file)))
             "*** Clear exposure only\n"
             (ende-test--chat-config-block '((clear-tools) (tools apply_patch)))
             "Delete through the patch's retained grant.\n"
             "** Remove delete grant\n"
             (ende-test--chat-config-block
              '((tools apply_patch)
                (clear-tool-resources)
                (set-tool-resources apply_patch :resources source disposable)
                (set-tool-resources edit_file create_file :resources source)))
             "A patch grant alone cannot delete.\n")
            (let* ((document (ende-chat--build-document))
                   (mixed (ende-test--chat-config-at document "Mixed"))
                   (exposed (ende-test--chat-config-at document "Hide delete"))
                   (hidden (ende-test--chat-config-at
                            document "Clear exposure only"))
                   (missing (ende-test--chat-config-at
                             document "Remove delete grant"))
                   (patch
                    (concat "*** Begin Patch\n"
                            "*** Update File: src/main.txt\n@@\n before\n-old\n+new\n"
                            "*** Add File: src/new.txt\n+created\n"
                            "*** Delete File: tmp/mixed.txt\n"
                            "*** End Patch\n")))
              (should (equal (plist-get mixed :active-tools)
                             '(apply_patch about_thinking)))
              (should (equal (plist-get exposed :active-tools)
                             '(apply_patch delete_file about_thinking)))
              (should (equal (plist-get hidden :active-tools)
                             '(apply_patch about_thinking)))
              (let ((result (ende-tool-call
                             'apply_patch (list :patch patch) root mixed)))
                (should (plist-get result :ok))
                (should (file-exists-p added))
                (should-not (file-exists-p mixed-delete)))
              (let ((hidden-patch
                     "*** Begin Patch\n*** Delete File: tmp/hidden.txt\n*** End Patch"))
                (let ((direct (ende-tool-call
                               'delete_file '(:path "tmp/hidden.txt") root hidden))
                      (applied (ende-tool-call
                                'apply_patch (list :patch hidden-patch)
                                root hidden)))
                  (should-not (plist-get direct :ok))
                  (should (string-match-p "not active" (plist-get direct :error)))
                  (should (plist-get applied :ok))
                  (should-not (file-exists-p hidden-delete))))
              (let ((delete-only-patch
                     "*** Begin Patch\n*** Delete File: tmp/denied.txt\n*** End Patch"))
                (let ((denied (ende-tool-call
                               'apply_patch (list :patch delete-only-patch)
                               root missing)))
                  (should-not (plist-get denied :ok))
                  (should (string-match-p "delete_file"
                                          (plist-get denied :error)))
                  (should (file-exists-p denied-delete)))))))
      (when (file-directory-p root)
        (delete-directory root t)))))



(ert-deftest ende-test/chat-config-definitions-can-remain-unused ()
  (let* ((root (make-temp-file "ende-config-library-" t))
         (cases '(("** No tools" nil)
                  ("** Inspect" (project_info list_project_files read_file))
                  ("*** Clear tools" nil)
                  ("*** Redefine active" nil)
                  ("*** Redefine unused" (project_info list_project_files read_file))
                  ("** Direct tools" (read_file))
                  ("** Direct grants" (read_file))
                  ("** Information" (project_info))
                  ("** Redefine only" nil)))
         (catalog (ende-tool-definitions))
         (catalog-names (ende-test--tool-definition-names catalog))
         (previous-definitions nil))
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
                  (should (equal (plist-get configuration :active-tools)
                                 (append (cadr case) '(about_thinking))))
                  (should (assq 'modify (plist-get configuration :groups)))
                  (should (assq 'unused (plist-get configuration :resources)))
                  (when (equal (car case) "** No tools")
                    (let ((denied
                           (ende-tool-call 'read_file nil root configuration)))
                      (should-not (plist-get denied :ok))
                      (should (string-match-p "not active"
                                              (plist-get denied :error)))))
                  (should (eq (ende-job-state (ende-ask)) 'completed))
                  (let* ((options (plist-get (car (last ende-test--calls)) :options))
                         (definitions (plist-get options :tool-definitions))
                         (names (ende-test--tool-definition-names definitions)))
                    (should (equal (sort (copy-sequence names) #'string<)
                                   (sort (copy-sequence catalog-names) #'string<)))
                    (should (equal definitions catalog))
                    (when previous-definitions
                      (should (equal definitions previous-definitions)))
                    (setq previous-definitions (copy-tree definitions))))))))
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
                    (should (equal (plist-get configuration :active-tools)
                                   (append active '(about_thinking))))
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
                                         project_info read_buffer about_thinking)))
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
                               '(read_file file_info project_info about_thinking)))
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

(ert-deftest ende-test/chat-system-prompt-preserves-document-config-source ()
  (ende-test--reset)
  (let* ((configuration
          (concat "#+begin_ende\n"
                  "; keep this comment and spacing\n"
                  "(tools   about_thinking) ; preserve this syntax\n"
                  "#+end_ende\n"))
         (ende-llm-function (ende-test--fake-llm '("Done."))))
    (with-temp-buffer
      (org-mode)
      (insert configuration "\n* Conversation\n** Question\nAsk.\n")
      (ende-test--chat-goto "** Question")
      (let* ((job (ende-ask))
             (messages (plist-get (car ende-test--calls) :messages))
             (system (cdr (assoc "system" messages)))
             (first-user
              (cl-find-if (lambda (message) (equal (car message) "user"))
                          messages)))
        (should (eq (ende-job-state job) 'completed))
        (should (string-prefix-p ende-chat--system system))
        (should (equal system
                       (concat ende-chat--system
                               "\n\nDocument-level Ende configuration:\n"
                               configuration)))
        (should (string-match-p (regexp-quote "#+begin_ende") system))
        (should (string-match-p (regexp-quote "#+end_ende") system))
        (should (= (ende-test--count-matches
                    (regexp-quote configuration) system)
                   1))
        (should first-user)
        (should-not (string-match-p (regexp-quote configuration)
                                    (cdr first-user)))))))

(ert-deftest ende-test/chat-system-prompt-without-document-config-is-unchanged ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("Done."))))
    (with-temp-buffer
      (org-mode)
      (insert "* Conversation\n** Question\nAsk.\n")
      (ende-test--chat-goto "** Question")
      (let* ((job (ende-ask))
             (messages (plist-get (car ende-test--calls) :messages)))
        (should (eq (ende-job-state job) 'completed))
        (should (equal (cdr (assoc "system" messages)) ende-chat--system))))))

(ert-deftest ende-test/chat-sibling-configuration-remains-history-only ()
  (let ((root (make-temp-file "ende-config-sibling-" t))
        (clear-block "#+begin_ende\n(clear-tools)\n#+end_ende\n"))
    (ende-test--reset)
    (unwind-protect
        (let ((ende-llm-function
               (ende-test--fake-llm '("First response." "Second response."))))
          (with-temp-buffer
            (org-mode)
            (insert
             (ende-test--chat-config-block
              `((root ,root)
                (resource project (include "*.el"))
                (tools read_file)
                (set-tool-resources read_file :resources project)))
             "\n* Conversation\n** First\n"
             clear-block
             "Temporary access change.\n** Second\nUse the document permissions.\n")
            (ende-test--chat-goto "** First")
            (should (eq (ende-job-state (ende-ask)) 'completed))
            (ende-test--chat-goto "** Second")
            (should (eq (ende-job-state (ende-ask)) 'completed))
            (let* ((document (ende-chat--build-document))
                   (first (ende-test--chat-config-at document "First"))
                   (second (ende-test--chat-config-at document "Second"))
                   (first-call (car ende-test--calls))
                   (second-call (cadr ende-test--calls))
                   (first-definitions
                    (plist-get (plist-get first-call :options) :tool-definitions))
                   (second-definitions
                    (plist-get (plist-get second-call :options) :tool-definitions))
                   (second-messages (plist-get second-call :messages))
                   (system (cdr (assoc "system" second-messages))))
              (should-not (memq 'read_file (plist-get first :active-tools)))
              (should (memq 'read_file (plist-get second :active-tools)))
              (should (equal (cdr (assq 'read_file
                                        (plist-get second :tool-resources)))
                             '(project)))
              (should (equal first-definitions second-definitions))
              (should (equal first-definitions (ende-tool-definitions)))
              (should (string-match-p
                       (regexp-quote "Same-level headings are siblings, not ancestors.")
                       system))
              (should
               (cl-some
                (lambda (message)
                  (and (equal (car message) "user")
                       (string-match-p (regexp-quote clear-block)
                                       (cdr message))))
                second-messages)))))
      (ende-test--reset)
      (delete-directory root t))))

(ert-deftest ende-test/chat-compaction-keeps-its-stable-system-prompt ()
  (ende-test--reset)
  (let* ((configuration
          (ende-test--chat-config-block '((tools about_thinking))))
         (ende-llm-function (ende-test--fake-llm '("Summary."))))
    (with-temp-buffer
      (org-mode)
      (insert configuration "\n* Conversation\n** Earlier\nFacts.\n"
              "** Summary\nKeep the key facts.\n")
      (ende-test--chat-goto "** Summary")
      (let* ((job (ende-compact))
             (messages (plist-get (car ende-test--calls) :messages))
             (system (cdr (assoc "system" messages))))
        (should (eq (ende-job-state job) 'completed))
        (should (equal system ende-chat--compact-system))
        (should-not (string-match-p "Document-level Ende configuration:" system))
        (should-not (string-match-p (regexp-quote configuration) system))))))

(provide 'ende-chat-config-test)

;;; ende-chat-config-test.el ends here
