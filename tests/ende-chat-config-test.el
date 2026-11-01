;;; ende-chat-config-test.el --- Chat configuration tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Coverage for configuration parsing, inheritance, and permissions.

;;; Code:

(require 'ert)
(require 'ende)
(require 'ende-test-helpers)
(require 'cl-lib)
(require 'ende-chat-test-helpers)

(ert-deftest ende-test/agent-config-rejects-duplicate-same-block-forms ()
  (dolist (forms '(("(agent-config worker (clear-tools))"
                   "(agent-config worker (clear-tools))")
                  ("(spawn-agent-config worker)"
                   "(spawn-agent-config worker)")
                  ("(agent-config worker (restrict-host-commands \"git\")"
                   "(restrict-host-commands :unrestricted))")))
    (with-temp-buffer
      (insert "#+begin_ende\n"
              (car forms) "\n"
              (cadr forms) "\n"
              "#+end_ende\n* Topic\n** Question\n")
      (org-mode)
      (let* ((document (ende-chat--build-document))
             (question (car (last (ende-chat--document-headlines document)))))
        (should-error
         (ende-chat-config-resolve-environment
          (ende-chat--document-tree document)
          (ende-chat--document-parent document) question)
         :type 'error)))))



(ert-deftest ende-test/agent-config-registry-is-isolated-and-symbolic ()
  (let ((root-a (make-temp-file "ende-agent-a-" t))
        (root-b (make-temp-file "ende-agent-b-" t)))
    (unwind-protect
        (with-temp-buffer
          (insert
           (format
            (concat "#+begin_ende\n(root %S)\n"
                    "(tools project_info)\n"
                    "(agent-config worker (root %S) (clear-tools))\n"
                    "(agent-config A (spawn-agent-config B))\n"
                    "(agent-config B (spawn-agent-config A))\n"
                    "(spawn-agent-config worker)\n"
                    "#+end_ende\n* Topic\n** Question\n")
            root-a root-b))
          (org-mode)
          (let* ((document (ende-chat--build-document))
                 (question (car (last (ende-chat--document-headlines document))))
                 (environment
                  (ende-chat-config-resolve-environment
                   (ende-chat--document-tree document)
                   (ende-chat--document-parent document) question))
                 (registry (plist-get environment :registry))
                 (worker (cdr (assq 'worker registry))))
            (should (equal (plist-get (plist-get environment :access-policy) :root)
                           (file-name-as-directory
                            (ende-project-normalize-root root-a))))
            (should (equal (plist-get worker :root)
                           (file-name-as-directory
                            (ende-project-normalize-root root-b))))
            (should (equal (plist-get worker :active-tools) '(about_thinking)))
            (should (eq (plist-get (cdr (assq 'A registry)) :spawn-agent-config)
                        'B))
            (should (eq (plist-get (cdr (assq 'B registry)) :spawn-agent-config)
                        'A))
            (let ((presentation
                   (ende-chat-config-capabilities environment)))
              (should (string-match-p "Direct capabilities:" presentation))
              (should (string-match-p
                       "Capabilities available through configured delegation:"
                       presentation))
              (should (string-match-p "agent-config A:" presentation))
              (should (string-match-p "agent-config B:" presentation))
              (should (string-match-p "spawn-agent-config: A" presentation))
              (should (string-match-p "spawn-agent-config: B" presentation)))))
      (delete-directory root-a t)
      (delete-directory root-b t))))

(ert-deftest ende-test/agent-config-shares-definitions-not-authority ()
  (let ((root (make-temp-file "ende-agent-definitions-" t))
        (ende-providers '(local remote))
        (ende-custom-providers ende-test--custom-providers))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert
           (ende-test--chat-config-block
            `((root ,root)
              (podman-image ,(concat "fixture@sha256:" (make-string 64 ?a)))
              (provider remote)
              (model "parent-model")
              (group inspect web_fetch)
              (resource docs :type url (include "https://docs.example.com/**"))
              (tools inspect project_info run_host_command spawn_agent)
              (set-tool-resources inspect :resources docs)
              (restrict-host-commands "git")
              (agent-config worker
                            (tools inspect)
                            (set-tool-resources inspect :resources docs))
              (agent-config idle)
              (agent-config prepared (restrict-host-commands "emacs"))
              (agent-config host
                            (root ,root)
                            (restrict-host-commands "emacs")
                            (tools run_host_command))
              (spawn-agent-config worker)))
           "* Topic\n** Question\n")
          (let* ((document (ende-chat--build-document))
                 (question (car (last (ende-chat--document-headlines document))))
                 (environment
                  (ende-chat-config-resolve-environment
                   (ende-chat--document-tree document)
                   (ende-chat--document-parent document) question))
                 (parent (plist-get environment :access-policy))
                 (registry (plist-get environment :registry))
                 (worker (cdr (assq 'worker registry)))
                 (idle (cdr (assq 'idle registry))))
            (dolist (name '(prepared host))
              (let ((policy (cdr (assq name registry))))
                (should (equal (plist-get policy :host-command-policy)
                               '(:restricted t :open ("emacs") :fixed nil)))
                (should (eq (not (null (memq 'run_host_command
                                            (plist-get policy :active-tools))))
                            (eq name 'host)))))
            (should (equal (plist-get parent :root)
                           (ende-project-normalize-root root)))
            (should (equal (plist-get parent :tool-resources)
                           '((web_fetch docs))))
            (should-not (cl-set-exclusive-or (plist-get worker :active-tools)
                                             '(web_fetch about_thinking)))
            (should (equal (plist-get worker :tool-resources)
                           '((web_fetch docs))))
            (should (equal (plist-get idle :active-tools) '(about_thinking)))
            (should-not (plist-get idle :tool-resources))
            (dolist (policy (list worker idle))
              (should policy)
              (should (equal (cdr (assq 'inspect (plist-get policy :groups)))
                             '(web_fetch)))
              (should (equal (plist-get
                              (cdr (assq 'docs (plist-get policy :resources)))
                              :include)
                             '("https://docs.example.com/**")))
              (dolist (field '(:root :podman-image :provider :model
                                    :spawn-agent-config))
                (should-not (plist-get policy field)))
              (should (eq (plist-get policy :host-command-policy) :inactive)))))
      (delete-directory root t))))

(ert-deftest ende-test/agent-config-local-definitions-override-visible-scope ()
  (let ((root (make-temp-file "ende-agent-overrides-" t)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert
           (ende-test--chat-config-block
            `((root ,root)
              (group inspect project_info)
              (resource source (include "doc/**"))
              (agent-config worker
                            (root ,root)
                            (tools inspect)
                            (set-tool-resources inspect :resources source)
                            (group inspect read_file)
                            (resource source (include "src/**")))
              (agent-config observer
                            (root ,root)
                            (tools inspect)
                            (set-tool-resources inspect :resources source))))
           "* Earlier sibling\n"
           (ende-test--chat-config-block
            '((group inspect edit_file)
              (resource source (include "private/**"))
              (group hidden edit_file)
              (resource hidden (include "secret/**"))))
           "* Topic\n"
           (ende-test--chat-config-block
            '((group inspect file_info read_file)
              (resource source (include "**/*.el"))))
           "** Question\n"
           (ende-test--chat-config-block
            '((resource source (include "lib/**")))))
          (let* ((document (ende-chat--build-document))
                 (question (car (last (ende-chat--document-headlines document))))
                 (environment
                  (ende-chat-config-resolve-environment
                   (ende-chat--document-tree document)
                   (ende-chat--document-parent document) question))
                 (parent (plist-get environment :access-policy))
                 (registry (plist-get environment :registry))
                 (worker (cdr (assq 'worker registry)))
                 (observer (cdr (assq 'observer registry))))
            (should-not (cl-set-exclusive-or (plist-get worker :active-tools)
                                             '(read_file about_thinking)))
            (should (equal (cdr (assq 'inspect (plist-get worker :groups)))
                           '(read_file)))
            (should (equal (plist-get worker :tool-resources)
                           '((read_file source))))
            (should (equal (plist-get
                            (cdr (assq 'source (plist-get worker :resources)))
                            :include)
                           '("src/**")))
            (should-not (cl-set-exclusive-or
                         (plist-get observer :active-tools)
                         '(file_info read_file about_thinking)))
            (should (= (length (plist-get observer :tool-resources)) 2))
            (dolist (tool '(file_info read_file))
              (should (equal (cdr (assq tool (plist-get observer :tool-resources)))
                             '(source))))
            (should-not (plist-get parent :tool-resources))
            (dolist (policy (list parent observer))
              (should-not (cl-set-exclusive-or
                           (cdr (assq 'inspect (plist-get policy :groups)))
                           '(file_info read_file)))
              (should (equal (plist-get
                              (cdr (assq 'source (plist-get policy :resources)))
                              :include)
                             '("lib/**"))))
            (dolist (policy (list parent worker observer))
              (should-not (assq 'hidden (plist-get policy :groups)))
              (should-not (assq 'hidden (plist-get policy :resources))))))
      (delete-directory root t))))

(ert-deftest ende-test/agent-config-freezes-provider-and-model-selection ()
  (let ((ende-providers '(local remote))
        (ende-custom-providers ende-test--custom-providers))
    (with-temp-buffer
      (insert (concat "#+begin_ende\n"
                      "(agent-config worker (provider remote) (model \"child-model\"))\n"
                      "(spawn-agent-config worker)\n"
                      "#+end_ende\n* Topic\n** Question\n"))
      (org-mode)
      (let* ((document (ende-chat--build-document))
             (question (car (last (ende-chat--document-headlines document))))
             (environment
              (ende-chat-config-resolve-environment
               (ende-chat--document-tree document)
               (ende-chat--document-parent document) question))
             (worker (cdr (assq 'worker (plist-get environment :registry))))
             (presentation (ende-chat-config-capabilities environment)))
        (should (eq (plist-get worker :provider) 'remote))
        (should (equal (plist-get worker :model) "child-model"))
        (should (string-match-p "worker provider: remote" presentation))
        (should (string-match-p "worker model: child-model" presentation))))))

(ert-deftest ende-test/agent-config-nearer-definition-replaces-and-sibling-is-hidden ()
  (let ((root-a (make-temp-file "ende-agent-a-" t))
        (root-b (make-temp-file "ende-agent-b-" t)))
    (unwind-protect
        (with-temp-buffer
          (insert
           (format
            (concat "#+begin_ende\n(agent-config worker (root %S))\n"
                    "#+end_ende\n* Earlier sibling\n"
                    "#+begin_ende\n(agent-config secret (root %S))\n"
                    "#+end_ende\n* Current\n"
                    "#+begin_ende\n(spawn-agent-config worker)\n"
                    "#+end_ende\n** Question\n"
                    "#+begin_ende\n(agent-config worker (root %S))\n"
                    "#+end_ende\n*** Child\n")
            root-a root-b root-b))
          (org-mode)
          (let* ((document (ende-chat--build-document))
                 (child (car (last (ende-chat--document-headlines document))))
                 (environment
                  (ende-chat-config-resolve-environment
                   (ende-chat--document-tree document)
                   (ende-chat--document-parent document) child))
                 (registry (plist-get environment :registry)))
            (should (equal (plist-get (cdr (assq 'worker registry)) :root)
                           (file-name-as-directory
                            (ende-project-normalize-root root-b))))
            (should-not (assq 'secret registry))))
      (delete-directory root-a t)
      (delete-directory root-b t))))

(ert-deftest ende-test/agent-config-validation-is-eager-and-isolated ()
  (let ((root (make-temp-file "ende-agent-validation-" t)))
    (unwind-protect
        (with-temp-buffer
          (insert
           (format (concat "#+begin_ende\n(root %S)\n"
                           "(resource project (include \"**\"))\n"
                           "(tools read_file)\n"
                           "(set-tool-resources read_file :resources project)\n"
                           "(agent-config unused (tools read_file))\n"
                           "#+end_ende\n* Topic\n** Question\n")
                   root))
          (org-mode)
          (let* ((document (ende-chat--build-document))
                 (question (car (last (ende-chat--document-headlines document)))))
            (should-error
             (ende-chat-config-resolve-environment
              (ende-chat--document-tree document)
              (ende-chat--document-parent document) question)
             :type 'error)))
      (delete-directory root t))))

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

(ert-deftest ende-test/chat-config-inspector-displays-resolved-policy ()
  (let ((root (make-temp-file "ende-config-inspector-" t)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert
           (ende-test--chat-config-block
            `((root ,root)
              (group development run_host_command)
              (tools development)
              (restrict-host-commands "git" (:fixed-args "make" "test"))))
           "* Conversation\n** Inspect me\n")
          (goto-char (point-max))
          (let* ((document (ende-chat--build-document))
                 (headline (ende-chat--headline-at-point document))
                 (expected
                  (ende-chat-config-resolve
                   (ende-chat--document-tree document)
                   (ende-chat--document-parent document) headline)))
            (ende-inspect-config-at-point)
            (with-current-buffer "*ende config*"
              (should (derived-mode-p 'special-mode))
              (should buffer-read-only)
              (goto-char (point-min))
              (should (equal (read (current-buffer)) expected))))
          (when (get-buffer "*ende config*")
            (kill-buffer "*ende config*")))
      (delete-directory root t))))

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

(ert-deftest ende-test/chat-config-optional-resource-requirements ()
  (let ((ende-tools--registry (copy-sequence ende-tools--registry))
        (root (make-temp-file "ende-config-optional-resource-" t))
        (tool-name 'ende_test_optional_file_tool))
    (unwind-protect
        (progn
          (ende-tool-register
           tool-name "Optional file-resource test tool"
           ende-tools--empty-object-schema
           (lambda (_arguments _root) '(:ok t)) nil nil
           '(:resource-kind file :requires-resources nil))
          (should-not
           (ende-tool-requires-resources (ende-tool--find tool-name)))
          (should (ende-tool-requires-resources (ende-tool--find 'read_file)))
          (should (ende-tool-requires-resources (ende-tool--find 'web_fetch)))
          (should-not (ende-tool-requires-resources
                       (ende-tool--find 'run_host_command)))
          (should-error
           (ende-tool-register
            'ende_test_invalid_resource_requirement
            "Invalid resource requirement" ende-tools--empty-object-schema
            (lambda (_arguments _root) '(:ok t)) nil nil
            '(:resource-kind none :requires-resources t)))
          (dolist (assignment
                   '(nil
                     ((set-tool-resources ende_test_optional_file_tool
                                          :resources project))))
            (with-temp-buffer
              (org-mode)
              (insert
               (ende-test--chat-config-block
                (append `((root ,root)
                          (resource project (include "**"))
                          (tools ende_test_optional_file_tool))
                        assignment))
               "* Conversation\n** Tool access\n")
              (let* ((document (ende-chat--build-document))
                     (configuration
                      (ende-test--chat-config-at document "Tool access"))
                     (call (ende-tool-call
                            tool-name nil root configuration)))
                (should (memq tool-name (plist-get configuration :active-tools)))
                (should (plist-get call :ok)))))
          (with-temp-buffer
            (org-mode)
            (insert
             (ende-test--chat-config-block
              `((root ,root)
                (resource docs :type url
                          (include "https://example.com/**"))
                (tools ende_test_optional_file_tool)
                (set-tool-resources ende_test_optional_file_tool
                                    :resources docs)))
             "* Conversation\n** Tool access\n")
            (should-error
             (ende-test--chat-config-at (ende-chat--build-document)
                                        "Tool access")))
          (with-temp-buffer
            (org-mode)
            (insert
             (ende-test--chat-config-block
              `((root ,root)
                (tools read_file)))
             "* Conversation\n** Tool access\n")
            (should-error
             (ende-test--chat-config-at (ende-chat--build-document)
                                        "Tool access"))))
      (delete-directory root t))))


(ert-deftest ende-test/chat-config-file-resources-have-independent-roots ()
  (let* ((project-a (make-temp-file "ende-config-project-a-" t))
         (project-b (make-temp-file "ende-config-project-b-" t))
         (external (make-temp-file "ende-config-external-" t))
         (project-a-file (expand-file-name "source.txt" project-a))
         (project-b-file (expand-file-name "source.txt" project-b))
         (external-file (expand-file-name "allowed/notes.txt" external))
         (missing (expand-file-name "missing" external)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory external-file) t)
          (with-temp-file project-a-file (insert "project A\n"))
          (with-temp-file project-b-file (insert "project B\n"))
          (with-temp-file external-file (insert "external\n"))
          (with-temp-buffer
            (org-mode)
            (insert
             (ende-test--chat-config-block
              `((root ,project-a)
                (resource project (include "**"))
                (resource external :root ,external (include "allowed/**"))
                (tools read_file)
                (set-tool-resources read_file :resources project external)))
             "* Conversation\n** Original root\nRead the original root.\n"
             "** Changed root\n"
             (ende-test--chat-config-block `((root ,project-b)))
             "Read the new root and the fixed external root.\n")
            (let* ((document (ende-chat--build-document))
                   (original (ende-test--chat-config-at document "Original root"))
                   (changed (ende-test--chat-config-at document "Changed root"))
                   (project-resource
                    (cdr (assq 'project (plist-get changed :resources))))
                   (external-resource
                    (cdr (assq 'external (plist-get changed :resources))))
                   (project-read
                    (ende-tool-call 'read_file '(:path "source.txt") nil changed))
                   (external-read
                    (ende-tool-call 'read_file (list :path external-file)
                                    nil changed)))
              (should (equal (plist-get original :root)
                             (ende-project-normalize-root project-a)))
              (should (equal (plist-get changed :root)
                             (ende-project-normalize-root project-b)))
              (should-not (plist-get project-resource :root))
              (should (equal (plist-get external-resource :root)
                             (ende-project-normalize-root external)))
              (should (equal (ende-tools--file-resource-root
                              project-resource (plist-get original :root))
                             (ende-project-normalize-root project-a)))
              (should (equal (ende-tools--file-resource-root
                              project-resource (plist-get changed :root))
                             (ende-project-normalize-root project-b)))
              (should (equal (ende-tools--file-resource-root
                              external-resource (plist-get original :root))
                             (ende-tools--file-resource-root
                              external-resource (plist-get changed :root))))
              (should (plist-get project-read :ok))
              (should (equal (plist-get (plist-get project-read :result) :content)
                             "project B\n"))
              (should (plist-get external-read :ok))
              (should (equal (plist-get (plist-get external-read :result) :path)
                             (file-truename external-file)))))
          (dolist (forms
                   (list
                    `((resource bad :type url :root ,external
                                (include "https://example.com/**")))
                    `((resource bad :root ,missing (include "**")))
                    `((resource bad (include "**") :root ,external))
                    '((resource bad (include "../secret")))
                    '((resource bad (include "/absolute/**")))))
            (with-temp-buffer
              (org-mode)
              (insert (ende-test--chat-config-block forms)
                      "* Conversation\n** Invalid\n")
              (should-error
               (ende-test--chat-config-at (ende-chat--build-document) "Invalid")))))
      (let ((failure
             (with-temp-buffer
               (org-mode)
               (insert
                (ende-test--chat-config-block
                 `((resource bad :type url :root ,external
                             (include "https://example.com/**"))))
                "* Conversation\n** Invalid\n")
               (should-error
                (ende-test--chat-config-at
                 (ende-chat--build-document) "Invalid")))))
        (should (string-match-p
                 ":root is only valid for file resources"
                 (error-message-string failure))))
      (dolist (directory (list project-a project-b external))
        (when (file-directory-p directory)
          (delete-directory directory t))))))

(ert-deftest ende-test/chat-patch-uses-only-apply-patch-grants ()
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
                (resource narrow-delete (include "tmp/direct.txt"))
                (set-tool-resources apply_patch :resources source disposable)))
             "* Conversation\n** Mixed\n"
             (ende-test--chat-config-block '((tools apply_patch)))
             "Apply a fully granted patch.\n"
             "** Hide delete\n"
             (ende-test--chat-config-block
              '((tools apply_patch delete_file)
                (set-tool-resources delete_file :resources narrow-delete)))
             "*** Clear exposure only\n"
             (ende-test--chat-config-block '((clear-tools) (tools apply_patch)))
             "Delete through the patch's retained grant.\n"
             "** Remove delete grant\n"
             (ende-test--chat-config-block
              '((tools apply_patch)
                (clear-tool-resources)
                (set-tool-resources apply_patch :resources source disposable)))
             "A patch grant alone permits patch operations.\n")
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
                               'delete_file '(:path "tmp/hidden.txt") root exposed))
                      (applied (ende-tool-call
                                'apply_patch (list :patch hidden-patch)
                                root hidden)))
                  (should-not (plist-get direct :ok))
                  (should (string-match-p "not authorized"
                                          (plist-get direct :error)))
                  (should (plist-get applied :ok))
                  (should-not (file-exists-p hidden-delete))))
              (let ((delete-only-patch
                     "*** Begin Patch\n*** Delete File: tmp/denied.txt\n*** End Patch"))
                (let ((applied (ende-tool-call
                                'apply_patch (list :patch delete-only-patch)
                                root missing)))
                  (should (plist-get applied :ok))
                  (should-not (file-exists-p denied-delete)))))))
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
                             (ende-test--tool-call 'project_info nil root configuration)
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
                      (let* ((call (ende-test--tool-call 'search_project '(:query "TOKEN")
                                                         root configuration))
                             (files (plist-get (plist-get call :result) :files)))
                        (should (plist-get call :ok))
                        (should (= (length files) 1))
                        (should (equal (plist-get (aref files 0) :path) "public.el"))
                        (should (= (length (plist-get (aref files 0) :matches)) 1))))))))))
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
        (should (equal (substring system 0
                                   (+ (length ende-chat--system)
                                      (length "\n\nDocument-level Ende configuration:\n")
                                      (length configuration)))
                       (concat ende-chat--system
                               "\n\nDocument-level Ende configuration:\n"
                               configuration)))
        (should (string-match-p "Configured capabilities:" system))
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
        (let ((system (cdr (assoc "system" messages))))
          (should (string-prefix-p ende-chat--system system))
          (should (string-match-p "Direct capabilities:" system))
          (should (string-match-p "about_thinking" system)))))))

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
                   (second-messages (plist-get second-call :messages)))
              (should-not (memq 'read_file (plist-get first :active-tools)))
              (should (memq 'read_file (plist-get second :active-tools)))
              (should (equal (cdr (assq 'read_file
                                        (plist-get second :tool-resources)))
                             '(project)))
              (should (equal first-definitions second-definitions))
              (should (equal first-definitions (ende-tool-definitions)))
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
