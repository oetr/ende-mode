;;; ende-tools-test.el --- Project-tool tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Coverage for project files, buffers, and scope enforcement.

;;; Code:

(require 'ert)
(require 'ende)
(require 'ende-test-helpers)
(require 'cl-lib)
(require 'json)
(require 'ende-chat-test-helpers)

(defun ende-test--tool-result (name arguments root)
  "Invoke tool NAME and return its successful result in ROOT."
  (let ((call (ende-tool-call name arguments root)))
    (should (plist-get call :ok))
    (plist-get call :result)))

(defun ende-test--project-tool-json-records
    (name arguments field _root configuration)
  "Call project tool NAME and return JSON-decoded FIELD records."
  (let* ((call (list :id "call-1" :name name
                     :arguments (json-encode arguments)))
         (response (ende-chat--tool-response call configuration))
         (decoded (ende-protocol-json-read-string response)))
    (should (eq (ende-protocol-json-get "ok" decoded) t))
    (ende-protocol-json-get
     field (ende-protocol-json-get "result" decoded))))

(defun ende-test--edit-file-call (path edits root)
  "Call `edit_file' for PATH with anchor EDITS under ROOT."
  (ende-tool-call "edit_file"
                  (list (cons "path" path) (cons "edits" edits))
                  root))

(defun ende-test--create-file-call (path content root)
  "Call `create_file' for PATH with CONTENT under ROOT."
  (ende-tool-call "create_file"
                  (list (cons "path" path) (cons "content" content))
                  root))

(defun ende-test--apply-patch-call (patch root &optional access-config)
  "Call apply_patch with PATCH within ROOT and optional ACCESS-CONFIG."
  (ende-tool-call "apply_patch" (list (cons "patch" patch))
                  root access-config))

(defun ende-test--tool-access-config (root forms)
  "Resolve tool access configuration FORMS with project ROOT."
  (with-temp-buffer
    (org-mode)
    (insert
     (ende-test--chat-config-block (cons `(root ,root) forms))
     "* Conversation\n** Tool access\n")
    (ende-test--chat-config-at (ende-chat--build-document) "Tool access")))

(defun ende-test--host-command-call (arguments configuration)
  "Run host command ARGUMENTS asynchronously and wait for its result."
  (let ((done nil)
        (result nil)
        (deadline (+ (float-time) 15.0)))
    (ende-tool-call-async "run_host_command" arguments nil configuration
                          (lambda (value) (setq result value done t)))
    (while (and (not done) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (unless done
      (error "timed out waiting for run_host_command"))
    result))

(ert-deftest ende-test/tool-registry-rebuild-removes-stale-registrations ()
  (let ((ende-tools--registry (copy-sequence ende-tools--registry))
        (tools-file
         (expand-file-name "ende-tools.el"
                           (file-name-directory (locate-library "ende.el")))))
    (ende-tool-register
     'ende_test_stale_registration "Stale test tool"
     ende-tools--empty-object-schema
     (lambda (_arguments _root) nil) nil :no-resource)
    (should (ende-tool--find 'ende_test_stale_registration))
    (load tools-file)
    (should-not (ende-tool--find 'ende_test_stale_registration))
    (should-not (ende-tool--find 'run_command))
    (should (ende-tool--find 'run_sandbox_command))
    (let ((definitions (ende-tool-definitions))
          (active-names (ende-tool-active-names nil)))
      (should (= (length definitions) (length ende-tools--registry)))
      (should (member 'about_thinking active-names))
      (should-not (memq 'ende_test_stale_registration active-names))
      (should (member "run_sandbox_command"
                      (mapcar (lambda (definition)
                                (alist-get :name
                                           (alist-get :function definition)))
                              definitions))))))

(defun ende-test--public-tool-schema (definitions name)
  "Return the public generated schema for tool NAME from DEFINITIONS."
  (let* ((definition
          (cl-find-if
           (lambda (candidate)
             (equal (alist-get :name (alist-get :function candidate)) name))
           definitions))
         (parameters (alist-get :parameters (alist-get :function definition))))
    (ende-protocol-json-read-string (json-encode parameters))))

(defun ende-test--wait-for-host-command-file (path)
  "Wait for the host command to create PATH."
  (let ((deadline (+ (float-time) 8.0)))
    (while (and (not (file-exists-p path)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (unless (file-exists-p path)
      (error "host command did not create %s" path))))

(defun ende-test--assert-host-process-stopped (pid)
  "Wait until host process PID is gone or only remains as a zombie."
  (let ((deadline (+ (float-time) 8.0))
        attributes)
    (while (and (< (float-time) deadline)
                (setq attributes (process-attributes pid))
                (not (equal (cdr (assq 'state attributes)) "Z")))
      (accept-process-output nil 0.05))
    (setq attributes (process-attributes pid))
    (should (or (null attributes)
                (equal (cdr (assq 'state attributes)) "Z")))))

(ert-deftest ende-test/project-tools-expose-json-definitions ()
  (let ((definitions (ende-tool-definitions)))
    (let ((names (mapcar (lambda (definition)
                           (alist-get :name (alist-get :function definition)))
                         definitions)))
      (should (= (length names) (length (delete-dups (copy-sequence names)))))
      (should-not (member "run_command" names))
      (should (member "run_sandbox_command" names))
      (should-not (ende-tool--find "run_command"))
      (dolist (required '("about_thinking" "project_info" "list_project_files" "file_info"
                          "read_file" "list_project_buffers" "read_buffer"
                          "search_project" "edit_file" "create_file"
                          "delete_file" "apply_patch" "web_fetch"
                          "run_sandbox_command" "run_host_command"
                          "copy_from_sandbox"))
        (should (member required names))))
    (dolist (definition definitions)
      (let* ((function (alist-get :function definition))
             (schema
              (ende-protocol-json-read-string
               (json-encode (alist-get :parameters function)))))
        (should (hash-table-p (gethash "properties" schema)))
        (should (eq (gethash "additionalProperties" schema) :false))))
    (let* ((schema (ende-test--public-tool-schema definitions "search_project"))
           (properties (gethash "properties" schema)))
      (should-not (gethash "required" schema))
      (should (= (gethash "minLength" (gethash "query" properties)) 1))
      (should (= (gethash "minLength" (gethash "glob" properties)) 1))
      (should (= (gethash "minimum" (gethash "context_before" properties)) 0))
      (should (= (gethash "minimum" (gethash "context_after" properties)) 0))
      (should (= ende-tools-max-search-context-lines 20))
      (should (= (gethash "minItems" (gethash "patterns" properties)) 1))
      (should (string-match-p "literal"
                              (gethash "description" (gethash "query" properties))))
      (should (string-match-p "literal"
                              (gethash "description" (gethash "patterns" properties))))
      (should (string-match-p "Emacs regexp"
                              (gethash "description" (gethash "regexp" properties)))))
    (dolist (case `(("list_project_files" "max_results"
                     ,ende-tools-max-listed-files)
                    ("read_file" "line_count" ,ende-tools-max-read-lines)
                    ("read_buffer" "line_count" ,ende-tools-max-read-lines)
                    ("search_project" "context_before"
                     ,ende-tools-max-search-context-lines)
                    ("search_project" "context_after"
                     ,ende-tools-max-search-context-lines)
                    ("search_project" "max_results"
                     ,ende-tools-max-search-results)))
      (let* ((schema (ende-test--public-tool-schema definitions (nth 0 case)))
             (property (gethash (nth 1 case) (gethash "properties" schema))))
        (should (= (gethash "maximum" property) (nth 2 case)))))
    (should-not (ende-tool-read-only (ende-tool--find "edit_file")))
    (should-not (ende-tool-read-only (ende-tool--find "create_file")))
    (should-not (ende-tool-read-only (ende-tool--find "copy_from_sandbox")))
    (should (ende-tool-async (ende-tool--find "run_sandbox_command")))
    (should (eq (ende-tool-resource-kind
                 (ende-tool--find "run_sandbox_command"))
                'file))
    (should-not (ende-tool-requires-resources
                 (ende-tool--find "run_sandbox_command")))
    (should (ende-tool-async (ende-tool--find "run_host_command")))
    (should-not (ende-tool-requires-resources
                 (ende-tool--find "run_host_command")))
    (should (eq (ende-tool-resource-kind
                 (ende-tool--find "run_host_command"))
                'none))
    (let* ((descriptions
            (mapcar (lambda (definition)
                      (cons (alist-get :name (alist-get :function definition))
                            (alist-get :description
                                       (alist-get :function definition))))
                    definitions))
           (run-description (cdr (assoc "run_sandbox_command" descriptions)))
           (host-description (cdr (assoc "run_host_command" descriptions))))
      (dolist (phrase '("private development workspace"
                        "normal tool" "builds" "tests" "linters"
                        "repository inspection" "temporary diagnostics"
                        "dedicated project mutation tools"))
        (should (string-match-p (regexp-quote phrase) run-description)))
      (dolist (phrase '("directly on the host" "bypassing the"
                        "private development workspace"
                        "only when host execution is specifically required"
                        "edit_file" "create_file" "delete_file" "apply_patch"))
        (should (string-match-p (regexp-quote phrase) host-description))))
    (let* ((schema (ende-test--public-tool-schema definitions "file_info"))
           (path (gethash "path" (gethash "properties" schema)))
           (read-file (ende-test--public-tool-schema definitions "read_file"))
           (read-path (gethash "path" (gethash "properties" read-file)))
           (read-buffer (ende-test--public-tool-schema definitions "read_buffer"))
           (buffer-id (gethash "buffer_id" (gethash "properties" read-buffer)))
           (list-files (ende-test--public-tool-schema definitions "list_project_files"))
           (list-glob (gethash "glob" (gethash "properties" list-files))))
      (should (= (gethash "minLength" path) 1))
      (should (= (gethash "minLength" read-path) 1))
      (should (= (gethash "minLength" buffer-id) 1))
      (should (= (gethash "minLength" list-glob) 1)))
    (let* ((fetch (ende-test--public-tool-schema definitions "web_fetch"))
           (max-chars (gethash "max_chars" (gethash "properties" fetch)))
           (patch (ende-test--public-tool-schema definitions "apply_patch"))
           (patch-property (gethash "patch" (gethash "properties" patch)))
           (patch-description (gethash "description" patch-property)))
      (should (= (gethash "maximum" max-chars)
                 ende-web-fetch-max-output-chars))
      (should (= (gethash "maxLength" patch-property) ende-patch-max-chars))
      (dolist (marker '("*** Begin Patch" "*** Add File" "*** Update File"
                        "*** Delete File" "@@" "*** End Patch"))
        (should (string-match-p (regexp-quote marker) patch-description)))
      (should (string-match-p "exact update context" patch-description)))
    (let* ((run (ende-test--public-tool-schema definitions "run_sandbox_command"))
           (run-cwd (gethash "description"
                             (gethash "cwd" (gethash "properties" run))))
           (host (ende-test--public-tool-schema definitions "run_host_command"))
           (host-cwd (gethash "description"
                              (gethash "cwd" (gethash "properties" host))))
           (copy (ende-test--public-tool-schema definitions "copy_from_sandbox"))
           (copy-properties (gethash "properties" copy))
           (create (ende-test--public-tool-schema definitions "create_file"))
           (create-description (gethash "description"
                                        (gethash "path" (gethash "properties" create)))))
      (dolist (phrase '("workspace-relative" "private workspace" "optional" "defaults to"))
        (should (string-match-p (regexp-quote phrase) run-cwd)))
      (dolist (phrase '("local working directory" "relative values" "project root"
                        "absolute local directories" "optional" "default"))
        (should (string-match-p (regexp-quote phrase) host-cwd)))
      (dolist (phrase '("workspace-relative" "existing regular file"))
        (should (string-match-p (regexp-quote phrase)
                                (gethash "description" (gethash "source" copy-properties)))))
      (dolist (phrase '("independently authorized" "project-relative" "creates a missing file"
                        "replaces an existing regular file"))
        (should (string-match-p (regexp-quote phrase)
                                (gethash "description" (gethash "destination" copy-properties)))))
      (should (string-match-p "parent directory must already exist" create-description)))
    (should-not (ende-tool--find "edit_source"))
    (should-not (plist-get (ende-tool-call "edit_source" nil) :ok))
    (should (string-match-p "edit_file" (json-encode definitions)))
    (should-not (string-match-p "edit_source" (json-encode definitions)))
    (let* ((definition
            (cl-find-if
             (lambda (candidate)
               (equal (alist-get :name (alist-get :function candidate))
                      "edit_file"))
             definitions))
           (schema (ende-protocol-json-read-string
                    (json-encode
                     (alist-get :parameters (alist-get :function definition)))))
           (properties (gethash "properties" schema))
           (item (gethash "items" (gethash "edits" properties)))
           (item-properties (gethash "properties" item))
           (edits-description
            (gethash "description" (gethash "edits" properties))))
      (should (string-match-p "disk contents" (gethash "description" schema)))
      (should (string-match-p "visiting buffer" (gethash "description" schema)))
      (should (string-match-p "same original file" edits-description))
      (should (string-match-p "not sequentially" edits-description))
      (should (string-match-p "not overlap" edits-description))
      (should (string-match-p "original file contents"
                              (gethash "description"
                                       (gethash "anchor" item-properties))))
      (should (string-match-p "one location"
                              (gethash "description"
                                       (gethash "anchor" item-properties))))
      (should (string-match-p "before or after"
                              (gethash "description"
                                       (gethash "position" item-properties))))
      (should (string-match-p "replace"
                              (gethash "description"
                                       (gethash "position" item-properties))))
      (should (string-match-p "particular repeated anchor"
                              (gethash "description"
                                       (gethash "occurrence" item-properties))))
      (should (string-match-p "every matching occurrence"
                              (gethash "description"
                                       (gethash "all" item-properties))))
      (should (equal (append (gethash "required" schema) nil)
                     '("path" "edits")))
      (should (eq (gethash "additionalProperties" schema) :false))
      (should-not (gethash "buffer_id" properties))
      (should-not (gethash "base_digest" properties))
      (should (= (gethash "minItems" (gethash "edits" properties)) 1))
      (should (= (gethash "maxItems" (gethash "edits" properties))
                 ende-tools-max-edits))
      (should (equal (append (gethash "required" item) nil)
                     '("anchor" "position" "text")))
      (should (eq (gethash "additionalProperties" item) :false))
      (should (equal (append (gethash "enum" (gethash "position"
                                                      item-properties))
                             nil)
                     '("before" "after" "replace")))
      (should (= (gethash "minLength" (gethash "anchor" item-properties)) 1))
      (should-not (gethash "minLength" (gethash "text" item-properties)))
      (should (= (gethash "minimum" (gethash "occurrence" item-properties))
                 1))
      (should (eq (gethash "const" (gethash "all" item-properties)) t))
      (should (equal (append (gethash "required" (gethash "not" item)) nil)
                     '("occurrence" "all"))))
    (let* ((definition
            (cl-find-if
             (lambda (candidate)
               (equal (alist-get :name (alist-get :function candidate))
                      "run_sandbox_command"))
             definitions))
           (schema (ende-protocol-json-read-string
                    (json-encode
                     (alist-get :parameters (alist-get :function definition)))))
           (properties (gethash "properties" schema)))
      (should (equal (append (gethash "required" schema) nil)
                     '("command")))
      (should (equal (sort (let (keys)
                             (maphash (lambda (key _value) (push key keys))
                                      properties)
                             keys)
                           #'string<)
                     '("command" "cwd" "timeout")))
      (should (eq (gethash "additionalProperties" schema) :false)))
    (let* ((definition
            (cl-find-if
             (lambda (candidate)
               (equal (alist-get :name (alist-get :function candidate))
                      "create_file"))
             definitions))
           (schema (ende-protocol-json-read-string
                    (json-encode
                     (alist-get :parameters (alist-get :function definition)))))
           (properties (gethash "properties" schema)))
      (should (equal (append (gethash "required" schema) nil)
                     '("path" "content")))
      (should (eq (gethash "additionalProperties" schema) :false))
      (should (= (gethash "minLength" (gethash "path" properties)) 1))
      (should (equal (gethash "type" (gethash "content" properties))
                     "string"))
      (should-not (gethash "minLength" (gethash "content" properties))))))

(ert-deftest ende-test/tool-json-results-normalize-only-boolean-fields ()
  (let* ((root (make-temp-file "ende-tool-json-results-" t))
         (configuration
          (ende-test--tool-access-config
           root '((resource project (include "**"))
                  (tools project_info read_file)
                  (set-tool-resources read_file :resources project))))
         (success
          (ende-protocol-json-read-string
           (ende-chat--tool-response
            (list :id "ok" :name "project_info" :arguments "{}")
            configuration)))
         (failure
          (ende-protocol-json-read-string
           (ende-chat--tool-response
            (list :id "bad" :name "read_file"
                  :arguments "{\"path\":\"missing\"}")
            configuration))))
    (unwind-protect
        (progn
          (should (eq (ende-protocol-json-get "ok" success) t))
          (should (eq (ende-protocol-json-get "ok" failure) :false))
          (should-not (string-match-p "\\\"ok\\\":null"
                                      (json-encode failure)))
          (let* ((done nil)
                 (async-response nil)
                 (async-config
                  (ende-test--tool-access-config root '((tools run_host_command)))))
            (cl-letf (((symbol-function 'ende-tools--run-host-command-process)
                       (lambda (_command _cwd _timeout callback _register)
                         (funcall callback
                                  (list :exit_code 9 :process_ok nil
                                        :stdout "" :stderr ""
                                        :stdout_truncated :json-false
                                        :stderr_truncated :json-false)
                                  nil))))
              (ende-chat--tool-response-async
               (list :id "async" :name "run_host_command"
                     :arguments "{\"command\":\"exit 9\"}")
               async-config
               (lambda (response) (setq async-response response done t))))
            (should done)
            (should (eq (ende-protocol-json-get "ok"
                                               (ende-protocol-json-read-string async-response))
                        t))
            (should (eq (ende-protocol-json-get
                         "process_ok"
                         (ende-protocol-json-get
                          "result" (ende-protocol-json-read-string async-response)))
                        :false)))
          (let* ((internal (ende-tool-call "read_file"
                                           '(("path" . "missing")) root configuration))
                 (encoded (ende-tools--json-encode-tool-result internal)))
            (should (null (plist-get internal :ok)))
            (should (string-match-p "\\\"ok\\\":false" encoded)))
          (let ((nullable
                 (ende-protocol-json-read-string
                  (ende-tools--json-encode-tool-result
                   '(:ok t :visiting_buffer nil
                     :visiting_buffer_modified nil
                     :visiting_buffer_warning nil)))))
            (should (eq (ende-protocol-json-get "ok" nullable) t))
            (should (eq (ende-protocol-json-get "visiting_buffer" nullable)
                        :false))
            (should (null (ende-protocol-json-get
                           "visiting_buffer_modified" nullable)))
            (should (eq (ende-protocol-json-get
                         "visiting_buffer_modified"
                         (ende-protocol-json-read-string
                          (ende-tools--json-encode-tool-result
                           '(:ok t :visiting_buffer t
                             :visiting_buffer_modified nil))))
                        :false))
            (should (eq (ende-protocol-json-get
                         "stdout_truncated"
                         (ende-protocol-json-read-string
                          (ende-tools--json-encode-tool-result
                           '(:ok t :stdout_truncated :json-false))))
                        :false))
            (should (null (ende-protocol-json-get
                           "visiting_buffer_warning" nullable)))))
      (delete-directory root t))))

(ert-deftest ende-test/sandbox-command-result-separates-process-success ()
  (should (plist-get (ende-sandbox--result 0 "" "" nil nil) :process_ok))
  (should-not (plist-get (ende-sandbox--result 7 "" "" nil nil) :process_ok)))

(ert-deftest ende-test/chat-host-command-policy-resolves-with-tool-exclusions ()
  (let ((root (make-temp-file "ende-host-policy-" t)))
    (unwind-protect
        (progn
          (let ((unrestricted
                 (ende-test--tool-access-config
                  root '((tools run_host_command))))
                (restricted
                 (ende-test--tool-access-config
                  root '((tools run_host_command)
                         (restrict-host-commands
                          "git" (:fixed-args "make" "test"))))))
            (should (eq (plist-get unrestricted :host-command-policy)
                        :unrestricted))
            (should (equal (plist-get restricted :host-command-policy)
                           '(:restricted t :open ("git")
                             :fixed (("make" "test"))))))
          (dolist (forms '(((restrict-host-commands "git"))
                           ((tools run_host_command)
                            (restrict-host-commands))
                           ((restrict-host-commands "git")
                            (tools run_host_command))
                           ((tools run_host_command)
                            (restrict-host-commands "git")
                            (restrict-host-commands "emacs"))
                           ((tools run_host_command)
                            (restrict-host-commands "git")
                            (tools run_host_command))
                           ((tools run_host_command)
                            (restrict-host-commands "/usr/bin/git"))
                           ((tools run_host_command)
                            (restrict-host-commands "git status"))
                           ((tools run_host_command)
                            (restrict-host-commands "."))
                           ((tools run_host_command)
                            (restrict-host-commands
                             (:fixed-args "../git" "status")))
                           ((tools run_host_command)
                            (restrict-host-commands (:fixed "git")))))
            (should-error (ende-test--tool-access-config root forms)))
          (with-temp-buffer
            (org-mode)
            (insert
             (ende-test--chat-config-block
              `((root ,root)
                (group development run_host_command read_file edit_file)
                (resource project (include "**"))
                (tools development)
                (set-tool-resources read_file :resources project)
                (set-tool-resources edit_file :resources project)
                (restrict-host-commands "git")))
             "* Conversation\n** Parent\n*** Selectively cleared\n"
             (ende-test--chat-config-block
              '((clear-tools run_host_command read_file)))
             "*** Re-enabled\n"
             (ende-test--chat-config-block '((tools run_host_command))))
            (let* ((document (ende-chat--build-document))
                   (parent (ende-test--chat-config-at document "Parent"))
                   (cleared
                    (ende-test--chat-config-at document "Selectively cleared"))
                   (reenabled (ende-test--chat-config-at document "Re-enabled")))
              (should (memq 'run_host_command (plist-get parent :active-tools)))
              (should-not (memq 'run_host_command
                                (plist-get cleared :active-tools)))
              (should-not (memq 'read_file (plist-get cleared :active-tools)))
              (should (memq 'edit_file (plist-get cleared :active-tools)))
              (should (eq (plist-get cleared :host-command-policy) :inactive))
              (should (memq 'run_host_command
                            (plist-get reenabled :active-tools)))
              (should (eq (plist-get reenabled :host-command-policy)
                          :unrestricted)))))
      (delete-directory root t))))

(ert-deftest ende-test/chat-host-command-restrictions-replace-by-block ()
  (let ((root (make-temp-file "ende-host-policy-inherit-" t)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert
           (ende-test--chat-config-block
            `((root ,root)
              (tools run_host_command)
              (restrict-host-commands "git")))
           "* Conversation\n** Parent\n*** Replaced\n"
           (ende-test--chat-config-block
            '((restrict-host-commands "emacs" (:fixed-args "ls"))))
           "*** Reset\n"
           (ende-test--chat-config-block '((tools run_host_command))))
          (let* ((document (ende-chat--build-document))
                 (parent (ende-test--chat-config-at document "Parent"))
                 (replaced (ende-test--chat-config-at document "Replaced"))
                 (reset (ende-test--chat-config-at document "Reset")))
            (should (equal (plist-get parent :host-command-policy)
                           '(:restricted t :open ("git") :fixed nil)))
            (should (equal (plist-get replaced :host-command-policy)
                           '(:restricted t :open ("emacs") :fixed (("ls")))))
            (should (eq (plist-get reset :host-command-policy) :unrestricted))))
      (delete-directory root t))))

(ert-deftest ende-test/run-host-command-config-needs-no-podman-or-resources ()
  (let* ((root (make-temp-file "ende-host-command-config-" t))
         (configuration
          (ende-test--tool-access-config root '((tools run_host_command)))))
    (unwind-protect
        (progn
          (should (member 'run_host_command
                          (plist-get configuration :active-tools)))
          (should (equal (plist-get configuration :root)
                         (file-name-as-directory root)))
          (should-not (plist-get configuration :podman-image))
          (should-not (assq 'run_host_command
                            (plist-get configuration :tool-resources))))
      (delete-directory root t))))

(ert-deftest ende-test/host-command-schema-and-restricted-argv-execution ()
  (let* ((root (make-temp-file "ende-host-argv-" t))
         (configuration
          (ende-test--tool-access-config
           root '((tools run_host_command)
                  (restrict-host-commands "printf"
                                          (:fixed-args "true")
                                          (:fixed-args "make" "test")
                                          (:fixed-args "true" "foo")))))
         (definitions (ende-tool-definitions configuration))
         (schema (ende-test--public-tool-schema definitions "run_host_command"))
         (command-schema
          (ende-protocol-json-get
           "command" (ende-protocol-json-get "properties" schema)))
         (branches (ende-protocol-json-get "oneOf" command-schema))
         (fixed (ende-protocol-json-get
                 "const"
                 (ende-protocol-json-get
                  "args"
                  (ende-protocol-json-get
                   "properties" (aref branches 1)))))
         (fixed-argv (ende-protocol-json-get
                      "const"
                      (ende-protocol-json-get
                       "args"
                       (ende-protocol-json-get
                        "properties" (aref branches 2)))))
         (unrestricted-schema
          (ende-test--public-tool-schema (ende-tool-definitions)
                                         "run_host_command")))
    (unwind-protect
        (progn
          (should (equal fixed []))
          (should (equal fixed-argv ["test"]))
          (should (equal (ende-protocol-json-get
                          "enum"
                          (ende-protocol-json-get
                           "program"
                           (ende-protocol-json-get
                            "properties" (aref branches 0))))
                         ["printf"]))
          (should (equal (ende-protocol-json-get
                          "type"
                          (ende-protocol-json-get
                           "command" (ende-protocol-json-get "properties" unrestricted-schema)))
                         "string"))
          (should (equal (json-encode unrestricted-schema)
                         (json-encode
                          (ende-test--public-tool-schema
                           (ende-tool-definitions) "run_host_command"))))
          (should (equal
                   (json-encode
                    (ende-test--public-tool-schema definitions "read_file"))
                   (json-encode
                    (ende-test--public-tool-schema (ende-tool-definitions)
                                                   "read_file"))))
          (let* ((literals '("|" ";" "&&" "||" ">" "$(x)" "`x`"))
                 (command (make-hash-table :test #'equal))
                 (_ (puthash "program" "printf" command))
                 (_ (puthash "args" (vconcat (cons "%s\\n" literals)) command))
                 (call (ende-test--host-command-call
                        (list (cons "command" command)) configuration)))
            (should (plist-get call :ok))
            (should (equal (plist-get (plist-get call :result) :stdout)
                           (concat (mapconcat #'identity literals "\n") "\n"))))
          (dolist (argv '(("python" "script.py")
                          ("true" "extra")
                          ("true" "bar")))
            (let* ((command (make-hash-table :test #'equal))
                   (_ (puthash "program" (car argv) command))
                   (_ (puthash "args" (vconcat (cdr argv)) command))
                   (call (ende-test--host-command-call
                          (list (cons "command" command)) configuration)))
              (should-not (plist-get call :ok))))
          (let ((command (make-hash-table :test #'equal)))
            (puthash "program" "true" command)
            (puthash "args" ["foo"] command)
            (should
             (plist-get
              (ende-test--host-command-call
               (list (cons "command" command)) configuration)
              :ok)))
          (let ((missing-args (make-hash-table :test #'equal)))
            (puthash "program" "printf" missing-args)
            (should-not
             (plist-get
              (ende-test--host-command-call
               (list (cons "command" missing-args)) configuration)
              :ok)))
          (let* ((changed
                  (ende-test--tool-access-config
                   root '((tools run_host_command)
                          (restrict-host-commands "printf" "true"))))
                 (changed-schema
                  (ende-test--public-tool-schema
                   (ende-tool-definitions changed) "run_host_command")))
            (should-not (equal schema changed-schema)))
          (let ((command (make-hash-table :test #'equal)))
            (puthash "program" "true" command)
            (puthash "args" [] command)
            (should
             (plist-get
              (ende-test--host-command-call
               (list (cons "command" command)) configuration)
              :ok))))
      (delete-directory root t))))

(ert-deftest ende-test/restricted-host-command-policy-is-enforced-by-tool-dispatch ()
  (let* ((root (make-temp-file "ende-host-deny-" t))
         (configuration
          (ende-test--tool-access-config
           root '((tools run_host_command)
                  (restrict-host-commands
                   "printf" (:fixed-args "true")))))
         (arguments (make-hash-table :test #'equal))
         (started nil))
    (puthash "program" "true" arguments)
    (puthash "args" ["extra"] arguments)
    (let ((result nil))
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest _)
                   (setq started t)
                   (error "process must not start"))))
        (ende-tool-call-async
         "run_host_command" `(("command" . ,arguments)) nil configuration
         (lambda (value) (setq result value))))
      (should-not started)
      (should-not (plist-get result :ok))
      (should (string-match-p "not allowed"
                              (plist-get result :error))))))

(ert-deftest ende-test/restricted-host-process-cancellation-and-timeout-stop-work ()
  (let* ((root (make-temp-file "ende-host-direct-stop-" t))
         (configuration
          (ende-test--tool-access-config
           root '((tools run_host_command)
                  (restrict-host-commands "sh"))))
         (cancel-file (expand-file-name "restricted-cancel.pid" root))
         (timeout-file (expand-file-name "restricted-timeout.pid" root))
         (process nil)
         (cancel nil)
         (real-make-process (symbol-function 'make-process)))
    (unwind-protect
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest arguments)
                     (setq process (apply real-make-process arguments))))
                  (ende-tools--register-cancellation-function
                   (lambda (function) (setq cancel function))))
          (let* ((command (make-hash-table :test #'equal))
                 (result nil))
            (puthash "program" "sh" command)
            (puthash "args"
                     (vector "-c" (format "sleep 30 & echo $! > %s; wait"
                                           (shell-quote-argument cancel-file)))
                     command)
            (ende-tool-call-async
             "run_host_command" (list (cons "command" command)) nil configuration
             (lambda (value) (setq result value)))
            (let ((deadline (+ (float-time) 5)))
              (while (and (not process) (< (float-time) deadline))
                (accept-process-output nil 0.02)))
            (should (process-live-p process))
            (should (functionp cancel))
            (ende-test--wait-for-host-command-file cancel-file)
            (let ((pid (string-to-number
                        (with-temp-buffer
                          (insert-file-contents cancel-file)
                          (buffer-string)))))
              (should (process-attributes pid))
              (funcall cancel)
              (should (ende-test--assert-host-process-stopped pid)))
            (should (not (plist-get result :ok))))
          (setq process nil cancel nil)
          (let* ((command (make-hash-table :test #'equal))
                 (done nil)
                 (result nil))
            (puthash "program" "sh" command)
            (puthash "args"
                     (vector "-c" (format "sleep 30 & echo $! > %s; wait"
                                           (shell-quote-argument timeout-file)))
                     command)
            (ende-tool-call-async
             "run_host_command"
             (list (cons "command" command) (cons "timeout" 1)) nil configuration
             (lambda (value) (setq result value done t)))
            (let ((deadline (+ (float-time) 8)))
              (while (and (not done) (< (float-time) deadline))
                (accept-process-output nil 0.05)))
            (should done)
            (should (= (plist-get (plist-get result :result) :exit_code) 124))
            (ende-test--wait-for-host-command-file timeout-file)
            (should (ende-test--assert-host-process-stopped
                     (string-to-number
                      (with-temp-buffer
                        (insert-file-contents timeout-file)
                        (buffer-string)))))))
      (when (and (processp process) (process-live-p process))
        (delete-process process))
      (delete-directory root t))))

(ert-deftest ende-test/run-host-command-uses-host-cwd-and-captures-results ()
  (let* ((root (make-temp-file "ende-host-command-" t))
         (cwd (expand-file-name "work" root))
         (configuration
          (ende-test--tool-access-config root '((tools run_host_command))))
         (call nil))
    (unwind-protect
        (progn
          (make-directory cwd)
          (setq call
                (ende-test--host-command-call
                 '(("command" . "printf 'out:%s\\n' \"$PWD\"; printf 'err\\n' >&2; exit 7")
                   ("cwd" . "work"))
                 configuration))
          (should (plist-get call :ok))
          (let ((result (plist-get call :result)))
            (should (= (plist-get result :exit_code) 7))
            (should-not (plist-get result :process_ok))
            (should (equal (plist-get result :stdout)
                           (format "out:%s\n" cwd)))
            (should (equal (plist-get result :stderr) "err\n"))
            (should (eq (plist-get result :stdout_truncated) :json-false))
            (should (eq (plist-get result :stderr_truncated) :json-false)))
          (setq call
                (ende-test--host-command-call
                 '(("command" . "printf '%070000d' 0")) configuration))
          (should (plist-get call :ok))
          (let ((result (plist-get call :result)))
            (should (= (plist-get result :exit_code) 0))
            (should (plist-get result :process_ok))
            (should (= (length (plist-get result :stdout)) 65536))
            (should (eq (plist-get result :stdout_truncated) t))))
      (delete-directory root t))))

(ert-deftest ende-test/run-host-command-cancellation-and-timeout-kill-groups ()
  (let* ((root (make-temp-file "ende-host-command-stop-" t))
         (configuration
          (ende-test--tool-access-config root '((tools run_host_command))))
         (cancel-file (expand-file-name "cancel.pid" root))
         (timeout-file (expand-file-name "timeout.pid" root))
         (cancel-function nil)
         (timeout-cancel nil))
    (unwind-protect
        (progn
          (let ((done nil)
                (calls 0)
                (call-result nil)
                (ende-tools--register-cancellation-function
                 (lambda (function) (setq cancel-function function))))
            (ende-tool-call-async
             "run_host_command"
             `(("command" . ,(format "sleep 30 & echo $! > %s; wait"
                                      (shell-quote-argument cancel-file))))
             nil configuration
             (lambda (value)
               (setq calls (1+ calls) call-result value done t)))
            (ende-test--wait-for-host-command-file cancel-file)
            (let ((pid (string-to-number
                        (with-temp-buffer
                          (insert-file-contents cancel-file)
                          (buffer-string)))))
              (should (> pid 0))
              (should (process-attributes pid))
              (should (functionp cancel-function))
              (funcall cancel-function)
              (should done)
              (should (= calls 1))
              (should-not (plist-get call-result :ok))
              (ende-test--assert-host-process-stopped pid)
              (accept-process-output nil 0.1)
              (should (= calls 1))))
          (let ((done nil)
                (calls 0)
                (call-result nil)
                (ende-tools--register-cancellation-function
                 (lambda (function) (setq timeout-cancel function))))
            (ende-tool-call-async
             "run_host_command"
             `(("command" . ,(format "sleep 30 & echo $! > %s; wait"
                                      (shell-quote-argument timeout-file)))
               ("timeout" . 1))
             nil configuration
             (lambda (value)
               (setq calls (1+ calls) call-result value done t)))
            (ende-test--wait-for-host-command-file timeout-file)
            (let ((pid (string-to-number
                        (with-temp-buffer
                          (insert-file-contents timeout-file)
                          (buffer-string))))
                  (deadline (+ (float-time) 8.0)))
              (while (and (not done) (< (float-time) deadline))
                (accept-process-output nil 0.05))
              (should done)
              (should (= calls 1))
              (should (plist-get call-result :ok))
              (should (= (plist-get (plist-get call-result :result)
                                    :exit_code)
                         124))
              (should (> pid 0))
              (ende-test--assert-host-process-stopped pid)
              (accept-process-output nil 0.1)
              (should (= calls 1)))))
      (when (functionp cancel-function)
        (funcall cancel-function))
      (when (functionp timeout-cancel)
        (funcall timeout-cancel))
      (delete-directory root t))))

(ert-deftest ende-test/project-tool-collections-encode-as-json-arrays ()
  (let* ((root (make-temp-file "ende-tools-json-collections-" t))
         (alpha (expand-file-name "alpha.txt" root))
         (beta (expand-file-name "beta.txt" root))
         (buffers nil)
         (empty-arguments (make-hash-table :test 'equal))
         (search-arguments (make-hash-table :test 'equal)))
    (puthash "query" "needle" search-arguments)
    (unwind-protect
        (progn
          (with-temp-file alpha (insert "needle alpha\n"))
          (with-temp-file beta (insert "needle beta\n"))
          (setq buffers (list (find-file-noselect alpha)
                              (find-file-noselect beta)))
          (let ((ende-project-root-directory root))
            (dolist (case (list (list "list_project_files" empty-arguments "files")
                                (list "search_project" search-arguments "matches")
                                (list "list_project_buffers" empty-arguments "buffers")))
              (let ((records (ende-test--project-tool-json-records
                              (nth 0 case) (nth 1 case) (nth 2 case)
                              root nil)))
                (should (vectorp records))
                (should (= (length records) 2))
                (should
                 (equal (mapcar (lambda (record)
                                  (ende-protocol-json-get "path" record))
                                (append records nil))
                        '("alpha.txt" "beta.txt")))))))
      (dolist (buffer buffers)
        (when (buffer-live-p buffer)
          (set-buffer-modified-p nil)
          (kill-buffer buffer)))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ende-test/project-tools-enforce-scope-and-report-file-info ()
  (let* ((root (make-temp-file "ende-tools-project-" t))
         (source (expand-file-name "src/main.el" root))
         (secret (expand-file-name ".env" root))
         (outside (make-temp-file "ende-tools-outside-")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory source) t)
          (with-temp-file source
            (insert "one\ntwo\nthree\n"))
          (with-temp-file secret
            (insert "TOKEN=secret\n"))
          (let ((ende-project-root-directory root))
            (let* ((info (ende-test--tool-result
                          "file_info" '(("path" . "src/main.el")) root))
                   (files (ende-test--tool-result
                           "list_project_files" nil root)))
              (should (= (plist-get info :line_count) 3))
              (should (= (length (plist-get files :files)) 1))
              (should (string= (plist-get
                                (elt (plist-get files :files) 0) :path)
                               "src/main.el")))
            (let ((failure
                   (ende-tool-call
                    "read_file" '(("path" . "../outside")) root)))
              (should-not (plist-get failure :ok))
              (should (string-match-p "outside the project"
                                      (plist-get failure :error))))))
      (when (file-directory-p root)
        (delete-directory root t))
      (when (file-exists-p outside)
        (delete-file outside)))))

(ert-deftest ende-test/file-tools-read-authorized-external-resources ()
  (let* ((root (make-temp-file "ende-tools-resource-project-" t))
         (external-root (make-temp-file "ende-tools-resource-external-" t))
         (unrelated-root (make-temp-file "ende-tools-resource-unrelated-" t))
         (project-file (expand-file-name "src/main.el" root))
         (external-file (expand-file-name "allowed/config.el" external-root))
         (excluded-file (expand-file-name "allowed/private/secret.el" external-root))
         (outside-include (expand-file-name "other/config.el" external-root))
         (unrelated-file (expand-file-name "secret.txt" unrelated-root)))
    (unwind-protect
        (progn
          (dolist (path (list project-file external-file excluded-file
                              outside-include unrelated-file))
            (make-directory (file-name-directory path) t))
          (with-temp-file project-file (insert "project contents\n"))
          (with-temp-file external-file (insert "external contents\n"))
          (with-temp-file excluded-file (insert "private contents\n"))
          (with-temp-file outside-include (insert "not included\n"))
          (with-temp-file unrelated-file (insert "unrelated\n"))
          (let* ((configuration
                  (ende-test--tool-access-config
                   root
                   `((resource project (include "**"))
                     (resource external :root ,external-root
                               (include "allowed/**")
                               (exclude "allowed/private/**"))
                     (tools read_file file_info list_project_files)
                     (set-tool-resources read_file file_info list_project_files
                                         :resources project external))))
                 (project-read
                  (ende-tool-call 'read_file '(:path "src/main.el")
                                  root configuration))
                 (external-read
                  (ende-tool-call 'read_file (list :path external-file)
                                  root configuration))
                 (excluded-read
                  (ende-tool-call 'read_file (list :path excluded-file)
                                  root configuration))
                 (outside-read
                  (ende-tool-call 'read_file (list :path outside-include)
                                  root configuration))
                 (unrelated-read
                  (ende-tool-call 'read_file (list :path unrelated-file)
                                  root configuration)))
            (should (plist-get project-read :ok))
            (should (equal (plist-get (plist-get project-read :result) :path)
                           "src/main.el"))
            (should (plist-get external-read :ok))
            (let* ((external-result (plist-get external-read :result))
                   (public-path (plist-get external-result :path))
                   (round-trip
                    (ende-tool-call 'file_info (list :path public-path)
                                    root configuration)))
              (should (file-name-absolute-p public-path))
              (should (equal public-path (file-truename external-file)))
              (should (equal (plist-get external-result :content)
                             "external contents\n"))
              (should (plist-get round-trip :ok))
              (should (equal (plist-get (plist-get round-trip :result) :path)
                             public-path)))
            (dolist (denied (list excluded-read outside-read unrelated-read))
              (should-not (plist-get denied :ok)))
            (let* ((listed (ende-tool-call 'list_project_files nil
                                           root configuration))
                   (paths (mapcar (lambda (record) (plist-get record :path))
                                  (plist-get (plist-get listed :result) :files))))
              (should (plist-get listed :ok))
              (should (equal paths '("src/main.el")))
              (should-not (member (file-truename external-file) paths))))
          ;; Internal calls without a frozen chat policy remain project-scoped.
          (let ((unscoped
                 (ende-tool-call 'read_file (list :path external-file) root)))
            (should-not (plist-get unscoped :ok))))
      (dolist (directory (list root external-root unrelated-root))
        (when (file-directory-p directory)
          (delete-directory directory t))))))

(ert-deftest ende-test/project-tools-edit-file-enforces-resource-scope ()
  (let* ((root (make-temp-file "ende-tools-edit-scope-" t))
         (allowed (expand-file-name "public.el" root))
         (blocked (expand-file-name "private.el" root)))
    (unwind-protect
        (progn
          (with-temp-file allowed (insert "public source\n"))
          (with-temp-file blocked (insert "private source\n"))
          (with-temp-buffer
            (org-mode)
            (insert
             (ende-test--chat-config-block
              `((root ,root)
                (group writer edit_file)
                (resource public (include "public.el"))
                (tools writer)
                (set-tool-resources writer :resources public)))
             "* Conversation\n** Edit\n")
            (let* ((document (ende-chat--build-document))
                   (configuration
                    (ende-test--chat-config-at document "Edit"))
                   (allowed-call
                    (ende-tool-call
                     'edit_file
                     (list :path "public.el"
                           :edits (list (ende-test--anchor-edit
                                         "source" "replace" "edited")))
                     root configuration))
                   (blocked-call
                    (ende-tool-call
                     'edit_file
                     (list :path "private.el"
                           :edits (list (ende-test--anchor-edit
                                         "source" "replace" "edited")))
                     root configuration))
                   (outside-call
                    (ende-tool-call
                     'edit_file
                     (list :path "../outside.el"
                           :edits (list (ende-test--anchor-edit
                                         "source" "replace" "edited")))
                     root configuration)))
              (should (plist-get allowed-call :ok))
              (should (equal (plist-get (plist-get allowed-call :result)
                                        :source_kind)
                             "file"))
              (should-not (plist-get blocked-call :ok))
              (should (string-match-p "not authorized"
                                      (plist-get blocked-call :error)))
              (should-not (plist-get outside-call :ok))
              (should (string-match-p "outside the project"
                                      (plist-get outside-call :error)))
              (should (equal (with-temp-buffer
                               (insert-file-contents allowed)
                               (buffer-string))
                             "public edited\n"))
              (should (equal (with-temp-buffer
                               (insert-file-contents blocked)
                               (buffer-string))
                             "private source\n")))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ende-test/project-tools-separate-disk-and-buffer-sources ()
  (let* ((root (make-temp-file "ende-tools-buffer-project-" t))
         (path (expand-file-name "main.el" root))
         (buffer nil))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "DISK_TOKEN from disk\nsecond disk\n"))
          (setq buffer (find-file-noselect path))
          (with-current-buffer buffer
            (erase-buffer)
            (insert "BUFFER_TOKEN unsaved\nsecond buffer\nthird buffer\n")
            (set-buffer-modified-p t))
          (let* ((ende-project-root-directory root)
                 (listed (ende-test--tool-result
                          "list_project_buffers" nil root))
                 (record (elt (plist-get listed :buffers) 0))
                 (buffer-id (plist-get record :buffer_id))
                 (buffer-read
                  (ende-test--tool-result
                   "read_buffer"
                   (list (cons "buffer_id" buffer-id)
                         (cons "start_line" 1)
                         (cons "line_count" 1))
                   root))

                 (file-read
                  (ende-test--tool-result
                   "read_file"
                   '(("path" . "main.el")
                     ("start_line" . 1)
                     ("line_count" . 1))
                   root))
                 (info (ende-test--tool-result
                        "file_info" '(("path" . "main.el")) root))
                 (files (ende-test--tool-result
                         "list_project_files"
                         '(("include_metadata" . t)) root))
                 (disk-search
                  (ende-test--tool-result
                   "search_project"
                   '(("query" . "DISK_TOKEN")) root))
                 (buffer-search
                  (ende-test--tool-result
                   "search_project"
                   '(("query" . "BUFFER_TOKEN")) root)))
            (should (string= (plist-get buffer-read :source_kind) "buffer"))
            (should (string= (plist-get buffer-read :content)
                             "BUFFER_TOKEN unsaved\n"))
            (should (string= (plist-get buffer-read :path) "main.el"))
            (should (string= (plist-get file-read :source_kind) "file"))
            (should (string= (plist-get file-read :content)
                             "DISK_TOKEN from disk\n"))
            (should (string= (plist-get file-read :digest)
                             (plist-get info :digest)))
            (should (= (plist-get info :line_count) 2))
            (should-not (equal
                         (plist-get file-read :digest)
                         (with-current-buffer buffer
                           (secure-hash 'sha256 (current-buffer)
                                        (point-min) (point-max)))))
            (should (= (plist-get info :byte_count)
                       (string-bytes "DISK_TOKEN from disk\nsecond disk\n")))
            (should (plist-get info :visiting_buffer))
            (should (eq (plist-get info :visiting_buffer_modified) t))
            (should (string= (plist-get
                              (elt (plist-get files :files) 0) :source_kind)
                             "file"))
            (should (string= (plist-get
                              (elt (plist-get files :files) 0) :digest)
                             (plist-get info :digest)))
            (should (= (length (plist-get disk-search :matches)) 1))
            (should (string= (plist-get
                              (elt (plist-get disk-search :matches) 0)
                              :source_kind)
                             "file"))
            (should (= (length (plist-get buffer-search :matches)) 0))))
      (when (buffer-live-p buffer)
        (set-buffer-modified-p nil)
        (kill-buffer buffer))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ende-test/project-tools-search-supports-literal-queries-and-patterns ()
  (let* ((root (make-temp-file "ende-tools-search-literals-" t))
         (path (expand-file-name "search.txt" root)))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "foo and bar\nfoo|bar\na.b and x+y\naXb\n"))
          (let* ((ende-project-root-directory root)
                 (literal (ende-test--tool-result
                           "search_project" '(("query" . "a.b")) root))
                 (either (ende-test--tool-result
                          "search_project" '(("patterns" . ["foo" "bar"])) root))
                 (metacharacters (ende-test--tool-result
                                  "search_project"
                                  '(("patterns" . ["a.b" "x+y"])) root))
                 (regexp (ende-test--tool-result
                          "search_project" '(("regexp" . "foo\\|bar")) root)))
            (should (= (length (plist-get literal :matches)) 1))
            (should (string= (plist-get (car (append (plist-get literal :matches) nil))
                                       :match)
                             "a.b and x+y"))
            (should (= (length (plist-get either :matches)) 4))
            (should (= (length (plist-get metacharacters :matches)) 2))
            (should (string= (plist-get (car (append (plist-get metacharacters :matches) nil))
                                       :match)
                             "a.b and x+y"))
            (should (= (length (plist-get regexp :matches)) 4))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ende-test/project-tools-search-requires-one-valid-search-form ()
  (let* ((root (make-temp-file "ende-tools-search-validation-" t))
         (path (expand-file-name "search.txt" root)))
    (unwind-protect
        (progn
          (with-temp-file path (insert "content\n"))
          (let ((ende-project-root-directory root))
            (dolist (arguments '(nil
                                (("query" . "one") ("regexp" . "one")
                                 ("glob" . "*.txt"))
                                (("query" . ""))
                                (("patterns" . []))
                                (("patterns" . ["ok" ""]))))
              (let ((call (ende-tool-call "search_project" arguments root)))
                (should-not (plist-get call :ok))))
            (let ((none (ende-tool-call "search_project" nil root))
                  (multiple (ende-tool-call
                             "search_project"
                             '(("query" . "one") ("patterns" . ["two"]))
                             root)))
              (should (string-match-p "exactly one" (plist-get none :error)))
              (should (string-match-p "exactly one"
                                      (plist-get multiple :error))))
            (let ((above (ende-tool-call
                          "search_project"
                          '(("query" . "needle") ("context_before" . 21))
                          root)))
              (should-not (plist-get above :ok))
              (should (string-match-p "integer from 0 to 20"
                                      (plist-get above :error))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ende-test/project-tools-read-search-and-cap-results ()
  (let* ((root (make-temp-file "ende-tools-search-project-" t))
         (first (expand-file-name "first.txt" root))
         (second (expand-file-name "second.txt" root)))
    (unwind-protect
        (progn
          (with-temp-file first
            (insert "needle one\nkeep\nneedle two\n"))
          (with-temp-file second
            (insert "needle three\n"))
          (let* ((ende-project-root-directory root)
                 (read (ende-test--tool-result
                        "read_file"
                        '(("path" . "first.txt")
                          ("start_line" . 2)
                          ("line_count" . 1))
                        root))
                 (search (ende-test--tool-result
                          "search_project"
                          '(("query" . "needle")
                            ("max_results" . 2)
                            ("context_before" . 1))
                          root)))
            (should (= (plist-get read :start_line) 2))
            (should (string= (plist-get read :content) "keep\n"))
            (should (= (length (plist-get search :matches)) 2))
            (should (plist-get search :truncated))
            (should (string-match-p "needle"
                                    (plist-get
                                     (elt (plist-get search :matches) 0)
                                     :context)))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ende-test/project-tools-edit-file-writes-disk-and-preserves-buffers ()
  (dolist (state '(no-buffer clean-buffer modified-buffer))
    (let* ((root (make-temp-file "ende-tools-edit-project-" t))
           (path (expand-file-name "main.el" root))
           (buffer nil))
      (unwind-protect
          (progn
            (with-temp-file path
              (insert "hello world\nkeep\n"))
            (unless (eq state 'no-buffer)
              (setq buffer (find-file-noselect path)))
            (when buffer
              (with-current-buffer buffer
                (when (eq state 'modified-buffer)
                  (erase-buffer)
                  (insert "unsaved buffer contents\n")
                  (set-buffer-modified-p t))
                (goto-char (+ (point-min) 3))
                (narrow-to-region (point-min) (+ (point-min) 8))
                (goto-char (+ (point-min) 2))))
            (let* ((snapshot
                    (and buffer
                         (with-current-buffer buffer
                           (list :text
                                 (save-restriction
                                   (widen)
                                   (buffer-substring-no-properties
                                    (point-min) (point-max)))
                                 :modified (buffer-modified-p)
                                 :point (point)
                                 :minimum (point-min)
                                 :maximum (point-max)
                                 :undo-list buffer-undo-list))))
                   (ende-project-root-directory root)
                   (result
                    (ende-test--tool-result
                     "edit_file"
                     (list (cons "path" "main.el")
                           (cons "edits"
                                 (list (ende-test--anchor-edit
                                        "world" "replace" "Emacs"))))
                     root)))
              (should (equal (plist-get result :operation) "edit"))
              (should (equal (plist-get result :status) "applied"))
              (should (equal (plist-get result :source_kind) "file"))
              (should (equal (plist-get result :write_target) "disk_file"))
              (should (= (plist-get result :edit_count) 1))
              (should (equal (with-temp-buffer
                               (insert-file-contents path)
                               (buffer-string))
                             "hello Emacs\nkeep\n"))
              (if buffer
                  (with-current-buffer buffer
                    (should (equal
                             (save-restriction
                               (widen)
                               (buffer-substring-no-properties
                                (point-min) (point-max)))
                             (plist-get snapshot :text)))
                    (should (eq (buffer-modified-p)
                                (plist-get snapshot :modified)))
                    (should (= (point) (plist-get snapshot :point)))
                    (should (= (point-min) (plist-get snapshot :minimum)))
                    (should (= (point-max) (plist-get snapshot :maximum)))
                    (should (eq buffer-undo-list
                                (plist-get snapshot :undo-list)))
                    (should (stringp
                             (plist-get result :visiting_buffer_warning)))
                    (should (eq (plist-get result :visiting_buffer_modified)
                                (eq state 'modified-buffer)))
                    (should (plist-get result :visiting_buffer))
                    (when (eq state 'modified-buffer)
                      (should (string-match-p
                               "saving.*overwrite"
                               (plist-get result :visiting_buffer_warning)))))
                (progn
                  (should-not (get-file-buffer path))
                  (should-not (plist-get result :visiting_buffer))
                  (should-not (plist-get result :visiting_buffer_warning))))))
        (when (buffer-live-p buffer)
          (set-buffer-modified-p nil)
          (kill-buffer buffer))
        (when (file-directory-p root)
          (delete-directory root t))))))

(ert-deftest ende-test/project-tools-edit-file-ignores-and-preserves-reviews ()
  (let* ((root (make-temp-file "ende-tools-review-project-" t))
         (path (expand-file-name "main.el" root))
         (buffer nil)
         (review nil))
    (unwind-protect
        (progn
          (with-temp-file path (insert "hello world\n"))
          (setq buffer (find-file-noselect path))
          (with-current-buffer buffer
            (erase-buffer)
            (insert "unsaved buffer contents\n")
            (set-buffer-modified-p t)
            (setq review (ende-review-create-overlay
                          buffer 1 8 "model" "client" "unsaved")))
          (let* ((ende-project-root-directory root)
                 (ende-review-conflict-policy 'block)
                 (overlays-before
                  (with-current-buffer buffer
                    (overlays-in (point-min) (point-max))))
                 (result
                  (ende-test--tool-result
                   "edit_file"
                   (list (cons "path" "main.el")
                         (cons "edits"
                               (list (ende-test--anchor-edit
                                      "world" "replace" "Emacs"))))
                   root)))
            (should (equal (plist-get result :status) "applied"))
            (should (equal (plist-get result :source_kind) "file"))
            (with-current-buffer buffer
              (should (equal overlays-before
                             (overlays-in (point-min) (point-max))))
              (should (ende-review-overlay-p review))
              (should (string= (buffer-string)
                               "unsaved buffer contents\n")))
            (should (string= (with-temp-buffer
                               (insert-file-contents path)
                               (buffer-string))
                             "hello Emacs\n"))))
      (when (buffer-live-p buffer)
        (set-buffer-modified-p nil)
        (kill-buffer buffer))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ende-test/project-tools-edit-file-preserves-encoding-and-line-endings ()
  (let* ((root (make-temp-file "ende-tools-encoding-project-" t))
         (path (expand-file-name "main.txt" root)))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert "café\nkeep\n")
            (let ((coding-system-for-write 'utf-8-dos))
              (write-region (point-min) (point-max) path nil nil)))
          (let ((result
                 (ende-test--tool-result
                  "edit_file"
                  (list (cons "path" "main.txt")
                        (cons "edits"
                              (list (ende-test--anchor-edit
                                     "café" "replace" "naïve"))))
                  root)))
            (should (equal (plist-get result :source_kind) "file"))
            (should (equal (with-temp-buffer
                             (insert-file-contents path)
                             (buffer-string))
                           "naïve\nkeep\n"))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents-literally path)
              (should (equal (buffer-string)
                             (encode-coding-string
                              "naïve\nkeep\n" 'utf-8-dos))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ende-test/project-tools-edit-file-detects-a-stale-source-before-writing ()
  (let* ((root (make-temp-file "ende-tools-direct-project-" t))
         (path (expand-file-name "main.el" root))
         (ende-project-root-directory root)
         (original (symbol-function #'ende-tools--apply-change-set-object)))
    (unwind-protect
        (progn
          (with-temp-file path (insert "original\n"))
          (cl-letf (((symbol-function #'ende-tools--apply-change-set-object)
                     (lambda (change-set)
                       (with-temp-file path (insert "changed elsewhere\n"))
                       (funcall original change-set))))
            (let ((failure
                   (ende-test--edit-file-call
                    "main.el"
                    (list (ende-test--anchor-edit
                           "original" "replace" "new"))
                    root)))
              (should-not (plist-get failure :ok))
              (should (string-match-p "source changed"
                                      (plist-get failure :error))))
            (should (equal (with-temp-buffer
                             (insert-file-contents path)
                             (buffer-string))
                           "changed elsewhere\n"))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ende-test/project-tools-edit-file-leaves-disk-unchanged-on-invalid-batches ()
  (let* ((root (make-temp-file "ende-tools-invalid-edits-" t))
         (path (expand-file-name "main.txt" root))
         (ende-project-root-directory root))
    (unwind-protect
        (progn
          (with-temp-file path (insert "one one"))
          (dolist (request
                   (list
                    (list (cons "path" "main.txt")
                          (cons "edits"
                                (list (ende-test--anchor-edit
                                       "one" "replace" "two"))))
                    (list (cons "path" "main.txt")
                          (cons "edits"
                                (list (ende-test--anchor-edit
                                       "missing" "replace" "two"))))
                    (list (cons "path" "main.txt")
                          (cons "edits"
                                (list (ende-test--anchor-edit
                                       "one" "replace" "two")
                                      (ende-test--anchor-edit
                                       "absent" "after" "!"))))
                    (list (cons "path" "main.txt")
                          (cons "buffer_id" "not-accepted")
                          (cons "base_digest" "not-accepted")
                          (cons "edits" nil))
                    (list (cons "edits"
                                (list (ende-test--anchor-edit
                                       "one" "replace" "two"))))
                    (list (cons "path" "main.txt")
                          (cons "edits" nil))
                    (list (cons "path" "main.txt")
                          (cons "edits"
                                (list (append
                                       (ende-test--anchor-edit
                                        "one" "replace" "two")
                                       '(("reason" . "unknown"))))))))
            (should-not (plist-get (ende-tool-call "edit_file" request root)
                                   :ok))
            (should (equal (with-temp-buffer
                             (insert-file-contents path)
                             (buffer-string))
                           "one one")))
          (with-temp-file path (insert "abcdef"))
          (let ((overlap
                 (ende-test--edit-file-call
                  "main.txt"
                  (list (ende-test--anchor-edit
                         "abc" "replace" "A")
                        (ende-test--anchor-edit
                         "bcd" "replace" "B"))
                  root)))
            (should-not (plist-get overlap :ok))
            (should (equal (with-temp-buffer
                             (insert-file-contents path)
                             (buffer-string))
                           "abcdef"))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ende-test/project-tools-edit-file-anchor-positions-and-selection ()
  (let* ((root (make-temp-file "ende-tools-anchors-" t))
         (path (expand-file-name "main.txt" root))
         (ende-project-root-directory root))
    (unwind-protect
        (progn
          (with-temp-file path (insert "hello world\n"))
          (should (plist-get
                   (ende-test--edit-file-call
                    "main.txt"
                    (list (ende-test--anchor-edit "world" "before" "[")
                          (ende-test--anchor-edit "world" "after" "]"))
                    root)
                   :ok))
          (should (equal (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string))
                         "hello [world]\n"))
          (with-temp-file path (insert "ab"))
          (should (plist-get
                   (ende-test--edit-file-call
                    "main.txt"
                    (list (ende-test--anchor-edit "a" "after" "!")
                          (ende-test--anchor-edit "b" "replace" "B"))
                    root)
                   :ok))
          (should (equal (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string))
                         "a!B"))
          (with-temp-file path (insert "one one"))
          (should (plist-get
                   (ende-test--edit-file-call
                    "main.txt"
                    (list (ende-test--anchor-edit
                           "one" "replace" "two" '("occurrence" . 2)))
                    root)
                   :ok))
          (should (equal (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string))
                         "one two"))
          (with-temp-file path (insert "one one"))
          (should (plist-get
                   (ende-test--edit-file-call
                    "main.txt"
                    (list (ende-test--anchor-edit
                           "one" "replace" "ONE" '("all" . t)))
                    root)
                   :ok))
          (should (equal (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string))
                         "ONE ONE"))
          (with-temp-file path (insert "one one"))
          (should-not (plist-get
                       (ende-test--edit-file-call
                        "main.txt"
                        (list (ende-test--anchor-edit
                               "one" "replace" "ONE" '("all" . :false)))
                        root)
                       :ok))
          (should-not (plist-get
                       (ende-test--edit-file-call
                        "main.txt"
                        (list (ende-test--anchor-edit
                               "one" "replace" "ONE"
                               '("occurrence" . 1) '("all" . t)))
                        root)
                       :ok))
          (should (equal (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string))
                         "one one"))
          (with-temp-file path (insert "remove this keep"))
          (should (plist-get
                   (ende-test--edit-file-call
                    "main.txt"
                    (list (ende-test--anchor-edit
                           "remove this " "replace" ""))
                    root)
                   :ok))
          (should (equal (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string))
                         "keep"))
          (with-temp-file path (insert "first middle last"))
          (should (plist-get
                   (ende-test--edit-file-call
                    "main.txt"
                    (list (ende-test--anchor-edit
                           "first" "replace" "a much longer first")
                          (ende-test--anchor-edit
                           "last" "replace" "end"))
                    root)
                   :ok))
          (should (equal (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string))
                         "a much longer first middle end"))
          (with-temp-file path (insert "Hello"))
          (should-not (plist-get
                       (ende-test--edit-file-call
                        "main.txt"
                        (list (ende-test--anchor-edit
                               "hello" "replace" "bye"))
                        root)
                       :ok))
          (should (equal (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string))
                         "Hello")))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ende-test/project-tools-edit-file-honors-edit-expansion-limits ()
  (let* ((root (make-temp-file "ende-tools-edit-limits-" t))
         (path (expand-file-name "main.txt" root))
         (ende-project-root-directory root))
    (unwind-protect
        (progn
          (with-temp-file path (insert "a b"))
          (let* ((ende-tools-max-edits 1)
                 (too-many
                  (ende-test--edit-file-call
                   "main.txt"
                   (list (ende-test--anchor-edit "a" "replace" "A")
                         (ende-test--anchor-edit "b" "replace" "B"))
                   root)))
            (should-not (plist-get too-many :ok))
            (should (equal (with-temp-buffer
                             (insert-file-contents path)
                             (buffer-string))
                           "a b")))
          (with-temp-file path (insert "x x"))
          (let* ((ende-max-normalized-edits 1)
                 (expansion
                  (ende-test--edit-file-call
                   "main.txt"
                   (list (ende-test--anchor-edit
                          "x" "replace" "X" '("all" . t)))
                   root)))
            (should-not (plist-get expansion :ok))
            (should (equal (with-temp-buffer
                             (insert-file-contents path)
                             (buffer-string))
                           "x x"))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ende-test/project-tools-create-file-applies-immediately ()
  (let* ((root (make-temp-file "ende-tools-create-project-" t))
         (directory (expand-file-name "src" root))
         (path (expand-file-name "new.el" directory))
         (ende-project-root-directory root))
    (unwind-protect
        (progn
          (make-directory directory)
          (let ((result
                 (ende-test--tool-result
                  "create_file"
                  '(("path" . "src/new.el")
                    ("content" . ";;; new.el --- sample\n(provide 'new)\n"))
                  root)))
            (should (equal (plist-get result :operation) "create"))
            (should (equal (plist-get result :status) "applied"))
            (should (equal (plist-get result :path) "src/new.el"))
            (should-not (plist-member result :change_set_id))
            (should-not (plist-member result :content))
            (should (equal (with-temp-buffer
                             (insert-file-contents path)
                             (buffer-string))
                           ";;; new.el --- sample\n(provide 'new)\n")))
          (let* ((result
                  (ende-test--tool-result
                   "create_file"
                   '(("path" . "src/empty.el") ("content" . ""))
                   root))
                 (empty-path (expand-file-name "empty.el" directory)))
            (should (equal (plist-get result :status) "applied"))
            (should (file-exists-p empty-path))
            (should (= (file-attribute-size (file-attributes empty-path)) 0))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ende-test/project-tools-create-file-refuses-existing-target ()
  (let* ((root (make-temp-file "ende-tools-create-direct-" t))
         (existing (expand-file-name "existing.el" root))
         (new (expand-file-name "new.el" root))
         (ende-project-root-directory root))
    (unwind-protect
        (progn
          (with-temp-file existing (insert "original"))
          (let ((failure
                 (ende-test--create-file-call "existing.el" "overwrite" root)))
            (should-not (plist-get failure :ok))
            (should (string-match-p "already exists; use edit_file"
                                    (plist-get failure :error)))
            (should (equal (with-temp-buffer
                             (insert-file-contents existing)
                             (buffer-string))
                           "original")))
          (let ((result (ende-test--tool-result
                         "create_file"
                         '(("path" . "new.el") ("content" . "created\n"))
                         root)))
            (should (equal (plist-get result :operation) "create"))
            (should (equal (plist-get result :status) "applied"))
            (should-not (plist-member result :change_set_id))
            (should (equal (with-temp-buffer
                             (insert-file-contents new)
                             (buffer-string))
                           "created\n"))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ende-test/file-tools-mutate-authorized-external-files ()
  (let* ((root (make-temp-file "ende-tools-mutation-project-" t))
         (external-root (make-temp-file "ende-tools-mutation-external-" t))
         (allowed-directory (expand-file-name "allowed" external-root))
         (blocked-directory (expand-file-name "blocked" external-root))
         (path (expand-file-name "allowed/new.txt" external-root))
         (blocked-path (expand-file-name "blocked/no.txt" external-root)))
    (unwind-protect
        (progn
          (make-directory allowed-directory t)
          (make-directory blocked-directory t)
          (let* ((configuration
                  (ende-test--tool-access-config
                   root
                   `((resource external :root ,external-root
                               (include "allowed/**"))
                     (tools read_file file_info edit_file create_file delete_file)
                     (set-tool-resources read_file file_info edit_file create_file
                                         delete_file :resources external))))
                 (created (ende-tool-call
                           'create_file
                           (list :path path :content "first version\n")
                           root configuration)))
            (should (plist-get created :ok))
            (should (equal (plist-get (plist-get created :result) :path)
                           path))
            (let ((read (ende-tool-call 'read_file (list :path path)
                                        root configuration)))
              (should (plist-get read :ok))
              (should (equal (plist-get (plist-get read :result) :content)
                             "first version\n")))
            (let ((edited
                   (ende-tool-call
                    'edit_file
                    (list :path path
                          :edits (list (ende-test--anchor-edit
                                        "first version" "replace" "second version")))
                    root configuration)))
              (should (plist-get edited :ok))
              (should (equal (plist-get (plist-get edited :result) :path)
                             path)))
            (let ((deleted (ende-tool-call 'delete_file (list :path path)
                                           root configuration)))
              (should (plist-get deleted :ok))
              (should (equal (plist-get (plist-get deleted :result) :path)
                             path))
              (should-not (file-exists-p path)))
            (let ((denied (ende-tool-call
                           'create_file
                           (list :path blocked-path :content "not allowed")
                           root configuration)))
              (should-not (plist-get denied :ok))
              (should-not (file-exists-p blocked-path)))))
      (when (file-directory-p root)
        (delete-directory root t))
      (when (file-directory-p external-root)
        (delete-directory external-root t)))))

(ert-deftest ende-test/file-tools-reject-external-symlink-escape ()
  (let* ((root (make-temp-file "ende-tools-symlink-project-" t))
         (external-root (make-temp-file "ende-tools-symlink-external-" t))
         (outside-root (make-temp-file "ende-tools-symlink-outside-" t))
         (allowed-directory (expand-file-name "allowed" external-root))
         (link (expand-file-name "allowed/escape" external-root))
         (outside-file (expand-file-name "secret.txt" outside-root)))
    (unwind-protect
        (progn
          (make-directory allowed-directory t)
          (with-temp-file outside-file (insert "secret\n"))
          (make-symbolic-link outside-root link)
          (let* ((configuration
                  (ende-test--tool-access-config
                   root
                   `((resource external :root ,external-root
                               (include "allowed/**"))
                     (tools read_file)
                     (set-tool-resources read_file :resources external))))
                 (escaped
                  (ende-tool-call
                   'read_file
                   (list :path (expand-file-name "secret.txt" link))
                   root configuration)))
            (should-not (plist-get escaped :ok))
            (should (file-exists-p outside-file))))
      (dolist (directory (list root external-root outside-root))
        (when (file-directory-p directory)
          (delete-directory directory t))))))

(ert-deftest ende-test/project-tools-create-file-is-exclusive-at-write-time ()
  (let* ((root (make-temp-file "ende-tools-create-race-" t))
         (path (expand-file-name "race.el" root))
         (ende-project-root-directory root)
         (original (symbol-function #'ende-tools--write-new-file)))
    (unwind-protect
        (cl-letf (((symbol-function #'ende-tools--write-new-file)
                   (lambda (content target)
                     (with-temp-file target (insert "won the race"))
                     (funcall original content target))))
          (let ((failure
                 (ende-test--create-file-call "race.el" "proposal" root)))
            (should-not (plist-get failure :ok)))
          (should (equal (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string))
                         "won the race")))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ende-test/project-tools-create-file-validates-path-and-symlinks ()
  (let* ((root (make-temp-file "ende-tools-create-paths-" t))
         (outside (make-temp-file "ende-tools-create-outside-" t))
         (link (expand-file-name "dangling-link" root))
         (absolute (expand-file-name "absolute-target" root))
         (ende-project-root-directory root))
    (unwind-protect
        (progn
          (make-symbolic-link "missing-target" link)
          (make-symbolic-link outside (expand-file-name "outside-dir" root))
          (dolist (path (list "missing-dir/new.el"
                              "../outside.el"
                              "outside-dir/escaped.el"
                              absolute
                              ".env"
                              "dangling-link"))
            (should-not (plist-get
                         (ende-test--create-file-call path "content" root)
                         :ok)))
          (should (file-symlink-p link))
          (should-not (file-exists-p (expand-file-name "missing-dir" root)))
          (should-not (file-exists-p absolute)))
      (when (file-directory-p root)
        (delete-directory root t))
      (when (file-directory-p outside)
        (delete-directory outside t)))))

(ert-deftest ende-test/project-tools-delete-file-checks-digest-and-buffer-state ()
  (let* ((root (make-temp-file "ende-tools-delete-" t))
         (path (expand-file-name "src/main.el" root))
         (outside (make-temp-file "ende-tools-delete-outside-"))
         (link (expand-file-name "src/link.el" root))
         (buffer nil))
    (unwind-protect
        (progn
          (make-directory (file-name-directory path))
          (with-temp-file path (insert "disk source\n"))
          (make-symbolic-link outside link)
          (let* ((info (ende-test--tool-result
                        "file_info" '(("path" . "src/main.el")) root))
                 (digest (plist-get info :digest))
                 (stale (ende-tool-call
                         "delete_file"
                         (list :path "src/main.el" :expected_digest (make-string 64 ?0))
                         root)))
            (should-not (plist-get stale :ok))
            (should (file-exists-p path))
            (should
             (string-match-p
              "symlink"
              (plist-get
               (ende-tool-call "delete_file"
                               (list :path "src/link.el") root)
               :error)))
            (dolist (bad-path
                     (list "../outside" (expand-file-name "src/main.el" root)
                           "src/../src/main.el" "src/" "src"))
              (should-not (plist-get
                           (ende-tool-call "delete_file"
                                           (list :path bad-path) root)
                           :ok)))
            (setq buffer (find-file-noselect path))
            (let ((deleted
                   (ende-tool-call
                    "delete_file"
                    (list :path "src/main.el" :expected_digest digest) root)))
              (should (plist-get deleted :ok))
              (let ((result (plist-get deleted :result)))
                (should (equal (plist-get result :operation) "delete"))
                (should (equal (plist-get result :status) "applied"))
                (should (equal (plist-get result :path) "src/main.el"))
                (should (plist-get result :visiting_buffer))
                (should (= (plist-get result :visiting_buffer_count) 1))
                (should (string-match-p "recreate"
                                        (plist-get result
                                                   :visiting_buffer_warning))))
              (should-not (file-exists-p path))
              (should (buffer-live-p buffer))
              (with-current-buffer buffer
                (should (equal (buffer-string) "disk source\n"))
                (should-not (buffer-modified-p))))
            (with-current-buffer buffer
              (erase-buffer)
              (insert "unsaved text\n")
              (set-buffer-modified-p t))
            (with-temp-file path (insert "disk source\n"))
            (let ((failure (ende-tool-call
                            "delete_file" '(("path" . "src/main.el")) root)))
              (should-not (plist-get failure :ok))
              (should (string-match-p "unsaved visiting buffer"
                                      (plist-get failure :error)))
              (should (file-exists-p path))
              (with-current-buffer buffer
                (should (equal (buffer-string) "unsaved text\n"))
                (should (buffer-modified-p))))))
      (when (buffer-live-p buffer)
        (set-buffer-modified-p nil)
        (kill-buffer buffer))
      (when (file-directory-p root)
        (delete-directory root t))
      (when (file-exists-p outside)
        (delete-file outside)))))

(ert-deftest ende-test/project-tools-delete-honors-trash-without-fallback ()
  (let* ((root (make-temp-file "ende-tools-trash-project-" t))
         (trash (make-temp-file "ende-tools-trash-dir-" t))
         (path (expand-file-name "discard.txt" root))
         (destination (expand-file-name "discard.txt" trash))
         (delete-by-moving-to-trash t)
         (calls 0))
    (unwind-protect
        (progn
          (with-temp-file path (insert "trash me\n"))
          (cl-letf (((symbol-function 'move-file-to-trash)
                     (lambda (_path)
                       (cl-incf calls)
                       (error "trash backend failed"))))
            (let ((failure (ende-tool-call
                            "delete_file" '(("path" . "discard.txt")) root)))
              (should-not (plist-get failure :ok))
              (should (string-match-p "trash backend failed"
                                      (plist-get failure :error)))
              (should (= calls 1))
              (should (file-exists-p path))))
          (cl-letf (((symbol-function 'move-file-to-trash)
                     (lambda (source)
                       (cl-incf calls)
                       (rename-file source destination))))
            (let ((result (ende-tool-call
                           "delete_file" '(("path" . "discard.txt")) root)))
              (should (plist-get result :ok))
              (should (= calls 2))
              (should-not (file-exists-p path))
              (should (file-exists-p destination)))))
      (when (file-directory-p root)
        (delete-directory root t))
      (when (file-directory-p trash)
        (delete-directory trash t)))))

(ert-deftest ende-test/project-tools-apply-patch-commits-mixed-disk-operations ()
  (let* ((root (make-temp-file "ende-tools-patch-project-" t))
         (source (expand-file-name "src/main.txt" root))
         (added (expand-file-name "src/new.txt" root))
         (deleted (expand-file-name "tmp/old.txt" root))
         (buffer nil)
         (patch (concat
                 "*** Begin Patch\n"
                 "*** Add File: src/new.txt\n+new text\n"
                 "*** Update File: src/main.txt\n@@\n before\n-old\n+updated\n"
                 "*** Delete File: tmp/old.txt\n"
                 "*** End Patch\n")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory source) t)
          (make-directory (file-name-directory deleted) t)
          (with-temp-buffer
            (insert "before\nold\n")
            (let ((coding-system-for-write 'utf-8-dos))
              (write-region (point-min) (point-max) source nil 'silent)))
          (set-file-modes source #o640)
          (with-temp-file deleted (insert "discard\n"))
          (setq buffer (find-file-noselect source))
          (let ((result (ende-test--apply-patch-call patch root)))
            (should (plist-get result :ok))
            (let* ((payload (plist-get result :result))
                   (files (plist-get payload :files)))
              (should (vectorp files))
              (should (= (length files) 3))
              (should (equal (mapcar (lambda (file)
                                       (plist-get file :operation))
                                     (append files nil))
                             '("add" "update" "delete")))
              (should (equal (mapcar (lambda (file)
                                       (plist-get file :status))
                                     (append files nil))
                             '("applied" "applied" "applied")))
              (should (plist-get (nth 1 (append files nil))
                                 :visiting_buffer_warning)))
            (should (equal (with-temp-buffer
                             (insert-file-contents added)
                             (buffer-string))
                           "new text\n"))
            (should (equal (with-temp-buffer
                             (insert-file-contents source)
                             (buffer-string))
                           "before\nupdated\n"))
            (should (= (logand (file-modes source) #o777) #o640))
            (should-not (file-exists-p deleted))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents-literally source)
              (should (equal (buffer-string)
                             (encode-coding-string
                              "before\nupdated\n" 'utf-8-dos))))
            (with-current-buffer buffer
              (should (equal (buffer-string) "before\nold\n"))
              (should-not (buffer-modified-p)))))
      (when (buffer-live-p buffer)
        (set-buffer-modified-p nil)
        (kill-buffer buffer))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ende-test/file-tools-apply-patch-mixes-project-and-external-targets ()
  (let* ((root (make-temp-file "ende-tools-patch-project-root-" t))
         (external-root (make-temp-file "ende-tools-patch-external-root-" t))
         (project-file (expand-file-name "src/main.txt" root))
         (external-file (expand-file-name "emacs/init.el" external-root)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory project-file) t)
          (make-directory (file-name-directory external-file) t)
          (with-temp-file project-file (insert "project before\nold\n"))
          (with-temp-file external-file (insert "external before\nold\n"))
          (let* ((configuration
                  (ende-test--tool-access-config
                   root
                   `((resource project (include "src/**"))
                     (resource dotfiles :root ,external-root
                               (include "emacs/**"))
                     (tools apply_patch)
                     (set-tool-resources apply_patch :resources project dotfiles))))
                 (patch
                  (concat "*** Begin Patch\n"
                          "*** Update File: src/main.txt\n"
                          "@@\n project before\n-old\n+project updated\n"
                          "*** Update File: " external-file "\n"
                          "@@\n external before\n-old\n+external updated\n"
                          "*** End Patch\n"))
                 (result (ende-test--apply-patch-call
                          patch root configuration))
                 (entries (append
                           (plist-get (plist-get result :result) :files)
                           nil)))
            (should (plist-get result :ok))
            (should (equal (mapcar (lambda (entry) (plist-get entry :path))
                                   entries)
                           (list "src/main.txt" external-file)))
            (should (equal (with-temp-buffer
                             (insert-file-contents project-file)
                             (buffer-string))
                           "project before\nproject updated\n"))
            (should (equal (with-temp-buffer
                             (insert-file-contents external-file)
                             (buffer-string))
                           "external before\nexternal updated\n")))
          ;; A project-only patch grant rejects the external operation before
          ;; committing even the authorized project operation in the batch.
          (let* ((project-only
                  (ende-test--tool-access-config
                   root
                   `((resource project (include "src/**"))
                     (tools apply_patch)
                     (set-tool-resources apply_patch :resources project))))
                 (denied-patch
                  (concat "*** Begin Patch\n"
                          "*** Update File: src/main.txt\n"
                          "@@\n project before\n-project updated\n+should stay\n"
                          "*** Update File: " external-file "\n"
                          "@@\n external before\n-external updated\n+should stay\n"
                          "*** End Patch\n"))
                 (denied (ende-test--apply-patch-call
                          denied-patch root project-only)))
            (should-not (plist-get denied :ok))
            (should (equal (with-temp-buffer
                             (insert-file-contents project-file)
                             (buffer-string))
                           "project before\nproject updated\n"))
            (should (equal (with-temp-buffer
                             (insert-file-contents external-file)
                             (buffer-string))
                           "external before\nexternal updated\n"))))
      (when (file-directory-p root)
        (delete-directory root t))
      (when (file-directory-p external-root)
        (delete-directory external-root t)))))

(ert-deftest ende-test/project-tools-apply-patch-preflight-leaves-files-alone ()
  (let* ((root (make-temp-file "ende-tools-patch-preflight-" t))
         (path (expand-file-name "main.txt" root))
         (buffer nil))
    (unwind-protect
        (progn
          (with-temp-file path (insert "first\nsecond\n"))
          (setq buffer (find-file-noselect path))
          (with-current-buffer buffer
            (goto-char (point-max))
            (insert "unsaved\n")
            (set-buffer-modified-p t))
          (let ((dirty-patch
                 (concat "*** Begin Patch\n"
                         "*** Add File: new.txt\n+never written\n"
                         "*** Update File: main.txt\n@@\n first\n-second\n+changed\n"
                         "*** End Patch\n")))
            (let ((failure (ende-test--apply-patch-call dirty-patch root)))
              (should-not (plist-get failure :ok))
              (should (string-match-p "unsaved visiting buffer"
                                      (plist-get failure :error)))
              (should-not (file-exists-p (expand-file-name "new.txt" root)))
              (should (equal (with-temp-buffer
                               (insert-file-contents path)
                               (buffer-string))
                             "first\nsecond\n"))
              (with-current-buffer buffer
                (should (string-suffix-p "unsaved\n" (buffer-string)))
                (should (buffer-modified-p)))))
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (dolist (patch
                   (list
                    "*** Begin Patch\n*** Update File: main.txt\n@@\n -absent\n+bad\n*** End Patch"
                    "*** Begin Patch\n*** Add File: main.txt\n+overwrite\n*** End Patch"))
            (let ((before (with-temp-buffer
                            (insert-file-contents path)
                            (buffer-string))))
              (should-not (plist-get (ende-test--apply-patch-call patch root)
                                     :ok))
              (should (equal (with-temp-buffer
                               (insert-file-contents path)
                               (buffer-string))
                             before)))))
      (with-temp-file path (insert "first\nfirst\n"))
      (let ((ambiguous
             (concat "*** Begin Patch\n*** Update File: main.txt\n"
                     "@@\n first\n+insert\n*** End Patch\n")))
        (should-not (plist-get (ende-test--apply-patch-call ambiguous root)
                               :ok))
        (should (equal (with-temp-buffer
                         (insert-file-contents path)
                         (buffer-string))
                       "first\nfirst\n")))
      (when (buffer-live-p buffer)
        (set-buffer-modified-p nil)
        (kill-buffer buffer))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ende-test/project-tools-apply-patch-rolls-back-failed-commit ()
  (let* ((root (make-temp-file "ende-tools-patch-rollback-" t))
         (first (expand-file-name "first.txt" root))
         (second (expand-file-name "second.txt" root))
         (patch (concat "*** Begin Patch\n"
                        "*** Update File: first.txt\n@@\n before\n-old\n+new\n"
                        "*** Update File: second.txt\n@@\n before\n-old\n+new\n"
                        "*** End Patch\n"))
         (commit (symbol-function #'ende-tools--commit-change-set))
         (count 0))
    (unwind-protect
        (progn
          (with-temp-file first (insert "before\nold\n"))
          (with-temp-file second (insert "before\nold\n"))
          (cl-letf (((symbol-function 'ende-tools--commit-change-set)
                     (lambda (change-set)
                       (prog1 (funcall commit change-set)
                         (cl-incf count)
                         (when (= count 1)
                           (error "injected commit failure"))))))
            (let* ((failure (ende-test--apply-patch-call patch root))
                   (report (plist-get failure :recovery)))
              (should-not (plist-get failure :ok))
              (should (string-match-p "injected commit failure"
                                      (plist-get failure :error)))
              (should (vectorp report))
              (should (= (length report) 2))
              (let ((first-report
                     (cl-find-if (lambda (entry)
                                   (equal (plist-get entry :path) "first.txt"))
                                 (append report nil)))
                    (second-report
                     (cl-find-if (lambda (entry)
                                   (equal (plist-get entry :path) "second.txt"))
                                 (append report nil))))
                (should (equal (plist-get first-report :status) "restored"))
                (should (equal (plist-get second-report :status)
                               "not_applied")))
              (dolist (path (list first second))
                (should (equal (with-temp-buffer
                                 (insert-file-contents path)
                                 (buffer-string))
                               "before\nold\n"))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ende-test/project-tools-apply-patch-retains-unsafe-recovery ()
  (let* ((root (make-temp-file "ende-tools-patch-recovery-" t))
         (path (expand-file-name "source.txt" root))
         (patch (concat "*** Begin Patch\n*** Update File: source.txt\n"
                        "@@\n before\n-old\n+patched\n*** End Patch\n"))
         (commit (symbol-function #'ende-tools--commit-change-set))
         (recovery nil))
    (unwind-protect
        (progn
          (with-temp-file path (insert "before\nold\n"))
          (cl-letf (((symbol-function 'ende-tools--commit-change-set)
                     (lambda (change-set)
                       (prog1 (funcall commit change-set)
                         (with-temp-file path (insert "concurrent edit\n"))
                         (error "fail after concurrent change")))))
            (let* ((failure (ende-test--apply-patch-call patch root))
                   (report (plist-get failure :recovery))
                   (entry (aref report 0)))
              (should-not (plist-get failure :ok))
              (should (equal (plist-get entry :status)
                             "concurrent_change_preserved"))
              (setq recovery (expand-file-name
                              (plist-get entry :recovery_file) root))
              (should (file-exists-p recovery))
              (should (= (logand (file-modes recovery) #o777) #o600))
              (should (equal (with-temp-buffer
                               (insert-file-contents recovery)
                               (buffer-string))
                             "before\nold\n"))
              (should (equal (with-temp-buffer
                               (insert-file-contents path)
                               (buffer-string))
                             "concurrent edit\n")))))
      (when (and recovery (file-exists-p recovery))
        (delete-file recovery))
      (when (file-directory-p root)
        (delete-directory root t)))))

(provide 'ende-tools-test)

;;; ende-tools-test.el ends here
