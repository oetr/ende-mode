;;; ende-sandbox-test.el --- Podman sandbox tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Behavior tests for authorization, persistence, and lifecycle boundaries.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ende)
(require 'ende-test-helpers)
(require 'ende-chat-test-helpers)

(defconst ende-test--sandbox-pin
  (concat "registry.example/ende-test@sha256:" (make-string 64 ?a)))

(defvar ende-test--sandbox-owner 700000)
(defvar ende-test--sandbox-image-cache :unset)

(defun ende-test--sandbox-config-forms (root forms &optional image)
  "Resolve a sandbox-enabled chat policy for ROOT and data FORMS."
  (with-temp-buffer
    (org-mode)
    (insert (format "#+begin_ende\n(root %S)\n" root))
    (when image
      (insert (format "(podman-image %S)\n" image)))
    (dolist (form forms)
      (insert (format "%S\n" form)))
    (insert "#+end_ende\n\n* Sandbox\n")
    (let* ((document (ende-chat--build-document))
           (job (car (ende-chat--document-headlines document))))
      (ende-chat-config-resolve
       (ende-chat--document-tree document)
       (ende-chat--document-parent document) job))))

(defun ende-test--sandbox-config (root &optional image)
  "Resolve a sandbox-enabled chat policy for ROOT."
  (ende-test--sandbox-config-forms
   root
   '((resource project (include "**") (exclude "private/**"))
     (resource output (include "out/**"))
     (tools run_command copy_from_sandbox list_project_files
            edit_file create_file delete_file)
     (set-tool-resources run_command list_project_files edit_file
                         create_file delete_file :resources project)
     (set-tool-resources copy_from_sandbox :resources output))
   image))

(defun ende-test--sandbox-workspace (sandbox relative)
  "Return RELATIVE path in SANDBOX's workspace."
  (expand-file-name relative (ende-sandbox-workspace sandbox)))

(defun ende-test--sandbox-fixture (owner root files)
  "Register a workspace snapshot for OWNER from FILES under ROOT."
  (let* ((base (make-temp-file "ende-sandbox-fixture-" t))
         (workspace (expand-file-name "workspace" base))
         (sandbox nil))
    (make-directory workspace)
    (dolist (source files)
      (let* ((relative (file-relative-name source root))
             (destination (expand-file-name relative workspace)))
        (make-directory (file-name-directory destination) t)
        (copy-file source destination)))
    (setq sandbox
          (ende-sandbox--create :owner owner :root root :base base
                                :workspace workspace))
    (puthash owner sandbox ende-sandbox--by-owner)
    sandbox))

(defun ende-test--sandbox-workspace-ready-p ()
  "Return non-nil when rootless Podman can create a tmpfs workspace here."
  (and (not (zerop (user-uid)))
       (executable-find "podman")
       (file-directory-p "/dev/shm")
       (file-writable-p "/dev/shm")
       (ende-sandbox--tmpfs-p (file-truename "/dev/shm"))))

(defun ende-test--sandbox-call (name arguments root configuration owner)
  "Call tool NAME and wait for its async or synchronous result."
  (let ((done nil)
        (result nil)
        (deadline (+ (float-time) 150.0))
        (ende-tools--execution-owner owner))
    (ende-tool-call-async
     name arguments root configuration
     (lambda (value)
       (setq result value done t)))
    (while (and (not done) (< (float-time) deadline))
      (accept-process-output nil 0.1))
    (unless done
      (error "timed out waiting for tool %s" name))
    result))

(defun ende-test--sandbox-run (arguments root configuration owner)
  "Run command ARGUMENTS for OWNER and return its structured result."
  (let ((call (ende-test--sandbox-call
               "run_command" arguments root configuration owner)))
    (should (plist-get call :ok))
    (plist-get call :result)))

(defun ende-test--sandbox-command-stderr (stderr)
  "Return command STDERR after any structured Podman diagnostic lines."
  (let ((diagnostics-end
         (if (string-match
              (concat
               "\\`\\(?:time=\"[^\"]+\" level=[[:alpha:]]+ "
               "msg=\"[^\n]*\"\n\\)*")
              stderr)
             (match-end 0)
           0)))
    (substring stderr diagnostics-end)))

(defun ende-test--sandbox-image-available-p (image)
  "Return non-nil if IMAGE is already available to local rootless Podman."
  (and (executable-find "podman")
       (not (zerop (user-uid)))
       (with-temp-buffer
         (and (zerop (call-process "podman" nil t nil "image" "exists" image))
              (zerop (call-process
                      "podman" nil t nil "info" "--format"
                      "{{.Host.Security.Rootless}}"))
              (string-match-p "true" (buffer-string))))))

(defun ende-test--sandbox-image ()
  "Return a locally installed digest-pinned image suitable for testing."
  (if (not (eq ende-test--sandbox-image-cache :unset))
      ende-test--sandbox-image-cache
    (setq ende-test--sandbox-image-cache
          (let ((configured (getenv "ENDE_PODMAN_TEST_IMAGE")))
            (cond
             ((and configured
                   (string-match-p
                    "\\`[[:graph:]]+@sha256:[[:xdigit:]]\\{64\\}\\'"
                    configured)
                   (ende-test--sandbox-image-available-p configured))
              configured)
             ((not (executable-find "podman")) nil)
             (t
              (with-temp-buffer
                (when (zerop
                       (call-process
                        "podman" nil t nil "images" "--format"
                        "{{.Repository}}@{{.Digest}}"))
                  (let ((images (split-string (buffer-string) "\n" t)))
                    (or (cl-find-if
                         (lambda (image)
                           (and (string-prefix-p "localhost/ende-dev@" image)
                                (ende-test--sandbox-image-available-p image)))
                         images)
                        (cl-find-if
                         (lambda (image)
                           (and (string-prefix-p
                                 "docker.io/library/alpine@" image)
                                (ende-test--sandbox-image-available-p image)))
                         images)
                        (cl-find-if
                         (lambda (image)
                           (and (not (string-prefix-p "<none>@" image))
                                (string-match-p
                                 "@sha256:[[:xdigit:]]\\{64\\}\\'" image)
                                (ende-test--sandbox-image-available-p image)))
                         images)))))))))))

(defun ende-test--sandbox-make-project (root)
  "Create a small project tree under ROOT."
  (make-directory (expand-file-name "public" root) t)
  (make-directory (expand-file-name "private" root) t)
  (make-directory (expand-file-name "out" root) t)
  (with-temp-file (expand-file-name "public/main.c" root)
    (insert "int main(void) { return 23; }\n"))
  (with-temp-file (expand-file-name "public/second.txt" root)
    (insert "another authorized file\n"))
  (with-temp-file (expand-file-name "private/secret.txt" root)
    (insert "not authorized\n"))
  (with-temp-file (expand-file-name ".env" root)
    (insert "TOKEN=never copied\n")))

(defun ende-test--sandbox-wait-clean (owners)
  "Wait until OWNER sandboxes have completed asynchronous cleanup."
  (let ((deadline (+ (float-time) 60.0)))
    (while (and (< (float-time) deadline)
                (cl-some #'ende-sandbox-for-owner owners))
      (accept-process-output nil 0.1)))
  (should-not (cl-some #'ende-sandbox-for-owner owners)))

(ert-deftest ende-test/sandbox-config-inherits-only-pinned-images ()
  (let* ((root (make-temp-file "ende-sandbox-config-" t))
         (first (concat "registry.example/base@sha256:" (make-string 64 ?b)))
         (second (concat "registry.example/child@sha256:" (make-string 64 ?c))))
    (unwind-protect
        (progn
          (with-temp-buffer
            (org-mode)
            (insert (format
                     (concat "#+begin_ende\n(root %S)\n(podman-image %S)\n"
                             "(resource project (include \"**\"))\n"
                             "(tools run_command)\n"
                             "(set-tool-resources run_command :resources project)\n"
                             "#+end_ende\n* Parent\n** Child\n"
                             "#+begin_ende\n(podman-image %S)\n#+end_ende\n")
                     root first second))
            (let* ((document (ende-chat--build-document))
                   (child (cadr (ende-chat--document-headlines document)))
                   (configuration
                    (ende-chat-config-resolve
                     (ende-chat--document-tree document)
                     (ende-chat--document-parent document) child)))
              (should (equal (plist-get configuration :podman-image) second))))
          (should-error (ende-test--sandbox-config root nil)
                        :type 'error)
          (should-error (ende-test--sandbox-config root "ubuntu:latest")
                        :type 'error))
      (delete-directory root t))))

(ert-deftest ende-test/chat-tool-round-binds-its-job-id-for-dispatch ()
  (let* ((job (ende--make-job :id 765432 :state 'running :type 'chat))
         (call '(:id "tool-call" :name "probe" :arguments "{}"))
         (owner-seen nil)
         (round-finished nil)
         (ende-tools--execution-owner 'outer-owner))
    (cl-letf (((symbol-function 'ende-job-current-p)
               (lambda (_job) t))
              ((symbol-function 'ende-job-emit-event)
               (lambda (&rest _args) nil))
              ((symbol-function 'ende-chat--persist-request-section)
               (lambda (&rest _args) nil))
              ((symbol-function 'ende-chat--tool-result-text)
               (lambda (&rest _args) "ok"))
              ((symbol-function 'ende-chat--tool-response-async)
               (lambda (_call _configuration callback)
                 (setq owner-seen ende-tools--execution-owner)
                 (funcall callback "{}")))
              (ende-tools--execution-owner 'outer-owner))
      (ende-chat--execute-tool-round
       job (list call) nil 1 "request" nil
       (lambda (_messages) (setq round-finished t))))
    (should round-finished)
    (should (= owner-seen (ende-job-id job)))
    (should (eq ende-tools--execution-owner 'outer-owner))))

(ert-deftest ende-test/sandbox-snapshot-uses-tool-scope-without-list-limit ()
  (let* ((root nil)
         (owner (cl-incf ende-test--sandbox-owner))
         (sandbox nil))
    (unless (ende-test--sandbox-workspace-ready-p)
      (ert-skip "rootless Podman and a writable /dev/shm are required"))
    (setq root (make-temp-file "ende-sandbox-snapshot-" t))
    (unwind-protect
        (progn
          (ende-test--sandbox-make-project root)
          (let* ((configuration (ende-test--sandbox-config
                                 root ende-test--sandbox-pin))
                 (ende-tools-max-listed-files 1)
                 (ende-tools--access-config configuration)
                 (files (ende-tools--authorized-project-files
                         'run_command root)))
            (should (= (length files) 2))
            (let* ((listing (ende-tool-call
                             "list_project_files" nil root configuration))
                   (result (plist-get listing :result)))
              (should (plist-get listing :ok))
              (should (= (length (plist-get result :files)) 1))
              (should (eq (plist-get result :truncated) t)))
            (setq sandbox
                  (ende-sandbox-create owner root ende-test--sandbox-pin files))
            (should (file-in-directory-p
                     (ende-sandbox-workspace sandbox) "/dev/shm/"))
            (should (file-exists-p
                     (ende-test--sandbox-workspace sandbox "public/main.c")))
            (should (file-exists-p
                     (ende-test--sandbox-workspace sandbox "public")))
            (should-not (file-exists-p
                         (ende-test--sandbox-workspace
                          sandbox "private/secret.txt")))
            (should-not (file-exists-p
                         (ende-test--sandbox-workspace sandbox ".env")))
            (with-temp-file
                (ende-test--sandbox-workspace sandbox "public/main.c")
              (insert "sandbox edit\n"))
            (should (equal
                     (with-temp-buffer
                       (insert-file-contents (expand-file-name "public/main.c" root))
                       (buffer-string))
                     "int main(void) { return 23; }\n"))))
      (when sandbox
        (ende-sandbox-cleanup-owner owner))
      (delete-directory root t))))

(ert-deftest ende-test/sandbox-mirrors-committed-source-changes ()
  (let* ((root (make-temp-file "ende-sandbox-sync-" t))
         (source (expand-file-name "src/foo.el" root))
         (created (expand-file-name "src/generated/deeper/new.el" root))
         (owner (cl-incf ende-test--sandbox-owner))
         (sandbox nil))
    (unwind-protect
        (progn
          (make-directory (file-name-directory source) t)
          (make-directory (file-name-directory created) t)
          (with-temp-file source (insert "version A\n"))
          (let* ((configuration
                  (ende-test--sandbox-config-forms
                   root
                   '((resource source (include "src/**"))
                     (tools run_command edit_file create_file delete_file)
                     (set-tool-resources run_command :resources source)
                     (set-tool-resources edit_file create_file delete_file
                                         :resources source))
                   ende-test--sandbox-pin))
                 (ende-tools--execution-owner owner))
            (setq sandbox
                  (ende-test--sandbox-fixture owner root (list source)))
            (should (equal
                     (with-temp-buffer
                       (insert-file-contents
                        (ende-test--sandbox-workspace sandbox "src/foo.el"))
                       (buffer-string))
                     "version A\n"))
            (let ((edited
                   (ende-tool-call
                    'edit_file
                    (list :path "src/foo.el"
                          :edits (list (ende-test--anchor-edit
                                        "version A" "replace" "version B")))
                    root configuration)))
              (should (plist-get edited :ok)))
            (should (eq sandbox (ende-sandbox-for-owner owner)))
            (should (equal
                     (with-temp-buffer
                       (insert-file-contents
                        (ende-test--sandbox-workspace sandbox "src/foo.el"))
                       (buffer-string))
                     "version B\n"))
            (let ((created-call
                   (ende-tool-call
                    'create_file
                    (list :path "src/generated/deeper/new.el"
                          :content "created\n")
                    root configuration)))
              (should (plist-get created-call :ok)))
            (should (equal
                     (with-temp-buffer
                       (insert-file-contents
                        (ende-test--sandbox-workspace
                         sandbox "src/generated/deeper/new.el"))
                       (buffer-string))
                     "created\n"))
            (let ((deleted
                   (ende-tool-call 'delete_file '(:path "src/foo.el")
                                   root configuration)))
              (should (plist-get deleted :ok)))
            (should-not
             (file-exists-p
              (ende-test--sandbox-workspace sandbox "src/foo.el")))
            (let* ((sandbox-file
                    (ende-test--sandbox-workspace
                     sandbox "src/generated/deeper/new.el"))
                   (base (ende-sandbox-base sandbox)))
              (delete-file sandbox-file)
              (make-symbolic-link "/etc/passwd" sandbox-file)
              (let ((edited
                     (ende-tool-call
                      'edit_file
                      (list :path "src/generated/deeper/new.el"
                            :edits (list (ende-test--anchor-edit
                                          "created" "replace" "modified")))
                      root configuration)))
                (should (plist-get edited :ok)))
              (should (equal
                       (with-temp-buffer
                         (insert-file-contents created)
                         (buffer-string))
                       "modified\n"))
              (should-not (ende-sandbox-for-owner owner))
              (should-not (file-exists-p base))
              (setq sandbox nil))))
      (ende-sandbox-cleanup-owner owner)
      (delete-directory root t))))

(ert-deftest ende-test/sandbox-sync-does-not-bypass-run-command-scope ()
  (let* ((root (make-temp-file "ende-sandbox-sync-scope-" t))
         (source (expand-file-name "src/main.el" root))
         (secret (expand-file-name "private/secret.el" root))
         (owner (cl-incf ende-test--sandbox-owner))
         (sandbox nil))
    (unwind-protect
        (progn
          (make-directory (file-name-directory source) t)
          (make-directory (file-name-directory secret) t)
          (with-temp-file source (insert "source\n"))
          (with-temp-file secret (insert "secret A\n"))
          (let* ((configuration
                  (ende-test--sandbox-config-forms
                   root
                   '((resource source (include "src/**"))
                     (resource editable (include "private/**"))
                     (tools run_command edit_file)
                     (set-tool-resources run_command :resources source)
                     (set-tool-resources edit_file :resources editable))
                    ende-test--sandbox-pin))
                 (ende-tools--execution-owner owner))
            (setq sandbox
                  (ende-test--sandbox-fixture owner root (list source)))
            (let ((edited
                   (ende-tool-call
                    'edit_file
                    (list :path "private/secret.el"
                          :edits (list (ende-test--anchor-edit
                                        "secret A" "replace" "secret B")))
                    root configuration)))
              (should (plist-get edited :ok)))
            (should (eq sandbox (ende-sandbox-for-owner owner)))
            (should-not
             (file-exists-p
              (ende-test--sandbox-workspace sandbox "private/secret.el")))
            (should (equal
                      (with-temp-buffer
                        (insert-file-contents secret)
                        (buffer-string))
                     "secret B\n"))))
      (ende-sandbox-cleanup-owner owner)
      (delete-directory root t))))

(ert-deftest ende-test/sandbox-copy-back-rechecks-destination-and-symlinks ()
  (let* ((root nil)
         (owner (cl-incf ende-test--sandbox-owner))
         (sandbox nil)
         (image ende-test--sandbox-pin))
    (unless (ende-test--sandbox-workspace-ready-p)
      (ert-skip "rootless Podman and a writable /dev/shm are required"))
    (setq root (make-temp-file "ende-sandbox-copy-" t))
    (unwind-protect
        (progn
          (ende-test--sandbox-make-project root)
          (let ((configuration (ende-test--sandbox-config root image)))
            (setq sandbox (ende-sandbox-create owner root image nil))
            (ende-tools--write-private-bytes
             (ende-test--sandbox-workspace sandbox "generated.bin")
             (unibyte-string 0 1 127 128 255))
            (let* ((ende-tools--execution-owner owner)
                   (copied (ende-tool-call
                            "copy_from_sandbox"
                            '(("source" . "generated.bin")
                              ("destination" . "out/generated.bin"))
                            root configuration))
                   (denied (ende-tool-call
                            "copy_from_sandbox"
                            '(("source" . "generated.bin")
                              ("destination" . "public/main.c"))
                            root configuration)))
              (should (plist-get copied :ok))
              (should-not (plist-get denied :ok))
              (should (string-match-p "not authorized"
                                      (plist-get denied :error)))
              (should (equal
                       (ende-tools--literal-file-bytes
                        (expand-file-name "out/generated.bin" root))
                       (unibyte-string 0 1 127 128 255)))
              (should (equal
                       (with-temp-buffer
                         (insert-file-contents
                          (expand-file-name "public/main.c" root))
                         (buffer-string))
                       "int main(void) { return 23; }\n")))
            (make-symbolic-link "/etc/passwd"
                                (ende-test--sandbox-workspace sandbox "leak"))
            (let* ((ende-tools--execution-owner owner)
                   (denied (ende-tool-call
                            "copy_from_sandbox"
                            '(("source" . "leak")
                              ("destination" . "out/leak"))
                            root configuration)))
              (should-not (plist-get denied :ok))
              (should (string-match-p "symlinks"
                                      (plist-get denied :error)))
              (should-not (file-exists-p (expand-file-name "out/leak" root)))))
          (let ((base (ende-sandbox-base sandbox)))
            (ende-sandbox--job-event
             (ende--make-job :id owner) '(:type :settled))
            (should-not (ende-sandbox-for-owner owner))
            (should-not (file-exists-p base)))
          (setq sandbox nil))
      (when sandbox
        (ende-sandbox-cleanup-owner owner))
      (delete-directory root t))))

(ert-deftest ende-test/sandbox-cwd-rejects-absolute-escaping-and-symlink-paths ()
  (let* ((root nil)
         (owner (cl-incf ende-test--sandbox-owner))
         (sandbox nil))
    (unless (ende-test--sandbox-workspace-ready-p)
      (ert-skip "rootless Podman and a writable /dev/shm are required"))
    (setq root (make-temp-file "ende-sandbox-cwd-" t))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "subdir" root))
          (setq sandbox (ende-sandbox-create
                         owner root ende-test--sandbox-pin nil))
          (make-directory
           (ende-test--sandbox-workspace sandbox "subdir"))
          (should (equal (ende-sandbox--cwd sandbox ".") "/workspace"))
          (should (equal (ende-sandbox--cwd sandbox "subdir")
                         "/workspace/subdir"))
          (should-error (ende-sandbox--cwd sandbox "/etc"))
          (should-error (ende-sandbox--cwd sandbox "../outside"))
          (make-symbolic-link "/etc"
                              (ende-test--sandbox-workspace sandbox "escape"))
          (should-error (ende-sandbox--cwd sandbox "escape")))
      (when sandbox
        (ende-sandbox-cleanup-owner owner))
      (delete-directory root t))))

(ert-deftest ende-test/sandbox-persistent-podman-commands-and-job-cleanup ()
  (let* ((image (ende-test--sandbox-image))
         (root nil)
         (owner-one (cl-incf ende-test--sandbox-owner))
         (owner-two (cl-incf ende-test--sandbox-owner))
         (containers nil))
    (unless image
      (ert-skip "rootless Podman and a local digest-pinned image are required"))
    (setq root (make-temp-file "ende-sandbox-podman-" t))
    (unwind-protect
        (progn
          (ende-test--sandbox-make-project root)
          (let ((configuration (ende-test--sandbox-config root image)))
            (let ((result
                   (ende-test--sandbox-run
                    '(("command" . "printf 'out'; printf 'err' >&2; exit 7"))
                    root configuration owner-one)))
              (should (= (plist-get result :exit_code) 7))
              (should (equal (plist-get result :stdout) "out"))
              (should (equal
                       (ende-test--sandbox-command-stderr
                        (plist-get result :stderr))
                       "err")))
            (let ((result
                   (ende-test--sandbox-run
                    '(("command" . "mkdir -p public/build && printf built > public/build/result"))
                    root configuration owner-one)))
              (should (= (plist-get result :exit_code) 0)))
            (let ((result
                   (ende-test--sandbox-run
                    '(("command" . "cat public/build/result"))
                    root configuration owner-one)))
              (should (equal (plist-get result :stdout) "built")))
            (let ((result
                   (ende-test--sandbox-run
                    '(("command" . "yes x | head -c 100000"))
                    root configuration owner-one)))
              (should (= (length (plist-get result :stdout))
                         ende-sandbox--output-limit))
              (should (eq (plist-get result :stdout_truncated) t)))
            (let ((result
                   (ende-test--sandbox-run
                    '(("command" . "yes x | head -c 100000 >&2"))
                    root configuration owner-one)))
              (should (= (length (plist-get result :stderr))
                         ende-sandbox--output-limit))
              (should (eq (plist-get result :stderr_truncated) t)))
            (let ((bad-cwd
                   (ende-test--sandbox-call
                    "run_command"
                    '(("command" . "pwd") ("cwd" . "../outside"))
                    root configuration owner-one)))
              (should-not (plist-get bad-cwd :ok))
              (should (string-match-p "escape"
                                      (plist-get bad-cwd :error))))
            (let ((timed
                   (ende-test--sandbox-run
                    `(("command" . ,(format
                                     "printf kept > public/timeout.txt; printf root > /tmp/ende-%d; sleep 10"
                                     owner-one))
                      ("timeout" . 1))
                    root configuration owner-one)))
              (should (= (plist-get timed :exit_code) 124)))
            (let ((restarted
                   (ende-test--sandbox-run
                    `(("command" . ,(format
                                     "cat public/timeout.txt; test ! -e /tmp/ende-%d"
                                     owner-one)))
                    root configuration owner-one)))
              (should (= (plist-get restarted :exit_code) 0))
              (should (equal (plist-get restarted :stdout) "kept")))
            (let ((isolated
                   (ende-test--sandbox-run
                    '(("command" . "test ! -e public/build/result && cat public/main.c"))
                    root configuration owner-two)))
              (should (= (plist-get isolated :exit_code) 0))
              (should (equal (plist-get isolated :stdout)
                             "int main(void) { return 23; }\n")))
            (should (equal
                     (with-temp-buffer
                       (insert-file-contents
                        (expand-file-name "public/main.c" root))
                       (buffer-string))
                     "int main(void) { return 23; }\n"))
            (let* ((sandbox (ende-sandbox-for-owner owner-one))
                   (container (ende-sandbox-container sandbox))
                   (sandbox-edit
                    (ende-test--sandbox-run
                     '(("command" . "printf sandbox-only > public/main.c"))
                     root configuration owner-one))
                   (edited
                    (ende-test--sandbox-call
                     "edit_file"
                     (list :path "public/main.c"
                           :edits (list (ende-test--anchor-edit
                                         "return 23" "replace" "return 42")))
                     root configuration owner-one))
                   (updated
                    (ende-test--sandbox-run
                     '(("command" . "cat public/main.c"))
                     root configuration owner-one)))
              (should (= (plist-get sandbox-edit :exit_code) 0))
              (should (plist-get edited :ok))
              (should (eq sandbox (ende-sandbox-for-owner owner-one)))
              (should (equal container (ende-sandbox-container sandbox)))
              (should (= (plist-get updated :exit_code) 0))
              (should (equal (plist-get updated :stdout)
                             "int main(void) { return 42; }\n"))
              (should (equal
                       (with-temp-buffer
                         (insert-file-contents
                          (expand-file-name "public/main.c" root))
                         (buffer-string))
                       "int main(void) { return 42; }\n"))))
          (dolist (owner (list owner-one owner-two))
            (let ((sandbox (ende-sandbox-for-owner owner)))
              (should sandbox)
              (push (cons (ende-sandbox-container sandbox)
                          (ende-sandbox-base sandbox))
                    containers)
              (ende-sandbox--job-event
               (ende--make-job :id owner) '(:type :settled)))
            (ende-sandbox-cleanup-owner owner))
          (ende-test--sandbox-wait-clean (list owner-one owner-two))
          (dolist (entry containers)
            (should-not (file-exists-p (cdr entry)))
            (with-temp-buffer
              (should-not
               (zerop (call-process "podman" nil t nil "container" "exists"
                                    (car entry)))))))
      (dolist (owner (list owner-one owner-two))
        (ende-sandbox-cleanup-owner owner))
      (ende-test--sandbox-wait-clean (list owner-one owner-two))
      (delete-directory root t))))

(ert-deftest ende-test/sandbox-start-failure-does-not-touch-project-files ()
  (let* ((available (ende-test--sandbox-image))
         (root nil)
         (owner (cl-incf ende-test--sandbox-owner)))
    (unless available
      (ert-skip "rootless Podman and a local digest-pinned image are required"))
    (setq root (make-temp-file "ende-sandbox-start-failure-" t))
    (unwind-protect
        (progn
          (ende-test--sandbox-make-project root)
          (let* ((missing-image
                  (concat (format "localhost/ende-missing-%d" owner)
                          "@sha256:" (make-string 64 ?f)))
                 (configuration
                  (ende-test--sandbox-config root missing-image))
                 (result
                  (ende-test--sandbox-call
                   "run_command" '(("command" . "touch public/changed"))
                   root configuration owner)))
            (should-not (plist-get result :ok))
            (should-not (ende-sandbox-for-owner owner))
            (should-not
             (zerop (call-process "podman" nil nil nil "image" "exists"
                                  missing-image)))
            (should-not (file-exists-p
                         (expand-file-name "public/changed" root)))
            (should (equal
                     (with-temp-buffer
                       (insert-file-contents
                        (expand-file-name "public/main.c" root))
                       (buffer-string))
                     "int main(void) { return 23; }\n"))))
      (ende-sandbox-cleanup-owner owner)
      (ende-test--sandbox-wait-clean (list owner))
      (when root
        (delete-directory root t)))))

(ert-deftest ende-test/sandbox-compiles-and-runs-across-command-calls ()
  (let* ((image (ende-test--sandbox-image))
         (root nil)
         (owner (cl-incf ende-test--sandbox-owner)))
    (unless image
      (ert-skip "rootless Podman and a local digest-pinned image are required"))
    (setq root (make-temp-file "ende-sandbox-compile-" t))
    (unwind-protect
        (progn
          (ende-test--sandbox-make-project root)
          (let* ((configuration (ende-test--sandbox-config root image))
                 (compiler
                  (ende-test--sandbox-run
                   '(("command" . "command -v cc"))
                   root configuration owner)))
            (unless (= (plist-get compiler :exit_code) 0)
              (ert-skip "the local test image has no C compiler"))
            (let ((compiled
                   (ende-test--sandbox-run
                    '(("command" . "cc public/main.c -o public/program"))
                    root configuration owner)))
              (should (= (plist-get compiled :exit_code) 0)))
            (let ((executed
                   (ende-test--sandbox-run
                    '(("command" . "./public/program"))
                    root configuration owner)))
              (should (= (plist-get executed :exit_code) 23)))))
      (ende-sandbox-cleanup-owner owner)
      (ende-test--sandbox-wait-clean (list owner))
      (delete-directory root t))))

(provide 'ende-sandbox-test)

;;; ende-sandbox-test.el ends here
