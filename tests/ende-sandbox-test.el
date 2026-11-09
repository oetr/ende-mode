;;; ende-sandbox-test.el --- Podman sandbox tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter Samarin
;;
;; Author: Peter Samarin <peter.samarin@gmail.com>
;; Maintainer: Peter Samarin <peter.samarin@gmail.com>
;; Keywords: lisp, tools
;; URL: https://github.com/oetr/ende-mode
;; SPDX-License-Identifier: AGPL-3.0-only

;; This file is part of Ende.

;; Ende is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, version 3 of the License.
;;
;; Ende is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with Ende.  If not, see <https://www.gnu.org/licenses/>.

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
     (tools run_sandbox_command copy_from_sandbox list_project_files
            edit_file create_file delete_file)
     (set-tool-resources run_sandbox_command list_project_files edit_file
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
  (let* ((done nil)
         (result nil)
         (cancel nil)
         (ende-tools--execution-owner owner)
         (ende-tools--register-cancellation-function
          (lambda (function) (setq cancel function))))
    (unwind-protect
        (progn
          (ende-tool-call-async
           name arguments root configuration
           (lambda (value) (setq result value done t)))
          (ende-test--wait-for (lambda () done) :timeout 150.0
                               :description (format "sandbox tool %s" name))
          result)
      (when cancel (funcall cancel)))))

(defun ende-test--sandbox-run (arguments root configuration owner)
  "Run command ARGUMENTS for OWNER and return its structured result."
  (let ((call (ende-test--sandbox-call
               "run_sandbox_command" arguments root configuration owner)))
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
  (ende-test--wait-for
   (lambda () (not (cl-some #'ende-sandbox-for-owner owners)))
   :timeout 60.0 :description "sandbox owner cleanup")
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
                             "(tools run_sandbox_command)\n"
                             "(set-tool-resources run_sandbox_command :resources project)\n"
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
          (let ((configuration
                 (ende-test--sandbox-config-forms
                  root
                  '((tools run_sandbox_command))
                  first)))
            (should (memq 'run_sandbox_command
                          (plist-get configuration :active-tools)))
            (should-not (assq 'run_sandbox_command
                              (plist-get configuration :tool-resources))))
          (with-temp-buffer
            (org-mode)
            (insert (ende-test--chat-config-block
                     `((root ,root) (tools run_sandbox_command)))
                    "* Sandbox\n")
            (should-error
             (ende-test--chat-config-at (ende-chat--build-document) "Sandbox")))
          (should-error (ende-test--sandbox-config root nil)
                        :type 'error)
          (should-error (ende-test--sandbox-config root "ubuntu:latest")
                        :type 'error))
      (delete-directory root t))))

(ert-deftest ende-test/sandbox-command-without-resources-starts-empty ()
  (let* ((root (make-temp-file "ende-sandbox-empty-" t))
         (owner (cl-incf ende-test--sandbox-owner))
         (configuration
          (ende-test--sandbox-config-forms
           root '((tools run_sandbox_command)) ende-test--sandbox-pin))
         (sandbox nil))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "project-file.txt" root)
            (insert "not copied\n"))
          (cl-letf (((symbol-function 'ende-sandbox-create)
                     (lambda (job root image files)
                       (should (= job owner))
                       (should (equal image ende-test--sandbox-pin))
                       (should-not files)
                       (let* ((base (make-temp-file "ende-empty-workspace-" t))
                              (workspace (expand-file-name "workspace" base)))
                         (make-directory workspace)
                         (setq sandbox
                               (ende-sandbox--create
                                :owner job :root root :base base
                                :workspace workspace))
                         (puthash job sandbox ende-sandbox--by-owner)
                         sandbox)))
                    ((symbol-function 'ende-sandbox-run-command)
                     (lambda (_sandbox _command _cwd _timeout callback _register)
                       (funcall callback '(:exit_code 0 :stdout "" :stderr "") nil))))
            (let ((result (ende-test--sandbox-call
                           "run_sandbox_command"
                           '(("command" . "true")) root configuration owner)))
              (should (plist-get result :ok))))
          (should sandbox)
          (should-not (directory-files
                       (ende-sandbox-workspace sandbox) nil "[^.]")))
      (ende-sandbox-cleanup-owner owner)
      (delete-directory root t))))

(ert-deftest ende-test/tool-runner-binds-its-job-id-for-dispatch ()
  (let* ((job (ende--make-job :id 765432 :state 'running :type 'chat))
         (call '(:id "tool-call" :name "probe" :arguments "{}"))
         (owner-seen nil)
         (round-finished nil)
         (ende-tools--execution-owner 'outer-owner))
    (cl-letf (((symbol-function 'ende-job-current-p)
               (lambda (_job) t))
              ((symbol-function 'ende-job-emit-event)
               (lambda (&rest _args) nil))
              ((symbol-function 'ende-tools-call-async)
               (lambda (_call _configuration callback)
                 (setq owner-seen ende-tools--execution-owner)
                 (funcall callback "{}")))
              (ende-tools--execution-owner 'outer-owner))
      (ende-agent--run-tools
       job (list call) nil
       (list :tool-name (lambda (call) (ende-tools-call-value call :name))
             :execute-tool #'ende-tools-call-async
             :tool-result-message (lambda (_job _call content) content))
       (lambda (_messages) (setq round-finished t))))
    (should round-finished)
    (should (= owner-seen (ende-job-id job)))
    (should (eq ende-tools--execution-owner 'outer-owner))))

(ert-deftest ende-test/sandbox-authorization-uses-only-assigned-files ()
  (let ((root (make-temp-file "ende-sandbox-authorization-" t)))
    (unwind-protect
        (progn
          (ende-test--sandbox-make-project root)
          (let* ((optional
                  (ende-test--sandbox-config-forms
                   root '((tools run_sandbox_command))
                   ende-test--sandbox-pin))
                 (ende-tools--access-config optional))
            (should-not
             (ende-tools--authorized-project-files 'run_sandbox_command root)))
          (let* ((configuration
                  (ende-test--sandbox-config-forms
                   root
                   '((resource project (include "**") (exclude "private/**"))
                     (tools run_sandbox_command)
                     (set-tool-resources run_sandbox_command :resources project))
                   ende-test--sandbox-pin))
                 (ende-tools--access-config configuration)
                 (files (ende-tools--authorized-project-files
                         'run_sandbox_command root)))
            (should (= (length files) 2))
            (should (member (expand-file-name "public/main.c" root) files))
            (should (member (expand-file-name "public/second.txt" root) files))
            (should-not (member (expand-file-name "private/secret.txt" root) files))
            (should-not (member (expand-file-name ".env" root) files))))
      (delete-directory root t))))

(ert-deftest ende-test/sandbox-snapshot-uses-tool-scope-without-list-limit ()
  :tags '(integration)
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
                         'run_sandbox_command root)))
            (should (= (length files) 2))
            (let* ((listing (ende-test--tool-call
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

(ert-deftest ende-test/sandbox-treats-tilde-path-components-literally ()
  (let* ((root (make-temp-file "ende-sandbox-tilde-" t))
         (source (expand-file-name "src/~/foo.el" root))
         (owner (cl-incf ende-test--sandbox-owner))
         (sandbox nil))
    (unless (and (file-directory-p "/dev/shm")
                 (file-writable-p "/dev/shm")
                 (ende-sandbox--tmpfs-p (file-truename "/dev/shm")))
      (delete-directory root t)
      (ert-skip "a writable /dev/shm tmpfs is required"))
    (unwind-protect
        (progn
          (make-directory (file-name-directory source) t)
          (with-temp-file source (insert "snapshot\n"))
          (cl-letf (((symbol-function 'user-uid) (lambda () 1000))
                    ((symbol-function 'executable-find)
                     (lambda (program)
                       (and (equal program "podman") "/usr/bin/podman"))))
            (setq sandbox
                  (ende-sandbox-create
                   owner root ende-test--sandbox-pin (list source))))
          (let ((sandbox-file
                 (ende-test--sandbox-workspace sandbox "src/~/foo.el")))
            (should (file-in-directory-p sandbox-file
                                         (ende-sandbox-workspace sandbox)))
            (should (equal (with-temp-buffer
                             (insert-file-contents sandbox-file)
                             (buffer-string))
                           "snapshot\n"))
            (with-temp-file source (insert "synchronized\n"))
            (ende-sandbox-sync-file sandbox source "src/~/foo.el")
            (should (equal (with-temp-buffer
                             (insert-file-contents sandbox-file)
                             (buffer-string))
                           "synchronized\n"))))
      (ende-sandbox-cleanup-owner owner)
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
                     (tools run_sandbox_command edit_file create_file delete_file)
                     (set-tool-resources run_sandbox_command :resources source)
                     (set-tool-resources edit_file create_file delete_file
                                         :resources source))
                   ende-test--sandbox-pin))
                 (ende-tools--execution-owner owner))
             (setq sandbox
                   (ende-test--sandbox-fixture owner root (list source)))
             (let ((artifact
                    (ende-test--sandbox-workspace sandbox "agent/build.log")))
               (make-directory (file-name-directory artifact) t)
               (with-temp-file artifact (insert "sandbox-only\n")))
             (should (equal
                     (with-temp-buffer
                       (insert-file-contents
                        (ende-test--sandbox-workspace sandbox "src/foo.el"))
                       (buffer-string))
                     "version A\n"))
            (let ((edited
                   (ende-test--tool-call
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
                         (ende-test--sandbox-workspace
                          sandbox "agent/build.log"))
                        (buffer-string))
                      "sandbox-only\n"))
            (should (equal
                     (with-temp-buffer
                       (insert-file-contents
                        (ende-test--sandbox-workspace sandbox "src/foo.el"))
                       (buffer-string))
                     "version B\n"))
            (let ((created-call
                   (ende-test--tool-call
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
                   (ende-test--tool-call 'delete_file '(:path "src/foo.el")
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
                     (ende-test--tool-call
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

(ert-deftest ende-test/sandbox-is-untouched-when-host-batch-rolls-back ()
  (let* ((root (make-temp-file "ende-sandbox-host-rollback-" t))
         (first (expand-file-name "src/first.el" root))
         (second (expand-file-name "src/second.el" root))
         (owner (cl-incf ende-test--sandbox-owner))
         (sandbox nil)
         (configuration
          (ende-test--sandbox-config-forms
           root
           '((resource source (include "src/**"))
             (tools apply_patch run_sandbox_command)
             (set-tool-resources apply_patch run_sandbox_command :resources source))
           ende-test--sandbox-pin))
         (patch (concat "*** Begin Patch\n"
                        "*** Update File: src/first.el\n@@\n-old\n+new\n"
                        "*** Update File: src/second.el\n@@\n-old\n+new\n"
                        "*** End Patch\n"))
         (commit (symbol-function 'ende-tools--commit-change-set))
         (count 0))
    (unwind-protect
        (progn
          (make-directory (file-name-directory first) t)
          (with-temp-file first (insert "old\n"))
          (with-temp-file second (insert "old\n"))
          (setq sandbox
                (ende-test--sandbox-fixture owner root (list first second)))
          (let ((ende-tools--execution-owner owner))
             (cl-letf (((symbol-function 'ende-tools--commit-change-set)
                       (lambda (change-set)
                         (setq count (1+ count))
                         (if (= count 2)
                             (error "injected later commit failure")
                           (funcall commit change-set)))))
              (let ((failure
                     (ende-test--tool-call
                      "apply_patch" (list (cons "patch" patch))
                      root configuration)))
                (should-not (plist-get failure :ok))
                (should (string-match-p "injected later commit failure"
                                        (plist-get failure :error))))))
          (dolist (path (list first second))
            (should (equal (with-temp-buffer
                             (insert-file-contents path)
                             (buffer-string))
                           "old\n")))
          (dolist (relative '("src/first.el" "src/second.el"))
            (should (equal (with-temp-buffer
                             (insert-file-contents
                              (ende-test--sandbox-workspace sandbox relative))
                             (buffer-string))
                           "old\n")))
          (should (eq sandbox (ende-sandbox-for-owner owner))))
      (ende-sandbox-cleanup-owner owner)
      (delete-directory root t))))

(ert-deftest ende-test/sandbox-cleanup-can-retry-and-traverse-private-trees ()
  (let* ((base (make-temp-file "ende-sandbox-cleanup-" t))
         (outside (make-temp-file "ende-sandbox-outside-" t))
         (owner (cl-incf ende-test--sandbox-owner))
         (sandbox (ende-sandbox--create :owner owner :base base))
         (locked (expand-file-name "locked" base)))
    (unwind-protect
        (progn
          (puthash owner sandbox ende-sandbox--by-owner)
          (make-directory locked)
          (make-symbolic-link outside (expand-file-name "link" locked))
          (set-file-modes locked 0)
          (cl-letf (((symbol-function 'ende-sandbox--delete-tree)
                     (lambda (_) (error "Deletion failed"))))
            (should-error (ende-sandbox-invalidate sandbox)))
          (should-not (ende-sandbox-for-owner owner))
          (should (memq sandbox ende-sandbox--retired))
          (ende-sandbox-cleanup-owner owner)
          (should-not (file-exists-p base))
          (should-not (memq sandbox ende-sandbox--retired))
          (should (file-directory-p outside)))
      (ende-sandbox-cleanup-owner owner)
      (delete-directory outside t))))

(ert-deftest ende-test/sandbox-sync-failure-keeps-host-edit-and-recreates-sandbox ()
  (let* ((root (make-temp-file "ende-sandbox-sync-failure-" t))
         (source (expand-file-name "src/main.el" root))
         (owner (cl-incf ende-test--sandbox-owner))
         (sandbox nil)
         (fresh nil)
         (configuration
          (ende-test--sandbox-config-forms
           root
           '((resource source (include "src/**"))
             (tools edit_file run_sandbox_command)
             (set-tool-resources edit_file run_sandbox_command :resources source))
           ende-test--sandbox-pin)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory source) t)
          (with-temp-file source (insert "before\n"))
          (setq sandbox (ende-test--sandbox-fixture owner root (list source)))
          (let ((old-base (ende-sandbox-base sandbox))
                (ende-tools--execution-owner owner))
            (cl-letf (((symbol-function 'ende-sandbox-sync-file)
                       (lambda (&rest _arguments)
                         (error "injected sandbox sync failure"))))
              (let ((edited
                     (ende-test--tool-call
                      "edit_file"
                      (list (cons "path" "src/main.el")
                            (cons "edits"
                                  (list (ende-test--anchor-edit
                                         "before" "replace" "committed"))))
                      root configuration)))
                (should (plist-get edited :ok))
                (should (string-match-p
                         "Host changes were committed"
                         (plist-get (plist-get edited :result) :sandbox_warning))))
            (should (equal (with-temp-buffer
                             (insert-file-contents source)
                             (buffer-string))
                           "committed\n"))
            (should-not (ende-sandbox-for-owner owner))
            (should-not (file-exists-p old-base))))
          (cl-letf (((symbol-function 'ende-sandbox-create)
                     (lambda (new-owner new-root _image files)
                       (setq fresh
                             (ende-test--sandbox-fixture
                              new-owner new-root files))))
                    ((symbol-function 'ende-sandbox-run-command)
                     (lambda (new-sandbox _command _cwd _timeout callback
                                             _register-cancellation)
                       (let ((text
                              (with-temp-buffer
                                (insert-file-contents
                                 (ende-test--sandbox-workspace
                                  new-sandbox "src/main.el"))
                                (buffer-string))))
                         (funcall callback
                                   (ende-sandbox--result
                                    0 text "" nil nil)
                                   nil)))))
            (let ((result
                   (ende-test--sandbox-call
                    "run_sandbox_command" '(("command" . "cat src/main.el"))
                    root configuration owner)))
              (should (plist-get result :ok))
              (should (equal (plist-get (plist-get result :result) :stdout)
                             "committed\n")))
             (should fresh)
             (should-not (eq sandbox fresh))
             (should (eq fresh (ende-sandbox-for-owner owner)))))
      (ende-sandbox-cleanup-owner owner)
      (delete-directory root t))))

(ert-deftest ende-test/sandbox-quiesces-source-batches-and-copy-back ()
  (let* ((root (make-temp-file "ende-sandbox-quiesce-" t))
         (first (expand-file-name "src/first.el" root))
         (second (expand-file-name "src/second.el" root))
         (output (expand-file-name "out/generated.bin" root))
         (owner (cl-incf ende-test--sandbox-owner))
         (sandbox nil)
         (container "fake-container")
         (paused nil)
         (actions nil)
         (configuration
          (ende-test--sandbox-config-forms
           root
           '((resource source (include "src/**"))
             (resource output (include "out/**"))
             (tools apply_patch run_sandbox_command copy_from_sandbox)
             (set-tool-resources apply_patch run_sandbox_command :resources source)
             (set-tool-resources copy_from_sandbox :resources output))
           ende-test--sandbox-pin))
         (patch (concat "*** Begin Patch\n"
                        "*** Update File: src/first.el\n@@\n-old\n+first updated\n"
                        "*** Update File: src/second.el\n@@\n-old\n+second updated\n"
                        "*** End Patch\n"))
         (sync-file (symbol-function 'ende-sandbox-sync-file))
         (workspace-path (symbol-function 'ende-sandbox--workspace-path)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory first) t)
          (make-directory (expand-file-name "out" root) t)
          (with-temp-file first (insert "old\n"))
          (with-temp-file second (insert "old\n"))
          (setq sandbox
                (ende-test--sandbox-fixture owner root (list first second)))
          (setf (ende-sandbox-container sandbox) container
                (ende-sandbox-started sandbox) t)
          (let ((ende-tools--execution-owner owner))
            (cl-letf (((symbol-function 'ende-sandbox--container-running-p)
                       (lambda (_sandbox callback)
                         (run-at-time 0 nil callback t nil)))
                      ((symbol-function 'ende-sandbox--podman-action)
                       (lambda (_sandbox action callback)
                         (push action actions)
                         (run-at-time
                          0 nil (lambda ()
                                  (setq paused (equal action "pause"))
                                  (funcall callback "" nil)))))
                      ((symbol-function 'ende-sandbox-sync-file)
                       (lambda (&rest arguments)
                         (should paused)
                         (apply sync-file arguments)))
                      ((symbol-function 'ende-sandbox--workspace-path)
                       (lambda (&rest arguments)
                         (should paused)
                         (apply workspace-path arguments))))
              (let ((edited
                     (ende-test--tool-call
                      "apply_patch" (list (cons "patch" patch))
                      root configuration)))
                (should (plist-get edited :ok)))
              (should (eq sandbox (ende-sandbox-for-owner owner)))
              (should (equal container (ende-sandbox-container sandbox)))
              (should (equal (nreverse (copy-sequence actions))
                             '("pause" "unpause")))
              (should-not paused)
              (let ((artifact
                     (ende-test--sandbox-workspace sandbox "agent.log")))
                (with-temp-file artifact (insert "built by sandbox\n")))
              (let ((copied
                     (ende-test--sandbox-call
                      "copy_from_sandbox"
                      '(("source" . "agent.log")
                        ("destination" . "out/generated.bin"))
                      root configuration owner)))
                (should (plist-get copied :ok)))
              (should (eq sandbox (ende-sandbox-for-owner owner)))
              (should (equal container (ende-sandbox-container sandbox)))
              (should (equal (nreverse (copy-sequence actions))
                             '("pause" "unpause" "pause" "unpause")))
              (should-not paused)
              (should (equal (with-temp-buffer
                               (insert-file-contents output)
                               (buffer-string))
                              "built by sandbox\n"))
              (let ((accessed nil)
                    (failure nil))
                (cl-letf (((symbol-function 'ende-sandbox--podman-action)
                           (lambda (_sandbox action callback)
                             (funcall callback ""
                                      (and (equal action "unpause")
                                           "injected unpause failure"))))
                          ((symbol-function 'ende-sandbox--remove-container)
                           (lambda (_sandbox callback)
                             (funcall callback nil))))
                  (ende-sandbox-with-quiesced-workspace
                   sandbox
                   (lambda (_operation done)
                     (setq accessed t)
                     (funcall done t nil))
                   (lambda (_value error) (setq failure error)))
                  (ende-test--wait-for (lambda () failure)))
                (should (equal failure "injected unpause failure"))
                (should accessed)
                (should-not (ende-sandbox-for-owner owner))))))
      (when sandbox
        (setf (ende-sandbox-container sandbox) nil
              (ende-sandbox-started sandbox) nil)
        (ende-sandbox--dispose sandbox))
      (ende-sandbox-cleanup-owner owner)
      (delete-directory root t))))

(ert-deftest ende-test/sandbox-deletion-collision-invalidates-without-recursion ()
  (let* ((root (make-temp-file "ende-sandbox-delete-collision-" t))
         (source (expand-file-name "src/main.el" root))
         (owner (cl-incf ende-test--sandbox-owner))
         (sandbox nil)
         (configuration
          (ende-test--sandbox-config-forms
           root
           '((resource source (include "src/**"))
             (tools delete_file run_sandbox_command)
             (set-tool-resources delete_file run_sandbox_command :resources source))
           ende-test--sandbox-pin)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory source) t)
          (with-temp-file source (insert "source\n"))
          (setq sandbox (ende-test--sandbox-fixture owner root (list source)))
          (let* ((sandbox-path
                  (ende-test--sandbox-workspace sandbox "src/main.el"))
                 (sentinel (expand-file-name "agent-data" sandbox-path)))
            (delete-file sandbox-path)
            (make-directory sandbox-path)
            (with-temp-file sentinel (insert "keep this subtree\n")))
          (setf (ende-sandbox-container sandbox) "fake-container"
                (ende-sandbox-started sandbox) t)
          (let ((ende-tools--execution-owner owner))
            (cl-letf (((symbol-function 'ende-sandbox--container-running-p)
                       (lambda (_sandbox callback)
                         (run-at-time 0 nil callback t nil)))
                      ((symbol-function 'ende-sandbox--podman-action)
                       (lambda (_sandbox _action callback)
                         (funcall callback "" nil)))
                      ((symbol-function 'ende-sandbox--remove-container)
                       (lambda (_sandbox callback) (funcall callback nil))))
              (let ((deleted
                     (ende-test--tool-call 'delete_file '(:path "src/main.el")
                                     root configuration)))
                (should (plist-get deleted :ok))))
          (should-not (file-exists-p source))
          (should-not (ende-sandbox-for-owner owner))
          (let ((sandbox-path
                 (ende-test--sandbox-workspace sandbox "src/main.el")))
            (should (file-directory-p sandbox-path))
            (should (equal (with-temp-buffer
                             (insert-file-contents
                              (expand-file-name "agent-data" sandbox-path))
                             (buffer-string))
                           "keep this subtree\n")))))
      (when sandbox
        (setf (ende-sandbox-container sandbox) nil
              (ende-sandbox-started sandbox) nil)
        (ende-sandbox--dispose sandbox))
      (delete-directory root t))))

(ert-deftest ende-test/sandbox-startup-error-describes-status-and-output ()
  (dolist (entry
           '(((125 "" "Error: image not known\n" nil nil nil)
              "sandbox could not be started: Podman exited with status 125\nPodman stderr:\nError: image not known\n")
             ((125 "runtime diagnostic\n" "" nil nil nil)
              "sandbox could not be started: Podman exited with status 125\nPodman stdout:\nruntime diagnostic\n")
             ((125 "partial out" "partial err" t t nil)
              "sandbox could not be started: Podman exited with status 125\nPodman stderr (truncated):\npartial err\nPodman stdout (truncated):\npartial out")
             ((125 "" "" nil nil nil)
              "sandbox could not be started: Podman exited with status 125")))
    (should (equal (apply #'ende-sandbox--startup-error (car entry))
                   (cadr entry))))
  (let ((ende-sandbox--startup-timeout 7))
    (should
     (equal (ende-sandbox--startup-error
             124 "" "Error: still starting\n" nil nil t)
            (concat "sandbox could not be started: "
                    "Podman startup timed out after 7 seconds (exit status 124)\n"
                    "Podman stderr:\nError: still starting\n")))))

(ert-deftest ende-test/sandbox-cancellation-stops-writers-before-disposal ()
  (let* ((root (make-temp-file "ende-sandbox-cancel-" t))
         (owner (cl-incf ende-test--sandbox-owner))
         (sandbox (ende-test--sandbox-fixture owner root nil))
         (startup nil)
         (remove nil)
         (cancel nil)
         (stopped nil)
         (calls 0))
    (unwind-protect
        (cl-letf (((symbol-function 'ende-sandbox--run-container-argv)
                   (lambda (_sandbox _name) '("podman" "run")))
                  ((symbol-function 'ende-sandbox--spawn)
                   (lambda (_sandbox _argv _timeout callback)
                     (setq startup callback)))
                  ((symbol-function 'ende-sandbox--cancel-process)
                   (lambda (_sandbox) (setq stopped t)))
                  ((symbol-function 'ende-sandbox--remove-container)
                   (lambda (_sandbox callback)
                     (should stopped)
                     (should (file-directory-p (ende-sandbox-base sandbox)))
                     (setq remove callback)))
                  ((symbol-function 'ende-sandbox--exec)
                   (lambda (&rest _args)
                     (ert-fail "a cancelled startup must not execute a command"))))
          (ende-sandbox-run-command
           sandbox "true" "." 30 (lambda (&rest _) (cl-incf calls))
           (lambda (function) (setq cancel function)))
          (should (functionp cancel))
          (should-error
           (ende-sandbox-run-command sandbox "true" "." 30 #'ignore nil))
          (funcall cancel)
          (should (= calls 1))
          (should (ende-sandbox-closing sandbox))
          (should (file-directory-p (ende-sandbox-base sandbox)))
          (funcall startup 0 "" "" nil nil nil)
          (should (= calls 1))
          (setf (ende-sandbox-container sandbox) nil)
          (funcall remove t)
          (ende-test--wait-for
           (lambda () (not (file-exists-p (ende-sandbox-base sandbox)))))
          (should-not (ende-sandbox-for-owner owner)))
      (setf (ende-sandbox-container sandbox) nil)
      (ende-sandbox-cleanup-owner owner)
      (delete-directory root t))))

(ert-deftest ende-test/sandbox-startup-failure-preserves-diagnostics-in-json ()
  (dolist (removed '(t nil))
    (let* ((root (make-temp-file "ende-sandbox-startup-json-" t))
           (owner (cl-incf ende-test--sandbox-owner))
           (sandbox (ende-test--sandbox-fixture owner root nil))
           (configuration (ende-test--sandbox-config root ende-test--sandbox-pin))
           (startup-callback nil)
           (cleanup-callback nil)
           (response nil)
           (ende-tools--execution-owner owner))
      (unwind-protect
          (cl-letf (((symbol-function 'ende-sandbox--run-container-argv)
                     (lambda (_sandbox _name) '("podman" "run")))
                    ((symbol-function 'ende-sandbox--spawn)
                     (lambda (_sandbox _argv _timeout callback)
                       (setq startup-callback callback)))
                    ((symbol-function 'ende-sandbox--remove-container)
                     (lambda (_sandbox callback)
                       (setq cleanup-callback callback)))
                    ((symbol-function 'ende-sandbox--exec)
                     (lambda (&rest _args)
                       (ert-fail "command must not run after startup failure"))))
            (ende-tools-call-async
             (list :id "startup-failure" :name "run_sandbox_command"
                   :arguments "{\"command\":\"pwd\"}")
             configuration
             (lambda (value) (setq response value)))
            (should (functionp startup-callback))
            (funcall startup-callback
                     125 "" "Error: original Podman failure\n" nil nil nil)
            (should (functionp cleanup-callback))
            (should-not response)
            (funcall cleanup-callback removed)
            (let ((decoded (ende-protocol-json-read-string response)))
              (should (eq (ende-protocol-json-get "ok" decoded) :false))
              (should
               (equal (ende-protocol-json-get "error" decoded)
                      (concat "sandbox could not be started: "
                              "Podman exited with status 125\nPodman stderr:\n"
                              "Error: original Podman failure\n"))))
            (if removed
                (progn
                  (should-not (ende-sandbox-for-owner owner))
                  (should-not (file-exists-p (ende-sandbox-base sandbox))))
              (should (eq sandbox (ende-sandbox-for-owner owner)))
              (should (ende-sandbox-restart-required sandbox))))
        (setf (ende-sandbox-container sandbox) nil)
        (ende-sandbox--dispose sandbox)
        (delete-directory root t)))))

(ert-deftest ende-test/sandbox-startup-preserves-immediate-launch-error ()
  (let ((sandbox (ende-sandbox--create :owner 1))
        (cleanup-callback nil)
        (failure nil))
    (cl-letf (((symbol-function 'ende-sandbox--run-container-argv)
               (lambda (_sandbox _name) '("podman" "run")))
              ((symbol-function 'ende-sandbox--spawn)
               (lambda (&rest _args) (error "Podman executable: permission denied")))
              ((symbol-function 'ende-sandbox--remove-container)
               (lambda (_sandbox callback) (setq cleanup-callback callback))))
      (ende-sandbox--start-container
       sandbox "pwd" "." 30
       (lambda (result error)
         (should-not result)
         (setq failure error)))
      (should (functionp cleanup-callback))
      (should-not failure)
      (funcall cleanup-callback nil)
      (should (equal failure
                     (concat "sandbox could not be started: "
                             "Podman executable: permission denied")))
      (should (ende-sandbox-restart-required sandbox)))))

(ert-deftest ende-test/run-sandbox-command-json-keeps-outer-success-for-failing-process ()
  (let* ((root (make-temp-file "ende-run-sandbox-command-json-" t))
         (owner (cl-incf ende-test--sandbox-owner))
         (workspace (make-temp-file "ende-run-sandbox-command-workspace-" t))
         (configuration
          (ende-test--sandbox-config-forms
           root
           '((resource project (include "**"))
             (tools run_sandbox_command)
             (set-tool-resources run_sandbox_command :resources project))
           ende-test--sandbox-pin))
         (sandbox (ende-sandbox--create
                   :owner owner :root root :workspace workspace))
         (response nil)
         (ende-tools--execution-owner owner))
    (unwind-protect
        (progn
          (puthash owner sandbox ende-sandbox--by-owner)
          (cl-letf (((symbol-function 'ende-sandbox-run-command)
                     (lambda (_sandbox _command _cwd _timeout callback _register)
                       (funcall callback
                                '(:exit_code 9 :process_ok nil :stdout ""
                                  :stderr "" :stdout_truncated :json-false
                                  :stderr_truncated :json-false)
                                nil))))
            (ende-tools-call-async
             (list :id "run-sandbox-command" :name "run_sandbox_command"
                   :arguments "{\"command\":\"exit 9\"}")
             configuration
             (lambda (value) (setq response value))))
          (let* ((decoded (ende-protocol-json-read-string response))
                 (result (ende-protocol-json-get "result" decoded)))
            (should (eq (ende-protocol-json-get "ok" decoded) t))
            (should (eq (ende-protocol-json-get "process_ok" result) :false))))
      (remhash owner ende-sandbox--by-owner)
      (ende-sandbox-cleanup-owner owner)
      (delete-directory workspace t)
      (delete-directory root t))))

(ert-deftest ende-test/sandbox-sync-does-not-bypass-run-sandbox-command-scope ()
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
                     (tools run_sandbox_command edit_file)
                     (set-tool-resources run_sandbox_command :resources source)
                     (set-tool-resources edit_file :resources editable))
                    ende-test--sandbox-pin))
                 (ende-tools--execution-owner owner))
            (setq sandbox
                  (ende-test--sandbox-fixture owner root (list source)))
            (let ((edited
                   (ende-test--tool-call
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

(ert-deftest ende-test/unresourced-sandbox-does-not-sync-project-mutations ()
  (let* ((root (make-temp-file "ende-sandbox-unresourced-sync-" t))
         (source (expand-file-name "src/main.el" root))
         (owner (cl-incf ende-test--sandbox-owner))
         (sandbox nil))
    (unwind-protect
        (progn
          (make-directory (file-name-directory source) t)
          (with-temp-file source (insert "before\n"))
          (let* ((configuration
                  (ende-test--sandbox-config-forms
                   root
                   '((resource editable (include "src/**"))
                     (tools run_sandbox_command edit_file)
                     (set-tool-resources edit_file :resources editable))
                   ende-test--sandbox-pin))
                 (ende-tools--execution-owner owner))
            (setq sandbox (ende-test--sandbox-fixture owner root nil))
            (let ((edited
                   (ende-test--tool-call
                    'edit_file
                    (list :path "src/main.el"
                          :edits (list (ende-test--anchor-edit
                                        "before" "replace" "after")))
                    root configuration)))
              (should (plist-get edited :ok)))
            (should (equal
                     (with-temp-buffer
                       (insert-file-contents source)
                       (buffer-string))
                     "after\n"))
            (should-not
             (file-exists-p
              (ende-test--sandbox-workspace sandbox "src/main.el")))))
      (ende-sandbox-cleanup-owner owner)
      (delete-directory root t))))

(ert-deftest ende-test/sandbox-copy-back-rechecks-destination-and-symlinks ()
  :tags '(integration)
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
                   (copied (ende-test--tool-call
                            "copy_from_sandbox"
                            '(("source" . "generated.bin")
                              ("destination" . "out/generated.bin"))
                            root configuration))
                   (denied (ende-test--tool-call
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
                   (denied (ende-test--tool-call
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
  :tags '(integration)
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
  :tags '(integration)
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
              (should-not (plist-get result :process_ok))
              (should (equal (plist-get result :stdout) "out"))
              (should (equal
                       (ende-test--sandbox-command-stderr
                        (plist-get result :stderr))
                       "err")))
            (let ((result
                   (ende-test--sandbox-run
                    '(("command" . "mkdir -p public/build && printf built > public/build/result"))
                    root configuration owner-one)))
              (should (= (plist-get result :exit_code) 0))
              (should (plist-get result :process_ok)))
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
                    "run_sandbox_command"
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
                     "int main(void) { return 23; }\n")))
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

(ert-deftest ende-test/sandbox-podman-host-edit-sync-preserves-artifacts ()
  :tags '(integration)
  (let* ((image (ende-test--sandbox-image))
         (root nil)
         (owner (cl-incf ende-test--sandbox-owner)))
    (unless image
      (ert-skip "rootless Podman and a local digest-pinned image are required"))
    (setq root (make-temp-file "ende-sandbox-sync-" t))
    (unwind-protect
        (progn
          (ende-test--sandbox-make-project root)
          (let* ((configuration (ende-test--sandbox-config root image))
                 (setup (ende-test--sandbox-run
                         '(("command" . "printf sandbox-only > public/main.c; printf 'keep through sync\\n' > agent-only.log"))
                         root configuration owner)))
            (should (= (plist-get setup :exit_code) 0))
            (let* ((sandbox (ende-sandbox-for-owner owner))
                   (container (ende-sandbox-container sandbox))
                   (artifact (ende-test--sandbox-workspace sandbox "agent-only.log"))
                   (edited (ende-test--sandbox-call
                            "edit_file"
                            (list :path "public/main.c"
                                  :edits (list (ende-test--anchor-edit
                                                "return 23" "replace" "return 42")))
                            root configuration owner)))
              (should (plist-get edited :ok))
              ;; Check survival before another command could start a replacement.
              (unless (eq sandbox (ende-sandbox-for-owner owner))
                (ert-skip
                 "this Podman runtime cannot pause containers for workspace synchronization"))
              (should (equal container (ende-sandbox-container sandbox)))
              (let ((updated (ende-test--sandbox-run
                              '(("command" . "cat public/main.c"))
                              root configuration owner)))
                (should (= (plist-get updated :exit_code) 0))
                (should (equal (plist-get updated :stdout)
                               "int main(void) { return 42; }\n")))
              (should (equal (with-temp-buffer
                               (insert-file-contents artifact)
                               (buffer-string))
                             "keep through sync\n"))
              (should (equal (with-temp-buffer
                               (insert-file-contents
                                (expand-file-name "public/main.c" root))
                               (buffer-string))
                             "int main(void) { return 42; }\n")))))
      (ende-sandbox-cleanup-owner owner)
      (ende-test--sandbox-wait-clean (list owner))
      (delete-directory root t))))

(ert-deftest ende-test/sandbox-start-failure-does-not-touch-project-files ()
  :tags '(integration)
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
                   "run_sandbox_command" '(("command" . "touch public/changed"))
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
  :tags '(integration)
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
