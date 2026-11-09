;;; ende-sandbox.el --- Per-job Podman workspaces -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter Samarin
;;
;; Author: Peter Samarin <peter.samarin@gmail.com>
;; Maintainer: Peter Samarin <peter.samarin@gmail.com>
;; Keywords: convenience, tools
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

;; Each agent job owns one tmpfs snapshot and one disposable rootless container.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ende-project)
(require 'ende-worker)
(declare-function ende-job-id "ende-queue" (job))

(cl-defstruct (ende-sandbox (:constructor ende-sandbox--create))
  owner root base workspace hooks container image process started
  restart-required closing cleanup-pending operation worker-cancel)

(cl-defstruct (ende-sandbox--operation
               (:constructor ende-sandbox--operation-create))
  sandbox callback register-cancellation finished)

(defconst ende-sandbox--root "/dev/shm")
(defconst ende-sandbox--output-limit 65536)
(defconst ende-sandbox--startup-timeout 120)
(defconst ende-sandbox--cleanup-timeout 30)

(defvar ende-sandbox--by-owner (make-hash-table :test #'eql))
(defvar ende-sandbox--retired nil)

(defun ende-sandbox--join-component (directory component)
  "Append validated literal COMPONENT to DIRECTORY without path expansion."
  (concat (file-name-as-directory directory) component))

(defun ende-sandbox--join-relative-path (directory relative)
  "Append validated workspace-relative RELATIVE literally to DIRECTORY."
  (dolist (component (split-string relative "/" nil))
    (setq directory (ende-sandbox--join-component directory component)))
  directory)

(defun ende-sandbox-for-owner (owner)
  "Return OWNER's sandbox, if one has been created."
  (gethash owner ende-sandbox--by-owner))

(defun ende-sandbox--tmpfs-p (directory)
  "Return non-nil when DIRECTORY is on a tmpfs mount."
  (let ((mount nil)
        (mount-length -1))
    (with-temp-buffer
      (insert-file-contents "/proc/mounts")
      (dolist (line (split-string (buffer-string) "\n" t))
        (let* ((fields (split-string line "[ \t]+"))
               (point (nth 1 fields)))
          (when (and point
                     (or (string= directory point)
                         (file-in-directory-p
                          directory (file-name-as-directory point)))
                     (> (length point) mount-length))
            (setq mount (nth 2 fields)
                  mount-length (length point))))))
    (equal mount "tmpfs")))

(defun ende-sandbox--delete-tree (path)
  "Remove PATH recursively without following symbolic links."
  (when (or (file-exists-p path) (file-symlink-p path))
    (if (and (file-directory-p path) (not (file-symlink-p path)))
        (progn
          (set-file-modes path #o700 'nofollow)
          (dolist (entry
                   (directory-files path t directory-files-no-dot-files-regexp))
            (ende-sandbox--delete-tree entry))
          (delete-directory path))
      (delete-file path))))

(defun ende-sandbox--copy-snapshot (files root workspace)
  "Copy authorized FILES below ROOT into WORKSPACE."
  (dolist (source files)
    (unless (and (not (file-symlink-p source))
                 (file-regular-p source)
                 (file-in-directory-p source root))
      (error "authorized project file changed during sandbox creation"))
    (let* ((relative (file-relative-name source root))
           (destination (ende-sandbox--join-relative-path workspace relative))
           (directory (file-name-directory destination)))
      (make-directory directory t)
      (copy-file source destination nil t nil)))
  workspace)

(defun ende-sandbox-create (owner root image)
  "Reserve OWNER's empty tmpfs workspace below ROOT for pinned IMAGE."
  (unless (and (integerp owner) (>= owner 0))
    (error "run_sandbox_command requires an active Ende job"))
  (unless (and (stringp image)
               (string-match-p
                "\\`[[:graph:]]+@sha256:[[:xdigit:]]\\{64\\}\\'"
                image))
    (error "a digest-pinned Podman image is required"))
  (when (zerop (user-uid))
    (error "Podman sandbox requires a non-root Ende process"))
  (unless (executable-find "podman")
    (error "Podman is not installed"))
  (when (ende-sandbox-for-owner owner)
    (error "sandbox already exists for this job"))
  (let* ((root (ende-project-normalize-root root))
         (prefix (expand-file-name (format "ende-sandbox-%d-" owner)
                                   ende-sandbox--root))
         (base nil)
         (sandbox nil))
    (unless (and (file-directory-p ende-sandbox--root)
                 (file-writable-p ende-sandbox--root)
                 (ende-sandbox--tmpfs-p (file-truename ende-sandbox--root)))
      (error "a writable tmpfs at /dev/shm is required for run_sandbox_command"))
    (setq base (make-temp-file prefix t))
    (unwind-protect
        (progn
          (set-file-modes base #o700)
          (let ((workspace (expand-file-name "workspace" base))
                (hooks (expand-file-name "hooks" base)))
            (make-directory workspace)
            (make-directory hooks)
            (set-file-modes workspace #o700)
            (set-file-modes hooks #o700)
            (setq sandbox
                  (ende-sandbox--create
                   :owner owner :root root :base base :workspace workspace
                   :hooks hooks :image image))
            (puthash owner sandbox ende-sandbox--by-owner)))
      (unless sandbox
        (dolist (path (list (expand-file-name "workspace" base)
                            (expand-file-name "hooks" base) base))
          (when (file-directory-p path) (delete-directory path)))))
    sandbox))

(defun ende-sandbox--environment ()
  "Return a minimal host environment for Podman."
  (delq nil
        (list (and (getenv "PATH") (concat "PATH=" (getenv "PATH")))
              (and (getenv "HOME") (concat "HOME=" (getenv "HOME")))
              (and (getenv "XDG_RUNTIME_DIR")
                   (concat "XDG_RUNTIME_DIR=" (getenv "XDG_RUNTIME_DIR")))
              (and (getenv "XDG_CONFIG_HOME")
                   (concat "XDG_CONFIG_HOME=" (getenv "XDG_CONFIG_HOME")))
              "CONTAINERS_CONF=/dev/null"
              "CONTAINERS_MOUNTS_CONF=/dev/null"
              "LC_ALL=C")))

(defun ende-sandbox--podman-prefix (sandbox)
  "Return fixed Podman global arguments for SANDBOX."
  (when (zerop (user-uid))
    (error "Podman sandbox requires a non-root Ende process"))
  (let ((podman (executable-find "podman")))
    (unless podman
      (error "Podman is not installed"))
    (list podman "--log-level=error" "--cgroup-manager=systemd"
          "--hooks-dir" (ende-sandbox-hooks sandbox))))

(defun ende-sandbox--podman-call (sandbox arguments callback)
  "Run Podman ARGUMENTS asynchronously and pass output and error to CALLBACK."
  (condition-case caught
      (ende-sandbox--spawn
       sandbox (append (ende-sandbox--podman-prefix sandbox) arguments) nil
       (lambda (code stdout stderr out-truncated err-truncated _timed-out)
         (if (and (zerop code) (not out-truncated) (not err-truncated))
             (funcall callback stdout nil)
           (funcall callback nil
                    (format "Podman %s failed (status %d): %s%s"
                            (car arguments) code
                            (string-trim (concat stderr stdout))
                            (if (or out-truncated err-truncated)
                                " (truncated)" ""))))))
    ((error quit) (funcall callback nil (error-message-string caught)))))

(defun ende-sandbox--container-running-p (sandbox callback)
  "Pass whether SANDBOX's container is running, or an error, to CALLBACK."
  (ende-sandbox--podman-call
   sandbox (list "inspect" "--format" "{{.State.Running}}"
                 (ende-sandbox-container sandbox))
   (lambda (output error)
     (cond
      (error (funcall callback nil error))
      ((string-match "\\(?:\\`\\|[\n\r]\\)true[ \t\r\n]*\\'" output)
       (funcall callback t nil))
      ((string-match "\\(?:\\`\\|[\n\r]\\)false[ \t\r\n]*\\'" output)
       (funcall callback nil nil))
      (t (funcall callback nil
                  (format "Podman returned an invalid container state: %s"
                          (string-trim output))))))))

(defun ende-sandbox--podman-action (sandbox action callback)
  "Run container ACTION for SANDBOX, then call CALLBACK with output and error."
  (ende-sandbox--podman-call
   sandbox (list action (ende-sandbox-container sandbox)) callback))

(defun ende-sandbox--operation-current-p (operation)
  "Return non-nil while OPERATION exclusively owns its active sandbox."
  (let ((sandbox (ende-sandbox--operation-sandbox operation)))
    (and (not (ende-sandbox--operation-finished operation))
         (not (ende-sandbox-closing sandbox))
         (eq operation (ende-sandbox-operation sandbox))
         (eq sandbox (ende-sandbox-for-owner (ende-sandbox-owner sandbox))))))

(defun ende-sandbox--finish-operation (operation value error)
  "Release OPERATION and deliver VALUE and ERROR exactly once."
  (unless (ende-sandbox--operation-finished operation)
    (let ((sandbox (ende-sandbox--operation-sandbox operation))
          (register (ende-sandbox--operation-register-cancellation operation)))
      (setf (ende-sandbox--operation-finished operation) t)
      (when (eq operation (ende-sandbox-operation sandbox))
        (setf (ende-sandbox-operation sandbox) nil))
      (when register (ignore-errors (funcall register nil)))
      (funcall (ende-sandbox--operation-callback operation) value error))))

(defun ende-sandbox--complete-operation (operation value error)
  "Finish current OPERATION, retiring its sandbox on ERROR."
  (when (ende-sandbox--operation-current-p operation)
    (if error
        (ende-sandbox--close
         (ende-sandbox--operation-sandbox operation) t error)
      (ende-sandbox--finish-operation operation value nil))))

(defun ende-sandbox--operate (sandbox callback register-cancellation function)
  "Exclusively run FUNCTION with a cancellable operation for SANDBOX.
CALLBACK receives the final value and error.  Register cancellation before
calling FUNCTION, which must eventually finish its operation."
  (when (ende-sandbox-closing sandbox)
    (error "the current job's sandbox is closing"))
  (when (ende-sandbox-operation sandbox)
    (error "another operation already owns the current job's sandbox"))
  (let ((operation (ende-sandbox--operation-create
                    :sandbox sandbox :callback callback
                    :register-cancellation register-cancellation)))
    (setf (ende-sandbox-operation sandbox) operation)
    (condition-case caught
        (progn
          (when register-cancellation
            (funcall register-cancellation
                     (lambda ()
                       (when (ende-sandbox--operation-current-p operation)
                         (ende-sandbox-invalidate sandbox)))))
          (when (ende-sandbox--operation-current-p operation)
            (funcall function operation)))
      ((error quit)
       (ende-sandbox--complete-operation
        operation nil (error-message-string caught))))))

(defun ende-sandbox--run-worker
    (operation library function arguments bindings callback)
  "Run workspace FUNCTION in a worker owned by OPERATION."
  (when (ende-sandbox--operation-current-p operation)
    (let ((sandbox (ende-sandbox--operation-sandbox operation)))
      (ende-worker-run-prewarmed
       library function arguments bindings
       (lambda (value error)
         (when (ende-sandbox--operation-current-p operation)
           (funcall callback value error)))
       (lambda (cancel)
         (if (and cancel (not (ende-sandbox--operation-current-p operation)))
             (funcall cancel)
           (setf (ende-sandbox-worker-cancel sandbox) cancel)))))))

(defun ende-sandbox--cancel-worker (sandbox)
  "Stop SANDBOX's workspace writer before container and file disposal."
  (when-let* ((cancel (ende-sandbox-worker-cancel sandbox)))
    (funcall cancel)
    (setf (ende-sandbox-worker-cancel sandbox) nil)))

(defun ende-sandbox--quiesce (operation function callback)
  "Run async FUNCTION under OPERATION while its container is paused.
FUNCTION receives OPERATION and a value/error continuation.  CALLBACK runs
only after resuming, or reports a management or workspace error."
  (let ((sandbox (ende-sandbox--operation-sandbox operation))
        (paused nil)
        (accessed nil)
        (completed nil))
    (cl-labels
        ((finish (value error)
           (when (and (not completed)
                      (ende-sandbox--operation-current-p operation))
             (setq completed t)
             (funcall callback value error)))
         (resume (value error)
           (when (and (not accessed)
                      (ende-sandbox--operation-current-p operation))
             (setq accessed t)
             (if paused
                 (ende-sandbox--podman-action
                  sandbox "unpause"
                  (lambda (_output resume-error)
                    (finish value (or error resume-error))))
               (finish value error))))
         (access ()
           (when (ende-sandbox--operation-current-p operation)
             (condition-case caught
                 (funcall function operation #'resume)
               ((error quit) (resume nil (error-message-string caught)))))))
      (if (and (ende-sandbox-started sandbox)
               (ende-sandbox-container sandbox))
          (ende-sandbox--container-running-p
           sandbox
           (lambda (running error)
             (when (ende-sandbox--operation-current-p operation)
               (cond
                (error (finish nil error))
                (running
                 (ende-sandbox--podman-action
                  sandbox "pause"
                  (lambda (_output pause-error)
                    (when (ende-sandbox--operation-current-p operation)
                      (if pause-error (finish nil pause-error)
                        (setq paused t)
                        (access))))))
                (t (access))))))
        (access)))))

(defun ende-sandbox-with-quiesced-workspace
    (sandbox function callback &optional register-cancellation)
  "Run async FUNCTION while SANDBOX's container is paused, then call CALLBACK.
FUNCTION receives an operation and a value/error continuation.  Invalidate
SANDBOX if inspecting, pausing, workspace access, or resuming fails."
  (ende-sandbox--operate
   sandbox callback register-cancellation
   (lambda (operation)
     (ende-sandbox--quiesce
      operation function
      (lambda (value error)
        (ende-sandbox--complete-operation operation value error))))))

(defun ende-sandbox--capture (value chunk)
  "Append CHUNK to capture VALUE, keeping at most `ende-sandbox--output-limit'."
  (let ((remaining (- ende-sandbox--output-limit (length (car value)))))
    (when (> remaining 0)
      (setcar value (concat (car value) (substring chunk 0 (min remaining
								(length chunk))))))
    (when (> (length chunk) remaining)
      (setcdr value t))))

(defun ende-sandbox--spawn (sandbox command timeout callback)
  "Run argv COMMAND asynchronously for SANDBOX.
TIMEOUT is in seconds.  CALLBACK receives exit code, output, and truncation
flags.  A nil TIMEOUT preserves an indefinite management wait."
  (let ((stdout (list ""))
        (stderr (list ""))
        (stderr-process nil)
        (process nil)
        (timer nil)
        (finished nil)
        (timed-out nil)
        (environment (ende-sandbox--environment)))
    (cl-labels
        ((finish (code)
           (unless finished
             (setq finished t)
             (when (timerp timer)
               (cancel-timer timer))
             (when (eq process (ende-sandbox-process sandbox))
               (setf (ende-sandbox-process sandbox) nil))
             (when (and stderr-process (process-live-p stderr-process))
               (ignore-errors (accept-process-output stderr-process 0))
               (ignore-errors (delete-process stderr-process)))
             (funcall callback code (car stdout) (car stderr)
                      (cdr stdout) (cdr stderr) timed-out)))
         (on-timeout ()
           (unless finished
             (setq timed-out t)
             (when (and process (process-live-p process))
               (delete-process process))
             (finish 124)))
         (on-sentinel (proc _event)
           (unless (process-live-p proc)
             (finish (if timed-out
                         124
                       (if (eq (process-status proc) 'exit)
                           (process-exit-status proc)
                         125))))))
      (condition-case caught
          (progn
            (setq stderr-process
                  (make-pipe-process
                   :name (generate-new-buffer-name "ende-sandbox-stderr")
                   :buffer nil :noquery t :coding 'utf-8-unix
                   :filter (lambda (_proc chunk)
                             (ende-sandbox--capture stderr chunk)))
                  process
                  (let ((process-environment environment))
                    (make-process
                     :name (generate-new-buffer-name "ende-sandbox-podman")
                     :command command :buffer nil :stderr stderr-process
                     :connection-type 'pipe :coding 'utf-8-unix :noquery t
                     :filter (lambda (_proc chunk)
                               (ende-sandbox--capture stdout chunk))
                     :sentinel #'on-sentinel))
                  timer (and timeout (run-at-time timeout nil #'on-timeout)))
            (process-put process 'ende-sandbox-timer timer)
            (setf (ende-sandbox-process sandbox) process)
            process)
        (error
         (when (and stderr-process (process-live-p stderr-process))
           (delete-process stderr-process))
         (signal (car caught) (cdr caught)))))))

(defun ende-sandbox--cancel-process (sandbox)
  "Cancel SANDBOX's current host-side Podman process."
  (let ((process (ende-sandbox-process sandbox)))
    (setf (ende-sandbox-process sandbox) nil)
    (when (processp process)
      (let ((timer (process-get process 'ende-sandbox-timer)))
        (when (timerp timer)
          (cancel-timer timer)))
      (when (process-live-p process)
        (delete-process process)))))

(defun ende-sandbox--new-container-name (sandbox)
  "Return a private name for SANDBOX's next container."
  (format "ende-%d-%x-%x" (ende-sandbox-owner sandbox)
          (truncate (float-time)) (random most-positive-fixnum)))

(defun ende-sandbox--run-container-argv (sandbox name)
  "Return the fixed Podman startup argv for SANDBOX named NAME."
  (append
   (ende-sandbox--podman-prefix sandbox)
   (list "run" "--detach" "--pull=never" "--network=none"
         "--pid=private" "--ipc=private" "--uts=private"
         "--user=0" "--cap-drop=all"
         "--security-opt=no-new-privileges" "--memory=2g"
         "--pids-limit=256" "--http-proxy=false" "--no-hosts"
         "--image-volume=ignore" "--stop-timeout=2" "--name" name
         "--volume"
         (format "%s:/workspace:rw,rprivate,Z"
                 (ende-sandbox-workspace sandbox))
         "--entrypoint" "/bin/sh" (ende-sandbox-image sandbox)
         "-c" "while :; do sleep 3600; done")))

(defun ende-sandbox--remove-container (sandbox callback)
  "Remove SANDBOX's container, then call CALLBACK with success status."
  (let ((name (ende-sandbox-container sandbox)))
    (if (null name)
        (funcall callback t)
      (condition-case nil
          (ende-sandbox--spawn
           sandbox
           (append (ende-sandbox--podman-prefix sandbox)
                   (list "rm" "--force" "--ignore" name))
           ende-sandbox--cleanup-timeout
           (lambda (code _stdout _stderr _out-truncated _err-truncated timed-out)
             (let ((success (and (not timed-out) (= code 0))))
               (when success
                 (setf (ende-sandbox-container sandbox) nil
                       (ende-sandbox-restart-required sandbox) nil))
               (funcall callback success))))
        (error
         (funcall callback nil))))))

(defun ende-sandbox--dispose (sandbox)
  "Forget stopped SANDBOX and remove its tmpfs files in a worker."
  (unless (ende-sandbox-cleanup-pending sandbox)
    (when (ende-sandbox-container sandbox)
      (error "cannot dispose a sandbox before its container is removed"))
    (setf (ende-sandbox-closing sandbox) t)
    (ende-sandbox--cancel-worker sandbox)
    (ende-sandbox--cancel-process sandbox)
    (setf (ende-sandbox-cleanup-pending sandbox) t)
    (cl-pushnew sandbox ende-sandbox--retired :test #'eq)
    (when (eq sandbox (ende-sandbox-for-owner (ende-sandbox-owner sandbox)))
      (remhash (ende-sandbox-owner sandbox) ende-sandbox--by-owner))
    (ende-worker-run
     'ende-sandbox #'ende-sandbox--delete-tree
     (list (ende-sandbox-base sandbox)) nil
     (lambda (_value error)
       (setf (ende-sandbox-cleanup-pending sandbox) nil)
       (if error
           (message "Ende sandbox cleanup failed: %s" error)
         (setq ende-sandbox--retired (delq sandbox ende-sandbox--retired)))))))

(defun ende-sandbox--close (sandbox invalidate &optional error)
  "Stop SANDBOX before deleting its tmpfs workspace.
When INVALIDATE is non-nil, make it unavailable to its owner immediately."
  (when (and sandbox invalidate
             (eq sandbox (ende-sandbox-for-owner (ende-sandbox-owner sandbox))))
    (remhash (ende-sandbox-owner sandbox) ende-sandbox--by-owner))
  (when (and sandbox (not (ende-sandbox-cleanup-pending sandbox)))
    (cl-pushnew sandbox ende-sandbox--retired :test #'eq)
    (setf (ende-sandbox-closing sandbox) t
          (ende-sandbox-cleanup-pending sandbox) t)
    (condition-case caught
        (progn
          (ende-sandbox--cancel-worker sandbox)
          (ende-sandbox--cancel-process sandbox)
          (unwind-protect
              (when-let* ((operation (ende-sandbox-operation sandbox)))
                (ende-sandbox--finish-operation
                 operation nil (or error "sandbox operation cancelled")))
            (if (ende-sandbox-container sandbox)
                (ende-sandbox--remove-container
                 sandbox
                 (lambda (success)
                   (setf (ende-sandbox-cleanup-pending sandbox) nil)
                   (when success
                     (ende-sandbox--dispose sandbox))))
              (setf (ende-sandbox-cleanup-pending sandbox) nil)
              (ende-sandbox--dispose sandbox))))
      ((error quit)
       (setf (ende-sandbox-cleanup-pending sandbox) nil)
       (when-let* ((operation (ende-sandbox-operation sandbox)))
         (ende-sandbox--finish-operation
          operation nil (or error (error-message-string caught))))
       (message "Ende sandbox cleanup failed: %s" (error-message-string caught))))))

(defun ende-sandbox-cleanup-owner (owner)
  "Stop OWNER's container before deleting its tmpfs workspace."
  (dolist (sandbox (delete-dups
                    (cons (ende-sandbox-for-owner owner)
                          (cl-remove-if-not
                           (lambda (sandbox) (= owner (ende-sandbox-owner sandbox)))
                           ende-sandbox--retired))))
    (ende-sandbox--close sandbox nil)))

(defun ende-sandbox-invalidate (sandbox)
  "Retire SANDBOX so its owner cannot reuse a stale workspace."
  (ende-sandbox--close sandbox t))

(defun ende-sandbox--job-event (job event)
  "Clean up a sandbox when its owning JOB settles."
  (when (eq (plist-get event :type) :settled)
    (ende-sandbox-cleanup-owner (ende-job-id job))))

(add-hook 'ende-job-event-functions #'ende-sandbox--job-event)

(defun ende-sandbox--workspace-target (sandbox relative)
  "Resolve workspace-relative RELATIVE beneath SANDBOX without symlinks."
  (unless (and (stringp relative)
               (not (string-empty-p relative))
               (not (file-name-absolute-p relative))
               (not (file-remote-p relative))
               (not (string-match-p "[[:cntrl:]]" relative))
               (not (string-match-p "\\\\" relative))
               (not (string-match-p "\\`/" relative))
               (not (cl-some (lambda (part)
                               (member part '("" "." "..")))
                             (split-string relative "/" nil))))
    (error "sandbox source must be a plain workspace-relative path"))
  (let* ((workspace (ende-sandbox-workspace sandbox))
         (components (split-string relative "/" nil))
         (path workspace))
    (unless (and (file-directory-p workspace) (not (file-symlink-p workspace)))
      (error "sandbox workspace is not a directory"))
    (dolist (component components)
      (setq path (ende-sandbox--join-component path component))
      (when (file-symlink-p path)
        (error "sandbox source paths may not contain symlinks")))
    path))

(defun ende-sandbox--workspace-path (sandbox relative)
  "Resolve existing regular RELATIVE beneath SANDBOX without symlinks."
  (let ((path (ende-sandbox--workspace-target sandbox relative)))
    (unless (and (file-regular-p path) (not (file-symlink-p path)))
      (error "sandbox source must be a regular file"))
    path))

(defun ende-sandbox-sync-file (sandbox source relative)
  "Replace RELATIVE in SANDBOX with regular file SOURCE."
  (unless (and (stringp source) (not (file-remote-p source))
               (not (file-symlink-p source)) (file-regular-p source))
    (error "sandbox sync source must be a regular local file"))
  (let* ((destination (ende-sandbox--workspace-target sandbox relative))
         (components (butlast (split-string relative "/" nil)))
         (directory (ende-sandbox-workspace sandbox)))
    (dolist (component components)
      (setq directory (ende-sandbox--join-component directory component))
      (cond
       ((file-symlink-p directory)
        (error "sandbox source paths may not contain symlinks"))
       ((file-directory-p directory) nil)
       ((file-exists-p directory)
        (error "sandbox parent path is not a directory"))
       (t
        (make-directory directory)
        (unless (and (file-directory-p directory)
                     (not (file-symlink-p directory)))
          (error "sandbox parent path is not a directory")))))
    (when (or (file-exists-p destination) (file-symlink-p destination))
      (unless (file-regular-p destination)
        (error "sandbox destination is not a regular file")))
    (let ((stage (make-temp-file (expand-file-name ".ende-sync-" directory))))
      (unwind-protect
          (progn
            (copy-file source stage t t)
            (set-file-modes stage (file-modes source))
            (ende-sandbox--workspace-target sandbox relative)
            (when (or (file-exists-p destination) (file-symlink-p destination))
              (unless (file-regular-p destination)
                (error "sandbox destination is not a regular file")))
            (rename-file stage destination t)
            (setq stage nil))
        (when stage
          (delete-file stage))))
    destination))

(defun ende-sandbox-delete-file (sandbox relative)
  "Remove RELATIVE from SANDBOX if present."
  (let ((path (ende-sandbox--workspace-target sandbox relative)))
    (when (or (file-exists-p path) (file-symlink-p path))
      (unless (and (file-regular-p path) (not (file-symlink-p path)))
        (error "sandbox deletion target is not a regular file"))
      (delete-file path))
    path))

(defun ende-sandbox-source-file (owner relative)
  "Return OWNER's regular workspace file named RELATIVE."
  (let ((sandbox (ende-sandbox-for-owner owner)))
    (unless (and sandbox (not (ende-sandbox-closing sandbox)))
      (error "the current job has no active sandbox"))
    (ende-sandbox--workspace-path sandbox relative)))

(defun ende-sandbox--cwd (sandbox cwd)
  "Resolve model-supplied CWD to a safe directory inside SANDBOX."
  (unless (and (stringp cwd) (not (string-empty-p cwd))
               (not (file-name-absolute-p cwd))
               (not (file-remote-p cwd))
               (not (string-match-p (string 0) cwd)))
    (error "cwd must be a non-empty relative workspace path"))
  (when (member ".." (split-string cwd "/" t))
    (error "cwd may not escape /workspace"))
  (let* ((components (cl-remove-if
                      (lambda (component)
                        (or (string-empty-p component)
                            (equal component ".")))
                      (split-string cwd "/" nil)))
         (relative (if components (mapconcat #'identity components "/") "."))
         (container-path (if components
                             (ende-sandbox--join-relative-path
                              "/workspace" relative)
                           "/workspace"))
         (host-path (if components
                        (ende-sandbox--join-relative-path
                         (ende-sandbox-workspace sandbox) relative)
                      (ende-sandbox-workspace sandbox)))
         (current (ende-sandbox-workspace sandbox)))
    (dolist (component components)
      (setq current (ende-sandbox--join-component current component))
      (when (file-symlink-p current)
        (error "cwd may not traverse workspace symlinks")))
    (unless (file-directory-p host-path)
      (error "cwd is not a workspace directory: %s" cwd))
    container-path))

(defun ende-sandbox--result (code stdout stderr stdout-truncated
                                  stderr-truncated)
  "Build the bounded result object for one sandbox command."
  (list :exit_code code :process_ok (= code 0)
        :stdout stdout :stderr stderr
        :stdout_truncated (if stdout-truncated t :json-false)
        :stderr_truncated (if stderr-truncated t :json-false)))

(defun ende-sandbox--exec (sandbox command cwd timeout callback)
  "Execute COMMAND in SANDBOX at CWD and call CALLBACK with its result."
  (let ((container (ende-sandbox-container sandbox))
        (container-cwd (ende-sandbox--cwd sandbox cwd)))
    (ende-sandbox--spawn
     sandbox
     (append (ende-sandbox--podman-prefix sandbox)
             (list "exec" "--workdir" container-cwd container
                   "/bin/sh" "-c" command))
     timeout
     (lambda (code stdout stderr stdout-truncated stderr-truncated timed-out)
       (unless (ende-sandbox-closing sandbox)
         (if timed-out
           (progn
             (setf (ende-sandbox-restart-required sandbox) t)
             (ende-sandbox--remove-container
              sandbox
              (lambda (removed)
                (if removed
                    (funcall callback
                             (ende-sandbox--result
                              124 stdout stderr stdout-truncated stderr-truncated)
                             nil)
                  (funcall callback nil "sandbox timed out and could not be stopped")))))
         (funcall callback
                  (ende-sandbox--result code stdout stderr
                                        stdout-truncated stderr-truncated)
                  nil)))))))

(defun ende-sandbox--startup-error
    (code stdout stderr stdout-truncated stderr-truncated timed-out)
  "Describe failed Podman startup with its bounded diagnostic output."
  (concat
   "sandbox could not be started: "
   (if timed-out
       (format "Podman startup timed out after %d seconds (exit status %d)"
               ende-sandbox--startup-timeout code)
     (format "Podman exited with status %d" code))
   (unless (string-empty-p stderr)
     (format "\nPodman stderr%s:\n%s"
             (if stderr-truncated " (truncated)" "") stderr))
   (unless (string-empty-p stdout)
     (format "\nPodman stdout%s:\n%s"
             (if stdout-truncated " (truncated)" "") stdout))))

(defun ende-sandbox--start-container (sandbox command cwd timeout callback)
  "Start SANDBOX's private container and execute COMMAND."
  (let ((name (ende-sandbox--new-container-name sandbox)))
    (setf (ende-sandbox-container sandbox) name)
    (cl-labels
        ((fail-start (message)
           (ende-sandbox--remove-container
            sandbox
            (lambda (removed)
              (unless (ende-sandbox-closing sandbox)
                (when (and removed (not (ende-sandbox-started sandbox)))
                  (ende-sandbox--dispose sandbox))
                (unless removed
                  (setf (ende-sandbox-restart-required sandbox) t))
                (funcall callback nil message))))))
      (condition-case caught
          (ende-sandbox--spawn
           sandbox (ende-sandbox--run-container-argv sandbox name)
           ende-sandbox--startup-timeout
           (lambda (code stdout stderr stdout-truncated stderr-truncated timed-out)
             (cond
              ((ende-sandbox-closing sandbox) nil)
              ((and (not timed-out) (= code 0))
               (setf (ende-sandbox-started sandbox) t)
               (condition-case caught
                   (ende-sandbox--exec sandbox command cwd timeout callback)
                 (error
                  (funcall callback nil (error-message-string caught)))))
              (t
               (fail-start
                (ende-sandbox--startup-error
                 code stdout stderr stdout-truncated stderr-truncated timed-out))))))
        (error
         (fail-start (format "sandbox could not be started: %s"
                             (error-message-string caught))))))))

(defun ende-sandbox--ensure-container (sandbox command cwd timeout callback)
  "Start a replacement container when needed, then run COMMAND."
  (cond
   ((ende-sandbox-closing sandbox)
    (funcall callback nil "the current job's sandbox is closing"))
   ((and (ende-sandbox-container sandbox)
         (not (ende-sandbox-restart-required sandbox)))
    (ende-sandbox--exec sandbox command cwd timeout callback))
   ((ende-sandbox-container sandbox)
    (ende-sandbox--remove-container
     sandbox
     (lambda (removed)
       (unless (ende-sandbox-closing sandbox)
         (if removed
             (ende-sandbox--start-container
              sandbox command cwd timeout callback)
           (funcall callback nil "sandbox container could not be stopped"))))))
   (t
    (ende-sandbox--start-container sandbox command cwd timeout callback))))

(defun ende-sandbox-run-command
    (sandbox command cwd timeout callback register-cancellation &optional prepare)
  "Run COMMAND in SANDBOX asynchronously.
CALLBACK receives a result and error.  REGISTER-CANCELLATION associates the
job-scoped cancellation function supplied by Ende's tool dispatcher.
Optional PREPARE receives an operation and continuation before first use."
  (unless (and (stringp command) (not (string-match-p (string 0) command)))
    (error "command must be a string without NUL characters"))
  (unless (and (integerp timeout) (<= 1 timeout 3600))
    (error "timeout must be an integer from 1 to 3600 seconds"))
  (ende-sandbox--operate
   sandbox callback register-cancellation
   (lambda (operation)
     (cl-labels
         ((run ()
            (when (ende-sandbox--operation-current-p operation)
              (condition-case caught
                  (progn
                    (ende-sandbox--cwd sandbox cwd)
                    (ende-sandbox--ensure-container
                     sandbox command cwd timeout
                     (lambda (result error)
                       (ende-sandbox--finish-operation operation result error))))
                ((error quit)
                 (ende-sandbox--finish-operation
                  operation nil (error-message-string caught)))))))
       (if prepare
           (funcall prepare operation
                    (lambda (_value error)
                      (if error
                          (ende-sandbox--complete-operation operation nil error)
                        (run))))
         (run))))))

(provide 'ende-sandbox)

;;; ende-sandbox.el ends here
