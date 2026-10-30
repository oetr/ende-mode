;;; ende-sandbox.el --- Per-job Podman workspaces -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools

;;; Commentary:

;; Each agent job owns one tmpfs snapshot and one disposable rootless container.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ende-project)
(require 'ende-queue)

(cl-defstruct (ende-sandbox (:constructor ende-sandbox--create))
  owner root base workspace hooks container image process started
  restart-required closing cleanup-pending)

(defconst ende-sandbox--root "/dev/shm")
(defconst ende-sandbox--output-limit 65536)
(defconst ende-sandbox--startup-timeout 120)
(defconst ende-sandbox--cleanup-timeout 30)

(defvar ende-sandbox--by-owner (make-hash-table :test #'eql))

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

(defun ende-sandbox-create (owner root image files)
  "Create OWNER's tmpfs snapshot from authorized FILES below ROOT."
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
            (ende-sandbox--copy-snapshot files root workspace)
            (setq sandbox
                  (ende-sandbox--create
                   :owner owner :root root :base base :workspace workspace
                   :hooks hooks :image image))
            (puthash owner sandbox ende-sandbox--by-owner)))
      (unless sandbox
        (when (file-directory-p base)
          (ende-sandbox--delete-tree base))))
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

(defun ende-sandbox--podman-call (sandbox &rest arguments)
  "Synchronously run Podman ARGUMENTS for SANDBOX and return its output."
  (let* ((prefix (ende-sandbox--podman-prefix sandbox))
         (program (car prefix))
         (operation (car arguments))
         (arguments (append (cdr prefix) arguments))
         (process-environment (ende-sandbox--environment)))
    (with-temp-buffer
      (let ((status (apply #'call-process program nil t nil arguments)))
        (unless (and (integerp status) (zerop status))
          (error "Podman %s failed: %s"
                 operation (string-trim (buffer-string))))
        (buffer-string)))))

(defun ende-sandbox--container-running-p (sandbox)
  "Return non-nil when SANDBOX's existing container is running."
  (let ((output
         (ende-sandbox--podman-call
          sandbox "inspect" "--format" "{{.State.Running}}"
          (ende-sandbox-container sandbox))))
    (cond
     ((string-match "\\(?:\\`\\|[\n\r]\\)true[ \t\r\n]*\\'" output) t)
     ((string-match "\\(?:\\`\\|[\n\r]\\)false[ \t\r\n]*\\'" output) nil)
     (t (error "Podman returned an invalid container state: %s"
               (string-trim output))))))

(defun ende-sandbox--podman-action (sandbox action)
  "Run container ACTION for SANDBOX."
  (ende-sandbox--podman-call sandbox action (ende-sandbox-container sandbox)))

(defun ende-sandbox-with-quiesced-workspace (sandbox function)
  "Call FUNCTION while SANDBOX's running container is paused.
Invalidate SANDBOX if pausing, workspace access, or unpausing fails."
  (let ((paused nil)
        (completed nil)
        (result nil)
        (failure nil))
    (unwind-protect
        (condition-case caught
            (progn
              (when (and (ende-sandbox-started sandbox)
                         (ende-sandbox-container sandbox)
                         (ende-sandbox--container-running-p sandbox))
                (ende-sandbox--podman-action sandbox "pause")
                (setq paused t))
              (setq result (funcall function)
                    completed t))
          ((error quit)
           (setq failure caught)))
      (when paused
        (condition-case caught
            (ende-sandbox--podman-action sandbox "unpause")
          ((error quit)
           (unless failure
             (setq failure caught)))))
      (when (or failure (not completed))
        (condition-case nil
            (ende-sandbox-invalidate sandbox)
          ((error quit) nil))))
    (when failure
      (signal (car failure) (cdr failure)))
    result))

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
flags."
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
                  timer (run-at-time timeout nil #'on-timeout))
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
  "Forget SANDBOX and remove its tmpfs files."
  (ende-sandbox--cancel-process sandbox)
  (when (eq sandbox (ende-sandbox-for-owner (ende-sandbox-owner sandbox)))
    (remhash (ende-sandbox-owner sandbox) ende-sandbox--by-owner))
  (ende-sandbox--delete-tree (ende-sandbox-base sandbox)))

(defun ende-sandbox--close (sandbox invalidate)
  "Stop SANDBOX before deleting its tmpfs workspace.
When INVALIDATE is non-nil, make it unavailable to its owner immediately."
  (when (and sandbox (not (ende-sandbox-cleanup-pending sandbox)))
    (when (and invalidate
               (eq sandbox (ende-sandbox-for-owner
                            (ende-sandbox-owner sandbox))))
      (remhash (ende-sandbox-owner sandbox) ende-sandbox--by-owner))
    (setf (ende-sandbox-closing sandbox) t)
    (if (ende-sandbox-container sandbox)
        (progn
          (setf (ende-sandbox-cleanup-pending sandbox) t)
          (ende-sandbox--cancel-process sandbox)
          (ende-sandbox--remove-container
           sandbox
           (lambda (success)
             (setf (ende-sandbox-cleanup-pending sandbox) nil)
             (when success
               (ende-sandbox--dispose sandbox)))))
      (progn
        (ende-sandbox--cancel-process sandbox)
        (ende-sandbox--dispose sandbox)))))

(defun ende-sandbox-cleanup-owner (owner)
  "Stop OWNER's container before deleting its tmpfs workspace."
  (ende-sandbox--close (ende-sandbox-for-owner owner) nil))

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
                  nil))))))

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
             (ende-sandbox--remove-container
              sandbox
              (lambda (removed)
                (unless (ende-sandbox-started sandbox)
                  (when removed
                    (ende-sandbox--dispose sandbox)))
                (unless removed
                  (setf (ende-sandbox-restart-required sandbox) t))
                (unless (ende-sandbox-closing sandbox)
                  (funcall callback nil
                           (ende-sandbox--startup-error
                            code stdout stderr stdout-truncated stderr-truncated
                            timed-out)))))))))
      (error
       (ende-sandbox--remove-container
        sandbox
        (lambda (removed)
          (unless (ende-sandbox-started sandbox)
            (when removed
              (ende-sandbox--dispose sandbox)))
          (unless removed
            (setf (ende-sandbox-restart-required sandbox) t))
          (unless (ende-sandbox-closing sandbox)
            (funcall callback nil
                     (format "sandbox could not be started: %s"
                             (error-message-string caught))))))))))

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
       (if removed
           (ende-sandbox--start-container
            sandbox command cwd timeout callback)
         (funcall callback nil "sandbox container could not be stopped")))))
   (t
    (ende-sandbox--start-container sandbox command cwd timeout callback))))

(defun ende-sandbox-run-command
    (sandbox command cwd timeout callback register-cancellation)
  "Run COMMAND in SANDBOX asynchronously.
CALLBACK receives a result and error.  REGISTER-CANCELLATION associates the
job-scoped cancellation function supplied by Ende's tool dispatcher."
  (unless (and (stringp command) (not (string-match-p (string 0) command)))
    (error "command must be a string without NUL characters"))
  (unless (and (integerp timeout) (<= 1 timeout 3600))
    (error "timeout must be an integer from 1 to 3600 seconds"))
  (ende-sandbox--cwd sandbox cwd)
  (let ((completed nil)
        (owner (ende-sandbox-owner sandbox)))
    (cl-labels
        ((finish (result error)
           (unless completed
             (setq completed t)
             (when (and (not (ende-sandbox-closing sandbox))
                        (functionp register-cancellation))
               (funcall register-cancellation nil))
             (unless (ende-sandbox-closing sandbox)
               (funcall callback result error)))))
      (when (functionp register-cancellation)
        (funcall register-cancellation
                 (lambda ()
                   (ende-sandbox-cleanup-owner owner))))
      (condition-case caught
          (ende-sandbox--ensure-container
           sandbox command cwd timeout #'finish)
        (error
         (finish nil (error-message-string caught)))))))

(provide 'ende-sandbox)

;;; ende-sandbox.el ends here
