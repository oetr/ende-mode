;;; deslop-project.el --- Project-scoped source access for deslop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools

;;; Commentary:

;; Project roots, path validation, visiting-buffer discovery, and stable
;; buffer identities used by deslop's model tools.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'subr-x)
(require 'deslop-core)

(defcustom deslop-project-root-directory nil
  "Explicit project root used by deslop tools.

When nil, the project containing the current buffer is used.  A tool session
may also pass a root directly to its dispatcher."
  :type '(choice (const :tag "Current project" nil)
                 (directory :tag "Project root"))
  :group 'deslop)

(defcustom deslop-project-excluded-path-patterns
  '("\\(?:\\`\\|/\\)\\.git\\(?:/\\|\\'\\)"
    "\\(?:\\`\\|/\\)\\.hg\\(?:/\\|\\'\\)"
    "\\(?:\\`\\|/\\)\\.svn\\(?:/\\|\\'\\)"
    "\\(?:\\`\\|/\\)node_modules\\(?:/\\|\\'\\)"
    "\\(?:\\`\\|/\\)\\.venv\\(?:/\\|\\'\\)"
    "\\(?:\\`\\|/\\)__pycache__\\(?:/\\|\\'\\)"
    "\\(?:\\`\\|/\\)\\.env\\(?:\\.[^/]+\\)*\\(?:/\\|\\'\\)"
    "\\(?:\\`\\|/\\)credentials?\\(?:\\.[^/]+\\)?\\'")
  "Regular expressions for project-relative paths hidden from tools."
  :type '(repeat regexp)
  :group 'deslop)

(defcustom deslop-project-max-file-bytes (* 10 1024 1024)
  "Maximum file size read into a tool request.

Nil permits files of any size."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Maximum bytes"))
  :group 'deslop)

(defvar deslop-project--next-buffer-id 0
  "Next numeric ID assigned to a project buffer.")

(defvar deslop-project--buffer-ids (make-hash-table :test #'eq)
  "Buffer to stable ID mapping for the current Emacs session.")

(defvar deslop-project--buffers-by-id (make-hash-table :test #'equal)
  "Stable ID to buffer mapping for the current Emacs session.")

(defun deslop-project--forget-buffer ()
  "Forget the stable ID of the buffer being killed."
  (let ((id (gethash (current-buffer) deslop-project--buffer-ids)))
    (when id
      (remhash id deslop-project--buffers-by-id)
      (remhash (current-buffer) deslop-project--buffer-ids))))

(defun deslop-project--normalize-root (root)
  "Return canonical directory ROOT, or signal an error."
  (unless (and (stringp root) (file-directory-p root))
    (error "deslop project root is not a directory: %s" root))
  (file-name-as-directory (file-truename root)))

(defun deslop-project-root (&optional buffer)
  "Return the canonical project root for BUFFER.

Use `deslop-project-root-directory' when it is non-nil; otherwise discover
the project from BUFFER or the current buffer."
  (let* ((buffer (or buffer (current-buffer)))
         (project-root
          (or deslop-project-root-directory
              (and (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (let ((project (project-current nil)))
                       (and project (project-root project))))))))
    (unless project-root
      (user-error "deslop: current buffer is not in a project"))
    (deslop-project--normalize-root project-root)))

(defun deslop-project--excluded-p (relative-path)
  "Return non-nil when RELATIVE-PATH is excluded from tool access."
  (cl-some (lambda (pattern)
             (string-match-p pattern relative-path))
           deslop-project-excluded-path-patterns))

(defun deslop-project--canonical-path (path root &optional for-create)
  "Return validated canonical PATH below ROOT.

PATH must be relative to ROOT.  When FOR-CREATE is non-nil, PATH itself may
not exist, but its existing parent directory must still be inside ROOT."
  (unless (and (stringp path) (not (string-empty-p path)))
    (error "project path must be a non-empty string"))
  (when (file-name-absolute-p path)
    (error "absolute paths are not allowed in project tools"))
  (let* ((root (deslop-project--normalize-root root))
         (expanded (expand-file-name path root))
         (canonical
          (if (file-exists-p expanded)
              (file-truename expanded)
            (if for-create
                (let ((parent (file-name-directory expanded)))
                  (unless (file-directory-p parent)
                    (error "parent directory does not exist: %s"
                           (file-name-directory path)))
                  (expand-file-name (file-name-nondirectory expanded)
                                    (file-truename parent)))
              expanded)))
         (relative (file-relative-name canonical root)))
    (unless (file-in-directory-p canonical root)
      (error "path is outside the project: %s" path))
    (when (deslop-project--excluded-p relative)
      (error "path is excluded from project tools: %s" path))
    canonical))

(defun deslop-project-path (path &optional root for-create)
  "Return validated canonical PATH below ROOT.

ROOT defaults to the current project.  FOR-CREATE allows a new file in an
existing project directory."
  (deslop-project--canonical-path
   path (or root (deslop-project-root)) for-create))

(defun deslop-project-relative-path (path &optional root)
  "Return PATH relative to ROOT after validating its project membership."
  (let* ((root (or root (deslop-project-root)))
         (canonical (deslop-project-path path root)))
    (file-relative-name canonical (deslop-project--normalize-root root))))

(defun deslop-project--assert-readable-file (path)
  "Signal when PATH is not a readable regular file or exceeds its limit."
  (unless (file-regular-p path)
    (error "project source is not a regular file: %s" path))
  (unless (file-readable-p path)
    (error "project source is not readable: %s" path))
  (when (and deslop-project-max-file-bytes
             (> (file-attribute-size (file-attributes path))
                deslop-project-max-file-bytes))
    (error "project file is larger than the tool limit: %s" path))
  path)

(defun deslop-project--buffer-path (buffer)
  "Return canonical visiting path for BUFFER, or nil."
  (when (and (buffer-live-p buffer)
             (buffer-file-name buffer))
    (condition-case nil
        (file-truename (buffer-file-name buffer))
      (error nil))))

(defun deslop-project-buffer-allowed-p (buffer &optional root)
  "Return non-nil when BUFFER visits an allowed file below ROOT."
  (let* ((root (or root (deslop-project-root buffer)))
         (path (deslop-project--buffer-path buffer)))
    (and path
         (file-in-directory-p path (deslop-project--normalize-root root))
         (not (deslop-project--excluded-p
               (file-relative-name path (deslop-project--normalize-root root)))))))

(defun deslop-project-visiting-buffer (path &optional root)
  "Return the live buffer visiting PATH below ROOT, or nil."
  (let ((canonical (deslop-project-path path root)))
    (cl-find-if
     (lambda (buffer)
       (let ((buffer-path (deslop-project--buffer-path buffer)))
         (and buffer-path (file-equal-p buffer-path canonical))))
     (buffer-list))))

(defun deslop-project-buffer-id (buffer)
  "Return a stable opaque ID for BUFFER."
  (or (gethash buffer deslop-project--buffer-ids)
      (let ((id (format "buffer-%d"
                        (cl-incf deslop-project--next-buffer-id))))
        (puthash buffer id deslop-project--buffer-ids)
        (puthash id buffer deslop-project--buffers-by-id)
        (with-current-buffer buffer
          (add-hook 'kill-buffer-hook #'deslop-project--forget-buffer nil t))
        id)))

(defun deslop-project-buffer-for-id (id &optional root)
  "Return allowed live buffer identified by ID, or signal an error."
  (let ((buffer (and (stringp id)
                     (gethash id deslop-project--buffers-by-id))))
    (unless (and (buffer-live-p buffer)
                 (deslop-project-buffer-allowed-p buffer root))
      (error "project buffer does not exist or is not allowed: %s" id))
    buffer))

(defun deslop-project--walk-files (root)
  "Return canonical regular files below ROOT, excluding configured paths."
  (let ((files nil)
        (visited-directories (make-hash-table :test #'equal)))
    (cl-labels
        ((walk (directory)
           (let ((canonical-directory (file-truename directory)))
             (unless (gethash canonical-directory visited-directories)
               (puthash canonical-directory t visited-directories)
               (dolist (entry
                        (directory-files directory t
                                         directory-files-no-dot-files-regexp))
                 (let* ((canonical
                         (condition-case nil
                             (file-truename entry)
                           (error nil)))
                        (relative (and canonical
                                       (file-relative-name canonical root))))
                   (when (and canonical relative
                              (file-in-directory-p canonical root)
                              (not (deslop-project--excluded-p relative)))
                     (cond
                      ((and (file-directory-p entry)
                            (not (file-symlink-p entry)))
                       (walk entry))
                      ((file-regular-p entry)
                       (push canonical files))))))))))
      (walk root))
    (sort (delete-dups files) #'string<)))

(defun deslop-project-files (&optional root)
  "Return all allowed regular project files below ROOT."
  (deslop-project--walk-files
   (deslop-project--normalize-root (or root (deslop-project-root)))))

(defun deslop-project-buffers (&optional root)
  "Return all live visiting buffers allowed below ROOT."
  (let ((root (deslop-project--normalize-root (or root (deslop-project-root)))))
    (cl-remove-if-not
     (lambda (buffer)
       (deslop-project-buffer-allowed-p buffer root))
     (buffer-list))))

(provide 'deslop-project)

;;; deslop-project.el ends here
