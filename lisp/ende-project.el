;;; ende-project.el --- Project-scoped source access for ende -*- lexical-binding: t; -*-

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

;; Project roots, path validation, visiting-buffer discovery, and stable
;; buffer identities used by ende's model tools.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'subr-x)
(require 'ende-core)

(defcustom ende-project-root-directory nil
  "Explicit project root used by ende tools.

When nil, the project containing the current buffer is used.  A tool session
may also pass a root directly to its dispatcher."
  :type '(choice (const :tag "Current project" nil)
                 (directory :tag "Project root"))
  :group 'ende)

(defcustom ende-project-excluded-path-patterns
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
  :group 'ende)

(defcustom ende-project-max-file-bytes (* 10 1024 1024)
  "Maximum file size read into a tool request.

Nil permits files of any size."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Maximum bytes"))
  :group 'ende)

(defconst ende-project-cache-limit 4096
  "Maximum successful file-policy decisions retained by one job.")

(cl-defstruct (ende-project-cache (:constructor ende-project-cache-create))
  "Disposable positive file-policy decisions for one policy context."
  context-key
  (allowed (make-hash-table :test #'equal)))

(defun ende-project-cache-context (cache key)
  "Establish KEY as CACHE's context, discarding decisions from other contexts."
  (unless (equal key (ende-project-cache-context-key cache))
    (clrhash (ende-project-cache-allowed cache))
    (setf (ende-project-cache-context-key cache) key)))

(defun ende-project-cache-merge (cache delta)
  "Admit positive decisions in DELTA into CACHE up to its fixed capacity."
  (let ((allowed (ende-project-cache-allowed cache)))
    (dolist (entry delta)
      (when (< (hash-table-count allowed) ende-project-cache-limit)
        (puthash (car entry) (cdr entry) allowed)))))

(defvar ende-project--next-buffer-id 0
  "Next numeric ID assigned to a project buffer.")

(defvar ende-project--buffer-ids (make-hash-table :test #'eq)
  "Buffer to stable ID mapping for the current Emacs session.")

(defvar ende-project--buffers-by-id (make-hash-table :test #'equal)
  "Stable ID to buffer mapping for the current Emacs session.")

(defun ende-project--forget-buffer ()
  "Forget the stable ID of the buffer being killed."
  (let ((id (gethash (current-buffer) ende-project--buffer-ids)))
    (when id
      (remhash id ende-project--buffers-by-id)
      (remhash (current-buffer) ende-project--buffer-ids))))

(defun ende-project-normalize-root (root)
  "Return canonical directory ROOT, or signal an error."
  (unless (and (stringp root) (file-directory-p root))
    (error "ende project root is not a directory: %s" root))
  (file-name-as-directory (file-truename root)))

(defun ende-project-root (&optional buffer)
  "Return the canonical project root for BUFFER.

Use `ende-project-root-directory' when it is non-nil; otherwise discover
the project from BUFFER or the current buffer."
  (let* ((buffer (or buffer (current-buffer)))
         (project-root
          (or ende-project-root-directory
              (and (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (let ((project (project-current nil)))
                       (and project (project-root project))))))))
    (unless project-root
      (user-error "ende: current buffer is not in a project"))
    (ende-project-normalize-root project-root)))

(defun ende-project--excluded-p (relative-path)
  "Return non-nil when RELATIVE-PATH is excluded from tool access."
  (cl-some (lambda (pattern)
             (string-match-p pattern relative-path))
           ende-project-excluded-path-patterns))

(defun ende-project--canonical-path (path root &optional for-create)
  "Return validated canonical PATH below ROOT.

PATH must be relative to ROOT.  When FOR-CREATE is non-nil, PATH itself may
not exist, but its existing parent directory must still be inside ROOT."
  (unless (and (stringp path) (not (string-empty-p path)))
    (error "project path must be a non-empty string"))
  (when (file-name-absolute-p path)
    (error "absolute paths are not allowed in project tools"))
  (let* ((root (ende-project-normalize-root root))
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
    (when (ende-project--excluded-p relative)
      (error "path is excluded from project tools: %s" path))
    canonical))

(defun ende-project-path (path &optional root for-create)
  "Return validated canonical PATH below ROOT.

ROOT defaults to the current project.  FOR-CREATE allows a new file in an
existing project directory."
  (ende-project--canonical-path
   path (or root (ende-project-root)) for-create))

(defun ende-project-relative-path (path &optional root)
  "Return PATH relative to ROOT after validating its project membership."
  (let* ((root (or root (ende-project-root)))
         (canonical (ende-project-path path root)))
    (file-relative-name canonical (ende-project-normalize-root root))))

(defun ende-project-assert-readable-file (path)
  "Signal when PATH is not a readable regular file or exceeds its limit."
  (unless (file-regular-p path)
    (error "project source is not a regular file: %s" path))
  (unless (file-readable-p path)
    (error "project source is not readable: %s" path))
  (when (and ende-project-max-file-bytes
             (> (file-attribute-size (file-attributes path))
                ende-project-max-file-bytes))
    (error "project file is larger than the tool limit: %s" path))
  path)

(defun ende-project-buffer-path (buffer)
  "Return canonical visiting path for BUFFER, or nil."
  (when (and (buffer-live-p buffer)
             (buffer-file-name buffer))
    (condition-case nil
        (file-truename (buffer-file-name buffer))
      (error nil))))

(defun ende-project-buffer-allowed-p (buffer &optional root)
  "Return non-nil when BUFFER visits an allowed file below ROOT."
  (let* ((root (or root (ende-project-root buffer)))
         (path (ende-project-buffer-path buffer)))
    (when path
      (let ((root (ende-project-normalize-root root)))
        (and (file-in-directory-p path root)
             (not (ende-project--excluded-p (file-relative-name path root))))))))

(defun ende-project-visiting-buffer (path &optional root)
  "Return the live buffer visiting PATH below ROOT, or nil."
  (let ((canonical (ende-project-path path root)))
    (cl-find-if
     (lambda (buffer)
       (let ((buffer-path (ende-project-buffer-path buffer)))
         (and buffer-path (file-equal-p buffer-path canonical))))
     (buffer-list))))

(defun ende-project-buffer-id (buffer)
  "Return a stable opaque ID for BUFFER."
  (or (gethash buffer ende-project--buffer-ids)
      (let ((id (format "buffer-%d"
                        (cl-incf ende-project--next-buffer-id))))
        (puthash buffer id ende-project--buffer-ids)
        (puthash id buffer ende-project--buffers-by-id)
        (with-current-buffer buffer
          (add-hook 'kill-buffer-hook #'ende-project--forget-buffer nil t))
        id)))

(defun ende-project-buffer-for-id (id &optional root)
  "Return allowed live buffer identified by ID, or signal an error."
  (let ((buffer (and (stringp id)
                     (gethash id ende-project--buffers-by-id))))
    (unless (and (buffer-live-p buffer)
                 (ende-project-buffer-allowed-p buffer root))
      (error "project buffer does not exist or is not allowed: %s" id))
    buffer))

(defun ende-project--contains-p (path root ignore-case)
  "Test canonical PATH's membership in canonical directory ROOT.
IGNORE-CASE describes the filesystem at ROOT.  Both names must already be
absolute and resolved; the trailing separator enforces component boundaries."
  (string-prefix-p (file-name-as-directory root)
                   (file-name-as-directory path) ignore-case))

(defun ende-project--map-files (root visit &optional descend-p candidate-p)
  "Visit current regular files below canonical ROOT in directory order.
Call VISIT with canonical path, project-relative name, and attributes.
A nil return stops traversal.  Return non-nil only for a complete traversal.
DESCEND-P filters relative directory names; CANDIDATE-P cheaply filters file
names before resolution.  Symlinks to directories are never traversed."
  (let ((seen (make-hash-table :test #'equal))
        (ignore-case (file-name-case-insensitive-p root)))
    (catch 'stopped
    (cl-labels
        ((walk (directory)
          (dolist (entry (directory-files
                          directory t directory-files-no-dot-files-regexp))
            (let* ((relative (file-relative-name entry root))
                   (attributes (file-attributes entry))
                   (type (file-attribute-type attributes)))
                     (cond
               ((eq type t)
                (when (and (not (ende-project--excluded-p relative))
                           (or (null descend-p) (funcall descend-p relative))
                           (not (file-symlink-p entry))
                           (ende-project--contains-p
                            (file-truename entry) root ignore-case))
                  (walk entry)))
               ((and attributes
                     (or (stringp type) (null candidate-p)
                         (funcall candidate-p relative)))
                (let* ((canonical (file-truename entry))
                       (relative (file-relative-name canonical root)))
                  (when (and (ende-project--contains-p canonical root ignore-case)
                             (not (ende-project--excluded-p relative))
                             (or (null candidate-p) (funcall candidate-p relative))
                             (not (gethash canonical seen))
                             (file-regular-p canonical))
                    (puthash canonical t seen)
                    (when (stringp type)
                      (setq attributes (file-attributes canonical)))
                    (unless (funcall visit canonical relative attributes)
                      (throw 'stopped nil))))))))))
      (walk root))
      t)))

(defun ende-project--walk-files (root)
  "Return current canonical regular files below canonical ROOT."
  (let (files)
    (ende-project--map-files
     root (lambda (path _relative _attributes) (push path files) t))
    (nreverse files)))

(defun ende-project-files (&optional root)
  "Return all allowed regular project files below ROOT in directory order."
  (ende-project--walk-files
   (ende-project-normalize-root (or root (ende-project-root)))))

(defun ende-project-buffers (&optional root)
  "Return all live visiting buffers allowed below ROOT."
  (let ((root (ende-project-normalize-root (or root (ende-project-root)))))
    (cl-remove-if-not
     (lambda (buffer)
       (ende-project-buffer-allowed-p buffer root))
     (buffer-list))))

(provide 'ende-project)

;;; ende-project.el ends here
