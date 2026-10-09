;;; ende-tool-store.el --- SQLite archive for tool payloads -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools, llm

;;; Commentary:

;; Immutable exact payloads referenced by generated chat tool sections.

;;; Code:

(require 'cl-lib)
(require 'sqlite)
(require 'subr-x)

(defcustom ende-tool-store-file
  (expand-file-name "ende/tool-payloads.sqlite3" user-emacs-directory)
  "SQLite database holding exact tool request and result payloads."
  :type 'file
  :group 'ende)

(defconst ende-tool-store--schema-version 1)
(defvar ende-tool-store--next-id 0)

(defun ende-tool-store--filename ()
  "Return the expanded configured tool-store filename."
  (expand-file-name ende-tool-store-file))

(defun ende-tool-store--check-sqlite ()
  "Signal an actionable error if this Emacs cannot use SQLite."
  (unless (and (fboundp 'sqlite-available-p) (sqlite-available-p))
    (user-error
     "Tool payload storage requires an Emacs build with SQLite support")))

(defun ende-tool-store--initialize (database filename)
  "Initialize schema-version-zero DATABASE at FILENAME."
  (let ((version (caar (sqlite-select database "PRAGMA user_version"))))
    (cond
     ((= version 0)
      (condition-case error-data
          (progn
            (sqlite-execute
             database
             (concat "CREATE TABLE tool_payloads ("
                     "execution_id TEXT NOT NULL, "
                     "kind TEXT NOT NULL CHECK (kind IN ('request', 'result')), "
                     "payload TEXT NOT NULL, "
                     "PRIMARY KEY (execution_id, kind))"))
            (sqlite-execute database "PRAGMA user_version = 1"))
        (error
         (error "Cannot initialize tool payload database %s: %s"
                filename (error-message-string error-data)))))
     ((/= version ende-tool-store--schema-version)
      (error "Unsupported tool payload database schema version %s in %s; use a fresh database by changing `ende-tool-store-file'"
             version filename))
     (t nil))))

(defun ende-tool-store--open (filename create)
  "Open and validate FILENAME.  CREATE allows making a missing database."
  (ende-tool-store--check-sqlite)
  (let ((exists (file-exists-p filename)))
    (when (or exists create)
      (when create
        (make-directory (file-name-directory filename) t))
      (let ((database
             (condition-case error-data
               (sqlite-open filename nil)
             (error
                (error "Cannot open tool payload database %s: %s. Check the file or set `ende-tool-store-file' to a fresh database"
                       filename (error-message-string error-data))))))
        (condition-case error-data
            (progn
              (ende-tool-store--initialize database filename)
              database)
          (error
           (sqlite-close database)
           (signal (car error-data) (cdr error-data))))))))

(defun ende-tool-store--call-with-database (create function)
  "Call FUNCTION with an initialized database, creating it when CREATE."
  (let* ((filename (ende-tool-store--filename))
         (database (ende-tool-store--open filename create)))
    (when database
      (unwind-protect
          (condition-case error-data
              (funcall function database)
            (error
             (error "Tool payload database %s failed: %s"
                    filename (error-message-string error-data))))
        (sqlite-close database)))))

(defun ende-tool-store-put (execution-id kind payload)
  "Store immutable JSON PAYLOAD for EXECUTION-ID and KIND."
  (unless (and (stringp execution-id) (not (string-empty-p execution-id))
               (member kind '(request result))
               (stringp payload))
    (error "Invalid tool payload record"))
  (let ((kind (symbol-name kind)))
    (ende-tool-store--call-with-database
     t
     (lambda (database)
       (sqlite-execute
        database
        "INSERT OR IGNORE INTO tool_payloads (execution_id, kind, payload) VALUES (?, ?, ?)"
        (vector execution-id kind payload))
       (let ((stored (caar (sqlite-select
                            database
                            "SELECT payload FROM tool_payloads WHERE execution_id = ? AND kind = ?"
                            (vector execution-id kind)))))
         (unless (equal stored payload)
           (error "Conflicting immutable tool payload for execution %s (%s)"
                  execution-id kind)))))))

(defun ende-tool-store-get (execution-id kind)
  "Return the exact payload for EXECUTION-ID and KIND, or nil if absent."
  (unless (and (stringp execution-id) (member kind '(request result)))
    (error "Invalid tool payload identity"))
  (ende-tool-store--call-with-database
   nil
   (lambda (database)
     (caar (sqlite-select
            database
            "SELECT payload FROM tool_payloads WHERE execution_id = ? AND kind = ?"
            (vector execution-id (symbol-name kind)))))))

(defun ende-tool-store--footprint ()
  "Return the configured filename and existing database files with sizes."
  (let* ((filename (ende-tool-store--filename))
         (files (cl-remove-if-not
                 #'file-exists-p
                 (list filename (concat filename "-wal")
                       (concat filename "-shm")
                       (concat filename "-journal")))))
    (list filename
          (mapcar (lambda (file)
                    (cons file (file-attribute-size (file-attributes file))))
                  files))))

;;;###autoload
(defun ende-tool-store-size ()
  "Report the tool payload database's on-disk footprint, including sidecars."
  (interactive)
  (pcase-let* ((`(,filename ,files) (ende-tool-store--footprint)))
    (if (null files)
        (progn
          (message "Tool payload database does not exist: %s" filename)
          nil)
      (let ((bytes (apply #'+ (mapcar #'cdr files))))
        (message "Tool payload database %s: %s across %s"
                 filename bytes
                 (mapconcat (lambda (entry)
                              (format "%s (%s bytes)" (car entry) (cdr entry)))
                            files ", "))
        bytes))))

(defun ende-tool-store--new-execution-id ()
  "Return a fresh Ende-generated ID, unique across local Emacs processes."
  (format "ende-%s-%s"
          (emacs-pid)
          (secure-hash
           'sha256
           (format "%s:%s:%s:%s:%s:%s"
                   (system-name) (emacs-pid) (current-time) (random)
                   (cl-incf ende-tool-store--next-id)
                   (make-temp-name "ende-tool-")))))

(provide 'ende-tool-store)

;;; ende-tool-store.el ends here
