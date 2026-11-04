;;; ende-tool-store.el --- SQLite archive for tool payloads -*- lexical-binding: t; -*-

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

;; Immutable exact payloads referenced by generated chat tool sections.

;;; Code:

(require 'cl-lib)
(require 'ende-queue)
(require 'json)
(require 'org)
(require 'sqlite)
(require 'subr-x)

(defcustom ende-tool-store-file
  (expand-file-name "ende/tool-payloads.sqlite3" user-emacs-directory)
  "SQLite database holding exact tool request and result payloads."
  :type 'file
  :group 'ende)

(defconst ende-tool-store--schema-version 1)
(defvar ende-tool-store--next-id 0)

(defvar-local ende-tool-store--local-file nil
  "Absolute tool store filename configured by the current Org document.")

(defun ende-tool-store--initialize-buffer ()
  "Initialize the current Org document's tool store setting."
  (when (derived-mode-p 'org-mode)
    (add-hook 'org-mode-hook #'ende-tool-store--initialize-buffer nil t))
  (setq ende-tool-store--local-file
        (when (derived-mode-p 'org-mode)
          (let (values)
            (org-with-wide-buffer
             (goto-char (point-min))
             (let ((case-fold-search t))
               (while (re-search-forward
                       (org-make-options-regexp '("ENDE_TOOL_STORE_FILE")) nil t)
                 (let ((element (org-element-at-point)))
                   (when (eq (org-element-type element) 'keyword)
                     (push (org-element-property :value element) values))))))
            (when (cdr values)
              (user-error "Duplicate ENDE_TOOL_STORE_FILE declarations"))
            (when values
              (let ((value (string-trim (car values))))
                (when (string-empty-p value)
                  (user-error "ENDE_TOOL_STORE_FILE must not be empty"))
                (unless (or buffer-file-name (file-name-absolute-p value))
                  (user-error "Relative ENDE_TOOL_STORE_FILE requires an Org file"))
                (let ((filename
                       (expand-file-name
                        value (and buffer-file-name
                                   (file-name-directory buffer-file-name)))))
                  (when (or (string-match-p "[\0\r\n]" filename)
                            (directory-name-p filename)
                            (file-directory-p filename))
                    (user-error "ENDE_TOOL_STORE_FILE must name a file"))
                  filename)))))))

(put 'ende-tool-store--initialize-buffer 'permanent-local-hook t)

(defun ende-tool-store--filename ()
  "Return the current buffer's absolute tool-store filename."
  (or ende-tool-store--local-file
      (expand-file-name ende-tool-store-file)))

(defun ende-tool-store--files (filename)
  "Return FILENAME and its SQLite auxiliary filenames."
  (mapcar (lambda (suffix) (concat filename suffix))
          '("" "-wal" "-shm" "-journal")))

(defun ende-tool-store--set-modes (file modes)
  "Set FILE's MODES without following a symbolic link.
Verify Unix permission bits on platforms that support them."
  (set-file-modes file modes 'nofollow)
  (unless (memq system-type '(windows-nt ms-dos))
    (unless (= (logand (file-modes file 'nofollow) #o7777) modes)
      (error "Cannot enforce tool store permissions on %s" file))))

(defun ende-tool-store--protect-files (filename &optional create)
  "Validate and protect FILENAME and its sidecars before SQLite uses them.
CREATE permits making missing directories, but only the dedicated default
Ende directory has its existing permissions changed."
  (when (file-remote-p filename)
    (user-error "Tool payload storage requires a local database: %s" filename))
  (let ((directory (file-name-directory filename)))
    (when create
      (make-directory directory t))
    (when (and (equal directory (expand-file-name "ende/" user-emacs-directory))
               (file-directory-p directory))
      (ende-tool-store--set-modes directory #o700)))
  (dolist (file (ende-tool-store--files filename))
    (when (file-symlink-p file)
      (error "Refusing symbolic link for tool store file: %s" file))
    (when-let* ((attributes (file-attributes file)))
      (when (or (file-attribute-type attributes)
                (> (file-attribute-link-number attributes) 1))
        (error "Refusing non-regular or linked tool store file: %s" file))
      (ende-tool-store--set-modes file #o600))))

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
      (sqlite-execute
       database
       (concat "CREATE TABLE tool_payloads ("
               "execution_id TEXT NOT NULL, "
               "kind TEXT NOT NULL CHECK (kind IN ('request', 'result')), "
               "payload TEXT NOT NULL, "
               "PRIMARY KEY (execution_id, kind))"))
      (sqlite-execute
       database (format "PRAGMA user_version = %d"
                        ende-tool-store--schema-version)))
     ((/= version ende-tool-store--schema-version)
      (error "Unsupported tool payload database schema version %s in %s; use a fresh database by changing `ende-tool-store-file'"
             version filename))
     (t nil)))
  (sqlite-execute database "DROP TABLE IF EXISTS assistant_protocol")
  (sqlite-execute database
                  "CREATE TABLE IF NOT EXISTS protocol_items (id TEXT PRIMARY KEY, provider TEXT NOT NULL, kind TEXT NOT NULL, payload TEXT NOT NULL)"))

(defun ende-tool-store--open (filename create)
  "Open and validate FILENAME.  CREATE allows making a missing database."
  (ende-tool-store--check-sqlite)
  (ende-tool-store--protect-files filename create)
  (let ((exists (file-exists-p filename)))
    (when (or exists create)
      (let ((database
             (condition-case error-data
               (sqlite-open filename)
               (error
                (error "Cannot open tool payload database %s: %s. Check the file or set `ende-tool-store-file' to a fresh database"
                       filename (error-message-string error-data))))))
        (condition-case error-data
            (progn
              (ende-tool-store--initialize database filename)
              database)
          (error
           (sqlite-close database)
           (error "Cannot initialize or validate tool payload database %s: %s. Check the file or set `ende-tool-store-file' to a fresh database"
                  filename (error-message-string error-data))))))))

(defun ende-tool-store--call-with-database (create function &optional filename)
  "Call FUNCTION with database FILENAME, creating it when CREATE.
FILENAME defaults to the current buffer's effective store."
  ;; SQLite can create sidecars during SQL execution and connection closing.
  (with-file-modes #o700
    (let* ((filename (expand-file-name (or filename (ende-tool-store--filename))))
           (database (ende-tool-store--open filename create)))
      (when database
        (unwind-protect
            (condition-case error-data
                (funcall function database)
              (error
               (error "Tool payload database %s failed: %s. Check the file or set `ende-tool-store-file' to a fresh database"
                      filename (error-message-string error-data))))
          (unwind-protect
              (sqlite-close database)
            (ende-tool-store--protect-files filename)))))))

(defun ende-tool-store-put (execution-id kind payload &optional filename)
  "Store immutable JSON PAYLOAD for EXECUTION-ID and KIND in FILENAME."
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
                  execution-id kind))))
     filename)))

(defun ende-tool-store-get (execution-id kind &optional filename)
  "Return EXECUTION-ID's KIND payload from FILENAME, or nil if absent."
  (unless (and (stringp execution-id) (member kind '(request result)))
    (error "Invalid tool payload identity"))
  (ende-tool-store--call-with-database
   nil
   (lambda (database)
     (caar (sqlite-select
            database
            "SELECT payload FROM tool_payloads WHERE execution_id = ? AND kind = ?"
            (vector execution-id (symbol-name kind)))))
   filename))

(defun ende-tool-store--encode-payload (&rest fields)
  "Encode string-keyed FIELDS as the existing durable JSON representation."
  (let ((object (make-hash-table :test #'equal)))
    (while fields
      (puthash (pop fields) (pop fields) object))
    (decode-coding-string (json-serialize object :null-object nil) 'utf-8-unix)))

(defun ende-tool-store--validate-call-item
    (provider-call-id name arguments provider item)
  "Validate optional provider ITEM against its normalized call fields."
  (when (or provider item)
    (unless (and (equal provider "openai-responses")
                 (hash-table-p item)
                 (equal (gethash "type" item) "function_call")
                 (equal (gethash "call_id" item) provider-call-id)
                 (equal (gethash "name" item) name)
                 (equal (gethash "arguments" item) arguments))
      (error "Provider tool call item disagrees with its normalized call"))))

(defun ende-tool-store--decode-payload (payload execution-id kind)
  "Decode and validate PAYLOAD for EXECUTION-ID and KIND."
  (let* ((record
          (condition-case error-data
              (json-parse-string payload :object-type 'hash-table
                                 :null-object nil :false-object :false)
            (error
             (error "Malformed stored %s payload for execution %s: %s"
                    kind execution-id (error-message-string error-data)))))
         (fields (if (eq kind 'request)
                     '("provider_call_id" "tool_name" "arguments")
                   '("provider_call_id" "content"))))
    (unless (and (hash-table-p record)
                 (cl-every (lambda (field) (stringp (gethash field record)))
                           fields)
                 (not (string-empty-p (gethash "provider_call_id" record)))
                 (or (eq kind 'result)
                     (not (string-empty-p (gethash "tool_name" record)))))
      (error "Malformed stored %s payload for execution %s"
             kind execution-id))
    (when (eq kind 'request)
      (ende-tool-store--validate-call-item
       (gethash "provider_call_id" record) (gethash "tool_name" record)
       (gethash "arguments" record) (gethash "protocol_provider" record)
       (gethash "protocol_item" record)))
    record))

(defun ende-tool-store-create-execution
    (provider-call-id name arguments &optional filename protocol-provider protocol-item)
  "Store a new request execution in FILENAME and return its semantic value.
Retain optional PROTOCOL-ITEM and its PROTOCOL-PROVIDER for exact replay."
  (unless (and (stringp provider-call-id) (not (string-empty-p provider-call-id))
               (stringp name) (not (string-empty-p name))
               (stringp arguments))
    (error "Malformed provider tool call"))
  (ende-tool-store--validate-call-item
   provider-call-id name arguments protocol-provider protocol-item)
  (let ((execution-id (ende-tool-store--new-execution-id)))
    (ende-tool-store-put
     execution-id 'request
     (apply #'ende-tool-store--encode-payload
            (append (list "provider_call_id" provider-call-id "tool_name" name
                          "arguments" arguments)
                    (when protocol-item
                      (list "protocol_provider" protocol-provider
                            "protocol_item" protocol-item))))
     filename)
    (list :execution-id execution-id
          :provider-call-id provider-call-id
          :name name
          :arguments arguments
          :protocol-provider protocol-provider
          :protocol-item protocol-item
          :request-present-p t
          :result-present-p nil
          :content nil)))

(defun ende-tool-store-store-result
    (execution-id provider-call-id content &optional filename)
  "Store CONTENT for EXECUTION-ID and PROVIDER-CALL-ID in FILENAME."
  (unless (and (stringp execution-id) (not (string-empty-p execution-id))
               (stringp provider-call-id) (not (string-empty-p provider-call-id))
               (stringp content))
    (error "Malformed tool result for execution %s" execution-id))
  (ende-tool-store-put
   execution-id 'result
   (ende-tool-store--encode-payload
    "provider_call_id" provider-call-id "content" content)
   filename))

(defun ende-tool-store-load-execution (execution-id &optional filename)
  "Load and validate EXECUTION-ID from FILENAME as a semantic execution plist.

Request and result records are optional.  Their presence is explicit in the
returned plist, and provider call IDs are checked when both records exist."
  (unless (and (stringp execution-id) (not (string-empty-p execution-id)))
    (error "Invalid tool execution identity"))
  (let* ((payloads
          (ende-tool-store--call-with-database
           nil
           (lambda (database)
             (list
              (caar (sqlite-select
                     database
                     "SELECT payload FROM tool_payloads WHERE execution_id = ? AND kind = 'request'"
                     (vector execution-id)))
              (caar (sqlite-select
                     database
                     "SELECT payload FROM tool_payloads WHERE execution_id = ? AND kind = 'result'"
                     (vector execution-id)))))
           filename))
         (request-payload (car payloads))
         (result-payload (cadr payloads))
         (request (and request-payload
                       (ende-tool-store--decode-payload
                        request-payload execution-id 'request)))
         (result (and result-payload
                      (ende-tool-store--decode-payload
                       result-payload execution-id 'result)))
         (request-call-id (and request (gethash "provider_call_id" request)))
         (result-call-id (and result (gethash "provider_call_id" result))))
    (when (and request result (not (equal request-call-id result-call-id)))
      (error "Malformed tool history: provider call IDs disagree for execution %s"
             execution-id))
    (list :execution-id execution-id
          :provider-call-id (or request-call-id result-call-id)
          :name (and request (gethash "tool_name" request))
          :arguments (and request (gethash "arguments" request))
          :protocol-provider (and request (gethash "protocol_provider" request))
          :protocol-item (and request (gethash "protocol_item" request))
          :request-present-p (and request t)
          :result-present-p (and result t)
          :content (and result (gethash "content" result)))))

(defun ende-tool-store-save-opaque (provider kind item &optional filename)
  "Store provider ITEM as an opaque protocol item and return its identity."
  (unless (and (member provider '("openai-responses" "anthropic-messages"))
               (stringp kind) (not (string-empty-p kind))
               (not (string-match-p "[[:cntrl:]]" kind))
               (hash-table-p item))
    (error "Invalid opaque protocol item"))
  (let ((id (ende-tool-store--new-execution-id))
        (payload (decode-coding-string (json-serialize item) 'utf-8-unix)))
    (ende-tool-store--call-with-database
     t
     (lambda (database)
       (sqlite-execute
        database
        "INSERT INTO protocol_items (id, provider, kind, payload) VALUES (?, ?, ?, ?)"
        (vector id provider kind payload)))
     filename)
    id))

(defun ende-tool-store-load-opaque (id &optional provider kind filename)
  "Load opaque ID from FILENAME, checking optional PROVIDER and KIND.
Return nil if the database or item is missing."
  (unless (and (stringp id) (not (string-empty-p id)))
    (error "Invalid opaque protocol item identity"))
  (let ((record
         (ende-tool-store--call-with-database
          nil
          (lambda (database)
            (car (sqlite-select
                  database
                  "SELECT provider, kind, payload FROM protocol_items WHERE id = ?"
                  (vector id))))
          filename)))
    (when record
      (let ((stored-provider (nth 0 record))
            (stored-kind (nth 1 record))
            (payload (nth 2 record)))
        (unless (and (or (null provider) (equal provider stored-provider))
                     (or (null kind) (equal kind stored-kind)))
          (error "Opaque protocol item metadata does not match item %s" id))
        (let ((item
               (condition-case error-data
                   (json-parse-string payload :object-type 'hash-table
                                      :array-type 'array
                                      :null-object nil :false-object json-false)
                 (error
                  (error "Malformed opaque protocol item %s: %s"
                         id (error-message-string error-data))))))
          (unless (hash-table-p item)
            (error "Malformed opaque protocol item %s" id))
          item)))))

(defun ende-tool-store--footprint ()
  "Return the configured filename and existing database files with sizes."
  (let* ((filename (ende-tool-store--filename))
         (files (cl-remove-if-not
                 #'file-exists-p
                 (ende-tool-store--files filename))))
    (list filename
          (mapcar (lambda (file)
                    (cons file (file-attribute-size (file-attributes file))))
                  files))))

;;;###autoload
(defun ende-tool-store-size ()
  "Report the tool payload database's on-disk footprint, including sidecars."
  (interactive)
  (pcase-let* ((`(,filename ,files) (ende-tool-store--footprint)))
    (if (not (file-exists-p filename))
        (let ((sidecars
               (cl-remove-if (lambda (entry) (equal (car entry) filename))
                             files)))
          (message "Tool payload database does not exist: %s%s"
                   filename
                   (if sidecars
                       (concat "; sidecars: "
                               (mapconcat
                                (lambda (entry)
                                  (format "%s (%s bytes)"
                                          (car entry) (cdr entry)))
                                sidecars ", "))
                     ""))
          nil)
      (let ((bytes (apply #'+ (mapcar #'cdr files))))
        (message "Tool payload database %s: %s across %s"
                 filename bytes
                 (mapconcat (lambda (entry)
                              (format "%s (%s bytes)" (car entry) (cdr entry)))
                            files ", "))
        bytes))))

(defun ende-tool-store--job-file (job)
  "Return live chat JOB's tool store filename, or nil for other jobs.
Refuse an unknown destination rather than risk purging an active store."
  (when (memq (ende-job-type job) '(chat chat-compact))
    (or (plist-get (ende-job-instruction job) :tool-store-file)
        (let ((target (ende-job-target job)))
          (unless (ende-target-live-p target)
            (user-error "Cannot determine tool payload database for active Ende job %s"
                        (ende-job-id job)))
          (with-current-buffer (ende-target-buffer target)
            (ende-tool-store--filename))))))

;;;###autoload
(defun ende-tool-store-purge ()
  "Confirm and delete the current buffer's tool database and SQLite sidecars."
  (interactive)
  (let ((filename (ende-tool-store--filename)))
    (when (yes-or-no-p
           (format "Permanently purge tool payload database %s and its SQLite auxiliary files? "
                   filename))
      (when (cl-some
             (lambda (job)
               (when-let* ((file (ende-tool-store--job-file job)))
                 (or (equal filename file) (file-equal-p filename file))))
             (ende-all-live-jobs))
        (user-error "An active Ende job is using tool payload database %s" filename))
      (ende-tool-store--protect-files filename)
      (let ((exists (file-exists-p filename))
            removed)
        (dolist (file (ende-tool-store--files filename))
          (when (file-exists-p file)
            (delete-file file)
            (setq removed t)))
        (if exists
            (message "Purged tool payload database and SQLite auxiliary files: %s"
                     filename)
          (message "Tool payload database does not exist: %s%s"
                   filename (if removed "; removed SQLite auxiliary files" "")))))))

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
