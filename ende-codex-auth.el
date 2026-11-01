;;; ende-codex-auth.el --- Private Codex account records -*- lexical-binding: t; -*-

;;; Commentary:

;; Ende-owned storage only.  Callers must validate OAuth identity before passing
;; credentials to `ende-codex-auth--register'.  This module does not authenticate
;; users, trust decoded JWT claims, import credentials, or select an account.
;; The public login and transport cutover are deliberately a separate feature.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'ende-protocol)
(require 'ende-provider-config)

(defvar ende-codex-auth--directory
  (expand-file-name "ende-codex/" user-emacs-directory)
  "Private local storage directory; never contains user configuration.")

(define-error 'ende-codex-auth-error "Codex credential storage is unavailable or unsafe")
(define-error 'ende-codex-auth-conflict
              "Codex credentials changed; retry from the current registration"
              'ende-codex-auth-error)
(define-error 'ende-codex-auth-busy
              "Codex credential storage is locked by another writer"
              'ende-codex-auth-error)

(defconst ende-codex-auth--fields
  '((:client-id . "client_id") (:subject . "subject") (:host-id . "host_id")
    (:registration-id . "registration_id") (:revision . "revision")
    (:access-token . "access_token") (:refresh-token . "refresh_token")
    (:id-token . "id_token") (:expires-at . "expires_at")
    (:account-id . "account_id")))

(defconst ende-codex-auth--credential-fields
  '(:client-id :subject :access-token :refresh-token :id-token :expires-at :account-id))

(defun ende-codex-auth--fail ()
  "Signal a value-free storage error, never including file contents."
  (signal 'ende-codex-auth-error nil))

(defun ende-codex-auth--guard (function)
  "Call FUNCTION without exposing credentials through debugger or errors."
  (let ((debug-on-error nil) (debug-on-quit nil) (debug-on-signal nil))
    (condition-case failure
        (funcall function)
      (ende-codex-auth-error (signal (car failure) nil))
      (error (ende-codex-auth--fail)))))

(defun ende-codex-auth--alias (account)
  "Validate ACCOUNT using the provider's existing :account grammar."
  (ende-provider-config--settings (list :account account) '(:account) "Codex account")
  (symbol-name account))

(defun ende-codex-auth--new-id ()
  "Return a cryptographically random opaque UUID URN."
  (let ((bytes (with-temp-buffer
                 (set-buffer-multibyte nil)
                 (insert-file-contents-literally "/dev/urandom" nil nil 16)
                 (string-to-list (buffer-string)))))
    (unless (= (length bytes) 16) (ende-codex-auth--fail))
    (setf (nth 6 bytes) (logior #x40 (logand #x0f (nth 6 bytes)))
          (nth 8 bytes) (logior #x80 (logand #x3f (nth 8 bytes))))
    (let ((hex (mapconcat (lambda (byte) (format "%02x" byte)) bytes "")))
      (format "urn:uuid:%s-%s-%s-%s-%s"
              (substring hex 0 8) (substring hex 8 12) (substring hex 12 16)
              (substring hex 16 20) (substring hex 20 32)))))

(defun ende-codex-auth--id-p (value)
  "Return non-nil for an opaque UUID URN VALUE."
  (and (stringp value)
       (let ((case-fold-search nil))
         (string-match-p
          "\\`urn:uuid:[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-4[0-9a-f]\\{3\\}-[89ab][0-9a-f]\\{3\\}-[0-9a-f]\\{12\\}\\'"
          value))))

(defun ende-codex-auth--text-p (value)
  "Return non-nil for a nonempty, single-line credential VALUE."
  (and (stringp value) (not (string-blank-p value))
       (not (string-match-p "[[:cntrl:]]" value))))

(defun ende-codex-auth--validate-credentials (credentials)
  "Validate CREDENTIALS without rendering any part of them in errors."
  (unless (and (proper-list-p credentials) (cl-evenp (length credentials)))
    (ende-codex-auth--fail))
  (let (seen)
    (cl-loop for (key _value) on credentials by #'cddr do
             (unless (and (memq key ende-codex-auth--credential-fields)
                          (not (memq key seen)))
               (ende-codex-auth--fail))
             (push key seen)))
  (dolist (key '(:client-id :subject :access-token :refresh-token :id-token))
    (unless (ende-codex-auth--text-p (plist-get credentials key))
      (ende-codex-auth--fail)))
  (unless (and (integerp (plist-get credentials :expires-at))
               (>= (plist-get credentials :expires-at) 0))
    (ende-codex-auth--fail))
  (when (plist-member credentials :account-id)
    (unless (ende-codex-auth--text-p (plist-get credentials :account-id))
      (ende-codex-auth--fail))))

(defun ende-codex-auth--attributes (path directory private)
  "Check existing PATH as DIRECTORY or a regular file.
PRIVATE requires owner-only modes; ancestors must not permit replacement by
other users.  No symbolic links or multiply linked credential files are safe."
  (setq path (directory-file-name path))
  (let ((attributes (file-attributes path 'integer)))
    (when attributes
      (let ((type (file-attribute-type attributes))
            (owner (file-attribute-user-id attributes))
            (modes (file-modes path)))
        (unless (and (if directory (eq type t) (null type))
                     (integerp modes)
                     (if private
                         (and (= owner (user-uid))
                              (= (logand modes #o7777) (if directory #o700 #o600)))
                       (and (memq owner (list 0 (user-uid)))
                            (or (= (logand modes #o022) 0)
                                ;; A trusted sticky ancestor such as /tmp cannot
                                ;; let another user replace our private directory.
                                (/= (logand modes #o1000) 0))))
                     (or directory (= (file-attribute-link-number attributes) 1)))
          (ende-codex-auth--fail))))
    attributes))

(defun ende-codex-auth--ensure-directory (path create private)
  "Validate PATH and its ancestors; CREATE missing directories privately."
  (let ((parent (file-name-directory (directory-file-name path))))
    (unless (equal (directory-file-name path) (directory-file-name parent))
      (ende-codex-auth--ensure-directory parent create nil)))
  (unless (ende-codex-auth--attributes path t private)
    (when create
      (let ((original (default-file-modes)))
        (unwind-protect
            (progn (set-default-file-modes #o700) (make-directory path))
          (set-default-file-modes original)))
      (unless (ende-codex-auth--attributes path t private)
        (ende-codex-auth--fail)))))

(defun ende-codex-auth--path (create)
  "Return checked local storage directory, optionally CREATE it."
  (unless (and (stringp ende-codex-auth--directory)
               (file-name-absolute-p ende-codex-auth--directory)
               (not (file-remote-p ende-codex-auth--directory)))
    (ende-codex-auth--fail))
  (let ((directory (directory-file-name (expand-file-name ende-codex-auth--directory))))
    (ende-codex-auth--ensure-directory directory create t)
    directory))

(defun ende-codex-auth--decode-record (account entry host-id)
  "Decode checked JSON ENTRY for ACCOUNT under installation HOST-ID."
  (unless (hash-table-p entry) (ende-codex-auth--fail))
  (let ((record (list :account account)) credentials)
    (maphash (lambda (key _value)
               (unless (rassoc key ende-codex-auth--fields) (ende-codex-auth--fail)))
             entry)
    (dolist (field ende-codex-auth--fields)
      (let ((value (gethash (cdr field) entry :missing)))
        (unless (eq value :missing)
          (setq record (plist-put record (car field) value))
          (when (memq (car field) ende-codex-auth--credential-fields)
            (setq credentials (plist-put credentials (car field) value))))))
    (ende-codex-auth--validate-credentials credentials)
    (unless (and (equal (plist-get record :host-id) host-id)
                 (ende-codex-auth--id-p (plist-get record :registration-id))
                 (ende-codex-auth--id-p (plist-get record :revision)))
      (ende-codex-auth--fail))
    record))

(defun ende-codex-auth--read-state (directory)
  "Read and validate private JSON state under DIRECTORY, or return nil."
  (let* ((file (expand-file-name "accounts.json" directory))
         (attributes (ende-codex-auth--attributes file nil t)))
    (when attributes
      (when (> (file-attribute-size attributes) (* 1024 1024))
        (ende-codex-auth--fail))
      (let* ((state (with-temp-buffer
                      (insert-file-contents-literally file)
                      (ende-protocol-json-read-string (buffer-string))))
             (host-id (and (hash-table-p state) (gethash "host_id" state)))
             (accounts (and (hash-table-p state) (gethash "accounts" state))))
        (unless (and (hash-table-p state) (= (hash-table-count state) 3)
                     (equal (gethash "version" state) 1)
                     (ende-codex-auth--id-p host-id) (hash-table-p accounts))
          (ende-codex-auth--fail))
        (maphash (lambda (alias entry)
                   (unless (and (stringp alias)
                                (let ((case-fold-search nil))
                                  (string-match-p "\\`[a-z0-9][a-z0-9_-]*\\'" alias)))
                     (ende-codex-auth--fail))
                   (ende-codex-auth--decode-record alias entry host-id))
                 accounts)
        state))))

(defun ende-codex-auth--write-state (directory state)
  "Atomically write STATE beside its destination in private DIRECTORY."
  (let ((file (expand-file-name "accounts.json" directory)) temporary)
    (unwind-protect
        (progn
          (ende-codex-auth--attributes file nil t)
          (setq temporary (make-temp-file (expand-file-name ".accounts-" directory)))
          (set-file-modes temporary #o600)
          (let ((coding-system-for-write 'utf-8-unix)
                (write-region-annotate-functions nil)
                (write-region-post-annotation-function nil))
            (write-region (concat (json-encode state) "\n") nil temporary nil 'silent))
          (ende-codex-auth--attributes temporary nil t)
          (ende-codex-auth--attributes file nil t)
          (rename-file temporary file t)
          (setq temporary nil))
      (when temporary (delete-file temporary)))))

(defun ende-codex-auth--transaction (function)
  "Run FUNCTION with fresh state under an exclusive cross-instance lock.
Never steal an existing lock, including after a crash.  Every writer rereads
under the lock and must still compare the individual account revision."
  (let* ((directory (ende-codex-auth--path t))
         (lock (expand-file-name ".writer-lock" directory)) acquired)
    (unwind-protect
        (progn
          (condition-case nil
              (let ((original (default-file-modes)))
                (unwind-protect
                    (progn (set-default-file-modes #o700) (make-directory lock)
                           (setq acquired t))
                  (set-default-file-modes original)))
            (file-already-exists (signal 'ende-codex-auth-busy nil)))
          (ende-codex-auth--attributes lock t t)
          (let ((state (or (ende-codex-auth--read-state directory)
                           (let ((new (make-hash-table :test #'equal)))
                             (puthash "version" 1 new)
                             (puthash "host_id" (ende-codex-auth--new-id) new)
                             (puthash "accounts" (make-hash-table :test #'equal) new)
                             new))))
            (prog1 (funcall function state)
              (ende-codex-auth--write-state directory state))))
      (when acquired (delete-directory lock)))))

(defun ende-codex-auth-read (account)
  "Return ACCOUNT's detached private record, never importing other credentials."
  (ende-codex-auth--guard
   (lambda ()
     (let* ((alias (ende-codex-auth--alias account))
            (state (ende-codex-auth--read-state (ende-codex-auth--path nil)))
            (entry (and state (gethash alias (gethash "accounts" state)))))
       (unless entry (ende-codex-auth--fail))
       (ende-codex-auth--decode-record account entry (gethash "host_id" state))))))

(defun ende-codex-auth--encode-record (record)
  "Encode private RECORD using only the fixed storage fields."
  (let ((entry (make-hash-table :test #'equal)))
    (dolist (field ende-codex-auth--fields)
      (when (plist-member record (car field))
        (puthash (cdr field) (plist-get record (car field)) entry)))
    entry))

(defun ende-codex-auth--register (account credentials &optional expected-revision replace)
  "Store externally validated CREDENTIALS as a new ACCOUNT registration.
An existing alias always requires explicit REPLACE authorization and its exact
EXPECTED-REVISION, even for another registration of the same subject.  This
internal function is not a login command or an identity verifier."
  (ende-codex-auth--guard
   (lambda ()
     (let ((alias (ende-codex-auth--alias account)))
       (ende-codex-auth--validate-credentials credentials)
       (ende-codex-auth--transaction
        (lambda (state)
          (let* ((accounts (gethash "accounts" state))
                 (old (gethash alias accounts)))
            (unless (and (equal expected-revision (and old (gethash "revision" old)))
                         (or (not old) replace))
              (signal 'ende-codex-auth-conflict nil))
            (let ((record (append (list :account account
                                        :host-id (gethash "host_id" state)
                                        :registration-id (ende-codex-auth--new-id)
                                        :revision (ende-codex-auth--new-id))
                                  (copy-tree credentials))))
              (puthash alias (ende-codex-auth--encode-record record) accounts)
              (ende-codex-auth--decode-record
               account (gethash alias accounts) (gethash "host_id" state))))))))))

(defun ende-codex-auth--same-registration-p (left right)
  "Return non-nil when private records LEFT and RIGHT name one registration."
  (and left right
       (cl-every (lambda (key) (equal (plist-get left key) (plist-get right key)))
                 '(:account :host-id :registration-id :client-id :subject))))

(defun ende-codex-auth--rotate (expected credentials)
  "Rotate EXPECTED's tokens using CREDENTIALS for the same verified registration.
Refuse stale snapshots and changed subjects or issued client IDs.  Only refresh
rotation preserves the registration identity used by queued requests."
  (ende-codex-auth--guard
   (lambda ()
     (let* ((account (plist-get expected :account))
            (alias (ende-codex-auth--alias account)))
       (ende-codex-auth--validate-credentials credentials)
       (unless (and (equal (plist-get expected :subject) (plist-get credentials :subject))
                    (equal (plist-get expected :client-id) (plist-get credentials :client-id)))
         (signal 'ende-codex-auth-conflict nil))
       (ende-codex-auth--transaction
        (lambda (state)
          (let* ((accounts (gethash "accounts" state))
                 (entry (gethash alias accounts))
                 (current (and entry (ende-codex-auth--decode-record
                                      account entry (gethash "host_id" state)))))
            (unless (and (ende-codex-auth--same-registration-p expected current)
                         (equal (plist-get expected :revision) (plist-get current :revision)))
              (signal 'ende-codex-auth-conflict nil))
            (let ((record (append (list :account account
                                        :host-id (plist-get current :host-id)
                                        :registration-id (plist-get current :registration-id)
                                        :revision (ende-codex-auth--new-id))
                                  (copy-tree credentials))))
              (puthash alias (ende-codex-auth--encode-record record) accounts)
              (ende-codex-auth--decode-record
               account (gethash alias accounts) (gethash "host_id" state))))))))))

(provide 'ende-codex-auth)
;;; ende-codex-auth.el ends here
