;;; ende-lock.el --- Permanent local SQLite mutex -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'sqlite)

(define-error 'ende-lock-error "SQLite mutex is unavailable or unsafe")
(define-error 'ende-lock-busy "SQLite mutex is held" 'ende-lock-error)

(defun ende-lock--fail ()
  "Reject an unavailable or unsafe mutex without including values."
  (signal 'ende-lock-error nil))

(defun ende-lock--attributes (file &optional required)
  "Validate FILE and return its attributes; REQUIRED forbids absence."
  (let ((attributes (file-attributes file 'integer)))
    (if attributes
        (unless (and (null (file-attribute-type attributes))
                     (equal (file-attribute-modes attributes) "-rw-------")
                     (= (file-attribute-link-number attributes) 1)
                     (= (file-attribute-user-id attributes) (user-uid)))
          (ende-lock--fail))
      (when required (ende-lock--fail)))
    attributes))

(defun ende-lock--identity (attributes)
  "Return the device and inode in ATTRIBUTES."
  (list (file-attribute-device-number attributes)
        (file-attribute-inode-number attributes)))

(defun ende-lock--files (file &optional identity required)
  "Validate FILE and sidecars, optionally requiring IDENTITY or existence."
  (let* ((attributes (ende-lock--attributes file (or identity required)))
         (device (file-attribute-device-number
                  (file-attributes (file-name-directory file) 'integer))))
    (when (or (and identity
                   (not (equal identity (ende-lock--identity attributes))))
              (and attributes
                   (not (equal device (file-attribute-device-number attributes)))))
      (ende-lock--fail))
    (dolist (suffix '("-journal" "-wal" "-shm"))
      (let ((sidecar (ende-lock--attributes (concat file suffix))))
        (when (and sidecar
                   (not (equal device (file-attribute-device-number sidecar))))
          (ende-lock--fail))))
    (and attributes (ende-lock--identity attributes))))

(defun ende-lock--unescape (field)
  "Decode one mountinfo FIELD, rejecting non-kernel escape syntax."
  (replace-regexp-in-string
   "\\\\[^\\\\]*"
   (lambda (escape)
     ;; Only these four bytes are escaped by Linux mountinfo.
     (let ((code (and (>= (length escape) 4) (substring escape 0 4))))
       (concat (or (cdr (assoc code '(("\\040" . " ") ("\\011" . "\t")
                                      ("\\012" . "\n") ("\\134" . "\\"))))
                   (ende-lock--fail))
               (substring escape 4))))
   field t t))

(defun ende-lock--contains-p (directory path)
  "Return non-nil if DIRECTORY contains PATH, including equality."
  (or (equal directory path)
      (string-prefix-p (file-name-as-directory directory) path)))

(defun ende-lock--mount (file)
  "Return a verified Linux mount topology for FILE's existing parent.
Only a small allowlist of local filesystems is accepted.  Mountinfo and
stat device numbers must agree; stacked or disconnected mounts fail closed."
  (unless (eq system-type 'gnu/linux) (ende-lock--fail))
  (let ((directory (directory-file-name (file-name-directory file)))
        (ids (make-hash-table :test #'equal)) mounts)
    (with-temp-buffer
      (insert-file-contents-literally "/proc/self/mountinfo")
      (unless (and (> (buffer-size) 0) (eq (char-before (point-max)) ?\n))
        (ende-lock--fail))
      (dolist (line (split-string (buffer-substring-no-properties
                                 (point-min) (1- (point-max))) "\n"))
        (let* ((fields (split-string line " "))
               (separator (seq-position fields "-" #'equal))
               (id (car fields))
               (parent (nth 1 fields))
               (device (nth 2 fields)))
          (unless (and separator (>= separator 6)
                       (= (length fields) (+ separator 4))
                       (not (member "" fields))
                       (string-match-p "\\`[1-9][0-9]*\\'" id)
                       (string-match-p "\\`[1-9][0-9]*\\'" parent)
                       (string-match-p "\\`[0-9]+:[0-9]+\\'" device)
                       (not (equal id parent))
                       (cl-every
                        (lambda (field)
                          (string-match-p
                           "\\`\\(?:\\(?:shared\\|master\\|propagate_from\\):[1-9][0-9]*\\|unbindable\\)\\'"
                           field))
                        (seq-subseq fields 6 separator))
                       (not (gethash id ids)))
            (ende-lock--fail))
          (puthash id t ids)
          (let ((root (ende-lock--unescape (nth 3 fields)))
                (point (ende-lock--unescape (nth 4 fields))))
            (unless (and (string-prefix-p "/" root)
                         (equal root (directory-file-name
                                      (expand-file-name root "/")))
                         (string-prefix-p "/" point)
                         (equal point (directory-file-name
                                       (expand-file-name point "/"))))
              (ende-lock--fail))
            (when (ende-lock--contains-p point file)
              (push (list point id parent device (nth (1+ separator) fields)
                          (nth 5 fields) (nth (+ separator 3) fields) root)
                    mounts))))))
    (setq mounts (sort mounts (lambda (a b) (> (length (car a))
                                              (length (car b))))))
    (unless (and mounts (equal (caar (last mounts)) "/"))
      (ende-lock--fail))
    (let ((tail mounts))
      (while (cdr tail)
        (unless (and (not (equal (caar tail) (caadr tail)))
                     (equal (nth 2 (car tail)) (nth 1 (cadr tail))))
          (ende-lock--fail))
        (setq tail (cdr tail))))
    (let* ((mount (car mounts))
           (numbers (mapcar #'string-to-number (split-string (nth 3 mount) ":")))
           (major (car numbers)) (minor (cadr numbers))
           ;; Linux's dev_t encoding, also used by Emacs file-attributes.
           (device (logior (ash (logand major #xfff) 8)
                           (ash (logand major (lognot #xfff)) 32)
                           (logand minor #xff)
                           (ash (logand minor (lognot #xff)) 12)))
           (attributes (file-attributes directory 'integer)))
      (unless (and (ende-lock--contains-p (car mount) directory)
                   (member (nth 4 mount) '("ext2" "ext3" "ext4" "xfs" "btrfs" "tmpfs"))
                   (equal device (file-attribute-device-number attributes))
                   (cl-every (lambda (options)
                               (let ((flags (split-string options ",")))
                                 (and (member "rw" flags) (not (member "ro" flags)))))
                             (list (nth 5 mount) (nth 6 mount))))
        (ende-lock--fail)))
    mounts))

(defun ende-lock--check (file mount identity)
  "Check that FILE still has MOUNT topology and IDENTITY."
  (unless (equal mount (ende-lock--mount file)) (ende-lock--fail))
  (ende-lock--files file identity))

(defun ende-lock-call (file function)
  "Call FUNCTION with no arguments while holding FILE's SQLite mutex.
The caller must already have validated FILE's private 0700 parent and
its ancestors.  FILE is permanent: never unlink or replace it, even to
recover an abandoned lock.  All users must follow this protocol.

Only supported local Linux filesystems are accepted.  The database and
any SQLite sidecars must be single-link, UID-owned regular 0600 files.
A retained connection holds BEGIN IMMEDIATE until FUNCTION returns or
exits nonlocally.  Return FUNCTION's value and preserve its errors/quits.
Signal `ende-lock-busy' only for acquisition contention, otherwise
`ende-lock-error' for guard failures, without underlying error data."
  (let (db held result cleanup-failed)
    (unwind-protect
        (progn
          (condition-case nil
              (progn
                (unless (and (sqlite-available-p) (stringp file)
                             (not (file-remote-p file)))
                  (ende-lock--fail))
                (setq file (expand-file-name file))
                ;; Resolve only the already-trusted parent, never a DB symlink.
                (setq file (expand-file-name
                            (file-name-nondirectory file)
                            (file-truename (file-name-directory file))))
                (let* ((mount (ende-lock--mount file))
                       (identity (ende-lock--files file))
                       (modes (default-file-modes)))
                  (unwind-protect
                      (progn
                        (set-default-file-modes #o600)
                        (let ((inhibit-quit t))
                          (setq db (sqlite-open file nil t)))
                        (ende-lock--check file mount identity)
                        (setq identity (ende-lock--files file nil t))
                        (sqlite-execute db "PRAGMA busy_timeout = 0")
                        (unless (equal (sqlite-select db "PRAGMA locking_mode = NORMAL")
                                       '(("normal")))
                          (ende-lock--fail))
                        (unless (member (sqlite-select db "PRAGMA journal_mode")
                                        '((("delete")) (("truncate")) (("persist"))))
                          (ende-lock--fail))
                        (ende-lock--check file mount identity)
                        (condition-case nil
                            (progn (sqlite-execute db "BEGIN IMMEDIATE")
                                   (setq held t))
                          (sqlite-locked-error (signal 'ende-lock-busy nil)))
                        (ende-lock--check file mount identity)
                        (when (sqlite-select
                               db "SELECT 1 FROM sqlite_schema WHERE type = 'table' AND name NOT GLOB 'sqlite_*' LIMIT 1")
                          (ende-lock--fail)))
                    (let ((inhibit-quit t))
                      (set-default-file-modes modes)))))
            (ende-lock-busy (signal 'ende-lock-busy nil))
            (error (ende-lock--fail)))
          (setq result (funcall function)))
      (let ((inhibit-quit t))
        (when db
          (when held
            (condition-case nil
                (sqlite-execute db "ROLLBACK")
              ((error quit) (setq cleanup-failed t))))
          (condition-case nil
              (sqlite-close db)
            ((error quit) (setq cleanup-failed t))))))
    (when cleanup-failed (ende-lock--fail))
    result))

(provide 'ende-lock)
;;; ende-lock.el ends here
