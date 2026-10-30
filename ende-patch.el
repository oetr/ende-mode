;;; ende-patch.el --- Limited text patch parser for Ende -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: tools, patch

;;; Commentary:

;; Parse a deliberately limited Codex-style patch language and apply exact
;; logical-line hunks.  This module performs no filesystem or authorization
;; work and is not a general patch-format implementation.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defcustom ende-patch-max-chars (* 1024 1024)
  "Maximum characters accepted in one patch string."
  :type 'integer
  :group 'ende)

(defcustom ende-patch-max-operations 50
  "Maximum file operation records accepted in one patch."
  :type 'integer
  :group 'ende)

(defun ende-patch--path (path)
  "Validate and return a strict relative or absolute local PATH."
  (unless (and (stringp path) (not (string-empty-p path)))
    (error "patch path must be a non-empty local path"))
  (when (or (file-remote-p path)
            (string-match-p "[[:cntrl:]\\\\]" path)
            (and (not (file-name-absolute-p path))
                 (string-match-p "\\`[[:alpha:]]:" path)))
    (error "patch path must be local and safe: %s" path))
  (when (cl-some (lambda (component)
                   (or (string-empty-p component)
                       (member component '("." ".."))))
                 (split-string path "/" (file-name-absolute-p path)))
    (error "patch path must not contain empty, . or .. components: %s" path))
  path)

(defun ende-patch--lines (patch)
  "Split PATCH into protocol lines, accepting LF or CRLF."
  (let ((lines (split-string patch "\n" nil)))
    (when (string-suffix-p "\n" patch)
      (setq lines (butlast lines)))
    (mapcar (lambda (line)
              (if (string-suffix-p "\r" line)
                  (substring line 0 -1)
                line))
            lines)))

(defun ende-patch--operation-header (line)
  "Return operation and path from file-operation header LINE, or nil."
  (cl-some (lambda (entry)
             (when (string-prefix-p (car entry) line)
               (cons (cdr entry) (substring line (length (car entry))))))
           '(("*** Add File: " . add)
             ("*** Update File: " . update)
             ("*** Delete File: " . delete))))

(defun ende-patch--parse-add (path lines)
  "Parse an addition at PATH from remaining LINES; return operation and rest."
  (let ((content-lines nil)
        done)
    (while (and lines (not done))
      (let ((line (car lines)))
        (cond
         ((or (ende-patch--operation-header line)
              (string= line "*** End Patch"))
          (setq done t))
         ((string-prefix-p "+" line)
          (push (substring line 1) content-lines)
          (setq lines (cdr lines)))
         (t (error "addition %s accepts only + lines, got %S" path line)))))
    (setq content-lines (nreverse content-lines))
    (list (list :operation 'add :path path
                :content (if content-lines
                             (concat (mapconcat #'identity content-lines "\n") "\n")
                           ""))
          lines)))

(defun ende-patch--hunk-line (line)
  "Parse one update hunk LINE into a (KIND . TEXT) entry."
  (unless (and (> (length line) 0)
               (memq (aref line 0) '(?\s ?- ?+)))
    (error "malformed patch hunk line: %S" line))
  (cons (pcase (aref line 0)
          (?\s 'context)
          (?- 'remove)
          (?+ 'add))
        (substring line 1)))

(defun ende-patch--parse-hunk (lines)
  "Parse one hunk from LINES; return (HUNK . REST)."
  (let ((hunk-lines nil)
        (end-of-file nil)
        done)
    (while (and lines (not done))
      (let ((line (car lines)))
        (cond
         ((string= line "@@")
          (setq done t))
         ((string= line "*** End of File")
          (when end-of-file
            (error "duplicate *** End of File marker in one hunk"))
          (setq end-of-file t
                lines (cdr lines)
                done t))
         ((or (ende-patch--operation-header line)
              (string= line "*** End Patch"))
          (setq done t))
         (t
          (push (ende-patch--hunk-line line) hunk-lines)
          (setq lines (cdr lines))))))
    (unless hunk-lines
      (error "patch hunks must contain at least one context, - or + line"))
    (cons (list :lines (nreverse hunk-lines) :end-of-file end-of-file)
          lines)))

(defun ende-patch--parse-update (path lines)
  "Parse update PATH from LINES; return operation and rest."
  (unless (and lines (string= (car lines) "@@"))
    (error "update %s must begin with a bare @@ hunk separator" path))
  (let ((hunks nil)
        (rest lines))
    (while (and rest (string= (car rest) "@@"))
      (setq rest (cdr rest))
      (let ((parsed (ende-patch--parse-hunk rest)))
        (push (car parsed) hunks)
        (setq rest (cdr parsed))))
    (unless hunks
      (error "update %s needs at least one hunk" path))
    (list (list :operation 'update :path path :hunks (nreverse hunks))
          rest)))

(defun ende-patch-parse (patch)
  "Parse complete PATCH and return ordered file operation plists.

Supported directives are Begin/End Patch, Add/Update/Delete File, bare @@,
context, removal, addition, and End of File.  Unsupported constructs signal."
  (unless (and (stringp patch) (not (string-empty-p patch)))
    (error "patch must be a non-empty string"))
  (when (> (length patch) ende-patch-max-chars)
    (error "patch exceeds the %d-character limit" ende-patch-max-chars))
  (let ((lines (ende-patch--lines patch))
        (operations nil)
        (seen (make-hash-table :test #'equal)))
    (unless (and lines (string= (car lines) "*** Begin Patch"))
      (error "patch must start with *** Begin Patch"))
    (setq lines (cdr lines))
    (while (and lines (not (string= (car lines) "*** End Patch")))
      (let* ((header (ende-patch--operation-header (car lines)))
             (operation (car-safe header))
             (path (and header (ende-patch--path (cdr header))))
             parsed)
        (unless header
          (error "unsupported patch construct: %S" (car lines)))
        (when (gethash path seen)
          (error "patch contains multiple operations for %s" path))
        (puthash path t seen)
        (setq lines (cdr lines))
        (pcase operation
          ('add (setq parsed (ende-patch--parse-add path lines)))
          ('update (setq parsed (ende-patch--parse-update path lines)))
          ('delete
           (when (and lines
                      (not (ende-patch--operation-header (car lines)))
                      (not (string= (car lines) "*** End Patch")))
             (error "delete %s does not accept patch content" path))
           (setq parsed (list (list :operation 'delete :path path) lines)))
          (_ (error "unsupported operation in patch")))
        (push (car parsed) operations)
        (setq lines (cadr parsed))
        (when (> (length operations) ende-patch-max-operations)
          (error "patch contains too many file operations (maximum %d)"
                 ende-patch-max-operations))))
    (unless (and lines (string= (car lines) "*** End Patch"))
      (error "patch is missing *** End Patch"))
    (unless (null (cdr lines))
      (error "unexpected content after *** End Patch"))
    (unless operations
      (error "patch contains no file operations"))
    (nreverse operations)))

(defun ende-patch--split-source (source)
  "Return (LINES FINAL-NEWLINE-P) for decoded SOURCE text."
  (if (string-empty-p source)
      (list nil nil)
    (let ((lines (split-string source "\n" nil))
          (final-newline (string-suffix-p "\n" source)))
      (when final-newline
        (setq lines (butlast lines)))
      (list lines final-newline))))

(defun ende-patch--hunk-sides (hunk)
  "Return the (OLD-LINES . NEW-LINES) represented by HUNK."
  (let (old new)
    (dolist (line (plist-get hunk :lines))
      (pcase (car line)
        ('context (push (cdr line) old) (push (cdr line) new))
        ('remove (push (cdr line) old))
        ('add (push (cdr line) new))))
    (cons (nreverse old) (nreverse new))))

(defun ende-patch--line-sequence-at-p (lines expected start)
  "Return non-nil when EXPECTED exactly matches LINES starting at START."
  (and (<= (+ start (length expected)) (length lines))
       (cl-loop for expected-line in expected
                for actual-line in (nthcdr start lines)
                always (string= expected-line actual-line))))

(defun ende-patch--hunk-matches (lines old end-of-file)
  "Return all exact positions of OLD in LINES, respecting EOF constraint."
  (let ((count (length old))
        (matches nil))
    (if (zerop count)
        (when (null lines)
          (setq matches '(0)))
      (cl-loop for start from 0 to (- (length lines) count)
               when (and (ende-patch--line-sequence-at-p lines old start)
                         (or (not end-of-file)
                             (= (+ start count) (length lines))))
               do (push start matches)))
    (nreverse matches)))

(defun ende-patch-apply-update (source hunks)
  "Apply exact HUNKS to decoded logical-line SOURCE and return new text.
The original final-newline style is retained."
  (let* ((split (ende-patch--split-source source))
         (lines (car split))
         (final-newline (cadr split))
         (original-empty (null lines))
         (cursor 0))
    (dolist (hunk hunks)
      (let* ((sides (ende-patch--hunk-sides hunk))
             (old (car sides))
             (new (cdr sides))
             (insertion-only
              (not (cl-some (lambda (line) (eq (car line) 'remove))
                            (plist-get hunk :lines))))
             (matches (ende-patch--hunk-matches
                       lines old (plist-get hunk :end-of-file))))
        (when (and insertion-only (null old) (not original-empty))
          (error "insertion-only hunk requires context lines"))
        (unless (= (length matches) 1)
          (error (if matches
                     "patch hunk is ambiguous: context matches more than once"
                   "patch hunk context was not found")))
        (let ((start (car matches)))
          (when (< start cursor)
            (error "patch hunks are out of order or overlap"))
          (setq lines
                (append (cl-subseq lines 0 start)
                        new
                        (nthcdr (+ start (length old)) lines))
                cursor (+ start (length new))))))
    (let ((result (mapconcat #'identity lines "\n")))
      (if (and final-newline lines)
          (concat result "\n")
        result))))

(provide 'ende-patch)

;;; ende-patch.el ends here
