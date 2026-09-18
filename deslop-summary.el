;;; deslop-summary.el --- Summary buffer for deslop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools, llm

;;; Commentary:

;; Ownership and rendering of the summary buffer.

;;; Code:

(require 'cl-lib)
(require 'deslop-core)
(require 'org)
(require 'rx)

(defcustom deslop-summary-buffer-name "*deslop-summary*"
  "Name of the buffer used for summaries."
  :type 'string
  :group 'deslop)

(defvar deslop-summary--buffer-object nil
  "Summary buffer owned by deslop, when one exists.")

(defvar-local deslop-summary--owned-buffer nil
  "Non-nil in a summary buffer created by deslop.")

(cl-defstruct (deslop-summary-source
               (:constructor deslop-summary-source--create))
  "Captured identity and line range for summarized source text."
  buffer name file start-line end-line id)

(defvar deslop-summary--next-source-id 0
  "Next session-local identifier for a summarized non-file buffer.")

(defvar deslop-summary--source-buffers
  (make-hash-table :test #'equal :weakness 'value)
  "Map summary source identifiers to live non-file buffers.")

(defconst deslop-summary--citation-regexp
  (rx "[["
      "deslop-source:" (group (one-or-more digit))
      "]["
      (group (minimal-match (zero-or-more any)))
      "]]")
  "Regexp for source citations emitted by the summarization model.")

(defun deslop-summary-source-capture (buffer start end)
  "Capture BUFFER identity and absolute line range for START through END."
  (unless (and (buffer-live-p buffer)
               (number-or-marker-p start)
               (number-or-marker-p end))
    (error "summary source no longer exists"))
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (unless (<= (point-min) start end (point-max))
        (error "summary source range no longer exists"))
      (let* ((file (and buffer-file-name
                        (copy-sequence buffer-file-name)))
             (id (unless file
                   (format "s%d" (cl-incf deslop-summary--next-source-id))))
             (source
              (deslop-summary-source--create
               :buffer buffer
               :name (copy-sequence (or file (buffer-name buffer)))
               :file file
               :start-line (line-number-at-pos start t)
               :end-line (line-number-at-pos
                          (if (> end start) (1- end) start) t)
               :id id)))
        (when id
          (puthash id buffer deslop-summary--source-buffers))
        source))))

(defun deslop-summary-number-source (source start-line)
  "Prefix each line of SOURCE with its absolute line from START-LINE."
  (let* ((trailing-newline (string-suffix-p "\n" source))
         (lines (split-string source "\n" nil))
         (lines (if trailing-newline (butlast lines) lines))
         (line start-line)
         (numbered
          (mapconcat
           (lambda (text)
             (prog1 (format "%d | %s" line text)
               (setq line (1+ line))))
           lines
           "\n")))
    (concat numbered (and trailing-newline "\n"))))

(defun deslop-summary--source-link (source line &optional description)
  "Return an Org link to LINE in SOURCE with DESCRIPTION."
  (let ((target
         (if-let* ((file (deslop-summary-source-file source)))
             (format "file:%s::%d" file line)
           (format "deslop-buffer:%s::%d"
                   (deslop-summary-source-id source) line))))
    (if (fboundp 'org-make-link-string)
        (org-make-link-string target (or description "source"))
      (format "[[%s][%s]]" target (or description "source")))))

(defun deslop-summary--rewrite-citations (source content)
  "Replace valid source citation placeholders in CONTENT for SOURCE."
  (replace-regexp-in-string
   deslop-summary--citation-regexp
   (lambda (match)
     (let* ((line (string-to-number (match-string 1 match)))
            (description (match-string 2 match)))
       (if (<= (deslop-summary-source-start-line source)
               line
               (deslop-summary-source-end-line source))
           (save-match-data
             (deslop-summary--source-link source line description))
         (format "[source line %d]" line))))
   content t t))

(defun deslop-summary-follow-buffer-link (path _argument)
  "Follow a safe summary buffer link encoded by PATH.
PATH must have the form SOURCE-ID::LINE."
  (unless (string-match "\\`\\(s[0-9]+\\)::\\([1-9][0-9]*\\)\\'" path)
    (user-error "Invalid deslop buffer link: %s" path))
  (let* ((id (match-string 1 path))
         (line (string-to-number (match-string 2 path)))
         (buffer (gethash id deslop-summary--source-buffers)))
    (unless (buffer-live-p buffer)
      (user-error "The referenced deslop source buffer no longer exists"))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (goto-char (point-min))
        (when (/= (forward-line (1- line)) 0)
          (user-error "Source buffer has no line %d" line))))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (widen)
      (goto-char (point-min))
      (forward-line (1- line)))))

(org-link-set-parameters "deslop-buffer"
                         :follow #'deslop-summary-follow-buffer-link)

(defun deslop-summary-buffer ()
  "Return a non-file-visiting summary buffer owned by deslop."
  (setq deslop-summary--buffer-object
        (deslop-owned-buffer deslop-summary--buffer-object
                              deslop-summary-buffer-name
                              'deslop-summary--owned-buffer
                              #'org-mode)))

(defun deslop-summary-append (source content display)
  "Append CONTENT for captured SOURCE to the summary buffer.
DISPLAY supplies the model label."
  (let ((summary-buffer (deslop-summary-buffer))
        (heading-link
         (deslop-summary--source-link
          source (deslop-summary-source-start-line source)
          (deslop-summary-source-name source)))
        (content (deslop-summary--rewrite-citations source content)))
    (with-current-buffer summary-buffer
      (let ((inhibit-read-only t))
        (save-restriction
          (widen)
          (save-excursion
            (goto-char (point-max))
            (atomic-change-group
              (unless (bobp) (insert "\n\n"))
              (insert (format "* %s - %s - %s\n"
                              heading-link
                              (format-time-string "%Y-%m-%d %H:%M:%S")
                              (or (plist-get display :model) "unknown")))
              (insert content))))))
    (deslop-ignore-errors #'display-buffer summary-buffer)
    t))

(provide 'deslop-summary)

;;; deslop-summary.el ends here
