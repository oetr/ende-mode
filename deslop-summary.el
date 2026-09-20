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
  buffer buffer-name name file start-line end-line id)

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

(defun deslop-summary--register-source-buffer (buffer)
  "Register non-file BUFFER and return its session-local source identifier."
  (let ((id (format "s%d" (cl-incf deslop-summary--next-source-id))))
    (puthash id buffer deslop-summary--source-buffers)
    id))

(defun deslop-summary-source-capture (buffer start end)
  "Capture BUFFER identity and absolute line range for START through END.
END is exclusive; an empty range captures the line containing START.
Markers must belong to BUFFER.  Names and line numbers are snapshots."
  (unless (and (buffer-live-p buffer)
               (number-or-marker-p start)
               (number-or-marker-p end)
               (or (not (markerp start))
                   (eq (marker-buffer start) buffer))
               (or (not (markerp end))
                   (eq (marker-buffer end) buffer)))
    (error "summary source no longer exists"))
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (unless (<= (point-min) start end (point-max))
        (error "summary source range no longer exists"))
      (let ((file (and buffer-file-name (copy-sequence buffer-file-name))))
        (deslop-summary-source--create
         :buffer buffer
         :buffer-name (copy-sequence (buffer-name buffer))
         :name (copy-sequence (or file (buffer-name buffer)))
         :file file
         :start-line (line-number-at-pos start t)
         :end-line (line-number-at-pos (if (> end start) (1- end) start) t)
         :id (unless file (deslop-summary--register-source-buffer buffer)))))))

(defun deslop-summary-number-source (source start-line)
  "Prefix each line of SOURCE with its absolute line from START-LINE.
Preserve a trailing newline without numbering an extra empty line."
  (let* ((trailing-newline (string-suffix-p "\n" source))
         (text (if trailing-newline (substring source 0 -1) source))
         (numbered
          (cl-loop for line in (split-string text "\n" nil)
                   for number from start-line
                   collect (format "%d | %s" number line))))
    (concat (mapconcat #'identity numbered "\n")
            (and trailing-newline "\n"))))

(defun deslop-summary--source-link (source line &optional description)
  "Return an Org link to LINE in SOURCE with DESCRIPTION."
  (let ((target
         (if-let* ((file (deslop-summary-source-file source)))
             (format "file:%s::%d" file line)
           (format "deslop-buffer:%s::%d"
                   (deslop-summary-source-id source) line))))
    (org-make-link-string target (or description "source"))))

(defun deslop-summary--citation-link (source line description)
  "Render a citation to LINE in SOURCE with DESCRIPTION.
Out-of-range citations become plain text rather than navigable links."
  (if (<= (deslop-summary-source-start-line source)
          line
          (deslop-summary-source-end-line source))
      (deslop-summary--source-link source line description)
    (format "[source line %d]" line)))

(defun deslop-summary--rewrite-citations (source content)
  "Replace valid source citation placeholders in CONTENT for SOURCE."
  (replace-regexp-in-string
   deslop-summary--citation-regexp
   (lambda (match)
     (let ((line (string-to-number (match-string 1 match)))
           (description (match-string 2 match)))
       (save-match-data
         (deslop-summary--citation-link source line description))))
   content t t))

(defun deslop-summary--sanitize (source content)
  "Rewrite citations and escape top-level Org headings in CONTENT for SOURCE.
Only top-level headings are escaped, to preserve summary entry boundaries;
other Org markup is intentionally left intact."
  (replace-regexp-in-string
   "^\\* " ",* "
   (deslop-summary--rewrite-citations source content)))

(defun deslop-summary--source-position (path)
  "Resolve PATH to a (BUFFER . POSITION) pair without moving point.
Signal a user error if the link, buffer, or requested line is invalid."
  (save-match-data
    (unless (string-match "\\`\\(s[0-9]+\\)::\\([1-9][0-9]*\\)\\'" path)
      (user-error "Invalid deslop buffer link: %s" path))
    (let* ((id (match-string 1 path))
           (line (string-to-number (match-string 2 path)))
           (buffer (gethash id deslop-summary--source-buffers)))
      (unless (buffer-live-p buffer)
        (user-error "The referenced deslop source buffer no longer exists"))
      (with-current-buffer buffer
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (unless (zerop (forward-line (1- line)))
              (user-error "Source buffer has no line %d" line))
            (cons buffer (point))))))))

(defun deslop-summary-follow-buffer-link (path _argument)
  "Follow a safe summary buffer link encoded by PATH.
PATH must have the form SOURCE-ID::LINE."
  (pcase-let ((`(,buffer . ,position) (deslop-summary--source-position path)))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (widen)
      (goto-char position))))

(org-link-set-parameters "deslop-buffer"
                         :follow #'deslop-summary-follow-buffer-link)

(defun deslop-summary-buffer ()
  "Return a non-file-visiting summary buffer owned by deslop."
  (setq deslop-summary--buffer-object
        (deslop-owned-buffer deslop-summary--buffer-object
                              deslop-summary-buffer-name
                              'deslop-summary--owned-buffer
                              #'org-mode)))

(defun deslop-summary--single-line (text)
  "Replace line breaks in display TEXT with spaces."
  (replace-regexp-in-string "[\r\n]+" " " text t t))

(defun deslop-summary--entry-text (source content display timestamp)
  "Render an Org entry for SOURCE and CONTENT using DISPLAY and TIMESTAMP.
DISPLAY supplies the model label; TIMESTAMP is a formatted time string."
  (let ((buffer-name
         (deslop-summary--single-line
          (deslop-summary-source-buffer-name source)))
        (model
         (deslop-summary--single-line
          (or (plist-get display :model) "unknown")))
        (heading-link
         (deslop-summary--source-link
          source (deslop-summary-source-start-line source)
          (deslop-summary--single-line
           (deslop-summary-source-name source)))))
    (concat (format "* *%s* - [%s] - %s\nFile: %s\n"
                    buffer-name timestamp model heading-link)
            (deslop-summary--sanitize source content))))

(defun deslop-summary--append-entry (buffer text)
  "Append entry TEXT to BUFFER and leave point at its heading.
Return the heading position."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          start)
      (save-restriction
        (widen)
        (save-excursion
          (goto-char (point-max))
          (atomic-change-group
            (unless (bobp) (insert "\n\n"))
            (setq start (point))
            (insert text)))
        (goto-char start))
      start)))

(defun deslop-summary-append (source content display)
  "Append CONTENT for captured SOURCE to the summary buffer.
DISPLAY supplies the model label.
Move point to the new heading without selecting the summary window."
  (let* ((summary-buffer (deslop-summary-buffer))
         (text (deslop-summary--entry-text
                source content display
                (format-time-string "%d %B %Y %H:%M:%S")))
         (start (deslop-summary--append-entry summary-buffer text))
         (window (deslop-ignore-errors #'display-buffer summary-buffer)))
    (when (window-live-p window)
      (set-window-point window start))
    t))

(provide 'deslop-summary)

;;; deslop-summary.el ends here
