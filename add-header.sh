#!/bin/sh
# add-header.sh --- Normalize Ende's Emacs Lisp release headers
#
# Copyright (C) 2026 Peter Samarin
# Author: Peter Samarin <peter.samarin@gmail.com>
# Maintainer: Peter Samarin <peter.samarin@gmail.com>
# URL: https://github.com/oetr/ende-mode
# SPDX-License-Identifier: AGPL-3.0-only
#
# This file is part of Ende.
# Ende is free software: you can redistribute it and/or modify it under
# the terms of the GNU Affero General Public License as published by the
# Free Software Foundation, version 3 of the License.
# Ende is distributed WITHOUT ANY WARRANTY; without even the implied
# warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See LICENSE or <https://www.gnu.org/licenses/> for details.
#
# Usage: sh add-header.sh [--check] [--] [FILE OR DIRECTORY ...]
# Without paths, process this script's directory recursively, not the cwd.
# Requires Emacs (override its executable with EMACS=/path/to/emacs).
# Existing filename/summary lines and Lisp dialect cookies are preserved.
# --check exits 1 for pending changes, 0 when clean; errors exit 2.
# All inputs are validated before writing; replacements are atomic per file.

set -eu
script_dir=$(CDPATH= cd -P "$(dirname "$0")" && pwd)
exec "${EMACS:-emacs}" -Q --script /dev/stdin -- "$script_dir" "$@" <<'ENDE_HEADER_ELISP'
;;; -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'lisp-mnt)
(require 'subr-x)

(defconst ende-header--contact "Peter Samarin <peter.samarin@gmail.com>")
(defconst ende-header--url "https://github.com/oetr/ende-mode")
(defconst ende-header--license
  (concat
   ";; This file is part of Ende.\n\n"
   ";; Ende is free software: you can redistribute it and/or modify\n"
   ";; it under the terms of the GNU Affero General Public License as published\n"
   ";; by the Free Software Foundation, version 3 of the License.\n"
   ";;\n"
   ";; Ende is distributed in the hope that it will be useful,\n"
   ";; but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
   ";; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
   ";; GNU Affero General Public License for more details.\n"
   ";;\n"
   ";; You should have received a copy of the GNU Affero General Public License\n"
   ";; along with Ende.  If not, see <https://www.gnu.org/licenses/>.\n"))

(defun ende-header--forms (text)
  "Return exact top-level form strings in TEXT, without evaluating them."
  (with-temp-buffer
    (insert text)
    (emacs-lisp-mode)
    (check-parens)
    (goto-char (point-min))
    (let (forms)
      (forward-comment (point-max))
      (while (< (point) (point-max))
        (let ((start (point)))
          (read (current-buffer))
          (push (buffer-substring-no-properties start (point)) forms))
        (forward-comment (point-max)))
      (nreverse forms))))

(defun ende-header--metadata (prefix)
  "Extract own copyrights, other metadata, and remaining comments in PREFIX.
Refuse foreign ownership or unfamiliar licensing instead of overwriting it."
  (with-temp-buffer
    (insert (string-replace ende-header--license "" prefix))
    (goto-char (point-min))
    (let ((case-fold-search t) copyrights fields)
      (while (re-search-forward "^;;[ \t]+Copyright[^\n]*\n?" nil t)
        (let ((start (match-beginning 0))
              (end (match-end 0))
              (line (string-trim-right (match-string 0))))
          (unless (string-match
                   "\\`;; Copyright (C) \\([0-9][0-9, -]*\\) Peter\\(?: Samarin\\)?\\'"
                   line)
            (error "Unrecognized copyright: %s" line))
          (push (concat ";; Copyright (C) " (match-string 1 line)
                        " Peter Samarin\n") copyrights)
          (delete-region start end)))
      (goto-char (point-min))
      (while (re-search-forward "^;;[ \t]+\\([[:alnum:]-]+\\):[ \t]*\\([^\n]*\\)" nil t)
        (let ((start (line-beginning-position))
              (field (downcase (match-string 1)))
              (value-start (match-beginning 2))
              value)
          (forward-line 1)
          (while (looking-at ";;[ \t][ \t]+[^\n]*\n?")
            (goto-char (match-end 0)))
          (setq value (string-trim
                       (replace-regexp-in-string
                        "\n;;[ \t]+" " "
                        (buffer-substring-no-properties value-start (point)))))
          (cond
           ((member field '("author" "maintainer"))
            (unless (string-match-p
                     "\\`Peter\\(?: Samarin\\)?\\(?: <[^<>\n]+>\\)?\\'" value)
              (error "Unrecognized %s: %s" field value)))
           ((member field '("spdx-license-identifier" "license"))
            (unless (equal value "AGPL-3.0-only")
              (error "Conflicting license: %s" value)))
           ((member field '("keywords" "url")))
           (t (push (buffer-substring-no-properties start (point)) fields)))
          (delete-region start (point))))
      (goto-char (point-min))
      (when (re-search-forward "licen[cs]e\\|copyright\\|SPDX" nil t)
        (error "Unrecognized license/copyright notice; review this header manually"))
      (goto-char (point-min))
      (while (re-search-forward "^;;[ \t]*$" nil t)
        (replace-match ""))
      (list (or (nreverse copyrights)
                '(";; Copyright (C) 2026 Peter Samarin\n"))
            (nreverse fields) (string-trim (buffer-string))))))

(defun ende-header--check-commentary (text)
  "Refuse structured legal declarations in TEXT that conflict with this project.
Ordinary prose and illustrative field values are not package metadata."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward
              (concat "^;;[ \t]+\\(?:Copyright[^\n]*"
                      "\\|\\(?:Author\\|Maintainer\\):[ \t]*"
                      "[[:alpha:] .'-]+\\(?: <[^<>\n]+>\\)?"
                      "\\|\\(?:SPDX-License-Identifier\\|License\\):[ \t]*"
                      "[[:alnum:] .()+-]+\\)[ \t]*$") nil t)
        (let ((start (match-beginning 0)))
          (forward-line 1)
          (while (looking-at ";;[ \t][ \t]+[^\n]*\n?")
            (goto-char (match-end 0)))
          (ende-header--metadata (buffer-substring-no-properties start (point))))))))

(defun ende-header--body (body name)
  "Remove redundant top-level Code comments and complete BODY's footer."
  (with-temp-buffer
    (insert body)
    (emacs-lisp-mode)
    (goto-char (point-min))
    (while (re-search-forward "^;;; Code:[ \t]*\n?" nil t)
      (let ((state (save-excursion
                     (save-match-data (syntax-ppss (match-beginning 0))))))
        (when (and (zerop (car state)) (not (nth 3 state)) (not (nth 4 state)))
          (replace-match ""))))
    (goto-char (point-min))
    (let ((last-end (point-min)) footers)
      (forward-comment (point-max))
      (while (< (point) (point-max))
        (read (current-buffer))
        (setq last-end (point))
        (forward-comment (point-max)))
      (goto-char last-end)
      (while (re-search-forward "^;;; [^\n]+\\.el ends here[ \t]*$" nil t)
        (push (match-string 0) footers))
      (unless (and (equal footers (list (concat ";;; " name " ends here")))
                   (string-suffix-p (car footers) (string-trim-right (buffer-string))))
        (goto-char last-end)
        (while (re-search-forward "^;;; [^\n]+\\.el ends here[ \t]*\n?" nil t)
          (replace-match ""))
        (goto-char (point-max))
        (delete-region (progn (skip-chars-backward " \t\n") (point)) (point-max))
        (insert "\n\n;;; " name " ends here\n")))
    (buffer-string)))

(defun ende-header--normalize (text file)
  "Normalize FILE's TEXT, preserving its first line and executable form bytes."
  (let ((original-forms (ende-header--forms text))
        (name (file-name-nondirectory file)))
    (with-temp-buffer
      (insert text)
      (emacs-lisp-mode)
      (goto-char (point-min))
      (unless (looking-at (concat ";;; " (regexp-quote name) " --- \\(.+\\)$"))
        (error "Expected an existing ;;; %s --- summary first line" name))
      (let* ((first-line (buffer-substring-no-properties (point-min) (line-end-position)))
             (summary (replace-regexp-in-string
                       "[ \t]+-\\*-.*-\\*-[ \t]*\\'" "" (match-string 1)))
             (prefix-start (progn (forward-line 1) (point)))
             (first-form (progn (goto-char (point-min))
                                (forward-comment (point-max)) (point)))
             code-start code-end commentary-start commentary-end body-start)
        (goto-char prefix-start)
        (when (re-search-forward "^;;; Code:[ \t]*\n?" first-form t)
          (setq code-start (match-beginning 0) code-end (match-end 0)))
        (setq body-start (or code-end first-form))
        (unless code-start
          ;; Keep autoload cookies attached to their forms, not in Commentary.
          (goto-char prefix-start)
          (when (re-search-forward "^;;;\\(?:###autoload\\| [^\n]+\\.el ends here\\)" first-form t)
            (setq body-start (match-beginning 0))))
        (goto-char prefix-start)
        (when (re-search-forward "^;;; Commentary:[ \t]*\n?" (or code-start body-start) t)
          (setq commentary-start (match-beginning 0) commentary-end (match-end 0)))
        (pcase-let* ((`(,copyrights ,fields ,extra)
                      (ende-header--metadata
                       (buffer-substring-no-properties
                        prefix-start (or commentary-start code-start body-start))))
                    (commentary (if commentary-end
                                    (string-trim
                                     (buffer-substring-no-properties
                                      commentary-end (or code-start body-start))
                                     "\n+" "\n+")
                                  (concat ";; " summary
                                          (if (string-suffix-p "." summary) "" "."))))
                    (body (ende-header--body
                           (buffer-substring-no-properties body-start (point-max)) name))
                    (result
                     (concat first-line "\n\n" (apply #'concat copyrights) ";;\n"
                             ";; Author: " ende-header--contact "\n"
                             ";; Maintainer: " ende-header--contact "\n"
                             (apply #'concat fields)
                             ";; Keywords: " (if (string-match-p "/tests/" file)
                                                  "lisp, tools" "convenience, tools") "\n"
                             ";; URL: " ende-header--url "\n"
                             ";; SPDX-License-Identifier: AGPL-3.0-only\n\n"
                             ende-header--license "\n;;; Commentary:\n\n" commentary
                             (if (string-empty-p extra) "" (concat "\n\n" extra))
                             "\n\n;;; Code:\n"
                             (if (or code-start (string-prefix-p "\n" body)) "" "\n") body)))
          (ende-header--check-commentary commentary)
          (unless (equal original-forms (ende-header--forms result))
            (error "Executable form bytes would change"))
          (with-temp-buffer
            (insert result)
            (unless (and (equal (lm-authors) '(("Peter Samarin" . "peter.samarin@gmail.com")))
                         (equal (lm-maintainers) (lm-authors))
                         (equal (lm-header "url") ende-header--url))
              (error "Generated metadata did not parse")))
          result)))))

(defun ende-header--bytes (file)
  "Read FILE literally, without hooks or local variable evaluation."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun ende-header--plan (file)
  "Validate FILE and return a replacement plan, or nil if unchanged."
  (let ((original (ende-header--bytes file)))
    (with-temp-buffer
      (insert-file-contents file)
      (let* ((coding buffer-file-coding-system)
             (text (buffer-string))
             (replacement (encode-coding-string (ende-header--normalize text file) coding)))
        (unless (equal original (encode-coding-string text coding))
          (error "Source encoding does not round-trip losslessly"))
        (unless (equal replacement original)
          (list file original replacement (file-modes file)))))))

(defun ende-header--write (plan)
  "Publish PLAN atomically, retaining permissions and checking for changes."
  (pcase-let ((`(,file ,original ,replacement ,mode) plan))
    (when (or (file-symlink-p file) (not (equal original (ende-header--bytes file))))
      (error "File changed during normalization: %s" file))
    (let ((stage (make-temp-file (expand-file-name ".ende-header-" (file-name-directory file)))))
      (unwind-protect
          (let ((coding-system-for-write 'no-conversion))
            (write-region replacement nil stage nil 'silent)
            (set-file-modes stage mode)
            (rename-file stage file t))
        (when (file-exists-p stage) (delete-file stage))))))

(defun ende-header--files (paths)
  "Collect sorted regular .el files in PATHS, excluding Git and symlinks."
  (let (files)
    (dolist (path paths)
      (setq path (expand-file-name path))
      (cond
       ((or (file-remote-p path) (file-symlink-p path))
        (error "Refusing remote or symlink target: %s" path))
       ((member ".git" (split-string path "/" t))
        (error "Refusing a Git-internal target: %s" path))
       ((file-directory-p path)
        (setq files
              (append files
                      (cl-remove-if-not
                       (lambda (file)
                         (and (not (file-symlink-p file)) (file-regular-p file)))
                       (directory-files-recursively
                        path "\\.el\\'" nil
                        (lambda (directory)
                          (not (equal (file-name-nondirectory directory) ".git"))))))))
       ((and (file-regular-p path) (string-suffix-p ".el" path))
        (push path files))
       (t (error "Not an Emacs Lisp file or directory: %s" path))))
    (sort (delete-dups files) #'string<)))

(defun ende-header--main (root arguments)
  "Process command-line ARGUMENTS, using ROOT when no paths are given."
  (let (check paths literal help)
    (dolist (argument arguments)
      (cond
       (literal (push argument paths))
       ((equal argument "--") (setq literal t))
       ((equal argument "--check") (setq check t))
       ((member argument '("--help" "-h")) (setq help t))
       ((string-prefix-p "-" argument) (error "Unknown option: %s" argument))
       (t (push argument paths))))
    (if help
        (progn
          (princ "Usage: sh add-header.sh [--check] [--] [FILE OR DIRECTORY ...]\n")
          (princ "Default: all .el files below the script, excluding .git and symlinks.\n")
          (princ "Preserves summaries, Lisp dialects, commentary, metadata, encodings and code.\n")
          (princ "Conflicting ownership/licensing or malformed Lisp is refused before writing.\n")
          (princ "Exit codes: 0 = success/clean; 1 = --check found changes; 2 = error.\n")
          0)
      (let* ((files (ende-header--files (or (nreverse paths) (list root))))
             ;; Complete preflight before touching any target.
             (plans (delq nil (mapcar
                              (lambda (file)
                                (condition-case err (ende-header--plan file)
                                  (error (error "%s: %s" file (error-message-string err)))))
                              files))))
        (dolist (plan plans)
          (unless check (ende-header--write plan))
          (princ (format "%s %s\n" (if check "Would update" "Updated") (car plan))))
        (princ (format "%d file(s) checked; %d %s.\n" (length files) (length plans)
                       (if check "need changes" "updated")))
        (if (and check plans) 1 0)))))

(when (equal (car command-line-args-left) "--") (pop command-line-args-left))
(let ((root (pop command-line-args-left))
      (arguments command-line-args-left))
  (setq command-line-args-left nil)
  (kill-emacs
   (condition-case err (ende-header--main root arguments)
     (error (message "add-header: %s" (error-message-string err)) 2))))
ENDE_HEADER_ELISP
