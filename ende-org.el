;;; ende-org.el --- Shared Org literal-region safety -*- lexical-binding: t; -*-

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

;; Shared Org literal-region safety for Ende.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defun ende-org--property (name value)
  "Return a sanitized, nonempty Org property for NAME and VALUE, or nil."
  (let ((value (cond ((stringp value) value)
                     ((numberp value) (format "%s" value))
                     ((memq value '(on off)) (symbol-name value)))))
    (when value
      (setq value (string-trim (replace-regexp-in-string "[\r\n]+" " " value)))
      (unless (string-empty-p value)
        (format ":%s: %s\n" name value)))))

(defun ende-org--metadata-properties (metadata)
  "Render retained response METADATA as durable Org properties."
  (mapconcat
   (lambda (field)
     (or (ende-org--property (cdr field) (plist-get metadata (car field))) ""))
   '((:model . "ENDE_MODEL")
     (:temperature . "ENDE_TEMPERATURE")
     (:thinking . "ENDE_THINKING")
     (:reasoning-effort . "ENDE_REASONING_EFFORT")
     (:input-tokens . "ENDE_INPUT_TOKENS")
     (:output-tokens . "ENDE_OUTPUT_TOKENS")
     (:cached-tokens . "ENDE_CACHED_TOKENS")) ""))

(defun ende-org--json-display-text (object &rest options)
  "Return OBJECT serialized as JSON text using OPTIONS."
  (decode-coding-string (apply #'json-serialize object options) 'utf-8-unix))

(defun ende-org--tool-argument-summary (arguments)
  "Return a compact display summary for JSON object ARGUMENTS, or nil."
  (condition-case nil
      (let ((object (json-parse-string
                     arguments :object-type 'alist :array-type 'array
                     :null-object :null :false-object :false)))
        (when (and (consp object)
                   (cl-every (lambda (entry)
                               (and (consp entry) (symbolp (car entry))))
                             object))
          (mapconcat
           (lambda (entry)
             (let* ((key (symbol-name (car entry)))
                    (display-key
                     (if (string-match-p "\\`[[:alpha:]_][[:alnum:]_-]*\\'" key)
                         key
                       (ende-org--json-display-text key)))
                    (value (ende-org--json-display-text
                            (cdr entry) :null-object :null :false-object :false)))
               (concat display-key "=" value)))
           object ", ")))
    (error nil)))

(defun ende-org--truncate-heading (title level)
  "Keep the complete heading TITLE at LEVEL within 180 display columns."
  (let ((width (max 1 (- 180 (1+ level)))))
    (if (> (string-width title) width)
        (truncate-string-to-width title width 0 nil "…")
      title)))

(defun ende-org--block-directive (line)
  "Return the indented, case-insensitive block directive in LINE, or nil."
  (let ((case-fold-search t))
    (cond
     ((string-match
       "\\`[ \t]*#\\+begin_\\([[:alnum:]_-]+\\)\\(?:[ \t].*\\)?[ \t\r]*\\'" line)
      (cons 'begin (downcase (match-string 1 line))))
     ((string-match "\\`[ \t]*#\\+end_\\([[:alnum:]_-]+\\)[ \t\r]*\\'" line)
      (cons 'end (downcase (match-string 1 line)))))))

(defun ende-org--mask-ranges (text ranges)
  "Mask buffer-position RANGES in TEXT, preserving newlines and coordinates."
  (let ((offset 0) parts)
    (dolist (range (sort (copy-sequence ranges)
                         (lambda (left right) (< (car left) (car right)))))
      (let ((start (1- (car range))) (end (1- (cdr range))))
        (push (substring text offset start) parts)
        (push (replace-regexp-in-string "[^\n]" " " (substring text start end)) parts)
        (setq offset end)))
    (push (substring text offset) parts)
    (apply #'concat (nreverse parts))))

(defun ende-org--blocks (text &optional repair)
  "Classify literal blocks in TEXT, optionally REPAIR assistant presentation.
Read-only analysis retains source coordinates, including for unfinished blocks.
REPAIR escapes block bodies and closes open blocks at this text's boundary.
The returned :edits retain the input coordinates of those repairs."
  (with-temp-buffer
    (insert (or text ""))
    (let (open-block block-start synthetic-newline ranges edits (offset 0))
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let* ((line-start (point))
               (line-end (line-end-position))
               (directive (ende-org--block-directive
                           (buffer-substring-no-properties line-start line-end))))
          (if open-block
              (if (and directive (eq (car directive) 'end)
                       (equal (cdr directive) open-block))
                  (progn
                    (push (cons block-start (min (point-max) (1+ line-end))) ranges)
                    (setq open-block nil block-start nil))
                (when repair
                  (skip-chars-forward " \t")
                  (when (looking-at "\\(?:\\*\\|#\\+\\|,\\(?:\\*\\|#\\+\\|,\\)\\)")
                    (let ((position (- (point) offset)))
                      (push (list position position ",") edits))
                    (insert ",")
                    (cl-incf offset))))
            (when (and directive (eq (car directive) 'begin))
              (setq open-block (cdr directive) block-start line-start)))
          (forward-line 1)))
      (when open-block
        (goto-char (point-max))
        (when repair
          (let* ((position (- (point) offset))
                 (suffix (concat (unless (bolp) (setq synthetic-newline t) "\n")
                                 (format "#+end_%s\n" open-block))))
            (push (list position position suffix) edits)
            (insert suffix)))
        (push (cons block-start (point-max)) ranges))
      (let ((content (buffer-string)))
        (list :content content :ranges (reverse ranges)
              :edits (nreverse edits)
              :synthetic-end (and repair open-block)
              :synthetic-newline synthetic-newline
              :parse-content (ende-org--mask-ranges content ranges))))))

(defun ende-org--apply-edits (text edits)
  "Apply (BEGIN END REPLACEMENT) EDITS in TEXT's source coordinate space."
  (with-temp-buffer
    (insert text)
    (dolist (edit (sort (copy-sequence edits)
                        (lambda (left right) (> (car left) (car right)))))
      (delete-region (nth 0 edit) (nth 1 edit))
      (goto-char (nth 0 edit))
      (insert (nth 2 edit)))
    (buffer-string)))

(provide 'ende-org)
;;; ende-org.el ends here
