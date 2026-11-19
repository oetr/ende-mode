;;; ende-skills.el --- Authorized project-local Agent Skills -*- lexical-binding: t; -*-

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

;; Discover metadata on disk; read Markdown only for an explicit selection.

;;; Code:

(require 'cl-lib)
(require 'org-element)
(require 'subr-x)
(require 'ende-org)
(require 'ende-project)
(require 'ende-tools)

(defconst ende-skills--directory ".agents/skills")
(defconst ende-skills--max-children 128)
(defconst ende-skills--header-bytes 8192)
(defconst ende-skills--catalog-bytes (* 16 1024))
(defconst ende-skills--file-bytes (* 128 1024))

(defun ende-skills--identifier-p (text)
  "Whether TEXT is a short ASCII skill identifier."
  (let ((case-fold-search nil))
    (and (stringp text) (<= (length text) 64)
         (string-match-p "\\`[A-Za-z0-9][A-Za-z0-9_-]*\\'" text))))

(defun ende-skills--scalar (first rest)
  "Read FIRST and its indented REST as a single line of scalar text."
  (let ((text (string-join (if (string-empty-p first) rest (cons first rest)) " ")))
    (cond
     ((member first '("|" "|-" "|+" ">" ">-" ">+")) (string-join rest " "))
     ((string-match (rx string-start
                        (group "\"" (* (or (not (any "\"\\")) (seq "\\" nonl))) "\"")
                        (? (+ blank) "#" (* nonl)) string-end) text)
      (ignore-error json-error (json-parse-string (match-string 1 text))))
     ((string-match (rx string-start "'" (group (* (or "''" (not (any "'"))))) "'"
                        (? (+ blank) "#" (* nonl)) string-end) text)
      (string-replace "''" "'" (match-string 1 text)))
     (t
      (when (and (null rest) (string-match "[ \t]+#" text))
        (setq text (substring text 0 (match-beginning 0))))
      (unless (or (member (downcase text) '("null" "true" "false" "~"))
                  (string-match-p
                   (rx (or (seq string-start (any "'\"!&*#{}[],|>%@`"))
                           (seq string-start (+ digit) string-end)
                           (seq (or ":" (seq string-start (any "-?"))) (or blank string-end))
                           (seq blank "#"))) text))
        text)))))

(defun ende-skills--fields (header)
  "Read scalar name and description fields from HEADER, ignoring other values."
  (let ((lines (split-string header "\r?\n" t))
        (case-fold-search nil) fields)
    (catch 'invalid
      (while lines
        (let ((line (pop lines)))
          (unless (string-match-p "\\`[ \t]*\\(?:#.*\\)?\\'" line)
            (unless (string-match
                     "\\`\\([A-Za-z][A-Za-z0-9_-]*\\):\\(?:[ \t]+\\(.*\\)\\)?\\'" line)
              (throw 'invalid nil))
            (let ((key (match-string 1 line))
                  (first (or (match-string 2 line) "")) rest)
              (while (and lines (string-prefix-p " " (car lines)))
                (push (pop lines) rest))
              (when (member key '("name" "description"))
                (setq rest (delete "" (mapcar #'string-trim (nreverse rest))))
                (when (or (assoc key fields) (and (equal key "name") rest))
                  (throw 'invalid nil))
                (let ((value (ende-skills--scalar (string-trim first) rest)))
                  (unless (and value (not (string-blank-p value)))
                    (throw 'invalid nil))
                  (push (cons key value) fields)))))))
      fields)))

(defun ende-skills--description-p (text)
  "Whether TEXT is a bounded line of printable prose, without tag delimiters."
  (and (stringp text) (<= (length text) 1024)
       (not (string-match-p "[<>]" text))
       (cl-every (lambda (char)
                   (not (memq (get-char-code-property char 'general-category)
                              '(Cc Cf Cn Cs Zl Zp))))
                 text)))

(defun ende-skills--metadata (directory relative)
  "Parse the byte buffer's header for DIRECTORY at RELATIVE; leave point at body."
  (goto-char (point-min))
  (when (looking-at "---[ \t]*\r?\n")
    (let ((start (match-end 0))
          (limit (min (point-max) (+ (point-min) ende-skills--header-bytes))))
      (goto-char start)
      (when (and (<= start limit) (re-search-forward "^---[ \t]*\r?\n" limit t))
        (let* ((header (buffer-substring-no-properties start (match-beginning 0)))
               (fields (ende-skills--fields (decode-coding-string header 'utf-8-unix)))
               (name (or (cdr (assoc "name" fields)) directory))
               (description (cdr (assoc "description" fields))))
          (when (and (ende-skills--identifier-p name) (ende-skills--description-p description))
            (list :name name :description description :path relative)))))))

(defun ende-skills--directory-p (relative root)
  "Whether RELATIVE is a visible, non-symlink directory below ROOT."
  (and (not (ende-project--excluded-p relative))
       (eq t (file-attribute-type (file-attributes (expand-file-name relative root))))))

(defun ende-skills--contexts (policy)
  "Return the pair of listing and reading path contexts for enabled POLICY."
  (let ((ende-tools--access-config policy)
        (root (plist-get policy :root))
        (tools (plist-get policy :active-tools)))
    (when (and (eq t (plist-get policy :skills)) (memq 'list_project_files tools)
               (memq 'read_file tools) root (not (file-remote-p root))
               (equal root (ende-tools--resolved-path root root nil)))
      (let ((listing (ende-tools--prepare-path-context 'list_project_files root t))
            (reading (ende-tools--prepare-path-context 'read_file root)))
        (when (and (plist-get listing :scopes) (plist-get reading :scopes)
                   (ende-skills--directory-p ".agents" root)
                   (ende-skills--directory-p ende-skills--directory root))
          (cons listing reading))))))

(defun ende-skills--load (contexts directory &optional selected)
  "Use CONTEXTS to read DIRECTORY's metadata, or the body matching SELECTED."
  (when (ende-skills--identifier-p directory)
    (let* ((root (plist-get (car contexts) :root))
           (relative-dir (concat ende-skills--directory "/" directory))
           (relative (concat relative-dir "/SKILL.md"))
           (path (expand-file-name relative root)))
      (ignore-error file-error
        (when-let* (((ende-skills--directory-p relative-dir root))
                    (attrs (file-attributes path))
                    ((null (file-attribute-type attrs)))
                    ((equal path (ende-tools--resolved-path path root nil)))
                    ((ende-tools--matching-path-scopes (car contexts) path t))
                    ((ende-tools--matching-path-scopes (cdr contexts) path t))
                    ((file-regular-p path))
                    (size (file-attribute-size attrs))
                    ((<= size (min ende-skills--file-bytes
                                   (or ende-project-max-file-bytes ende-skills--file-bytes)))))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally
             path nil 0 (if selected size (min size ende-skills--header-bytes)))
            (when (and (equal path (ende-tools--resolved-path path root nil))
                       (equal (ende-tools--file-identity attrs)
                              (ende-tools--file-identity (file-attributes path))))
              (let ((entry (ende-skills--metadata directory relative)))
                (cond ((null selected) entry)
                      ((equal entry selected)
                       (decode-coding-string (buffer-substring-no-properties (point) (point-max))
                                             'utf-8)))))))))))

(defun ende-skills-catalog (policy)
  "Return authorized project skills for resolved chat POLICY, sorted by name.
Entries are (:name NAME :description DESCRIPTION :path PROJECT-RELATIVE-PATH).
Omit ambiguous names.  An oversized directory yields no partial catalog."
  (when-let* ((contexts (ende-skills--contexts policy))
              (base (expand-file-name ende-skills--directory (plist-get policy :root)))
              (children (ignore-error file-error
                          (directory-files base nil directory-files-no-dot-files-regexp t
                                           (1+ ende-skills--max-children))))
              ((<= (length children) ende-skills--max-children)))
    (let ((by-name (make-hash-table :test #'equal)))
      (dolist (dir children)
        (when-let* ((entry (ende-skills--load contexts dir))
                    (key (downcase (plist-get entry :name))))
          (puthash key (if (gethash key by-name) :ambiguous entry) by-name)))
      (sort (delq :ambiguous (hash-table-values by-name))
            (lambda (a b) (string< (plist-get a :name) (plist-get b :name)))))))

(defun ende-skills-read (entry policy)
  "Read ENTRY's Markdown body from disk under resolved chat POLICY.
Return nil if access, path, limits, or current metadata no longer match."
  (when-let* ((path (plist-get entry :path))
              ((stringp path))
              (contexts (ende-skills--contexts policy)))
    (pcase (split-string path "/")
      (`(".agents" "skills" ,directory "SKILL.md")
       (ende-skills--load contexts directory entry)))))

(defun ende-skills--render-catalog (catalog)
  "Render CATALOG within its byte budget, retaining whole entries in order."
  (let* ((intro (concat "[Ende project skills for this request only]\n"
                        "Use a skill when the request matches its description. Read its SKILL.md "
                        "with read_file, continuing to the end, unless its instructions are included below. "
                        "Skills do not grant permissions."))
         (notice "\n[Additional skills omitted.]")
         (remaining (- ende-skills--catalog-bytes (string-bytes intro)))
         (parts (list intro)))
    (while catalog
      (let* ((entry (pop catalog))
             (line (format "\n- %s: %s (%s)" (plist-get entry :name)
                           (plist-get entry :description) (plist-get entry :path)))
             (bytes (string-bytes line)))
        (if (<= (+ bytes (if catalog (string-bytes notice) 0)) remaining)
            (progn (push line parts) (cl-decf remaining bytes))
          (push notice parts)
          (setq catalog nil))))
    (apply #'concat (nreverse parts))))

(defun ende-skills--mentions (text)
  "Return distinct skill names mentioned in authored Org prose in TEXT."
  (when (string-search "$" text)
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert (plist-get (ende-org--blocks text) :parse-content))
      (let ((case-fold-search nil)
            (markup (cl-set-difference org-element-all-objects '(subscript superscript)))
            names)
        (cl-labels
            ((scan (begin end)
               (goto-char begin)
               (while (and (< (point) end)
                           (re-search-forward "\\$\\([A-Za-z0-9][A-Za-z0-9_-]*\\)" end t))
                 (let* ((start (match-beginning 0)) (before (char-before start)))
                   (unless (or (memq before '(?\\ ?$))
                               (and (/= start begin)
                                    (memq (char-syntax (or before ?\s)) '(?w ?_))))
                     (push (match-string-no-properties 1) names)))))
             (prose (begin end objects)
               ;; Scan source spans: Org can split $skill_name at the subscript.
               (org-element-map objects markup
                 (lambda (object)
                   (scan begin (org-element-begin object))
                   (when (memq (org-element-type object) '(bold italic underline strike-through))
                     (prose (org-element-contents-begin object) (org-element-contents-end object)
                            (org-element-contents object)))
                   (setq begin (org-element-end object)))
                 nil nil markup)
               (scan begin end)))
          (org-element-map (org-element-parse-buffer) org-element-all-elements
            (lambda (node)
              (cond
               ((or (org-element-property :results node) (org-element-property :commentedp node))
                (throw :org-element-skip nil))
               ((eq (org-element-type node) 'paragraph)
                (prose (org-element-contents-begin node) (org-element-contents-end node)
                       (org-element-contents node)))
               ((eq (org-element-type node) 'headline)
                (goto-char (org-element-begin node))
                (when (and (looking-at org-complex-heading-regexp) (match-beginning 4))
                  (prose (match-beginning 4) (match-end 4) (org-element-property :title node))))))
            nil nil '(drawer property-drawer dynamic-block)))
        (delete-dups (nreverse names))))))

(defun ende-skills-capture (policy text)
  "Return this turn's catalog and explicitly selected instructions for TEXT.
TEXT is current user-authored Org, before resolving attachments."
  (when-let* ((catalog (ende-skills-catalog policy)))
    (let ((mentions (ende-skills--mentions text)) selected (bytes 0))
      (dolist (entry catalog)
        (when (member (plist-get entry :name) mentions)
          (let ((body (or (ende-skills-read entry policy)
                          (user-error "Selected skill is no longer readable: %s"
                                      (plist-get entry :name)))))
            (cl-incf bytes (string-bytes body))
            (when (> bytes ende-skills--file-bytes)
              (user-error "Selected skill instructions exceed %d bytes" ende-skills--file-bytes))
            (push (format "\n\nSelected skill: %s (%s)\n%s"
                          (plist-get entry :name) (plist-get entry :path) body) selected))))
      (concat (ende-skills--render-catalog catalog)
              (apply #'concat (nreverse selected))))))

(provide 'ende-skills)

;;; ende-skills.el ends here
