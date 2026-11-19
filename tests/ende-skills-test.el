;;; ende-skills-test.el --- Project-local skill tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter Samarin
;;
;; Author: Peter Samarin <peter.samarin@gmail.com>
;; Maintainer: Peter Samarin <peter.samarin@gmail.com>
;; Keywords: lisp, tools
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

;; Exercise discovery with resolved Org policies, not synthetic grants.

;;; Code:

(require 'ende-chat-test-helpers)
(require 'ende-skills)

(defun ende-test--write-skill (root directory contents)
  (let ((path (expand-file-name (concat ".agents/skills/" directory "/SKILL.md") root)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path (insert contents))))

(ert-deftest ende-test/skills-require-both-project-listing-and-reading ()
  (ende-test--with-project (root)
    (dolist (dir '("shared" "unlisted" "unreadable"))
      (ende-test--write-skill
       root dir (concat "---\nmetadata:\n  name: nested\n  short-description: Review changes\n"
                        "allowed-tools: [read_file]\ndescription: >-\n  Review\n  changes.\n---\nBody.\n")))
    (with-temp-buffer
      (org-mode)
      (insert (ende-test--chat-config-block
               `((root ,root) (tools list_project_files read_file)
                 (resource listed (include "*") (exclude "*/unlisted/*"))
                 (resource readable :root ,root (include "*") (exclude "*/unreadable/*"))
                 (set-tool-resources list_project_files :resources listed)
                 (set-tool-resources read_file :resources readable)))
              "* Default\n* Enabled\n" (ende-test--chat-config-block '((skills t)))
              "** Inherited\n** Off\n" (ende-test--chat-config-block '((skills nil)))
              "** Inactive\n" (ende-test--chat-config-block '((clear-tools read_file)))
              "** Fixed listing\n" (ende-test--chat-config-block
                                    '((set-tool-resources list_project_files :resources readable))))
      (let* ((doc (ende-chat--build-document))
             (enabled (ende-test--chat-config-at doc "Inherited"))
             (entry '(:name "shared" :description "Review changes."
                      :path ".agents/skills/shared/SKILL.md")))
        (should (equal (ende-skills-catalog enabled) (list entry)))
        (should (equal (ende-skills-read entry enabled) "Body.\n"))
        (dolist (scope '("Default" "Off" "Inactive" "Fixed listing"))
          (let ((policy (ende-test--chat-config-at doc scope)))
            (should-not (ende-skills-catalog policy))
            (should-not (ende-skills-read entry policy))))))))

(ert-deftest ende-test/skills-capture-bounds-catalog-not-explicit-selection ()
  (ende-test--with-project (root)
    (let ((names '("a" "b" "c" "d" "e" "f"))
          (description (make-string 1024 ?界)))
      (dolist (name names)
        (ende-test--write-skill root name (format "---\ndescription: %s\n---\nBody of %s.\n"
                                                  description name)))
      (with-temp-buffer
        (org-mode)
        (insert (ende-test--chat-config-block
                 `((root ,root) (skills t) (tools list_project_files read_file)
                   (resource project (include "*"))
                   (set-tool-resources list_project_files read_file :resources project)))
                "* Question\n")
        (let* ((policy (ende-test--chat-config-at (ende-chat--build-document) "Question"))
               (listing (ende-skills-capture policy "List skills."))
               (lines (seq-filter (lambda (s) (string-prefix-p "- " s)) (split-string listing "\n"))))
          (should (<= (string-bytes listing) ende-skills--catalog-bytes))
          (should (< 0 (length lines) (length names)))
          (cl-mapc (lambda (line name)
                     (should (equal line (format "- %s: %s (.agents/skills/%s/SKILL.md)"
                                                 name description name))))
                   lines names)
          (should (string-search "omitted" listing))
          (should (equal (ende-skills-capture policy "Use $f.")
                         (concat listing "\n\nSelected skill: f (.agents/skills/f/SKILL.md)\nBody of f.\n"))))))))

(provide 'ende-skills-test)

;;; ende-skills-test.el ends here
