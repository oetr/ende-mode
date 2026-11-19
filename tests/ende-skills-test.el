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

(ert-deftest ende-test/skills-require-both-project-listing-and-reading ()
  (ende-test--with-project (root)
    (dolist (dir '("shared" "unlisted" "unreadable"))
      (let ((path (expand-file-name (concat ".agents/skills/" dir "/SKILL.md") root)))
        (make-directory (file-name-directory path) t)
        (with-temp-file path (insert "---\ndescription: >-\n  Review\n  changes.\n---\nBody.\n"))))
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

(provide 'ende-skills-test)

;;; ende-skills-test.el ends here
