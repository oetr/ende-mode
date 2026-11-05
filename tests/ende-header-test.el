;;; ende-header-test.el --- Release header launcher tests -*- lexical-binding: t; -*-

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

;; Standalone regression tests for add-header.sh.  Every launcher invocation
;; except generation of this file's own header uses a disposable script copy
;; and static fixtures; no Git history or Ende runtime modules are required.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'lisp-mnt)
(require 'subr-x)

(defconst ende-header-test--script
  (expand-file-name "../add-header.sh"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Source launcher copied into disposable roots by these tests.")

(defconst ende-header-test--emacs
  (expand-file-name invocation-name invocation-directory)
  "Current Emacs executable, passed to the launcher through EMACS.")

(defconst ende-header-test--license
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
   ";; along with Ende.  If not, see <https://www.gnu.org/licenses/>.\n")
  "Expected complete release notice, independent of the launcher internals.")

(defmacro ende-header-test--with-root (&rest body)
  "Run BODY with DIRECTORY, ROOT and SCRIPT bound to temporary paths.
ROOT and SCRIPT contain spaces; the current directory is outside ROOT."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "ende-header-test-" t))
          (root (expand-file-name "release tree" directory))
          (script (expand-file-name "add-header.sh" root))
          (default-directory (file-name-as-directory directory)))
     (unwind-protect
         (progn
           (make-directory root)
           (copy-file ende-header-test--script script)
           ,@body)
       (delete-directory directory t))))

(defun ende-header-test--write (root name text &optional coding)
  "Write TEXT to NAME under ROOT with CODING, defaulting to UTF-8 Unix."
  (let ((file (expand-file-name name root))
        (coding-system-for-write (or coding 'utf-8-unix)))
    (make-directory (file-name-directory file) t)
    (with-temp-buffer
      (insert text)
      (write-region (point-min) (point-max) file nil 'silent))
    file))

(defun ende-header-test--fixture (root name &optional metadata body)
  "Write a sparse fixture NAME under ROOT with METADATA and BODY."
  (ende-header-test--write
   root name
   (concat ";;; " (file-name-nondirectory name)
           " --- A header fixture -*- lexical-binding: t; -*-\n"
           metadata "\n;;; Code:\n\n"
           (or body "(defvar header-fixture-value 1)\n"))))

(defun ende-header-test--text (file)
  "Return decoded FILE contents without visiting it."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun ende-header-test--bytes (file)
  "Return literal FILE bytes without decoding."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun ende-header-test--snapshot (file)
  "Capture FILE's bytes, permissions, modification time and inode."
  (let ((attributes (file-attributes file)))
    (list (ende-header-test--bytes file)
          (file-modes file)
          (file-attribute-modification-time attributes)
          (file-attribute-inode-number attributes))))

(defun ende-header-test--forms (text)
  "Read exact top-level form spans in TEXT without evaluating any form."
  (with-temp-buffer
    (insert text)
    (emacs-lisp-mode)
    (goto-char (point-min))
    (let (forms)
      (forward-comment (point-max))
      (while (< (point) (point-max))
        (let ((start (point)))
          (read (current-buffer))
          (push (buffer-substring-no-properties start (point)) forms))
        (forward-comment (point-max)))
      (nreverse forms))))

(defun ende-header-test--count (needle text)
  "Count literal NEEDLE occurrences in TEXT."
  (let ((start 0) (count 0))
    (while (string-match (regexp-quote needle) text start)
      (setq count (1+ count) start (match-end 0)))
    count))

(defun ende-header-test--run (script &rest arguments)
  "Run SCRIPT with ARGUMENTS, returning (STATUS . COMBINED-OUTPUT)."
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "EMACS" ende-header-test--emacs)
    (with-temp-buffer
      (let ((status (apply #'call-process "sh" nil '(t t) nil
                           script arguments)))
        (cons status (buffer-string))))))

(defun ende-header-test--expect (status script &rest arguments)
  "Assert SCRIPT with ARGUMENTS exits STATUS, and return its output."
  (let ((result (apply #'ende-header-test--run script arguments)))
    (ert-info ((format "add-header.sh %S returned %S:\n%s"
                       arguments (car result) (cdr result)))
      (should (equal status (car result))))
    (cdr result)))

(defun ende-header-test--assert-header (file first-line &optional years keywords)
  "Check FILE's normalized header, FIRST-LINE, YEARS and KEYWORDS."
  (let ((text (ende-header-test--text file)))
    (should (string-prefix-p (concat first-line "\n") text))
    (should (string-match-p
             (regexp-quote (concat ";; Copyright (C) " (or years "2026")
                                  " Peter Samarin\n")) text))
    (should (= 1 (ende-header-test--count ende-header-test--license text)))
    (should (= 1 (ende-header-test--count
                  ";; SPDX-License-Identifier: AGPL-3.0-only\n" text)))
    (should (string-match-p
             (regexp-quote (concat ";; Keywords: "
                                  (or keywords "convenience, tools") "\n"))
             text))
    (should (string-suffix-p
             (concat ";;; " (file-name-nondirectory file) " ends here\n") text))
    (with-temp-buffer
      (insert text)
      (should (equal (lm-authors)
                     '(("Peter Samarin" . "peter.samarin@gmail.com"))))
      (should (equal (lm-maintainers) (lm-authors)))
      (should (equal (lm-header "url") "https://github.com/oetr/ende-mode")))))

(ert-deftest ende-header-test-legacy-metadata-and-commentary-are-preserved ()
  (ende-header-test--with-root
    (let* ((first-line
            ";;; legacy.el --- Legacy utility -*- lexical-binding: nil; -*-")
           (version ";; Version: 0.3\n;;   development snapshot\n")
           (requires (concat ";; Package-Requires: ((emacs \"30.1\")\n"
                             ";;                    (websocket \"1.12\"))\n"))
           (unknown ";; X-Ende-Note: keep this\n;;   and this: verbatim\n")
           (commentary
            (concat ";; Original commentary: do not reflow.\n;;\n"
                    ";;     Indented café example (one two).\n"
                    ";; Author: Example Person (documentation, not metadata).\n"
                    ";; SPDX-License-Identifier: example text, not metadata.\n"))
           (text (concat first-line "\n\n"
                         ";; Copyright (C) 2022-2025 Peter\n"
                         ";; Author: Peter <obsolete@example.org>\n"
                         ";; Maintainer: Peter\n" version requires unknown
                         ";; Keywords: obsolete\n"
                         ";; URL: https://example.org/obsolete\n\n"
                         ";;; Commentary:\n\n" commentary "\n;;; Code:\n\n"
                         "(defvar legacy-value\n  '((one . \"opaque\")))\n"
                         "(provide 'legacy)\n\n;;; legacy.el ends here\n"))
           (file (ende-header-test--write root "legacy.el" text)))
      (ende-header-test--expect 0 script file)
      (ende-header-test--assert-header file first-line "2022-2025")
      (let ((result (ende-header-test--text file)))
        (dolist (preserved (list version requires unknown commentary))
          (should (string-match-p (regexp-quote preserved) result)))
        (should-not (string-match-p "obsolete@example\\.org" result))
        (should-not (string-match-p "https://example\\.org/obsolete" result))
        (should (equal (ende-header-test--forms text)
                       (ende-header-test--forms result)))))))

(ert-deftest ende-header-test-sparse-header-matrix ()
  (ende-header-test--with-root
    (let ((first-line
           ";;; sparse.el --- Small utility -*- lexical-binding: t; -*-")
          cases)
      (cl-loop
       for tail in
       '(""
         "\n(defvar sparse-value 1)\n"
         "\n;;; Code:\n\n(defvar sparse-value 2)\n"
         "\n;;; Commentary:\n\n;; Keep this description.\n\n(defvar sparse-value 3)\n"
         "\n;;; Commentary:\n\n;;; Code:\n\n(defvar sparse-value 4)\n"
         "\n;;;###autoload\n(defun sparse-command () (interactive))\n"
         "\n;;; Code:\n\n(provide 'sparse)"
         "\n;;; Code:\n\n(defvar sparse-value 5)\n;;; old.el ends here\n;;; sparse.el ends here\n;;; stale.el ends here\n")
       for index from 0
       do
       (let* ((text (concat first-line "\n" tail))
              (file (ende-header-test--write
                     root (format "case-%d/sparse.el" index) text)))
         (push (list :index index :text text :file file) cases)))
      (setq cases (nreverse cases))
      (apply #'ende-header-test--expect 0 script
             (mapcar (lambda (case) (plist-get case :file)) cases))
      (dolist (case cases)
        (ert-info ((format "Sparse fixture %d" (plist-get case :index)))
          (let ((text (plist-get case :text))
                (file (plist-get case :file)))
           (ende-header-test--assert-header file first-line)
           (let ((result (ende-header-test--text file)))
             (should (= 1 (ende-header-test--count ";;; Commentary:\n" result)))
             (should (= 1 (ende-header-test--count ";;; Code:\n" result)))
             (should (= 1 (ende-header-test--count " ends here" result)))
             (should (equal (ende-header-test--forms text)
                            (ende-header-test--forms result)))
             (when (string-match-p ";;;###autoload" text)
               (should (string-match-p
                        (regexp-quote ";;;###autoload\n(defun sparse-command")
                        result)))
             (unless (string-match-p ";;; Commentary:" text)
               (should (string-match-p
                        (regexp-quote ";; Small utility.\n") result))))))))))

(ert-deftest ende-header-test-unicode-crlf-and-opaque-form-spans ()
  (ende-header-test--with-root
    (let* ((first-line
            ";;; opaque.el --- Unicode fixture -*- coding: utf-8; lexical-binding: t; -*-")
           (text
            (concat first-line "\n\n;;; Commentary:\n\n;; Résumé λ 日本語.\n"
                    "\n;;; Code:\n\n"
                    "(defconst opaque-value\n"
                    "  \"café λ 日本語\n;;; Code:\n;;; Commentary:\n"
                    ";;; fake.el ends here\n;; Author: A string, not metadata\n\")\n"
                    "(defun opaque-result ()\n"
                    "  ;; The following marker is nested in a form.\n"
                    ";;; Code:\n"
                    "  (list opaque-value ?\\; '(#x2a . [one two])))\n"
                    "(error \"Fixture forms must never be evaluated\")\n"))
           (file (ende-header-test--write root "opaque.el" text 'utf-8-dos)))
      (ende-header-test--expect 0 script file)
      (ende-header-test--assert-header file first-line)
      (let ((result (ende-header-test--text file))
            (bytes (ende-header-test--bytes file)))
        (should (equal (ende-header-test--forms text)
                       (ende-header-test--forms result)))
        (should (= 3 (ende-header-test--count ";;; Code:\n" result)))
        (should (string-match-p "\r\n" bytes))
        (should-not (string-match-p "[^\r]\n" bytes))
        (dolist (form (ende-header-test--forms text))
          (should (string-match-p
                   (regexp-quote (encode-coding-string form 'utf-8-dos))
                   bytes)))))))

(ert-deftest ende-header-test-retains-non-utf8-source-encoding ()
  (ende-header-test--with-root
    (let* ((first-line
            ";;; latin.el --- Café -*- coding: iso-latin-1; lexical-binding: t; -*-")
           (text (concat first-line "\n\n;;; Code:\n\n"
                         "(defconst latin-value \"café déjà vu\")\n"))
           (file (ende-header-test--write root "latin.el" text 'iso-latin-1-unix)))
      (ende-header-test--expect 0 script file)
      (ende-header-test--assert-header file first-line)
      (let ((bytes (ende-header-test--bytes file)))
        (should (string-match-p (unibyte-string #xe9) bytes))
        (should-not (string-match-p (unibyte-string #xc3 #xa9) bytes)))
      (should (equal (ende-header-test--forms text)
                     (ende-header-test--forms (ende-header-test--text file)))))))

(ert-deftest ende-header-test-removes-duplicate-speech-style-code-marker ()
  (ende-header-test--with-root
    (let* ((text (concat ";;; speech.el --- Speech fixture -*- lexical-binding: t; -*-\n"
                         ";; Author: Peter\n\n;;; Commentary:\n\n"
                         ";; Speech commands.\n\n;;; Code:\n\n"
                         "(require 'cl-lib)\n\n;;; Code:\n\n"
                         "(defvar speech-fixture-value nil)\n"
                         "(provide 'speech)\n"))
           (file (ende-header-test--write root "speech.el" text)))
      (ende-header-test--expect 0 script file)
      (let ((result (ende-header-test--text file)))
        (should (= 1 (ende-header-test--count ";;; Code:\n" result)))
        (should (equal (ende-header-test--forms text)
                       (ende-header-test--forms result)))))))

(ert-deftest ende-header-test-idempotence-keeps-modes-mtime-and-inode ()
  (ende-header-test--with-root
    (let ((file (ende-header-test--fixture root "stable.el")))
      (set-file-modes file #o751)
      (ende-header-test--expect 0 script file)
      (should (= #o751 (file-modes file)))
      (set-file-times file (seconds-to-time 1000000000))
      (let ((before (ende-header-test--snapshot file)))
        (should (string-match-p
                 (regexp-quote "1 file(s) checked; 0 updated.")
                 (ende-header-test--expect 0 script file)))
        (should (equal before (ende-header-test--snapshot file)))
        (ende-header-test--expect 0 script "--check" file)
        (should (equal before (ende-header-test--snapshot file)))))))

(ert-deftest ende-header-test-check-statuses-and-no-writes ()
  (ende-header-test--with-root
    (let ((file (ende-header-test--fixture root "dirty.el")))
      (set-file-modes file #o640)
      (set-file-times file (seconds-to-time 1000000000))
      (let ((before (ende-header-test--snapshot file)))
        (should (string-match-p
                 (regexp-quote "1 file(s) checked; 1 need changes.")
                 (ende-header-test--expect 1 script "--check" file)))
        (should (equal before (ende-header-test--snapshot file))))
      (ende-header-test--expect 0 script file)
      (let ((before (ende-header-test--snapshot file)))
        (should (string-match-p
                 (regexp-quote "1 file(s) checked; 0 need changes.")
                 (ende-header-test--expect 0 script "--check" file)))
        (should (equal before (ende-header-test--snapshot file)))))))

(ert-deftest ende-header-test-default-recursion-is-relative-to-script ()
  (ende-header-test--with-root
    (let* ((top (ende-header-test--fixture root "top.el"))
           (nested (ende-header-test--fixture root "nested directory/child.el"))
           (test (ende-header-test--fixture root "tests/sample-test.el"))
           (outside (ende-header-test--fixture directory "outside.el"))
           (git (ende-header-test--write root ".git/invalid.el" "not Lisp ("))
           (nested-git (ende-header-test--write
                        root "nested directory/.git/invalid.el" "also invalid ("))
           (ignored (ende-header-test--write root "notes.txt" "do not change"))
           (untouched (mapcar (lambda (file)
                                (cons file (ende-header-test--snapshot file)))
                              (list outside git nested-git ignored))))
      (should (string-match-p
               (regexp-quote "3 file(s) checked; 3 updated.")
               (ende-header-test--expect 0 script)))
      (dolist (file (list top nested test))
        (ende-header-test--assert-header
         file (car (split-string (ende-header-test--text file) "\n"))
         nil (if (equal file test) "lisp, tools" "convenience, tools")))
      (dolist (entry untouched)
        (should (equal (cdr entry) (ende-header-test--snapshot (car entry)))))
      (ende-header-test--expect 0 script "--check"))))

(ert-deftest ende-header-test-spaces-and-leading-dash-paths ()
  (ende-header-test--with-root
    (let ((spaced (ende-header-test--fixture root "with spaces.el"))
          (dashed (ende-header-test--fixture root "-dash.el"))
          (default-directory (file-name-as-directory root)))
      (ende-header-test--expect 0 script "--" "with spaces.el" "-dash.el")
      (dolist (file (list spaced dashed))
        (ende-header-test--assert-header
         file (car (split-string (ende-header-test--text file) "\n"))))
      (ende-header-test--expect 0 script "--check" "--" "-dash.el"))))

(ert-deftest ende-header-test-overlapping-paths-are-deduplicated ()
  (ende-header-test--with-root
    (let* ((top (ende-header-test--fixture root "top.el"))
           (nested (ende-header-test--fixture root "nested/child.el"))
           (subdir (file-name-directory nested))
           (output (ende-header-test--expect
                    0 script root subdir top nested root)))
      (should (string-match-p
               (regexp-quote "2 file(s) checked; 2 updated.") output))
      (dolist (file (list top nested))
        (should (= 1 (ende-header-test--count
                      (concat "Updated " file "\n") output)))))))

(ert-deftest ende-header-test-recursion-skips-symlink-files-and-directories ()
  (ende-header-test--with-root
    (let* ((external (expand-file-name "external" directory))
           (target (ende-header-test--fixture external "untouched.el"))
           (regular (ende-header-test--fixture root "regular.el"))
           (link (expand-file-name "linked.el" root))
           (dirlink (expand-file-name "linked directory" root))
           (broken (expand-file-name "broken.el" root))
           (before (ende-header-test--snapshot target)))
      (skip-unless
       (condition-case nil
           (progn
             (make-symbolic-link target link)
             (make-symbolic-link external dirlink)
             (make-symbolic-link (expand-file-name "absent.el" external) broken)
             t)
         (file-error nil)))
      (should (string-match-p
               (regexp-quote "1 file(s) checked; 1 updated.")
               (ende-header-test--expect 0 script root)))
      (should (equal before (ende-header-test--snapshot target)))
      (dolist (file (list link dirlink broken))
        (should (file-symlink-p file)))
      (ende-header-test--expect 0 script "--check" regular))))

(ert-deftest ende-header-test-explicit-symlinks-are-refused-before-writes ()
  (ende-header-test--with-root
    (let* ((good (ende-header-test--fixture root "a-good.el"))
           (external (expand-file-name "external" directory))
           (target (ende-header-test--fixture external "target.el"))
           (link (expand-file-name "linked.el" root))
           (dirlink (expand-file-name "linked-directory" root))
           (broken (expand-file-name "broken.el" root))
           (before (ende-header-test--snapshot good))
           (target-before (ende-header-test--snapshot target)))
      (skip-unless
       (condition-case nil
           (progn
             (make-symbolic-link target link)
             (make-symbolic-link external dirlink)
             (make-symbolic-link (expand-file-name "absent.el" external) broken)
             t)
         (file-error nil)))
      (dolist (path (list link dirlink broken))
        (ende-header-test--expect 2 script good path)
        (should (equal before (ende-header-test--snapshot good)))
        (should (equal target-before (ende-header-test--snapshot target)))
        (should (file-symlink-p path))))))

(ert-deftest ende-header-test-invalid-arguments-do-not-write-good-file ()
  (ende-header-test--with-root
    (let* ((good (ende-header-test--fixture root "a-good.el"))
           (non-lisp (ende-header-test--write root "notes.txt" "not a target"))
           (missing (expand-file-name "absent.el" root))
           (before (ende-header-test--snapshot good)))
      (dolist (argument (list "--unknown-option" missing non-lisp))
        (ende-header-test--expect 2 script good argument)
        (should (equal before (ende-header-test--snapshot good))))
      (ende-header-test--expect 2 script "--check" good missing)
      (should (equal before (ende-header-test--snapshot good)))
      (should (string-match-p "Usage:" (ende-header-test--expect 0 script "--help")))
      (should (equal before (ende-header-test--snapshot good))))))

(ert-deftest ende-header-test-invalid-source-preflight-leaves-all-files-unchanged ()
  (ende-header-test--with-root
    (let* ((good (ende-header-test--fixture root "a-good.el"))
           (before (ende-header-test--snapshot good)))
      (dolist
          (case '(("malformed Lisp" nil "(defun unfinished (\n")
                  ("foreign author" ";; Author: Alice <alice@example.org>\n")
                  ("foreign maintainer" ";; Maintainer: Alice\n")
                  ("foreign copyright" ";; Copyright (C) 2020 Alice Example\n")
                  ("foreign SPDX" ";; SPDX-License-Identifier: MIT\n")
                  ("foreign license field" ";; License: GPL-3.0-only\n")
                  ("license continuation" ";; License: AGPL-3.0-only\n;;   or any later version.\n")
                  ("SPDX continuation" ";; SPDX-License-Identifier: AGPL-3.0-only\n;;   OR MIT\n")
                  ("maintainer continuation" ";; Maintainer: Peter\n;;   Alice <alice@example.org>\n")
                  ("unrecognized notice" ";; Released under the MIT license.\n")
                  ("missing filename header" nil "(defvar valid-body 1)\n"
                   ";; No filename summary line")
                  ("mismatched filename header" nil "(defvar valid-body 1)\n"
                   ";;; wrong.el --- A mismatched filename")))
        (ert-info ((car case))
          (let* ((bad (ende-header-test--fixture
                       root "z-bad.el" (nth 1 case) (nth 2 case)))
                 (first-line (nth 3 case)))
            (when first-line
              (ende-header-test--write
               root "z-bad.el" (concat first-line "\n" (nth 2 case))))
            (let ((bad-before (ende-header-test--snapshot bad)))
              (ende-header-test--expect 2 script good bad)
              (should (equal before (ende-header-test--snapshot good)))
              (should (equal bad-before (ende-header-test--snapshot bad))))))))))

(ert-deftest ende-header-test-foreign-author-continuation-is-not-silently-dropped ()
  (ende-header-test--with-root
    (let* ((good (ende-header-test--fixture root "a-good.el"))
           (foreign (ende-header-test--fixture
                     root "z-foreign.el"
                     (concat ";; Author: Peter\n"
                             ";;         Alice Example <alice@example.org>\n")))
           (before (ende-header-test--snapshot good))
           (foreign-before (ende-header-test--snapshot foreign)))
      (ende-header-test--expect 2 script good foreign)
      (should (equal before (ende-header-test--snapshot good)))
      (should (equal foreign-before (ende-header-test--snapshot foreign))))))

(ert-deftest ende-header-test-conflicting-declarations-in-commentary-are-refused ()
  (ende-header-test--with-root
    (let* ((good (ende-header-test--fixture root "a-good.el"))
           (before (ende-header-test--snapshot good)))
      (dolist (notice '(";; SPDX-License-Identifier: MIT\n"
                        ";; Copyright (C) 2025 Alice Example\n"
                        ";; Author: Alice Example <alice@example.org>\n"
                        ";; License: AGPL-3.0-only\n;;   or any later version.\n"))
        (let* ((foreign (ende-header-test--write
                         root "z-foreign.el"
                         (concat ";;; z-foreign.el --- Conflicting notice -*- lexical-binding: t; -*-\n\n"
                                 ";;; Commentary:\n\n" notice
                                 "\n;;; Code:\n\n(defvar foreign-value 1)\n")))
               (foreign-before (ende-header-test--snapshot foreign)))
          (ende-header-test--expect 2 script good foreign)
          (should (equal before (ende-header-test--snapshot good)))
          (should (equal foreign-before (ende-header-test--snapshot foreign))))))))

(ert-deftest ende-header-test-explicit-git-internals-are-refused ()
  (ende-header-test--with-root
    (let* ((good (ende-header-test--fixture root "a-good.el"))
           (internal (ende-header-test--fixture root ".git/hooks/internal.el"))
           (before (ende-header-test--snapshot good))
           (internal-before (ende-header-test--snapshot internal)))
      (dolist (path (list internal (file-name-directory internal)
                          (expand-file-name ".git" root)))
        (ende-header-test--expect 2 script good path)
        (should (equal before (ende-header-test--snapshot good)))
        (should (equal internal-before (ende-header-test--snapshot internal)))))))

(ert-deftest ende-header-test-recursion-skips-nonregular-files ()
  (skip-unless (executable-find "mkfifo"))
  (ende-header-test--with-root
    (let ((fifo (expand-file-name "pipe.el" root))
          (regular (ende-header-test--fixture root "regular.el")))
      (should (zerop (call-process "mkfifo" nil nil nil fifo)))
      ;; A regression must fail promptly rather than block opening the FIFO.
      (let ((process-environment (copy-sequence process-environment)) process)
        (setenv "EMACS" ende-header-test--emacs)
        (with-temp-buffer
          (unwind-protect
              (progn
                (setq process (make-process :name "ende-header-fifo-test"
                                            :buffer (current-buffer)
                                            :command (list "sh" script root)
                                            :connection-type 'pipe :noquery t))
                (let ((deadline (+ (float-time) 10)))
                  (while (and (process-live-p process) (< (float-time) deadline))
                    (accept-process-output process 0.05)))
                (should-not (process-live-p process))
                (should (zerop (process-exit-status process))))
            (when (and process (process-live-p process)) (delete-process process)))))
      (ende-header-test--assert-header
       regular (car (split-string (ende-header-test--text regular) "\n")))
      (should-not (file-regular-p fifo)))))

(provide 'ende-header-test)
;;; ende-header-test.el ends here
