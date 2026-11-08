;;; ende-chat-context-test.el --- Chat context tests -*- lexical-binding: t; -*-

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

;; Coverage for context traversal, roles, visibility, and saved history.

;;; Code:

(require 'ert)
(require 'ende)
(require 'ende-test-helpers)
(require 'cl-lib)
(require 'ende-chat-test-helpers)

(ert-deftest ende-test/chat-document-properties-follow-org ()
  (with-temp-buffer
    (org-mode)
    (insert "* Root\n:PROPERTIES:\n:ENDE_ROLE: user\n:ENDE_CHAT_ID: root\n:END:\n"
            "** Child\n:PROPERTIES:\n:ENDE_CHAT_ID: child\n:ENDE_CHAT_ID+: suffix\n:END:\n"
            "*** Unset\n:PROPERTIES:\n:ENDE_CHAT_ID: nil\n:END:\n"
            "*** Inherited\nBody.\n")
    (let ((document (ende-chat--build-document)))
      (dolist (headline (ende-chat--document-headlines document))
        (goto-char (org-element-property :begin headline))
        (dolist (descriptor ende-chat--parsed-property-descriptors)
          (should (equal (ende-chat--property document headline (cdr descriptor))
                         (org-entry-get nil (car descriptor)))))))))



(defun ende-test--chat-unpartitioned-text (document question)
  "Return the selected Org content for QUESTION without message boundaries."
  (let* ((root (ende-chat--root document question))
         (root-begin (org-element-property :begin root))
         (root-end (org-element-property :end root))
         (question-begin (org-element-property :begin question))
         (prefix
          (cl-remove-if-not
           (lambda (headline)
             (let ((begin (org-element-property :begin headline)))
               (and (>= begin root-begin)
                    (< begin question-begin)
                    (< begin root-end)
                    (ende-chat--eligible-p document headline))))
           (ende-chat--document-headlines document)))
         (current (ende-chat--current-headlines document question)))
    (concat (mapconcat (lambda (headline)
                         (ende-chat--section-text document headline))
                       prefix "")
            (mapconcat (lambda (headline)
                         (ende-chat--section-text document headline))
                       current ""))))



(ert-deftest ende-test/generated-title-preserves-serialized-question ()
  (ende-test--reset)
  (ende-test--with-buffer
   "* Chat\n** TODO [#A] :request:\nExplain how this parser works.\n"
   (org-mode)
   (ende-test--chat-goto "** TODO [#A] :request:")
   (let* ((document (ende-chat--build-document))
          (question (ende-chat--headline-at-point document))
          (before (ende-chat--serialize document question)))
     (ende-chat--set-generated-title question "Parser operation")
     (let* ((updated (ende-chat--build-document))
            (updated-question (ende-chat--headline-at-point updated))
            (after (ende-chat--serialize updated updated-question))
            (section (ende-chat--section-text updated updated-question)))
       (should (equal before after))
       (should (equal (org-element-property :raw-value updated-question)
                      "Parser operation"))
       (should (equal (ende-chat--property
                       updated updated-question :generated-title) "t"))
       (should-not (string-match-p "ENDE_CHAT_GENERATED_TITLE" (cdr (car after))))
       (should-not (string-match-p "Parser operation" (cdr (car after))))
       (should (string-match-p "Explain how this parser works\\." section))
       (ende-chat--set-headline-title updated-question "Renamed manually")
       (let* ((renamed (ende-chat--build-document))
              (renamed-question (ende-chat--headline-at-point renamed))
              (renamed-text
               (ende-test--chat-context-text
                (ende-chat--serialize renamed renamed-question))))
         (should-not (string-match-p "Renamed manually" renamed-text)))))))

(ert-deftest ende-test/reserved-chat-drawers-are-excluded-from-context ()
  (ende-test--reset)
  (ende-test--with-buffer
   (concat "* Topic\n"
           "** User note\n"
           ":PROPERTIES:\n:ENDE_ROLE: user\n:END:\n"
           ":ENDE_CHAT_PRIVATE:\nnot ordinary context\n:END:\n"
           "Keep this ordinary note.\n"
           "** Follow-up\nAsk\n")
   (org-mode)
   (ende-test--chat-goto "** Follow-up")
   (let* ((messages (ende-chat-context-at-point))
          (text (ende-test--chat-context-text messages)))
     (should (string-match-p "Keep this ordinary note" text))
     (should-not (string-match-p "not ordinary context" text))
     (should-not (ende-test--chat-protocol-messages messages)))))



(ert-deftest ende-test/tool-store-setting-is-host-only-context ()
  (with-temp-buffer
    (org-mode)
    (setq buffer-file-name (expand-file-name "chat.org" temporary-file-directory))
    (insert "* Question\n\nSurrounding user text.\n\n"
            "#+ENDE_TOOL_STORE_FILE: private.sqlite3\n\n"
            "What does this code do?\n")
    (ende-mode 1)
    (should (equal (ende-tool-store--filename)
                   (expand-file-name "private.sqlite3" temporary-file-directory)))
    (ende-test--chat-goto "* Question")
    (let ((text (cdr (assoc "user" (ende-chat-context-at-point)))))
      (should (string-match-p "Surrounding user text" text))
      (should (string-match-p "What does this code do?" text))
      (should-not (string-match-p "ENDE_TOOL_STORE_FILE" text))
      (should-not (string-match-p "private.sqlite3" text)))
    (let ((literal-text
           (concat "Mention #+ENDE_TOOL_STORE_FILE: inline.sqlite3 in prose.\n"
                   "#+begin_src org\n#+ENDE_TOOL_STORE_FILE: source.sqlite3\n#+end_src\n"
                   "#+begin_example\n#+ENDE_TOOL_STORE_FILE: example.sqlite3\n#+end_example\n")))
      (goto-char (point-max))
      (insert literal-text)
      (ende-test--chat-goto "* Question")
      (let ((text (cdr (assoc "user" (ende-chat-context-at-point)))))
        (should (string-suffix-p literal-text text))
        (should-not (string-match-p "private.sqlite3" text))))))

(ert-deftest ende-test/chat-reasoning-ancestors-follow-document-order ()
  (with-temp-buffer
    (org-mode)
    (insert (concat
             "* Root\n"
             "** Outer reasoning\n"
             ":PROPERTIES:\n:ENDE_VISIBILITY: reasoning\n:END:\n"
             "Outer context.\n"
             "*** Inner reasoning\n"
             ":PROPERTIES:\n:ENDE_VISIBILITY: reasoning\n:END:\n"
             "Inner context.\n"
             "**** Question\nAsk.\n"))
    (let* ((document (ende-chat--build-document))
           (question (car (last (ende-chat--document-headlines document))))
           (ancestors (ende-chat--reasoning-ancestors document question)))
      (should (equal (mapcar (lambda (headline)
                               (org-element-property :raw-value headline))
                             ancestors)
                     '("Outer reasoning" "Inner reasoning"))))))



(ert-deftest ende-test/chat-context-isolates-level-one-roots ()
  (ende-test--reset)
  (ende-test--with-buffer
   (concat "* First root\n"
           "First root material\n"
           "** First question\n"
           "First question\n"
           "* Second root\n"
           "Second root material\n"
           "** Second question\n"
           "Second question\n")
   (org-mode)
   (ende-test--chat-goto "** Second question")
   (let* ((messages (ende-chat-context-at-point))
          (text (ende-test--chat-context-text messages)))
     (should (string-match-p "Second root material" text))
     (should (string-match-p "Second question" text))
     (should-not (string-match-p "First root material" text))
     (should-not (string-match-p "First question" text)))))



(ert-deftest ende-test/chat-hidden-and-reasoning-obey-independent-policies ()
  (ende-test--reset)
  (ende-test--with-buffer
   (concat "* Root\n"
           "** Assistant material\n"
           ":PROPERTIES:\n"
           ":ENDE_ROLE: assistant\n"
           ":END:\n"
           "Answer\n"
           "*** Hidden assistant descendant\n"
           ":PROPERTIES:\n"
           ":ENDE_VISIBILITY: hidden\n"
           ":END:\n"
           "Never send this\n"
           "*** Reasoning section\n"
           ":PROPERTIES:\n"
           ":ENDE_VISIBILITY: reasoning\n"
           ":END:\n"
           "Section thought\n"
           "**** Hidden reasoning descendant\n"
           ":PROPERTIES:\n"
           ":ENDE_VISIBILITY: hidden\n"
           ":END:\n"
           "Still never send this\n"
           "** Question\n"
           "Ask\n")
   (org-mode)
   (ende-test--chat-goto "** Question")
   (let ((without-reasoning
          (ende-test--chat-context-text
           (let ((ende-chat-include-reasoning nil))
             (ende-chat-context-at-point)))))
     (should (string-match-p "Answer" without-reasoning))
     (should-not (string-match-p "Section thought" without-reasoning))
     (should-not (string-match-p "Never send this" without-reasoning)))
   (let ((with-reasoning
          (ende-test--chat-context-text
           (let ((ende-chat-include-reasoning t))
             (ende-chat-context-at-point)))))
     (should (string-match-p "Section thought" with-reasoning))
     (should-not (string-match-p "Never send this" with-reasoning)))))



(ert-deftest ende-test/chat-role-inheritance-is-overridden-explicitly ()
  (ende-test--reset)
  (ende-test--with-buffer
   (concat "* Root\n"
           "** Assistant section\n"
           ":PROPERTIES:\n"
           ":ENDE_ROLE: assistant\n"
           ":END:\n"
           "Assistant answer\n"
           "*** Inherited child\n"
           "Inherited assistant text\n"
           "*** Explicit user child\n"
           ":PROPERTIES:\n"
           ":ENDE_ROLE: user\n"
           ":END:\n"
           "User reference text\n"
           "** Question\n"
           "Ask\n")
   (org-mode)
   (ende-test--chat-goto "** Question")
   (let* ((messages (ende-chat-context-at-point))
          (roles (mapcar #'car messages))
          (text (ende-test--chat-context-text messages)))
     (should (equal roles '("user" "assistant" "user" "user")))
     (should (string-match-p "Assistant answer" text))
     (should (string-match-p "Inherited assistant text" text))
     (should (string-match-p "User reference text" text))
     (should (string-match-p "Inherited assistant text"
                             (cdr (nth 1 messages))))
     (should (equal (car (car (last messages))) "user")))))



(ert-deftest ende-test/chat-prefix-messages-follow-explicit-role-boundaries ()
  (ende-test--reset)
  (ende-test--with-buffer
   (concat "* Topic\n"
           "Root source.\n"
           "** Reference\n"
           "Reference source.\n"
           "*** Nested reference\n"
           "Nested reference source.\n"
           "** Assistant response\n"
           ":PROPERTIES:\n"
           ":ENDE_ROLE: assistant\n"
           ":END:\n"
           "Assistant source.\n"
           "*** Inherited assistant section\n"
           "Inherited assistant source.\n"
           "**** Deeper inherited section\n"
           "Deeper assistant source.\n"
           "*** Explicit user section\n"
           ":PROPERTIES:\n"
           ":ENDE_ROLE: user\n"
           ":END:\n"
           "Nested user source.\n"
           "**** Inherited user section\n"
           "Inherited user source.\n"
           "*** Resumed assistant section\n"
           "Resumed assistant source.\n"
           "*** Explicit same-role section\n"
           ":PROPERTIES:\n"
           ":ENDE_ROLE: assistant\n"
           ":END:\n"
           "Same-role boundary source.\n"
           "**** Inherited from same-role section\n"
           "Same-role descendant source.\n"
           "*** Resumed after same-role section\n"
           "Assistant after nested section source.\n"
           "** Question\n"
           "Current question source.\n"
           "** Later sibling\n"
           "Later sibling source.\n")
   (org-mode)
   (ende-test--chat-goto "** Question")
   (let* ((document (ende-chat--build-document))
          (question (ende-chat--headline-at-point document))
          (messages (ende-chat--serialize document question))
          (roles (mapcar #'car messages))
          (contents (mapcar #'cdr messages))
          (serialized-text (mapconcat #'identity contents "")))
     (should (equal roles
                    '("user" "assistant" "user" "assistant"
                      "assistant" "assistant" "user")))
     (should (equal serialized-text
                    (ende-test--chat-unpartitioned-text document question)))
     (should (string-match-p "Root source" (nth 0 contents)))
     (should (string-match-p "Nested reference source" (nth 0 contents)))
     (should-not (string-match-p "Assistant source" (nth 0 contents)))
     (should (string-match-p "Inherited assistant source" (nth 1 contents)))
     (should (string-match-p "Deeper assistant source" (nth 1 contents)))
     (should-not (string-match-p "Nested user source" (nth 1 contents)))
     (should (string-match-p "Nested user source" (nth 2 contents)))
     (should (string-match-p "Inherited user source" (nth 2 contents)))
     (should (string-match-p "Resumed assistant source" (nth 3 contents)))
     (should (string-match-p "Same-role boundary source" (nth 4 contents)))
     (should (string-match-p "Same-role descendant source" (nth 4 contents)))
     (should (string-match-p "Assistant after nested section source"
                             (nth 5 contents)))
     (should (string-match-p "Current question source" (nth 6 contents)))
     (should-not (string-match-p "Later sibling source" serialized-text)))))



(ert-deftest ende-test/chat-current-question-skips-independent-child-subtrees ()
  (ende-test--reset)
  (ende-test--with-buffer
   (concat "* Root\n"
           "** Question\n"
           "Question body\n"
           "*** Unmarked reference\n"
           "Include this reference\n"
           "*** Explicit user child\n"
           ":PROPERTIES:\n"
           ":ENDE_ROLE: user\n"
           ":END:\n"
           "Do not include this turn\n"
           "*** Assistant child\n"
           ":PROPERTIES:\n"
           ":ENDE_ROLE: assistant\n"
           ":END:\n"
           "Do not include this answer\n"
           "*** Hidden child\n"
           ":PROPERTIES:\n"
           ":ENDE_VISIBILITY: hidden\n"
           ":END:\n"
           "Do not include this private note\n"
           "*** Independent fence\n"
           ":PROPERTIES:\n:ENDE_ROLE: assistant\n"
           ":ENDE_CHAT_CONTEXT_FENCE: t\n:END:\n"
           "Do not include this independent summary\n"
           "*** Later unmarked reference\n"
           "Include this later reference\n"
           "** Later sibling\n"
           "Must not enter this request\n")
   (org-mode)
   (ende-test--chat-goto "** Question")
   (let ((text (ende-test--chat-context-text
                (ende-chat-context-at-point))))
     (should (string-match-p "Question body" text))
     (should (string-match-p "Include this reference" text))
     (should (string-match-p "Include this later reference" text))
     (should-not (string-match-p "Do not include this turn" text))
     (should-not (string-match-p "Do not include this answer" text))
     (should-not (string-match-p "Do not include this private note" text))
     (should-not (string-match-p "Do not include this independent summary" text))
     (should-not (string-match-p "Must not enter this request" text)))))



(ert-deftest ende-test/chat-root-prefix-policy-can-be-narrowed ()
  (ende-test--reset)
  (ende-test--with-buffer
   (concat "* Root\n"
           "Earlier material\n"
           "** Question\n"
           "Current material\n")
   (org-mode)
   (ende-test--chat-goto "** Question")
   (let ((root-prefix (ende-test--chat-context-text
		       (let ((ende-chat-context-policy 'root-prefix))
                         (ende-chat-context-at-point))))
         (current-only (ende-test--chat-context-text
                        (let ((ende-chat-context-policy
			       'current-section-only))
                          (ende-chat-context-at-point)))))
     (should (string-match-p "Earlier material" root-prefix))
     (should-not (string-match-p "Earlier material" current-only))
     (should (string-match-p "Current material" current-only)))))



(ert-deftest ende-test/chat-context-fences-substitute-history-by-scope ()
  (ende-test--reset)
  (let ((ende-chat-context-policy 'root-prefix)
        (ende-chat-include-reasoning nil))
    (ende-test--with-buffer
     (concat "* Q0\nOriginal root context\n"
	     "** R1\n:PROPERTIES:\n:ENDE_ROLE: assistant\n:END:\n"
	     "** C0\n:PROPERTIES:\n:ENDE_ROLE: assistant\n"
	     ":ENDE_CHAT_CONTEXT_FENCE: t\n:END:\n"
	     "** Q01\n:PROPERTIES:\n:ENDE_ROLE: user\n:END:\n"
	     "** R01\n:PROPERTIES:\n:ENDE_ROLE: assistant\n:END:\n"
	     "*** Q11\n:PROPERTIES:\n:ENDE_ROLE: user\n:END:\n"
	     "*** R11\n:PROPERTIES:\n:ENDE_ROLE: assistant\n:END:\n"
	     "*** C1\n:PROPERTIES:\n:ENDE_ROLE: assistant\n"
	     ":ENDE_CHAT_CONTEXT_FENCE: t\n:END:\n"
	     "**** Summary detail\nRemember this detail\n"
	     "*** Instructions\n:PROPERTIES:\n"
	     ":ENDE_VISIBILITY: hidden\n:END:\nNever send this\n"
	     "*** Q1\n:PROPERTIES:\n:ENDE_ROLE: user\n:END:\n"
	     "**** Reference\nCurrent question reference\n"
	     "*** R12\n:PROPERTIES:\n:ENDE_ROLE: assistant\n:END:\n"
	     "** Q2\n:PROPERTIES:\n:ENDE_ROLE: user\n:END:\n"
	     "** R02\n:PROPERTIES:\n:ENDE_ROLE: assistant\n:END:\n"
	     "*** Q21\n:PROPERTIES:\n:ENDE_ROLE: user\n:END:\n"
	     "**** Q211\n:PROPERTIES:\n:ENDE_ROLE: user\n:END:\n"
	     "** C2\n:PROPERTIES:\n:ENDE_ROLE: assistant\n"
	     ":ENDE_CHAT_CONTEXT_FENCE: t\n:END:\n"
	     "** Q3\n:PROPERTIES:\n:ENDE_ROLE: user\n:END:\n"
	     "* Other\n** Q4\n")
     (org-mode)
     ;; These queries exercise a local fence, an ancestor fence, an earlier
     ;; branch at the same depth, later fences, and an independent root.
     (dolist (case
		 '(("*** Q11" "C0" "Q01" "R01" "Q11")
                   ("*** Q1" "C1" "Summary detail" "Q1" "Reference")
                   ("** Q2" "C0" "Q01" "R01" "C1" "Summary detail"
                    "Q1" "Reference" "R12" "Q2")
                   ("*** Q21" "C0" "Q01" "R01" "C1" "Summary detail"
                    "Q1" "Reference" "R12" "Q2" "R02" "Q21")
                   ("**** Q211" "C0" "Q01" "R01" "C1" "Summary detail"
                    "Q1" "Reference" "R12" "Q2" "R02" "Q21" "Q211")
                   ("** Q3" "C2" "Q3")
                   ("** Q4" "Other" "Q4")))
       (ende-test--chat-goto (car case))
       (let* ((messages (ende-chat-context-at-point))
              (text (ende-test--chat-context-text messages)))
         (ert-info ((format "Context at %s" (car case)))
           (should (equal (ende-test--chat-context-headings messages)
                          (cdr case)))
           (should (equal (caar (last messages)) "user"))
           (should-not (string-match-p "Original root context" text))
           (should-not (string-match-p "Never send this" text))
           (should-not (string-match-p "ENDE_CHAT_CONTEXT_FENCE" text)))))
     ;; An unfinished result cannot replace history, even if it was marked.
     (dolist (state '("pending" "failed"))
       (ende-test--chat-goto "** C2")
       (org-entry-put nil ende-chat-state-property state)
       (org-entry-put nil ende-chat-visibility-property "hidden")
       (ende-test--chat-goto "** Q3")
       (should
        (equal (ende-test--chat-context-headings (ende-chat-context-at-point))
	       '("C0" "Q01" "R01" "C1" "Summary detail" "Q1" "Reference"
                 "R12" "Q2" "R02" "Q21" "Q211" "Q3"))))
     (let ((ende-chat-context-policy 'current-section-only))
       (ende-test--chat-goto "*** Q1")
       (should
        (equal (ende-test--chat-context-headings (ende-chat-context-at-point))
	       '("Q1" "Reference")))))))

(provide 'ende-chat-context-test)

;;; ende-chat-context-test.el ends here
