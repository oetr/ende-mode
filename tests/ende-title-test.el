;;; ende-title-test.el --- Title protocol integration -*- lexical-binding: t; -*-

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

;; Integration tests for the title protocol.

;;; Code:

(require 'ende-chat-test-helpers)

(ert-deftest ende-test/title-tool-is-internal-and-phase-gated ()
  (should (member "generate_title"
                  (ende-test--tool-definition-names (ende-tool-definitions))))
  (dolist (forms '(((tools generate_title))
                   ((group titles generate_title) (tools titles))
                   ((agent-config worker (tools generate_title))
                    (spawn-agent-config worker))))
    (with-temp-buffer
      (org-mode)
      (insert (ende-test--chat-config-block forms) "* Task\nWork.\n")
      (let ((document (ende-chat--build-document)))
        (should-error
         (ende-chat-config-resolve-environment
          (ende-chat--document-tree document)
          (ende-chat--document-parent document)
          (car (ende-chat--document-headlines document)))))))
  (let ((title nil)
        (ende-tools--request-phase 'title))
    (let ((ende-tools--generate-title-function
           (lambda (value) (setq title value))))
      (should-not (plist-get (ende-tool-call 'about_thinking
                                            '(:title "Work" :status "Work.")) :ok))
      (dolist (value '("" "  " "two\nlines" "* Heading" "\"Quoted\""
                       "'Quoted'" "/Italic/" "_Underlined_" "+Struck+"))
        (should-not (plist-get (ende-tool-call 'generate_title
                                              (list :title value)) :ok)))
      (should (plist-get (ende-tool-call 'generate_title
                                        '(:title "Inspect queue lifecycle")
                                        "/not/a/required/project"
                                        '(:active-tools nil :root "/missing")) :ok))
      (should (equal title "Inspect queue lifecycle"))))
  (let* ((title nil)
         (ende-tools--request-phase 'work)
         (ende-tools--generate-title-function
          (lambda (value) (setq title value))))
    (let ((result (ende-tool-call 'generate_title
                                  '(:title "Forbidden in work"))))
      (should-not (plist-get result :ok))
      (should (equal (plist-get result :error)
                     "title generation was not requested"))
      (should-not title))))

(ert-deftest ende-test/title-phase-isolates-a-whole-batch-and-continues ()
  (ende-test--reset)
  (let* ((ende-test--title-prelude t)
         (requests nil) (effects 0)
         (ende-tools--registry (copy-sequence ende-tools--registry))
         (ende-llm-function
          (lambda (messages callbacks &optional profile)
            (push (cons messages profile) requests)
            (funcall
             (plist-get callbacks :on-done)
             (if (= (length requests) 1)
                 '(:content "Title-only prose" :tool-calls
                   ((:id "title" :name "generate_title"
                     :arguments "{\"title\":\"Inspect queue lifecycle\"}")
                    (:id "spawn" :name "spawn_agent" :arguments "{\"title\":\"Forbidden\",\"task\":\"Forbidden\"}")
                    (:id "effect" :name "title_test_effect" :arguments "{}")))
               (if (= (length requests) 2)
                   '(:content "" :tool-calls
                     ((:id "title-work" :name "generate_title"
                       :arguments "{\"title\":\"Unauthorized rewrite\"}")
                      (:id "effect-work" :name "title_test_effect" :arguments "{}")))
                 '(:content "Actual answer")))))))
    (ende-tool-register 'title_test_effect "Test effect" '((:type . "object"))
                        (lambda (&rest _) (cl-incf effects)) nil :no-resource
                        '(:requires-root nil))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert (ende-test--chat-config-block '((tools title_test_effect)))
                  "* Root\n** \nInspect the queue.\n** Next\nContinue.\n")
          (ende-test--chat-goto "** ")
          (let ((job (ende-ask)))
            (should (eq (ende-job-state job) 'completed))
            (should (equal (ende-job-title job) "Inspect queue lifecycle"))
            (should (= effects 1))
            (should-not (ende-job-children job))
            (should (= (length requests) 3))
            (let* ((ordered (reverse requests))
                   (first (car ordered)) (second (cadr ordered))
                   (third (caddr ordered))
                   (title-request (car first))
                   (work-request (car second)))
              (should (equal (cdr (car (last title-request)))
                             ende-agent--title-control))
              (should (equal (butlast title-request) work-request))
              (should (string-match-p "Inspect the queue."
                                      (ende-test--chat-context-text work-request)))
              (should (member "generate_title"
                              (ende-test--tool-definition-names
                               (plist-get (cdr first) :tool-definitions))))
              (should (member "generate_title"
                              (ende-test--tool-definition-names
                               (plist-get (cdr second) :tool-definitions))))
              (should (equal (plist-get (cdr first) :tool-definitions)
                             (plist-get (cdr second) :tool-definitions)))
              (should (equal (plist-get (cdr first) :tool-definitions)
                             (plist-get (cdr third) :tool-definitions))))
            (let* ((work-messages (car (caddr (reverse requests))))
                   (assistant
                    (cl-find-if
                     (lambda (message)
                       (and (equal (plist-get message :role) "assistant")
                            (plist-get message :tool-calls)))
                     work-messages))
                   (tool-result
                    (cl-find-if (lambda (message)
                                  (and (equal (plist-get message :role) "tool")
                                       (equal (plist-get message :tool-call-id)
                                              "title-work")))
                                work-messages)))
              (should (equal (mapcar (lambda (call)
                                       (ende-tools-call-value call :name))
                                     (plist-get assistant :tool-calls))
                             '("generate_title" "title_test_effect")))
              (should tool-result)
              (let ((rejection (ende-protocol-json-read-string
                                (plist-get tool-result :content))))
                (should (eq (ende-protocol-json-get "ok" rejection) :false))
                (should (equal (ende-protocol-json-get "error" rejection)
                               "title generation was not requested"))))
            (should (string-match-p "Actual answer" (buffer-string)))
            (should-not (string-match-p "Title-only prose" (buffer-string)))
            (should-not (string-match-p "Ende temporary title phase"
                                        (buffer-string)))
            (should-not (string-match-p "spawn_agent" (buffer-string)))
            (should (string-match-p "\\*\\* Inspect queue lifecycle"
                                    (buffer-string)))
            (should (string-match-p "ENDE_CHAT_GENERATED_TITLE: t" (buffer-string)))
            (ende-test--chat-goto "** Next")
            (let ((context (ende-test--chat-context-text
                            (ende-chat-context-at-point))))
              (should (string-match-p "title generation was not requested" context))
              (should-not (string-match-p "Title-only prose" context))
              (should-not (string-match-p "Ende temporary title phase" context))
              (should-not (string-match-p "spawn_agent" context))
              (should (string-match-p "title_test_effect" context))
              (should-not (string-match-p "\\*\\* Inspect queue lifecycle"
                                          context)))))
      (ende-test--reset))))

(ert-deftest ende-test/title-correction-is-bounded-and-keeps-the-local-fallback ()
  (ende-test--reset)
  (let* ((ende-test--title-prelude t)
         (phases nil) (requests nil)
         (ende-llm-function
          (lambda (messages callbacks &optional profile)
            (push messages requests)
            (let ((job (ende-queue-live-job-by-id (plist-get profile :job-id))))
              (push (ende-job-request-phase job) phases))
            (funcall (plist-get callbacks :on-done)
                     (if (< (length phases) 3)
                         '(:content "Do not use this as the answer")
                       '(:content "Actual final answer"))))))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "* User heading\nDo the real task.\n")
          (goto-char (point-min))
          (let ((job (ende-ask)))
            (should (equal (nreverse phases) '(title title work)))
            (let* ((ordered (nreverse requests))
                   (first (car ordered))
                   (retry (cadr ordered))
                   (work (caddr ordered)))
              (should (equal (butlast first) work))
              (should (string-match-p "Do not use this as the answer"
                                      (ende-test--chat-context-text retry)))
              (should (string-match-p "Title correction"
                                      (ende-test--chat-context-text retry)))
              (should-not (string-match-p "temporary title phase"
                                          (ende-test--chat-context-text work))))
            (should (equal (ende-job-title job) "User heading"))
            (should (eq (ende-job-state job) 'completed))
            (should (string-match-p "Actual final answer" (buffer-string)))))
      (ende-test--reset))))

(ert-deftest ende-test/delegated-jobs-use-supplied-titles-without-a-prelude ()
  (ende-test--reset)
  (let* ((ende-test--title-prelude t)
         (requests nil)
         (ende-llm-function
          (lambda (messages callbacks &optional profile)
            (setq requests (append requests (list (list messages callbacks profile))))))
         (title '(:content "" :tool-calls
                  ((:id "title" :name "generate_title"
                    :arguments "{\"title\":\"Stable generated title\"}")))))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert (ende-test--chat-config-block
                   '((tools spawn_agent) (agent-config worker) (spawn-agent-config worker)))
                  "* Root\nDelegate the task.\n")
          (ende-test--chat-goto "* Root")
          (let ((root (ende-ask (ende-test--request-profile
                                 '(:name remote :model "fixture" :parallel t :max-concurrency 2)))))
            (funcall (plist-get (nth 1 (nth 0 requests)) :on-done) title)
            (funcall (plist-get (nth 1 (nth 1 requests)) :on-done)
                     '(:content "" :tool-calls
                       ((:id "spawn" :name "spawn_agent" :arguments "{\"title\":\"Inspect child task\",\"task\":\"Child task\"}"))))
            (should (eq (ende-job-state root) 'suspended))
            (let* ((child (car (ende-job-children root)))
                   (root-tools
                    (ende-test--tool-definition-names
                     (plist-get (nth 2 (nth 1 requests)) :tool-definitions)))
                   (child-tools
                    (ende-test--tool-definition-names
                     (plist-get (nth 2 (nth 2 requests)) :tool-definitions))))
              (should (member "generate_title" root-tools))
              (should (equal child-tools (remove "generate_title" root-tools)))
              (should (eq (ende-job-request-phase child) 'work))
              (should (equal (mapcar #'car (car (nth 2 requests)))
                             '("system" "user")))
              (should (equal (cdr (assoc "user" (car (nth 2 requests))))
                             "Child task"))
              (should-not (ende-job-title-attempts child))
              (should (equal (ende-job-title child) "Inspect child task"))
              (funcall (plist-get (nth 1 (nth 2 requests)) :on-done) '(:content "Child answer"))
              (should (eq (ende-job-state child) 'completed))
              (should (eq (ende-job-state root) 'running))
              (should (= (ende-job-title-attempts root) 1))
              (should (eq (ende-job-request-phase root) 'work))
              (funcall (plist-get (nth 1 (nth 3 requests)) :on-done) '(:content "Parent answer"))
              (should (eq (ende-job-state root) 'completed))
              (should (equal (ende-job-title root) "Stable generated title"))
              (should (= (length requests) 4)))))
      (ende-test--reset))))

(provide 'ende-title-test)

;;; ende-title-test.el ends here
