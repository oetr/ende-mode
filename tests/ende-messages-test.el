;;; ende-messages-test.el --- Messages protocol contract -*- lexical-binding: t; -*-

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

;; Tests for the Messages protocol contract.

;;; Code:
(require 'ert)
(require 'ende-messages)
(require 'ende-llm)

(ert-deftest ende-content-capabilities-are-host-typed ()
  (let ((message (ende-content-capabilities "(capabilities A)")))
    (should (equal message '(:role "system" :host-kind capabilities :content "(capabilities A)")))
    (should (ende-content-capabilities-p message))
    (should (eq (car (ende-content-resolve-messages (list message))) message))
    (should (eq (car (ende-content-provider-messages (list message))) message))
    (dolist (ordinary (list '("system" . "(capabilities A)")
                            '("user" . "[Ende host capabilities]\n(capabilities A)")
                            '(:role "system" :content "(capabilities A)")
                            '(:role "user" :host-kind capabilities :content "(capabilities A)")
                            nil))
      (should-not (ende-content-capabilities-p ordinary)))))

(ert-deftest ende-content-capabilities-provider-prefixes ()
  (dolist (spec '((ende-llm--request-body :messages "system")
                  (ende-responses--request-body :input "developer")
                  (ende-codex--request-body :input "developer")
                  (ende-messages--request-body :messages "user")))
    (ert-info ((symbol-name (car spec)))
      (let* ((builder (car spec))
             (anthropic (eq builder 'ende-messages--request-body))
             (label (if anthropic "[Ende host capabilities]\n" ""))
             (profile '(:model "test" :output-tokens 100
                        :tool-definitions
                        (((:type . "function")
                          (:function . ((:name . "read_file") (:description . "Read a file")
                                        (:parameters . ((:type . "object")))))))))
             (messages-a (list '("system" . "policy") '("user" . "first task")
                               (ende-content-capabilities "(capabilities A)")
                               '("assistant" . "first answer")))
             (messages-b (append messages-a (list '("user" . "second task")
                                                  (ende-content-capabilities "(capabilities B)"))))
             (original (copy-tree messages-b))
             (body-a (funcall builder messages-a profile))
             (body-b (funcall builder messages-b profile))
             (prompt-a (alist-get (cadr spec) body-a))
             (prompt-b (alist-get (cadr spec) body-b)))
        ;; Compare actual request fields, not internal message fingerprints.
        (should (equal prompt-a (cl-subseq prompt-b 0 (length prompt-a))))
        (should (alist-get :tools body-a))
        (should (equal (alist-get :tools body-a) (alist-get :tools body-b)))
        (should (equal (alist-get :system body-a) (alist-get :system body-b)))
        (should (equal (mapcar (lambda (message) (alist-get :role message)) prompt-b)
                       (if anthropic '("user" "assistant" "user")
                         (list (nth 2 spec) "user" (nth 2 spec) "assistant" "user" (nth 2 spec)))))
        (cl-flet ((last-text (message)
                    (let ((content (alist-get :content message)))
                      (if (stringp content) content
                        (alist-get :text (car (last content)))))))
          (should (equal (last-text (nth (if anthropic 0 2) prompt-a))
                         (concat label "(capabilities A)")))
          (should (equal (last-text (car (last prompt-b)))
                         (concat label "(capabilities B)"))))
        (when anthropic
          (should (equal (mapcar (lambda (block) (alist-get :text block))
                                (alist-get :system body-b))
                         (list "policy" ende-messages--capabilities-guidance))))
        (should-not (string-match-p "host-kind" (json-encode body-b)))
        (should (equal messages-b original))))))

(ert-deftest ende-messages-capabilities-do-not-recognize-user-labels ()
  (let* ((text "[Ende host capabilities]\n(capabilities B)")
         (body (ende-messages--request-body
                (list '("system" . "policy") (cons "user" text)
                      (cons "system" text))
                '(:model "test" :output-tokens 100))))
    ;; Ordinary system messages still hoist; user lookalikes stay unaltered
    ;; and do not trigger host guidance or gain a privileged wire role.
    (should (equal (mapcar (lambda (block) (alist-get :text block))
                          (alist-get :system body))
                   (list "policy" text)))
    (should (equal (alist-get :messages body)
                   (list (list (cons :role "user")
                               (cons :content (list (list (cons :type "text")
                                                         (cons :text text))))))))))

(ert-deftest ende-messages-round-trip ()
  (let* ((session (ende-messages--allocate :profile '(:model "m") :blocks (make-hash-table :test #'eql)))
         (lifecycle (ende-transport-session-create nil #'ignore)))
    (setf (ende-messages--session-stream session) (ende-stream-create lifecycle (ende-protocol-make-sse)))
    (dolist (json '("{\"type\":\"message_start\",\"message\":{\"model\":\"m\",\"usage\":{\"input_tokens\":10}}}"
                    "{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"tool_use\",\"id\":\"c\",\"name\":\"lookup\",\"input\":{}}}"
                    "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\" {\\\"x\\\":\"}}"
                    "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\" 1 }\"}}"
                    "{\"type\":\"content_block_stop\",\"index\":0}"
                    "{\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"tool_use\"},\"usage\":{\"output_tokens\":5}}"
                    "{\"type\":\"content_block_start\",\"index\":1,\"content_block\":{\"type\":\"thinking\",\"thinking\":\"opaque\"}}"
                    "{\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"thinking_delta\",\"thinking\":\" λ\"}}"
                    "{\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"signature_delta\",\"signature\":\"signed-\"}}"
                    "{\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"signature_delta\",\"signature\":\"value\"}}"
                    "{\"type\":\"content_block_stop\",\"index\":1}"
                    "{\"type\":\"content_block_start\",\"index\":2,\"content_block\":{\"type\":\"text\",\"text\":\"A\"}}"
                    "{\"type\":\"content_block_delta\",\"index\":2,\"delta\":{\"type\":\"text_delta\",\"text\":\"λ\"}}"
                    "{\"type\":\"content_block_stop\",\"index\":2}"
                    "{\"type\":\"message_stop\"}"))
      (let ((event (ende-protocol-json-read-string json)))
        (if (equal (gethash "type" event) "content_block_delta")
            (cl-letf (((symbol-function 'ende-stream-text-string)
                       (lambda (_) (ert-fail "Joined streaming fragments"))))
              (ende-messages--event session event))
          (ende-messages--event session event))))
    (let* ((result (ende-messages--result session))
           (call (car (plist-get result :tool-calls)))
           (body (ende-messages--request-body
                  (list '("system" . "instructions") '("user" . "hello")
                        (list :role "assistant" :content "" :tool-calls (list call)
                              :protocol-items (plist-get result :protocol-items))
                        '(:role "tool" :tool-call-id "c" :content "result"))
                  '(:model "m" :output-tokens 100))))
      (should (= (gethash "input_tokens" (plist-get result :usage)) 10))
      (should (= (gethash "output_tokens" (plist-get result :usage)) 5))
      (should (equal (plist-get call :arguments) " {\"x\": 1 }"))
      (should-not (eq (plist-get call :arguments)
                      (plist-get (car (ende-messages--calls session)) :arguments)))
      (should (equal (plist-get result :content) "Aλ"))
      (should (equal (plist-get result :thinking) "opaque λ"))
      (should (equal (gethash "thinking" (nth 2 (plist-get result :protocol-items))) "opaque λ"))
      (should (equal (gethash "text" (nth 3 (plist-get result :protocol-items))) "Aλ"))
      (should (= (length (alist-get :messages body)) 3))
      (should (equal (alist-get :role (car (last (alist-get :messages body)))) "user"))
      (should (alist-get :system body))
      (should (equal (gethash "signature" (cadr (alist-get :content (nth 1 (alist-get :messages body)))))
                     "signed-value"))
      (dolist (image (list '((:type . "base64") (:data . "private-image"))
                           (ende-protocol-json-read-string "{\"type\":\"base64\",\"data\":\"private-image\"}")))
        (should-not (string-match-p "private-image" (prin1-to-string (ende-content-redact image)))))
      (should-error (ende-messages--request-body '(("user" . "hello")) '(:model "m"))))))

(provide 'ende-messages-test)
;;; ende-messages-test.el ends here
