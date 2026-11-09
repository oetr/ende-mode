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
