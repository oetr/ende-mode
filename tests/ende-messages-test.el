;;; ende-messages-test.el --- Messages protocol contract -*- lexical-binding: t; -*-

;;; Code:
(require 'ert)
(require 'ende-messages)

(ert-deftest ende-messages-round-trip ()
  (let* ((session (ende-messages--allocate :profile '(:model "m") :blocks (make-hash-table :test #'eql)))
         (lifecycle (ende-transport-session-create nil #'ignore)))
    (setf (ende-messages--session-stream session) (ende-stream-create lifecycle (ende-protocol-make-sse)))
    (dolist (json '("{\"type\":\"message_start\",\"message\":{\"model\":\"m\",\"usage\":{\"input_tokens\":10}}}"
                    "{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"tool_use\",\"id\":\"c\",\"name\":\"lookup\",\"input\":{}}}"
                    "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"{\\\"x\\\":1}\"}}"
                    "{\"type\":\"content_block_stop\",\"index\":0}"
                    "{\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"tool_use\"},\"usage\":{\"output_tokens\":5}}"
                    "{\"type\":\"message_stop\"}"))
      (ende-messages--event session (ende-protocol-json-read-string json)))
    (let* ((result (ende-messages--result session))
           (call (car (plist-get result :tool-calls)))
           (body (ende-messages--request-body
                  (list '("system" . "instructions") '("user" . "hello")
                        (list :role "assistant" :content "" :tool-calls (list call)
                              :protocol-history (plist-get result :protocol-history))
                        '(:role "tool" :tool-call-id "c" :content "result"))
                  '(:model "m" :output-tokens 100))))
      (should (= (gethash "input_tokens" (plist-get result :usage)) 10))
      (should (= (gethash "output_tokens" (plist-get result :usage)) 5))
      (should (equal (plist-get call :arguments) "{\"x\":1}"))
      (should (= (length (alist-get :messages body)) 3))
      (should (equal (alist-get :role (car (last (alist-get :messages body)))) "user"))
      (should (alist-get :system body))
      (dolist (image (list '((:type . "base64") (:data . "private-image"))
                           (ende-protocol-json-read-string "{\"type\":\"base64\",\"data\":\"private-image\"}")))
        (should-not (string-match-p "private-image" (prin1-to-string (ende-content-redact image)))))
      (should-error (ende-messages--request-body '(("user" . "hello")) '(:model "m"))))))

(provide 'ende-messages-test)
;;; ende-messages-test.el ends here
