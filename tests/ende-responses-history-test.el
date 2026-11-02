;;; ende-responses-history-test.el --- Responses replay safety -*- lexical-binding: t; -*-

(require 'ert)
(require 'ende-codex)
(require 'ende-tool-store)

(defun ende-responses-history-test--json (text)
  "Read fixture TEXT as a provider object."
  (ende-protocol-json-read-string text))

(ert-deftest ende-responses-history-ordered-tool-rounds ()
  (let ((messages nil))
    (dotimes (round 3)
      (let* ((id (format "call-%d" round))
             (reasoning (ende-responses-history-test--json
                         "{\"type\":\"reasoning\",\"summary\":[],\"encrypted_content\":\"opaque-replay-secret\"}"))
             (call (ende-responses-history-test--json
                    (format "{\"type\":\"function_call\",\"call_id\":\"%s\",\"name\":\"read_file\",\"arguments\":\"{}\"}" id)))
             (session (ende-codex--make-session)))
        ;; Completion can repeat earlier done events and arrive out of order.
        (ende-codex--remember-output session call 1)
        (ende-codex--remember-output session reasoning 0)
        (ende-codex--remember-output session call 1)
        (let ((history (ende-codex--history session)))
          (should (equal (json-encode (vconcat (cdr history)))
                         (json-encode (vector reasoning call))))
          (should-not (eq reasoning (cadr history)))
          (setq messages
                (append messages
                        (list (list :role "assistant" :content ""
                                    :protocol-history history
                                    :tool-calls (list (list :id id :name "read_file" :arguments "{}")))
                              (list :role "tool" :tool-call-id id :content "result")))))))
    (let ((input (ende-codex--request-messages messages)))
      (should (equal (mapcar (lambda (item)
                              (if (hash-table-p item) (gethash "type" item)
                                (alist-get :type item))) input)
                     '("reasoning" "function_call" "function_call_output"
                       "reasoning" "function_call" "function_call_output"
                       "reasoning" "function_call" "function_call_output")))
      (should (equal (ende-protocol-json-get "encrypted_content" (car input))
                     "opaque-replay-secret"))
      (dotimes (round 3)
        (let ((call (nth (1+ (* 3 round)) input))
              (result (nth (+ 2 (* 3 round)) input)))
          (should (equal (ende-protocol-json-get "call_id" call)
                         (alist-get :call_id result)))
          (should (equal (alist-get :call_id result) (format "call-%d" round))))))))

(ert-deftest ende-responses-history-completed-message-and-phase ()
  (let* ((output (ende-responses-history-test--json
                  "{\"type\":\"response.completed\",\"response\":{\"output\":[{\"type\":\"reasoning\",\"encrypted_content\":\"first\",\"summary\":[]},{\"type\":\"reasoning\",\"encrypted_content\":\"second\",\"summary\":[]},{\"type\":\"message\",\"role\":\"assistant\",\"phase\":\"final_answer\",\"content\":[{\"type\":\"output_text\",\"text\":\"answer\"}]}]}}"))
         (session (ende-codex--make-session)))
    (ende-codex--handle-event session output)
    (let* ((history (ende-codex--history session))
           (input (ende-codex--request-messages
                   (list (list :role "assistant" :content "answer" :protocol-history history)))))
      (should (= (length input) 3))
      (should (equal (ende-protocol-json-get "encrypted_content" (nth 1 input)) "second"))
      (should (equal (ende-protocol-json-get "phase" (nth 2 input)) "final_answer")))))

(ert-deftest ende-responses-history-done-and-legacy-fallback ()
  (let ((session (ende-codex--make-session)))
    (ende-codex--handle-event
     session (ende-responses-history-test--json
              "{\"type\":\"response.output_item.done\",\"output_index\":0,\"item\":{\"type\":\"reasoning\",\"encrypted_content\":\"opaque\"}}"))
    (should (= (length (cdr (ende-codex--history session))) 1)))
  (let* ((call '(:id "old-call" :name "read_file" :arguments "{}"))
         (input (ende-codex--request-messages
                 (list (list :role "assistant" :content "Checking" :tool-calls (list call))
                       (list :tool-call-id "old-call" :content "result")))))
    (should (= (length input) 3))
    (should (equal (alist-get :type (nth 1 input)) "function_call"))))

(ert-deftest ende-responses-history-private-store-roundtrip ()
  (let* ((directory (make-temp-file "ende-replay-test-" t))
         (file (expand-file-name "payloads.sqlite3" directory))
         (history (list 'openai-responses
                        (ende-responses-history-test--json
                         "{\"type\":\"reasoning\",\"encrypted_content\":\"unchanged-secret\",\"summary\":[]}"))))
    (unwind-protect
        (let* ((id (ende-tool-store-save-protocol history "answer" nil file))
               (restored (ende-tool-store-load-protocol id "answer" nil file)))
          (should (equal (json-encode (vconcat (cdr restored)))
                         (json-encode (vconcat (cdr history)))))
          (should (= (logand (file-modes file) #o777) #o600)))
      (delete-directory directory t))))

(ert-deftest ende-responses-history-private-redaction ()
  (let* ((object (ende-responses-history-test--json
                  "{\"type\":\"reasoning\",\"encrypted_content\":\"opaque-secret\",\"summary\":[]}"))
         (representations (list object '((:encrypted_content . "opaque-secret"))
                                '(encrypted_content . "opaque-secret")
                                '(:encrypted_content "opaque-secret" :other "kept")
                                (vector object))))
    (dolist (value representations)
      (should-not (string-match-p "opaque-secret" (prin1-to-string (ende-content-redact value)))))
    (should (equal (gethash "encrypted_content" object) "opaque-secret"))
    (should-not (string-match-p "opaque-secret"
                               (ende-transport-safe-error-text (json-encode object))))))

(ert-deftest ende-responses-history-stateless-request ()
  (dolist (builder '(ende-codex--request-body ende-responses--request-body))
    (let ((body (funcall builder '(("user" . "Hello")) '(:model "test"))))
      (when (eq builder 'ende-codex--request-body)
        (should (equal (alist-get :include body) ["reasoning.encrypted_content"])))
      (should (eq (alist-get :store body) json-false))
      (should (eq (alist-get :stream body) t))
      (should-not (assq :previous_response_id body))
      (should-not (assq :prompt_cache_key body)))))

(provide 'ende-responses-history-test)
;;; ende-responses-history-test.el ends here
