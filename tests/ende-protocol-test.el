;;; ende-protocol-test.el --- Protocol parser tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Coverage for incremental protocol and reasoning parsers.

;;; Code:

(require 'ert)
(require 'ende)
(require 'ende-test-helpers)
(require 'cl-lib)
(require 'json)
(require 'ende-protocol)

;;;; Incremental reasoning parser

(defun ende-test--reasoning-callbacks (thinking content)
  "Return callbacks appending thinking and content into two symbols."
  (list :on-delta
        (lambda (type text)
          (if (eq type :thinking)
              (set thinking (concat (symbol-value thinking) text))
            (set content (concat (symbol-value content) text))))
        :on-status #'ignore))



(ert-deftest ende-test/tool-protocols-preserve-calls-and-results ()
  (let* ((definition
          (cl-find-if
           (lambda (candidate)
             (equal (alist-get :name (alist-get :function candidate))
                    "read_file"))
           (ende-tool-definitions)))
         (profile (list :model "model" :tool-definitions (list definition)))
         (call '(:id "call-1" :name "read_file"
                     :arguments "{\"path\":\"main.el\"}"))
         (messages (list (cons "user" "read a file")
                         (list :role "assistant" :content nil
                               :tool-calls (list call))
                         (list :role "tool" :tool-call-id "call-1"
                               :content "{\"ok\":true}")))
         (chat-body (ende-llm--request-body messages profile))
         (responses-body (ende-codex--request-body messages profile)))
    (should (equal (alist-get :tool_choice chat-body) "auto"))
    (should (equal (alist-get :tool_choice responses-body) "auto"))
    (let* ((assistant (nth 1 (alist-get :messages chat-body)))
           (tool-call (car (alist-get :tool_calls assistant))))
      (should (equal (alist-get :id tool-call) "call-1"))
      (should (equal (alist-get :name (alist-get :function tool-call))
                     "read_file")))
    (should (equal (alist-get :type (nth 1 (alist-get :input responses-body)))
                   "function_call"))
    (should (equal (alist-get :type (nth 2 (alist-get :input responses-body)))
                   "function_call_output"))
    (let ((session (ende-llm--make-session
                    :tool-calls (make-hash-table :test #'eql)
                    :estimated-chars 0)))
      (ende-llm--collect-tool-calls
       session
       (vector (json-parse-string
                "{\"index\":0,\"id\":\"call-1\",\"function\":{\"name\":\"read_\",\"arguments\":\"{\\\"path\\\":\"}}"
                :object-type 'hash-table)))
      (ende-llm--collect-tool-calls
       session
       (vector (json-parse-string
                "{\"index\":0,\"function\":{\"name\":\"file\",\"arguments\":\"\\\"main.el\\\"}\"}}"
                :object-type 'hash-table)))
      (should (equal (car (ende-llm--result session)) :content))
      (should (equal (plist-get (car (plist-get (ende-llm--result session)
                                                :tool-calls))
                                :arguments)
                     "{\"path\":\"main.el\"}")))
    (let* ((session (ende-codex--make-session
                     :tool-calls (make-hash-table :test #'eql)
                     :estimated-chars 0))
           (event
            (json-parse-string
             "{\"type\":\"response.completed\",\"response\":{\"output\":[{\"type\":\"function_call\",\"call_id\":\"call-2\",\"name\":\"read_file\",\"arguments\":\"{\\\"path\\\":\\\"main.el\\\"}\"}]}}"
             :object-type 'hash-table)))
      (ende-codex--handle-event session event)
      (should (equal (plist-get (car (plist-get (ende-codex--result session)
                                                :tool-calls))
                                :id)
                     "call-2")))))



(ert-deftest ende-test/thinking-parser-separation ()
  (let ((thinking-symbol (make-symbol "thinking"))
        (content-symbol (make-symbol "content")))
    (set thinking-symbol "")
    (set content-symbol "")
    (let* ((parser (ende-protocol-make-reasoning))
           (callbacks (ende-test--reasoning-callbacks
                       thinking-symbol content-symbol)))
      (ende-protocol-reasoning-feed
       parser "Intro <think>Plan here" callbacks)
      (ende-protocol-reasoning-feed
       parser "...done</think>Final answer" callbacks)
      (should-not (ende-protocol-reasoning-finish parser callbacks))
      (should (string= (symbol-value thinking-symbol) "Plan here...done"))
      (should (string= (symbol-value content-symbol) "Intro Final answer")))))



(ert-deftest ende-test/thinking-parser-all-delimiter-splits ()
  (dolist (split (number-sequence 0 (length "<think>secret</think>answer")))
    (let ((thinking "") (content "")
          (parser (ende-protocol-make-reasoning)))
      (let ((callbacks
             (list :on-delta
                   (lambda (type text)
                     (if (eq type :thinking)
                         (setq thinking (concat thinking text))
                       (setq content (concat content text)))))))
        (ende-protocol-reasoning-feed
         parser (substring "<think>secret</think>answer" 0 split) callbacks)
        (ende-protocol-reasoning-feed
         parser (substring "<think>secret</think>answer" split) callbacks)
        (should-not (ende-protocol-reasoning-finish parser callbacks))
        (should (string= thinking "secret"))
        (should (string= content "answer"))))))



(ert-deftest ende-test/thinking-parser-unclosed-is-error ()
  (let ((parser (ende-protocol-make-reasoning)))
    (ende-protocol-reasoning-feed parser "<think>secret" (list :on-delta #'ignore))
    (should (ende-error-p
             (ende-protocol-reasoning-finish parser (list :on-delta #'ignore))))))

;;;; SSE parser

(ert-deftest ende-test/sse-parser-fragmented-and-done ()
  (let ((parser (ende-protocol-make-sse))
        (events nil))
    (setq events
          (append events
                  (ende-protocol-sse-feed
                   parser "data: {\"choices\":[{\"delta\":{\"content\":\"hi")))
    (setq events
          (append events
                  (ende-protocol-sse-feed
                   parser " there\"}}]}\r\n\r\ndata: [DONE]\r\n\r\n")))
    (should (= (length events) 2))
    (should (eq (plist-get (nth 0 events) :type) :json))
    (should (string= (ende-protocol-json-get
                      "content"
                      (ende-protocol-json-get
                       "delta"
                       (aref (ende-protocol-json-get
                              "choices"
                              (plist-get (nth 0 events) :value))
                             0)))
                     "hi there"))
    (should (eq (plist-get (nth 1 events) :type) :done))
    (should-not (ende-protocol-sse-finish parser))))



(ert-deftest ende-test/sse-parser-rejects-malformed-and-ignores-late-data ()
  (let ((parser (ende-protocol-make-sse)))
    (should-error
     (ende-protocol-sse-feed parser "data: {bad}\n\n"))
    (let ((done-parser (ende-protocol-make-sse)))
      (ende-protocol-sse-feed done-parser "data: [DONE]\n\n")
      (should-not
       (ende-protocol-sse-feed
        done-parser "data: {\"choices\":[]}\n\n")))))



(ert-deftest ende-test/sse-parser-requires-done ()
  (let ((parser (ende-protocol-make-sse)))
    (ende-protocol-sse-feed parser "data: {\"choices\":[]}\n\n")
    (should-error (ende-protocol-sse-finish parser))))



(ert-deftest ende-test/sse-parser-flushes-final-event-without-done ()
  (let ((parser (ende-protocol-make-sse)))
    (should-not
     (ende-protocol-sse-feed parser "data: {\"type\":\"response.completed\"}"))
    (let ((events (ende-protocol-sse-flush parser)))
      (should (= (length events) 1))
      (should (string=
               (ende-protocol-json-get
                "type" (plist-get (car events) :value))
               "response.completed")))
    (should-not (ende-protocol-sse-flush parser))
    (should-error (ende-protocol-sse-finish parser))))



(ert-deftest ende-test/json-parser-does-not-intern-keys-or-accept-garbage ()
  (let ((key (format "ende-untrusted-key-%d" (random most-positive-fixnum))))
    (should-not (intern-soft key))
    (let ((payload (ende-protocol-json-read-string
                    (format "{\"%s\":1}" key))))
      (should (hash-table-p payload))
      (should (= (ende-protocol-json-get key payload) 1))
      (should-not (intern-soft key))))
  (should-error (ende-protocol-json-read-string "{\"ok\":1} garbage")))



(ert-deftest ende-test/sse-parser-enforces-line-and-event-limits ()
  (let ((ende-max-sse-event-bytes 8)
        (parser (ende-protocol-make-sse)))
    (should-error (ende-protocol-sse-feed parser "123456789")))
  (let ((ende-max-sse-event-bytes 8)
        (parser (ende-protocol-make-sse)))
    (should-error
     (ende-protocol-sse-feed
      parser (mapconcat (lambda (_) "data:\n") (number-sequence 1 10) "")))))

(ert-deftest ende-test/json-key-presence-distinguishes-null-from-missing ()
  (let ((object (ende-protocol-json-read-string
                 "{\"null\":null,\"false\":false,\"zero\":0,\"empty\":\"\"}")))
    (should-not (ende-protocol-json-get "null" object))
    (dolist (key '("null" "false" "zero" "empty"))
      (should (ende-protocol-json-has-key-p key object)))
    (should-not (ende-protocol-json-has-key-p "missing" object))
    (puthash "sentinel" (make-symbol "missing") object)
    (should (ende-protocol-json-has-key-p "sentinel" object)))
  (should-not (ende-protocol-json-has-key-p
               "missing" (make-hash-table :test 'equal)))
  (should-not (ende-protocol-json-has-key-p "key" nil))
  (should-not (ende-protocol-json-has-key-p "key" '(("key" . t)))))

(provide 'ende-protocol-test)

;;; ende-protocol-test.el ends here
