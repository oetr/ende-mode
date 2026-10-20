;;; ende-debug-test.el --- LLM debugging tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Synthetic coverage for opt-in request recording and cache diagnostics.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ende)
(require 'ende-test-helpers)

(defun ende-debug-test--read-bytes (path)
  "Return literal bytes stored in PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defun ende-debug-test--read-meta (directory sequence)
  "Read the metadata file for SEQUENCE under DIRECTORY."
  (ende-protocol-json-read-string
   (with-temp-buffer
     (insert-file-contents
      (expand-file-name (format "%04d-meta.json" sequence) directory))
     (buffer-string))))

(ert-deftest ende-test/debug-captures-exact-transport-body-without-altering-it ()
  (let ((directory (make-temp-file "ende-debug-fidelity-" t)))
    (unwind-protect
        (dolist (transport '(chat codex))
          (let* ((profile
                  (list :name 'debug-test :job-id 17 :model "test-model"
                        :url (if (eq transport 'codex)
                                 "https://chatgpt.com/backend-api/codex/responses"
                               "https://example.test/v1/chat/completions")
                        :temperature 0.2
                        :controls '((:temperature :path ("temperature")))
                        :require-sse t))
                 (chunks (if (eq transport 'codex)
                             (list (concat "data: {\"type\":\"response.completed\","
                                           "\"response\":{\"usage\":{"
                                           "\"output_tokens\":1}}}\n\n"))
                           (list "data: [DONE]\n\n")))
                 (ende-debug--session-directory directory)
                 (ende-debug--sequence 0)
                 (ende-debugging-enabled nil)
                 (unrecorded
                  (ende-test--run-stream-request transport chunks nil nil profile))
                 (ende-debugging-enabled t)
                 (recorded
                  (ende-test--run-stream-request transport chunks nil nil profile))
                 (recording-file (expand-file-name "0001-request.json" directory))
                 (completion
                  (ende-protocol-json-get
                   "completion" (ende-debug-test--read-meta directory 1)))
                 (request-body (plist-get recorded :request-body)))
            (should (equal (plist-get unrecorded :request-body) request-body))
            (should (equal (ende-debug-test--read-bytes recording-file)
                           request-body))
            (should (= (logand (file-modes recording-file) #o777) #o600))
            (should (= (ende-protocol-json-get "http_status" completion) 200))
            (should (file-exists-p
                     (expand-file-name "0001-messages.el" directory)))))
      (delete-directory directory t))))

(ert-deftest ende-test/debug-disabled-does-not-create-captures ()
  (let* ((directory (make-temp-file "ende-debug-disabled-" t))
         (ende-debugging-enabled nil)
         (ende-debug--session-directory directory)
         (ende-debug--sequence 0))
    (unwind-protect
        (progn
          (should-not
           (ende-debug-record-request
            "{}" '((:model . "test-model"))
            '(("user" . "prompt")) '(:model "test-model")
            "openai-compatible" "https://example.test/v1/chat/completions"))
          (should (= ende-debug--sequence 0))
          (should (equal (directory-files directory nil "^[^.]") nil)))
      (delete-directory directory t))))

(ert-deftest ende-test/debug-completions-stay-associated-after-disable ()
  (let* ((directory (make-temp-file "ende-debug-completion-" t))
         (ende-debugging-enabled t)
         (ende-debug--session-directory directory)
         (ende-debug--sequence 0)
         (profile '(:name test :job-id 23 :model "test-model"))
         (messages '(("user" . "prompt")))
         (record-a (ende-debug-record-request
                    "{\"model\":\"test-model\"}"
                    '((:model . "test-model")) messages profile
                    "openai-compatible" "https://example.test/v1"))
         (record-b (ende-debug-record-request
                    "{\"model\":\"test-model\"}"
                    '((:model . "test-model")) messages profile
                    "openai-compatible" "https://example.test/v1"))
         (session-a (ende-llm--make-session :callbacks nil :model "test-model"))
         (session-b (ende-llm--make-session :callbacks nil :model "test-model")))
    (unwind-protect
        (progn
          (setf (ende-llm--session-debug-record session-a) record-a
                (ende-llm--session-debug-record session-b) record-b
                (ende-stream-session-usage (ende-llm--session-stream session-a))
                (ende-protocol-json-read-string
                 (concat "{\"input_tokens\":100,\"output_tokens\":5,"
                         "\"input_tokens_details\":{\"cached_tokens\":0}}")))
          (setq ende-debugging-enabled nil)
          (ende-llm--settle session-b 'failure
                            (ende-error-create :kind 'transport
                                               :message "synthetic failure"))
          (ende-llm--settle session-a 'success (ende-llm--result session-a))
          (let* ((completion-a (ende-protocol-json-get
                                "completion"
                                (ende-debug-test--read-meta directory 1)))
                 (completion-b (ende-protocol-json-get
                                "completion"
                                (ende-debug-test--read-meta directory 2))))
            (should (equal (ende-protocol-json-get "outcome" completion-a)
                           "success"))
            (should (= (ende-protocol-json-get "input_tokens" completion-a)
                       100))
            (should (= (ende-protocol-json-get
                        "cached_input_tokens" completion-a) 0))
            (should (eq (ende-protocol-json-get
                        "cached_tokens_reported" completion-a) t))
            (should (equal (ende-protocol-json-get "outcome" completion-b)
                           "failure"))
            (should (eq (ende-protocol-json-get
                        "cached_tokens_reported" completion-b) :false))
            (should-not (ende-protocol-json-get
                         "cached_input_tokens" completion-b))))
      (delete-directory directory t))))

(provide 'ende-debug-test)

;;; ende-debug-test.el ends here
