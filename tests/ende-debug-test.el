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
                 (request-body (plist-get recorded :request-body)))
            (should (equal (plist-get unrecorded :request-body) request-body))
            (should (equal (ende-debug-test--read-bytes recording-file)
                           request-body))
            (should (= (logand (file-modes recording-file) #o777) #o600))
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

(provide 'ende-debug-test)

;;; ende-debug-test.el ends here
