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

(defun ende-debug-test--record-synthetic (request job usage)
  "Record synthetic REQUEST for JOB with provider USAGE."
  (let* ((model (alist-get :model request))
         (profile (list :name 'debug-test :job-id job :model model
			:reasoning-effort (or (alist-get :reasoning_effort
                                                         request)
                                              "high")))
         (body (encode-coding-string (json-encode request) 'utf-8))
         (record (ende-debug-record-request
                  body request '(("user" . "synthetic")) profile
                  "synthetic" "https://example.test/v1")))
    (ende-debug-record-completion record 'success usage 200 nil)))

(ert-deftest ende-test/debug-captures-exact-transport-body-without-altering-it ()
  (let ((directory (make-temp-file "ende-debug-fidelity-" t)))
    (unwind-protect
        (dolist (transport '(chat codex))
          (let* ((codex (eq transport 'codex))
                 (profile
                  (append
                   (list :name 'debug-test
                         :api (if codex 'openai-responses 'openai-chat-completions)
                         :auth-method (if codex 'codex-oauth 'none)
                         :job-id 17 :model "test-model"
                         :url (if codex
                                  "https://api.openai.com/v1/responses"
                                "https://example.test/v1/chat/completions")
                         :require-sse t)
                   (unless codex
                     (list :temperature 0.2
                           :controls '((:temperature :path ("temperature")))))
                   (when codex
                     (list :codex-auth-source 'ende :account 'default))))
                 (chunks (if (eq transport 'codex)
                             (list (concat "data: {\"type\":\"response.completed\","
                                           "\"response\":{\"usage\":{"
                                           "\"output_tokens\":1}}}\n\n"))
                           (list "data: [DONE]\n\n")))
                 (messages '(("user" . "Hello from 世界 — café")))
                 (ende-debug--session-directory directory)
                 (ende-debug--sequence 0)
                 (ende-debugging-enabled nil)
                 (unrecorded
                  (ende-test--run-stream-request
                   transport chunks nil nil profile nil messages))
                 (ende-debugging-enabled t)
                 (recorded
                  (ende-test--run-stream-request
                   transport chunks nil nil profile nil messages))
                 (recording-file (expand-file-name "0001-request.json" directory))
                 (completion
                  (ende-protocol-json-get
                   "completion" (ende-debug-test--read-meta directory 1)))
                 (request-body (plist-get recorded :request-body)))
            (should (equal (plist-get unrecorded :request-body) request-body))
            (should (equal (ende-debug-test--read-bytes recording-file)
                           request-body))
            (should (= (logand (file-modes recording-file) #o777) #o600))
            (should (= (logand
                        (file-modes (expand-file-name "0001-meta.json"
                                                      directory))
                        #o777)
                       #o600))
            (should (= (logand
                        (file-modes (expand-file-name "0001-messages.el"
                                                      directory))
                        #o777)
                       #o600))
            (should (string-match-p
                     "世界"
                     (with-temp-buffer
                       (insert-file-contents
                        (expand-file-name "0001-messages.el" directory))
                       (buffer-string))))
            (should (= (ende-protocol-json-get "http_status" completion) 200))
            (let ((metadata (ende-debug-test--read-meta directory 1)))
              (should (= (ende-protocol-json-get "job_id" metadata) 17))
              (should (equal (ende-protocol-json-get "transport" metadata)
                             (if codex
                                 "openai-responses"
                               "openai-compatible")))
              (should-not
               (string-match-p "transport-test-token"
                               (with-temp-buffer
                                 (insert-file-contents
                                  (expand-file-name "0001-meta.json"
                                                    directory))
                                 (buffer-string))))
              (when codex
                (should (string-match-p
                         "Authorization: Bearer transport-test-token"
                         (plist-get recorded :config-text)))))
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

(ert-deftest ende-test/debug-module-loads-before-streaming-transports-on-reload ()
  (let* ((directory (file-name-directory (locate-library "ende.el")))
         (files (ende-reload--files directory))
         (debug-position (cl-position "ende-debug.el" files :test #'equal))
         (codex-position (cl-position "ende-codex.el" files :test #'equal))
         (llm-position (cl-position "ende-llm.el" files :test #'equal)))
    (should debug-position)
    (should codex-position)
    (should llm-position)
    (should (< debug-position codex-position))
    (should (< debug-position llm-position))))

(ert-deftest ende-test/debug-toggle-creates-and-retains-private-session ()
  (let* ((root (make-temp-file "ende-debug-session-" t))
         (user-emacs-directory (file-name-as-directory root))
         (ende-debugging-enabled nil)
         (ende-debug--session-directory nil)
         (ende-debug--sequence 0)
         (original-file-modes (default-file-modes)))
    (unwind-protect
        (progn
          (ende-debugging-on)
          (let ((directory ende-debug--session-directory))
            (should ende-debugging-enabled)
            (should (file-directory-p directory))
            (should (string-prefix-p user-emacs-directory directory))
            (should (= (logand (file-modes directory) #o777) #o700))
            (ende-debugging-off)
            (should-not ende-debugging-enabled)
            (should (= (default-file-modes) original-file-modes))
            (should (file-directory-p directory))))
      (delete-directory root t))))

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
          (ende-debugging-off)
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

(ert-deftest ende-test/debug-cache-report-compares-saved-chat-and-responses ()
  (let* ((directory (make-temp-file "ende-debug-report-" t))
         (ende-debugging-enabled t)
         (ende-debug--session-directory directory)
         (ende-debug--sequence 0)
         (system '((:role . "system") (:content . "system instructions")))
         (system-changed
          '((:role . "system") (:content . "changed system instructions")))
         (user-a '((:role . "user") (:content . "first question")))
         (user-b '((:role . "user") (:content . "second question")))
         (assistant '((:role . "assistant") (:content . "answer")))
         (tools-a [((:type . "function") (:name . "lookup"))])
         (tools-b [((:type . "function") (:name . "read_file"))])
         (initial `((:model . "chat-model")
                    (:messages . [,system ,user-a])
                    (:reasoning . ((:effort . "high")))
                    (:tools . ,tools-a)))
         (grown `((:model . "chat-model")
                  (:messages . [,system ,user-a ,assistant ,user-b])
                  (:reasoning . ((:effort . "high")))
                  (:tools . ,tools-a)))
         (modified `((:model . "chat-model")
                     (:messages . [,system-changed ((:role . "user")
                                                    (:content . "edited question"))
						   ,assistant])
                     (:reasoning . ((:effort . "high")))
                     (:tools . ,tools-a)))
         (tool-changed `((:model . "chat-model")
                         (:messages . [,system-changed ((:role . "user")
                                                        (:content . "edited question"))
						       ,assistant])
                         (:reasoning . ((:effort . "high")))
                         (:tools . ,tools-b)))
         (settings-changed `((:model . "chat-model-v2")
                             (:messages . [,system-changed ((:role . "user")
                                                            (:content . "edited question"))
							   ,assistant])
                             (:reasoning . ((:effort . "low")))
                             (:reasoning_effort . "low")
                             (:tools . ,tools-b)))
         (cache-a (ende-protocol-json-read-string
                   (concat "{\"input_tokens\":100,\"output_tokens\":5,"
                           "\"input_tokens_details\":{\"cached_tokens\":90,"
                           "\"cache_creation_input_tokens\":3}}")))
         (cache-zero (ende-protocol-json-read-string
                      (concat "{\"input_tokens\":200,\"output_tokens\":5,"
                              "\"input_tokens_details\":{\"cached_tokens\":0}}")))
         (cache-missing (ende-protocol-json-read-string
                         "{\"input_tokens\":210,\"output_tokens\":5}"))
         (report nil))
    (unwind-protect
        (progn
          (ende-debug-test--record-synthetic initial 31 cache-a)
          (ende-debug-test--record-synthetic initial 31 cache-a)
          (ende-debug-test--record-synthetic grown 31 cache-zero)
          (ende-debug-test--record-synthetic modified 31 cache-missing)
          (ende-debug-test--record-synthetic tool-changed 31 cache-zero)
          (ende-debug-test--record-synthetic settings-changed 31 cache-missing)
          (ende-debug-test--record-synthetic
           '((:model . "responses-model")
             (:input . [((:role . "user") (:content . "question"))]))
           32 cache-a)
          (ende-debug-test--record-synthetic
           '((:model . "responses-model")
             (:input . [((:role . "user") (:content . "question"))
                        ((:role . "assistant") (:content . "answer"))]))
           32 cache-zero)
          (setq report
                (ende-debug--format-report
                 directory (ende-debug--read-captures directory)))
          (should (string-match-p "0 appended, previous prefix identical" report))
          (should (string-match-p "2 appended, previous prefix identical" report))
          (should (string-match-p "previous prefix modified, removed, or reordered"
                                  report))
          (should (string-match-p "Tools:           changed" report))
          (should (string-match-p "Model:           changed" report))
          (should (string-match-p "Reasoning:       changed" report))
          (should (string-match-p "Instructions:    changed" report))
          (should (string-match-p "input\\[1\\]" report))
          (should (string-match-p "Cached: 0 (0.0%)" report))
          (should (string-match-p "Cached: not reported" report))
          (should (string-match-p "First changed path:" report))
          (should (= (ende-protocol-json-get
                      "cache_write_tokens"
                      (ende-protocol-json-get
                       "completion" (ende-debug-test--read-meta directory 1)))
                     3))
          (should (string-match-p
                   "The provider cache outcome's cause is not determined" report))
          (let (opened-buffer)
            (cl-letf (((symbol-function 'pop-to-buffer)
                       (lambda (buffer &rest _) (setq opened-buffer buffer))))
              (ende-debug-cache-report))
            (should (buffer-live-p opened-buffer))
            (with-current-buffer opened-buffer
              (should (string-match-p "Ende cache report"
                                      (buffer-string)))
              (should buffer-read-only))))
      (delete-directory directory t))))

(provide 'ende-debug-test)

;;; ende-debug-test.el ends here
