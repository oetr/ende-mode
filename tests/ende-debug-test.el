;;; ende-debug-test.el --- LLM debugging tests -*- lexical-binding: t; -*-

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

(defun ende-debug-test--capture (sequence job fingerprint)
  "Return a synthetic capture for SEQUENCE, JOB, and FINGERPRINT."
  (list :sequence sequence :bytes "{\"input\":[]}"
        :request (ende-protocol-json-read-string "{\"input\":[]}")
        :metadata (ende-protocol-json-read-string
                   (json-encode `((:job_id . ,job)
                                  (:session_fingerprint . ,fingerprint)
                                  (:provider . "test-provider")
                                  (:transport . "test-transport")
                                  (:endpoint . "https://example.test/v1"))))))

(defun ende-debug-test--pair-headers (captures)
  "Return comparison headings in the report for CAPTURES."
  (cl-remove-if-not
   (lambda (line) (string-prefix-p "Request #" line))
   (split-string (ende-debug--format-report "/synthetic" captures) "\n" t)))

(ert-deftest ende-test/debug-captures-exact-transport-body-without-altering-it ()
  (let ((directory (make-temp-file "ende-debug-fidelity-" t)))
    (unwind-protect
        (dolist (transport '(chat codex))
          (let* ((codex (eq transport 'codex))
                 (profile
                  (append
                   (list :name 'debug-test
                         :api (if codex 'openai-responses 'openai-chat-completions)
                         :auth-method (if codex 'chatgpt-plan-oauth 'none)
                         :job-id 17 :model "test-model"
                         :url (if codex
                                  "https://api.openai.com/v1/responses"
                                "https://example.test/v1/chat/completions")
                         :require-sse t)
                   (unless codex
                     (list :temperature 0.2
                           :controls '((:temperature :path ("temperature")))))
                   (when codex
                     (list :codex-auth-source 'ende :account 'default
                           :codex-session-id "debug-test-session"))))
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
          (cl-letf (((symbol-function 'ende-debug--input-fingerprints)
                     (lambda (&rest _) (ert-fail "Debugging is disabled"))))
            (should-not
             (ende-debug-record-request
              "{}" '((:model . "test-model"))
              '(("user" . "prompt")) '(:model "test-model")
              "openai-compatible" "https://example.test/v1/chat/completions")))
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
          (should (string-match-p "prefix was preserved; 0 appended" report))
          (should (string-match-p "prefix was preserved; 2 appended" report))
          (should (string-match-p "original input prefix modified, removed, or reordered"
                                  report))
          (should (string-match-p "Tools:           changed" report))
          (should (string-match-p "Model:           changed" report))
          (should (string-match-p "Reasoning:       changed" report))
          (should (string-match-p "Instructions:    changed" report))
          (should (string-match-p "input\\[1\\]" report))
          (should (string-match-p "Cached: 0 (0.0%)" report))
          (should (string-match-p "Cached: not reported" report))
          (should (string-match-p "First appended item:" report))
          (should (= (ende-protocol-json-get
                      "cache_write_tokens"
                      (ende-protocol-json-get
                       "completion" (ende-debug-test--read-meta directory 1)))
                     3))
          (should (string-match-p
                   "The provider cache outcome's cause is not determined" report))
          (should (string-match-p
                   "does not establish whether that change caused a cache miss" report))
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

(ert-deftest ende-debug-cross-job-pairs-follow-interleaved-sessions ()
  (let ((captures (list (ende-debug-test--capture 7 11 "session-a")
                        (ende-debug-test--capture 8 12 "session-a")
                        (ende-debug-test--capture 9 21 "session-b")
                        (ende-debug-test--capture 10 12 "session-a")
                        (ende-debug-test--capture 11 22 "session-b")
                        (ende-debug-test--capture 12 13 "session-a"))))
    (should (equal (sort (ende-debug-test--pair-headers captures) #'string<)
                   (sort (list "Request #7 -> #8 (job 11 -> job 12)"
                               "Request #8 -> #10 (job 12)"
                               "Request #9 -> #11 (job 21 -> job 22)"
                               "Request #10 -> #12 (job 12 -> job 13)")
                         #'string<)))))

(ert-deftest ende-debug-cross-job-pairs-require-known-scoped-identities ()
  (dolist (key '("provider" "transport" "endpoint" "session_fingerprint"))
    (let ((previous (ende-debug-test--capture 7 11 "session"))
          (current (ende-debug-test--capture 8 12 "session")))
      (puthash key "different" (plist-get current :metadata))
      (should-not (ende-debug-test--pair-headers (list previous current)))))
  (dolist (key '("provider" "transport" "endpoint" "session_fingerprint" "job_id"))
    (dolist (unknown '(absent nil ""))
      (let ((captures (list (ende-debug-test--capture 7 11 "session")
                            (ende-debug-test--capture 8 12 "session"))))
        (dolist (capture captures)
          (if (eq unknown 'absent)
              (remhash key (plist-get capture :metadata))
            (puthash key unknown (plist-get capture :metadata))))
        (should-not (ende-debug-test--pair-headers captures)))))
  (let ((captures (list (ende-debug-test--capture 7 nil nil)
                        (ende-debug-test--capture 8 nil nil))))
    (dolist (capture captures)
      (setf (plist-get capture :metadata) nil))
    (should-not (ende-debug-test--pair-headers captures)))
  (should-not (ende-debug-test--pair-headers
               (list (ende-debug-test--capture 7 11 "session")
                     (ende-debug-test--capture 8 nil "session")
                     (ende-debug-test--capture 9 12 "session")))))

(ert-deftest ende-debug-within-job-pairs-survive-session-changes ()
  (let* ((captures (list (ende-debug-test--capture 1 11 "session-a")
                         (ende-debug-test--capture 2 22 "session-b")
                         (ende-debug-test--capture 3 11 "changed-session")
                         (ende-debug-test--capture 4 11 nil)
                         (ende-debug-test--capture 5 11 "")
                         (ende-debug-test--capture 6 11 nil)))
         (old-metadata (plist-get (car (last captures)) :metadata)))
    (dolist (key '("provider" "transport" "endpoint" "session_fingerprint"))
      (remhash key old-metadata))
    (should (equal (ende-debug-test--pair-headers captures)
                   '("Request #1 -> #3 (job 11)"
                     "Request #3 -> #4 (job 11)"
                     "Request #4 -> #5 (job 11)"
                     "Request #5 -> #6 (job 11)")))
    (let ((report (ende-debug--format-report "/synthetic" captures)))
      (should (string-match-p "Session:         changed" report))
      (should (string-match-p "Session:         unknown" report)))))

(ert-deftest ende-debug-private-replay-and-cache-metadata ()
  (let* ((directory (make-temp-file "ende-debug-private-" t))
         (ende-debugging-enabled t)
         (ende-debug--session-directory directory)
         (ende-debug--sequence 0)
         (item (ende-protocol-json-read-string
                "{\"type\":\"reasoning\",\"encrypted_content\":\"secret-replay+/=encrypted_content\"}"))
         (body `((:model . "requested") (:input . [,item])
                 (:include . ["reasoning.encrypted_content"])
                 (:prompt_cache_key . "secret-cache-identity")
                 (:prompt_cache_options . ,(ende-protocol-json-read-string
                                           "{\"mode\":\"implicit\"}"))))
         (messages (list (list :protocol-items (list 'openai-responses item))))
         (original-body (json-encode body))
         (original-messages (prin1-to-string messages)))
    (unwind-protect
        (let ((record (ende-debug-record-request
                       original-body body messages
                       nil "test" "https://example.test")))
          (ende-debug-record-response
           record (ende-protocol-json-read-string
                   "{\"model\":\"actual\",\"service_tier\":\"default\",\"id\":\"resp-1\",\"output\":[{\"encrypted_content\":\"response-only-replay\"}]}"))
          (ende-debug-record-completion
           record 'success (ende-protocol-json-read-string
                            "{\"input_tokens\":4096,\"input_tokens_details\":{\"cached_tokens\":2048},\"cache_creation_input_tokens\":0}") 200 nil)
          (let ((meta (ende-debug-test--read-meta directory 1)))
            (should (eq (gethash "cache_key_supplied" meta) t))
            (should (equal (gethash "response_model" meta) "actual"))
            (should (equal (gethash "response_service_tier" meta) "default"))
            (should (equal (gethash "include" (gethash "request_settings" meta))
                           ["reasoning.encrypted_content"])))
          (let* ((request (ende-protocol-json-read-string
                           (ende-debug-test--read-bytes
                            (expand-file-name "0001-request.json" directory))))
                 (saved-messages
                  (with-temp-buffer
                    (insert-file-contents
                     (expand-file-name "0001-messages.el" directory))
                    (read (current-buffer)))))
            (should (equal (gethash "encrypted_content"
                                    (aref (gethash "input" request) 0))
                           (gethash "encrypted_content" item)))
            (should (equal (gethash "include" request)
                           ["reasoning.encrypted_content"]))
            (should (equal (gethash "encrypted_content"
                                    (cadr (plist-get (car saved-messages)
                                                     :protocol-items)))
                           (gethash "encrypted_content" item))))
          (dolist (file (directory-files directory t "^[^.].*"))
            (let ((text (ende-debug-test--read-bytes file)))
              (should-not (string-match-p "secret-cache-identity\\|response-only-replay" text))))
          (should (equal (json-encode body) original-body))
          (should (equal (prin1-to-string messages) original-messages)))
      (delete-directory directory t))))

(ert-deftest ende-debug-reasoning-redaction-is-scoped ()
  (let* ((ciphertext "opaque-encrypted_content-prompt_cache_key+/=")
         (object (ende-protocol-json-read-string
                  (format "{\"encrypted_content\":%s}" (json-encode ciphertext))))
         (values (list object (list (cons :encrypted_content ciphertext))
                       (cons 'encrypted_content ciphertext)
                       (list :encrypted_content ciphertext :other "kept")
                       (vector object)
                       "reasoning.encrypted_content")))
    (dolist (value values)
      (let ((original (prin1-to-string value)))
        (should (equal (prin1-to-string (ende-debug--redact value)) original))
        (should-not (equal (ende-content-redact value) value))
        (should-not (string-match-p
                     "opaque-encrypted_content"
                     (prin1-to-string (ende-content-redact value))))
        (should (equal (prin1-to-string value) original))))
    (let* ((body `((:input . [,object])
                   (:include . ["reasoning.encrypted_content"])))
           (bytes (encode-coding-string
                   (concat "  " (json-encode body) "\n") 'utf-8))
           (directory (make-temp-file "ende-debug-reasoning-" t))
           (ende-debugging-enabled t)
           (ende-debug--session-directory directory)
           (ende-debug--sequence 0))
      (unwind-protect
          (progn
            (should (ende-debug-record-request
                     bytes body nil nil "test" "https://example.test"))
            (should (equal (ende-debug-test--read-bytes
                            (expand-file-name "0001-request.json" directory))
                           bytes)))
        (delete-directory directory t)))))

(ert-deftest ende-debug-weighted-reuse-and-missing-counts ()
  (let* ((directory (make-temp-file "ende-cache-weighted-" t))
         (ende-debugging-enabled t)
         (ende-debug--session-directory directory)
         (ende-debug--sequence 0)
         (request '((:model . "test") (:input . []))))
    (unwind-protect
        (progn
          (dolist (usage '((:input_tokens 1000 :output_tokens 10
                           :input_tokens_details (:cached_tokens 900) :cache_write_tokens 0)
                          (:input_tokens 100 :output_tokens 10
                           :input_tokens_details (:cached_tokens 0))
                          (:input_tokens 500 :output_tokens 0)))
            (ende-debug-test--record-synthetic request 1 usage))
          (let* ((captures (ende-debug--read-captures directory))
                 (report (ende-debug--format-report directory captures))
                 (zero (gethash "completion" (ende-debug-test--read-meta directory 2)))
                 (missing (gethash "completion" (ende-debug-test--read-meta directory 3))))
            (should (string-match-p "81.8% (900/1100 tokens; 2 requests with both counts)" report))
            (should (string-match-p "Cache write: 0" report))
            (should (= (gethash "cached_input_tokens" zero) 0))
            (should (eq (gethash "cached_tokens_reported" zero) t))
            (should-not (gethash "cached_input_tokens" missing))
            (should (eq (gethash "cached_tokens_reported" missing) :false))
            (should (eq (gethash "cache_write_tokens_reported" zero) :false))))
      (delete-directory directory t))))

(ert-deftest ende-debug-protected-data-and-stable-fingerprints ()
  (let* ((directory (make-temp-file "ende-cache-secrets-" t))
         (ende-debugging-enabled t)
         (ende-debug--session-directory directory)
         (ende-debug--sequence 0)
         (body '((:prompt_cache_key . "secret-cache")
                 (:authorization . "secret-request-auth")
                 (:include . ["reasoning.encrypted_content"])
                 (:input . []) (:stream . t)))
         (profile '(:codex-session-id "secret-session"))
         (messages '((:authorization "secret-auth" :access-token "secret-access"
                      :refresh_token "secret-refresh" :session-id "secret-session"
                      :id-token "secret-id" :encrypted_content "secret-reasoning")
                     ("user" . "reasoning.encrypted_content Authorization: Bearer secret-inline"))))
    (unwind-protect
        (progn
          (dotimes (_ 2)
            (ende-debug-record-request (json-encode body) body messages profile "test" nil))
          (should (equal (gethash "session_fingerprint" (ende-debug-test--read-meta directory 1))
                         (gethash "session_fingerprint" (ende-debug-test--read-meta directory 2))))
          (should (stringp (gethash "session_fingerprint" (ende-debug-test--read-meta directory 1))))
          (dolist (session '(nil ""))
            (ende-debug-record-request
             (json-encode body) body messages (list :codex-session-id session) "test" nil)
            (should-not (gethash "session_fingerprint"
                                 (ende-debug-test--read-meta directory ende-debug--sequence))))
          (dolist (file (directory-files directory t "^[^.].*"))
            (let ((text (ende-debug-test--read-bytes file)))
              (dolist (secret '("secret-cache" "secret-session" "secret-request-auth"
                                "secret-auth" "secret-access" "secret-refresh"
                                "secret-id" "secret-inline"))
                (should-not (string-match-p (regexp-quote secret) text)))))
          (should (string-match-p
                   "secret-reasoning"
                   (ende-debug-test--read-bytes
                    (expand-file-name "0001-messages.el" directory)))))
      (delete-directory directory t))
    (dolist (entry '(("Authorization: Bearer secret-auth" . "secret-auth")
                     ("session-id: secret-session" . "secret-session")
                     ("access_token=secret-access" . "secret-access")
                     ("refresh_token: secret-refresh" . "secret-refresh")
                     ("id_token=secret-id" . "secret-id")
                     ("prompt_cache_key: secret-cache" . "secret-cache")
                     ("encrypted_content: secret-reasoning" . "secret-reasoning")))
      (let* ((text (car entry))
             (secret (regexp-quote (cdr entry)))
             (event (make-hash-table :test #'equal))
             (error (make-hash-table :test #'equal))
             (session (ende-codex--make-session)) received)
        (puthash "message" text error)
        (puthash "type" "error" event)
        (puthash "error" error event)
        (should-not (string-match-p secret (ende-content-redact text)))
        (should-not (string-match-p secret (ende-transport-safe-error-text text)))
        (setq session (ende-codex--make-session
                       :callbacks (list :on-error (lambda (value) (setq received value)))))
        (ende-codex--handle-event session event)
        (should received)
        (should-not (string-match-p secret (prin1-to-string received)))))))

(ert-deftest ende-debug-structural-fingerprints-represent-json-values ()
  (let ((fingerprint (lambda (item)
                       (cdr (assoc "items" (ende-debug--input-fingerprints
                                           (concat "{\"input\":[" item "]}")))))))
    (should (equal (funcall fingerprint "{\"a\":[true,false,null,{},[]],\"b\":{\"x\":\"世界\\n café\",\"y\":1}}")
                   (funcall fingerprint "{\"b\":{\"y\":1,\"x\":\"世界\\n café\"},\"a\":[true,false,null,{},[]]}")))
    (let* ((values '("null" "false" "true" "{}" "[]" "{\"a\":null}"
                     "[1,2]" "[2,1]" "\"世界\\n café\"" "\"世界 café\""))
           (entries (mapcar (lambda (value) (aref (funcall fingerprint value) 0)) values)))
      (should (= (length entries)
                 (length (delete-dups (mapcar (lambda (entry) (cdr (assoc "sha256" entry))) entries))))))
    (let ((entry (aref (funcall fingerprint "\"世界\\n\"") 0))
          (bytes (encode-coding-string (json-encode "世界\n") 'utf-8)))
      (should (= (cdr (assoc "byte_length" entry)) (length bytes))))))

(ert-deftest ende-debug-original-prefix-survives-redaction-and-appended-reasoning ()
  (let* ((directory (make-temp-file "ende-debug-originals-" t))
         (ende-debugging-enabled t)
         (ende-debug--session-directory directory)
         (ende-debug--sequence 0)
         (reasoning '((:type . "reasoning") (:id . "rs-input") (:encrypted_content . "opaque")))
         (tool '((:type . "function_call_output") (:call_id . "call-input")
                 (:output . "Authorization: Bearer original")))
         (changed '((:type . "function_call_output") (:call_id . "call-input")
                    (:output . "Authorization: Bearer changed"))))
    (unwind-protect
        (progn
          (dolist (items (list (vector reasoning tool) (vector reasoning changed)
                              (vector reasoning changed reasoning)))
            (ende-debug-test--record-synthetic `((:input . ,items)) 1 nil))
          (let* ((captures (ende-debug--read-captures directory))
                 (collision (ende-debug--pair-report (nth 0 captures) (nth 1 captures)))
                 (grown (ende-debug--pair-report (nth 1 captures) (nth 2 captures))))
            (should (ende-debug--json-equal-p (plist-get (nth 0 captures) :request)
                                             (plist-get (nth 1 captures) :request)))
            (should (string-match-p "input\\[1\\] (contents redacted)" collision))
            (should (string-match-p "previous original input prefix was preserved" grown))
            (should (string-match-p "1 appended (1 reasoning)" grown))
            (should (string-match-p "First appended item: input\\[2\\]" grown))
            (remhash "input_fingerprints" (plist-get (nth 0 captures) :metadata))
            (should (string-match-p "cannot establish equality of redacted originals"
                                    (ende-debug--pair-report (nth 0 captures) (nth 1 captures))))))
      (delete-directory directory t))))

(provide 'ende-debug-test)

;;; ende-debug-test.el ends here
