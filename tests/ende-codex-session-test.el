;;; ende-codex-session-test.el --- Session affinity -*- lexical-binding: t; -*-

(require 'ert)
(require 'ende-test-helpers)
(require 'ende-codex-auth-test)
(require 'ende-chat)
(require 'ende-agent)

(ert-deftest ende-codex-session-identity-and-account-isolation ()
  (let ((first (ende-codex--session-id-for-identity "root-a" 'personal)))
    (should (equal first (ende-codex--session-id-for-identity "root-a" 'personal)))
    (should-not (equal first (ende-codex--session-id-for-identity "root-b" 'personal)))
    (should-not (equal first (ende-codex--session-id-for-identity "root-a" 'work)))
    (should (string-match-p "\\`[0-9a-f]\\{64\\}\\'" first))))

(ert-deftest ende-codex-session-job-fallback-and-child-isolation ()
  (with-temp-buffer
    (let* ((target (ende-target-create :buffer (current-buffer) :start 1 :end 1 :cursor 1))
           (profile (ende-codex-auth-test--profile 'personal))
           (first (ende--job-from-request (list 'insert :target target :profile profile)))
           (second (ende--job-from-request (list 'insert :target target :profile profile)))
           (id (plist-get (ende-job-profile first) :codex-session-id))
           (family (ende--make-job-family :inference-template (ende-job-inference-template first)))
           (child-profile (ende-agent-profile-for-policy family nil)))
      (unwind-protect
          (progn
            (should id)
            (dotimes (_round 3)
              (should (equal id (plist-get (ende-codex--ensure-session-id
                                           (ende-job-profile first)) :codex-session-id))))
            (should-not (equal id (plist-get (ende-job-profile second) :codex-session-id)))
            (should-not (plist-get profile :codex-session-id))
            (should-not (plist-get child-profile :codex-session-id))
            (should-not (equal id (plist-get (ende-codex--ensure-session-id child-profile)
                                            :codex-session-id))))
        (ende-target-release target)))))

(defun ende-codex-session-test--enqueue (text heading &optional account compact)
  "Queue HEADING in Org TEXT without dispatch; return profile and saved text."
  (with-temp-buffer
    (insert text)
    (org-mode)
    (goto-char (point-min))
    (search-forward heading)
    (beginning-of-line)
    (let ((document (ende-chat--build-document)) captured)
      (cl-letf (((symbol-function 'ende-enqueue)
                 (lambda (request)
                   (setq captured (plist-get (cdr request) :profile))
                   (ende--make-job :id 1)))
                ((symbol-function 'ende-process-next) #'ignore))
        (ende-chat--enqueue document (ende-chat--headline-at-point document)
                            (ende-codex-auth-test--profile (or account 'personal)) compact))
      (list captured (buffer-string)))))

(ert-deftest ende-codex-session-org-followup-and-reload ()
  (let* ((first (ende-codex-session-test--enqueue "* Root\n** First\nQuestion\n" "** First"))
         (id (plist-get (car first) :codex-session-id))
         ;; A new Org buffer reparses the persisted text, just as after reload.
         (followup (ende-codex-session-test--enqueue
                    (concat (cadr first) "\n** Followup\nQuestion\n") "** Followup"))
         (independent (ende-codex-session-test--enqueue "* Other\n** First\nQuestion\n" "** First")))
    (should (equal id (plist-get (car followup) :codex-session-id)))
    (should-not (equal id (plist-get (car independent) :codex-session-id)))))

(ert-deftest ende-codex-session-explicit-key-preserves-header ()
  (let* ((profile (append (ende-codex-auth-test--profile 'personal)
                          '(:codex-session-id "stable-test-session"
                            :parameters (:prompt_cache_key "explicit-cache-key"))))
         (stream "data: {\"type\":\"response.completed\",\"response\":{\"output\":[]}}\n\n")
         (messages '(("user" . "hello")))
         (original (ende-codex--request-body messages profile)))
    (dotimes (_round 3)
      (let ((result (ende-test--run-stream-request 'codex (list stream) nil nil profile nil messages)))
        (should-not (plist-get result :error))
        (should (string-match-p "session-id: stable-test-session" (plist-get result :config-text)))
        (should (equal (ende-protocol-json-get
                        "prompt_cache_key" (ende-protocol-json-read-string
                                            (plist-get result :request-body)))
                       "explicit-cache-key"))
        (should-not (string-match-p "stable-test-session" (prin1-to-string (plist-get result :command))))))
    (should (equal (json-encode original)
                   (json-encode (ende-codex--request-body
                                 messages (plist-put (copy-tree profile) :codex-session-id "different")))))
    (should (equal (cdr (assoc "prompt_cache_key" original)) "explicit-cache-key"))
    (should (eq (alist-get :store original) json-false))
    (should (eq (alist-get :stream original) t))
    (let ((public (ende-test--run-stream-request 'chat '("data: [DONE]\n\n"))))
      (should-not (string-match-p "session-id:" (or (plist-get public :config-text) ""))))))

(ert-deftest ende-codex-session-automatic-serialized-affinity ()
  (let* ((profile (ende-codex--ensure-session-id
                   (ende-codex-auth-test--profile 'personal)))
         (id (plist-get profile :codex-session-id))
         (stream "data: {\"type\":\"response.completed\",\"response\":{\"output\":[]}}\n\n"))
    (dotimes (_round 3)
      (let* ((result (ende-test--run-stream-request 'codex (list stream) nil nil profile))
             (wire (plist-get result :request-body))
             (body (ende-protocol-json-read-string wire)))
        (should-not (plist-get result :error))
        (should (string-match-p (concat "session-id: " id) (plist-get result :config-text)))
        (should (equal (gethash "prompt_cache_key" body) id))
        (should (= (ende-test--count-matches "\"prompt_cache_key\"[[:space:]]*:" wire) 1))
        (should (eq (gethash "store" body) :false))
        (should (eq (gethash "stream" body) t))
        (should (equal (gethash "include" body) ["reasoning.encrypted_content"]))))
    (dolist (parameters '((:prompt_cache_key "keyword")
                          ("prompt_cache_key" "string")))
      (let* ((copy (plist-put (copy-tree profile) :parameters parameters))
             (result (ende-test--run-stream-request 'codex (list stream) nil nil copy))
             (wire (plist-get result :request-body)))
        (should-not (plist-get result :error))
        (should (equal (ende-protocol-json-get "prompt_cache_key"
                                              (ende-protocol-json-read-string wire))
                       (car (last parameters))))
        (should (= (ende-test--count-matches "\"prompt_cache_key\"[[:space:]]*:" wire) 1))
        (should (string-match-p (concat "session-id: " id) (plist-get result :config-text)))))
    (let* ((other (ende-codex--ensure-session-id
                   (plist-put (copy-tree profile) :account 'work)))
           (result (ende-test--run-stream-request 'codex (list stream) nil nil other)))
      (should-not (equal id (plist-get other :codex-session-id)))
      (should (equal (ende-protocol-json-get "prompt_cache_key"
                                            (ende-protocol-json-read-string
                                             (plist-get result :request-body)))
                     (plist-get other :codex-session-id))))))

(ert-deftest ende-codex-session-direct-and-public-provider-isolation ()
  (let* ((profile (ende-codex-auth-test--profile 'personal))
         (stream "data: {\"type\":\"response.completed\",\"response\":{\"output\":[]}}\n\n")
         (result (ende-test--run-stream-request 'codex (list stream) nil nil profile))
         (key (ende-protocol-json-get "prompt_cache_key"
                                      (ende-protocol-json-read-string
                                       (plist-get result :request-body)))))
    (should (stringp key))
    (should (string-match-p (concat "session-id: " key) (plist-get result :config-text)))
    (should-not (plist-get profile :codex-session-id))
    (dolist (transport '(responses chat messages))
      (let* ((public (list :name 'public :service 'openai :model "test-model"
                           :api (pcase transport
                                  ('responses 'openai-responses)
                                  ('messages 'anthropic-messages)
                                  (_ 'openai-chat-completions))
                           :auth-method 'none :url "https://example.test/v1/responses"
                           :output-tokens 100))
             (request (ende-test--run-stream-request transport (list stream) nil nil public)))
        (should-not (string-match-p "session-id:" (or (plist-get request :config-text) "")))
        (should-not (string-match-p "prompt_cache_key" (or (plist-get request :request-body) "")))))))

(ert-deftest ende-codex-session-org-compaction-fence-and-accounts ()
  (let* ((first (ende-codex-session-test--enqueue
                 "* Root\n** First\nQuestion\n" "** First"))
         (id (plist-get (car first) :codex-session-id))
         (text (concat (cadr first) "\n** Compact\nSummarize.\n"))
         (compacted (ende-codex-session-test--enqueue text "** Compact" 'personal t))
         (work (ende-codex-session-test--enqueue text "** Compact" 'work t))
         (fenced (ende-codex-session-test--enqueue
                  (concat (cadr first)
                          "\n** Summary\n:PROPERTIES:\n:ENDE_CHAT_CONTEXT_FENCE: t\n:END:\nSummary.\n** Followup\nContinue.\n")
                  "** Followup")))
    (should (equal id (plist-get (car compacted) :codex-session-id)))
    (should (equal id (plist-get (car fenced) :codex-session-id)))
    (should-not (equal id (plist-get (car work) :codex-session-id)))))

(ert-deftest ende-codex-session-queued-parent-child-wire-rounds ()
  (with-temp-buffer
    (let* ((target (ende-target-create :buffer (current-buffer) :start 1 :end 1 :cursor 1))
           (template (ende-codex-auth-test--profile 'personal))
           (parent (ende--job-from-request (list 'insert :target target :profile template)))
           (family (ende--make-job-family :inference-template (ende-job-inference-template parent)))
           (inherited (ende-agent-profile-for-policy family nil))
           (child (ende--job-from-request (list 'insert :target target :profile inherited)))
           (id (plist-get (ende-job-profile parent) :codex-session-id))
           (child-id (plist-get (ende-job-profile child) :codex-session-id))
           (stream "data: {\"type\":\"response.completed\",\"response\":{\"output\":[]}}\n\n"))
      (unwind-protect
          (progn
            (should-not (equal id child-id))
            (should (eq (plist-get (ende-job-profile parent) :codex-binding)
                        (plist-get (ende-job-profile child) :codex-binding)))
            ;; Credentials are synthetic; this test verifies queue snapshots and
            ;; serialized rounds, not the separately tested credential store.
            (cl-letf (((symbol-function 'ende-codex--check-binding) #'ignore)
                      ((symbol-function 'ende-codex--profile-auth)
                       (lambda (_profile) '(:account personal))))
              (dolist (job (list parent child))
                (let ((profile (ende-job-profile job)) (messages '(("user" . "hello"))))
                  (dotimes (round 3)
                    (let* ((result (ende-test--run-stream-request 'codex (list stream) nil nil profile nil messages))
                           (key (plist-get profile :codex-session-id)))
                      (should-not (plist-get result :error))
                      (should (equal (ende-protocol-json-get "prompt_cache_key"
                                                            (ende-protocol-json-read-string
                                                             (plist-get result :request-body))) key))
                      (should (string-match-p (concat "session-id: " key) (plist-get result :config-text)))
                      (setq messages
                            (append messages
                                    (list (list :role "assistant" :content ""
                                                :tool-calls (list (list :id (format "call-%d" round)
                                                                        :name "read_file" :arguments "{}")))
                                          (list :role "tool" :tool-call-id (format "call-%d" round)
                                                :content "result")))))))))
            (should-not (plist-get template :codex-session-id))
            (should-not (plist-get inherited :codex-session-id)))
        (ende-target-release target)))))

(ert-deftest ende-codex-session-request-invariants-and-merged-override ()
  (let* ((profile (ende-codex--ensure-session-id (ende-codex-auth-test--profile 'personal)))
         (stream "data: {\"type\":\"response.completed\",\"response\":{\"output\":[]}}\n\n")
         (parameters (ende-provider-config-merge-parameters
                      '(:prompt_cache_key "old") '("prompt_cache_key" "override" :include ["message.output_text.logprobs"])))
         (request (plist-put (copy-tree profile) :parameters parameters))
         (result (ende-test--run-stream-request 'codex (list stream) nil nil request))
         (wire (plist-get result :request-body))
         (body (ende-protocol-json-read-string wire)))
    (should-not (plist-get result :error))
    (should (= (ende-test--count-matches "\"prompt_cache_key\"[[:space:]]*:" wire) 1))
    (should (= (ende-test--count-matches "\"include\"[[:space:]]*:" wire) 1))
    (should (equal (gethash "prompt_cache_key" body) "override"))
    (should (equal (gethash "include" body)
                   ["message.output_text.logprobs" "reasoning.encrypted_content"]))
    (dolist (parameters '((:previous_response_id "resp-old") (:store t) (:stream :json-false)
                          (:temperature 0.5) (:max_output_tokens 100)))
      (should-error (ende-codex--request-body
                     '(("user" . "hello"))
                     (plist-put (copy-tree profile) :parameters parameters))))
    (let* ((bad (plist-put (copy-tree profile) :request-headers '("Session-ID: manual")))
           (session (ende-codex--make-session :profile bad)))
      (should-error (ende-codex--start-prepared session '(:access-token "synthetic") "{}" nil nil)))))

(provide 'ende-codex-session-test)
;;; ende-codex-session-test.el ends here
