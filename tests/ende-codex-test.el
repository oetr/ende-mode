;;; ende-codex-test.el --- Codex provider tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Coverage for OpenAI Responses, OAuth, quota, and model probing.

;;; Code:

(require 'ert)
(require 'ende)
(require 'ende-test-helpers)
(require 'cl-lib)
(require 'json)
(require 'ende-chat-test-helpers)



(ert-deftest ende-test/openai-tool-diagnostics-are-safe-and-configurable ()
  (should (eq ende-tool-diagnostics t))
  (let* ((definition
          (cl-find-if
           (lambda (candidate)
             (equal (alist-get :name (alist-get :function candidate))
                    "read_file"))
           (ende-tool-definitions)))
         (body (ende-codex--request-body
                (list (cons "user" "question"))
                (list :model "gpt-test"
                      :tool-definitions (list definition))))
         (events nil)
         (session
          (ende-codex--make-session
           :profile '(:model "gpt-test")
           :callbacks (list :on-event
                            (lambda (event) (push event events)))
           :tool-calls (make-hash-table :test #'eql))))
    (ende-codex--diagnose-request session body)
    (ende-codex--handle-event
     session
     (ende-protocol-json-read-string
      (concat
       "{\"type\":\"response.completed\",\"response\":{\"output\":["
       "{\"type\":\"function_call\",\"call_id\":\"call-1\","
       "\"name\":\"read_file\",\"arguments\":"
       "\"{\\\"path\\\":\\\"secret.el\\\"}\"}]}}")))
    (let ((diagnostics
           (mapcar (lambda (event) (plist-get event :text))
                   (cl-remove-if-not
                    (lambda (event)
                      (eq (plist-get event :type) :diagnostic))
                    (nreverse events)))))
      (should (equal diagnostics
                     '("OpenAI Responses tools sent: read_file"
                       "OpenAI Responses output items: function_call(read_file)")))
      (should-not (string-match-p "secret\\.el"
                                  (mapconcat #'identity diagnostics "\n")))))
  (let ((ende-tool-diagnostics nil)
        (events nil))
    (let ((codex-session
           (ende-codex--make-session
            :callbacks (list :on-event
                             (lambda (event) (push event events)))))
          (chat-session
           (ende-llm--make-session
            :callbacks (list :on-event
                             (lambda (event) (push event events)))
            :tool-calls (make-hash-table :test #'eql))))
      (ende-codex--diagnose-request codex-session '((:tools . nil)))
      (ende-llm--diagnose-request chat-session '((:tools . nil)))
      (ende-codex--diagnose-output-item
       codex-session "response.output_item.done"
       (ende-protocol-json-read-string
        "{\"type\":\"function_call\",\"name\":\"read_file\"}"))
      (ende-llm--diagnose-result chat-session)
      (should-not events))))



(ert-deftest ende-test/openai-streamed-function-call-is-collected-without-final-output ()
  (let* ((events nil)
         (clock 5.0)
         (session
          (ende-codex--make-session
           :profile '(:model "gpt-test")
           :callbacks (list :on-event
                            (lambda (event) (push event events))))))
    (cl-letf (((symbol-function 'float-time) (lambda (&optional _) clock)))
      (dolist
          (event
           (list
            (concat
             "{\"type\":\"response.output_item.added\","
             "\"output_index\":0,\"item\":{\"id\":\"fc_1\","
             "\"type\":\"function_call\",\"call_id\":\"call-1\","
             "\"name\":\"read_file\",\"arguments\":\"\"}}")
            (concat
             "{\"type\":\"response.function_call_arguments.delta\","
             "\"output_index\":0,\"item_id\":\"fc_1\","
             "\"delta\":\"\"}")
            (concat
             "{\"type\":\"response.function_call_arguments.delta\","
             "\"output_index\":0,\"item_id\":\"fc_1\","
             "\"delta\":\"{\\\"path\\\":\"}")
            (concat
             "{\"type\":\"response.function_call_arguments.delta\","
             "\"output_index\":0,\"item_id\":\"fc_1\","
             "\"delta\":\"\\\"ende.el\\\"}\"}")
            (concat
             "{\"type\":\"response.function_call_arguments.done\","
             "\"output_index\":0,\"item_id\":\"fc_1\","
             "\"arguments\":\"{\\\"path\\\":\\\"ende.el\\\"}\"}")
            (concat
             "{\"type\":\"response.output_item.done\","
             "\"output_index\":0,\"item\":{\"id\":\"fc_1\","
             "\"type\":\"function_call\",\"call_id\":\"call-1\","
             "\"name\":\"read_file\","
             "\"arguments\":\"{\\\"path\\\":\\\"ende.el\\\"}\"}}")
            (concat
             "{\"type\":\"response.completed\",\"response\":{"
             "\"output\":[],\"usage\":{\"output_tokens\":40}}}")))
        (when (and (string-match-p "response.function_call_arguments.delta" event)
                   (not (string-match-p "\\\"delta\\\":\\\"\\\"" event)))
          (setq clock (if (ende-stream-session-generation-start
                           (ende-codex--session-stream session))
                          11.0
                        10.0)))
        (when (string-match-p "response.completed" event)
          (setq clock 12.0))
        (ende-codex--handle-event
         session (ende-protocol-json-read-string event))
        (when (or (string-match-p "response.output_item.added" event)
                  (string-match-p "\\\"delta\\\":\\\"\\\"" event))
          (should-not (ende-stream-session-generation-start
                       (ende-codex--session-stream session)))))
      (let* ((result (ende-codex--result session))
             (call (car (plist-get result :tool-calls)))
             (usage-event
              (cl-find-if (lambda (event)
                            (eq (plist-get event :type) :usage))
                          events))
             (diagnostics
              (mapcar (lambda (event) (plist-get event :text))
                      (cl-remove-if-not
                       (lambda (event)
                         (eq (plist-get event :type) :diagnostic))
                       (nreverse events)))))
        (should (= (ende-stream-session-generation-start
                    (ende-codex--session-stream session))
                   10.0))
        (should (= (plist-get usage-event :speed) 20.0))
        (should (= (plist-get result :speed) 20.0))
        (should (equal (plist-get call :id) "call-1"))
        (should (equal (plist-get call :name) "read_file"))
        (should (equal (plist-get call :arguments)
                       "{\"path\":\"ende.el\"}"))
        (should (member
                 "OpenAI Responses response.output_item.added: function_call(read_file)"
                 diagnostics))
        (should (member
                 "OpenAI Responses response.output_item.done: function_call(read_file)"
                 diagnostics))
        (should (member "OpenAI Responses output items: (none)" diagnostics))
        (should-not (string-match-p "ende\\.el"
                                    (mapconcat #'identity diagnostics "\n")))))))

(ert-deftest ende-test/codex-session-clears-resource-slots ()
  (let ((process-buffer (generate-new-buffer " *ende-test-codex-process*"))
        (stderr-buffer (generate-new-buffer " *ende-test-codex-stderr*"))
        (headers-file (make-temp-file "ende-test-codex-headers-"))
        (config-file (make-temp-file "ende-test-codex-config-")))
    (let ((session
           (ende-codex--make-session
            :callbacks nil)))
      (ende-transport-session-set-resources
       (ende-codex--lifecycle session)
       (list :response-buffer process-buffer
             :stderr-buffer stderr-buffer
             :headers-file headers-file :config-file config-file))
      (ende-codex--fail
       session (ende-error-create :kind 'transport :message "failed"))
      (should-not (ende-transport-session-process
                   (ende-codex--lifecycle session)))
      (should-not (ende-transport-session-process-buffer
                   (ende-codex--lifecycle session)))
      (should-not (ende-transport-session-stderr-buffer
                   (ende-codex--lifecycle session)))
      (should-not (ende-transport-session-stderr-process
                   (ende-codex--lifecycle session)))
      (should-not (ende-transport-session-headers-file
                   (ende-codex--lifecycle session)))
      (should-not (ende-transport-session-config-file
                   (ende-codex--lifecycle session)))
      (should-not (buffer-live-p process-buffer))
      (should-not (buffer-live-p stderr-buffer))
      (should-not (file-exists-p headers-file))
      (should-not (file-exists-p config-file)))))



(ert-deftest ende-test/openai-profile-uses-opencode-oauth ()
  (let ((profile (ende-provider-profile 'openai)))
    (should (eq (plist-get profile :wire-api) 'responses))
    (should (eq (plist-get profile :auth-method) 'opencode-oauth))
    (should (string= (plist-get profile :model) "gpt-5.6-luna"))
    (should (string= (plist-get profile :url)
                     "https://chatgpt.com/backend-api/codex/responses"))
    (should-not (ende-provider-configured-model profile))
    (should-not (plist-get (ende-provider-profile-snapshot 'openai) :access))
    (should-not (plist-get (ende-provider-profile-snapshot 'openai) :refresh)))
  (let ((ende-codex-model "gpt-configured"))
    (should (string= (ende-provider-configured-model
                      (ende-provider-profile 'openai))
                     "gpt-configured"))))



(ert-deftest ende-test/openai-model-probe-parses-official-slugs ()
  (should (equal
           (ende-codex--parse-models
            (concat "{\"models\":[{\"slug\":\"gpt-5.6-luna\"},"
                    "{\"slug\":\"gpt-5.5\"},{\"title\":\"ignored\"}]}"))
           '("gpt-5.6-luna" "gpt-5.5"))))



(ert-deftest ende-test/openai-model-probe-response-is-bounded ()
  (let ((ende-provider-max-discovery-bytes 4)
        (captured nil)
        (process nil)
        (result nil)
        (real-make-process (symbol-function 'make-process)))
    (cl-letf (((symbol-function 'ende-codex--resolve-auth)
               (lambda (callback)
                 (funcall callback '(:access "access-token"))
                 nil))
              ((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq captured args
                       process
                       (funcall real-make-process
                                :name (plist-get args :name)
                                :buffer nil
                                :command (list "cat")
                                :connection-type 'pipe)))))
      (unwind-protect
          (progn
            (ende-codex-probe-models
             (lambda (models error)
               (setq result (list models error))))
            (should (process-live-p process))
            (funcall (plist-get captured :filter) process "oversized")
            (should (equal (car result) nil))
            (should (ende-error-p (cadr result)))
            (should (eq (ende-error-kind (cadr result)) 'resource))
            (should-not (process-live-p process))
            (should-not ende-codex--model-probe-processes))
        (when (and (processp process) (process-live-p process))
          (delete-process process))
        (setq ende-codex--model-probe-processes nil)))))



(ert-deftest ende-test/openai-quota-parser-and-display-are-window-aware ()
  (let* ((now 2000000000)
         (quota
          (ende-codex--parse-quota
           (concat
            "{\"rate_limit\":{"
            "\"primary_window\":{\"used_percent\":28,"
            "\"reset_at\":2000018660,\"limit_window_seconds\":18000},"
            "\"secondary_window\":{\"used_percent\":59,"
            "\"reset_at\":2000007980,\"limit_window_seconds\":604800}}}"))))
    (should (= (plist-get (plist-get quota :primary) :used-percent) 28))
    (should (= (plist-get (plist-get quota :secondary) :used-percent) 59))
    (should
     (string=
      (ende-codex-quota-string quota now)
      "5h 11m: 72% left · weekly: 41% left · reset in 2h 13m"))
    (should-error
     (ende-codex--parse-quota
      "{\"rate_limit\":{\"primary_window\":null,\"secondary_window\":null}}"))))



(ert-deftest ende-test/openai-quota-display-follows-secondary-reset ()
  (let ((now 2000000000)
        (quota
         '(:primary (:used-percent 10
				   :reset-at 2000003600
				   :window-seconds 18000)
		    :secondary (:used-percent 20
					      :reset-at 2000021600
					      :window-seconds 604800))))
    (should
     (string=
      (ende-codex-quota-string quota now)
      "1h: 90% left · weekly: 80% left · reset in 6h"))
    (should
     (string=
      (ende-codex-quota-string (list :primary (plist-get quota :primary)) now)
      "1h: 90% left"))))



(ert-deftest ende-test/openai-quota-probe-uses-account-scoped-oauth ()
  (let ((captured nil)
        (config-text nil)
        (process nil)
        (result nil)
        (failure nil)
        (real-make-process (symbol-function 'make-process)))
    (cl-letf (((symbol-function 'ende-codex--resolve-auth)
               (lambda (callback)
                 (funcall callback
                          '(:access "access-token" :account-id "account-id"))
                 nil))
              ((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq captured args)
                 (let ((config
                        (cadr (member "--config"
                                      (plist-get args :command)))))
                   (when config
                     (with-temp-buffer
                       (insert-file-contents config)
                       (setq config-text (buffer-string)))))
                 (setq process
                       (funcall real-make-process
                                :name (plist-get args :name)
                                :buffer nil
                                :command '("cat")
                                :connection-type 'pipe)))))
      (unwind-protect
          (progn
            (ende-codex-probe-quota
             (lambda (quota error)
               (setq result quota
                     failure error)))
            (let* ((command (plist-get captured :command))
                   (headers-file
                    (cadr (member "--dump-header" command))))
              (with-temp-file headers-file
                (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n"))
              (funcall
               (plist-get captured :filter)
               process
               "{\"rate_limit\":{\"primary_window\":{\"used_percent\":22}}}")
              (process-send-eof process)
              (while (process-live-p process)
                (accept-process-output process 0.05))
              (funcall (plist-get captured :sentinel) process "finished\n"))
            (should result)
            (should-not failure)
            (should (string-match-p
                     "Authorization: Bearer access-token" config-text))
            (should (string-match-p
                     "ChatGPT-Account-Id: account-id" config-text))
            (should (member "Accept: application/json" (plist-get captured :command)))
            (should-not (member "access-token" (plist-get captured :command)))
            (should-not ende-codex--quota-probe-processes))
        (when (and (processp process) (process-live-p process))
          (delete-process process))))))



(ert-deftest ende-test/openai-request-body-matches-codex-responses-shape ()
  (let* ((body (ende-protocol-json-read-string
                (json-encode
                 (ende-codex--request-body
                  (list (cons "system" "rules")
                        (cons "user" "question")
                        (cons "assistant" "answer"))
                  '(:model "gpt-test")))))
         (input (ende-protocol-json-get "input" body))
         (user (aref input 1))
         (user-content (aref (ende-protocol-json-get "content" user) 0))
         (assistant (aref input 2))
         (assistant-content
          (aref (ende-protocol-json-get "content" assistant) 0)))
    (should (eq (ende-protocol-json-get "store" body) :false))
    (should (eq (ende-protocol-json-get "stream" body) t))
    (should (string= (ende-protocol-json-get "text" user-content)
                     "question"))
    (should (string= (ende-protocol-json-get "type" user-content)
                     "input_text"))
    (should (string= (ende-protocol-json-get "text" assistant-content)
                     "answer"))
    (should (string= (ende-protocol-json-get "type" assistant-content)
                     "output_text"))))

(ert-deftest ende-test/codex-job-affinity-is-stable-and-transport-only ()
  (let* ((directory (make-temp-file "ende-codex-affinity-debug-" t))
         (ende-debugging-enabled t)
         (ende-debug--session-directory directory)
         (ende-debug--sequence 0)
         (profile
          '(:name openai :type codex :wire-api responses
            :url "https://chatgpt.com/backend-api/codex/responses"
            :model "gpt-test" :require-sse t
            :tool-definitions
            (((:type . "function")
              (:function . ((:name . "inspect")
                            (:description . "Inspect a fixture")
                            (:parameters . ((:type . "object")))))))))
         (messages '(("system" . "Keep the existing instructions.")
                     ("user" . "Inspect the fixture.")))
         (chunk
          (concat "data: {\"type\":\"response.completed\","
                  "\"response\":{\"usage\":{\"input_tokens\":12,"
                  "\"output_tokens\":1}}}\n\n"))
         (buffer-a (generate-new-buffer " *ende-codex-job-a*"))
         (buffer-b (generate-new-buffer " *ende-codex-job-b*"))
         (target-a (with-current-buffer buffer-a
                     (insert "fixture A")
                     (ende-target-create :buffer buffer-a
                                         :start (point-min) :end (point-max)
                                         :cursor (point-min) :review-policy 'ignore)))
         (target-b (with-current-buffer buffer-b
                     (insert "fixture B")
                     (ende-target-create :buffer buffer-b
                                         :start (point-min) :end (point-max)
                                         :cursor (point-min) :review-policy 'ignore)))
         job-a job-b)
    (unwind-protect
        (progn
          (setq job-a
                (ende--job-from-request
                 (cons 'insert (list :target target-a :profile profile)))
                job-b
                (ende--job-from-request
                 (cons 'insert (list :target target-b :profile profile))))
          (let* ((session-a (plist-get (ende-job-profile job-a)
                                       :codex-session-id))
                 (session-b (plist-get (ende-job-profile job-b)
                                       :codex-session-id))
                 (round-a1
                  (ende-test--run-stream-request
                   'codex (list chunk) nil nil (ende-job-profile job-a)
                   nil messages))
                 (round-b1
                  (ende-test--run-stream-request
                   'codex (list chunk) nil nil (ende-job-profile job-b)
                   nil messages))
                 (round-a2
                  (ende-test--run-stream-request
                   'codex (list chunk) nil nil (ende-job-profile job-a)
                   nil messages))
                 (expected-body
                  (encode-coding-string
                   (json-encode (ende-codex--request-body
                                 messages (ende-job-profile job-a)))
                   'utf-8))
                 (report
                  (ende-debug--format-report
                   directory (ende-debug--read-captures directory))))
            (should (stringp session-a))
            (should (stringp session-b))
            (should-not (equal session-a session-b))
            (dolist (round (list round-a1 round-b1 round-a2))
              (should (equal (plist-get round :request-body) expected-body)))
            (dolist (round (list round-a1 round-a2))
              (should (string-match-p
                       (regexp-quote (concat "session-id: " session-a))
                       (plist-get round :config-text))))
            (should (string-match-p
                     (regexp-quote (concat "session-id: " session-b))
                     (plist-get round-b1 :config-text)))
            (should (string-match-p "Request #1 -> #3" report))
            (should (string-match-p "Session ID:      unchanged" report))))
      (when job-a (ende-target-release (ende-job-target job-a)))
      (when job-b (ende-target-release (ende-job-target job-b)))
      (when (buffer-live-p buffer-a) (kill-buffer buffer-a))
      (when (buffer-live-p buffer-b) (kill-buffer buffer-b))
      (delete-directory directory t))))

(ert-deftest ende-test/codex-chat-affinity-uses-durable-conversation-root ()
  (ende-test--reset)
  (let ((profile
         '(:name openai :type codex :wire-api responses
           :url "https://chatgpt.com/backend-api/codex/responses"
           :model "gpt-test"))
        root-job follow-up-job independent-job)
    (unwind-protect
        (cl-letf (((symbol-function 'ende-process-next) #'ignore))
          (ende-test--with-buffer
              (concat "* Main conversation\n"
                      "** First request\nInitial request.\n"
                      "** Follow-up\nContinue this conversation.\n"
                      "* Independent conversation\nA separate task.\n")
            (org-mode)
            (ende-test--chat-goto "** First request")
            (let* ((document (ende-chat--build-document))
                   (question (ende-chat--headline-at-point document)))
              (setq root-job (ende-chat--enqueue document question profile)))
            (let ((conversation-id
                   (save-excursion
                     (ende-test--chat-goto "* Main conversation")
                     (org-entry-get nil ende-chat-id-property))))
              (should (stringp conversation-id))
              (let* ((document (ende-chat--build-document))
                     (question (ende-chat--find-by-property
                                document ende-chat-id-property conversation-id)))
                (should-not
                 (string-match-p
                  (regexp-quote conversation-id)
                  (prin1-to-string (ende-chat--serialize document question)))))
              (ende-reload)
              (should
               (equal conversation-id
                      (save-excursion
                        (ende-test--chat-goto "* Main conversation")
                        (org-entry-get nil ende-chat-id-property))))
              (cl-letf (((symbol-function 'ende-process-next) #'ignore))
                (ende-test--chat-goto "** Follow-up")
                (let* ((document (ende-chat--build-document))
                       (question (ende-chat--headline-at-point document)))
                  (setq follow-up-job
                        (ende-chat--enqueue document question profile)))
                (ende-test--chat-goto "* Independent conversation")
                (let* ((document (ende-chat--build-document))
                       (question (ende-chat--headline-at-point document)))
                  (setq independent-job
                        (ende-chat--enqueue document question profile)))
                (should (equal
                         (plist-get (ende-job-profile root-job) :codex-session-id)
                         (plist-get (ende-job-profile follow-up-job)
                                    :codex-session-id)))
                (should-not
                 (equal (plist-get (ende-job-profile root-job) :codex-session-id)
                        (plist-get (ende-job-profile independent-job)
                                   :codex-session-id)))
                (let* ((worker-profile
                        (ende-agent-profile-for-policy
                         (ende-job-family root-job) nil))
                       (worker-buffer (generate-new-buffer
                                       " *ende-codex-worker-test*"))
                       (worker-target
                        (with-current-buffer worker-buffer
                          (insert "worker")
                          (ende-target-create
                           :buffer worker-buffer :start (point-min)
                           :end (point-max) :cursor (point-min)
                           :review-policy 'ignore)))
                       (worker-job
                        (ende--job-from-request
                         (cons 'agent (list :target worker-target
                                            :profile worker-profile)))))
                  (unwind-protect
                      (progn
                        (should-not
                         (equal (plist-get worker-profile :codex-session-id)
                                (plist-get (ende-job-profile root-job)
                                           :codex-session-id)))
                        (should-not
                         (equal (plist-get (ende-job-profile worker-job)
                                           :codex-session-id)
                                (plist-get (ende-job-profile root-job)
                                           :codex-session-id)))
                        (should-not
                         (equal (plist-get (ende-job-profile worker-job)
                                           :codex-session-id)
                                (plist-get (ende-job-profile independent-job)
                                           :codex-session-id))))
                    (ende-target-release (ende-job-target worker-job))
                    (kill-buffer worker-buffer)))))))
      (ende-test--reset))))



(ert-deftest ende-test/openai-http-error-keeps-provider-detail ()
  (let ((headers-file (make-temp-file "ende-openai-error-")))
    (unwind-protect
        (progn
          (with-temp-file headers-file
            (insert "HTTP/1.1 400 Bad Request\r\n\r\n"))
          (let ((session
                 (ende-codex--make-session)))
            (ende-stream-append-preview
             (ende-codex--session-stream session)
             "{\"detail\":\"invalid request body\"}")
            (ende-transport-session-set-resources
             (ende-codex--lifecycle session)
             (list :headers-file headers-file))
            (should (string= (ende-error-message
                              (ende-codex--http-error session))
                             "HTTP status 400: invalid request body"))))
      (when (file-exists-p headers-file)
        (delete-file headers-file)))))



(ert-deftest ende-test/openai-auth-entry-is-read-from-opencode-file ()
  (let* ((file (make-temp-file "ende-openai-auth-test-"))
         (ende-codex-auth-file file))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "{\"openai\":{\"type\":\"oauth\","
                    "\"access\":\"access-token\","
                    "\"refresh\":\"refresh-token\","
                    "\"expires\":9999999999999,"
                    "\"accountId\":\"account-id\"}}"))
          (let ((auth (ende-codex--auth-entry)))
            (should (string= (plist-get auth :access) "access-token"))
            (should (string= (plist-get auth :refresh) "refresh-token"))
            (should (string= (plist-get auth :account-id) "account-id"))))
      (when (file-exists-p file)
        (delete-file file)))))



(ert-deftest ende-test/openai-transport-keeps-token-out-of-command-arguments ()
  (let* ((auth-file (make-temp-file "ende-openai-auth-test-"))
         (ende-codex-auth-file auth-file)
         (captured nil)
         (process nil)
         (result nil)
         (failure nil)
         (real-make-process (symbol-function 'make-process)))
    (unwind-protect
        (progn
          (with-temp-file auth-file
            (insert "{\"openai\":{\"type\":\"oauth\","
                    "\"access\":\"access-token\","
                    "\"refresh\":\"refresh-token\","
                    "\"expires\":9999999999999,"
                    "\"accountId\":\"account-id\"}}"))
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest args)
                       (setq captured args)
                       (setq process
                             (funcall real-make-process
                                      :name (plist-get args :name)
                                      :buffer (plist-get args :buffer)
                                      :command (list "cat")
                                      :connection-type 'pipe)))))
            (ende-codex-default
             (list (cons "user" "body"))
             (list :on-done (lambda (value) (setq result value))
                   :on-error (lambda (value) (setq failure value)))
             (ende-provider-profile 'openai))
            (let* ((command (plist-get captured :command))
                   (config (cadr (member "--config" command)))
                   (headers-file (cadr (member "--dump-header" command)))
                   (filter (plist-get captured :filter))
                   (sentinel (plist-get captured :sentinel)))
              (should (not (member "access-token" command)))
              (should (member "Accept: text/event-stream" command))
              (should (file-exists-p config))
              (with-temp-file headers-file
                (insert "HTTP/1.1 200 OK\r\n\r\n"))
              (with-temp-buffer
                (insert-file-contents config)
                (should (string-match-p "Authorization: Bearer access-token"
                                        (buffer-string))))
              (should (equal (cadr (member "--user-agent" command))
                             ende-client-name))
              (funcall
               filter
               process
               (concat "data: {\"type\":\"response.output_text.delta\","
                       "\"delta\":\"answer\"}\n\n"
                       "data: {\"type\":\"response.completed\","
                       "\"response\":{\"model\":\"gpt-test\","
                       "\"usage\":{\"output_tokens\":7}}}"))
              (while (process-live-p process)
                (accept-process-output process 0.05))
              (funcall sentinel process "finished\n")
              (should-not failure)
              (should (string= (plist-get result :content) "answer"))
              (should (string= (plist-get result :model) "gpt-test"))
              (should (= (plist-get result :tokens) 7))
              (should (string= (plist-get result :finish-reason) "stop"))
              (should-not (file-exists-p config)))))
      (when (and (processp process) (process-live-p process))
        (delete-process process))
      (when (file-exists-p auth-file)
        (delete-file auth-file)))))



(ert-deftest ende-test/openai-rejects-explicit-non-sse-content-type ()
  (let ((headers-file (make-temp-file "ende-openai-headers-"))
        (process (make-process :name "ende-openai-test"
                               :command (list "true") :noquery t))
        (result nil)
        (failure nil)
        (events nil))
    (unwind-protect
        (let ((session
               (ende-codex--make-session
                :profile '(:model "gpt-test")
                :callbacks
                (list :on-event (lambda (event) (push event events))
                      :on-done (lambda (value) (setq result value))
                      :on-error (lambda (error) (setq failure error))))))
          (ende-transport-session-set-resources
           (ende-codex--lifecycle session)
           (list :process process :headers-file headers-file))
          (with-temp-file headers-file
            (insert "HTTP/2 200\r\nContent-Type: application/json\r\n\r\n"))
          (ende-codex--filter
           session process
           (concat "data: {\"type\":\"response.output_text.delta\","
                   "\"delta\":\"answer\"}\n\n"
                   "data: {\"type\":\"response.completed\","
                   "\"response\":{\"model\":\"gpt-test\"}}\n\n"
                   "data: [DONE]\n\n"))
          (while (process-live-p process)
            (accept-process-output process 0.05))
          (ende-codex--sentinel session process "finished\n")
          (should-not result)
          (should (eq (ende-error-kind failure) 'protocol))
          (should (string-match-p "application/json"
                                  (ende-error-message failure)))
          (let ((partial
                 (cl-find-if
                  (lambda (event)
                    (eq (plist-get event :type) :partial-result))
                  events)))
            (should (string= (plist-get (plist-get partial :result) :content)
                             "answer"))))
      (when (process-live-p process)
        (delete-process process))
      (when (file-exists-p headers-file)
        (delete-file headers-file)))))



(ert-deftest ende-test/openai-missing-content-type-requires-terminal-event ()
  (let ((headers-file (make-temp-file "ende-openai-headers-"))
        (process (make-process :name "ende-openai-incomplete-test"
                               :command (list "true") :noquery t))
        (failure nil))
    (unwind-protect
        (let ((session
               (ende-codex--make-session
                :profile '(:model "gpt-test")
                :callbacks (list :on-error
                                 (lambda (error) (setq failure error))))))
          (ende-transport-session-set-resources
           (ende-codex--lifecycle session)
           (list :process process :headers-file headers-file))
          (with-temp-file headers-file
            (insert "HTTP/2 200\r\n\r\n"))
          (ende-codex--filter session process "data: [DONE]\n\n")
          (while (process-live-p process)
            (accept-process-output process 0.05))
          (ende-codex--sentinel session process "finished\n")
          (should (eq (ende-error-kind failure) 'protocol))
          (should (string-match-p "without a terminal response event"
                                  (ende-error-message failure))))
      (when (process-live-p process)
        (delete-process process))
      (when (file-exists-p headers-file)
        (delete-file headers-file)))))



(ert-deftest ende-test/openai-refresh-rotates-opencode-credentials-safely ()
  (let* ((auth-file (make-temp-file "ende-openai-auth-test-"))
         (ende-codex-auth-file auth-file)
         (ende-codex-issuer "https://auth.openai.com:443")
         (ende-codex--refresh-flights (make-hash-table :test #'equal))
         (ende-codex--refresh-processes nil)
         (captured nil)
         (process nil)
         (refreshed :pending)
         (refreshed-again :pending)
         (make-process-count 0)
         (real-make-process (symbol-function 'make-process))
         (real-send-eof (symbol-function 'process-send-eof))
         (auth '(:access "old-access" :refresh "old-refresh"
			 :expires 0 :account-id "account-id")))
    (unwind-protect
        (progn
          (with-temp-file auth-file
            (insert "{\"openai\":{\"type\":\"oauth\","
                    "\"access\":\"old-access\","
                    "\"refresh\":\"old-refresh\","
                    "\"expires\":0,"
                    "\"accountId\":\"account-id\"}}"))
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest args)
                       (setq make-process-count (1+ make-process-count))
                       (setq captured args)
                       (setq process
                             (funcall real-make-process
                                      :name (plist-get args :name)
                                      :buffer (plist-get args :buffer)
                                      :stderr (plist-get args :stderr)
                                      :command (list "cat")
                                      :connection-type 'pipe
                                      :noquery t
                                      :filter (plist-get args :filter)
                                      :sentinel (plist-get args :sentinel)))))
                    ;; The fixture, not request startup, controls mock exit.
                    ((symbol-function 'process-send-string) #'ignore)
                    ((symbol-function 'process-send-eof) #'ignore))
            (ende-codex--refresh-auth auth
                                      (lambda (value) (setq refreshed value)))
            (ende-codex--refresh-auth
             auth (lambda (value) (setq refreshed-again value)))
            (should (= make-process-count 1))
            (let* ((command (plist-get captured :command))
                   (headers-file
                    (cadr (member "--dump-header" command)))
                   (filter (plist-get captured :filter)))
              (should-not (member "old-refresh" command))
              (with-temp-file headers-file
                (insert "HTTP/1.1 200 OK\r\n\r\n"))
              (funcall filter process
                       (concat "{\"access_token\":\"new-access\","
                               "\"refresh_token\":\"new-refresh\","
                               "\"expires_in\":3600}"))
              (funcall real-send-eof process)
              (let ((deadline (+ (float-time) 5.0)))
                (while (and (or (eq refreshed :pending)
                                (eq refreshed-again :pending))
                            (< (float-time) deadline))
                  (accept-process-output nil 0.01)))
              (should-not (eq refreshed :pending))
              (should-not (eq refreshed-again :pending))
              (should-not (process-live-p process))
              (should (string= (plist-get refreshed :access) "new-access"))
              (should (string= (plist-get refreshed :refresh) "new-refresh"))
              (should (equal refreshed-again refreshed))
              (should (string= (plist-get (ende-codex--auth-entry) :refresh)
                               "new-refresh")))))
      (when (and (processp process) (process-live-p process))
        (delete-process process))
      (when (file-exists-p auth-file)
        (delete-file auth-file)))))

(ert-deftest ende-test/openai-refresh-cancelling-one-waiter-keeps-flight ()
  (let* ((auth-file (make-temp-file "ende-openai-shared-refresh-"))
         (ende-codex-auth-file auth-file)
         (ende-codex--refresh-flights (make-hash-table :test #'equal))
         (ende-codex--refresh-processes nil)
         (process nil)
         (process-count 0)
         (first-value :pending)
         (second-value :pending)
         (auth '(:access "old-access" :refresh "old-refresh"
			 :expires 0 :account-id "account-id"))
         (real-make-process (symbol-function 'make-process))
         (real-send-eof (symbol-function 'process-send-eof)))
    (unwind-protect
        (progn
          (with-temp-file auth-file
            (insert "{\"openai\":{\"type\":\"oauth\","
                    "\"access\":\"old-access\","
                    "\"refresh\":\"old-refresh\","
                    "\"expires\":0,\"accountId\":\"account-id\"}}"))
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest args)
                       (cl-incf process-count)
                       (setq process
                             (funcall real-make-process
                                      :name (plist-get args :name)
                                      :buffer (plist-get args :buffer)
                                      :stderr (plist-get args :stderr)
                                      :command '("cat")
                                      :connection-type 'pipe :noquery t
                                      :filter (plist-get args :filter)
                                      :sentinel (plist-get args :sentinel)))))
                    ((symbol-function 'process-send-string) #'ignore)
                    ((symbol-function 'process-send-eof) #'ignore))
            (let* ((cancel-first
                    (ende-codex--refresh-auth
                     auth (lambda (value) (setq first-value value))))
                   (cancel-second
                    (ende-codex--refresh-auth
                     auth (lambda (value) (setq second-value value))))
                   (key (list (expand-file-name auth-file)
                              ende-codex-issuer ende-codex-client-id))
                   (flight (gethash key ende-codex--refresh-flights)))
              (should (= process-count 1))
              (funcall cancel-first)
              (should (process-live-p process))
              (should (= (length (ende-codex--refresh-flight-waiters flight)) 1))
              (with-temp-file (ende-codex--refresh-flight-headers-file flight)
                (insert "HTTP/1.1 200 OK\r\n\r\n"))
              (ende-codex--refresh-filter
               flight process
               (concat "{\"access_token\":\"new-access\","
                       "\"refresh_token\":\"new-refresh\","
                       "\"expires_in\":3600}"))
              (funcall real-send-eof process)
              ;; Terminal process status does not imply sentinel delivery.
              ;; Wait for the remaining callback, not merely process exit.
              (let ((deadline (+ (float-time) 5.0)))
                (while (and (eq second-value :pending)
                            (< (float-time) deadline))
                  (accept-process-output nil 0.01)))
              (should-not (eq second-value :pending))
              (should-not (process-live-p process))
              (should (eq first-value :pending))
              (should (equal (plist-get second-value :access) "new-access"))
              (should-not (gethash key ende-codex--refresh-flights))
              (should-not ende-codex--refresh-processes)
              (should-not
               (buffer-live-p
                (ende-codex--refresh-flight-response-buffer flight)))
              (should-not (file-exists-p
                           (ende-codex--refresh-flight-headers-file flight)))
              (funcall cancel-second))))
      (when (and (processp process) (process-live-p process))
        (delete-process process))
      (when (file-exists-p auth-file)
        (delete-file auth-file)))))



(ert-deftest ende-test/openai-refresh-startup-failure-does-not-wedge-flight ()
  (let* ((key '(ende-test-refresh))
         (result nil)
         (flight
          (ende-codex--make-refresh-flight
           :key key
           :auth '(:refresh "refresh-token")
           :waiters
           (list
            (ende-codex--make-refresh-waiter
             :callback (lambda (value) (setq result value))))))
         (ende-codex--refresh-flights (make-hash-table :test #'equal))
         (ende-codex--refresh-processes nil))
    (puthash key flight ende-codex--refresh-flights)
    (cl-letf (((symbol-function 'make-temp-file)
               (lambda (&rest _)
                 (error "temporary file creation failed"))))
      (ende-codex--start-refresh-flight flight))
    (should (ende-codex--refresh-flight-finished flight))
    (should (ende-error-p result))
    (should-not (gethash key ende-codex--refresh-flights))
    (should-not ende-codex--refresh-processes)
    (should-not (buffer-live-p
                 (ende-codex--refresh-flight-response-buffer flight)))
    (should-not (buffer-live-p
                 (ende-codex--refresh-flight-stderr-buffer flight)))))



(ert-deftest ende-test/openai-credentials-use-only-official-endpoints ()
  (should (string= (ende-codex--validated-generation-url
                    "https://chatgpt.com:443/backend-api/codex/responses")
                   "https://chatgpt.com:443/backend-api/codex/responses"))
  (should (string= (ende-codex--validated-usage-url)
                   "https://chatgpt.com/backend-api/wham/usage"))
  (should-error
   (ende-codex--validated-generation-url
    "https://chatgpt.com.attacker.test/backend-api/codex/responses"))
  (should-error
   (ende-codex--validated-generation-url
    "https://chatgpt.com/backend-api/codex/responses?redirect=attacker"))
  (let ((ende-codex-usage-endpoint
         "https://chatgpt.com/backend-api/wham/usage?redirect=attacker"))
    (should-error (ende-codex--validated-usage-url)))
  (let ((ende-codex-issuer "https://auth.openai.com.attacker.test"))
    (should-error (ende-codex--validated-refresh-url))))



(ert-deftest ende-test/openai-request-body-applies-output-token-limit ()
  (let ((ende-max-output-tokens 123))
    (let ((body (ende-codex--request-body
                 (list (cons "user" "body"))
                 '(:model "test-model"))))
      (should (= (alist-get :max_output_tokens body) 123)))))



(ert-deftest ende-test/openai-credential-replacement-is-side-by-side ()
  (let* ((directory (make-temp-file "ende-openai-auth-dir-" t))
         (file (expand-file-name "auth.json" directory))
         (ende-codex-auth-file file)
         (old '(:access "old-access" :refresh "old-refresh"
			:expires 0 :account-id "account-id"))
         (new '(:access "new-access" :refresh "new-refresh"
			:expires 9999999999999 :account-id "account-id")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "{\"openai\":{\"type\":\"oauth\","
                    "\"access\":\"old-access\","
                    "\"refresh\":\"old-refresh\","
                    "\"expires\":0,\"accountId\":\"account-id\"}}"))
          (ende-codex--store-auth new (ende-codex--auth-version old))
          (should (string= (plist-get (ende-codex--auth-entry) :access)
                           "new-access"))
          (should (= (logand (file-modes file) #o777) #o600))
          (should-not
           (directory-files directory nil "\\`\\.ende-openai-auth-" t))
          (should-error
           (ende-codex--store-auth
            old (ende-codex--auth-version
                 '(:access "other" :refresh "other" :expires 0
			   :account-id "account-id")))))
      (when (file-directory-p directory)
        (delete-directory directory t)))))



(ert-deftest ende-test/openai-responses-events-normalize-to-ende-result ()
  (let* ((events nil)
         (result nil)
         (failure nil)
         (session
          (ende-codex--make-session
           :profile '(:model "gpt-test")
           :callbacks (list :on-event (lambda (event) (push event events))
                            :on-done (lambda (value) (setq result value))
                            :on-error (lambda (value) (setq failure value))))))
    (ende-codex--handle-event
     session
     (ende-protocol-json-read-string
      "{\"type\":\"response.created\",\"response\":{\"model\":\"gpt-actual\"}}"))
    (ende-codex--handle-event
     session
     (ende-protocol-json-read-string
      "{\"type\":\"response.reasoning_summary_text.delta\",\"delta\":\"thinking\"}"))
    (ende-codex--handle-event
     session
     (ende-protocol-json-read-string
      "{\"type\":\"response.output_text.delta\",\"delta\":\"answer\"}"))
    (ende-codex--handle-event
     session
     (ende-protocol-json-read-string
      "{\"type\":\"response.completed\",\"response\":{\"model\":\"gpt-actual\",\"usage\":{\"output_tokens\":7}}}"))
    (setq result (ende-codex--result session))
    (should-not failure)
    (should (string= (plist-get result :content) "answer"))
    (should (string= (plist-get result :thinking) "thinking"))
    (should (string= (plist-get result :model) "gpt-actual"))
    (should (= (plist-get result :tokens) 7))
    (should (ende-codex--session-terminal-event session))
    (should-not (ende-transport-session-terminal
                 (ende-codex--lifecycle session)))))

(ert-deftest ende-test/codex-reasoning-summary-deltas-remain-item-scoped ()
  (let* ((events nil)
         (session
          (ende-codex--make-session
           :profile '(:model "gpt-test")
           :callbacks (list :on-event (lambda (event) (push event events))))))
    (dolist (event
             '("{\"type\":\"response.reasoning_summary_text.delta\",\"item_id\":\"rs_1\",\"delta\":\"first \"}"
               "{\"type\":\"response.reasoning_summary_text.delta\",\"item_id\":\"rs_2\",\"delta\":\"second\"}"
               "{\"type\":\"response.reasoning_summary_text.delta\",\"item_id\":\"rs_1\",\"delta\":\"summary\"}"
               "{\"type\":\"response.reasoning_summary_text.done\",\"item_id\":\"rs_1\",\"text\":\"first summary\"}"
               "{\"type\":\"response.reasoning_summary_text.done\",\"item_id\":\"rs_2\",\"text\":\"second\"}"))
      (ende-codex--handle-event
       session (ende-protocol-json-read-string event)))
    (let ((summaries (nreverse
                      (cl-remove-if-not
                       (lambda (event)
                         (eq (plist-get event :type) :reasoning-summary))
                       events))))
      (should (equal (mapcar (lambda (event)
                               (cons (plist-get event :item)
                                     (plist-get event :text)))
                             summaries)
                     '(("rs_1" . "first ")
                       ("rs_2" . "second")
                       ("rs_1" . "summary")))))))

(ert-deftest ende-test/codex-effort-probe-uses-selected-model-capabilities ()
  (let ((calls 0)
        (results nil)
        (entries
         (ende-codex--parse-model-entries
          (concat
           "{\"models\":["
           "{\"slug\":\"other\",\"supported_reasoning_levels\":[{\"effort\":\"none\"}]},"
           "{\"slug\":\"selected\",\"default_reasoning_level\":\"low\","
           "\"supported_reasoning_levels\":[{\"effort\":\"high\"},{\"effort\":\"low\"},"
           "{\"effort\":\"\"},{\"effort\":\"low\"}]}]}"))))
    (cl-letf (((symbol-function 'ende-codex--probe-model-entries)
               (lambda (callback)
                 (cl-incf calls)
                 (funcall callback entries nil)
                 (lambda () nil))))
      (dotimes (_ 2)
        (ende-codex-probe-reasoning-efforts
         nil "selected"
         (lambda (efforts default error)
           (push (list efforts default error) results))))
      (should (= calls 2))
      (should (equal results
                     '((("high" "low") "low" nil)
                       (("high" "low") "low" nil)))))))

(provide 'ende-codex-test)

;;; ende-codex-test.el ends here
