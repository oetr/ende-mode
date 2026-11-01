;;; ende-transport-test.el --- Transport tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Coverage for default transport, HTTP/SSE handling, and resource limits.

;;; Code:

(require 'ert)
(require 'ende)
(require 'ende-test-helpers)
(require 'cl-lib)
(require 'json)

(ert-deftest ende-test/curl-recognizes-explicit-loopback-hosts ()
  (dolist (host '("localhost" "LOCALHOST" "::1" "127.0.0.1" "127.42.255.254"))
    (should (ende-curl--loopback-host-p host)))
  (dolist (host '("128.0.0.1" "192.168.1.1" "127.0.0.256" "127.0.0" "127x0x0x1"))
    (should-not (ende-curl--loopback-host-p host))))



(defun ende-test--run-default-transport (header response)
  "Run the default transport with a mocked process and HEADER/RESPONSE.
Return (RESULT ERROR COMMAND DONE-COUNT ERROR-COUNT CANCEL-FUNCTION EVENTS)."
  (let ((captured nil)
        (process nil)
        (result nil)
        (failure nil)
        (done-count 0)
        (error-count 0)
        (cancel-function nil)
        (events nil)
        (ende-provider-discovery-enabled nil)
        (real-make-process (symbol-function 'make-process))
        (profile (list :name 'test :api 'openai-chat-completions :auth-method 'none
                       :job-id 1
                       :url "https://example.test/v1"
                       :model "test-model"
                       :temperature 0.2
                       :controls '((:temperature :path ("temperature")))
                       :require-sse t)))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq captured args)
                 (setq process
                       (funcall real-make-process
				:name
				(plist-get args :name)
				:buffer
				(plist-get args :buffer)
				:command (list "cat")
				:connection-type 'pipe)))))
      (unwind-protect
          (progn
            (ende-llm-default
             (list (cons "user" "body"))
             (list :on-start (lambda (cancel)
                               (setq cancel-function cancel))
                   :on-event (lambda (event)
                               (push event events))
                   :on-done (lambda (value)
                              (setq done-count (1+ done-count))
                              (setq result value))
                   :on-error (lambda (value)
                               (setq error-count (1+ error-count))
                               (setq failure value)))
             profile)
            (let* ((command (plist-get captured :command))
                   (headers-file (nth (1+ (cl-position "--dump-header"
                                                       command :test #'string=))
                                      command))
                   (filter (plist-get captured :filter))
                   (sentinel (plist-get captured :sentinel)))
              (with-temp-file headers-file
                (insert header))
              (funcall filter process response)
              (when (process-live-p process)
                (process-send-eof process))
              (while (process-live-p process)
                (accept-process-output process 0.05))
              (funcall sentinel process "finished\n")
              (list result failure command done-count error-count
                    cancel-function events)))
        (when (and (processp process) (process-live-p process))
          (delete-process process))))))



(defun ende-test--with-idle-request (timeout function &optional command)
  "Call FUNCTION with a request guarded by TIMEOUT, then clean up.
Use COMMAND for the process, or a silent sleep process by default."
  (let* ((ende-response-idle-timeout timeout)
         (request (ende-curl-start-request
                   :profile '(:url "https://example.test/v1")
                   :name "ende-test-idle"
                   :command (lambda (_headers _config)
                              (or command '("sleep" "30")))
                   :filter #'ignore :sentinel #'ignore))
         (process (plist-get request :process)))
    (unwind-protect
        (funcall function process)
      (ende-curl-cleanup-resources
       (list (plist-get request :response-buffer)
             (plist-get request :stderr-buffer))
       (list (plist-get request :headers-file)
             (plist-get request :config-file))
       (list process (plist-get request :stderr-process))))))

(ert-deftest ende-test/transport-session-settles-once-and-cleans-resources ()
  (let* ((process-buffer (generate-new-buffer " *ende-test-process*"))
         (stderr-buffer (generate-new-buffer " *ende-test-stderr*"))
         (headers-file (make-temp-file "ende-test-headers-"))
         (config-file (make-temp-file "ende-test-config-"))
         (events 0)
         (done 0)
         (errors 0)
         (discovery-cancelled nil)
         (session
          (ende-llm--make-session
           :callbacks
           (list :on-event (lambda (_event)
                             (setq events (1+ events))
                             (error "callback failed"))
                 :on-done (lambda (_result) (setq done (1+ done)))
                 :on-error (lambda (_error) (setq errors (1+ errors)))))))
    (ende-transport-session-add-canceller
     (ende-llm--lifecycle session)
     (lambda () (setq discovery-cancelled t)))
    (ende-transport-session-set-resources
     (ende-llm--lifecycle session)
     (list :response-buffer process-buffer
           :stderr-buffer stderr-buffer
           :headers-file headers-file :config-file config-file))
    (ende-llm--fail
     session (ende-error-create :kind 'transport :message "failed"))
    (ende-llm--fail
     session (ende-error-create :kind 'transport :message "late"))
    (ende-llm--settle session 'success "late")
    (should (eq (ende-transport-session-terminal
                 (ende-llm--lifecycle session))
                'failure))
    (should (= events 1))
    (should (= done 0))
    (should (= errors 1))
    (should discovery-cancelled)
    (should-not (ende-transport-session-auxiliary-cancellers
                 (ende-llm--lifecycle session)))
    (should-not (ende-transport-session-callbacks
                 (ende-llm--lifecycle session)))
    (should-not (ende-transport-session-process
                 (ende-llm--lifecycle session)))
    (should-not (ende-transport-session-process-buffer
                 (ende-llm--lifecycle session)))
    (should-not (ende-transport-session-stderr-buffer
                 (ende-llm--lifecycle session)))
    (should-not (ende-transport-session-stderr-process
                 (ende-llm--lifecycle session)))
    (should-not (ende-transport-session-headers-file
                 (ende-llm--lifecycle session)))
    (should-not (ende-transport-session-config-file
                 (ende-llm--lifecycle session)))
    (should-not (buffer-live-p process-buffer))
    (should-not (buffer-live-p stderr-buffer))
    (should-not (file-exists-p headers-file))
    (should-not (file-exists-p config-file))))



(ert-deftest ende-test/provider-processes-from-tramp-stay-local ()
  (dolist (operation '(generation codex-generation discovery models refresh))
    (let ((native-comp-enable-subr-trampolines nil)
          (default-directory "/ssh:ende-test.invalid:/source/")
          (ende-provider-discovery-enabled nil)
          (ende-provider--catalog-cache (make-hash-table :test #'equal))
          (ende-provider--catalog-inflight (make-hash-table :test #'equal))
          (ende-codex--refresh-flights (make-hash-table :test #'equal))
          (auth '(:access "test-access" :refresh "test-refresh"
			  :expires 9999999999999))
          (profile '(:name test :api openai-chat-completions :auth-method none
                           :url "https://example.test/v1/chat/completions"
                           :models-url "https://example.test/v1/models"
                           :model "test-model" :temperature 0.2
                           :controls ((:temperature :path ("temperature")))))
          (cancel nil) (process nil) (failure nil)
          (process-count 0) (pipe-count 0) (send-count 0) (files nil)
          (real-process (symbol-function 'make-process))
          (real-pipe (symbol-function 'make-pipe-process))
          (real-temp (symbol-function 'make-temp-file)))
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest args)
                   (should-not (file-remote-p default-directory))
                   (should (file-directory-p default-directory))
                   (cl-incf process-count)
                   (dolist (option '("--dump-header" "--config"))
                     (let ((path (cadr (member option (plist-get args :command)))))
                       (when path
                         (should (file-name-absolute-p path))
                         (should-not (file-remote-p path))
                         (should (file-exists-p path)))))
                   (setq process
                         (funcall real-process :name "ende-test-local"
                                  :command '("cat") :connection-type 'pipe
                                  :noquery t))))
                ((symbol-function 'make-pipe-process)
                 (lambda (&rest args)
                   (should-not (file-remote-p default-directory))
                   (cl-incf pipe-count)
                   (apply real-pipe args)))
                ((symbol-function 'make-temp-file)
                 (lambda (&rest args)
                   (should-not (file-remote-p default-directory))
                   (let ((file (apply real-temp args)))
                     (push file files)
                     file)))
                ((symbol-function 'process-send-string)
                 (lambda (target body)
                   (should (eq target process))
                   (should-not (file-remote-p default-directory))
                   (should (stringp body))
                   (when (eq operation 'refresh)
                     (should (string-match-p "refresh_token=test-refresh" body)))
                   (cl-incf send-count)))
                ((symbol-function 'ende-codex--auth-entry) (lambda () auth)))
        (unwind-protect
            (let ((callbacks (list :on-start (lambda (value) (setq cancel value))
                                   :on-error (lambda (value) (setq failure value)))))
              (pcase operation
                ('generation
                 (ende-llm-default '(("user" . "remote source")) callbacks profile))
                ('codex-generation
                 (ende-codex-default '(("user" . "remote source")) callbacks
                                     '(:service openai-codex :account default :api openai-responses
                                       :auth-method codex-oauth
                                       :url "https://chatgpt.com/backend-api/codex/responses" :model "test-model")))
                ('discovery
                 (let ((ende-provider-discovery-enabled t))
                   (setq cancel (ende-provider-discover profile #'ignore))))
                ('models
                 (setq cancel (ende-codex-probe-models
                               (lambda (_models error) (setq failure error)))))
                ('refresh
                 (setq cancel (ende-codex--refresh-auth
                               auth (lambda (value) (setq failure value))))))
              (should-not failure)
              (should (= process-count 1))
              (should (= pipe-count 1))
              (should (= send-count (if (memq operation '(discovery models)) 0 1)))
              (should files)
              (should (equal default-directory "/ssh:ende-test.invalid:/source/")))
          (when cancel (funcall cancel))
          (when (and process (process-live-p process)) (delete-process process))
          (when process
            (ende-curl-cleanup-resources
             (list (process-get process :ende-response-buffer)
                   (process-get process :ende-stderr-buffer))
             files
             (list (process-get process :ende-stderr-process)))))))))



(ert-deftest ende-test/provider-rejects-unsafe-temporary-directory ()
  (dolist (directory '("/ssh:ende-test.invalid:/tmp/" "relative-temp/"))
    (let ((native-comp-enable-subr-trampolines nil)
          (temporary-file-directory directory))
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest _) (ert-fail "Started a process")))
                ((symbol-function 'make-temp-file)
                 (lambda (&rest _) (ert-fail "Created a temporary file")))
                ((symbol-function 'ende-curl--auth-secret)
                 (lambda (&rest _) (ert-fail "Read credentials"))))
        (should-error (ende-curl-local-directory) :type 'user-error)
        (should-error
         (ende-curl-start-request :profile '(:url "https://example.test"))
         :type 'user-error)
        (should-error (ende-curl--config '(:url "https://example.test"))
                      :type 'user-error)))))



(ert-deftest ende-test/default-transport-completes-valid-sse ()
  (let ((out (ende-test--run-default-transport
              "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n"
              "data: {\"model\":\"actual-model\",\"provider\":\"llama.cpp\",\"choices\":[{\"delta\":{\"content\":\"hello\"}}]}\n\ndata: [DONE]\n\n")))
    (should-not (nth 1 out))
    (should (string= (plist-get (nth 0 out) :content) "hello"))
    (should (string= (plist-get (nth 0 out) :model) "actual-model"))
    (should (string= (plist-get (nth 0 out) :provider) "llama.cpp"))
    (should (= (nth 3 out) 1))
    (should (= (nth 4 out) 0))
    (should (functionp (nth 5 out)))
    (should-not (member "hello" (nth 2 out)))
    (should (member "Accept: text/event-stream" (nth 2 out)))
    (should (equal (cadr (member "--user-agent" (nth 2 out)))
                   ende-client-name))
    (should (member "@-" (nth 2 out)))))



(ert-deftest ende-test/default-transport-accepts-text-completion-deltas ()
  (let ((out (ende-test--run-default-transport
              "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n"
              "data: {\"choices\":[{\"text\":\"legacy\"}]}\n\ndata: [DONE]\n\n")))
    (should-not (nth 1 out))
    (should (string= (plist-get (nth 0 out) :content) "legacy"))))



(ert-deftest ende-test/default-transport-requires-done ()
  (let ((out (ende-test--run-default-transport
              "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n"
              "data: {\"choices\":[{\"finish_reason\":\"stop\",\"delta\":{\"content\":\"answer\"}}]}\n\n")))
    (should-not (nth 0 out))
    (should (eq (ende-error-kind (nth 1 out)) 'protocol))
    (should (string-match-p "without \\[DONE\\]"
                            (ende-error-message (nth 1 out))))))



(ert-deftest ende-test/default-transport-retains-joined-partial-result ()
  (let* ((first
          "data: {\"choices\":[{\"finish_reason\":null,\"delta\":{\"reasoning_content\":\"Sub\"}}]}\n\n")
         (second
          "data: {\"choices\":[{\"finish_reason\":\"length\",\"delta\":{\"reasoning_content\":\"headings\"}}]}\n\n")
         (out (ende-test--run-default-transport
               "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n"
               (concat first second
                       "data: {\"choices\":[],\"usage\":{\"completion_tokens\":4096}}\n\n"
                       "data: [DONE]\n\n")))
         (partial (cl-find-if
                   (lambda (event)
                     (eq (plist-get event :type) :partial-result))
                   (nth 6 out))))
    (should partial)
    (should (string= (plist-get (plist-get partial :result) :thinking)
                     "Subheadings"))
    (should (string= (plist-get (plist-get partial :result) :finish-reason)
                     "length"))))



(ert-deftest ende-test/default-transport-rejects-http-errors ()
  (let ((out (ende-test--run-default-transport
              "HTTP/1.1 401 Unauthorized\r\nContent-Type: text/event-stream\r\n\r\n"
              "data: {\"error\":{\"message\":\"bad key\"}}\n\ndata: [DONE]\n\n")))
    (should-not (nth 0 out))
    (should (ende-error-p (nth 1 out)))
    (should (eq (ende-error-kind (nth 1 out)) 'http))
    (should (= (nth 3 out) 0))
    (should (= (nth 4 out) 1))))



(ert-deftest ende-test/default-transport-settles-empty-provider-error ()
  (let ((out (ende-test--run-default-transport
              "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n"
              "data: {\"error\":{}}\n\ndata: [DONE]\n\n")))
    (should-not (nth 0 out))
    (should (eq (ende-error-kind (nth 1 out)) 'provider))
    (should (= (nth 4 out) 1))))



(ert-deftest ende-test/default-transport-done-ignores-same-chunk-trailing-data ()
  (let ((out (ende-test--run-default-transport
              "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n"
              (concat
               "data: {\"choices\":[{\"delta\":{\"content\":\"ok\"}}]}\n\n"
               "data: [DONE]\n\n"
               "data: malformed trailing bytes\n\n"))))
    (should (string= (plist-get (nth 0 out) :content) "ok"))
    (should-not (nth 1 out))
    (should (= (nth 3 out) 1))))



(ert-deftest ende-test/stream-transports-share-request-lifecycle-contract ()
  (ende-test--reset)
  (dolist (transport '(chat codex))
    (let* ((success
            (ende-test--run-stream-request
             transport
             (if (eq transport 'codex)
                 (list
                  (concat "data: {\"type\":\"response.output_text.delta\","
                          "\"delta\":\"answer\"}\n\n")
                  (concat "data: {\"type\":\"response.completed\","
                          "\"response\":{\"model\":\"test-model\","
                          "\"usage\":{\"output_tokens\":2}}}\n\n"))
               (list
                (concat "data: {\"choices\":[{\"delta\":{"
                        "\"content\":\"answer\"}}]}\n\n")
                (concat "data: {\"choices\":[],\"usage\":{"
                        "\"completion_tokens\":2}}\n\n")
                "data: [DONE]\n\n"))))
           (failure
            (ende-test--run-stream-request
             transport
             (if (eq transport 'codex)
                 (list
                  (concat "data: {\"type\":\"response.output_text.delta\","
                          "\"delta\":\"partial\"}\n\n")
                  (concat "data: {\"type\":\"response.failed\","
                          "\"error\":{\"message\":\"provider failed\"}}\n\n"))
               (list
                (concat "data: {\"choices\":[{\"delta\":{"
                        "\"content\":\"partial\"}}]}\n\n")
                (concat "data: {\"error\":{\"message\":"
                        "\"provider failed\"}}\n\n")))))
           (partial-event
            (cl-find-if (lambda (event)
                          (eq (plist-get event :type) :partial-result))
                        (plist-get failure :events)))
           (failure-order (plist-get failure :order)))
      (should-not (plist-get success :error))
      (unless (equal (plist-get (plist-get success :result) :content) "answer")
        (ert-fail (format "%s success transport did not return content: %S"
                          transport success)))
      (should (eq (car (plist-get success :order)) :start))
      (should (eq (car (last (plist-get success :order))) :done))
      (should (plist-get success :cleaned))
      (should (eq (ende-error-kind (plist-get failure :error)) 'provider))
      (should (equal (plist-get (plist-get partial-event :result) :content)
                     "partial"))
      (should (< (cl-position :partial-result failure-order)
                 (cl-position :on-error failure-order)))
      (should (plist-get failure :cleaned)))
    (let* ((cancelled (ende-test--run-stream-request transport nil t)))
      (should (eq (ende-error-kind (plist-get cancelled :error)) 'cancelled))
      (should (equal (plist-get cancelled :order)
                     '(:start :error :on-error)))
      (should (plist-get cancelled :cleaned)))))

(ert-deftest ende-test/stream-transports-share-output-limit-ordering ()
  (dolist (transport '(chat codex))
    (let* ((ende-max-output-chars 2)
           (chunks
            (if (eq transport 'codex)
                (list
                 (concat "data: {\"type\":\"response.output_text.delta\","
                         "\"delta\":\"ab\"}\n\n")
                 (concat "data: {\"type\":\"response.output_text.delta\","
                         "\"delta\":\"c\"}\n\n"))
              (list
               (concat "data: {\"choices\":[{\"delta\":{"
                       "\"content\":\"ab\"}}]}\n\n")
               (concat "data: {\"choices\":[{\"delta\":{"
                       "\"content\":\"c\"}}]}\n\n"))))
           (out (ende-test--run-stream-request transport chunks))
           (events (plist-get out :events))
           (partial (cl-find-if
                     (lambda (event)
                       (eq (plist-get event :type) :partial-result))
                     events)))
      (should (eq (ende-error-kind (plist-get out :error)) 'resource))
      (should (equal (mapcar (lambda (event) (plist-get event :type)) events)
                     (if (eq transport 'codex)
                         '(:diagnostic :status :delta :status
                           :partial-result :error)
                       '(:diagnostic :delta :usage :status
                         :partial-result :error))))
      (should (equal (plist-get (plist-get partial :result) :content) "ab"))
      (should (plist-get out :cleaned)))))

(ert-deftest ende-test/estimated-usage-cadence-remains-provider-specific ()
  (let ((ende-usage-update-interval 60))
    (dolist (transport '(chat codex))
      (let* ((chunks
              (if (eq transport 'codex)
                  (list
                   (concat "data: {\"type\":\"response.output_text.delta\","
                           "\"delta\":\"a\"}\n\n")
                   (concat "data: {\"type\":\"response.output_text.delta\","
                           "\"delta\":\"b\"}\n\n")
                   (concat "data: {\"type\":\"response.completed\","
                           "\"response\":{\"usage\":{\"output_tokens\":2}}}"
                           "\n\n"))
                (list
                 (concat "data: {\"choices\":[{\"delta\":{"
                         "\"content\":\"a\"}}]}\n\n")
                 (concat "data: {\"choices\":[{\"delta\":{"
                         "\"content\":\"b\"}}]}\n\n")
                 (concat "data: {\"choices\":[],\"usage\":{"
                         "\"completion_tokens\":2}}\n\n")
                 "data: [DONE]\n\n")))
             (out (ende-test--run-stream-request transport chunks))
             (estimated
              (cl-remove-if-not
               (lambda (event)
                 (and (eq (plist-get event :type) :usage)
                      (plist-get event :estimated)))
               (plist-get out :events))))
        (should-not (plist-get out :error))
        (should (= (length estimated) (if (eq transport 'chat) 1 0)))
        (should (plist-get out :cleaned))))))

(ert-deftest ende-test/stream-speed-requires-a-valid-measured-interval ()
  (let* ((now 10.0)
         (events nil)
         (lifecycle
          (ende-transport-session-create
           (list :on-event (lambda (event) (push event events))) nil))
         (stream (ende-stream-create lifecycle nil))
         (usage (json-parse-string "{\"output_tokens\":113}"
                                   :object-type 'hash-table)))
    (cl-letf (((symbol-function 'float-time) (lambda (&optional _) now)))
      (ende-stream-record-usage stream usage "output_tokens")
      (should-not (plist-get (car events) :speed))
      (should-not (plist-get
                   (ende-stream-result stream "output_tokens" nil "test")
                   :speed))
      (setf (ende-stream-session-generation-start stream) now
            (ende-stream-session-estimated-chars stream) 452)
      (setq now 10.01)
      (ende-stream-emit-estimated-usage stream)
      (should-not (plist-get (car events) :speed))
      (ende-stream-record-usage stream usage "output_tokens")
      (should-not (plist-get (car events) :speed))
      (setq now 11.0)
      (ende-stream-record-usage stream usage "output_tokens")
      (should (numberp (plist-get (car events) :speed)))
      (should (numberp
               (plist-get
                (ende-stream-result stream "output_tokens" nil "test")
                :speed))))))

(ert-deftest ende-test/active-callback-failure-cleans-up-once ()
  (ende-test--reset)
  (dolist (transport '(chat codex))
    (let ((cleanup-count 0)
          (real-cleanup (symbol-function 'ende-transport-session-cleanup)))
      (let ((out
             (cl-letf (((symbol-function 'ende-transport-session-cleanup)
                        (lambda (session)
                          (cl-incf cleanup-count)
                          (funcall real-cleanup session))))
               (ende-test--run-stream-request
                transport
                (list (if (eq transport 'codex)
                          (concat "data: {\"type\":\"response.output_text.delta\","
                                  "\"delta\":\"answer\"}\n\n")
                        (concat "data: {\"choices\":[{\"delta\":{"
                                "\"content\":\"answer\"}}]}\n\n")))
                nil nil nil (lambda () (error "active callback failed"))))))
        (should (eq (ende-error-kind (plist-get out :error)) 'callback))
        (should (= cleanup-count 1))
        (should (= (cl-count :on-error (plist-get out :order)) 1))
        (should (plist-get out :cleaned))))))



(ert-deftest ende-test/stream-transports-count-multibyte-response-bytes ()
  (ende-test--reset)
  (dolist (transport '(chat codex))
    (let* ((prefix
            (if (eq transport 'codex)
                (concat "data: {\"type\":\"response.output_text.delta\","
                        "\"delta\":\"é\"}\n\n")
              (concat "data: {\"choices\":[{\"delta\":{"
                      "\"content\":\"é\"}}]}\n\n")))
           (out
            (ende-test--run-stream-request
             transport (list prefix "世界") nil (string-bytes prefix)))
           (partial
            (cl-find-if (lambda (event)
                          (eq (plist-get event :type) :partial-result))
                        (plist-get out :events))))
      (should (eq (ende-error-kind (plist-get out :error)) 'resource))
      (should (equal (plist-get (plist-get partial :result) :content) "é"))
      (should (plist-get out :cleaned)))))



(ert-deftest ende-test/deepseek-stream-auth-usage-and-tool-history-round-trip ()
  (let* ((token "deepseek-secret-token")
         (ende-builtin-provider-settings '((deepseek :models ("deepseek-flash"))))
         (profile (ende-provider-profile 'deepseek))
         (auth-query nil)
         (out nil)
         (chunks
          (list
           (concat "data: {\"model\":\"deepseek-flash\",\"choices\":[{"
                   "\"delta\":{\"reasoning_content\":\"Reviewing\"}}]}\n\n")
           (concat "data: {\"choices\":[{\"delta\":{"
                   "\"content\":\"Answer\"}}]}\n\n")
           (concat "data: {\"choices\":[{\"delta\":{\"tool_calls\":[{"
                   "\"index\":0,\"id\":\"call-1\",\"type\":\"function\","
                   "\"function\":{\"name\":\"lookup\","
                   "\"arguments\":\"{\\\"q\\\":\"}}]}}]}\n\n")
           (concat "data: {\"choices\":[{\"delta\":{\"tool_calls\":[{"
                   "\"index\":0,\"function\":{"
                   "\"arguments\":\"\\\"deep\\\"}\"}}]}}]}\n\n")
           (concat "data: {\"choices\":[],\"usage\":{\"prompt_tokens\":5,"
                   "\"completion_tokens\":3,\"total_tokens\":8}}\n\n")
           "data: [DONE]\n\n")))
    (cl-letf (((symbol-function 'ende-credentials-secret)
               (lambda (resolved)
                 (setq auth-query (list :host (plist-get resolved :auth-source-host)
                                        :port (plist-get resolved :auth-source-port)
                                        :user (plist-get resolved :auth-source-user)))
                 token)))
      (setq out
            (ende-test--run-stream-request
             'openai-compatible chunks nil nil profile)))
    (let* ((result (plist-get out :result))
           (calls (plist-get result :tool-calls))
           (call (car calls))
           (request-body
            (ende-protocol-json-read-string
             (plist-get out :request-body)))
           (history-body
            (ende-protocol-json-read-string
             (json-encode
              (ende-llm--request-body
               (list (cons "user" "question")
                     (list :role "assistant" :content "Answer"
                           :tool-calls calls)
                     (list :role "tool" :content "{\"found\":true}"
                           :tool-call-id "call-1"))
               profile))))
           (history (ende-protocol-json-get "messages" history-body))
           (assistant (aref history 1))
           (history-call
            (aref (ende-protocol-json-get "tool_calls" assistant) 0))
           (tool-message (aref history 2))
           (command (plist-get out :command)))
      (should (equal (plist-get result :content) "Answer"))
      (should (equal (plist-get result :thinking) "Reviewing"))
      (should (= (plist-get result :tokens) 3))
      (should (equal (plist-get call :id) "call-1"))
      (should (equal (plist-get call :name) "lookup"))
      (should (equal (plist-get call :arguments) "{\"q\":\"deep\"}"))
      (should (equal (ende-protocol-json-get "model" request-body)
                     "deepseek-flash"))
      (should (equal (plist-get result :reasoning-content) "Reviewing"))
      (should-not (ende-protocol-json-has-key-p "temperature" request-body))
      (should-not (ende-protocol-json-has-key-p "reasoning" request-body))
      (should (eq (ende-protocol-json-get
                   "include_usage"
                   (ende-protocol-json-get "stream_options" request-body))
                  t))
      (should (equal (ende-protocol-json-get "id" history-call) "call-1"))
      (should (equal
               (ende-protocol-json-get
                "arguments" (ende-protocol-json-get "function" history-call))
               "{\"q\":\"deep\"}"))
      (should (equal (ende-protocol-json-get "tool_call_id" tool-message)
                     "call-1"))
      (should (equal (plist-get auth-query :host) "api.deepseek.com"))
      (should (equal (plist-get auth-query :user) "default"))
      (should (= (plist-get auth-query :port) 443))
      (should (string-match-p "Authorization: Bearer deepseek-secret-token"
                              (plist-get out :config-text)))
      (should-not (member token command))
      (should-not (string-match-p token (plist-get out :request-body)))
      (should-not (string-match-p token
                                  (prin1-to-string
                                   (ende-provider-profile-snapshot 'deepseek))))
      (should (equal (cadr (member "--url" command))
                     "https://api.deepseek.com/chat/completions"))
      (should (ende-provider-model-discovery-available-p profile))
      (should (plist-get out :cleaned)))))



(ert-deftest ende-test/curl-stderr-is-byte-bounded ()
  (let ((buffer (generate-new-buffer " *ende-test-stderr*"))
        (process (make-pipe-process :name "ende-test-stderr-process"
                                    :buffer nil :noquery t))
        (ende-max-stderr-bytes 4))
    (unwind-protect
        (progn
          (ende-curl-stderr-filter buffer process "12345")
          (ende-curl-stderr-filter buffer process "6")
          (with-current-buffer buffer
            (should (string= (buffer-string) "1234"))))
      (delete-process process)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ende-test/auth-and-body-are-not-command-arguments ()
  (let* ((token "secret-token")
         (profile (list :name 'remote
                        :url "https://example.test/v1"
                        :model "model"
                        :temperature 0.2
                        :auth-source-host "example.test"
                        :auth-source-user "ende"
                        :auth-source-port 443))
         (config nil))
    (cl-letf (((symbol-function 'ende-credentials-secret)
               (lambda (_) token)))
      (setq config (ende-curl--config profile))
      (unwind-protect
          (let ((command (ende-curl-command
                          profile "/tmp/headers" config)))
            (should-not (member token command))
            (should-not (member "body" command))
            (should-not (member "--fail-with-body" command)))
        (when (file-exists-p config) (delete-file config))))))



(ert-deftest ende-test/unbounded-output-omits-provider-token-limit ()
  (let ((ende-default-output-tokens nil))
    (let ((body (ende-llm--request-body
                 (list (cons "user" "body"))
                 '(:model "test-model" :temperature 0.2
			  :controls ((:temperature :path ("temperature")))))))
      (should-not (alist-get :max_tokens body))
      (should-not (string-match-p "max_tokens" (json-encode body))))))



(ert-deftest ende-test/curl-config-escapes-and-rejects-unsafe-headers ()
  (let* ((profile '(:url "https://example.test/v1"
			 :request-headers ("X-Title: a\"b\\c")))
         (config (ende-curl--config profile)))
    (unwind-protect
        (with-temp-buffer
          (should (= (logand (file-modes config) #o777) #o600))
          (insert-file-contents config)
          (should (string=
                   (buffer-string)
                   "header = \"X-Title: a\\\"b\\\\c\"\n")))
      (when (file-exists-p config)
        (delete-file config))))
  (dolist (header '("X-Test: ok\nurl = https://attacker.test"
                    "Host: attacker.test"
                    "X-Auth-Token: secret"
                    "Accept: application/json"
                    "Content-Length: 0"))
    (should-error
     (ende-curl--config
      (list :url "https://example.test/v1"
            :request-headers (list header))))))



(ert-deftest ende-test/curl-response-headers-select-final-response ()
  (let ((file (make-temp-file "ende-response-headers-")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "HTTP/1.1 200 Connection established\r\n\r\n"
                    "HTTP/2 103 Early Hints\r\nLink: </model>\r\n\r\n"
                    "HTTP/2 200\r\n"
                    "CoNtEnT-TyPe: text/event-stream; charset=utf-8\r\n\r\n"))
          (should (equal (ende-curl-response-headers file)
                         '(200 . "text/event-stream; charset=utf-8")))
          (should (ende-curl-event-stream-p
                   "text/event-stream; charset=utf-8"))
          (should-not (ende-curl-event-stream-p
                       "application/json; note=text/event-stream")))
      (when (file-exists-p file)
        (delete-file file)))))



(ert-deftest ende-test/provider-url-and-curl-command-are-hardened ()
  (dolist (url '("http://127.1/v1" "http://0177.0.0.1/v1"
                 "http://0x7f000001/v1" "https://example.test:65536/v1"
                 "https://example.test:0/v1" "https://example.test\\@other.test/v1"
                 "https://%65xample.test/v1" "https://example.test/v1%2fother"))
    (should-error (ende-curl--validated-url (list :url url))))
  (should-error
   (ende-curl--validated-url '(:url "http://127.attacker.test/v1")))
  (should-error
   (ende-curl--validated-url '(:url "https://example.test/v1?key=secret")))
  (should-error
   (ende-curl--validated-url
    '(:url "https://other.test/v1"
	   :auth-source-host "example.test"
	   :auth-source-user "ende"
	   :auth-source-port 443)))
  (let ((command
         (ende-curl-command
          '(:url "https://example.test/v1") "/tmp/headers" nil)))
    (should (string= (nth 1 command) "--disable"))
    (should (member "--globoff" command))
    (should (member "--noproxy" command))
    (should (equal (cadr (member "--user-agent" command))
                   ende-client-name))
    (should (member "--url" command))))

(ert-deftest ende-curl-loopback-target-is-pinned ()
  (let* ((ende-allow-environment-proxy t)
         (url "http://localhost:8080/v1/chat/completions")
         (command (ende-curl-command (list :url url) "/tmp/headers" nil)))
    (should (equal (cadr (member "--resolve" command)) "localhost:8080:127.0.0.1"))
    (should (equal (cadr (member "--noproxy" command)) "*"))
    (should (equal (cadr (member "--max-redirs" command)) "0"))
    (should (equal (cadr (member "--url" command)) url))
    (should-not (member "--location" command))))



(ert-deftest ende-test/curl-command-leaves-idle-timeout-to-elisp ()
  (let ((ende-request-timeout nil)
        (ende-response-idle-timeout 60))
    (let ((command (ende-curl-command
                    '(:url "https://example.test/v1") "/tmp/headers" nil)))
      (should-not (member "--max-time" command))
      (should-not (member "--speed-time" command))
      (should-not (member "--speed-limit" command)))))



(ert-deftest ende-test/curl-idle-heartbeat-and-captured-setting ()
  (ende-test--with-idle-request
   180
   (lambda (process)
     (let ((ende-response-idle-timeout 999)
           (parser (ende-protocol-make-sse)))
       ;; A comment yields no SSE event, but is still response activity.
       (should-not (ende-protocol-sse-feed parser ": heartbeat\n\n"))
       (process-put process :ende-last-response-time 0)
       (cl-letf (((symbol-function 'float-time) (lambda (&optional _) 179)))
         (funcall (process-filter process) process ": heartbeat\n\n"))
       ;; The original deadline has passed; the heartbeat extended it.
       (cl-letf (((symbol-function 'float-time) (lambda (&optional _) 181)))
         (ende-curl--check-idle process))
       (should (process-live-p process))
       (should (timerp (process-get process :ende-idle-timer)))
       ;; Empty chunks must not extend the deadline.
       (cl-letf (((symbol-function 'float-time) (lambda (&optional _) 358)))
         (funcall (process-filter process) process ""))
       (cl-letf (((symbol-function 'float-time) (lambda (&optional _) 359)))
         (ende-curl--check-idle process))
       (should-not (process-live-p process))
       (should-not (process-get process :ende-idle-timer))
       (let ((failure (process-get process :ende-idle-error)))
         (should (eq (ende-error-kind failure) 'timeout))
         (should (equal (ende-error-message failure)
                        "No response data received for 180 seconds (`ende-response-idle-timeout`).")))))))



(ert-deftest ende-test/curl-idle-timer-fires-before-first-response ()
  (ende-test--with-idle-request
   0.05
   (lambda (process)
     (let ((deadline (+ (float-time) 2)))
       (while (and (process-live-p process) (< (float-time) deadline))
         (accept-process-output nil 0.02)))
     (should (process-get process :ende-idle-error))
     (should-not (process-live-p process))
     (should-not (process-get process :ende-idle-timer)))))



(ert-deftest ende-test/curl-idle-cancellation-releases-timer ()
  (ende-test--with-idle-request
   180
   (lambda (process)
     (let ((timer (process-get process :ende-idle-timer)))
       (delete-process process)
       (should-not (process-get process :ende-idle-timer))
       (should-not (memq timer timer-list))
       (ende-curl--check-idle process)
       (should-not (process-get process :ende-idle-error))))))



(ert-deftest ende-test/curl-idle-error-reaches-both-transports-once ()
  (dolist (transport '(llm codex))
    (ende-test--with-idle-request
     180
     (lambda (process)
       (let* ((errors nil)
              (callbacks (list :on-error (lambda (error) (push error errors))))
              (session (if (eq transport 'llm)
                           (ende-llm--make-session :callbacks callbacks)
                         (ende-codex--make-session :callbacks callbacks)))
              (sentinel (if (eq transport 'llm)
                            #'ende-llm--process-sentinel
                          #'ende-codex--sentinel)))
         (process-put process :ende-last-response-time 0)
         (ende-curl--check-idle process)
         (funcall sentinel session process "killed\n")
         (funcall sentinel session process "killed\n")
         (should (= (length errors) 1))
         (should (eq (car errors) (process-get process :ende-idle-error))))))))



(ert-deftest ende-test/curl-idle-normal-exit-releases-timer ()
  (ende-test--with-idle-request
   180
   (lambda (process)
     (let ((timer (process-get process :ende-idle-timer))
           (deadline (+ (float-time) 2)))
       (while (and (process-live-p process) (< (float-time) deadline))
         (accept-process-output nil 0.02))
       (should (eq (process-status process) 'exit))
       (should-not (process-get process :ende-idle-error))
       (should-not (process-get process :ende-idle-timer))
       (should-not (memq timer timer-list))))
   '("true")))



(ert-deftest ende-test/curl-idle-refresh-reports-timeout-and-cleans-up ()
  (let ((ende-response-idle-timeout 180)
        (ende-codex--refresh-flights (make-hash-table :test #'equal))
        (ende-codex--refresh-processes nil)
        (ende-codex-auth-file (expand-file-name "unused-test-auth.json"))
        (cancel nil)
        (failure nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ende-codex--refresh-command)
                   (lambda (&rest _) '("sleep" "30")))
                  ((symbol-function 'process-send-string) #'ignore)
                  ((symbol-function 'process-send-eof) #'ignore))
          (setq cancel (ende-codex--refresh-auth
                        '(:refresh "test-refresh")
                        (lambda (result) (setq failure result))))
          (let* ((process (car ende-codex--refresh-processes))
                 (buffer (process-buffer process)))
            (should (timerp (process-get process :ende-idle-timer)))
            (process-put process :ende-last-response-time 0)
            (ende-curl--check-idle process)
            (should (eq (ende-error-kind failure) 'timeout))
            (should-not (buffer-live-p buffer))
            (should-not (process-get process :ende-idle-timer))
            (should (= (hash-table-count ende-codex--refresh-flights) 0))))
      (when cancel (funcall cancel)))))



(ert-deftest ende-test/default-transport-enforces-response-byte-limit ()
  (let* ((ende-max-response-bytes 16)
         (out (ende-test--run-default-transport
               "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n"
               (make-string 100 ?X))))
    (should-not (nth 0 out))
    (should (eq (ende-error-kind (nth 1 out)) 'resource))
    (should (= (nth 4 out) 1))))



(ert-deftest ende-test/response-byte-limit-retains-safe-prefix ()
  (let* ((prefix
          "data: {\"choices\":[{\"delta\":{\"content\":\"kept\"}}]}\n\n")
         (ende-max-response-bytes (string-bytes prefix))
         (out (ende-test--run-default-transport
               "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n"
               (concat prefix (make-string 100 ?X))))
         (partial (cl-find-if
                   (lambda (event)
                     (eq (plist-get event :type) :partial-result))
                   (nth 6 out))))
    (should-not (nth 0 out))
    (should (eq (ende-error-kind (nth 1 out)) 'resource))
    (should partial)
    (should (string= (plist-get (plist-get partial :result) :content)
                     "kept"))))

(ert-deftest ende-test/incomplete-generation-never-succeeds ()
  (dolist (case '((chat . "data: {\"choices\":[{\"delta\":{\"content\":\"partial\"},\"finish_reason\":\"length\"}]}\n\ndata: [DONE]\n\n")
                  (codex . "data: {\"type\":\"response.incomplete\",\"response\":{\"status\":\"incomplete\"}}\n\n")))
    (let ((out (ende-test--run-stream-request (car case) (list (cdr case)))))
      (should-not (plist-get out :result))
      (should (eq (ende-error-kind (plist-get out :error)) 'incomplete)))))

(ert-deftest ende-test/explicit-apis-share-stream-lifecycle ()
  (dolist (case '((responses openai-responses
                            "data: {\"type\":\"response.output_text.delta\",\"delta\":\"answer\"}\n\ndata: {\"type\":\"response.completed\",\"response\":{\"status\":\"completed\"}}\n\n")
                  (messages anthropic-messages
                            "data: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}\n\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"answer\"}}\n\ndata: {\"type\":\"content_block_stop\",\"index\":0}\n\ndata: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"}}\n\ndata: {\"type\":\"message_stop\"}\n\n")))
    (let* ((profile (list :api (nth 1 case) :auth-method 'none :url "https://example.test/api"
                          :model "m" :output-tokens 50 :require-sse t))
           (out (ende-test--run-stream-request (car case) (list (nth 2 case)) nil nil profile)))
      (should-not (plist-get out :error))
      (should (equal (plist-get (plist-get out :result) :content) "answer"))
      (should (plist-get out :cleaned))
      (should (eq (car (last (plist-get out :order))) :done)))))

(provide 'ende-transport-test)

;;; ende-transport-test.el ends here
