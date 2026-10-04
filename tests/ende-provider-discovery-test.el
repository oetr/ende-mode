;;; ende-provider-discovery-test.el --- Provider discovery tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Coverage for model discovery, caching, metadata, and reasoning efforts.

;;; Code:

(require 'ert)
(require 'ende)
(require 'ende-test-helpers)
(require 'cl-lib)
(require 'json)

;;;; Profiles, history, and transport command safety

(ert-deftest ende-test/provider-discovery-cache-is-bounded ()
  (let ((ende-provider--discovery-cache (make-hash-table :test #'equal))
        (ende-provider--discovery-inflight (make-hash-table :test #'equal))
        (ende-provider-discovery-cache-size 2))
    (dotimes (index 5)
      (ende-provider--finish-discovery (list index) (list :model index)))
    (should (= (hash-table-count ende-provider--discovery-cache) 2))))



(ert-deftest ende-test/provider-client-display-names ()
  (should (string= (ende-provider-client-name
                    '(:url "http://localhost:8080/v1"))
                   "localhost"))
  (should (string= (ende-provider-client-name
                    '(:url "http://192.168.1.20:8080/v1"))
                   "192.168.1.20"))
  (should (string= (ende-provider-client-name
                    '(:url "https://api.openai.com/v1/chat/completions"))
                   "OpenAI")))



(ert-deftest ende-test/provider-discovery-parses-model-metadata ()
  (let* ((profile '(:url "http://localhost:8080/v1/chat/completions"
			 :model "llama3"))
         (metadata
          (ende-provider--discovery-metadata
           profile
           (ende-protocol-json-read-string
            (concat
             "{\"object\":\"list\",\"data\":["
             "{\"id\":\"llama3\",\"name\":\"Llama 3\","
             "\"owned_by\":\"llama.cpp\",\"pricing\":{"
             "\"prompt\":\"0.000003\",\"completion\":\"0.000015\","
             "\"input_cache_read\":\"0.000001\","
             "\"input_cache_write\":\"0.000004\","
             "\"input_cache_write_1h\":\"0.000005\","
             "\"internal_reasoning\":\"0\"}},"
             "{\"id\":\"llama4\"}]}")))))
    (should (string= (ende-provider-models-url profile)
                     "http://localhost:8080/v1/models"))
    (should (eq (plist-get metadata :source) :discovery))
    (should (equal (plist-get metadata :models) '("llama3" "llama4")))
    (let ((detail (car (plist-get metadata :model-details))))
      (should (string= (plist-get detail :id) "llama3"))
      (should (string= (plist-get detail :name) "Llama 3"))
      (should (equal (plist-get detail :pricing)
                     '(:prompt "0.000003"
			       :completion "0.000015"
			       :internal-reasoning "0"
			       :input-cache-read "0.000001"
			       :input-cache-write "0.000004"
			       :input-cache-write-1h "0.000005"))))
    (should (string= (plist-get metadata :model) "llama3"))
    (should (string= (plist-get metadata :owner) "llama.cpp"))
    (should (string= (plist-get metadata :client) "llama.cpp"))))



(ert-deftest ende-test/openrouter-effort-probe-is-live-and-cache-independent ()
  (let ((ende-provider-discovery-enabled t)
        (ende-provider--discovery-cache (make-hash-table :test #'equal))
        (ende-provider--model-cache (make-hash-table :test #'equal))
        (requests nil)
        (results nil)
        (profile (ende-provider-profile 'openrouter)))
    (puthash 'metadata 'unchanged ende-provider--discovery-cache)
    (puthash 'models 'unchanged ende-provider--model-cache)
    (cl-letf
        (((symbol-function 'ende-provider--start-live-model-request)
          (lambda (_profile url callback &optional _discovery-key)
            (push url requests)
            (funcall
             callback
             (ende-protocol-json-read-string
              (concat
               "{\"data\":["
               "{\"id\":\"other\",\"reasoning\":{\"supported_efforts\":[\"none\"]}},"
               "{\"id\":\"selected\",\"reasoning\":{\"supported_efforts\":[\"high\",\"low\",\"high\"],"
               "\"default_effort\":\"low\"}}]}"))
             nil)
            (lambda () nil))))
      (dotimes (_ 2)
        (ende-provider-probe-reasoning-efforts
         profile "selected"
         (lambda (efforts default error)
           (push (list efforts default error) results))))
      (should (equal requests
                     '("https://openrouter.ai/api/v1/models"
                       "https://openrouter.ai/api/v1/models")))
      (should (equal results
                     '((("high" "low") "low" nil)
                       (("high" "low") "low" nil))))
      (should (eq (gethash 'metadata ende-provider--discovery-cache)
                  'unchanged))
      (should (eq (gethash 'models ende-provider--model-cache) 'unchanged)))))



(ert-deftest ende-test/reasoning-effort-discovery-is-limited-to-builtins ()
  (let* ((called nil)
         (result nil)
         (ende-provider-discovery-enabled t)
         (profile
          (list :name 'remote :reasoning-effort-discovery-function
                (lambda (&rest _)
                  (setq called t)))))
    (ende-provider-probe-reasoning-efforts
     profile "remote-model"
     (lambda (efforts default error)
       (setq result (list efforts default error))))
    (should-not called)
    (should-not (nth 0 result))
    (should-not (nth 1 result))
    (should (ende-error-p (nth 2 result)))
    (should (string-match-p "no live reasoning-effort endpoint"
                            (ende-error-message (nth 2 result))))))



(ert-deftest ende-test/reasoning-effort-is-serialized-only-by-supported-apis ()
  (let* ((messages (list (cons "user" "question")))
         (openrouter
          (list :model "router-model" :temperature 0.2
                :reasoning-effort-wire-api 'openrouter
                :reasoning-effort "high"))
         (responses
          (list :model "codex-model" :reasoning-effort-wire-api 'responses
                :reasoning-effort "low"))
         (unsupported (list :model "local-model" :temperature 0.2
                            :reasoning-effort "high"))
         (router-body
          (ende-protocol-json-read-string
           (json-encode (ende-llm--request-body messages openrouter))))
         (responses-body
          (ende-protocol-json-read-string
           (json-encode (ende-codex--request-body messages responses))))
         (unsupported-body
          (ende-protocol-json-read-string
           (json-encode (ende-llm--request-body messages unsupported)))))
    (should (string=
             (ende-protocol-json-get
              "effort" (ende-protocol-json-get "reasoning" router-body))
             "high"))
    (should (string=
             (ende-protocol-json-get
              "effort" (ende-protocol-json-get "reasoning" responses-body))
             "low"))
    (should-not (ende-protocol-json-has-key-p "reasoning" unsupported-body))))



(ert-deftest ende-test/provider-model-price-annotation-omits-invalid-fields ()
  (should
   (string=
    (ende-provider-model-price-annotation
     '(:pricing (:prompt "0" :completion "0.000015"
                         :input-cache-read "not-a-price"
                         :input-cache-write-1h "0.000005")))
    "  in: free  out: $15  cache-write-1h: $5")))



(ert-deftest ende-test/provider-model-price-annotation-formats-small-values ()
  (should
   (string=
    (ende-provider-model-price-annotation
     '(:pricing (:prompt "0.000003"
			 :completion "0.000015"
			 :input-cache-read "0.0000003")))
    "  in: $3  out: $15  cache-read: $0.30")))



(ert-deftest ende-test/provider-discovery-does-not-borrow-unrelated-owner ()
  (let* ((profile '(:url "https://example.test/v1/chat/completions"
			 :model "requested"))
         (metadata
          (ende-provider--discovery-metadata
           profile
           (ende-protocol-json-read-string
            "{\"data\":[{\"id\":\"other\",\"owned_by\":\"wrong-owner\"}]}"))))
    (should (equal (plist-get metadata :models) '("other")))
    (should-not (plist-get metadata :owner))
    (should-not (plist-get metadata :client))))



(ert-deftest ende-test/provider-discovery-is-async-and-cached ()
  (let ((profile '(:name local
			 :url "http://localhost:8080/v1/chat/completions"
			 :model "llama3"))
        (captured nil)
        (process nil)
        (values nil)
        (probe-values nil)
        (make-process-count 0)
        (real-make-process (symbol-function 'make-process)))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq captured args)
                 (setq make-process-count (1+ make-process-count))
                 (setq process
                       (funcall real-make-process
                                :name (plist-get args :name)
                                :buffer nil
                                :command (list "cat")
                                :connection-type 'pipe)))))
      (unwind-protect
          (progn
            (ende-provider-probe-models
             profile
             (lambda (models error)
               (push (list models error) probe-values)))
            (should (process-live-p process))
            (let* ((command (plist-get captured :command))
                   (header-position
                    (cl-position "--dump-header" command :test #'string=))
                   (headers-file (nth (1+ header-position) command)))
              (should (member "GET" command))
              (should (member "http://localhost:8080/v1/models" command))
              (with-temp-file headers-file
                (insert "HTTP/1.1 200 OK\r\n"
                        "Content-Type: application/json\r\n\r\n"))
              (funcall
               (plist-get captured :filter) process
               "{\"data\":[{\"id\":\"llama3\",\"owned_by\":\"llama.cpp\"},{\"id\":\"llama4\"}]}")
              (should (string-match-p
                       "llama3"
                       (with-current-buffer (plist-get captured :buffer)
                         (buffer-string))))
              (should (plist-get
                       (ende-provider--parse-discovery-response
                        profile (plist-get captured :buffer) headers-file)
                       :model))
              (process-send-eof process)
              (while (process-live-p process)
                (accept-process-output process 0.05))
              (funcall (plist-get captured :sentinel) process "finished\n"))
            (should (= make-process-count 1))
            (should (= (length probe-values) 1))
            (should (equal (car (car probe-values)) '("llama3" "llama4")))
            (should-not (cadr (car probe-values)))
            (ende-provider-discover
             profile (lambda (metadata) (push metadata values)))
            (should (= (length values) 1))
            (should (string= (plist-get (car values) :model) "llama3"))
            (let ((models nil)
                  (probe-error nil))
              (ende-provider-probe-models
               profile
               (lambda (value error)
                 (setq models value
                       probe-error error)))
              (should (equal models '("llama3" "llama4")))
              (should-not probe-error))
            (ende-provider-discover
             profile (lambda (metadata) (push metadata values)))
            (should (= make-process-count 1))
            (should (= (length values) 2))
            (should (string= (plist-get (car values) :model) "llama3")))
        (when (and (processp process) (process-live-p process))
          (delete-process process))
        (clrhash ende-provider--discovery-cache)
        (clrhash ende-provider--discovery-inflight)))))



(ert-deftest ende-test/model-probe-cache-shares-custom-discovery ()
  (let ((probe-count 0)
        (cancel-count 0)
        (probe-callback nil)
        (first-values nil)
        (second-values nil)
        (third-values nil)
        (ende-provider--model-cache (make-hash-table :test #'equal))
        (ende-provider--model-inflight (make-hash-table :test #'equal))
        (ende-provider--model-cancellations (make-hash-table :test #'equal))
        (ende-provider--model-generation 0))
    (let* ((probe
            (lambda (callback)
              (setq probe-callback callback)
              (cl-incf probe-count)
              (lambda () (cl-incf cancel-count))))
           (profile (list :name 'openai
                          :url "https://chatgpt.example/responses"
                          :model "gpt-saved"
                          :model-discovery-function probe))
           (changed-profile (plist-put (copy-sequence profile)
                                       :model "gpt-changed")))
      (unwind-protect
          (progn
            (ende-provider-probe-models
             profile
             (lambda (models error)
               (setq first-values (list models error))))
            (ende-provider-probe-models
             profile
             (lambda (models error)
               (setq second-values (list models error))))
            (should (= probe-count 1))
            (funcall probe-callback '("gpt-saved" "gpt-next") nil)
            (should (equal first-values
                           (list '("gpt-saved" "gpt-next") nil)))
            (should (equal second-values
                           (list '("gpt-saved" "gpt-next") nil)))
            (ende-provider-probe-models
             changed-profile
             (lambda (models error)
               (setq third-values (list models error))))
            (should (= probe-count 1))
            (should (equal third-values
                           (list '("gpt-saved" "gpt-next") nil)))
            (should (> (ende-provider-models-generation profile) 0))
            (ende-provider-clear-discovery-cache)
            (ende-provider-probe-models
             changed-profile
             (lambda (models error)
               (setq third-values (list models error))))
            (should (= probe-count 2))
            (funcall probe-callback '("gpt-new") nil)
            (should (equal third-values (list '("gpt-new") nil)))
            (should (= cancel-count 0)))
        (ende-provider-abort-discovery)
        (clrhash ende-provider--model-cache)
        (clrhash ende-provider--model-inflight)
        (clrhash ende-provider--model-cancellations)))))



(ert-deftest ende-test/model-probe-failures-are-retried-and-success-is-cached ()
  (let ((attempts 0)
        (first-error nil)
        (retry-models nil)
        (retry-error nil)
        (cached-models nil)
        (ende-provider--model-cache (make-hash-table :test #'equal))
        (ende-provider--model-inflight (make-hash-table :test #'equal))
        (ende-provider--model-cancellations (make-hash-table :test #'equal))
        (ende-provider--model-generation 0))
    (let* ((probe
            (lambda (callback)
              (cl-incf attempts)
              (if (= attempts 1)
                  (funcall
                   callback nil
                   (ende-error-create
                    :kind 'discovery
                    :message "temporary probe failure"))
                (funcall callback '("remote/first" "remote/second") nil))
              (lambda () nil)))
           (profile (list :name 'remote
                          :url "http://remote.example/v1/chat/completions"
                          :model-discovery-function probe)))
      (ende-provider-probe-models
       profile
       (lambda (_models error)
         (setq first-error error)))
      (should (= attempts 1))
      (should (ende-error-p first-error))
      (should (string= (ende-error-message first-error)
                       "temporary probe failure"))
      (should-not (ende-provider-cached-models profile))

      (ende-provider-probe-models
       profile
       (lambda (models error)
         (setq retry-models models
               retry-error error)))
      (should (= attempts 2))
      (should-not retry-error)
      (should (equal retry-models
                     '("remote/first" "remote/second")))

      (ende-provider-probe-models
       profile
       (lambda (models error)
         (setq cached-models (list models error))))
      (should (= attempts 2))
      (should (equal cached-models
                     (list '("remote/first" "remote/second") nil))))))



(ert-deftest ende-test/generic-model-probe-retries-after-failure ()
  (let* ((profile '(:name remote
			  :url "http://remote.example/v1/chat/completions"))
         (requests 0)
         (first-error nil)
         (retry-models nil)
         (retry-error nil)
         (discovery-key (ende-provider--discovery-key profile))
         (ende-provider--discovery-cache (make-hash-table :test #'equal))
         (ende-provider--model-cache (make-hash-table :test #'equal))
         (ende-provider--model-inflight (make-hash-table :test #'equal))
         (ende-provider--model-cancellations (make-hash-table :test #'equal))
         (ende-provider--model-generation 0))
    (cl-letf (((symbol-function 'ende-provider-discover)
               (lambda (_profile callback)
                 (cl-incf requests)
                 (if (= requests 1)
                     (progn
                       (puthash discovery-key
                                (list :time (float-time) :metadata nil)
                                ende-provider--discovery-cache)
                       (funcall callback nil))
                   (funcall callback '(:models ("remote/first"))))
                 (lambda () nil))))
      (ende-provider-probe-models
       profile
       (lambda (_models error)
         (setq first-error error)))
      (should (= requests 1))
      (should (ende-error-p first-error))
      (should-not (gethash discovery-key ende-provider--discovery-cache))
      (should-not (ende-provider-cached-models profile))

      (ende-provider-probe-models
       profile
       (lambda (models error)
         (setq retry-models models
               retry-error error)))
      (should (= requests 2))
      (should-not retry-error)
      (should (equal retry-models '("remote/first")))
      (should (equal (plist-get (ende-provider-cached-models profile)
                                :models)
                     '("remote/first"))))))



(ert-deftest ende-test/provider-discovery-cancellation-releases-last-waiter ()
  (let ((profile '(:name local
			 :url "http://localhost:8080/v1/chat/completions"
			 :model "llama3"))
        (process nil)
        (value nil)
        (real-make-process (symbol-function 'make-process)))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq process
                       (funcall real-make-process
                                :name (plist-get args :name)
                                :buffer nil
                                :command (list "cat")
                                :connection-type 'pipe)))))
      (unwind-protect
          (let* ((key (ende-provider--discovery-key profile))
                 (cancel
                  (ende-provider-discover
                   profile (lambda (metadata) (setq value metadata)))))
            (should (process-live-p process))
            (should (gethash key ende-provider--discovery-inflight))
            (funcall cancel)
            (should-not value)
            (should-not (process-live-p process))
            (should-not (gethash key ende-provider--discovery-inflight))
            (should-not (gethash key ende-provider--discovery-cache))
            (should-not ende-provider--discovery-processes)
            (let* ((probe-value nil)
                   (probe-error nil)
                   (probe-cancel
                    (ende-provider-probe-models
                     profile
                     (lambda (models error)
                       (setq probe-value models
                             probe-error error)))))
              (should (process-live-p process))
              (funcall probe-cancel)
              (should-not probe-value)
              (should-not probe-error)
              (should-not (process-live-p process))
              (should-not (gethash key ende-provider--discovery-inflight))
              (should-not ende-provider--discovery-processes)))
        (when (and (processp process) (process-live-p process))
          (delete-process process))
        (clrhash ende-provider--discovery-cache)
        (clrhash ende-provider--discovery-inflight)
        (setq ende-provider--discovery-processes nil)))))



(ert-deftest ende-test/provider-discovery-response-is-bounded ()
  (let ((profile '(:name local
			 :url "http://localhost:8080/v1/chat/completions"
			 :model "llama3"))
        (ende-provider-max-discovery-bytes 4)
        (captured nil)
        (process nil)
        (values nil)
        (real-make-process (symbol-function 'make-process)))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq captured args)
                 (setq process
                       (funcall real-make-process
                                :name (plist-get args :name)
                                :buffer (plist-get args :buffer)
                                :command (list "cat")
                                :connection-type 'pipe)))))
      (unwind-protect
          (progn
            (ende-provider-discover
             profile (lambda (metadata) (push metadata values)))
            (funcall (plist-get captured :filter) process "oversized")
            (should (equal values '(nil)))
            (should-not (process-live-p process))
            (should (= (hash-table-count
                        ende-provider--discovery-inflight) 0)))
        (when (and (processp process) (process-live-p process))
          (delete-process process))
        (clrhash ende-provider--discovery-cache)
        (clrhash ende-provider--discovery-inflight)))))

(provide 'ende-provider-discovery-test)

;;; ende-provider-discovery-test.el ends here
