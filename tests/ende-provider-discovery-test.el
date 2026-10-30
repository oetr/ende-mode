;;; ende-provider-discovery-test.el --- Provider discovery tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Coverage for model discovery, caching, metadata, and reasoning efforts.

;;; Code:

(require 'ert)
(require 'ende)
(require 'ende-test-helpers)
(require 'cl-lib)
(require 'json)

(ert-deftest ende-test/provider-discovery-cache-is-bounded ()
  (let ((ende-provider--catalog-cache (make-hash-table :test #'equal))
        (ende-provider-discovery-cache-size 2))
    (dotimes (index 5)
      (when (>= (hash-table-count ende-provider--catalog-cache)
                ende-provider-discovery-cache-size)
        (ende-provider--prune-cache ende-provider--catalog-cache))
      (puthash (list index)
               (list :time (float index) :models (list (number-to-string index)))
               ende-provider--catalog-cache))
    (should (= (hash-table-count ende-provider--catalog-cache) 2))))



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
          (ende-provider--catalog-metadata
           profile
           (ende-provider--catalog-from-payload
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
             "{\"id\":\"llama4\"}]}"))))))
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
        (ende-provider--catalog-cache (make-hash-table :test #'equal))
        (requests nil)
        (results nil)
        (profile (ende-provider-profile 'openrouter)))
    (puthash 'metadata 'unchanged ende-provider--catalog-cache)
    (cl-letf
        (((symbol-function 'ende-provider--start-live-model-request)
          (lambda (_profile url callback)
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
      (should (eq (gethash 'metadata ende-provider--catalog-cache)
                  'unchanged)))))



(ert-deftest ende-test/reasoning-effort-discovery-uses-profile-probe-and-static-choices ()
  (let* ((called nil)
         (result nil)
         (ende-provider-discovery-enabled t)
         (profile
          (list :name 'remote :reasoning-effort-probe-function
                (lambda (_profile model callback)
                  (setq called model)
                  (funcall callback '("low" "high") "low" nil)
                  (lambda () nil)))))
    (ende-provider-probe-reasoning-efforts
     profile "remote-model"
     (lambda (efforts default error)
       (setq result (list efforts default error))))
    (should (equal called "remote-model"))
    (should (equal (car result) '("low" "high")))
    (should (equal (cadr result) "low"))
    (should-not (nth 2 result)))
  (let ((ende-provider-discovery-enabled nil)
        (result nil))
    (ende-provider-probe-reasoning-efforts
     '(:name static
	     :controls ((:reasoning-effort :path ("effort")
					   :choices ("low" "high"))))
     "static-model"
     (lambda (efforts default error)
       (setq result (list efforts default error))))
    (should (equal (car result) '("low" "high")))
    (should-not (cadr result))
    (should-not (nth 2 result))))



(ert-deftest ende-test/reasoning-effort-uses-configured-chat-and-response-mappings ()
  (let* ((messages (list (cons "user" "question")))
         (openrouter
          (list :model "router-model" :temperature 0.2
                :controls '((:temperature :path ("temperature"))
                            (:reasoning-effort :path ("reasoning" "effort")))
                :reasoning-effort "high"))
         (responses
          (list :model "codex-model"
                :controls '((:reasoning-effort :path ("reasoning" "effort")))
                :reasoning-effort "low"))
         (router-body
          (ende-protocol-json-read-string
           (json-encode (ende-llm--request-body messages openrouter))))
         (responses-body
          (ende-protocol-json-read-string
           (json-encode (ende-codex--request-body messages responses)))))
    (should (string=
             (ende-protocol-json-get
              "effort" (ende-protocol-json-get "reasoning" router-body))
             "high"))
    (should (string=
             (ende-protocol-json-get
              "effort" (ende-protocol-json-get "reasoning" responses-body))
             "low"))))



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
          (ende-provider--catalog-metadata
           profile
           (ende-provider--catalog-from-payload
            (ende-protocol-json-read-string
             "{\"data\":[{\"id\":\"other\",\"owned_by\":\"wrong-owner\"}]}")))))
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
                       (ende-provider--catalog-metadata
                        profile
                        (ende-provider--catalog-from-payload
                         (ende-protocol-json-read-string
                          (with-current-buffer (plist-get captured :buffer)
                            (buffer-string)))))
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
        (clrhash ende-provider--catalog-cache)
        (clrhash ende-provider--catalog-inflight)))))



(ert-deftest ende-test/model-probe-cache-shares-custom-discovery ()
  (let ((probe-count 0)
        (cancel-count 0)
        (probe-callback nil)
        (first-values nil)
        (second-values nil)
        (third-values nil)
        (ende-provider--catalog-cache (make-hash-table :test #'equal))
        (ende-provider--catalog-inflight (make-hash-table :test #'equal))
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
        (clrhash ende-provider--catalog-cache)
        (clrhash ende-provider--catalog-inflight)))))



(ert-deftest ende-test/model-probe-failures-are-retried-and-success-is-cached ()
  (let ((attempts 0)
        (first-error nil)
        (retry-models nil)
        (retry-error nil)
        (cached-models nil)
        (ende-provider--catalog-cache (make-hash-table :test #'equal))
        (ende-provider--catalog-inflight (make-hash-table :test #'equal))
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
         (ende-provider--catalog-cache (make-hash-table :test #'equal))
         (ende-provider--discovery-failures (make-hash-table :test #'equal))
         (ende-provider--catalog-inflight (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'ende-provider--start-live-model-request)
               (lambda (_profile _url callback)
                 (cl-incf requests)
                 (if (= requests 1)
                     (funcall callback nil
                              (ende-error-create :kind 'transport
                                                 :message "temporary"))
                   (funcall
                    callback
                    (ende-protocol-json-read-string
                     "{\"data\":[{\"id\":\"remote/first\"}]}")
                    nil))
                 (lambda () nil))))
      (ende-provider-probe-models
       profile (lambda (_models error) (setq first-error error)))
      (should (= requests 1))
      (should (ende-error-p first-error))
      (should-not (ende-provider--cached-failure-p
                   (ende-provider--catalog-key profile)))
      (ende-provider-probe-models
       profile (lambda (models error)
                 (setq retry-models models retry-error error)))
      (should (= requests 2))
      (should-not retry-error)
      (should (equal retry-models '("remote/first"))))))

(ert-deftest ende-test/model-discovery-rejects-empty-and-failed-catalogs ()
  (dolist (custom '(nil t))
    (dolist (failed '(nil t))
      (let* ((ende-provider--catalog-cache (make-hash-table :test #'equal))
             (ende-provider--catalog-inflight (make-hash-table :test #'equal))
             (failure (and failed (ende-error-create :kind 'transport
                                                     :message "unavailable")))
             (notifications nil)
             (complete (lambda (callback)
                         (funcall callback
                                  (if custom '("partial")
                                    (ende-protocol-json-read-string
                                     "{\"data\":[{\"id\":\"partial\"}]}"))
                                  failure)
                         (funcall callback nil nil)))
             (profile (list :name 'remote
                            :url "http://remote.example/v1/chat/completions"
                            :model-discovery-function
                            (and custom (lambda (callback)
                                          (if failed (funcall complete callback)
                                            (funcall callback nil nil)))))))
        (cl-letf (((symbol-function 'ende-provider--start-live-model-request)
                   (lambda (_profile _url callback)
                     (if failed (funcall complete callback)
                       (funcall callback nil nil)))))
          (ende-provider-probe-models
           profile (lambda (models error) (push (list models error) notifications))))
        (should (= (length notifications) 1))
        (should-not (caar notifications))
        (let ((error (cadar notifications)))
          (should (if failed (eq error failure)
                    (equal (ende-error-message error) "Provider returned no models"))))
        (should-not (ende-provider-cached-models profile))))))

(ert-deftest ende-test/runtime-and-model-consumers-share-one-catalog-flight ()
  (let* ((profile '(:name remote
                    :url "http://remote.example/v1/chat/completions"
                    :model "remote/selected"))
         (key (ende-provider--catalog-key profile))
         (requests 0)
         (live-callback nil)
         (cancel-count 0)
         (runtime-values nil)
         (model-values nil)
         (ende-provider-discovery-enabled t)
         (ende-provider-discovery-ttl 30)
         (ende-provider--catalog-cache (make-hash-table :test #'equal))
         (ende-provider--discovery-failures (make-hash-table :test #'equal))
         (ende-provider--catalog-inflight (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'ende-provider--start-live-model-request)
               (lambda (_profile _url callback)
                 (cl-incf requests)
                 (setq live-callback callback)
                 (lambda () (cl-incf cancel-count)))))
      (let ((cancel-runtime
             (ende-provider-discover profile
                                     (lambda (value)
                                       (push value runtime-values))))
            (cancel-model
             (ende-provider-probe-models
              profile (lambda (models error)
                        (push (list models error) model-values)))))
        (should (= requests 1))
        (funcall cancel-runtime)
        (should (= cancel-count 0))
        (funcall live-callback nil
                 (ende-error-create :kind 'transport :message "offline"))
        (should-not runtime-values)
        (should (= (length model-values) 1))
        (should (ende-error-p (cadar model-values)))
        (should-not (ende-provider--cached-failure-p key))
        (funcall cancel-model))
      (let ((cancel-runtime
             (ende-provider-discover profile
                                     (lambda (value)
                                       (push value runtime-values)))))
        (should (= requests 2))
        (funcall live-callback nil
                 (ende-error-create :kind 'transport :message "offline"))
        (should (null (car runtime-values)))
        (should (ende-provider--cached-failure-p key))
        (let ((metadata nil))
          (ende-provider-discover profile (lambda (value) (setq metadata value)))
          (should (= requests 2))
          (should-not metadata))
        (funcall cancel-runtime))
      (let ((probe-result nil))
        (ende-provider-probe-models
         profile (lambda (models error)
                   (setq probe-result (list models error))))
        (should (= requests 3))
        (funcall live-callback
                 (ende-protocol-json-read-string
                  "{\"data\":[{\"id\":\"remote/selected\",\"name\":\"Selected\"}]}")
                 nil)
        (should (equal probe-result (list '("remote/selected") nil))))
      (ende-provider-clear-discovery-cache)
      (let ((catalog-callback nil))
        (ende-provider-discover
         profile (lambda (metadata) (push metadata runtime-values)))
        (ende-provider-probe-models
         profile (lambda (models error)
                   (setq catalog-callback (list models error))))
        (should (= requests 4))
        (funcall live-callback
                 (ende-protocol-json-read-string
                  "{\"data\":[{\"id\":\"remote/selected\",\"name\":\"Selected\"}]}")
                 nil)
        (should (equal catalog-callback '(("remote/selected") nil)))
        (should (equal (plist-get (ende-provider-cached-models profile) :models)
                       '("remote/selected")))
        (should (equal (plist-get (car runtime-values) :models)
                       '("remote/selected")))
        (should (equal (plist-get (car (ende-provider-model-details profile))
                                  :name)
                       "Selected"))))))

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
          (let* ((key (ende-provider--catalog-key profile))
                 (cancel
                  (ende-provider-discover
                   profile (lambda (metadata) (setq value metadata)))))
            (should (process-live-p process))
            (should (gethash key ende-provider--catalog-inflight))
            (funcall cancel)
            (should-not value)
            (should-not (process-live-p process))
            (should-not (gethash key ende-provider--catalog-inflight))
            (should-not (gethash key ende-provider--catalog-cache))
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
              (should-not (gethash key ende-provider--catalog-inflight))
              (should-not ende-provider--discovery-processes)))
        (when (and (processp process) (process-live-p process))
          (delete-process process))
        (clrhash ende-provider--catalog-cache)
        (clrhash ende-provider--catalog-inflight)
        (setq ende-provider--discovery-processes nil)))))

(ert-deftest ende-test/provider-discovery-cancelling-one-waiter-keeps-request ()
  (let ((profile '(:name local
			 :url "http://localhost:8080/v1/chat/completions"
			 :model "llama3"))
        (process nil)
        (make-process-count 0)
        (first-value :pending)
        (second-value :pending)
        (ende-provider-discovery-enabled t)
        (ende-provider--catalog-cache (make-hash-table :test #'equal))
        (ende-provider--catalog-inflight (make-hash-table :test #'equal))
        (ende-provider--discovery-processes nil)
        (real-make-process (symbol-function 'make-process))
        (real-send-eof (symbol-function 'process-send-eof)))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (cl-incf make-process-count)
                 (setq process
                       (funcall real-make-process
                                :name (plist-get args :name)
                                :buffer (plist-get args :buffer)
                                :command '("cat")
                                :connection-type 'pipe :noquery t
                                :filter (plist-get args :filter)
                                :sentinel (plist-get args :sentinel)))))
              ((symbol-function 'process-send-string) #'ignore)
              ((symbol-function 'process-send-eof) #'ignore))
      (unwind-protect
          (let* ((cancel-first
                  (ende-provider-discover
                   profile (lambda (value) (setq first-value value))))
                 (cancel-second
                  (ende-provider-discover
                   profile (lambda (value) (setq second-value value))))
                 (key (ende-provider--catalog-key profile))
                 (response-buffer (process-get process :ende-response-buffer))
                 (headers-file (process-get process :ende-headers-file))
                 (filter (process-filter process)))
            (should (= make-process-count 1))
            (funcall cancel-first)
            (should (process-live-p process))
            (should (= (length
                        (ende-provider--catalog-flight-waiters
                         (gethash key ende-provider--catalog-inflight)))
                       1))
            (with-temp-file headers-file
              (insert "HTTP/1.1 200 OK\r\n\r\n"))
            (funcall filter process "{\"data\":[{\"id\":\"llama3\"}]}")
            (funcall real-send-eof process)
            (while (process-live-p process)
              (accept-process-output process 0.01))
            (should (eq first-value :pending))
            (should (plist-get second-value :models))
            (should (equal (plist-get second-value :models) '("llama3")))
            (should-not (gethash key ende-provider--catalog-inflight))
            (should-not (buffer-live-p response-buffer))
            (should-not (file-exists-p headers-file))
            (funcall cancel-second))
        (when (and (processp process) (process-live-p process))
          (delete-process process))
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
                        ende-provider--catalog-inflight) 0)))
        (when (and (processp process) (process-live-p process))
          (delete-process process))
        (clrhash ende-provider--catalog-cache)
        (clrhash ende-provider--catalog-inflight)))))

(provide 'ende-provider-discovery-test)

;;; ende-provider-discovery-test.el ends here
