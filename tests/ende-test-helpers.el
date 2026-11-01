;;; ende-test-helpers.el --- Shared ERT helpers for ende -*- lexical-binding: t; -*-

;;; Commentary:

;; Mock state and fixtures shared by the independent ERT suites.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'ende-provider)
(require 'ende)

(defun ende-test--custom-profile (name &rest settings)
  "Return an explicit custom provider table for NAME with SETTINGS."
  (list (cons name (ende-plist-merge
                    '(:api openai-chat-completions :auth-method none
                      :base-url "http://localhost:8080/v1" :models ("test-model")
                      :temperature 0.8)
                    settings))))

(defconst ende-test--custom-providers
  '((local :api openai-chat-completions :auth-method none
           :base-url "http://localhost:8080/v1" :models ("test-model") :temperature 0.8)
    (remote :api openai-chat-completions :auth-method none
            :base-url "https://remote.test/v1")))

(setq ende-providers '(local)
      ende-custom-providers (copy-tree ende-test--custom-providers)
      ende-builtin-provider-settings
      '((router-batch :provider openrouter :transport openrouter-batch
                      :models ("openai/gpt-4o-mini"))
        (openrouter :models ("openai/gpt-4o-mini"))
        (openai-codex :models ("gpt-5.6-luna"))))

(defconst ende-test--tool-store-file
  (expand-file-name (format "ende-tool-store-tests-%s.sqlite3" (emacs-pid))
                    temporary-file-directory))

(setq ende-tool-store-file ende-test--tool-store-file)

(defvar ende-test--title-prelude nil
  "Non-nil for fixtures exercising the title protocol rather than work only.")

(defun ende-test--work-only (job &rest _)
  "Start JOB at work for fixtures focused on work-phase behavior."
  (unless ende-test--title-prelude
    (setf (ende-job-request-phase job) 'work)))

(advice-add 'ende-agent-run :before #'ende-test--work-only)

(defvar ende-test--prompts nil)

(defvar ende-test--calls nil)

(defvar ende-test--fake-responses nil)

(defconst ende-test--dashboard-profile
  '(:name remote :api openai-chat-completions :auth-method none
    :url "https://remote.test/v1/chat/completions"
    :model "dashboard-test" :max-concurrency 2 :parallel nil))

(defun ende-test--request-profile (settings)
  "Complete synthetic request SETTINGS with explicit transport configuration."
  (ende-plist-merge
   '(:api openai-chat-completions :auth-method none
     :url "https://remote.test/v1/chat/completions")
   settings))

(defun ende-test--dashboard-request (profile)
  "Build a deterministic insertion request using PROFILE."
  (let ((position (point-max)))
    (list 'insert
          :target (ende-target-create
                   :buffer (current-buffer) :start position :end position
                   :cursor position :review-policy 'ignore)
          :profile (ende-test--request-profile profile))))

(defun ende-test--callback (call name &rest args)
  "Invoke callback NAME from CALL with ARGS when it exists."
  (let ((callback (plist-get (plist-get call :callbacks) name)))
    (when callback
      (apply callback args))))

(defun ende-test--overlay-label (overlay)
  "Return the display label attached to OVERLAY."
  (substring-no-properties
   (or (overlay-get overlay 'before-string)
       (overlay-get overlay 'after-string)
       "")))

(defun ende-test--wait-for (predicate)
  "Wait for asynchronous PREDICATE to become true."
  (let ((deadline (+ (float-time) 10)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (should (funcall predicate))))

(defun ende-test--tool-call (name arguments root &optional configuration)
  "Wait for tool NAME's result under CONFIGURATION in ROOT."
  (let* ((done nil)
         (result nil)
         (cancel nil)
         (ende-tools--register-cancellation-function
          (lambda (function) (setq cancel function))))
    (unwind-protect
        (progn
          (ende-tool-call-async
           name arguments root configuration
           (lambda (value) (setq result value done t)))
          (ende-test--wait-for (lambda () done))
          result)
      (when cancel (funcall cancel)))))

(defun ende-test--complete-call (call)
  "Complete asynchronous fake CALL using its queued response."
  (let ((response (plist-get call :response)))
    (cond
     ((eq response :stall) nil)
     ((eq response :error)
      (ende-test--callback
       call :on-error
       (ende-error-create :kind 'mock :message "mock failure")))
     ((stringp response)
      (ende-test--callback call :on-event
                           (list :type :status :status "Generating"))
      (ende-test--callback call :on-event
                           (list :type :delta :channel :content
                                 :text response))
      (ende-test--callback call :on-event
                           (list :type :usage :tokens 10 :speed 50.0))
      (ende-test--callback call :on-done (list :content response)))
     ((and (listp response) response)
      (when (or (plist-get response :reported-model)
                (plist-get response :reported-provider))
        (ende-test--callback
         call :on-event
         (list :type :metadata
               :metadata
               (list :source :response
                     :model (plist-get response :reported-model)
                     :provider (plist-get response :reported-provider)))))
      (when (stringp (plist-get response :thinking))
        (ende-test--callback
         call :on-event
         (list :type :delta :channel :thinking
               :text (plist-get response :thinking))))
      (when (stringp (plist-get response :content))
        (ende-test--callback
         call :on-event
         (list :type :delta :channel :content
               :text (plist-get response :content))))
      (when (plist-member response :usage)
        (ende-test--callback
         call :on-event
         (list :type :usage
               :tokens (plist-get response :tokens)
               :speed (plist-get response :speed)
               :usage (plist-get response :usage))))
      (when (listp (plist-get response :partial))
        (ende-test--callback
         call :on-event
         (list :type :partial-result :result (plist-get response :partial))))
      (ende-test--callback call :on-done
                           (if (plist-member response :content)
                               response
                             (plist-put (copy-sequence response)
                                        :content ""))))
     (t
      (ende-test--callback call :on-error
                           (format "unknown mock response: %S" response))))))

(defun ende-test--fake-llm (responses &optional asynchronous process-factory)
  "Return a mock LLM dispatcher for RESPONSES.
When ASYNCHRONOUS is non-nil, calls remain in `ende-test--calls' until
`ende-test--complete-call' is invoked.  PROCESS-FACTORY can provide a real
process for cancellation tests."
  (setq ende-test--fake-responses (copy-sequence responses))
  (lambda (messages callbacks &optional options)
    (push messages ende-test--prompts)
    (let ((call (list :messages messages
                      :callbacks callbacks
                      :options options
                      :response (pop ende-test--fake-responses))))
      (when process-factory
        (setq call
              (plist-put call :process (funcall process-factory)))
        (ende-test--callback
         call :on-start
         (lambda ()
           (let ((process (plist-get call :process)))
             (when (and (processp process) (process-live-p process))
               (process-put process :ende-cancelled t)
               (delete-process process))))))
      (setq ende-test--calls
            (append ende-test--calls (list call)))
      (unless asynchronous
        (ende-test--complete-call call))
      call)))

(defun ende-test--user-message (messages)
  "Return the user content from MESSAGES."
  (cdr (assoc "user" messages)))

(defun ende-test--select (start end point)
  "Activate the region START..END with point at POINT."
  (setq transient-mark-mode t)
  (goto-char point)
  (set-mark (if (= point end) start end))
  (setq mark-active t))

(defun ende-test--count-matches (regexp text)
  "Return the number of nonoverlapping REGEXP matches in TEXT."
  (let ((start 0)
        (count 0))
    (while (string-match regexp text start)
      (setq count (1+ count))
      (setq start (match-end 0)))
    count))

(defun ende-test--run-stream-request
    (transport chunks &optional cancel-on-start response-limit profile
               event-callback-error messages)
  "Run TRANSPORT with SSE CHUNKS and return its callback/resource state.
When CANCEL-ON-START is non-nil, cancel from the start callback.
RESPONSE-LIMIT dynamically sets `ende-max-response-bytes'.  PROFILE overrides
the transport's representative default profile.  EVENT-CALLBACK-ERROR, when
non-nil, is signalled from the event callback."
  (let* ((ende-provider-discovery-enabled nil)
         (ende-max-response-bytes response-limit)
         (real-make-process (symbol-function 'make-process))
         (real-process-send-string (symbol-function 'process-send-string))
         (request-args nil)
         (request-body nil)
         (config-text nil)
         (process nil)
         (events nil)
         (order nil)
         (result nil)
         (failure nil)
         (messages (or messages (list (cons "user" "question"))))
         (callbacks
          (list :on-start
                (lambda (cancel)
                  (push :start order)
                  (when cancel-on-start (funcall cancel)))
                :on-event
                (lambda (event)
                  (push event events)
                  (when (and event-callback-error
                             (eq (plist-get event :type) :delta))
                    (funcall event-callback-error))
                  (push (plist-get event :type) order))
                :on-done
                (lambda (value)
                  (setq result value)
                  (push :done order))
                :on-error
                (lambda (error)
                  (setq failure error)
                  (push :on-error order))))
         (profile
          (or profile
              (list :name 'transport-test
                    :service (when (eq transport 'codex) 'openai-codex)
                    :account (when (eq transport 'codex) 'default)
                    :api (if (eq transport 'codex) 'openai-responses 'openai-chat-completions)
                    :auth-method (if (eq transport 'codex) 'codex-oauth 'none)
                    :url (if (eq transport 'codex)
                             "https://chatgpt.com/backend-api/codex/responses"
                           "https://example.test/v1/chat/completions")
                    :model "test-model"
                    :temperature (unless (eq transport 'codex) 0.2)
                    :controls (unless (eq transport 'codex)
                                '((:temperature :path ("temperature"))))
                    :require-sse t)))
         (ende-codex-auth-file (expand-file-name "unused-test-auth.json"))
         (auth '(:access "transport-test-token"
			 :expires 9999999999999)))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq request-args args)
                 (setq process
                       (funcall real-make-process
                                :name (plist-get args :name)
                                :buffer nil :command '("cat")
                                :connection-type 'pipe :noquery t
                                :filter #'ignore :sentinel #'ignore))))
              ((symbol-function 'process-send-string)
               (lambda (target body)
                 (setq request-body (concat (or request-body "") body))
                 (funcall real-process-send-string target body)))
              ((symbol-function 'ende-codex--auth-entry) (lambda () auth))
              ((symbol-function 'ende-codex--resolve-auth)
               (lambda (callback)
                 (funcall callback auth)
                 nil)))
      (unwind-protect
          (progn
            (pcase transport
              ('codex (ende-codex-default messages callbacks profile))
              ('responses (ende-responses-default messages callbacks profile))
              ('messages (ende-messages-default messages callbacks profile))
              (_ (ende-llm-default messages callbacks profile)))
            (unless cancel-on-start
              (let* ((command (plist-get request-args :command))
                     (headers-file
                      (cadr (member "--dump-header" command)))
                     (config-file (cadr (member "--config" command)))
                     (filter (plist-get request-args :filter))
                     (sentinel (plist-get request-args :sentinel)))
                (when (and config-file (file-readable-p config-file))
                  (with-temp-buffer
                    (insert-file-contents config-file)
                    (setq config-text (buffer-string))))
                (with-temp-file headers-file
                  (insert "HTTP/1.1 200 OK\r\n"
                          (if (eq transport 'codex)
                              "\r\n"
                            "Content-Type: text/event-stream\r\n\r\n")))
                (dolist (chunk chunks)
                  (funcall filter process chunk))
                (when (process-live-p process)
                  (process-send-eof process)
                  (while (process-live-p process)
                    (accept-process-output process 0.05)))
                (when (and (processp process)
                           (memq (process-status process)
                                 '(exit signal closed failed)))
                  (funcall sentinel process "finished\n"))))
            (let* ((command (plist-get request-args :command))
                   (response-buffer
                    (and process (process-get process :ende-response-buffer)))
                   (stderr-buffer
                    (and process (process-get process :ende-stderr-buffer)))
                   (stderr-process
                    (and process (process-get process :ende-stderr-process)))
                   (headers-file
                    (and process (process-get process :ende-headers-file)))
                   (config-file
                    (and process (process-get process :ende-config-file))))
              (list :events (nreverse events)
                    :order (nreverse order)
                    :result result :error failure
                    :request-body request-body :command command
                    :config-text config-text
                    :cleaned
                    (and (not (and (processp process)
                                   (process-live-p process)))
                         (not (buffer-live-p response-buffer))
                         (not (buffer-live-p stderr-buffer))
                         (not (and (processp stderr-process)
                                   (process-live-p stderr-process)))
                         (not (and headers-file
                                   (file-exists-p headers-file)))
                         (not (and config-file
                                   (file-exists-p config-file)))))))
        (when (and (processp process) (process-live-p process))
          (delete-process process))))))

(defun ende-test--reset ()
  "Reset global state and clean test-created processes and buffers."
  (dolist (file (list ende-test--tool-store-file
                      (concat ende-test--tool-store-file "-wal")
                      (concat ende-test--tool-store-file "-shm")
                      (concat ende-test--tool-store-file "-journal")))
    (when (file-exists-p file)
      (delete-file file)))
  (setq ende-tool-store-file ende-test--tool-store-file)
  (ende-abort-queue)
  (dolist (family (copy-sequence ende--finished-families))
    (ende-queue-evict-family family))
  (when (fboundp #'ende-openrouter-batch-abort)
    (ende-openrouter-batch-abort))
  (when (buffer-live-p ende-rubber-duck--buffer)
    (kill-buffer ende-rubber-duck--buffer))
  (setq ende-rubber-duck--buffer nil)
  (dolist (call ende-test--calls)
    (let ((process (plist-get call :process)))
      (when (and (processp process) (process-live-p process))
        (delete-process process))))
  (setq ende-model-selection--last-used nil)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (setq ende-model-selection--buffer-selection nil)))
  (setq ende-test--prompts nil
        ende-test--calls nil
        ende-test--fake-responses nil
        ende-model-selection--last-used nil
        ende-queue nil
        ende--queue-tail nil
        ende--queue-suspended nil
        ende--active-jobs nil
        ende--aborting nil
        ende--dispatching nil
        ende--next-job-id 0
        ende--next-dispatch-order 0
        ende--next-fence-id 0
        ende-queue-paused nil
        ende-openrouter-batch--next-request-id 0
        ende-openrouter-batch--known-groups nil
        ende-summary--next-source-id 0)
  (when (boundp 'ende-openrouter-batch--groups)
    (clrhash ende-openrouter-batch--groups))
  (ende-provider-abort-discovery)
  (clrhash ende-provider--catalog-cache)
  (clrhash ende-provider--discovery-failures)
  (clrhash ende-provider--catalog-inflight)
  (setq ende-provider--model-generation 0)
  (clrhash ende-selector--model-warnings)
  (clrhash ende-model-selection--provenance)
  (clrhash ende-model-selection--buffer-selections)
  (clrhash ende-model-selection--shared-control-preferences)
  (clrhash ende-model-selection--buffer-control-preferences)
  (ende-provider-clear-runtime-settings)
  (clrhash ende-history--entries)
  (when (buffer-live-p ende-history--buffer-object)
    (kill-buffer ende-history--buffer-object))
  (setq ende-history--buffer-object nil)
  (when (buffer-live-p ende-summary--buffer-object)
    (kill-buffer ende-summary--buffer-object))
  (setq ende-summary--buffer-object nil)
  (clrhash ende-summary--source-buffers)
  (when (buffer-live-p ende-chat--buffer-object)
    (kill-buffer ende-chat--buffer-object))
  (setq ende-chat--buffer-object nil
        ende-chat--next-id 0)
  (ende-ui-clear-all))

(defmacro ende-test--with-buffer (contents &rest body)
  "Evaluate BODY in a temporary buffer containing CONTENTS."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,contents)
     ,@body))

(defun ende-test--anchor-edit (anchor position text &rest properties)
  "Build an anchor edit with optional additional PROPERTIES."
  (append (list (cons "anchor" anchor)
                (cons "position" position)
                (cons "text" text))
          properties))

(provide 'ende-test-helpers)

;;; ende-test-helpers.el ends here
