;;; ende-test-helpers.el --- Shared ERT helpers for ende -*- lexical-binding: t; -*-

;;; Commentary:

;; Mock state and fixtures shared by the independent ERT suites.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'ende-provider)
(require 'ende)

(defvar ende-test--prompts nil)

(defvar ende-test--calls nil)

(defvar ende-test--fake-responses nil)

(defconst ende-test--dashboard-profile
  '(:name remote :model "dashboard-test" :max-concurrency 2 :parallel nil))

(defun ende-test--dashboard-request (profile)
  "Build a deterministic insertion request using PROFILE."
  (let ((position (point-max)))
    (list 'insert
          :target (ende-target-create
                   :buffer (current-buffer) :start position :end position
                   :cursor position :review-policy 'ignore)
          :profile profile)))

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
      (ende-test--callback call :on-done response))
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
      (ende-test--callback call :on-done response))
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
    (transport chunks &optional cancel-on-start response-limit profile)
  "Run TRANSPORT with SSE CHUNKS and return its callback/resource state.
When CANCEL-ON-START is non-nil, cancel from the start callback.
RESPONSE-LIMIT dynamically sets `ende-max-response-bytes'.  PROFILE overrides
the transport's representative default profile."
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
         (messages (list (cons "user" "question")))
         (callbacks
          (list :on-start
                (lambda (cancel)
                  (push :start order)
                  (when cancel-on-start (funcall cancel)))
                :on-event
                (lambda (event)
                  (push event events)
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
                    :url (if (eq transport 'codex)
                             "https://chatgpt.com/backend-api/codex/responses"
                           "https://example.test/v1/chat/completions")
                    :model "test-model" :temperature 0.2 :require-sse t)))
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
              ((symbol-function 'ende-codex--resolve-auth)
               (lambda (callback)
                 (funcall callback auth)
                 nil)))
      (unwind-protect
          (progn
            (if (eq transport 'codex)
                (ende-codex-default messages callbacks profile)
              (ende-llm-default messages callbacks profile))
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
  (ende-abort-queue)
  (when (fboundp #'ende-openrouter-batch-abort)
    (ende-openrouter-batch-abort))
  (when (buffer-live-p ende-rubber-duck--buffer)
    (kill-buffer ende-rubber-duck--buffer))
  (setq ende-rubber-duck--buffer nil)
  (dolist (call ende-test--calls)
    (let ((process (plist-get call :process)))
      (when (and (processp process) (process-live-p process))
        (delete-process process))))
  (setq ende-test--prompts nil
        ende-test--calls nil
        ende-test--fake-responses nil
        ende-model-selection--last-used nil
        ende-queue nil
        ende--queue-tail nil
        ende--queue-suspended nil
        ende--current-job nil
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
  (clrhash ende-provider--discovery-cache)
  (clrhash ende-provider--discovery-inflight)
  (clrhash ende-provider--model-cache)
  (clrhash ende-provider--model-inflight)
  (clrhash ende-provider--model-cancellations)
  (setq ende-provider--model-generation 0)
  (clrhash ende-selector--model-validations)
  (clrhash ende-selector--effort-selections)
  (clrhash ende-model-selection--buffer-selections)
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
