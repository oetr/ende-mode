;;; ende-openrouter-batch-test.el --- OpenRouter batch tests -*- lexical-binding: t; -*-

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

;; Coverage for batch submission, polling, results, and cancellation.

;;; Code:

(require 'ert)
(require 'ende)
(require 'ende-test-helpers)
(require 'cl-lib)
(require 'json)



(ert-deftest ende-test/openrouter-batch-tool-diagnostics-use-shared-setting ()
  (should (eq ende-tool-diagnostics t))
  (let* ((events nil)
         (request
          (ende-openrouter-batch--make-request
           :callbacks (list :on-event
                            (lambda (event) (push event events)))))
         (body
          (ende-protocol-json-read-string
           (concat
            "{\"choices\":[{\"message\":{\"content\":\"answer\","
            "\"tool_calls\":[{\"function\":{\"name\":\"read_file\","
            "\"arguments\":\"{\\\"path\\\":\\\"secret.el\\\"}\"}}]}}]}"))))
    (ende-openrouter-batch--diagnose-request request)
    (ende-openrouter-batch--diagnose-result request body)
    (let ((diagnostics (mapcar (lambda (event) (plist-get event :text))
                               (nreverse events))))
      (should (equal diagnostics
                     '("OpenRouter Batch request tools: (none)"
                       "OpenRouter Batch output items: message, tool_call(read_file)")))
      (should-not (string-match-p
                   "secret\\.el"
                   (mapconcat #'identity diagnostics "\n")))))
  (let ((ende-tool-diagnostics nil)
        (events nil))
    (let ((request
           (ende-openrouter-batch--make-request
            :callbacks (list :on-event
                             (lambda (event) (push event events))))))
      (ende-openrouter-batch--diagnose-request request)
      (ende-openrouter-batch--diagnose-result
       request (ende-protocol-json-read-string "{\"choices\":[]}"))
      (should-not events))))

(ert-deftest ende-test/openrouter-batch-profile-has-separate-endpoints ()
  (let ((profile (ende-provider-profile 'router-batch)))
    (should (eq (plist-get profile :transport) 'openrouter-batch))
    (should (string= (plist-get profile :url)
                     "https://openrouter.ai/api/v1/chat/completions"))
    (should (string= (plist-get profile :batch-url)
                     "https://openrouter.ai/api/v1/batches"))
    (should (string= (plist-get profile :batch-endpoint)
                     "/v1/chat/completions"))
    (should (equal (plist-get (ende-provider-profile 'openrouter)
                              :transport)
                   'auto))
    (should-error
     (ende-openrouter-batch--validate-options
      (plist-put (copy-sequence profile)
                 :batch-url "https://openrouter.ai/api/v1/chat/completions")))))



(ert-deftest ende-test/openrouter-batch-initial-404-retries-then-completes ()
  (let* ((profile (plist-put
                   (plist-put
                    (plist-put (copy-sequence
                                (ende-provider-profile 'router-batch))
                               :batch-poll-initial-delay 2)
                    :batch-poll-max-delay 60)
                   :batch-poll-jitter 0))
         (delays nil)
         (result nil)
         (failure nil)
         (request
          (ende-openrouter-batch--make-request
           :custom-id "ende-job-404"
           :profile profile
           :callbacks
           (list :on-done (lambda (value) (setq result value))
                 :on-error (lambda (value) (setq failure value)))))
         (group
          (ende-openrouter-batch--make-group
           :profile profile
           :requests (list request)
           :batch-id "batch-1789766830-OF6zb6TiexdSIEpfCv8N"
           :submitted-at (float-time)
           :deadline (+ (float-time) 300)
           :not-found-attempt 0)))
    (cl-letf
        (((symbol-function 'ende-openrouter-batch--schedule-poll)
          (lambda (_group &optional delay)
            (when delay
              (setq delays (append delays (list delay))))))
         ((symbol-function 'ende-openrouter-batch--status)
          (lambda (_group _status) nil)))
      (ende-openrouter-batch--poll-finished
       group 404 "{\"error\":{\"message\":\"not found\"}}" nil)
      (ende-openrouter-batch--poll-finished
       group 404 "{\"error\":{\"message\":\"not found\"}}" nil)
      (should-not failure)
      (should-not (ende-openrouter-batch--group-terminal group))
      (should (= (ende-openrouter-batch--group-not-found-attempt group) 2))
      (should (equal delays '(2 4)))
      (ende-openrouter-batch--poll-finished
       group 200 "{\"status\":\"validating\"}" nil)
      (should (ende-openrouter-batch--group-batch-visible-p group))
      (should (= (ende-openrouter-batch--group-not-found-attempt group) 0))
      (ende-openrouter-batch--poll-finished
       group 200
       (format
        (concat
         "{\"status\":\"completed\",\"results\":[{"
         "\"custom_id\":\"%s\",\"response\":{"
         "\"status_code\":200,\"body\":{\"choices\":[{"
         "\"message\":{\"content\":\"done\"}}]}}}]}"
         )
        (ende-openrouter-batch--request-custom-id request))
       nil))
    (should-not failure)
    (should (string= (plist-get result :content) "done"))
    (should (ende-openrouter-batch--group-terminal group))))



(ert-deftest ende-test/openrouter-batch-404-expires-with-context ()
  (let* ((profile (plist-put
                   (plist-put
                    (plist-put
                     (copy-sequence (ende-provider-profile 'router-batch))
                     :batch-poll-initial-delay 1)
                    :batch-poll-max-delay 4)
                   :batch-poll-jitter 0))
         (failure nil)
         (delays nil)
         (request
          (ende-openrouter-batch--make-request
           :custom-id "ende-job-expiring"
           :profile profile
           :callbacks (list :on-error (lambda (value) (setq failure value)))))
         (group
          (ende-openrouter-batch--make-group
           :profile profile
           :requests (list request)
           :batch-id "batch-1789766830-OF6zb6TiexdSIEpfCv8N"
           :submitted-at (float-time)
           :deadline (+ (float-time) 300)
           :not-found-attempt 0)))
    (setf (ende-openrouter-batch--group-profile group)
          (plist-put profile :batch-poll-not-found-max-age 3))
    (cl-letf
        (((symbol-function 'ende-openrouter-batch--schedule-poll)
          (lambda (_group &optional delay)
            (push delay delays))))
      (ende-openrouter-batch--poll-finished
       group 404 "{\"error\":{\"message\":\"not found\"}}" nil)
      (should-not failure)
      (should (equal delays '(1)))
      (setf (ende-openrouter-batch--group-submitted-at group)
            (- (float-time) 2.5))
      (ende-openrouter-batch--poll-finished
       group 404 "{\"error\":{\"message\":\"not found\"}}" nil))
    (should (ende-error-p failure))
    (should (eq (ende-error-kind failure) 'http))
    (should (string-match-p
             (regexp-quote
              "batch-1789766830-OF6zb6TiexdSIEpfCv8N")
             (ende-error-message failure)))
    (should (string-match-p
             (regexp-quote
              "https://openrouter.ai/api/v1/batches/batch-1789766830-OF6zb6TiexdSIEpfCv8N")
             (ende-error-message failure)))
    (should (string-match-p "ever being observed"
                            (ende-error-message failure)))
    (should (ende-openrouter-batch--group-terminal group))))



(ert-deftest ende-test/openrouter-batch-404-after-visibility-is-terminal ()
  (let* ((profile (ende-provider-profile 'router-batch))
         (failure nil)
         (request
          (ende-openrouter-batch--make-request
           :custom-id "ende-job-visible"
           :profile profile
           :callbacks (list :on-error (lambda (value) (setq failure value)))))
         (group
          (ende-openrouter-batch--make-group
           :profile profile
           :requests (list request)
           :batch-id "batch-visible"
           :submitted-at (float-time)
           :deadline (+ (float-time) 300)
           :not-found-attempt 0
           :batch-visible-p t)))
    (cl-letf (((symbol-function 'ende-openrouter-batch--schedule-poll)
               (lambda (&rest _arguments)
                 (error "visible batch must not retry 404"))))
      (ende-openrouter-batch--poll-finished
       group 404 "{\"error\":{\"message\":\"not found\"}}" nil))
    (should (ende-error-p failure))
    (should (string-match-p "previously visible"
                            (ende-error-message failure)))
    (should (ende-openrouter-batch--group-terminal group))))



(ert-deftest ende-test/openrouter-batch-body-is-non-streaming-and-ordered ()
  (let* ((ende-default-output-tokens 64)
         (profile (plist-put (ende-provider-profile 'router-batch)
                             :reasoning-effort "medium"))
         (group (ende-openrouter-batch--make-group :profile profile))
         (request
          (ende-openrouter-batch--make-request
           :custom-id "ende-job-1"
           :messages '( ("system" . "rules") ("user" . "question"))
           :profile profile)))
    (setf (ende-openrouter-batch--group-requests group) (list request))
    (let* ((encoded (ende-openrouter-batch--body group))
           (text (decode-coding-string encoded 'utf-8))
           (body (ende-protocol-json-read-string text))
           (request-body
            (ende-protocol-json-get
             "body"
             (aref (ende-protocol-json-get "requests" body) 0))))
      (should (string-match-p
               "\"endpoint\".*\"model\".*\"requests\"" text))
      (should (string= (ende-protocol-json-get "endpoint" body)
                       "/v1/chat/completions"))
      (should (string= (ende-protocol-json-get "custom_id"
                                               (aref
                                                (ende-protocol-json-get
                                                 "requests" body)
                                                0))
                       "ende-job-1"))
      (should-not (ende-protocol-json-has-key-p "stream" request-body))
      (should-not (ende-protocol-json-has-key-p "model" request-body))
      (should-not (ende-protocol-json-get "temperature" request-body))
      (should (= (ende-protocol-json-get "max_tokens" request-body) 64))
      (should (string=
               (ende-protocol-json-get
                "effort" (ende-protocol-json-get "reasoning" request-body))
               "medium")))))



(ert-deftest ende-test/openrouter-batches-separate-incompatible-profiles ()
  (let* ((base (ende-provider-profile 'router-batch))
         (low (plist-put (copy-tree base) :reasoning-effort "low"))
         (high (plist-put (copy-tree base) :reasoning-effort "high"))
         (temperature (plist-put (copy-tree base) :temperature 0.7)))
    (should-not (equal (ende-openrouter-batch--profile-key low)
                       (ende-openrouter-batch--profile-key high)))
    (should-not (equal (ende-openrouter-batch--profile-key base)
                       (ende-openrouter-batch--profile-key temperature)))
    (dolist (field '(:auth-source-host :auth-source-user :auth-source-port))
      (should-not
       (equal (ende-openrouter-batch--profile-key base)
              (ende-openrouter-batch--profile-key
               (plist-put (copy-tree base) field "other-account")))))))

(ert-deftest ende-test/openrouter-batch-only-submits-live-requests ()
  (let* ((profile (ende-provider-profile 'router-batch))
         (requests (mapcar
                    (lambda (id)
                      (ende-openrouter-batch--make-request
                       :custom-id id :profile profile
                       :messages (list (cons "user" id))))
                    '("cancelled" "active")))
         (group (ende-openrouter-batch--make-group
                 :profile profile :requests requests)))
    (ende-openrouter-batch--cancel-request (car requests))
    (let* ((body (ende-protocol-json-read-string
                  (ende-openrouter-batch--body group)))
           (submitted (ende-protocol-json-get "requests" body)))
      (should (= 1 (length submitted)))
      (should (equal "active" (ende-protocol-json-get "custom_id" (aref submitted 0)))))))

(ert-deftest ende-test/openrouter-batch-expiry-settles-and-releases-group ()
  (let* ((failure nil)
         (clock 1)
         (requests (cl-loop repeat 2 collect
                            (ende-openrouter-batch--make-request
                             :started-at 0
                             :callbacks (list :on-error (lambda (error) (setq failure error clock 100))))))
         (group (ende-openrouter-batch--make-group
                 :profile nil :requests requests
                 :submitted-at 0 :deadline 0))
         (ende-openrouter-batch--known-groups (list group)))
    (cl-letf (((symbol-function 'float-time) (lambda (&optional _) clock)))
      (ende-openrouter-batch--schedule-poll group))
    (should (eq (ende-error-kind failure) 'timeout))
    (should (cl-every (lambda (request)
                        (and (ende-openrouter-batch--request-settled request)
                             (= (ende-openrouter-batch--request-duration-ms request) 1000))) requests))
    (should (ende-openrouter-batch--group-terminal group))
    (should-not ende-openrouter-batch--known-groups)
    (should-not (ende-openrouter-batch--group-poll-timer group))))



(ert-deftest ende-test/openrouter-batch-completion-routes-by-custom-id ()
  (let* ((profile (ende-provider-profile 'router-batch))
         (events nil)
         (result nil)
         (failure nil)
         (request
          (ende-openrouter-batch--make-request
           :custom-id "ende-job-2"
           :profile profile
           :callbacks
           (list :on-event (lambda (event) (push event events))
                 :on-done (lambda (value) (setq result value))
                 :on-error (lambda (value) (setq failure value)))))
         (cancelled-request
          (ende-openrouter-batch--make-request
           :custom-id "unrelated" :profile profile :cancelled t))
         (group
          (ende-openrouter-batch--make-group
           :profile profile :requests (list request cancelled-request)))
         (payload
          (ende-protocol-json-read-string
           (concat
            "{\"status\":\"completed\","
            "\"usage\":{\"prompt_tokens\":5,\"completion_tokens\":3},"
            "\"results\":["
            "{\"custom_id\":\"unrelated\",\"error\":{\"message\":\"ignored\"}},"
            "{\"custom_id\":\"ende-job-2\",\"response\":{"
            "\"status_code\":200,\"body\":{\"model\":\"actual/model\","
            "\"choices\":[{\"message\":{\"content\":\"answer\","
            "\"reasoning_content\":\"thought\"},\"finish_reason\":\"stop\"}]}}}]}"))))
    (ende-openrouter-batch--finish-completed group payload)
    (should-not failure)
    (should (string= (plist-get result :content) "answer"))
    (should (string= (plist-get result :thinking) "thought"))
    (should (string= (plist-get result :model) "actual/model"))
    (should (= (plist-get result :tokens) 3))
    (should (cl-find-if (lambda (event) (eq (plist-get event :type) :metadata))
                        events))
    (should (cl-find-if (lambda (event) (eq (plist-get event :type) :usage))
                        events))))



(ert-deftest ende-test/openrouter-batch-result-failure-does-not-wedge-group ()
  (let* ((profile (ende-provider-profile 'router-batch))
         (failure nil)
         (request
          (ende-openrouter-batch--make-request
           :custom-id "ende-job-3"
           :profile profile
           :callbacks (list :on-error (lambda (value) (setq failure value)))))
         (group
          (ende-openrouter-batch--make-group
           :profile profile :requests (list request)))
         (payload
          (ende-protocol-json-read-string
           "{\"status\":\"completed\",\"results\":[{\"custom_id\":\"ende-job-3\",\"error\":{\"message\":\"provider failed\"}}]}")))
    (ende-openrouter-batch--finish-completed group payload)
    (should (ende-error-p failure))
    (should (eq (ende-error-kind failure) 'provider))
    (should-not (ende-openrouter-batch--group-stage group))
    (should (ende-openrouter-batch--group-terminal group))))



(ert-deftest ende-test/openrouter-batch-request-cancellation-cleans-pending-group ()
  (ende-test--reset)
  (let ((profile (plist-put (ende-provider-profile 'router-batch)
                            :batch-window 60))
        (cancel nil)
        (done nil)
        (failure nil))
    (unwind-protect
        (progn
          (ende-openrouter-batch-default
           '(("user" . "question"))
           (list :on-start (lambda (function) (setq cancel function))
                 :on-done (lambda (_) (setq done t))
                 :on-error (lambda (_) (setq failure t)))
           profile)
          (should (functionp cancel))
          (funcall cancel)
          (should-not done)
          (should-not failure)
          (should (= (hash-table-count ende-openrouter-batch--groups) 0))
          (should-not ende-openrouter-batch--known-groups))
      (ende-openrouter-batch-abort))))



(ert-deftest ende-test/openrouter-batch-json-command-uses-json-accept ()
  (let ((command
         (ende-curl-json-command
          '(:url "https://openrouter.ai/api/v1/chat/completions"
		 :auth-source-host "openrouter.ai"
		 :auth-source-user "ende"
		 :auth-source-port 443)
          "/tmp/headers" nil "GET" "application/json" nil
          "https://openrouter.ai/api/v1/batches/batch_1")))
    (should (member "Accept: application/json" command))
    (should-not (member "Accept: text/event-stream" command))
    (should-not (member "@-" command))
    (should (equal (cadr (member "--url" command))
                   "https://openrouter.ai/api/v1/batches/batch_1"))))

(ert-deftest ende-test/openrouter-batch-last-member-stops-local-work ()
  (let* ((ende-openrouter-batch--known-groups nil)
         (ende-openrouter-batch--groups (make-hash-table :test #'equal))
         (calls 0)
         (requests (cl-loop repeat 2 collect
                            (ende-openrouter-batch--make-request
                             :messages '(("user" . "question"))
                             :callbacks (list :on-event (lambda (_) (cl-incf calls))
                                              :on-done (lambda (_) (cl-incf calls))
                                              :on-error (lambda (_) (cl-incf calls))))))
         (buffer (generate-new-buffer " *ende-batch-cancel*"))
         (process (make-process :name "ende-batch-cancel" :command '("cat")
                                :buffer nil :connection-type 'pipe :noquery t))
         (stage (list :request (list :process process :response-buffer buffer)
                      :callback (lambda (&rest _) (cl-incf calls))))
         (group (ende-openrouter-batch--make-group
                 :requests requests :body "payload" :stage stage
                 :poll-timer (run-at-time 60 nil #'ignore))))
    (unwind-protect
        (progn
          (ende-openrouter-batch--register-group group)
          (ende-openrouter-batch--cancel-request (car requests))
          (should (process-live-p process))
          (should (ende-openrouter-batch--group-poll-timer group))
          (should-not (ende-openrouter-batch--group-terminal group))
          (ende-openrouter-batch--cancel-request (cadr requests))
          (should (ende-openrouter-batch--group-terminal group))
          (should-not (process-live-p process))
          (should-not (buffer-live-p buffer))
          (should-not (ende-openrouter-batch--group-stage group))
          (should-not (ende-openrouter-batch--group-poll-timer group))
          (should-not (ende-openrouter-batch--group-body group))
          (should-not ende-openrouter-batch--known-groups)
          (should-not (plist-get stage :callback))
          (dolist (request requests)
            (should-not (ende-openrouter-batch--request-callbacks request))
            (should-not (ende-openrouter-batch--request-messages request)))
          (ende-openrouter-batch--stage-sentinel group stage process "finished")
          (ende-openrouter-batch--submit-finished
           group 202 "{\"id\":\"late\",\"status\":\"validating\"}" nil)
          (ende-openrouter-batch--poll-finished
           group 200 "{\"status\":\"in_progress\"}" nil)
          (ende-openrouter-batch--finish-completed group nil)
          (ende-openrouter-batch--poll group)
          (should-not (ende-openrouter-batch--group-batch-id group))
          (should-not (ende-openrouter-batch--group-poll-timer group))
          (should (zerop calls)))
      (ende-openrouter-batch-abort)
      (when (process-live-p process) (delete-process process))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))



(ert-deftest ende-test/openrouter-batch-submits-polls-and-settles ()
  (let* ((profile (plist-put (ende-provider-profile 'router-batch)
                             :batch-window 0))
         (stages nil)
         (processes nil)
         (files nil)
         (events nil)
         (result nil)
         (failure nil)
         (real-make-process (symbol-function 'make-process)))
    (cl-letf
        (((symbol-function 'ende-curl-start-request)
          (lambda (&rest arguments)
            (let* ((response-buffer (generate-new-buffer
                                     " *ende-batch-test-response*"))
                   (stderr-buffer (generate-new-buffer
                                   " *ende-batch-test-stderr*"))
                   (headers-file (make-temp-file "ende-batch-test-headers-"))
                   (process
                    (funcall real-make-process
                             :name (generate-new-buffer-name
                                    "ende-batch-test-process")
                             :buffer nil
                             :command (list "cat")
                             :connection-type 'pipe
                             :noquery t))
                   (request (list :process process
                                  :response-buffer response-buffer
                                  :stderr-buffer stderr-buffer
                                  :stderr-process nil
                                  :headers-file headers-file
                                  :config-file nil))
                   (stage (list :request request
                                :filter (plist-get arguments :filter)
                                :sentinel (plist-get arguments :sentinel)
                                :command (plist-get arguments :command))))
              (process-put process :ende-response-buffer response-buffer)
              (process-put process :ende-stderr-buffer stderr-buffer)
              (process-put process :ende-headers-file headers-file)
              (process-put process :ende-stderr-process nil)
              (setq stages (append stages (list stage))
                    processes (cons process processes)
                    files (cons headers-file files))
              request))))
      (unwind-protect
          (progn
            (ende-openrouter-batch-default
             '(("user" . "question"))
             (list :on-event (lambda (event) (push event events))
                   :on-done (lambda (value) (setq result value))
                   :on-error (lambda (value) (setq failure value)))
             profile)
            (let ((group (car ende-openrouter-batch--known-groups)))
              (ende-openrouter-batch--flush group)
              (let ((stage (nth 0 stages)))
                (with-temp-file (plist-get (plist-get stage :request)
                                           :headers-file)
                  (insert "HTTP/1.1 202 Accepted\r\n"
                          "Content-Type: application/json\r\n\r\n"))
                (funcall (plist-get stage :filter)
                         (plist-get (plist-get stage :request) :process)
                         "{\"id\":\"batch_1\",\"status\":\"validating\"}")
                (ende-test--wait-for
                 (lambda ()
                   (not (process-live-p
                         (plist-get (plist-get stage :request) :process))))
                 :description "batch submit/poll fixture process exit")
                (funcall (plist-get stage :sentinel)
                         (plist-get (plist-get stage :request) :process)
                         "finished"))
              (ende-openrouter-batch--cancel-timer
               (ende-openrouter-batch--group-poll-timer group))
              (setf (ende-openrouter-batch--group-poll-timer group) nil)
              (ende-openrouter-batch--poll group)
              (let ((stage (nth 1 stages)))
                (should (stringp
                         (plist-get (plist-get stage :request)
                                    :headers-file)))
                (with-temp-file (plist-get (plist-get stage :request)
                                           :headers-file)
                  (insert "HTTP/1.1 200 OK\r\n"
                          "Content-Type: application/json\r\n\r\n"))
                (funcall (plist-get stage :filter)
                         (plist-get (plist-get stage :request) :process)
                         (format
                          (concat
                           "{\"status\":\"completed\",\"usage\":{"
                           "\"completion_tokens\":2},\"results\":[{"
                           "\"custom_id\":\"%s\","
                           "\"response\":{\"status_code\":200,\"body\":{"
                           "\"choices\":[{\"message\":{\"content\":\"done\"}}]}}}]}"
                           )
                          (ende-openrouter-batch--request-custom-id
                           (car (ende-openrouter-batch--group-requests
                                 group)))))
                (ende-test--wait-for
                 (lambda ()
                   (not (process-live-p
                         (plist-get (plist-get stage :request) :process))))
                 :description "batch submit/poll fixture process exit")
                (funcall (plist-get stage :sentinel)
                         (plist-get (plist-get stage :request) :process)
                         "finished")))
            (should-not failure)
            (should (string= (plist-get result :content) "done"))
            (should (= (plist-get result :tokens) 2))
            (should (cl-find-if
                     (lambda (event)
                       (equal (plist-get event :status) "Submitting batch"))
                     events))
            (should (member "Accept: application/json"
                            (funcall (plist-get (nth 0 stages) :command)
                                     "/tmp/headers" nil)))
            (should-not (member "--data-binary"
				(funcall (plist-get (nth 1 stages) :command)
                                         "/tmp/headers" nil))))
        (ende-openrouter-batch-abort)
        (dolist (process processes)
          (when (process-live-p process)
            (delete-process process)))
        (dolist (file files)
          (when (file-exists-p file)
            (delete-file file)))))))



(ert-deftest ende-test/openrouter-auto-handoff-transfers-stream-ownership ()
  (dolist (action '(cancel complete))
    (ende-test--reset)
    (let* ((ende-provider-discovery-enabled nil)
           (ende-builtin-provider-settings '((openrouter :models ("openai/gpt-4o-mini"))))
           (ende-provider--runtime-settings (make-hash-table :test #'eq))
           (profile (plist-put (ende-provider-profile 'openrouter)
                               :batch-window 60))
           (processes nil)
           (stream-args nil)
           (cancel-owner nil)
           (cancellations nil)
           (events nil)
           (done-count 0)
           (error-count 0)
           (real-make-process (symbol-function 'make-process))
           (real-send-eof (symbol-function 'process-send-eof)))
      (cl-labels
          ((finish-request (process status body)
             (let ((sentinel (process-sentinel process))
                   (sentinel-finished nil))
               ;; Process exit can precede the sentinel's stage cleanup and
               ;; handoff/result callbacks.  Wait for its terminal return.
               (set-process-sentinel
                process
                (lambda (proc event)
                  (funcall sentinel proc event)
                  (when (memq (process-status proc) '(exit signal closed failed))
                    (setq sentinel-finished t))))
               (unwind-protect
                   (progn
                     (with-temp-file (process-get process :ende-headers-file)
                       (insert (format "HTTP/1.1 %d Mock\r\n" status)
                               "Content-Type: application/json\r\n\r\n"))
                     (funcall (process-filter process) process body)
                     (funcall real-send-eof process)
                     (ende-test--wait-for
                      (lambda () sentinel-finished) :timeout 5.0
                      :description "batch sentinel callback completion")
                     (should sentinel-finished))
                 (set-process-sentinel process sentinel)))))
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest args)
                     (let ((process
                            (funcall real-make-process
                                     :name (plist-get args :name)
                                     :buffer (plist-get args :buffer)
                                     :stderr (plist-get args :stderr)
                                     :command '("cat")
                                     :connection-type 'pipe :noquery t
                                     :filter (plist-get args :filter)
                                     :sentinel (plist-get args :sentinel))))
                       (process-put process :ende-test-args args)
                       (push process processes)
                       (unless stream-args
                         (setq stream-args args))
                       process)))
                  ((symbol-function 'process-send-string) #'ignore)
                  ((symbol-function 'process-send-eof) #'ignore)
                  ((symbol-function 'ende-credentials-secret)
                   (lambda (_) "openrouter-test-token")))
          (unwind-protect
              (progn
                (ende-llm-default
                 '(("user" . "question"))
                 (list :on-start (lambda (cancel)
                                   (push cancel cancellations)
                                   (setq cancel-owner cancel))
                       :on-event (lambda (event) (push event events))
                       :on-done (lambda (_) (cl-incf done-count))
                       :on-error (lambda (_) (cl-incf error-count)))
                 profile)
                (let* ((stream-process (car processes))
                       (response-buffer
                        (process-get stream-process :ende-response-buffer))
                       (stderr-buffer
                        (process-get stream-process :ende-stderr-buffer))
                       (stderr-process
                        (process-get stream-process :ende-stderr-process))
                       (headers-file
                        (process-get stream-process :ende-headers-file))
                       (config-file
                        (process-get stream-process :ende-config-file)))
                  (finish-request
                   stream-process 404
                   (concat "{\"error\":{\"message\":"
                           "\"This model is only available through the Batch API.\"}}"))
                  (should (= (length cancellations) 2))
                  (should (eq cancel-owner (car cancellations)))
                  (should-not (eq (car cancellations) (cadr cancellations)))
                  (should-not (process-live-p stream-process))
                  (should-not (buffer-live-p response-buffer))
                  (should-not (buffer-live-p stderr-buffer))
                  (should-not (process-live-p stderr-process))
                  (should-not (file-exists-p headers-file))
                  (should-not (file-exists-p config-file))
                  (let* ((group (car ende-openrouter-batch--known-groups))
                         (stream-filter (plist-get stream-args :filter))
                         (stream-sentinel (plist-get stream-args :sentinel))
                         (event-count (length events)))
                    (should group)
                    (funcall stream-filter stream-process "data: late\n\n")
                    (funcall stream-sentinel stream-process "finished\n")
                    (funcall (cadr cancellations))
                    (should (memq group ende-openrouter-batch--known-groups))
                    (should (= event-count (length events)))
                    (should (= done-count 0))
                    (should (= error-count 0))
                    (if (eq action 'cancel)
                        (progn
                          (funcall cancel-owner)
                          (should-not
                           (memq group ende-openrouter-batch--known-groups))
                          (should (= done-count 0))
                          (should (= error-count 0)))
                      (ende-openrouter-batch--flush group)
                      (let ((submit-process (car processes)))
                        (finish-request
                         submit-process 202
                         "{\"id\":\"batch_handoff\",\"status\":\"validating\"}"))
                      (ende-openrouter-batch--cancel-timer
                       (ende-openrouter-batch--group-poll-timer group))
                      (setf (ende-openrouter-batch--group-poll-timer group) nil)
                      (ende-openrouter-batch--poll group)
                      (let* ((poll-process (car processes))
                             (request (car
                                       (ende-openrouter-batch--group-requests
                                        group)))
                             (result-body
                              (format
                               (concat
                                "{\"status\":\"completed\","
                                "\"usage\":{\"completion_tokens\":2},"
                                "\"results\":[{\"custom_id\":\"%s\","
                                "\"response\":{\"status_code\":200,"
                                "\"body\":{\"choices\":[{\"message\":{"
                                "\"content\":\"done\"}}]}}}]}")
                               (ende-openrouter-batch--request-custom-id
                                request))))
                        (finish-request poll-process 200 result-body))
                      (funcall cancel-owner)
                      (funcall stream-filter stream-process "data: late\n\n")
                      (funcall stream-sentinel stream-process "finished\n")
                      (should (= done-count 1))
                      (should (= error-count 0))))))
            (ende-openrouter-batch-abort)
            (dolist (process processes)
              (when (process-live-p process)
                (delete-process process))
              (ende-curl-cleanup-resources
               (list (process-get process :ende-response-buffer)
                     (process-get process :ende-stderr-buffer))
               (list (process-get process :ende-headers-file)
                     (process-get process :ende-config-file))
               (list (process-get process :ende-stderr-process))))))))))

(ert-deftest ende-test/openrouter-auto-fallback-policy-is-specific ()
  (let ((profile '(:service openrouter :transport auto)))
    (should (ende-openrouter-batch-auto-fallback-p
             profile 404 "This model is available through the Batch API"))
    (should-not (ende-openrouter-batch-auto-fallback-p
                 profile 400 "This model is available through the Batch API"))
    (should-not (ende-openrouter-batch-auto-fallback-p
                 profile 404 "ordinary not found"))
    (should-not (ende-openrouter-batch-auto-fallback-p
                 '(:transport openrouter-batch) 404 "Batch API"))
    (should-not (ende-openrouter-batch-auto-fallback-p
                 '(:api openai-chat-completions :transport auto) 404 "Batch API"))))

(provide 'ende-openrouter-batch-test)

;;; ende-openrouter-batch-test.el ends here
