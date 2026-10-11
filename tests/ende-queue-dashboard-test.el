;;; ende-queue-dashboard-test.el --- Queue dashboard behavior -*- lexical-binding: t; -*-

;;; Commentary:

;; Behavioral coverage for dashboard state, views, and actions using
;; deterministic fake asynchronous providers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ende-test-helpers)
(require 'ende-queue-ui)
(require 'ende-chat-test-helpers)

(defun ende-test--dashboard-reset ()
  "Reset scheduler state and tear down dashboard-only state."
  (ende-test--reset)
  (when (buffer-live-p ende-queue-ui--buffer)
    (kill-buffer ende-queue-ui--buffer))
  (clrhash ende-queue-ui--marked-ids)
  (clrhash ende-queue-ui--expanded-jobs)
  (clrhash ende-queue-ui--expanded-fences)
  (clrhash ende-queue-ui--folded-buffers)
  (setq ende-queue-ui--buffer-order nil))

(defun ende-test--dashboard-job-row (dashboard id)
  "Return the rendered summary line for job ID in DASHBOARD."
  (with-current-buffer dashboard
    (let ((position (ende-queue-ui--position-for-key (cons 'job id))))
      (when position
        (save-excursion
          (goto-char position)
          (buffer-substring-no-properties
           (line-beginning-position) (line-end-position)))))))

(defun ende-test--dashboard-token-cell (dashboard id)
  "Return the rendered Tokens cell for job ID in DASHBOARD."
  (with-current-buffer dashboard
    (let* ((columns (ende-queue-ui--columns ende-queue-ui-view))
           (column (cl-position 'tokens columns :key #'car))
           (width (nth 1 (nth column columns)))
           (offset (+ 3 (cl-loop for entry in (seq-take columns column)
                                 sum (1+ (nth 1 entry)))))
           (position (ende-queue-ui--position-for-key (cons 'job id))))
      (when position
        (save-excursion
          (goto-char position)
          (string-trim
           (buffer-substring-no-properties
            (+ (line-beginning-position) offset)
            (+ (line-beginning-position) offset width))))))))

(defun ende-test--dashboard-face-span-p (dashboard text face)
  "Return non-nil if TEXT in DASHBOARD has exactly FACE.
When FACE is non-nil, it must also end precisely at TEXT's boundaries."
  (with-current-buffer dashboard
    (save-excursion
      (goto-char (point-min))
      (let (matched)
        (while (and (not matched) (search-forward text nil t))
          (let* ((end (point))
                 (start (- end (length text)))
                 (position start)
                 (matches t))
            (while (and matches (< position end))
              (unless (eq (get-text-property position 'face) face)
                (setq matches nil))
              (setq position (1+ position)))
            (when (and matches
                       (or (null face)
                           (and (not (eq (get-text-property (1- start) 'face)
                                         face))
                                (not (eq (get-text-property end 'face) face)))))
              (setq matched t))))
        matched))))

(defun ende-test--dashboard-assert-chat-details
    (dashboard job title timestamp status)
  "Assert expanded chat detail faces, indentation, and preview in DASHBOARD."
  (with-current-buffer dashboard
    (let* ((contents (buffer-string))
           (lines (split-string contents "\n")))
      (should truncate-lines)
      (should (member "    Model: gpt-6.1-sol max" lines))
      (should
       (member (format "    Activity: %s (updated %s)" title timestamp)
               lines))
      (should (member (concat "        " status) lines))
      (should (member "    Execution: Running" lines))
      (should (member "    Tool calls: 2" lines))
      (should (member "    Latest user message: * How LLMs work" lines))
      (should (member "        Can you tell me how LLMs work?" lines))
      (should (member "        Explain the basic idea." lines))
      (should (member "        Keep it concise." lines))
      (should (member "        Use two sentences." lines))
      (should-not
       (member "        This sixth line should stay hidden." lines))
      (should-not (string-match-p "Source metadata:" contents))
      (should-not (string-match-p "Name: Roadmap.org" contents))
      (should-not (string-match-p "Type: file" contents))
      (should
       (ende-test--dashboard-face-span-p
        dashboard "Model:" 'font-lock-type-face))
      (should (ende-test--dashboard-face-span-p
               dashboard "gpt-6.1-sol max" nil))
      (should
       (ende-test--dashboard-face-span-p
        dashboard "Activity:" 'font-lock-type-face))
      (should
       (ende-test--dashboard-face-span-p
        dashboard title 'font-lock-function-name-face))
      (should
       (ende-test--dashboard-face-span-p
        dashboard (format "(updated %s)" timestamp)
        'ende-queue-ui-heading-face))
      (should
       (ende-test--dashboard-face-span-p
        dashboard status 'font-lock-doc-face))
      (should
       (ende-test--dashboard-face-span-p
        dashboard "Latest user message:" 'font-lock-type-face))
      (should
       (ende-test--dashboard-face-span-p dashboard "* How LLMs work" nil))
      (should
       (ende-test--dashboard-face-span-p
        dashboard "Can you tell me how LLMs work?" nil))
      (goto-char (point-min))
      (let ((activity-position
             (and (search-forward "Activity:" nil t)
                  (- (point) (length "Activity:")))))
        (should activity-position)
        (should (eq (get-text-property activity-position
                                       'ende-queue-ui-kind)
                    'detail))
        (should (eq (get-text-property activity-position
                                       'ende-queue-ui-owner)
                    job))))))

(defun ende-test--dashboard-flush-refresh (dashboard)
  "Run DASHBOARD's pending refresh now, keeping tests independent of timers."
  (with-current-buffer dashboard
    (when (timerp ende-queue-ui--refresh-timer)
      (cancel-timer ende-queue-ui--refresh-timer)
      (setq ende-queue-ui--refresh-timer nil)))
  (ende-queue-ui--refresh-callback dashboard))

(ert-deftest ende-test/dashboard-summary-rows-render-job-fields-in-both-views ()
  (ende-test--dashboard-reset)
  (let ((ende-llm-function (ende-test--fake-llm nil t)))
    (ende-test--with-buffer "base"
			    (ende-queue-pause)
			    (let* ((first (ende-enqueue
					   (ende-test--dashboard-request
					    ende-test--dashboard-profile)))
				   (second (ende-enqueue
					    (ende-test--dashboard-request
					     ende-test--dashboard-profile)))
				   (dashboard (ende-queue-open))
				   (first-id (ende-job-id first))
				   (second-id (ende-job-id second))
				   (jobs-first-row (ende-test--dashboard-job-row dashboard first-id))
				   (jobs-second-row (ende-test--dashboard-job-row dashboard second-id)))
			      (should (string-match-p (format "\\b%d\\b" first-id) jobs-first-row))
			      (should (string-match-p "insert" jobs-first-row))
			      (should (string-match-p "Ready" jobs-first-row))
			      (should (string-match-p "wait" jobs-first-row))
			      (should (string-match-p (format "\\b%d\\b" second-id) jobs-second-row))
			      (should (string-match-p "Behind #" jobs-second-row))
			      (with-current-buffer dashboard
				(ende-queue-ui-switch-view))
			      (let ((buffers-first-row
				     (ende-test--dashboard-job-row dashboard first-id))
				    (buffers-second-row
				     (ende-test--dashboard-job-row dashboard second-id)))
				(should (string-match-p (format "\\b%d\\b" first-id)
							buffers-first-row))
				(should (string-match-p "Queued" buffers-first-row))
				(should (string-match-p "insert" buffers-first-row))
				(should (string-match-p (format "\\b%d\\b" second-id)
							buffers-second-row))
				(should (string-match-p "Behind #" buffers-second-row)))))))

(ert-deftest ende-test/dashboard-tokens-follow-latest-provider-usage ()
  (save-window-excursion
    (delete-other-windows)
    (ende-test--dashboard-reset)
    (let ((ende-llm-function (ende-test--fake-llm '(:stall) t)))
      (ende-test--with-buffer "base"
        (let* ((job (ende-enqueue
                     (ende-test--dashboard-request
                      ende-test--dashboard-profile)))
               (dashboard (ende-queue-open))
               (id (ende-job-id job)))
          (set-window-buffer (selected-window) dashboard)
          (dolist (view '(jobs buffers))
            (with-current-buffer dashboard
              (unless (eq ende-queue-ui-view view)
                (ende-queue-ui-switch-view)))
            (should (equal (ende-test--dashboard-token-cell dashboard id) "-")))
          (ende--handle-llm-event
           job '(:type :usage :usage (:input_tokens 18 :output_tokens 7)))
          (should (timerp (buffer-local-value
                           'ende-queue-ui--refresh-timer dashboard)))
          (ende-test--dashboard-flush-refresh dashboard)
          (should (equal (ende-test--dashboard-token-cell dashboard id) "25"))
          (ende--handle-llm-event
           job '(:type :usage :usage (:total_tokens 52)))
          (ende-test--dashboard-flush-refresh dashboard)
          (should (equal (ende-test--dashboard-token-cell dashboard id) "52"))
          (ende--handle-llm-event
           job '(:type :usage :tokens 99 :estimated t))
          (should-not (buffer-local-value
                       'ende-queue-ui--refresh-timer dashboard))
          (should (equal (ende-job-last-usage job) '(:total_tokens 52)))
          (dolist (view '(jobs buffers))
            (with-current-buffer dashboard
              (unless (eq ende-queue-ui-view view)
                (ende-queue-ui-switch-view)))
            (should (string-match-p "Tokens"
                                    (with-current-buffer dashboard
                                      (buffer-string))))
            (should (equal (ende-test--dashboard-token-cell dashboard id) "52")))
          (when (buffer-live-p dashboard)
            (kill-buffer dashboard)))))))

(ert-deftest ende-test/dashboard-chat-job-without-tools-shows-empty-tool-details ()
  (ende-test--dashboard-reset)
  (with-temp-buffer
    (org-mode)
    (let ((job (ende--make-job
                :id 41 :type 'chat :state 'running
                :target (ende-target-create
                         :buffer (current-buffer) :start (point-min)
                         :end (point-max) :cursor (point-min)
                         :review-policy 'ignore)
                :instruction '(:request-id "request-without-tools")
                :profile '(:name remote :model "fixture"))))
      (ende-queue-ui--job-details job 'running)
      (should (string-match-p "Last tool call: ---" (buffer-string)))
      (should (string-match-p "Status: ---" (buffer-string))))))

(ert-deftest ende-test/dashboard-unfolded-job-shows-latest-matched-tool-result ()
  (save-window-excursion
    (delete-other-windows)
    (ende-test--dashboard-reset)
    (let* ((ende-tools--registry (copy-sequence ende-tools--registry))
           (pending nil)
           (ende-llm-function
            (ende-test--fake-llm
             (list
              (list :tool-calls
                    (list '(:id "first" :name "ende_test_dashboard_tool"
                            :arguments "{\"value\":\"first\"}")))
              (list :tool-calls
                    (list '(:id "progress-one" :name "about_thinking"
                            :arguments "{\"title\":\"Inspecting\",\"status\":\"Still working.\"}")))
              (list :tool-calls
                    (list '(:id "second" :name "ende_test_dashboard_tool"
                            :arguments "{\"value\":\"second\"}")))
              (list :tool-calls
                    (list '(:id "progress-two" :name "about_thinking"
                            :arguments "{\"title\":\"Checking\",\"status\":\"Still working.\"}")))
              "Finished")
             t))
           (dashboard nil))
      (unwind-protect
          (progn
            (ende-tool-register
             'ende_test_dashboard_tool "Dashboard fixture"
             ende-tools--empty-object-schema
             (lambda (arguments _root callback)
               (push (cons (gethash "value" arguments) callback) pending))
             nil :no-resource '(:requires-root nil :async t))
            (ende-test--with-buffer
             (concat (ende-test--chat-config-block
                      '((group dashboard ende_test_dashboard_tool)
                        (tools dashboard)))
                     "* Conversation\n** Question\nInspect.\n")
             (org-mode)
             (ende-test--chat-goto "** Question")
             (let* ((job (ende-ask '(:name remote :model "fixture")))
                    (id (ende-job-id job)))
               (setq dashboard (ende-queue-open))
               (set-window-buffer (selected-window) dashboard)
               (with-current-buffer dashboard
                 (goto-char (ende-queue-ui--position-for-key (cons 'job id)))
                 (ende-queue-ui-toggle-entry))
               (ende-test--complete-call (car ende-test--calls))
               (ende-test--dashboard-flush-refresh dashboard)
               (with-current-buffer dashboard
                 (should (string-match-p "Last tool call: ---" (buffer-string)))
                 (should (string-match-p "Status: ---" (buffer-string))))
               (funcall (cdar pending) "first completed" nil)
               (setq pending (cdr pending))
               (ende-test--dashboard-flush-refresh dashboard)
               (with-current-buffer dashboard
                 (should (string-match-p "Last tool call: ---" (buffer-string))))
               (ende-test--complete-call (nth 1 ende-test--calls))
               (ende-test--dashboard-flush-refresh dashboard)
               (with-current-buffer dashboard
                 (should (string-match-p "Last tool call: ---" (buffer-string))))
               (ende-test--complete-call (nth 2 ende-test--calls))
               (ende-test--dashboard-flush-refresh dashboard)
               (with-current-buffer dashboard
                 (should (string-match-p "Last tool call: ---" (buffer-string)))
                 (should (string-match-p "Status: ---" (buffer-string))))
               (funcall (cdar pending) nil "fixture failure")
               (setq pending (cdr pending))
               (ende-test--dashboard-flush-refresh dashboard)
               (with-current-buffer dashboard
                 (should (string-match-p "Last tool call: ---" (buffer-string))))
               (ende-test--complete-call (nth 3 ende-test--calls))
               (ende-test--dashboard-flush-refresh dashboard)
               (with-current-buffer dashboard
                 (should (string-match-p "Last tool call: ---" (buffer-string)))
                 (should (string-match-p "Status: ---" (buffer-string)))
                 (should-not (string-match-p "about_thinking" (buffer-string)))))))
        (when (buffer-live-p dashboard)
          (kill-buffer dashboard))
        (ende-test--reset)))))

(ert-deftest ende-test/dashboard-expanded-chat-details-style-and-refresh-in-both-views ()
  (save-window-excursion
    (delete-other-windows)
    (ende-test--dashboard-reset)
    (let* ((ende--job-handlers (copy-alist ende--job-handlers))
           (ende-llm-function (ende-test--fake-llm nil t))
           (profile '(:name remote :model "gpt-6.1-sol"
                            :reasoning-effort "max" :max-concurrency 2
                            :parallel nil :tool-definitions (tool)))
           (activity-title "Inspecting sources")
           (updated (format-time-string "%H:%M:%S" 0))
           (activity-message
            (concat "Reviewing the complete activity message for horizontal "
                    "scrolling " (make-string 96 ?x)))
           (user-message
            (concat "* How LLMs work\n"
                    "Can you tell me how LLMs work?\n"
                    "Explain the basic idea.\n"
                    "Keep it concise.\n"
                    "Use two sentences.\n"
                    "This sixth line should stay hidden."))
           job dashboard)
      (ende-register-job-handler 'chat (lambda (_job) nil))
      (unwind-protect
          (ende-test--with-buffer "base"
				  (let* ((request (ende-test--dashboard-request profile))
					 (messages (list (cons "user" user-message)))
					 prepared)
				    (setcar request 'chat)
				    (setq job (ende-enqueue request))
				    (should (eq (ende-job-state job) 'running))
				    (ende-job-emit-event
				     job (list :type :agent-activity :title activity-title
					       :status activity-message :updated-at 0))
				    (ende-job-emit-event job '(:type :tool-call-count :count 2))
				    (setq prepared
					  (cl-letf (((symbol-function
						      'ende-operations--source-metadata-block)
						     (lambda (_job)
						       (concat "Source metadata:\n"
							       "Name: Roadmap.org\n"
							       "Type: file\n"
							       "Major mode: org-mode\n\n"))))
					    (ende-operations--add-source-metadata job messages)))
				    (should (string-match-p "Source metadata:" (cdar prepared)))
				    ;; The provider-facing event must retain the original user text.
				    (ende-job-emit-event
				     job (list :type :begin :messages prepared))
				    (setq dashboard (ende-queue-open))
				    (set-window-buffer (selected-window) dashboard)
				    (with-current-buffer dashboard
				      (goto-char (ende-queue-ui--position-for-key
						  (cons 'job (ende-job-id job))))
				      (ende-queue-ui-toggle-entry))
				    (dolist (view '(jobs buffers))
				      (with-current-buffer dashboard
					(unless (eq ende-queue-ui-view view)
					  (ende-queue-ui-switch-view)))
				      (ende-test--dashboard-assert-chat-details
				       dashboard job activity-title updated activity-message))
				    (let ((refreshed-title "Reviewing changed content")
					  (refreshed-time (format-time-string "%H:%M:%S" 3600))
					  (refreshed-message "Rechecking the updated activity line"))
				      (ende-job-emit-event
				       job (list :type :agent-activity :title refreshed-title
						 :status refreshed-message :updated-at 3600))
				      (ende-test--dashboard-flush-refresh dashboard)
				      (ende-test--dashboard-assert-chat-details
				       dashboard job refreshed-title refreshed-time
				       refreshed-message))))
        (when (buffer-live-p dashboard)
          (kill-buffer dashboard))))))

(ert-deftest ende-test/dashboard-chat-details-show-effort-and-tool-call-state ()
  (ende-test--dashboard-reset)
  (let ((ende-queue-ui-view 'jobs))
    (with-temp-buffer
      (let ((enabled
             (ende--make-job
              :id 41 :type 'chat :state 'running
              :profile '(:name remote :model "gpt-6.1-sol"
                               :reasoning-effort "max" :parallel t
                               :max-concurrency 2 :tool-definitions (tool)))))
        (ende-job-emit-event enabled '(:type :tool-call-count :count 0))
        (ende-job-emit-event enabled '(:type :tool-call-count :count 2))
        (ende-job-emit-event enabled '(:type :tool-call-count :count 5))
        (should (equal (ende-queue-ui--operation enabled) "chat"))
        (ende-queue-ui--job-details enabled 'running)
        (let ((details (buffer-string)))
          (should (string-match-p "Model: gpt-6.1-sol max" details))
          (should (string-match-p "Tool calls: 5" details)))
        (erase-buffer)
        (setf (ende-job-profile enabled)
              '(:name remote :model "gpt-6.1-sol" :parallel t
                      :max-concurrency 2 :tool-definitions nil))
        (ende-queue-ui--job-details enabled 'running)
        (should (string-match-p "Tool calls: <disabled>" (buffer-string)))
        (erase-buffer)
        (setf (ende-job-profile enabled)
              (plist-put (ende-job-profile enabled) :tool-definitions '(tool)))
        (ende-queue-ui--job-details enabled 'queued)
        (should-not (string-match-p "Tool calls:" (buffer-string)))))))

(ert-deftest ende-test/dashboard-refreshes-job-lifecycle-across-display-changes ()
  (save-window-excursion
    (delete-other-windows)
    (ende-test--dashboard-reset)
    (let* ((ende--job-handlers (copy-alist ende--job-handlers))
           (profile (plist-put (copy-tree ende-test--dashboard-profile)
                               :tool-definitions '(tool)))
           job dashboard)
      (ende-register-job-handler 'chat (lambda (_job) nil))
      (ende-test--with-buffer "source"
			      (let* ((source (current-buffer))
				     (request (ende-test--dashboard-request profile)))
				(setcar request 'chat)
				(setq job (ende-enqueue request)
				      dashboard (ende-queue-open))
				(ende-job-emit-event job '(:type :tool-call-count :count 0))
				(with-current-buffer dashboard
				  (goto-char (ende-queue-ui--position-for-key
					      (cons 'job (ende-job-id job))))
				  (ende-queue-ui-toggle-entry))
				(let* ((dashboard-window (get-buffer-window dashboard t))
				       (source-window (split-window-right nil dashboard-window)))
				  (set-window-buffer dashboard-window dashboard)
				  (set-window-buffer source-window source)
				  (select-window source-window)

				  ;; A displayed but unselected dashboard still follows job events.
				  (ende-job-emit-event
				   job '(:type :tool-call-count :count 2))
				  (ende-test--dashboard-flush-refresh dashboard)
				  (with-current-buffer dashboard
				    (should (string-match-p "Tool calls: 2" (buffer-string))))

				  ;; Quit cancels display timers, but leaves event observers attached.
				  (select-window dashboard-window)
				  (ende-queue-ui-quit)
				  (unless (window-live-p dashboard-window)
				    (setq dashboard-window (selected-window)))
				  (should-not (get-buffer-window dashboard t))
				  (should (memq #'ende-queue-ui--job-event
						ende-job-event-functions))
				  (should (memq #'ende-queue-ui--queue-event
						ende-queue-event-functions))
				  (ende-job-emit-event
				   job '(:type :tool-call-count :count 3))
				  (ende-test--dashboard-flush-refresh dashboard)

				  ;; Showing it again schedules a refresh without requiring `g'.
				  (set-window-buffer dashboard-window dashboard)
				  (run-hook-with-args 'window-buffer-change-functions
						      (window-frame dashboard-window))
				  (ende-test--dashboard-flush-refresh dashboard)
				  (with-current-buffer dashboard
				    (should (string-match-p "Tool calls: 3" (buffer-string))))

				  ;; Completion while hidden is reflected when the view returns.
				  (set-window-buffer dashboard-window source)
				  (ende-job-finish job "done")
				  (ende-test--dashboard-flush-refresh dashboard)
				  (set-window-buffer dashboard-window dashboard)
				  (run-hook-with-args 'window-buffer-change-functions
						      (window-frame dashboard-window))
				  (ende-test--dashboard-flush-refresh dashboard)
				  (with-current-buffer dashboard
				    (should-not (ende-queue-ui--position-for-key
						 (cons 'job (ende-job-id job))))))
				(when (buffer-live-p dashboard)
				  (kill-buffer dashboard))
				(should-not (memq #'ende-queue-ui--job-event
						  ende-job-event-functions))
				(should-not (memq #'ende-queue-ui--queue-event
						  ende-queue-event-functions))
				(should-not (memq #'ende-queue-ui--window-buffer-changed
						  window-buffer-change-functions)))))))

(ert-deftest ende-test/dashboard-header-shows-state-appropriate-action ()
  (ende-test--dashboard-reset)
  (let ((ende-llm-function (ende-test--fake-llm '(:stall) t)))
    (ende-test--with-buffer "base"
			    (let* ((job (ende-enqueue
					 (ende-test--dashboard-request
					  ende-test--dashboard-profile)))
				   (dashboard (ende-queue-open)))
			      (with-current-buffer dashboard
				(should (string-match-p "ACTIVE  P pause" (buffer-string)))
				(ende-queue-pause)
				(ende-queue-ui--render)
				(should (string-match-p "PAUSED (no new starts)  R resume"
							(buffer-string)))
				(ende-queue-resume)
				(ende-queue-resume)
				(ende-queue-ui--render)
				(should (string-match-p "ACTIVE  P pause" (buffer-string))))
			      (should (eq (ende-job-state job) 'running))
			      (should (= (length ende-test--calls) 1))))))

(ert-deftest ende-test/queue-pause-is-independent-of-dashboard-lifecycle ()
  (ende-test--dashboard-reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" A" " B" " C") t)))
    (ende-test--with-buffer "base"
			    (let* ((first (ende-enqueue
					   (ende-test--dashboard-request
					    ende-test--dashboard-profile)))
				   (second nil)
				   (third nil))
			      (ende-queue-pause)
			      (setq second
				    (ende-enqueue (ende-test--dashboard-request
						   ende-test--dashboard-profile)))
			      (ende-test--complete-call (car ende-test--calls))
			      (setq third
				    (ende-enqueue (ende-test--dashboard-request
						   ende-test--dashboard-profile)))
			      (should (eq (ende-job-state first) 'completed))
			      (should (eq (ende-job-state second) 'queued))
			      (should (eq (ende-job-state third) 'queued))
			      (should (= (length ende-test--calls) 1))
			      (let ((dashboard (ende-queue-open)))
				(with-current-buffer dashboard
				  (ende-queue-ui-quit))
				(should ende-queue-paused))
			      (ende-queue-resume)
			      (should-not ende-queue-paused)
			      (should (eq (ende-job-state second) 'running))
			      (should (= (length ende-test--calls) 2))
			      (ende-test--complete-call (nth 1 ende-test--calls))
			      (should (eq (ende-job-state third) 'running))
			      (ende-test--complete-call (nth 2 ende-test--calls))
			      (should (null (ende-pending-jobs)))))))

(ert-deftest ende-test/dashboard-views-preserve-job-identity-and-ui-state ()
  (ende-test--dashboard-reset)
  (let ((ende-llm-function (ende-test--fake-llm nil t)))
    (ende-test--with-buffer "base"
			    (ende-queue-pause)
			    (let* ((first (ende-enqueue
					   (ende-test--dashboard-request
					    ende-test--dashboard-profile)))
				   (second (ende-enqueue
					    (ende-test--dashboard-request
					     ende-test--dashboard-profile)))
				   (dashboard (ende-queue-open))
				   (id (ende-job-id first)))
			      (with-current-buffer dashboard
				(goto-char (ende-queue-ui--position-for-key (cons 'job id)))
				(ende-queue-ui-mark)
				(ende-queue-ui-toggle-entry)
				(should (gethash id ende-queue-ui--expanded-jobs))
				(forward-line 1)
				(should (eq (ende-job-id (ende-queue-ui--current-job)) id))
				(ende-queue-ui--render)
				(should (eq (ende-job-id (ende-queue-ui--current-job)) id))
				(ende-queue-ui-switch-view)
				(should (equal (get-text-property
						(ende-queue-ui--position-for-key (cons 'job id))
						'ende-queue-ui-job-id)
					       id))
				(should (= (ende-queue-ui--marked-count) 1))
				(should (gethash id ende-queue-ui--expanded-jobs))
				(should (string-match-p "State" (buffer-string)))
				(goto-char (point-min))
				(search-forward "Buffer " nil t)
				(beginning-of-line)
				(let ((source (ende-target-buffer (ende-job-target first))))
				  (ende-queue-ui-toggle-entry)
				  (should (gethash source ende-queue-ui--folded-buffers))
				  (should-not (ende-queue-ui--position-for-key (cons 'job id)))
				  (ende-queue-ui-toggle-entry)
				  (should-not (gethash source ende-queue-ui--folded-buffers))
				  (should (ende-queue-ui--position-for-key (cons 'job id))))
				(ende-queue-ui-switch-view)
				(goto-char (ende-queue-ui--position-for-key (cons 'job id)))
				(ende-queue-ui-cancel))
			      (should (eq (ende-job-state first) 'cancelled))
			      (should (eq (ende-job-state second) 'queued))
			      (should (= (length ende-test--calls) 0))))))

(ert-deftest ende-test/dashboard-inspection-is-read-only-for-scheduler-state ()
  (ende-test--dashboard-reset)
  (let ((ende-llm-function (ende-test--fake-llm nil t)))
    (ende-test--with-buffer "base"
			    (ende-queue-pause)
			    (let* ((first (ende-enqueue
					   (ende-test--dashboard-request
					    ende-test--dashboard-profile)))
				   (second (ende-enqueue
					    (ende-test--dashboard-request
					     ende-test--dashboard-profile)))
				   (fence (ende-fence-all))
				   (before (ende-queue-entries))
				   (membership (copy-sequence (ende-queue-fence-jobs fence)))
				   (dashboard (ende-queue-open)))
			      (ende-job-explanation second)
			      (with-current-buffer dashboard
				(goto-char (ende-queue-ui--position-for-key
					    (cons 'job (ende-job-id second))))
				(ende-queue-ui-toggle-entry)
				(ende-queue-ui--render))
			      (should (equal before (ende-queue-entries)))
			      (should (equal membership (ende-queue-fence-jobs fence)))
			      (should (eq (ende-job-state first) 'queued))
			      (should-not ende--active-jobs)
			      (should (= (length ende-test--calls) 0))))))

(ert-deftest ende-test/dashboard-batch-cancel-keeps-captured-job-ids ()
  (ende-test--dashboard-reset)
  (let ((ende-llm-function (ende-test--fake-llm nil t)))
    (ende-test--with-buffer "base"
			    (ende-queue-pause)
			    (let* ((first (ende-enqueue
					   (ende-test--dashboard-request
					    ende-test--dashboard-profile)))
				   (second (ende-enqueue
					    (ende-test--dashboard-request
					     ende-test--dashboard-profile)))
				   (third (ende-enqueue
					   (ende-test--dashboard-request
					    ende-test--dashboard-profile)))
				   (dashboard (ende-queue-open)))
			      (with-current-buffer dashboard
				(goto-char (ende-queue-ui--position-for-key
					    (cons 'job (ende-job-id first))))
				(ende-queue-ui-mark)
				(goto-char (ende-queue-ui--position-for-key
					    (cons 'job (ende-job-id second))))
				(ende-queue-ui-mark)
				(cl-letf (((symbol-function 'yes-or-no-p)
					   (lambda (prompt)
					     (should (string-match-p "0 running and 2 queued" prompt))
					     (ende-queue-move (ende-job-id third) -1)
					     t)))
				  (ende-queue-ui-cancel)))
			      (should (eq (ende-job-state first) 'cancelled))
			      (should (eq (ende-job-state second) 'cancelled))
			      (should (eq (ende-job-state third) 'queued))
			      (should (equal (ende-pending-jobs) (list third)))))))

(ert-deftest ende-test/dashboard-cancel-all-preserves-broad-abort-hooks ()
  (ende-test--dashboard-reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" A" " B") t))
        (abort-hook-ran nil)
        prompt)
    (let ((ende-queue-abort-functions
           (cons (lambda () (setq abort-hook-ran t))
                 ende-queue-abort-functions)))
      (ende-test--with-buffer "base"
			      (let* ((running (ende-enqueue
					       (ende-test--dashboard-request
						ende-test--dashboard-profile)))
				     (queued (ende-enqueue
					      (ende-test--dashboard-request
					       ende-test--dashboard-profile)))
				     (dashboard (ende-queue-open)))
				(with-current-buffer dashboard
				  (cl-letf (((symbol-function 'yes-or-no-p)
					     (lambda (text) (setq prompt text) t)))
				    (ende-queue-ui-cancel-all)))
				(should (string-match-p "1 running and 1 queued" prompt))
				(should abort-hook-ran)
				(should (eq (ende-job-state running) 'cancelled))
				(should (eq (ende-job-state queued) 'cancelled))
				(should-not ende--active-jobs)
				(should-not (ende-pending-jobs)))))))

(ert-deftest ende-test/dashboard-status-follows-org-result-reference ()
  (ende-test--dashboard-reset)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (let* ((question-id "question-dashboard-history")
               (request-id "request-dashboard-history")
               (call
                (ende-chat--prepare-tool-call
                 '(:id "provider-dashboard-history" :name "read_file"
                   :arguments "{\"path\":\"fixture.el\"}"))))
          (insert "* Conversation\n** Question\nInspect.\n"
                  (ende-chat--work-text 2 question-id request-id)
                  (ende-chat--assistant-protocol-text
                   3 question-id request-id "" '(:content "") (list call))
                  (ende-chat--tool-result-text
                   3 request-id call "{\"ok\":true}"))
          (save-excursion
            (goto-char (point-min))
            (re-search-forward "^\\*\\* Agent: Work")
            (org-back-to-heading t)
            (org-entry-delete nil ende-chat-state-property))
          (let* ((document (ende-chat--build-document))
                 (result
                  (car (cl-remove-if-not
                        (lambda (headline)
                          (ende-chat--tool-result-protocol-p document headline))
                        (ende-chat--document-headlines document))))
                 (job
                  (ende--make-job
                   :id 42 :type 'chat :state 'running
                   :target (ende-target-create
                            :buffer (current-buffer) :start (point-min)
                            :end (point-max) :cursor (point-min)
                            :review-policy 'ignore)
                   :instruction (list :request-id request-id))))
            (delete-region (org-element-property :begin result)
                           (org-element-property :end result))
            (should (equal (ende-queue-ui--last-tool-call-from-history job)
                           '("read_file: path=\"fixture.el\"" "---"))))))
    (ende-test--dashboard-reset)))

(provide 'ende-queue-dashboard-test)

;;; ende-queue-dashboard-test.el ends here
