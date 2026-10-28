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

(ert-deftest ende-test/dashboard-injects-guidance-only-for-running-chat-at-point ()
  (ende-test--dashboard-reset)
  (let* ((target (ende-target-create
                  :buffer (current-buffer) :start (point-min) :end (point-max)
                  :cursor (point-min) :review-policy 'ignore))
         (chat (ende--make-job :id 101 :type 'chat :state 'running
                               :target target))
         (other (ende--make-job :id 102 :type 'insert :state 'running
                                :target target))
         (queued (ende--make-job :id 103 :type 'chat :state 'queued
                                 :target target))
         (stale (ende--make-job :id 104 :type 'chat :state 'running
                                :target target))
         (dashboard (generate-new-buffer "*ende-dashboard-guidance-test*"))
         (positions nil)
         (prompts nil))
    (unwind-protect
        (progn
          (setq ende--active-jobs (list chat other))
          (puthash (ende-job-id other) t ende-queue-ui--marked-ids)
          (with-current-buffer dashboard
            (ende-queue-ui-mode)
            (let ((inhibit-read-only t))
              (dolist (job (list chat other queued stale))
                (let ((start (point)))
                  (insert (format "Job %d\n" (ende-job-id job)))
                  (add-text-properties
                   start (point)
                   (list 'ende-queue-ui-kind 'job
                         'ende-queue-ui-owner job))
                  (push (cons job start) positions)))))
          (cl-letf (((symbol-function #'read-string)
                     (lambda (prompt) (push prompt prompts) "  keep this text  ")))
            (with-current-buffer dashboard
              (goto-char (cdr (assq chat positions)))
              (ende-queue-ui-inject-guidance))
            (should (equal (ende-job-pending-guidance chat)
                           '("  keep this text  ")))
            (should-not (ende-job-pending-guidance other))
            (should (equal prompts '("Inject prompt: ")))
            (cl-letf (((symbol-function #'read-string)
                       (lambda (_prompt) " \t\n")))
              (with-current-buffer dashboard
                (goto-char (cdr (assq chat positions)))
                (should-error (ende-queue-ui-inject-guidance)
                              :type 'user-error)))
            (should (equal (ende-job-pending-guidance chat)
                           '("  keep this text  ")))
            (setq prompts nil)
            (dolist (job (list other queued stale))
              (with-current-buffer dashboard
                (goto-char (cdr (assq job positions)))
                (should-error (ende-queue-ui-inject-guidance)
                              :type 'user-error)))
            (should-not prompts)
            (should (eq (lookup-key ende-queue-ui-mode-map (kbd "i"))
                        #'ende-queue-ui-inject-guidance)))
          (should (equal (ende-job-take-pending-guidance chat)
                         '("  keep this text  ")))
          (should-not (ende-job-pending-guidance chat)))
      (when (buffer-live-p dashboard)
        (kill-buffer dashboard))
      (ende-test--dashboard-reset))))

(ert-deftest ende-test/dashboard-guidance-key-survives-reload ()
  (save-window-excursion
    (ende-test--dashboard-reset)
    (let* ((original-i (lookup-key ende-queue-ui-mode-map (kbd "i")))
           (target (ende-target-create
                    :buffer (current-buffer) :start (point-min) :end (point-max)
                    :cursor (point-min) :review-policy 'ignore))
           (job (ende--make-job :id 105 :type 'chat :state 'running
                                :target target
                                :profile ende-test--dashboard-profile))
           (dashboard (generate-new-buffer "*ende-dashboard-guidance-key-test*"))
           prompts)
      (unwind-protect
          (progn
            (setq ende--active-jobs (list job))
            (set-window-buffer (selected-window) dashboard)
            (with-current-buffer dashboard
              (ende-queue-ui-mode)
              (ende-queue-ui--render)
              (goto-char (ende-queue-ui--position-for-key (cons 'job 105)))
              (should (eq (ende-queue-ui--current-job) job)))
            ;; A pre-existing dashboard map is retained by `defvar' on reload.
            (define-key ende-queue-ui-mode-map (kbd "i") nil)
            (should-not (eq (with-current-buffer dashboard
                              (key-binding (kbd "i")))
                            #'ende-queue-ui-inject-guidance))
            (load-file (expand-file-name
                        "ende-queue-ui.el"
                        (file-name-directory (locate-library "ende-queue-ui"))))
            (cl-letf (((symbol-function #'read-string)
                       (lambda (prompt)
                         (push prompt prompts)
                         "Follow this instruction")))
              (with-current-buffer dashboard
                (should (eq (key-binding (kbd "i"))
                            #'ende-queue-ui-inject-guidance))
                (execute-kbd-macro (kbd "i"))))
            (should (equal prompts '("Inject prompt: ")))
            (should (equal (ende-job-pending-guidance job)
                           '("Follow this instruction"))))
        (define-key ende-queue-ui-mode-map (kbd "i") original-i)
        (when (buffer-live-p dashboard)
          (kill-buffer dashboard))
        (ende-test--dashboard-reset)))))

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

(ert-deftest ende-test/dashboard-inspects-agent-and-chat-work-without-editing ()
  (save-window-excursion
    (delete-other-windows)
    (ende-test--dashboard-reset)
    (let* ((directory (make-temp-file "ende-work-inspection-" t))
           (user-emacs-directory directory)
           (task "Inspect the private task.")
           (assistant "Inspecting the tool round.\n#+end_example\n* Supplied heading")
           (status "Checking the output.\n#+end_example\n* Supplied heading")
           (reasoning "Private reasoning must not appear in the log.")
           (call (list :id "progress" :name "about_thinking"
                       :arguments (json-encode (list :title "Checking work" :status status))))
           (ende-llm-function
            (ende-test--fake-llm
             (list (list :tool-calls
                         (list (list :id "spawn" :name "spawn_agent"
                                     :arguments (json-encode (list :task task)))))
                   (list :content assistant :tool-calls (list call)
                         :reasoning-content reasoning)
                   "Private final reply." "Parent final reply.") t))
           root child staging owned dashboard source)
      (unwind-protect
          (ende-test--with-buffer
           (concat (ende-test--chat-config-block
                    '((tools spawn_agent)
                      (agent-config worker)
                      (spawn-agent-config worker)))
                   "* Question\nDelegate an inspection.\n")
           (org-mode)
           (ende-test--chat-goto "* Question")
           (setq source (current-buffer)
                 root (ende-ask))
           (ende-test--complete-call (car ende-test--calls))
           (setq child (car (ende-job-children root))
                 staging (ende-chat--staging-buffer root)
                 owned (ende-job-owned-buffer child)
                 dashboard (ende-queue-open))
           (should (eq (ende-target-buffer (ende-job-target child)) owned))
           (with-current-buffer dashboard
             (goto-char (ende-queue-ui--position-for-key (cons 'job (ende-job-id root))))
             (ende-queue-ui-visit-agent-work))
           (should (eq (window-buffer (selected-window)) staging))
           (with-current-buffer staging
             (should (= (point) (point-max)))
             (should-error (insert "user edit") :type 'buffer-read-only))
           (with-current-buffer dashboard
             (goto-char (ende-queue-ui--position-for-key (cons 'job (ende-job-id child))))
             (ende-queue-ui-visit-agent-work))
           (should (eq (window-buffer (selected-window)) owned))
           (with-current-buffer owned
             (should (derived-mode-p 'org-mode))
             (should font-lock-mode)
             (should (= (point) (point-max)))
             (should-error (insert "user edit") :type 'buffer-read-only)
             (should (string-match-p (regexp-quote task) (buffer-string))))
           (delete-other-windows)
           (let* ((reader (selected-window))
                  (follower (split-window-right)))
             (set-window-buffer follower owned)
             (set-window-point follower (with-current-buffer owned (point-max)))
             (with-current-buffer owned (goto-char (+ (point-min) 2)))
             (let ((position (window-point reader))
                   (start (window-start reader)))
               (ende-test--complete-call (nth 1 ende-test--calls))
               (should (eq (selected-window) reader))
               (should (= (window-point reader) position))
               (should (= (window-start reader) start))
               (should (= (window-point follower) (with-current-buffer owned (point-max)))))
             (with-current-buffer owned
               (let* ((document (org-element-parse-buffer))
                      (bodies (org-element-map document 'example-block
                                (lambda (block)
                                  (org-unescape-code-in-string
                                   (org-element-property :value block))))))
                 (dolist (text (list assistant status "about_thinking" "accepted"))
                   (should (cl-some (lambda (body) (string-match-p (regexp-quote text) body))
                                    bodies)))
                 (should-not (member "Supplied heading"
                                     (org-element-map document 'headline
                                       (lambda (node) (org-element-property :raw-value node)))))
                 (should-not (string-match-p (regexp-quote reasoning) (buffer-string)))))
             (let* ((messages (plist-get (nth 2 ende-test--calls) :messages))
                    (sent (nth 2 messages)))
               (should (equal (plist-get sent :content) assistant))
               (should (equal (plist-get sent :tool-calls) (list call)))
               (should (equal (plist-get sent :reasoning-content) reasoning))
               (should (equal (plist-get (nth 3 messages) :tool-call-id) "progress")))
             (ende-work-buffer-quit)
             (should (eq (window-buffer (selected-window)) dashboard))
             (should (eq (window-buffer follower) owned))
             (should (eq (ende-job-state child) 'running))
             (with-current-buffer dashboard
               (goto-char (ende-queue-ui--position-for-key (cons 'job (ende-job-id child))))
               (ende-queue-ui-visit-agent-work))
             (should (= (window-point (selected-window))
                        (with-current-buffer owned (point-max))))
             (ende-test--complete-call (nth 2 ende-test--calls))
             (should (eq (ende-job-state child) 'completed))
             (with-current-buffer owned
               (should buffer-read-only)
               (should (string-match-p "Private final reply" (buffer-string)))
               (should (string-match-p "completed" (buffer-string))))
             (let ((parent-messages (prin1-to-string (plist-get (nth 3 ende-test--calls) :messages))))
               (should-not (string-match-p (regexp-quote assistant) parent-messages))
               (should-not (string-match-p "Checking work" parent-messages)))
             (with-current-buffer source
               (should-not (string-match-p "Checking work" (buffer-string))))
             (ende-test--complete-call (nth 3 ende-test--calls)))
           (should (eq (lookup-key ende-queue-ui-mode-map (kbd "w"))
                       #'ende-queue-ui-visit-agent-work)))
        (ende-test--dashboard-reset)
        (dolist (buffer (list staging owned))
          (when (buffer-live-p buffer) (kill-buffer buffer)))
        (ende-work-buffer--collect)
        (delete-directory directory t)))))

(ert-deftest ende-test/work-buffers-outlive-jobs-only-while-displayed ()
  (save-window-excursion
    (delete-other-windows)
    (ende-test--dashboard-reset)
    (let* ((directory (make-temp-file "ende-work-lifecycle-" t))
           (user-emacs-directory directory)
           (ende-llm-function
            (ende-test--fake-llm
             '((:tool-calls ((:id "spawn" :name "spawn_agent"
                                 :arguments "{\"task\":\"Inspect privately\"}")))
               :error "Chat finished") t))
           root child staging owned)
      (unwind-protect
          (ende-test--with-buffer
           (concat (ende-test--chat-config-block
                    '((tools spawn_agent)
                      (agent-config worker)
                      (spawn-agent-config worker)))
                   "* Question\nDelegate an inspection.\n")
           (org-mode)
           (ende-test--chat-goto "* Question")
           (setq root (ende-ask)
                 ende-queue-paused t)
           (ende-test--complete-call (car ende-test--calls))
           (setq child (car (ende-job-children root))
                 staging (ende-chat--staging-buffer root)
                 owned (ende-job-owned-buffer child))
           (should (eq (ende-job-state root) 'suspended))
           (should (eq (ende-job-state child) 'queued))
           (run-hook-with-args 'window-buffer-change-functions (selected-frame))
           (ende-work-buffer--collect)
           (should (buffer-live-p staging))
           (should (buffer-live-p owned))
           (setq ende-queue-paused nil)
           (ende-process-next)
           (should (eq (ende-job-state child) 'running))
           (ende-work-buffer--collect)
           (should (buffer-live-p owned))
           (let ((first (selected-window))
                 (second (split-window-right))
                 (source (current-buffer))
                 (index 1))
             (dolist (entry (list (cons child owned) (cons root staging)))
               (let* ((job (car entry))
                      (buffer (cdr entry))
                      (hook (ende-job-owned-buffer-hook job)))
                 (set-window-buffer first buffer)
                 (set-window-buffer second buffer)
                 (ende-test--complete-call (nth index ende-test--calls))
                 (setq index (1+ index))
                 (should (eq (ende-job-state job) (if (eq job child) 'failed 'completed)))
                 (should (buffer-live-p buffer))
                 (when (eq job child)
                   (with-current-buffer buffer
                     (should (string-match-p "mock failure" (buffer-string)))))
                 (should-not (ende-job-cancel-function job))
                 (should-not (memq job (ende-all-live-jobs)))
                 (should-not (ende-job-owned-buffer job))
                 (should-not (ende-chat--staging-buffer job))
                 (when hook
                   (should-not (memq hook (buffer-local-value 'kill-buffer-hook buffer))))
                 (set-window-buffer first source)
                 (run-hook-with-args 'window-buffer-change-functions (selected-frame))
                 (ende-work-buffer--collect)
                 (should (buffer-live-p buffer))
                 (delete-window second)
                 (run-hook-with-args 'window-buffer-change-functions (selected-frame))
                 (ende-work-buffer--collect)
                 (should-not (buffer-live-p buffer))
                 (should (buffer-live-p source))
                 (should-not (buffer-local-value 'buffer-read-only source))
                 (setq second (split-window-right))))))
        (ende-test--dashboard-reset)
        (dolist (buffer (list staging owned))
          (when (buffer-live-p buffer) (kill-buffer buffer)))
        (ende-work-buffer--collect)
        (delete-directory directory t)))))

(ert-deftest ende-test/work-buffer-retirement-survives-repeated-reload ()
  (save-window-excursion
    (delete-other-windows)
    (ende-test--dashboard-reset)
    (let* ((directory (make-temp-file "ende-work-reload-" t))
           (user-emacs-directory directory)
           (ende-llm-function
            (ende-test--fake-llm
             '((:tool-calls ((:id "spawn" :name "spawn_agent"
                                 :arguments "{\"task\":\"Await cancellation\"}")))
               :stall) t))
           root child staging owned)
      (unwind-protect
          (ende-test--with-buffer
           (concat (ende-test--chat-config-block
                    '((tools spawn_agent)
                      (agent-config worker)
                      (spawn-agent-config worker)))
                   "* Question\nDelegate work before reloading.\n")
           (org-mode)
           (ende-test--chat-goto "* Question")
           (setq root (ende-ask))
           (ende-test--complete-call (car ende-test--calls))
           (setq child (car (ende-job-children root))
                 staging (ende-chat--staging-buffer root)
                 owned (ende-job-owned-buffer child))
           (let ((first (selected-window))
                 (second (split-window-right)))
             (set-window-buffer first owned)
             (set-window-buffer second staging)
             (dotimes (_ 2)
               (ende-reload)
               (should (buffer-live-p owned))
               (should (buffer-live-p staging))
               (run-hook-with-args 'window-buffer-change-functions (selected-frame)))
             (should (eq (ende-job-state child) 'cancelled))
             (should (eq (ende-job-state root) 'cancelled))
             (should-not (ende-job-owned-buffer child))
             (should-not (ende-chat--staging-buffer root))
             (should-not (ende-all-live-jobs))
             (with-current-buffer owned
               (should (string-match-p
                        (regexp-quote (ende-error-display-message (ende-job-error child)))
                        (buffer-string))))
             (dolist (observer '(ende-agent--work-event ende-chat--dispose-staging-buffer))
               (should (= (cl-count observer ende-job-event-functions) 1)))
             (dolist (hook '(window-buffer-change-functions delete-frame-functions))
               (should (= (cl-count #'ende-work-buffer--windows-changed
                                    (symbol-value hook))
                          1)))
             (should (= (cl-count #'ende-work-buffer--collect timer-list
                                 :key #'timer--function)
                        1))
             (dolist (buffer (list owned staging))
               (with-current-buffer buffer
                 (should (eq (key-binding (kbd "q")) #'ende-work-buffer-quit))))
             (select-window first)
             (ende-work-buffer-quit)
             (should (eq (window-buffer (selected-window)) ende-queue-ui--buffer))
             (set-window-buffer second ende-queue-ui--buffer)
             (run-hook-with-args 'delete-frame-functions (selected-frame))
             (ende-work-buffer--collect)
             (should-not (buffer-live-p owned))
             (should-not (buffer-live-p staging))
             (should-not (cl-find #'ende-work-buffer--collect timer-list
                                  :key #'timer--function))))
        (ende-test--dashboard-reset)
        (dolist (buffer (list owned staging))
          (when (buffer-live-p buffer) (kill-buffer buffer)))
        (ende-work-buffer--collect)
        (delete-directory directory t)))))

(ert-deftest ende-test/dashboard-agent-work-errors-without-creating-staging-buffer ()
  (ende-test--dashboard-reset)
  (with-temp-buffer
    (org-mode)
    (let* ((job
            (ende--make-job
             :id 92 :type 'chat :state 'running
             :target (ende-target-create
                      :buffer (current-buffer) :start (point-min)
                      :end (point-max) :cursor (point-min)
                      :review-policy 'ignore)
             :instruction '(:request-id "dashboard-no-work")))
           (dashboard (generate-new-buffer "*ende-dashboard-no-work-test*")))
      (unwind-protect
          (progn
            (with-current-buffer dashboard
              (ende-queue-ui-mode)
              (let ((inhibit-read-only t))
                (insert "Job entry\n"))
              (let ((inhibit-read-only t))
                (add-text-properties
                 (point-min) (point-max)
                 (list 'ende-queue-ui-kind 'job
                       'ende-queue-ui-owner job))))
            (dolist (type '(chat agent))
              (setf (ende-job-type job) type)
              (let ((dead (generate-new-buffer " *ende-dead-work-test*")))
                (kill-buffer dead)
                (dolist (buffer (list nil dead))
                  (setf (ende-job-owned-buffer job) buffer
                        (ende-job-instruction job)
                        (plist-put (ende-job-instruction job) :staging-buffer buffer))
                  (with-current-buffer dashboard
                    (goto-char (point-min))
                    (should-error (ende-queue-ui-visit-agent-work) :type 'user-error))
                  (should (eq (ende-job-owned-buffer job) buffer))
                  (should (eq (plist-get (ende-job-instruction job) :staging-buffer)
                              buffer))))))
        (when (buffer-live-p dashboard)
          (kill-buffer dashboard))))))

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

(ert-deftest ende-test/dashboard-shows-and-cancels-suspended-jobs ()
  (ende-test--dashboard-reset)
  (let ((ende-llm-function (ende-test--fake-llm nil t))
        (ende--queue-suspended t)
        (accepted-prompt nil)
        job dashboard)
    (let ((source (generate-new-buffer " *ende-suspended-dashboard-source*")))
      (unwind-protect
          (with-current-buffer source
            (insert "suspended dashboard")
            (let* ((family (ende--make-job-family :policy-frozen-p t))
                   (target (ende-target-create
                            :buffer (current-buffer)
                            :start (point-min) :end (point-min)
                            :cursor (point-min))))
              (setq job (ende--make-job
                         :id 918 :type 'chat :state 'suspended :family family
                         :schedule-path nil :target target
                         :profile ende-test--dashboard-profile
                         :started-at (float-time) :enqueued-at (float-time)
                         :status "Suspended"))
              (setf (ende-job-family-root family) job
                    (ende-job-resume-function job) (lambda () nil))
              (ende--queue-push family)
              (ende--queue-insert-family-member job)
              (setq dashboard (ende-queue-open))
              (should (eq (car (ende-queue-ui--live-jobs)) job))
              (should (string-match-p
                       "Suspended · 1"
                       (with-current-buffer dashboard (buffer-string))))
              (should (string-match-p
                       "Suspended"
                       (ende-test--dashboard-job-row
                        dashboard (ende-job-id job))))
              (with-current-buffer dashboard
                (goto-char (ende-queue-ui--position-for-key
                            (cons 'job (ende-job-id job))))
                (cl-letf (((symbol-function 'yes-or-no-p)
                           (lambda (prompt)
                             (setq accepted-prompt prompt)
                             t)))
                  (ende-queue-ui-cancel)))
              (should (string-match-p
                       "0 running, 0 queued, and 1 suspended"
                       accepted-prompt))
              (should (eq (ende-job-state job) 'cancelled))
              (should-not (ende-queue-family-markers))))
        (when (buffer-live-p source)
          (kill-buffer source))))))

(provide 'ende-queue-dashboard-test)

;;; ende-queue-dashboard-test.el ends here
