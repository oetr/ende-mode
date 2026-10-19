;;; ende-finished-test.el --- Finished-job navigation tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'ende)
(require 'ende-test-helpers)

(defconst ende-finished-test--profile
  '(:name remote :model "finished-test" :max-concurrency 1 :parallel nil))

(defun ende-finished-test--enqueue (buffer operation &optional instruction)
  "Enqueue OPERATION targeting BUFFER with optional INSTRUCTION."
  (with-current-buffer buffer
    (let ((position (point)))
      (ende-enqueue
       (list operation
             :target (ende-target-create
                      :buffer buffer :start position :end position
                      :cursor position :review-policy 'ignore)
             :profile ende-finished-test--profile
             :instruction instruction)))))

(ert-deftest ende-test/finished-jobs-track-terminal-snapshots-and-bound-retention ()
  (ende-test--reset)
  (let ((ende-finished-limit 2)
        (ende-finished-jobs nil)
        (ende-finished--live-jobs (make-hash-table :test #'eql))
        (source (generate-new-buffer " *ende-finished-lifecycle*"))
        active)
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert "* Source heading\nbody\n")
            (org-mode)
            (goto-char (point-max)))
          (ende-register-job-handler
           'finished-test (lambda (job) (setq active job)))
          (dolist (_ '(1 2 3))
            (let ((job (ende-finished-test--enqueue source 'finished-test)))
              (should (eq job active))
              (ende-job-finish job "done")))
          (should (= (length ende-finished-jobs) 2))
          (should (= (ende-finished-job-id (car ende-finished-jobs))
                     (ende-job-id active)))
          (should (cl-every (lambda (entry)
                             (and (ende-finished-job-p entry)
                                  (not (ende-job-p entry))))
                           ende-finished-jobs))
          (should (equal (ende-finished-job-title (car ende-finished-jobs))
                         "Source heading"))
          (should (integerp (ende-finished-job-line (car ende-finished-jobs))))
          (with-current-buffer source
            (goto-char (point-min)))
          (let ((cancelled (ende-finished-test--enqueue
                            source 'finished-test)))
            (let ((buffer-name (buffer-name source)))
              (kill-buffer source)
              (setq source nil)
              (let ((snapshot
                     (cl-find (ende-job-id cancelled) ende-finished-jobs
                              :key #'ende-finished-job-id)))
                (should snapshot)
                (should (eq (ende-finished-job-state snapshot) 'cancelled))
                (should (equal (ende-finished-job-buffer-name snapshot)
                               buffer-name))
                (should (integerp (ende-finished-job-line snapshot)))
                (should (ende-finished-job-p snapshot))))))
      (when (buffer-live-p source)
        (kill-buffer source))
      (setq ende--job-handlers
            (assq-delete-all 'finished-test ende--job-handlers))
      (ende-test--reset))))

(defun ende-finished-test--entry
    (buffer file &optional id operation request-id question-id line)
  "Make one navigation fixture for BUFFER and FILE."
  (ende-finished--make-job
   :id (or id 9001) :operation (or operation 'chat)
   :state 'completed :finished-at (float-time)
   :buffer buffer :buffer-name (buffer-name buffer) :file file
   :line (or line 6)
   :title (pcase question-id
            ("question-1" "First question")
            ("question-2" "Second question")
            (_ "Source heading"))
   :request-id request-id :question-id question-id))

(defun ende-finished-test--visible-heading (window)
  "Return the heading at WINDOW's actual visible point."
  (with-current-buffer (window-buffer window)
    (save-excursion
      (goto-char (window-point window))
      (org-back-to-heading t)
      (org-get-heading t t t t))))

(ert-deftest ende-test/finished-chat-navigation-uses-durable-identities ()
  (let* ((file (make-temp-file "ende-finished-chat-" nil ".org"))
         (buffer nil)
         (first nil)
         (second nil)
         (finished-buffer (get-buffer-create ende-finished-buffer-name))
         (text (concat
                "* Conversation\n"
                "** First question\n:PROPERTIES:\n:ENDE_ROLE: user\n"
                ":ENDE_CHAT_ID: question-1\n:END:\nFirst instructions.\n"
                "** Agent: Reply\n:PROPERTIES:\n:ENDE_ROLE: assistant\n"
                ":ENDE_CHAT_REQUEST_ID: request-1\n:END:\nFirst answer.\n"
                "** Second question\n:PROPERTIES:\n:ENDE_ROLE: user\n"
                ":ENDE_CHAT_ID: question-2\n:END:\nSecond instructions.\n"
                "** Agent: Reply\n:PROPERTIES:\n:ENDE_ROLE: assistant\n"
                ":ENDE_CHAT_REQUEST_ID: request-2\n:END:\nSecond answer.\n")))
    (unwind-protect
        (progn
          (with-temp-file file (insert text))
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer (org-mode) (save-buffer))
          (setq first (ende-finished-test--entry
                       buffer file 9001 'chat "request-1" "question-1"))
          (setq second (ende-finished-test--entry
                        buffer file 9002 'chat "request-2" "question-2"))
          (let ((window (selected-window)))
            (switch-to-buffer buffer)
            (goto-char (point-min))
            (org-fold-show-all)
            (goto-char (point-max))
            (set-window-point window (point))
            (ende-finished-visit-job first)
            (should (equal (ende-finished-test--visible-heading window)
                           "Agent: Reply"))
            (should (= (line-number-at-pos (window-point window)) 8))
            (should-not (invisible-p (window-point window)))
            (ende-finished-visit-job second)
            (should (= (line-number-at-pos (window-point window)) 20))
            (should-not (invisible-p (window-point window)))
            ;; With the source already displayed in another window, RET must
            ;; update that destination window's point, not merely buffer point.
            (let ((other (split-window-right)))
              (set-window-buffer other buffer)
              (set-window-point other (point-min))
              (select-window window)
              (switch-to-buffer finished-buffer)
              (ende-finished-visit-job first)
              (should (eq (window-buffer (selected-window)) buffer))
              (should (= (window-point (selected-window))
                         (save-excursion
                           (goto-char (point-min))
                           (re-search-forward
                            ":ENDE_CHAT_REQUEST_ID: request-1")
                           (org-back-to-heading t)
                           (point))))
              (delete-other-windows)))
          ;; Closing and reopening must still choose this request's reply.
          (kill-buffer buffer)
          (setq buffer nil)
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer (org-mode))
          (ende-finished-visit-job second)
          (should (= (line-number-at-pos (window-point)) 20))
          ;; If the reply is removed, use this job's own question ID.
          (with-current-buffer buffer
            (goto-char (point-min))
            (re-search-forward ":ENDE_CHAT_REQUEST_ID: request-2")
            (org-back-to-heading t)
            (delete-region (line-beginning-position) (progn
                                                        (org-end-of-subtree t t)
                                                        (point)))
            (save-buffer))
          (ende-finished-visit-job second)
          (should (equal (ende-finished-test--visible-heading
                          (selected-window)) "Second question"))
          (with-current-buffer finished-buffer
            (ende-finished-mode)
            (setq ende-finished-jobs (list first second))
            (tabulated-list-print t)
            (should (string-match-p "First question" (buffer-string)))
            (should (string-match-p "Second question" (buffer-string)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (when (file-exists-p file) (delete-file file))
      (when (buffer-live-p finished-buffer) (kill-buffer finished-buffer)))))

(ert-deftest ende-test/finished-chat-navigation-survives-renamed-replies ()
  (let* ((buffer (generate-new-buffer " *ende-finished-renamed-replies*"))
         (finished-buffer (get-buffer-create ende-finished-buffer-name))
         (first (ende-finished--make-job
                 :id 9301 :operation 'chat :state 'completed
                 :finished-at (float-time) :buffer buffer
                 :request-id "renamed-request-1" :question-id "renamed-q1"))
         (second (ende-finished--make-job
                  :id 9302 :operation 'chat :state 'completed
                  :finished-at (float-time) :buffer buffer
                  :request-id "renamed-request-2" :question-id "renamed-q2")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (org-mode)
            (insert "* Conversation\n"
                    "** Question one\n:PROPERTIES:\n:ENDE_ROLE: user\n"
                    ":ENDE_CHAT_ID: renamed-q1\n:END:\nAsk one.\n"
                    "** A renamed assistant heading\n:PROPERTIES:\n"
                    ":ENDE_ROLE: assistant\n"
                    ":ENDE_CHAT_REQUEST_ID: renamed-request-1\n:END:\n"
                    "First response.\n"
                    "** Question two\n:PROPERTIES:\n:ENDE_ROLE: user\n"
                    ":ENDE_CHAT_ID: renamed-q2\n:END:\nAsk two.\n"
                    "** Agent: Work\n:PROPERTIES:\n:ENDE_CHAT_WORK: t\n"
                    ":ENDE_CHAT_REQUEST_ID: renamed-request-2\n:END:\n"
                    "*** Internal assistant\n:PROPERTIES:\n"
                    ":ENDE_ROLE: assistant\n"
                    ":ENDE_CHAT_REQUEST_ID: renamed-request-2\n:END:\n"
                    "Not the reply.\n"
                    "** Summary\n:PROPERTIES:\n:ENDE_ROLE: assistant\n"
                    ":ENDE_CHAT_REQUEST_ID: renamed-request-2\n"
                    ":ENDE_CHAT_CONTEXT_FENCE: t\n:END:\nSummary.\n"
                    "** Another renamed assistant heading\n:PROPERTIES:\n"
                    ":ENDE_ROLE: assistant\n"
                    ":ENDE_CHAT_REQUEST_ID: renamed-request-2\n:END:\n"
                    "Second response.\n")
            (goto-char (point-max)))
          (with-current-buffer finished-buffer
            (setq ende-finished-jobs (list first second)))
          (ende-finished-visit-job first)
          (should (eq (window-buffer (selected-window)) buffer))
          (should (equal (ende-finished-test--visible-heading
                          (selected-window)) "A renamed assistant heading"))
          (ende-finished-visit-job second)
          (should (equal (ende-finished-test--visible-heading
                          (selected-window)) "Another renamed assistant heading"))
          (with-current-buffer buffer
            (goto-char (point-min))
            (re-search-forward "\\*\\* Summary$")
            (org-entry-delete nil "ENDE_CHAT_CONTEXT_FENCE"))
          ;; Without the fence property this heading is an ordinary assistant
          ;; response for the request, even though its title says Summary.
          (ende-finished-visit-job second)
          (should (equal (ende-finished-test--visible-heading
                          (selected-window)) "Summary")))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (when (buffer-live-p finished-buffer) (kill-buffer finished-buffer)))))

(ert-deftest ende-test/finished-reload-upgrades-legacy-snapshots-once ()
  (let* ((file (make-temp-file "ende-finished-reload-" nil ".org"))
         (buffer nil)
         (ende-finished-limit 10)
         (ende-finished-jobs nil)
         (ende-finished--live-jobs (make-hash-table :test #'eql))
         (finished-buffer (get-buffer-create ende-finished-buffer-name))
         marker legacy-without-marker legacy-with-marker)
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "* Conversation\n"
                    "** The recovered request\n:PROPERTIES:\n"
                    ":ENDE_ROLE: user\n:ENDE_CHAT_ID: reload-question\n"
                    ":END:\nPlease inspect this.\n"
                    "** An unrelated user question\n:PROPERTIES:\n"
                    ":ENDE_ROLE: user\n:ENDE_CHAT_ID: unrelated-question\n"
                    ":END:\nDo not associate this by position.\n"
                    "** Renamed assistant response\n:PROPERTIES:\n"
                    ":ENDE_ROLE: assistant\n"
                    ":ENDE_CHAT_REQUEST_ID: reload-request\n"
                    ":ENDE_CHAT_REPLY_TO: reload-question\n:END:\n"
                    "Done.\n"))
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer (org-mode))
          (setq marker (with-current-buffer buffer
                         (copy-marker (point-min))))
          ;; These vectors exactly model the two previous generated layouts:
          ;; prior to markers (11 slots), and marker plus no question ID (12).
          (setq legacy-without-marker
                (vector 'ende-finished-job 9401 'chat 'failed 123.0 buffer
                        (buffer-name buffer) file 5 "Agent: Reply"
                        "reload-request"))
          (setq legacy-with-marker
                (vector 'ende-finished-job 9402 'chat-compact 'cancelled 124.0
                        buffer (buffer-name buffer) marker file 6
                        "Agent: Context Fence" "reload-request"))
          (setq ende-finished-jobs
                (list legacy-without-marker legacy-with-marker
                      (vector 'ende-finished-job 9403 'chat 'failed 125.0 buffer
                              (buffer-name buffer) file 7 "Agent: Reply"
                              "missing-link-request")))
          (cl-letf (((symbol-function 'message) #'ignore))
            (ende-reload))
          (should (cl-every #'ende-finished-job-p ende-finished-jobs))
          (should (equal (mapcar #'ende-finished-job-id ende-finished-jobs)
                         '(9401 9402 9403)))
          (should (= (ende-finished-job-finished-at (car ende-finished-jobs))
                     123.0))
          (should (equal (ende-finished-job-request-id (car ende-finished-jobs))
                         "reload-request"))
          (should (eq (ende-finished-job-marker (car ende-finished-jobs)) nil))
          (should (eq (ende-finished-job-marker (cadr ende-finished-jobs)) marker))
          (should (eq (ende-finished-job-state (car ende-finished-jobs)) 'failed))
          (should (equal (ende-finished-job-buffer-name (car ende-finished-jobs))
                         (buffer-name buffer)))
          (should (equal (ende-finished-job-file (car ende-finished-jobs)) file))
          (should (= (ende-finished-job-line (car ende-finished-jobs)) 5))
          (should (equal (ende-finished-job-question-id (car ende-finished-jobs))
                         "reload-question"))
          (should (equal (ende-finished-job-title (car ende-finished-jobs))
                         "The recovered request"))
          (should (equal (ende-finished-job-question-id (cadr ende-finished-jobs))
                         "reload-question"))
          (should-not (ende-finished-job-question-id (nth 2 ende-finished-jobs)))
          (should (equal (ende-finished-job-title (nth 2 ende-finished-jobs))
                         "Agent: Reply"))
          (ende-finished-visit-job (car ende-finished-jobs))
          (should (eq (window-buffer (selected-window)) buffer))
          (should (equal (ende-finished-test--visible-heading
                          (selected-window)) "Renamed assistant response"))
          ;; A normal new chat record still uses the current struct layout.
          (with-current-buffer buffer
            (let* ((target (ende-target-create
                            :buffer buffer :start (point-min) :end (point-min)
                            :cursor (point-min) :review-policy 'ignore))
                   (job (ende--make-job
                         :id 9404 :type 'chat :target target
                         :instruction '(:request-id "reload-request"
                                        :question-id "reload-question")))
                   (entry (progn
                            (ende-finished--track job)
                            (ende-finished--record job 'completed))))
              (should (ende-finished-job-p entry))
              (should (equal (ende-finished-job-question-id entry)
                             "reload-question"))
              (should (equal (ende-finished-job-title entry)
                             "The recovered request"))))
          (with-current-buffer finished-buffer
            (ende-finished-mode)
            (tabulated-list-print t)
            (should (string-match-p "The recovered request"
                                    (buffer-string)))))
      (when (markerp marker) (set-marker marker nil))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (when (file-exists-p file) (delete-file file))
      (when (buffer-live-p finished-buffer) (kill-buffer finished-buffer)))))

(ert-deftest ende-test/finished-chat-titles-resolve-at-settlement ()
  (let ((buffer (generate-new-buffer " *ende-finished-titles*"))
        (ende-finished-limit 10)
        (ende-finished-jobs nil)
        (ende-finished--live-jobs (make-hash-table :test #'eql))
        jobs)
    (unwind-protect
        (with-current-buffer buffer
          (org-mode)
          (insert "** \n:PROPERTIES:\n:ENDE_ROLE: user\n"
                  ":ENDE_CHAT_ID: title-question\n:END:\n"
                  "A generated title should be used.\n")
          (let* ((job (ende--make-job
                       :id 9101 :type 'chat :target
                       (ende-target-create :buffer buffer :start 1 :end 1
                                           :cursor 1 :review-policy 'ignore)
                       :instruction '(:request-id "title-request"
                                      :question-id "title-question"))))
            (ende-finished--track job)
            ;; Generated during execution, after the queued snapshot was made.
            (goto-char (point-min))
            (org-edit-headline "Generated title")
            (push (ende-finished--record job 'completed) jobs))
          (goto-char (point-max))
          (insert "\n** \n:PROPERTIES:\n:ENDE_ROLE: user\n"
                  ":ENDE_CHAT_ID: excerpt-question\n:END:\n"
                  "  Summarize   this request across\nmultiple lines.\n")
          (let ((job (ende--make-job
                      :id 9102 :type 'chat :target
                      (ende-target-create :buffer buffer :start (point-min)
                                          :end (point-min) :cursor (point-min)
                                          :review-policy 'ignore)
                      :instruction '(:request-id "excerpt-request"
                                     :question-id "excerpt-question"))))
            (ende-finished--track job)
            (push (ende-finished--record job 'failed) jobs))
          (let ((generated (cl-find 9101 jobs :key #'ende-finished-job-id))
                (excerpt (cl-find 9102 jobs :key #'ende-finished-job-id)))
            (should (equal (ende-finished-job-title generated)
                           "Generated title"))
            (should (equal (ende-finished-job-title excerpt)
                           "Summarize this request across multiple lines."))
            (should-not (string-match-p "^\\*+ Summarize" (buffer-string)))))
      (dolist (job ende-finished-jobs)
        (when (markerp (ende-finished-job-marker job))
          (set-marker (ende-finished-job-marker job) nil)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest ende-test/finished-non-chat-markers-and-reopened-lines ()
  (let* ((file (make-temp-file "ende-finished-non-chat-" nil ".org"))
         (buffer nil)
         (other nil)
         (finished-buffer (get-buffer-create ende-finished-buffer-name))
         (ende-finished-limit 1)
         (ende-finished-jobs nil)
         (ende-finished--live-jobs (make-hash-table :test #'eql)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "first\nsecond\nthird\n"))
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer (text-mode) (goto-char (point-min)))
          (let* ((target (ende-target-create
                          :buffer buffer :start 14 :end 14 :cursor 14
                          :review-policy 'ignore))
                 (job (ende--make-job :id 9201 :type 'insert :target target))
                 (entry nil))
            (ende-finished--track job)
            (with-current-buffer buffer
              (goto-char (point-min))
              (insert "new\n"))
            (setq entry (ende-finished--record job 'completed))
            (should (eq (marker-buffer (ende-finished-job-marker entry)) buffer))
            (with-current-buffer buffer
              (should (= (line-number-at-pos
                          (ende-finished--recorded-position entry buffer)) 4)))
            (setq other (generate-new-buffer " *ende-finished-eviction*"))
            (with-current-buffer other
              (text-mode)
              (let* ((target (ende-target-create
                              :buffer other :start 1 :end 1 :cursor 1
                              :review-policy 'ignore))
                     (next (ende--make-job :id 9202 :type 'insert :target target)))
                (ende-finished--track next)
                (ende-finished--record next 'completed)))
            (should (= (length ende-finished-jobs) 1))
            (should (null (marker-buffer (ende-finished-job-marker entry))))
            (should (eq (marker-buffer
                         (ende-finished-job-marker (car ende-finished-jobs)))
                        other))
            (setq ende-finished-jobs (list entry)))
          (kill-buffer buffer)
          (setq buffer nil)
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer (text-mode))
          (let ((line (with-current-buffer buffer
                        (save-excursion
                          (goto-char (point-min))
                          (forward-line 3)
                          (point)))))
            (ende-finished-visit-job (car ende-finished-jobs))
            (should (= (window-point (selected-window)) line)))
          (with-current-buffer finished-buffer
            (ende-finished-mode)
            (setq ende-finished-jobs (list (car ende-finished-jobs)))
            (tabulated-list-print t)))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (when (buffer-live-p other) (kill-buffer other))
      (when (file-exists-p file) (delete-file file))
      (when (buffer-live-p finished-buffer) (kill-buffer finished-buffer)))))

(ert-deftest ende-test/finished-selector-entry-is-advertised-and-opens-list ()
  (let ((closed nil)
        (opened nil))
    (with-temp-buffer
      (ende-selector-mode)
      (setq ende-selector--providers '(remote)
            ende-selector--provider-index 0
            ende-selector--models '("model")
            ende-selector--model-index 0
            ende-selector--model-state :ready)
      (ende-selector--render)
      (should (string-match-p "f  - finished jobs" (buffer-string)))
      (should (eq (key-binding (kbd "f"))
                  #'ende-selector-finished-jobs))
      (cl-letf (((symbol-function 'ende-selector-close)
                 (lambda () (setq closed t)))
                ((symbol-function 'ende-finished-open)
                 (lambda () (setq opened t))))
        (call-interactively (key-binding (kbd "f")))))
    (should closed)
    (should opened)))

(ert-deftest ende-test/finished-module-reloads-before-selector-with-one-observer ()
  (let* ((directory (file-name-directory (locate-library "ende.el")))
         (files (ende-reload--files directory)))
    (should (< (cl-position "ende-finished.el" files :test #'equal)
               (cl-position "ende-selector.el" files :test #'equal)))
    (cl-letf (((symbol-function 'message) #'ignore))
      (ende-reload))
    (should (featurep 'ende-finished))
    (should (eq (lookup-key ende-selector-mode-map (kbd "f"))
                #'ende-selector-finished-jobs))
    (should (= (cl-count #'ende-finished--job-event
                         ende-job-event-functions)
               1))))

(provide 'ende-finished-test)

;;; ende-finished-test.el ends here
