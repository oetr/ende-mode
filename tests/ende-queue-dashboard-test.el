;;; ende-queue-dashboard-test.el --- Unified tree integration -*- lexical-binding: t; -*-

(require 'ende-chat-test-helpers)
(require 'ende-queue-ui)

(defun ende-test--dashboard-reset ()
  "Clear retained test state and dashboard state."
  (ende-test--reset)
  (when (buffer-live-p ende-queue-ui--buffer) (kill-buffer ende-queue-ui--buffer))
  (clrhash ende-queue-ui--folds)
  (clrhash ende-queue-ui--marked-ids))

(defun ende-test--dashboard-child (parent title)
  "Publish a fixture child of PARENT with TITLE."
  (ende-job-publish-child
   parent (list 'agent :profile ende-test--dashboard-profile
                :instruction (list :task title) :prepare #'ende-agent-private-target)
   (ende-job-next-child-path parent)))

(ert-deftest ende-test/dashboard-renders-both-sections-and-descendant-folds ()
  (ende-test--dashboard-reset)
  (let ((ende--queue-suspended t) (ende--job-handlers '((insert . ignore)))
        root child grandchild finished dashboard)
    (unwind-protect
        (with-temp-buffer
          (rename-buffer "源 source")
          (insert "user source")
          (setq root (ende-enqueue (ende-test--dashboard-request ende-test--dashboard-profile)))
          (let ((ende--queue-suspended nil)) (ende-process-next))
          (setq child (ende-test--dashboard-child root "Child task")
                grandchild (progn
                             (setf (ende-job-family-policy-frozen-p (ende-job-family root)) t)
                             (ende-test--dashboard-child child "Grandchild task"))
                finished (ende-enqueue (ende-test--dashboard-request ende-test--dashboard-profile)))
          (ende--job-settle finished 'completed "done")
          (setf (ende-job-title root) "Active root"
                (ende-job-title child) "Child task"
                (ende-job-title grandchild) "Grandchild task"
                (ende-job-title finished) "Finished root"
                (ende-job-last-usage grandchild) '(:input_tokens 100200 :output_tokens 9000)
                (ende-job-enqueued-at root) 10
                (ende-job-started-at root) 20
                (ende-job-enqueued-at finished) 10
                (ende-job-started-at finished) 20
                (ende-job-finished-at finished) 68)
          (cl-letf (((symbol-function 'float-time) (lambda (&optional _) 256)))
            (setq dashboard (ende-queue-open))
            (with-current-buffer dashboard
              (let ((text (buffer-string)))
                (should (string-match-p "Active (1)" text))
                (should (string-match-p "Finished (1)" text))
                (should (string-match-p "100,200" text))
                (should (string-match-p "3m56s" text))
                (should (string-match-p "48s" text))
                (should (string-match-p "└─ ▼ Child task" text))
                (should (string-match-p "   └─ Grandchild task" text))
                (should-not (string-match-p "Provider:\\|Activity:\\|Model:" text)))
              (let ((positions (ende-queue-ui--entry-positions)) widths)
                (dolist (position positions)
                  (goto-char position)
                  (search-forward (ende-job-title (ende-queue-ui--current-job)))
                  (push (- (current-column)
                           (string-width (ende-job-title (ende-queue-ui--current-job)))) widths))
                (should (= (nth 0 widths) (nth 3 widths))))
              (goto-char (ende-queue-ui--position-for-key (cons 'job (ende-job-id child))))
              (ende-queue-ui-toggle-entry)
              (should-not (ende-queue-ui--position-for-key (cons 'job (ende-job-id grandchild))))
              (should (= (length (ende-queue-ui--entry-positions)) 3))
              (goto-char (ende-queue-ui--position-for-key (cons 'job (ende-job-id finished))))
              (dolist (command '(ende-queue-ui-cancel ende-queue-ui-mark
                                 ende-queue-ui-inject-guidance ende-queue-ui-move-up))
                (should-error (funcall command) :type 'user-error))
              (should truncate-lines)
              (should-not (get-text-property (point) 'ende-queue-ui-owner)))))
      (ende-test--dashboard-reset))))

(ert-deftest ende-test/dashboard-elapsed-resets-only-on-first-start ()
  (ende-test--dashboard-reset)
  (let ((ende--queue-suspended t)
        (ende--job-handlers '((insert . ignore)))
        (clock 10))
    (unwind-protect
        (with-temp-buffer
          (insert "Elapsed time source")
          (cl-letf (((symbol-function 'float-time) (lambda (&optional _) clock)))
            (let ((job (ende-enqueue
                        (ende-test--dashboard-request ende-test--dashboard-profile))))
              (cl-labels ((elapsed () (nth 4 (ende-queue-ui--cells job clock))))
                (setq clock 50)
                (should (eq (ende-job-state job) 'queued))
                (should (equal (elapsed) "40s"))
                (setq clock 70)
                (let ((ende--queue-suspended nil)) (ende-process-next))
                (should (eq (ende-job-state job) 'running))
                (should (equal (elapsed) "0s"))
                (setq clock 90)
                (should (equal (elapsed) "20s"))
                (setq clock 100)
                (ende-job-suspend job #'ignore)
                (should (eq (ende-job-state job) 'suspended))
                (should (equal (elapsed) "30s"))
                (setq clock 110)
                (ende-job-resume-queued job)
                (should (eq (ende-job-state job) 'queued))
                (should (equal (elapsed) "40s"))
                (setq clock 130)
                (let ((ende--queue-suspended nil)) (ende-process-next))
                (should (eq (ende-job-state job) 'running))
                (should (equal (elapsed) "1m00s"))
                (setq clock 150)
                (ende-job-finish job "done")
                (should (eq (ende-job-state job) 'completed))
                (setq clock 200)
                (should (equal (elapsed) "1m20s"))))))
      (ende-test--dashboard-reset))))

(ert-deftest ende-test/dashboard-tool-status-keeps-following-columns-fixed ()
  (ende-test--dashboard-reset)
  (let ((ende--job-handlers '((insert . ignore))) dashboard)
    (unwind-protect
        (with-temp-buffer
          (insert "Status alignment source")
          (let ((job (ende-enqueue
                      (ende-test--dashboard-request ende-test--dashboard-profile)))
                positions)
            (setf (ende-job-title job) "Stable task")
            (setq dashboard (ende-queue-open))
            (dolist (status '("Thinking" "read_file" "list_project_buffers"
                              "a_tool_name_longer_than_twenty_columns" "Done (ok)"))
              (ende--handle-llm-event job (list :type :status :status status))
              (with-current-buffer dashboard
                (ende-queue-ui--render)
                (goto-char (point-min))
                (search-forward "Status")
                (let* ((start (- (current-column) (length "Status")))
                       (expected (if (> (string-width status) 20)
                                     "a_tool_name_longer_…" status)))
                  (search-forward "Context")
                  (push (current-column) positions)
                  (goto-char (ende-queue-ui--position-for-key
                              (cons 'job (ende-job-id job))))
                  (move-to-column start)
                  (should (looking-at (regexp-quote expected)))
                  (forward-char (length expected))
                  (should (looking-at " +0"))
                  (search-forward "Stable task")
                  (push (current-column) positions))))
            (let ((task-column (pop positions))
                  (context-column (pop positions)))
              (while positions
                (should (= task-column (pop positions)))
                (should (= context-column (pop positions)))))))
      (ende-test--dashboard-reset))))

(ert-deftest ende-test/dashboard-untitled-jobs-never-preview-prompts ()
  (ende-test--dashboard-reset)
  (let ((ende--queue-suspended t) root child dashboard)
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "* \n#+begin_ende\n(clear-tools)\n#+end_ende\nPRIVATE_USER_INPUT\n"
                  "* Named question\nPRIVATE_NAMED_INPUT\n")
          (ende-test--chat-goto "* ")
          (setq root (ende-ask ende-test--dashboard-profile))
          (setf (ende-job-family-policy-frozen-p (ende-job-family root)) t)
          (setq child
                (ende-job-publish-child
                 root (list 'agent :profile ende-test--dashboard-profile
                            :instruction '(:task "PRIVATE_CHILD_INPUT")
                            :prepare #'ende-agent-private-target)
                 (ende-job-next-child-path root)))
          (ende-test--chat-goto "* Named question")
          (should (equal (ende-job-title (ende-ask ende-test--dashboard-profile))
                         "Named question"))
          (should (equal (ende-job-title root) "tbd."))
          (should (equal (ende-job-title child) "tbd."))
          (puthash (ende-job-id root) 'expanded ende-queue-ui--folds)
          (setq dashboard (ende-queue-open))
          (dotimes (pass 2)
            (when (= pass 1)
              (ende--job-settle child 'completed "done")
              (ende--job-settle root 'completed "done"))
            (with-current-buffer dashboard
              (ende-queue-ui--render)
              (let ((text (buffer-string)))
                (should (string-match-p "└─ tbd\\." text))
                (should (string-match-p "Named question" text))
                (should-not (string-match-p "PRIVATE_\\|begin_ende" text)))))
          (setf (ende-job-title child) "Generated child title")
          (with-current-buffer dashboard
            (ende-queue-ui--render)
            (should (string-match-p "└─ Generated child title" (buffer-string)))))
      (ende-test--dashboard-reset))))

(ert-deftest ende-test/dashboard-retained-work-and-hooks-survive-two-reloads ()
  (save-window-excursion
    (ende-test--dashboard-reset)
    (let ((ende--queue-suspended t) root child work dashboard source)
      (unwind-protect
          (with-temp-buffer
            (setq source (current-buffer))
            (insert "never erase this source")
            (setq root (ende-enqueue (ende-test--dashboard-request ende-test--dashboard-profile)))
            (setf (ende-job-family-policy-frozen-p (ende-job-family root)) t)
            (setq child (ende-test--dashboard-child root "Retained work")
                  work (ende-job-work-buffer child))
            (ende--job-settle root 'cancelled)
            (setq dashboard (ende-queue-open))
            (dotimes (_ 2)
              (ende-reload)
              (with-current-buffer dashboard
                (ende-queue-ui--render)
                (goto-char (ende-queue-ui--position-for-key (cons 'job (ende-job-id root))))
                (unless (ende-queue-ui--expanded-p root) (ende-queue-ui-toggle-entry))
                (goto-char (ende-queue-ui--position-for-key (cons 'job (ende-job-id child))))
                (ende-queue-ui-visit-agent-work))
              (should (eq (window-buffer (selected-window)) work))
              (should (buffer-local-value 'buffer-read-only work))
              (with-current-buffer dashboard
                (should (eq (key-binding (kbd "w")) #'ende-queue-ui-visit-agent-work))
                (should (eq (key-binding (kbd "i")) #'ende-queue-ui-inject-guidance))
                (should (memq (key-binding (kbd "v")) '(nil undefined)))
                (should (memq (key-binding (kbd "d")) '(nil undefined))))
              (dolist (observer '(ende-agent--work-event ende-chat--dispose-staging-buffer
                                   ende-queue-ui--job-event))
                (should (= (cl-count observer ende-job-event-functions) 1)))
              (should (= (cl-count #'ende-queue-ui--queue-event ende-queue-event-functions) 1))
              (should (buffer-live-p source)))
            (kill-buffer work)
            (with-current-buffer dashboard
              (should-error (ende-queue-ui-visit-agent-work) :type 'user-error))
            (should (member "ende-queue-source.el"
                            (ende-reload--files default-directory))))
        (ende-test--dashboard-reset)))))

(ert-deftest ende-test/dashboard-forgets-evicted-ids-even-while-hidden ()
  (save-window-excursion
  (ende-test--dashboard-reset)
  (let ((ende--queue-suspended t) (ende-queue-finished-limit 0)
        root child work dashboard terminal-events)
    (unwind-protect
        (with-temp-buffer
          (insert "source stays alive")
          (setq root (ende-enqueue (ende-test--dashboard-request ende-test--dashboard-profile)))
          (setf (ende-job-family-policy-frozen-p (ende-job-family root)) t)
          (setq child (ende-test--dashboard-child root "Child")
                work (ende-job-work-buffer child)
                dashboard (ende-queue-open))
          (puthash (ende-job-id root) 'expanded ende-queue-ui--folds)
          (puthash (ende-job-id child) t ende-queue-ui--marked-ids)
          (with-current-buffer dashboard
            (ende-queue-ui--render)
            (set-window-buffer (selected-window) (get-buffer-create " *hidden-dashboard*")))
          (let ((ende-job-event-functions
                 (cons (lambda (_job event)
                         (when (eq (plist-get event :type) :settled)
                           (should (buffer-live-p work))
                           (push t terminal-events))) ende-job-event-functions)))
            (ende--job-settle root 'cancelled))
          (should (= (length terminal-events) 2))
          (should-not (buffer-live-p work))
          (should-not (gethash (ende-job-id root) ende-queue-ui--folds))
          (should-not (gethash (ende-job-id child) ende-queue-ui--marked-ids))
          (with-current-buffer dashboard
            (should-error (ende-queue-ui--current-job) :type 'user-error)))
      (ende-test--dashboard-reset)
      (when-let* ((hidden (get-buffer " *hidden-dashboard*"))) (kill-buffer hidden))))))

(provide 'ende-queue-dashboard-test)
