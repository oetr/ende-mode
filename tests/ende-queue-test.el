;;; ende-queue-test.el --- Queue lifecycle tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Coverage for FIFO ordering, cancellation, stale callbacks, and settlement.

;;; Code:

(require 'ert)
(require 'ende)
(require 'ende-test-helpers)
(require 'cl-lib)



(defun ende-test--sleep-process ()
  "Create a process that remains live until explicitly terminated."
  (make-process :name (generate-new-buffer-name "ende-test-sleep")
                :command (list "sleep" "30")
                :connection-type 'pipe
                :noquery t))



(ert-deftest ende-test/queue-snapshots-do-not-expose-list-state ()
  (let* ((active-job (ende--make-job :id 1 :state 'running))
         (pending-job (ende--make-job :id 2 :state 'queued))
         (ende--active-jobs (list active-job))
         (ende-queue (list pending-job)))
    (let ((active (ende-active-jobs))
          (pending (ende-pending-jobs)))
      (setcar active :changed)
      (setcar pending :changed)
      (should (eq (car ende--active-jobs) active-job))
      (should (eq (car ende-queue) pending-job))
      (should (cl-every #'ende-job-p (ende-active-jobs)))
      (should (cl-every #'ende-job-p (ende-pending-jobs))))))

(ert-deftest ende-test/family-queue-keeps-markers-and-sorts-numeric-paths ()
  (let* ((root (ende--make-job :id 1 :state 'queued :schedule-path nil))
         (family (ende--make-job-family :root root))
         (ende-queue (list family root))
         (ende--queue-tail (last ende-queue))
         (paths '((2) (1 2) (1 1) (1)))
         children)
    (setf (ende-job-family root) family)
    (dolist (path paths)
      (let ((child (ende--make-job :id (+ 10 (length children))
                                   :state 'queued :family family
                                   :schedule-path path)))
        (push child children)
        (ende--queue-insert-family-member child)))
    (should (equal (mapcar #'ende-job-schedule-path
                           (ende-physical-waiting-jobs))
                   '(nil (1) (1 1) (1 2) (2))))
    (should (equal (mapcar #'ende-job-id (ende-queue-entries))
                   '(1 13 12 11 10)))
    (should (eq (car (ende-queue-family-markers)) family))
    (should (ende--schedule-path-before-p '(1 2) '(1 10)))
    (should (ende--schedule-path-before-p '(1) '(1 1)))
    (should-not (ende--schedule-path-before-p '(2) '(1 9)))))

(ert-deftest ende-test/cancelling-parent-terminalizes-live-descendants ()
  (ende-test--reset)
  (let ((ende--queue-suspended t)
        (ende-llm-function (ende-test--fake-llm nil t))
        (child-cancelled nil)
        root child)
    (ende-test--with-buffer "base"
      (let ((target (ende-target-create
                     :buffer (current-buffer)
                     :start (point-min) :end (point-min) :cursor (point-min))))
        (setq root (ende-enqueue
                    (list 'ownership-test :target target
                          :profile ende-test--dashboard-profile))
              child (ende--make-job :id 200 :type 'ownership-test
                                    :state 'queued :family (ende-job-family root)
                                    :parent root :schedule-path '(1)
                                    :target target
                                    :profile ende-test--dashboard-profile
                                    :cancel-function
                                    (lambda ()
                                      (setq child-cancelled
                                            (eq (ende-job-state child)
                                                'cancelled)))))
        (setf (ende-job-children root) (list child))
        (ende--queue-insert-family-member child)
        (ende--cancel-live-jobs (list root))
        (should (eq (ende-job-state root) 'cancelled))
        (should (eq (ende-job-state child) 'cancelled))
        (should child-cancelled)
        (should-not (ende-all-live-jobs))
        (should-not (ende-queue-family-markers))))))

(ert-deftest ende-test/publishing-agent-child-preserves-family-and-origin ()
  (ende-test--reset)
  (let ((ende--queue-suspended t)
        (ende-llm-function (ende-test--fake-llm nil t))
        root child path)
    (ende-test--with-buffer "origin"
      (setq root
            (ende-enqueue
             (ende-test--dashboard-request '(:name remote :model "fixture"))))
      (setf (ende-job-family-policy-frozen-p (ende-job-family root)) t)
      (setq path (ende-job-next-child-path root)
            child
            (ende-job-publish-child
             root
             (cons 'agent
                   (list :profile '(:name remote :model "fixture")
                         :access-policy '(:active-tools (about_thinking))
                         :instruction '(:task "inspect")
                         :prepare (lambda (job)
                                    (setf (ende-job-target job)
                                          (ende-agent-private-target job)))))
             path))
      (should (equal (ende-job-schedule-path child) '(1)))
      (should (eq (ende-job-parent child) root))
      (should (eq (ende-job-family child) (ende-job-family root)))
      (should (eq (ende-job-origin-buffer child) (current-buffer)))
      (should (not (eq (ende-target-buffer (ende-job-target child))
                       (current-buffer))))
      (should (= (ende-job-next-child-index root) 1))
      (ende--cancel-live-jobs (list root))
      (should (eq (ende-job-state child) 'cancelled))
      (should-not (buffer-live-p (ende-job-owned-buffer child))))))

(ert-deftest ende-test/internal-agent-runs-shared-loop-on-private-target ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("child answer") t))
        (ende--queue-suspended t)
        (root-buffer nil)
        family child)
    (ende-test--with-buffer "user source"
      (setq root-buffer (current-buffer)
            family (ende--make-job-family
                    :registry nil :inference-template
                    '(:name remote :model "fixture" :parallel t
                      :max-concurrency 2)))
      (setq child
            (ende--make-job
             :id 902 :type 'agent :state 'queued :family family
             :profile (copy-tree (ende-job-family-inference-template family))
             :access-policy '(:root nil :active-tools (about_thinking)
                              :tool-resources nil)
             :instruction (list :task "inspect safely" :registry nil)
             :schedule-path '(1)))
      (setf (ende-job-family-root family) child
            (ende-job-family child) family
            (ende-job-target child) (ende-agent-private-target child))
      (ende--queue-push family)
      (ende--queue-insert-family-member child)
      (setq ende--queue-suspended nil)
      (ende-process-next)
      (should (eq (ende-job-state child) 'running))
      (let ((sent (plist-get (car ende-test--calls) :messages)))
        (should (string-match-p "inspect safely" (cdr (assoc "user" sent))))
        (should-not (string-match-p "user source" (format "%s" sent)))
        (should-not (string-match-p "Name:"
                                    (format "%s" sent))))
      (ende-test--complete-call (car ende-test--calls))
      (should (eq (ende-job-state child) 'completed))
      (should (equal (ende-job-result child) "child answer"))
      (should-not (buffer-live-p (ende-job-owned-buffer child)))
      (should (buffer-live-p root-buffer)))))

(ert-deftest ende-test/spawn-batch-releases-parent-and-resumes-through-dispatch ()
  (ende-test--reset)
  (let ((ende--queue-suspended t)
        (calls nil)
        (continuation-runs 0)
        parent batch)
    (ende-test--with-buffer "batch source"
      (let* ((target (ende-target-create
                      :buffer (current-buffer)
                      :start (point-min) :end (point-min) :cursor (point-min)))
             (family (ende--make-job-family
                      :parallel t :max-concurrency 2 :policy-frozen-p t
                      :origin (current-buffer)))
             (profile '(:name remote :model "fixture" :parallel t
                        :max-concurrency 2)))
        (setq parent
              (ende--make-job :id 910 :type 'agent :state 'running
                              :family family :schedule-path nil :target target
                              :profile profile :access-policy '(:active-tools nil)
                              :started-at (float-time) :execution-generation 1))
        (setf (ende-job-family-root family) parent)
        (push parent ende--active-jobs)
        (ende--queue-push family)
        (setq batch
              (ende-agent--launch-batch
               parent '("one" "two")
               (lambda (call _position settle)
                 (push (cons call settle) calls))
               (lambda (results)
                 (cl-incf continuation-runs)
                 (ende-job-finish parent results))))
        (should (eq (ende-job-state parent) 'suspended))
        (should-not (memq parent (ende-running-jobs)))
        (should (= (ende-agent-spawn-batch-outstanding batch) 2))
        (funcall (cdr (assoc "one" calls)) "first")
        (should (eq (ende-job-state parent) 'suspended))
        (let ((ende--queue-suspended t))
          (funcall (cdr (assoc "two" calls)) "second")
          (should (eq (ende-job-state parent) 'queued)))
        (should (= continuation-runs 0))
        (should (functionp (ende-job-resume-function parent)))
        (should (eq (ende-agent-spawn-batch-completed-p batch) t))
        (setq ende--queue-suspended nil)
        (ende-process-next)
        (should (= continuation-runs 1))
        (should (eq (ende-job-state parent) 'completed))))))

(ert-deftest ende-test/spawn-batch-settles-synchronous-results-once ()
  (ende-test--reset)
  (let ((ende--queue-suspended t)
        (settled nil)
        (continued nil)
        parent batch)
    (ende-test--with-buffer "batch source"
      (let* ((target (ende-target-create
                      :buffer (current-buffer)
                      :start (point-min) :end (point-min) :cursor (point-min)))
             (family (ende--make-job-family
                      :parallel t :max-concurrency 2 :policy-frozen-p t
                      :origin (current-buffer)))
             (profile '(:name remote :model "fixture" :parallel t
                        :max-concurrency 2)))
        (setq parent
              (ende--make-job :id 912 :type 'agent :state 'running
                              :family family :schedule-path nil :target target
                              :profile profile :started-at (float-time)
                              :execution-generation 4))
        (setf (ende-job-family-root family) parent)
        (push parent ende--active-jobs)
        (ende--queue-push family)
        (setq batch
              (ende-agent--launch-batch
               parent '("one" "two")
               (lambda (call position settle)
                 (push position settled)
                 (funcall settle call)
                 (funcall settle "duplicate"))
               (lambda (results)
                 (setq continued results)
                 (ende-job-finish parent results))))
        (should (eq (ende-job-state parent) 'completed))
        (should (equal (sort settled #'<) '(0 1)))
        (should (equal continued '("one" "two")))
        (should (eq (ende-agent-spawn-batch-completed-p batch) t))))))

(ert-deftest ende-test/spawn-agent-tool-publishes-child-and-settles-final-answer ()
  (ende-test--reset)
  (let ((ende--queue-suspended t)
        (ende-tools--execution-owner nil)
        (ende-tools--register-cancellation-function nil)
        (ende-llm-function (ende-test--fake-llm nil t))
        root child answer settle-count)
    (ende-test--with-buffer "agent origin"
      (setq root
            (ende-enqueue
             (ende-test--dashboard-request '(:name remote :model "fixture"))))
      (setf (ende-job-family-policy-frozen-p (ende-job-family root)) t
            (ende-job-access-policy root)
            '(:active-tools (spawn_agent) :spawn-agent-config worker))
      (setf (ende-job-family-registry (ende-job-family root))
            '((worker . (:root nil :active-tools (about_thinking)
                         :tool-resources nil))))
      (setq ende--active-jobs (list root)
            ende-queue (delq root ende-queue))
      (setf (ende-job-state root) 'running
            (ende-job-execution-generation root) 1)
      (setf (ende-job-type root) 'chat)
      (let ((ende-tools--execution-owner (ende-job-id root)))
        (should-error
         (funcall (ende-tool-handler (ende-tool--find 'spawn_agent))
                  (let ((object (make-hash-table :test #'equal)))
                    (puthash "task" "" object) object)
                  nil (lambda (&rest _) nil)))
        (funcall (ende-tool-handler (ende-tool--find 'spawn_agent))
                 (let ((object (make-hash-table :test #'equal)))
                   (puthash "task" "do inspection" object) object)
                 nil
                 (lambda (result error)
                   (setq settle-count (1+ (or settle-count 0))
                         answer (cons result error))))
        (setq child (car (ende-job-children root)))
        (should (ende-job-p child))
        (should (eq (ende-job-state child) 'queued))
        (should (eq (ende-job-parent child) root))
        (should (equal (plist-get (ende-job-instruction child) :task)
                       "do inspection"))
        (should (buffer-live-p (ende-job-owned-buffer child)))
        (should-not answer)
        (ende-job-skip child "test terminalization")
        (should (= settle-count 1))
        (should (string-match-p "skipped" (car answer)))
        (should-not (cdr answer))
        (should-not (buffer-live-p (ende-job-owned-buffer child))))
      (should (buffer-live-p (current-buffer)))
      (ende--cancel-live-jobs (list root)))))

(ert-deftest ende-test/spawn-agent-settles-if-child-cancels-during-publication ()
  (ende-test--reset)
  (let ((ende--queue-suspended t)
        (ende-tools--execution-owner nil)
        (ende-tools--register-cancellation-function nil)
        (ende-llm-function (ende-test--fake-llm nil t))
        root child settlements)
    (ende-test--with-buffer "publication cancellation"
      (setq root (ende-enqueue
                  (cons 'chat
                        (append
                         (cdr (ende-test--dashboard-request
                               '(:name remote :model "fixture")))
                         '(:instruction (:request-id "publication"
                                        :question-id "question"))))))
      (setf (ende-job-family-policy-frozen-p (ende-job-family root)) t
            (ende-job-family-registry (ende-job-family root))
            '((worker . (:root nil :active-tools (about_thinking)
                         :tool-resources nil)))
            (ende-job-access-policy root)
            '(:active-tools (spawn_agent) :spawn-agent-config worker)
            ende--active-jobs (list root)
            ende-queue (delq root ende-queue))
      (setf (ende-job-state root) 'running
            (ende-job-execution-generation root) 1)
      (add-hook 'ende-job-event-functions
                (lambda (job event)
                  (when (and (eq (ende-job-parent job) root)
                             (eq (plist-get event :type) :queued))
                    (setq child job)
                    (ende-job-skip job "cancelled during publication"))))
      (let ((ende-tools--execution-owner (ende-job-id root)))
        (funcall (ende-tool-handler (ende-tool--find 'spawn_agent))
                 (let ((object (make-hash-table :test #'equal)))
                   (puthash "task" "cancel now" object) object)
                 nil
                 (lambda (result error)
                   (push (list result error) settlements))))
      (should (ende-job-p child))
      (should (eq (ende-job-state child) 'skipped))
      (should (= (length settlements) 1))
      (should (string-match-p "skipped" (caar settlements)))
      (should (null (cadar settlements)))
      (should-not (ende-job-children root))
      (ende--cancel-live-jobs (list root)))))

(ert-deftest ende-test/failed-child-publication-cleans-private-target-and-ownership ()
  (ende-test--reset)
  (let ((ende--queue-suspended t)
        parent child-buffer source
        (prepare-target (symbol-function 'ende-agent-private-target)))
    (ende-test--with-buffer "publication failure"
      (setq source (current-buffer))
      (let ((family (ende--make-job-family
                     :policy-frozen-p t
                     :inference-template '(:name remote :model "fixture"))))
        (setq parent (ende--make-job
                      :id 916 :type 'agent :state 'running
                      :family family :schedule-path nil
                      :target (ende-target-create
                               :buffer (current-buffer)
                               :start (point-min) :end (point-min)
                               :cursor (point-min))
                      :profile '(:name remote :model "fixture")
                      :access-policy '(:active-tools (spawn_agent)
                                       :spawn-agent-config worker)
                      :execution-generation 1))
        (setf (ende-job-family-root family) parent
              (ende-job-family-registry family)
              '((worker . (:root nil :active-tools (about_thinking)
                           :tool-resources nil)))
              ende--active-jobs (list parent))
        (ende--queue-push family)
        (cl-letf (((symbol-function 'ende-agent-private-target)
                   (lambda (job)
                     (prog1 (funcall prepare-target job)
                       (setq child-buffer (ende-job-owned-buffer job)))))
                  ((symbol-function 'ende--queue-insert-family-member)
                   (lambda (_job) (error "injected publication failure"))))
          (should-error (ende-agent--make-child parent "inspect")
                        :type 'error))
        (should (= (or (ende-job-next-child-index parent) 0) 1))
        (should-not (ende-job-children parent))
        (should-not (ende-queue-entries))
        (should (memq family (ende-queue-family-markers)))
        (should (bufferp child-buffer))
        (should-not (buffer-live-p child-buffer))
        (should (buffer-live-p source))
        (ende--cancel-live-jobs (list parent))))))

(ert-deftest ende-test/policy-changes-reject-delegated-jobs-atomically ()
  (ende-test--reset)
  (let ((ende--queue-suspended t))
    (ende-test--with-buffer "policy root"
      (let* ((root (ende-enqueue
                    (ende-test--dashboard-request
                     '(:name remote :model "fixture"))))
             (family (ende-job-family root))
             (before (copy-tree (ende-job-profile root)))
             (child (ende--make-job
                     :id 919 :type 'agent :state 'queued :family family
                     :parent root :schedule-path '(1)
                     :profile '(:name remote :model "fixture")
                     :access-policy '(:active-tools nil))))
        (setf (ende-job-family-root family) root
              (ende-job-children root) (list child))
        (ende--queue-insert-family-member child)
        (should-error (ende-queue-change-policy
                       (list (ende-job-id root) (ende-job-id child))
                       'parallel)
                      :type 'user-error)
        (should (equal (ende-job-profile root) before))
        (should-not (plist-get (ende-job-profile child) :parallel))
        (should (eq (ende-job-state child) 'queued))
        (ende--cancel-live-jobs (list root))))))

(ert-deftest ende-test/fence-waits-for-family-through-delegation ()
  (ende-test--reset)
  (let ((ende--queue-suspended t)
        root child later fence later-ran-after-root)
    (let ((ende--job-handlers
           (append
            (list
             (cons 'fence-parent-test
                   (lambda (job)
                     (setq fence (ende-fence-all)
                           later
                           (ende-enqueue
                            (list 'fence-later-test
                                  :target (ende-target-create
                                           :buffer (current-buffer)
                                           :start (point-min) :end (point-max)
                                           :cursor (point-min))
                                  :profile '(:name remote :model "fixture")))
                           child
                           (ende-job-publish-child
                            job
                            (cons 'fence-child-test
                                  (list :target
                                        (ende-target-create
                                         :buffer (current-buffer)
                                         :start (point-min) :end (point-min)
                                         :cursor (point-min))
                                        :profile (ende-job-profile job)))
                            (ende-job-next-child-path job)))
                     (setf (ende-job-delegated-settlement child)
                           (lambda (_child)
                             (ende-job-resume-queued job)))
                     (ende-job-suspend
                      job (lambda () (ende-job-finish job "root complete")))))
             (cons 'fence-child-test
                   (lambda (job)
                     (should (eq (ende-job-state root) 'suspended))
                     (should (eq (ende-job-state later) 'queued))
                     (should (memq fence (ende-queue-fences)))
                     (ende-job-finish job "child complete")))
             (cons 'fence-later-test
                   (lambda (job)
                     (setq later-ran-after-root
                           (eq (ende-job-state root) 'completed))
                     (ende-job-finish job "later complete"))))
            ende--job-handlers)))
      (ende-test--with-buffer "fenced delegation"
        (setq root
              (ende-enqueue
               (list 'fence-parent-test
                     :target (ende-target-create
                              :buffer (current-buffer)
                              :start (point-min) :end (point-max)
                              :cursor (point-min))
                     :profile '(:name remote :model "fixture"
                                :parallel t :max-concurrency 2))))
        (setq ende--queue-suspended nil)
        (ende-process-next)
        (should (eq (ende-job-state root) 'completed))
        (should (eq (ende-job-state child) 'completed))
        (should (eq (ende-job-state later) 'completed))
        (should later-ran-after-root)
        (should-not (ende-queue-fences))
        (should-not (ende-queue-family-markers))))))

(ert-deftest ende-test/recursive-agents-settle-through-the-family-scheduler ()
  (ende-test--reset)
  (let* ((tool-call (lambda (id task)
                      (list :id id :name "spawn_agent"
                            :arguments (json-encode `((task . ,task))))))
         (policy '(:root nil :active-tools (about_thinking spawn_agent)
                   :tool-resources nil :spawn-agent-config worker))
         (profile '(:name remote :model "fixture" :parallel t
                    :max-concurrency 2))
         (ende-llm-function
          (ende-test--fake-llm
           (list (list :tool-calls (list (funcall tool-call "root-call" "child")))
                 (list :tool-calls (list (funcall tool-call "child-call" "grandchild")))
                 "grandchild answer"
                 "child answer"
                 "root answer") t))
         (ende--queue-suspended t)
         root child grandchild)
    (ende-test--with-buffer "agent source"
      (let* ((request
              (cons 'agent
                    (list :profile profile :access-policy policy
                          :instruction '(:task "root task")
                          :prepare #'ende-agent-private-target)))
             (family nil))
        (setq root (ende--job-from-request request)
              family (ende--make-job-family
                      :root root :parallel t :max-concurrency 2
                      :policy-frozen-p t :origin (current-buffer)
                      :inference-template (ende-job-inference-template root)
                      :registry `((worker . ,policy))))
        (setf (ende-job-family root) family
              (ende-job-schedule-path root) nil
              (ende-job-next-child-index root) 0)
        (ende--job-register-origin root)
        (ende--queue-push family)
        (ende--queue-insert-family-member root)
        (setq ende--queue-suspended nil)
        (ende-process-next)
        (should (= (length ende-test--calls) 1))
        (ende-test--complete-call (nth 0 ende-test--calls))
        (setq child (car (ende-job-children root)))
        (should (eq (ende-job-state root) 'suspended))
        (should (equal (ende-job-schedule-path child) '(1)))
        (should (= (length ende-test--calls) 2))
        (ende-test--complete-call (nth 1 ende-test--calls))
        (setq grandchild (car (ende-job-children child)))
        (should (eq (ende-job-state child) 'suspended))
        (should (equal (ende-job-schedule-path grandchild) '(1 1)))
        (should (= (length ende-test--calls) 3))
        (ende-test--complete-call (nth 2 ende-test--calls))
        (should (equal (ende-job-result grandchild) "grandchild answer"))
        (should (= (length ende-test--calls) 4))
        (ende-test--complete-call (nth 3 ende-test--calls))
        (should (equal (ende-job-result child) "child answer"))
        (should (= (length ende-test--calls) 5))
        (ende-test--complete-call (nth 4 ende-test--calls))
        (should (equal (ende-job-result root) "root answer"))
        (should (eq (ende-job-state root) 'completed))
        (should (eq (ende-job-state child) 'completed))
        (should (eq (ende-job-state grandchild) 'completed))
        (should-not (ende-queue-family-markers))
        (should-not (buffer-live-p (ende-job-owned-buffer child)))
        (should-not (buffer-live-p (ende-job-owned-buffer grandchild)))))))

(ert-deftest ende-test/shared-loop-spawn-batch-orders-and-resumes ()
  (ende-test--reset)
  (let ((ende--queue-suspended t)
        (settlers nil)
        (results nil)
        job family)
    (ende-test--with-buffer "shared loop"
      (setq family (ende--make-job-family :policy-frozen-p t)
            job (ende--make-job :id 915 :type 'agent :state 'running
                                :family family :schedule-path nil
                                :target (ende-target-create
                                         :buffer (current-buffer)
                                         :start (point-min) :end (point-min)
                                         :cursor (point-min))
                                :profile '(:tool-definitions (fixture))
                                :started-at (float-time)
                                :execution-generation 1))
      (setf (ende-job-family-root family) job)
      (ende--queue-push family)
      (push job ende--active-jobs)
      (let ((callbacks
             (list :tool-name (lambda (call) (plist-get call :name))
                   :execute-tool (lambda (call _configuration settle)
                                   (push (cons (plist-get call :id) settle) settlers))
                   :tool-result-message
                   (lambda (_job call content)
                     (list :id (plist-get call :id) :content content))
                   :execute-spawn-batch #'ende-agent--spawn-batch)))
        (ende-agent--run-tools
         job '((:id "A" :name "spawn_agent")
               (:id "B" :name "spawn_agent"))
         nil callbacks (lambda (messages) (setq results messages)))
        (should (eq (ende-job-state job) 'suspended))
        (funcall (cdr (assoc "B" settlers)) "second")
        (funcall (cdr (assoc "A" settlers)) "first")
        (should (eq (ende-job-state job) 'queued))
        (setq ende--queue-suspended nil)
        (ende-process-next)
        (should (equal (mapcar (lambda (message) (plist-get message :id))
                               results)
                       '("A" "B")))))))

(ert-deftest ende-test/private-agent-buffer-is-owned-and-invalidates-on-kill ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm nil t))
        job source target owned)
    (ende-test--with-buffer "source"
      (setq source (current-buffer))
      (setq job (ende--make-job :id 901 :type 'agent :state 'queued
                                :access-policy '(:active-tools (about_thinking))))
      (setq target (ende-agent-private-target job)
            owned (ende-job-owned-buffer job))
      (should (buffer-live-p owned))
      (should (eq (ende-target-buffer target) owned))
      (should-not (buffer-file-name owned))
      (kill-buffer owned)
      (should (eq (ende-job-state job) 'cancelled))
      (should (buffer-live-p source))
      (should-not (ende-job-owned-buffer job)))))

(ert-deftest ende-test/family-inference-template-is-clean-and-immutable ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm nil t))
        (profile '(:name remote :model "root-model" :temperature 0.4)))
    (ende-test--with-buffer "base"
      (let* ((job (ende-enqueue
                   (ende-test--dashboard-request profile)))
             (family (ende-job-family job))
             (template (ende-job-family-inference-template family)))
        (should (equal (plist-get template :model) "root-model"))
        (should-not (plist-member template :job-id))
        (setf (ende-job-profile job)
              (plist-put (ende-job-profile job) :tool-definitions '(temporary)))
        (should-not (plist-member
                     (ende-job-family-inference-template family)
                     :tool-definitions))
        (ende-abort-queue)))))

(ert-deftest ende-test/agent-profile-uses-policy-without-inheriting-request-state ()
  (let* ((family (ende--make-job-family
                  :inference-template
                  '(:name remote :model "root-model" :temperature 0.2
                    :tool-definitions (root-tools) :job-id 9)))
         (unchanged (ende-agent-profile-for-policy
                     family '(:active-tools (about_thinking))))
         (selected (ende-agent-profile-for-policy
                    family '(:provider remote :model "child-model"))))
    (should (equal (plist-get unchanged :model) "root-model"))
    (should-not (plist-get unchanged :tool-definitions))
    (should-not (plist-get unchanged :job-id))
    (should (eq (plist-get selected :name) 'remote))
    (should (equal (plist-get selected :model) "child-model"))
    (should-not (plist-get selected :tool-definitions))))

(ert-deftest ende-test/shared-loop-batches-only-adjacent-spawn-calls ()
  (ende-test--reset)
  (let ((ende--queue-suspended t)
        (batches nil)
        (executed nil)
        (results nil)
        job)
    (ende-test--with-buffer "tool source"
      (setq job
            (ende--make-job :id 911 :type 'agent :state 'running
                            :target (ende-target-create
                                     :buffer (current-buffer)
                                     :start (point-min) :end (point-min)
                                     :cursor (point-min))
                            :profile '(:tool-definitions (fixture))
                            :execution-generation 1))
      (push job ende--active-jobs)
      (let ((callbacks
             (list :tool-name (lambda (call) (plist-get call :name))
                   :execute-spawn-batch
                   (lambda (_job calls _configuration settle)
                     (push (mapcar (lambda (call) (plist-get call :id)) calls)
                           batches)
                     (funcall settle
                              (mapcar (lambda (call)
                                        (list :role "tool"
                                              :tool-call-id (plist-get call :id)
                                              :content (plist-get call :id)))
                                      calls)))
                   :execute-tool
                   (lambda (call _configuration settle)
                     (push (plist-get call :id) executed)
                     (funcall settle (plist-get call :id)))
                   :tool-result-message
                   (lambda (_job call content)
                     (list :role "tool" :tool-call-id (plist-get call :id)
                           :content content)))))
        (ende-agent--run-tools
         job '((:id "A" :name "spawn_agent")
               (:id "B" :name "spawn_agent")
               (:id "R" :name "read_file")
               (:id "C" :name "spawn_agent"))
         nil callbacks (lambda (messages) (setq results messages))))
      (should (equal (nreverse batches) '(("A" "B") ("C"))))
      (should (equal (nreverse executed) '("R")))
      (should (equal (mapcar (lambda (message)
                               (plist-get message :tool-call-id))
                             results)
                     '("A" "B" "R" "C"))))))

(ert-deftest ende-test/suspended-job-resumes-only-through-scheduler ()
  (ende-test--reset)
  (let ((handler-runs 0)
        (resumed nil)
        (ende-llm-function (ende-test--fake-llm nil t)))
    (ende-register-job-handler
     'suspend-test
     (lambda (job)
       (cl-incf handler-runs)
       (ende-job-suspend
        job (lambda ()
              (setq resumed t)
              (ende-job-finish job "done")))))
    (ende-test--with-buffer "base"
      (let ((ende--queue-suspended t)
            job)
        (setq job
              (ende-enqueue
               (list 'suspend-test
                     :target (ende-target-create
                              :buffer (current-buffer)
                              :start (point-min) :end (point-min)
                              :cursor (point-min))
                     :profile ende-test--dashboard-profile)))
        (setq ende--queue-suspended nil)
        (ende-process-next)
        (should (eq (ende-job-state job) 'suspended))
        (should-not (memq job (ende-running-jobs)))
        (should (eq (car (ende-physical-waiting-jobs)) job))
        (should (eq (car (ende-queue-suspended-jobs)) job))
        (should (equal (ende-all-live-jobs) (list job)))
        (should-not resumed)
        (let ((ende--queue-suspended t))
          (ende-job-resume-queued job)
          (should (eq (ende-job-state job) 'queued))
          (should-not resumed))
        (ende-process-next)
        (should resumed)
        (should (= handler-runs 1))
        (should (eq (ende-job-state job) 'completed))))))

(ert-deftest ende-test/queue-status-uses-active-jobs-as-its-representative ()
  (ende-test--reset)
  (let ((profile '(:name remote :model "fixture" :parallel t
                         :max-concurrency 2))
        (ende-llm-function (ende-test--fake-llm '(" done" :stall) t)))
    (ende-test--with-buffer "base"
      (let* ((first (ende-enqueue (ende-test--dashboard-request profile)))
             (second (ende-enqueue (ende-test--dashboard-request profile)))
             (status (ende-queue-status)))
        (should (equal (plist-get status :active) (ende-job-id second)))
        (should (equal (plist-get status :active-jobs)
                       (list (ende-job-id second) (ende-job-id first))))
        (ende-queue-cancel-ids (list (ende-job-id second)))
        (setq status (ende-queue-status))
        (should (equal (plist-get status :active) (ende-job-id first)))
        (should (equal (plist-get status :active-jobs)
                       (list (ende-job-id first))))
        (ende-test--complete-call (car ende-test--calls))
        (setq status (ende-queue-status))
        (should-not (plist-get status :active))
        (should-not (plist-get status :active-jobs))))))

;;;; Queue lifecycle

(ert-deftest ende-test/queue-fifo-ordering-async ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm '(" 1" " 2" " 3") t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (setq ende--queue-suspended t)
			    (ende-insert-at-cursor)
			    (ende-insert-at-cursor)
			    (ende-insert-at-cursor)
			    (setq ende--queue-suspended nil)
			    (ende-process-next)
			    (should (= (length ende-test--calls) 1))
			    (ende-test--complete-call (nth 0 ende-test--calls))
			    (ende-test--complete-call (nth 1 ende-test--calls))
			    (ende-test--complete-call (nth 2 ende-test--calls))
			    (should (string= (buffer-string) "base 1 2 3"))
			    (should (null ende-queue))
			    (should-not (ende-active-jobs)))))



(ert-deftest ende-test/abort-invalidates-before-kill ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm '(:stall "never") t
                              #'ende-test--sleep-process)))
    (ende-test--with-buffer "content"
			    (let ((job (ende-insert-at-cursor)))
			      (ende-insert-at-cursor)
			      (let ((call (car ende-test--calls)))
				(should (ende-active-jobs))
				(ende-abort-queue)
				(should (eq (ende-job-state job) 'cancelled))
				(should-not (ende-active-jobs))
				(should (null ende-queue))
				(should-not (process-live-p (plist-get call :process)))
				;; A late transport callback must be inert.
				(ende-test--callback call :on-done '(:content "orphan"))
				(should (string= (buffer-string) "content")))))))



(ert-deftest ende-test/stale-callback-cannot-settle-next-job ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm '(" A" " B") t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (setq ende--queue-suspended t)
			    (ende-insert-at-cursor)
			    (ende-insert-at-cursor)
			    (setq ende--queue-suspended nil)
			    (ende-process-next)
			    (let ((first (nth 0 ende-test--calls)))
			      (ende-test--complete-call first)
			      (let ((second (nth 1 ende-test--calls)))
				(should second)
				(should (eq (ende-job-state (car (ende-active-jobs))) 'running))
				(ende-test--callback first :on-done '(:content " stale"))
				(ende-test--callback first :on-error "stale error")
				(should (eq (ende-job-state (car (ende-active-jobs))) 'running))
				(ende-test--complete-call second)))
			    (should (string= (buffer-string) "base A B")))))



(ert-deftest ende-test/duplicate-completion-is-idempotent ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" X") t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((job (ende-insert-at-cursor))
				  (call nil))
			      (setq call (car ende-test--calls))
			      (ende-test--complete-call call)
			      (ende-test--callback call :on-done '(:content " X"))
			      (should (eq (ende-job-state job) 'completed))
			      (should (string= (buffer-string) "base X"))
			      (should-not (ende-active-jobs))))))



(ert-deftest ende-test/malformed-structured-error-cannot-wedge-queue ()
  (ende-test--reset)
  (let ((ende-llm-function
         (lambda (_messages callbacks &optional _options)
           (funcall (plist-get callbacks :on-error)
                    (ende-error-create :kind 'provider :message 123)))))
    (ende-test--with-buffer "base"
			    (let ((job (ende-insert-at-cursor)))
			      (should (eq (ende-job-state job) 'failed))
			      (should-not (ende-active-jobs))))))



(ert-deftest ende-test/synchronous-failure-backlog-is-iterative ()
  (ende-test--reset)
  (ende-test--with-buffer "base"
			  (let ((ende--queue-suspended t)
				(max-lisp-eval-depth 100))
			    (dotimes (_ 500)
			      (ende-enqueue
			       (list 'unknown
				     :target (ende-target-create
					      :buffer (current-buffer) :start 1 :end 1 :cursor 1))))
			    (setq ende--queue-suspended nil)
			    (cl-letf (((symbol-function 'message) #'ignore))
			      (ende-process-next))
			    (should-not (ende-active-jobs))
			    (should-not ende-queue))))



(ert-deftest ende-test/quit-and-cleanup-errors-still-settle-jobs ()
  (ende-test--reset)
  (let ((ende--job-handlers (copy-tree ende--job-handlers)))
    (ende-register-job-handler
     'quit-test (lambda (_job) (signal 'quit nil)))
    (ende-test--with-buffer "base"
			    (let ((job (ende-enqueue
					(list 'quit-test
					      :target (ende-target-create
						       :buffer (current-buffer)
						       :start 1 :end 1 :cursor 1)))))
			      (should (eq (ende-job-state job) 'failed))
			      (should-not (ende-active-jobs)))))
  (ende-test--reset)
  (let ((ende--job-handlers (copy-tree ende--job-handlers)))
    (ende-register-job-handler
     'cleanup-test (lambda (job) (ende-job-finish job "ok")))
    (ende-test--with-buffer "base"
			    (let* ((overlay (ende-ui-create-overlay
					     (current-buffer) 1 1 "Queued"))
				   (job nil))
			      (cl-letf (((symbol-function 'ende-ui-delete-overlay)
					 (lambda (_) (error "cleanup failed"))))
				(setq job
				      (ende-enqueue
				       (list 'cleanup-test
					     :target (ende-target-create
						      :buffer (current-buffer)
						      :start 1 :end 1 :cursor 1)
					     :overlay overlay))))
			      (should (eq (ende-job-state job) 'completed))
			      (should-not (ende-active-jobs))
			      (delete-overlay overlay)))))

;;;; Target failure paths

(ert-deftest ende-test/killed-source-buffer-cancels-only-its-jobs-once ()
  (ende-test--reset)
  (let* ((profile '(:name remote :model "fixture" :parallel t
                           :max-concurrency 2))
         (ende-llm-function (ende-test--fake-llm '(" late" " kept") t))
         (buffer (generate-new-buffer " *ende-killed*"))
         (other-buffer (generate-new-buffer " *ende-survivor*"))
         (active nil)
         (other-active nil)
         (pending nil)
         (cancel-count 0)
         (settle-count 0)
         (ende-job-event-functions
          (list (lambda (job event)
                  (when (and (eq job active)
                             (eq (plist-get event :type) :settled))
                    (setq settle-count (1+ settle-count)))))))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "base")
            (setq active
                  (ende-enqueue
                   (ende-test--dashboard-request profile))))
          (with-current-buffer other-buffer
            (insert "other")
            (setq other-active
                  (ende-enqueue
                   (ende-test--dashboard-request profile))))
          (let ((ende--queue-suspended t))
            (with-current-buffer buffer
              (setq pending
                    (ende-enqueue
                     (ende-test--dashboard-request profile)))))
          (should (eq (ende-job-state active) 'running))
          (should (eq (ende-job-state other-active) 'running))
          (should (eq (ende-job-state pending) 'queued))
          (ende-job-cancellation-set
           active (lambda () (setq cancel-count (1+ cancel-count))))
          (kill-buffer buffer)
          (should (eq (ende-job-state active) 'cancelled))
          (should (eq (ende-job-state pending) 'cancelled))
          (should (eq (ende-job-state other-active) 'running))
          (should (equal (ende-active-jobs) (list other-active)))
          (should-not (memq pending ende-queue))
          (should (= cancel-count 1))
          (should (= settle-count 1))
          (ende-test--callback (car ende-test--calls) :on-done '(:content "late"))
          (ende-test--callback (car ende-test--calls) :on-error "late error")
          (should (eq (ende-job-state active) 'cancelled))
          (should (= cancel-count 1))
          (should (= settle-count 1))
          (should-not (ende-job-result active))
          (should (eq (ende-job-state other-active) 'running))
          (ende-test--complete-call (nth 1 ende-test--calls))
          (should (eq (ende-job-state other-active) 'completed))
          (should (equal (with-current-buffer other-buffer (buffer-string))
                         "other kept")))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (when (buffer-live-p other-buffer) (kill-buffer other-buffer)))))



(ert-deftest ende-test/source-buffer-cancellation-preserves-outer-abort ()
  (ende-test--reset)
  (let ((first (generate-new-buffer " *ende-abort-first*"))
        (second (generate-new-buffer " *ende-abort-second*"))
        (observed nil)
        (first-job nil)
        (second-job nil))
    (unwind-protect
        (progn
          (with-current-buffer first
            (insert "first")
            (setq first-job
                  (ende--make-job
                   :id 1 :state 'running
                   :target (ende-target-create
                            :buffer first :start 1 :end 1 :cursor 1)
                   :cancel-function
                   (lambda ()
                     (kill-buffer second)
                     (setq observed ende--aborting)))
                  ende--source-buffer-jobs (list first-job))
            (add-hook 'kill-buffer-hook #'ende--source-buffer-killed nil t))
          (with-current-buffer second
            (insert "second")
            (setq second-job
                  (ende--make-job
                   :id 2 :state 'queued
                   :target (ende-target-create
                            :buffer second :start 1 :end 1 :cursor 1)))
            (setq ende--source-buffer-jobs (list second-job))
            (add-hook 'kill-buffer-hook #'ende--source-buffer-killed nil t))
          (setq ende--active-jobs (list first-job)
                ende-queue (list second-job)
                ende--queue-tail ende-queue)
          (ende-abort-queue)
          (should observed)
          (should-not ende--aborting)
          (should (eq (ende-job-state first-job) 'cancelled))
          (should (eq (ende-job-state second-job) 'cancelled)))
      (when (buffer-live-p first) (kill-buffer first))
      (when (buffer-live-p second) (kill-buffer second)))))



(ert-deftest ende-test/read-only-target-settles-job ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" X") t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((job (ende-insert-at-cursor))
				  (call nil))
			      (setq call (car ende-test--calls))
			      (setq buffer-read-only t)
			      (ende-test--complete-call call)
			      (should (eq (ende-job-state job) 'failed))
			      (should-not (ende-active-jobs))
			      (should (string= (buffer-string) "base"))))))



(ert-deftest ende-test/narrowed-target-settles-job ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" X") t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((job (ende-insert-at-cursor))
				  (call nil))
			      (setq call (car ende-test--calls))
			      (narrow-to-region (point-min) (1- (point-max)))
			      (ende-test--complete-call call)
			      (should (eq (ende-job-state job) 'failed))
			      (should-not (ende-active-jobs))))))



(ert-deftest ende-test/widening-does-not-expand-queued-context ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" X") t)))
    (ende-test--with-buffer "SECRET safe SECRET"
			    (narrow-to-region 8 12)
			    (goto-char 10)
			    (setq ende--queue-suspended t)
			    (ende-insert-at-cursor)
			    (widen)
			    (setq ende--queue-suspended nil)
			    (ende-process-next)
			    (let ((user-message
				   (cdr (cadr (plist-get (car ende-test--calls) :messages)))))
			      (should (string-match-p "sa<CURSOR>fe" user-message))
			      (should-not (string-match-p "SECRET" user-message))))))



(ert-deftest ende-test/modification-hook-settles-job ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" X") t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((job (ende-insert-at-cursor))
				  (call nil))
			      (setq call (car ende-test--calls))
			      (add-hook 'before-change-functions
					(lambda (&rest _) (error "blocked")) nil t)
			      (ende-test--complete-call call)
			      (should (eq (ende-job-state job) 'failed))
			      (should (string= (buffer-string) "base"))
			      (should-not (ende-active-jobs))))))

(ert-deftest ende-test/same-endpoint-preserves-fifo ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("A" "B") t)))
    (ende-test--with-buffer "alpha"
			    (ende-test--select 1 6 6)
			    (setq ende--queue-suspended t)
			    (ende-insert-at-cursor)
			    (ende-insert-at-cursor)
			    (setq ende--queue-suspended nil)
			    (ende-process-next)
			    (ende-test--complete-call (nth 0 ende-test--calls))
			    (ende-test--complete-call (nth 1 ende-test--calls))
			    (should (string= (buffer-string) "alpha\nA\nB")))))

;;;; Profiles, history, and transport command safety

(ert-deftest ende-test/profile-is-snapshotted-at-enqueue ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(:stall) t))
        (ende-provider-endpoint "http://one.example/v1")
        (ende-provider-profiles '((local :temperature 0.2))))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((job (ende-insert-at-cursor)))
			      (setq ende-provider-endpoint "http://two.example/v1"
				    ende-provider-profiles
				    '((local :temperature 0.9
					     :controls ((:temperature :path ("changed"))))))
			      (should (string= (plist-get (ende-job-profile job) :url)
					       "http://one.example/v1"))
			      (should (= (plist-get (ende-job-profile job) :temperature) 0.2))
			      (should (equal (plist-get (ende-job-profile job) :controls)
					     '((:temperature :path ("temperature")))))))))

(provide 'ende-queue-test)

;;; ende-queue-test.el ends here
