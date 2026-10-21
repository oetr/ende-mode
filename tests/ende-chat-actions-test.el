;;; ende-chat-actions-test.el --- Chat action tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Coverage for ask and compact actions and asynchronous lifecycle.

;;; Code:

(require 'ert)
(require 'ende)
(require 'ende-test-helpers)
(require 'cl-lib)
(require 'ende-chat-test-helpers)



(defun ende-test--chat-response-count ()
  "Return the number of generated Agent: Reply headlines in the buffer."
  (cl-count-if
   (lambda (headline)
     (string= (org-element-property :raw-value headline) "Agent: Reply"))
   (org-element-map (org-element-parse-buffer) 'headline #'identity)))



(defun ende-test--chat-response-has-separator-p (headline)
  "Return non-nil when HEADLINE has a blank line before it."
  (save-excursion
    (goto-char (org-element-property :begin headline))
    (forward-line -1)
    (looking-at-p "^[[:space:]]*$")))

(ert-deftest ende-test/direct-chat-request-uses-buffer-selection ()
  (ende-test--reset)
  (let ((ende-model-selection-scope 'buffer)
        (ende-model-selection--last-used nil)
        (ende-model-selection--buffer-selections
         (make-hash-table :test #'equal))
        (ende-model-selection--shared-control-preferences
         (make-hash-table :test #'equal))
        (ende-model-selection--buffer-control-preferences
         (make-hash-table :test #'equal))
        (ende-llm-function (ende-test--fake-llm '("Reply"))))
    (unwind-protect
        (ende-test--with-buffer "* Root\n** Question\nAsk\n"
				(org-mode)
				(ende-test--chat-goto "** Question")
				(ende-model-selection-save
				 '(:provider openrouter :model "chat-model"))
				(let ((job (ende-ask)))
				  (should (eq (plist-get (ende-job-profile job) :name) 'openrouter))
				  (should (equal (plist-get (ende-job-profile job) :model)
						 "chat-model"))))
      (ende-test--reset))))

;;;; Org-backed chat

(ert-deftest ende-test/chat-staging-buffer-is-read-only-but-appendable ()
  (ende-test--reset)
  (let* ((job (ende--make-job :instruction '(:request-id "readonly-staging")))
         (buffer (ende-chat--staging-buffer job t)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (should (derived-mode-p 'org-mode))
            (should font-lock-mode)
            (should buffer-read-only)
            (should-error (insert "user edit") :type 'buffer-read-only))
          (ende-chat--staging-append job "first")
          (ende-chat--staging-append job " second")
          (ende-chat--staging-append
           job "\n** Agent: Work\n:PROPERTIES:\n:ENDE_CHAT_WORK: t\n:END:\n\n")
          (ende-chat--staging-append
           job "*** Section\nbody\n**** Nested\nnested body\n*** Another\nmore body\n")
          (with-current-buffer buffer
            (should buffer-read-only)
            (should (string-match-p "first second" (buffer-string)))
            (should (= (how-many org-heading-regexp (point-min) (point-max)) 4))
            (goto-char (point-min))
            (search-forward "Agent: Work")
            (should-not (org-invisible-p (line-beginning-position)))
            (dolist (text '("body" "nested body" "more body"))
              (goto-char (point-min))
              (search-forward text)
              (should (org-invisible-p (point))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ende-test/chat-staging-append-preserves-current-point ()
  (ende-test--reset)
  (let* ((job (ende--make-job
               :instruction '(:request-id "staging-point-neutral-current")))
         (buffer (ende-chat--staging-buffer job t)))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) buffer)
          (ende-chat--staging-append job "First section\nSecond section\n")
          (with-current-buffer buffer
            (goto-char (+ (point-min) 5))
            (let ((previous-point (point)))
              (ende-chat--staging-append job "Third section\n")
              (should (= (point) previous-point)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ende-test/chat-staging-append-preserves-nonselected-window-position ()
  (ende-test--reset)
  (let* ((job (ende--make-job
               :instruction '(:request-id "staging-point-neutral-window")))
         (buffer (ende-chat--staging-buffer job t))
         (other-buffer (generate-new-buffer "*staging-window-other*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (ende-chat--staging-append
           job "First section\nSecond section\nThird section\n")
          (let* ((work-window (selected-window))
                 (other-window (split-window-right)))
            (set-window-buffer work-window buffer)
            (set-window-buffer other-window other-buffer)
            (with-selected-window work-window
              (goto-char (+ (point-min) 5))
              (set-window-start work-window (point-min) t))
            (let ((previous-point (window-point work-window))
                  (previous-start (window-start work-window)))
              (select-window other-window)
              (ende-chat--staging-append job "Fourth section\n")
              (should (= (window-point work-window) previous-point))
              (should (= (window-start work-window) previous-start)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (buffer-live-p other-buffer)
        (kill-buffer other-buffer)))))

(ert-deftest ende-test/chat-completion-reduces-whole-transcript-parsing ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("Answer") t)))
    (ende-test--with-buffer
     "* Root\n** Question\nAsk\n"
     (org-mode)
     (ende-test--chat-goto "** Question")
     (let* ((job (ende-ask))
            (target (current-buffer))
            (whole-transcript-parses 0)
            (scoped-target-parses 0)
            (other-buffer-parses 0)
            (parse-buffer (symbol-function 'org-element-parse-buffer)))
       (cl-letf (((symbol-function 'org-element-parse-buffer)
                  (lambda (&rest arguments)
                    (if (eq (current-buffer) target)
                        (if (and (= (point-min) 1)
                                 (= (point-max) (1+ (buffer-size))))
                            (cl-incf whole-transcript-parses)
                          (cl-incf scoped-target-parses))
                      (cl-incf other-buffer-parses))
                    (apply parse-buffer arguments))))
         (ende-test--complete-call (car ende-test--calls)))
       (should (eq (ende-job-state job) 'completed))
       (should (= whole-transcript-parses 0))
       (should (> scoped-target-parses 0))
       (should (> other-buffer-parses 0))))))

(ert-deftest ende-test/chat-enqueue-avoids-whole-transcript-parsing ()
  (ende-test--reset)
  (let ((ende--queue-suspended t))
    (ende-test--with-buffer
     (concat "* Root\n"
             "** Question\nAsk\n"
             "** Existing agent section\n"
             ":PROPERTIES:\n:ENDE_ROLE: assistant\n:END:\n"
             "Keep this drawer.\n")
     (org-mode)
     (ende-test--chat-goto "** Question")
     (let* ((document (ende-chat--build-document))
            (question (ende-chat--headline-at-point document))
            (target (current-buffer))
            (whole-transcript-parses 0)
            (scoped-target-parses 0)
            (parse-buffer (symbol-function 'org-element-parse-buffer)))
       (cl-letf (((symbol-function 'org-element-parse-buffer)
                  (lambda (&rest arguments)
                    (when (eq (current-buffer) target)
                      (if (and (= (point-min) 1)
                               (= (point-max) (1+ (buffer-size))))
                          (cl-incf whole-transcript-parses)
                        (cl-incf scoped-target-parses)))
                    (apply parse-buffer arguments))))
         (ende-chat--enqueue document question nil))
       (should (= whole-transcript-parses 0))
       (should (> scoped-target-parses 0))))))

(ert-deftest ende-test/chat-completion-survives-unrelated-edit ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("Answer") t)))
    (ende-test--with-buffer
     "* Root\n** Question\nAsk\n* Notes\nKeep this note.\n"
     (org-mode)
     (ende-test--chat-goto "** Question")
     (let* ((job (ende-ask))
            (instruction (ende-job-instruction job))
            (target (current-buffer))
            (whole-transcript-parses 0)
            (parse-buffer (symbol-function 'org-element-parse-buffer)))
       (goto-char (point-max))
       (insert "Added while the request was running.\n")
       (cl-letf (((symbol-function 'org-element-parse-buffer)
                  (lambda (&rest arguments)
                    (when (and (eq (current-buffer) target)
                               (= (point-min) 1)
                               (= (point-max) (1+ (buffer-size))))
                      (cl-incf whole-transcript-parses))
                    (apply parse-buffer arguments))))
         (ende-test--complete-call (car ende-test--calls)))
       (should (eq (ende-job-state job) 'completed))
       (should (= whole-transcript-parses 0))
       (should (string-match-p
                "Keep this note.\nAdded while the request was running."
                (buffer-string)))
       (let* ((document (ende-chat--build-document))
              (reply
               (cl-find-if
                (lambda (headline)
                  (and (equal (org-element-property :raw-value headline)
                              "Agent: Reply")
                       (equal (ende-chat--property document headline :request-id)
                              (plist-get instruction :request-id))))
                (ende-chat--document-headlines document))))
         (should reply)
         (should (equal (ende-chat--property document reply :reply-to)
                        (plist-get instruction :question-id)))
         (should (string-match-p
                  "Answer"
                  (buffer-substring-no-properties
                   (org-element-property :begin reply)
                   (org-element-property :end reply)))))))))

(ert-deftest ende-test/chat-question-marker-survives-top-level-insertion ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("Answer") t)))
    (ende-test--with-buffer
     "* Question\nAsk this.\n"
     (org-mode)
     (ende-test--chat-goto "* Question")
     (let* ((job (ende-ask))
            (instruction (ende-job-instruction job))
            (question-id (plist-get instruction :question-id))
            (request-id (plist-get instruction :request-id)))
       (goto-char (point-min))
       (insert "* Inserted section\nDo not modify this.\n\n")
       (ende-test--complete-call (car ende-test--calls))
       (should (eq (ende-job-state job) 'completed))
       (let* ((document (ende-chat--build-document))
              (question (ende-chat--find-by-property
                         document ende-chat-id-property question-id))
              (reply (ende-chat--find-by-property
                      document ende-chat-request-id-property request-id))
              (inserted (cl-find-if
                         (lambda (headline)
                           (equal (org-element-property :raw-value headline)
                                  "Inserted section"))
                         (ende-chat--document-headlines document))))
         (should question)
         (should reply)
         (should inserted)
         (should (equal (ende-chat--property document reply :reply-to)
                        question-id))
         (should (string-match-p "Do not modify this."
                                 (ende-chat--section-text document inserted)))
         (should (equal (org-element-property :raw-value question)
                        "Question")))))))

(ert-deftest ende-test/chat-question-marker-falls-back-to-durable-id-after-move ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("Answer") t)))
    (ende-test--with-buffer
     "* Root\n** Other\nKeep this section.\n** Question\nAsk this.\n** Next\nKeep this untouched.\n"
     (org-mode)
     (ende-test--chat-goto "** Question")
     (let* ((job (ende-ask))
            (instruction (ende-job-instruction job))
            (question-id (plist-get instruction :question-id))
            (request-id (plist-get instruction :request-id)))
       (let* ((document (ende-chat--build-document))
              (question (ende-chat--find-by-property
                         document ende-chat-id-property question-id))
              (begin (org-element-property :begin question))
              (end (save-excursion
                     (goto-char begin)
                     (org-end-of-subtree t t)
                     (point)))
              (subtree (delete-and-extract-region begin end)))
         (goto-char (point-min))
         (re-search-forward "^\\*\\* Other[ \t]*$")
         (beginning-of-line)
         (insert subtree))
       (ende-test--complete-call (car ende-test--calls))
       (should (eq (ende-job-state job) 'completed))
       (let* ((document (ende-chat--build-document))
              (question (ende-chat--find-by-property
                         document ende-chat-id-property question-id))
              (reply (ende-chat--find-by-property
                      document ende-chat-request-id-property request-id)))
         (should question)
         (should reply)
         (should (equal (ende-chat--property document reply :reply-to)
                        question-id))
         (should (string-match-p "Keep this section." (buffer-string)))
         (should (string-match-p "Keep this untouched." (buffer-string)))
         (should
          (< (org-element-property :begin question)
             (org-element-property :begin reply))))))))

(ert-deftest ende-test/chat-compaction-survives-unrelated-edit ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("Summary") t)))
    (ende-test--with-buffer
     "* Topic\nOriginal history.\n** Instructions\nKeep decisions.\n** Next\nContinue.\n* Notes\nPreserve this note.\n"
     (org-mode)
     (ende-test--chat-goto "** Instructions")
     (let* ((job (ende-compact))
            (request-id (plist-get (ende-job-instruction job) :request-id)))
       (ende-test--chat-goto "* Notes")
       (org-end-of-meta-data t)
       (insert "Added while compaction was running.\n")
       (ende-test--complete-call (car ende-test--calls))
       (should (eq (ende-job-state job) 'completed))
       (should (string-match-p "Added while compaction was running"
                               (buffer-string)))
       (let* ((document (ende-chat--build-document))
              (fence (ende-chat--find-by-property
                      document ende-chat-request-id-property request-id)))
         (should (ende-chat--context-fence-p document fence))
         (should (string-match-p "Summary"
                                 (buffer-substring-no-properties
                                  (org-element-property :begin fence)
                                  (org-element-property :end fence)))))))))

(ert-deftest ende-test/chat-completion-rejects-an-edited-destination ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("Generated answer") t)))
    (ende-test--with-buffer
     "* Root\n** Question\nAsk\n"
     (org-mode)
     (ende-test--chat-goto "** Question")
     (let ((job (ende-ask)))
       (pcase-let ((`(,_document ,placeholder)
                    (ende-chat--resolve-request-destination job)))
         (goto-char (org-element-property :begin placeholder))
         (org-edit-headline "User-edited destination"))
       (ende-test--complete-call (car ende-test--calls))
       (should (eq (ende-job-state job) 'failed))
       (should (string-match-p "User-edited destination" (buffer-string)))
       (should-not (string-match-p "Generated answer" (buffer-string)))
       (should (string-match-p "destination was edited or moved"
                               (buffer-string)))))))



(ert-deftest ende-test/chat-and-compaction-reject-invalid-destinations ()
  (dolist (compact '(nil t))
    (dolist (mutation '(title role visibility state parent level delete))
      (ert-info ((format "%s destination mutation: %s"
                         (if compact "compaction" "chat") mutation))
        (ende-test--reset)
        (let ((ende-llm-function
               (ende-test--fake-llm
                (list (if compact "Generated summary" "Generated answer")) t)))
          (ende-test--with-buffer
           "* Topic\n** Question\nKeep these instructions.\n** Next\nContinue.\n"
           (org-mode)
           (ende-test--chat-goto "** Question")
           (let* ((job (if compact (ende-compact) (ende-ask)))
                  (placeholder
                   (cadr (ende-chat--resolve-request-destination job))))
             (goto-char (org-element-property :begin placeholder))
             (pcase mutation
               ('title (org-edit-headline "User-edited destination"))
               ('role (org-entry-put nil ende-chat-role-property "user"))
               ('visibility
                (org-entry-put nil ende-chat-visibility-property "normal"))
               ('state (org-entry-put nil ende-chat-state-property "failed"))
               ('parent (org-demote-subtree))
               ('level (org-promote-subtree))
               ('delete
                (delete-region (org-element-property :begin placeholder)
                               (org-element-property :end placeholder))))
             (ende-test--complete-call (car ende-test--calls))
             (should (eq (ende-job-state job) 'failed))
             (should-not (string-match-p "Generated answer" (buffer-string)))
             (should-not (string-match-p "Generated summary" (buffer-string)))
             (should (string-match-p "Keep these instructions" (buffer-string))))))))))

(ert-deftest ende-test/concurrent-chat-and-compaction-stay-in-their-buffers ()
  (ende-test--reset)
  (let ((ende-provider-profiles '((local :parallel t :max-concurrency 2)))
        (ende-llm-function
         (ende-test--fake-llm '("Compaction-only summary" "Chat-only answer") t))
        (chat-buffer (generate-new-buffer " *ende-chat-live-location*"))
        (compact-buffer (generate-new-buffer " *ende-compact-live-location*")))
    (unwind-protect
        (let (chat-job compact-job)
          (with-current-buffer compact-buffer
            (org-mode)
            (insert "* Topic\nEarlier history.\n** Instructions\nRetain decisions.\n")
            (ende-test--chat-goto "** Instructions")
            (setq compact-job (ende-compact)))
          (with-current-buffer chat-buffer
            (org-mode)
            (insert "* Topic\n** Question\nAnswer this.\n")
            (ende-test--chat-goto "** Question")
            (setq chat-job (ende-ask)))
          (should (eq (ende-job-state compact-job) 'running))
          (should (eq (ende-job-state chat-job) 'running))
          (ende-test--complete-call (nth 1 ende-test--calls))
          (ende-test--complete-call (nth 0 ende-test--calls))
          (should (eq (ende-job-state compact-job) 'completed))
          (should (eq (ende-job-state chat-job) 'completed))
          (with-current-buffer compact-buffer
            (should (string-match-p "Compaction-only summary" (buffer-string)))
            (should-not (string-match-p "Chat-only answer" (buffer-string))))
          (with-current-buffer chat-buffer
            (should (string-match-p "Chat-only answer" (buffer-string)))
            (should-not (string-match-p "Compaction-only summary"
                                        (buffer-string)))))
      (ende-test--reset)
      (when (buffer-live-p chat-buffer) (kill-buffer chat-buffer))
      (when (buffer-live-p compact-buffer) (kill-buffer compact-buffer)))))

(ert-deftest ende-test/chat-unrelated-edits-during-async-tool-round-are-safe ()
  (ende-test--reset)
  (let* ((call '(:id "deferred-call" :name "about_thinking"
                 :arguments "{\"title\":\"Checking\",\"status\":\"Waiting\"}"))
         (ende-llm-function
          (ende-test--fake-llm
           (list (list :tool-calls (list call)) "Answer after the tool") t))
         deferred-tool-completion)
    (ende-test--with-buffer
     "* Topic\n** Question\nInspect.\n** Notes\nPreserve this note.\n"
     (org-mode)
     (ende-test--chat-goto "** Question")
     (let* ((job (ende-ask))
            (request-id (plist-get (ende-job-instruction job) :request-id)))
       (cl-letf (((symbol-function 'ende-chat--tool-response-async)
                  (lambda (_call _configuration callback)
                    (setq deferred-tool-completion callback))))
         (ende-test--complete-call (car ende-test--calls))
         (should (eq (ende-job-state job) 'running))
         (should (functionp deferred-tool-completion))
         (ende-test--chat-goto "** Notes")
         (org-end-of-meta-data t)
         (insert "Added during the asynchronous tool round.\n")
         (funcall deferred-tool-completion "{\\\"ok\\\":true}")
         (let ((staged (ende-chat--staged-text job)))
           (should staged)
           (should (string-match-p "Agent: Work" staged))
           (should (string-match-p "about_thinking" staged))
           (should (string-match-p "ENDE_TOOL_KIND: result" staged))
           (should (string-match-p "Agent: Reply" (buffer-string)))
           (should-not (string-match-p "Agent: Work" (buffer-string)))
           (should-not (string-match-p "ENDE_TOOL_KIND" (buffer-string)))))
       (should (= (length ende-test--calls) 2))
       (ende-test--complete-call (nth 1 ende-test--calls))
       (should (eq (ende-job-state job) 'completed))
       (should (string-match-p "Added during the asynchronous tool round"
                               (buffer-string)))
       (let* ((document (ende-chat--build-document))
              (work (ende-chat--request-work-section document request-id))
              (children (ende-chat--children document work))
              (call-node (cl-find-if
                          (lambda (node)
                            (ende-chat--tool-call-protocol-p document node))
                          children))
              (result-node (cl-find-if
                            (lambda (node)
                              (ende-chat--tool-result-protocol-p document node))
                            children))
              (reply (cl-find-if
                      (lambda (node)
                        (and (equal (org-element-property :raw-value node)
                                    "Agent: Reply")
                             (equal (ende-chat--property document node :request-id)
                                    request-id)
                             (null (ende-chat--work-ancestor document node))))
                      (ende-chat--document-headlines document)))
              (call-pos (org-element-property :begin call-node))
              (result-pos (org-element-property :begin result-node))
              (reply-pos (org-element-property :begin reply)))
         (should work)
         (should call-node)
         (should result-node)
         (should reply)
         (should (< call-pos result-pos reply-pos))
          (should (eq (ende-chat--parent document call-node) work))
          (should (eq (ende-chat--parent document result-node) work)))))))

(ert-deftest ende-test/chat-recursively-delegates-with-chat-owned-results ()
  (ende-test--reset)
  (let* ((spawn-call (lambda (id task)
                       (list :id id :name "spawn_agent"
                             :arguments (json-encode `((task . ,task))))))
         (ende-llm-function
          (ende-test--fake-llm
           (list (list :tool-calls
                       (list (funcall spawn-call "root-child" "child task")))
                 (list :tool-calls
                       (list (funcall spawn-call "child-grandchild"
                                      "grandchild task")))
                 "Grandchild final only."
                 "Child final only."
                 "Root final answer.") t))
         root child grandchild)
    (ende-test--with-buffer
     (concat (ende-test--chat-config-block
              '((tools about_thinking spawn_agent)
                (agent-config worker
                              (tools about_thinking spawn_agent)
                              (spawn-agent-config worker))
                (spawn-agent-config worker)))
             "* Conversation\n** Question\nRoot confidential context.\n")
     (org-mode)
     (ende-test--chat-goto "** Question")
     (setq root (ende-ask))
     (should (= (length ende-test--calls) 1))
     (ende-test--complete-call (nth 0 ende-test--calls))
     (setq child (car (ende-job-children root)))
     (should (eq (ende-job-state root) 'suspended))
     (should (= (length ende-test--calls) 2))
     (should (string-match-p
              "child task"
              (ende-test--chat-context-text
               (plist-get (nth 1 ende-test--calls) :messages))))
     (should-not (string-match-p
                  "Root confidential context"
                  (ende-test--chat-context-text
                   (plist-get (nth 1 ende-test--calls) :messages))))
     (ende-test--complete-call (nth 1 ende-test--calls))
     (setq grandchild (car (ende-job-children child)))
     (should (eq (ende-job-state child) 'suspended))
     (should (= (length ende-test--calls) 3))
     (ende-test--complete-call (nth 2 ende-test--calls))
     (should (equal (ende-job-result grandchild) "Grandchild final only."))
     (should (= (length ende-test--calls) 4))
     (ende-test--complete-call (nth 3 ende-test--calls))
     (should (equal (ende-job-result child) "Child final only."))
     (should (= (length ende-test--calls) 5))
     (ende-test--complete-call (nth 4 ende-test--calls))
     (should (equal (plist-get (ende-job-result root) :content)
                    "Root final answer."))
     (should (eq (ende-job-state root) 'completed))
     (should (eq (ende-job-state child) 'completed))
     (should (eq (ende-job-state grandchild) 'completed))
     (should (string-match-p "Root final answer" (buffer-string)))
     (should-not (string-match-p "Grandchild final only" (buffer-string)))
     (should-not (ende-queue-family-markers)))))

(ert-deftest ende-test/chat-batches-sibling-agents-and-stores-results-in-call-order ()
  (ende-test--reset)
  (let* ((spawn-call (lambda (id task)
                       (list :id id :name "spawn_agent"
                             :arguments (json-encode `((task . ,task))))))
         (first-call (funcall spawn-call "sibling-a" "task A"))
         (second-call (funcall spawn-call "sibling-b" "task B"))
         (ende-llm-function
          (ende-test--fake-llm
           (list (list :tool-calls (list first-call second-call))
                 "Sibling A final."
                 "Sibling B final."
                 "Root batch answer.") t))
         (profile '(:name remote :model "fixture" :parallel t
                    :max-concurrency 2))
         root children)
    (ende-test--with-buffer
     (concat (ende-test--chat-config-block
              '((tools about_thinking spawn_agent)
                (agent-config worker (tools about_thinking))
                (spawn-agent-config worker)))
             "* Conversation\n** Question\nDelegate both checks.\n")
     (org-mode)
     (ende-test--chat-goto "** Question")
     (setq root (ende-ask profile))
     (ende-test--complete-call (nth 0 ende-test--calls))
     (setq children (copy-sequence (ende-job-children root)))
     (should (= (length children) 2))
     (should (equal (mapcar #'ende-job-schedule-path children) '((1) (2))))
     (should (eq (ende-job-state root) 'suspended))
     (should (= (length ende-test--calls) 3))
     (ende-test--complete-call (nth 2 ende-test--calls))
     (should (eq (ende-job-state root) 'suspended))
     (should (= (length ende-test--calls) 3))
     (ende-test--complete-call (nth 1 ende-test--calls))
     (should (= (length ende-test--calls) 4))
     (let* ((messages (ende-test--chat-protocol-messages
                       (plist-get (nth 3 ende-test--calls) :messages)))
            (results (cl-remove-if-not
                      (lambda (message)
                        (equal (plist-get message :role) "tool"))
                      messages)))
       (should (equal (mapcar (lambda (message)
                                (plist-get message :tool-call-id))
                              results)
                      '("sibling-a" "sibling-b")))
       (should (string-match-p "Sibling A final"
                               (plist-get (car results) :content)))
       (should (string-match-p "Sibling B final"
                               (plist-get (cadr results) :content))))
     (ende-test--complete-call (nth 3 ende-test--calls))
     (should (eq (ende-job-state root) 'completed))
     (should (string-match-p "Root batch answer" (buffer-string)))
     (should-not (string-match-p "Sibling A final" (buffer-string)))
     (should-not (string-match-p "Sibling B final" (buffer-string))))))

(ert-deftest ende-test/chat-cancelling-suspended-batch-invalidates-late-children ()
  (ende-test--reset)
  (let* ((spawn-call (lambda (id task)
                       (list :id id :name "spawn_agent"
                             :arguments (json-encode `((task . ,task))))))
         (ende-llm-function
          (ende-test--fake-llm
           (list (list :tool-calls
                       (list (funcall spawn-call "cancel-a" "task A")
                             (funcall spawn-call "cancel-b" "task B")))
                 :stall :stall) t))
         (profile '(:name remote :model "fixture" :parallel t
                    :max-concurrency 2))
         (late-count 0)
         root children child-buffers)
    (ende-test--with-buffer
     (concat (ende-test--chat-config-block
              '((tools about_thinking spawn_agent)
                (agent-config worker (tools about_thinking))
                (spawn-agent-config worker)))
             "* Conversation\n** Question\nDelegate and await.\n")
     (org-mode)
     (ende-test--chat-goto "** Question")
     (setq root (ende-ask profile))
     (ende-test--complete-call (nth 0 ende-test--calls))
     (setq children (copy-sequence (ende-job-children root))
           child-buffers (mapcar #'ende-job-owned-buffer children))
     (should (eq (ende-job-state root) 'suspended))
     (should (equal (mapcar #'ende-job-state children) '(running running)))
     (should (= (length ende-test--calls) 3))
     (ende-queue-cancel-ids (list (ende-job-id root)))
     (should (eq (ende-job-state root) 'cancelled))
     (should (equal (mapcar #'ende-job-state children)
                    '(cancelled cancelled)))
     (should-not (ende-active-jobs))
     (should-not (ende-pending-jobs))
     (should-not (ende-queue-family-markers))
     (should (cl-every (lambda (buffer) (not (buffer-live-p buffer)))
                       child-buffers))
     (dolist (call (cdr ende-test--calls))
       (ende-test--callback call :on-done '(:content "late child answer"))
       (cl-incf late-count))
     (should (= late-count 2))
     (should (= (length ende-test--calls) 3))
     (should-not (string-match-p "late child answer" (buffer-string))))))

(ert-deftest ende-test/chat-intermediate-tool-round-ignores-stale-destination-marker ()
  (ende-test--reset)
  (let* ((directory (make-temp-file "ende-chat-staging-rounds-" t))
         (user-emacs-directory directory)
         (first '(:id "round-one" :name "about_thinking"
                  :arguments "{\"title\":\"First\",\"status\":\"Done\"}"))
         (second '(:id "round-two" :name "about_thinking"
                   :arguments "{\"title\":\"Second\",\"status\":\"Done\"}"))
         (ende-llm-function
          (ende-test--fake-llm
           (list (list :tool-calls (list first))
                 (list :tool-calls (list second))
                 "Finished after both rounds") t)))
    (unwind-protect
        (ende-test--with-buffer
         "* Root\n** Question\nInvestigate.\n"
         (org-mode)
         (ende-test--chat-goto "** Question")
         (let ((job (ende-ask)))
           (ende-test--complete-call (nth 0 ende-test--calls))
           (should (= (length ende-test--calls) 2))
           ;; Insert at the original destination marker.  It now names this
           ;; unrelated heading while the pending request remains intact below.
           (goto-char (ende-target-cursor-marker (ende-job-target job)))
           (insert "** Notes\nInserted ahead of the pending request.\n\n")
           (ende-test--complete-call (nth 1 ende-test--calls))
           (should (eq (ende-job-state job) 'running))
           (should (= (length ende-test--calls) 3))
           (should (string-match-p "Second"
                                   (ende-chat--staged-text job)))
           (should-not (string-match-p "Agent: Work" (buffer-string)))
           (should-not (string-match-p "ENDE_TOOL_KIND" (buffer-string)))
           (ende-job-fail job "test cleanup")
           (should (eq (ende-job-state job) 'failed))))
      (delete-directory directory t))))

(ert-deftest ende-test/chat-failure-materializes-by-durable-request-id ()
  (ende-test--reset)
  (let* ((directory (make-temp-file "ende-chat-staged-failure-id-" t))
         (user-emacs-directory directory)
         (call '(:id "durable-failure-call" :name "about_thinking"
                 :arguments "{\"title\":\"Staged\",\"status\":\"Done\"}"))
         (ende-llm-function
          (ende-test--fake-llm
           (list (list :tool-calls (list call)) "Unused final") t)))
    (unwind-protect
        (ende-test--with-buffer
         "* Root\n** Question\nAsk\n"
         (org-mode)
         (ende-test--chat-goto "** Question")
         (let* ((job (ende-ask))
                (request-id (plist-get (ende-job-instruction job) :request-id)))
           (ende-test--complete-call (car ende-test--calls))
           (set-marker (ende-target-cursor-marker (ende-job-target job))
                       (point-min) (current-buffer))
           (ende--handle-llm-event
            job '(:type :reasoning-summary :item "failure"
                  :text "Failure context"))
           (ende-job-fail job "Injected after the tool round")
           (should (eq (ende-job-state job) 'failed))
           (should (string-match-p "Agent: Work" (buffer-string)))
           (should (string-match-p "Injected after the tool round"
                                   (buffer-string)))
           (let* ((document (ende-chat--build-document))
                  (reply (ende-chat--request-state-headline
                          document request-id "failed"))
                  (work (ende-chat--request-work-section document request-id)))
             (should reply)
             (let ((summary
                    (cl-find-if
                     (lambda (headline)
                       (equal (ende-chat--property document headline :visibility)
                              "reasoning_summary"))
                     (ende-chat--children document work))))
               (should summary)
               (should (equal (org-element-property :raw-value summary)
                              "Agent: Failure context"))
               (should (string-blank-p
                        (ende-chat--protocol-body-string summary))))
             (should (equal (ende-chat--property document reply :reply-to)
                            (plist-get (ende-job-instruction job)
                                       :question-id))))))
      (delete-directory directory t))))

(ert-deftest ende-test/chat-failed-materialization-retains-current-recovery ()
  (ende-test--reset)
  (let* ((directory (make-temp-file "ende-chat-staged-retain-" t))
         (user-emacs-directory directory)
         (call '(:id "retained-work-call" :name "about_thinking"
                 :arguments "{\"title\":\"Durable\",\"status\":\"Done\"}"))
         (ende-llm-function
          (ende-test--fake-llm
           (list (list :tool-calls (list call)) "Unused final") t)))
    (unwind-protect
        (ende-test--with-buffer
         "* Root\n** Question\nAsk\n* Other\nKeep this section.\n"
         (org-mode)
         (ende-test--chat-goto "** Question")
         (let* ((job (ende-ask))
                (request-id (plist-get (ende-job-instruction job) :request-id)))
           (ende-test--complete-call (car ende-test--calls))
           (let* ((file (plist-get (ende-job-instruction job) :recovery-file))
                  (document (ende-chat--build-document))
                  (pending (ende-chat--request-state-headline
                            document request-id "pending")))
             (delete-region (org-element-property :begin pending)
                            (org-element-property :end pending))
             (let ((before (buffer-string)))
               (ende-job-fail job "Destination was removed")
               (should (eq (ende-job-state job) 'failed))
               (should (equal before (buffer-string)))
               (should (string-match-p "Keep this section" (buffer-string)))
               (should (file-exists-p file))
               (let ((recovery (ende-chat--read-recovery-file file)))
                 (should (string-match-p "Agent: Work" (cadr recovery)))
                 (should (string-match-p "Durable"
                                         (cadr recovery))))))))
      (delete-directory directory t))))

(ert-deftest ende-test/chat-staging-buffer-creation-remains-lazy ()
  (let ((job (ende--make-job :instruction '(:request-id "lazy-staging"))))
    (should-not (ende-chat--staging-buffer job))
    (should-not (plist-get (ende-job-instruction job) :staging-buffer))))

(ert-deftest ende-test/chat-staging-keybinding-is-buffer-local ()
  (let ((org-buffer (generate-new-buffer " *ende-chat-org-test*"))
        (job (ende--make-job :instruction '(:request-id "keymap-test")))
        staging-buffer)
    (unwind-protect
        (progn
          (with-current-buffer org-buffer (org-mode))
          (let ((org-q (with-current-buffer org-buffer (key-binding (kbd "q")))))
            (setq staging-buffer (ende-chat--staging-buffer job t))
            (should (eq (with-current-buffer org-buffer
                          (key-binding (kbd "q")))
                        org-q))
            (should (eq (with-current-buffer staging-buffer
                          (key-binding (kbd "q")))
                        #'ende-chat--staging-quit))))
      (when (buffer-live-p staging-buffer) (kill-buffer staging-buffer))
      (kill-buffer org-buffer))))

(ert-deftest ende-test/chat-staging-does-not-copy-to-kill-ring ()
  (let ((buffer (generate-new-buffer " *ende-chat-staging-test*"))
        (kill-ring '("previous entry"))
        (kill-ring-yank-pointer nil)
        clipboard-text
        (interprogram-cut-function
         (lambda (text) (setq clipboard-text text))))
    (unwind-protect
        (progn
          (with-current-buffer buffer (org-mode))
          (ende-chat--staging-insert buffer "* Work\n** Summary\n")
          (should (equal kill-ring '("previous entry")))
          (should-not clipboard-text)
          (with-current-buffer buffer
            (should (string-match-p "Summary" (buffer-string)))))
      (kill-buffer buffer))))

(ert-deftest ende-test/chat-reasoning-summaries-follow-tool-rounds ()
  (ende-test--reset)
  (let* ((call '(:id "summary-call" :name "about_thinking"
                 :arguments "{\"title\":\"Looking\",\"status\":\"Around\"}"))
         (ende-llm-function
          (ende-test--fake-llm
           (list (list :tool-calls (list call)) "Answer") t))
         tool-completion)
    (ende-test--with-buffer
     "* Chat\n** Question\nAsk\n"
     (org-mode)
     (ende-test--chat-goto "** Question")
     (let* ((job (ende-ask))
            (chat-buffer (current-buffer)))
       (cl-letf (((symbol-function 'ende-chat--tool-response-async)
                  (lambda (_call _configuration callback)
                    (setq tool-completion callback))))
         (with-temp-buffer
           (insert "ambient callback buffer")
           (ende--handle-llm-event
            job '(:type :reasoning-summary :item "before" :text "Before "))
           (with-current-buffer (ende-chat--staging-buffer job)
             (should (string-match-p "Agent: Before" (buffer-string))))
           (ende--handle-llm-event
            job '(:type :reasoning-summary :item "before" :text "tools"))
           (with-current-buffer (ende-chat--staging-buffer job)
             (should (string-match-p "Agent: Before tools" (buffer-string))))
           (should (equal (buffer-string) "ambient callback buffer")))
         (with-current-buffer chat-buffer
           (should-not (string-match-p "Agent: Before tools"
                                       (buffer-string))))
         (ende-test--complete-call (car ende-test--calls))
         (funcall tool-completion "{}")
         (should (ende-chat--staging-buffer job))
         (with-temp-buffer
           (insert "ambient callback buffer")
           (ende--handle-llm-event
            job '(:type :reasoning-summary :item "after" :text "After "))
           (should (equal (buffer-string) "ambient callback buffer")))
         (with-current-buffer chat-buffer
           (should-not (string-match-p "Agent: After "
                                       (buffer-string))))
         (ende--handle-llm-event
          job '(:type :reasoning-summary :item "after" :text "After tools"
                :replace t))
         (with-current-buffer (ende-chat--staging-buffer job)
           (should (string-match-p "Agent: After tools" (buffer-string)))
           (should (= 1 (how-many "Agent: After" (point-min) (point-max)))))
         (ende-test--complete-call (nth 1 ende-test--calls)))
       (should (eq (ende-job-state job) 'completed))
       (let* ((document (ende-chat--build-document))
              (headlines (ende-chat--document-headlines document))
              (work (cl-find-if
                     (lambda (headline)
                       (equal (org-element-property :raw-value headline)
                              "Agent: Work"))
                     headlines))
              (reply (cl-find-if
                      (lambda (headline)
                        (equal (org-element-property :raw-value headline)
                               "Agent: Reply"))
                      headlines)))
         (should work)
         (should reply)
         (should (= 1 (cl-count-if
                       (lambda (headline)
                         (equal (org-element-property :raw-value headline)
                                "Agent: Work"))
                       headlines)))
         (should (equal (mapcar
                         (lambda (headline)
                           (org-element-property :raw-value headline))
                         (cl-remove-if-not
                          (lambda (headline)
                            (member (org-element-property :raw-value headline)
                                    '("Agent: Before tools" "Agent: After tools")))
                          (ende-chat--children document work)))
                        '("Agent: Before tools" "Agent: After tools")))
         (cl-flet ((title (headline)
                     (org-element-property :raw-value headline)))
           (let ((titles (mapcar #'title headlines)))
             (should (< (cl-position "Agent: Work" titles :test #'equal)
                        (cl-position "Agent: Reply" titles :test #'equal))))
           (let ((summaries
                  (cl-remove-if-not
                   (lambda (headline)
                     (equal (ende-chat--property document headline :visibility)
                            "reasoning_summary"))
                   headlines)))
             (should (equal (mapcar #'title summaries)
                            '("Agent: Before tools" "Agent: After tools")))
             (should (equal (mapcar #'title
                                    (cl-remove-if-not
                                     (lambda (headline)
                                       (memq headline
                                             (ende-chat--children document work)))
                                     summaries))
                            '("Agent: Before tools" "Agent: After tools")))
             (should (equal (ende-chat--protocol-body-string reply) "Answer"))
             (dolist (summary summaries)
               (should (eq (ende-chat--parent document summary)
                           work))
               (should (string-blank-p
                        (ende-chat--protocol-body-string summary)))
               (should-not (ende-chat--property document summary :id))
               (should (equal (ende-chat--property document summary :role)
                              "assistant")))
             (with-current-buffer chat-buffer
               (goto-char (org-element-property :begin work))
               (org-fold-show-all)
               (org-fold-hide-subtree)
               (goto-char (org-element-property :begin (car summaries)))
               (should (invisible-p (line-beginning-position)))))))))))

(ert-deftest ende-test/chat-summary-only-ordinary-reply-uses-work ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("Answer") t)))
    (ende-test--with-buffer
     "* Chat\n** Question\nAsk\n** Followup\nFollow up\n"
     (org-mode)
     (ende-test--chat-goto "** Question")
     (let* ((job (ende-ask))
            (request-id (plist-get (ende-job-instruction job) :request-id)))
       (ende--handle-llm-event
        job '(:type :reasoning-summary :item "ordinary" :text "Ordinary plan"))
       (ende-test--complete-call (car ende-test--calls))
       (should (eq (ende-job-state job) 'completed))
       (ende-test--chat-goto "** Followup")
       (let* ((document (ende-chat--build-document))
              (work (ende-chat--request-work-section document request-id))
              (reply (cl-find-if
                      (lambda (headline)
                        (and (equal (org-element-property :raw-value headline)
                                    "Agent: Reply")
                             (equal (ende-chat--property document headline
                                                        :request-id)
                                    request-id)))
                      (ende-chat--document-headlines document)))
              (summary (car (ende-chat--children document work)))
              (context (ende-test--chat-context-text
                        (ende-chat--serialize
                         document (ende-chat--headline-at-point document)))))
         (should work)
         (should reply)
         (should (eq (ende-chat--parent document reply)
                     (ende-chat--parent document work)))
         (should (eq (ende-chat--parent document summary) work))
         (should (string-match-p "Answer" context))
         (should-not (string-match-p "Ordinary plan" context)))))))

(ert-deftest ende-test/chat-reasoning-summaries-never-enter-context ()
  (ende-test--reset)
  (ende-test--with-buffer
   (concat "* Chat\n"
           "** Agent: Work :ignore:noexport:\n"
           ":PROPERTIES:\n"
           ":ENDE_CHAT_WORK: t\n"
           ":ENDE_CHAT_REQUEST_ID: fixture-request\n"
           ":ENDE_CHAT_REPLY_TO: fixture-question\n"
           ":END:\n\n"
           "*** Agent: **Unremarkable title**\n"
           ":PROPERTIES:\n"
           ":ENDE_ROLE: assistant\n"
           ":ENDE_VISIBILITY: reasoning_summary\n"
           ":END:\n\n"
           "** Agent: Reply\n"
           ":PROPERTIES:\n"
           ":ENDE_ROLE: assistant\n"
           ":ENDE_CHAT_REQUEST_ID: fixture-request\n"
           ":ENDE_CHAT_REPLY_TO: fixture-question\n"
           ":END:\n"
           "Done.\n"
           "** Question\nAsk.\n")
   (org-mode)
   (ende-test--chat-goto "** Question")
   (let* ((document (ende-chat--build-document))
          (reply (cl-find-if
                  (lambda (headline)
                    (equal (org-element-property :raw-value headline)
                           "Agent: Reply"))
                  (ende-chat--document-headlines document)))
          (work (ende-chat--request-work-section document "fixture-request"))
          (summary (car (ende-chat--children document work))))
     (should (eq (ende-chat--parent document summary) work))
     (should (string-match-p "Done"
                             (ende-chat--protocol-body-string reply))))
   (dolist (include '(nil t))
     (let* ((ende-chat-include-reasoning include)
            (document (ende-chat--build-document))
            (question (ende-chat--headline-at-point document))
            (text (ende-test--chat-context-text
                   (ende-chat--serialize document question))))
       (should (string-match-p "Done" text))
       (should-not (string-match-p "Unremarkable title" text))))))

(ert-deftest ende-test/chat-malformed-staged-work-rolls-back-and-retains-recovery ()
  (ende-test--reset)
  (let* ((directory (make-temp-file "ende-chat-staged-malformed-" t))
         (user-emacs-directory directory)
         (call '(:id "malformed-work-call" :name "about_thinking"
                 :arguments "{\"title\":\"Malformed\",\"status\":\"Done\"}"))
         (ende-llm-function
          (ende-test--fake-llm
           (list (list :tool-calls (list call))
                 "#+ENDE_CHAT_TITLE: Renamed question\nFinal answer") t)))
    (unwind-protect
        (ende-test--with-buffer
         "* Root\n** \nAsk\n"
         (org-mode)
         (ende-test--chat-goto "** ")
         (let* ((job (ende-ask))
                (request-id (plist-get (ende-job-instruction job) :request-id))
                (before (buffer-string)))
           (ende-test--complete-call (car ende-test--calls))
           (with-current-buffer (ende-chat--staging-buffer job)
             (let ((inhibit-read-only t))
               (goto-char (point-min))
               (should (re-search-forward "^:ENDE_CHAT_WORK: t$" nil t))
               (replace-match ":ENDE_CHAT_WORK: malformed")))
           (ende-test--complete-call (nth 1 ende-test--calls))
           (should (eq (ende-job-state job) 'failed))
           (should (equal before (buffer-string)))
           (let* ((document (ende-chat--build-document))
                  (pending (ende-chat--request-state-headline
                            document request-id "pending"))
                  (file (plist-get (ende-job-instruction job) :recovery-file))
                  (recovery (ende-chat--read-recovery-file file)))
             (should pending)
             (should (file-exists-p file))
             (should (string-match-p "ENDE_CHAT_WORK: malformed"
                                     (cadr recovery)))
             (should (string-match-p "Final answer" (cadr recovery))))))
      (delete-directory directory t))))

(ert-deftest ende-test/chat-failure-after-tool-work-keeps-staged-transcript ()
  (ende-test--reset)
  (let* ((call '(:id "failed-call" :name "about_thinking"
                 :arguments "{\"title\":\"Working\",\"status\":\"Done\"}"))
         (ende-llm-function
          (ende-test--fake-llm
           (list (list :tool-calls (list call)) :error) t)))
    (ende-test--with-buffer
     "* Root\n** Question\nAsk\n"
     (org-mode)
     (ende-test--chat-goto "** Question")
     (let* ((job (ende-ask))
            (request-id (plist-get (ende-job-instruction job) :request-id)))
       (ende-test--complete-call (car ende-test--calls))
       (should (= (length ende-test--calls) 2))
       (ende-test--complete-call (nth 1 ende-test--calls))
       (should (eq (ende-job-state job) 'failed))
       (should (string-match-p "Agent: Work" (buffer-string)))
       (should (string-match-p "Agent: Reply" (buffer-string)))
       (should (string-match-p "mock failure" (buffer-string)))
       (should (null (ende-chat--staging-buffer job)))
       (let* ((document (ende-chat--build-document))
              (reply (ende-chat--request-state-headline
                      document request-id "failed")))
         (should reply))))))

(ert-deftest ende-test/chat-tool-boundaries-update-private-recovery-snapshot ()
  (ende-test--reset)
  (let* ((directory (make-temp-file "ende-chat-recovery-test-" t))
         (user-emacs-directory directory)
         (call '(:id "snapshot-call" :name "about_thinking"
                 :arguments "{\"title\":\"Checkpoint\",\"status\":\"Test\"}"))
         (ende-llm-function
          (ende-test--fake-llm
           (list (list :tool-calls (list call)) "Final snapshot answer") t)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "* Root\n** Question\nAsk\n")
          (ende-test--chat-goto "** Question")
          (let* ((job (ende-ask))
                 (request-id (plist-get (ende-job-instruction job) :request-id))
                 deferred-tool-completion)
            (cl-letf (((symbol-function 'ende-chat--tool-response-async)
                       (lambda (_call _configuration callback)
                         (setq deferred-tool-completion callback))))
              (ende-test--complete-call (car ende-test--calls))
              (let* ((file (plist-get (ende-job-instruction job) :recovery-file))
                     (first (with-temp-buffer
                              (insert-file-contents file)
                              (buffer-string))))
                (should (file-exists-p file))
                (should (equal (file-modes file) #o600))
                (should (string-match-p request-id first))
                (should (string-match-p "Agent: Work" first))
                (should-not (string-match-p "ENDE_TOOL_KIND: result" first))
                (funcall deferred-tool-completion "{\\\"ok\\\":true}")
                (let ((latest (with-temp-buffer
                                (insert-file-contents file)
                                (buffer-string))))
                  (should (string-match-p "ENDE_TOOL_KIND: result" latest))
                  (should (not (equal first latest)))))
              (should (= (length ende-test--calls) 2))
              (ende-test--complete-call (nth 1 ende-test--calls))
              (should (eq (ende-job-state job) 'completed))
              (should-not (file-exists-p
                           (expand-file-name
                            (concat request-id ".org")
                            (expand-file-name "ende/chat-recovery/"
                                              user-emacs-directory))))
              (should (null (ende-chat--staging-buffer job))))))
      (delete-directory directory t))))

(ert-deftest ende-test/chat-unsafe-recovery-only-opens-inspection-buffer ()
  (ende-test--reset)
  (let* ((directory (make-temp-file "ende-chat-recovery-unsafe-" t))
         (user-emacs-directory directory)
         (recovery-directory (expand-file-name "ende/chat-recovery/" directory))
         (file nil))
    (unwind-protect
        (progn
          (make-directory recovery-directory t)
          (setq file (expand-file-name "stale-request.org" recovery-directory))
          (with-temp-file file
            (insert "# ENDE_CHAT_RECOVERY: "
                    (prin1-to-string
                     '(:request-id "stale-request"
                       :question-id "old-question"
                       :source-buffer "*other-chat*"))
                    "\n** Agent: Work\nRecovered transcript.\n"))
          (ende-test--with-buffer
           "* Other\n** Question\nKeep this pending.\n"
           (org-mode)
           (ende-test--chat-goto "** Question")
           (let* ((job (ende-chat--enqueue
                        (ende-chat--build-document)
                        (ende-chat--headline-at-point
                         (ende-chat--build-document)) nil))
                  (before (buffer-string))
                  (inspection-buffer nil))
             (cl-letf (((symbol-function 'completing-read)
                        (lambda (&rest _) file))
                       ((symbol-function 'display-buffer)
                        (lambda (buffer &rest _)
                          (setq inspection-buffer buffer))))
               (ende-chat-recover-work))
             (should (equal before (buffer-string)))
             (should (buffer-live-p inspection-buffer))
             (with-current-buffer inspection-buffer
               (should (string-match-p "Recovered transcript" (buffer-string)))
               (should buffer-read-only))
             (should (eq (ende-job-state job) 'running))
             (ende-job-fail job "test cleanup"))))
      (delete-directory directory t))))

(ert-deftest ende-test/chat-safe-recovery-materializes-matching-request ()
  (ende-test--reset)
  (let* ((directory (make-temp-file "ende-chat-recovery-safe-" t))
         (user-emacs-directory directory)
         (recovery-directory (expand-file-name "ende/chat-recovery/" directory))
         (file nil))
    (unwind-protect
        (progn
          (make-directory recovery-directory t)
          (ende-test--with-buffer
           "* Root\n** Question\nAsk\n"
           (org-mode)
           (ende-test--chat-goto "** Question")
           (let* ((job (ende-ask))
                  (instruction (ende-job-instruction job))
                  (request-id (plist-get instruction :request-id))
                  (question-id (plist-get instruction :question-id))
                  (transcript
                   (concat
                    (ende-chat--work-text 2 question-id request-id)
                    (ende-chat--assistant-protocol-text
                     3 question-id request-id "" '(:content "Recovered turn")
                     nil nil 'org)))
                  (metadata
                   (list :request-id request-id :question-id question-id
                         :source-buffer (buffer-name))))
             (setq file (expand-file-name (concat request-id ".org")
                                          recovery-directory))
             (with-temp-file file
               (insert "# ENDE_CHAT_RECOVERY: "
                       (prin1-to-string metadata) "\n" transcript))
             (cl-letf (((symbol-function 'completing-read)
                        (lambda (&rest _) file)))
               (ende-chat-recover-work))
             (should (eq (ende-job-state job) 'running))
             (should (string-match-p "Recovered turn" (buffer-string)))
             (should-not (file-exists-p file))
             (let* ((document (ende-chat--build-document))
                    (reply (ende-chat--request-state-headline
                            document request-id "failed")))
               (should reply))
             (ende-job-fail job "test cleanup"))))
      (delete-directory directory t))))

(ert-deftest ende-test/chat-recovery-opens-an-unvisited-source-file ()
  (ende-test--reset)
  (let* ((directory (make-temp-file "ende-chat-recovery-unopened-" t))
         (user-emacs-directory directory)
         (source-file (expand-file-name "conversation.org" directory))
         (recovery-directory
          (expand-file-name "ende/chat-recovery/" directory))
         (request-id "unopened-request")
         (question-id "unopened-question")
         (recovery-file
          (expand-file-name (concat request-id ".org") recovery-directory))
         (source-buffer nil)
         (transcript
          (concat
           (ende-chat--work-text 2 question-id request-id)
           (ende-chat--assistant-protocol-text
            3 question-id request-id "" '(:content "Recovered after restart")
            nil nil 'org))))
    (unwind-protect
        (progn
          (make-directory recovery-directory t)
          (with-temp-file source-file
            (insert "* Root\n** Question\n:PROPERTIES:\n"
                    ":ENDE_ROLE: user\n"
                    (ende-chat--response-property
                     ende-chat-id-property question-id)
                    ":END:\nAsk\n\n"
                    (ende-chat--placeholder-text 2 question-id request-id)))
          (with-temp-file recovery-file
            (insert "# ENDE_CHAT_RECOVERY: "
                    (prin1-to-string
                     (list :request-id request-id :question-id question-id
                           :source-file source-file))
                    "\n" transcript))
          (should-not (find-buffer-visiting source-file))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _) recovery-file)))
            (ende-chat-recover-work))
          (setq source-buffer (find-buffer-visiting source-file))
          (should (buffer-live-p source-buffer))
          (with-current-buffer source-buffer
            (let* ((document (ende-chat--build-document))
                   (reply (ende-chat--request-state-headline
                           document request-id "failed")))
              (should reply)
              (should (string-match-p "Recovered after restart"
                                      (buffer-string)))
              (should (buffer-modified-p))))
          (should-not (file-exists-p recovery-file)))
      (when (buffer-live-p source-buffer)
        (kill-buffer source-buffer))
      (delete-directory directory t))))


(ert-deftest ende-test/chat-persistence-failure-restores-the-pending-slot ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("Answer") t)))
    (ende-test--with-buffer
     "* Root\n** Question\nAsk\n"
     (org-mode)
     (ende-test--chat-goto "** Question")
     (let* ((job (ende-ask))
            (instruction (ende-job-instruction job))
            (target-marker (ende-target-cursor-marker (ende-job-target job)))
            (position (marker-position target-marker))
            (before (buffer-string))
            (response (ende-chat--assistant-text
                       2 (plist-get instruction :question-id)
                       (plist-get instruction :request-id) ""
                       '(:content "Generated answer"))))
       (cl-letf (((symbol-function 'ende-chat--hide-control-drawers-in-region)
                  (lambda (&rest _arguments)
                    (error "Injected drawer-folding failure"))))
          (should-error
           (ende-chat--persist-ordinary-reply job response)))
       (should (equal before (buffer-string)))
       (should (= position (marker-position target-marker)))
       (should (ende-chat--resolve-request-destination job))
       (ende-test--complete-call (car ende-test--calls))
       (should (eq (ende-job-state job) 'completed))))))



(ert-deftest ende-test/chat-request-markers-are-released-on-settlement ()
  (dolist (outcome '(completed failed cancelled))
    (ende-test--reset)
    (let ((ende-llm-function (ende-test--fake-llm '("Answer") t)))
      (ende-test--with-buffer
       "* Root\n** Question\nAsk\n"
       (org-mode)
       (ende-test--chat-goto "** Question")
       (let* ((job (ende-ask))
              (question-marker
               (plist-get (ende-job-instruction job) :question-marker)))
         (should (eq (marker-buffer question-marker) (current-buffer)))
         (pcase outcome
           ('completed (ende-test--complete-call (car ende-test--calls)))
           ('failed (ende-job-fail job "Injected failure"))
           ('cancelled (ende-abort-queue)))
          (should (eq (ende-job-state job) outcome))
          (should-not (marker-buffer question-marker))
          (should-not (plist-get (ende-job-instruction job)
				:question-marker)))))))



(ert-deftest ende-test/chat-compaction-preserves-instructions-and-org-layout ()
  (dolist (level '(2 4))
    (dolist (input '(("" "")
                     ("" "Keep the decisions.")
                     ("My summary" "")
                     ("My summary" "Keep the decisions.")
                     ("" "" "A note added during generation.")))
      (ende-test--reset)
      (let* ((title (nth 0 input))
             (body (nth 1 input))
             (late-note (nth 2 input))
             (ende-chat-compact-instruction "Keep the essentials.")
             (ende-llm-function
              (ende-test--fake-llm
               '((:content "#+ENDE_CHAT_TITLE: Ignore this title\n* Facts\nSummary"
			   :thinking "Plan")) t)))
        (ende-test--with-buffer "* Topic\nEarlier conversation\n"
				(org-mode)
				(goto-char (point-max))
				(cl-loop for depth from 2 below level
					 do (insert (make-string depth ?*) " Ancestor\n"))
				(let ((start (point)))
				  (insert (make-string level ?*) " " title "\n" body "\n"
					  (make-string level ?*) " Next\nContinue\n")
				  (goto-char start)
				  (let* ((job (ende-compact '(:name local :model "compact-model")))
					 (question-id (plist-get (ende-job-instruction job) :question-id))
					 (call (car ende-test--calls))
					 (messages (plist-get call :messages)))
				    (should (eq (ende-job-state job) 'running))
				    (should (string-match-p
					     (regexp-quote (if (string-empty-p body)
							       ende-chat-compact-instruction body))
					     (cdar (last messages))))
				    (should-not (string-match-p "ENDE_CHAT_TITLE" (cdar messages)))
				    (when (and (not (string-empty-p title))
					       (string-empty-p body))
				      (should-not (string-match-p (regexp-quote title)
								  (cdar (last messages)))))
				    (when late-note
				      (org-end-of-meta-data t)
				      (insert late-note "\n"))
				    (ende-test--complete-call call)
				    (should (eq (ende-job-state job) 'completed))
				    (let* ((document (ende-chat--build-document))
					   (question (ende-chat--find-by-property
						      document ende-chat-id-property question-id))
					   (fence (ende-chat--find-by-property
						   document ende-chat-request-id-property
						   (plist-get (ende-job-instruction job) :request-id)))
					   (reasoning (ende-chat--find-by-property
						       document ende-chat-reasoning-for-property
						       (plist-get (ende-job-instruction job)
								  :request-id))))
				      (should (ende-chat--context-fence-p document fence))
				      (should (= (org-element-property :level fence) level))
				      (should (equal (org-element-property :raw-value fence)
						     "Agent: Context Fence"))
				      (should (eq (ende-chat--parent document reasoning) fence))
				      (should-not (ende-chat--context-fence-p document reasoning))
				      (should (equal (ende-test--chat-headline-property
						      fence ende-chat-model-property)
						     "compact-model"))
				      (if (and (string-empty-p title) (string-empty-p body)
					       (not late-note))
					  (should-not question)
					(should question)
					(should (equal (org-element-property :raw-value question)
						       (if (string-empty-p title)
							   "User: Context Fence Instructions" title)))
					(should (equal (ende-chat--property document question :visibility)
						       "hidden"))
					(should (= (org-element-property :end question)
						   (org-element-property :begin fence)))
					(should (string-match-p (regexp-quote (or late-note body))
								(buffer-string))))
				      (should-not (string-match-p "Ignore this title" (buffer-string)))
				      (should (cl-every #'ende-test--chat-control-drawer-hidden-p
							(ende-test--chat-control-drawers))))
				    (ende-test--chat-goto (concat (make-string level ?*) " Next"))
				    (let ((ende-chat-include-reasoning nil))
				      (should
				       (equal (ende-test--chat-context-headings
					       (ende-chat-context-at-point))
					      '("Agent: Context Fence" "Facts" "Next")))))))))))



(ert-deftest ende-test/chat-compaction-rejects-an-edited-destination ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("Summary") t)))
    (ende-test--with-buffer
     "* Topic\nHistory\n** Instructions\nKeep decisions\n** Next\nContinue\n"
     (org-mode)
     (ende-test--chat-goto "** Instructions")
     (let ((job (ende-compact)))
       (pcase-let ((`(,_document ,destination)
                    (ende-chat--resolve-request-destination job)))
         (goto-char (org-element-property :begin destination))
         (org-edit-headline "Moved context fence"))
       (ende-test--complete-call (car ende-test--calls))
       (should (eq (ende-job-state job) 'failed))
       (should (string-match-p "Moved context fence" (buffer-string)))
       (should-not (string-match-p "Summary" (buffer-string)))
       (should (string-match-p "destination was edited or moved"
                               (buffer-string)))))))


(ert-deftest ende-test/chat-compaction-and-asks-share-live-context ()
  (ende-test--reset)
  (let ((ende-provider-profiles '((local :parallel t :max-concurrency 2)))
        (ende-llm-function
         (ende-test--fake-llm
          '("First summary" "New answer" "Second summary" "Final answer"
            "Third summary") t)))
    (ende-test--with-buffer
     (concat "* Topic\nOriginal history\n"
	     "** First compaction\nKeep decisions\n"
	     "** Follow up\nNew question\n"
	     "** Second compaction\nKeep commands\n"
	     "** Final question\nContinue\n")
     (org-mode)
     (ende-test--chat-goto "** First compaction")
     (let ((first (ende-compact)))
       (ende-test--chat-goto "** Follow up")
       (let ((ask (ende-ask)))
         (should (eq (ende-job-state ask) 'queued))
         (ende-test--complete-call (nth 0 ende-test--calls))
         (should (eq (ende-job-state first) 'completed))
         (should (eq (ende-job-state ask) 'running))
         (let ((prompt (ende-test--chat-context-text
                        (plist-get (nth 1 ende-test--calls) :messages))))
           (should (string-match-p "First summary" prompt))
           (should-not (string-match-p "Original history" prompt))
           (should-not (string-match-p "Keep decisions" prompt)))
         (ende-test--chat-goto "** Second compaction")
         (let ((second (ende-compact)))
           (should (eq (ende-job-state second) 'queued))
           (ende-test--chat-goto "** Follow up")
           (org-end-of-meta-data t)
           (insert "Latest detail\n")
           (ende-test--complete-call (nth 1 ende-test--calls))
           (should (eq (ende-job-state second) 'running))
           (let ((prompt (ende-test--chat-context-text
                          (plist-get (nth 2 ende-test--calls) :messages))))
             (dolist (text '("First summary" "New answer" "Latest detail"
                             "Keep commands"))
	       (should (string-match-p text prompt)))
             (should-not (string-match-p "Original history" prompt)))
           (ende-test--chat-goto "** Final question")
           (let ((final (ende-ask)))
             (should (eq (ende-job-state final) 'queued))
             (ende-test--complete-call (nth 2 ende-test--calls))
             (let ((prompt (ende-test--chat-context-text
                            (plist-get (nth 3 ende-test--calls) :messages))))
	       (should (string-match-p "Second summary" prompt))
	       (dolist (text '("First summary" "New answer" "Latest detail"
			       "Keep commands" "Original history"))
                 (should-not (string-match-p text prompt))))
             (ende-test--complete-call (nth 3 ende-test--calls))
             (should (eq (ende-job-state final) 'completed)))
           ;; Compacting an existing fence summarizes its result, not its
           ;; archived instructions or the conversation after it.
           (let* ((document (ende-chat--build-document))
                  (fence (ende-chat--find-by-property
                          document ende-chat-request-id-property
                          (plist-get (ende-job-instruction second) :request-id))))
             (goto-char (org-element-property :begin fence))
             (let ((third (ende-compact)))
	       (should
                (equal (ende-test--chat-context-headings
                        (butlast (cdr (plist-get (nth 4 ende-test--calls)
                                                 :messages))))
		       '("Agent: Context Fence")))
	       (ende-test--complete-call (nth 4 ende-test--calls))
	       (should (eq (ende-job-state third) 'completed)))))))
     ;; A fresh buffer reconstructs the same context using only durable Org.
     (let ((text (buffer-string)))
       (with-temp-buffer
         (org-mode)
         (insert text)
         (ende-test--chat-goto "** Final question")
         (let* ((messages (ende-chat-context-at-point))
                (context (ende-test--chat-context-text messages)))
           (should (equal (ende-test--chat-context-headings messages)
                          '("Agent: Context Fence" "Final question")))
           (should (string-match-p "Third summary" context))
           (should-not (string-match-p "Second summary" context))))))))



(ert-deftest ende-test/chat-compaction-failures-leave-history-usable ()
  (dolist (outcome '(:error :empty :title-only :cancelled :deleted))
    (ende-test--reset)
    (let ((ende-llm-function
           (ende-test--fake-llm
            (list (pcase outcome
                    (:error :error)
                    (:empty "")
                    (:title-only "#+ENDE_CHAT_TITLE: Not a summary\n")
                    (_ "Summary"))) t)))
      (ende-test--with-buffer
       "* Topic\nOriginal history\n** Instructions\nKeep decisions\n** Next\nAsk\n"
       (org-mode)
       (ende-test--chat-goto "** Instructions")
       (let ((job (ende-compact)))
         (pcase outcome
           (:cancelled (ende-abort-queue))
           (:deleted
            (pcase-let ((`(,_document ,headline) (ende-chat--resolve-request-destination job)))
              (delete-region (org-element-property :begin headline)
                             (org-element-property :end headline)))))
         (ende-test--complete-call (car ende-test--calls))
         (should (eq (ende-job-state job)
                     (if (eq outcome :cancelled) 'cancelled 'failed)))
         (should (string-match-p "Keep decisions" (buffer-string)))
         (let ((document (ende-chat--build-document)))
           (should-not
            (cl-some (lambda (headline)
		       (ende-chat--context-fence-p document headline))
                     (ende-chat--document-headlines document))))
         (ende-test--chat-goto "** Next")
         (let* ((messages (ende-chat-context-at-point))
                (context (ende-test--chat-context-text messages)))
           (should (equal (ende-test--chat-context-headings messages)
                          '("Topic" "Next")))
           (should (string-match-p "Original history" context))
           (should-not (string-match-p "Keep decisions" context))))))))



(ert-deftest ende-test/chat-compaction-supports-queued-repeats ()
  (dolist (body '("" "Keep the decisions"))
    (ende-test--reset)
    (let ((ende-llm-function
           (ende-test--fake-llm '("First summary" "Second summary") t)))
      (ende-test--with-buffer
       (concat "* Topic\nOriginal history\n** \n" body "\n** Next\nContinue\n")
       (org-mode)
       (ende-test--chat-goto "** ")
       (let ((first (ende-compact))
             (second (ende-compact)))
         (should (eq (ende-job-state second) 'queued))
         (ende-test--complete-call (nth 0 ende-test--calls))
         (should (eq (ende-job-state first) 'completed))
         (should (eq (ende-job-state second) 'running))
         (let ((context (ende-test--chat-context-text
                         (plist-get (nth 1 ende-test--calls) :messages))))
           (should (string-match-p "First summary" context))
           (should-not (string-match-p "Original history" context)))
         (ende-test--complete-call (nth 1 ende-test--calls))
         (should (eq (ende-job-state second) 'completed))
         (ende-test--chat-goto "** Next")
         (let* ((messages (ende-chat-context-at-point))
                (context (ende-test--chat-context-text messages)))
           (should (equal (ende-test--chat-context-headings messages)
                          '("Agent: Context Fence" "Next")))
           (should (string-match-p "Second summary" context))
           (should-not (string-match-p "First summary" context)))
         (if (string-empty-p body)
             (should-not (string-match-p "^\\*\\* *$" (buffer-string)))
           (should (string-match-p body (buffer-string)))))))))



(ert-deftest ende-test/chat-ask-marks-question-and-retries-without-sent-state ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm
          '((:content "First answer" :thinking "First plan")
            (:content "Second answer" :thinking "Second plan")))))
    (ende-test--with-buffer "* Root\n** Question\nAsk again\n"
			    (org-mode)
			    (ende-test--chat-goto "** Question")
			    (ende-ask)
			    (ende-ask)
			    (should (= (ende-test--chat-response-count) 2))
			    (should (= (length (ende-test--chat-reasoning-headlines)) 2))
			    (should (equal (org-entry-get nil ende-chat-role-property)
					   "user"))
			    (should (< (string-match-p "First answer" (buffer-string))
				       (string-match-p "Second answer" (buffer-string))))
			    (let* ((document (ende-chat--build-document))
				   (headlines (ende-chat--document-headlines document))
				   (reasoning
				    (cl-remove-if-not
				     (lambda (headline)
				       (string= (org-element-property :raw-value headline)
						"Agent: Reasoning"))
				     headlines))
				   (replies
				    (cl-remove-if-not
				     (lambda (headline)
				       (string= (org-element-property :raw-value headline)
						"Agent: Reply"))
				     headlines)))
			      (should (< (org-element-property :begin (nth 0 reasoning))
					 (org-element-property :begin (nth 0 replies))))
			      (should (< (org-element-property :begin (nth 0 replies))
					 (org-element-property :begin (nth 1 reasoning))))
			      (should (< (org-element-property :begin (nth 1 reasoning))
					 (org-element-property :begin (nth 1 replies))))
			      (dolist (pair (cl-mapcar #'cons reasoning replies))
				(let ((request-id
				       (ende-chat--property document (cdr pair) :request-id)))
				  (should (equal (ende-chat--property document (car pair)
								      :reasoning-for)
						 request-id))
				  (should (eq (ende-chat--find-by-property
					       document ende-chat-request-id-property request-id)
					      (cdr pair))))))
			    (should-not (cl-some #'ende-review-overlay-p
						 (overlays-in (point-min) (point-max)))))))



(ert-deftest ende-test/chat-failure-keeps-question-and-reserves-slot ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(:error))))
    (ende-test--with-buffer "* Root\n** Question\nOriginal text\n"
			    (org-mode)
			    (ende-test--chat-goto "** Question")
			    (let ((job (ende-ask)))
			      (should (eq (ende-job-state job) 'failed)))
			    (should (string-match-p "Original text" (buffer-string)))
			    (should (string-match-p ":ENDE_CHAT_STATE: failed" (buffer-string)))
			    (should (string-match-p ":ENDE_VISIBILITY: hidden" (buffer-string))))))



(ert-deftest ende-test/chat-queued-context-is-built-at-dispatch-and-hides-placeholders ()
  (ende-test--reset)
  (let ((ende-provider-profiles '((local :parallel t :max-concurrency 2)))
        (ende-llm-function (ende-test--fake-llm '("First" "Second") t)))
    (ende-test--with-buffer "* Root\n** Question\nAsk\n"
			    (org-mode)
			    (ende-test--chat-goto "** Question")
			    (let ((point-before (point)))
			      (setq ende--queue-suspended t)
			      (let ((first (ende-ask))
				    (second (ende-ask)))
				(should (= (point) point-before))
				(let ((drawers (ende-test--chat-control-drawers)))
				  (should (= (length drawers) 3))
				  (should (cl-every #'ende-test--chat-control-drawer-hidden-p
						    drawers)))
				(let ((headlines (ende-test--chat-response-headlines)))
				  (should (= (length headlines) 2))
				  (should (cl-every #'ende-test--chat-response-has-separator-p
						    headlines))
				  (dolist (job (list first second))
				    (should
				     (cl-some
				      (lambda (headline)
					(= (overlay-start (ende-job-overlay job))
					   (1- (org-element-property :begin headline))))
				      headlines))))
				(setq ende--queue-suspended nil)
				(ende-process-next)
				(should (= (length ende-test--calls) 2))
				(dolist (messages ende-test--prompts)
				  (let ((users (mapcar #'cdr
						       (cl-remove-if-not
							(lambda (message)
							  (equal (car message) "user"))
							messages))))
				    (should (cl-every
					     (lambda (text)
					       (and (not (string-match-p "ENDE_CHAT_STATE" text))
						    (not (string-match-p "Agent: Reply" text))))
					     users))))
				(ende-test--complete-call (nth 1 ende-test--calls))
				(should (eq (ende-job-state second) 'completed))
				(should (eq (ende-job-state first) 'running))
				(should (string-match-p "Second" (buffer-string)))
				(should (cl-every #'ende-test--chat-control-drawer-hidden-p
						  (ende-test--chat-control-drawers)))
				(should (string-match-p "ENDE_CHAT_STATE: pending" (buffer-string)))
				(ende-test--complete-call (nth 0 ende-test--calls))
				(should (eq (ende-job-state first) 'completed))
				(should (< (string-match-p "First" (buffer-string))
					   (string-match-p "Second" (buffer-string))))
				(should (cl-every #'ende-test--chat-control-drawer-hidden-p
						  (ende-test--chat-control-drawers)))
				(should (cl-every #'ende-test--chat-response-has-separator-p
						  (ende-test--chat-response-headlines))))))))



(ert-deftest ende-test/chat-preserves-point-through-async-lifecycle ()
  (dolist (response '("Answer" :error))
    (ende-test--reset)
    (let ((ende-llm-function (ende-test--fake-llm (list response) t)))
      (ende-test--with-buffer
       "* Root\n** Question\nAsk\n** Work\nKeep editing\n"
       (org-mode)
       (ende-test--chat-goto "** Question")
       (let ((question-point (point)))
         (setq ende--queue-suspended t)
         (let ((job (ende-ask)))
           (should (= (point) question-point))
           (setq ende--queue-suspended nil)
           (ende-process-next)
           (goto-char (point-min))
           (search-forward "Keep editing")
           (let ((line (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position)))
                 (column (current-column)))
             (ende-test--complete-call (car ende-test--calls))
             (should (equal line
                            (buffer-substring-no-properties
                             (line-beginning-position)
                             (line-end-position))))
             (should (= column (current-column)))
             (should (eq (ende-job-state job)
                         (if (eq response :error) 'failed 'completed))))))))))



(ert-deftest ende-test/chat-ask-promotes-the-selected-headline-to-user ()
  (dolist (local-role '(nil "assistant"))
    (ende-test--reset)
    (let ((ende-llm-function (ende-test--fake-llm '("Answer"))))
      (ende-test--with-buffer
       (concat "* Root\n"
	       "** Agent: Reply\n"
	       ":PROPERTIES:\n:ENDE_ROLE: assistant\n:END:\n"
	       "Prior assistant answer.\n"
	       "*** Follow-up\n"
	       (if local-role
                   (format ":PROPERTIES:\n:ENDE_ROLE: %s\n:END:\n"
                           local-role)
                 "")
	       "Question from this node.\n")
       (org-mode)
       (ende-test--chat-goto "*** Follow-up")
       (let* ((job (ende-ask))
              (document (ende-chat--build-document))
              (question
	       (ende-chat--find-by-property
                document ende-chat-id-property
                (plist-get (ende-job-instruction job) :question-id)))
              (parent (ende-chat--parent document question))
              (messages (plist-get (car ende-test--calls) :messages))
              (assistant-text
	       (mapconcat
                (lambda (message)
                  (if (equal (car message) "assistant")
                      (cdr message)
                    ""))
                messages "\n"))
              (final-message (car (last messages))))
         (should (eq (ende-job-state job) 'completed))
         (should (equal (ende-chat--property document question :role)
                        "user"))
         (should (equal (ende-chat--property document parent :role)
                        "assistant"))
         (should-not (ende-chat--property document question :visibility))
         (should (string-match-p "Prior assistant answer" assistant-text))
         (should-not (string-match-p "Question from this node" assistant-text))
         (should (equal (car final-message) "user"))
         (should (string-match-p "Question from this node"
                                 (cdr final-message))))))))



(ert-deftest ende-test/chat-ask-rejects-hidden-nodes-and-reasoning-roots ()
  (dolist (property
           '(":ENDE_VISIBILITY: hidden"
             ":ENDE_VISIBILITY: reasoning"))
    (ende-test--reset)
    (ende-test--with-buffer
     (format "* Root\n** Question\n:PROPERTIES:\n%s\n:END:\nAsk\n"
	     property)
     (org-mode)
     (ende-test--chat-goto "** Question")
     (should-error (ende-ask) :type 'user-error))))

(provide 'ende-chat-actions-test)

;;; ende-chat-actions-test.el ends here
