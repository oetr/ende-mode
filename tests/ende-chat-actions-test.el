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

(ert-deftest ende-test/chat-completion-rediscovers-after-unrelated-edit ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("Answer") t)))
    (ende-test--with-buffer
     "* Root\n** Question\nAsk\n* Notes\nKeep this note.\n"
     (org-mode)
     (ende-test--chat-goto "** Question")
     (let* ((job (ende-ask))
            (instruction (ende-job-instruction job)))
       (goto-char (point-max))
       (insert "Added while the request was running.\n")
       (ende-test--complete-call (car ende-test--calls))
       (should (eq (ende-job-state job) 'completed))
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
          (ende-chat--persist-request-section job response nil nil t)))
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
         (should-not (plist-get (ende-job-instruction job) :question-marker))
         (should-not (plist-get (ende-job-instruction job)
				:request-end-marker)))))))



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
