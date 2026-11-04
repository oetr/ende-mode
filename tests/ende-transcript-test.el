;;; ende-transcript-test.el --- Transcript ownership contracts -*- lexical-binding: t; -*-

(require 'ert)
(require 'ende-transcript)
(require 'ende-chat-test-helpers)

(ert-deftest ende-transcript-response-boundaries ()
  (let ((transcript (ende-transcript-create :task "Task")))
    (dotimes (_ 2)
      (ende-transcript-begin-response transcript)
      (ende-transcript-event transcript '(:type :delta :channel :content :text "partial"))
      (ende-transcript-event transcript '(:type :reasoning-summary :item "reused" :text "old"))
      (ende-transcript-event transcript '(:type :reasoning-summary :item "reused" :text "plan" :replace t))
      (ende-transcript-accept transcript '(:content "complete") nil)
      (ende-transcript-accept transcript '(:content "complete") nil))
    (ende-transcript-finish transcript 'cancelled)
    (ende-transcript-finish transcript 'cancelled)
    (let ((entries (ende-transcript-entries transcript)))
      (should (equal (mapcar #'ende-transcript-entry-kind entries)
                     '(response summary response summary status)))
      (should (equal (mapcar #'ende-transcript-entry-content (butlast entries))
                     '("complete" "plan" "complete" "plan")))
      (should (= (length entries)
                 (length (delete-dups (mapcar #'ende-transcript-entry-id entries))))))))

(ert-deftest ende-transcript-viewer-cannot-affect-execution-or-persistence ()
  (cl-labels
      ((response (round)
         (let* ((id (format "call-%d" round))
                (text (format "Response %d" round))
                (arguments (json-encode (list :title "Checking" :status text)))
                (call (list :id id :name "about_thinking" :arguments arguments)))
           (list :content text :reported-model (format "model-%d" round)
                 :usage (list :input_tokens (* 10 round) :output_tokens round)
                 :tool-calls (list call)
                 :protocol-items
                 (list 'openai-responses
                       (ende-protocol-json-read-string
                        "{\"type\":\"reasoning\",\"encrypted_content\":\"opaque\",\"summary\":[]}")
                       (ende-protocol-json-read-string
                        (json-encode
                         (list :type "message" :role "assistant"
                               :content (vector (list :type "output_text" :text text)))))
                       (ende-protocol-json-read-string
                        (json-encode
                         (list :type "function_call" :id (concat "item-" id)
                               :call_id id :name "about_thinking" :arguments arguments
                               :status "completed" :extra '(:retained t))))))))
       (state (job)
         (list (prin1-to-string (ende-job-transcript job))
               ende-chat--next-id ende-tool-store--next-id
               (with-temp-buffer
                 (insert-file-contents-literally ende-tool-store-file)
                 (secure-hash 'sha256 (current-buffer)))))
       (run (inspect)
         (ende-test--reset)
         (let ((ende-tool-store--next-id 0)
               (ende-llm-function
                (ende-test--fake-llm
                 (list (response 1) (response 2)
                       '(:content "Final answer" :reported-model "model-final")) t))
               viewer)
           (unwind-protect
               (cl-letf (((symbol-function 'ende-chat--new-id)
                          (lambda (prefix)
                            (format "%s-%d" prefix (cl-incf ende-chat--next-id))))
                         ((symbol-function 'ende-tool-store--new-execution-id)
                          (lambda ()
                            (format "execution-%d" (cl-incf ende-tool-store--next-id)))))
                 (ende-test--with-buffer "* Chat\n** Question\nInvestigate.\n"
                   (rename-buffer " *ende-transcript-source*")
                   (org-mode)
                   (ende-test--chat-goto "** Question")
                   (let* ((job (ende-ask (ende-test--request-profile
                                         '(:name remote :model "fixture"))))
                          (source (buffer-string)))
                     (dotimes (round 3)
                       (let ((call (nth round ende-test--calls)))
                         (should call)
                         (when inspect
                           (setq viewer (ende-work-buffer-open job))
                           (with-current-buffer viewer (org-overview)))
                         (ende-test--callback
                          call :on-event '(:type :delta :channel :content :text "Partial "))
                         (ende-test--callback
                          call :on-event '(:type :reasoning-summary :item "reused" :text "Draft"))
                         (when inspect
                           (kill-buffer viewer)
                           (should-not (ende-job-work-buffer job))
                           (should (eq (ende-job-state job) 'running)))
                         (ende-test--callback
                          call :on-event
                          (list :type :reasoning-summary :item "reused"
                                :text (format "Plan %d" (1+ round)) :replace t))
                         (ende-test--callback
                          call :on-event '(:type :reasoning-summary :item "reused" :text " complete"))
                         (when inspect (setq viewer (ende-work-buffer-open job)))
                         (ende-test--complete-call call)
                         (unless inspect (should-not (ende-job-work-buffer job)))
                         (when (< round 2) (should (equal source (buffer-string))))))
                     (should (eq (ende-job-state job) 'completed))
                     (should (= (length ende-test--calls) 3))
                     (let ((entries (ende-transcript-entries (ende-job-transcript job))))
                       (should (equal (mapcar #'ende-transcript-entry-content
                                              (cl-remove-if-not
                                               (lambda (entry)
                                                 (eq (ende-transcript-entry-kind entry) 'summary))
                                               entries))
                                      '("Plan 1 complete" "Plan 2 complete" "Plan 3 complete"))))
                     (let ((before (state job)) rendered)
                       (cl-letf (((symbol-function 'ende-artifact-prepare-text)
                                  (lambda (&rest _) (ert-fail "Rendering prepared artifacts"))))
                         (setq rendered (ende-chat--render-transcript job))
                         (should (equal rendered (ende-chat--render-transcript job))))
                       (should (equal before (state job))))
                     (list (prin1-to-string
                            (mapcar (lambda (call)
                                      (list (plist-get call :messages)
                                            (plist-get (plist-get call :options) :tool-definitions)))
                                    ende-test--calls))
                           (buffer-string)))))
             (when (buffer-live-p viewer) (kill-buffer viewer))
             (ende-test--reset)))))
    ;; Identities are deterministic at creation, not normalized after the fact.
    (should (equal (run nil) (run t)))))

(ert-deftest ende-transcript-history-preserves-mixed-turns-and-editable-final ()
  (ende-test--reset)
  (let* ((call '(:id "mixed" :name "about_thinking"
                     :arguments "{\"title\":\"Working\",\"status\":\"Reading\"}"))
         (message (lambda (text)
                    (ende-protocol-json-read-string
                     (json-encode (list :type "message" :role "assistant"
                                        :content (vector (list :type "output_text" :text text)))))))
         (opaque (ende-protocol-json-read-string
                  "{\"type\":\"reasoning\",\"encrypted_content\":\"tail\"}"))
         (provider-call (ende-protocol-json-read-string
                         (json-encode (list :type "function_call" :call_id "mixed"
                                            :name (plist-get call :name)
                                            :arguments (plist-get call :arguments)))))
         (first (list :content "After call" :tool-calls (list call)
                      :protocol-items (list 'openai-responses provider-call (funcall message "After call"))))
         (final (list :content "Final answer"
                      :protocol-items (list 'openai-responses (funcall message "Final answer") opaque)))
         (ende-llm-function (ende-test--fake-llm (list first final))))
    (ende-test--with-buffer "* Chat\n** Question\nInvestigate.\n"
      (org-mode)
      (ende-test--chat-goto "** Question")
      (should (eq (ende-job-state (ende-ask)) 'completed))
      (goto-char (point-min))
      (search-forward "Final answer")
      (replace-match "Edited answer")
      (goto-char (point-max))
      (insert "** Next\nContinue\n")
      (ende-test--chat-goto "** Next")
      (let* ((messages (ende-chat-context-at-point))
             (items (ende-codex--request-messages messages))
             (types (mapcar (lambda (item)
                              (if (hash-table-p item) (gethash "type" item)
                                (or (alist-get :type item) "message"))) items)))
        (should (equal (cl-remove-if (lambda (type) (equal type "message")) types)
                       '("function_call" "function_call_output" "reasoning")))
        (should (< (cl-position "function_call" types :test #'equal)
                   (cl-position "function_call_output" types :test #'equal)))
        (should (string-match-p "Edited answer" (ende-test--chat-context-text messages)))
        (should-not (string-match-p "Final answer" (ende-test--chat-context-text messages)))
        (should (equal (ende-protocol-json-get "encrypted_content" (nth (- (length items) 2) items)) "tail")))
      (ende-test--chat-goto "** Agent: Reply")
      (should (= 1 (cl-count "Edited answer" (ende-chat-context-at-point)
                             :key #'ende-content-body :test #'equal))))))

(provide 'ende-transcript-test)
;;; ende-transcript-test.el ends here
