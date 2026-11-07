;;; ende-transcript-test.el --- Transcript ownership contracts -*- lexical-binding: t; -*-

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

;; Tests for transcript ownership contracts.

;;; Code:

(require 'ert)
(require 'ende-transcript)
(require 'ende-chat-test-helpers)

(ert-deftest ende-transcript-response-boundaries ()
  (let ((transcript (ende-transcript-create :task "Task")) previous-items)
    (cl-labels ((leaf (kind key)
                 (gethash (cons kind key) (ende-transcript-sources transcript)))
               (observe (event) (ende-transcript-event transcript event)))
      (dotimes (round 2)
        (ende-transcript-begin-response transcript)
        (should-not (ende-transcript-metadata transcript))
        (observe (list :type :metadata :metadata (list :source :response :model round)))
        (observe '(:type :usage :usage (:output_tokens 5)))
        (observe '(:type :item :key 7 :kind text :text ""))
        (should-not (leaf 'text 7))
        (observe '(:type :delta :channel :thinking :item-key 4 :text "private"))
        (observe '(:type :reasoning-summary :item "reused" :item-key 4 :text "old"))
        (observe '(:type :reasoning-summary :item "reused" :item-key 4 :text "plan" :replace t))
        (observe '(:type :item :key 2 :kind tool-call
                  :call (:key 2 :id "call" :name "inspect" :arguments "{")))
        (observe '(:type :agent-activity :title "Checking" :status "Reading"))
        (observe '(:type :delta :channel :content :item-key 7 :text "partial"))
        (let* ((opaque (ende-protocol-json-read-string "{\"type\":\"reasoning\",\"encrypted_content\":\"opaque\"}"))
               (message (ende-protocol-json-read-string
                         "{\"type\":\"message\",\"content\":[{\"type\":\"output_text\",\"text\":\"complete\"}]}"))
               (raw-call (ende-protocol-json-read-string
                          "{\"type\":\"function_call\",\"call_id\":\"call\",\"name\":\"inspect\",\"arguments\":\"{}\"}"))
               (result (list :content "complete" :thinking "private"
                             :protocol-items (list 'openai-responses raw-call opaque message)
                             :protocol-item-keys '(2 4 7)))
               (call (car (ende-agent--prepare-calls
                           (ende--make-job :id "child" :transcript transcript)
                           '((:key 2 :id "call" :name "inspect" :arguments "{}")) result)))
               (response (ende-transcript-response transcript)))
          (observe (list :type :item :key 4 :kind 'opaque :provider "openai-responses"
                         :item (copy-hash-table opaque)))
          (let* ((entries (copy-sequence (ende-transcript-entries transcript)))
                 (ids (mapcar #'ende-transcript-entry-id entries))
                 (items (list (leaf 'tool-call 2) (leaf 'opaque 4) (leaf 'text 7))))
            (should (equal (mapcar #'ende-transcript-entry-kind
                                   (cl-remove-if-not
                                    (lambda (entry)
                                      (and (= (ende-transcript-entry-round entry) (1+ round))
                                           (not (eq (ende-transcript-entry-kind entry) 'response)))) entries))
                           '(thinking summary tool-call progress text opaque)))
            (should (eq response (ende-transcript-accept transcript result (list call))))
            (should (= (length items) (length (plist-get (ende-transcript-entry-data response) :items))))
            (should (cl-every #'eq items (plist-get (ende-transcript-entry-data response) :items)))
            (should-not (cl-intersection previous-items items :test #'eq))
            (should (= (plist-get (plist-get (ende-transcript-entry-data response) :response-metadata) :model) round))
            (should (equal (plist-get (ende-transcript-entry-data response) :usage) '(:output_tokens 5)))
            (ende-transcript-accept transcript result (list call))
            (should (equal ids (mapcar #'ende-transcript-entry-id (ende-transcript-entries transcript))))
            (should (cl-every #'eq entries (ende-transcript-entries transcript)))
            (setq previous-items items))))
      (let* ((sources (ende-transcript-sources transcript))
             (response (ende-transcript-response transcript))
             (synthetic (ende-transcript-accept
                         transcript '(:content "Guidance")
                         '((:key 2 :id "synthetic" :name "user_guidance" :arguments "{}"))
                         '(:synthetic t))))
        (should (eq sources (ende-transcript-sources transcript)))
        (should (eq response (ende-transcript-response transcript)))
        (should (eq (leaf 'tool-call 2) (car previous-items)))
        (should-not (cl-intersection previous-items (plist-get (ende-transcript-entry-data synthetic) :items) :test #'eq)))
      (ende-transcript-begin-response transcript)
      (observe '(:type :delta :channel :content :text "unfinished full"))
      (let* ((text (leaf 'text :content))
             (partial (list :content "unfinished full" :thinking "partial reasoning"
                            :tool-calls '((:key 2 :id "unfinished" :name "inspect" :arguments "{"))
                            :protocol-item-keys '(:content)
                            :protocol-items
                            (list 'openai-responses
                                  (ende-protocol-json-read-string "{\"type\":\"message\",\"content\":[]}")))))
        (observe (list :type :partial-result :result partial))
        (should (eq text (leaf 'text :content)))
        (should (equal (ende-transcript-entry-content text) "unfinished full"))
        (should (eq (car (plist-get partial :tool-calls))
                    (plist-get (ende-transcript-entry-data (leaf 'tool-call 2)) :call)))
        (should (eq partial (plist-get (ende-transcript-entry-data
                                       (ende-transcript-response transcript)) :partial-result)))
        (ende-transcript-finish transcript 'cancelled)
        (ende-transcript-finish transcript 'cancelled)
        (should (eq (ende-transcript-entry-status text) 'interrupted))
        (should (eq (ende-transcript-entry-status (leaf 'tool-call 2)) 'interrupted))
        (should (eq (ende-transcript-entry-status (ende-transcript-response transcript)) 'interrupted)))
      (let ((entries (ende-transcript-entries transcript)))
        (should (= 1 (cl-count 'status entries :key #'ende-transcript-entry-kind)))
        (should (= (length entries)
                   (length (delete-dups (mapcar #'ende-transcript-entry-id entries)))))))))

(ert-deftest ende-transcript-tools-retain-independent-completion-times ()
  (let* ((transcript (ende-transcript-create))
         (job (ende--make-job :transcript transcript))
         (calls (list (list :id "one") (list :id "two")))
         (clock 10)
         pending)
    (ende-transcript-begin-response transcript)
    (ende-transcript-accept transcript nil calls)
    (cl-letf (((symbol-function 'float-time) (lambda (&optional _) clock)))
      (dolist (call calls)
        (ende-agent--execute-tool
         job call nil
         (lambda (_call _configuration callback) (push callback pending))
         (lambda (content) (ende-transcript-tool-result transcript call content))))
      (setq clock 12)
      (funcall (car pending) "second")
      (setq clock 15)
      (funcall (cadr pending) "first")
      (setq clock 100)
      (funcall (car pending) "late")
      (let ((results (cl-remove-if-not
                      (lambda (entry) (eq (ende-transcript-entry-kind entry) 'tool-result))
                      (ende-transcript-entries transcript))))
        (should (equal (mapcar #'ende-transcript-entry-content results) '("second" "first")))
        (should (equal (mapcar (lambda (entry) (plist-get (ende-transcript-entry-data entry) :duration-ms))
                               results)
                       '(2000 5000)))))))

(ert-deftest ende-transcript-viewer-cannot-affect-execution-or-persistence ()
  (cl-labels
      ((response (round)
         (let* ((id (format "call-%d" round))
                (text (format "Response %d" round))
                (arguments (json-encode (list :title "Checking" :status text)))
                (call (list :key 2 :id id :name "about_thinking" :arguments arguments)))
           (list :content text :duration-ms (* 100 round) :reported-model (format "model-%d" round)
                 :usage (list :input_tokens (* 10 round) :output_tokens round)
                 :tool-calls (list call) :protocol-item-keys '(0 1 2)
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
       (durations ()
         (sort (delq nil (org-map-entries
                          (lambda ()
                            (when-let* ((value (org-entry-get nil "ENDE_DURATION_MS")))
                              (string-to-number value))))) #'<))
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
                       '(:content "Final answer" :duration-ms 300 :reported-model "model-final")) t))
               viewer)
           (unwind-protect
               (cl-letf (((symbol-function 'ende-chat--new-id)
                          (lambda (prefix)
                            (format "%s-%d" prefix (cl-incf ende-chat--next-id))))
                         ((symbol-function 'ende-duration-ms) (lambda (&rest _) 5))
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
                          call :on-event (list :type :delta :channel :content :text "Partial "
                                               :item-key (if (< round 2) 1 :content)))
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
                     (should (equal (durations) '(5 5 100 200 300)))
                     (when inspect
                       (should (equal (durations) (with-current-buffer viewer (durations)))))
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
