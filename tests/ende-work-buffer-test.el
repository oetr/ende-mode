;;; ende-work-buffer-test.el --- Disposable Work viewer concepts -*- lexical-binding: t; -*-

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

;; Tests for disposable Work viewer concepts.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ende-work-buffer)
(require 'ende-queue-ui)

(defun ende-test-work--record (transcript)
  "Record the same mixed response for main and delegated TRANSCRIPT."
  (let ((call '(:id "provider-call" :name "inspect" :arguments "{\"path\":\"a.el\"}"
               :execution-id "execution" :store-file "/tmp/explicit-work-store.sqlite")))
    (ende-transcript-begin-response transcript)
    (ende-transcript-event transcript '(:type :delta :channel :content :text "A reply"))
    (ende-transcript-event transcript '(:type :reasoning-summary :item "reason" :text "A summary"))
    (ende-transcript-accept transcript '(:content "A reply") (list call))
    (ende-transcript-tool-result transcript call "{\"ok\":false,\"content\":\"one\\ntwo\"}")
    (ende-transcript-event transcript '(:type :agent-activity :title "Checking" :status "In progress"))
    (ende-transcript-finish transcript 'completed)))

(ert-deftest ende-test/work-disposable-main-child-and-dashboard-parity ()
  (let* ((main (ende--make-job :id 800 :state 'running :instruction '(:task "Task text")))
         (child (ende--make-job :id 801 :parent main :state 'completed :instruction '(:task "Task text")))
         (ende--retained-jobs (make-hash-table :test #'eql))
         (org-mode-hook (list (lambda () (setq-local ende-test-work-hook-ran t))))
         buffers)
    (unwind-protect
        (progn
          (puthash 800 main ende--retained-jobs)
          (puthash 801 child ende--retained-jobs)
          (with-temp-buffer
            (insert (propertize "main\n" 'ende-queue-ui-job-id 800))
            (goto-char (point-min))
            (cl-letf (((symbol-function 'pop-to-buffer) #'identity))
              (push (ende-queue-ui-visit-agent-work) buffers)))
          (ende-test-work--record (ende-job-ensure-transcript main))
          (ende-test-work--record (ende-job-ensure-transcript child))
          (should-not (ende-job-work-buffer child))
          (push (ende-work-buffer-open child) buffers)
          (should (equal (with-current-buffer (car buffers) (buffer-string))
                         (with-current-buffer (cadr buffers) (buffer-string))))
          (kill-buffer (ende-job-work-buffer main))
          (should-not (ende-job-work-buffer main))
          (should (eq (ende-job-state main) 'running))
          (setf (ende-job-state main) 'completed)
          (with-temp-buffer
            (insert (propertize "finished\n" 'ende-queue-ui-job-id 800))
            (goto-char (point-min))
            (cl-letf (((symbol-function 'pop-to-buffer) #'identity))
              (push (ende-queue-ui-visit-agent-work) buffers)))
          (should (equal (with-current-buffer (car buffers) (buffer-string))
                         (with-current-buffer (cadr buffers) (buffer-string))))
          (dolist (buffer (cl-remove-if-not #'buffer-live-p buffers))
            (with-current-buffer buffer
              (should (derived-mode-p 'org-mode))
              (should ende-work-buffer-mode)
              (should buffer-read-only)
              (should (bound-and-true-p ende-test-work-hook-ran))
              (should-not (string-prefix-p " " (buffer-name)))
              (should (eq (key-binding (kbd "TAB")) #'org-cycle))
              (should (eq (key-binding (kbd "r")) #'ende-work-buffer-inspect-raw))
              (should (= (hash-table-count ende-work-buffer--sections)
                         (length (org-element-map (org-element-parse-buffer) 'headline #'identity))))
              (should (= 1 (how-many "^\\*+ Tool: inspect" (point-min) (point-max))))
              (should (= 1 (how-many "^\\*+ Result (failed)" (point-min) (point-max)))))))
      (dolist (buffer buffers) (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest ende-test/work-decoded-json-is-safe-pure-and-raw-is-exact ()
  (should (equal (ende-work-buffer--readable-json "\"one\\ntwo\"") "\"one\ntwo\""))
  (should (equal (ende-work-buffer--readable-json "\"one\\\\ntwo\"") "\"one\\ntwo\""))
  (let* ((object (make-hash-table :test #'equal))
         (_ (puthash "multiline" "one\n#+end_example\n* forged\n,#+end_example" object))
         (_ (puthash "literal" "one\\ntwo" object))
         (arguments (json-serialize object))
         (transcript (ende-transcript-create :task "#+end_example\n* forged"))
         (call (list :id "call" :name "inspect" :arguments arguments
                     :execution-id "exact" :store-file "/tmp/entry-specific.sqlite"))
         (text (ende-transcript-new-entry transcript 'text "* forged\n#+end_example"
                                          '(:normalized (:content "Do not substitute prepared text"))))
         (artifact (ende-transcript-new-entry transcript 'text ""
                                              '(:normalized (:content "[[file:/tmp/generated.png]]"))))
         (tool (ende-transcript-new-entry transcript 'tool-call nil (list :call call)))
         (opaque (ende-transcript-new-entry transcript 'opaque nil
                                             (list :provider "provider" :kind "opaque-test" :item object)))
         (job (ende--make-job :id 802 :transcript transcript))
         (raw "not JSON\n#+end_example\n* forged")
         buffer result raw-buffer)
    (ende-transcript-begin-response transcript)
    (ende-transcript-accept transcript '(:content "original") (list call)
                            (list :items (list text artifact tool opaque)))
    (setq result (ende-transcript-tool-result transcript call raw))
    (ende-transcript-append transcript 'summary raw)
    (ende-transcript-append transcript 'progress raw (list :title "Title\n* forged"))
    (ende-transcript-finish transcript 'failed raw)
    (let ((before (prin1-to-string transcript)))
      (unwind-protect
          (cl-letf (((symbol-function 'ende-tool-store-put) (lambda (&rest _) (ert-fail "Renderer wrote storage")))
                    ((symbol-function 'ende-tool-store--new-execution-id) (lambda () (ert-fail "Renderer allocated an ID")))
                    ((symbol-function 'ende-tool-store-load-execution) (lambda (&rest _) (ert-fail "Renderer read storage")))
                    ((symbol-function 'ende-tool-store-save-opaque) (lambda (&rest _) (ert-fail "Renderer stored an item")))
                    ((symbol-function 'ende-artifact-prepare-text) (lambda (&rest _) (ert-fail "Renderer prepared artifacts")))
                    ((symbol-function 'pop-to-buffer) #'identity))
            (setq buffer (ende-work-buffer-open job))
            (with-current-buffer buffer
              (let ((rendered (buffer-string)))
                (should (string-match-p (regexp-quote "one\n,#+end_example\n,* forged\n,,#+end_example") rendered))
                (should (string-match-p (regexp-quote "\"literal\": \"one\\ntwo\"") rendered))
                (should (string-match-p (regexp-quote "[[file:/tmp/generated.png]]") rendered))
                (should-not (string-match-p "Do not substitute prepared text" rendered))
                (should-not (string-match-p "^\\* forged$" rendered))
                (should (= (length (org-element-map (org-element-parse-buffer) 'example-block #'identity)) 9)))
              (dolist (entry (list tool result))
                (goto-char (ende-work-buffer--section-start
                            (gethash (ende-transcript-entry-id entry) ende-work-buffer--sections)))
                (setq raw-buffer (ende-work-buffer-inspect-raw))
                (with-current-buffer raw-buffer
                  (should buffer-read-only)
                  (should (equal (buffer-string) (if (eq entry tool) arguments raw)))
                  (should (string-match-p "/tmp/entry-specific.sqlite" header-line-format)))))
            (should (eq (ende-work-buffer--raw-payload opaque) object))
            (should (equal (ende-work-buffer--raw-payload artifact) ""))
            (should (equal before (prin1-to-string transcript))))
        (when (buffer-live-p buffer) (kill-buffer buffer))
        (when (buffer-live-p raw-buffer) (kill-buffer raw-buffer))))
    (let* ((transcript (ende-transcript-create))
           (job (ende--make-job :id 806 :transcript transcript))
           (call '(:id "stored-call" :name "inspect" :execution-id "stored" :store-file "/tmp/exact.sqlite"))
           (ende-tool-store-file "/tmp/other-ambient.sqlite")
           (org-mode-hook (list (lambda () (setq-local ende-tool-store--local-file ende-tool-store-file))))
           (content "{\"content\":\"stored\\nresult\"}")
           loaded)
      (ende-transcript-accept transcript '(:content "") (list call))
      (ende-transcript-tool-result transcript call nil)
      (unwind-protect
          (cl-letf (((symbol-function 'ende-tool-store--filename) (lambda () (ert-fail "Used ambient store")))
                    ((symbol-function 'ende-tool-store-load-execution)
                     (lambda (id file)
                       (push (list id file) loaded)
                       (list :arguments arguments :content content))))
            (setq buffer (ende-work-buffer-open job))
            (with-current-buffer buffer
              (should (equal ende-tool-store--local-file "/tmp/other-ambient.sqlite"))
              (should (string-match-p (regexp-quote "\"multiline\": \"one\n,#+end_example") (buffer-string)))
              (should (string-match-p (regexp-quote "\"content\": \"stored\nresult\"") (buffer-string))))
            (should (equal loaded '(("stored" "/tmp/exact.sqlite") ("stored" "/tmp/exact.sqlite")))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest ende-test/work-result-status-requires-an-explicit-outer-boolean ()
  (let* ((transcript (ende-transcript-create))
         (job (ende--make-job :id 807 :transcript transcript))
         (call '(:id "call" :name "inspect" :arguments "" :execution-id "status" :store-file "/tmp/unused.sqlite"))
         (cases '((" { \"ok\" : true } " succeeded)
                  ("{\"ok\":false,\"result\":{\"ok\":true}}" failed)
                  ("{\"ok\":\"false\"}" returned)
                  ("{\"ok\":null}" returned)
                  ("{\"result\":{\"ok\":true}}" returned)
                  ("[{\"ok\":true}]" returned)
                  ("success: {\"ok\":true}" returned)
                  ("{\"ok\":true} trailing" returned)
                  ("returned, not necessarily successful" returned)
                  ("" returned)))
         buffer)
    (ende-transcript-accept transcript '(:content "") (list call))
    (dolist (case cases)
      (let ((entry (ende-transcript-tool-result transcript call (car case))))
        (should (eq (ende-transcript-entry-status entry) (cadr case)))
        (should (eq (ende-transcript-entry-content entry) (car case)))))
    (unwind-protect
        (cl-letf (((symbol-function 'ende-tool-store-load-execution)
                   (lambda (&rest _) (ert-fail "Existing raw payload loaded storage"))))
          (setq buffer (ende-work-buffer-open job))
          (with-current-buffer buffer
            (should (= 1 (how-many "^\\*+ Result (succeeded)" (point-min) (point-max))))
            (should (= 1 (how-many "^\\*+ Result (failed)" (point-min) (point-max))))
            (should (= 8 (how-many "^\\*+ Result (returned)" (point-min) (point-max))))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest ende-test/work-new-details-fold-without-overriding-reader-expansion ()
  (dolist (lazy '(nil t))
    (let* ((job (ende--make-job :id 804 :instruction '(:task "Task")))
           (transcript (ende-job-ensure-transcript job))
           (call '(:id "call" :name "inspect" :arguments "{}" :execution-id "folds"))
           buffer response tool opaque summary progress result)
      (unwind-protect
          (progn
            (unless lazy (setq buffer (ende-work-buffer-open job)))
            (ende-transcript-begin-response transcript)
            (setq tool (ende-transcript-new-entry transcript 'tool-call nil (list :call call))
                  opaque (ende-transcript-new-entry transcript 'opaque nil
                                                    '(:provider "test" :kind "details" :item "payload"))
                  response (ende-transcript-accept
                            transcript '(:content "") (list call)
                            (list :items (list tool opaque))))
            (ende-transcript-event transcript '(:type :reasoning-summary :item "reason" :text "First"))
            (setq summary (car (last (ende-transcript-entries transcript)))
                  progress (ende-transcript-append transcript 'progress "First" '(:title "Checking")))
            (when lazy (setq buffer (ende-work-buffer-open job)))
            (with-current-buffer buffer
              (cl-labels ((section (entry)
                            (gethash (ende-transcript-entry-id entry) ende-work-buffer--sections))
                          (expand (section)
                            (goto-char (ende-work-buffer--section-start section))
                            (org-fold-show-subtree)))
                (should-not (ende-work-buffer--folded-p (section response)))
                (dolist (entry (list tool opaque summary progress))
                  (should (ende-work-buffer--folded-p (section entry))))
                (expand (section tool))
                (expand (section opaque))
                (expand (section summary))
                (expand (section progress))
                (ende-transcript-notify transcript response)
                (ende-transcript-event transcript '(:type :reasoning-summary :item "reason" :text " second"))
                (setf (ende-transcript-entry-content progress) "Updated")
                (ende-transcript-notify transcript progress)
                (dolist (entry (list tool opaque summary progress))
                  (should-not (ende-work-buffer--folded-p (section entry))))
                (let ((arguments (gethash (cons 'arguments (ende-transcript-entry-id tool))
                                          ende-work-buffer--sections)))
                  (should-not (ende-work-buffer--folded-p arguments))
                  (goto-char (ende-work-buffer--section-start arguments))
                  (org-fold-hide-subtree)
                  (setq result (ende-transcript-tool-result transcript call "First result"))
                  (should (ende-work-buffer--folded-p arguments))
                  (should (ende-work-buffer--folded-p (section result)))
                  (should-not (ende-work-buffer--folded-p (section tool))))
                (expand (section result))
                (setf (ende-transcript-entry-content result) "Updated result")
                (ende-transcript-notify transcript result)
                (should-not (ende-work-buffer--folded-p (section result)))
                (setq progress (ende-transcript-append transcript 'progress "Next" '(:title "New activity")))
                (should (ende-work-buffer--folded-p (section progress))))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest ende-test/work-final-reply-is-labelled-and-recreated ()
  (let* ((job (ende--make-job :id 805))
         (transcript (ende-job-ensure-transcript job))
         (buffer (ende-work-buffer-open job))
         rendered)
    (unwind-protect
        (progn
          (ende-transcript-begin-response transcript)
          (ende-transcript-event transcript '(:type :delta :channel :content :text "Final answer"))
          (ende-transcript-accept transcript '(:content "Final answer") nil nil t)
          (ende-transcript-finish transcript 'completed)
          (with-current-buffer buffer
            (should (= 1 (how-many "^\\* Response 1 (complete) — Final reply$" (point-min) (point-max))))
            (should (= 1 (how-many "Final answer" (point-min) (point-max))))
            (setq rendered (buffer-string)))
          (kill-buffer buffer)
          (setq buffer (ende-work-buffer-open job))
          (should (equal rendered (with-current-buffer buffer (buffer-string)))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest ende-test/work-incremental-fontification-folds-and-following ()
  (let* ((job (ende--make-job :id 803))
         (transcript (ende-job-ensure-transcript job))
         (buffer (ende-work-buffer-open job)))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) buffer)
          (let ((reader (selected-window)) (following (split-window-below)))
            (set-window-buffer following buffer)
            (setf (ende-transcript-task transcript) "Keep this task")
            (ende-transcript-begin-response transcript)
            (ende-transcript-event transcript '(:type :delta :channel :content :text "Read this\n#"))
            (with-current-buffer buffer
              (should (string-match-p "Keep this task" (buffer-string)))
              (let* ((response (ende-transcript-response transcript))
                     (root (gethash (ende-transcript-entry-id response) ende-work-buffer--sections))
                     (text (gethash (cons 'stream (ende-transcript-entry-id response)) ende-work-buffer--sections))
                     (task (gethash 'task ende-work-buffer--sections))
                     (reader-point (+ 2 (ende-work-buffer--section-body-start text)))
                     (sentinel (make-overlay 3 5)))
                (goto-char (ende-work-buffer--section-start task))
                (org-fold-hide-subtree)
                (goto-char reader-point)
                (set-window-point reader reader-point)
                (set-window-point following (point-max))
                (ende-transcript-event transcript '(:type :delta :channel :content :text "+end_example\nMore text"))
                (should (eq text (gethash (cons 'stream (ende-transcript-entry-id response)) ende-work-buffer--sections)))
                (should (= (overlay-start sentinel) 3))
                (should (= (point) reader-point))
                (should (= (window-point reader) reader-point))
                (should (= (window-point following) (point-max)))
                (should (ende-work-buffer--folded-p task))
                (should-not (ende-work-buffer--folded-p root))
                (should (eq (get-text-property 3 'face) 'org-level-1))
                (should (string-match-p (regexp-quote "\n,#+end_example\nMore text") (buffer-string)))
                (goto-char (ende-work-buffer--section-start root))
                (org-fold-hide-subtree)
                (ende-transcript-event transcript '(:type :delta :channel :content :text " again"))
                (should (ende-work-buffer--folded-p root))
                (ende-transcript-append transcript 'progress "Still running" '(:title "Next"))
                (let* ((progress (car (last (ende-transcript-entries transcript))))
                       (section (gethash (ende-transcript-entry-id progress) ende-work-buffer--sections)))
                  (should-not (invisible-p (ende-work-buffer--section-start section)))
                  (should (eq (get-text-property (+ 2 (ende-work-buffer--section-start section)) 'face)
                              'org-level-1)))
                (let ((call '(:id "call" :name "inspect" :arguments "{}" :execution-id "incremental")))
                  (ende-transcript-accept transcript (list :content (ende-transcript-entry-content response))
                                          (list call))
                  (let* ((items (plist-get (ende-transcript-entry-data response) :items))
                         (tool (gethash (ende-transcript-entry-id (cadr items)) ende-work-buffer--sections))
                         (arguments (gethash (cons 'arguments (ende-transcript-entry-id (cadr items)))
                                             ende-work-buffer--sections)))
                    (should (eq text (gethash (ende-transcript-entry-id (car items)) ende-work-buffer--sections)))
                    (should (ende-work-buffer--folded-p root))
                    (goto-char (ende-work-buffer--section-start root))
                    (org-fold-show-subtree)
                    (goto-char (ende-work-buffer--section-start arguments))
                    (org-fold-hide-subtree)
                    (ende-transcript-tool-result transcript call "returned, not necessarily successful")
                    (should (ende-work-buffer--folded-p arguments))
                    (should-not (ende-work-buffer--folded-p tool))
                    (should-not (ende-work-buffer--folded-p root))
                    (should (= 1 (how-many "^\\*+ Tool: inspect" (point-min) (point-max))))))
                (delete-overlay sentinel)))))
      (kill-buffer buffer))))

(provide 'ende-work-buffer-test)
;;; ende-work-buffer-test.el ends here
