;;; ende-chat-benchmark.el --- Reproducible chat benchmarks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter Samarin
;;
;; Author: Peter Samarin <peter.samarin@gmail.com>
;; Maintainer: Peter Samarin <peter.samarin@gmail.com>
;; Keywords: convenience, tools
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

;; Run with:
;;   emacs -Q --batch -L lisp -L benchmark -l ende-chat-benchmark \
;;     -f ende-chat-benchmark-run
;; `ende-chat-benchmark-startup-run' measures handoff to the first LLM call,
;; including the title phase, without completing that call.
;; The benchmark uses fresh Org buffers for each mutating operation.  Parser
;; counts distinguish the target transcript from response-sized temporary
;; buffers, and timing is informational rather than a test threshold.

;;; Code:

(require 'ende)

(defconst ende-chat-benchmark--profile
  '(:name local :model "benchmark-model" :parallel t :max-concurrency 2
    :api openai-chat-completions :auth-method none
    :url "http://127.0.0.1:8080/v1/chat/completions")
  "Profile used by the synthetic completion benchmark.")

(defun ende-chat-benchmark--conversation-section (index)
  "Return a synthetic user/assistant exchange numbered INDEX."
  (let ((question-id (format "benchmark-question-%d" index)))
    (concat
     (format "** User turn %d\n" index)
     ":PROPERTIES:\n"
     ":ENDE_ROLE: user\n"
     (format ":ENDE_CHAT_ID: %s\n" question-id)
     ":END:\n"
     (format "Ordinary prose with /emphasis/, [[https://example.test][a link]], \\alpha, and $x^2$. Turn %d.\n"
             index)
     (format "*** Nested detail %d\nNested section text.\n" index)
     "#+begin_src emacs-lisp\n"
     ",** Apparent heading inside source\n"
     ":PROPERTIES:\n:ENDE_ROLE: assistant\n:END:\n"
     "(message \"source stays opaque\")\n"
     "#+end_src\n"
     "#+begin_example\n"
     ",*** Apparent heading inside example\n"
     "#+end_example\n"
     (format "** Assistant turn %d\n" index)
     ":PROPERTIES:\n"
     ":ENDE_ROLE: assistant\n"
     (format ":BENCHMARK_CUSTOM: value-%d\n" index)
     ":END:\n"
     (format "A response with *strong text* and \"quoted\" prose. Turn %d.\n" index)
     ":NOTES:\nA drawer that is not Ende metadata.\n:END:\n")))

(defun ende-chat-benchmark--tool-section (index)
  "Return one generated tool-protocol sequence numbered INDEX."
  (let* ((question-id (format "benchmark-tool-question-%d" index))
         (request-id (format "benchmark-tool-request-%d" index))
         (call (ende-chat--prepare-tool-call
                (list :id (format "benchmark-call-%d" index)
                      :name "read_file"
                      :arguments (format "{\"path\":\"src/%d.el\"}" index))))
         (job (ende--make-job
               :type 'chat :profile ende-chat-benchmark--profile
               :instruction (list :response-level 2 :question-id question-id
                                  :request-id request-id
                                  :tool-store-file (ende-tool-store--filename))))
         (work '(:content "Inspecting a source file."))
         (reply '(:content "The source check is complete.")))
    (cl-letf (((symbol-function 'ende-chat--checkpoint-transcript) #'ignore))
      (ende-chat--record-response job work (list call) (list :normalized work))
      (ende-chat--record-tool-result job call "Source contents for the benchmark.")
      (ende-transcript-begin-response (ende-job-transcript job))
      (ende-chat--record-response job reply nil (list :normalized reply) t))
    (replace-regexp-in-string
     ":ENDE_CHAT_STATE: pending\n" "" (ende-chat--render-transcript job))))

(defun ende-chat-benchmark--transcript (section-count)
  "Return a synthetic Org transcript with SECTION-COUNT exchanges."
  (concat
   "* Synthetic conversation\nRoot context.\n"
   (mapconcat
    (lambda (index)
      (concat (ende-chat-benchmark--conversation-section index)
              (when (= (% index 20) 0)
                (ende-chat-benchmark--tool-section index))))
    (number-sequence 1 section-count)
    "")
   "** Benchmark completion question\n"
   ":PROPERTIES:\n:ENDE_ROLE: user\n:ENDE_CHAT_ID: benchmark-final-question\n:END:\n"
   "Please provide a concise answer.\n"))

(defun ende-chat-benchmark--measure (label buffer function)
  "Measure FUNCTION as LABEL in target BUFFER and print parse/GC details."
  (garbage-collect)
  (let ((whole-target-parses 0)
        (scoped-target-parses 0)
        (other-parses 0)
        (parse-function (symbol-function 'org-element-parse-buffer))
        (gc-count-before gcs-done)
        (gc-time-before gc-elapsed)
        (started (current-time)))
    (cl-letf (((symbol-function 'org-element-parse-buffer)
               (lambda (&rest arguments)
                 (if (eq (current-buffer) buffer)
                     (if (and (= (point-min) 1)
                              (= (point-max) (1+ (buffer-size))))
                         (cl-incf whole-target-parses)
                       (cl-incf scoped-target-parses))
                   (cl-incf other-parses))
                 (apply parse-function arguments))))
      (funcall function))
    (princ
     (format "%s: elapsed=%.3fs gc-count=%d gc-time=%.3fs whole-target-parses=%d scoped-target-parses=%d other-buffer-parses=%d\n"
             label
             (float-time (time-since started))
             (- gcs-done gc-count-before)
             (- gc-elapsed gc-time-before)
             whole-target-parses scoped-target-parses other-parses))))

(defun ende-chat-benchmark--call-isolated (function)
  "Call FUNCTION with a fresh Org buffer and private job/storage state."
  (let ((directory (make-temp-file "ende-chat-benchmark-" t)))
    (unwind-protect
        (let* ((user-emacs-directory (file-name-as-directory directory))
               (default-directory user-emacs-directory)
               (ende-tool-store-file (expand-file-name "tools.sqlite3" directory))
               (ende-tool-store--next-id ende-tool-store--next-id)
               (ende-chat--next-id ende-chat--next-id)
               (ende-queue nil) (ende--queue-tail nil)
               (ende--active-jobs nil) (ende--finished-families nil)
               (ende--retained-jobs (make-hash-table :test #'eql))
               (ende--settled-families nil) (ende--settlement-depth 0)
               (ende--next-job-id ende--next-job-id)
               (ende--next-dispatch-order ende--next-dispatch-order)
               (ende--next-fence-id ende--next-fence-id)
               (ende-queue-paused nil) (ende--queue-suspended nil)
               (ende--dispatching nil) (ende--aborting nil)
               (ende-ui--managed-overlays nil) (ende-ui--spinning-overlays nil)
               (ende-ui--spinner-timer nil) (ende-ui--spinner-index 0)
               (ende-queue-ui--buffer nil)
               (ende-queue-ui--marked-ids (make-hash-table :test #'eql))
               (ende-queue-ui--folds (make-hash-table :test #'eql))
               (ende-history--entries (make-hash-table :test #'eq))
               (ende-history--buffer-object nil)
               (ende-history-buffer-name
                (generate-new-buffer-name " *ende-benchmark-history*"))
               (ende-responses-ws--connections (make-hash-table :test #'equal))
               (ende-sandbox--by-owner (make-hash-table :test #'eql))
               (ende-sandbox--retired nil)
               (ende-agent--project-caches (make-hash-table :test #'eq))
               ;; Defer existing timer callbacks until this synchronous scope ends.
               (timer-list nil) (timer-idle-list nil))
          (with-temp-buffer
            (unwind-protect
                (progn
                  (org-mode)
                  (ende-tool-store--call-with-database t #'ignore)
                  (funcall function (current-buffer)))
              (unwind-protect
                  (let ((ende--queue-suspended t))
                    (ende-queue-cancel-ids
                     (mapcar #'ende-job-id (ende-all-live-jobs)))
                    (dolist (family (copy-sequence ende--finished-families))
                      (ende-queue-evict-family family)))
                (ende-ui-clear-all)
                (when (buffer-live-p ende-history--buffer-object)
                  (kill-buffer ende-history--buffer-object))
                (dolist (timer (append timer-list timer-idle-list))
                  (cancel-timer timer))))))
      (delete-directory directory t))))

(defun ende-chat-benchmark--with-transcript (section-count function)
  "Call FUNCTION in a fresh Org buffer containing SECTION-COUNT exchanges."
  (ende-chat-benchmark--call-isolated
   (lambda (buffer)
     (insert (ende-chat-benchmark--transcript section-count))
     (funcall function buffer))))

(defun ende-chat-benchmark--run-sample (section-count operation)
  "Run one fresh SECTION-COUNT sample for OPERATION."
  (pcase operation
    ('document
     (ende-chat-benchmark--with-transcript
      section-count
      (lambda (buffer)
        (ende-chat-benchmark--measure
         "document build" buffer #'ende-chat--build-document))))
    ('drawers
     (ende-chat-benchmark--with-transcript
      section-count
      (lambda (buffer)
        (ende-chat-benchmark--measure
         "full drawer hide" buffer #'ende-chat--hide-control-drawers))))
    ((or 'completion 'completion-with-reasoning)
     (let* ((ende-chat-benchmark--callbacks nil)
            (title-pending t)
            (ende-llm-function
             (lambda (_messages callbacks _profile)
               (if title-pending
                   (progn
                     (setq title-pending nil)
                     (funcall
                      (plist-get callbacks :on-done)
                      '(:tool-calls
                        ((:id "benchmark-title" :name "generate_title"
                          :arguments "{\"title\":\"Benchmark completion\"}")))))
                 (setq ende-chat-benchmark--callbacks callbacks)))))
       (ende-chat-benchmark--with-transcript
        section-count
        (lambda (buffer)
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "** Benchmark completion question\n")
            (org-back-to-heading t)
            (let* ((job (ende-ask ende-chat-benchmark--profile))
                   (result
                    (if (eq operation 'completion-with-reasoning)
                        '(:content "Benchmark answer with /emphasis/ and [[https://example.test][a link]]."
				   :thinking "Benchmark reasoning with *emphasis*.")
                      '(:content "Benchmark answer with /emphasis/ and [[https://example.test][a link]].")))
                   (callback (plist-get ende-chat-benchmark--callbacks :on-done)))
              (unless callback
                (error "Benchmark LLM callback was not captured"))
              (ende-chat-benchmark--measure
               (if (eq operation 'completion-with-reasoning)
                   "completion with reasoning"
                 "completion without reasoning")
               buffer
               (lambda () (funcall callback result)))
              (unless (eq (ende-job-state job) 'completed)
                (error "Benchmark response did not complete: %s"
                       (ende-job-state job)))))))))))

;;;###autoload
(defun ende-chat-benchmark-run (&optional section-count samples)
  "Benchmark chat document building and persistence on a synthetic transcript.
SECTION-COUNT defaults to 200 user/assistant exchanges; SAMPLES defaults to 3.
Mutating drawer and completion samples always use fresh buffers."
  (interactive)
  (let ((section-count (or section-count 200))
        (samples (or samples 3)))
    (unless (and (integerp section-count) (> section-count 0)
                 (integerp samples) (> samples 0))
      (error "SECTION-COUNT and SAMPLES must be positive integers"))
    (princ (format "Org %s; %d exchanges; %d samples\n"
                   (org-version) section-count samples))
    (dolist (operation '(document drawers completion completion-with-reasoning))
      (dotimes (_ samples)
        (ende-chat-benchmark--run-sample section-count operation)))))

(defconst ende-chat-benchmark-startup-scenarios
  '(plain deep unrelated-roots folded-hidden context-fence tool-heavy)
  "Org constellations supported by `ende-chat-benchmark-startup-run'.")

(defun ende-chat-benchmark--startup-fixture (size scenario)
  "Insert SIZE historical exchanges for SCENARIO and select the final question.
Return fixture dimensions before any job or timing is started."
  (unless (eq scenario 'unrelated-roots)
    (insert "* Synthetic conversation\nRoot context.\n"))
  (dotimes (offset size)
    (let* ((index (1+ offset))
           (section (ende-chat-benchmark--conversation-section index)))
      (pcase scenario
        ('deep
         (setq section
               (replace-regexp-in-string
                "^\\*+" (lambda (stars)
                            (concat (make-string (min offset 7) ?*) stars))
                section)))
        ('unrelated-roots
         (insert (format "* Unrelated root %d\nUNRELATED_HISTORY\n" index)))
        ('folded-hidden
         (when (zerop (% index 2))
           (setq section
                 (replace-regexp-in-string
                  ":PROPERTIES:\n"
                  ":PROPERTIES:\n:ENDE_VISIBILITY: hidden\n" section t t)))))
      (insert section)
      (when (eq scenario 'tool-heavy)
        (insert (ende-chat-benchmark--tool-section index)))))
  (pcase scenario
    ('unrelated-roots (insert "* Synthetic conversation\nRoot context.\n"))
    ('context-fence
     (insert "** Summary\n:PROPERTIES:\n:ENDE_ROLE: assistant\n"
             ":ENDE_CHAT_CONTEXT_FENCE: t\n:END:\nFENCE_SUMMARY\n")))
  (insert (make-string (if (eq scenario 'deep) (+ 2 (min size 7)) 2) ?*)
          " Benchmark startup question\n:PROPERTIES:\n:ENDE_ROLE: user\n:END:\n"
          "BENCHMARK_STARTUP_QUESTION: Please provide a concise answer.\n")
  (org-fold-show-all)
  (when (eq scenario 'folded-hidden)
    (ende-chat--hide-control-drawers)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\*\\* \\(User\\|Assistant\\) turn" nil t)
        (org-fold-hide-subtree))))
  (goto-char (point-max))
  (org-back-to-heading t)
  (let ((headings 0) (roots 0) (depth 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\(\\*+\\) " nil t)
        (let ((level (length (match-string 1))))
          (cl-incf headings)
          (when (= level 1) (cl-incf roots))
          (setq depth (max depth level)))))
    (list :bytes (string-bytes (buffer-string)) :headings headings
          :roots roots :max-level depth
          :folded-subtrees (if (eq scenario 'folded-hidden) (* 2 size) 0)
          :hidden-subtrees (if (eq scenario 'folded-hidden) (* 2 (/ size 2)) 0)
          :fences (if (eq scenario 'context-fence) 1 0)
          :tool-pairs (if (eq scenario 'tool-heavy) size 0)
          :db-bytes (file-attribute-size
                     (file-attributes (ende-tool-store--filename))))))

(defun ende-chat-benchmark--startup-sample (size scenario)
  "Measure handoff to first LLM entry for SIZE and SCENARIO."
  (ende-chat-benchmark--call-isolated
   (lambda (_buffer)
     (let ((fixture (ende-chat-benchmark--startup-fixture size scenario))
           (ende-chat-context-policy 'root-prefix)
           (ende-chat-include-reasoning nil)
           (ende-max-request-chars nil)
           (dispatches 0)
           started gc-count-before gc-time-before elapsed gc-count gc-seconds
           prompt request-profile job)
       (let ((ende-llm-function
              (lambda (messages _callbacks profile)
                (let ((stopped (current-time)))
                  (when (zerop dispatches)
                    (setq elapsed (float-time (time-subtract stopped started))
                          gc-count (- gcs-done gc-count-before)
                          gc-seconds (- gc-elapsed gc-time-before))))
                (cl-incf dispatches)
                (setq prompt messages request-profile profile))))
         (garbage-collect)
         (setq gc-count-before gcs-done gc-time-before gc-elapsed)
         (setq started (current-time))
         (setq job (ende-ask ende-chat-benchmark--profile)))
       (unless (and (= dispatches 1) (ende-job-current-p job)
                    (eq (plist-get request-profile :request-phase) 'title)
                    (equal (car (last prompt))
                           (cons "user" ende-agent--title-control)))
         (error "Startup benchmark expected one title dispatch, got %d (%s): %S"
                dispatches (ende-job-state job) (ende-job-error job)))
       (let ((text (ende-chat--render-context prompt))
             (calls 0) (results 0) (history 0) (position 0))
         (dolist (message prompt)
           (cl-incf calls (length (plist-get message :tool-calls)))
           (when (equal (ende-content-role message) "tool")
             (cl-incf results)))
         (while (string-match "User turn [0-9]+" text position)
           (cl-incf history)
           (setq position (match-end 0)))
         (unless (and (string-match-p "BENCHMARK_STARTUP_QUESTION" text)
                      (= history (pcase scenario
                                   ((or 'unrelated-roots 'context-fence) 0)
                                   ('folded-hidden (/ (1+ size) 2))
                                   (_ size)))
                      (= calls (plist-get fixture :tool-pairs))
                      (= results calls)
                      (not (string-match-p "UNRELATED_HISTORY" text))
                      (or (not (eq scenario 'context-fence))
                          (string-match-p "FENCE_SUMMARY" text)))
           (error "Unexpected startup benchmark prompt for %s/%d" scenario size)))
       (list :fixture fixture :elapsed elapsed :gc-count gc-count
             :gc-seconds gc-seconds :dispatches dispatches
             :phase (plist-get request-profile :request-phase))))))

(defun ende-chat-benchmark--timing-summary (samples)
  "Return elapsed min/median/max and total GC for SAMPLES."
  (let* ((times (sort (mapcar (lambda (sample) (plist-get sample :elapsed))
                            samples) #'<))
         (count (length times))
         (middle (/ count 2)))
    (list :min (car times)
          :median (if (cl-oddp count) (nth middle times)
                    (/ (+ (nth (1- middle) times) (nth middle times)) 2.0))
          :max (car (last times))
          :gc-count (cl-loop for sample in samples sum (plist-get sample :gc-count))
          :gc-seconds (cl-loop for sample in samples sum (plist-get sample :gc-seconds)))))

;;;###autoload
(defun ende-chat-benchmark-startup-run (&optional sizes samples scenarios)
  "Benchmark handoff immediately before `ende-ask' to first LLM entry.
SIZES defaults to (10 100) historical exchanges; SAMPLES defaults to 3.
SCENARIOS defaults to `ende-chat-benchmark-startup-scenarios'.  Deep history
uses up to eight nested history levels; unrelated roots move all history out
of the selected conversation.  Tool-heavy adds a stored tool pair per exchange.
Fixture construction, folding, database writes, and explicit GC are not timed.
Normal job hooks run, but no title is completed and no network is contacted.
Print raw/summary seconds and GC, and return a plist with versions and cases."
  (interactive)
  (let ((sizes (or sizes '(10 100)))
        (samples (or samples 3))
        (scenarios (or scenarios ende-chat-benchmark-startup-scenarios))
        cases)
    (unless (and (consp sizes) (proper-list-p sizes)
                 (cl-every (lambda (size) (and (integerp size) (> size 0))) sizes)
                 (integerp samples) (> samples 0)
                 (consp scenarios) (proper-list-p scenarios)
                 (cl-every (lambda (scenario)
                             (memq scenario ende-chat-benchmark-startup-scenarios))
                           scenarios))
      (error "Expected positive SIZES/SAMPLES and supported startup SCENARIOS"))
    (princ (format "Startup: Emacs %s; Org %s; %d samples; seconds to first title dispatch\n"
                   emacs-version (org-version) samples))
    (dolist (size sizes)
      (dolist (scenario scenarios)
        (let (raw fixture)
          (dotimes (index samples)
            (let ((sample (ende-chat-benchmark--startup-sample size scenario)))
              (setq fixture (plist-get sample :fixture))
              (cl-remf sample :fixture)
              (push sample raw)
              (princ
               (format "raw size=%d scenario=%s sample=%d fixture=%S elapsed=%.6f gc=%d/%.6fs\n"
                       size scenario (1+ index) fixture (plist-get sample :elapsed)
                       (plist-get sample :gc-count) (plist-get sample :gc-seconds)))))
          (setq raw (nreverse raw))
          (let ((summary (ende-chat-benchmark--timing-summary raw)))
            (push (list :size size :scenario scenario :fixture fixture
                        :samples raw :summary summary) cases)
            (princ (format "summary size=%d scenario=%s n=%d min/median/max=%.6f/%.6f/%.6fs gc-total=%d/%.6fs\n"
                           size scenario samples (plist-get summary :min)
                           (plist-get summary :median) (plist-get summary :max)
                           (plist-get summary :gc-count) (plist-get summary :gc-seconds)))))))
    (list :emacs-version emacs-version :org-version (org-version)
          :cases (nreverse cases))))

(provide 'ende-chat-benchmark)

;;; ende-chat-benchmark.el ends here
