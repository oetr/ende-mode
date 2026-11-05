;;; ende-chat-benchmark.el --- Reproducible chat insertion benchmark -*- lexical-binding: t; -*-

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
;;   emacs -Q --batch -L lisp -l ende-chat-benchmark \
;;     -f ende-chat-benchmark-run
;; The benchmark uses fresh Org buffers for each mutating operation.  Parser
;; counts distinguish the target transcript from response-sized temporary
;; buffers, and timing is informational rather than a test threshold.

;;; Code:

(require 'ende-chat)

(defconst ende-chat-benchmark--profile
  '(:name local :model "benchmark-model" :parallel t :max-concurrency 2)
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
     "** Apparent heading inside source\n"
     ":PROPERTIES:\n:ENDE_ROLE: assistant\n:END:\n"
     "(message \"source stays opaque\")\n"
     "#+end_src\n"
     "#+begin_example\n"
     "*** Apparent heading inside example\n"
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
         (call (list :id (format "benchmark-call-%d" index)
                     :name "read_file"
                     :arguments (format "{\"path\":\"src/%d.el\"}" index)
                     :execution-id (format "benchmark-execution-%d" index)))
         (job (ende--make-job
               :type 'chat :profile ende-chat-benchmark--profile
               :instruction (list :response-level 2 :question-id question-id
                                  :request-id request-id)))
         (work '(:content "Inspecting a source file."))
         (reply '(:content "The source check is incomplete.")))
    (cl-letf (((symbol-function 'ende-chat--checkpoint-transcript) #'ignore))
      (ende-chat--record-response job work (list call) (list :normalized work))
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

(defun ende-chat-benchmark--with-transcript (section-count function)
  "Call FUNCTION in a fresh Org buffer containing SECTION-COUNT exchanges."
  (with-temp-buffer
    (org-mode)
    (insert (ende-chat-benchmark--transcript section-count))
    (funcall function (current-buffer))))

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
            (ende-llm-function
             (lambda (_messages callbacks _profile)
               (setq ende-chat-benchmark--callbacks callbacks))))
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

(provide 'ende-chat-benchmark)

;;; ende-chat-benchmark.el ends here
