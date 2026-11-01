;;; ende-summary-test.el --- Summary tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Coverage for summaries, citations, progress, and source links.

;;; Code:

(require 'ert)
(require 'ende)
(require 'ende-test-helpers)
(require 'cl-lib)

(ert-deftest ende-test/summarize-opens-buffer ()
  (ende-test--reset)
  (let ((ende-custom-providers (ende-test--custom-profile 'local :models '("gpt-5.6-luna")))
        (ende-llm-function
         (ende-test--fake-llm '("** Key point\n- short summary")))
        (displayed nil))
    (cl-letf (((symbol-function 'display-buffer)
               (lambda (buffer &rest _)
                 (setq displayed buffer)))
              ((symbol-function 'format-time-string)
               (lambda (&rest _) "2026-09-18 11:10:07")))
      (ende-test--with-buffer "long source"
			      (rename-buffer "*summary-source*" t)
			      (let ((job (ende-summarize)))
				(should (eq (ende-job-state job) 'completed))
				(should (eq displayed (get-buffer ende-summary-buffer-name)))
				(with-current-buffer ende-summary-buffer-name
				  (should (eq major-mode 'org-mode))
				  (should (string=
					   (buffer-string)
					   (concat "* **summary-source** - "
						   "2026-09-18 11:10:07\n"
						   "File: [[ende-buffer:s1::1][*summary-source*]]\n"
						   "Model: gpt-5.6-luna\n"
						   "\n"
						   "** Key point\n- short summary")))
				  (should-not (ende-review--overlays-in-buffer))))))))

(ert-deftest ende-test/summary-entries-are-appended-as-org-headings ()
  (ende-test--reset)
  (let ((first (generate-new-buffer "first.org"))
        (second (generate-new-buffer "second.org")))
    (unwind-protect
        (let ((first-source (ende-summary-source-capture first 1 1))
              (second-source (ende-summary-source-capture second 1 1)))
          (cl-letf (((symbol-function 'format-time-string)
                     (lambda (&rest _) "2026-09-18 11:10:07")))
            (ende-summary-append
             first-source "First summary" '(:model "one"))
            (ende-summary-append
             second-source "Second summary" '(:model "two"))))
      (kill-buffer first)
      (kill-buffer second)))
  (with-current-buffer ende-summary-buffer-name
    (should (string=
             (buffer-string)
             (concat "* *first.org* - 2026-09-18 11:10:07\n"
                     "File: [[ende-buffer:s1::1][first.org]]\n"
                     "Model: one\n"
                     "\n"
                     "First summary\n\n"
                     "* *second.org* - 2026-09-18 11:10:07\n"
                     "File: [[ende-buffer:s2::1][second.org]]\n"
                     "Model: two\n"
                     "\n"
                     "Second summary")))))

(ert-deftest ende-test/summarize-reports-progress-and-points-at-new-entry ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("summary") t)))
    (with-current-buffer (ende-summary-buffer)
      (insert "existing")
      (goto-char (point-min)))
    (ende-test--with-buffer "source"
			    (let* ((job (ende-summarize))
				   (call (car ende-test--calls))
				   (overlay (ende-job-overlay job)))
			      (ende-test--callback
			       call :on-event '(:type :status :status "Generating"))
			      (ende-test--callback
			       call :on-event '(:type :usage :tokens 12 :speed 34.0))
			      (should (string= (overlay-get overlay 'status) "Generating"))
			      (should (= (overlay-get overlay 'tokens) 12))
			      (ende-test--complete-call call)))
    (with-current-buffer ende-summary-buffer-name
      (should (= (point)
                 (save-excursion
                   (goto-char (point-min))
                   (search-forward "\n\n")
                   (point)))))))

(ert-deftest ende-test/summarize-survives-source-edit-during-generation ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("summary") t)))
    (ende-test--with-buffer "source"
			    (let* ((job (ende-summarize))
				   (call (car ende-test--calls)))
			      (goto-char (point-min))
			      (delete-char 1)
			      (insert "S")
			      (should (eq (ende-job-state job) 'running))
			      (should (string= (buffer-string) "Source"))
			      (should (string-match-p
				       "Line-numbered text to summarize:\n1 | source"
				       (ende-test--user-message
					(car ende-test--prompts))))
			      (ende-test--complete-call call)
			      (should (eq (ende-job-state job) 'completed))))
    (with-current-buffer ende-summary-buffer-name
      (should (string-match-p "summary" (buffer-string))))))

(ert-deftest ende-test/summarize-captures-source-name-across-async-callback ()
  (ende-test--reset)
  (let ((ende-custom-providers (ende-test--custom-profile 'local :models '("async-model")))
        (ende-llm-function (ende-test--fake-llm '("summary") t)))
    (with-temp-buffer
      (rename-buffer "*phrases*" t)
      (insert "source")
      (let* ((source-name (buffer-name))
             (job (ende-summarize))
             (call (car ende-test--calls)))
        (rename-buffer "*renamed-phrases*" t)
        (with-temp-buffer
          (cl-letf (((symbol-function 'format-time-string)
                     (lambda (&rest _) "2026-09-18 11:10:07")))
            (ende-test--complete-call call)))
        (should (eq (ende-job-state job) 'completed))
        (with-current-buffer ende-summary-buffer-name
          (should (string-prefix-p
                   (format (concat "* *%s* - 2026-09-18 11:10:07\n"
                                   "File: [[ende-buffer:s1::1][%s]]\n"
                                   "Model: async-model\n\n"
                                   "summary")
                           source-name
                           source-name)
                   (buffer-string))))))))

(ert-deftest ende-test/summary-append-is-atomic ()
  (ende-test--reset)
  (with-current-buffer (ende-summary-buffer)
    (insert "existing"))
  (let ((source (ende-summary-source-capture (current-buffer) 1 1)))
    (should-error (ende-summary-append source 'invalid nil)))
  (with-current-buffer ende-summary-buffer-name
    (should (string= (buffer-string) "existing"))))

(ert-deftest ende-test/summarize-requests-org-mode-syntax ()
  (should (string-match-p "Org-mode summary." ende-summarize-system))
  (should (string-match-p "ende-source:LINE" ende-summarize-system)))

(ert-deftest ende-test/summarize-rewrites-file-citations-locally ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm
          '("Claim [[ende-source:3][source]]. [[ende-source:2][important claim]]."))))
    (ende-test--with-buffer "ignored\nfirst\nsecond"
			    (setq-local buffer-file-name "/tmp/ende source.org")
			    (ende-test--select 9 (point-max) (point-max))
			    (ende-summarize)
			    (let ((prompt (ende-test--user-message
					   (car ende-test--prompts))))
			      (should (string-match-p "Source lines: 2-3" prompt))
			      (should (string-match-p "2 | first\n3 | second" prompt))
			      (should-not (string-match-p "/tmp/ende source\\.org" prompt)))))
  (with-current-buffer ende-summary-buffer-name
    (should
     (string-match-p
      (regexp-quote
       "[[file:/tmp/ende source.org::2][/tmp/ende source.org]]")
      (buffer-string)))
    (should
     (string-match-p
      (regexp-quote "Claim [[file:/tmp/ende source.org::3][source]].")
      (buffer-string)))
    (should
     (string-match-p
      (regexp-quote
       "[[file:/tmp/ende source.org::2][important claim]]")
      (buffer-string)))
    (should-not (string-match-p "ende-source:3" (buffer-string)))))

(ert-deftest ende-test/summarize-preserves-tramp-paths-in-local-links ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm
          '("Remote claim [[ende-source:1][source]]"))))
    (ende-test--with-buffer "remote"
			    (setq-local buffer-file-name "/ssh:user@example:/tmp/remote file.org")
			    (ende-summarize)
			    (should-not
			     (string-match-p
			      "ssh:user@example"
			      (ende-test--user-message (car ende-test--prompts))))))
  (with-current-buffer ende-summary-buffer-name
    (should
     (string-match-p
      (regexp-quote
       "[[file:/ssh:user@example:/tmp/remote file.org::1][source]]")
      (buffer-string)))))

(ert-deftest ende-test/summarize-neutralizes-out-of-range-citations ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm
          '("Claim [[ende-source:99][source]]."))))
    (ende-test--with-buffer "one line"
			    (ende-summarize)))
  (with-current-buffer ende-summary-buffer-name
    (should (string-match-p (regexp-quote "Claim [source line 99].")
                            (buffer-string)))
    (should-not (string-match-p "ende-source:99" (buffer-string)))))

(ert-deftest ende-test/summary-buffer-links-track-renamed-live-buffers ()
  (ende-test--reset)
  (let ((source-buffer (generate-new-buffer "ende-link-source")))
    (unwind-protect
        (let (source)
          (with-current-buffer source-buffer
            (insert "first\nsecond\n")
            (setq source
                  (ende-summary-source-capture
                   source-buffer (point-min) (point-max)))
            (rename-buffer "ende-link-source-renamed" t))
          (save-current-buffer
            (cl-letf (((symbol-function 'pop-to-buffer)
                       (lambda (buffer &rest _)
                         (set-buffer buffer))))
              (ende-summary-follow-buffer-link
               (format "%s::2" (ende-summary-source-id source)) nil)
              (should (eq (current-buffer) source-buffer))
              (should (= (line-number-at-pos nil t) 2)))))
      (kill-buffer source-buffer))))

(ert-deftest ende-test/summary-buffer-links-reject-killed-buffers ()
  (ende-test--reset)
  (let* ((buffer (generate-new-buffer "ende-dead-source"))
         (source (ende-summary-source-capture buffer 1 1))
         (path (format "%s::1" (ende-summary-source-id source))))
    (kill-buffer buffer)
    (should-error (ende-summary-follow-buffer-link path nil)
                  :type 'user-error)))

(ert-deftest ende-test/summary-uses-reported-model ()
  (ende-test--reset)
  (let ((ende-custom-providers (ende-test--custom-profile 'local :models '("configured-model")))
        (ende-llm-function
         (ende-test--fake-llm
          '((:content "summary"
		      :reported-model "actual-model"
		      :reported-provider "llama.cpp")))))
    (cl-letf (((symbol-function 'format-time-string)
               (lambda (&rest _) "2026-09-18 11:10:07")))
      (ende-test--with-buffer "source"
			      (rename-buffer "reported-source" t)
			      (let ((job (ende-summarize)))
				(should (eq (ende-job-state job) 'completed)))))
    (with-current-buffer ende-summary-buffer-name
      (should (string-prefix-p
               (concat "* *reported-source* - 2026-09-18 11:10:07\n"
                       "File: [[ende-buffer:s1::1][reported-source]]\n"
                       "Model: actual-model\n\n")
               (buffer-string))))))

(ert-deftest ende-test/summary-runs-over-pending-review ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("summary"))))
    (ende-test--with-buffer "generated text"
			    (let ((review (ende-review-create-overlay
					   (current-buffer) 1 (point-max) nil nil "")))
			      (let ((job (ende-summarize)))
				(should (eq (ende-job-state job) 'completed))
				(should (ende-review-overlay-p review))
				(should (string-match-p
					 "generated text"
					 (ende-test--user-message (car ende-test--prompts))))
				(with-current-buffer ende-summary-buffer-name
				  (should (string-match-p "summary" (buffer-string)))))))))

(provide 'ende-summary-test)

;;; ende-summary-test.el ends here
