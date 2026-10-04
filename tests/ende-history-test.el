;;; ende-history-test.el --- History tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Coverage for history recording, retention, and display.

;;; Code:

(require 'ert)
(require 'ende)
(require 'ende-test-helpers)
(require 'cl-lib)

(ert-deftest ende-test/history-retains-terminal-result ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" result"))))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (ende-insert-at-cursor)
			    (with-current-buffer ende-history-buffer-name
			      (let ((text (buffer-string)))
				(should (string-match-p "Output" text))
				(should (= (ende-test--count-matches "result" text) 1))
				(should (string-match-p "Source metadata:" text))
				(should (string-match-p "base" text))
				(should (eq buffer-undo-list t)))))))

(ert-deftest ende-test/history-retains-diagnostics ()
  (ende-test--reset)
  (let ((job (ende--make-job :id 42 :type 'chat)))
    (ende-history-event
     job '(:type :begin :messages (("user" . "question"))))
    (ende-history-event
     job '(:type :diagnostic :text "OpenAI Responses tools sent: read_file"))
    (ende-history-event
     job '(:type :diagnostic
		 :text "OpenAI Responses output items: function_call(read_file)"))
    (ende-history-event
     job '(:type :done :content "answer" :result (:content "answer")))
    (with-current-buffer ende-history-buffer-name
      (let ((text (buffer-string)))
        (should (string-match-p "\\*\\* Diagnostics" text))
        (should (string-match-p
                 "OpenAI Responses tools sent: read_file" text))
        (should (string-match-p
                 "OpenAI Responses output items: function_call(read_file)"
                 text))))))

(ert-deftest ende-test/history-omits-source-metadata-without-prompts ()
  (ende-test--reset)
  (let ((ende-history-retain-prompts nil)
        (ende-llm-function (ende-test--fake-llm '(" result"))))
    (ende-test--with-buffer "secret source"
			    (setq-local buffer-file-name "/tmp/secret.el")
			    (setq-local major-mode 'emacs-lisp-mode)
			    (goto-char (point-max))
			    (ende-insert-at-cursor)
			    (with-current-buffer ende-history-buffer-name
			      (let ((text (buffer-string)))
				(should-not (string-match-p "Source metadata:" text))
				(should-not (string-match-p "secret source" text)))))))

(ert-deftest ende-test/history-summarizes-attributed-usage ()
  (ende-test--reset)
  (let* ((usage
          (json-parse-string
           (concat
            "{\"input_tokens\":5002,"
            "\"output_tokens\":1216,"
            "\"total_tokens\":6218,"
            "\"attribution\":{\"items\":{"
            "\"msg_input_a\":{\"input_tokens\":4000,\"output_tokens\":0},"
            "\"msg_input_b\":{\"input_tokens\":1000,\"output_tokens\":0},"
            "\"rs_reasoning\":{\"input_tokens\":2,\"output_tokens\":145},"
            "\"msg_output\":{\"input_tokens\":0,\"output_tokens\":1071}"
            "}}}")
           :object-type 'hash-table))
         (ende-llm-function
          (ende-test--fake-llm
           (list (list :content "result"
                       :tokens 1216
                       :speed 38.24
                       :usage usage)))))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (ende-insert-at-cursor)
			    (with-current-buffer ende-history-buffer-name
			      (let ((text (buffer-string)))
				(should (string-match-p "Stream tokens: 1216" text))
				(should (string-match-p "Estimated: no" text))
				(should (string-match-p "Speed: 38.24 t/s" text))
				(should (string-match-p
					 "Messages: 3 (2 input, 1 output)" text))
				(should (string-match-p
					 "Tokens: 6,218 total (5,002 input, 1,216 output)" text))
				(should-not (string-match-p "Provider usage:" text))
				(should-not (string-match-p "msg_" text))
				(should-not (string-match-p "attribution" text)))))))

(ert-deftest ende-test/history-retains-failed-stream-and-summarizes-usage ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm
          '((:thinking "reasoning before failure"
		       :content ""
		       :tokens 4096
		       :speed 58.4
		       :usage (:prompt_tokens 12
					      :completion_tokens 4096
					      :reasoning_tokens 4000
					      :total_tokens 4108))))))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((job (ende-insert-at-cursor)))
			      (should (eq (ende-job-state job) 'failed)))
			    (with-current-buffer ende-history-buffer-name
			      (let ((text (buffer-string)))
				(should (string-match-p "Prompt" text))
				(should (string-match-p "base" text))
				(should (string-match-p "Thinking" text))
				(should (string-match-p "reasoning before failure" text))
				(should (string-match-p
					 "Tokens: 4,108 total (12 input, 4,096 output)" text))
				(should-not (string-match-p "reasoning_tokens" text))
				(should (string-match-p "empty completion" text))
				(should buffer-read-only)
				(should-error (insert "user edit") :type 'buffer-read-only)
				(let ((buffer-read-only nil))
				  (should-error (insert "user edit") :type 'user-error)))))))

(ert-deftest ende-test/history-retains-joined-partial-result-on-failure ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm
          '((:content ""
		      :partial (:content ""
					 :thinking "Subheadings: ** TODO"
					 :tokens 4096
					 :speed 57.1
					 :finish-reason "length"))))))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((job (ende-insert-at-cursor)))
			      (should (eq (ende-job-state job) 'failed)))
			    (with-current-buffer ende-history-buffer-name
			      (let ((text (buffer-string)))
				(should (string-match-p "Thinking" text))
				(should (string-match-p (regexp-quote "Subheadings: ** TODO")
							text))
				(should (string-match-p "Tokens: 4096" text))
				(should (string-match-p "Finish reason: length" text))
				(should-not (string-match-p "\\(^\\|\\n\\)[[:space:]]*data:"
							    text)))))))

(ert-deftest ende-test/history-command-displays-buffer ()
  (ende-test--reset)
  (let ((displayed nil))
    (cl-letf (((symbol-function 'display-buffer)
               (lambda (buffer &rest _) (setq displayed buffer))))
      (ende-history)
      (should (eq displayed (get-buffer ende-history-buffer-name)))))
  (should (eq (lookup-key ende-mode-map (kbd "C-c e"))
              #'ende-selector-open)))

(ert-deftest ende-test/history-terminal-cleanup-survives-disable-and-write-errors ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" result") t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (ende-insert-at-cursor)
			    (should (= (hash-table-count ende-history--entries) 1))
			    (let ((ende-history-enabled nil))
			      (ende-test--complete-call (car ende-test--calls)))
			    (should (= (hash-table-count ende-history--entries) 0))))
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" result") t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (ende-insert-at-cursor)
			    (with-current-buffer (ende-history--buffer)
			      (add-hook 'before-change-functions
					(lambda (&rest _) (error "history blocked")) nil t))
			    (ende-test--complete-call (car ende-test--calls))
			    (should (= (hash-table-count ende-history--entries) 0)))))

(ert-deftest ende-test/history-is-bounded-and-body-cannot-create-headings ()
  (ende-test--reset)
  (let ((ende-history-max-entries 2)
        (ende-llm-function
         (ende-test--fake-llm '(" one" " two" "\n* fake heading"))))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (dotimes (_ 3)
			      (ende-insert-at-cursor)
			      (ende-review-delete-overlay
			       (cl-find-if #'ende-review-overlay-p
					   (overlays-in (point-min) (point-max)))))
			    (with-current-buffer ende-history-buffer-name
			      (let ((text (buffer-string)))
				(should (= (ende-test--count-matches "^\\* Job " text) 2))
				(should-not (string-match-p "^\\* fake heading" text)))))))

(ert-deftest ende-test/history-replaces-oldest-entries-at-default-limit ()
  (ende-test--reset)
  (let ((ende-history-max-entries 100)
        (ende-llm-function
         (ende-test--fake-llm
          (cl-loop for index from 0 to 102
                   collect (format " item-%d" index)))))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (dotimes (_ 103)
			      (ende-insert-at-cursor)
			      (ende-review-delete-overlay
			       (cl-find-if #'ende-review-overlay-p
					   (overlays-in (point-min) (point-max)))))
			    (with-current-buffer ende-history-buffer-name
			      (let ((text (buffer-string)))
				(should (= (ende-test--count-matches "^\\* Job " text) 100))
				(should-not (string-match-p "^\\* Job 1:" text))
				(should-not (string-match-p "^\\* Job 3:" text))
				(should (string-match-p "^\\* Job 103:" text)))))))

(provide 'ende-history-test)

;;; ende-history-test.el ends here
