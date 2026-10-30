;;; ende-target-test.el --- Target capture tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Coverage for captured sources, metadata, and target conflicts.

;;; Code:

(require 'ert)
(require 'ende)
(require 'ende-test-helpers)
(require 'cl-lib)

(ert-deftest ende-test/target-scopes-evaluate-once-and-clean-up ()
  (with-temp-buffer
    (insert "source")
    (let ((target (ende-target-create :buffer (current-buffer) :start 2 :end 5 :cursor 2))
          (calls 0)
          (current-start 'caller))
      (unwind-protect
          (progn
            (ende-target-with-restriction (progn (cl-incf calls) target)
              (should (eq current-start 'caller)))
            (should (= calls 1))
            (should-error
             (ende-target-with-application (progn (cl-incf calls) target)
               (should (ende-target-applying target))
               (error "stop")))
            (should (= calls 2))
            (should-not (ende-target-applying target))
            (should (= (point-max) 7)))
        (ende-target-release target)))))

(ert-deftest ende-test/source-metadata-uses-file-basename-and-mode ()
  (ende-test--with-buffer "source"
			  (setq-local buffer-file-name "/ssh:user@example:/path/to/example.el")
			  (setq-local major-mode 'emacs-lisp-mode)
			  (let ((target (ende-target-create
					 :buffer (current-buffer)
					 :start (point-min)
					 :end (point-max)
					 :cursor (point-min)
					 :selected t)))
			    (unwind-protect
				(let ((metadata (ende-target-source-metadata target)))
				  (should (string= (plist-get metadata :name) "example.el"))
				  (should (string= (plist-get metadata :type) "file"))
				  (should (string= (plist-get metadata :major-mode)
						   "emacs-lisp-mode"))
				  (should-not (string-match-p "/" (plist-get metadata :name))))
			      (ende-target-release target)))))

(ert-deftest ende-test/source-metadata-bounds-buffer-names ()
  (ende-test--with-buffer "source"
			  (rename-buffer (concat "scratch\n" (make-string 300 ?x)) t)
			  (setq-local major-mode 'text-mode)
			  (let ((target (ende-target-create
					 :buffer (current-buffer)
					 :start (point-min)
					 :end (point-max)
					 :cursor (point-min))))
			    (unwind-protect
				(let ((name (plist-get (ende-target-source-metadata target)
						       :name)))
				  (should (string-prefix-p "scratch " name))
				  (should (<= (length name)
					      ende-target--source-metadata-field-max-length))
				  (should-not (string-match-p "[[:cntrl:]]" name)))
			      (ende-target-release target)))))

(ert-deftest ende-test/all-operations-include-source-metadata ()
  (dolist (type '(insert improve summarize write-anywhere insert-anywhere))
    (ende-test--reset)
    (let* ((response
            (pcase type
              ('insert "completion")
              ('improve "improved")
              ('summarize "summary")
              ('write-anywhere
               (concat "{\"version\":1,\"edits\":[{\"anchor\":\"test\","
                       "\"position\":\"replace\",\"text\":\"changed\"}]}"))
              ('insert-anywhere
               (concat "{\"version\":1,\"edits\":[{\"anchor\":\"test\","
                       "\"position\":\"before\",\"text\":\"X\"}]}"))))
           (ende-llm-function
            (ende-test--fake-llm (list response))))
      (ende-test--with-buffer "test"
			      (setq-local buffer-file-name "/tmp/example.el")
			      (setq-local major-mode 'emacs-lisp-mode)
			      (goto-char (point-max))
			      (pcase type
				('insert (ende-insert-at-cursor))
				('improve (ende-improve-at-cursor "Improve it"))
				('summarize (ende-summarize))
				('write-anywhere (ende-write-anywhere "Change it"))
				('insert-anywhere (ende-insert-anywhere "Insert it")))
			      (let ((user-message
				     (ende-test--user-message (car ende-test--prompts))))
				(should (string-prefix-p "Source metadata:\n" user-message))
				(should (string-match-p "Name: example\\.el" user-message))
				(should (string-match-p "Type: file" user-message))
				(should (string-match-p "Major mode: emacs-lisp-mode"
							user-message))
				(should (= 1 (ende-test--count-matches
					      "Source metadata:" user-message))))))))

(ert-deftest ende-test/source-name-disclosure-can-be-overridden ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("one" "two")))
        (ende-source-name-disclosure nil))
    (ende-test--with-buffer "source"
			    (rename-buffer "private-buffer" t)
			    (setq-local major-mode 'python-mode)
			    (ende-enqueue-operation 'insert)
			    (let ((without-name
				   (ende-test--user-message (car ende-test--prompts))))
			      (should-not (string-match-p "Name:" without-name))
			      (should (string-match-p "Type: buffer" without-name))
			      (should (string-match-p "Major mode: python-mode" without-name)))
			    (ende-approve-all-output)
			    (ende-enqueue-operation 'insert nil '(:source-name-disclosure t))
			    (let ((with-name
				   (ende-test--user-message (car ende-test--prompts))))
			      (should (string-match-p "Name: private-buffer" with-name))))))

(ert-deftest ende-test/source-metadata-counts-toward-request-budget ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("unused"))))
    (ende-test--with-buffer "source"
			    (setq-local buffer-file-name "/tmp/example.el")
			    (setq-local major-mode 'emacs-lisp-mode)
			    (let* ((target (ende-target-create
					    :buffer (current-buffer)
					    :start (point-min)
					    :end (point-max)
					    :cursor (point-min)))
				   (job (ende--make-job :type 'insert
							:target target
							:profile nil))
				   (messages (list (cons "system" "system")
						   (cons "user" "Document:\nsource")))
				   (base-size
				    (cl-loop for message in messages
					     sum (+ (length (format "%s" (car message)))
						    (length (format "%s" (cdr message)))))))
			      (unwind-protect
				  (let ((ende-max-request-chars base-size))
				    (should-error (ende--dispatch-llm job messages))
				    (should-not ende-test--calls))
				(ende-target-release target))))))

(ert-deftest ende-test/target-context-window-enforces-source-budget ()
  (let* ((ende-target-max-context-chars 30)
         (ende-target-window-chars 100)
         (context (ende-target-string-marker-context
                   (concat (make-string 50 ?A) (make-string 50 ?B)) 50))
         (source-chars (+ (cl-count ?A context) (cl-count ?B context))))
    (should (<= source-chars ende-target-max-context-chars))
    (should (string-match-p "<CURSOR>" context))
    (should (string-match-p "characters elided" context))))

(ert-deftest ende-test/large-replacement-targets-are-rejected-before-queueing ()
  (ende-test--reset)
  (let ((ende-target-max-context-chars 4))
    (ende-test--with-buffer "oversized"
			    (should-error (ende-improve-at-cursor "rewrite") :type 'user-error)
			    (should-error (ende-summarize) :type 'user-error)
			    (should-not ende-queue)
			    (should-not (ende-active-jobs))
			    (should-not ende-ui--managed-overlays))))

(ert-deftest ende-test/changed-selection-is-skipped ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("X") t)))
    (ende-test--with-buffer "original"
			    (ende-test--select 1 9 9)
			    (let ((job (ende-insert-at-cursor))
				  (call nil))
			      (setq call (car ende-test--calls))
			      (goto-char 3)
			      (delete-char 1)
			      (insert "X")
			      (ende-test--complete-call call)
			      (should (eq (ende-job-state job) 'failed))
			      (should-not (ende-active-jobs))))))

(ert-deftest ende-test/permitted-source-edits-do-not-invalidate-targets ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("summary") t)))
    (ende-test--with-buffer "source"
      (let* ((job (ende-summarize))
             (call (car ende-test--calls)))
        (goto-char (point-min))
        (delete-char 1)
        (insert "S")
        (should (eq (ende-job-state job) 'running))
        (ende-test--complete-call call)
        (should (eq (ende-job-state job) 'completed))
        (with-current-buffer ende-summary-buffer-name
          (should (string-match-p "summary" (buffer-string))))))))

(ert-deftest ende-test/direct-request-uses-and-snapshots-buffer-selection ()
  (ende-test--reset)
  (let ((ende-model-selection-scope 'buffer)
        (ende-model-selection--last-used nil)
        (ende-model-selection--buffer-selections
         (make-hash-table :test #'equal))
        (ende-model-selection--shared-control-preferences
         (make-hash-table :test #'equal))
        (ende-model-selection--buffer-control-preferences
         (make-hash-table :test #'equal))
        (ende-providers '(openrouter))
        (ende-provider-profiles nil)
        (ende-llm-function (ende-test--fake-llm '("summary" "summary"))))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert "Document to summarize")
            (ende-model-selection-save
             '(:provider openrouter :model "source-model"
                         :control-preferences (:reasoning-effort "high")))
            (let ((ende--queue-suspended t)
                  job)
              (setq job (ende-summarize))
              (ende-model-selection-save
               '(:provider local :model "later-model"))
              (setq ende--queue-suspended nil)
              (ende-process-next)
              (should (eq (ende-job-state job) 'completed))
              (should (eq (plist-get (ende-job-profile job) :name)
                          'openrouter))
              (should (equal (plist-get (ende-job-profile job) :model)
                             "source-model"))
              (should (equal
                       (plist-get (ende-job-profile job) :reasoning-effort)
                       "high"))))
            (with-temp-buffer
              (insert "Another document")
              (ende-model-selection-save
               '(:provider openrouter :model "stale-model"
                           :control-preferences (:reasoning-effort "high")))
              (let* ((job (ende-summarize))
                     (request-profile
                      (plist-get (car (last ende-test--calls)) :options)))
                (should (eq (ende-job-state job) 'completed))
                (should (= (length ende-test--calls) 2))
                (should (equal (plist-get request-profile :model)
                               "stale-model"))
                (should (equal (plist-get request-profile :reasoning-effort)
                               "high")))))
      (ende-test--reset))))

(ert-deftest ende-test/insert-anchor-does-not-follow-edits-at-point ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" X") t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let* ((job (ende-insert-at-cursor))
				   (call (car ende-test--calls))
				   (overlay (ende-job-overlay job)))
			      (dotimes (_ 5)
				(insert "\n"))
			      (should (= (overlay-start overlay) 5))
			      (should (= (ende-target-cursor (ende-job-target job))
					 5))
			      (ende-test--complete-call call)
			      (should (string= (buffer-string) "base X\n\n\n\n\n"))))))

(provide 'ende-target-test)

;;; ende-target-test.el ends here
