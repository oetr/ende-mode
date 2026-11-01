;;; ende-rubber-duck-test.el --- Rubber-duck tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Coverage for the rubber-duck worksheet review workflow.

;;; Code:

(require 'ert)
(require 'ende)
(require 'ende-test-helpers)
(require 'cl-lib)

;;;; Rubber-duck exercises

(defconst ende-test--duck-assessment
  "\"assessment\":{\"score\":3,\"correctness\":\"Accurate main idea.\",\"coverage\":\"Explain the boundary condition.\"}"
  "Valid assessment fields for mocked rubber-duck responses.")

(defun ende-test--duck-result (&optional edits)
  "Return a rubber-duck response with JSON EDITS, defaulting to none."
  (format "{\"version\":1,\"edits\":%s,%s}"
          (or edits "[]") ende-test--duck-assessment))

(defmacro ende-test--with-duck (responses &rest body)
  "Run BODY with a disposable worksheet and asynchronous RESPONSES."
  (declare (indent 1) (debug t))
  `(progn
     (ende-test--reset)
     (let ((ende-llm-function (ende-test--fake-llm ,responses t)))
       (unwind-protect
           (save-window-excursion
             (with-temp-buffer
               (insert "original")
               (ende-test--select (point-min) (point-max) (point-max))
               (ende-rubber-duck 'detailed)
               (with-current-buffer ende-rubber-duck--buffer
                 ,@body)))
         (ende-test--reset)))))

(ert-deftest ende-test/rubber-duck-without-region-captures-buffer ()
  (ende-test--reset)
  (unwind-protect
      (save-window-excursion
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "first\nsecond")
          (goto-char 8)
          (deactivate-mark)
          (let ((source-name (buffer-name)))
            (ende-rubber-duck 'overview)
            (with-current-buffer ende-rubber-duck--buffer
              (let ((exercise ende-rubber-duck--exercise))
                (should (equal (buffer-string) "first\nsecond"))
                (should (eq major-mode 'emacs-lisp-mode))
                (should (equal (ende-rubber-duck--exercise-source exercise)
                               (format "%s:1" source-name))))))))
    (ende-test--reset)))

(ert-deftest ende-test/rubber-duck-keeps-the-capture-time-model ()
  (ende-test--reset)
  (let* ((source (generate-new-buffer " *ende-rubber-duck-model-source*"))
         (ende-custom-providers (ende-test--custom-profile 'local :models '("capture-model")))
         (ende-model-selection-scope 'buffer)
         (ende-model-selection--last-used nil)
         (ende-model-selection--buffer-selections
          (make-hash-table :test #'equal))
         (ende-model-selection--shared-control-preferences
          (make-hash-table :test #'equal))
         (ende-model-selection--buffer-control-preferences
          (make-hash-table :test #'equal))
         (ende-llm-function
          (ende-test--fake-llm (list (ende-test--duck-result) t))))
    (unwind-protect
        (save-window-excursion
          (with-current-buffer source
            (insert "Original material")
            (ende-model-selection-save
             '(:provider local :model "capture-model")))
          (with-current-buffer source
            (ende-rubber-duck 'overview))
          (with-current-buffer source
            (ende-model-selection-save
             '(:provider openrouter :model "later-model")))
          (with-current-buffer ende-rubber-duck--buffer
            (goto-char (point-max))
            (insert " My explanation")
            (let ((job (ende-rubber-duck-review)))
              (should (equal (plist-get (ende-job-profile job) :model)
                             "capture-model"))
              (should (equal
                       (plist-get (plist-get (car ende-test--calls) :options)
                                  :model)
                       "capture-model")))))
      (when (buffer-live-p source)
        (kill-buffer source))
      (ende-test--reset))))

(ert-deftest ende-test/rubber-duck-review-shows-progress ()
  (ende-test--with-duck (list :stall)
			(erase-buffer)
			(insert "My explanation")
			(let* ((job (ende-rubber-duck-review))
			       (overlay (ende-job-overlay job)))
			  (should (eq (ende-job-state job) 'running))
			  (should (= (overlay-start overlay) (point-min)))
			  (should (= (overlay-end overlay) (point-max)))
			  (should (overlay-get overlay 'before-string))
			  (should-not (overlay-get overlay 'face))
			  (should (string-match-p
				   "Thinking" (ende-test--overlay-label overlay)))
			  (ende-abort-queue)
			  (should (eq (ende-job-state job) 'cancelled))
			  (should-not (ende-job-overlay job))
			  (should-not (overlay-buffer overlay)))))

(ert-deftest ende-test/rubber-duck-insertions-score-and-rejection ()
  (ende-test--with-duck
   (list (ende-test--duck-result
          "[{\"anchor\":\"first\",\"position\":\"after\",\"text\":\" [AI: one]\"},{\"anchor\":\"last\",\"position\":\"after\",\"text\":\" [AI: two]\"}]"))
   (erase-buffer)
   (insert "first explanation\nlast")
   (let* ((submission (buffer-string))
          (ende-history-enabled t)
          (job (ende-rubber-duck-review)))
     (should-error (ende-rubber-duck-review) :type 'user-error)
     (ende-test--complete-call (car ende-test--calls))
     (should (eq (ende-job-state job) 'completed))
     (should-not (ende-job-overlay job))
     (should ende-rubber-duck--reviewed)
     (should (string-prefix-p
              "first [AI: one] explanation\nlast [AI: two]\n\n--- AI assessment ---"
              (buffer-string)))
     (should (string-suffix-p "Coverage: Explain the boundary condition.\n"
                              (buffer-string)))
     (should (= 3 (length (ende-review--overlays-in-buffer))))
     (should-not (ende-history--entry job))
     (should (buffer-live-p ende-history--buffer-object))
     (with-current-buffer ende-history--buffer-object
       (should (string-match-p "AI: one" (buffer-string)))
       (should (string-match-p "Accurate main idea" (buffer-string))))
     (ende-reject-all-output)
     (should (equal submission (buffer-string)))
     ;; Rejecting feedback does not turn the exercise into a retry session.
     (should-error (ende-rubber-duck-review) :type 'user-error))))

(ert-deftest ende-test/rubber-duck-empty-edits-and-unchanged-submission ()
  (ende-test--with-duck (list (ende-test--duck-result))
			(should-error (ende-rubber-duck-review) :type 'user-error)
			(should-not ende-test--calls)
			(goto-char (point-max))
			(insert "\nMy explanation.")
			;; A region or narrowing must not limit the worksheet sent for review.
			(narrow-to-region 3 6)
			(let ((job (ende-rubber-duck-review)))
			  (ende-test--complete-call (car ende-test--calls))
			  (should (eq (ende-job-state job) 'completed))
			  (widen)
			  (should (string-match-p "Demonstrated understanding: 3/4 — Mostly sound"
						  (buffer-string)))
			  (should (= 1 (length (ende-review--overlays-in-buffer)))))))

(ert-deftest ende-test/rubber-duck-invalid-response-is-atomic ()
  (dolist (response
           (list
            (ende-test--duck-result
             "[{\"anchor\":\"original\",\"position\":\"after\",\"text\":\"valid\"},{\"anchor\":\"original\",\"position\":\"replace\",\"text\":\"invalid\"}]")
            (ende-test--duck-result
             "[{\"anchor\":\"missing\",\"position\":\"after\",\"text\":\"invalid\"}]")
            "{\"version\":1,\"edits\":[],\"assessment\":{\"score\":9}}"))
    (ende-test--with-duck (list response)
			  (goto-char (point-max))
			  (insert " explanation")
			  (let ((submission (buffer-string))
				(job (ende-rubber-duck-review)))
			    (ende-test--complete-call (car ende-test--calls))
			    (should (eq (ende-job-state job) 'failed))
			    (should (equal submission (buffer-string)))
			    (should-not (ende-review--overlays-in-buffer))
			    (should-not ende-rubber-duck--reviewed)))))

(ert-deftest ende-test/rubber-duck-application-failure-rolls-back-overlays ()
  (ende-test--with-duck
   (list (ende-test--duck-result
          "[{\"anchor\":\"original\",\"position\":\"after\",\"text\":\" annotation\"}]"))
   (goto-char (point-max))
   (insert " explanation")
   (let ((submission (buffer-string))
         (job (ende-rubber-duck-review))
         (create (symbol-function 'ende--create-review-overlay))
         (count 0))
     (cl-letf (((symbol-function 'ende--create-review-overlay)
                (lambda (&rest args)
                  (if (= (cl-incf count) 2)
                      (error "Mock overlay failure")
                    (apply create args)))))
       (ende-test--complete-call (car ende-test--calls)))
     (should (eq (ende-job-state job) 'failed))
     (should (equal submission (buffer-string)))
     (should-not (ende-review--overlays-in-buffer)))))

(ert-deftest ende-test/rubber-duck-edits-invalidate-pending-feedback ()
  (dolist (position '(1 5 21))
    (ende-test--with-duck (list (ende-test--duck-result))
			  (goto-char (point-max))
			  (insert " explanation")
			  (let ((job (ende-rubber-duck-review)))
			    (goto-char position)
			    (insert "user edit")
			    (let ((changed (buffer-string)))
			      (ende-test--complete-call (car ende-test--calls))
			      (should (eq (ende-job-state job) 'failed))
			      (should (equal changed (buffer-string)))
			      (should-not (ende-review--overlays-in-buffer)))))))

(ert-deftest ende-test/rubber-duck-replacement-cancels-and-ignores-late-response ()
  (ende-test--with-duck (list (ende-test--duck-result))
			(let ((old (current-buffer)))
			  (goto-char (point-max))
			  (insert " explanation")
			  (let ((job (ende-rubber-duck-review))
				(cancelled nil))
			    (ende-test--callback (car ende-test--calls) :on-start
						 (lambda () (setq cancelled t)))
			    (emacs-lisp-mode)
			    (ende-rubber-duck-mode -1)
			    (with-temp-buffer
			      (insert "second")
			      (ende-test--select 1 (point-max) (point-max))
			      (ende-rubber-duck 'overview))
			    (should cancelled)
			    (should (eq (ende-job-state job) 'cancelled))
			    (should-not (buffer-live-p old))
			    (ende-test--complete-call (car ende-test--calls))
			    (with-current-buffer ende-rubber-duck--buffer
			      (should (equal (buffer-string) "second"))
			      (should-not (ende-review--overlays-in-buffer))
			      (should-not ende-rubber-duck--reviewed))
			    (kill-buffer ende-rubber-duck--buffer)
			    (should-not ende-rubber-duck--buffer)))))

(ert-deftest ende-test/rubber-duck-queued-review-disposed-before-dispatch ()
  (ende-test--with-duck (list (ende-test--duck-result))
			(goto-char (point-max))
			(insert " explanation")
			(let* ((ende--queue-suspended t)
			       (job (ende-rubber-duck-review)))
			  (should (eq (ende-job-state job) 'queued))
			  (emacs-lisp-mode)
			  (kill-buffer (current-buffer))
			  (should (eq (ende-job-state job) 'cancelled))
			  (should-not ende-queue)
			  (should-not ende-test--calls))))

(ert-deftest ende-test/rubber-duck-selector-captures-source-and-profile ()
  (ende-test--reset)
  (let ((ende-custom-providers (ende-test--custom-profile 'local :models '("duck-model"))))
  (unwind-protect
      (save-window-excursion
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "selected source")
          (ende-test--select 1 (point-max) (point-max))
          (let ((source (current-buffer)))
            (ende-selector-open)
            (with-current-buffer ende-selector-buffer-name
              (setq ende-selector--models '("duck-model")
                    ende-selector--model-index 0)
              (should (eq (lookup-key ende-selector-mode-map (kbd "r"))
                          #'ende-selector-rubber-duck))
              (ende-selector--run 'rubber-duck 'foundations))
            (should (equal (with-current-buffer source (buffer-string))
                           "selected source"))
            (with-current-buffer ende-rubber-duck--buffer
              (let ((exercise ende-rubber-duck--exercise))
                (should (equal (buffer-string) "selected source"))
                (should (eq major-mode 'emacs-lisp-mode))
                (should ende-rubber-duck-mode)
                (should (eq (ende-rubber-duck--exercise-level exercise)
                            'foundations))
                (should (equal
                         (plist-get (ende-rubber-duck--exercise-profile exercise)
                                    :model)
                         "duck-model")))))))
    (ende-test--reset))))

(ert-deftest ende-test/rubber-duck-survives-mode-switches-and-toggles ()
  (ende-test--with-duck (list (ende-test--duck-result))
			(let ((exercise ende-rubber-duck--exercise))
			  (goto-char (point-max))
			  (insert " explanation")
			  (ende-rubber-duck-mode -1)
			  (setq-local header-line-format "Previous header")
			  (ende-rubber-duck-mode 1)
			  (ende-rubber-duck-mode 1)
			  (ende-rubber-duck-mode -1)
			  (should (equal header-line-format "Previous header"))
			  (emacs-lisp-mode)
			  (should-not ende-rubber-duck-mode)
			  (should (eq exercise ende-rubber-duck--exercise))
			  (ende-rubber-duck-mode 1)
			  (text-mode)
			  (should ende-rubber-duck-mode)
			  (should (equal header-line-format '(:eval (ende-rubber-duck--header))))
			  (should (eq (key-binding (kbd "C-c C-r")) #'ende-rubber-duck-review))
			  (let ((job (ende-rubber-duck-review)))
			    (emacs-lisp-mode)
			    (should (eq job ende-rubber-duck--job))
			    (ende-test--complete-call (car ende-test--calls))
			    (should (eq (ende-job-state job) 'completed))
			    (text-mode)
			    (should ende-rubber-duck--reviewed)
			    (should-error (ende-rubber-duck-review) :type 'user-error)
			    (ende-reject-all-output)
			    (should (equal (buffer-string) "original explanation"))))))

(ert-deftest ende-test/rubber-duck-mode-switch-preserves-change-monitoring ()
  (ende-test--with-duck (list (ende-test--duck-result))
			(goto-char (point-max))
			(insert " explanation")
			(let ((job (ende-rubber-duck-review)))
			  (emacs-lisp-mode)
			  (text-mode)
			  (goto-char 4)
			  (insert "changed")
			  (should (eq (ende-job-state job) 'failed))
			  (let ((submission (buffer-string)))
			    (ende-test--complete-call (car ende-test--calls))
			    (should (equal submission (buffer-string)))))))

(ert-deftest ende-test/rubber-duck-failed-replacement-retains-draft ()
  (ende-test--with-duck nil
			(let ((old (current-buffer))
			      (exercise ende-rubber-duck--exercise))
			  (with-temp-buffer
			    (insert "second")
			    (ende-test--select 1 (point-max) (point-max))
			    (cl-letf (((symbol-function 'fundamental-mode)
				       (lambda () (error "Mock initialization failure"))))
			      (should-error (ende-rubber-duck 'overview))))
			  (should (eq old ende-rubber-duck--buffer))
			  (should (buffer-live-p old))
			  (should (eq exercise ende-rubber-duck--exercise)))))

(ert-deftest ende-test/rubber-duck-replacement-from-worksheet-keeps-history ()
  (ende-test--with-duck (list (ende-test--duck-result))
			(goto-char (point-max))
			(insert " explanation")
			(ende-rubber-duck-review)
			(ende-test--complete-call (car ende-test--calls))
			(let ((old (current-buffer)))
			  (ende-test--select 1 9 9)
			  (ende-rubber-duck 'overview)
			  (should-not (buffer-live-p old))
			  (with-current-buffer ende-rubber-duck--buffer
			    (should (equal (buffer-string) "original"))
			    (should-not ende-rubber-duck--reviewed))
			  (with-current-buffer ende-history--buffer-object
			    (should (string-match-p "Accurate main idea" (buffer-string)))))))

(provide 'ende-rubber-duck-test)

;;; ende-rubber-duck-test.el ends here
