;;; ende-operations-test.el --- Operation tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Coverage for insert and improve operations, prompts, and result application.

;;; Code:

(require 'ert)
(require 'ende)
(require 'ende-test-helpers)
(require 'cl-lib)

;;;; Normal operations

(ert-deftest ende-test/insert-no-selection ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" there"))))
    (ende-test--with-buffer "hello world"
			    (goto-char 6)
			    (let ((job (ende-insert-at-cursor)))
			      (should (eq (ende-job-state job) 'completed))
			      (should (string= (buffer-string) "hello there world"))
			      (let ((user-msg (cdr (cadr (car ende-test--prompts)))))
				(should (string-match "hello<CURSOR> world" user-msg)))))))

(ert-deftest ende-test/insert-at-buffer-boundaries ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("START " " END"))))
    (ende-test--with-buffer "middle"
			    (goto-char (point-min))
			    (ende-insert-at-cursor)
			    (goto-char (point-max))
			    (ende-insert-at-cursor)
			    (should (string= (buffer-string) "START middle END")))))

(ert-deftest ende-test/insert-selected-placements ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm '("HEADER" "FOOTER") t)))
    (ende-test--with-buffer "alpha beta"
			    (ende-test--select 1 11 1)
			    (setq ende--queue-suspended t)
			    (ende-insert-at-cursor)
			    (ende-test--select 1 11 11)
			    (ende-insert-at-cursor)
			    (setq ende--queue-suspended nil)
			    (ende-process-next)
			    (ende-test--complete-call (nth 0 ende-test--calls))
			    (ende-test--complete-call (nth 1 ende-test--calls))
			    (should (string= (buffer-string)
					     "HEADER\nalpha beta\nFOOTER")))))

(ert-deftest ende-test/insert-does-not-mutate-selected-characters ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("MIDDLE"))))
    (ende-test--with-buffer "foobar"
			    (let ((overlay (ende-ui-create-overlay (current-buffer) 1 7 "Queued"))
				  (request nil))
			      (setq request
				    (list 'insert
					  :target
					  (ende-target-create
					   :buffer (current-buffer) :start 1 :end 7 :cursor 4
					   :selected t :placement :inside :offset 3
					   :source-snapshot "foobar")
					  :overlay overlay))
			      (ende-enqueue request))
			    (should (string= (buffer-string) "foo\nMIDDLE\nbar")))))

(ert-deftest ende-test/insert-large-context-elision ()
  (ende-test--reset)
  (let ((ende-target-max-context-chars 30)
        (ende-target-window-chars 10)
        (ende-llm-function (ende-test--fake-llm '("X"))))
    (ende-test--with-buffer
     (concat (make-string 20 ?A) (make-string 20 ?B))
     (goto-char 21)
     (ende-insert-at-cursor)
     (let ((user-msg (cdr (cadr (car ende-test--prompts)))))
       (should (string-match-p "characters elided" user-msg))
       (should (string-match "AAAA<CURSOR>BBBB" user-msg))))))

(ert-deftest ende-test/final-request-budget-includes-instructions ()
  (ende-test--reset)
  (let ((ende-max-request-chars 100)
        (ende-insert-instruction (make-string 200 ?X))
        (ende-llm-function (ende-test--fake-llm '("never"))))
    (ende-test--with-buffer "base"
			    (let ((job (ende-insert-at-cursor)))
			      (should (eq (ende-job-state job) 'failed))
			      (should-not ende-test--calls)
			      (should (string= (buffer-string) "base"))))))

(ert-deftest ende-test/custom-transport-output-is-bounded ()
  (ende-test--reset)
  (let ((ende-max-output-chars 3)
        (ende-llm-function (ende-test--fake-llm '("four"))))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((job (ende-insert-at-cursor)))
			      (should (eq (ende-job-state job) 'failed))
			      (should (string= (buffer-string) "base"))))))

(ert-deftest ende-test/custom-transport-output-can-be-unbounded ()
  (ende-test--reset)
  (let ((ende-max-output-chars nil)
        (ende-llm-function (ende-test--fake-llm '("four"))))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((job (ende-insert-at-cursor)))
			      (should (eq (ende-job-state job) 'completed))
			      (should (string= (buffer-string) "basefour"))))))

(ert-deftest ende-test/improve-replaces-selection ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("improved"))))
    (ende-test--with-buffer "old text"
			    (ende-test--select 1 9 9)
			    (let ((job (ende-improve-at-cursor "make it better")))
			      (should (eq (ende-job-state job) 'completed))
			      (should (string= (buffer-string) "improved"))
			      (should (cl-some #'ende-review-overlay-p
					       (overlays-in (point-min) (point-max))))))))

(ert-deftest ende-test/empty-result-does-not-insert-separators ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("\n") t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((job (ende-insert-at-cursor))
				  (call nil))
			      (setq call (car ende-test--calls))
			      (ende-test--complete-call call)
			      (should (eq (ende-job-state job) 'failed))
			      (should (string= (buffer-string) "base"))))))

(ert-deftest ende-test/direct-effort-preflight-is-cancellable ()
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
        (ende-llm-function (ende-test--fake-llm '("unused")))
        (effort-callback nil)
        (cancel-count 0))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'ende-provider-probe-reasoning-efforts)
                     (lambda (_profile _model callback)
                       (setq effort-callback callback)
                       (lambda () (cl-incf cancel-count)))))
            (with-temp-buffer
              (insert "Document to summarize")
              (ende-model-selection-save
               '(:provider openrouter :model "source-model"
                           :control-preferences (:reasoning-effort "high")))
              (let ((job (ende-summarize)))
                (should (eq (ende-job-state job) 'running))
                (should-not ende-test--calls)
                (ende-abort-queue)
                (should (eq (ende-job-state job) 'cancelled))
                (should (= cancel-count 1))
                (funcall effort-callback '("high") "high" nil)
                (should-not ende-test--calls)))))
      (ende-test--reset))))

(provide 'ende-operations-test)

;;; ende-operations-test.el ends here
