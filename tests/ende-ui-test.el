;;; ende-ui-test.el --- UI lifecycle tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Coverage for progress overlays, status, and cleanup.

;;; Code:

(require 'ert)
(require 'ende)
(require 'ende-test-helpers)
(require 'cl-lib)

(ert-deftest ende-test/reset-does-not-kill-unowned-named-buffers ()
  (let* ((ende-history-buffer-name " *ende-unowned-history*")
         (ende-summary-buffer-name " *ende-unowned-summary*")
         (history (get-buffer-create ende-history-buffer-name))
         (summary (get-buffer-create ende-summary-buffer-name)))
    (unwind-protect
        (progn
          (setq ende-history--buffer-object nil
                ende-summary--buffer-object nil)
          (ende-test--reset)
          (should (buffer-live-p history))
          (should (buffer-live-p summary)))
      (when (buffer-live-p history)
        (kill-buffer history))
      (when (buffer-live-p summary)
        (kill-buffer summary)))))

(ert-deftest ende-test/anywhere-progress-precedes-selected-region ()
  (dolist (type '(insert-anywhere write-anywhere))
    (ende-test--reset)
    (let ((ende-llm-function (ende-test--fake-llm (list :stall) t)))
      (ende-test--with-buffer "before target after"
			      (ende-test--select 8 14 14)
			      (let* ((job (ende-enqueue-operation type))
				     (overlay (ende-job-overlay job)))
				(should (= (overlay-start overlay) 8))
				(should (= (overlay-end overlay) 14))
				(should (overlay-get overlay 'before-string))
				(should-not (overlay-get overlay 'after-string))
				(should-not (overlay-get overlay 'face))
				(should (string-match-p
					 "Thinking" (ende-test--overlay-label overlay)))
				(ende-abort-queue)
				(should-not (overlay-buffer overlay)))))))

(ert-deftest ende-test/deleted-pending-overlay-cannot-fail-committed-insert ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" A" " B") t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (setq ende--queue-suspended t)
			    (let ((first (ende-insert-at-cursor))
				  (second (ende-insert-at-cursor)))
			      (delete-overlay (ende-job-overlay second))
			      (setq ende--queue-suspended nil)
			      (ende-process-next)
			      (ende-test--complete-call (nth 0 ende-test--calls))
			      (should (eq (ende-job-state first) 'completed))
			      (ende-test--complete-call (nth 1 ende-test--calls))
			      (should (eq (ende-job-state second) 'completed))
			      (should (string= (buffer-string) "base A B"))))))

;;;; Overlay lifecycle

(ert-deftest ende-test/zero-width-overlay-boundary-behavior ()
  (ende-test--reset)
  (ende-test--with-buffer "abc"
			  (goto-char (point-max))
			  (let ((ov (ende-ui-create-overlay (current-buffer) (point) (point)
							    "Queued")))
			    (insert "\n")
			    (should (= (overlay-start ov) 4))
			    (should (= (overlay-end ov) 4))
			    (should-not (overlay-get ov 'before-string))
			    (should (overlay-get ov 'after-string))
			    (should (= (point) 5))
			    (goto-char (point-min))
			    (insert "x")
			    (should (= (overlay-start ov) 5))
			    (should (= (overlay-end ov) 5)))))

(ert-deftest ende-test/overlay-lifecycle ()
  (ende-test--reset)
  (ende-test--with-buffer "hello"
			  (let ((ov (ende-ui-create-overlay (current-buffer) 1 3 "Queued")))
			    (should (overlayp ov))
			    (should (= (length ende-ui--managed-overlays) 1))
			    (should-not ende-ui--spinning-overlays)
			    (should-not ende-ui--spinner-timer)
			    (ende-ui-update-overlay ov :status "Thinking")
			    (should (memq ov ende-ui--spinning-overlays))
			    (should (timerp ende-ui--spinner-timer))
			    (ende-ui-delete-overlay ov)
			    (should (null ende-ui--managed-overlays))
			    (should (null ende-ui--spinner-timer)))))

(provide 'ende-ui-test)

;;; ende-ui-test.el ends here
