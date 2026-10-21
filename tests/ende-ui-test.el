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

(ert-deftest ende-test/overlay-structured-content-updates-without-buffer-edits ()
  (ende-test--reset)
  (ende-test--with-buffer "source text"
    (goto-char 4)
    (let* ((job (ende--make-job
                 :profile '(:name openai :model "gpt-6-luna"
                           :reasoning-effort "max")))
           (overlay (ende-ui-create-overlay (current-buffer) (point) (point)))
           (source (buffer-string))
           (tick (buffer-chars-modified-tick))
           (position (point))
           (ende-job-event-functions (list #'ende-ui-job-event)))
      (setf (ende-job-overlay job) overlay)
      (ende-ui-job-event
       job '(:type :queued :client "OpenAI" :model "gpt-6-luna"))
      (ende-ui-update-overlay
       overlay :status "Thinking" :activity-title "Inspecting event models"
       :activity-status "Provider summaries arrive as streaming events."
       :usage '(:input_tokens 69975 :input_tokens_details (:cached_tokens 69632)
                :output_tokens 5134)
       :tokens 5134 :speed 83.2)
      (ende-job-emit-event
       job '(:type :reasoning-summary :item summary-1
             :text "Latest provider summary"))
      (let* ((display (overlay-get overlay 'after-string))
             (plain (substring-no-properties display)))
        (should (< (string-match "OpenAI gpt-6-luna max" plain)
                   (string-match "Tokens: 69,975/69,632" plain)))
        (should (< (string-match "Tokens: 69,975/69,632" plain)
                   (string-match "Thinking — Inspecting event models" plain)))
        (should (< (string-match "Thinking — Inspecting event models" plain)
                   (string-match "Provider summaries arrive" plain)))
        (should (< (string-match "Provider summaries arrive" plain)
                   (string-match "Reasoning: Latest provider summary" plain)))
        (ende-ui-update-overlay overlay :status "Generating")
        (ende-ui--refresh-all-overlays)
        (should (string-match-p "Inspecting event models"
                                (ende-test--overlay-label overlay)))
        (should (string-match-p "Latest provider summary"
                                (ende-test--overlay-label overlay))))
      (should (equal source (buffer-string)))
      (should (= tick (buffer-chars-modified-tick)))
      (should (= position (point)))
      (ende-ui-delete-overlay overlay))))

(ert-deftest ende-test/overlay-hides-unavailable-optional-details ()
  (ende-test--reset)
  (ende-test--with-buffer "source"
    (let* ((overlay (ende-ui-create-overlay (current-buffer) (point) (point)))
           (plain (ende-test--overlay-label overlay)))
      (should-not (string-match-p "Tokens:" plain))
      (should-not (string-match-p "Reasoning:" plain))
      (should (string-match-p "Queued" plain))
      (ende-ui-delete-overlay overlay))))

(ert-deftest ende-test/spinner-fallback-and-reduced-motion-cleanup ()
  (ende-test--reset)
  (let ((ende-ui--svg-spinner-frames nil)
        (ende-ui--spinning-overlays nil))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) nil)))
      (should (equal (ende-ui--spinner-frames)
                     ende-ui--fallback-spinner-frames))
      (should (equal (ende-ui--spinner-frame 0) "◴")))
    (when (image-type-available-p 'svg)
      (cl-letf (((symbol-function 'display-graphic-p)
                 (lambda (&optional _) t)))
        (let ((frame (ende-ui--spinner-frame 3)))
          (should (imagep (get-text-property 0 'display frame)))
          (should (= (length ende-ui--svg-spinner-frames) 12)))))
    (ende-test--with-buffer "spinner"
      (let ((ende-reduced-motion t)
            (overlay (ende-ui-create-overlay
                      (current-buffer) (point) (point))))
        (ende-ui-update-overlay overlay :status "Thinking")
        (should-not ende-ui--spinner-timer)
        (should (equal (substring-no-properties
                        (overlay-get overlay 'after-string))
                       "◴ \nThinking\n"))
        (ende-ui-delete-overlay overlay)
        (should-not ende-ui--spinner-timer)))))

(ert-deftest ende-test/usage-events-retain-context-for-estimated-output ()
  (ende-test--reset)
  (ende-test--with-buffer "usage source"
    (let* ((job (ende--make-job :state 'running :execution-generation 1))
           (ende--active-jobs (list job))
           (ende-job-event-functions nil)
           (overlay (ende-ui-create-overlay (current-buffer) (point) (point))))
      (setf (ende-job-overlay job) overlay)
      (ende--handle-llm-event
       job '(:type :usage :tokens 12 :speed 6.0
             :usage (:input_tokens 120
                     :input_tokens_details (:cached_tokens 40)
                     :output_tokens 12)))
      (ende--handle-llm-event
       job '(:type :usage :tokens 25 :speed 5.0 :estimated t))
      (let ((plain (ende-test--overlay-label overlay)))
        (should (string-match-p
                 "Tokens: 120/40 - 5.0 t/s, Output: ~25" plain)))
      (ende-ui-delete-overlay overlay))))

(provide 'ende-ui-test)

;;; ende-ui-test.el ends here
