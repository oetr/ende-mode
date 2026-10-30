;;; ende-review-test.el --- Review tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Coverage for approval, rejection, overlays, conflicts, and highlighting.

;;; Code:

(require 'ert)
(require 'ende)
(require 'ende-test-helpers)
(require 'cl-lib)

(defmacro ende-test--with-face-colors (face foreground background &rest body)
  (declare (indent 3))
  `(let ((foreground (face-attribute ,face :foreground))
         (background (face-attribute ,face :background)))
     (unwind-protect
         (progn
           (set-face-attribute ,face nil :foreground ,foreground :background ,background)
           ,@body)
       (set-face-attribute ,face nil :foreground foreground :background background))))

(ert-deftest ende-test/output-remains-marked-until-approved ()
  (ende-test--reset)
  (let ((ende-provider-model "test-model")
        (ende-review-show-badges t)
        (ende-llm-function (ende-test--fake-llm '(" generated") t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let* ((job (ende-insert-at-cursor))
				   (queued-overlay (ende-job-overlay job)))
			      (should (string-match-p "test-model"
						      (ende-test--overlay-label queued-overlay)))
			      (should (string-match-p "localhost"
						      (ende-test--overlay-label queued-overlay)))
			      (ende-test--complete-call (car ende-test--calls))
			      (let ((review (cl-find-if
					     #'ende-review-overlay-p
					     (overlays-in (point-min) (point-max)))))
				(should review)
				(should (string= (overlay-get review 'model) "test-model"))
				(should (string= (overlay-get review 'client) "localhost"))
				(should (string-match-p "test-model"
							(ende-test--overlay-label review)))
				(should (string-match-p "localhost"
							(ende-test--overlay-label review)))
				(dolist (key '("RET" "C-m" "DEL" "<backspace>" "C-d"))
				  (should-not (lookup-key (overlay-get review 'keymap)
							  (kbd key))))
				(should (eq (lookup-key (overlay-get review 'keymap)
							(kbd "C-c C-c"))
					    #'ende-approve-output))
				(should (eq (lookup-key (overlay-get review 'keymap)
							(kbd "C-c C-k"))
					    #'ende-reject-output))
				(goto-char (overlay-end review))
				(should-not (eq (key-binding (kbd "RET"))
						#'ende-approve-output))
				(goto-char (1+ (overlay-start review)))
				(insert "edited ")
				(should (ende-review-overlay-p review))
				(ende-approve-output)
				(should-not (ende-review-overlay-p review)))))))

(ert-deftest ende-test/enter-works-after-editing-past-review-output ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" generated") t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (ende-insert-at-cursor)
			    (let ((call (car ende-test--calls)))
			      (ende-test--complete-call call)
			      (let ((review (cl-find-if
					     #'ende-review-overlay-p
					     (overlays-in (point-min) (point-max)))))
				(goto-char (overlay-end review))
				(newline)
				(insert "typed")
				(should-not (eq (key-binding (kbd "RET"))
						#'ende-approve-output))
				(funcall (key-binding (kbd "RET")))
				(should (string= (buffer-string) "base generated\ntyped\n"))
				(should (ende-review-overlay-p review)))))))

(ert-deftest ende-test/task-cannot-start-inside-pending-review ()
  (ende-test--reset)
  (let ((ende-review-conflict-policy 'block))
    (ende-test--with-buffer "generated text"
			    (let ((review (ende-review-create-overlay
					   (current-buffer) 1 (point-max) nil nil "")))
			      (goto-char 5)
			      (should-error (ende-insert-at-cursor) :type 'user-error)
			      (ende-test--select 2 6 6)
			      (should-error (ende-improve-at-cursor "rewrite") :type 'user-error)
			      (should-not ende-queue)
			      (ende-review-delete-overlay review)))))

(ert-deftest ende-test/overlapping-write-asks-before-approving-reviews ()
  (ende-test--reset)
  (let ((ende-review-conflict-policy 'confirm)
        (ende-llm-function (ende-test--fake-llm '("!"))))
    (ende-test--with-buffer "old text"
			    (let ((conflict (ende-review-create-overlay
					     (current-buffer) 1 4 nil nil "old"))
				  (unrelated (ende-review-create-overlay
					      (current-buffer) 5 (point-max) nil nil "text")))
			      (goto-char 2)
			      (cl-letf (((symbol-function 'yes-or-no-p)
					 (lambda (&rest _) nil)))
				(should-error (ende-insert-at-cursor) :type 'user-error))
			      (should (ende-review-overlay-p conflict))
			      (should (ende-review-overlay-p unrelated))
			      (should-not ende-queue)
			      (cl-letf (((symbol-function 'yes-or-no-p)
					 (lambda (&rest _) t)))
				(let ((job (ende-insert-at-cursor)))
				  (should (eq (ende-job-state job) 'completed))))
			      (should-not (ende-review-overlay-p conflict))
			      (should (ende-review-overlay-p unrelated))
			      (should (= (length (ende-review--overlays-in-buffer)) 2))
			      (should (string= (buffer-string) "o!ld text"))))))

(ert-deftest ende-test/failed-enqueue-preserves-pending-reviews ()
  (ende-test--reset)
  (let ((ende-review-conflict-policy 'approve))
    (ende-test--with-buffer "generated text"
      (let ((review (ende-review-create-overlay
                     (current-buffer) 1 (point-max) nil nil "original")))
        (goto-char 5)
        (cl-letf (((symbol-function 'ende-enqueue)
                   (lambda (&rest _) (error "Enqueue failed"))))
          (should-error (ende-insert-at-cursor)))
        (should (ende-review-overlay-p review))
        (should-not (ende-all-live-jobs))))))

(ert-deftest ende-test/automatic-review-conflict-approval-is-local ()
  (ende-test--reset)
  (let ((ende-review-conflict-policy 'approve)
        (ende-llm-function (ende-test--fake-llm '("!"))))
    (ende-test--with-buffer "old text"
			    (let ((conflict (ende-review-create-overlay
					     (current-buffer) 1 4 nil nil "old"))
				  (unrelated (ende-review-create-overlay
					      (current-buffer) 5 (point-max) nil nil "text")))
			      (goto-char 2)
			      (cl-letf (((symbol-function 'yes-or-no-p)
					 (lambda (&rest _) (error "confirmation was not expected"))))
				(let ((job (ende-insert-at-cursor)))
				  (should (eq (ende-job-state job) 'completed))))
			      (should-not (ende-review-overlay-p conflict))
			      (should (ende-review-overlay-p unrelated))
			      (should (string= (buffer-string) "o!ld text"))))))

(ert-deftest ende-test/queued-task-cannot-dispatch-into-new-review ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" never") t)))
    (ende-test--with-buffer "base"
			    (goto-char 3)
			    (setq ende--queue-suspended t)
			    (let ((job (ende-insert-at-cursor))
				  (review (ende-review-create-overlay
					   (current-buffer) 1 (point-max) nil nil "")))
			      (setq ende--queue-suspended nil)
			      (ende-process-next)
			      (should (eq (ende-job-state job) 'failed))
			      (should-not ende-test--calls)
			      (ende-review-delete-overlay review)))))

(ert-deftest ende-test/review-creation-failure-leaves-no-overlay ()
  (ende-test--reset)
  (ende-test--with-buffer "text"
			  (let ((ende-review-highlight-blend 'invalid))
			    (should-error
			     (ende-review-create-overlay (current-buffer) 1 3 nil nil ""))
			    (should-not (ende-review--overlays-in-buffer)))))

(ert-deftest ende-test/response-metadata-wins-for-review-label ()
  (ende-test--reset)
  (let ((ende-provider-model "configured-model")
        (ende-review-show-badges t)
        (ende-llm-function
         (ende-test--fake-llm
          '((:content " generated"
		      :reported-model "actual-model"
		      :reported-provider "llama.cpp"))
          t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((job (ende-insert-at-cursor)))
			      (ende-test--complete-call (car ende-test--calls))
			      (let ((review (cl-find-if
					     #'ende-review-overlay-p
					     (overlays-in (point-min) (point-max)))))
				(should review)
				(should (string= (overlay-get review 'model) "actual-model"))
				(should (string= (overlay-get review 'client) "llama.cpp"))
				(should (string-match-p "actual-model"
							(ende-test--overlay-label review)))
				(should-not (string-match-p "configured-model"
							    (ende-test--overlay-label review)))
				(should (eq (ende-job-state job) 'completed)))))))

(ert-deftest ende-test/response-metadata-cannot-be-overwritten-by-discovery ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" X") t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let* ((job (ende-insert-at-cursor))
				   (call (car ende-test--calls)))
			      (ende-test--callback
			       call :on-event
			       '(:type :metadata :metadata
				       (:source :response :model "response-model" :provider "llama.cpp")))
			      (ende-test--callback
			       call :on-event
			       '(:type :metadata :metadata
				       (:source :discovery :model "stale-model" :provider "stale")))
			      (should (string= (overlay-get (ende-job-overlay job) 'model)
					       "response-model"))
			      (should (string= (overlay-get (ende-job-overlay job) 'client)
					       "llama.cpp"))))))

(ert-deftest ende-test/reject-restores-properties-and-saving-remains-allowed ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("new")))
        (kill-ring nil)
        (kill-ring-yank-pointer nil))
    (ende-test--with-buffer "old"
			    (add-text-properties 1 4 '(ende-test-property original))
			    (ende-test--select 1 4 4)
			    (ende-improve-at-cursor "rewrite")
			    (run-hooks 'before-save-hook)
			    (goto-char 2)
			    (ende-reject-output)
			    (should (string= (buffer-string) "old"))
			    (should (equal kill-ring '("new")))
			    (should (eq (get-text-property 1 'ende-test-property) 'original))
			    (should-not (ende-review--overlays-in-buffer)))))

(ert-deftest ende-test/mass-review-actions-cover-the-whole-buffer ()
  (ende-test--reset)
  (let ((kill-ring nil)
        (kill-ring-yank-pointer nil))
    (ende-test--with-buffer "XY"
			    (let ((first (ende-review-create-overlay
					  (current-buffer) 1 2 nil nil "A"))
				  (second (ende-review-create-overlay
					   (current-buffer) 2 3 nil nil "B")))
			      (should (= (length (ende-review--overlays-in-buffer)) 2))
			      (ende-reject-all-output)
			      (should (string= (buffer-string) "AB"))
			      (should (equal kill-ring '("X" "Y")))
			      (should-not (ende-review--overlays-in-buffer))
			      (should-not (ende-review-overlay-p first))
			      (should-not (ende-review-overlay-p second))
			      (ende-review-create-overlay
			       (current-buffer) 1 2 nil nil "A")
			      (ende-review-create-overlay
			       (current-buffer) 2 3 nil nil "B")
			      (ende-approve-all-output)
			      (should-not (ende-review--overlays-in-buffer))))))

(ert-deftest ende-test/narrowing-does-not-affect-review-discovery ()
  (ende-test--reset)
  (ende-test--with-buffer "first second"
			  (let ((first (ende-review-create-overlay
					(current-buffer) 1 6 nil nil ""))
				(second (ende-review-create-overlay
					 (current-buffer) 7 13 nil nil "")))
			    (narrow-to-region 1 6)
			    (ende-review-delete-overlay first)
			    (should (= (length (ende-review--overlays-in-buffer)) 1))
			    (widen)
			    (should (= (length (ende-review--overlays-in-buffer)) 1))
			    (ende-review-delete-overlay second))))

(ert-deftest ende-test/review-badges-default-to-hidden ()
  (ende-test--reset)
  (let ((default (default-value 'ende-review-show-badges)))
    (unwind-protect
        (progn
          (set-default 'ende-review-show-badges nil)
          (ende-test--with-buffer "generated"
				  (let ((review (ende-review-create-overlay
						 (current-buffer) 1 2 "model" "client" "")))
				    (should-not (local-variable-p 'ende-review-show-badges))
				    (should-not (overlay-get review 'before-string))
				    (should (ende-review-overlay-p review))
				    (ende-review-delete-overlay review))))
      (set-default 'ende-review-show-badges default))))

(ert-deftest ende-test/review-badge-toggle-preserves-review-highlight ()
  (ende-test--reset)
  (ende-test--with-face-colors 'font-lock-keyword-face "#0000ff" "#ffffff"
    (let ((ende-review-highlight t))
      (ende-test--with-buffer "defunLLM"
        (add-text-properties 1 6 '(face font-lock-keyword-face))
        (setq-local ende-review-show-badges nil)
        (let* ((review (ende-review-create-overlay
                        (current-buffer) 6 9 "model" "client" ""))
               (face (overlay-get review 'face)))
          (should (ende-review-overlay-p review))
          (should-not (overlay-get review 'before-string))
          (should (equal face '(:foreground "#0000ff" :background "#d9d9ff")))
          (ende-toggle-review-badges)
          (should ende-review-show-badges)
          (should (string-match-p "client model" (ende-test--overlay-label review)))
          (should (equal (overlay-get review 'face) face))
          (ende-toggle-review-badges)
          (should-not ende-review-show-badges)
          (should-not (overlay-get review 'before-string))
          (should (equal (overlay-get review 'face) face))
          (goto-char (1+ (overlay-start review)))
          (ende-approve-output)
          (should-not (ende-review-overlay-p review)))))))

(ert-deftest ende-test/review-badge-global-toggle-respects-local-values ()
  (ende-test--reset)
  (let ((default (default-value 'ende-review-show-badges))
        (inherited (generate-new-buffer "*ende-review-badges-inherited*"))
        (local (generate-new-buffer "*ende-review-badges-local*"))
        inherited-review local-review)
    (unwind-protect
        (progn
          (set-default 'ende-review-show-badges nil)
          (with-current-buffer inherited
            (insert "generated")
            (setq inherited-review
                  (ende-review-create-overlay
                   (current-buffer) 1 2 "model" "client" "")))
          (with-current-buffer local
            (insert "generated")
            (setq-local ende-review-show-badges nil)
            (setq local-review
                  (ende-review-create-overlay
                   (current-buffer) 1 2 "model" "client" "")))
          (with-current-buffer inherited
            (ende-toggle-review-badges t))
          (should (default-value 'ende-review-show-badges))
          (with-current-buffer inherited
            (should-not (local-variable-p 'ende-review-show-badges))
            (should (overlay-get inherited-review 'before-string)))
          (with-current-buffer local
            (should (local-variable-p 'ende-review-show-badges))
            (should-not (overlay-get local-review 'before-string))
            (ende-toggle-review-badges)
            (should (overlay-get local-review 'before-string)))
          (with-current-buffer inherited
            (ende-toggle-review-badges t))
          (should-not (default-value 'ende-review-show-badges))
          (with-current-buffer inherited
            (should-not (overlay-get inherited-review 'before-string)))
          (with-current-buffer local
            (should (overlay-get local-review 'before-string))))
      (when (buffer-live-p inherited)
        (kill-buffer inherited))
      (when (buffer-live-p local)
        (kill-buffer local))
      (set-default 'ende-review-show-badges default))))

;;;; Review highlight

(ert-deftest ende-test/review-highlight-blend-color-math ()
  (should (string= (ende-review--blend-color "#ffffff" "#0000ff" 0.15) "#d9d9ff"))
  (should (string= (ende-review--blend-color "#000000" "#ffffff" 0.5) "#808080"))
  (should (string= (ende-review--blend-color "#333333" "#ffffff" 0.0) "#333333"))
  (should (string= (ende-review--blend-color "#333333" "#ffffff" 1.0) "#ffffff"))
  (should (string= (ende-review--blend-color "#102030" "#905010" 0.25) "#302c28"))
  (should (string= (ende-review--blend-color "#102030" "#905010" 0.75) "#704418"))
  (should (null (ende-review--blend-color "#abc" "#ffffff" 0.5)))
  (should (null (ende-review--blend-color "#ffffff" "junk" 0.5))))

(ert-deftest ende-test/review-highlight-blend-clamps-ratio-and-defaults-to-base ()
  (should (string= (ende-review--blend-color "#102030" "#905010" nil) "#102030"))
  (should (string= (ende-review--blend-color "#102030" "#905010" -0.5) "#102030"))
  (should (string= (ende-review--blend-color "#102030" "#905010" 1.5) "#905010")))

(ert-deftest ende-test/review-highlight-hex-round-trip ()
  (should (equal (ende-review--hex-components "#a1b2c3") '(161 178 195)))
  (should (string= (ende-review--hex-join '(161 178 195)) "#a1b2c3"))
  (should (null (ende-review--hex-components "red")))
  (should (null (ende-review--hex-components "#12345")))
  (should (null (ende-review--hex-components "#1234567")))
  (should (null (ende-review--hex-join '(1 2)))))

(ert-deftest ende-test/review-highlight-face-attr-resolution ()
  (ende-test--with-face-colors 'font-lock-keyword-face "#0000ff" "#ffffff"
    (should (string= (ende-review--face-attr 'font-lock-keyword-face :foreground)
                     "#0000ff"))
    (should (string= (ende-review--face-attr 'font-lock-keyword-face :background)
                     "#ffffff"))
    (should (string= (ende-review--face-attr
                    '(:foreground "red" :background "blue") :foreground)
                     "red"))
    (should (string= (ende-review--face-attr
                    '((:foreground "cyan") (:weight bold)) :foreground)
                     "cyan"))
    (should (string= (ende-review--face-attr
                    '((:foreground "cyan") (:background "magenta")) :background)
                     "magenta"))
    (should (null (ende-review--face-attr '(:weight bold) :foreground)))))

(ert-deftest ende-test/review-highlight-style-from-adjacent-face ()
  (ende-test--with-face-colors 'font-lock-keyword-face "#0000ff" "#ffffff"
    (ende-test--with-buffer "defun x"
      (add-text-properties 1 6 '(face font-lock-keyword-face))
      (should (equal (ende-review--style (current-buffer) 6)
                     '("#0000ff" . "#d9d9ff"))))))

(ert-deftest ende-test/review-highlight-style-default-fallback ()
  (ende-test--with-face-colors 'default "#111111" "#222222"
    (ende-test--with-buffer "plain x"
      (should (equal (ende-review--style (current-buffer) 6)
                     '("#111111" . "#1f1f1f")))
      (should (equal (ende-review--style (current-buffer) 1)
                     '("#111111" . "#1f1f1f"))))))

(ert-deftest ende-test/review-highlight-dark-background-light-foreground ()
  (ende-test--with-face-colors 'default "#e6e6e6" "#1e1e1e"
    (ende-test--with-buffer "plain text"
      (should (equal (ende-review--style (current-buffer) 6)
                     '("#e6e6e6" . "#3c3c3c"))))))

(ert-deftest ende-test/review-highlight-overlay-sets-face ()
  (ende-test--reset)
  (ende-test--with-face-colors 'font-lock-keyword-face "#0000ff" "#ffffff"
    (ende-test--with-buffer "defunLLM"
      (add-text-properties 1 6 '(face font-lock-keyword-face))
      (let ((overlay (ende-review-create-overlay (current-buffer) 6 9 nil nil "")))
        (should (overlayp overlay))
        (should (equal (overlay-get overlay 'face)
                       '(:foreground "#0000ff" :background "#d9d9ff")))
        (should (eq (get-char-property 3 'face) 'font-lock-keyword-face))
        (should (null (get-char-property 3 'foreground-color)))
        (ende-review-delete-overlay overlay)
        (should (eq (get-char-property 3 'face) 'font-lock-keyword-face))))))

(ert-deftest ende-test/review-highlight-disabled-sets-no-properties ()
  (ende-test--reset)
  (ende-test--with-buffer "defunLLM"
			  (add-text-properties 1 6 '(face font-lock-keyword-face))
			  (let ((ende-review-highlight nil))
			    (let ((overlay (ende-review-create-overlay
					    (current-buffer) 6 9 nil nil "")))
			      (should (null (overlay-get overlay 'face)))))))

(ert-deftest ende-test/review-highlight-approval-cleans-properties ()
  (ende-test--reset)
  (ende-test--with-face-colors 'font-lock-keyword-face "#0000ff" "#ffffff"
    (let ((ende-llm-function (ende-test--fake-llm '("LLM"))))
      (ende-test--with-buffer "kw rest"
        (add-text-properties 1 3 '(face font-lock-keyword-face))
        (goto-char 3)
        (ende-insert-at-cursor)
        (should (string= (buffer-string) "kwLLM rest"))
        (let ((review (cl-find-if #'ende-review-overlay-p
                                 (overlays-in (point-min) (point-max)))))
          (should review)
          (should (equal (overlay-get review 'face)
                         '(:foreground "#0000ff" :background "#d9d9ff")))
          (goto-char 4)
          (ende-approve-output)
          (should (string= (buffer-string) "kwLLM rest"))
          (should-not (ende-review-overlay-p review)))))))

(ert-deftest ende-test/review-highlight-rejection-preserves-original-properties ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("new"))))
    (ende-test--with-buffer "old text"
			    (add-text-properties 1 4 '(face font-lock-string-face))
			    (add-text-properties 1 4 '(foreground-color "#123456"))
			    (ende-test--select 1 9 1)
			    (ende-improve-at-cursor "rewrite")
			    (should (string= (buffer-string) "new"))
			    (let ((review (cl-find-if #'ende-review-overlay-p
						      (overlays-in (point-min) (point-max)))))
			      (should review)
			      (goto-char 2)
			      (ende-reject-output)
			      (should (string= (buffer-string) "old text"))
			      (should (eq (get-char-property 1 'face) 'font-lock-string-face))
			      (should (string= (get-char-property 1 'foreground-color) "#123456"))
			      (should (null (get-char-property 1 'background-color)))))))

(provide 'ende-review-test)

;;; ende-review-test.el ends here
