;;; ende-edit-test.el --- Structured-edit tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter Samarin
;;
;; Author: Peter Samarin <peter.samarin@gmail.com>
;; Maintainer: Peter Samarin <peter.samarin@gmail.com>
;; Keywords: lisp, tools
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

;; Coverage for anchors, reconciliation, repair, and edit application.

;;; Code:

(require 'ert)
(require 'ende)
(require 'ende-test-helpers)
(require 'cl-lib)

(ert-deftest ende-test/anchors-match-literally-regardless-of-buffer-settings ()
  (dolist (case-fold-search '(nil t))
    (should (equal '(4) (ende-edit--anchor-offsets "Foo foo" "foo")))
    (should (equal '(0) (ende-edit--anchor-offsets "a.b axb" "a.b")))))

(ert-deftest ende-test/structured-edit-parsing-propagates-quit ()
  (cl-letf (((symbol-function 'ende-edit--parse-anchor-edits)
             (lambda (&rest _) (signal 'quit nil))))
    (dolist (parser '(ende-edit-parse-insertions ende-edit-parse-replacements))
      (should
       (condition-case nil
           (progn (funcall parser "{\"version\":1,\"edits\":[{}]}" "x") nil)
         (quit t))))))

(ert-deftest ende-test/structured-edit-quit-keeps-earlier-edits-and-stops ()
  (dolist (type '(insert-anywhere write-anywhere))
    (ende-test--reset)
    (let* ((position (if (eq type 'write-anywhere) "replace" "after"))
           (response
            (format "{\"version\":1,\"edits\":[{\"anchor\":\"x\",\"position\":\"%s\",\"text\":\"!\",\"all\":true}]}"
                    position))
           (ende-llm-function
            (ende-test--fake-llm (list (list :content response)) t))
           (create-review (symbol-function 'ende--create-review-overlay))
           (attempts 0))
      (ende-test--with-buffer "x x x"
        (let* ((job (ende-enqueue-operation type))
               (target (ende-job-target job)))
          (cl-letf (((symbol-function 'ende--create-review-overlay)
                     (lambda (&rest args)
                       (if (= (cl-incf attempts) 2)
                           (signal 'quit nil)
                         (apply create-review args)))))
            (should
             (condition-case nil
                 (progn (ende-test--complete-call (car ende-test--calls)) nil)
               (quit t))))
          (should (equal (buffer-string)
                         (if (eq type 'write-anywhere) "x x !" "x x x!")))
          (should (= attempts 2))
          (should (= (length (ende-review--overlays-in-buffer)) 1))
          (should (= (length ende-test--calls) 1))
          (should (equal (ende-job-error job) '(quit)))
          (should (eq (ende-job-state job) 'failed))
          (should-not (ende-target-applying target))
          (should-not (ende-target-before-change-hook target))
          (should-not (ende-job-overlay job))
          (should-not (ende-running-jobs)))))))

(ert-deftest ende-test/write-anywhere-creates-independent-reviews ()
  (ende-test--reset)
  (let* ((response
          "{\"version\":1,\"edits\":[{\"anchor\":\"one\",\"position\":\"before\",\"text\":\"A\"},{\"anchor\":\"two\",\"position\":\"replace\",\"text\":\"TWO\"}]}" )
         (ende-llm-function
          (ende-test--fake-llm (list (list :content response)))))
    (ende-test--with-buffer "one two"
			    (let ((job (ende-write-anywhere)))
			      (should (eq (ende-job-state job) 'completed))
			      (should (string= (buffer-string) "Aone TWO"))
			      (should-not buffer-read-only)
			      (let ((reviews (ende-review--overlays-in-buffer)))
				(should (= (length reviews) 2))
				(goto-char (overlay-start (car reviews)))
				(ende-approve-output)
				(should (= (length (ende-review--overlays-in-buffer)) 1))
				(let ((remaining (car (ende-review--overlays-in-buffer))))
				  (goto-char (overlay-start remaining))
				  (ende-reject-output))
				(should (string= (buffer-string) "Aone two")))))))

(ert-deftest ende-test/write-anywhere-applies-valid-edits-around-invalid-ones ()
  (ende-test--reset)
  (let* ((response
          "{\"version\":1,\"edits\":[{\"anchor\":\"one\",\"position\":\"replace\",\"text\":\"ONE\"},{\"anchor\":\"missing\",\"position\":\"replace\",\"text\":\"bad\"},{\"anchor\":\"two\",\"position\":\"after\",\"text\":\"!\"}]}" )
         (repair-response
          "{\"version\":1,\"edits\":[{\"anchor\":\"missing\",\"position\":\"replace\",\"text\":\"bad\"}]}" )
         (ende-llm-function
          (ende-test--fake-llm
           (list (list :content response)
                 (list :content repair-response)))))
    (ende-test--with-buffer "one two"
			    (let ((job (ende-write-anywhere)))
			      (should (eq (ende-job-state job) 'completed))
			      (should (string= (buffer-string) "ONE two!"))
			      (should (= (length (ende-review--overlays-in-buffer)) 2))
			      (with-current-buffer ende-history--buffer-object
				(let ((history (buffer-string)))
				  (should (string-match-p "Skipped edits" history))
				  (should (string-match-p "COMPLETED WITH WARNINGS" history))
				  (should (string-match-p "Requested: 3" history))
				  (should (string-match-p "Edit 2: anchor was not found" history))
				  (should (string-match-p
					   (regexp-quote "{\"anchor\":\"missing\"")
					   history))))))))

(ert-deftest ende-test/write-anywhere-supports-occurrence-and-all-anchors ()
  (ende-test--reset)
  (let* ((notifications 0)
         (ende-review-change-functions
          (cons (lambda (_) (cl-incf notifications)) ende-review-change-functions))
         (response
          "{\"version\":1,\"edits\":[{\"anchor\":\"one\",\"position\":\"replace\",\"all\":true,\"text\":\"ONE\"}]}" )
         (ende-llm-function
          (ende-test--fake-llm (list (list :content response)))))
    (ende-test--with-buffer "one\none"
			    (let ((job (ende-write-anywhere)))
			      (should (eq (ende-job-state job) 'completed))
			      (should (string= (buffer-string) "ONE\nONE"))
			      (should (= notifications 1))
			      (should (= (length (ende-review--overlays-in-buffer)) 2))))))

(ert-deftest ende-test/write-anywhere-retains-all-skipped-edits-on-failure ()
  (ende-test--reset)
  (let* ((response
          "{\"version\":1,\"edits\":[{\"anchor\":\"missing\",\"position\":\"replace\",\"text\":\"bad\"}]}" )
         (repair-response
          "{\"version\":1,\"edits\":[{\"anchor\":\"missing\",\"position\":\"before\",\"text\":\"bad\"}]}" )
         (ende-llm-function
          (ende-test--fake-llm
           (list (list :content response)
                 (list :content repair-response)))))
    (ende-test--with-buffer "test"
			    (let ((job (ende-write-anywhere)))
			      (should (eq (ende-job-state job) 'failed))
			      (should (string= (buffer-string) "test"))
			      (should-not (ende-review--overlays-in-buffer))
			      (with-current-buffer ende-history--buffer-object
				(let ((history (buffer-string)))
				  (should (string-match-p "Skipped edits" history))
				  (should (string-match-p "Edit 1: anchor was not found" history))
				  (should (string-match-p
					   (regexp-quote "{\"anchor\":\"missing\"")
					   history))))))))

(ert-deftest ende-test/anywhere-matches-only-selected-source ()
  (dolist (type '(insert-anywhere write-anywhere))
    (dolist (qualifier '("" ",\"occurrence\":1" ",\"all\":true"))
      (ende-test--reset)
      (let* ((position (if (eq type 'write-anywhere) "replace" "after"))
             (response
              (format "{\"version\":1,\"edits\":[{\"anchor\":\"same\",\"position\":\"%s\",\"text\":\"!\"%s}]}"
                      position qualifier))
             (ende-llm-function
              (ende-test--fake-llm (list (list :content response)))))
        (ende-test--with-buffer "outside same|same|same outside"
				(ende-test--select 14 18 18)
				(let ((job (ende-enqueue-operation type "Edit the target")))
				  (should (eq (ende-job-state job) 'completed))
				  (should (string-suffix-p
					   "Document:\nsame"
					   (ende-test--user-message (car ende-test--prompts))))
				  (should (string= (buffer-string)
						   (if (eq type 'write-anywhere)
						       "outside same|!|same outside"
						     "outside same|same!|same outside")))
				  (should-not (buffer-narrowed-p))))))))

(ert-deftest ende-test/anywhere-rejects-anchors-outside-selection ()
  (dolist (type '(insert-anywhere write-anywhere))
    (ende-test--reset)
    (let* ((ende-structured-edit-repair-attempts 0)
           (position (if (eq type 'write-anywhere) "replace" "after"))
           (response
            (format "{\"version\":1,\"edits\":[{\"anchor\":\"outside\",\"position\":\"%s\",\"text\":\"!\"},{\"anchor\":\"|same\",\"position\":\"%s\",\"text\":\"!\"},{\"anchor\":\"same|\",\"position\":\"%s\",\"text\":\"!\"}]}"
                    position position position))
           (ende-llm-function
            (ende-test--fake-llm (list (list :content response)))))
      (ende-test--with-buffer "outside|same|outside"
			      (ende-test--select 9 13 13)
			      (let ((job (ende-enqueue-operation type)))
				(should (eq (ende-job-state job) 'failed))
				(should (string= (buffer-string) "outside|same|outside"))
				(should (= (plist-get (ende--structured-summary
						       (ende-job-structured-state job))
						      :skipped)
					   3)))))))

(ert-deftest ende-test/anywhere-repair-keeps-moving-selection-and-edge-insertions ()
  (dolist (type '(insert-anywhere write-anywhere))
    (ende-test--reset)
    (let* ((ende-structured-edit-repair-attempts 1)
           (response
            "{\"version\":1,\"edits\":[{\"anchor\":\"same\",\"position\":\"before\",\"text\":\"A\"},{\"anchor\":\"same\",\"position\":\"after\",\"text\":\"Z\"},{\"anchor\":\"outside\",\"position\":\"after\",\"text\":\"!\"}]}")
           (repair
            "{\"version\":1,\"edits\":[{\"anchor\":\"Z\",\"position\":\"after\",\"text\":\"!\"},{\"anchor\":\"outside\",\"position\":\"after\",\"text\":\"BAD\"}]}")
           (ende-llm-function
            (ende-test--fake-llm
             (list (list :content response) (list :content repair)) t)))
      (ende-test--with-buffer "outside|same|outside"
			      (ende-test--select 9 13 13)
			      (let ((job (ende-enqueue-operation type)))
				(should-not (buffer-narrowed-p))
				(deactivate-mark)
				(goto-char (point-min))
				(insert "user ")
				(ende-test--complete-call (car ende-test--calls))
				(should (eq (ende-job-state job) 'running))
				(should (string-suffix-p
					 "Current document:\nAsameZ"
					 (ende-test--user-message (car ende-test--prompts))))
				(goto-char (point-min))
				(insert "more ")
				(ende-test--complete-call (nth 1 ende-test--calls))
				(should (eq (ende-job-state job) 'completed))
				(should (string= (buffer-string)
						 "more user outside|AsameZ!|outside"))
				(should-not (buffer-narrowed-p)))))))

(ert-deftest ende-test/anywhere-without-selection-respects-narrowing ()
  (dolist (type '(insert-anywhere write-anywhere))
    (ende-test--reset)
    (let* ((response
            "{\"version\":1,\"edits\":[{\"anchor\":\"same\",\"position\":\"after\",\"text\":\"!\",\"all\":true}]}")
           (ende-llm-function
            (ende-test--fake-llm (list (list :content response)))))
      (ende-test--with-buffer "same|same|same"
			      (narrow-to-region 6 10)
			      (let ((job (ende-enqueue-operation type)))
				(should (eq (ende-job-state job) 'completed))
				(should (buffer-narrowed-p))
				(should (string= (buffer-string) "same!"))
				(widen)
				(should (string= (buffer-string) "same|same!|same")))))))

(ert-deftest ende-test/insert-anywhere-rejects-character-offsets ()
  (ende-test--reset)
  (let* ((response
          "{\"version\":1,\"edits\":[{\"start\":1,\"end\":1,\"expected\":\"\",\"text\":\"X\"}]}" )
         (ende-llm-function
          (ende-test--fake-llm (list (list :content response)))))
    (ende-test--with-buffer "test"
			    (let ((job (ende-insert-anywhere)))
			      (should (eq (ende-job-state job) 'failed))
			      (should (string= (buffer-string) "test"))
			      (should-not (ende-review--overlays-in-buffer))))))

(ert-deftest ende-test/insert-anywhere-reports-missing-fields ()
  (ende-test--reset)
  (let* ((response
          "{\"version\":1,\"edits\":[{\"position\":\"before\",\"text\":\"X\"}]}" )
         (repair-response response)
         (ende-llm-function
          (ende-test--fake-llm
           (list (list :content response)
                 (list :content repair-response)))))
    (ende-test--with-buffer "test"
			    (let ((job (ende-insert-anywhere)))
			      (should (eq (ende-job-state job) 'failed))
			      (should (string-match-p "missing required anchor, position, or text field"
						      (ende-error-message (ende-job-error job))))))))

(ert-deftest ende-test/insert-anywhere-creates-independent-insertions ()
  (ende-test--reset)
  (let* ((response
          "{\"version\":1,\"edits\":[{\"anchor\":\"test\",\"position\":\"before\",\"text\":\"A\"},{\"anchor\":\"test\",\"position\":\"after\",\"text\":\"B\"}]}" )
         (ende-llm-function
          (ende-test--fake-llm (list (list :content response)))))
    (ende-test--with-buffer "test"
			    (let ((job (ende-insert-anywhere
					"Add a marker at every appropriate position")))
			      (should (eq (ende-job-state job) 'completed))
			      (should (string= (buffer-string) "AtestB"))
			      (should (= (length (ende-review--overlays-in-buffer)) 2))
			      (let ((messages (car ende-test--prompts)))
				(should (string-match-p "INSERT-ANYWHERE"
							(cdr (cadr messages))))
				(should (string-match-p
					 "Add a marker at every appropriate position"
					 (cdr (cadr messages))))
				(should (string-match-p "Multiple insertions"
							(cdr (car messages)))))))))

(ert-deftest ende-test/insert-anywhere-applies-valid-edits-around-invalid-ones ()
  (ende-test--reset)
  (let* ((response
          "{\"version\":1,\"edits\":[{\"anchor\":\"aa\",\"position\":\"before\",\"text\":\"A\"},{\"anchor\":\"missing\",\"position\":\"before\",\"text\":\"bad\"},{\"anchor\":\"bb\",\"position\":\"after\",\"text\":\"B\"}]}" )
         (repair-response
          "{\"version\":1,\"edits\":[{\"anchor\":\"missing\",\"position\":\"before\",\"text\":\"bad\"}]}" )
         (ende-llm-function
          (ende-test--fake-llm
           (list (list :content response)
                 (list :content repair-response)))))
    (ende-test--with-buffer "aa\nbb"
			    (let ((job (ende-insert-anywhere)))
			      (should (eq (ende-job-state job) 'completed))
			      (should (string= (buffer-string) "Aaa\nbbB"))
			      (should (= (length (ende-review--overlays-in-buffer)) 2))
			      (should (string-match-p
				       "Skipped edits"
				       (with-current-buffer ende-history--buffer-object
					 (buffer-string))))
			      (should (string-match-p
				       "Edit 2: anchor was not found"
				       (with-current-buffer ende-history--buffer-object
					 (buffer-string))))))))

(ert-deftest ende-test/insert-anywhere-supports-occurrence-and-all-anchors ()
  (ende-test--reset)
  (let* ((notifications 0)
         (ende-review-change-functions
          (cons (lambda (_) (cl-incf notifications)) ende-review-change-functions))
         (response
          "{\"version\":1,\"edits\":[{\"anchor\":\"one\",\"position\":\"before\",\"occurrence\":1,\"text\":\"A\"},{\"anchor\":\"one\",\"position\":\"after\",\"all\":true,\"text\":\"B\"}]}" )
         (ende-llm-function
          (ende-test--fake-llm (list (list :content response)))))
    (ende-test--with-buffer "one\none"
			    (let ((job (ende-insert-anywhere)))
			      (should (eq (ende-job-state job) 'completed))
			      (should (string= (buffer-string) "AoneB\noneB"))
			      (should (= notifications 1))
			      (should (= (length (ende-review--overlays-in-buffer)) 3))))))

(ert-deftest ende-test/structured-edit-normalized-expansion-is-bounded ()
  (let ((ende-max-normalized-edits 2))
    (should-error
     (ende-edit-parse-insertions
      "{\"version\":1,\"edits\":[{\"anchor\":\"x\",\"position\":\"before\",\"all\":true,\"text\":\"!\"}]}"
      "x x x")
     :type 'ende-edit-resource-error)))

(ert-deftest ende-test/structured-edit-conflicts-use-a-sweep ()
  (let ((parsed
         (ende-edit-parse-replacements
          "{\"version\":1,\"edits\":[{\"anchor\":\"abc\",\"position\":\"replace\",\"text\":\"A\"},{\"anchor\":\"bcd\",\"position\":\"replace\",\"text\":\"B\"}]}"
          "abcdef")))
    (should-not (plist-get parsed :edits))
    (should (= (length (plist-get parsed :errors)) 2))))

(ert-deftest ende-test/structured-edit-rejects-before-replace-at-same-anchor ()
  (let ((parsed
         (ende-edit-parse-replacements
          "{\"version\":1,\"edits\":[{\"anchor\":\"abc\",\"position\":\"replace\",\"text\":\"X\"},{\"anchor\":\"abc\",\"position\":\"before\",\"text\":\"Y\"}]}"
          "abc")))
    (should-not (plist-get parsed :edits))
    (should (= (length (plist-get parsed :errors)) 2))
    (should (string-match-p
             "overlaps"
             (plist-get (car (plist-get parsed :errors)) :message)))))

(ert-deftest ende-test/structured-edits-preserve-user-point-by-default ()
  (should-not ende-structured-edit-move-point)
  (dolist (spec '((write-anywhere "replace" "MIDDLE" "left MIDDLE right")
                  (insert-anywhere "after" "!" "left middle! right")))
    (pcase-let ((`(,type ,position ,text ,expected) spec))
      (ende-test--reset)
      (let* ((response
              (format (concat "{\"version\":1,\"edits\":[{\"anchor\":\"middle\","
                              "\"position\":\"%s\",\"text\":\"%s\"}]}")
                      position text))
             (ende-review-change-functions
              (cons (lambda (buffer)
                      (with-current-buffer buffer (goto-char (point-max))))
                    ende-review-change-functions))
             (ende-llm-function
              (ende-test--fake-llm (list (list :content response)) t)))
        (ende-test--with-buffer "left middle right"
				(let ((source (current-buffer))
				      (callback-buffer (generate-new-buffer " *ende-callback*")))
				  (unwind-protect
				      (progn
					(ende-test--select 6 12 12)
					(let* ((job (ende-enqueue-operation type "Change middle"))
					       (call (car ende-test--calls)))
					  (deactivate-mark)
					  (goto-char 2)
					  (with-current-buffer callback-buffer
					    (ende-test--complete-call call)
					    (should (eq (current-buffer) callback-buffer)))
					  (with-current-buffer source
					    (should (eq (ende-job-state job) 'completed))
					    (should (string= (buffer-string) expected))
					    (should (= (point) 2))
					    (should-not (buffer-narrowed-p)))))
				    (when (buffer-live-p callback-buffer)
				      (kill-buffer callback-buffer)))))))))

(ert-deftest ende-test/structured-edits-move-point-when-enabled ()
  (dolist (spec '((write-anywhere "replace" "MIDDLE" "left MIDDLE right")
                  (insert-anywhere "after" "!" "left middle! right")))
    (pcase-let ((`(,type ,position ,text ,expected) spec))
      (ende-test--reset)
      (let* ((ende-structured-edit-move-point t)
             (response
              (format (concat "{\"version\":1,\"edits\":[{\"anchor\":\"middle\","
                              "\"position\":\"%s\",\"text\":\"%s\"}]}")
                      position text))
             (ende-llm-function
              (ende-test--fake-llm (list (list :content response)) t)))
        (ende-test--with-buffer "left middle right"
				(ende-test--select 6 12 12)
				(let* ((job (ende-enqueue-operation type "Change middle"))
				       (call (car ende-test--calls)))
				  (deactivate-mark)
				  (goto-char 2)
				  (ende-test--complete-call call)
				  (let ((review (car (ende-review--overlays-in-buffer))))
				    (should (eq (ende-job-state job) 'completed))
				    (should (string= (buffer-string) expected))
				    (should review)
				    (should (= (point) (overlay-end review))))))))))

(ert-deftest ende-test/structured-edit-orders-adjacent-after-before-replace ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm
          (list
           (list :content
                 "{\"version\":1,\"edits\":[{\"anchor\":\"XYZ\",\"position\":\"replace\",\"text\":\"Q\"},{\"anchor\":\"abc\",\"position\":\"after\",\"text\":\"!\"}]}")))))
    (ende-test--with-buffer "abcXYZ"
			    (let ((job (ende-write-anywhere)))
			      (should (eq (ende-job-state job) 'completed))
			      (should (string= (buffer-string) "abc!Q"))))))

(ert-deftest ende-test/structured-edit-repairs-failed-edits ()
  (ende-test--reset)
  (let* ((first-response
          "{\"version\":1,\"edits\":[{\"anchor\":\"one\",\"position\":\"replace\",\"text\":\"ONE\"},{\"anchor\":\"two\",\"position\":\"replace\",\"occurrence\":2,\"text\":\"BAD\"}]}" )
         (repair-response
          "{\"version\":1,\"edits\":[{\"anchor\":\"two\",\"position\":\"replace\",\"text\":\"TWO\"}]}" )
         (ende-llm-function
          (ende-test--fake-llm
           (list (list :content first-response)
                 (list :content repair-response)))))
    (ende-test--with-buffer "one two"
			    (setq-local buffer-file-name "/tmp/example.el")
			    (setq-local major-mode 'emacs-lisp-mode)
			    (let* ((job (ende-write-anywhere "Fix every item"))
				   (summary (plist-get (ende-job-result job)
						       :structured-summary))
				   (repair-user (cdr (cadr (car ende-test--prompts)))))
			      (should (eq (ende-job-state job) 'completed))
			      (should (string= (buffer-string) "ONE TWO"))
			      (should (= (plist-get summary :requested) 2))
			      (should (= (plist-get summary :first-pass-applied) 1))
			      (should (= (plist-get summary :repaired) 1))
			      (should (= (plist-get summary :skipped) 0))
			      (should (= (plist-get summary :repair-attempts) 1))
			      (should (string-match-p "Name: example\\.el" repair-user))
			      (should (string-match-p "Fix every item" repair-user))
			      (should (string-match-p "occurrence 2 is not present" repair-user))
			      (should (string-match-p "Current document:\nONE two" repair-user))
			      (with-current-buffer ende-history--buffer-object
				(should (string-match-p "Structured edits" (buffer-string)))
				(should (string-match-p "Repaired: 1" (buffer-string))))))))

(ert-deftest ende-test/structured-edit-rejects-read-only-before-dispatch ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm '("unused") t)))
    (ende-test--with-buffer "test"
			    (setq buffer-read-only t)
			    (let ((job (ende-write-anywhere)))
			      (should (eq (ende-job-state job) 'failed))
			      (should-not ende-test--calls)
			      (should (string-match-p "read-only"
						      (error-message-string
						       (ende-job-error job))))))))

(ert-deftest ende-test/structured-edit-rejects-read-only-before-application ()
  (ende-test--reset)
  (let* ((response
          "{\"version\":1,\"edits\":[{\"anchor\":\"test\",\"position\":\"replace\",\"text\":\"TEST\"}]}" )
         (ende-llm-function
          (ende-test--fake-llm (list (list :content response)) t)))
    (ende-test--with-buffer "test"
			    (let ((job (ende-write-anywhere)))
			      (setq buffer-read-only t)
			      (ende-test--complete-call (car ende-test--calls))
			      (should (eq (ende-job-state job) 'failed))
			      (should (string= (buffer-string) "test"))
			      (should-not (ende-review--overlays-in-buffer))))))

(ert-deftest ende-test/write-anywhere-leaves-buffer-writable-while-running ()
  (ende-test--reset)
  (let* ((response
          "{\"version\":1,\"edits\":[{\"anchor\":\"test\",\"position\":\"replace\",\"text\":\"TEST\"}]}" )
         (ende-llm-function
          (ende-test--fake-llm (list (list :content response)) t)))
    (ende-test--with-buffer "test"
			    (let ((job (ende-write-anywhere)))
			      (should (eq (ende-job-state job) 'running))
			      (should-not buffer-read-only)
			      (run-hooks 'before-save-hook)
			      (goto-char (point-min))
			      (insert "user\n")
			      (should (string= (buffer-string) "user\ntest"))
			      (ende-test--complete-call (car ende-test--calls))
			      (should (eq (ende-job-state job) 'completed))
			      (should (string= (buffer-string) "user\nTEST"))
			      (ende-abort-queue)
			      (should-not buffer-read-only)))))

(ert-deftest ende-test/insert-anywhere-leaves-buffer-writable-while-running ()
  (ende-test--reset)
  (let* ((response
          "{\"version\":1,\"edits\":[{\"anchor\":\"test\",\"position\":\"after\",\"text\":\"!\"}]}" )
         (ende-llm-function
          (ende-test--fake-llm (list (list :content response)) t)))
    (ende-test--with-buffer "test"
			    (let ((job (ende-insert-anywhere)))
			      (should (eq (ende-job-state job) 'running))
			      (let ((overlay (ende-job-overlay job)))
				(should (= (overlay-start overlay) (point-min)))
				(should (= (overlay-end overlay) (point-max)))
				(should (overlay-get overlay 'before-string))
				(should-not (overlay-get overlay 'face))
				(should (string-match-p
					 "Thinking" (ende-test--overlay-label overlay))))
			      (should-not buffer-read-only)
			      (ende-abort-queue)
			      (should-not (ende-job-overlay job))
			      (should-not buffer-read-only)))))

(ert-deftest ende-test/insert-anywhere-reconciles-against-current-buffer ()
  (ende-test--reset)
  (let* ((response
          "{\"version\":1,\"edits\":[{\"anchor\":\"test\",\"position\":\"after\",\"text\":\"!\"},{\"anchor\":\"missing\",\"position\":\"before\",\"text\":\"?\"}]}" )
         (repair-response
          "{\"version\":1,\"edits\":[{\"anchor\":\"missing\",\"position\":\"before\",\"text\":\"?\"}]}" )
         (ende-llm-function
          (ende-test--fake-llm
           (list (list :content response)
                 (list :content repair-response)) t)))
    (ende-test--with-buffer "test"
			    (let ((job (ende-insert-anywhere)))
			      (goto-char (point-min))
			      (insert "user\n")
			      (should (eq (ende-job-state job) 'running))
			      (ende-test--complete-call (car ende-test--calls))
			      (should (eq (ende-job-state job) 'running))
			      (ende-test--complete-call (nth 1 ende-test--calls))
			      (should (eq (ende-job-state job) 'completed))
			      (should (string= (buffer-string) "user\ntest!"))
			      (should (= (length (ende-review--overlays-in-buffer)) 1))
			      (should (string-match-p
				       "Skipped edits"
				       (with-current-buffer ende-history--buffer-object
					 (buffer-string))))))))

(ert-deftest ende-test/write-anywhere-applies-from-another-current-buffer ()
  (ende-test--reset)
  (let* ((response
          "{\"version\":1,\"edits\":[{\"anchor\":\"test\",\"position\":\"before\",\"text\":\"A\"}]}" )
         (ende-llm-function
          (ende-test--fake-llm (list (list :content response)) t)))
    (ende-test--with-buffer "test"
			    (let ((job (ende-write-anywhere))
				  (callback-buffer (generate-new-buffer " *ende-callback*")))
			      (unwind-protect
				  (with-current-buffer callback-buffer
				    (ende-test--complete-call (car ende-test--calls)))
				(when (buffer-live-p callback-buffer)
				  (kill-buffer callback-buffer)))
			      (should (eq (ende-job-state job) 'completed))
			      (should (string= (buffer-string) "Atest"))
			      (should-not buffer-read-only)
			      (should (= (length (ende-review--overlays-in-buffer)) 1))))))

(provide 'ende-edit-test)

;;; ende-edit-test.el ends here
