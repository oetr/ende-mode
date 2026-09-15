;;; deslop-test.el --- ERT tests for deslop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools, llm

;;; Commentary:

;; ERT coverage for synchronous operations, asynchronous lifecycle behavior,
;; target conflicts, and the incremental streaming parsers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'deslop-provider)
(require 'deslop)

;;;; Test harness

(defvar deslop-test--prompts nil)
(defvar deslop-test--calls nil)
(defvar deslop-test--fake-responses nil)

(defun deslop-test--callback (call name &rest args)
  "Invoke callback NAME from CALL with ARGS when it exists."
  (let ((callback (plist-get (plist-get call :callbacks) name)))
    (when callback
      (apply callback args))))

(defun deslop-test--overlay-label (overlay)
  "Return the display label attached to OVERLAY."
  (substring-no-properties
   (or (overlay-get overlay 'before-string)
       (overlay-get overlay 'after-string)
       "")))

(defun deslop-test--complete-call (call)
  "Complete asynchronous fake CALL using its queued response."
  (let ((response (plist-get call :response)))
    (cond
     ((eq response :stall) nil)
     ((eq response :error)
      (deslop-test--callback
       call :on-error
       (deslop-error-create :kind 'mock :message "mock failure")))
     ((stringp response)
       (deslop-test--callback call :on-event
                              (list :type :status :status "Generating"))
       (deslop-test--callback call :on-event
                              (list :type :delta :channel :content
                                    :text response))
       (deslop-test--callback call :on-event
                              (list :type :usage :tokens 10 :speed 50.0))
       (deslop-test--callback call :on-done response))
      ((and (listp response) response)
       (when (or (plist-get response :reported-model)
                 (plist-get response :reported-provider))
         (deslop-test--callback
          call :on-event
          (list :type :metadata
                :metadata
                (list :source :response
                      :model (plist-get response :reported-model)
                      :provider (plist-get response :reported-provider)))))
       (when (stringp (plist-get response :thinking))
         (deslop-test--callback
          call :on-event
          (list :type :delta :channel :thinking
                :text (plist-get response :thinking))))
       (when (stringp (plist-get response :content))
         (deslop-test--callback
          call :on-event
          (list :type :delta :channel :content
                :text (plist-get response :content))))
       (when (plist-member response :usage)
         (deslop-test--callback
          call :on-event
          (list :type :usage
                :tokens (plist-get response :tokens)
                :speed (plist-get response :speed)
                :usage (plist-get response :usage))))
       (when (listp (plist-get response :partial))
         (deslop-test--callback
          call :on-event
          (list :type :partial-result :result (plist-get response :partial))))
       (deslop-test--callback call :on-done response))
     (t
      (deslop-test--callback call :on-error
                             (format "unknown mock response: %S" response))))))

(defun deslop-test--fake-llm (responses &optional asynchronous process-factory)
  "Return a mock LLM dispatcher for RESPONSES.
When ASYNCHRONOUS is non-nil, calls remain in `deslop-test--calls' until
`deslop-test--complete-call' is invoked.  PROCESS-FACTORY can provide a real
process for cancellation tests."
  (setq deslop-test--fake-responses (copy-sequence responses))
  (lambda (messages callbacks &optional options)
    (push messages deslop-test--prompts)
    (let ((call (list :messages messages
                      :callbacks callbacks
                      :options options
                      :response (pop deslop-test--fake-responses))))
      (when process-factory
        (setq call
              (plist-put call :process (funcall process-factory)))
        (deslop-test--callback
         call :on-start
         (lambda ()
           (let ((process (plist-get call :process)))
             (when (and (processp process) (process-live-p process))
               (process-put process :deslop-cancelled t)
               (delete-process process))))))
      (setq deslop-test--calls
            (append deslop-test--calls (list call)))
      (unless asynchronous
        (deslop-test--complete-call call))
      call)))

(defun deslop-test--sleep-process ()
  "Create a process that remains live until explicitly terminated."
  (make-process :name (generate-new-buffer-name "deslop-test-sleep")
                :command (list "sleep" "30")
                :connection-type 'pipe
                :noquery t))

(defun deslop-test--select (start end point)
  "Activate the region START..END with point at POINT."
  (setq transient-mark-mode t)
  (goto-char point)
  (set-mark (if (= point end) start end))
  (setq mark-active t))

(defun deslop-test--count-matches (regexp text)
  "Return the number of nonoverlapping REGEXP matches in TEXT."
  (let ((start 0)
        (count 0))
    (while (string-match regexp text start)
      (setq count (1+ count))
      (setq start (match-end 0)))
    count))

(defun deslop-test--reset ()
  "Reset global state and clean test-created processes and buffers."
  (deslop-abort-queue)
  (dolist (call deslop-test--calls)
    (let ((process (plist-get call :process)))
      (when (and (processp process) (process-live-p process))
        (delete-process process))))
  (setq deslop-test--prompts nil
        deslop-test--calls nil
        deslop-test--fake-responses nil
        deslop-queue nil
        deslop--queue-tail nil
         deslop--queue-suspended nil
         deslop--current-job nil
         deslop--active-jobs nil
         deslop--aborting nil
         deslop--dispatching nil
         deslop--next-job-id 0)
  (clrhash deslop-provider--discovery-cache)
  (clrhash deslop-provider--discovery-inflight)
  (deslop-provider-clear-runtime-settings)
   (clrhash deslop-history--entries)
   (deslop-tools-clear-change-sets)
   (setq deslop-tools--next-change-set-id 0)
   (when (buffer-live-p deslop-history--buffer-object)
    (kill-buffer deslop-history--buffer-object))
  (setq deslop-history--buffer-object nil)
   (when (buffer-live-p deslop-summary--buffer-object)
     (kill-buffer deslop-summary--buffer-object))
   (setq deslop-summary--buffer-object nil)
   (deslop-ui-clear-all))

(ert-deftest deslop-test/queue-snapshots-do-not-expose-list-state ()
  (let ((deslop--active-jobs (list :active))
        (deslop-queue (list :pending)))
    (let ((active (deslop-active-jobs))
          (pending (deslop-pending-jobs)))
      (setcar active :changed)
      (setcar pending :changed)
      (should (equal deslop--active-jobs '(:active)))
      (should (equal deslop-queue '(:pending))))))

(defun deslop-test--tool-result (name arguments root)
  "Invoke tool NAME and return its successful result in ROOT."
  (let ((call (deslop-tool-call name arguments root)))
    (should (plist-get call :ok))
    (plist-get call :result)))

(ert-deftest deslop-test/project-tools-expose-json-definitions ()
  (should (= (length (deslop-tool-definitions)) 8))
  (should-not (deslop-tool-read-only
               (deslop-tool--find "edit_source")))
  (should (string-match-p
           "edit_source"
           (json-encode (deslop-tool-definitions)))))

(ert-deftest deslop-test/project-tools-enforce-scope-and-report-file-info ()
  (let* ((root (make-temp-file "deslop-tools-project-" t))
         (source (expand-file-name "src/main.el" root))
         (secret (expand-file-name ".env" root))
         (outside (make-temp-file "deslop-tools-outside-")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory source) t)
          (with-temp-file source
            (insert "one\ntwo\nthree\n"))
          (with-temp-file secret
            (insert "TOKEN=secret\n"))
          (let ((deslop-project-root-directory root))
            (let* ((info (deslop-test--tool-result
                          "file_info" '(("path" . "src/main.el")) root))
                   (files (deslop-test--tool-result
                           "list_project_files" nil root)))
              (should (= (plist-get info :line_count) 3))
              (should (= (length (plist-get files :files)) 1))
              (should (string= (plist-get
                                (car (plist-get files :files)) :path)
                               "src/main.el")))
            (let ((failure
                   (deslop-tool-call
                    "read_file" '(("path" . "../outside")) root)))
              (should-not (plist-get failure :ok))
              (should (string-match-p "outside the project"
                                      (plist-get failure :error))))))
      (when (file-directory-p root)
        (delete-directory root t))
      (when (file-exists-p outside)
        (delete-file outside)))))

(ert-deftest deslop-test/project-tools-prefer-unsaved-visiting-buffers ()
  (let* ((root (make-temp-file "deslop-tools-buffer-project-" t))
         (path (expand-file-name "main.el" root))
         (buffer nil))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "disk\nsecond\n"))
          (setq buffer (find-file-noselect path))
          (with-current-buffer buffer
            (goto-char (point-min))
            (insert "unsaved\n")
            (set-buffer-modified-p t))
          (let* ((deslop-project-root-directory root)
                 (listed (deslop-test--tool-result
                          "list_project_buffers" nil root))
                 (record (car (plist-get listed :buffers)))
                 (buffer-id (plist-get record :buffer_id))
                 (read (deslop-test--tool-result
                        "read_buffer"
                        (list (cons "buffer_id" buffer-id)
                              (cons "start_line" 1)
                              (cons "line_count" 1))
                        root)))
            (should (string= (plist-get read :source_kind) "buffer"))
            (should (string= (plist-get read :content) "unsaved\n"))
            (should (string= (plist-get read :path) "main.el"))))
      (when (buffer-live-p buffer)
        (set-buffer-modified-p nil)
        (kill-buffer buffer))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest deslop-test/project-tools-read-search-and-cap-results ()
  (let* ((root (make-temp-file "deslop-tools-search-project-" t))
         (first (expand-file-name "first.txt" root))
         (second (expand-file-name "second.txt" root)))
    (unwind-protect
        (progn
          (with-temp-file first
            (insert "needle one\nkeep\nneedle two\n"))
          (with-temp-file second
            (insert "needle three\n"))
          (let* ((deslop-project-root-directory root)
                 (read (deslop-test--tool-result
                        "read_file"
                        '(("path" . "first.txt")
                          ("start_line" . 2)
                          ("line_count" . 1))
                        root))
                 (search (deslop-test--tool-result
                          "search_project"
                          '(("regexp" . "needle")
                            ("max_results" . 2)
                            ("context_before" . 1))
                          root)))
            (should (= (plist-get read :start_line) 2))
            (should (string= (plist-get read :content) "keep\n"))
            (should (= (length (plist-get search :matches)) 2))
            (should (plist-get search :truncated))
            (should (string-match-p "needle"
                                    (plist-get
                                     (car (plist-get search :matches))
                                     :context)))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest deslop-test/project-tools-edit-requires-digest-and-is-reviewable ()
  (let* ((root (make-temp-file "deslop-tools-edit-project-" t))
         (path (expand-file-name "main.el" root))
         (buffer nil))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "hello world\n"))
          (let* ((deslop-project-root-directory root)
                 (info (deslop-test--tool-result
                        "file_info" '(("path" . "main.el")) root))
                 (proposal (deslop-test--tool-result
                            "edit_source"
                            (list
                             (cons "path" "main.el")
                             (cons "base_digest" (plist-get info :digest))
                             (cons "edits"
                                   (list
                                    (list (cons "start" 6)
                                          (cons "end" 11)
                                          (cons "expected" "world")
                                          (cons "replacement" "Emacs")))))
                            root))
                 (change-set-id (plist-get proposal :change_set_id)))
            (should (string= (plist-get proposal :status) "pending"))
            (should (string= (with-temp-buffer
                               (insert-file-contents path)
                               (buffer-string))
                             "hello world\n"))
             (deslop-tools-apply-change-set change-set-id)
             (should-error (deslop-tools-change-set change-set-id))
             (should (string= (with-temp-buffer
                               (insert-file-contents path)
                               (buffer-string))
                             "hello Emacs\n")))
          (setq buffer (find-file-noselect path))
          (let* ((deslop-project-root-directory root)
                 (listed (deslop-test--tool-result
                          "list_project_buffers" nil root))
                 (buffer-id (plist-get
                             (car (plist-get listed :buffers)) :buffer_id))
                 (info (deslop-test--tool-result
                        "file_info" '(("path" . "main.el")) root))
                 (proposal (deslop-test--tool-result
                            "edit_source"
                            (list
                             (cons "buffer_id" buffer-id)
                             (cons "base_digest" (plist-get info :digest))
                             (cons "edits"
                                   (list
                                    (list (cons "start" 6)
                                          (cons "end" 11)
                                          (cons "expected" "Emacs")
                                          (cons "replacement" "Lisp")))))
                            root)))
             (let ((change-set-id (plist-get proposal :change_set_id)))
               (deslop-tools-apply-change-set change-set-id)
               (should-error (deslop-tools-change-set change-set-id)))
             (with-current-buffer buffer
              (should (string= (buffer-string) "hello Lisp\n"))
              (should (cl-some #'deslop-review-overlay-p
                               (overlays-in (point-min) (point-max)))))))
          (let ((failure
                 (deslop-tool-call
                  "edit_source"
                  '(("path" . "main.el")
                    ("base_digest" . "stale")
                    ("edits" . ((("start" . 0)
                                  ("end" . 5)
                                  ("expected" . "hello")
                                  ("replacement" . "bye")))))
                  root)))
            (should-not (plist-get failure :ok)))
      (when (buffer-live-p buffer)
        (set-buffer-modified-p nil)
        (kill-buffer buffer))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest deslop-test/project-tools-can-directly-write-closed-files ()
  (let* ((root (make-temp-file "deslop-tools-direct-project-" t))
         (path (expand-file-name "main.el" root)))
    (unwind-protect
        (progn
          (deslop-tools-clear-change-sets)
          (with-temp-file path
            (insert "old\n"))
          (let* ((deslop-tools-direct-write t)
                 (info (deslop-test--tool-result
                        "file_info" '(("path" . "main.el")) root))
                 (result (deslop-test--tool-result
                          "edit_source"
                          (list
                           (cons "path" "main.el")
                           (cons "base_digest" (plist-get info :digest))
                           (cons "edits"
                                 (list
                                  (list (cons "start" 0)
                                        (cons "end" 3)
                                        (cons "expected" "old")
                                        (cons "replacement" "new")))))
                          root)))
            (should (string= (plist-get result :status) "applied"))
            (should (string= (with-temp-buffer
                               (insert-file-contents path)
                               (buffer-string))
                             "new\n"))))
       (when (file-directory-p root)
         (delete-directory root t)))))

(ert-deftest deslop-test/project-tools-retain-failed-change-sets-until-settled ()
  (let* ((root (make-temp-file "deslop-tools-pending-project-" t))
         (path (expand-file-name "main.el" root))
         (deslop-project-root-directory root))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "old\n"))
          (let* ((info (deslop-test--tool-result
                        "file_info" '(("path" . "main.el")) root))
                 (proposal (deslop-test--tool-result
                            "edit_source"
                            (list
                             (cons "path" "main.el")
                             (cons "base_digest" (plist-get info :digest))
                             (cons "edits"
                                   (list
                                    (list (cons "start" 0)
                                          (cons "end" 3)
                                          (cons "expected" "old")
                                          (cons "replacement" "new")))))
                            root))
                 (id (plist-get proposal :change_set_id)))
            (with-temp-file path
              (insert "changed\n"))
            (should-error (deslop-tools-apply-change-set id))
            (should (deslop-change-set-p (deslop-tools-change-set id)))
            (deslop-tools-reject-change-set id)
            (should-error (deslop-tools-change-set id))))
       (when (file-directory-p root)
         (delete-directory root t)))))

(ert-deftest deslop-test/project-tools-reject-ambiguous-boundary-edits ()
  (should-error
   (deslop-tools--validate-edits
    '((("start" . 3) ("end" . 3) ("expected" . "")
       ("replacement" . "!"))
      (("start" . 3) ("end" . 6) ("expected" . "XYZ")
       ("replacement" . "Q")))
    "abcXYZ")))

(defmacro deslop-test--with-buffer (contents &rest body)
  "Evaluate BODY in a temporary buffer containing CONTENTS."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,contents)
     ,@body))

(ert-deftest deslop-test/reset-does-not-kill-unowned-named-buffers ()
  (let* ((deslop-history-buffer-name " *deslop-unowned-history*")
         (deslop-summary-buffer-name " *deslop-unowned-summary*")
         (history (get-buffer-create deslop-history-buffer-name))
         (summary (get-buffer-create deslop-summary-buffer-name)))
    (unwind-protect
        (progn
           (setq deslop-history--buffer-object nil
                 deslop-summary--buffer-object nil)
          (deslop-test--reset)
          (should (buffer-live-p history))
          (should (buffer-live-p summary)))
      (when (buffer-live-p history)
        (kill-buffer history))
      (when (buffer-live-p summary)
        (kill-buffer summary)))))

;;;; Normal operations

(ert-deftest deslop-test/insert-no-selection ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" there"))))
    (deslop-test--with-buffer "hello world"
      (goto-char 6)
      (let ((job (deslop-insert-at-cursor)))
        (should (eq (deslop-job-state job) 'completed))
        (should (string= (buffer-string) "hello there world"))
        (let ((user-msg (cdr (cadr (car deslop-test--prompts)))))
          (should (string-match "hello<CURSOR> world" user-msg)))))))

(ert-deftest deslop-test/insert-at-buffer-boundaries ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '("START " " END"))))
    (deslop-test--with-buffer "middle"
      (goto-char (point-min))
      (deslop-insert-at-cursor)
      (goto-char (point-max))
      (deslop-insert-at-cursor)
      (should (string= (buffer-string) "START middle END")))))

(ert-deftest deslop-test/insert-selected-placements ()
  (deslop-test--reset)
  (let ((deslop-llm-function
         (deslop-test--fake-llm '("HEADER" "FOOTER") t)))
    (deslop-test--with-buffer "alpha beta"
      (deslop-test--select 1 11 1)
      (setq deslop--queue-suspended t)
      (deslop-insert-at-cursor)
      (deslop-test--select 1 11 11)
      (deslop-insert-at-cursor)
      (setq deslop--queue-suspended nil)
      (deslop-process-next)
      (deslop-test--complete-call (nth 0 deslop-test--calls))
      (deslop-test--complete-call (nth 1 deslop-test--calls))
      (should (string= (buffer-string)
                       "HEADER\nalpha beta\nFOOTER")))))

(ert-deftest deslop-test/insert-does-not-mutate-selected-characters ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '("MIDDLE"))))
    (deslop-test--with-buffer "foobar"
      (let ((overlay (deslop-ui-create-overlay (current-buffer) 1 7 "Queued"))
            (request nil))
        (setq request
              (list 'insert
                    :target
                    (deslop-target-create
                     :buffer (current-buffer) :start 1 :end 7 :cursor 4
                     :selected t :placement :inside :offset 3
                     :source-snapshot "foobar")
                    :overlay overlay))
      (deslop-enqueue request))
      (should (string= (buffer-string) "foo\nMIDDLE\nbar")))))

(ert-deftest deslop-test/insert-large-context-elision ()
  (deslop-test--reset)
  (let ((deslop-target-max-context-chars 30)
        (deslop-target-window-chars 10)
        (deslop-llm-function (deslop-test--fake-llm '("X"))))
    (deslop-test--with-buffer
        (concat (make-string 20 ?A) (make-string 20 ?B))
      (goto-char 21)
      (deslop-insert-at-cursor)
      (let ((user-msg (cdr (cadr (car deslop-test--prompts)))))
        (should (string-match-p "characters elided" user-msg))
        (should (string-match "AAAA<CURSOR>BBBB" user-msg))))))

(ert-deftest deslop-test/target-context-window-enforces-source-budget ()
  (let* ((deslop-target-max-context-chars 30)
         (deslop-target-window-chars 100)
         (context (deslop-target-string-marker-context
                    (concat (make-string 50 ?A) (make-string 50 ?B)) 50))
         (source-chars (+ (cl-count ?A context) (cl-count ?B context))))
    (should (<= source-chars deslop-target-max-context-chars))
    (should (string-match-p "<CURSOR>" context))
    (should (string-match-p "characters elided" context))))

(ert-deftest deslop-test/large-replacement-targets-are-rejected-before-queueing ()
  (deslop-test--reset)
  (let ((deslop-target-max-context-chars 4))
    (deslop-test--with-buffer "oversized"
      (should-error (deslop-improve-at-cursor "rewrite") :type 'user-error)
      (should-error (deslop-summarize) :type 'user-error)
      (should-not deslop-queue)
      (should-not deslop--current-job)
      (should-not deslop-ui--managed-overlays))))

(ert-deftest deslop-test/final-request-budget-includes-instructions ()
  (deslop-test--reset)
  (let ((deslop-max-request-chars 100)
        (deslop-insert-instruction (make-string 200 ?X))
        (deslop-llm-function (deslop-test--fake-llm '("never"))))
    (deslop-test--with-buffer "base"
      (let ((job (deslop-insert-at-cursor)))
        (should (eq (deslop-job-state job) 'failed))
        (should-not deslop-test--calls)
        (should (string= (buffer-string) "base"))))))

(ert-deftest deslop-test/custom-transport-output-is-bounded ()
  (deslop-test--reset)
  (let ((deslop-max-output-chars 3)
        (deslop-llm-function (deslop-test--fake-llm '("four"))))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (let ((job (deslop-insert-at-cursor)))
         (should (eq (deslop-job-state job) 'failed))
         (should (string= (buffer-string) "base"))))))

(ert-deftest deslop-test/custom-transport-output-can-be-unbounded ()
  (deslop-test--reset)
  (let ((deslop-max-output-chars nil)
        (deslop-llm-function (deslop-test--fake-llm '("four"))))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (let ((job (deslop-insert-at-cursor)))
        (should (eq (deslop-job-state job) 'completed))
        (should (string= (buffer-string) "basefour"))))))

(ert-deftest deslop-test/improve-replaces-selection ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '("improved"))))
    (deslop-test--with-buffer "old text"
      (deslop-test--select 1 9 9)
      (let ((job (deslop-improve-at-cursor "make it better")))
        (should (eq (deslop-job-state job) 'completed))
        (should (string= (buffer-string) "improved"))
        (should (cl-some #'deslop-review-overlay-p
                         (overlays-in (point-min) (point-max))))))))

(ert-deftest deslop-test/summarize-opens-buffer ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '("short summary")))
        (displayed nil))
    (cl-letf (((symbol-function 'display-buffer)
               (lambda (buffer &rest _)
                 (setq displayed buffer))))
      (deslop-test--with-buffer "long source"
        (let ((job (deslop-summarize)))
          (should (eq (deslop-job-state job) 'completed))
          (should (eq displayed (get-buffer deslop-summary-buffer-name)))
          (should (string-match-p
                   "short summary"
                   (with-current-buffer deslop-summary-buffer-name
                     (buffer-string))))
          (with-current-buffer deslop-summary-buffer-name
            (should (cl-some #'deslop-review-overlay-p
                             (overlays-in (point-min) (point-max))))))))))

(ert-deftest deslop-test/reject-summary-removes-entire-appended-block ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '("short summary"))))
    (deslop-test--with-buffer "source"
      (deslop-summarize))
    (with-current-buffer deslop-summary-buffer-name
      (goto-char (point-max))
      (deslop-reject-output)
      (should (string-empty-p (buffer-string))))))

(ert-deftest deslop-test/summarize-reports-progress-and-preserves-buffer-point ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '("summary") t)))
     (with-current-buffer (deslop-summary-buffer)
      (insert "existing")
      (goto-char (point-min)))
    (deslop-test--with-buffer "source"
      (let* ((job (deslop-summarize))
             (call (car deslop-test--calls))
             (overlay (deslop-job-overlay job)))
        (deslop-test--callback
         call :on-event '(:type :status :status "Generating"))
        (deslop-test--callback
         call :on-event '(:type :usage :tokens 12 :speed 34.0))
        (should (string= (overlay-get overlay 'status) "Generating"))
        (should (= (overlay-get overlay 'tokens) 12))
        (deslop-test--complete-call call)))
    (with-current-buffer deslop-summary-buffer-name
      (should (= (point) (point-min))))))

(ert-deftest deslop-test/output-remains-marked-until-approved ()
  (deslop-test--reset)
  (let ((deslop-provider-model "test-model")
        (deslop-llm-function (deslop-test--fake-llm '(" generated") t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (let* ((job (deslop-insert-at-cursor))
             (queued-overlay (deslop-job-overlay job)))
        (should (string-match-p "test-model"
                                (deslop-test--overlay-label queued-overlay)))
        (should (string-match-p "localhost"
                                (deslop-test--overlay-label queued-overlay)))
        (deslop-test--complete-call (car deslop-test--calls))
        (let ((review (cl-find-if
                       #'deslop-review-overlay-p
                       (overlays-in (point-min) (point-max)))))
          (should review)
          (should (string= (overlay-get review 'model) "test-model"))
          (should (string= (overlay-get review 'client) "localhost"))
          (should (string-match-p "test-model"
                                  (deslop-test--overlay-label review)))
           (should (string-match-p "localhost"
                                   (deslop-test--overlay-label review)))
            (dolist (key '("RET" "C-m" "DEL" "<backspace>" "C-d"))
              (should-not (lookup-key (overlay-get review 'keymap)
                                      (kbd key))))
            (should (eq (lookup-key (overlay-get review 'keymap)
                                    (kbd "C-c C-c"))
                        #'deslop-approve-output))
            (should (eq (lookup-key (overlay-get review 'keymap)
                                    (kbd "C-c C-k"))
                        #'deslop-reject-output))
           (goto-char (overlay-end review))
           (should-not (eq (key-binding (kbd "RET"))
                           #'deslop-approve-output))
           (goto-char (1+ (overlay-start review)))
           (insert "edited ")
           (should (deslop-review-overlay-p review))
           (deslop-approve-output)
           (should-not (deslop-review-overlay-p review)))))))

(ert-deftest deslop-test/enter-works-after-editing-past-review-output ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" generated") t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (deslop-insert-at-cursor)
      (let ((call (car deslop-test--calls)))
        (deslop-test--complete-call call)
        (let ((review (cl-find-if
                       #'deslop-review-overlay-p
                       (overlays-in (point-min) (point-max)))))
          (goto-char (overlay-end review))
          (newline)
          (insert "typed")
          (should-not (eq (key-binding (kbd "RET"))
                          #'deslop-approve-output))
          (funcall (key-binding (kbd "RET")))
          (should (string= (buffer-string) "base generated\ntyped\n"))
          (should (deslop-review-overlay-p review)))))))

(ert-deftest deslop-test/task-cannot-start-inside-pending-review ()
  (deslop-test--reset)
  (deslop-test--with-buffer "generated text"
    (let ((review (deslop-review-create-overlay
                   (current-buffer) 1 (point-max) nil nil "")))
      (goto-char 5)
      (should-error (deslop-insert-at-cursor) :type 'user-error)
      (deslop-test--select 2 6 6)
      (should-error (deslop-improve-at-cursor "rewrite") :type 'user-error)
      (should-error (deslop-summarize) :type 'user-error)
      (should-not deslop-queue)
      (deslop-review-delete-overlay review))))

(ert-deftest deslop-test/queued-task-cannot-dispatch-into-new-review ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" never") t)))
    (deslop-test--with-buffer "base"
      (goto-char 3)
      (setq deslop--queue-suspended t)
      (let ((job (deslop-insert-at-cursor))
            (review (deslop-review-create-overlay
                     (current-buffer) 1 (point-max) nil nil "")))
        (setq deslop--queue-suspended nil)
        (deslop-process-next)
        (should (eq (deslop-job-state job) 'failed))
        (should-not deslop-test--calls)
        (deslop-review-delete-overlay review)))))

(ert-deftest deslop-test/review-creation-failure-leaves-no-overlay ()
  (deslop-test--reset)
  (deslop-test--with-buffer "text"
    (let ((deslop-review-highlight-blend 'invalid))
      (should-error
       (deslop-review-create-overlay (current-buffer) 1 3 nil nil ""))
      (should-not (deslop-review--overlays-in-buffer)))))

(ert-deftest deslop-test/response-metadata-wins-for-review-label ()
  (deslop-test--reset)
  (let ((deslop-provider-model "configured-model")
        (deslop-llm-function
         (deslop-test--fake-llm
          '((:content " generated"
             :reported-model "actual-model"
             :reported-provider "llama.cpp"))
          t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (let ((job (deslop-insert-at-cursor)))
        (deslop-test--complete-call (car deslop-test--calls))
        (let ((review (cl-find-if
                       #'deslop-review-overlay-p
                       (overlays-in (point-min) (point-max)))))
          (should review)
          (should (string= (overlay-get review 'model) "actual-model"))
          (should (string= (overlay-get review 'client) "llama.cpp"))
          (should (string-match-p "actual-model"
                                  (deslop-test--overlay-label review)))
          (should-not (string-match-p "configured-model"
                                      (deslop-test--overlay-label review)))
          (should (eq (deslop-job-state job) 'completed)))))))

(ert-deftest deslop-test/response-metadata-cannot-be-overwritten-by-discovery ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" X") t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (let* ((job (deslop-insert-at-cursor))
             (call (car deslop-test--calls)))
        (deslop-test--callback
         call :on-event
         '(:type :metadata :metadata
           (:source :response :model "response-model" :provider "llama.cpp")))
        (deslop-test--callback
         call :on-event
         '(:type :metadata :metadata
           (:source :discovery :model "stale-model" :provider "stale")))
        (should (string= (overlay-get (deslop-job-overlay job) 'model)
                         "response-model"))
        (should (string= (overlay-get (deslop-job-overlay job) 'client)
                         "llama.cpp"))))))

(ert-deftest deslop-test/reject-restores-properties-and-saving-remains-allowed ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '("new")))
        (kill-ring nil)
        (kill-ring-yank-pointer nil))
    (deslop-test--with-buffer "old"
      (add-text-properties 1 4 '(deslop-test-property original))
      (deslop-test--select 1 4 4)
      (deslop-improve-at-cursor "rewrite")
      (run-hooks 'before-save-hook)
      (goto-char 2)
      (deslop-reject-output)
      (should (string= (buffer-string) "old"))
      (should (equal kill-ring '("new")))
      (should (eq (get-text-property 1 'deslop-test-property) 'original))
      (should-not (deslop-review--overlays-in-buffer)))))

(ert-deftest deslop-test/mass-review-actions-cover-the-whole-buffer ()
  (deslop-test--reset)
  (let ((kill-ring nil)
        (kill-ring-yank-pointer nil))
    (deslop-test--with-buffer "XY"
      (let ((first (deslop-review-create-overlay
                    (current-buffer) 1 2 nil nil "A"))
            (second (deslop-review-create-overlay
                     (current-buffer) 2 3 nil nil "B")))
        (should (= (length (deslop-review--overlays-in-buffer)) 2))
        (deslop-reject-all-output)
        (should (string= (buffer-string) "AB"))
        (should (equal kill-ring '("X" "Y")))
        (should-not (deslop-review--overlays-in-buffer))
        (should-not (deslop-review-overlay-p first))
        (should-not (deslop-review-overlay-p second))
        (deslop-review-create-overlay
         (current-buffer) 1 2 nil nil "A")
        (deslop-review-create-overlay
         (current-buffer) 2 3 nil nil "B")
        (deslop-approve-all-output)
        (should-not (deslop-review--overlays-in-buffer))))))

(ert-deftest deslop-test/write-anywhere-creates-independent-reviews ()
  (deslop-test--reset)
  (let* ((response
          "{\"version\":1,\"edits\":[{\"anchor\":\"one\",\"position\":\"before\",\"text\":\"A\"},{\"anchor\":\"two\",\"position\":\"replace\",\"text\":\"TWO\"}]}" )
         (deslop-llm-function
          (deslop-test--fake-llm (list (list :content response)))))
    (deslop-test--with-buffer "one two"
      (let ((job (deslop-write-anywhere)))
        (should (eq (deslop-job-state job) 'completed))
         (should (string= (buffer-string) "Aone TWO"))
        (should-not buffer-read-only)
        (let ((reviews (deslop-review--overlays-in-buffer)))
          (should (= (length reviews) 2))
          (goto-char (overlay-start (car reviews)))
          (deslop-approve-output)
          (should (= (length (deslop-review--overlays-in-buffer)) 1))
          (let ((remaining (car (deslop-review--overlays-in-buffer))))
            (goto-char (overlay-start remaining))
            (deslop-reject-output))
           (should (string= (buffer-string) "Aone two")))))))

(ert-deftest deslop-test/write-anywhere-applies-valid-edits-around-invalid-ones ()
  (deslop-test--reset)
  (let* ((response
           "{\"version\":1,\"edits\":[{\"anchor\":\"one\",\"position\":\"replace\",\"text\":\"ONE\"},{\"anchor\":\"missing\",\"position\":\"replace\",\"text\":\"bad\"},{\"anchor\":\"two\",\"position\":\"after\",\"text\":\"!\"}]}" )
         (repair-response
          "{\"version\":1,\"edits\":[{\"anchor\":\"missing\",\"position\":\"replace\",\"text\":\"bad\"}]}" )
          (deslop-llm-function
           (deslop-test--fake-llm
            (list (list :content response)
                  (list :content repair-response)))))
    (deslop-test--with-buffer "one two"
      (let ((job (deslop-write-anywhere)))
        (should (eq (deslop-job-state job) 'completed))
        (should (string= (buffer-string) "ONE two!"))
        (should (= (length (deslop-review--overlays-in-buffer)) 2))
        (with-current-buffer deslop-history--buffer-object
           (let ((history (buffer-string)))
             (should (string-match-p "Skipped edits" history))
             (should (string-match-p "COMPLETED WITH WARNINGS" history))
             (should (string-match-p "Requested: 3" history))
             (should (string-match-p "Edit 2: anchor was not found" history))
            (should (string-match-p
                     (regexp-quote "{\"anchor\":\"missing\"")
                     history))))))))

(ert-deftest deslop-test/write-anywhere-supports-occurrence-and-all-anchors ()
  (deslop-test--reset)
  (let* ((response
          "{\"version\":1,\"edits\":[{\"anchor\":\"one\",\"position\":\"replace\",\"all\":true,\"text\":\"ONE\"}]}" )
         (deslop-llm-function
          (deslop-test--fake-llm (list (list :content response)))))
    (deslop-test--with-buffer "one\none"
      (let ((job (deslop-write-anywhere)))
        (should (eq (deslop-job-state job) 'completed))
        (should (string= (buffer-string) "ONE\nONE"))
        (should (= (length (deslop-review--overlays-in-buffer)) 2))))))

(ert-deftest deslop-test/write-anywhere-retains-all-skipped-edits-on-failure ()
  (deslop-test--reset)
  (let* ((response
           "{\"version\":1,\"edits\":[{\"anchor\":\"missing\",\"position\":\"replace\",\"text\":\"bad\"}]}" )
         (repair-response
          "{\"version\":1,\"edits\":[{\"anchor\":\"missing\",\"position\":\"before\",\"text\":\"bad\"}]}" )
          (deslop-llm-function
           (deslop-test--fake-llm
            (list (list :content response)
                  (list :content repair-response)))))
    (deslop-test--with-buffer "test"
      (let ((job (deslop-write-anywhere)))
        (should (eq (deslop-job-state job) 'failed))
        (should (string= (buffer-string) "test"))
        (should-not (deslop-review--overlays-in-buffer))
        (with-current-buffer deslop-history--buffer-object
          (let ((history (buffer-string)))
            (should (string-match-p "Skipped edits" history))
            (should (string-match-p "Edit 1: anchor was not found" history))
            (should (string-match-p
                     (regexp-quote "{\"anchor\":\"missing\"")
                     history))))))))

(ert-deftest deslop-test/insert-anywhere-rejects-character-offsets ()
  (deslop-test--reset)
  (let* ((response
          "{\"version\":1,\"edits\":[{\"start\":1,\"end\":1,\"expected\":\"\",\"text\":\"X\"}]}" )
         (deslop-llm-function
          (deslop-test--fake-llm (list (list :content response)))))
    (deslop-test--with-buffer "test"
      (let ((job (deslop-insert-anywhere)))
        (should (eq (deslop-job-state job) 'failed))
        (should (string= (buffer-string) "test"))
         (should-not (deslop-review--overlays-in-buffer))))))

(ert-deftest deslop-test/insert-anywhere-reports-missing-fields ()
  (deslop-test--reset)
  (let* ((response
           "{\"version\":1,\"edits\":[{\"position\":\"before\",\"text\":\"X\"}]}" )
         (repair-response response)
          (deslop-llm-function
           (deslop-test--fake-llm
            (list (list :content response)
                  (list :content repair-response)))))
    (deslop-test--with-buffer "test"
      (let ((job (deslop-insert-anywhere)))
        (should (eq (deslop-job-state job) 'failed))
        (should (string-match-p "missing required anchor, position, or text field"
                               (deslop-error-message (deslop-job-error job))))))))

(ert-deftest deslop-test/insert-anywhere-creates-independent-insertions ()
  (deslop-test--reset)
  (let* ((response
          "{\"version\":1,\"edits\":[{\"anchor\":\"test\",\"position\":\"before\",\"text\":\"A\"},{\"anchor\":\"test\",\"position\":\"after\",\"text\":\"B\"}]}" )
         (deslop-llm-function
          (deslop-test--fake-llm (list (list :content response)))))
    (deslop-test--with-buffer "test"
      (let ((job (deslop-insert-anywhere
                  "Add a marker at every appropriate position")))
        (should (eq (deslop-job-state job) 'completed))
        (should (string= (buffer-string) "AtestB"))
        (should (= (length (deslop-review--overlays-in-buffer)) 2))
        (let ((messages (car deslop-test--prompts)))
          (should (string-match-p "INSERT-ANYWHERE"
                                  (cdr (cadr messages))))
          (should (string-match-p
                   "Add a marker at every appropriate position"
                   (cdr (cadr messages))))
           (should (string-match-p "Multiple insertions"
                                   (cdr (car messages)))))))))

(ert-deftest deslop-test/insert-anywhere-applies-valid-edits-around-invalid-ones ()
  (deslop-test--reset)
  (let* ((response
           "{\"version\":1,\"edits\":[{\"anchor\":\"aa\",\"position\":\"before\",\"text\":\"A\"},{\"anchor\":\"missing\",\"position\":\"before\",\"text\":\"bad\"},{\"anchor\":\"bb\",\"position\":\"after\",\"text\":\"B\"}]}" )
         (repair-response
          "{\"version\":1,\"edits\":[{\"anchor\":\"missing\",\"position\":\"before\",\"text\":\"bad\"}]}" )
          (deslop-llm-function
           (deslop-test--fake-llm
            (list (list :content response)
                  (list :content repair-response)))))
    (deslop-test--with-buffer "aa\nbb"
      (let ((job (deslop-insert-anywhere)))
        (should (eq (deslop-job-state job) 'completed))
        (should (string= (buffer-string) "Aaa\nbbB"))
        (should (= (length (deslop-review--overlays-in-buffer)) 2))
        (should (string-match-p
                 "Skipped edits"
                 (with-current-buffer deslop-history--buffer-object
                   (buffer-string))))
        (should (string-match-p
                 "Edit 2: anchor was not found"
                 (with-current-buffer deslop-history--buffer-object
                   (buffer-string))))))))

(ert-deftest deslop-test/insert-anywhere-supports-occurrence-and-all-anchors ()
  (deslop-test--reset)
  (let* ((response
           "{\"version\":1,\"edits\":[{\"anchor\":\"one\",\"position\":\"before\",\"occurrence\":1,\"text\":\"A\"},{\"anchor\":\"one\",\"position\":\"after\",\"all\":true,\"text\":\"B\"}]}" )
         (deslop-llm-function
          (deslop-test--fake-llm (list (list :content response)))))
    (deslop-test--with-buffer "one\none"
      (let ((job (deslop-insert-anywhere)))
        (should (eq (deslop-job-state job) 'completed))
         (should (string= (buffer-string) "AoneB\noneB"))
         (should (= (length (deslop-review--overlays-in-buffer)) 3))))))

(ert-deftest deslop-test/structured-edit-normalized-expansion-is-bounded ()
  (let ((deslop-max-normalized-edits 2))
    (should-error
     (deslop-edit-parse-insertions
      "{\"version\":1,\"edits\":[{\"anchor\":\"x\",\"position\":\"before\",\"all\":true,\"text\":\"!\"}]}"
      "x x x")
     :type 'deslop-edit-resource-error)))

(ert-deftest deslop-test/structured-edit-conflicts-use-a-sweep ()
  (let ((parsed
         (deslop-edit-parse-replacements
          "{\"version\":1,\"edits\":[{\"anchor\":\"abc\",\"position\":\"replace\",\"text\":\"A\"},{\"anchor\":\"bcd\",\"position\":\"replace\",\"text\":\"B\"}]}"
          "abcdef")))
    (should-not (plist-get parsed :edits))
    (should (= (length (plist-get parsed :errors)) 2))))

(ert-deftest deslop-test/structured-edit-rejects-before-replace-at-same-anchor ()
  (let ((parsed
         (deslop-edit-parse-replacements
          "{\"version\":1,\"edits\":[{\"anchor\":\"abc\",\"position\":\"replace\",\"text\":\"X\"},{\"anchor\":\"abc\",\"position\":\"before\",\"text\":\"Y\"}]}"
          "abc")))
    (should-not (plist-get parsed :edits))
    (should (= (length (plist-get parsed :errors)) 2))
    (should (string-match-p
             "overlaps"
             (plist-get (car (plist-get parsed :errors)) :message)))))

(ert-deftest deslop-test/structured-edit-orders-adjacent-after-before-replace ()
  (deslop-test--reset)
  (let ((deslop-llm-function
         (deslop-test--fake-llm
          (list
           (list :content
                 "{\"version\":1,\"edits\":[{\"anchor\":\"XYZ\",\"position\":\"replace\",\"text\":\"Q\"},{\"anchor\":\"abc\",\"position\":\"after\",\"text\":\"!\"}]}")))))
    (deslop-test--with-buffer "abcXYZ"
      (let ((job (deslop-write-anywhere)))
        (should (eq (deslop-job-state job) 'completed))
        (should (string= (buffer-string) "abc!Q"))))))

(ert-deftest deslop-test/structured-edit-repairs-failed-edits ()
  (deslop-test--reset)
  (let* ((first-response
          "{\"version\":1,\"edits\":[{\"anchor\":\"one\",\"position\":\"replace\",\"text\":\"ONE\"},{\"anchor\":\"two\",\"position\":\"replace\",\"occurrence\":2,\"text\":\"BAD\"}]}" )
         (repair-response
          "{\"version\":1,\"edits\":[{\"anchor\":\"two\",\"position\":\"replace\",\"text\":\"TWO\"}]}" )
         (deslop-llm-function
          (deslop-test--fake-llm
           (list (list :content first-response)
                 (list :content repair-response)))))
    (deslop-test--with-buffer "one two"
      (let* ((job (deslop-write-anywhere "Fix every item"))
             (summary (plist-get (deslop-job-result job)
                                 :structured-summary))
             (repair-user (cdr (cadr (car deslop-test--prompts)))))
        (should (eq (deslop-job-state job) 'completed))
        (should (string= (buffer-string) "ONE TWO"))
        (should (= (plist-get summary :requested) 2))
        (should (= (plist-get summary :first-pass-applied) 1))
        (should (= (plist-get summary :repaired) 1))
        (should (= (plist-get summary :skipped) 0))
        (should (= (plist-get summary :repair-attempts) 1))
        (should (string-match-p "Fix every item" repair-user))
        (should (string-match-p "occurrence 2 is not present" repair-user))
        (should (string-match-p "Current document:\nONE two" repair-user))
        (with-current-buffer deslop-history--buffer-object
          (should (string-match-p "Structured edits" (buffer-string)))
          (should (string-match-p "Repaired: 1" (buffer-string))))))))

(ert-deftest deslop-test/structured-edit-rejects-read-only-before-dispatch ()
  (deslop-test--reset)
  (let ((deslop-llm-function
         (deslop-test--fake-llm '("unused") t)))
    (deslop-test--with-buffer "test"
      (setq buffer-read-only t)
      (let ((job (deslop-write-anywhere)))
        (should (eq (deslop-job-state job) 'failed))
        (should-not deslop-test--calls)
        (should (string-match-p "read-only"
                                (error-message-string
                                 (deslop-job-error job))))))))

(ert-deftest deslop-test/structured-edit-rejects-read-only-before-application ()
  (deslop-test--reset)
  (let* ((response
          "{\"version\":1,\"edits\":[{\"anchor\":\"test\",\"position\":\"replace\",\"text\":\"TEST\"}]}" )
         (deslop-llm-function
          (deslop-test--fake-llm (list (list :content response)) t)))
    (deslop-test--with-buffer "test"
      (let ((job (deslop-write-anywhere)))
        (setq buffer-read-only t)
        (deslop-test--complete-call (car deslop-test--calls))
        (should (eq (deslop-job-state job) 'failed))
        (should (string= (buffer-string) "test"))
        (should-not (deslop-review--overlays-in-buffer))))))

(ert-deftest deslop-test/write-anywhere-leaves-buffer-writable-while-running ()
  (deslop-test--reset)
  (let* ((response
         "{\"version\":1,\"edits\":[{\"anchor\":\"test\",\"position\":\"replace\",\"text\":\"TEST\"}]}" )
         (deslop-llm-function
          (deslop-test--fake-llm (list (list :content response)) t)))
    (deslop-test--with-buffer "test"
      (let ((job (deslop-write-anywhere)))
        (should (eq (deslop-job-state job) 'running))
        (should-not buffer-read-only)
        (run-hooks 'before-save-hook)
        (goto-char (point-min))
        (insert "user\n")
        (should (string= (buffer-string) "user\ntest"))
        (deslop-test--complete-call (car deslop-test--calls))
        (should (eq (deslop-job-state job) 'completed))
        (should (string= (buffer-string) "user\nTEST"))
        (deslop-abort-queue)
        (should-not buffer-read-only)))))

(ert-deftest deslop-test/insert-anywhere-leaves-buffer-writable-while-running ()
  (deslop-test--reset)
  (let* ((response
          "{\"version\":1,\"edits\":[{\"anchor\":\"test\",\"position\":\"after\",\"text\":\"!\"}]}" )
         (deslop-llm-function
          (deslop-test--fake-llm (list (list :content response)) t)))
     (deslop-test--with-buffer "test"
        (let ((job (deslop-insert-anywhere)))
          (should (eq (deslop-job-state job) 'running))
          (should-not (deslop-job-overlay job))
          (should-not buffer-read-only)
          (deslop-abort-queue)
          (should-not buffer-read-only)))))

(ert-deftest deslop-test/mode-line-reports-buffer-job-count ()
  (deslop-test--reset)
  (let ((deslop-llm-function
         (deslop-test--fake-llm (list :stall :stall) t)))
    (deslop-test--with-buffer "test"
       (deslop-mode 1)
      (should (string= deslop--mode-line-string " deslop[0]"))
      (let ((first (deslop-write-anywhere))
            (second nil))
        (should (eq (deslop-job-state first) 'running))
        (should (string= deslop--mode-line-string " deslop[1]"))
        (setq second (deslop-write-anywhere))
        (should (eq (deslop-job-state second) 'queued))
        (should (string= deslop--mode-line-string " deslop[2]"))
        (should-not (deslop-job-overlay first))
        (should-not (deslop-job-overlay second))
         (deslop-abort-queue)
         (should (string= deslop--mode-line-string " deslop[0]"))))))

(ert-deftest deslop-test/mode-line-reports-pending-review-count ()
  (deslop-test--reset)
  (deslop-test--with-buffer "test"
    (deslop-mode 1)
    (should (string= deslop--mode-line-string " deslop[0]"))
    (let ((review (deslop-review-create-overlay
                   (current-buffer) 1 2 nil nil "t")))
      (should (string= deslop--mode-line-string " deslop[0|1]"))
      (deslop-approve-all-output)
      (should (string= deslop--mode-line-string " deslop[0]"))
      (deslop-review-create-overlay
       (current-buffer) 1 2 nil nil "t")
      (should (string= deslop--mode-line-string " deslop[0|1]"))
      (deslop-reject-all-output)
      (should (string= deslop--mode-line-string " deslop[0]"))
      (should-not (deslop-review-overlay-p review)))))

(ert-deftest deslop-test/insert-anywhere-reconciles-against-current-buffer ()
  (deslop-test--reset)
  (let* ((response
           "{\"version\":1,\"edits\":[{\"anchor\":\"test\",\"position\":\"after\",\"text\":\"!\"},{\"anchor\":\"missing\",\"position\":\"before\",\"text\":\"?\"}]}" )
         (repair-response
          "{\"version\":1,\"edits\":[{\"anchor\":\"missing\",\"position\":\"before\",\"text\":\"?\"}]}" )
          (deslop-llm-function
           (deslop-test--fake-llm
            (list (list :content response)
                  (list :content repair-response)) t)))
    (deslop-test--with-buffer "test"
      (let ((job (deslop-insert-anywhere)))
        (goto-char (point-min))
        (insert "user\n")
         (should (eq (deslop-job-state job) 'running))
         (deslop-test--complete-call (car deslop-test--calls))
         (should (eq (deslop-job-state job) 'running))
         (deslop-test--complete-call (nth 1 deslop-test--calls))
         (should (eq (deslop-job-state job) 'completed))
        (should (string= (buffer-string) "user\ntest!"))
        (should (= (length (deslop-review--overlays-in-buffer)) 1))
        (should (string-match-p
                 "Skipped edits"
                 (with-current-buffer deslop-history--buffer-object
                   (buffer-string))))))))

(ert-deftest deslop-test/write-anywhere-applies-from-another-current-buffer ()
  (deslop-test--reset)
  (let* ((response
          "{\"version\":1,\"edits\":[{\"anchor\":\"test\",\"position\":\"before\",\"text\":\"A\"}]}" )
         (deslop-llm-function
          (deslop-test--fake-llm (list (list :content response)) t)))
    (deslop-test--with-buffer "test"
      (let ((job (deslop-write-anywhere))
            (callback-buffer (generate-new-buffer " *deslop-callback*")))
        (unwind-protect
            (with-current-buffer callback-buffer
              (deslop-test--complete-call (car deslop-test--calls)))
          (when (buffer-live-p callback-buffer)
            (kill-buffer callback-buffer)))
        (should (eq (deslop-job-state job) 'completed))
        (should (string= (buffer-string) "Atest"))
        (should-not buffer-read-only)
        (should (= (length (deslop-review--overlays-in-buffer)) 1))))))

;;;; Queue lifecycle

(ert-deftest deslop-test/queue-fifo-ordering-async ()
  (deslop-test--reset)
  (let ((deslop-llm-function
         (deslop-test--fake-llm '(" 1" " 2" " 3") t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (setq deslop--queue-suspended t)
      (deslop-insert-at-cursor)
      (deslop-insert-at-cursor)
      (deslop-insert-at-cursor)
      (setq deslop--queue-suspended nil)
      (deslop-process-next)
      (should (= (length deslop-test--calls) 1))
      (deslop-test--complete-call (nth 0 deslop-test--calls))
      (deslop-test--complete-call (nth 1 deslop-test--calls))
      (deslop-test--complete-call (nth 2 deslop-test--calls))
      (should (string= (buffer-string) "base 1 2 3"))
      (should (null deslop-queue))
      (should (null deslop--current-job)))))

(ert-deftest deslop-test/abort-invalidates-before-kill ()
  (deslop-test--reset)
  (let ((deslop-llm-function
         (deslop-test--fake-llm '(:stall "never") t
                                #'deslop-test--sleep-process)))
    (deslop-test--with-buffer "content"
      (let ((job (deslop-insert-at-cursor)))
        (deslop-insert-at-cursor)
        (let ((call (car deslop-test--calls)))
          (should deslop--current-job)
          (deslop-abort-queue)
          (should (eq (deslop-job-state job) 'cancelled))
          (should-not deslop--current-job)
          (should (null deslop-queue))
          (should-not (process-live-p (plist-get call :process)))
          ;; A late transport callback must be inert.
          (deslop-test--callback call :on-done "orphan")
          (should (string= (buffer-string) "content")))))))

(ert-deftest deslop-test/stale-callback-cannot-settle-next-job ()
  (deslop-test--reset)
  (let ((deslop-llm-function
         (deslop-test--fake-llm '(" A" " B") t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (setq deslop--queue-suspended t)
      (deslop-insert-at-cursor)
      (deslop-insert-at-cursor)
      (setq deslop--queue-suspended nil)
      (deslop-process-next)
      (let ((first (nth 0 deslop-test--calls)))
        (deslop-test--complete-call first)
        (let ((second (nth 1 deslop-test--calls)))
          (should second)
          (should (eq (deslop-job-state deslop--current-job) 'running))
          (deslop-test--callback first :on-done " stale")
          (deslop-test--callback first :on-error "stale error")
          (should (eq (deslop-job-state deslop--current-job) 'running))
          (deslop-test--complete-call second)))
      (should (string= (buffer-string) "base A B")))))

(ert-deftest deslop-test/duplicate-completion-is-idempotent ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" X") t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (let ((job (deslop-insert-at-cursor))
            (call nil))
        (setq call (car deslop-test--calls))
        (deslop-test--complete-call call)
        (deslop-test--callback call :on-done " X")
        (should (eq (deslop-job-state job) 'completed))
        (should (string= (buffer-string) "base X"))
        (should-not deslop--current-job)))))

(ert-deftest deslop-test/deleted-pending-overlay-cannot-fail-committed-insert ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" A" " B") t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (setq deslop--queue-suspended t)
      (let ((first (deslop-insert-at-cursor))
            (second (deslop-insert-at-cursor)))
        (delete-overlay (deslop-job-overlay second))
        (setq deslop--queue-suspended nil)
        (deslop-process-next)
        (deslop-test--complete-call (nth 0 deslop-test--calls))
        (should (eq (deslop-job-state first) 'completed))
        (deslop-test--complete-call (nth 1 deslop-test--calls))
        (should (eq (deslop-job-state second) 'completed))
        (should (string= (buffer-string) "base A B"))))))

(ert-deftest deslop-test/malformed-structured-error-cannot-wedge-queue ()
  (deslop-test--reset)
  (let ((deslop-llm-function
         (lambda (_messages callbacks &optional _options)
           (funcall (plist-get callbacks :on-error)
                    (deslop-error-create :kind 'provider :message 123)))))
    (deslop-test--with-buffer "base"
      (let ((job (deslop-insert-at-cursor)))
        (should (eq (deslop-job-state job) 'failed))
        (should-not deslop--current-job)
        (should-not deslop--current-job)))))

(ert-deftest deslop-test/synchronous-failure-backlog-is-iterative ()
  (deslop-test--reset)
  (deslop-test--with-buffer "base"
    (let ((deslop--queue-suspended t)
          (max-lisp-eval-depth 100))
      (dotimes (_ 500)
        (deslop-enqueue
         (list 'unknown
               :target (deslop-target-create
                        :buffer (current-buffer) :start 1 :end 1 :cursor 1))))
      (setq deslop--queue-suspended nil)
      (cl-letf (((symbol-function 'message) #'ignore))
        (deslop-process-next))
      (should-not deslop--current-job)
      (should-not deslop-queue))))

(ert-deftest deslop-test/quit-and-cleanup-errors-still-settle-jobs ()
  (deslop-test--reset)
  (let ((deslop--job-handlers (copy-tree deslop--job-handlers)))
    (deslop-register-job-handler
     'quit-test (lambda (_job) (signal 'quit nil)))
    (deslop-test--with-buffer "base"
      (let ((job (deslop-enqueue
                  (list 'quit-test
                        :target (deslop-target-create
                                 :buffer (current-buffer)
                                 :start 1 :end 1 :cursor 1)))))
        (should (eq (deslop-job-state job) 'failed))
        (should-not deslop--current-job))))
  (deslop-test--reset)
  (let ((deslop--job-handlers (copy-tree deslop--job-handlers)))
    (deslop-register-job-handler
     'cleanup-test (lambda (job) (deslop-job-finish job "ok")))
    (deslop-test--with-buffer "base"
      (let* ((overlay (deslop-ui-create-overlay
                       (current-buffer) 1 1 "Queued"))
             (job nil))
        (cl-letf (((symbol-function 'deslop-ui-delete-overlay)
                   (lambda (_) (error "cleanup failed"))))
          (setq job
                 (deslop-enqueue
                  (list 'cleanup-test
                        :target (deslop-target-create
                                 :buffer (current-buffer)
                                 :start 1 :end 1 :cursor 1)
                        :overlay overlay))))
        (should (eq (deslop-job-state job) 'completed))
        (should-not deslop--current-job)
        (delete-overlay overlay)))))

;;;; Target failure paths

(ert-deftest deslop-test/killed-source-buffer-cancels-active-and-queued-jobs ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" X") t))
        (buffer (generate-new-buffer " *deslop-killed*"))
        (active nil)
        (pending nil))
    (unwind-protect
        (progn
          (with-current-buffer buffer
             (insert "base")
             (goto-char (point-max))
             (setq active (deslop-insert-at-cursor)))
          (let ((deslop--queue-suspended t))
            (with-current-buffer buffer
              (setq pending (deslop-insert-at-cursor))))
          (should (eq (deslop-job-state active) 'running))
          (should (eq (deslop-job-state pending) 'queued))
          (kill-buffer buffer)
          (should (eq (deslop-job-state active) 'cancelled))
          (should (eq (deslop-job-state pending) 'cancelled))
          (should-not deslop--current-job)
          (should-not deslop-queue)
          (deslop-test--callback (car deslop-test--calls) :on-done "late")
          (should (eq (deslop-job-state active) 'cancelled)))
       (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest deslop-test/source-buffer-cancellation-preserves-outer-abort ()
  (deslop-test--reset)
  (let ((first (generate-new-buffer " *deslop-abort-first*"))
        (second (generate-new-buffer " *deslop-abort-second*"))
        (observed nil)
        (first-job nil)
        (second-job nil))
    (unwind-protect
        (progn
          (with-current-buffer first
            (insert "first")
            (setq first-job
                  (deslop--make-job
                   :id 1 :state 'running
                   :target (deslop-target-create
                            :buffer first :start 1 :end 1 :cursor 1)
                   :cancel-function
                   (lambda ()
                     (kill-buffer second)
                     (setq observed deslop--aborting)))
                  deslop--source-buffer-jobs (list first-job))
            (add-hook 'kill-buffer-hook #'deslop--source-buffer-killed nil t))
          (with-current-buffer second
            (insert "second")
            (setq second-job
                  (deslop--make-job
                   :id 2 :state 'queued
                   :target (deslop-target-create
                            :buffer second :start 1 :end 1 :cursor 1)))
            (setq deslop--source-buffer-jobs (list second-job))
            (add-hook 'kill-buffer-hook #'deslop--source-buffer-killed nil t))
          (setq deslop--active-jobs (list first-job)
                deslop--current-job first-job
                deslop-queue (list second-job)
                deslop--queue-tail deslop-queue)
          (deslop-abort-queue)
          (should observed)
          (should-not deslop--aborting)
          (should (eq (deslop-job-state first-job) 'cancelled))
          (should (eq (deslop-job-state second-job) 'cancelled)))
      (when (buffer-live-p first) (kill-buffer first))
      (when (buffer-live-p second) (kill-buffer second)))))

(ert-deftest deslop-test/read-only-target-settles-job ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" X") t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (let ((job (deslop-insert-at-cursor))
            (call nil))
        (setq call (car deslop-test--calls))
        (setq buffer-read-only t)
        (deslop-test--complete-call call)
        (should (eq (deslop-job-state job) 'failed))
        (should-not deslop--current-job)
        (should (string= (buffer-string) "base"))))))

(ert-deftest deslop-test/narrowed-target-settles-job ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" X") t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (let ((job (deslop-insert-at-cursor))
            (call nil))
        (setq call (car deslop-test--calls))
        (narrow-to-region (point-min) (1- (point-max)))
        (deslop-test--complete-call call)
         (should (eq (deslop-job-state job) 'failed))
         (should-not deslop--current-job)))))

(ert-deftest deslop-test/widening-does-not-expand-queued-context ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" X") t)))
    (deslop-test--with-buffer "SECRET safe SECRET"
      (narrow-to-region 8 12)
      (goto-char 10)
      (setq deslop--queue-suspended t)
      (deslop-insert-at-cursor)
      (widen)
      (setq deslop--queue-suspended nil)
      (deslop-process-next)
      (let ((user-message
             (cdr (cadr (plist-get (car deslop-test--calls) :messages)))))
        (should (string-match-p "sa<CURSOR>fe" user-message))
        (should-not (string-match-p "SECRET" user-message))))))

(ert-deftest deslop-test/modification-hook-settles-job ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" X") t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (let ((job (deslop-insert-at-cursor))
            (call nil))
        (setq call (car deslop-test--calls))
        (add-hook 'before-change-functions
                  (lambda (&rest _) (error "blocked")) nil t)
        (deslop-test--complete-call call)
        (should (eq (deslop-job-state job) 'failed))
        (should (string= (buffer-string) "base"))
        (should-not deslop--current-job)))))

(ert-deftest deslop-test/empty-result-does-not-insert-separators ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '("\n") t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (let ((job (deslop-insert-at-cursor))
            (call nil))
        (setq call (car deslop-test--calls))
        (deslop-test--complete-call call)
        (should (eq (deslop-job-state job) 'failed))
        (should (string= (buffer-string) "base"))))))

;;;; Anchor ordering and conflicts

(ert-deftest deslop-test/same-endpoint-preserves-fifo ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '("A" "B") t)))
    (deslop-test--with-buffer "alpha"
      (deslop-test--select 1 6 6)
      (setq deslop--queue-suspended t)
      (deslop-insert-at-cursor)
      (deslop-insert-at-cursor)
      (setq deslop--queue-suspended nil)
      (deslop-process-next)
      (deslop-test--complete-call (nth 0 deslop-test--calls))
       (deslop-test--complete-call (nth 1 deslop-test--calls))
       (should (string= (buffer-string) "alpha\nA\nB")))))

(ert-deftest deslop-test/parallel-same-endpoint-preserves-enqueue-order ()
  (deslop-test--reset)
  (let ((deslop-llm-function
         (deslop-test--fake-llm '("A" "B") t))
        (profile '(:name remote :model "remote-model"
                   :max-concurrency 2 :parallel t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (let ((deslop--queue-suspended t)
            (target (deslop-target-create
                     :buffer (current-buffer) :start 5 :end 5 :cursor 5)))
        (deslop-enqueue (list 'insert :target target :profile profile))
        (deslop-enqueue
         (list 'insert
               :target (deslop-target-create
                        :buffer (current-buffer) :start 5 :end 5 :cursor 5)
               :profile profile))
        (setq deslop--queue-suspended nil)
        (deslop-process-next))
      (should (= (length deslop--active-jobs) 2))
      (deslop-test--complete-call (nth 1 deslop-test--calls))
      (deslop-test--complete-call (nth 0 deslop-test--calls))
      (should (string= (buffer-string) "baseAB")))))

(ert-deftest deslop-test/parallel-job-at-provider-limit-keeps-policy ()
  (deslop-test--reset)
  (let ((deslop-llm-function
         (deslop-test--fake-llm '("A" "B") t))
        (profile '(:name remote :model "remote-model"
                   :max-concurrency 1 :parallel t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (let ((first (deslop-enqueue
                    (list 'insert
                          :target (deslop-target-create
                                   :buffer (current-buffer)
                                   :start 5 :end 5 :cursor 5)
                          :profile profile)))
            (second nil))
        (setq second
              (deslop-enqueue
               (list 'insert
                     :target (deslop-target-create
                              :buffer (current-buffer)
                              :start 5 :end 5 :cursor 5)
                     :profile profile)))
        (should (eq (deslop-job-state first) 'running))
        (should (eq (deslop-job-state second) 'queued))
         (should (plist-get (deslop-job-profile second) :parallel))
         (deslop-test--complete-call (nth 0 deslop-test--calls))
        (should (eq (deslop-job-state second) 'running))
         (deslop-test--complete-call (nth 1 deslop-test--calls))
         (should (string= (buffer-string) "baseAB"))))))

(ert-deftest deslop-test/equal-provider-identities-preserve-fifo ()
  (deslop-test--reset)
  (let* ((deslop-llm-function
          (deslop-test--fake-llm '("X" "A" "B") t))
         (url "http://same.example/v1/chat/completions")
         (active-profile (list :url (copy-sequence url)
                               :model "active" :max-concurrency 2
                               :parallel t))
         (serial-profile (list :url (copy-sequence url)
                               :model "serial" :max-concurrency 2
                               :parallel nil))
         (parallel-profile (list :url (copy-sequence url)
                                 :model "parallel" :max-concurrency 2
                                 :parallel t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (let ((active (deslop-enqueue
                     (list 'insert
                           :target (deslop-target-create
                                    :buffer (current-buffer)
                                    :start 5 :end 5 :cursor 5)
                           :profile active-profile)))
            (serial nil)
            (parallel nil))
        (let ((deslop--queue-suspended t))
          (setq serial
                (deslop-enqueue
                 (list 'insert
                       :target (deslop-target-create
                                :buffer (current-buffer)
                                :start 5 :end 5 :cursor 5)
                       :profile serial-profile))
                parallel
                (deslop-enqueue
                 (list 'insert
                       :target (deslop-target-create
                                :buffer (current-buffer)
                                :start 5 :end 5 :cursor 5)
                       :profile parallel-profile))))
        (should (eq (deslop-job-state active) 'running))
        (should (eq (deslop-job-state serial) 'queued))
        (should (eq (deslop-job-state parallel) 'queued))
        (should (= (length deslop-test--calls) 1))
        (deslop-test--complete-call (car deslop-test--calls))
        (should (eq (deslop-job-state serial) 'running))
        (should (eq (deslop-job-state parallel) 'queued))
        (deslop-test--complete-call (nth 1 deslop-test--calls))
        (deslop-test--complete-call (nth 2 deslop-test--calls))
        (should (string= (buffer-string) "baseXAB"))))))

(ert-deftest deslop-test/parallel-slots-are-scoped-per-provider ()
  (deslop-test--reset)
  (let ((deslop-llm-function
         (deslop-test--fake-llm '("A1" "B1" "A2") t))
        (first-profile '(:name first :model "first-model"
                         :max-concurrency 1 :parallel t))
        (second-profile '(:name second :model "second-model"
                          :max-concurrency 1 :parallel t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (let ((first (deslop-enqueue
                    (list 'insert
                          :target (deslop-target-create
                                   :buffer (current-buffer)
                                   :start 5 :end 5 :cursor 5)
                          :profile first-profile)))
            (second nil)
            (third nil))
        (setq second
              (deslop-enqueue
               (list 'insert
                     :target (deslop-target-create
                              :buffer (current-buffer)
                              :start 5 :end 5 :cursor 5)
                     :profile first-profile)))
        (setq third
              (deslop-enqueue
               (list 'insert
                     :target (deslop-target-create
                              :buffer (current-buffer)
                              :start 5 :end 5 :cursor 5)
                     :profile second-profile)))
        (should (eq (deslop-job-state first) 'running))
        (should (eq (deslop-job-state second) 'queued))
        (should (eq (deslop-job-state third) 'running))
        (should (= (length deslop--active-jobs) 2))
        (deslop-abort-queue)))))

(ert-deftest deslop-test/changed-selection-is-skipped ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '("X") t)))
    (deslop-test--with-buffer "original"
      (deslop-test--select 1 9 9)
      (let ((job (deslop-insert-at-cursor))
            (call nil))
        (setq call (car deslop-test--calls))
        (goto-char 3)
        (delete-char 1)
        (insert "X")
        (deslop-test--complete-call call)
        (should (eq (deslop-job-state job) 'failed))
        (should-not deslop--current-job)))))

;;;; Incremental reasoning parser

(defun deslop-test--reasoning-callbacks (thinking content)
  "Return callbacks appending thinking and content into two symbols."
  (list :on-delta
        (lambda (type text)
          (if (eq type :thinking)
              (set thinking (concat (symbol-value thinking) text))
            (set content (concat (symbol-value content) text))))
        :on-status #'ignore))

(ert-deftest deslop-test/thinking-parser-separation ()
  (let ((thinking-symbol (make-symbol "thinking"))
        (content-symbol (make-symbol "content")))
    (set thinking-symbol "")
    (set content-symbol "")
    (let* ((parser (deslop-protocol-make-reasoning))
           (callbacks (deslop-test--reasoning-callbacks
                       thinking-symbol content-symbol)))
      (deslop-protocol-reasoning-feed
       parser "Intro <think>Plan here" callbacks)
      (deslop-protocol-reasoning-feed
       parser "...done</think>Final answer" callbacks)
      (should-not (deslop-protocol-reasoning-finish parser callbacks))
      (should (string= (symbol-value thinking-symbol) "Plan here...done"))
      (should (string= (symbol-value content-symbol) "Intro Final answer")))))

(ert-deftest deslop-test/thinking-parser-all-delimiter-splits ()
  (dolist (split (number-sequence 0 (length "<think>secret</think>answer")))
    (let ((thinking "") (content "")
          (parser (deslop-protocol-make-reasoning)))
      (let ((callbacks
             (list :on-delta
                   (lambda (type text)
                     (if (eq type :thinking)
                         (setq thinking (concat thinking text))
                       (setq content (concat content text)))))))
        (deslop-protocol-reasoning-feed
         parser (substring "<think>secret</think>answer" 0 split) callbacks)
        (deslop-protocol-reasoning-feed
         parser (substring "<think>secret</think>answer" split) callbacks)
        (should-not (deslop-protocol-reasoning-finish parser callbacks))
        (should (string= thinking "secret"))
        (should (string= content "answer"))))))

(ert-deftest deslop-test/thinking-parser-unclosed-is-error ()
  (let ((parser (deslop-protocol-make-reasoning)))
    (deslop-protocol-reasoning-feed parser "<think>secret" (list :on-delta #'ignore))
    (should (deslop-error-p
              (deslop-protocol-reasoning-finish parser (list :on-delta #'ignore))))))

;;;; SSE parser

(ert-deftest deslop-test/sse-parser-fragmented-and-done ()
  (let ((parser (deslop-protocol-make-sse))
        (events nil))
    (setq events
          (append events
                   (deslop-protocol-sse-feed
                   parser "data: {\"choices\":[{\"delta\":{\"content\":\"hi")))
    (setq events
          (append events
                   (deslop-protocol-sse-feed
                   parser " there\"}}]}\r\n\r\ndata: [DONE]\r\n\r\n")))
    (should (= (length events) 2))
    (should (eq (plist-get (nth 0 events) :type) :json))
    (should (string= (deslop-protocol-json-get
                      "content"
                      (deslop-protocol-json-get
                       "delta"
                        (aref (deslop-protocol-json-get
                              "choices"
                              (plist-get (nth 0 events) :value))
                             0)))
                     "hi there"))
    (should (eq (plist-get (nth 1 events) :type) :done))
     (should-not (deslop-protocol-sse-finish parser))))

(ert-deftest deslop-test/sse-parser-rejects-malformed-and-ignores-late-data ()
  (let ((parser (deslop-protocol-make-sse)))
    (should-error
     (deslop-protocol-sse-feed parser "data: {bad}\n\n"))
    (let ((done-parser (deslop-protocol-make-sse)))
      (deslop-protocol-sse-feed done-parser "data: [DONE]\n\n")
      (should-not
       (deslop-protocol-sse-feed
        done-parser "data: {\"choices\":[]}\n\n")))))

(ert-deftest deslop-test/sse-parser-requires-done ()
  (let ((parser (deslop-protocol-make-sse)))
    (deslop-protocol-sse-feed parser "data: {\"choices\":[]}\n\n")
    (should-error (deslop-protocol-sse-finish parser))))

(ert-deftest deslop-test/transport-session-settles-once-and-cleans-resources ()
  (let* ((process-buffer (generate-new-buffer " *deslop-test-process*"))
         (stderr-buffer (generate-new-buffer " *deslop-test-stderr*"))
         (headers-file (make-temp-file "deslop-test-headers-"))
         (config-file (make-temp-file "deslop-test-config-"))
         (events 0)
         (done 0)
          (errors 0)
          (discovery-cancelled nil)
          (session
           (deslop-llm--make-session
           :callbacks
           (list :on-event (lambda (_event)
                             (setq events (1+ events))
                             (error "callback failed"))
                  :on-done (lambda (_result) (setq done (1+ done)))
                  :on-error (lambda (_error) (setq errors (1+ errors))))
            :discovery-cancel (lambda () (setq discovery-cancelled t))
            :process-buffer process-buffer :stderr-buffer stderr-buffer
           :headers-file headers-file :config-file config-file)))
    (deslop-llm--fail
     session (deslop-error-create :kind 'transport :message "failed"))
    (deslop-llm--fail
     session (deslop-error-create :kind 'transport :message "late"))
    (deslop-llm--settle session 'success "late")
    (should (eq (deslop-llm--session-terminal session) 'failure))
    (should (= events 1))
    (should (= done 0))
     (should (= errors 1))
     (should discovery-cancelled)
     (should-not (deslop-llm--session-discovery-cancel session))
     (should-not (deslop-llm--session-callbacks session))
     (should-not (deslop-llm--session-process session))
     (should-not (deslop-llm--session-process-buffer session))
     (should-not (deslop-llm--session-stderr-buffer session))
     (should-not (deslop-llm--session-stderr-process session))
     (should-not (deslop-llm--session-headers-file session))
     (should-not (deslop-llm--session-config-file session))
     (should-not (buffer-live-p process-buffer))
    (should-not (buffer-live-p stderr-buffer))
     (should-not (file-exists-p headers-file))
     (should-not (file-exists-p config-file))))

(ert-deftest deslop-test/codex-session-clears-resource-slots ()
  (let ((process-buffer (generate-new-buffer " *deslop-test-codex-process*"))
        (stderr-buffer (generate-new-buffer " *deslop-test-codex-stderr*"))
        (headers-file (make-temp-file "deslop-test-codex-headers-"))
        (config-file (make-temp-file "deslop-test-codex-config-")))
    (let ((session
           (deslop-codex--make-session
            :callbacks nil
            :process-buffer process-buffer
            :stderr-buffer stderr-buffer
            :headers-file headers-file
            :config-file config-file)))
      (deslop-codex--fail
       session (deslop-error-create :kind 'transport :message "failed"))
      (should-not (deslop-codex--session-process session))
      (should-not (deslop-codex--session-process-buffer session))
      (should-not (deslop-codex--session-stderr-buffer session))
      (should-not (deslop-codex--session-stderr-process session))
      (should-not (deslop-codex--session-headers-file session))
      (should-not (deslop-codex--session-config-file session))
      (should-not (buffer-live-p process-buffer))
      (should-not (buffer-live-p stderr-buffer))
      (should-not (file-exists-p headers-file))
      (should-not (file-exists-p config-file)))))

(ert-deftest deslop-test/provider-processes-from-tramp-stay-local ()
  (dolist (operation '(generation codex-generation discovery models refresh))
    (let ((native-comp-enable-subr-trampolines nil)
          (default-directory "/ssh:deslop-test.invalid:/source/")
          (deslop-provider-discovery-enabled nil)
          (deslop-provider--discovery-cache (make-hash-table :test #'equal))
          (deslop-provider--discovery-inflight (make-hash-table :test #'equal))
          (deslop-codex--refresh-flights (make-hash-table :test #'equal))
          (auth '(:access "test-access" :refresh "test-refresh"
                  :expires 9999999999999))
          (profile '(:name test :url "https://example.test/v1/chat/completions"
                     :model "test-model" :temperature 0.2))
          (cancel nil) (process nil) (failure nil)
          (process-count 0) (pipe-count 0) (send-count 0) (files nil)
          (real-process (symbol-function 'make-process))
          (real-pipe (symbol-function 'make-pipe-process))
          (real-temp (symbol-function 'make-temp-file)))
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest args)
                   (should-not (file-remote-p default-directory))
                   (should (file-directory-p default-directory))
                   (cl-incf process-count)
                   (dolist (option '("--dump-header" "--config"))
                     (let ((path (cadr (member option (plist-get args :command)))))
                       (when path
                         (should (file-name-absolute-p path))
                         (should-not (file-remote-p path))
                         (should (file-exists-p path)))))
                   (setq process
                         (funcall real-process :name "deslop-test-local"
                                  :command '("cat") :connection-type 'pipe
                                  :noquery t))))
                ((symbol-function 'make-pipe-process)
                 (lambda (&rest args)
                   (should-not (file-remote-p default-directory))
                   (cl-incf pipe-count)
                   (apply real-pipe args)))
                ((symbol-function 'make-temp-file)
                 (lambda (&rest args)
                   (should-not (file-remote-p default-directory))
                   (let ((file (apply real-temp args)))
                     (push file files)
                     file)))
                ((symbol-function 'process-send-string)
                 (lambda (target body)
                   (should (eq target process))
                   (should-not (file-remote-p default-directory))
                   (should (stringp body))
                   (when (eq operation 'refresh)
                     (should (string-match-p "refresh_token=test-refresh" body)))
                   (cl-incf send-count)))
                ((symbol-function 'deslop-codex--auth-entry) (lambda () auth)))
        (unwind-protect
            (let ((callbacks (list :on-start (lambda (value) (setq cancel value))
                                   :on-error (lambda (value) (setq failure value)))))
              (pcase operation
                ('generation
                 (deslop-llm-default '(("user" . "remote source")) callbacks profile))
                ('codex-generation
                 (deslop-codex-default '(("user" . "remote source")) callbacks))
                ('discovery
                 (let ((deslop-provider-discovery-enabled t))
                   (setq cancel (deslop-provider-discover profile #'ignore))))
                ('models
                 (setq cancel (deslop-codex-probe-models
                               (lambda (_models error) (setq failure error)))))
                ('refresh
                 (setq cancel (deslop-codex--refresh-auth
                               auth (lambda (value) (setq failure value))))))
              (should-not failure)
              (should (= process-count 1))
              (should (= pipe-count 1))
              (should (= send-count (if (memq operation '(discovery models)) 0 1)))
              (should files)
              (should (equal default-directory "/ssh:deslop-test.invalid:/source/")))
          (when cancel (funcall cancel))
          (when (and process (process-live-p process)) (delete-process process))
          (when process
            (deslop-curl-cleanup-resources
             (list (process-get process :deslop-response-buffer)
                   (process-get process :deslop-stderr-buffer))
             files
             (list (process-get process :deslop-stderr-process)))))))))

(ert-deftest deslop-test/provider-rejects-unsafe-temporary-directory ()
  (dolist (directory '("/ssh:deslop-test.invalid:/tmp/" "relative-temp/"))
    (let ((native-comp-enable-subr-trampolines nil)
          (temporary-file-directory directory))
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest _) (ert-fail "Started a process")))
                ((symbol-function 'make-temp-file)
                 (lambda (&rest _) (ert-fail "Created a temporary file")))
                ((symbol-function 'deslop-curl--auth-secret)
                 (lambda (&rest _) (ert-fail "Read credentials"))))
        (should-error (deslop-curl-local-directory) :type 'user-error)
        (should-error
         (deslop-curl-start-request :profile '(:url "https://example.test"))
         :type 'user-error)
        (should-error (deslop-curl--config '(:url "https://example.test"))
                      :type 'user-error)))))

(defun deslop-test--run-default-transport (header response)
  "Run the default transport with a mocked process and HEADER/RESPONSE.
Return (RESULT ERROR COMMAND DONE-COUNT ERROR-COUNT CANCEL-FUNCTION EVENTS)."
  (let ((captured nil)
        (process nil)
        (result nil)
         (failure nil)
         (done-count 0)
         (error-count 0)
         (cancel-function nil)
         (events nil)
        (deslop-provider-discovery-enabled nil)
        (real-make-process (symbol-function 'make-process))
         (profile (list :name 'test
                        :job-id 1
                        :url "https://example.test/v1"
                       :model "test-model"
                       :temperature 0.2
                       :require-sse t)))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq captured args)
                 (setq process
                       (funcall real-make-process
                        :name
                        (plist-get args :name)
                        :buffer
                        (plist-get args :buffer)
                        :command (list "cat")
                        :connection-type 'pipe)))))
      (unwind-protect
          (progn
            (deslop-llm-default
             (list (cons "user" "body"))
               (list :on-start (lambda (cancel)
                                 (setq cancel-function cancel))
                     :on-event (lambda (event)
                                 (push event events))
                     :on-done (lambda (value)
                               (setq done-count (1+ done-count))
                               (setq result value))
                    :on-error (lambda (value)
                                (setq error-count (1+ error-count))
                                (setq failure value)))
              profile)
            (let* ((command (plist-get captured :command))
                   (headers-file (nth (1+ (cl-position "--dump-header"
                                                      command :test #'string=))
                                      command))
                   (filter (plist-get captured :filter))
                   (sentinel (plist-get captured :sentinel)))
              (with-temp-file headers-file
                (insert header))
               (funcall filter process response)
              (when (process-live-p process)
                (process-send-eof process))
              (while (process-live-p process)
                (accept-process-output process 0.05))
               (funcall sentinel process "finished\n")
                (list result failure command done-count error-count
                      cancel-function events)))
        (when (and (processp process) (process-live-p process))
          (delete-process process))))))

(ert-deftest deslop-test/default-transport-completes-valid-sse ()
  (let ((out (deslop-test--run-default-transport
               "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n"
               "data: {\"model\":\"actual-model\",\"provider\":\"llama.cpp\",\"choices\":[{\"delta\":{\"content\":\"hello\"}}]}\n\ndata: [DONE]\n\n")))
    (should-not (nth 1 out))
    (should (string= (plist-get (nth 0 out) :content) "hello"))
    (should (string= (plist-get (nth 0 out) :model) "actual-model"))
    (should (string= (plist-get (nth 0 out) :provider) "llama.cpp"))
    (should (= (nth 3 out) 1))
    (should (= (nth 4 out) 0))
    (should (functionp (nth 5 out)))
    (should-not (member "hello" (nth 2 out)))
    (should (member "@-" (nth 2 out)))))

(ert-deftest deslop-test/default-transport-accepts-text-completion-deltas ()
  (let ((out (deslop-test--run-default-transport
               "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n"
               "data: {\"choices\":[{\"text\":\"legacy\"}]}\n\ndata: [DONE]\n\n")))
    (should-not (nth 1 out))
    (should (string= (plist-get (nth 0 out) :content) "legacy"))))

(ert-deftest deslop-test/default-transport-retains-joined-partial-result ()
  (let* ((first
          "data: {\"choices\":[{\"finish_reason\":null,\"delta\":{\"reasoning_content\":\"Sub\"}}]}\n\n")
         (second
          "data: {\"choices\":[{\"finish_reason\":\"length\",\"delta\":{\"reasoning_content\":\"headings\"}}]}\n\n")
         (out (deslop-test--run-default-transport
               "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n"
               (concat first second
                       "data: {\"choices\":[],\"usage\":{\"completion_tokens\":4096}}\n\n"
                       "data: [DONE]\n\n")))
         (partial (cl-find-if
                   (lambda (event)
                     (eq (plist-get event :type) :partial-result))
                   (nth 6 out))))
    (should partial)
    (should (string= (plist-get (plist-get partial :result) :thinking)
                     "Subheadings"))
    (should (string= (plist-get (plist-get partial :result) :finish-reason)
                     "length"))))

(ert-deftest deslop-test/default-transport-rejects-http-errors ()
  (let ((out (deslop-test--run-default-transport
              "HTTP/1.1 401 Unauthorized\r\nContent-Type: text/event-stream\r\n\r\n"
              "data: {\"error\":{\"message\":\"bad key\"}}\n\ndata: [DONE]\n\n")))
    (should-not (nth 0 out))
    (should (deslop-error-p (nth 1 out)))
    (should (eq (deslop-error-kind (nth 1 out)) 'http))
    (should (= (nth 3 out) 0))
    (should (= (nth 4 out) 1))))

(ert-deftest deslop-test/default-transport-settles-empty-provider-error ()
  (let ((out (deslop-test--run-default-transport
              "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n"
              "data: {\"error\":{}}\n\ndata: [DONE]\n\n")))
    (should-not (nth 0 out))
    (should (eq (deslop-error-kind (nth 1 out)) 'provider))
    (should (= (nth 4 out) 1))))

(ert-deftest deslop-test/default-transport-done-ignores-same-chunk-trailing-data ()
  (let ((out (deslop-test--run-default-transport
              "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n"
              (concat
               "data: {\"choices\":[{\"delta\":{\"content\":\"ok\"}}]}\n\n"
               "data: [DONE]\n\n"
               "data: malformed trailing bytes\n\n"))))
     (should (string= (plist-get (nth 0 out) :content) "ok"))
     (should-not (nth 1 out))
     (should (= (nth 3 out) 1))))

(ert-deftest deslop-test/openai-profile-uses-opencode-oauth ()
  (let ((profile (deslop-provider-profile 'openai)))
    (should (eq (plist-get profile :wire-api) 'responses))
    (should (eq (plist-get profile :auth-method) 'opencode-oauth))
    (should (string= (plist-get profile :model) "gpt-5.6-luna"))
    (should (string= (plist-get profile :url)
                     "https://chatgpt.com/backend-api/codex/responses"))
    (should-not (plist-get (deslop-provider-profile-snapshot 'openai) :access))
    (should-not (plist-get (deslop-provider-profile-snapshot 'openai) :refresh))))

(ert-deftest deslop-test/provider-list-and-configured-models-are-selectable ()
  (let ((deslop-providers '(openai local))
        (deslop-provider-profiles
         '((local :models ("local-one" "local-two")))))
    (should (equal (deslop-provider-names) '(openai local)))
    (should (equal (deslop-provider-models
                    (deslop-provider-profile 'local))
                   '("local-one" "local-two")))))

(ert-deftest deslop-test/provider-runtime-concurrency-and-parallel-settings ()
  (deslop-provider-clear-runtime-settings)
  (let ((deslop-provider-profiles
         '((remote :max-concurrency 3 :parallel nil))))
    (let ((profile (deslop-provider-profile 'remote)))
      (should (= (deslop-provider-max-concurrency profile) 3))
      (should-not (deslop-provider-parallel-enabled-p profile)))
    (deslop-provider-set-runtime-setting 'remote :max-concurrency 5)
    (deslop-provider-set-runtime-setting 'remote :parallel t)
    (let ((profile (deslop-provider-profile 'remote)))
      (should (= (deslop-provider-max-concurrency profile) 5))
      (should (deslop-provider-parallel-enabled-p profile)))))

(ert-deftest deslop-test/openai-model-probe-parses-official-slugs ()
  (should (equal
           (deslop-codex--parse-models
            (concat "{\"models\":[{\"slug\":\"gpt-5.6-luna\"},"
                    "{\"slug\":\"gpt-5.5\"},{\"title\":\"ignored\"}]}"))
           '("gpt-5.6-luna" "gpt-5.5"))))

(ert-deftest deslop-test/openai-model-probe-response-is-bounded ()
  (let ((deslop-provider-max-discovery-bytes 4)
        (captured nil)
        (process nil)
        (result nil)
        (real-make-process (symbol-function 'make-process)))
    (cl-letf (((symbol-function 'deslop-codex--resolve-auth)
               (lambda (callback)
                 (funcall callback '(:access "access-token"))
                 nil))
              ((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq captured args
                       process
                       (funcall real-make-process
                                :name (plist-get args :name)
                                :buffer nil
                                :command (list "cat")
                                :connection-type 'pipe)))))
      (unwind-protect
          (progn
            (deslop-codex-probe-models
             (lambda (models error)
               (setq result (list models error))))
            (should (process-live-p process))
            (funcall (plist-get captured :filter) process "oversized")
            (should (equal (car result) nil))
            (should (deslop-error-p (cadr result)))
            (should (eq (deslop-error-kind (cadr result)) 'resource))
            (should-not (process-live-p process))
            (should-not deslop-codex--model-probe-processes))
        (when (and (processp process) (process-live-p process))
          (delete-process process))
        (setq deslop-codex--model-probe-processes nil)))))

(ert-deftest deslop-test/selector-runs-from-source-with-selected-profile ()
  (let* ((source (generate-new-buffer " *deslop-selector-source*"))
         (selector (generate-new-buffer " *deslop-selector-test*"))
         (deslop-providers '(local))
         (deslop-provider-profiles '((local :models ("one" "two"))))
         (queued nil))
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert "source"))
          (with-current-buffer selector
            (deslop-selector-mode)
            (setq deslop-selector--source-buffer source
                  deslop-selector--providers '(local)
                  deslop-selector--provider-index 0
                  deslop-selector--models '("one" "two")
                  deslop-selector--model-index 0)
            (deslop-selector-next-model)
             (cl-letf (((symbol-function 'deslop-enqueue-operation)
                       (lambda (type instruction profile)
                         (setq queued
                               (list type instruction profile (current-buffer))))))
               (deslop-selector-insert))))
          (should (eq (nth 0 queued) 'insert))
          (should (string= (plist-get (nth 2 queued) :model) "two"))
          (should (eq (nth 3 queued) source)))
      (dolist (buffer (list source selector))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))))

(ert-deftest deslop-test/selector-uses-bottom-window-and-quits-it-on-close ()
  (let ((source (generate-new-buffer " *deslop-selector-window-source*"))
        (display-action nil)
        (quit-window-call nil)
        (probe-profile nil)
        (selector nil))
    (unwind-protect
        (with-current-buffer source
          (cl-letf (((symbol-function 'display-buffer)
                     (lambda (buffer action)
                       (setq display-action (list buffer action))
                       (selected-window)))
                     ((symbol-function 'quit-window)
                      (lambda (kill window)
                        (setq quit-window-call (list kill window))))
                     ((symbol-function 'deslop-provider-probe-models)
                      (lambda (profile callback)
                        (setq probe-profile profile)
                        (funcall callback '("local-model") nil)
                        (lambda () nil))))
            (let ((deslop-providers '(local))
                  (deslop-provider-profiles
                   '((local :model "local-model"))))
              (deslop-selector-open)
              (setq selector (get-buffer deslop-selector-buffer-name))
              (should (eq (plist-get probe-profile :name) 'local))
              (should (eq (car display-action) selector))
              (should (eq (car (car (cadr display-action)))
                          #'display-buffer-at-bottom))
              (should (eq (cdr (assq 'window-height
                                     (cdr (cadr display-action))))
                          #'deslop-selector--fit-window))
              (should (equal (cdr (assq 'window-min-height
                                        (cdr (cadr display-action))))
                             deslop-selector-window-min-height))
              (with-current-buffer selector
                (deslop-selector-close))
              (should (equal (car quit-window-call) t))
              (should (eq (cadr quit-window-call) (selected-window)))
              (should-not (buffer-live-p selector)))))
      (when (buffer-live-p selector)
        (kill-buffer selector))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(ert-deftest deslop-test/selector-changes-provider-runtime-settings ()
  (deslop-provider-clear-runtime-settings)
  (let ((deslop-providers '(remote))
        (deslop-provider-profiles
         '((remote :model "remote-model" :max-concurrency 2
                   :parallel nil))))
    (with-temp-buffer
      (deslop-selector-mode)
      (setq deslop-selector--providers '(remote)
            deslop-selector--provider-index 0
            deslop-selector--models '("remote-model")
            deslop-selector--model-index 0)
      (cl-letf (((symbol-function 'read-number)
                 (lambda (&rest _) 4)))
       (deslop-selector-set-concurrency))
      (should (= (deslop-provider-max-concurrency
                  (deslop-provider-profile 'remote))
                 4))
      (should-not (deslop-selector--provider-parallel-p))
      (deslop-selector-toggle-parallel)
      (should (deslop-selector--provider-parallel-p))
      (deslop-selector--render)
       (dolist (key '("Provider:" "Model:" "Concurrency:" "Parallel:"
                      "i  - insert" "I  - insert anywhere"
                      "w  - write anywhere" "m  - improve" "s  - summarize"
                      "RET       - confirm choice"
                      "C-c C-c   - approve all LLM changes"
                      "C-c C-k   - reject all LLM changes"))
         (should (string-match-p (regexp-quote key) (buffer-string))))
       (should (= (count-lines (point-min) (point-max)) 15)))))

(ert-deftest deslop-test/selector-runtime-keybindings-survive-reload ()
  (let ((original-c (lookup-key deslop-selector-mode-map (kbd "c")))
        (original-C (lookup-key deslop-selector-mode-map (kbd "C")))
        (original-P (lookup-key deslop-selector-mode-map (kbd "P"))))
    (unwind-protect
        (progn
          ;; Simulate a keymap retained from before the new bindings existed.
          (define-key deslop-selector-mode-map (kbd "c") nil)
          (define-key deslop-selector-mode-map (kbd "C") nil)
          (define-key deslop-selector-mode-map (kbd "P") nil)
          (with-temp-buffer
            (deslop-selector-mode)
            (should (eq (key-binding (kbd "c"))
                        #'deslop-selector-set-concurrency))
            (should (eq (key-binding (kbd "C"))
                        #'deslop-selector-set-concurrency))
             (should (eq (key-binding (kbd "P"))
                         #'deslop-selector-toggle-parallel))
             (should (eq (key-binding (kbd "n"))
                         #'deslop-selector-next-provider))
             (should (eq (key-binding (kbd "p"))
                         #'deslop-selector-previous-provider))
             (should (eq (key-binding (kbd "RET"))
                         #'deslop-selector-confirm))
             (should (eq (key-binding (kbd "C-c C-c"))
                         #'deslop-selector-approve-source-output))
             (should (eq (key-binding (kbd "C-c C-k"))
                         #'deslop-selector-reject-source-output))
             (should-not (eq (key-binding (kbd "DEL"))
                             #'deslop-selector-reject-source-output))
             (should-not (eq (key-binding (kbd "<backspace>"))
                             #'deslop-selector-reject-source-output))
            (should (eq (key-binding (kbd "w"))
                        #'deslop-selector-write-anywhere))
            (should (eq (key-binding (kbd "I"))
                        #'deslop-selector-insert-anywhere))))
      (define-key deslop-selector-mode-map (kbd "c") original-c)
       (define-key deslop-selector-mode-map (kbd "C") original-C)
       (define-key deslop-selector-mode-map (kbd "P") original-P))))

(ert-deftest deslop-test/selector-labels-are-single-line-and-display-bounded ()
  (let ((multiline (deslop-selector--display-label "provider\nspoof" 24))
        (long (deslop-selector--display-label
               "provider-name-that-is-longer-than-the-row" 24)))
    (should (string-match-p "provider spoof" multiline))
    (should-not (string-match-p "\n" multiline))
    (should (= (string-width multiline) 24))
    (should (string-suffix-p "..." (string-trim-right long)))
    (should (= (string-width long) 24))))

(ert-deftest deslop-test/selector-probe-callbacks-are-generation-bound ()
  (let ((buffer (generate-new-buffer " *deslop-selector-probe-test*"))
        (callbacks nil)
        (cancelled 0)
        (deslop-provider-profiles
         '((remote :url "http://remote.example/v1"
                   :model "configured"))))
    (unwind-protect
        (with-current-buffer buffer
          (deslop-selector-mode)
          (setq deslop-selector--providers '(remote)
                deslop-selector--provider-index 0
                deslop-selector--models '("configured")
                deslop-selector--model-index 0)
          (cl-letf (((symbol-function 'deslop-provider-probe-models)
                     (lambda (_profile callback)
                       (setq callbacks (append callbacks (list callback)))
                       (lambda () (setq cancelled (1+ cancelled))))))
            (deslop-selector--set-provider 0)
            (let ((stale (car callbacks)))
              (deslop-selector--cancel-probe)
              (deslop-selector--set-provider 0)
              (funcall stale '("stale") nil)
              (should (equal deslop-selector--models '("configured")))
              (let ((current (car (last callbacks))))
                (kill-buffer buffer)
                (should (= cancelled 2))
                (funcall current '("after-kill") nil)
                (should-not (buffer-live-p buffer))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest deslop-test/openai-request-body-matches-codex-responses-shape ()
  (let* ((body (deslop-protocol-json-read-string
                (json-encode
                 (deslop-codex--request-body
                  (list (cons "system" "rules")
                        (cons "user" "question")
                        (cons "assistant" "answer"))
                  '(:model "gpt-test")))))
          (input (deslop-protocol-json-get "input" body))
         (user (aref input 1))
          (user-content (aref (deslop-protocol-json-get "content" user) 0))
         (assistant (aref input 2))
         (assistant-content
          (aref (deslop-protocol-json-get "content" assistant) 0)))
    (should (eq (deslop-protocol-json-get "store" body) :false))
    (should (eq (deslop-protocol-json-get "stream" body) t))
    (should (string= (deslop-protocol-json-get "text" user-content)
                     "question"))
    (should (string= (deslop-protocol-json-get "type" user-content)
                     "input_text"))
    (should (string= (deslop-protocol-json-get "text" assistant-content)
                     "answer"))
    (should (string= (deslop-protocol-json-get "type" assistant-content)
                     "output_text"))))

(ert-deftest deslop-test/openai-http-error-keeps-provider-detail ()
  (let ((headers-file (make-temp-file "deslop-openai-error-")))
    (unwind-protect
        (progn
          (with-temp-file headers-file
            (insert "HTTP/1.1 400 Bad Request\r\n\r\n"))
          (let* ((session
                  (deslop-codex--make-session
                   :headers-file headers-file
                   :preview-parts '("{\"detail\":\"invalid request body\"}")))
                 (error (deslop-codex--http-error session)))
            (should (string= (deslop-error-message error)
                             "HTTP status 400: invalid request body"))))
      (when (file-exists-p headers-file)
        (delete-file headers-file)))))

(ert-deftest deslop-test/openai-auth-entry-is-read-from-opencode-file ()
  (let* ((file (make-temp-file "deslop-openai-auth-test-"))
         (deslop-codex-auth-file file))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "{\"openai\":{\"type\":\"oauth\","
                    "\"access\":\"access-token\","
                    "\"refresh\":\"refresh-token\","
                    "\"expires\":9999999999999,"
                    "\"accountId\":\"account-id\"}}"))
          (let ((auth (deslop-codex--auth-entry)))
            (should (string= (plist-get auth :access) "access-token"))
            (should (string= (plist-get auth :refresh) "refresh-token"))
            (should (string= (plist-get auth :account-id) "account-id"))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest deslop-test/openai-transport-keeps-token-out-of-command-arguments ()
  (let* ((auth-file (make-temp-file "deslop-openai-auth-test-"))
         (deslop-codex-auth-file auth-file)
         (captured nil)
         (process nil)
         (result nil)
         (real-make-process (symbol-function 'make-process)))
    (unwind-protect
        (progn
          (with-temp-file auth-file
            (insert "{\"openai\":{\"type\":\"oauth\","
                    "\"access\":\"access-token\","
                    "\"refresh\":\"refresh-token\","
                    "\"expires\":9999999999999,"
                    "\"accountId\":\"account-id\"}}"))
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest args)
                       (setq captured args)
                       (setq process
                             (funcall real-make-process
                                      :name (plist-get args :name)
                                      :buffer (plist-get args :buffer)
                                      :command (list "cat")
                                      :connection-type 'pipe)))))
            (deslop-codex-default
             (list (cons "user" "body"))
             (list :on-done (lambda (value) (setq result value))
                   :on-error (lambda (value) (setq result value)))
             (deslop-provider-profile 'openai))
             (let* ((command (plist-get captured :command))
                    (config (cadr (member "--config" command)))
                    (headers-file (cadr (member "--dump-header" command)))
                    (filter (plist-get captured :filter))
                    (sentinel (plist-get captured :sentinel)))
               (should (not (member "access-token" command)))
               (should (file-exists-p config))
               (with-temp-file headers-file
                 (insert "HTTP/1.1 200 OK\r\n"
                         "Content-Type: text/event-stream\r\n\r\n"))
                (with-temp-buffer
                  (insert-file-contents config)
                  (should (string-match-p "Authorization: Bearer access-token"
                                         (buffer-string))))
               (with-temp-buffer
                 (insert-file-contents config)
                 (should (string-match-p "User-Agent: deslop"
                                         (buffer-string))))
               (funcall
                filter
                process
                (concat "data: {\"type\":\"response.output_text.delta\","
                        "\"delta\":\"answer\"}\n\n"
                        "data: {\"type\":\"response.completed\","
                        "\"response\":{\"model\":\"gpt-test\"}}\n\n"
                        "data: [DONE]\n\n"))
               (while (process-live-p process)
                 (accept-process-output process 0.05))
               (funcall sentinel process "finished\n")
               (should (string= (plist-get result :content) "answer"))
               (should-not (file-exists-p config)))))
      (when (and (processp process) (process-live-p process))
        (delete-process process))
      (when (file-exists-p auth-file)
        (delete-file auth-file)))))

(ert-deftest deslop-test/openai-refresh-rotates-opencode-credentials-safely ()
  (let* ((auth-file (make-temp-file "deslop-openai-auth-test-"))
         (deslop-codex-auth-file auth-file)
         (deslop-codex-issuer "https://auth.openai.com:443")
         (captured nil)
         (process nil)
         (refreshed nil)
         (refreshed-again nil)
         (make-process-count 0)
         (real-make-process (symbol-function 'make-process))
         (auth '(:access "old-access" :refresh "old-refresh"
                 :expires 0 :account-id "account-id")))
    (unwind-protect
        (progn
          (with-temp-file auth-file
            (insert "{\"openai\":{\"type\":\"oauth\","
                    "\"access\":\"old-access\","
                    "\"refresh\":\"old-refresh\","
                    "\"expires\":0,"
                    "\"accountId\":\"account-id\"}}"))
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest args)
                        (setq make-process-count (1+ make-process-count))
                        (setq captured args)
                       (setq process
                             (funcall real-make-process
                                      :name (plist-get args :name)
                                      :buffer (plist-get args :buffer)
                                      :stderr (plist-get args :stderr)
                                      :command (list "cat")
                                      :connection-type 'pipe
                                      :noquery t)))))
             (deslop-codex--refresh-auth auth
                                         (lambda (value) (setq refreshed value)))
             (deslop-codex--refresh-auth
              auth (lambda (value) (setq refreshed-again value)))
             (should (= make-process-count 1))
             (let* ((command (plist-get captured :command))
                   (headers-file
                    (cadr (member "--dump-header" command)))
                   (filter (plist-get captured :filter))
                   (sentinel (plist-get captured :sentinel)))
              (should-not (member "old-refresh" command))
             (with-temp-file headers-file
                (insert "HTTP/1.1 200 OK\r\n\r\n"))
              (with-current-buffer (plist-get captured :buffer)
                (erase-buffer))
              (when (process-live-p process)
                (process-send-eof process))
              (while (process-live-p process)
                (accept-process-output process 0.05))
              (with-current-buffer (plist-get captured :buffer)
                (erase-buffer))
              (funcall filter process
                       (concat "{\"access_token\":\"new-access\","
                               "\"refresh_token\":\"new-refresh\","
                               "\"expires_in\":3600}"))
              (funcall sentinel process "finished\n")
               (should (string= (plist-get refreshed :access) "new-access"))
               (should (string= (plist-get refreshed :refresh) "new-refresh"))
               (should (equal refreshed-again refreshed))
               (should (string= (plist-get (deslop-codex--auth-entry) :refresh)
                                "new-refresh")))))
      (when (and (processp process) (process-live-p process))
        (delete-process process))
       (when (file-exists-p auth-file)
          (delete-file auth-file)))))

(ert-deftest deslop-test/openai-refresh-startup-failure-does-not-wedge-flight ()
  (let* ((key '(deslop-test-refresh))
         (result nil)
         (flight
          (deslop-codex--make-refresh-flight
           :key key
           :auth '(:refresh "refresh-token")
           :waiters
           (list
            (deslop-codex--make-refresh-waiter
             :callback (lambda (value) (setq result value))))))
         (deslop-codex--refresh-flights (make-hash-table :test #'equal))
         (deslop-codex--refresh-processes nil))
    (puthash key flight deslop-codex--refresh-flights)
    (cl-letf (((symbol-function 'make-temp-file)
               (lambda (&rest _)
                 (error "temporary file creation failed"))))
      (deslop-codex--start-refresh-flight flight))
    (should (deslop-codex--refresh-flight-finished flight))
    (should (deslop-error-p result))
    (should-not (gethash key deslop-codex--refresh-flights))
    (should-not deslop-codex--refresh-processes)
    (should-not (buffer-live-p
                 (deslop-codex--refresh-flight-response-buffer flight)))
    (should-not (buffer-live-p
                 (deslop-codex--refresh-flight-stderr-buffer flight)))))

(ert-deftest deslop-test/openai-credentials-use-only-official-endpoints ()
  (should (string= (deslop-codex--validated-generation-url
                    "https://chatgpt.com:443/backend-api/codex/responses")
                   "https://chatgpt.com:443/backend-api/codex/responses"))
  (should-error
   (deslop-codex--validated-generation-url
    "https://chatgpt.com.attacker.test/backend-api/codex/responses"))
  (should-error
   (deslop-codex--validated-generation-url
    "https://chatgpt.com/backend-api/codex/responses?redirect=attacker"))
  (let ((deslop-codex-issuer "https://auth.openai.com.attacker.test"))
    (should-error (deslop-codex--validated-refresh-url))))

(ert-deftest deslop-test/openai-request-body-applies-output-token-limit ()
  (let ((deslop-max-output-tokens 123))
    (let ((body (deslop-codex--request-body
                 (list (cons "user" "body"))
                 '(:model "test-model"))))
      (should (= (alist-get :max_output_tokens body) 123)))))

(ert-deftest deslop-test/curl-stderr-is-byte-bounded ()
  (let ((buffer (generate-new-buffer " *deslop-test-stderr*"))
        (process (make-pipe-process :name "deslop-test-stderr-process"
                                    :buffer nil :noquery t))
        (deslop-max-stderr-bytes 4))
    (unwind-protect
        (progn
          (deslop-curl-stderr-filter buffer process "12345")
          (deslop-curl-stderr-filter buffer process "6")
          (with-current-buffer buffer
            (should (string= (buffer-string) "1234"))))
      (delete-process process)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest deslop-test/openai-credential-replacement-is-side-by-side ()
  (let* ((directory (make-temp-file "deslop-openai-auth-dir-" t))
         (file (expand-file-name "auth.json" directory))
         (deslop-codex-auth-file file)
         (old '(:access "old-access" :refresh "old-refresh"
                :expires 0 :account-id "account-id"))
         (new '(:access "new-access" :refresh "new-refresh"
                :expires 9999999999999 :account-id "account-id")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "{\"openai\":{\"type\":\"oauth\","
                    "\"access\":\"old-access\","
                    "\"refresh\":\"old-refresh\","
                    "\"expires\":0,\"accountId\":\"account-id\"}}"))
          (deslop-codex--store-auth new (deslop-codex--auth-version old))
          (should (string= (plist-get (deslop-codex--auth-entry) :access)
                           "new-access"))
          (should (= (logand (file-modes file) #o777) #o600))
          (should-not
           (directory-files directory nil "\\`\\.deslop-openai-auth-" t))
          (should-error
           (deslop-codex--store-auth
            old (deslop-codex--auth-version
                 '(:access "other" :refresh "other" :expires 0
                   :account-id "account-id")))))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest deslop-test/openai-responses-events-normalize-to-deslop-result ()
          (let* ((events nil)
          (result nil)
         (failure nil)
         (session
         (deslop-codex--make-session
          :profile '(:model "gpt-test")
          :callbacks (list :on-event (lambda (event) (push event events))
                           :on-done (lambda (value) (setq result value))
                           :on-error (lambda (value) (setq failure value)))
          :sse (deslop-protocol-make-sse)
          :estimated-chars 0)))
    (deslop-codex--handle-event
     session
      (deslop-protocol-json-read-string
      "{\"type\":\"response.created\",\"response\":{\"model\":\"gpt-actual\"}}"))
    (deslop-codex--handle-event
     session
      (deslop-protocol-json-read-string
      "{\"type\":\"response.reasoning_summary_text.delta\",\"delta\":\"thinking\"}"))
    (deslop-codex--handle-event
     session
      (deslop-protocol-json-read-string
      "{\"type\":\"response.output_text.delta\",\"delta\":\"answer\"}"))
    (deslop-codex--handle-event
     session
      (deslop-protocol-json-read-string
      "{\"type\":\"response.completed\",\"response\":{\"model\":\"gpt-actual\",\"usage\":{\"output_tokens\":7}}}"))
    (setq result (deslop-codex--result session))
    (should-not failure)
    (should (string= (plist-get result :content) "answer"))
    (should (string= (plist-get result :thinking) "thinking"))
    (should (string= (plist-get result :model) "gpt-actual"))
    (should (= (plist-get result :tokens) 7))
    (should (deslop-codex--session-terminal-event session))
    (should-not (deslop-codex--session-terminal session))))

;;;; Profiles, history, and transport command safety

(ert-deftest deslop-test/profile-is-snapshotted-at-enqueue ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(:stall) t))
        (deslop-provider-endpoint "http://one.example/v1"))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (let ((job (deslop-insert-at-cursor)))
        (setq deslop-provider-endpoint "http://two.example/v1")
        (should (string= (plist-get (deslop-job-profile job) :url)
                         "http://one.example/v1"))))))

(ert-deftest deslop-test/auth-and-body-are-not-command-arguments ()
  (let* ((token "secret-token")
         (profile (list :name 'remote
                        :url "https://example.test/v1"
                        :model "model"
                         :temperature 0.2
                         :auth-source-host "example.test"
                         :auth-source-user "deslop"
                         :auth-source-port 443))
         (config nil))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _) (list (list :secret token)))))
      (setq config (deslop-curl--config profile))
      (unwind-protect
          (let ((command (deslop-curl-command
                          profile "/tmp/headers" config)))
            (should-not (member token command))
            (should-not (member "body" command))
            (should-not (member "--fail-with-body" command)))
         (when (file-exists-p config) (delete-file config))))))

(ert-deftest deslop-test/unbounded-output-omits-provider-token-limit ()
  (let ((deslop-max-output-tokens nil))
    (let ((body (deslop-llm--request-body
                 (list (cons "user" "body"))
                 '(:model "test-model" :temperature 0.2))))
      (should-not (alist-get :max_tokens body))
      (should-not (string-match-p "max_tokens" (json-encode body))))))

(ert-deftest deslop-test/curl-config-escapes-and-rejects-unsafe-headers ()
  (let* ((profile '(:url "https://example.test/v1"
                    :request-headers ("X-Title: a\"b\\c")))
         (config (deslop-curl--config profile)))
    (unwind-protect
        (with-temp-buffer
          (should (= (logand (file-modes config) #o777) #o600))
          (insert-file-contents config)
          (should (string=
                   (buffer-string)
                   "header = \"X-Title: a\\\"b\\\\c\"\n")))
      (when (file-exists-p config)
        (delete-file config))))
  (dolist (header '("X-Test: ok\nurl = https://attacker.test"
                    "Host: attacker.test"
                    "X-Auth-Token: secret"
                    "Content-Length: 0"))
    (should-error
      (deslop-curl--config
      (list :url "https://example.test/v1"
            :request-headers (list header))))))

(ert-deftest deslop-test/provider-url-and-curl-command-are-hardened ()
  (should-error
    (deslop-curl--validated-url '(:url "http://127.attacker.test/v1")))
  (should-error
    (deslop-curl--validated-url '(:url "https://example.test/v1?key=secret")))
  (should-error
    (deslop-curl--validated-url
    '(:url "https://other.test/v1"
      :auth-source-host "example.test"
      :auth-source-user "deslop"
      :auth-source-port 443)))
  (let ((command
         (deslop-curl-command
          '(:url "https://example.test/v1") "/tmp/headers" nil)))
    (should (string= (nth 1 command) "--disable"))
    (should (member "--globoff" command))
    (should (member "--noproxy" command))
     (should (member "--url" command))))

(ert-deftest deslop-test/curl-command-uses-idle-not-total-timeout-by-default ()
  (let ((deslop-request-timeout nil)
        (deslop-response-idle-timeout 60))
    (let ((command (deslop-curl-command
                    '(:url "https://example.test/v1") "/tmp/headers" nil)))
      (should-not (member "--max-time" command))
      (should (equal (cadr (member "--speed-time" command)) "60"))
      (should (equal (cadr (member "--speed-limit" command)) "1")))))

(ert-deftest deslop-test/json-parser-does-not-intern-keys-or-accept-garbage ()
  (let ((key (format "deslop-untrusted-key-%d" (random most-positive-fixnum))))
    (should-not (intern-soft key))
    (let ((payload (deslop-protocol-json-read-string
                    (format "{\"%s\":1}" key))))
      (should (hash-table-p payload))
      (should (= (deslop-protocol-json-get key payload) 1))
      (should-not (intern-soft key))))
  (should-error (deslop-protocol-json-read-string "{\"ok\":1} garbage")))

(ert-deftest deslop-test/default-transport-enforces-response-byte-limit ()
  (let* ((deslop-max-response-bytes 16)
         (out (deslop-test--run-default-transport
               "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n"
               (make-string 100 ?X))))
    (should-not (nth 0 out))
    (should (eq (deslop-error-kind (nth 1 out)) 'resource))
    (should (= (nth 4 out) 1))))

(ert-deftest deslop-test/response-byte-limit-retains-safe-prefix ()
  (let* ((prefix
          "data: {\"choices\":[{\"delta\":{\"content\":\"kept\"}}]}\n\n")
         (deslop-max-response-bytes (string-bytes prefix))
         (out (deslop-test--run-default-transport
               "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n"
               (concat prefix (make-string 100 ?X))))
         (partial (cl-find-if
                   (lambda (event)
                     (eq (plist-get event :type) :partial-result))
                   (nth 6 out))))
    (should-not (nth 0 out))
    (should (eq (deslop-error-kind (nth 1 out)) 'resource))
    (should partial)
    (should (string= (plist-get (plist-get partial :result) :content)
                     "kept"))))

(ert-deftest deslop-test/sse-parser-enforces-line-and-event-limits ()
  (let ((deslop-max-sse-event-bytes 8)
        (parser (deslop-protocol-make-sse)))
    (should-error (deslop-protocol-sse-feed parser "123456789")))
  (let ((deslop-max-sse-event-bytes 8)
        (parser (deslop-protocol-make-sse)))
    (should-error
     (deslop-protocol-sse-feed
      parser (mapconcat (lambda (_) "data:\n") (number-sequence 1 10) "")))))

(ert-deftest deslop-test/provider-discovery-cache-is-bounded ()
  (let ((deslop-provider--discovery-cache (make-hash-table :test #'equal))
        (deslop-provider--discovery-inflight (make-hash-table :test #'equal))
        (deslop-provider-discovery-cache-size 2))
    (dotimes (index 5)
      (deslop-provider--finish-discovery (list index) (list :model index)))
    (should (= (hash-table-count deslop-provider--discovery-cache) 2))))

(ert-deftest deslop-test/provider-client-display-names ()
  (should (string= (deslop-provider-client-name
                    '(:url "http://localhost:8080/v1"))
                   "localhost"))
  (should (string= (deslop-provider-client-name
                    '(:url "http://192.168.1.20:8080/v1"))
                   "192.168.1.20"))
  (should (string= (deslop-provider-client-name
                    '(:url "https://api.openai.com/v1/chat/completions"))
                   "OpenAI")))

(ert-deftest deslop-test/provider-discovery-parses-model-metadata ()
  (let* ((profile '(:url "http://localhost:8080/v1/chat/completions"
                    :model "llama3"))
         (metadata
          (deslop-provider--discovery-metadata
           profile
             (deslop-protocol-json-read-string
            "{\"object\":\"list\",\"data\":[{\"id\":\"llama3\",\"owned_by\":\"llama.cpp\"},{\"id\":\"llama4\"}]}"))))
    (should (string= (deslop-provider-models-url profile)
                     "http://localhost:8080/v1/models"))
    (should (eq (plist-get metadata :source) :discovery))
    (should (equal (plist-get metadata :models) '("llama3" "llama4")))
    (should (string= (plist-get metadata :model) "llama3"))
    (should (string= (plist-get metadata :owner) "llama.cpp"))
    (should (string= (plist-get metadata :client) "llama.cpp"))))

(ert-deftest deslop-test/provider-discovery-does-not-borrow-unrelated-owner ()
  (let* ((profile '(:url "https://example.test/v1/chat/completions"
                    :model "requested"))
         (metadata
          (deslop-provider--discovery-metadata
           profile
            (deslop-protocol-json-read-string
             "{\"data\":[{\"id\":\"other\",\"owned_by\":\"wrong-owner\"}]}"))))
    (should (equal (plist-get metadata :models) '("other")))
    (should-not (plist-get metadata :owner))
    (should-not (plist-get metadata :client))))

(ert-deftest deslop-test/provider-discovery-is-async-and-cached ()
  (let ((profile '(:name local
                   :url "http://localhost:8080/v1/chat/completions"
                   :model "llama3"))
        (captured nil)
        (process nil)
        (values nil)
        (probe-values nil)
        (make-process-count 0)
        (real-make-process (symbol-function 'make-process)))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq captured args)
                 (setq make-process-count (1+ make-process-count))
                 (setq process
                       (funcall real-make-process
                                :name (plist-get args :name)
                                :buffer nil
                                :command (list "cat")
                                :connection-type 'pipe)))))
      (unwind-protect
          (progn
            (deslop-provider-probe-models
             profile
             (lambda (models error)
               (push (list models error) probe-values)))
            (should (process-live-p process))
            (let* ((command (plist-get captured :command))
                   (header-position
                    (cl-position "--dump-header" command :test #'string=))
                   (headers-file (nth (1+ header-position) command)))
              (should (member "GET" command))
              (should (member "http://localhost:8080/v1/models" command))
              (with-temp-file headers-file
                (insert "HTTP/1.1 200 OK\r\n"
                        "Content-Type: application/json\r\n\r\n"))
              (funcall
               (plist-get captured :filter) process
               "{\"data\":[{\"id\":\"llama3\",\"owned_by\":\"llama.cpp\"},{\"id\":\"llama4\"}]}")
              (should (string-match-p
                       "llama3"
                       (with-current-buffer (plist-get captured :buffer)
                         (buffer-string))))
              (should (plist-get
                       (deslop-provider--parse-discovery-response
                        profile (plist-get captured :buffer) headers-file)
                       :model))
              (process-send-eof process)
              (while (process-live-p process)
                (accept-process-output process 0.05))
              (funcall (plist-get captured :sentinel) process "finished\n"))
            (should (= make-process-count 1))
            (should (= (length probe-values) 1))
            (should (equal (car (car probe-values)) '("llama3" "llama4")))
            (should-not (cadr (car probe-values)))
            (deslop-provider-discover
             profile (lambda (metadata) (push metadata values)))
            (should (= (length values) 1))
            (should (string= (plist-get (car values) :model) "llama3"))
            (let ((models nil)
                  (probe-error nil))
              (deslop-provider-probe-models
               profile
               (lambda (value error)
                 (setq models value
                       probe-error error)))
              (should (equal models '("llama3" "llama4")))
              (should-not probe-error))
            (deslop-provider-discover
             profile (lambda (metadata) (push metadata values)))
            (should (= make-process-count 1))
            (should (= (length values) 2))
            (should (string= (plist-get (car values) :model) "llama3")))
        (when (and (processp process) (process-live-p process))
          (delete-process process))
        (clrhash deslop-provider--discovery-cache)
        (clrhash deslop-provider--discovery-inflight)))))

(ert-deftest deslop-test/provider-discovery-cancellation-releases-last-waiter ()
  (let ((profile '(:name local
                   :url "http://localhost:8080/v1/chat/completions"
                   :model "llama3"))
        (process nil)
        (value nil)
        (real-make-process (symbol-function 'make-process)))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq process
                       (funcall real-make-process
                                :name (plist-get args :name)
                                :buffer nil
                                :command (list "cat")
                                :connection-type 'pipe)))))
      (unwind-protect
          (let* ((key (deslop-provider--discovery-key profile))
                 (cancel
                  (deslop-provider-discover
                   profile (lambda (metadata) (setq value metadata)))))
            (should (process-live-p process))
            (should (gethash key deslop-provider--discovery-inflight))
            (funcall cancel)
            (should-not value)
            (should-not (process-live-p process))
             (should-not (gethash key deslop-provider--discovery-inflight))
             (should-not (gethash key deslop-provider--discovery-cache))
             (should-not deslop-provider--discovery-processes)
             (let* ((probe-value nil)
                    (probe-error nil)
                    (probe-cancel
                    (deslop-provider-probe-models
                     profile
                     (lambda (models error)
                       (setq probe-value models
                             probe-error error)))))
               (should (process-live-p process))
               (funcall probe-cancel)
               (should-not probe-value)
               (should-not probe-error)
               (should-not (process-live-p process))
               (should-not (gethash key deslop-provider--discovery-inflight))
               (should-not deslop-provider--discovery-processes)))
        (when (and (processp process) (process-live-p process))
          (delete-process process))
        (clrhash deslop-provider--discovery-cache)
        (clrhash deslop-provider--discovery-inflight)
        (setq deslop-provider--discovery-processes nil)))))

(ert-deftest deslop-test/provider-discovery-response-is-bounded ()
  (let ((profile '(:name local
                   :url "http://localhost:8080/v1/chat/completions"
                   :model "llama3"))
        (deslop-provider-max-discovery-bytes 4)
        (captured nil)
        (process nil)
        (values nil)
        (real-make-process (symbol-function 'make-process)))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq captured args)
                 (setq process
                       (funcall real-make-process
                                :name (plist-get args :name)
                                :buffer (plist-get args :buffer)
                                :command (list "cat")
                                :connection-type 'pipe)))))
      (unwind-protect
          (progn
            (deslop-provider-discover
             profile (lambda (metadata) (push metadata values)))
            (funcall (plist-get captured :filter) process "oversized")
            (should (equal values '(nil)))
            (should-not (process-live-p process))
            (should (= (hash-table-count
                        deslop-provider--discovery-inflight) 0)))
        (when (and (processp process) (process-live-p process))
          (delete-process process))
        (clrhash deslop-provider--discovery-cache)
        (clrhash deslop-provider--discovery-inflight)))))

(ert-deftest deslop-test/history-retains-terminal-result ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" result"))))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (deslop-insert-at-cursor)
      (with-current-buffer deslop-history-buffer-name
        (let ((text (buffer-string)))
          (should (string-match-p "Output" text))
          (should (= (deslop-test--count-matches "result" text) 1))
          (should (string-match-p "base" text))
          (should (eq buffer-undo-list t)))))))

(ert-deftest deslop-test/history-retains-failed-stream-and-raw-usage ()
  (deslop-test--reset)
  (let ((deslop-llm-function
         (deslop-test--fake-llm
          '((:thinking "reasoning before failure"
             :content ""
             :tokens 4096
             :speed 58.4
             :usage (:prompt_tokens 12
                     :completion_tokens 4096
                     :reasoning_tokens 4000
                     :total_tokens 4108))))))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (let ((job (deslop-insert-at-cursor)))
        (should (eq (deslop-job-state job) 'failed)))
      (with-current-buffer deslop-history-buffer-name
        (let ((text (buffer-string)))
          (should (string-match-p "Prompt" text))
          (should (string-match-p "base" text))
          (should (string-match-p "Thinking" text))
          (should (string-match-p "reasoning before failure" text))
          (should (string-match-p "reasoning_tokens" text))
          (should (string-match-p "empty completion" text))
          (should buffer-read-only)
          (should-error (insert "user edit") :type 'buffer-read-only)
          (let ((buffer-read-only nil))
            (should-error (insert "user edit") :type 'user-error)))))))

(ert-deftest deslop-test/history-retains-joined-partial-result-on-failure ()
  (deslop-test--reset)
  (let ((deslop-llm-function
         (deslop-test--fake-llm
          '((:content ""
             :partial (:content ""
                       :thinking "Subheadings: ** TODO"
                       :tokens 4096
                       :speed 57.1
                       :finish-reason "length"))))))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (let ((job (deslop-insert-at-cursor)))
        (should (eq (deslop-job-state job) 'failed)))
      (with-current-buffer deslop-history-buffer-name
        (let ((text (buffer-string)))
          (should (string-match-p "Thinking" text))
          (should (string-match-p (regexp-quote "Subheadings: ** TODO")
                                  text))
          (should (string-match-p "Tokens: 4096" text))
          (should (string-match-p "Finish reason: length" text))
          (should-not (string-match-p "data:" text)))))))

(ert-deftest deslop-test/history-command-displays-buffer ()
  (deslop-test--reset)
  (let ((displayed nil))
    (cl-letf (((symbol-function 'display-buffer)
               (lambda (buffer &rest _) (setq displayed buffer))))
      (deslop-history)
      (should (eq displayed (get-buffer deslop-history-buffer-name)))))
  (should (eq (lookup-key deslop-mode-map (kbd "C-c e"))
              #'deslop-selector-open)))

(ert-deftest deslop-test/history-terminal-cleanup-survives-disable-and-write-errors ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" result") t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (deslop-insert-at-cursor)
      (should (= (hash-table-count deslop-history--entries) 1))
      (let ((deslop-history-enabled nil))
        (deslop-test--complete-call (car deslop-test--calls)))
      (should (= (hash-table-count deslop-history--entries) 0))))
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" result") t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (deslop-insert-at-cursor)
      (with-current-buffer (deslop-history--buffer)
        (add-hook 'before-change-functions
                  (lambda (&rest _) (error "history blocked")) nil t))
      (deslop-test--complete-call (car deslop-test--calls))
      (should (= (hash-table-count deslop-history--entries) 0)))))

(ert-deftest deslop-test/history-is-bounded-and-body-cannot-create-headings ()
  (deslop-test--reset)
  (let ((deslop-history-max-entries 2)
        (deslop-llm-function
         (deslop-test--fake-llm '(" one" " two" "\n* fake heading"))))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (dotimes (_ 3)
        (deslop-insert-at-cursor)
        (deslop-review-delete-overlay
         (cl-find-if #'deslop-review-overlay-p
                     (overlays-in (point-min) (point-max)))))
      (with-current-buffer deslop-history-buffer-name
        (let ((text (buffer-string)))
           (should (= (deslop-test--count-matches "^\\* Job " text) 2))
           (should-not (string-match-p "^\\* fake heading" text)))))))

(ert-deftest deslop-test/history-replaces-oldest-entries-at-default-limit ()
  (deslop-test--reset)
  (let ((deslop-history-max-entries 100)
        (deslop-llm-function
         (deslop-test--fake-llm
          (cl-loop for index from 0 to 102
                   collect (format " item-%d" index)))))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (dotimes (_ 103)
        (deslop-insert-at-cursor)
        (deslop-review-delete-overlay
         (cl-find-if #'deslop-review-overlay-p
                     (overlays-in (point-min) (point-max)))))
      (with-current-buffer deslop-history-buffer-name
        (let ((text (buffer-string)))
          (should (= (deslop-test--count-matches "^\\* Job " text) 100))
          (should-not (string-match-p "^\\* Job 1:" text))
          (should-not (string-match-p "^\\* Job 3:" text))
          (should (string-match-p "^\\* Job 103:" text)))))))

;;;; Overlay lifecycle

(ert-deftest deslop-test/insert-anchor-does-not-follow-edits-at-point ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '(" X") t)))
    (deslop-test--with-buffer "base"
      (goto-char (point-max))
      (let* ((job (deslop-insert-at-cursor))
             (call (car deslop-test--calls))
             (overlay (deslop-job-overlay job)))
        (dotimes (_ 5)
          (insert "\n"))
        (should (= (overlay-start overlay) 5))
        (should (= (deslop-target-cursor (deslop-job-target job))
                   5))
        (deslop-test--complete-call call)
        (should (string= (buffer-string) "base X\n\n\n\n\n"))))))

(ert-deftest deslop-test/zero-width-overlay-boundary-behavior ()
  (deslop-test--reset)
  (deslop-test--with-buffer "abc"
    (goto-char (point-max))
    (let ((ov (deslop-ui-create-overlay (current-buffer) (point) (point)
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

(ert-deftest deslop-test/overlay-lifecycle ()
  (deslop-test--reset)
  (deslop-test--with-buffer "hello"
    (let ((ov (deslop-ui-create-overlay (current-buffer) 1 3 "Queued")))
      (should (overlayp ov))
      (should (= (length deslop-ui--managed-overlays) 1))
      (should-not deslop-ui--spinning-overlays)
      (should-not deslop-ui--spinner-timer)
      (deslop-ui-update-overlay ov :status "Thinking")
      (should (memq ov deslop-ui--spinning-overlays))
      (should (timerp deslop-ui--spinner-timer))
      (deslop-ui-delete-overlay ov)
      (should (null deslop-ui--managed-overlays))
      (should (null deslop-ui--spinner-timer)))))

(ert-deftest deslop-test/narrowing-does-not-affect-review-discovery ()
  (deslop-test--reset)
  (deslop-test--with-buffer "first second"
    (let ((first (deslop-review-create-overlay
                  (current-buffer) 1 6 nil nil ""))
          (second (deslop-review-create-overlay
                   (current-buffer) 7 13 nil nil "")))
      (narrow-to-region 1 6)
      (deslop-review-delete-overlay first)
      (should (= (length (deslop-review--overlays-in-buffer)) 1))
      (widen)
      (should (= (length (deslop-review--overlays-in-buffer)) 1))
      (deslop-review-delete-overlay second))))

;;;; Review highlight

(ert-deftest deslop-test/review-highlight-blend-color-math ()
  (should (string= (deslop-review--blend-color "#ffffff" "#0000ff" 0.15) "#d9d9ff"))
  (should (string= (deslop-review--blend-color "#000000" "#ffffff" 0.5) "#808080"))
  (should (string= (deslop-review--blend-color "#333333" "#ffffff" 0.0) "#333333"))
  (should (string= (deslop-review--blend-color "#333333" "#ffffff" 1.0) "#ffffff"))
  (should (null (deslop-review--blend-color "#abc" "#ffffff" 0.5)))
  (should (null (deslop-review--blend-color "#ffffff" "junk" 0.5))))

(ert-deftest deslop-test/review-highlight-hex-round-trip ()
  (should (equal (deslop-review--hex-components "#a1b2c3") '(161 178 195)))
  (should (string= (deslop-review--hex-join '(161 178 195)) "#a1b2c3"))
  (should (null (deslop-review--hex-components "red")))
  (should (null (deslop-review--hex-components "#12345")))
  (should (null (deslop-review--hex-components "#1234567")))
  (should (null (deslop-review--hex-join '(1 2)))))

(ert-deftest deslop-test/review-highlight-face-attr-resolution ()
  (set-face-foreground 'font-lock-keyword-face "#0000ff")
  (set-face-background 'font-lock-keyword-face "#ffffff")
  (should (string= (deslop-review--face-attr 'font-lock-keyword-face :foreground)
                   "#0000ff"))
  (should (string= (deslop-review--face-attr 'font-lock-keyword-face :background)
                   "#ffffff"))
  (should (string= (deslop-review--face-attr
                    '(:foreground "red" :background "blue") :foreground)
                   "red"))
  (should (string= (deslop-review--face-attr
                    '((:foreground "cyan") (:weight bold)) :foreground)
                   "cyan"))
  (should (string= (deslop-review--face-attr
                    '((:foreground "cyan") (:background "magenta")) :background)
                   "magenta"))
  (should (null (deslop-review--face-attr '(:weight bold) :foreground))))

(ert-deftest deslop-test/review-highlight-style-from-adjacent-face ()
  (set-face-foreground 'font-lock-keyword-face "#0000ff")
  (set-face-background 'font-lock-keyword-face "#ffffff")
  (deslop-test--with-buffer "defun x"
    (add-text-properties 1 6 '(face font-lock-keyword-face))
    (should (equal (deslop-review--style (current-buffer) 6)
                   '("#0000ff" . "#d9d9ff")))))

(ert-deftest deslop-test/review-highlight-style-default-fallback ()
  (set-face-foreground 'default "#111111")
  (set-face-background 'default "#222222")
  (deslop-test--with-buffer "plain x"
    (should (equal (deslop-review--style (current-buffer) 6)
                   '("#111111" . "#1f1f1f")))
    (should (equal (deslop-review--style (current-buffer) 1)
                   '("#111111" . "#1f1f1f")))))

(ert-deftest deslop-test/review-highlight-dark-background-light-foreground ()
  (set-face-foreground 'default "#e6e6e6")
  (set-face-background 'default "#1e1e1e")
  (deslop-test--with-buffer "plain text"
    (should (equal (deslop-review--style (current-buffer) 6)
                   '("#e6e6e6" . "#3c3c3c")))))

(ert-deftest deslop-test/review-highlight-overlay-sets-face ()
  (deslop-test--reset)
  (set-face-foreground 'font-lock-keyword-face "#0000ff")
  (set-face-background 'font-lock-keyword-face "#ffffff")
  (deslop-test--with-buffer "defunLLM"
    (add-text-properties 1 6 '(face font-lock-keyword-face))
    (let ((overlay (deslop-review-create-overlay
                    (current-buffer) 6 9 nil nil "")))
      (should (overlayp overlay))
      (should (equal (overlay-get overlay 'face)
                     '(:foreground "#0000ff" :background "#d9d9ff")))
      (should (eq (get-char-property 3 'face) 'font-lock-keyword-face))
      (should (null (get-char-property 3 'foreground-color)))
      (deslop-review-delete-overlay overlay)
      (should (eq (get-char-property 3 'face) 'font-lock-keyword-face)))))

(ert-deftest deslop-test/review-highlight-disabled-sets-no-properties ()
  (deslop-test--reset)
  (deslop-test--with-buffer "defunLLM"
    (add-text-properties 1 6 '(face font-lock-keyword-face))
    (let ((deslop-review-highlight nil))
      (let ((overlay (deslop-review-create-overlay
                      (current-buffer) 6 9 nil nil "")))
        (should (null (overlay-get overlay 'face)))))))

(ert-deftest deslop-test/review-highlight-approval-cleans-properties ()
  (deslop-test--reset)
  (set-face-foreground 'font-lock-keyword-face "#0000ff")
  (set-face-background 'font-lock-keyword-face "#ffffff")
  (let ((deslop-llm-function (deslop-test--fake-llm '("LLM"))))
    (deslop-test--with-buffer "kw rest"
      (add-text-properties 1 3 '(face font-lock-keyword-face))
      (goto-char 3)
      (deslop-insert-at-cursor)
      (should (string= (buffer-string) "kwLLM rest"))
      (let ((review (cl-find-if #'deslop-review-overlay-p
                                (overlays-in (point-min) (point-max)))))
        (should review)
        (should (equal (overlay-get review 'face)
                       '(:foreground "#0000ff" :background "#d9d9ff")))
        (goto-char 4)
        (deslop-approve-output)
        (should (string= (buffer-string) "kwLLM rest"))
        (should-not (deslop-review-overlay-p review))))))

(ert-deftest deslop-test/review-highlight-rejection-preserves-original-properties ()
  (deslop-test--reset)
  (let ((deslop-llm-function (deslop-test--fake-llm '("new"))))
    (deslop-test--with-buffer "old text"
      (add-text-properties 1 4 '(face font-lock-string-face))
      (add-text-properties 1 4 '(foreground-color "#123456"))
      (deslop-test--select 1 9 1)
      (deslop-improve-at-cursor "rewrite")
      (should (string= (buffer-string) "new"))
      (let ((review (cl-find-if #'deslop-review-overlay-p
                                (overlays-in (point-min) (point-max)))))
        (should review)
        (goto-char 2)
        (deslop-reject-output)
        (should (string= (buffer-string) "old text"))
        (should (eq (get-char-property 1 'face) 'font-lock-string-face))
        (should (string= (get-char-property 1 'foreground-color) "#123456"))
        (should (null (get-char-property 1 'background-color)))))))

(provide 'deslop-test)

;;; deslop-test.el ends here
