;;; ende-test.el --- ERT tests for ende -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools, llm

;;; Commentary:

;; ERT coverage for synchronous operations, asynchronous lifecycle behavior,
;; target conflicts, and the incremental streaming parsers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'ende-provider)
(require 'ende)

;;;; Test harness

(defvar ende-test--prompts nil)
(defvar ende-test--calls nil)
(defvar ende-test--fake-responses nil)

(defun ende-test--callback (call name &rest args)
  "Invoke callback NAME from CALL with ARGS when it exists."
  (let ((callback (plist-get (plist-get call :callbacks) name)))
    (when callback
      (apply callback args))))

(defun ende-test--overlay-label (overlay)
  "Return the display label attached to OVERLAY."
  (substring-no-properties
   (or (overlay-get overlay 'before-string)
       (overlay-get overlay 'after-string)
       "")))

(defun ende-test--complete-call (call)
  "Complete asynchronous fake CALL using its queued response."
  (let ((response (plist-get call :response)))
    (cond
     ((eq response :stall) nil)
     ((eq response :error)
      (ende-test--callback
       call :on-error
       (ende-error-create :kind 'mock :message "mock failure")))
     ((stringp response)
      (ende-test--callback call :on-event
                           (list :type :status :status "Generating"))
      (ende-test--callback call :on-event
                           (list :type :delta :channel :content
                                 :text response))
      (ende-test--callback call :on-event
                           (list :type :usage :tokens 10 :speed 50.0))
      (ende-test--callback call :on-done response))
     ((and (listp response) response)
      (when (or (plist-get response :reported-model)
                (plist-get response :reported-provider))
        (ende-test--callback
         call :on-event
         (list :type :metadata
               :metadata
               (list :source :response
                     :model (plist-get response :reported-model)
                     :provider (plist-get response :reported-provider)))))
      (when (stringp (plist-get response :thinking))
        (ende-test--callback
         call :on-event
         (list :type :delta :channel :thinking
               :text (plist-get response :thinking))))
      (when (stringp (plist-get response :content))
        (ende-test--callback
         call :on-event
         (list :type :delta :channel :content
               :text (plist-get response :content))))
      (when (plist-member response :usage)
        (ende-test--callback
         call :on-event
         (list :type :usage
               :tokens (plist-get response :tokens)
               :speed (plist-get response :speed)
               :usage (plist-get response :usage))))
      (when (listp (plist-get response :partial))
        (ende-test--callback
         call :on-event
         (list :type :partial-result :result (plist-get response :partial))))
      (ende-test--callback call :on-done response))
     (t
      (ende-test--callback call :on-error
                           (format "unknown mock response: %S" response))))))

(defun ende-test--fake-llm (responses &optional asynchronous process-factory)
  "Return a mock LLM dispatcher for RESPONSES.
When ASYNCHRONOUS is non-nil, calls remain in `ende-test--calls' until
`ende-test--complete-call' is invoked.  PROCESS-FACTORY can provide a real
process for cancellation tests."
  (setq ende-test--fake-responses (copy-sequence responses))
  (lambda (messages callbacks &optional options)
    (push messages ende-test--prompts)
    (let ((call (list :messages messages
                      :callbacks callbacks
                      :options options
                      :response (pop ende-test--fake-responses))))
      (when process-factory
        (setq call
              (plist-put call :process (funcall process-factory)))
        (ende-test--callback
         call :on-start
         (lambda ()
           (let ((process (plist-get call :process)))
             (when (and (processp process) (process-live-p process))
               (process-put process :ende-cancelled t)
               (delete-process process))))))
      (setq ende-test--calls
            (append ende-test--calls (list call)))
      (unless asynchronous
        (ende-test--complete-call call))
      call)))

(defun ende-test--user-message (messages)
  "Return the user content from MESSAGES."
  (cdr (assoc "user" messages)))

(defun ende-test--sleep-process ()
  "Create a process that remains live until explicitly terminated."
  (make-process :name (generate-new-buffer-name "ende-test-sleep")
                :command (list "sleep" "30")
                :connection-type 'pipe
                :noquery t))

(defun ende-test--select (start end point)
  "Activate the region START..END with point at POINT."
  (setq transient-mark-mode t)
  (goto-char point)
  (set-mark (if (= point end) start end))
  (setq mark-active t))

(defun ende-test--count-matches (regexp text)
  "Return the number of nonoverlapping REGEXP matches in TEXT."
  (let ((start 0)
        (count 0))
    (while (string-match regexp text start)
      (setq count (1+ count))
      (setq start (match-end 0)))
    count))

(defun ende-test--reset ()
  "Reset global state and clean test-created processes and buffers."
  (ende-abort-queue)
  (when (fboundp #'ende-openrouter-batch-abort)
    (ende-openrouter-batch-abort))
  (when (buffer-live-p ende-rubber-duck--buffer)
    (kill-buffer ende-rubber-duck--buffer))
  (setq ende-rubber-duck--buffer nil)
  (dolist (call ende-test--calls)
    (let ((process (plist-get call :process)))
      (when (and (processp process) (process-live-p process))
        (delete-process process))))
  (setq ende-test--prompts nil
        ende-test--calls nil
        ende-test--fake-responses nil
        ende-model-selection--last-used nil
        ende-queue nil
        ende--queue-tail nil
        ende--queue-suspended nil
        ende--current-job nil
        ende--active-jobs nil
        ende--fences nil
        ende--aborting nil
        ende--dispatching nil
        ende--next-job-id 0
        ende-openrouter-batch--next-request-id 0
        ende-openrouter-batch--known-groups nil
        ende-summary--next-source-id 0)
  (when (boundp 'ende-openrouter-batch--groups)
    (clrhash ende-openrouter-batch--groups))
  (clrhash ende-provider--discovery-cache)
  (clrhash ende-provider--discovery-inflight)
  (clrhash ende-provider--model-cache)
  (clrhash ende-provider--model-inflight)
  (clrhash ende-provider--model-cancellations)
  (setq ende-provider--model-generation 0)
  (clrhash ende-selector--model-validations)
  (clrhash ende-selector--effort-selections)
  (clrhash ende-model-selection--buffer-selections)
  (ende-provider-clear-runtime-settings)
  (clrhash ende-history--entries)
  (ende-tools-clear-change-sets)
  (setq ende-tools--next-change-set-id 0)
  (when (buffer-live-p ende-history--buffer-object)
    (kill-buffer ende-history--buffer-object))
  (setq ende-history--buffer-object nil)
  (when (buffer-live-p ende-summary--buffer-object)
    (kill-buffer ende-summary--buffer-object))
  (setq ende-summary--buffer-object nil)
  (clrhash ende-summary--source-buffers)
  (when (buffer-live-p ende-chat--buffer-object)
    (kill-buffer ende-chat--buffer-object))
  (setq ende-chat--buffer-object nil
        ende-chat--next-id 0)
  (ende-ui-clear-all))

(ert-deftest ende-test/queue-snapshots-do-not-expose-list-state ()
  (let ((ende--active-jobs (list :active))
        (ende-queue (list :pending)))
    (let ((active (ende-active-jobs))
          (pending (ende-pending-jobs)))
      (setcar active :changed)
      (setcar pending :changed)
      (should (equal ende--active-jobs '(:active)))
      (should (equal ende-queue '(:pending))))))

(defun ende-test--tool-result (name arguments root)
  "Invoke tool NAME and return its successful result in ROOT."
  (let ((call (ende-tool-call name arguments root)))
    (should (plist-get call :ok))
    (plist-get call :result)))

(ert-deftest ende-test/project-tools-expose-json-definitions ()
  (let ((definitions (ende-tool-definitions)))
    (should (= (length definitions) 8))
    (dolist (definition definitions)
      (let* ((function (alist-get :function definition))
             (schema
              (ende-protocol-json-read-string
               (json-encode (alist-get :parameters function)))))
        (should (hash-table-p (gethash "properties" schema)))
        (should (eq (gethash "additionalProperties" schema) :false))))
    (should-not (ende-tool-read-only
                 (ende-tool--find "edit_source")))
    (should (string-match-p
             "edit_source"
             (json-encode (ende-tool-definitions))))))

(ert-deftest ende-test/project-tools-enforce-scope-and-report-file-info ()
  (let* ((root (make-temp-file "ende-tools-project-" t))
         (source (expand-file-name "src/main.el" root))
         (secret (expand-file-name ".env" root))
         (outside (make-temp-file "ende-tools-outside-")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory source) t)
          (with-temp-file source
            (insert "one\ntwo\nthree\n"))
          (with-temp-file secret
            (insert "TOKEN=secret\n"))
          (let ((ende-project-root-directory root))
            (let* ((info (ende-test--tool-result
                          "file_info" '(("path" . "src/main.el")) root))
                   (files (ende-test--tool-result
                           "list_project_files" nil root)))
              (should (= (plist-get info :line_count) 3))
              (should (= (length (plist-get files :files)) 1))
              (should (string= (plist-get
                                (car (plist-get files :files)) :path)
                               "src/main.el")))
            (let ((failure
                   (ende-tool-call
                    "read_file" '(("path" . "../outside")) root)))
              (should-not (plist-get failure :ok))
              (should (string-match-p "outside the project"
                                      (plist-get failure :error))))))
      (when (file-directory-p root)
        (delete-directory root t))
      (when (file-exists-p outside)
        (delete-file outside)))))

(ert-deftest ende-test/project-tools-prefer-unsaved-visiting-buffers ()
  (let* ((root (make-temp-file "ende-tools-buffer-project-" t))
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
          (let* ((ende-project-root-directory root)
                 (listed (ende-test--tool-result
                          "list_project_buffers" nil root))
                 (record (car (plist-get listed :buffers)))
                 (buffer-id (plist-get record :buffer_id))
                 (read (ende-test--tool-result
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

(ert-deftest ende-test/project-tools-read-search-and-cap-results ()
  (let* ((root (make-temp-file "ende-tools-search-project-" t))
         (first (expand-file-name "first.txt" root))
         (second (expand-file-name "second.txt" root)))
    (unwind-protect
        (progn
          (with-temp-file first
            (insert "needle one\nkeep\nneedle two\n"))
          (with-temp-file second
            (insert "needle three\n"))
          (let* ((ende-project-root-directory root)
                 (read (ende-test--tool-result
                        "read_file"
                        '(("path" . "first.txt")
                          ("start_line" . 2)
                          ("line_count" . 1))
                        root))
                 (search (ende-test--tool-result
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

(ert-deftest ende-test/project-tools-edit-requires-digest-and-is-reviewable ()
  (let* ((root (make-temp-file "ende-tools-edit-project-" t))
         (path (expand-file-name "main.el" root))
         (buffer nil))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "hello world\n"))
          (let* ((ende-project-root-directory root)
                 (info (ende-test--tool-result
                        "file_info" '(("path" . "main.el")) root))
                 (proposal (ende-test--tool-result
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
            (ende-tools-apply-change-set change-set-id)
            (should-error (ende-tools-change-set change-set-id))
            (should (string= (with-temp-buffer
                               (insert-file-contents path)
                               (buffer-string))
                             "hello Emacs\n")))
          (setq buffer (find-file-noselect path))
          (let* ((ende-project-root-directory root)
                 (listed (ende-test--tool-result
                          "list_project_buffers" nil root))
                 (buffer-id (plist-get
                             (car (plist-get listed :buffers)) :buffer_id))
                 (info (ende-test--tool-result
                        "file_info" '(("path" . "main.el")) root))
                 (proposal (ende-test--tool-result
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
              (ende-tools-apply-change-set change-set-id)
              (should-error (ende-tools-change-set change-set-id)))
            (with-current-buffer buffer
              (should (string= (buffer-string) "hello Lisp\n"))
              (should (cl-some #'ende-review-overlay-p
                               (overlays-in (point-min) (point-max)))))))
      (let ((failure
             (ende-tool-call
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

(ert-deftest ende-test/project-tools-can-directly-write-closed-files ()
  (let* ((root (make-temp-file "ende-tools-direct-project-" t))
         (path (expand-file-name "main.el" root)))
    (unwind-protect
        (progn
          (ende-tools-clear-change-sets)
          (with-temp-file path
            (insert "old\n"))
          (let* ((ende-tools-direct-write t)
                 (info (ende-test--tool-result
                        "file_info" '(("path" . "main.el")) root))
                 (result (ende-test--tool-result
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

(ert-deftest ende-test/project-tools-retain-failed-change-sets-until-settled ()
  (let* ((root (make-temp-file "ende-tools-pending-project-" t))
         (path (expand-file-name "main.el" root))
         (ende-project-root-directory root))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "old\n"))
          (let* ((info (ende-test--tool-result
                        "file_info" '(("path" . "main.el")) root))
                 (proposal (ende-test--tool-result
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
            (should-error (ende-tools-apply-change-set id))
            (should (ende-change-set-p (ende-tools-change-set id)))
            (ende-tools-reject-change-set id)
            (should-error (ende-tools-change-set id))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ende-test/project-tools-reject-ambiguous-boundary-edits ()
  (should-error
   (ende-tools--validate-edits
    '((("start" . 3) ("end" . 3) ("expected" . "")
       ("replacement" . "!"))
      (("start" . 3) ("end" . 6) ("expected" . "XYZ")
       ("replacement" . "Q")))
    "abcXYZ")))

(ert-deftest ende-test/chat-tool-config-resolves-inheritance-and-rejects-code ()
  (let* ((root (make-temp-file "ende-chat-config-" t))
         (child-root (make-temp-file "ende-chat-child-config-" t))
         (configuration
          (format
           (concat
            "#+begin_ende\n(root %S)\n"
            "(group inspect project_info list_project_files file_info read_file "
            "search_project list_project_buffers read_buffer)\n"
            "(group modify edit_source)\n"
            "(resource project (include \"**\") (exclude \"private/**\"))\n"
            "(resource chat (include \"main.el\"))\n"
            "(tools inspect)\n"
            "(set-tool-resources inspect :resources project)\n"
            "#+end_ende\n\n"
            "* Review\n** Edit\n"
            "#+begin_ende\n(root %S)\n(tools modify)\n"
            "(set-tool-resources modify :resources chat)\n"
            "(set-tool-resources read_file :resources chat)\n"
            "(add-tool-resources read_file :resources project)\n"
            "#+end_ende\n*** Inspect only\n"
            "#+begin_ende\n(clear-tools)\n(tools inspect)\n"
            "(clear-tool-resources)\n"
            "(set-tool-resources inspect :resources project)\n"
            "#+end_ende\n")
           root child-root)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "private" root))
          (with-temp-buffer
            (insert configuration)
            (org-mode)
            (let* ((document (ende-chat--build-document))
                   (headlines (ende-chat--document-headlines document))
                   (edit (cl-find-if
                          (lambda (headline)
                            (equal (org-element-property :raw-value headline)
                                   "Edit"))
                          headlines))
                   (inspect-only
                    (cl-find-if
                     (lambda (headline)
                       (equal (org-element-property :raw-value headline)
                              "Inspect only"))
                     headlines))
                   (edit-config
                    (ende-chat-config-resolve
                     (ende-chat--document-tree document)
                     (ende-chat--document-parent document) edit))
                   (inspect-config
                    (ende-chat-config-resolve
                     (ende-chat--document-tree document)
                     (ende-chat--document-parent document) inspect-only)))
              (should (equal (plist-get edit-config :root)
                             (ende-project-normalize-root child-root)))
              (should (memq 'edit_source
                            (plist-get edit-config :active-tools)))
              (should (equal (cdr (assq 'edit_source
                                        (plist-get edit-config
                                                   :tool-resources)))
                             '(chat)))
              (should (equal (cdr (assq 'read_file
                                        (plist-get edit-config
                                                   :tool-resources)))
                             '(chat project)))
              (should (equal (plist-get inspect-config :active-tools)
                             '(project_info list_project_files file_info
					    read_file search_project list_project_buffers
					    read_buffer)))
              (should (equal (plist-get inspect-config :root)
                             (ende-project-normalize-root child-root)))
              (should-not (memq 'edit_source
                                (plist-get inspect-config :active-tools)))
              (should-not (assq 'edit_source
                                (plist-get inspect-config :tool-resources)))))
          (let ((ran nil))
            (should-error
             (ende-chat-config--read-forms "(#.(setq ran t))"))
            (should-not ran))
          (with-temp-buffer
            (insert "#+begin_ende\n(tools read_file)\n#+end_ende\n* Q\n")
            (org-mode)
            (let* ((document (ende-chat--build-document))
                   (question (car (ende-chat--document-headlines document))))
              (should-error
               (ende-chat-config-resolve
                (ende-chat--document-tree document)
                (ende-chat--document-parent document) question))))
          (with-temp-buffer
            (insert
             (format
              (concat "#+begin_ende\n(root %S)\n(group inspect read_file)\n"
                      "#+end_ende\n* A\n** B\n#+begin_ende\n"
                      "(group inspect project_info)\n#+end_ende\n")
              root))
            (org-mode)
            (let* ((document (ende-chat--build-document))
                   (question (car (last (ende-chat--document-headlines
                                         document)))))
              (should-error
               (ende-chat-config-resolve
                (ende-chat--document-tree document)
                (ende-chat--document-parent document) question)))))
      (delete-directory root t)
      (delete-directory child-root t))))

(ert-deftest ende-test/chat-tool-loop-filters-search-and-authorizes-calls ()
  (let* ((root (make-temp-file "ende-chat-tools-" t))
         (public (expand-file-name "main.el" root))
         (private-directory (expand-file-name "private" root))
         (private (expand-file-name "secret.txt" private-directory))
         (visited nil)
         (frozen-configuration nil)
         (original-search (symbol-function #'ende-tools--search-source)))
    (ende-test--reset)
    (unwind-protect
        (progn
          (make-directory private-directory)
          (with-temp-file public (insert "public text\n"))
          (with-temp-file private (insert "classified text\n"))
          (let ((ende-llm-function
                 (ende-test--fake-llm
                  (list
                   (list :tool-calls
                         (list (list :id "call-search"
                                     :name "search_project"
                                     :arguments "{\"regexp\":\"classified\"}")))
                   "Search complete")
                  t)))
            (with-temp-buffer
              (insert
               (format
                (concat
                 "#+begin_ende\n(root %S)\n"
                 "(group inspect project_info list_project_files read_file "
                 "search_project)\n"
                 "(resource project (include \"**\") "
                 "(exclude \"private/**\"))\n"
                 "(tools inspect)\n"
                 "(set-tool-resources inspect :resources project)\n"
                 "#+end_ende\n\n* Find classified text\nSearch now\n")
                root))
              (org-mode)
              (ende-test--chat-goto "* Find classified text")
              (cl-letf (((symbol-function #'ende-tools--search-source)
                         (lambda (source regexp before after maximum)
                           (push (plist-get source :relative) visited)
                           (funcall original-search source regexp before after
                                    maximum))))
                (ende-ask)
                (let* ((document (ende-chat--build-document))
                       (question (car (ende-chat--document-headlines document))))
                  (setq frozen-configuration
                        (ende-chat-config-resolve
                         (ende-chat--document-tree document)
                         (ende-chat--document-parent document) question))
                  (goto-char (point-min))
                  (should (search-forward
                           "(resource project (include \"**\")" nil t))
                  (replace-match
                   "(resource project (include \"private/**\")" t t)
                  (ende-test--complete-call (car ende-test--calls))
                  (ende-test--complete-call (nth 1 ende-test--calls))))
              (should (equal (nreverse visited) '("main.el")))
              (should (string-match-p "Search complete" (buffer-string)))
              (should (= (length ende-test--calls) 2))
              (let* ((first-call (car ende-test--calls))
                     (definitions (plist-get (plist-get first-call :options)
                                             :tool-definitions))
                     (second-messages (plist-get (nth 1 ende-test--calls)
                                                 :messages))
                     (tool-reply (car (last second-messages))))
                (should (equal (sort (mapcar (lambda (definition)
                                               (alist-get
                                                :name
                                                (alist-get :function
                                                           definition)))
                                             definitions)
                                     #'string<)
                               '("list_project_files" "project_info"
                                 "read_file" "search_project")))
                (should (equal (plist-get tool-reply :role) "tool"))
                (should-not (string-match-p "classified"
                                            (plist-get tool-reply :content)))
                (let* ((reply (json-parse-string
                               (plist-get tool-reply :content)
                               :object-type 'hash-table))
                       (result (gethash "result" reply)))
                  (should (= (gethash "files_inspected" result) 1))))
              (let* ((configuration frozen-configuration)
                     (denied
                      (ende-tool-call
                       "read_file" '(("path" . "private/secret.txt"))
                       root configuration))
                     (allowed
                      (ende-tool-call "read_file" '(("path" . "main.el"))
                                      root configuration))
                     (no-resources (copy-tree configuration)))
                (should (plist-get allowed :ok))
                (should (string-match-p
                         "public text"
                         (plist-get (plist-get allowed :result) :content)))
                (should-not (plist-get denied :ok))
                (should (string-match-p "not authorized"
                                        (plist-get denied :error)))
                (setf (plist-get no-resources :tool-resources) nil)
                (should (string-match-p
                         "no assigned resources"
                         (plist-get
                          (ende-tool-call
                           "read_file" '(("path" . "main.el"))
                           root no-resources)
                          :error)))
                (should (plist-get
                         (ende-tool-call "project_info" nil root no-resources)
                         :ok))))))
      (ende-test--reset)
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ende-test/tool-protocols-preserve-calls-and-results ()
  (let* ((definition
          (cl-find-if
           (lambda (candidate)
             (equal (alist-get :name (alist-get :function candidate))
                    "read_file"))
           (ende-tool-definitions)))
         (profile (list :model "model" :tool-definitions (list definition)))
         (call '(:id "call-1" :name "read_file"
                     :arguments "{\"path\":\"main.el\"}"))
         (messages (list (cons "user" "read a file")
                         (list :role "assistant" :content nil
                               :tool-calls (list call))
                         (list :role "tool" :tool-call-id "call-1"
                               :content "{\"ok\":true}")))
         (chat-body (ende-llm--request-body messages profile))
         (responses-body (ende-codex--request-body messages profile)))
    (should (equal (alist-get :tool_choice chat-body) "auto"))
    (should (equal (alist-get :tool_choice responses-body) "auto"))
    (let* ((assistant (nth 1 (alist-get :messages chat-body)))
           (tool-call (car (alist-get :tool_calls assistant))))
      (should (equal (alist-get :id tool-call) "call-1"))
      (should (equal (alist-get :name (alist-get :function tool-call))
                     "read_file")))
    (should (equal (alist-get :type (nth 1 (alist-get :input responses-body)))
                   "function_call"))
    (should (equal (alist-get :type (nth 2 (alist-get :input responses-body)))
                   "function_call_output"))
    (let ((session (ende-llm--make-session
                    :tool-calls (make-hash-table :test #'eql)
                    :estimated-chars 0)))
      (ende-llm--collect-tool-calls
       session
       (vector (json-parse-string
                "{\"index\":0,\"id\":\"call-1\",\"function\":{\"name\":\"read_\",\"arguments\":\"{\\\"path\\\":\"}}"
                :object-type 'hash-table)))
      (ende-llm--collect-tool-calls
       session
       (vector (json-parse-string
                "{\"index\":0,\"function\":{\"name\":\"file\",\"arguments\":\"\\\"main.el\\\"}\"}}"
                :object-type 'hash-table)))
      (should (equal (car (ende-llm--result session)) :content))
      (should (equal (plist-get (car (plist-get (ende-llm--result session)
                                                :tool-calls))
                                :arguments)
                     "{\"path\":\"main.el\"}")))
    (let* ((session (ende-codex--make-session
                     :tool-calls (make-hash-table :test #'eql)
                     :estimated-chars 0))
           (event
            (json-parse-string
             "{\"type\":\"response.completed\",\"response\":{\"output\":[{\"type\":\"function_call\",\"call_id\":\"call-2\",\"name\":\"read_file\",\"arguments\":\"{\\\"path\\\":\\\"main.el\\\"}\"}]}}"
             :object-type 'hash-table)))
      (ende-codex--handle-event session event)
      (should (equal (plist-get (car (plist-get (ende-codex--result session)
                                                :tool-calls))
                                :id)
                     "call-2")))))

(ert-deftest ende-test/openai-tool-diagnostics-are-safe-and-configurable ()
  (should (eq ende-tool-diagnostics t))
  (let* ((definition
          (cl-find-if
           (lambda (candidate)
             (equal (alist-get :name (alist-get :function candidate))
                    "read_file"))
           (ende-tool-definitions)))
         (body (ende-codex--request-body
                (list (cons "user" "question"))
                (list :model "gpt-test"
                      :tool-definitions (list definition))))
         (events nil)
         (session
          (ende-codex--make-session
           :profile '(:model "gpt-test")
           :callbacks (list :on-event
                            (lambda (event) (push event events)))
           :tool-calls (make-hash-table :test #'eql))))
    (ende-codex--diagnose-request session body)
    (ende-codex--handle-event
     session
     (ende-protocol-json-read-string
      (concat
       "{\"type\":\"response.completed\",\"response\":{\"output\":["
       "{\"type\":\"function_call\",\"call_id\":\"call-1\","
       "\"name\":\"read_file\",\"arguments\":"
       "\"{\\\"path\\\":\\\"secret.el\\\"}\"}]}}")))
    (let ((diagnostics
           (mapcar (lambda (event) (plist-get event :text))
                   (cl-remove-if-not
                    (lambda (event)
                      (eq (plist-get event :type) :diagnostic))
                    (nreverse events)))))
      (should (equal diagnostics
                     '("OpenAI Responses tools sent: read_file"
                       "OpenAI Responses output items: function_call(read_file)")))
      (should-not (string-match-p "secret\\.el"
                                  (mapconcat #'identity diagnostics "\n")))))
  (let ((ende-tool-diagnostics nil)
        (events nil))
    (let ((codex-session
           (ende-codex--make-session
            :callbacks (list :on-event
                             (lambda (event) (push event events)))))
          (chat-session
           (ende-llm--make-session
            :callbacks (list :on-event
                             (lambda (event) (push event events)))
            :tool-calls (make-hash-table :test #'eql))))
      (ende-codex--diagnose-request codex-session '((:tools . nil)))
      (ende-llm--diagnose-request chat-session '((:tools . nil)))
      (ende-codex--diagnose-output-item
       codex-session "response.output_item.done"
       (ende-protocol-json-read-string
        "{\"type\":\"function_call\",\"name\":\"read_file\"}"))
      (ende-llm--diagnose-result chat-session)
      (should-not events))))

(ert-deftest ende-test/chat-completions-tool-diagnostics-cover-local-and-openrouter ()
  (should (eq ende-tool-diagnostics t))
  (let* ((definition
          (cl-find-if
           (lambda (candidate)
             (equal (alist-get :name (alist-get :function candidate))
                    "read_file"))
           (ende-tool-definitions)))
         (tool-event
          (ende-protocol-json-read-string
           (concat
            "{\"choices\":[{\"delta\":{\"tool_calls\":["
            "{\"index\":0,\"id\":\"call-1\",\"type\":\"function\","
            "\"function\":{\"name\":\"read_file\","
            "\"arguments\":\"{\\\"path\\\":\\\"secret.el\\\"}\"}}]}}]}"))))
    (dolist (provider '(local openrouter))
      (let* ((profile (list :name provider :model "test-model"
                            :tool-definitions (list definition)))
             (body (ende-llm--request-body
                    (list (cons "user" "read the file")) profile))
             (events nil)
             (session
              (ende-llm--make-session
               :profile profile
               :callbacks (list :on-event
                                (lambda (event) (push event events)))
               :tool-calls (make-hash-table :test #'eql))))
        (ende-llm--diagnose-request session body)
        (ende-llm--handle-json session tool-event)
        (ende-llm--diagnose-result session)
        (let ((diagnostics
               (mapcar (lambda (event) (plist-get event :text))
                       (cl-remove-if-not
                        (lambda (event)
                          (eq (plist-get event :type) :diagnostic))
                        (nreverse events)))))
          (should (equal diagnostics
                         '("Chat Completions tools sent: read_file"
                           "Chat Completions output items: tool_call(read_file)")))
          (should-not (string-match-p
                       "secret\\.el"
                       (mapconcat #'identity diagnostics "\n"))))))))

(ert-deftest ende-test/openrouter-batch-tool-diagnostics-use-shared-setting ()
  (should (eq ende-tool-diagnostics t))
  (let* ((events nil)
         (request
          (ende-openrouter-batch--make-request
           :callbacks (list :on-event
                            (lambda (event) (push event events)))))
         (body
          (ende-protocol-json-read-string
           (concat
            "{\"choices\":[{\"message\":{\"content\":\"answer\","
            "\"tool_calls\":[{\"function\":{\"name\":\"read_file\","
            "\"arguments\":\"{\\\"path\\\":\\\"secret.el\\\"}\"}}]}}]}"))))
    (ende-openrouter-batch--diagnose-request request)
    (ende-openrouter-batch--diagnose-result request body)
    (let ((diagnostics (mapcar (lambda (event) (plist-get event :text))
                               (nreverse events))))
      (should (equal diagnostics
                     '("OpenRouter Batch request tools: (none)"
                       "OpenRouter Batch output items: message, tool_call(read_file)")))
      (should-not (string-match-p
                   "secret\\.el"
                   (mapconcat #'identity diagnostics "\n")))))
  (let ((ende-tool-diagnostics nil)
        (events nil))
    (let ((request
           (ende-openrouter-batch--make-request
            :callbacks (list :on-event
                             (lambda (event) (push event events))))))
      (ende-openrouter-batch--diagnose-request request)
      (ende-openrouter-batch--diagnose-result
       request (ende-protocol-json-read-string "{\"choices\":[]}"))
      (should-not events))))

(ert-deftest ende-test/openai-streamed-function-call-is-collected-without-final-output ()
  (let* ((events nil)
         (session
          (ende-codex--make-session
           :profile '(:model "gpt-test")
           :callbacks (list :on-event
                            (lambda (event) (push event events)))
           :estimated-chars 0)))
    (dolist
        (event
         (list
          (concat
           "{\"type\":\"response.output_item.added\","
           "\"output_index\":0,\"item\":{\"id\":\"fc_1\","
           "\"type\":\"function_call\",\"call_id\":\"call-1\","
           "\"name\":\"read_file\",\"arguments\":\"\"}}")
          (concat
           "{\"type\":\"response.function_call_arguments.delta\","
           "\"output_index\":0,\"item_id\":\"fc_1\","
           "\"delta\":\"{\\\"path\\\":\"}")
          (concat
           "{\"type\":\"response.function_call_arguments.delta\","
           "\"output_index\":0,\"item_id\":\"fc_1\","
           "\"delta\":\"\\\"ende.el\\\"}\"}")
          (concat
           "{\"type\":\"response.function_call_arguments.done\","
           "\"output_index\":0,\"item_id\":\"fc_1\","
           "\"arguments\":\"{\\\"path\\\":\\\"ende.el\\\"}\"}")
          (concat
           "{\"type\":\"response.output_item.done\","
           "\"output_index\":0,\"item\":{\"id\":\"fc_1\","
           "\"type\":\"function_call\",\"call_id\":\"call-1\","
           "\"name\":\"read_file\","
           "\"arguments\":\"{\\\"path\\\":\\\"ende.el\\\"}\"}}")
          (concat
           "{\"type\":\"response.completed\",\"response\":{"
           "\"output\":[],\"usage\":{\"output_tokens\":40}}}")))
      (ende-codex--handle-event
       session (ende-protocol-json-read-string event)))
    (let* ((call (car (plist-get (ende-codex--result session) :tool-calls)))
           (diagnostics
            (mapcar (lambda (event) (plist-get event :text))
                    (cl-remove-if-not
                     (lambda (event)
                       (eq (plist-get event :type) :diagnostic))
                     (nreverse events)))))
      (should (equal (plist-get call :id) "call-1"))
      (should (equal (plist-get call :name) "read_file"))
      (should (equal (plist-get call :arguments)
                     "{\"path\":\"ende.el\"}"))
      (should (member
               "OpenAI Responses response.output_item.added: function_call(read_file)"
               diagnostics))
      (should (member
               "OpenAI Responses response.output_item.done: function_call(read_file)"
               diagnostics))
      (should (member "OpenAI Responses output items: (none)" diagnostics))
      (should-not (string-match-p "ende\\.el"
                                  (mapconcat #'identity diagnostics "\n"))))))

(defmacro ende-test--with-buffer (contents &rest body)
  "Evaluate BODY in a temporary buffer containing CONTENTS."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,contents)
     ,@body))

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
			    (should-not ende--current-job)
			    (should-not ende-ui--managed-overlays))))

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

(ert-deftest ende-test/summarize-opens-buffer ()
  (ende-test--reset)
  (let ((ende-provider-model "gpt-5.6-luna")
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
  (let ((ende-provider-model "async-model")
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
    (should-not (string-match-p "lop-source:3" (buffer-string)))))

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
  (let ((ende-provider-model "configured-model")
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
  (let* ((response
          "{\"version\":1,\"edits\":[{\"anchor\":\"one\",\"position\":\"replace\",\"all\":true,\"text\":\"ONE\"}]}" )
         (ende-llm-function
          (ende-test--fake-llm (list (list :content response)))))
    (ende-test--with-buffer "one\none"
			    (let ((job (ende-write-anywhere)))
			      (should (eq (ende-job-state job) 'completed))
			      (should (string= (buffer-string) "ONE\nONE"))
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
  (let* ((response
          "{\"version\":1,\"edits\":[{\"anchor\":\"one\",\"position\":\"before\",\"occurrence\":1,\"text\":\"A\"},{\"anchor\":\"one\",\"position\":\"after\",\"all\":true,\"text\":\"B\"}]}" )
         (ende-llm-function
          (ende-test--fake-llm (list (list :content response)))))
    (ende-test--with-buffer "one\none"
			    (let ((job (ende-insert-anywhere)))
			      (should (eq (ende-job-state job) 'completed))
			      (should (string= (buffer-string) "AoneB\noneB"))
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

(ert-deftest ende-test/mode-line-reports-buffer-job-count ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm (list :stall :stall) t)))
    (ende-test--with-buffer "test"
			    (ende-mode 1)
			    (should (string= ende--mode-line-string " ende[0]"))
			    (let ((first (ende-write-anywhere))
				  (second nil))
			      (should (eq (ende-job-state first) 'running))
			      (should (string= ende--mode-line-string " ende[1]"))
			      (setq second (ende-write-anywhere))
			      (should (eq (ende-job-state second) 'queued))
			      (should (string= ende--mode-line-string " ende[2]"))
			      (should (string-match-p
				       "Thinking"
				       (ende-test--overlay-label (ende-job-overlay first))))
			      (should (string-match-p
				       "Queued"
				       (ende-test--overlay-label (ende-job-overlay second))))
			      (ende-abort-queue)
			      (should-not (ende-job-overlay first))
			      (should-not (ende-job-overlay second))
			      (should (string= ende--mode-line-string " ende[0]"))))))

(ert-deftest ende-test/mode-line-reports-pending-review-count ()
  (ende-test--reset)
  (ende-test--with-buffer "test"
			  (ende-mode 1)
			  (should (string= ende--mode-line-string " ende[0]"))
			  (let ((review (ende-review-create-overlay
					 (current-buffer) 1 2 nil nil "t")))
			    (should (string= ende--mode-line-string " ende[0|1]"))
			    (ende-approve-all-output)
			    (should (string= ende--mode-line-string " ende[0]"))
			    (ende-review-create-overlay
			     (current-buffer) 1 2 nil nil "t")
			    (should (string= ende--mode-line-string " ende[0|1]"))
			    (ende-reject-all-output)
			    (should (string= ende--mode-line-string " ende[0]"))
			    (should-not (ende-review-overlay-p review)))))

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

;;;; Queue lifecycle

(ert-deftest ende-test/queue-fifo-ordering-async ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm '(" 1" " 2" " 3") t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (setq ende--queue-suspended t)
			    (ende-insert-at-cursor)
			    (ende-insert-at-cursor)
			    (ende-insert-at-cursor)
			    (setq ende--queue-suspended nil)
			    (ende-process-next)
			    (should (= (length ende-test--calls) 1))
			    (ende-test--complete-call (nth 0 ende-test--calls))
			    (ende-test--complete-call (nth 1 ende-test--calls))
			    (ende-test--complete-call (nth 2 ende-test--calls))
			    (should (string= (buffer-string) "base 1 2 3"))
			    (should (null ende-queue))
			    (should (null ende--current-job)))))

(ert-deftest ende-test/abort-invalidates-before-kill ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm '(:stall "never") t
                              #'ende-test--sleep-process)))
    (ende-test--with-buffer "content"
			    (let ((job (ende-insert-at-cursor)))
			      (ende-insert-at-cursor)
			      (let ((call (car ende-test--calls)))
				(should ende--current-job)
				(ende-abort-queue)
				(should (eq (ende-job-state job) 'cancelled))
				(should-not ende--current-job)
				(should (null ende-queue))
				(should-not (process-live-p (plist-get call :process)))
				;; A late transport callback must be inert.
				(ende-test--callback call :on-done "orphan")
				(should (string= (buffer-string) "content")))))))

(ert-deftest ende-test/stale-callback-cannot-settle-next-job ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm '(" A" " B") t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (setq ende--queue-suspended t)
			    (ende-insert-at-cursor)
			    (ende-insert-at-cursor)
			    (setq ende--queue-suspended nil)
			    (ende-process-next)
			    (let ((first (nth 0 ende-test--calls)))
			      (ende-test--complete-call first)
			      (let ((second (nth 1 ende-test--calls)))
				(should second)
				(should (eq (ende-job-state ende--current-job) 'running))
				(ende-test--callback first :on-done " stale")
				(ende-test--callback first :on-error "stale error")
				(should (eq (ende-job-state ende--current-job) 'running))
				(ende-test--complete-call second)))
			    (should (string= (buffer-string) "base A B")))))

(ert-deftest ende-test/duplicate-completion-is-idempotent ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" X") t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((job (ende-insert-at-cursor))
				  (call nil))
			      (setq call (car ende-test--calls))
			      (ende-test--complete-call call)
			      (ende-test--callback call :on-done " X")
			      (should (eq (ende-job-state job) 'completed))
			      (should (string= (buffer-string) "base X"))
			      (should-not ende--current-job)))))

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

(ert-deftest ende-test/malformed-structured-error-cannot-wedge-queue ()
  (ende-test--reset)
  (let ((ende-llm-function
         (lambda (_messages callbacks &optional _options)
           (funcall (plist-get callbacks :on-error)
                    (ende-error-create :kind 'provider :message 123)))))
    (ende-test--with-buffer "base"
			    (let ((job (ende-insert-at-cursor)))
			      (should (eq (ende-job-state job) 'failed))
			      (should-not ende--current-job)
			      (should-not ende--current-job)))))

(ert-deftest ende-test/synchronous-failure-backlog-is-iterative ()
  (ende-test--reset)
  (ende-test--with-buffer "base"
			  (let ((ende--queue-suspended t)
				(max-lisp-eval-depth 100))
			    (dotimes (_ 500)
			      (ende-enqueue
			       (list 'unknown
				     :target (ende-target-create
					      :buffer (current-buffer) :start 1 :end 1 :cursor 1))))
			    (setq ende--queue-suspended nil)
			    (cl-letf (((symbol-function 'message) #'ignore))
			      (ende-process-next))
			    (should-not ende--current-job)
			    (should-not ende-queue))))

(ert-deftest ende-test/quit-and-cleanup-errors-still-settle-jobs ()
  (ende-test--reset)
  (let ((ende--job-handlers (copy-tree ende--job-handlers)))
    (ende-register-job-handler
     'quit-test (lambda (_job) (signal 'quit nil)))
    (ende-test--with-buffer "base"
			    (let ((job (ende-enqueue
					(list 'quit-test
					      :target (ende-target-create
						       :buffer (current-buffer)
						       :start 1 :end 1 :cursor 1)))))
			      (should (eq (ende-job-state job) 'failed))
			      (should-not ende--current-job))))
  (ende-test--reset)
  (let ((ende--job-handlers (copy-tree ende--job-handlers)))
    (ende-register-job-handler
     'cleanup-test (lambda (job) (ende-job-finish job "ok")))
    (ende-test--with-buffer "base"
			    (let* ((overlay (ende-ui-create-overlay
					     (current-buffer) 1 1 "Queued"))
				   (job nil))
			      (cl-letf (((symbol-function 'ende-ui-delete-overlay)
					 (lambda (_) (error "cleanup failed"))))
				(setq job
				      (ende-enqueue
				       (list 'cleanup-test
					     :target (ende-target-create
						      :buffer (current-buffer)
						      :start 1 :end 1 :cursor 1)
					     :overlay overlay))))
			      (should (eq (ende-job-state job) 'completed))
			      (should-not ende--current-job)
			      (delete-overlay overlay)))))

;;;; Target failure paths

(ert-deftest ende-test/killed-source-buffer-cancels-active-and-queued-jobs ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" X") t))
        (buffer (generate-new-buffer " *ende-killed*"))
        (active nil)
        (pending nil))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "base")
            (goto-char (point-max))
            (setq active (ende-insert-at-cursor)))
          (let ((ende--queue-suspended t))
            (with-current-buffer buffer
              (setq pending (ende-insert-at-cursor))))
          (should (eq (ende-job-state active) 'running))
          (should (eq (ende-job-state pending) 'queued))
          (kill-buffer buffer)
          (should (eq (ende-job-state active) 'cancelled))
          (should (eq (ende-job-state pending) 'cancelled))
          (should-not ende--current-job)
          (should-not ende-queue)
          (ende-test--callback (car ende-test--calls) :on-done "late")
          (should (eq (ende-job-state active) 'cancelled)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest ende-test/source-buffer-cancellation-preserves-outer-abort ()
  (ende-test--reset)
  (let ((first (generate-new-buffer " *ende-abort-first*"))
        (second (generate-new-buffer " *ende-abort-second*"))
        (observed nil)
        (first-job nil)
        (second-job nil))
    (unwind-protect
        (progn
          (with-current-buffer first
            (insert "first")
            (setq first-job
                  (ende--make-job
                   :id 1 :state 'running
                   :target (ende-target-create
                            :buffer first :start 1 :end 1 :cursor 1)
                   :cancel-function
                   (lambda ()
                     (kill-buffer second)
                     (setq observed ende--aborting)))
                  ende--source-buffer-jobs (list first-job))
            (add-hook 'kill-buffer-hook #'ende--source-buffer-killed nil t))
          (with-current-buffer second
            (insert "second")
            (setq second-job
                  (ende--make-job
                   :id 2 :state 'queued
                   :target (ende-target-create
                            :buffer second :start 1 :end 1 :cursor 1)))
            (setq ende--source-buffer-jobs (list second-job))
            (add-hook 'kill-buffer-hook #'ende--source-buffer-killed nil t))
          (setq ende--active-jobs (list first-job)
                ende--current-job first-job
                ende-queue (list second-job)
                ende--queue-tail ende-queue)
          (ende-abort-queue)
          (should observed)
          (should-not ende--aborting)
          (should (eq (ende-job-state first-job) 'cancelled))
          (should (eq (ende-job-state second-job) 'cancelled)))
      (when (buffer-live-p first) (kill-buffer first))
      (when (buffer-live-p second) (kill-buffer second)))))

(ert-deftest ende-test/read-only-target-settles-job ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" X") t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((job (ende-insert-at-cursor))
				  (call nil))
			      (setq call (car ende-test--calls))
			      (setq buffer-read-only t)
			      (ende-test--complete-call call)
			      (should (eq (ende-job-state job) 'failed))
			      (should-not ende--current-job)
			      (should (string= (buffer-string) "base"))))))

(ert-deftest ende-test/narrowed-target-settles-job ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" X") t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((job (ende-insert-at-cursor))
				  (call nil))
			      (setq call (car ende-test--calls))
			      (narrow-to-region (point-min) (1- (point-max)))
			      (ende-test--complete-call call)
			      (should (eq (ende-job-state job) 'failed))
			      (should-not ende--current-job)))))

(ert-deftest ende-test/widening-does-not-expand-queued-context ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" X") t)))
    (ende-test--with-buffer "SECRET safe SECRET"
			    (narrow-to-region 8 12)
			    (goto-char 10)
			    (setq ende--queue-suspended t)
			    (ende-insert-at-cursor)
			    (widen)
			    (setq ende--queue-suspended nil)
			    (ende-process-next)
			    (let ((user-message
				   (cdr (cadr (plist-get (car ende-test--calls) :messages)))))
			      (should (string-match-p "sa<CURSOR>fe" user-message))
			      (should-not (string-match-p "SECRET" user-message))))))

(ert-deftest ende-test/modification-hook-settles-job ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(" X") t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((job (ende-insert-at-cursor))
				  (call nil))
			      (setq call (car ende-test--calls))
			      (add-hook 'before-change-functions
					(lambda (&rest _) (error "blocked")) nil t)
			      (ende-test--complete-call call)
			      (should (eq (ende-job-state job) 'failed))
			      (should (string= (buffer-string) "base"))
			      (should-not ende--current-job)))))

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

;;;; Anchor ordering and conflicts

(ert-deftest ende-test/same-endpoint-preserves-fifo ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("A" "B") t)))
    (ende-test--with-buffer "alpha"
			    (ende-test--select 1 6 6)
			    (setq ende--queue-suspended t)
			    (ende-insert-at-cursor)
			    (ende-insert-at-cursor)
			    (setq ende--queue-suspended nil)
			    (ende-process-next)
			    (ende-test--complete-call (nth 0 ende-test--calls))
			    (ende-test--complete-call (nth 1 ende-test--calls))
			    (should (string= (buffer-string) "alpha\nA\nB")))))

(ert-deftest ende-test/parallel-same-endpoint-preserves-enqueue-order ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm '("A" "B") t))
        (profile '(:name remote :model "remote-model"
			 :max-concurrency 2 :parallel t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((ende--queue-suspended t)
				  (target (ende-target-create
					   :buffer (current-buffer) :start 5 :end 5 :cursor 5)))
			      (ende-enqueue (list 'insert :target target :profile profile))
			      (ende-enqueue
			       (list 'insert
				     :target (ende-target-create
					      :buffer (current-buffer) :start 5 :end 5 :cursor 5)
				     :profile profile))
			      (setq ende--queue-suspended nil)
			      (ende-process-next))
			    (should (= (length ende--active-jobs) 2))
			    (ende-test--complete-call (nth 1 ende-test--calls))
			    (ende-test--complete-call (nth 0 ende-test--calls))
			    (should (string= (buffer-string) "baseAB")))))

(ert-deftest ende-test/parallel-job-at-provider-limit-keeps-policy ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm '("A" "B") t))
        (profile '(:name remote :model "remote-model"
			 :max-concurrency 1 :parallel t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((first (ende-enqueue
					  (list 'insert
						:target (ende-target-create
							 :buffer (current-buffer)
							 :start 5 :end 5 :cursor 5)
						:profile profile)))
				  (second nil))
			      (setq second
				    (ende-enqueue
				     (list 'insert
					   :target (ende-target-create
						    :buffer (current-buffer)
						    :start 5 :end 5 :cursor 5)
					   :profile profile)))
			      (should (eq (ende-job-state first) 'running))
			      (should (eq (ende-job-state second) 'queued))
			      (should (plist-get (ende-job-profile second) :parallel))
			      (ende-test--complete-call (nth 0 ende-test--calls))
			      (should (eq (ende-job-state second) 'running))
			      (ende-test--complete-call (nth 1 ende-test--calls))
			      (should (string= (buffer-string) "baseAB"))))))

(ert-deftest ende-test/parallel-job-uses-free-slot-alongside-serial-job ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm '("A" "B") t))
        (serial-profile '(:name remote :model "serial"
				:max-concurrency 4 :parallel nil))
        (parallel-profile '(:name remote :model "parallel"
				  :max-concurrency 4 :parallel t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((first
				   (ende-enqueue
				    (list 'insert
					  :target (ende-target-create
						   :buffer (current-buffer)
						   :start 5 :end 5 :cursor 5)
					  :profile serial-profile)))
				  (second nil))
			      (let ((ende--queue-suspended t))
				(setq second
				      (ende-enqueue
				       (list 'insert
					     :target (ende-target-create
						      :buffer (current-buffer)
						      :start 5 :end 5 :cursor 5)
					     :profile parallel-profile))))
			      (should (eq (ende-job-state first) 'running))
			      (should (eq (ende-job-state second) 'queued))
			      (ende-process-next)
			      (should (eq (ende-job-state second) 'running))
			      (should (= (length ende--active-jobs) 2))
			      (ende-test--complete-call (nth 0 ende-test--calls))
			      (ende-test--complete-call (nth 1 ende-test--calls))
			      (should (string= (buffer-string) "baseAB"))))))

(ert-deftest ende-test/global-fence-waits-for-existing-jobs ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm '("A" "B" "C") t))
        (profile '(:name remote :model "remote-model"
			 :max-concurrency 4 :parallel t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((first (ende-enqueue
					  (list 'insert
						:target (ende-target-create
							 :buffer (current-buffer)
							 :start 5 :end 5 :cursor 5)
						:profile profile)))
				  (second (ende-enqueue
					   (list 'insert
						 :target (ende-target-create
							  :buffer (current-buffer)
							  :start 5 :end 5 :cursor 5)
						 :profile profile)))
				  (third nil))
			      (ende-fence-all)
			      (setq third
				    (ende-enqueue
				     (list 'insert
					   :target (ende-target-create
						    :buffer (current-buffer)
						    :start 5 :end 5 :cursor 5)
					   :profile profile)))
			      (should (eq (ende-job-state first) 'running))
			      (should (eq (ende-job-state second) 'running))
			      (should (eq (ende-job-state third) 'queued))
			      (should (= (length ende--fences) 1))
			      (ende-test--complete-call (nth 0 ende-test--calls))
			      (should (eq (ende-job-state third) 'queued))
			      (ende-test--complete-call (nth 1 ende-test--calls))
			      (should (eq (ende-job-state third) 'running))
			      (should-not ende--fences)
			      (ende-test--complete-call (nth 2 ende-test--calls))
			      (should (string= (buffer-string) "baseABC"))))))

(ert-deftest ende-test/provider-fence-does-not-block-other-providers ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm '("A1" "B1" "A2" "B2") t))
        (first-profile '(:name first :model "first-model"
                               :max-concurrency 4 :parallel t))
        (second-profile '(:name second :model "second-model"
				:max-concurrency 4 :parallel t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((first (ende-enqueue
					  (list 'insert
						:target (ende-target-create
							 :buffer (current-buffer)
							 :start 5 :end 5 :cursor 5)
						:profile first-profile)))
				  (second (ende-enqueue
					   (list 'insert
						 :target (ende-target-create
							  :buffer (current-buffer)
							  :start 5 :end 5 :cursor 5)
						 :profile second-profile)))
				  (third nil)
				  (fourth nil))
			      (ende-fence-provider 'first)
			      (setq third
				    (ende-enqueue
				     (list 'insert
					   :target (ende-target-create
						    :buffer (current-buffer)
						    :start 5 :end 5 :cursor 5)
					   :profile first-profile)))
			      (setq fourth
				    (ende-enqueue
				     (list 'insert
					   :target (ende-target-create
						    :buffer (current-buffer)
						    :start 5 :end 5 :cursor 5)
					   :profile second-profile)))
			      (should (eq (ende-job-state first) 'running))
			      (should (eq (ende-job-state second) 'running))
			      (should (eq (ende-job-state third) 'queued))
			      (should (eq (ende-job-state fourth) 'running))
			      (ende-test--complete-call (nth 1 ende-test--calls))
			      (ende-test--complete-call (nth 0 ende-test--calls))
			      (should (eq (ende-job-state third) 'running))
			      (ende-test--complete-call (nth 3 ende-test--calls))
			      (ende-test--complete-call (nth 2 ende-test--calls))
			      (should (string= (buffer-string) "baseA1B1B2A2"))))))

(ert-deftest ende-test/equal-provider-identities-share-free-slots ()
  (ende-test--reset)
  (let* ((ende-llm-function
          (ende-test--fake-llm '("X" "A" "B") t))
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
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((active (ende-enqueue
					   (list 'insert
						 :target (ende-target-create
							  :buffer (current-buffer)
							  :start 5 :end 5 :cursor 5)
						 :profile active-profile)))
				  (serial nil)
				  (parallel nil))
			      (let ((ende--queue-suspended t))
				(setq serial
				      (ende-enqueue
				       (list 'insert
					     :target (ende-target-create
						      :buffer (current-buffer)
						      :start 5 :end 5 :cursor 5)
					     :profile serial-profile))
				      parallel
				      (ende-enqueue
				       (list 'insert
					     :target (ende-target-create
						      :buffer (current-buffer)
						      :start 5 :end 5 :cursor 5)
					     :profile parallel-profile))))
			      (should (eq (ende-job-state active) 'running))
			      (should (eq (ende-job-state serial) 'queued))
			      (should (eq (ende-job-state parallel) 'queued))
			      (should (= (length ende-test--calls) 1))
			      (ende-test--complete-call (car ende-test--calls))
			      (should (eq (ende-job-state serial) 'running))
			      (should (eq (ende-job-state parallel) 'running))
			      (ende-test--complete-call (nth 1 ende-test--calls))
			      (ende-test--complete-call (nth 2 ende-test--calls))
			      (should (string= (buffer-string) "baseXAB"))))))

(ert-deftest ende-test/parallel-slots-are-scoped-per-provider ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm '("A1" "B1" "A2") t))
        (first-profile '(:name first :model "first-model"
                               :max-concurrency 1 :parallel t))
        (second-profile '(:name second :model "second-model"
				:max-concurrency 1 :parallel t)))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((first (ende-enqueue
					  (list 'insert
						:target (ende-target-create
							 :buffer (current-buffer)
							 :start 5 :end 5 :cursor 5)
						:profile first-profile)))
				  (second nil)
				  (third nil))
			      (setq second
				    (ende-enqueue
				     (list 'insert
					   :target (ende-target-create
						    :buffer (current-buffer)
						    :start 5 :end 5 :cursor 5)
					   :profile first-profile)))
			      (setq third
				    (ende-enqueue
				     (list 'insert
					   :target (ende-target-create
						    :buffer (current-buffer)
						    :start 5 :end 5 :cursor 5)
					   :profile second-profile)))
			      (should (eq (ende-job-state first) 'running))
			      (should (eq (ende-job-state second) 'queued))
			      (should (eq (ende-job-state third) 'running))
			      (should (= (length ende--active-jobs) 2))
			      (ende-abort-queue)))))

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
			      (should-not ende--current-job)))))

;;;; Incremental reasoning parser

(defun ende-test--reasoning-callbacks (thinking content)
  "Return callbacks appending thinking and content into two symbols."
  (list :on-delta
        (lambda (type text)
          (if (eq type :thinking)
              (set thinking (concat (symbol-value thinking) text))
            (set content (concat (symbol-value content) text))))
        :on-status #'ignore))

(ert-deftest ende-test/thinking-parser-separation ()
  (let ((thinking-symbol (make-symbol "thinking"))
        (content-symbol (make-symbol "content")))
    (set thinking-symbol "")
    (set content-symbol "")
    (let* ((parser (ende-protocol-make-reasoning))
           (callbacks (ende-test--reasoning-callbacks
                       thinking-symbol content-symbol)))
      (ende-protocol-reasoning-feed
       parser "Intro <think>Plan here" callbacks)
      (ende-protocol-reasoning-feed
       parser "...done</think>Final answer" callbacks)
      (should-not (ende-protocol-reasoning-finish parser callbacks))
      (should (string= (symbol-value thinking-symbol) "Plan here...done"))
      (should (string= (symbol-value content-symbol) "Intro Final answer")))))

(ert-deftest ende-test/thinking-parser-all-delimiter-splits ()
  (dolist (split (number-sequence 0 (length "<think>secret</think>answer")))
    (let ((thinking "") (content "")
          (parser (ende-protocol-make-reasoning)))
      (let ((callbacks
             (list :on-delta
                   (lambda (type text)
                     (if (eq type :thinking)
                         (setq thinking (concat thinking text))
                       (setq content (concat content text)))))))
        (ende-protocol-reasoning-feed
         parser (substring "<think>secret</think>answer" 0 split) callbacks)
        (ende-protocol-reasoning-feed
         parser (substring "<think>secret</think>answer" split) callbacks)
        (should-not (ende-protocol-reasoning-finish parser callbacks))
        (should (string= thinking "secret"))
        (should (string= content "answer"))))))

(ert-deftest ende-test/thinking-parser-unclosed-is-error ()
  (let ((parser (ende-protocol-make-reasoning)))
    (ende-protocol-reasoning-feed parser "<think>secret" (list :on-delta #'ignore))
    (should (ende-error-p
             (ende-protocol-reasoning-finish parser (list :on-delta #'ignore))))))

;;;; SSE parser

(ert-deftest ende-test/sse-parser-fragmented-and-done ()
  (let ((parser (ende-protocol-make-sse))
        (events nil))
    (setq events
          (append events
                  (ende-protocol-sse-feed
                   parser "data: {\"choices\":[{\"delta\":{\"content\":\"hi")))
    (setq events
          (append events
                  (ende-protocol-sse-feed
                   parser " there\"}}]}\r\n\r\ndata: [DONE]\r\n\r\n")))
    (should (= (length events) 2))
    (should (eq (plist-get (nth 0 events) :type) :json))
    (should (string= (ende-protocol-json-get
                      "content"
                      (ende-protocol-json-get
                       "delta"
                       (aref (ende-protocol-json-get
                              "choices"
                              (plist-get (nth 0 events) :value))
                             0)))
                     "hi there"))
    (should (eq (plist-get (nth 1 events) :type) :done))
    (should-not (ende-protocol-sse-finish parser))))

(ert-deftest ende-test/sse-parser-rejects-malformed-and-ignores-late-data ()
  (let ((parser (ende-protocol-make-sse)))
    (should-error
     (ende-protocol-sse-feed parser "data: {bad}\n\n"))
    (let ((done-parser (ende-protocol-make-sse)))
      (ende-protocol-sse-feed done-parser "data: [DONE]\n\n")
      (should-not
       (ende-protocol-sse-feed
        done-parser "data: {\"choices\":[]}\n\n")))))

(ert-deftest ende-test/sse-parser-requires-done ()
  (let ((parser (ende-protocol-make-sse)))
    (ende-protocol-sse-feed parser "data: {\"choices\":[]}\n\n")
    (should-error (ende-protocol-sse-finish parser))))

(ert-deftest ende-test/sse-parser-flushes-final-event-without-done ()
  (let ((parser (ende-protocol-make-sse)))
    (should-not
     (ende-protocol-sse-feed parser "data: {\"type\":\"response.completed\"}"))
    (let ((events (ende-protocol-sse-flush parser)))
      (should (= (length events) 1))
      (should (string=
               (ende-protocol-json-get
                "type" (plist-get (car events) :value))
               "response.completed")))
    (should-not (ende-protocol-sse-flush parser))
    (should-error (ende-protocol-sse-finish parser))))

(ert-deftest ende-test/transport-session-settles-once-and-cleans-resources ()
  (let* ((process-buffer (generate-new-buffer " *ende-test-process*"))
         (stderr-buffer (generate-new-buffer " *ende-test-stderr*"))
         (headers-file (make-temp-file "ende-test-headers-"))
         (config-file (make-temp-file "ende-test-config-"))
         (events 0)
         (done 0)
         (errors 0)
         (discovery-cancelled nil)
         (session
          (ende-llm--make-session
           :callbacks
           (list :on-event (lambda (_event)
                             (setq events (1+ events))
                             (error "callback failed"))
                 :on-done (lambda (_result) (setq done (1+ done)))
                 :on-error (lambda (_error) (setq errors (1+ errors))))
           :discovery-cancel (lambda () (setq discovery-cancelled t))
           :process-buffer process-buffer :stderr-buffer stderr-buffer
           :headers-file headers-file :config-file config-file)))
    (ende-llm--fail
     session (ende-error-create :kind 'transport :message "failed"))
    (ende-llm--fail
     session (ende-error-create :kind 'transport :message "late"))
    (ende-llm--settle session 'success "late")
    (should (eq (ende-llm--session-terminal session) 'failure))
    (should (= events 1))
    (should (= done 0))
    (should (= errors 1))
    (should discovery-cancelled)
    (should-not (ende-llm--session-discovery-cancel session))
    (should-not (ende-llm--session-callbacks session))
    (should-not (ende-llm--session-process session))
    (should-not (ende-llm--session-process-buffer session))
    (should-not (ende-llm--session-stderr-buffer session))
    (should-not (ende-llm--session-stderr-process session))
    (should-not (ende-llm--session-headers-file session))
    (should-not (ende-llm--session-config-file session))
    (should-not (buffer-live-p process-buffer))
    (should-not (buffer-live-p stderr-buffer))
    (should-not (file-exists-p headers-file))
    (should-not (file-exists-p config-file))))

(ert-deftest ende-test/codex-session-clears-resource-slots ()
  (let ((process-buffer (generate-new-buffer " *ende-test-codex-process*"))
        (stderr-buffer (generate-new-buffer " *ende-test-codex-stderr*"))
        (headers-file (make-temp-file "ende-test-codex-headers-"))
        (config-file (make-temp-file "ende-test-codex-config-")))
    (let ((session
           (ende-codex--make-session
            :callbacks nil
            :process-buffer process-buffer
            :stderr-buffer stderr-buffer
            :headers-file headers-file
            :config-file config-file)))
      (ende-codex--fail
       session (ende-error-create :kind 'transport :message "failed"))
      (should-not (ende-codex--session-process session))
      (should-not (ende-codex--session-process-buffer session))
      (should-not (ende-codex--session-stderr-buffer session))
      (should-not (ende-codex--session-stderr-process session))
      (should-not (ende-codex--session-headers-file session))
      (should-not (ende-codex--session-config-file session))
      (should-not (buffer-live-p process-buffer))
      (should-not (buffer-live-p stderr-buffer))
      (should-not (file-exists-p headers-file))
      (should-not (file-exists-p config-file)))))

(ert-deftest ende-test/provider-processes-from-tramp-stay-local ()
  (dolist (operation '(generation codex-generation discovery models refresh))
    (let ((native-comp-enable-subr-trampolines nil)
          (default-directory "/ssh:ende-test.invalid:/source/")
          (ende-provider-discovery-enabled nil)
          (ende-provider--discovery-cache (make-hash-table :test #'equal))
          (ende-provider--discovery-inflight (make-hash-table :test #'equal))
          (ende-codex--refresh-flights (make-hash-table :test #'equal))
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
                         (funcall real-process :name "ende-test-local"
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
                ((symbol-function 'ende-codex--auth-entry) (lambda () auth)))
        (unwind-protect
            (let ((callbacks (list :on-start (lambda (value) (setq cancel value))
                                   :on-error (lambda (value) (setq failure value)))))
              (pcase operation
                ('generation
                 (ende-llm-default '(("user" . "remote source")) callbacks profile))
                ('codex-generation
                 (ende-codex-default '(("user" . "remote source")) callbacks))
                ('discovery
                 (let ((ende-provider-discovery-enabled t))
                   (setq cancel (ende-provider-discover profile #'ignore))))
                ('models
                 (setq cancel (ende-codex-probe-models
                               (lambda (_models error) (setq failure error)))))
                ('refresh
                 (setq cancel (ende-codex--refresh-auth
                               auth (lambda (value) (setq failure value))))))
              (should-not failure)
              (should (= process-count 1))
              (should (= pipe-count 1))
              (should (= send-count (if (memq operation '(discovery models)) 0 1)))
              (should files)
              (should (equal default-directory "/ssh:ende-test.invalid:/source/")))
          (when cancel (funcall cancel))
          (when (and process (process-live-p process)) (delete-process process))
          (when process
            (ende-curl-cleanup-resources
             (list (process-get process :ende-response-buffer)
                   (process-get process :ende-stderr-buffer))
             files
             (list (process-get process :ende-stderr-process)))))))))

(ert-deftest ende-test/provider-rejects-unsafe-temporary-directory ()
  (dolist (directory '("/ssh:ende-test.invalid:/tmp/" "relative-temp/"))
    (let ((native-comp-enable-subr-trampolines nil)
          (temporary-file-directory directory))
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest _) (ert-fail "Started a process")))
                ((symbol-function 'make-temp-file)
                 (lambda (&rest _) (ert-fail "Created a temporary file")))
                ((symbol-function 'ende-curl--auth-secret)
                 (lambda (&rest _) (ert-fail "Read credentials"))))
        (should-error (ende-curl-local-directory) :type 'user-error)
        (should-error
         (ende-curl-start-request :profile '(:url "https://example.test"))
         :type 'user-error)
        (should-error (ende-curl--config '(:url "https://example.test"))
                      :type 'user-error)))))

(defun ende-test--run-default-transport (header response)
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
        (ende-provider-discovery-enabled nil)
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
            (ende-llm-default
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

(ert-deftest ende-test/default-transport-completes-valid-sse ()
  (let ((out (ende-test--run-default-transport
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
    (should (member "Accept: text/event-stream" (nth 2 out)))
    (should (equal (cadr (member "--user-agent" (nth 2 out)))
                   ende-client-name))
    (should (member "@-" (nth 2 out)))))

(ert-deftest ende-test/default-transport-accepts-text-completion-deltas ()
  (let ((out (ende-test--run-default-transport
              "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n"
              "data: {\"choices\":[{\"text\":\"legacy\"}]}\n\ndata: [DONE]\n\n")))
    (should-not (nth 1 out))
    (should (string= (plist-get (nth 0 out) :content) "legacy"))))

(ert-deftest ende-test/default-transport-requires-done ()
  (let ((out (ende-test--run-default-transport
              "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n"
              "data: {\"choices\":[{\"finish_reason\":\"stop\",\"delta\":{\"content\":\"answer\"}}]}\n\n")))
    (should-not (nth 0 out))
    (should (eq (ende-error-kind (nth 1 out)) 'protocol))
    (should (string-match-p "without \\[DONE\\]"
                            (ende-error-message (nth 1 out))))))

(ert-deftest ende-test/default-transport-retains-joined-partial-result ()
  (let* ((first
          "data: {\"choices\":[{\"finish_reason\":null,\"delta\":{\"reasoning_content\":\"Sub\"}}]}\n\n")
         (second
          "data: {\"choices\":[{\"finish_reason\":\"length\",\"delta\":{\"reasoning_content\":\"headings\"}}]}\n\n")
         (out (ende-test--run-default-transport
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

(ert-deftest ende-test/default-transport-rejects-http-errors ()
  (let ((out (ende-test--run-default-transport
              "HTTP/1.1 401 Unauthorized\r\nContent-Type: text/event-stream\r\n\r\n"
              "data: {\"error\":{\"message\":\"bad key\"}}\n\ndata: [DONE]\n\n")))
    (should-not (nth 0 out))
    (should (ende-error-p (nth 1 out)))
    (should (eq (ende-error-kind (nth 1 out)) 'http))
    (should (= (nth 3 out) 0))
    (should (= (nth 4 out) 1))))

(ert-deftest ende-test/default-transport-settles-empty-provider-error ()
  (let ((out (ende-test--run-default-transport
              "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n"
              "data: {\"error\":{}}\n\ndata: [DONE]\n\n")))
    (should-not (nth 0 out))
    (should (eq (ende-error-kind (nth 1 out)) 'provider))
    (should (= (nth 4 out) 1))))

(ert-deftest ende-test/default-transport-done-ignores-same-chunk-trailing-data ()
  (let ((out (ende-test--run-default-transport
              "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n"
              (concat
               "data: {\"choices\":[{\"delta\":{\"content\":\"ok\"}}]}\n\n"
               "data: [DONE]\n\n"
               "data: malformed trailing bytes\n\n"))))
    (should (string= (plist-get (nth 0 out) :content) "ok"))
    (should-not (nth 1 out))
    (should (= (nth 3 out) 1))))

(ert-deftest ende-test/openai-profile-uses-opencode-oauth ()
  (let ((profile (ende-provider-profile 'openai)))
    (should (eq (plist-get profile :wire-api) 'responses))
    (should (eq (plist-get profile :auth-method) 'opencode-oauth))
    (should (string= (plist-get profile :model) "gpt-5.6-luna"))
    (should (string= (plist-get profile :url)
                     "https://chatgpt.com/backend-api/codex/responses"))
    (should-not (ende-provider-configured-model profile))
    (should-not (plist-get (ende-provider-profile-snapshot 'openai) :access))
    (should-not (plist-get (ende-provider-profile-snapshot 'openai) :refresh)))
  (let ((ende-codex-model "gpt-configured"))
    (should (string= (ende-provider-configured-model
                      (ende-provider-profile 'openai))
                     "gpt-configured"))))

(ert-deftest ende-test/provider-list-and-configured-models-are-selectable ()
  (let ((ende-providers '(openai local))
        (ende-provider-profiles
         '((local :models ("local-one" "local-two")))))
    (should (equal (ende-provider-names) '(openai local)))
    (should (equal (ende-provider-models
                    (ende-provider-profile 'local))
                   '("local-one" "local-two")))))

(ert-deftest ende-test/provider-runtime-concurrency-and-parallel-settings ()
  (ende-provider-clear-runtime-settings)
  (let ((ende-provider-profiles
         '((remote :max-concurrency 3 :parallel nil))))
    (let ((profile (ende-provider-profile 'remote)))
      (should (= (ende-provider-max-concurrency profile) 3))
      (should-not (ende-provider-parallel-enabled-p profile)))
    (ende-provider-set-runtime-setting 'remote :max-concurrency 5)
    (ende-provider-set-runtime-setting 'remote :parallel t)
    (let ((profile (ende-provider-profile 'remote)))
      (should (= (ende-provider-max-concurrency profile) 5))
      (should (ende-provider-parallel-enabled-p profile)))))

(ert-deftest ende-test/openai-model-probe-parses-official-slugs ()
  (should (equal
           (ende-codex--parse-models
            (concat "{\"models\":[{\"slug\":\"gpt-5.6-luna\"},"
                    "{\"slug\":\"gpt-5.5\"},{\"title\":\"ignored\"}]}"))
           '("gpt-5.6-luna" "gpt-5.5"))))

(ert-deftest ende-test/openai-model-probe-response-is-bounded ()
  (let ((ende-provider-max-discovery-bytes 4)
        (captured nil)
        (process nil)
        (result nil)
        (real-make-process (symbol-function 'make-process)))
    (cl-letf (((symbol-function 'ende-codex--resolve-auth)
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
            (ende-codex-probe-models
             (lambda (models error)
               (setq result (list models error))))
            (should (process-live-p process))
            (funcall (plist-get captured :filter) process "oversized")
            (should (equal (car result) nil))
            (should (ende-error-p (cadr result)))
            (should (eq (ende-error-kind (cadr result)) 'resource))
            (should-not (process-live-p process))
            (should-not ende-codex--model-probe-processes))
        (when (and (processp process) (process-live-p process))
          (delete-process process))
        (setq ende-codex--model-probe-processes nil)))))

(ert-deftest ende-test/openai-quota-parser-and-display-are-window-aware ()
  (let* ((now 2000000000)
         (quota
          (ende-codex--parse-quota
           (concat
            "{\"rate_limit\":{"
            "\"primary_window\":{\"used_percent\":28,"
            "\"reset_at\":2000018660,\"limit_window_seconds\":18000},"
            "\"secondary_window\":{\"used_percent\":59,"
            "\"reset_at\":2000007980,\"limit_window_seconds\":604800}}}"))))
    (should (= (plist-get (plist-get quota :primary) :used-percent) 28))
    (should (= (plist-get (plist-get quota :secondary) :used-percent) 59))
    (should
     (string=
      (ende-codex-quota-string quota now)
      "5h 11m: 72% left · weekly: 41% left · reset in 2h 13m"))
    (should-error
     (ende-codex--parse-quota
      "{\"rate_limit\":{\"primary_window\":null,\"secondary_window\":null}}"))))

(ert-deftest ende-test/openai-quota-display-follows-secondary-reset ()
  (let ((now 2000000000)
        (quota
         '(:primary (:used-percent 10
				   :reset-at 2000003600
				   :window-seconds 18000)
		    :secondary (:used-percent 20
					      :reset-at 2000021600
					      :window-seconds 604800))))
    (should
     (string=
      (ende-codex-quota-string quota now)
      "1h: 90% left · weekly: 80% left · reset in 6h"))
    (should
     (string=
      (ende-codex-quota-string (list :primary (plist-get quota :primary)) now)
      "1h: 90% left"))))

(ert-deftest ende-test/openai-quota-probe-uses-account-scoped-oauth ()
  (let ((captured nil)
        (config-text nil)
        (process nil)
        (result nil)
        (failure nil)
        (real-make-process (symbol-function 'make-process)))
    (cl-letf (((symbol-function 'ende-codex--resolve-auth)
               (lambda (callback)
                 (funcall callback
                          '(:access "access-token" :account-id "account-id"))
                 nil))
              ((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq captured args)
                 (let ((config
                        (cadr (member "--config"
                                      (plist-get args :command)))))
                   (when config
                     (with-temp-buffer
                       (insert-file-contents config)
                       (setq config-text (buffer-string)))))
                 (setq process
                       (funcall real-make-process
                                :name (plist-get args :name)
                                :buffer nil
                                :command '("cat")
                                :connection-type 'pipe)))))
      (unwind-protect
          (progn
            (ende-codex-probe-quota
             (lambda (quota error)
               (setq result quota
                     failure error)))
            (let* ((command (plist-get captured :command))
                   (headers-file
                    (cadr (member "--dump-header" command))))
              (with-temp-file headers-file
                (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n"))
              (funcall
               (plist-get captured :filter)
               process
               "{\"rate_limit\":{\"primary_window\":{\"used_percent\":22}}}")
              (process-send-eof process)
              (while (process-live-p process)
                (accept-process-output process 0.05))
              (funcall (plist-get captured :sentinel) process "finished\n"))
            (should result)
            (should-not failure)
            (should (string-match-p
                     "Authorization: Bearer access-token" config-text))
            (should (string-match-p
                     "ChatGPT-Account-Id: account-id" config-text))
            (should (member "Accept: application/json" (plist-get captured :command)))
            (should-not (member "access-token" (plist-get captured :command)))
            (should-not ende-codex--quota-probe-processes))
        (when (and (processp process) (process-live-p process))
          (delete-process process))))))

(ert-deftest ende-test/selector-quota-is-scoped-and-ignores-stale-results ()
  (let ((buffer (generate-new-buffer " *ende-selector-quota-test*"))
        (quota-callback nil)
        (cancel-count 0)
        (ende-providers '(local openai))
        (ende-provider-profiles
         '((local :model "test-model" :models ("test-model"))
           (openai :model "test-model" :auth-method opencode-oauth
                   :models ("test-model"))))
        (ende-provider--runtime-settings (make-hash-table :test #'equal))
        (ende-selector--last-selection nil))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(local openai)
                ende-selector--provider-index 0)
          (cl-letf (((symbol-function 'ende-provider-probe-models)
                     (lambda (_profile callback)
                       (funcall callback '("test-model") nil)
                       (lambda () nil)))
                    ((symbol-function 'ende-codex-probe-quota)
                     (lambda (callback)
                       (setq quota-callback callback)
                       (lambda () (cl-incf cancel-count)))))
            (ende-selector--set-provider 0)
            (should-not quota-callback)
            (should (string-match-p "Quota: unavailable"
                                    (buffer-string)))
            (ende-selector--set-provider 1)
            (should quota-callback)
            (should (eq ende-selector--quota-state :loading))
            (should (string-match-p "Quota: loading\.\.\."
                                    (buffer-string)))
            (let ((now (floor (float-time))))
              (funcall
               quota-callback
               (list :primary
                     (list :used-percent 28
                           :reset-at (+ now 18660)
                           :window-seconds 18000)
                     :secondary
                     (list :used-percent 59
                           :reset-at (+ now 7980)
                           :window-seconds 604800))
               nil)
              (should (eq ende-selector--quota-state :ready))
              (should (string-match-p
                       "Quota: 5h 1[01]m: 72% left"
                       (buffer-string)))
              (ende-selector--set-provider 1)
              (let ((stale-callback quota-callback))
                (ende-selector--set-provider 0)
                (should (= cancel-count 1))
                (should (eq ende-selector--quota-state :unavailable))
                (funcall stale-callback
                         (list :primary
                               (list :used-percent 1
                                     :reset-at (+ now 18660)
                                     :window-seconds 18000))
                         nil)
                (should (eq ende-selector--quota-state :unavailable))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ende-test/model-selection-restores-by-buffer-identity ()
  (let* ((name " *ende-model-selection-identity*")
         (file "/tmp/ende-model-selection-identity.el")
         (other-file "/tmp/ende-model-selection-other.el")
         (source nil)
         (other nil)
         (reopened nil)
         (different nil)
         (ende-model-selection-scope 'buffer)
         (ende-model-selection--last-used nil)
         (ende-model-selection--buffer-selections
          (make-hash-table :test #'equal))
         (ende-provider-model "default-model"))
    (unwind-protect
        (progn
          (setq source (get-buffer-create name))
          (with-current-buffer source
            (setq buffer-file-name file)
            (ende-mode 1)
            (should-not (ende-model-selection-for-buffer source))
            (should (equal (plist-get
                            (ende-provider-profile-for-buffer source) :model)
                           "default-model"))
            (should (= (hash-table-count
                        ende-model-selection--buffer-selections)
                       0))
            (ende-model-selection-save
             '(:provider remote :model "file-model"
                         :reasoning-effort "high")
             source))
          (kill-buffer source)
          (setq source nil
                other (get-buffer-create " *ende-model-selection-latest*"))
          (with-current-buffer other
            (setq buffer-file-name other-file)
            (ende-model-selection-save
             '(:provider openrouter :model "latest-model") other))
          (kill-buffer other)
          (setq other nil
                reopened (get-buffer-create name))
          (with-current-buffer reopened
            (setq buffer-file-name file)
            (ende-mode 1)
            (should (equal (ende-model-selection-for-buffer reopened)
                           '(:provider remote :model "file-model"
                                       :reasoning-effort "high")))
            (should (equal (plist-get
                            (ende-provider-profile-for-buffer reopened) :model)
                           "file-model"))
            (should (equal
                     (plist-get
                      (ende-provider-profile-for-buffer reopened)
                      :reasoning-effort)
                     "high")))
          (kill-buffer reopened)
          (setq reopened nil
                different (get-buffer-create name))
          (with-current-buffer different
            (setq buffer-file-name other-file)
            (ende-mode 1)
            (should (equal (ende-model-selection-for-buffer different)
                           '(:provider openrouter :model "latest-model"))))))
    (dolist (buffer (list source other reopened different))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ende-test/model-selection-global-scope-shares-one-choice ()
  (let ((ende-model-selection-scope 'global)
        (ende-model-selection--last-used nil)
        (ende-model-selection--buffer-selections
         (make-hash-table :test #'equal))
        (first (generate-new-buffer " *ende-model-selection-global-one*"))
        (second (generate-new-buffer " *ende-model-selection-global-two*")))
    (unwind-protect
        (progn
          (ende-model-selection-save
           '(:provider openai :model "shared-model"
                       :reasoning-effort "medium")
           first)
          (should (equal (ende-model-selection-for-buffer second)
                         '(:provider openai :model "shared-model"
                                     :reasoning-effort "medium")))
          (should (= (hash-table-count
                      ende-model-selection--buffer-selections)
                     0)))
      (dolist (buffer (list first second))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest ende-test/direct-request-uses-and-snapshots-buffer-selection ()
  (ende-test--reset)
  (let ((ende-model-selection-scope 'buffer)
        (ende-model-selection--last-used nil)
        (ende-model-selection--buffer-selections
         (make-hash-table :test #'equal))
        (ende-providers '(openrouter))
        (ende-provider-profiles nil)
        (ende-llm-function (ende-test--fake-llm '("summary")))
        (probes nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'ende-provider-probe-reasoning-efforts)
                     (lambda (profile model callback)
                       (push (list (plist-get profile :name) model) probes)
                       (funcall callback
                                (if (equal model "stale-model")
                                    '("low")
                                  '("high" "low"))
                                "low" nil)
                       (lambda () nil))))
            (with-temp-buffer
              (insert "Document to summarize")
              (ende-model-selection-save
               '(:provider openrouter :model "source-model"
                           :reasoning-effort "high"))
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
                         "high"))
                (should (equal (caar probes) 'openrouter))
                (should (equal (cadar probes) "source-model"))))
            (with-temp-buffer
              (insert "Another document")
              (ende-model-selection-save
               '(:provider openrouter :model "stale-model"
                           :reasoning-effort "high"))
              (let ((job (ende-summarize)))
                (should (eq (ende-job-state job) 'failed))
                (should (= (length ende-test--calls) 1))
                (should (equal (cadar probes) "stale-model"))))))
      (ende-test--reset))))

(ert-deftest ende-test/direct-effort-preflight-is-cancellable ()
  (ende-test--reset)
  (let ((ende-model-selection-scope 'buffer)
        (ende-model-selection--last-used nil)
        (ende-model-selection--buffer-selections
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
                           :reasoning-effort "high"))
              (let ((job (ende-summarize)))
                (should (eq (ende-job-state job) 'running))
                (should-not ende-test--calls)
                (ende-abort-queue)
                (should (eq (ende-job-state job) 'cancelled))
                (should (= cancel-count 1))
                (funcall effort-callback '("high") "high" nil)
                (should-not ende-test--calls)))))
      (ende-test--reset))))

(ert-deftest ende-test/direct-chat-request-uses-buffer-selection ()
  (ende-test--reset)
  (let ((ende-model-selection-scope 'buffer)
        (ende-model-selection--last-used nil)
        (ende-model-selection--buffer-selections
         (make-hash-table :test #'equal))
        (ende-llm-function (ende-test--fake-llm '("Reply"))))
    (unwind-protect
        (ende-test--with-buffer "* Root\n** Question\nAsk\n"
				(org-mode)
				(ende-test--chat-goto "** Question")
				(ende-model-selection-save
				 '(:provider openrouter :model "chat-model"))
				(let ((job (ende-ask)))
				  (should (eq (plist-get (ende-job-profile job) :name) 'openrouter))
				  (should (equal (plist-get (ende-job-profile job) :model)
						 "chat-model"))))
      (ende-test--reset))))

(ert-deftest ende-test/selector-runs-from-source-with-selected-profile ()
  (let* ((source (generate-new-buffer " *ende-selector-source*"))
         (selector (generate-new-buffer " *ende-selector-test*"))
         (ende-providers '(local))
         (ende-provider-profiles
          '((local :models ("one" "two")
                   :reasoning-effort "made-up"
                   :reasoning-effort-wire-api openrouter)))
         (ende-selector--last-selection nil)
         (queued nil))
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert "source"))
          (with-current-buffer selector
            (ende-selector-mode)
            (setq ende-selector--source-buffer source
                  ende-selector--providers '(local)
                  ende-selector--provider-index 0
                  ende-selector--models '("one" "two")
                  ende-selector--model-index 0)
            (ende-selector-next-model)
            (cl-letf (((symbol-function 'ende-enqueue-operation)
                       (lambda (type instruction profile)
                         (setq queued
                               (list type instruction profile (current-buffer))))))
              (ende-selector-insert)))
          (should (eq (nth 0 queued) 'insert))
          (should (string= (plist-get (nth 2 queued) :model) "two"))
          (should-not (plist-get (nth 2 queued) :reasoning-effort))
          (should (equal ende-selector--last-selection
                         '(:provider local :model "two")))
          (should (equal (ende-model-selection-for-buffer source)
                         '(:provider local :model "two")))
          (should (eq (nth 3 queued) source)))
      (dolist (buffer (list source selector))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest ende-test/selector-model-completion-updates-choice ()
  (let ((buffer (generate-new-buffer " *ende-selector-model-test*"))
        (completion-call nil)
        (ende-selector--last-selection nil))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(remote)
                ende-selector--provider-index 0
                ende-selector--models
                '("remote/alpha" "remote/beta" "other/gamma")
                ende-selector--model-details
                '((:id "remote/alpha" :name "Alpha Model"
                       :pricing (:prompt "0.000003" :completion "0.000009"
					 :input-cache-read "0.000001"
					 :input-cache-write "0.000004"))
                  (:id "remote/beta" :name "Beta Model")
                  (:id "other/gamma" :name "Gamma Model"))
                ende-selector--model-index 0
                ende-selector--model-state :ready)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (prompt collection predicate require-match
                                     initial-input history default &rest _)
                       (setq completion-call
                             (list :prompt prompt
                                   :collection collection
                                   :predicate predicate
                                   :require-match require-match
                                   :initial-input initial-input
                                   :history history
                                   :default default
                                   :extra-properties completion-extra-properties
                                   :styles completion-styles
                                   :ignore-case completion-ignore-case))
                       "remote/beta")))
            (ende-selector-select-model))
          (should (equal (plist-get completion-call :prompt) "Model: "))
          (should (equal (plist-get completion-call :collection)
                         '("remote/alpha" "remote/beta" "other/gamma")))
          (should-not (member "input" (plist-get completion-call :collection)))
          (should (plist-get completion-call :require-match))
          (should-not (plist-get completion-call :initial-input))
          (should (equal (plist-get completion-call :default)
                         "remote/alpha"))
          (let* ((properties (plist-get completion-call :extra-properties))
                 (affixation (plist-get properties :affixation-function))
                 (affixed (funcall affixation
                                   (plist-get completion-call :collection)))
                 (priced (car affixed))
                 (unpriced (cadr affixed))
                 (priced-model (car priced))
                 (priced-suffix (nth 2 priced)))
            (should (functionp affixation))
            (should-not (plist-get properties :annotation-function))
            (should (string= (substring-no-properties priced-model)
                             "remote/alpha"))
            (should (eq (get-text-property 0 'face priced-model)
                        'ende-selector-model-face))
            (should
             (string=
              (substring-no-properties priced-suffix)
              "  in: $3  out: $9  cache-read: $1  cache-write: $4"))
            (should (eq (get-text-property (string-match "in:" priced-suffix)
                                           'face priced-suffix)
                        'ende-selector-price-muted-face))
            (should (eq (get-text-property (string-match "out:" priced-suffix)
                                           'face priced-suffix)
                        'ende-selector-price-output-face))
            (should (eq (get-text-property
                         (string-match "cache-read:" priced-suffix)
                         'face priced-suffix)
                        'ende-selector-price-muted-face))
            (should (equal (mapcar #'substring-no-properties
                                   (list (car unpriced) (nth 2 unpriced)))
                           '("remote/beta" ""))))
          (should (equal (plist-get completion-call :styles) '(substring)))
          (should (plist-get completion-call :ignore-case))
          (should (= ende-selector--model-index 1))
          (should (string= (ende-selector--model-name) "remote/beta"))
          (should (equal ende-selector--last-selection
                         '(:provider remote :model "remote/beta"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ende-test/selector-preserves-remembered-model-after-discovery ()
  (let ((buffer (generate-new-buffer " *ende-selector-discovery-test*"))
        (probe-callback nil)
        (ende-selector--last-selection
         '(:provider remote :model "remote/remembered"))
        (ende-provider-profiles
         '((remote :url "http://remote.example/v1/chat/completions"
                   :model "remote/default"))))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(remote)
                ende-selector--provider-index 0)
          (cl-letf (((symbol-function 'ende-provider-probe-models)
                     (lambda (_profile callback)
                       (setq probe-callback callback)
                       (lambda () nil))))
            (ende-selector--set-provider 0)
            (should (string= (ende-selector--model-name)
                             "remote/remembered"))
            (should (string-match-p "validating"
                                    (buffer-string)))
            (funcall probe-callback
                     '("remote/default" "remote/remembered") nil)
            (should (string= (ende-selector--model-name)
                             "remote/remembered"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ende-test/selector-uses-cached-models-before-rendering ()
  (let* ((buffer (generate-new-buffer " *ende-selector-cached-models-test*"))
         (profile '(:name remote
			  :url "http://remote.example/v1/chat/completions"))
         (ende-providers '(remote))
         (ende-provider-profiles
          '((remote :url "http://remote.example/v1/chat/completions")))
         (ende-selector--last-selection
          '(:provider remote :model "remote/saved"))
         (ende-provider--model-cache (make-hash-table :test #'equal))
         (ende-provider--model-inflight (make-hash-table :test #'equal))
         (ende-provider--model-cancellations (make-hash-table :test #'equal))
         (ende-provider--model-generation 0))
    (unwind-protect
        (progn
          (ende-provider--finish-model-discovery
           (ende-provider--model-cache-key profile)
           '("remote/first" "remote/saved") nil nil)
          (with-current-buffer buffer
            (ende-selector-mode)
            (setq ende-selector--providers '(remote)
                  ende-selector--provider-index 0)
            (cl-letf (((symbol-function 'ende-provider-probe-models)
                       (lambda (&rest _)
                         (error "cached model catalog was not used"))))
              (ende-selector--set-provider 0)
              (should (string= (ende-selector--model-name)
                               "remote/saved"))
              (should (eq ende-selector--model-state :ready))
              (setq ende-selector--last-selection nil)
              (ende-selector--set-provider 0)
              (should (string= (ende-selector--model-name)
                               "remote/first")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ende-test/selector-validates-first-model-without-preference ()
  (let ((buffer (generate-new-buffer " *ende-selector-first-model-test*"))
        (probe-callback nil)
        (ende-providers '(remote))
        (ende-provider-profiles
         '((remote :url "http://remote.example/v1/chat/completions")))
        (ende-selector--last-selection nil))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(remote)
                ende-selector--provider-index 0)
          (cl-letf (((symbol-function 'ende-provider-probe-models)
                     (lambda (_profile callback)
                       (setq probe-callback callback)
                       (lambda () nil))))
            (ende-selector--set-provider 0)
            (should-not (ende-selector--model-name))
            (should (string-match-p "validating" (buffer-string)))
            (funcall probe-callback '("remote/first" "remote/second") nil)
            (should (string= (ende-selector--model-name)
                             "remote/first"))
            (should (eq ende-selector--model-state :ready))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ende-test/selector-validates-preferences-once-per-catalog ()
  (let ((buffer (generate-new-buffer " *ende-selector-validation-test*"))
        (messages nil)
        (ende-providers '(remote))
        (ende-provider-profiles
         '((remote :url "http://remote.example/v1/chat/completions")))
        (ende-selector--last-selection
         '(:provider remote :model "remote/missing"))
        (ende-selector--model-validations (make-hash-table :test #'equal))
        (ende-provider--model-cache (make-hash-table :test #'equal))
        (ende-provider--model-inflight (make-hash-table :test #'equal))
        (ende-provider--model-cancellations (make-hash-table :test #'equal))
        (ende-provider--model-generation 0))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(remote)
                ende-selector--provider-index 0)
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (push (apply #'format format-string args) messages)))
                    ((symbol-function 'ende-provider-probe-models)
                     (lambda (_profile callback)
                       (funcall callback '("remote/first" "remote/second") nil)
                       (lambda () nil))))
            (ende-selector--set-provider 0)
            (should (string= (ende-selector--model-name) "remote/first"))
            (should (= (cl-count-if
                        (lambda (value)
                          (string-match-p "unavailable" value))
                        messages)
                       1))
            (ende-selector--set-provider 0)
            (should (= (cl-count-if
                        (lambda (value)
                          (string-match-p "unavailable" value))
                        messages)
                       1))
            (setq ende-selector--last-selection
                  '(:provider remote :model "remote/second"))
            (ende-selector--set-provider 0)
            (should (string= (ende-selector--model-name)
                             "remote/second"))
            (should (= (cl-count-if
                        (lambda (value)
                          (string-match-p "unavailable" value))
                        messages)
                       1))
            (ende-provider-clear-discovery-cache)
            (setq ende-selector--last-selection
                  '(:provider remote :model "remote/missing"))
            (ende-selector--set-provider 0)
            (should (= (cl-count-if
                        (lambda (value)
                          (string-match-p "unavailable" value))
                        messages)
                       2))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ende-test/selector-model-completion-rejects-incomplete-discovery ()
  (let ((buffer (generate-new-buffer " *ende-selector-model-loading-test*"))
        (called nil))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(remote)
                ende-selector--provider-index 0
                ende-selector--models '("remote/default")
                ende-selector--model-index 0
                ende-selector--model-state :loading)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _)
                       (setq called t)
                       "remote/default")))
            (should-error (ende-selector-select-model) :type 'user-error))
          (should-not called))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ende-test/selector-effort-picker-uses-live-values-and-remembers-choice ()
  (let ((buffer (generate-new-buffer " *ende-selector-effort-picker-test*"))
        (calls nil)
        (ende-provider-profiles '((remote :model "remote-model")))
        (ende-model-selection-scope 'global)
        (ende-selector--effort-selections (make-hash-table :test #'equal)))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(remote)
                ende-selector--provider-index 0
                ende-selector--models '("remote-model")
                ende-selector--model-index 0
                ende-selector--model-state :ready)
          (cl-letf (((symbol-function 'ende-provider-probe-reasoning-efforts)
                     (lambda (profile model callback)
                       (push (list profile model) calls)
                       (funcall callback '("high" "low") "low" nil)
                       (lambda () nil)))
                    ((symbol-function 'completing-read)
                     (lambda (prompt collection _predicate require-match
                                     _initial _history default &rest _)
                       (should (string= prompt "Reasoning effort: "))
                       (should (equal collection '("high" "low")))
                       (should require-match)
                       (should (string= default "low"))
                       "high")))
            (ende-selector-select-effort))
          (should (= (length calls) 1))
          (should (string= (cadar calls) "remote-model"))
          (should (string= ende-selector--effort "high"))
          (should (eq ende-selector--effort-state :ready))
          (should ende-selector--effort-validated-p)
          (should (string=
                   (gethash '(remote "remote-model")
                            ende-selector--effort-selections)
                   "high"))
          (should (string-match-p "Effort:.*high" (buffer-string))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ende-test/selector-effort-is-saved-only-for-its-source-buffer ()
  (let* ((source (generate-new-buffer " *ende-effort-source-one*"))
         (other-source (generate-new-buffer " *ende-effort-source-two*"))
         (selector (generate-new-buffer " *ende-effort-source-selector*"))
         (ende-model-selection-scope 'buffer)
         (ende-model-selection--last-used nil)
         (ende-model-selection--buffer-selections
          (make-hash-table :test #'equal))
         (ende-provider-profiles '((remote :model "remote-model"))))
    (unwind-protect
        (progn
          (with-current-buffer source
            (ende-model-selection-save
             '(:provider remote :model "remote-model")))
          (with-current-buffer other-source
            (ende-model-selection-save
             '(:provider remote :model "remote-model")))
          (with-current-buffer selector
            (ende-selector-mode)
            (setq ende-selector--source-buffer source
                  ende-selector--providers '(remote)
                  ende-selector--provider-index 0
                  ende-selector--models '("remote-model")
                  ende-selector--model-index 0
                  ende-selector--model-state :ready)
            (cl-letf (((symbol-function 'ende-provider-probe-reasoning-efforts)
                       (lambda (_profile _model callback)
                         (funcall callback '("high" "low") "low" nil)
                         (lambda () nil)))
                      ((symbol-function 'completing-read)
                       (lambda (&rest _) "high")))
              (ende-selector-select-effort))
            (should (equal
                     (plist-get (ende-model-selection-for-buffer source)
                                :reasoning-effort)
                     "high"))
            (should-not
             (plist-get (ende-model-selection-for-buffer other-source)
                        :reasoning-effort))
            (setq ende-selector--source-buffer other-source)
            (ende-selector--set-effort-context)
            (should-not ende-selector--effort)))
      (dolist (buffer (list source other-source selector))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest ende-test/selector-effort-probes-cancel-on-model-change ()
  (let ((buffer (generate-new-buffer " *ende-selector-effort-cancel-test*"))
        (callback nil)
        (cancelled 0)
        (completed nil)
        (ende-provider-profiles '((remote :model "first")))
        (ende-model-selection-scope 'global)
        (ende-selector--effort-selections (make-hash-table :test #'equal)))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(remote)
                ende-selector--provider-index 0
                ende-selector--models '("first" "second")
                ende-selector--model-index 0
                ende-selector--model-state :ready)
          (cl-letf (((symbol-function 'ende-provider-probe-reasoning-efforts)
                     (lambda (_profile _model probe-callback)
                       (setq callback probe-callback)
                       (lambda () (cl-incf cancelled))))
                    ((symbol-function 'completing-read)
                     (lambda (&rest _)
                       (setq completed t)
                       "high")))
            (ende-selector-select-effort)
            (should (eq ende-selector--effort-state :loading))
            (ende-selector-next-model)
            (should (= cancelled 1))
            (should (string= (ende-selector--model-name) "second"))
            (funcall callback '("high") "high" nil)
            (should-not completed)
            (should-not ende-selector--effort)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ende-test/selector-validates-remembered-effort-before-dispatch ()
  (let ((source (generate-new-buffer " *ende-selector-effort-source*"))
        (selector (generate-new-buffer " *ende-selector-effort-dispatch*"))
        (probe-callback nil)
        (queued nil)
        (ende-provider-profiles '((remote :model "remote-model")))
        (ende-model-selection-scope 'global)
        (ende-selector--effort-selections (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (puthash '(remote "remote-model") "high"
                   ende-selector--effort-selections)
          (with-current-buffer selector
            (ende-selector-mode)
            (setq ende-selector--source-buffer source
                  ende-selector--providers '(remote)
                  ende-selector--provider-index 0
                  ende-selector--models '("remote-model")
                  ende-selector--model-index 0
                  ende-selector--model-state :ready)
            (ende-selector--set-effort-context)
            (cl-letf (((symbol-function 'ende-provider-probe-reasoning-efforts)
                       (lambda (_profile _model callback)
                         (setq probe-callback callback)
                         (lambda () nil)))
                      ((symbol-function 'ende-enqueue-operation)
                       (lambda (type instruction profile)
                         (setq queued (list type instruction profile)))))
              (ende-selector-insert)
              (should (eq ende-selector--effort-state :loading))
              (should-not queued)
              (funcall probe-callback '("high" "low") "low" nil)))
          (should (equal (take 2 queued) '(insert nil)))
          (should (string= (plist-get (nth 2 queued) :model) "remote-model"))
          (should (string= (plist-get (nth 2 queued) :reasoning-effort) "high")))
      (dolist (buffer (list source selector))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest ende-test/selector-rejects-effort-removed-by-live-validation ()
  (let ((source (generate-new-buffer " *ende-selector-effort-stale-source*"))
        (selector (generate-new-buffer " *ende-selector-effort-stale*"))
        (queued nil)
        (ende-provider-profiles '((remote :model "remote-model")))
        (ende-model-selection-scope 'global)
        (ende-selector--effort-selections (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (puthash '(remote "remote-model") "high"
                   ende-selector--effort-selections)
          (with-current-buffer selector
            (ende-selector-mode)
            (setq ende-selector--source-buffer source
                  ende-selector--providers '(remote)
                  ende-selector--provider-index 0
                  ende-selector--models '("remote-model")
                  ende-selector--model-index 0
                  ende-selector--model-state :ready)
            (ende-selector--set-effort-context)
            (cl-letf (((symbol-function 'ende-provider-probe-reasoning-efforts)
                       (lambda (_profile _model callback)
                         (funcall callback '("low") "low" nil)
                         (lambda () nil)))
                      ((symbol-function 'ende-enqueue-operation)
                       (lambda (&rest _)
                         (setq queued t))))
              (ende-selector-insert))
            (should-not queued)
            (should-not ende-selector--effort)
            (should (eq ende-selector--effort-state :error))
            (should-not (gethash '(remote "remote-model")
                                 ende-selector--effort-selections))))
      (dolist (buffer (list source selector))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest ende-test/selector-uses-bottom-window-and-quits-it-on-close ()
  (let ((source (generate-new-buffer " *ende-selector-window-source*"))
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
                    ((symbol-function 'ende-provider-probe-models)
                     (lambda (profile callback)
                       (setq probe-profile profile)
                       (funcall callback '("local-model") nil)
                       (lambda () nil))))
            (let ((ende-providers '(local))
                  (ende-provider-profiles
                   '((local :model "local-model"))))
              (ende-selector-open)
              (setq selector (get-buffer ende-selector-buffer-name))
              (should (eq (plist-get probe-profile :name) 'local))
              (should (eq (car display-action) selector))
              (should (eq (car (car (cadr display-action)))
                          #'display-buffer-at-bottom))
              (should (eq (cdr (assq 'window-height
                                     (cdr (cadr display-action))))
                          #'ende-selector--fit-window))
              (should (equal (cdr (assq 'window-min-height
                                        (cdr (cadr display-action))))
                             ende-selector-window-min-height))
              (with-current-buffer selector
                (ende-selector-close))
              (should (equal (car quit-window-call) t))
              (should (eq (cadr quit-window-call) (selected-window)))
              (should-not (buffer-live-p selector)))))
      (when (buffer-live-p selector)
        (kill-buffer selector))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(ert-deftest ende-test/selector-changes-provider-runtime-settings ()
  (ende-provider-clear-runtime-settings)
  (let ((ende-providers '(remote))
        (ende-provider-profiles
         '((remote :model "remote-model" :max-concurrency 2
                   :parallel nil))))
    (with-temp-buffer
      (ende-selector-mode)
      (setq ende-selector--providers '(remote)
            ende-selector--provider-index 0
            ende-selector--models '("remote-model")
            ende-selector--model-index 0)
      (cl-letf (((symbol-function 'read-number)
                 (lambda (&rest _) 4)))
        (ende-selector-set-concurrency))
      (should (= (ende-provider-max-concurrency
                  (ende-provider-profile 'remote))
                 4))
      (should-not (ende-selector--provider-parallel-p))
      (ende-selector-toggle-parallel)
      (should (ende-selector--provider-parallel-p))
      (ende-selector--render)
      (dolist (key '("Provider:" "Model:" "Concurrency:" "Parallel:" "Effort:"
                     "Quota: unavailable"
                     "i  - insert" "a  - ask" "I  - insert anywhere"
                     "k  - compact chat context"
                     "w  - write anywhere"
                     "m  - select/search model"
                     "e  - select reasoning effort"
                     "s  - summarize"
                     "r  - rubber duck"
                     "RET       - confirm and save selection"
                     "C-c C-c   - approve all LLM changes"
                     "C-c C-k   - reject all LLM changes"))
        (should (string-match-p (regexp-quote key) (buffer-string)))))))

(ert-deftest ende-test/selector-runtime-keybindings-survive-reload ()
  (let ((original-c (lookup-key ende-selector-mode-map (kbd "c")))
        (original-C (lookup-key ende-selector-mode-map (kbd "C")))
        (original-P (lookup-key ende-selector-mode-map (kbd "P")))
        (original-e (lookup-key ende-selector-mode-map (kbd "e")))
        (original-a (lookup-key ende-selector-mode-map (kbd "a")))
        (original-k (lookup-key ende-selector-mode-map (kbd "k"))))
    (unwind-protect
        (progn
          ;; Simulate a keymap retained from before the new bindings existed.
          (define-key ende-selector-mode-map (kbd "c") nil)
          (define-key ende-selector-mode-map (kbd "C") nil)
          (define-key ende-selector-mode-map (kbd "P") nil)
          (define-key ende-selector-mode-map (kbd "e") nil)
          (define-key ende-selector-mode-map (kbd "a") nil)
          (define-key ende-selector-mode-map (kbd "k") nil)
          (with-temp-buffer
            (ende-selector-mode)
            (should (eq (key-binding (kbd "c"))
                        #'ende-selector-set-concurrency))
            (should (eq (key-binding (kbd "C"))
                        #'ende-selector-set-concurrency))
            (should (eq (key-binding (kbd "P"))
                        #'ende-selector-toggle-parallel))
            (should (eq (key-binding (kbd "n"))
                        #'ende-selector-next-provider))
            (should (eq (key-binding (kbd "p"))
                        #'ende-selector-previous-provider))
            (should (eq (key-binding (kbd "m"))
                        #'ende-selector-select-model))
            (should (eq (key-binding (kbd "e"))
                        #'ende-selector-select-effort))
            (should (eq (key-binding (kbd "a"))
                        #'ende-selector-ask))
            (should (eq (key-binding (kbd "k"))
                        #'ende-selector-compact))
            (should (eq (key-binding (kbd "RET"))
                        #'ende-selector-confirm))
            (should (eq (key-binding (kbd "C-c C-c"))
                        #'ende-selector-approve-source-output))
            (should (eq (key-binding (kbd "C-c C-k"))
                        #'ende-selector-reject-source-output))
            (should-not (eq (key-binding (kbd "DEL"))
                            #'ende-selector-reject-source-output))
            (should-not (eq (key-binding (kbd "<backspace>"))
                            #'ende-selector-reject-source-output))
            (should (eq (key-binding (kbd "w"))
                        #'ende-selector-write-anywhere))
            (should (eq (key-binding (kbd "I"))
                        #'ende-selector-insert-anywhere))))
      (define-key ende-selector-mode-map (kbd "c") original-c)
      (define-key ende-selector-mode-map (kbd "C") original-C)
      (define-key ende-selector-mode-map (kbd "P") original-P)
      (define-key ende-selector-mode-map (kbd "e") original-e)
      (define-key ende-selector-mode-map (kbd "a") original-a)
      (define-key ende-selector-mode-map (kbd "k") original-k))))

(ert-deftest ende-test/selector-labels-are-single-line-and-display-bounded ()
  (let ((multiline (ende-selector--display-label "provider\nspoof" 24))
        (long (ende-selector--display-label
               "provider-name-that-is-longer-than-the-row" 24)))
    (should (string-match-p "provider spoof" multiline))
    (should-not (string-match-p "\n" multiline))
    (should (= (string-width multiline) 24))
    (should (string-suffix-p "..." (string-trim-right long)))
    (should (= (string-width long) 24))))

(ert-deftest ende-test/selector-probe-callbacks-are-generation-bound ()
  (let ((buffer (generate-new-buffer " *ende-selector-probe-test*"))
        (callbacks nil)
        (cancelled 0)
        (ende-model-selection-scope 'buffer)
        (ende-model-selection--last-used nil)
        (ende-model-selection--buffer-selections
         (make-hash-table :test #'equal))
        (ende-provider-profiles
         '((remote :url "http://remote.example/v1"
                   :model "configured"))))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(remote)
                ende-selector--provider-index 0
                ende-selector--models '("configured")
                ende-selector--model-index 0)
          (cl-letf (((symbol-function 'ende-provider-probe-models)
                     (lambda (_profile callback)
                       (setq callbacks (append callbacks (list callback)))
                       (lambda () (setq cancelled (1+ cancelled))))))
            (ende-selector--set-provider 0)
            (let ((stale (car callbacks)))
              (ende-selector--cancel-probe)
              (ende-selector--set-provider 0)
              (funcall stale '("stale") nil)
              (should (equal ende-selector--models '("configured")))
              (let ((current (car (last callbacks))))
                (kill-buffer buffer)
                (should (= cancelled 2))
                (funcall current '("after-kill") nil)
                (should-not (buffer-live-p buffer))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ende-test/openai-request-body-matches-codex-responses-shape ()
  (let* ((body (ende-protocol-json-read-string
                (json-encode
                 (ende-codex--request-body
                  (list (cons "system" "rules")
                        (cons "user" "question")
                        (cons "assistant" "answer"))
                  '(:model "gpt-test")))))
         (input (ende-protocol-json-get "input" body))
         (user (aref input 1))
         (user-content (aref (ende-protocol-json-get "content" user) 0))
         (assistant (aref input 2))
         (assistant-content
          (aref (ende-protocol-json-get "content" assistant) 0)))
    (should (eq (ende-protocol-json-get "store" body) :false))
    (should (eq (ende-protocol-json-get "stream" body) t))
    (should (string= (ende-protocol-json-get "text" user-content)
                     "question"))
    (should (string= (ende-protocol-json-get "type" user-content)
                     "input_text"))
    (should (string= (ende-protocol-json-get "text" assistant-content)
                     "answer"))
    (should (string= (ende-protocol-json-get "type" assistant-content)
                     "output_text"))))

(ert-deftest ende-test/openai-http-error-keeps-provider-detail ()
  (let ((headers-file (make-temp-file "ende-openai-error-")))
    (unwind-protect
        (progn
          (with-temp-file headers-file
            (insert "HTTP/1.1 400 Bad Request\r\n\r\n"))
          (let* ((session
                  (ende-codex--make-session
                   :headers-file headers-file
                   :preview-parts '("{\"detail\":\"invalid request body\"}")))
                 (error (ende-codex--http-error session)))
            (should (string= (ende-error-message error)
                             "HTTP status 400: invalid request body"))))
      (when (file-exists-p headers-file)
        (delete-file headers-file)))))

(ert-deftest ende-test/openai-auth-entry-is-read-from-opencode-file ()
  (let* ((file (make-temp-file "ende-openai-auth-test-"))
         (ende-codex-auth-file file))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "{\"openai\":{\"type\":\"oauth\","
                    "\"access\":\"access-token\","
                    "\"refresh\":\"refresh-token\","
                    "\"expires\":9999999999999,"
                    "\"accountId\":\"account-id\"}}"))
          (let ((auth (ende-codex--auth-entry)))
            (should (string= (plist-get auth :access) "access-token"))
            (should (string= (plist-get auth :refresh) "refresh-token"))
            (should (string= (plist-get auth :account-id) "account-id"))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest ende-test/openai-transport-keeps-token-out-of-command-arguments ()
  (let* ((auth-file (make-temp-file "ende-openai-auth-test-"))
         (ende-codex-auth-file auth-file)
         (captured nil)
         (process nil)
         (result nil)
         (failure nil)
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
            (ende-codex-default
             (list (cons "user" "body"))
             (list :on-done (lambda (value) (setq result value))
                   :on-error (lambda (value) (setq failure value)))
             (ende-provider-profile 'openai))
            (let* ((command (plist-get captured :command))
                   (config (cadr (member "--config" command)))
                   (headers-file (cadr (member "--dump-header" command)))
                   (filter (plist-get captured :filter))
                   (sentinel (plist-get captured :sentinel)))
              (should (not (member "access-token" command)))
              (should (member "Accept: text/event-stream" command))
              (should (file-exists-p config))
              (with-temp-file headers-file
                (insert "HTTP/1.1 200 OK\r\n\r\n"))
              (with-temp-buffer
                (insert-file-contents config)
                (should (string-match-p "Authorization: Bearer access-token"
                                        (buffer-string))))
              (should (equal (cadr (member "--user-agent" command))
                             ende-client-name))
              (funcall
               filter
               process
               (concat "data: {\"type\":\"response.output_text.delta\","
                       "\"delta\":\"answer\"}\n\n"
                       "data: {\"type\":\"response.completed\","
                       "\"response\":{\"model\":\"gpt-test\","
                       "\"usage\":{\"output_tokens\":7}}}"))
              (while (process-live-p process)
                (accept-process-output process 0.05))
              (funcall sentinel process "finished\n")
              (should-not failure)
              (should (string= (plist-get result :content) "answer"))
              (should (string= (plist-get result :model) "gpt-test"))
              (should (= (plist-get result :tokens) 7))
              (should (string= (plist-get result :finish-reason) "stop"))
              (should-not (file-exists-p config)))))
      (when (and (processp process) (process-live-p process))
        (delete-process process))
      (when (file-exists-p auth-file)
        (delete-file auth-file)))))

(ert-deftest ende-test/openai-rejects-explicit-non-sse-content-type ()
  (let ((headers-file (make-temp-file "ende-openai-headers-"))
        (process (make-process :name "ende-openai-test"
                               :command (list "true") :noquery t))
        (result nil)
        (failure nil)
        (events nil))
    (unwind-protect
        (let ((session
               (ende-codex--make-session
                :profile '(:model "gpt-test")
                :process process
                :headers-file headers-file
                :callbacks
                (list :on-event (lambda (event) (push event events))
                      :on-done (lambda (value) (setq result value))
                      :on-error (lambda (error) (setq failure error)))
                :sse (ende-protocol-make-sse)
                :estimated-chars 0
                :received-bytes 0
                :preview-bytes 0)))
          (with-temp-file headers-file
            (insert "HTTP/2 200\r\nContent-Type: application/json\r\n\r\n"))
          (ende-codex--filter
           session process
           (concat "data: {\"type\":\"response.output_text.delta\","
                   "\"delta\":\"answer\"}\n\n"
                   "data: {\"type\":\"response.completed\","
                   "\"response\":{\"model\":\"gpt-test\"}}\n\n"
                   "data: [DONE]\n\n"))
          (while (process-live-p process)
            (accept-process-output process 0.05))
          (ende-codex--sentinel session process "finished\n")
          (should-not result)
          (should (eq (ende-error-kind failure) 'protocol))
          (should (string-match-p "application/json"
                                  (ende-error-message failure)))
          (let ((partial
                 (cl-find-if
                  (lambda (event)
                    (eq (plist-get event :type) :partial-result))
                  events)))
            (should (string= (plist-get (plist-get partial :result) :content)
                             "answer"))))
      (when (process-live-p process)
        (delete-process process))
      (when (file-exists-p headers-file)
        (delete-file headers-file)))))

(ert-deftest ende-test/openai-missing-content-type-requires-terminal-event ()
  (let ((headers-file (make-temp-file "ende-openai-headers-"))
        (process (make-process :name "ende-openai-incomplete-test"
                               :command (list "true") :noquery t))
        (failure nil))
    (unwind-protect
        (let ((session
               (ende-codex--make-session
                :profile '(:model "gpt-test")
                :process process
                :headers-file headers-file
                :callbacks (list :on-error
                                 (lambda (error) (setq failure error)))
                :sse (ende-protocol-make-sse)
                :estimated-chars 0
                :received-bytes 0
                :preview-bytes 0)))
          (with-temp-file headers-file
            (insert "HTTP/2 200\r\n\r\n"))
          (ende-codex--filter session process "data: [DONE]\n\n")
          (while (process-live-p process)
            (accept-process-output process 0.05))
          (ende-codex--sentinel session process "finished\n")
          (should (eq (ende-error-kind failure) 'protocol))
          (should (string-match-p "without a terminal response event"
                                  (ende-error-message failure))))
      (when (process-live-p process)
        (delete-process process))
      (when (file-exists-p headers-file)
        (delete-file headers-file)))))

(ert-deftest ende-test/openai-refresh-rotates-opencode-credentials-safely ()
  (let* ((auth-file (make-temp-file "ende-openai-auth-test-"))
         (ende-codex-auth-file auth-file)
         (ende-codex-issuer "https://auth.openai.com:443")
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
            (ende-codex--refresh-auth auth
                                      (lambda (value) (setq refreshed value)))
            (ende-codex--refresh-auth
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
              (should (string= (plist-get (ende-codex--auth-entry) :refresh)
                               "new-refresh")))))
      (when (and (processp process) (process-live-p process))
        (delete-process process))
      (when (file-exists-p auth-file)
        (delete-file auth-file)))))

(ert-deftest ende-test/openai-refresh-startup-failure-does-not-wedge-flight ()
  (let* ((key '(ende-test-refresh))
         (result nil)
         (flight
          (ende-codex--make-refresh-flight
           :key key
           :auth '(:refresh "refresh-token")
           :waiters
           (list
            (ende-codex--make-refresh-waiter
             :callback (lambda (value) (setq result value))))))
         (ende-codex--refresh-flights (make-hash-table :test #'equal))
         (ende-codex--refresh-processes nil))
    (puthash key flight ende-codex--refresh-flights)
    (cl-letf (((symbol-function 'make-temp-file)
               (lambda (&rest _)
                 (error "temporary file creation failed"))))
      (ende-codex--start-refresh-flight flight))
    (should (ende-codex--refresh-flight-finished flight))
    (should (ende-error-p result))
    (should-not (gethash key ende-codex--refresh-flights))
    (should-not ende-codex--refresh-processes)
    (should-not (buffer-live-p
                 (ende-codex--refresh-flight-response-buffer flight)))
    (should-not (buffer-live-p
                 (ende-codex--refresh-flight-stderr-buffer flight)))))

(ert-deftest ende-test/openai-credentials-use-only-official-endpoints ()
  (should (string= (ende-codex--validated-generation-url
                    "https://chatgpt.com:443/backend-api/codex/responses")
                   "https://chatgpt.com:443/backend-api/codex/responses"))
  (should (string= (ende-codex--validated-usage-url)
                   "https://chatgpt.com/backend-api/wham/usage"))
  (should-error
   (ende-codex--validated-generation-url
    "https://chatgpt.com.attacker.test/backend-api/codex/responses"))
  (should-error
   (ende-codex--validated-generation-url
    "https://chatgpt.com/backend-api/codex/responses?redirect=attacker"))
  (let ((ende-codex-usage-endpoint
         "https://chatgpt.com/backend-api/wham/usage?redirect=attacker"))
    (should-error (ende-codex--validated-usage-url)))
  (let ((ende-codex-issuer "https://auth.openai.com.attacker.test"))
    (should-error (ende-codex--validated-refresh-url))))

(ert-deftest ende-test/openai-request-body-applies-output-token-limit ()
  (let ((ende-max-output-tokens 123))
    (let ((body (ende-codex--request-body
                 (list (cons "user" "body"))
                 '(:model "test-model"))))
      (should (= (alist-get :max_output_tokens body) 123)))))

(ert-deftest ende-test/curl-stderr-is-byte-bounded ()
  (let ((buffer (generate-new-buffer " *ende-test-stderr*"))
        (process (make-pipe-process :name "ende-test-stderr-process"
                                    :buffer nil :noquery t))
        (ende-max-stderr-bytes 4))
    (unwind-protect
        (progn
          (ende-curl-stderr-filter buffer process "12345")
          (ende-curl-stderr-filter buffer process "6")
          (with-current-buffer buffer
            (should (string= (buffer-string) "1234"))))
      (delete-process process)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ende-test/openai-credential-replacement-is-side-by-side ()
  (let* ((directory (make-temp-file "ende-openai-auth-dir-" t))
         (file (expand-file-name "auth.json" directory))
         (ende-codex-auth-file file)
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
          (ende-codex--store-auth new (ende-codex--auth-version old))
          (should (string= (plist-get (ende-codex--auth-entry) :access)
                           "new-access"))
          (should (= (logand (file-modes file) #o777) #o600))
          (should-not
           (directory-files directory nil "\\`\\.ende-openai-auth-" t))
          (should-error
           (ende-codex--store-auth
            old (ende-codex--auth-version
                 '(:access "other" :refresh "other" :expires 0
			   :account-id "account-id")))))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest ende-test/openai-responses-events-normalize-to-ende-result ()
  (let* ((events nil)
         (result nil)
         (failure nil)
         (session
          (ende-codex--make-session
           :profile '(:model "gpt-test")
           :callbacks (list :on-event (lambda (event) (push event events))
                            :on-done (lambda (value) (setq result value))
                            :on-error (lambda (value) (setq failure value)))
           :sse (ende-protocol-make-sse)
           :estimated-chars 0)))
    (ende-codex--handle-event
     session
     (ende-protocol-json-read-string
      "{\"type\":\"response.created\",\"response\":{\"model\":\"gpt-actual\"}}"))
    (ende-codex--handle-event
     session
     (ende-protocol-json-read-string
      "{\"type\":\"response.reasoning_summary_text.delta\",\"delta\":\"thinking\"}"))
    (ende-codex--handle-event
     session
     (ende-protocol-json-read-string
      "{\"type\":\"response.output_text.delta\",\"delta\":\"answer\"}"))
    (ende-codex--handle-event
     session
     (ende-protocol-json-read-string
      "{\"type\":\"response.completed\",\"response\":{\"model\":\"gpt-actual\",\"usage\":{\"output_tokens\":7}}}"))
    (setq result (ende-codex--result session))
    (should-not failure)
    (should (string= (plist-get result :content) "answer"))
    (should (string= (plist-get result :thinking) "thinking"))
    (should (string= (plist-get result :model) "gpt-actual"))
    (should (= (plist-get result :tokens) 7))
    (should (ende-codex--session-terminal-event session))
    (should-not (ende-codex--session-terminal session))))

;;;; Profiles, history, and transport command safety

(ert-deftest ende-test/profile-is-snapshotted-at-enqueue ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(:stall) t))
        (ende-provider-endpoint "http://one.example/v1"))
    (ende-test--with-buffer "base"
			    (goto-char (point-max))
			    (let ((job (ende-insert-at-cursor)))
			      (setq ende-provider-endpoint "http://two.example/v1")
			      (should (string= (plist-get (ende-job-profile job) :url)
					       "http://one.example/v1"))))))

(ert-deftest ende-test/auth-and-body-are-not-command-arguments ()
  (let* ((token "secret-token")
         (profile (list :name 'remote
                        :url "https://example.test/v1"
                        :model "model"
                        :temperature 0.2
                        :auth-source-host "example.test"
                        :auth-source-user "ende"
                        :auth-source-port 443))
         (config nil))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _) (list (list :secret token)))))
      (setq config (ende-curl--config profile))
      (unwind-protect
          (let ((command (ende-curl-command
                          profile "/tmp/headers" config)))
            (should-not (member token command))
            (should-not (member "body" command))
            (should-not (member "--fail-with-body" command)))
        (when (file-exists-p config) (delete-file config))))))

(ert-deftest ende-test/unbounded-output-omits-provider-token-limit ()
  (let ((ende-max-output-tokens nil))
    (let ((body (ende-llm--request-body
                 (list (cons "user" "body"))
                 '(:model "test-model" :temperature 0.2))))
      (should-not (alist-get :max_tokens body))
      (should-not (string-match-p "max_tokens" (json-encode body))))))

(ert-deftest ende-test/curl-config-escapes-and-rejects-unsafe-headers ()
  (let* ((profile '(:url "https://example.test/v1"
			 :request-headers ("X-Title: a\"b\\c")))
         (config (ende-curl--config profile)))
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
                    "Accept: application/json"
                    "Content-Length: 0"))
    (should-error
     (ende-curl--config
      (list :url "https://example.test/v1"
            :request-headers (list header))))))

(ert-deftest ende-test/curl-response-headers-select-final-response ()
  (let ((file (make-temp-file "ende-response-headers-")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "HTTP/1.1 200 Connection established\r\n\r\n"
                    "HTTP/2 103 Early Hints\r\nLink: </model>\r\n\r\n"
                    "HTTP/2 200\r\n"
                    "CoNtEnT-TyPe: text/event-stream; charset=utf-8\r\n\r\n"))
          (should (equal (ende-curl-response-headers file)
                         '(200 . "text/event-stream; charset=utf-8")))
          (should (ende-curl-event-stream-p
                   "text/event-stream; charset=utf-8"))
          (should-not (ende-curl-event-stream-p
                       "application/json; note=text/event-stream")))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest ende-test/provider-url-and-curl-command-are-hardened ()
  (should-error
   (ende-curl--validated-url '(:url "http://127.attacker.test/v1")))
  (should-error
   (ende-curl--validated-url '(:url "https://example.test/v1?key=secret")))
  (should-error
   (ende-curl--validated-url
    '(:url "https://other.test/v1"
	   :auth-source-host "example.test"
	   :auth-source-user "ende"
	   :auth-source-port 443)))
  (let ((command
         (ende-curl-command
          '(:url "https://example.test/v1") "/tmp/headers" nil)))
    (should (string= (nth 1 command) "--disable"))
    (should (member "--globoff" command))
    (should (member "--noproxy" command))
    (should (equal (cadr (member "--user-agent" command))
                   ende-client-name))
    (should (member "--url" command))))

(ert-deftest ende-test/curl-command-leaves-idle-timeout-to-elisp ()
  (let ((ende-request-timeout nil)
        (ende-response-idle-timeout 60))
    (let ((command (ende-curl-command
                    '(:url "https://example.test/v1") "/tmp/headers" nil)))
      (should-not (member "--max-time" command))
      (should-not (member "--speed-time" command))
      (should-not (member "--speed-limit" command)))))

(defun ende-test--with-idle-request (timeout function &optional command)
  "Call FUNCTION with a request guarded by TIMEOUT, then clean up.
Use COMMAND for the process, or a silent sleep process by default."
  (let* ((ende-response-idle-timeout timeout)
         (request (ende-curl-start-request
                   :profile '(:url "https://example.test/v1")
                   :name "ende-test-idle"
                   :command (lambda (_headers _config)
                              (or command '("sleep" "30")))
                   :filter #'ignore :sentinel #'ignore))
         (process (plist-get request :process)))
    (unwind-protect
        (funcall function process)
      (ende-curl-cleanup-resources
       (list (plist-get request :response-buffer)
             (plist-get request :stderr-buffer))
       (list (plist-get request :headers-file)
             (plist-get request :config-file))
       (list process (plist-get request :stderr-process))))))

(ert-deftest ende-test/curl-idle-heartbeat-and-captured-setting ()
  (ende-test--with-idle-request
   180
   (lambda (process)
     (let ((ende-response-idle-timeout 999)
           (parser (ende-protocol-make-sse)))
       ;; A comment yields no SSE event, but is still response activity.
       (should-not (ende-protocol-sse-feed parser ": heartbeat\n\n"))
       (process-put process :ende-last-response-time 0)
       (cl-letf (((symbol-function 'float-time) (lambda (&optional _) 179)))
         (funcall (process-filter process) process ": heartbeat\n\n"))
       ;; The original deadline has passed; the heartbeat extended it.
       (cl-letf (((symbol-function 'float-time) (lambda (&optional _) 181)))
         (ende-curl--check-idle process))
       (should (process-live-p process))
       (should (timerp (process-get process :ende-idle-timer)))
       ;; Empty chunks must not extend the deadline.
       (cl-letf (((symbol-function 'float-time) (lambda (&optional _) 358)))
         (funcall (process-filter process) process ""))
       (cl-letf (((symbol-function 'float-time) (lambda (&optional _) 359)))
         (ende-curl--check-idle process))
       (should-not (process-live-p process))
       (should-not (process-get process :ende-idle-timer))
       (let ((failure (process-get process :ende-idle-error)))
         (should (eq (ende-error-kind failure) 'timeout))
         (should (equal (ende-error-message failure)
                        "No response data received for 180 seconds (`ende-response-idle-timeout`).")))))))

(ert-deftest ende-test/curl-idle-timer-fires-before-first-response ()
  (ende-test--with-idle-request
   0.05
   (lambda (process)
     (let ((deadline (+ (float-time) 2)))
       (while (and (process-live-p process) (< (float-time) deadline))
         (accept-process-output nil 0.02)))
     (should (process-get process :ende-idle-error))
     (should-not (process-live-p process))
     (should-not (process-get process :ende-idle-timer)))))

(ert-deftest ende-test/curl-idle-cancellation-releases-timer ()
  (ende-test--with-idle-request
   180
   (lambda (process)
     (let ((timer (process-get process :ende-idle-timer)))
       (delete-process process)
       (should-not (process-get process :ende-idle-timer))
       (should-not (memq timer timer-list))
       (ende-curl--check-idle process)
       (should-not (process-get process :ende-idle-error))))))

(ert-deftest ende-test/curl-idle-error-reaches-both-transports-once ()
  (dolist (transport '(llm codex))
    (ende-test--with-idle-request
     180
     (lambda (process)
       (let* ((errors nil)
              (callbacks (list :on-error (lambda (error) (push error errors))))
              (session (if (eq transport 'llm)
                           (ende-llm--make-session :callbacks callbacks)
                         (ende-codex--make-session :callbacks callbacks)))
              (sentinel (if (eq transport 'llm)
                            #'ende-llm--process-sentinel
                          #'ende-codex--sentinel)))
         (process-put process :ende-last-response-time 0)
         (ende-curl--check-idle process)
         (funcall sentinel session process "killed\n")
         (funcall sentinel session process "killed\n")
         (should (= (length errors) 1))
         (should (eq (car errors) (process-get process :ende-idle-error))))))))

(ert-deftest ende-test/curl-idle-normal-exit-releases-timer ()
  (ende-test--with-idle-request
   180
   (lambda (process)
     (let ((timer (process-get process :ende-idle-timer))
           (deadline (+ (float-time) 2)))
       (while (and (process-live-p process) (< (float-time) deadline))
         (accept-process-output nil 0.02))
       (should (eq (process-status process) 'exit))
       (should-not (process-get process :ende-idle-error))
       (should-not (process-get process :ende-idle-timer))
       (should-not (memq timer timer-list))))
   '("true")))

(ert-deftest ende-test/curl-idle-refresh-reports-timeout-and-cleans-up ()
  (let ((ende-response-idle-timeout 180)
        (ende-codex--refresh-flights (make-hash-table :test #'equal))
        (ende-codex--refresh-processes nil)
        (ende-codex-auth-file (expand-file-name "unused-test-auth.json"))
        (cancel nil)
        (failure nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ende-codex--refresh-command)
                   (lambda (&rest _) '("sleep" "30")))
                  ((symbol-function 'process-send-string) #'ignore)
                  ((symbol-function 'process-send-eof) #'ignore))
          (setq cancel (ende-codex--refresh-auth
                        '(:refresh "test-refresh")
                        (lambda (result) (setq failure result))))
          (let* ((process (car ende-codex--refresh-processes))
                 (buffer (process-buffer process)))
            (should (timerp (process-get process :ende-idle-timer)))
            (process-put process :ende-last-response-time 0)
            (ende-curl--check-idle process)
            (should (eq (ende-error-kind failure) 'timeout))
            (should-not (buffer-live-p buffer))
            (should-not (process-get process :ende-idle-timer))
            (should (= (hash-table-count ende-codex--refresh-flights) 0))))
      (when cancel (funcall cancel)))))

(ert-deftest ende-test/json-parser-does-not-intern-keys-or-accept-garbage ()
  (let ((key (format "ende-untrusted-key-%d" (random most-positive-fixnum))))
    (should-not (intern-soft key))
    (let ((payload (ende-protocol-json-read-string
                    (format "{\"%s\":1}" key))))
      (should (hash-table-p payload))
      (should (= (ende-protocol-json-get key payload) 1))
      (should-not (intern-soft key))))
  (should-error (ende-protocol-json-read-string "{\"ok\":1} garbage")))

(ert-deftest ende-test/default-transport-enforces-response-byte-limit ()
  (let* ((ende-max-response-bytes 16)
         (out (ende-test--run-default-transport
               "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n"
               (make-string 100 ?X))))
    (should-not (nth 0 out))
    (should (eq (ende-error-kind (nth 1 out)) 'resource))
    (should (= (nth 4 out) 1))))

(ert-deftest ende-test/response-byte-limit-retains-safe-prefix ()
  (let* ((prefix
          "data: {\"choices\":[{\"delta\":{\"content\":\"kept\"}}]}\n\n")
         (ende-max-response-bytes (string-bytes prefix))
         (out (ende-test--run-default-transport
               "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n"
               (concat prefix (make-string 100 ?X))))
         (partial (cl-find-if
                   (lambda (event)
                     (eq (plist-get event :type) :partial-result))
                   (nth 6 out))))
    (should-not (nth 0 out))
    (should (eq (ende-error-kind (nth 1 out)) 'resource))
    (should partial)
    (should (string= (plist-get (plist-get partial :result) :content)
                     "kept"))))

(ert-deftest ende-test/sse-parser-enforces-line-and-event-limits ()
  (let ((ende-max-sse-event-bytes 8)
        (parser (ende-protocol-make-sse)))
    (should-error (ende-protocol-sse-feed parser "123456789")))
  (let ((ende-max-sse-event-bytes 8)
        (parser (ende-protocol-make-sse)))
    (should-error
     (ende-protocol-sse-feed
      parser (mapconcat (lambda (_) "data:\n") (number-sequence 1 10) "")))))

(ert-deftest ende-test/provider-discovery-cache-is-bounded ()
  (let ((ende-provider--discovery-cache (make-hash-table :test #'equal))
        (ende-provider--discovery-inflight (make-hash-table :test #'equal))
        (ende-provider-discovery-cache-size 2))
    (dotimes (index 5)
      (ende-provider--finish-discovery (list index) (list :model index)))
    (should (= (hash-table-count ende-provider--discovery-cache) 2))))

(ert-deftest ende-test/provider-client-display-names ()
  (should (string= (ende-provider-client-name
                    '(:url "http://localhost:8080/v1"))
                   "localhost"))
  (should (string= (ende-provider-client-name
                    '(:url "http://192.168.1.20:8080/v1"))
                   "192.168.1.20"))
  (should (string= (ende-provider-client-name
                    '(:url "https://api.openai.com/v1/chat/completions"))
                   "OpenAI")))

(ert-deftest ende-test/provider-discovery-parses-model-metadata ()
  (let* ((profile '(:url "http://localhost:8080/v1/chat/completions"
			 :model "llama3"))
         (metadata
          (ende-provider--discovery-metadata
           profile
           (ende-protocol-json-read-string
            (concat
             "{\"object\":\"list\",\"data\":["
             "{\"id\":\"llama3\",\"name\":\"Llama 3\","
             "\"owned_by\":\"llama.cpp\",\"pricing\":{"
             "\"prompt\":\"0.000003\",\"completion\":\"0.000015\","
             "\"input_cache_read\":\"0.000001\","
             "\"input_cache_write\":\"0.000004\","
             "\"input_cache_write_1h\":\"0.000005\","
             "\"internal_reasoning\":\"0\"}},"
             "{\"id\":\"llama4\"}]}")))))
    (should (string= (ende-provider-models-url profile)
                     "http://localhost:8080/v1/models"))
    (should (eq (plist-get metadata :source) :discovery))
    (should (equal (plist-get metadata :models) '("llama3" "llama4")))
    (let ((detail (car (plist-get metadata :model-details))))
      (should (string= (plist-get detail :id) "llama3"))
      (should (string= (plist-get detail :name) "Llama 3"))
      (should (equal (plist-get detail :pricing)
                     '(:prompt "0.000003"
			       :completion "0.000015"
			       :internal-reasoning "0"
			       :input-cache-read "0.000001"
			       :input-cache-write "0.000004"
			       :input-cache-write-1h "0.000005"))))
    (should (string= (plist-get metadata :model) "llama3"))
    (should (string= (plist-get metadata :owner) "llama.cpp"))
    (should (string= (plist-get metadata :client) "llama.cpp"))))

(ert-deftest ende-test/codex-effort-probe-uses-selected-model-capabilities ()
  (let ((calls 0)
        (results nil)
        (entries
         (ende-codex--parse-model-entries
          (concat
           "{\"models\":["
           "{\"slug\":\"other\",\"supported_reasoning_levels\":[{\"effort\":\"none\"}]},"
           "{\"slug\":\"selected\",\"default_reasoning_level\":\"low\","
           "\"supported_reasoning_levels\":[{\"effort\":\"high\"},{\"effort\":\"low\"},"
           "{\"effort\":\"\"},{\"effort\":\"low\"}]}]}"))))
    (cl-letf (((symbol-function 'ende-codex--probe-model-entries)
               (lambda (callback)
                 (cl-incf calls)
                 (funcall callback entries nil)
                 (lambda () nil))))
      (dotimes (_ 2)
        (ende-codex-probe-reasoning-efforts
         nil "selected"
         (lambda (efforts default error)
           (push (list efforts default error) results))))
      (should (= calls 2))
      (should (equal results
                     '((("high" "low") "low" nil)
                       (("high" "low") "low" nil)))))))

(ert-deftest ende-test/openrouter-effort-probe-is-live-and-cache-independent ()
  (let ((ende-provider-discovery-enabled t)
        (ende-provider--discovery-cache (make-hash-table :test #'equal))
        (ende-provider--model-cache (make-hash-table :test #'equal))
        (requests nil)
        (results nil)
        (profile (ende-provider-profile 'openrouter)))
    (puthash 'metadata 'unchanged ende-provider--discovery-cache)
    (puthash 'models 'unchanged ende-provider--model-cache)
    (cl-letf
        (((symbol-function 'ende-provider--start-live-model-request)
          (lambda (_profile url callback &optional _discovery-key)
            (push url requests)
            (funcall
             callback
             (ende-protocol-json-read-string
              (concat
               "{\"data\":["
               "{\"id\":\"other\",\"reasoning\":{\"supported_efforts\":[\"none\"]}},"
               "{\"id\":\"selected\",\"reasoning\":{\"supported_efforts\":[\"high\",\"low\",\"high\"],"
               "\"default_effort\":\"low\"}}]}"))
             nil)
            (lambda () nil))))
      (dotimes (_ 2)
        (ende-provider-probe-reasoning-efforts
         profile "selected"
         (lambda (efforts default error)
           (push (list efforts default error) results))))
      (should (equal requests
                     '("https://openrouter.ai/api/v1/models"
                       "https://openrouter.ai/api/v1/models")))
      (should (equal results
                     '((("high" "low") "low" nil)
                       (("high" "low") "low" nil))))
      (should (eq (gethash 'metadata ende-provider--discovery-cache)
                  'unchanged))
      (should (eq (gethash 'models ende-provider--model-cache) 'unchanged)))))

(ert-deftest ende-test/reasoning-effort-discovery-is-limited-to-builtins ()
  (let ((called nil)
        (result nil)
        (ende-provider-discovery-enabled t)
        (profile
         (list :name 'remote :reasoning-effort-discovery-function
               (lambda (&rest _)
                 (setq called t)))))
    (ende-provider-probe-reasoning-efforts
     profile "remote-model"
     (lambda (efforts default error)
       (setq result (list efforts default error))))
    (should-not called)
    (should-not (nth 0 result))
    (should-not (nth 1 result))
    (should (ende-error-p (nth 2 result)))
    (should (string-match-p "no live reasoning-effort endpoint"
                            (ende-error-message (nth 2 result))))))

(ert-deftest ende-test/reasoning-effort-is-serialized-only-by-supported-apis ()
  (let* ((messages (list (cons "user" "question")))
         (openrouter
          (list :model "router-model" :temperature 0.2
                :reasoning-effort-wire-api 'openrouter
                :reasoning-effort "high"))
         (responses
          (list :model "codex-model" :reasoning-effort-wire-api 'responses
                :reasoning-effort "low"))
         (unsupported (list :model "local-model" :temperature 0.2
                            :reasoning-effort "high"))
         (router-body
          (ende-protocol-json-read-string
           (json-encode (ende-llm--request-body messages openrouter))))
         (responses-body
          (ende-protocol-json-read-string
           (json-encode (ende-codex--request-body messages responses))))
         (unsupported-body
          (ende-protocol-json-read-string
           (json-encode (ende-llm--request-body messages unsupported)))))
    (should (string=
             (ende-protocol-json-get
              "effort" (ende-protocol-json-get "reasoning" router-body))
             "high"))
    (should (string=
             (ende-protocol-json-get
              "effort" (ende-protocol-json-get "reasoning" responses-body))
             "low"))
    (should-not (ende-protocol-json-has-key-p "reasoning" unsupported-body))))

(ert-deftest ende-test/provider-model-price-annotation-omits-invalid-fields ()
  (should
   (string=
    (ende-provider-model-price-annotation
     '(:pricing (:prompt "0" :completion "0.000015"
                         :input-cache-read "not-a-price"
                         :input-cache-write-1h "0.000005")))
    "  in: free  out: $15  cache-write-1h: $5")))

(ert-deftest ende-test/provider-model-price-annotation-formats-small-values ()
  (should
   (string=
    (ende-provider-model-price-annotation
     '(:pricing (:prompt "0.000003"
			 :completion "0.000015"
			 :input-cache-read "0.0000003")))
    "  in: $3  out: $15  cache-read: $0.30")))

(ert-deftest ende-test/provider-discovery-does-not-borrow-unrelated-owner ()
  (let* ((profile '(:url "https://example.test/v1/chat/completions"
			 :model "requested"))
         (metadata
          (ende-provider--discovery-metadata
           profile
           (ende-protocol-json-read-string
            "{\"data\":[{\"id\":\"other\",\"owned_by\":\"wrong-owner\"}]}"))))
    (should (equal (plist-get metadata :models) '("other")))
    (should-not (plist-get metadata :owner))
    (should-not (plist-get metadata :client))))

(ert-deftest ende-test/provider-discovery-is-async-and-cached ()
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
            (ende-provider-probe-models
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
                       (ende-provider--parse-discovery-response
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
            (ende-provider-discover
             profile (lambda (metadata) (push metadata values)))
            (should (= (length values) 1))
            (should (string= (plist-get (car values) :model) "llama3"))
            (let ((models nil)
                  (probe-error nil))
              (ende-provider-probe-models
               profile
               (lambda (value error)
                 (setq models value
                       probe-error error)))
              (should (equal models '("llama3" "llama4")))
              (should-not probe-error))
            (ende-provider-discover
             profile (lambda (metadata) (push metadata values)))
            (should (= make-process-count 1))
            (should (= (length values) 2))
            (should (string= (plist-get (car values) :model) "llama3")))
        (when (and (processp process) (process-live-p process))
          (delete-process process))
        (clrhash ende-provider--discovery-cache)
        (clrhash ende-provider--discovery-inflight)))))

(ert-deftest ende-test/model-probe-cache-shares-custom-discovery ()
  (let ((probe-count 0)
        (cancel-count 0)
        (probe-callback nil)
        (first-values nil)
        (second-values nil)
        (third-values nil)
        (ende-provider--model-cache (make-hash-table :test #'equal))
        (ende-provider--model-inflight (make-hash-table :test #'equal))
        (ende-provider--model-cancellations (make-hash-table :test #'equal))
        (ende-provider--model-generation 0))
    (let* ((probe
            (lambda (callback)
              (setq probe-callback callback)
              (cl-incf probe-count)
              (lambda () (cl-incf cancel-count))))
           (profile (list :name 'openai
                          :url "https://chatgpt.example/responses"
                          :model "gpt-saved"
                          :model-discovery-function probe))
           (changed-profile (plist-put (copy-sequence profile)
                                       :model "gpt-changed")))
      (unwind-protect
          (progn
            (ende-provider-probe-models
             profile
             (lambda (models error)
               (setq first-values (list models error))))
            (ende-provider-probe-models
             profile
             (lambda (models error)
               (setq second-values (list models error))))
            (should (= probe-count 1))
            (funcall probe-callback '("gpt-saved" "gpt-next") nil)
            (should (equal first-values
                           (list '("gpt-saved" "gpt-next") nil)))
            (should (equal second-values
                           (list '("gpt-saved" "gpt-next") nil)))
            (ende-provider-probe-models
             changed-profile
             (lambda (models error)
               (setq third-values (list models error))))
            (should (= probe-count 1))
            (should (equal third-values
                           (list '("gpt-saved" "gpt-next") nil)))
            (should (> (ende-provider-models-generation profile) 0))
            (ende-provider-clear-discovery-cache)
            (ende-provider-probe-models
             changed-profile
             (lambda (models error)
               (setq third-values (list models error))))
            (should (= probe-count 2))
            (funcall probe-callback '("gpt-new") nil)
            (should (equal third-values (list '("gpt-new") nil)))
            (should (= cancel-count 0)))
        (ende-provider-abort-discovery)
        (clrhash ende-provider--model-cache)
        (clrhash ende-provider--model-inflight)
        (clrhash ende-provider--model-cancellations)))))

(ert-deftest ende-test/model-probe-failures-are-retried-and-success-is-cached ()
  (let ((attempts 0)
        (first-error nil)
        (retry-models nil)
        (retry-error nil)
        (cached-models nil)
        (ende-provider--model-cache (make-hash-table :test #'equal))
        (ende-provider--model-inflight (make-hash-table :test #'equal))
        (ende-provider--model-cancellations (make-hash-table :test #'equal))
        (ende-provider--model-generation 0))
    (let* ((probe
            (lambda (callback)
              (cl-incf attempts)
              (if (= attempts 1)
                  (funcall
                   callback nil
                   (ende-error-create
                    :kind 'discovery
                    :message "temporary probe failure"))
                (funcall callback '("remote/first" "remote/second") nil))
              (lambda () nil)))
           (profile (list :name 'remote
                          :url "http://remote.example/v1/chat/completions"
                          :model-discovery-function probe)))
      (ende-provider-probe-models
       profile
       (lambda (_models error)
         (setq first-error error)))
      (should (= attempts 1))
      (should (ende-error-p first-error))
      (should (string= (ende-error-message first-error)
                       "temporary probe failure"))
      (should-not (ende-provider-cached-models profile))

      (ende-provider-probe-models
       profile
       (lambda (models error)
         (setq retry-models models
               retry-error error)))
      (should (= attempts 2))
      (should-not retry-error)
      (should (equal retry-models
                     '("remote/first" "remote/second")))

      (ende-provider-probe-models
       profile
       (lambda (models error)
         (setq cached-models (list models error))))
      (should (= attempts 2))
      (should (equal cached-models
                     (list '("remote/first" "remote/second") nil))))))

(ert-deftest ende-test/generic-model-probe-retries-after-failure ()
  (let* ((profile '(:name remote
			  :url "http://remote.example/v1/chat/completions"))
         (requests 0)
         (first-error nil)
         (retry-models nil)
         (retry-error nil)
         (discovery-key (ende-provider--discovery-key profile))
         (ende-provider--discovery-cache (make-hash-table :test #'equal))
         (ende-provider--model-cache (make-hash-table :test #'equal))
         (ende-provider--model-inflight (make-hash-table :test #'equal))
         (ende-provider--model-cancellations (make-hash-table :test #'equal))
         (ende-provider--model-generation 0))
    (cl-letf (((symbol-function 'ende-provider-discover)
               (lambda (_profile callback)
                 (cl-incf requests)
                 (if (= requests 1)
                     (progn
                       (puthash discovery-key
                                (list :time (float-time) :metadata nil)
                                ende-provider--discovery-cache)
                       (funcall callback nil))
                   (funcall callback '(:models ("remote/first"))))
                 (lambda () nil))))
      (ende-provider-probe-models
       profile
       (lambda (_models error)
         (setq first-error error)))
      (should (= requests 1))
      (should (ende-error-p first-error))
      (should-not (gethash discovery-key ende-provider--discovery-cache))
      (should-not (ende-provider-cached-models profile))

      (ende-provider-probe-models
       profile
       (lambda (models error)
         (setq retry-models models
               retry-error error)))
      (should (= requests 2))
      (should-not retry-error)
      (should (equal retry-models '("remote/first")))
      (should (equal (plist-get (ende-provider-cached-models profile)
                                :models)
                     '("remote/first"))))))

(ert-deftest ende-test/selector-retries-failed-model-probe ()
  (let* ((buffer (generate-new-buffer " *ende-selector-probe-retry-test*"))
         (attempts 0)
         (probe
          (lambda (callback)
            (cl-incf attempts)
            (if (= attempts 1)
                (funcall
                 callback nil
                 (ende-error-create
                  :kind 'discovery
                  :message "temporary probe failure"))
              (funcall callback '("remote/first") nil))
            (lambda () nil)))
         (ende-providers '(remote))
         (ende-provider-profiles
          (list (list 'remote
                      :url "http://remote.example/v1/chat/completions"
                      :model-discovery-function probe)))
         (ende-selector--last-selection nil)
         (ende-selector--model-validations (make-hash-table :test #'equal))
         (ende-provider--model-cache (make-hash-table :test #'equal))
         (ende-provider--model-inflight (make-hash-table :test #'equal))
         (ende-provider--model-cancellations (make-hash-table :test #'equal))
         (ende-provider--model-generation 0))
    (unwind-protect
        (with-current-buffer buffer
          (ende-selector-mode)
          (setq ende-selector--providers '(remote)
                ende-selector--provider-index 0)
          (ende-selector--set-provider 0)
          (should (= attempts 1))
          (should (eq ende-selector--model-state :error))
          (should (string-match-p "probe failed" (buffer-string)))

          (ende-selector--set-provider 0)
          (should (= attempts 2))
          (should (eq ende-selector--model-state :ready))
          (should (string= (ende-selector--model-name) "remote/first"))

          (ende-selector--set-provider 0)
          (should (= attempts 2)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ende-test/provider-discovery-cancellation-releases-last-waiter ()
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
          (let* ((key (ende-provider--discovery-key profile))
                 (cancel
                  (ende-provider-discover
                   profile (lambda (metadata) (setq value metadata)))))
            (should (process-live-p process))
            (should (gethash key ende-provider--discovery-inflight))
            (funcall cancel)
            (should-not value)
            (should-not (process-live-p process))
            (should-not (gethash key ende-provider--discovery-inflight))
            (should-not (gethash key ende-provider--discovery-cache))
            (should-not ende-provider--discovery-processes)
            (let* ((probe-value nil)
                   (probe-error nil)
                   (probe-cancel
                    (ende-provider-probe-models
                     profile
                     (lambda (models error)
                       (setq probe-value models
                             probe-error error)))))
              (should (process-live-p process))
              (funcall probe-cancel)
              (should-not probe-value)
              (should-not probe-error)
              (should-not (process-live-p process))
              (should-not (gethash key ende-provider--discovery-inflight))
              (should-not ende-provider--discovery-processes)))
        (when (and (processp process) (process-live-p process))
          (delete-process process))
        (clrhash ende-provider--discovery-cache)
        (clrhash ende-provider--discovery-inflight)
        (setq ende-provider--discovery-processes nil)))))

(ert-deftest ende-test/provider-discovery-response-is-bounded ()
  (let ((profile '(:name local
			 :url "http://localhost:8080/v1/chat/completions"
			 :model "llama3"))
        (ende-provider-max-discovery-bytes 4)
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
            (ende-provider-discover
             profile (lambda (metadata) (push metadata values)))
            (funcall (plist-get captured :filter) process "oversized")
            (should (equal values '(nil)))
            (should-not (process-live-p process))
            (should (= (hash-table-count
                        ende-provider--discovery-inflight) 0)))
        (when (and (processp process) (process-live-p process))
          (delete-process process))
        (clrhash ende-provider--discovery-cache)
        (clrhash ende-provider--discovery-inflight)))))

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

;;;; Overlay lifecycle

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
  (set-face-foreground 'font-lock-keyword-face "#0000ff")
  (set-face-background 'font-lock-keyword-face "#ffffff")
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
			      (should (string-match-p "client model"
						      (ende-test--overlay-label review)))
			      (should (equal (overlay-get review 'face) face))
			      (ende-toggle-review-badges)
			      (should-not ende-review-show-badges)
			      (should-not (overlay-get review 'before-string))
			      (should (equal (overlay-get review 'face) face))
			      (goto-char (1+ (overlay-start review)))
			      (ende-approve-output)
			      (should-not (ende-review-overlay-p review))))))

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
  (should (null (ende-review--blend-color "#abc" "#ffffff" 0.5)))
  (should (null (ende-review--blend-color "#ffffff" "junk" 0.5))))

(ert-deftest ende-test/review-highlight-hex-round-trip ()
  (should (equal (ende-review--hex-components "#a1b2c3") '(161 178 195)))
  (should (string= (ende-review--hex-join '(161 178 195)) "#a1b2c3"))
  (should (null (ende-review--hex-components "red")))
  (should (null (ende-review--hex-components "#12345")))
  (should (null (ende-review--hex-components "#1234567")))
  (should (null (ende-review--hex-join '(1 2)))))

(ert-deftest ende-test/review-highlight-face-attr-resolution ()
  (set-face-foreground 'font-lock-keyword-face "#0000ff")
  (set-face-background 'font-lock-keyword-face "#ffffff")
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
  (should (null (ende-review--face-attr '(:weight bold) :foreground))))

(ert-deftest ende-test/review-highlight-style-from-adjacent-face ()
  (set-face-foreground 'font-lock-keyword-face "#0000ff")
  (set-face-background 'font-lock-keyword-face "#ffffff")
  (ende-test--with-buffer "defun x"
			  (add-text-properties 1 6 '(face font-lock-keyword-face))
			  (should (equal (ende-review--style (current-buffer) 6)
					 '("#0000ff" . "#d9d9ff")))))

(ert-deftest ende-test/review-highlight-style-default-fallback ()
  (set-face-foreground 'default "#111111")
  (set-face-background 'default "#222222")
  (ende-test--with-buffer "plain x"
			  (should (equal (ende-review--style (current-buffer) 6)
					 '("#111111" . "#1f1f1f")))
			  (should (equal (ende-review--style (current-buffer) 1)
					 '("#111111" . "#1f1f1f")))))

(ert-deftest ende-test/review-highlight-dark-background-light-foreground ()
  (set-face-foreground 'default "#e6e6e6")
  (set-face-background 'default "#1e1e1e")
  (ende-test--with-buffer "plain text"
			  (should (equal (ende-review--style (current-buffer) 6)
					 '("#e6e6e6" . "#3c3c3c")))))

(ert-deftest ende-test/review-highlight-overlay-sets-face ()
  (ende-test--reset)
  (set-face-foreground 'font-lock-keyword-face "#0000ff")
  (set-face-background 'font-lock-keyword-face "#ffffff")
  (ende-test--with-buffer "defunLLM"
			  (add-text-properties 1 6 '(face font-lock-keyword-face))
			  (let ((overlay (ende-review-create-overlay
					  (current-buffer) 6 9 nil nil "")))
			    (should (overlayp overlay))
			    (should (equal (overlay-get overlay 'face)
					   '(:foreground "#0000ff" :background "#d9d9ff")))
			    (should (eq (get-char-property 3 'face) 'font-lock-keyword-face))
			    (should (null (get-char-property 3 'foreground-color)))
			    (ende-review-delete-overlay overlay)
			    (should (eq (get-char-property 3 'face) 'font-lock-keyword-face)))))

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
  (set-face-foreground 'font-lock-keyword-face "#0000ff")
  (set-face-background 'font-lock-keyword-face "#ffffff")
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
			      (should-not (ende-review-overlay-p review))))))

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
         (ende-model-selection-scope 'buffer)
         (ende-model-selection--last-used nil)
         (ende-model-selection--buffer-selections
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
    (ende-test--reset)))

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

(ert-deftest ende-test/openrouter-batch-profile-has-separate-endpoints ()
  (let ((profile (ende-provider-profile 'openrouter-batch)))
    (should (eq (plist-get profile :transport) 'openrouter-batch))
    (should (string= (plist-get profile :url)
                     "https://openrouter.ai/api/v1/chat/completions"))
    (should (string= (plist-get profile :batch-url)
                     "https://openrouter.ai/api/v1/batches"))
    (should (string= (plist-get profile :batch-endpoint)
                     "/v1/chat/completions"))
    (should (equal (plist-get (ende-provider-profile 'openrouter)
                              :transport)
                   'auto))
    (should-error
     (ende-openrouter-batch--validate-options
      (plist-put (copy-sequence profile)
                 :batch-url "https://openrouter.ai/api/v1/chat/completions")))))

(ert-deftest ende-test/openrouter-profiles-advertise-ende-mode ()
  (dolist (name '(openrouter openrouter-batch))
    (let* ((profile (ende-provider-profile name))
           (headers (plist-get profile :request-headers))
           (config
            (ende-curl--config
             (list :url (plist-get profile :url)
                   :request-headers headers))))
      (unwind-protect
          (progn
            (should (equal headers
                           '("HTTP-Referer: https://github.com/oetr/ende-mode"
                             "X-OpenRouter-Title: ende-mode")))
            (with-temp-buffer
              (insert-file-contents config)
              (should (equal
                       (buffer-string)
                       (concat
                        "header = \"HTTP-Referer: https://github.com/oetr/ende-mode\"\n"
                        "header = \"X-OpenRouter-Title: ende-mode\"\n")))))
        (when (file-exists-p config)
          (delete-file config))))))

(ert-deftest ende-test/openrouter-batch-initial-404-retries-then-completes ()
  (let* ((profile (plist-put
                   (plist-put
                    (plist-put (copy-sequence
                                (ende-provider-profile 'openrouter-batch))
                               :batch-poll-initial-delay 2)
                    :batch-poll-max-delay 60)
                   :batch-poll-jitter 0))
         (delays nil)
         (result nil)
         (failure nil)
         (request
          (ende-openrouter-batch--make-request
           :custom-id "ende-job-404"
           :profile profile
           :callbacks
           (list :on-done (lambda (value) (setq result value))
                 :on-error (lambda (value) (setq failure value)))))
         (group
          (ende-openrouter-batch--make-group
           :profile profile
           :requests (list request)
           :batch-id "batch-1789766830-OF6zb6TiexdSIEpfCv8N"
           :submitted-at (float-time)
           :deadline (+ (float-time) 300)
           :not-found-attempt 0)))
    (cl-letf
        (((symbol-function 'ende-openrouter-batch--schedule-poll)
          (lambda (_group &optional delay)
            (when delay
              (setq delays (append delays (list delay))))))
         ((symbol-function 'ende-openrouter-batch--status)
          (lambda (_group _status) nil)))
      (ende-openrouter-batch--poll-finished
       group 404 "{\"error\":{\"message\":\"not found\"}}" nil)
      (ende-openrouter-batch--poll-finished
       group 404 "{\"error\":{\"message\":\"not found\"}}" nil)
      (should-not failure)
      (should-not (ende-openrouter-batch--group-terminal group))
      (should (= (ende-openrouter-batch--group-not-found-attempt group) 2))
      (should (equal delays '(2 4)))
      (ende-openrouter-batch--poll-finished
       group 200 "{\"status\":\"validating\"}" nil)
      (should (ende-openrouter-batch--group-batch-visible-p group))
      (should (= (ende-openrouter-batch--group-not-found-attempt group) 0))
      (ende-openrouter-batch--poll-finished
       group 200
       (format
        (concat
         "{\"status\":\"completed\",\"results\":[{"
         "\"custom_id\":\"%s\",\"response\":{"
         "\"status_code\":200,\"body\":{\"choices\":[{"
         "\"message\":{\"content\":\"done\"}}]}}}]}"
         )
        (ende-openrouter-batch--request-custom-id request))
       nil))
    (should-not failure)
    (should (string= (plist-get result :content) "done"))
    (should (ende-openrouter-batch--group-terminal group))))

(ert-deftest ende-test/openrouter-batch-404-expires-with-context ()
  (let* ((profile (plist-put
                   (plist-put
                    (plist-put
                     (copy-sequence (ende-provider-profile 'openrouter-batch))
                     :batch-poll-initial-delay 1)
                    :batch-poll-max-delay 4)
                   :batch-poll-jitter 0))
         (failure nil)
         (delays nil)
         (request
          (ende-openrouter-batch--make-request
           :custom-id "ende-job-expiring"
           :profile profile
           :callbacks (list :on-error (lambda (value) (setq failure value)))))
         (group
          (ende-openrouter-batch--make-group
           :profile profile
           :requests (list request)
           :batch-id "batch-1789766830-OF6zb6TiexdSIEpfCv8N"
           :submitted-at (float-time)
           :deadline (+ (float-time) 300)
           :not-found-attempt 0)))
    (setf (ende-openrouter-batch--group-profile group)
          (plist-put profile :batch-poll-not-found-max-age 3))
    (cl-letf
        (((symbol-function 'ende-openrouter-batch--schedule-poll)
          (lambda (_group &optional delay)
            (push delay delays))))
      (ende-openrouter-batch--poll-finished
       group 404 "{\"error\":{\"message\":\"not found\"}}" nil)
      (should-not failure)
      (should (equal delays '(1)))
      (setf (ende-openrouter-batch--group-submitted-at group)
            (- (float-time) 2.5))
      (ende-openrouter-batch--poll-finished
       group 404 "{\"error\":{\"message\":\"not found\"}}" nil))
    (should (ende-error-p failure))
    (should (eq (ende-error-kind failure) 'http))
    (should (string-match-p
             (regexp-quote
              "batch-1789766830-OF6zb6TiexdSIEpfCv8N")
             (ende-error-message failure)))
    (should (string-match-p
             (regexp-quote
              "https://openrouter.ai/api/v1/batches/batch-1789766830-OF6zb6TiexdSIEpfCv8N")
             (ende-error-message failure)))
    (should (string-match-p "ever being observed"
                            (ende-error-message failure)))
    (should (ende-openrouter-batch--group-terminal group))))

(ert-deftest ende-test/openrouter-batch-404-after-visibility-is-terminal ()
  (let* ((profile (ende-provider-profile 'openrouter-batch))
         (failure nil)
         (request
          (ende-openrouter-batch--make-request
           :custom-id "ende-job-visible"
           :profile profile
           :callbacks (list :on-error (lambda (value) (setq failure value)))))
         (group
          (ende-openrouter-batch--make-group
           :profile profile
           :requests (list request)
           :batch-id "batch-visible"
           :submitted-at (float-time)
           :deadline (+ (float-time) 300)
           :not-found-attempt 0
           :batch-visible-p t)))
    (cl-letf (((symbol-function 'ende-openrouter-batch--schedule-poll)
               (lambda (&rest _arguments)
                 (error "visible batch must not retry 404"))))
      (ende-openrouter-batch--poll-finished
       group 404 "{\"error\":{\"message\":\"not found\"}}" nil))
    (should (ende-error-p failure))
    (should (string-match-p "previously visible"
                            (ende-error-message failure)))
    (should (ende-openrouter-batch--group-terminal group))))

(ert-deftest ende-test/openrouter-batch-body-is-non-streaming-and-ordered ()
  (let* ((ende-max-output-tokens 64)
         (profile (plist-put (ende-provider-profile 'openrouter-batch)
                             :reasoning-effort "medium"))
         (group (ende-openrouter-batch--make-group :profile profile))
         (request
          (ende-openrouter-batch--make-request
           :custom-id "ende-job-1"
           :messages '( ("system" . "rules") ("user" . "question"))
           :profile profile)))
    (setf (ende-openrouter-batch--group-requests group) (list request))
    (let* ((encoded (ende-openrouter-batch--body group))
           (text (decode-coding-string encoded 'utf-8))
           (body (ende-protocol-json-read-string text))
           (request-body
            (ende-protocol-json-get
             "body"
             (aref (ende-protocol-json-get "requests" body) 0))))
      (should (string-match-p
               "\"endpoint\".*\"model\".*\"requests\"" text))
      (should (string= (ende-protocol-json-get "endpoint" body)
                       "/v1/chat/completions"))
      (should (string= (ende-protocol-json-get "custom_id"
                                               (aref
                                                (ende-protocol-json-get
                                                 "requests" body)
                                                0))
                       "ende-job-1"))
      (should-not (ende-protocol-json-has-key-p "stream" request-body))
      (should-not (ende-protocol-json-has-key-p "model" request-body))
      (should (= (ende-protocol-json-get "max_tokens" request-body) 64))
      (should (string=
               (ende-protocol-json-get
                "effort" (ende-protocol-json-get "reasoning" request-body))
               "medium")))))

(ert-deftest ende-test/openrouter-batches-separate-reasoning-efforts ()
  (let* ((low (plist-put (ende-provider-profile 'openrouter-batch)
                         :reasoning-effort "low"))
         (high (plist-put (ende-provider-profile 'openrouter-batch)
                          :reasoning-effort "high")))
    (should-not (equal (ende-openrouter-batch--profile-key low)
                       (ende-openrouter-batch--profile-key high)))))

(ert-deftest ende-test/openrouter-batch-completion-routes-by-custom-id ()
  (let* ((profile (ende-provider-profile 'openrouter-batch))
         (events nil)
         (result nil)
         (failure nil)
         (request
          (ende-openrouter-batch--make-request
           :custom-id "ende-job-2"
           :profile profile
           :callbacks
           (list :on-event (lambda (event) (push event events))
                 :on-done (lambda (value) (setq result value))
                 :on-error (lambda (value) (setq failure value)))))
         (cancelled-request
          (ende-openrouter-batch--make-request
           :custom-id "unrelated" :profile profile :cancelled t))
         (group
          (ende-openrouter-batch--make-group
           :profile profile :requests (list request cancelled-request)))
         (payload
          (ende-protocol-json-read-string
           (concat
            "{\"status\":\"completed\","
            "\"usage\":{\"prompt_tokens\":5,\"completion_tokens\":3},"
            "\"results\":["
            "{\"custom_id\":\"unrelated\",\"error\":{\"message\":\"ignored\"}},"
            "{\"custom_id\":\"ende-job-2\",\"response\":{"
            "\"status_code\":200,\"body\":{\"model\":\"actual/model\","
            "\"choices\":[{\"message\":{\"content\":\"answer\","
            "\"reasoning_content\":\"thought\"},\"finish_reason\":\"stop\"}]}}}]}"))))
    (ende-openrouter-batch--finish-completed group payload)
    (should-not failure)
    (should (string= (plist-get result :content) "answer"))
    (should (string= (plist-get result :thinking) "thought"))
    (should (string= (plist-get result :model) "actual/model"))
    (should (= (plist-get result :tokens) 3))
    (should (cl-find-if (lambda (event) (eq (plist-get event :type) :metadata))
                        events))
    (should (cl-find-if (lambda (event) (eq (plist-get event :type) :usage))
                        events))))

(ert-deftest ende-test/openrouter-batch-result-failure-does-not-wedge-group ()
  (let* ((profile (ende-provider-profile 'openrouter-batch))
         (failure nil)
         (request
          (ende-openrouter-batch--make-request
           :custom-id "ende-job-3"
           :profile profile
           :callbacks (list :on-error (lambda (value) (setq failure value)))))
         (group
          (ende-openrouter-batch--make-group
           :profile profile :requests (list request)))
         (payload
          (ende-protocol-json-read-string
           "{\"status\":\"completed\",\"results\":[{\"custom_id\":\"ende-job-3\",\"error\":{\"message\":\"provider failed\"}}]}")))
    (ende-openrouter-batch--finish-completed group payload)
    (should (ende-error-p failure))
    (should (eq (ende-error-kind failure) 'provider))
    (should-not (ende-openrouter-batch--group-stage group))
    (should (ende-openrouter-batch--group-terminal group))))

(ert-deftest ende-test/openrouter-batch-request-cancellation-cleans-pending-group ()
  (ende-test--reset)
  (let ((profile (plist-put (ende-provider-profile 'openrouter-batch)
                            :batch-window 60))
        (cancel nil)
        (done nil)
        (failure nil))
    (unwind-protect
        (progn
          (ende-openrouter-batch-default
           '(("user" . "question"))
           (list :on-start (lambda (function) (setq cancel function))
                 :on-done (lambda (_) (setq done t))
                 :on-error (lambda (_) (setq failure t)))
           profile)
          (should (functionp cancel))
          (funcall cancel)
          (should-not done)
          (should-not failure)
          (should (= (hash-table-count ende-openrouter-batch--groups) 0))
          (should-not ende-openrouter-batch--known-groups))
      (ende-openrouter-batch-abort))))

(ert-deftest ende-test/openrouter-batch-json-command-uses-json-accept ()
  (let ((command
         (ende-curl-json-command
          '(:url "https://openrouter.ai/api/v1/chat/completions"
		 :auth-source-host "openrouter.ai"
		 :auth-source-user "ende"
		 :auth-source-port 443)
          "/tmp/headers" nil "GET" "application/json" nil
          "https://openrouter.ai/api/v1/batches/batch_1")))
    (should (member "Accept: application/json" command))
    (should-not (member "Accept: text/event-stream" command))
    (should-not (member "@-" command))
    (should (equal (cadr (member "--url" command))
                   "https://openrouter.ai/api/v1/batches/batch_1"))))

(ert-deftest ende-test/openrouter-batch-submits-polls-and-settles ()
  (let* ((profile (plist-put (ende-provider-profile 'openrouter-batch)
                             :batch-window 0))
         (stages nil)
         (processes nil)
         (files nil)
         (events nil)
         (result nil)
         (failure nil)
         (real-make-process (symbol-function 'make-process)))
    (cl-letf
        (((symbol-function 'ende-curl-start-request)
          (lambda (&rest arguments)
            (let* ((response-buffer (generate-new-buffer
                                     " *ende-batch-test-response*"))
                   (stderr-buffer (generate-new-buffer
                                   " *ende-batch-test-stderr*"))
                   (headers-file (make-temp-file "ende-batch-test-headers-"))
                   (process
                    (funcall real-make-process
                             :name (generate-new-buffer-name
                                    "ende-batch-test-process")
                             :buffer nil
                             :command (list "cat")
                             :connection-type 'pipe
                             :noquery t))
                   (request (list :process process
                                  :response-buffer response-buffer
                                  :stderr-buffer stderr-buffer
                                  :stderr-process nil
                                  :headers-file headers-file
                                  :config-file nil))
                   (stage (list :request request
                                :filter (plist-get arguments :filter)
                                :sentinel (plist-get arguments :sentinel)
                                :command (plist-get arguments :command))))
              (process-put process :ende-response-buffer response-buffer)
              (process-put process :ende-stderr-buffer stderr-buffer)
              (process-put process :ende-headers-file headers-file)
              (process-put process :ende-stderr-process nil)
              (setq stages (append stages (list stage))
                    processes (cons process processes)
                    files (cons headers-file files))
              request))))
      (unwind-protect
          (progn
            (ende-openrouter-batch-default
             '(("user" . "question"))
             (list :on-event (lambda (event) (push event events))
                   :on-done (lambda (value) (setq result value))
                   :on-error (lambda (value) (setq failure value)))
             profile)
            (let ((group (car ende-openrouter-batch--known-groups)))
              (ende-openrouter-batch--flush group)
              (let ((stage (nth 0 stages)))
                (with-temp-file (plist-get (plist-get stage :request)
                                           :headers-file)
                  (insert "HTTP/1.1 202 Accepted\r\n"
                          "Content-Type: application/json\r\n\r\n"))
                (funcall (plist-get stage :filter)
                         (plist-get (plist-get stage :request) :process)
                         "{\"id\":\"batch_1\",\"status\":\"validating\"}")
                (while (process-live-p
                        (plist-get (plist-get stage :request) :process))
                  (accept-process-output
                   (plist-get (plist-get stage :request) :process) 0.01))
                (funcall (plist-get stage :sentinel)
                         (plist-get (plist-get stage :request) :process)
                         "finished"))
              (ende-openrouter-batch--cancel-timer
               (ende-openrouter-batch--group-poll-timer group))
              (setf (ende-openrouter-batch--group-poll-timer group) nil)
              (ende-openrouter-batch--poll group)
              (let ((stage (nth 1 stages)))
                (should (stringp
                         (plist-get (plist-get stage :request)
                                    :headers-file)))
                (with-temp-file (plist-get (plist-get stage :request)
                                           :headers-file)
                  (insert "HTTP/1.1 200 OK\r\n"
                          "Content-Type: application/json\r\n\r\n"))
                (funcall (plist-get stage :filter)
                         (plist-get (plist-get stage :request) :process)
                         (format
                          (concat
                           "{\"status\":\"completed\",\"usage\":{"
                           "\"completion_tokens\":2},\"results\":[{"
                           "\"custom_id\":\"%s\","
                           "\"response\":{\"status_code\":200,\"body\":{"
                           "\"choices\":[{\"message\":{\"content\":\"done\"}}]}}}]}"
                           )
                          (ende-openrouter-batch--request-custom-id
                           (car (ende-openrouter-batch--group-requests
                                 group)))))
                (while (process-live-p
                        (plist-get (plist-get stage :request) :process))
                  (accept-process-output
                   (plist-get (plist-get stage :request) :process) 0.01))
                (funcall (plist-get stage :sentinel)
                         (plist-get (plist-get stage :request) :process)
                         "finished")))
            (should-not failure)
            (should (string= (plist-get result :content) "done"))
            (should (= (plist-get result :tokens) 2))
            (should (cl-find-if
                     (lambda (event)
                       (equal (plist-get event :status) "Submitting batch"))
                     events))
            (should (member "Accept: application/json"
                            (funcall (plist-get (nth 0 stages) :command)
                                     "/tmp/headers" nil)))
            (should-not (member "--data-binary"
				(funcall (plist-get (nth 1 stages) :command)
                                         "/tmp/headers" nil))))
        (ende-openrouter-batch-abort)
        (dolist (process processes)
          (when (process-live-p process)
            (delete-process process)))
        (dolist (file files)
          (when (file-exists-p file)
            (delete-file file)))))))

(ert-deftest ende-test/openrouter-auto-handoff-only-matches-batch-error ()
  (let ((headers-file (make-temp-file "ende-auto-batch-"))
        (handoff nil))
    (unwind-protect
        (progn
          (with-temp-file headers-file
            (insert "HTTP/1.1 404 Not Found\r\n"
                    "Content-Type: application/json\r\n\r\n"))
          (cl-letf
              (((symbol-function 'ende-openrouter-batch-default)
                (lambda (messages callbacks profile)
                  (setq handoff (list messages callbacks profile))))
               ((symbol-function 'ende-llm--cleanup-session)
                (lambda (_session) nil)))
            (let ((session
                   (ende-llm--make-session
                    :profile '(:transport auto
					  :batch-url "https://openrouter.ai/api/v1/batches")
                    :messages '(("user" . "question"))
                    :headers-file headers-file
                    :response-preview-parts
                    '("{\"error\":{\"message\":\"This model is only available through the Batch API. Use the /api/v1/batches endpoint instead.\"}}"))))
              (ende-llm--finish-success session)
              (should handoff)
              (should (eq (plist-get (nth 2 handoff) :transport)
                          'openrouter-batch)))))
      (when (file-exists-p headers-file)
        (delete-file headers-file)))))

;;;; Org-backed chat

(defun ende-test--chat-goto (heading)
  "Move point to the Org headline whose exact text is HEADING."
  (goto-char (point-min))
  (search-forward (concat heading "\n"))
  (org-back-to-heading t))

(defun ende-test--selector-ask-from-buffer (source)
  "Run the selector's ask action for SOURCE with the test model."
  (with-temp-buffer
    (ende-selector-mode)
    (setq ende-selector--source-buffer source
          ende-selector--providers '(remote)
          ende-selector--provider-index 0
          ende-selector--models '("selected-model")
          ende-selector--model-index 0
          ende-selector--model-state :ready
          ende-selector--effort nil)
    (cl-letf (((symbol-function 'ende-selector-close) #'ignore))
      (call-interactively
       (lookup-key ende-selector-mode-map (kbd "a"))))))

(defun ende-test--chat-context-text (messages)
  "Return all serialized chat message text in MESSAGES."
  (mapconcat (lambda (message) (cdr message)) messages "\n"))

(defun ende-test--chat-context-headings (messages)
  "Return the headings present in serialized MESSAGES, in order."
  (with-temp-buffer
    (org-mode)
    (insert (ende-test--chat-context-text messages))
    (org-element-map (org-element-parse-buffer) 'headline
		     (lambda (headline) (org-element-property :raw-value headline)))))

(defun ende-test--chat-message-role (message)
  "Return the role of an ordinary or protocol-shaped chat MESSAGE."
  (if (and (consp message) (stringp (car message)))
      (car message)
    (plist-get message :role)))

(defun ende-test--chat-protocol-messages (messages)
  "Return protocol-shaped messages from serialized MESSAGES."
  (cl-remove-if-not
   (lambda (message)
     (and (not (and (consp message) (stringp (car message))))
          (plist-member message :role)))
   messages))

(ert-deftest ende-test/chat-tool-context-round-trips-to-provider-protocols ()
  (let* ((transcript
          (list (list :role "assistant" :content nil
                      :tool-calls
                      (list (list :id "call-1" :name "read_file"
                                  :arguments "{ \"path\" : \"foo.el\" }")
                            (list :id "call-2" :name "read_file"
                                  :arguments "{\"path\":\"bar.el\"}")))
                (list :role "tool" :tool-call-id "call-1"
                      :content "{\"text\":\"first line\\nsecond line\"}")
                (list :role "tool" :tool-call-id "call-2"
                      :content "{\"text\":\"another result\"}")
                (list :role "assistant" :content "One more lookup."
                      :tool-calls
                      (list (list :id "call-3" :name "read_file"
                                  :arguments "{\"path\":\"third.el\"}")))
                (list :role "tool" :tool-call-id "call-3"
                      :content "{\"text\":\"third result\"}")))
         (encoded
          (json-encode
           (list
            (cons :version 1)
            (cons :messages
                  (vconcat
                   (mapcar
                    (lambda (message)
                      (pcase (plist-get message :role)
                        ("assistant"
                         (list (cons :role "assistant")
                               (cons :content (plist-get message :content))
                               (cons :tool_calls
                                     (vconcat
                                      (mapcar
                                       (lambda (call)
                                         (list
                                          (cons :id (plist-get call :id))
                                          (cons :name (plist-get call :name))
                                          (cons :arguments
                                                (plist-get call :arguments))))
                                       (plist-get message :tool-calls))))))
                        ("tool"
                         (list (cons :role "tool")
                               (cons :tool_call_id
                                     (plist-get message :tool-call-id))
                               (cons :content (plist-get message :content))))))
                    transcript))))))
         (decoded (ende-chat--decode-tool-context encoded))
         (chat-assistant (ende-llm--message-json (car decoded)))
         (chat-tool (ende-llm--message-json (nth 1 decoded)))
         (responses (ende-codex--request-messages decoded)))
    (should-not (string-match-p "\n" encoded))
    (should (equal decoded transcript))
    (should (equal (plist-get (car (plist-get (car decoded) :tool-calls))
                              :arguments)
                   "{ \"path\" : \"foo.el\" }"))
    (should (equal (alist-get :arguments
                              (alist-get :function
                                         (car (alist-get :tool_calls
                                                         chat-assistant))))
                   "{ \"path\" : \"foo.el\" }"))
    (should (equal (alist-get :tool_call_id chat-tool) "call-1"))
    (should (equal (alist-get :call_id (nth 0 responses)) "call-1"))
    (should (equal (alist-get :call_id (nth 1 responses)) "call-2"))
    (should (equal (alist-get :output (nth 2 responses))
                   (plist-get (nth 1 decoded) :content)))
    (should (equal (alist-get :call_id (nth 5 responses)) "call-3"))
    (should (equal (alist-get :output (nth 6 responses))
                   (plist-get (nth 4 decoded) :content)))
    (should-error
     (ende-chat--validate-tool-context
      '((:role "tool" :tool-call-id "orphan" :content "result")))
     :type 'error)
    (let ((failure
           (condition-case caught
               (progn (ende-chat--decode-tool-context "{not JSON") nil)
             (error (error-message-string caught)))))
      (should (string-match-p "Invalid ENDE_CHAT_TOOL_CONTEXT JSON" failure)))
    (let ((failure
           (condition-case caught
               (progn
                 (ende-chat--decode-tool-context
                  "{\"version\":2,\"messages\":[]}")
                 nil)
             (error (error-message-string caught)))))
      (should (string-match-p "Unsupported ENDE_CHAT_TOOL_CONTEXT version: 2"
                              failure)))))

(ert-deftest ende-test/chat-single-tool-round-is-restored-before-the-reply ()
  (let* ((call (list :id "read-1" :name "read_file"
                     :arguments "{ \"path\" : \"foo.el\" }"))
         (assistant
          (ende-chat--assistant-protocol-text
           2 "question-1" "request-1" ""
           '(:content "I'll inspect foo.el.") (list call)))
         (tool (ende-chat--tool-result-text
                2 "request-1" call "{\"result\":\"source text\"}"))
         (answer (ende-chat--assistant-text
                  2 "question-1" "request-1" "" '(:content "Final answer"))))
    (with-temp-buffer
      (org-mode)
      (insert "* Topic\n** Question\nRead foo.el.\n")
      (insert assistant tool answer)
      (insert "** Follow-up\nAsk about nil.\n")
      (ende-test--chat-goto "** Follow-up")
      (let* ((messages (ende-chat-context-at-point))
             (protocol (ende-test--chat-protocol-messages messages))
             (reply (cl-position-if
                     (lambda (message)
                       (and (equal (ende-test--chat-message-role message)
                                   "assistant")
                            (stringp (cdr message))
                            (string-match-p "Final answer" (cdr message))))
                     messages)))
        (should (= (length protocol) 2))
        (should (equal (mapcar #'ende-test--chat-message-role messages)
                       '("user" "assistant" "tool" "assistant" "user")))
        (should (equal (plist-get (car protocol) :content)
                       "I'll inspect foo.el."))
        (should (equal (plist-get
                        (car (plist-get (car protocol) :tool-calls))
                        :arguments)
                       "{ \"path\" : \"foo.el\" }"))
        (should (equal (plist-get (cadr protocol) :tool-call-id) "read-1"))
        (should (= reply (1+ (cl-position (car (last protocol))
                                          messages :test #'equal))))))))

(ert-deftest ende-test/chat-protocol-sections-round-trip-losslessly-in-order ()
  (let* ((first-calls
          (list (list :id "call-A/01" :name "read_file"
                      :arguments "{\n  \"path\" : \"foo.el\",\n  \"note\" : \"* literal\"\n}" )
                (list :id "call-B:02" :name "search_project"
                      :arguments "{ \"regexp\" : \"foo\\\\nbar\" }")))
         (second-call
          (list :id "call-C" :name "read_buffer"
                :arguments "{\"buffer_id\":\"buffer-1\"}"))
         (assistant-one "I'll inspect both.\n* this is still assistant content\n")
         (result-one "first line\n* raw result heading\n#+begin_example\nlast\n")
         (result-two "{ \"matches\" : [1, 2] }\n")
         (assistant-two "")
         (result-three "Result C\n")
         (final-content "Done.\n\n")
         (request-id "request-roundtrip")
         (sections
          (concat
           (ende-chat--assistant-protocol-text
            2 "question-roundtrip" request-id ""
            (list :content assistant-one) first-calls)
           (ende-chat--tool-result-text 2 request-id (nth 0 first-calls)
					result-one)
           (ende-chat--tool-result-text 2 request-id (nth 1 first-calls)
					result-two)
           (ende-chat--assistant-protocol-text
            2 "question-roundtrip" request-id ""
            (list :content assistant-two) (list second-call))
           (ende-chat--tool-result-text 2 request-id second-call result-three)
           (ende-chat--assistant-protocol-text
            2 "question-roundtrip" request-id ""
            (list :content final-content) nil))))
    (with-temp-buffer
      (org-mode)
      (insert "* Topic\n** Question\nInspect these sources.\n")
      (insert sections)
      (insert "** Follow-up\nContinue from the tool results.\n")
      (ende-test--chat-goto "** Follow-up")
      (let* ((document (ende-chat--build-document))
             (headlines (ende-chat--document-headlines document))
             (messages (ende-chat-context-at-point))
             (protocol (ende-test--chat-protocol-messages messages))
             (assistant-headlines
              (cl-remove-if-not
               (lambda (headline)
                 (equal (org-element-property :raw-value headline)
                        "Agent: Reply"))
               headlines))
             (calls
              (cl-remove-if-not
               (lambda (headline)
                 (ende-chat--protocol-title-p headline "Tool Call:"))
               headlines))
             (results
              (cl-remove-if-not
               (lambda (headline)
                 (ende-chat--protocol-title-p headline "Tool Result:"))
               headlines)))
        (should (equal
                 (mapcar #'ende-test--chat-message-role messages)
                 '("user" "assistant" "tool" "tool" "assistant" "tool"
                   "assistant" "user")))
        (should (equal
                 (mapcar (lambda (message)
                           (if (eq (car-safe message) :role)
                               (plist-get message :role)
                             (car message)))
                         protocol)
                 '("assistant" "tool" "tool" "assistant" "tool")))
        (should (equal (plist-get (nth 0 protocol) :content) assistant-one))
        (should
         (equal (mapcar (lambda (call) (plist-get call :id))
                        (plist-get (nth 0 protocol) :tool-calls))
                '("call-A/01" "call-B:02")))
        (should
         (equal (mapcar (lambda (call) (plist-get call :arguments))
                        (plist-get (nth 0 protocol) :tool-calls))
                (mapcar (lambda (call) (plist-get call :arguments))
                        first-calls)))
        (should (equal (plist-get (nth 1 protocol) :content) result-one))
        (should (equal (plist-get (nth 2 protocol) :content) result-two))
        (should (equal (plist-get (nth 3 protocol) :content) assistant-two))
        (should (equal (plist-get (nth 4 protocol) :content) result-three))
        (should (equal (cdr (nth 6 messages)) final-content))
        (should (equal (mapcar (lambda (headline)
                                 (ende-chat--property
                                  document headline :tool-call-id))
                               calls)
                       '("call-A/01" "call-B:02" "call-C")))
        (should (equal (mapcar (lambda (headline)
                                 (ende-chat--property
                                  document headline :tool-call-id))
                               results)
                       '("call-A/01" "call-B:02" "call-C")))
        (should (equal (mapcar (lambda (headline)
                                 (ende-chat--property
                                  document headline :tool-name))
                               calls)
                       '("read_file" "search_project" "read_buffer")))
        (should (equal (mapcar (lambda (headline)
                                 (ende-chat--property
                                  document headline :tool-name))
                               results)
                       '("read_file" "search_project" "read_buffer")))
        (should (equal (mapcar (lambda (headline)
                                 (ende-chat--property
                                  document headline :request-id))
                               assistant-headlines)
                       (list request-id request-id request-id)))
        (should (equal (mapcar (lambda (headline)
                                 (org-element-property :level headline))
                               assistant-headlines)
                       '(2 2 2)))
        (should (equal (mapcar (lambda (headline)
                                 (org-element-property :level headline))
                               calls)
                       '(3 3 3)))
        (dolist (headline (append calls results))
          (should (equal (ende-chat--property
                          document headline :request-id)
                         request-id)))
        (should (eq (ende-chat--parent document (car calls))
                    (car assistant-headlines)))))))

(ert-deftest ende-test/chat-protocol-validation-rejects-malformed-and-unowned-results ()
  (with-temp-buffer
    (org-mode)
    (insert "* Topic\n** Question\nAsk\n")
    (insert
     (ende-chat--assistant-protocol-text
      2 "question-bad" "request-bad" ""
      '(:content "")
      '((:id "call-bad" :name "read_file" :arguments "{}"))))
    (insert
     (concat
      "** Tool Result: read_file\n:PROPERTIES:\n"
      ":ENDE_CHAT_REQUEST_ID: request-bad\n"
      ":ENDE_TOOL_NAME: read_file\n:END:\n\n{}\n\n"
      "** Follow-up\nAsk\n"))
    (ende-test--chat-goto "** Follow-up")
    (let ((failure
           (condition-case caught
               (progn (ende-chat-context-at-point) nil)
             (error (error-message-string caught)))))
      (should (string-match-p "Malformed generated tool result" failure))))
  (with-temp-buffer
    (org-mode)
    (insert "* Topic\n** Question\nAsk\n")
    (insert
     (ende-chat--assistant-protocol-text
      2 "question-duplicate" "request-duplicate" ""
      '(:content "")
      '((:id "same-call" :name "read_file" :arguments "{}")
        (:id "same-call" :name "search_project" :arguments "{}"))))
    (insert "** Follow-up\nAsk again.\n")
    (ende-test--chat-goto "** Follow-up")
    (let ((failure
           (condition-case caught
               (progn (ende-chat-context-at-point) nil)
             (error (error-message-string caught)))))
      (should (string-match-p "Duplicate generated tool-call ID" failure))))
  (ende-test--with-buffer
   (concat "* Topic\n** Tool Result: read_file\n"
           "This is a user-authored note, not protocol state.\n"
           "** Follow-up\nAsk about the note.\n")
   (org-mode)
   (ende-test--chat-goto "** Follow-up")
   (let* ((messages (ende-chat-context-at-point))
          (text (ende-test--chat-context-text messages)))
     (should (string-match-p "Tool Result: read_file" text))
     (should-not (ende-test--chat-protocol-messages messages)))))

(ert-deftest ende-test/chat-tool-context-drawer-errors-when-serialized-as-assistant ()
  (dolist (case '(("not JSON" "Invalid ENDE_CHAT_TOOL_CONTEXT JSON")
                  ("{\"version\":2,\"messages\":[]}"
                   "Unsupported ENDE_CHAT_TOOL_CONTEXT version")))
    (ende-test--reset)
    (ende-test--with-buffer
     (format
      (concat "* Topic\n"
	      "** Agent: Reply\n"
	      ":PROPERTIES:\n:ENDE_ROLE: assistant\n:END:\n"
	      ":%s:\n%s\n:END:\n\nAnswer\n"
	      "** Follow-up\nAsk\n")
      ende-chat--tool-context-drawer (car case))
     (org-mode)
     (ende-test--chat-goto "** Follow-up")
     (let ((failure
            (condition-case caught
                (progn (ende-chat-context-at-point) nil)
              (error (error-message-string caught)))))
       (should (string-match-p (cadr case) failure))))))

(ert-deftest ende-test/chat-tool-context-drawer-is-not-user-protocol-history ()
  (ende-test--reset)
  (ende-test--with-buffer
   (concat "* Topic\n"
           "** User note\n"
           ":PROPERTIES:\n:ENDE_ROLE: user\n:END:\n"
           ":ENDE_CHAT_TOOL_CONTEXT:\nnot JSON\n:END:\n"
           "Keep this ordinary note.\n"
           "** Follow-up\nAsk\n")
   (org-mode)
   (ende-test--chat-goto "** Follow-up")
   (let* ((messages (ende-chat-context-at-point))
          (text (ende-test--chat-context-text messages)))
     (should (string-match-p "Keep this ordinary note" text))
     (should-not (string-match-p "not JSON" text))
     (should-not (ende-test--chat-protocol-messages messages)))))

(ert-deftest ende-test/chat-work-container-is-only-created-for-tool-loops ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '("A plain answer.") t)))
    (ende-test--with-buffer
     "* Topic\n** Question\nAnswer this.\n"
     (org-mode)
     (ende-test--chat-goto "** Question")
     (let ((job (ende-ask)))
       (ende-test--complete-call (car ende-test--calls))
       (should (eq (ende-job-state job) 'completed)))
     (let* ((document (ende-chat--build-document))
            (headlines (ende-chat--document-headlines document)))
       (should-not
        (cl-some (lambda (headline)
                   (ende-chat--work-section-p document headline))
                 headlines))
       (should (= (cl-count-if
                   (lambda (headline)
                     (and (equal (org-element-property :raw-value headline)
                                 "Agent: Reply")
                          (equal (ende-chat--property document headline :role)
                                 "assistant")))
                   headlines)
                  1))))))

(ert-deftest ende-test/chat-user-authored-agent-work-heading-is-ordinary-content ()
  (ende-test--reset)
  (ende-test--with-buffer
   (concat "* Topic\n"
           "** User\nA question.\n"
           "** Agent: Work\nThese are ordinary user notes.\n"
           "** Follow-up\nContinue.\n")
   (org-mode)
   (ende-test--chat-goto "** Follow-up")
   (let* ((document (ende-chat--build-document))
          (work (cl-find-if
                 (lambda (headline)
                   (equal (org-element-property :raw-value headline)
                          "Agent: Work"))
                 (ende-chat--document-headlines document)))
          (messages (ende-chat-context-at-point)))
     (should work)
     (should-not (ende-chat--work-section-p document work))
     (let ((work-message (car (last messages 2))))
       (should (string-match-p "Agent: Work"
			       (ende-test--chat-context-text
                                (list work-message))))
       (should (string-match-p "These are ordinary user notes"
			       (ende-test--chat-context-text
                                (list work-message))))))))

(ert-deftest ende-test/chat-work-validation-rejects-duplicate-request-containers ()
  (let* ((request-id "request-duplicate-work")
         (question-id "question-duplicate-work")
         (first-call '(:id "call-first" :name "read_file" :arguments "{}"))
         (second-call '(:id "call-second" :name "read_file" :arguments "{}"))
         (protocol
          (concat
           (ende-chat--work-text 2 question-id request-id)
           (ende-chat--assistant-protocol-text
            3 question-id request-id "" '(:content "First round")
            (list first-call))
           (ende-chat--tool-result-text 3 request-id first-call "First result")
           (ende-chat--work-text 2 question-id request-id)
           (ende-chat--assistant-protocol-text
            3 question-id request-id "" '(:content "Second round")
            (list second-call))
           (ende-chat--tool-result-text 3 request-id second-call "Second result")
           (ende-chat--assistant-protocol-text
            2 question-id request-id "" '(:content "Final") nil nil 'org))))
    (with-temp-buffer
      (org-mode)
      (insert "* Topic\n** User\nQuestion\n")
      (insert protocol)
      (insert "** Follow-up\nContinue\n")
      (ende-test--chat-goto "** Follow-up")
      (let ((failure
             (condition-case caught
                 (progn (ende-chat-context-at-point) nil)
               (error (error-message-string caught)))))
        (should (string-match-p "Multiple generated Agent: Work sections"
                                failure))))))

(ert-deftest ende-test/chat-tool-history-survives-follow-up-and-compaction ()
  (let ((root (make-temp-file "ende-chat-history-" t)))
    (ende-test--reset)
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "foo.el" root)
            (insert "FOO_SOURCE_TOKEN\n(defun foo (x) x)\n"))
          (with-temp-file (expand-file-name "bar.el" root)
            (insert "BAR_SOURCE_TOKEN\n(defun bar (x) x)\n"))
          (with-temp-file (expand-file-name "baz.el" root)
            (insert "BAZ_SOURCE_TOKEN\n(defun baz (x) x)\n"))
          (let ((ende-llm-function
                 (ende-test--fake-llm
                  (list
                   (list :tool-calls
                         (list (list :id "call-A" :name "read_file"
                                     :arguments "{ \"path\" : \"foo.el\" }")
                               (list :id "call-B" :name "read_file"
                                     :arguments "{\"path\":\"bar.el\"}")))
                   (list :tool-calls
                         (list (list :id "call-C" :name "read_file"
                                     :arguments "{\"path\":\"baz.el\"}")))
                   "Final answer about the source."
                   "Follow-up answer without another lookup."
                   "Compact summary only.")
                  t)))
            (ende-test--with-buffer
	     (format
	      (concat
	       "#+begin_ende\n(root %S)\n"
	       "(group inspect project_info list_project_files read_file search_project)\n"
	       "(resource project (include \"**\"))\n"
	       "(tools inspect)\n"
	       "(set-tool-resources inspect :resources project)\n"
	       "#+end_ende\n\n"
	       "* Topic\n"
	       "** Question\nRead foo.el and bar.el.\n"
	       "** Follow-up\nWhat would happen if foo received nil?\n"
	       "** Compact\nKeep the important source behavior.\n"
	       "** Later\nContinue from the summary.\n")
	      root)
             (org-mode)
             (ende-test--chat-goto "** Question")
             (let ((job (ende-ask)))
	       (ende-test--complete-call (nth 0 ende-test--calls))
	       (should (= (length ende-test--calls) 2))
	       (ende-test--complete-call (nth 1 ende-test--calls))
	       (should (= (length ende-test--calls) 3))
	       (ende-test--complete-call (nth 2 ende-test--calls))
	       (should (eq (ende-job-state job) 'completed)))
             (ende-test--chat-goto "** Follow-up")
             (beginning-of-line)
             (insert "** Tool Result: read_file\n"
                     "This is a user-authored note.\n\n"
                     "** Agent: Work\n"
                     "This heading is user-authored.\n\n")
             (let* ((first-round
                     (last (plist-get (nth 1 ende-test--calls) :messages) 3))
                    (second-round
                     (last (plist-get (nth 2 ende-test--calls) :messages) 2))
                    (expected-transcript (append first-round second-round))
                    (document (ende-chat--build-document))
                    (headlines (ende-chat--document-headlines document))
                    (reply
                     (cl-find-if
                      (lambda (headline)
                        (and (equal (org-element-property :raw-value headline)
                                    "Agent: Reply")
                             (null (ende-chat--work-ancestor
                                    document headline))))
                      headlines))
                    (works
                     (cl-remove-if-not
                      (lambda (headline)
                        (ende-chat--work-section-p document headline))
                      headlines))
                    (work (car works))
                    (protocol-headlines
                     (cl-remove-if-not
                      (lambda (headline)
                        (or (equal (org-element-property :raw-value headline)
                                   "Agent: Reply")
                            (and (ende-chat--protocol-title-p
                                  headline "Tool Call:")
                                 (ende-chat--tool-call-protocol-p
                                  document headline))
                            (ende-chat--tool-result-protocol-p document headline)))
                      headlines))
                    (calls
                     (cl-remove-if-not
                      (lambda (headline)
                        (and (ende-chat--protocol-title-p
                              headline "Tool Call:")
                             (ende-chat--tool-call-protocol-p
                              document headline)))
                      headlines))
                    (results
                     (cl-remove-if-not
                      (lambda (headline)
                        (and (ende-chat--protocol-title-p
                              headline "Tool Result:")
                             (ende-chat--tool-result-protocol-p
                              document headline)))
                      headlines)))
	       (should reply)
	       (should (= (length works) 1))
	       (should (equal (ende-chat--property document work :request-id)
                              (ende-chat--property document reply :request-id)))
	       (should (eq (ende-chat--parent document work)
                           (ende-chat--parent document reply)))
	       (should (= (org-element-property :level work)
                          (org-element-property :level reply)))
	       (should (< (org-element-property :begin work)
                          (org-element-property :begin reply)))
	       (should
                (eq reply
                    (cadr (memq work
                                (ende-chat--children
                                 document (ende-chat--parent document work))))))
	       (dolist (child (ende-chat--children document work))
                 (should (equal (ende-chat--property document child :request-id)
                                (ende-chat--property document work :request-id))))
	       (should-not (cl-some
                            (lambda (drawers) drawers)
                            (hash-table-values
                             (ende-chat--document-tool-context-drawers
                              document))))
	       (should
                (equal (mapcar (lambda (headline)
                                 (org-element-property :raw-value headline))
			       protocol-headlines)
		       '("Agent: Reply"
                         "Tool Call: read_file" "Tool Call: read_file"
                         "Tool Result: read_file" "Tool Result: read_file"
                         "Agent: Reply" "Tool Call: read_file"
                         "Tool Result: read_file" "Agent: Reply")))
	       (should (equal
                        (mapcar (lambda (message)
                                  (if (equal (plist-get message :role) "tool")
                                      (plist-get message :tool-call-id)
                                    (mapcar (lambda (call)
                                              (plist-get call :id))
                                            (plist-get message :tool-calls))))
				expected-transcript)
                        '(("call-A" "call-B") "call-A" "call-B"
                          ("call-C") "call-C")))
	       (should (equal (mapcar (lambda (headline)
                                        (ende-chat--property
                                         document headline :tool-call-id))
                                      calls)
                              '("call-A" "call-B" "call-C")))
	       (should (equal (mapcar (lambda (headline)
                                        (ende-chat--property
                                         document headline :tool-call-id))
                                      results)
                              '("call-A" "call-B" "call-C")))
	       (should
                (ende-test--chat-headline-text-hidden-p
                 work "FOO_SOURCE_TOKEN"))
	       (should-not
                (ende-test--chat-headline-text-hidden-p
                 reply "Final answer about the source."))
	       (let ((user-note
                      (cl-find-if
		       (lambda (headline)
                         (and (equal (org-element-property :raw-value headline)
                                     "Tool Result: read_file")
                              (null (ende-chat--property
                                     document headline :tool-call-id))))
		       headlines)))
                 (should user-note)
                 (should-not
                  (ende-test--chat-headline-text-hidden-p
                   user-note "This is a user-authored note.")))
	       (ende-test--chat-goto "** Follow-up")
	       (let* ((messages (ende-chat-context-at-point))
                      (protocol (ende-test--chat-protocol-messages messages))
                      (reply-index
		       (cl-position-if
			(lambda (message)
			  (and (consp message) (stringp (car message))
			       (equal (car message) "assistant")
			       (string-match-p "Final answer about the source"
					       (cdr message))))
			messages)))
		 (should (equal protocol expected-transcript))
		 (should (= reply-index (1+ (cl-position (car (last protocol))
                                                         messages :test #'equal))))
		 (should (equal (ende-test--chat-message-role (car (last messages)))
				"user"))
		 (should
                  (cl-some
                   (lambda (message)
                     (and (consp message) (stringp (car message))
                          (string-match-p "This heading is user-authored"
                                          (cdr message))))
                   messages))
		 (should
                  (equal (mapcar #'ende-test--chat-message-role messages)
			 '("user" "user" "assistant" "tool" "tool"
                           "assistant" "tool" "assistant" "user" "user")))
		 (ende-test--chat-goto "** Agent: Work")
		 (org-fold-show-subtree)
		 (ende-test--chat-goto "** Follow-up")
		 (should (equal messages (ende-chat-context-at-point)))
		 (ende-test--chat-goto "** Agent: Work")
		 (org-fold-hide-subtree)
		 (ende-test--chat-goto "** Follow-up")
		 (let ((follow-up (ende-ask)))
                   (should (eq (ende-job-state follow-up) 'running))
                   (ende-test--complete-call (nth 3 ende-test--calls))
                   (should (eq (ende-job-state follow-up) 'completed)))
		 (should-not
                  (string-match-p "ENDE_CHAT_TOOL_CONTEXT" (buffer-string))))
	       (ende-test--chat-goto "** Compact")
	       (let ((compaction (ende-compact)))
		 (should (= (length ende-test--calls) 5))
		 (let ((compaction-protocol
			(ende-test--chat-protocol-messages
			 (plist-get (nth 4 ende-test--calls) :messages))))
                   (should
                    (equal compaction-protocol expected-transcript)))
		 (ende-test--complete-call (nth 4 ende-test--calls))
		 (should (eq (ende-job-state compaction) 'completed)))
	       (ende-test--chat-goto "** Later")
	       (let* ((messages (ende-chat-context-at-point))
                      (text (ende-test--chat-context-text messages)))
		 (should (string-match-p "Compact summary only" text))
		 (should-not (ende-test--chat-protocol-messages messages))
		 (should-not (string-match-p "FOO_SOURCE_TOKEN" text))
		 (should-not (string-match-p "call-A" text)))))))
      (ende-test--reset)
      (when (file-directory-p root)
	(delete-directory root t)))))

(ert-deftest ende-test/chat-tool-failure-keeps-completed-protocol-history ()
  (let ((root (make-temp-file "ende-chat-partial-tools-" t)))
    (ende-test--reset)
    (unwind-protect
        (let ((ende-llm-function
               (ende-test--fake-llm
                (list
                 (list :tool-calls
                       (list (list :id "call-A" :name "read_file"
                                   :arguments "{\"path\":\"a.el\"}")
                             (list :id "call-B" :name "read_file"
                                   :arguments "{\"path\":\"b.el\"}")))
                 "This must not be requested")
                t)))
          (ende-test--with-buffer
           (format (concat "#+begin_ende\n(root %S)\n"
                           "(group inspect read_file)\n"
                           "(resource project (include \"**\"))\n"
                           "(tools inspect)\n"
                           "(set-tool-resources inspect :resources project)\n"
                           "#+end_ende\n\n"
                           "* Topic\n** Question\nRead both files.\n"
                           "** Follow-up\nAsk about the results.\n")
                   root)
           (org-mode)
           (ende-test--chat-goto "** Question")
           (let ((job (ende-ask))
                 (completed nil))
             (cl-letf
                 (((symbol-function 'ende-chat--tool-response)
                   (lambda (call _configuration)
                     (if (equal (ende-chat--tool-value call :id) "call-A")
                         (progn (setq completed t) "result-A")
		       (error "second tool failed")))))
	       (ende-test--complete-call (car ende-test--calls)))
             (should completed)
             (should (eq (ende-job-state job) 'failed))
             (let* ((document (ende-chat--build-document))
                    (work
                     (cl-find-if
                      (lambda (headline)
                        (ende-chat--work-section-p document headline))
                      (ende-chat--document-headlines document)))
                    (reply
                     (cl-find-if
                      (lambda (headline)
                        (and (equal (org-element-property :raw-value headline)
                                    "Agent: Reply")
                             (equal (ende-chat--property document headline :request-id)
                                    (plist-get (ende-job-instruction job) :request-id))
                             (null (ende-chat--work-ancestor document headline))))
                      (ende-chat--document-headlines document)))
                    (intermediate
                     (cl-find-if
                      (lambda (headline)
                        (and (equal (org-element-property :raw-value headline)
                                    "Agent: Reply")
                             (eq (ende-chat--work-ancestor document headline)
                                 work)))
                      (ende-chat--document-headlines document)))
                    (calls
                     (cl-remove-if-not
                      (lambda (headline)
                        (ende-chat--protocol-title-p headline "Tool Call:"))
                      (ende-chat--document-headlines document)))
                    (results
                     (cl-remove-if-not
                      (lambda (headline)
                        (ende-chat--protocol-title-p
                         headline "Tool Result:"))
                      (ende-chat--document-headlines document)))
                    (message
                     (ende-chat--protocol-assistant-message document intermediate)))
	       (should work)
	       (should-not reply)
	       (should (equal (ende-chat--property document work :state)
                              "failed"))
	       (should (equal (mapcar (lambda (call)
                                        (ende-chat--property
                                         document call :tool-call-id))
                                      calls)
                              '("call-A" "call-B")))
	       (should (equal (mapcar (lambda (result)
                                        (ende-chat--property
                                         document result :tool-call-id))
                                      results)
                              '("call-A")))
	       (should (equal (plist-get message :tool-calls)
                              '((:id "call-A" :name "read_file"
                                     :arguments "{\"path\":\"a.el\"}")
                                (:id "call-B" :name "read_file"
                                     :arguments "{\"path\":\"b.el\"}"))))
	       (should-not
                (string-match-p "This must not be requested" (buffer-string)))))))
      (ende-test--reset)
      (when (file-directory-p root)
        (delete-directory root t)))))

(defun ende-test--chat-unpartitioned-text (document question)
  "Return the selected Org content for QUESTION without message boundaries."
  (let* ((root (ende-chat--root document question))
         (root-begin (org-element-property :begin root))
         (root-end (org-element-property :end root))
         (question-begin (org-element-property :begin question))
         (prefix
          (cl-remove-if-not
           (lambda (headline)
             (let ((begin (org-element-property :begin headline)))
               (and (>= begin root-begin)
                    (< begin question-begin)
                    (< begin root-end)
                    (ende-chat--eligible-p document headline))))
           (ende-chat--document-headlines document)))
         (current (ende-chat--current-headlines document question)))
    (concat (mapconcat (lambda (headline)
                         (ende-chat--section-text document headline))
                       prefix "")
            (mapconcat (lambda (headline)
                         (ende-chat--section-text document headline))
                       current ""))))

(defun ende-test--chat-response-count ()
  "Return the number of generated Agent: Reply headlines in the buffer."
  (cl-count-if
   (lambda (headline)
     (string= (org-element-property :raw-value headline) "Agent: Reply"))
   (org-element-map (org-element-parse-buffer) 'headline #'identity)))

(defun ende-test--chat-control-drawers ()
  "Return the Ende control property drawers in the current buffer."
  (cl-remove-if-not
   (lambda (drawer)
     (cl-every #'ende-chat--reserved-property-p
               (org-element-map drawer 'node-property #'identity)))
   (org-element-map (org-element-parse-buffer) 'property-drawer #'identity)))

(defun ende-test--chat-control-drawer-hidden-p (drawer)
  "Return non-nil when DRAWER's first property line is folded."
  (save-excursion
    (goto-char (org-element-property :begin drawer))
    (forward-line 1)
    (invisible-p (point))))

(defun ende-test--chat-response-headlines ()
  "Return generated Agent: Reply headlines in document order."
  (cl-remove-if-not
   (lambda (headline)
     (string= (org-element-property :raw-value headline) "Agent: Reply"))
   (org-element-map (org-element-parse-buffer) 'headline #'identity)))

(defun ende-test--chat-response-has-separator-p (headline)
  "Return non-nil when HEADLINE has a blank line before it."
  (save-excursion
    (goto-char (org-element-property :begin headline))
    (forward-line -1)
    (looking-at-p "^[[:space:]]*$")))

(defun ende-test--chat-reasoning-headlines ()
  "Return generated Agent: Reasoning headlines in document order."
  (cl-remove-if-not
   (lambda (headline)
     (string= (org-element-property :raw-value headline) "Agent: Reasoning"))
   (org-element-map (org-element-parse-buffer) 'headline #'identity)))

(defun ende-test--chat-headline-property (headline property)
  "Return PROPERTY from HEADLINE."
  (save-excursion
    (goto-char (org-element-property :begin headline))
    (org-entry-get nil property)))

(defun ende-test--chat-headline-text-hidden-p (headline text)
  "Return non-nil when TEXT in HEADLINE's subtree is folded."
  (save-excursion
    (goto-char (org-element-property :begin headline))
    (when (search-forward text (org-element-property :end headline) t)
      (invisible-p (1- (point))))))

(ert-deftest ende-test/chat-context-isolates-level-one-roots ()
  (ende-test--reset)
  (ende-test--with-buffer
   (concat "* First root\n"
           "First root material\n"
           "** First question\n"
           "First question\n"
           "* Second root\n"
           "Second root material\n"
           "** Second question\n"
           "Second question\n")
   (org-mode)
   (ende-test--chat-goto "** Second question")
   (let* ((messages (ende-chat-context-at-point))
          (text (ende-test--chat-context-text messages)))
     (should (string-match-p "Second root material" text))
     (should (string-match-p "Second question" text))
     (should-not (string-match-p "First root material" text))
     (should-not (string-match-p "First question" text)))))

(ert-deftest ende-test/chat-hidden-and-reasoning-obey-independent-policies ()
  (ende-test--reset)
  (ende-test--with-buffer
   (concat "* Root\n"
           "** Assistant material\n"
           ":PROPERTIES:\n"
           ":ENDE_ROLE: assistant\n"
           ":END:\n"
           "Answer\n"
           "*** Hidden assistant descendant\n"
           ":PROPERTIES:\n"
           ":ENDE_VISIBILITY: hidden\n"
           ":END:\n"
           "Never send this\n"
           "*** Reasoning section\n"
           ":PROPERTIES:\n"
           ":ENDE_VISIBILITY: reasoning\n"
           ":END:\n"
           "Section thought\n"
           "**** Hidden reasoning descendant\n"
           ":PROPERTIES:\n"
           ":ENDE_VISIBILITY: hidden\n"
           ":END:\n"
           "Still never send this\n"
           "** Question\n"
           "Ask\n")
   (org-mode)
   (ende-test--chat-goto "** Question")
   (let ((without-reasoning
          (ende-test--chat-context-text
           (let ((ende-chat-include-reasoning nil))
             (ende-chat-context-at-point)))))
     (should (string-match-p "Answer" without-reasoning))
     (should-not (string-match-p "Section thought" without-reasoning))
     (should-not (string-match-p "Never send this" without-reasoning)))
   (let ((with-reasoning
          (ende-test--chat-context-text
           (let ((ende-chat-include-reasoning t))
             (ende-chat-context-at-point)))))
     (should (string-match-p "Section thought" with-reasoning))
     (should-not (string-match-p "Never send this" with-reasoning)))))

(ert-deftest ende-test/chat-role-inheritance-is-overridden-explicitly ()
  (ende-test--reset)
  (ende-test--with-buffer
   (concat "* Root\n"
           "** Assistant section\n"
           ":PROPERTIES:\n"
           ":ENDE_ROLE: assistant\n"
           ":END:\n"
           "Assistant answer\n"
           "*** Inherited child\n"
           "Inherited assistant text\n"
           "*** Explicit user child\n"
           ":PROPERTIES:\n"
           ":ENDE_ROLE: user\n"
           ":END:\n"
           "User reference text\n"
           "** Question\n"
           "Ask\n")
   (org-mode)
   (ende-test--chat-goto "** Question")
   (let* ((messages (ende-chat-context-at-point))
          (roles (mapcar #'car messages))
          (text (ende-test--chat-context-text messages)))
     (should (equal roles '("user" "assistant" "user" "user")))
     (should (string-match-p "Assistant answer" text))
     (should (string-match-p "Inherited assistant text" text))
     (should (string-match-p "User reference text" text))
     (should (string-match-p "Inherited assistant text"
                             (cdr (nth 1 messages))))
     (should (equal (car (car (last messages))) "user")))))

(ert-deftest ende-test/chat-prefix-messages-follow-explicit-role-boundaries ()
  (ende-test--reset)
  (ende-test--with-buffer
   (concat "* Topic\n"
           "Root source.\n"
           "** Reference\n"
           "Reference source.\n"
           "*** Nested reference\n"
           "Nested reference source.\n"
           "** Assistant response\n"
           ":PROPERTIES:\n"
           ":ENDE_ROLE: assistant\n"
           ":END:\n"
           "Assistant source.\n"
           "*** Inherited assistant section\n"
           "Inherited assistant source.\n"
           "**** Deeper inherited section\n"
           "Deeper assistant source.\n"
           "*** Explicit user section\n"
           ":PROPERTIES:\n"
           ":ENDE_ROLE: user\n"
           ":END:\n"
           "Nested user source.\n"
           "**** Inherited user section\n"
           "Inherited user source.\n"
           "*** Resumed assistant section\n"
           "Resumed assistant source.\n"
           "*** Explicit same-role section\n"
           ":PROPERTIES:\n"
           ":ENDE_ROLE: assistant\n"
           ":END:\n"
           "Same-role boundary source.\n"
           "**** Inherited from same-role section\n"
           "Same-role descendant source.\n"
           "*** Resumed after same-role section\n"
           "Assistant after nested section source.\n"
           "** Question\n"
           "Current question source.\n"
           "** Later sibling\n"
           "Later sibling source.\n")
   (org-mode)
   (ende-test--chat-goto "** Question")
   (let* ((document (ende-chat--build-document))
          (question (ende-chat--headline-at-point document))
          (messages (ende-chat--serialize document question))
          (roles (mapcar #'car messages))
          (contents (mapcar #'cdr messages))
          (serialized-text (mapconcat #'identity contents "")))
     (should (equal roles
                    '("user" "assistant" "user" "assistant"
                      "assistant" "assistant" "user")))
     (should (equal serialized-text
                    (ende-test--chat-unpartitioned-text document question)))
     (should (string-match-p "Root source" (nth 0 contents)))
     (should (string-match-p "Nested reference source" (nth 0 contents)))
     (should-not (string-match-p "Assistant source" (nth 0 contents)))
     (should (string-match-p "Inherited assistant source" (nth 1 contents)))
     (should (string-match-p "Deeper assistant source" (nth 1 contents)))
     (should-not (string-match-p "Nested user source" (nth 1 contents)))
     (should (string-match-p "Nested user source" (nth 2 contents)))
     (should (string-match-p "Inherited user source" (nth 2 contents)))
     (should (string-match-p "Resumed assistant source" (nth 3 contents)))
     (should (string-match-p "Same-role boundary source" (nth 4 contents)))
     (should (string-match-p "Same-role descendant source" (nth 4 contents)))
     (should (string-match-p "Assistant after nested section source"
                             (nth 5 contents)))
     (should (string-match-p "Current question source" (nth 6 contents)))
     (should-not (string-match-p "Later sibling source" serialized-text)))))

(ert-deftest ende-test/chat-current-question-skips-independent-child-subtrees ()
  (ende-test--reset)
  (ende-test--with-buffer
   (concat "* Root\n"
           "** Question\n"
           "Question body\n"
           "*** Unmarked reference\n"
           "Include this reference\n"
           "*** Explicit user child\n"
           ":PROPERTIES:\n"
           ":ENDE_ROLE: user\n"
           ":END:\n"
           "Do not include this turn\n"
           "*** Assistant child\n"
           ":PROPERTIES:\n"
           ":ENDE_ROLE: assistant\n"
           ":END:\n"
           "Do not include this answer\n"
           "*** Hidden child\n"
           ":PROPERTIES:\n"
           ":ENDE_VISIBILITY: hidden\n"
           ":END:\n"
           "Do not include this private note\n"
           "*** Later unmarked reference\n"
           "Include this later reference\n"
           "** Later sibling\n"
           "Must not enter this request\n")
   (org-mode)
   (ende-test--chat-goto "** Question")
   (let ((text (ende-test--chat-context-text
                (ende-chat-context-at-point))))
     (should (string-match-p "Question body" text))
     (should (string-match-p "Include this reference" text))
     (should (string-match-p "Include this later reference" text))
     (should-not (string-match-p "Do not include this turn" text))
     (should-not (string-match-p "Do not include this answer" text))
     (should-not (string-match-p "Do not include this private note" text))
     (should-not (string-match-p "Must not enter this request" text)))))

(ert-deftest ende-test/chat-root-prefix-policy-can-be-narrowed ()
  (ende-test--reset)
  (ende-test--with-buffer
   (concat "* Root\n"
           "Earlier material\n"
           "** Question\n"
           "Current material\n")
   (org-mode)
   (ende-test--chat-goto "** Question")
   (let ((root-prefix (ende-test--chat-context-text
		       (let ((ende-chat-context-policy 'root-prefix))
                         (ende-chat-context-at-point))))
         (current-only (ende-test--chat-context-text
                        (let ((ende-chat-context-policy
			       'current-section-only))
                          (ende-chat-context-at-point)))))
     (should (string-match-p "Earlier material" root-prefix))
     (should-not (string-match-p "Earlier material" current-only))
     (should (string-match-p "Current material" current-only)))))

(ert-deftest ende-test/chat-context-fences-substitute-history-by-scope ()
  (ende-test--reset)
  (let ((ende-chat-context-policy 'root-prefix)
        (ende-chat-include-reasoning nil))
    (ende-test--with-buffer
     (concat "* Q0\nOriginal root context\n"
	     "** R1\n:PROPERTIES:\n:ENDE_ROLE: assistant\n:END:\n"
	     "** C0\n:PROPERTIES:\n:ENDE_ROLE: assistant\n"
	     ":ENDE_CHAT_CONTEXT_FENCE: t\n:END:\n"
	     "** Q01\n:PROPERTIES:\n:ENDE_ROLE: user\n:END:\n"
	     "** R01\n:PROPERTIES:\n:ENDE_ROLE: assistant\n:END:\n"
	     "*** Q11\n:PROPERTIES:\n:ENDE_ROLE: user\n:END:\n"
	     "*** R11\n:PROPERTIES:\n:ENDE_ROLE: assistant\n:END:\n"
	     "*** C1\n:PROPERTIES:\n:ENDE_ROLE: assistant\n"
	     ":ENDE_CHAT_CONTEXT_FENCE: t\n:END:\n"
	     "**** Summary detail\nRemember this detail\n"
	     "*** Instructions\n:PROPERTIES:\n"
	     ":ENDE_VISIBILITY: hidden\n:END:\nNever send this\n"
	     "*** Q1\n:PROPERTIES:\n:ENDE_ROLE: user\n:END:\n"
	     "**** Reference\nCurrent question reference\n"
	     "*** R12\n:PROPERTIES:\n:ENDE_ROLE: assistant\n:END:\n"
	     "** Q2\n:PROPERTIES:\n:ENDE_ROLE: user\n:END:\n"
	     "** R02\n:PROPERTIES:\n:ENDE_ROLE: assistant\n:END:\n"
	     "*** Q21\n:PROPERTIES:\n:ENDE_ROLE: user\n:END:\n"
	     "**** Q211\n:PROPERTIES:\n:ENDE_ROLE: user\n:END:\n"
	     "** C2\n:PROPERTIES:\n:ENDE_ROLE: assistant\n"
	     ":ENDE_CHAT_CONTEXT_FENCE: t\n:END:\n"
	     "** Q3\n:PROPERTIES:\n:ENDE_ROLE: user\n:END:\n"
	     "* Other\n** Q4\n")
     (org-mode)
     ;; These queries exercise a local fence, an ancestor fence, an earlier
     ;; branch at the same depth, later fences, and an independent root.
     (dolist (case
	      '(("*** Q11" "C0" "Q01" "R01" "Q11")
                ("*** Q1" "C1" "Summary detail" "Q1" "Reference")
                ("** Q2" "C0" "Q01" "R01" "C1" "Summary detail"
                 "Q1" "Reference" "R12" "Q2")
                ("*** Q21" "C0" "Q01" "R01" "C1" "Summary detail"
                 "Q1" "Reference" "R12" "Q2" "R02" "Q21")
                ("**** Q211" "C0" "Q01" "R01" "C1" "Summary detail"
                 "Q1" "Reference" "R12" "Q2" "R02" "Q21" "Q211")
                ("** Q3" "C2" "Q3")
                ("** Q4" "Other" "Q4")))
       (ende-test--chat-goto (car case))
       (let* ((messages (ende-chat-context-at-point))
              (text (ende-test--chat-context-text messages)))
         (ert-info ((format "Context at %s" (car case)))
           (should (equal (ende-test--chat-context-headings messages)
                          (cdr case)))
           (should (equal (caar (last messages)) "user"))
           (should-not (string-match-p "Original root context" text))
           (should-not (string-match-p "Never send this" text))
           (should-not (string-match-p "ENDE_CHAT_CONTEXT_FENCE" text)))))
     ;; An unfinished result cannot replace history, even if it was marked.
     (dolist (state '("pending" "failed"))
       (ende-test--chat-goto "** C2")
       (org-entry-put nil ende-chat-state-property state)
       (org-entry-put nil ende-chat-visibility-property "hidden")
       (ende-test--chat-goto "** Q3")
       (should
        (equal (ende-test--chat-context-headings (ende-chat-context-at-point))
	       '("C0" "Q01" "R01" "C1" "Summary detail" "Q1" "Reference"
                 "R12" "Q2" "R02" "Q21" "Q211" "Q3"))))
     (let ((ende-chat-context-policy 'current-section-only))
       (ende-test--chat-goto "*** Q1")
       (should
        (equal (ende-test--chat-context-headings (ende-chat-context-at-point))
	       '("Q1" "Reference")))))))

(ert-deftest ende-test/chat-compaction-preserves-instructions-and-org-layout ()
  (dolist (level '(2 4))
    (dolist (input '(("" "")
                     ("" "Keep the decisions.")
                     ("My summary" "")
                     ("My summary" "Keep the decisions.")
                     ("" "" "A note added during generation.")))
      (ende-test--reset)
      (let* ((title (nth 0 input))
             (body (nth 1 input))
             (late-note (nth 2 input))
             (ende-chat-compact-instruction "Keep the essentials.")
             (ende-llm-function
              (ende-test--fake-llm
               '((:content "#+ENDE_CHAT_TITLE: Ignore this title\n* Facts\nSummary"
			   :thinking "Plan")) t)))
        (ende-test--with-buffer "* Topic\nEarlier conversation\n"
				(org-mode)
				(goto-char (point-max))
				(cl-loop for depth from 2 below level
					 do (insert (make-string depth ?*) " Ancestor\n"))
				(let ((start (point)))
				  (insert (make-string level ?*) " " title "\n" body "\n"
					  (make-string level ?*) " Next\nContinue\n")
				  (goto-char start)
				  (let* ((job (ende-compact '(:name local :model "compact-model")))
					 (question-id (plist-get (ende-job-instruction job) :question-id))
					 (call (car ende-test--calls))
					 (messages (plist-get call :messages)))
				    (should (eq (ende-job-state job) 'running))
				    (should (string-match-p
					     (regexp-quote (if (string-empty-p body)
							       ende-chat-compact-instruction body))
					     (cdar (last messages))))
				    (should-not (string-match-p "ENDE_CHAT_TITLE" (cdar messages)))
				    (when (and (not (string-empty-p title))
					       (string-empty-p body))
				      (should-not (string-match-p (regexp-quote title)
								  (cdar (last messages)))))
				    (when late-note
				      (org-end-of-meta-data t)
				      (insert late-note "\n"))
				    (ende-test--complete-call call)
				    (should (eq (ende-job-state job) 'completed))
				    (let* ((document (ende-chat--build-document))
					   (question (ende-chat--find-by-property
						      document ende-chat-id-property question-id))
					   (fence (ende-chat--find-by-property
						   document ende-chat-request-id-property
						   (plist-get (ende-job-instruction job) :request-id)))
					   (reasoning (ende-chat--find-by-property
						       document ende-chat-reasoning-for-property
						       (plist-get (ende-job-instruction job)
								  :request-id))))
				      (should (ende-chat--context-fence-p document fence))
				      (should (= (org-element-property :level fence) level))
				      (should (equal (org-element-property :raw-value fence)
						     "Agent: Context Fence"))
				      (should (eq (ende-chat--parent document reasoning) fence))
				      (should-not (ende-chat--context-fence-p document reasoning))
				      (should (equal (ende-test--chat-headline-property
						      fence ende-chat-model-property)
						     "compact-model"))
				      (if (and (string-empty-p title) (string-empty-p body)
					       (not late-note))
					  (should-not question)
					(should question)
					(should (equal (org-element-property :raw-value question)
						       (if (string-empty-p title)
							   "User: Context Fence Instructions" title)))
					(should (equal (ende-chat--property document question :visibility)
						       "hidden"))
					(should (= (org-element-property :end question)
						   (org-element-property :begin fence)))
					(should (string-match-p (regexp-quote (or late-note body))
								(buffer-string))))
				      (should-not (string-match-p "Ignore this title" (buffer-string)))
				      (should (cl-every #'ende-test--chat-control-drawer-hidden-p
							(ende-test--chat-control-drawers))))
				    (ende-test--chat-goto (concat (make-string level ?*) " Next"))
				    (let ((ende-chat-include-reasoning nil))
				      (should
				       (equal (ende-test--chat-context-headings
					       (ende-chat-context-at-point))
					      '("Agent: Context Fence" "Facts" "Next")))))))))))

(ert-deftest ende-test/chat-compaction-and-asks-share-live-context ()
  (ende-test--reset)
  (let ((ende-provider-profiles '((local :parallel t :max-concurrency 2)))
        (ende-llm-function
         (ende-test--fake-llm
          '("First summary" "New answer" "Second summary" "Final answer"
            "Third summary") t)))
    (ende-test--with-buffer
     (concat "* Topic\nOriginal history\n"
	     "** First compaction\nKeep decisions\n"
	     "** Follow up\nNew question\n"
	     "** Second compaction\nKeep commands\n"
	     "** Final question\nContinue\n")
     (org-mode)
     (ende-test--chat-goto "** First compaction")
     (let ((first (ende-compact)))
       (ende-test--chat-goto "** Follow up")
       (let ((ask (ende-ask)))
         (should (eq (ende-job-state ask) 'queued))
         (ende-test--complete-call (nth 0 ende-test--calls))
         (should (eq (ende-job-state first) 'completed))
         (should (eq (ende-job-state ask) 'running))
         (let ((prompt (ende-test--chat-context-text
                        (plist-get (nth 1 ende-test--calls) :messages))))
           (should (string-match-p "First summary" prompt))
           (should-not (string-match-p "Original history" prompt))
           (should-not (string-match-p "Keep decisions" prompt)))
         (ende-test--chat-goto "** Second compaction")
         (let ((second (ende-compact)))
           (should (eq (ende-job-state second) 'queued))
           (ende-test--chat-goto "** Follow up")
           (org-end-of-meta-data t)
           (insert "Latest detail\n")
           (ende-test--complete-call (nth 1 ende-test--calls))
           (should (eq (ende-job-state second) 'running))
           (let ((prompt (ende-test--chat-context-text
                          (plist-get (nth 2 ende-test--calls) :messages))))
             (dolist (text '("First summary" "New answer" "Latest detail"
                             "Keep commands"))
	       (should (string-match-p text prompt)))
             (should-not (string-match-p "Original history" prompt)))
           (ende-test--chat-goto "** Final question")
           (let ((final (ende-ask)))
             (should (eq (ende-job-state final) 'queued))
             (ende-test--complete-call (nth 2 ende-test--calls))
             (let ((prompt (ende-test--chat-context-text
                            (plist-get (nth 3 ende-test--calls) :messages))))
	       (should (string-match-p "Second summary" prompt))
	       (dolist (text '("First summary" "New answer" "Latest detail"
			       "Keep commands" "Original history"))
                 (should-not (string-match-p text prompt))))
             (ende-test--complete-call (nth 3 ende-test--calls))
             (should (eq (ende-job-state final) 'completed)))
           ;; Compacting an existing fence summarizes its result, not its
           ;; archived instructions or the conversation after it.
           (let* ((document (ende-chat--build-document))
                  (fence (ende-chat--find-by-property
                          document ende-chat-request-id-property
                          (plist-get (ende-job-instruction second) :request-id))))
             (goto-char (org-element-property :begin fence))
             (let ((third (ende-compact)))
	       (should
                (equal (ende-test--chat-context-headings
                        (butlast (cdr (plist-get (nth 4 ende-test--calls)
                                                 :messages))))
		       '("Agent: Context Fence")))
	       (ende-test--complete-call (nth 4 ende-test--calls))
	       (should (eq (ende-job-state third) 'completed)))))))
     ;; A fresh buffer reconstructs the same context using only durable Org.
     (let ((text (buffer-string)))
       (with-temp-buffer
         (org-mode)
         (insert text)
         (ende-test--chat-goto "** Final question")
         (let* ((messages (ende-chat-context-at-point))
                (context (ende-test--chat-context-text messages)))
           (should (equal (ende-test--chat-context-headings messages)
                          '("Agent: Context Fence" "Final question")))
           (should (string-match-p "Third summary" context))
           (should-not (string-match-p "Second summary" context))))))))

(ert-deftest ende-test/chat-compaction-failures-leave-history-usable ()
  (dolist (outcome '(:error :empty :title-only :cancelled :deleted))
    (ende-test--reset)
    (let ((ende-llm-function
           (ende-test--fake-llm
            (list (pcase outcome
                    (:error :error)
                    (:empty "")
                    (:title-only "#+ENDE_CHAT_TITLE: Not a summary\n")
                    (_ "Summary"))) t)))
      (ende-test--with-buffer
       "* Topic\nOriginal history\n** Instructions\nKeep decisions\n** Next\nAsk\n"
       (org-mode)
       (ende-test--chat-goto "** Instructions")
       (let ((job (ende-compact)))
         (pcase outcome
           (:cancelled (ende-abort-queue))
           (:deleted
            (pcase-let ((`(,_document ,headline) (ende-chat--placeholder job)))
              (delete-region (org-element-property :begin headline)
                             (org-element-property :end headline)))))
         (ende-test--complete-call (car ende-test--calls))
         (should (eq (ende-job-state job)
                     (if (eq outcome :cancelled) 'cancelled 'failed)))
         (should (string-match-p "Keep decisions" (buffer-string)))
         (let ((document (ende-chat--build-document)))
           (should-not
            (cl-some (lambda (headline)
		       (ende-chat--context-fence-p document headline))
                     (ende-chat--document-headlines document))))
         (ende-test--chat-goto "** Next")
         (let* ((messages (ende-chat-context-at-point))
                (context (ende-test--chat-context-text messages)))
           (should (equal (ende-test--chat-context-headings messages)
                          '("Topic" "Next")))
           (should (string-match-p "Original history" context))
           (should-not (string-match-p "Keep decisions" context))))))))

(ert-deftest ende-test/chat-compaction-supports-queued-repeats ()
  (dolist (body '("" "Keep the decisions"))
    (ende-test--reset)
    (let ((ende-llm-function
           (ende-test--fake-llm '("First summary" "Second summary") t)))
      (ende-test--with-buffer
       (concat "* Topic\nOriginal history\n** \n" body "\n** Next\nContinue\n")
       (org-mode)
       (ende-test--chat-goto "** ")
       (let ((first (ende-compact))
             (second (ende-compact)))
         (should (eq (ende-job-state second) 'queued))
         (ende-test--complete-call (nth 0 ende-test--calls))
         (should (eq (ende-job-state first) 'completed))
         (should (eq (ende-job-state second) 'running))
         (let ((context (ende-test--chat-context-text
                         (plist-get (nth 1 ende-test--calls) :messages))))
           (should (string-match-p "First summary" context))
           (should-not (string-match-p "Original history" context)))
         (ende-test--complete-call (nth 1 ende-test--calls))
         (should (eq (ende-job-state second) 'completed))
         (ende-test--chat-goto "** Next")
         (let* ((messages (ende-chat-context-at-point))
                (context (ende-test--chat-context-text messages)))
           (should (equal (ende-test--chat-context-headings messages)
                          '("Agent: Context Fence" "Next")))
           (should (string-match-p "Second summary" context))
           (should-not (string-match-p "First summary" context)))
         (if (string-empty-p body)
             (should-not (string-match-p "^\\*\\* *$" (buffer-string)))
           (should (string-match-p body (buffer-string)))))))))

(ert-deftest ende-test/chat-ask-marks-question-and-retries-without-sent-state ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm
          '((:content "First answer" :thinking "First plan")
            (:content "Second answer" :thinking "Second plan")))))
    (ende-test--with-buffer "* Root\n** Question\nAsk again\n"
			    (org-mode)
			    (ende-test--chat-goto "** Question")
			    (ende-ask)
			    (ende-ask)
			    (should (= (ende-test--chat-response-count) 2))
			    (should (= (length (ende-test--chat-reasoning-headlines)) 2))
			    (should (equal (org-entry-get nil ende-chat-role-property)
					   "user"))
			    (should (< (string-match-p "First answer" (buffer-string))
				       (string-match-p "Second answer" (buffer-string))))
			    (let* ((document (ende-chat--build-document))
				   (headlines (ende-chat--document-headlines document))
				   (reasoning
				    (cl-remove-if-not
				     (lambda (headline)
				       (string= (org-element-property :raw-value headline)
						"Agent: Reasoning"))
				     headlines))
				   (replies
				    (cl-remove-if-not
				     (lambda (headline)
				       (string= (org-element-property :raw-value headline)
						"Agent: Reply"))
				     headlines)))
			      (should (< (org-element-property :begin (nth 0 reasoning))
					 (org-element-property :begin (nth 0 replies))))
			      (should (< (org-element-property :begin (nth 0 replies))
					 (org-element-property :begin (nth 1 reasoning))))
			      (should (< (org-element-property :begin (nth 1 reasoning))
					 (org-element-property :begin (nth 1 replies))))
			      (dolist (pair (cl-mapcar #'cons reasoning replies))
				(let ((request-id
				       (ende-chat--property document (cdr pair) :request-id)))
				  (should (equal (ende-chat--property document (car pair)
								      :reasoning-for)
						 request-id))
				  (should (eq (ende-chat--find-by-property
					       document ende-chat-request-id-property request-id)
					      (cdr pair))))))
			    (should-not (cl-some #'ende-review-overlay-p
						 (overlays-in (point-min) (point-max)))))))

(ert-deftest ende-test/chat-title-directive-is-applied-and-removed ()
  (ende-test--reset)
  (let ((ende-llm-function
         (ende-test--fake-llm
          (list (concat "#+ENDE_CHAT_TITLE: Explain the request\n\n"
                        "#+begin_example\n"
                        "Instructions → User question — café\n"
                        "#+end_example\n\n"
                        "Answer\n* Generated")))))
    (ende-test--with-buffer "* Root\n** \nRequest body\n"
			    (org-mode)
			    (ende-test--chat-goto "** ")
			    (let ((job (ende-ask)))
			      (should (eq (ende-job-state job) 'completed)))
			    (should-not (ende-test--chat-reasoning-headlines))
			    (should (string-match-p "^\\*\\* Explain the request$"
						    (buffer-string)))
			    (should-not (string-match-p "ENDE_CHAT_TITLE" (buffer-string)))
			    (should (string-match-p "Answer" (buffer-string)))
			    (should (string-match-p
				     "Instructions → User question — café" (buffer-string)))
			    (should (string-match-p "^\\*\\*\\* Generated$"
						    (buffer-string))))))

(ert-deftest ende-test/chat-response-blocks-stay-opaque-and-close-at-boundary ()
  (let* ((source
          (concat "* First active heading\n"
                  "#+begin_src org\n"
                  "#+ENDE_CHAT_TITLE: literal text\n"
                  ":PROPERTIES:\n:ENDE_ROLE: user\n:END:\n"
                  "*** Literal source heading\n"
                  "Unicode: café → “quoted”.\n"
                  ",* Previously escaped heading\n"
                  ",#+end_src\n"
                  "#+begin_quote\n**** Nested source heading\n#+end_quote\n"
                  "#+end_src\n"
                  "** Second active heading\n"
                  "#+begin_quote\n*** Literal quote heading\n"))
         (normalized (ende-chat--normalize-response source 2)))
    (should-not (plist-get normalized :title))
    (should
     (equal (plist-get normalized :content)
            (concat "*** First active heading\n"
                    "#+begin_src org\n"
                    ",#+ENDE_CHAT_TITLE: literal text\n"
                    ":PROPERTIES:\n:ENDE_ROLE: user\n:END:\n"
                    ",*** Literal source heading\n"
                    "Unicode: café → “quoted”.\n"
                    ",,* Previously escaped heading\n"
                    ",,#+end_src\n"
                    ",#+begin_quote\n,**** Nested source heading\n,#+end_quote\n"
                    "#+end_src\n"
                    "**** Second active heading\n"
                    "#+begin_quote\n,*** Literal quote heading\n"
                    "#+end_quote")))))

(ert-deftest ende-test/chat-unclosed-response-block-does-not-swallow-later-replies ()
  (ende-test--reset)
  (let ((ende-provider-profiles '((local :parallel t :max-concurrency 2)))
        (ende-llm-function
         (ende-test--fake-llm
          '((:content "First answer\n#+begin_src org\n*** Literal heading"
		      :thinking "Plan\n#+begin_quote\n* Literal thought")
            "Second answer"
            "Third answer")
          t)))
    (ende-test--with-buffer
     (concat "* Topic\n"
	     "** First question\nFirst request\n"
	     "** Second question\nSecond request\n"
	     "** Third question\nThird request\n"
	     "** Later section\nKeep this section\n")
     (org-mode)
     (ende-test--chat-goto "** First question")
     (setq ende--queue-suspended t)
     (let ((first-job (ende-ask)))
       (ende-test--chat-goto "** Second question")
       (let ((second-job (ende-ask)))
         (setq ende--queue-suspended nil)
         (ende-process-next)
         (should (= (length ende-test--calls) 2))
         ;; The lower reply finishes while the earlier request is still open.
         (ende-test--complete-call (nth 1 ende-test--calls))
         (should (eq (ende-job-state second-job) 'completed))
         (ende-test--complete-call (nth 0 ende-test--calls))
         (should (eq (ende-job-state first-job) 'completed))
         (should (string-match-p (regexp-quote "#+end_src")
                                 (buffer-string)))
         (should (string-match-p (regexp-quote "#+end_quote")
                                 (buffer-string)))
         (should (string-match-p (regexp-quote ",* Literal thought")
                                 (buffer-string)))
         (should (string-match-p
                  (regexp-quote
                   "#+begin_src org\n,*** Literal heading\n#+end_src")
                  (buffer-string)))
         (let* ((document (ende-chat--build-document))
                (headlines (ende-chat--document-headlines document)))
           (dolist (title '("First question" "Second question"
                            "Third question" "Later section"))
             (should (cl-some
                      (lambda (headline)
                        (string= (org-element-property :raw-value headline)
                                 title))
                      headlines)))
           (should-not
            (cl-some
             (lambda (headline)
	       (member (org-element-property :raw-value headline)
		       '("Literal heading" "Literal thought")))
             headlines))
           (should (= (length (ende-test--chat-response-headlines)) 2)))
         ;; A fresh request below the repaired response must remain usable.
         (ende-test--chat-goto "** Third question")
         (let ((third-job (ende-ask)))
           (should (= (length ende-test--calls) 3))
           (ende-test--complete-call (nth 2 ende-test--calls))
           (should (eq (ende-job-state third-job) 'completed)))
         (ende-test--chat-goto "** Later section")
         (let ((context (ende-test--chat-context-text
                         (ende-chat-context-at-point))))
           (should (string-match-p "First answer" context))
           (should (string-match-p "Second answer" context))
           (should (string-match-p "Third answer" context))
           (should
            (string-match-p
             (regexp-quote
              "First answer\n#+begin_src org\n*** Literal heading")
             context))
           (should-not (string-match-p (regexp-quote "#+end_src") context))
           (should (string-match-p "Keep this section" context))))))))

(ert-deftest ende-test/chat-reasoning-is-org-structured-folded-and-configurable ()
  (ende-test--reset)
  (let* ((thinking (concat "Plan with /ordinary Org emphasis/.\n"
                           "* First thought\n"
                           "** Nested thought\n"
                           "#+begin_src emacs-lisp\n"
                           "(message \"reasoning source\")\n"
                           "#+end_src\n"))
         (ende-llm-function
          (ende-test--fake-llm
           (list (list :content "Answer\n** Reply section"
                       :thinking thinking)))))
    (ende-test--with-buffer
     (concat "* Root\n"
	     "** Question\n"
	     "Ask\n"
	     "** Next\n"
	     "Next\n")
     (org-mode)
     (ende-test--chat-goto "** Question")
     (ende-ask)
     (let* ((document (ende-chat--build-document))
            (tree (ende-chat--document-tree document))
            (headlines (ende-chat--document-headlines document))
            (reasoning
             (cl-find-if
              (lambda (headline)
                (string= (org-element-property :raw-value headline)
                         "Agent: Reasoning"))
              headlines))
            (reply
             (cl-find-if
              (lambda (headline)
                (string= (org-element-property :raw-value headline)
                         "Agent: Reply"))
              headlines))
            (reasoning-id
             (ende-chat--property document reasoning :reasoning-for))
            (reply-id (ende-chat--property document reply :request-id))
            (first-thought
             (cl-find-if
              (lambda (headline)
                (string= (org-element-property :raw-value headline)
                         "First thought"))
              headlines))
            (nested-thought
             (cl-find-if
              (lambda (headline)
                (string= (org-element-property :raw-value headline)
                         "Nested thought"))
              headlines))
            (reply-section
             (cl-find-if
              (lambda (headline)
                (string= (org-element-property :raw-value headline)
                         "Reply section"))
              headlines))
            (source-blocks
             (org-element-map reasoning 'src-block #'identity)))
       (should (equal (org-element-property :raw-value reasoning)
                      "Agent: Reasoning"))
       (should (equal (org-element-property :raw-value reply) "Agent: Reply"))
       (should (= (org-element-property :level reasoning) 2))
       (should (= (org-element-property :level reply) 2))
       (should (eq (ende-chat--parent document reasoning)
                   (ende-chat--parent document reply)))
       (should (< (org-element-property :begin reasoning)
                  (org-element-property :begin reply)))
       (should (eq (ende-chat--effective-role document reasoning) 'assistant))
       (should (eq (ende-chat--effective-role document reply) 'assistant))
       (should (eq (ende-chat--effective-visibility document reasoning)
                   'reasoning))
       (should (eq (ende-chat--effective-visibility document reply) 'normal))
       (should (equal reasoning-id reply-id))
       (should-not (ende-chat--property document reasoning :request-id))
       (should (eq (ende-chat--find-by-property
                    document ende-chat-request-id-property reply-id)
                   reply))
       (should (equal (org-element-property :raw-value first-thought)
                      "First thought"))
       (should (= (org-element-property :level first-thought) 3))
       (should (= (org-element-property :level nested-thought) 4))
       (should (eq (ende-chat--parent document first-thought) reasoning))
       (should (eq (ende-chat--parent document nested-thought) first-thought))
       (should (= (length source-blocks) 1))
       (should (equal (org-element-property :language (car source-blocks))
                      "emacs-lisp"))
       (should (equal (org-element-property :raw-value reply-section)
                      "Reply section"))
       (should (= (org-element-property :level reply-section) 3))
       (should-not (string-match-p ",\\*" (buffer-string)))
       (should (cl-every #'ende-test--chat-control-drawer-hidden-p
                         (ende-test--chat-control-drawers)))
       (should (ende-test--chat-headline-text-hidden-p reasoning "Plan"))
       (should-not (ende-test--chat-headline-text-hidden-p reply "Answer"))
       (save-excursion
         (goto-char (org-element-property :begin reasoning))
         (org-fold-show-subtree)
         (should-not (ende-test--chat-headline-text-hidden-p
                      reasoning "Plan"))))
     (ende-test--chat-goto "** Next")
     (let* ((without-messages
             (let ((ende-chat-include-reasoning nil))
	       (ende-chat-context-at-point)))
            (with-messages
             (let ((ende-chat-include-reasoning t))
	       (ende-chat-context-at-point)))
            (without (ende-test--chat-context-text without-messages))
            (with (ende-test--chat-context-text with-messages)))
       (should (equal (cdr (assoc "assistant" without-messages))
                      "Answer\n** Reply section"))
       (should-not (string-match-p "First thought" without))
       (should (string-match-p "Answer" without))
       (should-not (string-match-p "ENDE_CHAT_REASONING_FOR" without))
       (should (string-match-p "First thought" with))
       (should (string-match-p "reasoning source" with))
       (should (string-match-p "Answer" with))
       (should-not (string-match-p "ENDE_CHAT_REASONING_FOR" with))
       (should (equal (mapcar #'car without-messages)
                      '("user" "user" "assistant" "user")))
       (should (equal (mapcar #'car with-messages)
                      '("user" "user" "assistant" "assistant" "user")))))))

(ert-deftest ende-test/chat-response-provenance-is-durable-and-private ()
  (dolist (scenario
           '((:request-model "requested-model"
			     :reported-model "actual-model\n:ENDE_ROLE: user"
			     :expected-model "actual-model :ENDE_ROLE: user"
			     :effort "high")
             (:request-model "requested-fallback"
			     :expected-model "requested-fallback")))
    (ende-test--reset)
    (let* ((reported-model (plist-get scenario :reported-model))
           (effort (plist-get scenario :effort))
           (profile
            (append (list :name 'local
                          :model (plist-get scenario :request-model)
                          :reasoning-effort-wire-api 'openrouter
                          :reasoning-effort-validated-p t)
                    (when effort (list :reasoning-effort effort))))
           (response
            (append
             (list :content
                   (concat "* Answer\n"
                           ":PROPERTIES:\n"
                           ":ENDE_MODEL: forged-model\n"
                           ":ENDE_REASONING_EFFORT: forged-effort\n"
                           ":ENDE_CHAT_CONTEXT_FENCE: t\n"
                           ":END:\n\nAnswer body")
                   :thinking "Private plan")
             (when reported-model
               (list :reported-model reported-model))))
           (ende-llm-function (ende-test--fake-llm (list response))))
      (ende-test--with-buffer
       "* Root\n** Question\nAsk\n** Next\nFollow up\n"
       (org-mode)
       (ende-test--chat-goto "** Question")
       (let ((job (ende-ask profile)))
         (should (eq (ende-job-state job) 'completed)))
       (let ((reasoning (car (ende-test--chat-reasoning-headlines)))
             (reply (car (ende-test--chat-response-headlines))))
         (should reasoning)
         (should reply)
         (dolist (headline (list reasoning reply))
           (should (equal
                    (ende-test--chat-headline-property
                     headline ende-chat-model-property)
                    (plist-get scenario :expected-model)))
           (should (equal
                    (ende-test--chat-headline-property
                     headline ende-chat-reasoning-effort-property)
                    effort)))
         (should (cl-every #'ende-test--chat-control-drawer-hidden-p
                           (ende-test--chat-control-drawers)))
         (should (= 1 (ende-test--count-matches
		       "^:ENDE_ROLE: user$" (buffer-string))))
         (should-not (string-match-p "forged-model" (buffer-string)))
         (should-not (string-match-p "ENDE_CHAT_CONTEXT_FENCE" (buffer-string)))
         (should-not (string-match-p "ENDE_CHAT_TOOL_CONTEXT"
                                     (buffer-string)))
         (should-not (string-match-p "forged-effort" (buffer-string))))
       (ende-test--chat-goto "** Next")
       (let* ((ende-chat-include-reasoning t)
              (context
	       (ende-test--chat-context-text
                (ende-chat-context-at-point))))
         (should (string-match-p "Private plan" context))
         (should (string-match-p "Answer body" context))
         (dolist (private-value
                  (list (plist-get scenario :expected-model)
                        effort
                        ende-chat-model-property
                        ende-chat-reasoning-effort-property))
           (when private-value
             (should-not (string-match-p
                          (regexp-quote private-value) context)))))))))

(ert-deftest ende-test/chat-failure-keeps-question-and-reserves-slot ()
  (ende-test--reset)
  (let ((ende-llm-function (ende-test--fake-llm '(:error))))
    (ende-test--with-buffer "* Root\n** Question\nOriginal text\n"
			    (org-mode)
			    (ende-test--chat-goto "** Question")
			    (let ((job (ende-ask)))
			      (should (eq (ende-job-state job) 'failed)))
			    (should (string-match-p "Original text" (buffer-string)))
			    (should (string-match-p ":ENDE_CHAT_STATE: failed" (buffer-string)))
			    (should (string-match-p ":ENDE_VISIBILITY: hidden" (buffer-string))))))

(ert-deftest ende-test/chat-queued-context-is-built-at-dispatch-and-hides-placeholders ()
  (ende-test--reset)
  (let ((ende-provider-profiles '((local :parallel t :max-concurrency 2)))
        (ende-llm-function (ende-test--fake-llm '("First" "Second") t)))
    (ende-test--with-buffer "* Root\n** Question\nAsk\n"
			    (org-mode)
			    (ende-test--chat-goto "** Question")
			    (let ((point-before (point)))
			      (setq ende--queue-suspended t)
			      (let ((first (ende-ask))
				    (second (ende-ask)))
				(should (= (point) point-before))
				(let ((drawers (ende-test--chat-control-drawers)))
				  (should (= (length drawers) 3))
				  (should (cl-every #'ende-test--chat-control-drawer-hidden-p
						    drawers)))
				(let ((headlines (ende-test--chat-response-headlines)))
				  (should (= (length headlines) 2))
				  (should (cl-every #'ende-test--chat-response-has-separator-p
						    headlines))
				  (dolist (job (list first second))
				    (should
				     (cl-some
				      (lambda (headline)
					(= (overlay-start (ende-job-overlay job))
					   (1- (org-element-property :begin headline))))
				      headlines))))
				(setq ende--queue-suspended nil)
				(ende-process-next)
				(should (= (length ende-test--calls) 2))
				(dolist (messages ende-test--prompts)
				  (let ((users (mapcar #'cdr
						       (cl-remove-if-not
							(lambda (message)
							  (equal (car message) "user"))
							messages))))
				    (should (cl-every
					     (lambda (text)
					       (and (not (string-match-p "ENDE_CHAT_STATE" text))
						    (not (string-match-p "Agent: Reply" text))))
					     users))))
				(ende-test--complete-call (nth 1 ende-test--calls))
				(should (eq (ende-job-state second) 'completed))
				(should (eq (ende-job-state first) 'running))
				(should (string-match-p "Second" (buffer-string)))
				(should (cl-every #'ende-test--chat-control-drawer-hidden-p
						  (ende-test--chat-control-drawers)))
				(should (string-match-p "ENDE_CHAT_STATE: pending" (buffer-string)))
				(ende-test--complete-call (nth 0 ende-test--calls))
				(should (eq (ende-job-state first) 'completed))
				(should (< (string-match-p "First" (buffer-string))
					   (string-match-p "Second" (buffer-string))))
				(should (cl-every #'ende-test--chat-control-drawer-hidden-p
						  (ende-test--chat-control-drawers)))
				(should (cl-every #'ende-test--chat-response-has-separator-p
						  (ende-test--chat-response-headlines))))))))

(ert-deftest ende-test/chat-preserves-point-through-async-lifecycle ()
  (dolist (response '("Answer" :error))
    (ende-test--reset)
    (let ((ende-llm-function (ende-test--fake-llm (list response) t)))
      (ende-test--with-buffer
       "* Root\n** Question\nAsk\n** Work\nKeep editing\n"
       (org-mode)
       (ende-test--chat-goto "** Question")
       (let ((question-point (point)))
         (setq ende--queue-suspended t)
         (let ((job (ende-ask)))
           (should (= (point) question-point))
           (setq ende--queue-suspended nil)
           (ende-process-next)
           (goto-char (point-min))
           (search-forward "Keep editing")
           (let ((line (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position)))
                 (column (current-column)))
             (ende-test--complete-call (car ende-test--calls))
             (should (equal line
                            (buffer-substring-no-properties
                             (line-beginning-position)
                             (line-end-position))))
             (should (= column (current-column)))
             (should (eq (ende-job-state job)
                         (if (eq response :error) 'failed 'completed))))))))))

(ert-deftest ende-test/chat-ask-promotes-the-selected-headline-to-user ()
  (dolist (local-role '(nil "assistant"))
    (ende-test--reset)
    (let ((ende-llm-function (ende-test--fake-llm '("Answer"))))
      (ende-test--with-buffer
       (concat "* Root\n"
	       "** Agent: Reply\n"
	       ":PROPERTIES:\n:ENDE_ROLE: assistant\n:END:\n"
	       "Prior assistant answer.\n"
	       "*** Follow-up\n"
	       (if local-role
                   (format ":PROPERTIES:\n:ENDE_ROLE: %s\n:END:\n"
                           local-role)
                 "")
	       "Question from this node.\n")
       (org-mode)
       (ende-test--chat-goto "*** Follow-up")
       (let* ((job (ende-ask))
              (document (ende-chat--build-document))
              (question
	       (ende-chat--find-by-property
                document ende-chat-id-property
                (plist-get (ende-job-instruction job) :question-id)))
              (parent (ende-chat--parent document question))
              (messages (plist-get (car ende-test--calls) :messages))
              (assistant-text
	       (mapconcat
                (lambda (message)
                  (if (equal (car message) "assistant")
                      (cdr message)
                    ""))
                messages "\n"))
              (final-message (car (last messages))))
         (should (eq (ende-job-state job) 'completed))
         (should (equal (ende-chat--property document question :role)
                        "user"))
         (should (equal (ende-chat--property document parent :role)
                        "assistant"))
         (should-not (ende-chat--property document question :visibility))
         (should (string-match-p "Prior assistant answer" assistant-text))
         (should-not (string-match-p "Question from this node" assistant-text))
         (should (equal (car final-message) "user"))
         (should (string-match-p "Question from this node"
                                 (cdr final-message))))))))

(ert-deftest ende-test/selector-ask-includes-only-reasoning-ancestors ()
  (dolist (policy '(root-prefix current-section-only))
    (ende-test--reset)
    (let ((ende-chat-context-policy policy)
          (ende-chat-include-reasoning nil)
          (ende-provider-profiles '((remote :model "configured-model")))
          (ende-llm-function (ende-test--fake-llm '("Answer"))))
      (ende-test--with-buffer
       (concat "* Root\n"
	       "** Reasoning cousin\n"
	       ":PROPERTIES:\n"
	       ":ENDE_ROLE: assistant\n"
	       ":ENDE_VISIBILITY: reasoning\n"
	       ":END:\nCousin reasoning text.\n"
	       "** Agent: Reasoning\n"
	       ":PROPERTIES:\n"
	       ":ENDE_ROLE: assistant\n"
	       ":ENDE_VISIBILITY: reasoning\n"
	       ":END:\nOuter reasoning context.\n"
	       "*** Reasoning aunt\n"
	       ":PROPERTIES:\n:ENDE_VISIBILITY: reasoning\n:END:\n"
	       "Aunt reasoning text.\n"
	       "*** Nested reasoning\n"
	       "Nested reasoning context.\n"
	       "**** Reasoning sibling\n"
	       ":PROPERTIES:\n:ENDE_VISIBILITY: reasoning\n:END:\n"
	       "Sibling reasoning text.\n"
	       "**** Follow-up\nQuestion from reasoning.\n")
       (org-mode)
       (ende-test--chat-goto "**** Follow-up")
       (let ((source (current-buffer)))
         (ende-test--selector-ask-from-buffer source))
       (let* ((call (car ende-test--calls))
              (messages (plist-get call :messages))
              (assistant-text
	       (mapconcat
                (lambda (message)
                  (if (equal (car message) "assistant")
                      (cdr message)
                    ""))
                messages "\n"))
              (final-message (car (last messages)))
              (document (ende-chat--build-document)))
         (should (equal (plist-get (plist-get call :options) :model)
                        "selected-model"))
         (should (string-match-p "Answer" (buffer-string)))
         (should (string-match-p "Outer reasoning context" assistant-text))
         (should (string-match-p "Nested reasoning context" assistant-text))
         (dolist (excluded '("Cousin reasoning text"
                             "Aunt reasoning text"
                             "Sibling reasoning text"))
           (should-not (string-match-p (regexp-quote excluded)
				       assistant-text)))
         (should (equal (car final-message) "user"))
         (should (string-match-p "Question from reasoning"
                                 (cdr final-message)))
         (let* ((question (ende-chat--headline-at-point document))
                (parent (ende-chat--parent document question)))
           (should (equal (ende-chat--property document question :role)
                          "user"))
           (should-not (ende-chat--property document question :visibility))
           (should (eq (ende-chat--effective-visibility document parent)
		       'reasoning))))))))

(ert-deftest ende-test/selector-ask-rejects-hidden-ancestors ()
  (ende-test--reset)
  (let ((ende-provider-profiles '((remote :model "configured-model")))
        (ende-llm-function (ende-test--fake-llm '("Must not run"))))
    (ende-test--with-buffer
     (concat "* Root\n"
	     "** Hidden assistant context\n"
	     ":PROPERTIES:\n"
	     ":ENDE_ROLE: assistant\n"
	     ":ENDE_VISIBILITY: hidden\n"
	     ":END:\n"
	     "*** Follow-up\nQuestion inside hidden context.\n")
     (org-mode)
     (ende-test--chat-goto "*** Follow-up")
     (let ((source (current-buffer)))
       (should-error (ende-test--selector-ask-from-buffer source)
                     :type 'user-error))
     (ende-test--chat-goto "*** Follow-up")
     (should-not (org-entry-get nil ende-chat-role-property nil))
     (should-not (org-entry-get nil ende-chat-id-property nil))
     (should-not ende-test--calls))))

(ert-deftest ende-test/chat-ask-rejects-hidden-nodes-and-reasoning-roots ()
  (dolist (property
           '(":ENDE_VISIBILITY: hidden"
             ":ENDE_VISIBILITY: reasoning"))
    (ende-test--reset)
    (ende-test--with-buffer
     (format "* Root\n** Question\n:PROPERTIES:\n%s\n:END:\nAsk\n"
	     property)
     (org-mode)
     (ende-test--chat-goto "** Question")
     (should-error (ende-ask) :type 'user-error))))

(ert-deftest ende-test/selector-chat-actions-use-selected-profile-in-org-buffer ()
  (dolist (key '("a" "k"))
    (ende-test--reset)
    (let ((ende-selector--last-selection nil)
          (ende-provider-profiles '((remote :model "configured-model")))
          (ende-llm-function (ende-test--fake-llm '("Reply"))))
      (ende-test--with-buffer "* Root\nEarlier context\n** Question\nAsk\n"
			      (org-mode)
			      (ende-test--chat-goto "** Question")
			      (let ((source (current-buffer)))
				(with-temp-buffer
				  (ende-selector-mode)
				  (setq ende-selector--source-buffer source
					ende-selector--providers '(remote)
					ende-selector--provider-index 0
					ende-selector--models '("selected-model")
					ende-selector--model-index 0
					ende-selector--model-state :ready
					ende-selector--effort nil)
				  (cl-letf (((symbol-function 'ende-selector-close) #'ignore))
				    (call-interactively (lookup-key ende-selector-mode-map (kbd key)))))
				(let ((profile (plist-get (car ende-test--calls) :options)))
				  (should (eq (plist-get profile :name) 'remote))
				  (should (equal (plist-get profile :model) "selected-model"))
				  (should (string-match-p "Reply" (buffer-string)))))))))

(ert-deftest ende-test/selector-ask-prepares-chat-from-non-org-buffer ()
  (dolist (initial '("" "* Existing\nPrevious material\n"))
    (ende-test--reset)
    (let ((ende-selector--last-selection nil)
          (ende-llm-function (ende-test--fake-llm '("Reply"))))
      (ende-test--with-buffer "source text"
			      (let ((source (current-buffer))
				    (shown nil)
				    (chat (ende-chat--ensure-buffer)))
				(with-current-buffer chat
				  (insert initial))
				(with-temp-buffer
				  (ende-selector-mode)
				  (setq ende-selector--source-buffer source
					ende-selector--providers '(local)
					ende-selector--provider-index 0
					ende-selector--models '("local-model")
					ende-selector--model-index 0
					ende-selector--model-state :ready
					ende-selector--effort nil)
				  (cl-letf (((symbol-function 'ende-selector-close) #'ignore)
					    ((symbol-function 'pop-to-buffer)
					     (lambda (buffer) (setq shown buffer))))
				    (ende-selector-ask)
				    (with-current-buffer chat
				      (goto-char (point-max))
				      (insert "Question\n"))
				    (setq ende-selector--source-buffer chat)
				    (ende-selector-ask)))
				(should (eq shown chat))
				(with-current-buffer chat
				  (should (derived-mode-p 'org-mode))
				  (should (bound-and-true-p ende-mode))
				  (should (= (how-many "^\\* *$" (point-min) (point-max)) 1))
				  (should (string-match-p "Reply" (buffer-string)))
				  (should (string-match-p "^\\*\\* Agent: Reply$"
							  (buffer-string)))
				  (should (string-match-p ":ENDE_ROLE: assistant"
							  (buffer-string)))
				  (when (not (string-empty-p initial))
				    (should (string-match-p "Previous material"
							    (buffer-string))))))))))

(provide 'ende-test)

;;; ende-test.el ends here
