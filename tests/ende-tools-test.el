;;; ende-tools-test.el --- Project-tool tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Coverage for project files, buffers, and scope enforcement.

;;; Code:

(require 'ert)
(require 'ende)
(require 'ende-test-helpers)
(require 'cl-lib)
(require 'json)
(require 'ende-chat-test-helpers)

(defun ende-test--tool-result (name arguments root)
  "Invoke tool NAME and return its successful result in ROOT."
  (let ((call (ende-tool-call name arguments root)))
    (should (plist-get call :ok))
    (plist-get call :result)))

(defun ende-test--edit-file-call (path edits root)
  "Call `edit_file' for PATH with anchor EDITS under ROOT."
  (ende-tool-call "edit_file"
                  (list (cons "path" path) (cons "edits" edits))
                  root))

(defun ende-test--create-file-call (path content root)
  "Call `create_file' for PATH with CONTENT under ROOT."
  (ende-tool-call "create_file"
                  (list (cons "path" path) (cons "content" content))
                  root))

(ert-deftest ende-test/project-tools-expose-json-definitions ()
  (let ((definitions (ende-tool-definitions)))
    (let ((names (mapcar (lambda (definition)
                           (alist-get :name (alist-get :function definition)))
                         definitions)))
      (should (= (length names) (length (delete-dups (copy-sequence names)))))
      (dolist (required '("project_info" "list_project_files" "file_info"
                          "read_file" "list_project_buffers" "read_buffer"
                          "search_project" "edit_file" "create_file"))
        (should (member required names))))
    (dolist (definition definitions)
      (let* ((function (alist-get :function definition))
             (schema
              (ende-protocol-json-read-string
               (json-encode (alist-get :parameters function)))))
        (should (hash-table-p (gethash "properties" schema)))
        (should (eq (gethash "additionalProperties" schema) :false))))
    (should-not (ende-tool-read-only (ende-tool--find "edit_file")))
    (should-not (ende-tool-read-only (ende-tool--find "create_file")))
    (should-not (ende-tool--find "edit_source"))
    (should-not (plist-get (ende-tool-call "edit_source" nil) :ok))
    (should (string-match-p "edit_file" (json-encode definitions)))
    (should-not (string-match-p "edit_source" (json-encode definitions)))
    (let* ((definition
            (cl-find-if
             (lambda (candidate)
               (equal (alist-get :name (alist-get :function candidate))
                      "edit_file"))
             definitions))
           (schema (ende-protocol-json-read-string
                    (json-encode
                     (alist-get :parameters (alist-get :function definition)))))
           (properties (gethash "properties" schema))
           (item (gethash "items" (gethash "edits" properties)))
           (item-properties (gethash "properties" item)))
      (should (equal (append (gethash "required" schema) nil)
                     '("path" "edits")))
      (should (eq (gethash "additionalProperties" schema) :false))
      (should-not (gethash "buffer_id" properties))
      (should-not (gethash "base_digest" properties))
      (should (= (gethash "minItems" (gethash "edits" properties)) 1))
      (should (equal (append (gethash "required" item) nil)
                     '("anchor" "position" "text")))
      (should (eq (gethash "additionalProperties" item) :false))
      (should (equal (append (gethash "enum" (gethash "position"
                                                      item-properties))
                             nil)
                     '("before" "after" "replace")))
      (should (= (gethash "minLength" (gethash "anchor" item-properties)) 1))
      (should-not (gethash "minLength" (gethash "text" item-properties)))
      (should (= (gethash "minimum" (gethash "occurrence" item-properties))
                 1)))
    (let* ((definition
            (cl-find-if
             (lambda (candidate)
               (equal (alist-get :name (alist-get :function candidate))
                      "create_file"))
             definitions))
           (schema (ende-protocol-json-read-string
                    (json-encode
                     (alist-get :parameters (alist-get :function definition)))))
           (properties (gethash "properties" schema)))
      (should (equal (append (gethash "required" schema) nil)
                     '("path" "content")))
      (should (eq (gethash "additionalProperties" schema) :false))
      (should (= (gethash "minLength" (gethash "path" properties)) 1))
      (should (equal (gethash "type" (gethash "content" properties))
                     "string"))
      (should-not (gethash "minLength" (gethash "content" properties))))))

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

(ert-deftest ende-test/project-tools-edit-file-enforces-resource-scope ()
  (let* ((root (make-temp-file "ende-tools-edit-scope-" t))
         (allowed (expand-file-name "public.el" root))
         (blocked (expand-file-name "private.el" root)))
    (unwind-protect
        (progn
          (with-temp-file allowed (insert "public source\n"))
          (with-temp-file blocked (insert "private source\n"))
          (with-temp-buffer
            (org-mode)
            (insert
             (ende-test--chat-config-block
              `((root ,root)
                (group writer edit_file)
                (resource public (include "public.el"))
                (tools writer)
                (set-tool-resources writer :resources public)))
             "* Conversation\n** Edit\n")
            (let* ((document (ende-chat--build-document))
                   (configuration
                    (ende-test--chat-config-at document "Edit"))
                   (allowed-call
                    (ende-tool-call
                     'edit_file
                     (list :path "public.el"
                           :edits (list (ende-test--anchor-edit
                                         "source" "replace" "edited")))
                     root configuration))
                   (blocked-call
                    (ende-tool-call
                     'edit_file
                     (list :path "private.el"
                           :edits (list (ende-test--anchor-edit
                                         "source" "replace" "edited")))
                     root configuration))
                   (outside-call
                    (ende-tool-call
                     'edit_file
                     (list :path "../outside.el"
                           :edits (list (ende-test--anchor-edit
                                         "source" "replace" "edited")))
                     root configuration)))
              (should (plist-get allowed-call :ok))
              (should (equal (plist-get (plist-get allowed-call :result)
                                        :source_kind)
                             "file"))
              (should-not (plist-get blocked-call :ok))
              (should (string-match-p "not authorized"
                                      (plist-get blocked-call :error)))
              (should-not (plist-get outside-call :ok))
              (should (string-match-p "outside the project"
                                      (plist-get outside-call :error)))
              (should (equal (with-temp-buffer
                               (insert-file-contents allowed)
                               (buffer-string))
                             "public edited\n"))
              (should (equal (with-temp-buffer
                               (insert-file-contents blocked)
                               (buffer-string))
                             "private source\n")))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ende-test/project-tools-separate-disk-and-buffer-sources ()
  (let* ((root (make-temp-file "ende-tools-buffer-project-" t))
         (path (expand-file-name "main.el" root))
         (buffer nil))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "DISK_TOKEN from disk\nsecond disk\n"))
          (setq buffer (find-file-noselect path))
          (with-current-buffer buffer
            (erase-buffer)
            (insert "BUFFER_TOKEN unsaved\nsecond buffer\nthird buffer\n")
            (set-buffer-modified-p t))
          (let* ((ende-project-root-directory root)
                 (listed (ende-test--tool-result
                          "list_project_buffers" nil root))
                 (record (car (plist-get listed :buffers)))
                 (buffer-id (plist-get record :buffer_id))
                 (buffer-read
                  (ende-test--tool-result
                   "read_buffer"
                   (list (cons "buffer_id" buffer-id)
                         (cons "start_line" 1)
                         (cons "line_count" 1))
                   root))
                 (file-read
                  (ende-test--tool-result
                   "read_file"
                   '(("path" . "main.el")
                     ("start_line" . 1)
                     ("line_count" . 1))
                   root))
                 (info (ende-test--tool-result
                        "file_info" '(("path" . "main.el")) root))
                 (files (ende-test--tool-result
                         "list_project_files"
                         '(("include_metadata" . t)) root))
                 (disk-search
                  (ende-test--tool-result
                   "search_project"
                   '(("regexp" . "DISK_TOKEN")) root))
                 (buffer-search
                  (ende-test--tool-result
                   "search_project"
                   '(("regexp" . "BUFFER_TOKEN")) root)))
            (should (string= (plist-get buffer-read :source_kind) "buffer"))
            (should (string= (plist-get buffer-read :content)
                             "BUFFER_TOKEN unsaved\n"))
            (should (string= (plist-get buffer-read :path) "main.el"))
            (should (string= (plist-get file-read :source_kind) "file"))
            (should (string= (plist-get file-read :content)
                             "DISK_TOKEN from disk\n"))
            (should (string= (plist-get file-read :digest)
                             (plist-get info :digest)))
            (should (= (plist-get info :line_count) 2))
            (should-not (equal
                         (plist-get file-read :digest)
                         (with-current-buffer buffer
                           (secure-hash 'sha256 (current-buffer)
                                        (point-min) (point-max)))))
            (should (= (plist-get info :byte_count)
                       (string-bytes "DISK_TOKEN from disk\nsecond disk\n")))
            (should (plist-get info :visiting_buffer))
            (should (eq (plist-get info :visiting_buffer_modified) t))
            (should (string= (plist-get
                              (car (plist-get files :files)) :source_kind)
                             "file"))
            (should (string= (plist-get
                              (car (plist-get files :files)) :digest)
                             (plist-get info :digest)))
            (should (= (length (plist-get disk-search :matches)) 1))
            (should (string= (plist-get
                              (car (plist-get disk-search :matches)) :source_kind)
                             "file"))
            (should-not (plist-get buffer-search :matches))))
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

(ert-deftest ende-test/project-tools-edit-file-writes-disk-and-preserves-buffers ()
  (dolist (state '(no-buffer clean-buffer modified-buffer))
    (let* ((root (make-temp-file "ende-tools-edit-project-" t))
           (path (expand-file-name "main.el" root))
           (buffer nil))
      (unwind-protect
          (progn
            (with-temp-file path
              (insert "hello world\nkeep\n"))
            (unless (eq state 'no-buffer)
              (setq buffer (find-file-noselect path)))
            (when buffer
              (with-current-buffer buffer
                (when (eq state 'modified-buffer)
                  (erase-buffer)
                  (insert "unsaved buffer contents\n")
                  (set-buffer-modified-p t))
                (goto-char (+ (point-min) 3))
                (narrow-to-region (point-min) (+ (point-min) 8))
                (goto-char (+ (point-min) 2))))
            (let* ((snapshot
                    (and buffer
                         (with-current-buffer buffer
                           (list :text
                                 (save-restriction
                                   (widen)
                                   (buffer-substring-no-properties
                                    (point-min) (point-max)))
                                 :modified (buffer-modified-p)
                                 :point (point)
                                 :minimum (point-min)
                                 :maximum (point-max)
                                 :undo-list buffer-undo-list))))
                   (ende-project-root-directory root)
                   (result
                    (ende-test--tool-result
                     "edit_file"
                     (list (cons "path" "main.el")
                           (cons "edits"
                                 (list (ende-test--anchor-edit
                                        "world" "replace" "Emacs"))))
                     root)))
              (should (equal (plist-get result :operation) "edit"))
              (should (equal (plist-get result :status) "applied"))
              (should (equal (plist-get result :source_kind) "file"))
              (should (equal (plist-get result :write_target) "disk_file"))
              (should (= (plist-get result :edit_count) 1))
              (should (equal (with-temp-buffer
                               (insert-file-contents path)
                               (buffer-string))
                             "hello Emacs\nkeep\n"))
              (if buffer
                  (with-current-buffer buffer
                    (should (equal
                             (save-restriction
                               (widen)
                               (buffer-substring-no-properties
                                (point-min) (point-max)))
                             (plist-get snapshot :text)))
                    (should (eq (buffer-modified-p)
                                (plist-get snapshot :modified)))
                    (should (= (point) (plist-get snapshot :point)))
                    (should (= (point-min) (plist-get snapshot :minimum)))
                    (should (= (point-max) (plist-get snapshot :maximum)))
                    (should (eq buffer-undo-list
                                (plist-get snapshot :undo-list)))
                    (should (stringp
                             (plist-get result :visiting_buffer_warning)))
                    (should (eq (plist-get result :visiting_buffer_modified)
                                (eq state 'modified-buffer)))
                    (should (plist-get result :visiting_buffer))
                    (when (eq state 'modified-buffer)
                      (should (string-match-p
                               "saving.*overwrite"
                               (plist-get result :visiting_buffer_warning)))))
                (progn
                  (should-not (get-file-buffer path))
                  (should-not (plist-get result :visiting_buffer))
                  (should-not (plist-get result :visiting_buffer_warning))))))
        (when (buffer-live-p buffer)
          (set-buffer-modified-p nil)
          (kill-buffer buffer))
        (when (file-directory-p root)
          (delete-directory root t))))))

(ert-deftest ende-test/project-tools-edit-file-ignores-and-preserves-reviews ()
  (let* ((root (make-temp-file "ende-tools-review-project-" t))
         (path (expand-file-name "main.el" root))
         (buffer nil)
         (review nil))
    (unwind-protect
        (progn
          (with-temp-file path (insert "hello world\n"))
          (setq buffer (find-file-noselect path))
          (with-current-buffer buffer
            (erase-buffer)
            (insert "unsaved buffer contents\n")
            (set-buffer-modified-p t)
            (setq review (ende-review-create-overlay
                          buffer 1 8 "model" "client" "unsaved")))
          (let* ((ende-project-root-directory root)
                 (ende-review-conflict-policy 'block)
                 (overlays-before
                  (with-current-buffer buffer
                    (overlays-in (point-min) (point-max))))
                 (result
                  (ende-test--tool-result
                   "edit_file"
                   (list (cons "path" "main.el")
                         (cons "edits"
                               (list (ende-test--anchor-edit
                                      "world" "replace" "Emacs"))))
                   root)))
            (should (equal (plist-get result :status) "applied"))
            (should (equal (plist-get result :source_kind) "file"))
            (with-current-buffer buffer
              (should (equal overlays-before
                             (overlays-in (point-min) (point-max))))
              (should (ende-review-overlay-p review))
              (should (string= (buffer-string)
                               "unsaved buffer contents\n")))
             (should (string= (with-temp-buffer
                                (insert-file-contents path)
                                (buffer-string))
                              "hello Emacs\n"))))
      (when (buffer-live-p buffer)
        (set-buffer-modified-p nil)
        (kill-buffer buffer))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ende-test/project-tools-edit-file-preserves-encoding-and-line-endings ()
  (let* ((root (make-temp-file "ende-tools-encoding-project-" t))
         (path (expand-file-name "main.txt" root)))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert "café\nkeep\n")
            (let ((coding-system-for-write 'utf-8-dos))
              (write-region (point-min) (point-max) path nil nil)))
          (let ((result
                 (ende-test--tool-result
                  "edit_file"
                  (list (cons "path" "main.txt")
                        (cons "edits"
                              (list (ende-test--anchor-edit
                                     "café" "replace" "naïve"))))
                  root)))
            (should (equal (plist-get result :source_kind) "file"))
            (should (equal (with-temp-buffer
                             (insert-file-contents path)
                             (buffer-string))
                           "naïve\nkeep\n"))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents-literally path)
              (should (equal (buffer-string)
                              (encode-coding-string
                               "naïve\nkeep\n" 'utf-8-dos))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ende-test/project-tools-edit-file-detects-a-stale-source-before-writing ()
  (let* ((root (make-temp-file "ende-tools-direct-project-" t))
         (path (expand-file-name "main.el" root))
         (ende-project-root-directory root)
         (original (symbol-function #'ende-tools--apply-change-set-object)))
    (unwind-protect
        (progn
          (with-temp-file path (insert "original\n"))
          (cl-letf (((symbol-function #'ende-tools--apply-change-set-object)
                     (lambda (change-set)
                       (with-temp-file path (insert "changed elsewhere\n"))
                       (funcall original change-set))))
            (let ((failure
                   (ende-test--edit-file-call
                    "main.el"
                    (list (ende-test--anchor-edit
                           "original" "replace" "new"))
                    root)))
              (should-not (plist-get failure :ok))
              (should (string-match-p "source changed"
                                      (plist-get failure :error))))
            (should (equal (with-temp-buffer
                             (insert-file-contents path)
                             (buffer-string))
                           "changed elsewhere\n"))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ende-test/project-tools-edit-file-leaves-disk-unchanged-on-invalid-batches ()
  (let* ((root (make-temp-file "ende-tools-invalid-edits-" t))
         (path (expand-file-name "main.txt" root))
         (ende-project-root-directory root))
    (unwind-protect
        (progn
          (with-temp-file path (insert "one one"))
          (dolist (request
                   (list
                    (list (cons "path" "main.txt")
                          (cons "edits"
                                (list (ende-test--anchor-edit
                                       "one" "replace" "two"))))
                    (list (cons "path" "main.txt")
                          (cons "edits"
                                (list (ende-test--anchor-edit
                                       "missing" "replace" "two"))))
                    (list (cons "path" "main.txt")
                          (cons "edits"
                                (list (ende-test--anchor-edit
                                       "one" "replace" "two")
                                      (ende-test--anchor-edit
                                       "absent" "after" "!"))))
                    (list (cons "path" "main.txt")
                          (cons "buffer_id" "not-accepted")
                          (cons "base_digest" "not-accepted")
                          (cons "edits" nil))
                    (list (cons "edits"
                                (list (ende-test--anchor-edit
                                       "one" "replace" "two"))))
                    (list (cons "path" "main.txt")
                          (cons "edits" nil))
                    (list (cons "path" "main.txt")
                          (cons "edits"
                                (list (append
                                       (ende-test--anchor-edit
                                        "one" "replace" "two")
                                       '(("reason" . "unknown"))))))))
            (should-not (plist-get (ende-tool-call "edit_file" request root)
                                   :ok))
            (should (equal (with-temp-buffer
                             (insert-file-contents path)
                             (buffer-string))
                           "one one")))
          (with-temp-file path (insert "abcdef"))
          (let ((overlap
                 (ende-test--edit-file-call
                  "main.txt"
                  (list (ende-test--anchor-edit
                         "abc" "replace" "A")
                        (ende-test--anchor-edit
                         "bcd" "replace" "B"))
                  root)))
            (should-not (plist-get overlap :ok))
            (should (equal (with-temp-buffer
                             (insert-file-contents path)
                             (buffer-string))
                           "abcdef"))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ende-test/project-tools-edit-file-anchor-positions-and-selection ()
  (let* ((root (make-temp-file "ende-tools-anchors-" t))
         (path (expand-file-name "main.txt" root))
         (ende-project-root-directory root))
    (unwind-protect
        (progn
          (with-temp-file path (insert "hello world\n"))
          (should (plist-get
                   (ende-test--edit-file-call
                    "main.txt"
                    (list (ende-test--anchor-edit "world" "before" "[")
                          (ende-test--anchor-edit "world" "after" "]"))
                    root)
                   :ok))
          (should (equal (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string))
                         "hello [world]\n"))
          (with-temp-file path (insert "ab"))
          (should (plist-get
                   (ende-test--edit-file-call
                    "main.txt"
                    (list (ende-test--anchor-edit "a" "after" "!")
                          (ende-test--anchor-edit "b" "replace" "B"))
                    root)
                   :ok))
          (should (equal (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string))
                         "a!B"))
          (with-temp-file path (insert "one one"))
          (should (plist-get
                   (ende-test--edit-file-call
                    "main.txt"
                    (list (ende-test--anchor-edit
                           "one" "replace" "two" '("occurrence" . 2)))
                    root)
                   :ok))
          (should (equal (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string))
                         "one two"))
          (with-temp-file path (insert "one one"))
          (should (plist-get
                   (ende-test--edit-file-call
                    "main.txt"
                    (list (ende-test--anchor-edit
                           "one" "replace" "ONE" '("all" . t)))
                    root)
                   :ok))
          (should (equal (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string))
                         "ONE ONE"))
          (with-temp-file path (insert "remove this keep"))
          (should (plist-get
                   (ende-test--edit-file-call
                    "main.txt"
                    (list (ende-test--anchor-edit
                           "remove this " "replace" ""))
                    root)
                   :ok))
          (should (equal (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string))
                         "keep"))
          (with-temp-file path (insert "first middle last"))
          (should (plist-get
                   (ende-test--edit-file-call
                    "main.txt"
                    (list (ende-test--anchor-edit
                           "first" "replace" "a much longer first")
                          (ende-test--anchor-edit
                           "last" "replace" "end"))
                    root)
                   :ok))
          (should (equal (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string))
                         "a much longer first middle end"))
          (with-temp-file path (insert "Hello"))
          (should-not (plist-get
                       (ende-test--edit-file-call
                        "main.txt"
                        (list (ende-test--anchor-edit
                               "hello" "replace" "bye"))
                        root)
                       :ok))
          (should (equal (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string))
                         "Hello")))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ende-test/project-tools-edit-file-honors-edit-expansion-limits ()
  (let* ((root (make-temp-file "ende-tools-edit-limits-" t))
         (path (expand-file-name "main.txt" root))
         (ende-project-root-directory root))
    (unwind-protect
        (progn
          (with-temp-file path (insert "a b"))
          (let* ((ende-tools-max-edits 1)
                 (too-many
                  (ende-test--edit-file-call
                   "main.txt"
                   (list (ende-test--anchor-edit "a" "replace" "A")
                         (ende-test--anchor-edit "b" "replace" "B"))
                   root)))
            (should-not (plist-get too-many :ok))
            (should (equal (with-temp-buffer
                             (insert-file-contents path)
                             (buffer-string))
                           "a b")))
          (with-temp-file path (insert "x x"))
          (let* ((ende-max-normalized-edits 1)
                 (expansion
                  (ende-test--edit-file-call
                   "main.txt"
                   (list (ende-test--anchor-edit
                          "x" "replace" "X" '("all" . t)))
                   root)))
            (should-not (plist-get expansion :ok))
            (should (equal (with-temp-buffer
                             (insert-file-contents path)
                             (buffer-string))
                           "x x"))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ende-test/project-tools-create-file-applies-immediately ()
  (let* ((root (make-temp-file "ende-tools-create-project-" t))
         (directory (expand-file-name "src" root))
         (path (expand-file-name "new.el" directory))
         (ende-project-root-directory root))
    (unwind-protect
        (progn
          (make-directory directory)
          (let ((result
                 (ende-test--tool-result
                  "create_file"
                  '(("path" . "src/new.el")
                    ("content" . ";;; new.el --- sample\n(provide 'new)\n"))
                  root)))
            (should (equal (plist-get result :operation) "create"))
            (should (equal (plist-get result :status) "applied"))
            (should (equal (plist-get result :path) "src/new.el"))
            (should-not (plist-member result :change_set_id))
            (should-not (plist-member result :content))
            (should (equal (with-temp-buffer
                             (insert-file-contents path)
                             (buffer-string))
                           ";;; new.el --- sample\n(provide 'new)\n")))
          (let* ((result
                  (ende-test--tool-result
                   "create_file"
                   '(("path" . "src/empty.el") ("content" . ""))
                   root))
                 (empty-path (expand-file-name "empty.el" directory)))
            (should (equal (plist-get result :status) "applied"))
            (should (file-exists-p empty-path))
            (should (= (file-attribute-size (file-attributes empty-path)) 0))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ende-test/project-tools-create-file-refuses-existing-target ()
  (let* ((root (make-temp-file "ende-tools-create-direct-" t))
         (existing (expand-file-name "existing.el" root))
         (new (expand-file-name "new.el" root))
         (ende-project-root-directory root))
    (unwind-protect
        (progn
          (with-temp-file existing (insert "original"))
          (let ((failure
                 (ende-test--create-file-call "existing.el" "overwrite" root)))
            (should-not (plist-get failure :ok))
            (should (string-match-p "already exists; use edit_file"
                                    (plist-get failure :error)))
            (should (equal (with-temp-buffer
                             (insert-file-contents existing)
                             (buffer-string))
                           "original")))
          (let ((result (ende-test--tool-result
                         "create_file"
                         '(("path" . "new.el") ("content" . "created\n"))
                         root)))
            (should (equal (plist-get result :operation) "create"))
            (should (equal (plist-get result :status) "applied"))
            (should-not (plist-member result :change_set_id))
            (should (equal (with-temp-buffer
                             (insert-file-contents new)
                             (buffer-string))
                           "created\n"))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ende-test/project-tools-create-file-is-exclusive-at-write-time ()
  (let* ((root (make-temp-file "ende-tools-create-race-" t))
         (path (expand-file-name "race.el" root))
         (ende-project-root-directory root)
         (original (symbol-function #'ende-tools--write-new-file)))
    (unwind-protect
        (cl-letf (((symbol-function #'ende-tools--write-new-file)
                   (lambda (content target)
                     (with-temp-file target (insert "won the race"))
                     (funcall original content target))))
          (let ((failure
                 (ende-test--create-file-call "race.el" "proposal" root)))
            (should-not (plist-get failure :ok)))
          (should (equal (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string))
                         "won the race")))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ende-test/project-tools-create-file-validates-path-and-symlinks ()
  (let* ((root (make-temp-file "ende-tools-create-paths-" t))
         (outside (make-temp-file "ende-tools-create-outside-" t))
         (link (expand-file-name "dangling-link" root))
         (absolute (expand-file-name "absolute-target" root))
         (ende-project-root-directory root))
    (unwind-protect
        (progn
          (make-symbolic-link "missing-target" link)
          (make-symbolic-link outside (expand-file-name "outside-dir" root))
          (dolist (path (list "missing-dir/new.el"
                              "../outside.el"
                              "outside-dir/escaped.el"
                              absolute
                              ".env"
                              "dangling-link"))
            (should-not (plist-get
                         (ende-test--create-file-call path "content" root)
                         :ok)))
          (should (file-symlink-p link))
          (should-not (file-exists-p (expand-file-name "missing-dir" root)))
          (should-not (file-exists-p absolute)))
      (when (file-directory-p root)
        (delete-directory root t))
      (when (file-directory-p outside)
        (delete-directory outside t)))))

(provide 'ende-tools-test)

;;; ende-tools-test.el ends here
