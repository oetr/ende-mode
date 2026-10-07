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

(defun ende-test--apply-patch-call (patch root &optional access-config)
  "Call apply_patch with PATCH within ROOT and optional ACCESS-CONFIG."
  (ende-tool-call "apply_patch" (list (cons "patch" patch))
                  root access-config))

(ert-deftest ende-test/project-tools-expose-json-definitions ()
  (let ((definitions (ende-tool-definitions)))
    (let ((names (mapcar (lambda (definition)
                           (alist-get :name (alist-get :function definition)))
                         definitions)))
      (should (= (length names) (length (delete-dups (copy-sequence names)))))
      (dolist (required '("about_thinking" "project_info" "list_project_files" "file_info"
                          "read_file" "list_project_buffers" "read_buffer"
                          "search_project" "edit_file" "create_file"
                          "delete_file" "apply_patch" "web_fetch"))
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

(ert-deftest ende-test/project-tools-delete-file-checks-digest-and-buffer-state ()
  (let* ((root (make-temp-file "ende-tools-delete-" t))
         (path (expand-file-name "src/main.el" root))
         (outside (make-temp-file "ende-tools-delete-outside-"))
         (link (expand-file-name "src/link.el" root))
         (buffer nil))
    (unwind-protect
        (progn
          (make-directory (file-name-directory path))
          (with-temp-file path (insert "disk source\n"))
          (make-symbolic-link outside link)
          (let* ((info (ende-test--tool-result
                        "file_info" '(("path" . "src/main.el")) root))
                 (digest (plist-get info :digest))
                 (stale (ende-tool-call
                         "delete_file"
                         (list :path "src/main.el" :expected_digest (make-string 64 ?0))
                         root)))
            (should-not (plist-get stale :ok))
            (should (file-exists-p path))
            (should
             (string-match-p
              "symlink"
              (plist-get
               (ende-tool-call "delete_file"
                               (list :path "src/link.el") root)
               :error)))
            (dolist (bad-path
                     (list "../outside" (expand-file-name "src/main.el" root)
                           "src/../src/main.el" "src/" "src"))
              (should-not (plist-get
                           (ende-tool-call "delete_file"
                                           (list :path bad-path) root)
                           :ok)))
            (setq buffer (find-file-noselect path))
            (let ((deleted
                   (ende-tool-call
                    "delete_file"
                    (list :path "src/main.el" :expected_digest digest) root)))
              (should (plist-get deleted :ok))
              (let ((result (plist-get deleted :result)))
                (should (equal (plist-get result :operation) "delete"))
                (should (equal (plist-get result :status) "applied"))
                (should (equal (plist-get result :path) "src/main.el"))
                (should (plist-get result :visiting_buffer))
                (should (= (plist-get result :visiting_buffer_count) 1))
                (should (string-match-p "recreate"
                                        (plist-get result
                                                   :visiting_buffer_warning))))
              (should-not (file-exists-p path))
              (should (buffer-live-p buffer))
              (with-current-buffer buffer
                (should (equal (buffer-string) "disk source\n"))
                (should-not (buffer-modified-p))))
            (with-current-buffer buffer
              (erase-buffer)
              (insert "unsaved text\n")
              (set-buffer-modified-p t))
            (with-temp-file path (insert "disk source\n"))
            (let ((failure (ende-tool-call
                            "delete_file" '(("path" . "src/main.el")) root)))
              (should-not (plist-get failure :ok))
              (should (string-match-p "unsaved visiting buffer"
                                      (plist-get failure :error)))
              (should (file-exists-p path))
              (with-current-buffer buffer
                (should (equal (buffer-string) "unsaved text\n"))
                (should (buffer-modified-p))))))
      (when (buffer-live-p buffer)
        (set-buffer-modified-p nil)
        (kill-buffer buffer))
      (when (file-directory-p root)
        (delete-directory root t))
      (when (file-exists-p outside)
        (delete-file outside)))))

(ert-deftest ende-test/project-tools-delete-honors-trash-without-fallback ()
  (let* ((root (make-temp-file "ende-tools-trash-project-" t))
         (trash (make-temp-file "ende-tools-trash-dir-" t))
         (path (expand-file-name "discard.txt" root))
         (destination (expand-file-name "discard.txt" trash))
         (delete-by-moving-to-trash t)
         (calls 0))
    (unwind-protect
        (progn
          (with-temp-file path (insert "trash me\n"))
          (cl-letf (((symbol-function 'move-file-to-trash)
                     (lambda (_path)
                       (cl-incf calls)
                       (error "trash backend failed"))))
            (let ((failure (ende-tool-call
                            "delete_file" '(("path" . "discard.txt")) root)))
              (should-not (plist-get failure :ok))
              (should (string-match-p "trash backend failed"
                                      (plist-get failure :error)))
              (should (= calls 1))
              (should (file-exists-p path))))
          (cl-letf (((symbol-function 'move-file-to-trash)
                     (lambda (source)
                       (cl-incf calls)
                       (rename-file source destination))))
            (let ((result (ende-tool-call
                           "delete_file" '(("path" . "discard.txt")) root)))
              (should (plist-get result :ok))
              (should (= calls 2))
              (should-not (file-exists-p path))
              (should (file-exists-p destination)))))
      (when (file-directory-p root)
        (delete-directory root t))
      (when (file-directory-p trash)
        (delete-directory trash t)))))

(ert-deftest ende-test/project-tools-apply-patch-commits-mixed-disk-operations ()
  (let* ((root (make-temp-file "ende-tools-patch-project-" t))
         (source (expand-file-name "src/main.txt" root))
         (added (expand-file-name "src/new.txt" root))
         (deleted (expand-file-name "tmp/old.txt" root))
         (buffer nil)
         (patch (concat
                 "*** Begin Patch\n"
                 "*** Add File: src/new.txt\n+new text\n"
                 "*** Update File: src/main.txt\n@@\n before\n-old\n+updated\n"
                 "*** Delete File: tmp/old.txt\n"
                 "*** End Patch\n")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory source) t)
          (make-directory (file-name-directory deleted) t)
          (with-temp-buffer
            (insert "before\nold\n")
            (let ((coding-system-for-write 'utf-8-dos))
              (write-region (point-min) (point-max) source nil 'silent)))
          (set-file-modes source #o640)
          (with-temp-file deleted (insert "discard\n"))
          (setq buffer (find-file-noselect source))
          (let ((result (ende-test--apply-patch-call patch root)))
            (should (plist-get result :ok))
            (let* ((payload (plist-get result :result))
                   (files (plist-get payload :files)))
              (should (vectorp files))
              (should (= (length files) 3))
              (should (equal (mapcar (lambda (file)
                                       (plist-get file :operation))
                                     (append files nil))
                             '("add" "update" "delete")))
              (should (equal (mapcar (lambda (file)
                                       (plist-get file :status))
                                     (append files nil))
                             '("applied" "applied" "applied")))
              (should (plist-get (nth 1 (append files nil))
                                 :visiting_buffer_warning)))
            (should (equal (with-temp-buffer
                             (insert-file-contents added)
                             (buffer-string))
                           "new text\n"))
            (should (equal (with-temp-buffer
                             (insert-file-contents source)
                             (buffer-string))
                           "before\nupdated\n"))
            (should (= (logand (file-modes source) #o777) #o640))
            (should-not (file-exists-p deleted))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents-literally source)
              (should (equal (buffer-string)
                             (encode-coding-string
                              "before\nupdated\n" 'utf-8-dos))))
            (with-current-buffer buffer
              (should (equal (buffer-string) "before\nold\n"))
              (should-not (buffer-modified-p)))))
      (when (buffer-live-p buffer)
        (set-buffer-modified-p nil)
        (kill-buffer buffer))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ende-test/project-tools-apply-patch-preflight-leaves-files-alone ()
  (let* ((root (make-temp-file "ende-tools-patch-preflight-" t))
         (path (expand-file-name "main.txt" root))
         (buffer nil))
    (unwind-protect
        (progn
          (with-temp-file path (insert "first\nsecond\n"))
          (setq buffer (find-file-noselect path))
          (with-current-buffer buffer
            (goto-char (point-max))
            (insert "unsaved\n")
            (set-buffer-modified-p t))
          (let ((dirty-patch
                 (concat "*** Begin Patch\n"
                         "*** Add File: new.txt\n+never written\n"
                         "*** Update File: main.txt\n@@\n first\n-second\n+changed\n"
                         "*** End Patch\n")))
            (let ((failure (ende-test--apply-patch-call dirty-patch root)))
              (should-not (plist-get failure :ok))
              (should (string-match-p "unsaved visiting buffer"
                                      (plist-get failure :error)))
              (should-not (file-exists-p (expand-file-name "new.txt" root)))
              (should (equal (with-temp-buffer
                               (insert-file-contents path)
                               (buffer-string))
                             "first\nsecond\n"))
              (with-current-buffer buffer
                (should (string-suffix-p "unsaved\n" (buffer-string)))
                (should (buffer-modified-p)))))
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (dolist (patch
                   (list
                    "*** Begin Patch\n*** Update File: main.txt\n@@\n -absent\n+bad\n*** End Patch"
                    "*** Begin Patch\n*** Add File: main.txt\n+overwrite\n*** End Patch"))
            (let ((before (with-temp-buffer
                            (insert-file-contents path)
                            (buffer-string))))
              (should-not (plist-get (ende-test--apply-patch-call patch root)
                                     :ok))
              (should (equal (with-temp-buffer
                               (insert-file-contents path)
                               (buffer-string))
                             before)))))
      (with-temp-file path (insert "first\nfirst\n"))
      (let ((ambiguous
             (concat "*** Begin Patch\n*** Update File: main.txt\n"
                     "@@\n first\n+insert\n*** End Patch\n")))
        (should-not (plist-get (ende-test--apply-patch-call ambiguous root)
                               :ok))
        (should (equal (with-temp-buffer
                         (insert-file-contents path)
                         (buffer-string))
                       "first\nfirst\n")))
      (when (buffer-live-p buffer)
        (set-buffer-modified-p nil)
        (kill-buffer buffer))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ende-test/project-tools-apply-patch-rolls-back-failed-commit ()
  (let* ((root (make-temp-file "ende-tools-patch-rollback-" t))
         (first (expand-file-name "first.txt" root))
         (second (expand-file-name "second.txt" root))
         (patch (concat "*** Begin Patch\n"
                        "*** Update File: first.txt\n@@\n before\n-old\n+new\n"
                        "*** Update File: second.txt\n@@\n before\n-old\n+new\n"
                        "*** End Patch\n"))
         (commit (symbol-function #'ende-tools--commit-change-set))
         (count 0))
    (unwind-protect
        (progn
          (with-temp-file first (insert "before\nold\n"))
          (with-temp-file second (insert "before\nold\n"))
          (cl-letf (((symbol-function 'ende-tools--commit-change-set)
                     (lambda (change-set)
                       (prog1 (funcall commit change-set)
                         (cl-incf count)
                         (when (= count 1)
                           (error "injected commit failure"))))))
            (let* ((failure (ende-test--apply-patch-call patch root))
                   (report (plist-get failure :recovery)))
              (should-not (plist-get failure :ok))
              (should (string-match-p "injected commit failure"
                                      (plist-get failure :error)))
              (should (vectorp report))
              (should (= (length report) 2))
              (let ((first-report
                     (cl-find-if (lambda (entry)
                                   (equal (plist-get entry :path) "first.txt"))
                                 (append report nil)))
                    (second-report
                     (cl-find-if (lambda (entry)
                                   (equal (plist-get entry :path) "second.txt"))
                                 (append report nil))))
                (should (equal (plist-get first-report :status) "restored"))
                (should (equal (plist-get second-report :status)
                               "not_applied")))
              (dolist (path (list first second))
                (should (equal (with-temp-buffer
                                 (insert-file-contents path)
                                 (buffer-string))
                               "before\nold\n"))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ende-test/project-tools-apply-patch-retains-unsafe-recovery ()
  (let* ((root (make-temp-file "ende-tools-patch-recovery-" t))
         (path (expand-file-name "source.txt" root))
         (patch (concat "*** Begin Patch\n*** Update File: source.txt\n"
                        "@@\n before\n-old\n+patched\n*** End Patch\n"))
         (commit (symbol-function #'ende-tools--commit-change-set))
         (recovery nil))
    (unwind-protect
        (progn
          (with-temp-file path (insert "before\nold\n"))
          (cl-letf (((symbol-function 'ende-tools--commit-change-set)
                     (lambda (change-set)
                       (prog1 (funcall commit change-set)
                         (with-temp-file path (insert "concurrent edit\n"))
                         (error "fail after concurrent change")))))
            (let* ((failure (ende-test--apply-patch-call patch root))
                   (report (plist-get failure :recovery))
                   (entry (aref report 0)))
              (should-not (plist-get failure :ok))
              (should (equal (plist-get entry :status)
                             "concurrent_change_preserved"))
              (setq recovery (expand-file-name
                              (plist-get entry :recovery_file) root))
              (should (file-exists-p recovery))
              (should (= (logand (file-modes recovery) #o777) #o600))
              (should (equal (with-temp-buffer
                               (insert-file-contents recovery)
                               (buffer-string))
                             "before\nold\n"))
              (should (equal (with-temp-buffer
                               (insert-file-contents path)
                               (buffer-string))
                             "concurrent edit\n")))))
      (when (and recovery (file-exists-p recovery))
        (delete-file recovery))
      (when (file-directory-p root)
        (delete-directory root t)))))

(provide 'ende-tools-test)

;;; ende-tools-test.el ends here
