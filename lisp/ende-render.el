;;; ende-render.el --- Pure tool and reply presentations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter Samarin
;;
;; Author: Peter Samarin <peter.samarin@gmail.com>
;; Maintainer: Peter Samarin <peter.samarin@gmail.com>
;; Keywords: convenience, tools
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

;; Presentations contain only strings, semantic state, and folding intent.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ende-protocol)
(require 'ende-patch)

(defface ende-render-request-face '((t :inherit font-lock-keyword-face))
  "Tool request headings." :group 'ende)
(defface ende-render-success-face '((t :inherit success))
  "Successful outcomes and added lines." :group 'ende)
(defface ende-render-warning-face '((t :inherit warning))
  "Incomplete outcomes." :group 'ende)
(defface ende-render-error-face '((t :inherit error))
  "Failed outcomes and removed lines." :group 'ende)
(defface ende-render-subdued-face '((t :inherit shadow))
  "Context and line numbers." :group 'ende)

(defconst ende-render--missing (make-symbol "missing"))

(defconst ende-render--tools
  '(("project_info" project "file_count")
    ("list_project_files" files "files") ("file_info" metadata "line_count")
    ("list_project_buffers" buffers "buffers")
    ("read_file" read "content") ("read_buffer" read "content")
    ("search_project" search "files")
    ("edit_file" edit "status") ("create_file" create "status")
    ("delete_file" delete "status") ("copy_from_sandbox" copy "status")
    ("apply_patch" patch "files")
    ("run_host_command" command "exit_code") ("run_sandbox_command" command "exit_code")
    ("web_fetch" web "status") ("generate_image" image "artifacts")
    ("spawn_agent" agent nil) ("about_thinking" activity "accepted")
    ("generate_title" title "accepted"))
  "Static (NAME FAMILY RESULT-FIELD) table, not an execution registry.
RESULT-FIELD gates specialization; unfamiliar results remain readable.")

(defun ende-render--decode (content)
  "Decode complete JSON CONTENT once, retaining non-JSON text verbatim."
  (if (stringp content)
      (condition-case nil (ende-protocol-json-read-string content)
        (error content))
    content))

(defun ende-render--key (key)
  "Return the string spelling of object KEY."
  (if (keywordp key) (substring (symbol-name key) 1)
    (if (symbolp key) (symbol-name key) key)))

(defun ende-render--object-p (value)
  "Whether VALUE is a decoded object."
  (or (hash-table-p value)
      (and (consp value)
           (or (keywordp (car value)) (consp (car value))))))

(defun ende-render--pairs (object)
  "Return fresh key/value pairs for OBJECT without changing it."
  (cond
   ((hash-table-p object)
    (let (pairs)
      (maphash (lambda (key value) (push (cons key value) pairs)) object)
      (nreverse pairs)))
   ((keywordp (car-safe object))
    (cl-loop for (key value) on object by #'cddr
             collect (cons (ende-render--key key) value)))
   ((consp object)
    (mapcar (lambda (pair) (cons (ende-render--key (car pair)) (cdr pair)))
            object))))

(defun ende-render--get (object key &optional default)
  "Return OBJECT's KEY, using DEFAULT only when it is absent."
  (if (hash-table-p object) (gethash key object default)
    (let ((pair (assoc key (ende-render--pairs object))))
      (if pair (cdr pair) default))))

(defun ende-render--has (object key)
  "Whether OBJECT contains KEY, including null and false values."
  (not (eq (ende-render--get object key ende-render--missing)
           ende-render--missing)))

(defun ende-render--items (value)
  "Return decoded array VALUE as a list."
  (cond ((and (vectorp value) (not (stringp value))) (append value nil))
        ((consp value) value)
        (t (error "Not an array"))))

(defun ende-render--false-p (value)
  "Whether VALUE is an explicit JSON false."
  (memq value '(:false :json-false)))

(defun ende-render--label (key)
  "Make a readable label from KEY."
  (capitalize (replace-regexp-in-string "_" " " (ende-render--key key))))

(defun ende-render--join (&rest parts)
  "Join nonempty string PARTS as paragraphs."
  (mapconcat #'identity
             (cl-remove-if (lambda (s) (or (null s) (equal s ""))) parts)
             "\n\n"))

(defun ende-render--value (value)
  "Format decoded VALUE without decoding nested strings."
  (cond
   ((stringp value) (if (string-empty-p value) "\"\"" (copy-sequence value)))
   ((memq value '(nil :null :json-null)) "null")
   ((eq value t) "true")
   ((ende-render--false-p value) "false")
   ((ende-render--object-p value)
    (let ((pairs (ende-render--pairs value)))
      (if (null pairs) "{}"
        (mapconcat
         (lambda (pair)
           (let ((text (ende-render--value (cdr pair))))
             (concat (ende-render--label (car pair)) ":"
                     (if (string-match-p "\n" text)
                         (concat "\n  " (replace-regexp-in-string "\n" "\n  " text))
                       (concat " " text)))))
         pairs "\n"))))
   ((or (vectorp value) (listp value))
    (let ((items (ende-render--items value)))
      (if (null items) "[]"
        (mapconcat (lambda (item)
                     (concat "- " (replace-regexp-in-string
                                   "\n" "\n  " (ende-render--value item))))
                   items "\n"))))
   (t (format "%s" value))))

(defun ende-render--readable (value)
  "Return readable decoded VALUE, preserving plain strings."
  (condition-case nil
      (if (stringp value) (copy-sequence value) (ende-render--value value))
    (error (let ((print-circle t)) (prin1-to-string value)))))

(defun ende-render-readable (content)
  "Return readable CONTENT, a payload string or decoded object."
  (save-match-data (ende-render--readable (ende-render--decode content))))

(defun ende-render--fields (object keys)
  "Format the present KEYS of OBJECT, in order."
  (mapconcat
   (lambda (key)
     (concat (ende-render--label key) ": "
             (ende-render--value (ende-render--get object key))))
   (cl-remove-if-not (lambda (key) (ende-render--has object key)) keys) "\n"))

(defun ende-render--presentation (summary body &optional state open)
  "Make an independent presentation from SUMMARY, BODY, STATE and OPEN."
  (list :summary (copy-sequence
                  (string-trim (replace-regexp-in-string "[\n\r\t]+" " " summary)))
        :body (copy-sequence (or body ""))
        :state (or state 'neutral) :folded (not open)))

(defun ende-render--target (object &optional arguments)
  "Return the target named by OBJECT or request ARGUMENTS."
  (or (ende-render--get object "path")
      (ende-render--get object "buffer_id")
      (ende-render--get object "destination")
      (ende-render--get arguments "path")
      (ende-render--get arguments "buffer_id")
      (ende-render--get arguments "destination") ""))

(defun ende-render--count (n noun)
  "Format count N of NOUN."
  (format "%d %s%s" n noun (if (= n 1) "" "s")))

(defun ende-render--source-summary (source &optional kind)
  "Compactly describe SOURCE's path and sizes, with optional source KIND.
Only buffer listings include live buffer identity and modification state."
  (mapconcat
   #'identity
   (delq nil
         (append
          (list (ende-render--get source "path") kind
                (when (equal kind "buffer") (ende-render--get source "buffer_id"))
                (when (and (equal kind "buffer") (eq (ende-render--get source "buffer_modified") t))
                  "modified"))
          (cl-loop for (key . noun) in '(("line_count" . "line") ("byte_count" . "byte"))
                   when (ende-render--has source key)
                   collect (ende-render--count (ende-render--get source key) noun))))
   " · "))

(defun ende-render--marked (text face)
  "Copy TEXT and mark it with semantic FACE for the host fontifier."
  (propertize (copy-sequence text) 'ende-render-face face))

(defun ende-render--lines (text)
  "Split TEXT into source lines, without inventing a final empty line."
  (unless (string-empty-p text)
    (split-string (if (string-suffix-p "\n" text) (substring text 0 -1) text)
                  "\n" nil)))

(defun ende-render--numbered (text start)
  "Number source TEXT beginning at START."
  (mapconcat (lambda (line)
               (prog1 (concat (ende-render--marked
                               (format "%4d │ " start) 'ende-render-subdued-face)
                              line)
                 (cl-incf start)))
             (ende-render--lines text) "\n"))

(defun ende-render--diff-lines (text prefix face)
  "Prefix TEXT lines with PREFIX and annotate with FACE."
  (mapconcat (lambda (line) (ende-render--marked (concat prefix line) face))
             (ende-render--lines text) "\n"))

(defun ende-render--edit-preview (edit)
  "Present one proposed anchor EDIT, not an applied diff."
  (let* ((position (ende-render--get edit "position"))
         (anchor (ende-render--get edit "anchor"))
         (text (ende-render--get edit "text"))
         (context (ende-render--diff-lines anchor "  " 'ende-render-subdued-face))
         (added (ende-render--diff-lines text "+ " 'ende-render-success-face)))
    (ende-render--join
     (concat (capitalize position)
             (cond ((eq (ende-render--get edit "all") t) " · all occurrences")
                   ((ende-render--has edit "occurrence")
                    (format " · occurrence %s" (ende-render--get edit "occurrence")))
                   (t "")))
     (pcase position
       ("replace" (ende-render--join
                   (ende-render--diff-lines anchor "- " 'ende-render-error-face)
                   added))
       ("before" (ende-render--join added context))
       ("after" (ende-render--join context added))
       (_ (error "Unknown edit position"))))))

(defun ende-render--patch-preview (patch)
  "Annotate proposed PATCH lines without parsing or applying operations."
  (mapconcat
   (lambda (line)
     (ende-render--marked
      line (cond ((string-prefix-p "+" line) 'ende-render-success-face)
                 ((string-prefix-p "-" line) 'ende-render-error-face)
                 (t 'ende-render-subdued-face))))
   (ende-render--lines patch) "\n"))

(defun ende-render--command (name arguments)
  "Display NAME's command in ARGUMENTS, keeping literal host argv explicit."
  (let ((command (ende-render--get arguments "command")))
    (if (equal name "run_host_command")
        (concat (ende-render--get command "program")
                (mapconcat (lambda (arg) (concat " " (prin1-to-string arg)))
                           (ende-render--items (ende-render--get command "args")) ""))
      (or command ""))))

(defun ende-render--request (name args)
  "Format the decoded request ARGS for static tool NAME."
  (let ((target (ende-render--target args)) summary body)
    (pcase (cadr (assoc name ende-render--tools))
      ((or 'project 'buffers) (setq summary ""))
      ('files
       (setq summary (or (ende-render--get args "glob") "")
             body (ende-render--fields args '("glob" "max_results" "include_metadata"))))
      ('metadata (setq summary target))
      ('read
       (let ((start (ende-render--get args "start_line" 1))
             (count (ende-render--get args "line_count")))
         (setq summary (concat target " · "
                               (if count (format "lines %d–%d" start (+ start count -1))
                                 (format "from line %d" start)))
               body (ende-render--fields args '("start_line" "line_count")))))
      ('search
       (let* ((selector (cl-find-if (lambda (key) (ende-render--has args key))
                                    '("query" "patterns" "regexp")))
              (value (ende-render--get args selector)))
         (unless selector (error "Missing search selector"))
         (setq summary (concat selector " "
                               (if (equal selector "patterns")
                                   (mapconcat #'prin1-to-string (ende-render--items value) ", ")
                                 (prin1-to-string value))
                               (when (ende-render--has args "glob")
                                 (concat " · " (ende-render--get args "glob"))))
               body (ende-render--fields args '("query" "patterns" "regexp" "glob"
                                                "max_results" "context_before" "context_after")))))
      ('edit
       (let ((edits (ende-render--items (ende-render--get args "edits"))))
         (setq summary (format "%s · %s proposed" target
                               (ende-render--count (length edits) "anchor edit"))
               body (mapconcat #'ende-render--edit-preview edits "\n\n"))))
      ('create
       (let ((content (ende-render--get args "content")))
         (setq summary (format "%s · %s" target
                               (ende-render--count (string-bytes content) "byte"))
               body (ende-render--diff-lines content "+ " 'ende-render-success-face))))
      ('delete
       (setq summary target
             body (ende-render--fields args '("expected_digest"))))
      ('patch
       (let* ((patch (ende-render--get args "patch"))
              (paths (cl-loop for line in (ende-patch--lines patch)
                              for header = (ende-patch--operation-header line)
                              when header collect (cdr header))))
         (setq summary (mapconcat #'identity (delete-dups paths) ", ")
               body (ende-render--patch-preview patch))))
      ('copy
       (setq summary (concat (ende-render--get args "source") " → " target)
             body (ende-render--fields args '("source" "destination"))))
      ('command
       (let ((command (ende-render--command name args)))
         (setq summary command
               body (ende-render--fields args '("cwd" "timeout")))))
      ('web
       (setq summary (ende-render--get args "url")
             body (ende-render--fields args '("url" "max_chars"))))
      ('image
       (setq summary (concat (ende-render--get args "prompt")
                             (let ((width (ende-render--get args "width"))
                                   (height (ende-render--get args "height")))
                               (cond ((and width height) (format " · %s×%s" width height))
                                     (width (format " · width %s" width))
                                     (height (format " · height %s" height)))))
             body (ende-render--readable args)))
      ('agent
       (setq summary (ende-render--get args "title")
             body (ende-render--get args "task")))
      ('activity
       (setq summary (ende-render--get args "title") body (ende-render--get args "status")))
      ('title (setq summary (ende-render--get args "title")))
      (_ (setq summary "Request" body (ende-render--readable args))))
    (ende-render--presentation summary body)))

(defun ende-render-tool-request (name arguments)
  "Present tool NAME's ARGUMENTS without executing or preparing anything."
  (save-match-data
    (let ((args (ende-render--decode arguments))
          (name (if (symbolp name) (symbol-name name) name)))
      (condition-case nil
          (if (ende-render--object-p args)
              (ende-render--request name args)
            (ende-render--presentation "Request" (ende-render--readable args)))
        (error (ende-render--presentation "Request"
                                         (ende-render--readable args)))))))

(defun ende-render--read-result (name result args)
  "Present the returned source range in RESULT for NAME and ARGS."
  (let* ((start (ende-render--get result "start_line"))
         (end (ende-render--get result "end_line"))
         (empty (< end start))
         (excerpt (or (eq (ende-render--get result "truncated") t)
                      (> start 1))))
    (ende-render--presentation
     (format "%s %s · %s%s"
             (if (equal name "read_file") "Disk" "Buffer")
             (ende-render--target result args)
             (if empty "empty" (format "lines %d–%d" start end))
             (if excerpt " · excerpt" ""))
     (ende-render--join
      (ende-render--fields result '("source_kind" "start_line" "end_line" "total_lines"))
      (if empty "(empty source)"
        (ende-render--numbered (ende-render--get result "content") start))))))

(defun ende-render--search-result (result)
  "Present returned matching source lines from RESULT, not inspected totals."
  (let* ((files (ende-render--items (ende-render--get result "files")))
         (count (cl-loop for file in files
                         sum (length (ende-render--items (ende-render--get file "matches"))))))
    (ende-render--presentation
     (format "%s in %s%s" (ende-render--count count "matching line")
             (ende-render--count (length files) "file")
             (if (eq (ende-render--get result "truncated") t) " · returned, truncated" ""))
     (ende-render--join
      (ende-render--fields result '("files_inspected"))
      (mapconcat
       (lambda (file)
         (ende-render--join
          (ende-render--get file "path")
          (mapconcat
           (lambda (match)
             (ende-render--join
              (concat (ende-render--marked
                       (format "%s:%s │ " (ende-render--get match "line")
                               (ende-render--get match "column")) 'ende-render-subdued-face)
                      (ende-render--get match "match"))
              (when (ende-render--has match "context")
                (concat "Context:\n" (ende-render--get match "context")))))
           (ende-render--items (ende-render--get file "matches")) "\n\n")))
       files "\n\n")))))

(defun ende-render--mutation-result (name result args)
  "Present actual file operations in RESULT for NAME and ARGS."
  (if (equal name "apply_patch")
      (let ((files (ende-render--items (ende-render--get result "files"))))
        (ende-render--presentation
         (concat (if (and files (cl-every
                                (lambda (file) (equal (ende-render--get file "status") "applied"))
                                files))
                     "Patch applied · " "Patch result · ")
                 (ende-render--count (length files) "file"))
         (mapconcat (lambda (file)
                      (format "%s · %s · %s" (ende-render--get file "path")
                              (ende-render--get file "operation")
                              (ende-render--get file "status")))
                    files "\n")))
    (let* ((applied (equal (ende-render--get result "status") "applied"))
           (action (if applied
                       (pcase name ("edit_file" "Edited") ("create_file" "Created")
                              ("delete_file" "Deleted") (_ "Copied to"))
                     "File result")))
      (ende-render--presentation
       (concat action " " (ende-render--target result args)
               (if (and (equal name "edit_file") (ende-render--has result "edit_count"))
                   (concat " · " (ende-render--count (ende-render--get result "edit_count") "edit")) ""))
       (ende-render--fields result '("operation" "path" "status" "edit_count"))))))

(defun ende-render--command-result (result)
  "Present the process outcome in RESULT, with useful output first."
  (let ((failed (or (ende-render--false-p (ende-render--get result "process_ok"))
                    (and (numberp (ende-render--get result "exit_code"))
                         (/= (ende-render--get result "exit_code") 0))))
        (truncated (or (eq (ende-render--get result "stdout_truncated") t)
                       (eq (ende-render--get result "stderr_truncated") t))))
    (ende-render--presentation
     (concat (if (numberp (ende-render--get result "exit_code"))
                 (format "exit %s" (ende-render--get result "exit_code")) "Response returned")
             (if failed " · failed" "") (if truncated " · output truncated" ""))
     (mapconcat
      (lambda (key)
        (concat key (when (eq (ende-render--get result (concat key "_truncated")) t)
                      " (truncated)")
                ":\n" (ende-render--get result key)))
      (cl-remove-if-not
       (lambda (key)
         (let ((text (ende-render--get result key)))
           (and (stringp text) (not (string-empty-p text)))))
       (if failed '("stderr" "stdout") '("stdout" "stderr")))
      "\n\n")
     (cond (failed 'error) (truncated 'warning)
           ((eq (ende-render--get result "process_ok") t) 'success) (t 'neutral)))))

(defun ende-render--response (name result args)
  "Format decoded RESULT for static tool NAME with optional ARGS."
  (let ((field (caddr (assoc name ende-render--tools))))
    (when (and field (not (ende-render--has result field)))
      (error "Unfamiliar result shape")))
  (pcase (cadr (assoc name ende-render--tools))
    ('project
     (ende-render--presentation
      (format "%s · %s · %s" (or (ende-render--get result "name") "")
              (ende-render--count (ende-render--get result "file_count") "file")
              (ende-render--count (ende-render--get result "buffer_count") "buffer"))
      (ende-render--fields result '("root"))))
    ((or 'files 'buffers)
     (let* ((buffers (equal name "list_project_buffers"))
            (items (ende-render--items (ende-render--get result (if buffers "buffers" "files")))))
       (ende-render--presentation
        (concat (ende-render--count (length items) (if buffers "buffer" "file")) " returned"
                (if (eq (ende-render--get result "truncated") t) " · truncated" ""))
        (mapconcat (lambda (item)
                     (ende-render--source-summary item (when buffers "buffer")))
                   items "\n"))))
    ('metadata
     (ende-render--presentation
      (ende-render--source-summary result "disk")
      (ende-render--fields result '("disk_mtime"))))
    ('read (ende-render--read-result name result args))
    ('search (ende-render--search-result result))
    ((or 'edit 'create 'delete 'copy 'patch)
     (ende-render--mutation-result name result args))
    ('command (ende-render--command-result result))
    ('web
     (let ((status (ende-render--get result "status")))
       (ende-render--presentation
        (format "HTTP %s%s · %s" status
                (if (eq (ende-render--get result "truncated") t) " · truncated" "")
                (ende-render--get result "final_url"))
        (ende-render--join
         (ende-render--fields result '("requested_url" "final_url" "status" "content_type"))
         (ende-render--get result "content"))
        (when (and (numberp status) (>= status 400)) 'error))))
    ('image
     (let ((artifacts (ende-render--items (ende-render--get result "artifacts"))))
       (ende-render--presentation
        (concat (ende-render--count (length artifacts) "image artifact") " returned")
        (ende-render--join (ende-render--fields args '("caption"))
                           (mapconcat #'identity artifacts "\n")))))
    ('agent
     (ende-render--presentation
      (concat "Agent response returned"
              (when (ende-render--has args "title") (concat " · " (ende-render--get args "title"))))
      (ende-render--readable result)))
    ((or 'activity 'title)
     (if (eq (ende-render--get result "accepted") t)
         (ende-render--presentation
          (if (equal name "about_thinking") "Activity accepted" "Title accepted") "")
       (ende-render--presentation "Response returned" (ende-render--readable result))))
    (_ (ende-render--presentation "Response returned" (ende-render--readable result)))))

(defun ende-render--response-envelope (name value args)
  "Present decoded VALUE and its outcome for NAME, using request ARGS."
  (let* ((envelope (and (ende-render--object-p value) (ende-render--has value "ok")))
         (result (if (and envelope (ende-render--has value "result"))
                     (ende-render--get value "result") value))
         (failed (and envelope
                      (or (ende-render--false-p (ende-render--get value "ok"))
                          (ende-render--has value "error"))))
         (recovery (ende-render--get value "recovery"))
         (warning (ende-render--get result "sandbox_warning")))
    (condition-case nil
        (let ((presentation
               (cond
                (failed
                 (ende-render--presentation
                  (concat "Failed"
                          (when recovery
                            (if (cl-some (lambda (entry)
                                           (or (equal (ende-render--get entry "status") "recovery_needed")
                                               (ende-render--get entry "recovery_file")))
                                         (ende-render--items recovery))
                                " · recovery needed" " · recovery reported")))
                  (ende-render--join
                   (ende-render--readable (ende-render--get value "error"))
                   (when recovery (concat "Recovery:\n" (ende-render--readable recovery)))) 'error))
                ((or (ende-render--object-p result) (equal name "spawn_agent"))
                 (ende-render--response name result args))
                (t (ende-render--presentation "Response returned"
                                              (ende-render--readable result))))))
          (when (and (eq (plist-get presentation :state) 'neutral)
                     (not (equal name "spawn_agent"))
                     envelope (eq (ende-render--get value "ok") t))
            (setq presentation (plist-put presentation :state 'success)))
          (when (and (eq (plist-get presentation :state) 'success)
                     (not (eq (cadr (assoc name ende-render--tools)) 'read))
                     (eq (ende-render--get result "truncated") t))
            (setq presentation (plist-put presentation :state 'warning)))
          (when (and (stringp warning) (not (string-empty-p warning)))
            (setq presentation
                  (ende-render--presentation
                   (concat (plist-get presentation :summary) " · sandbox sync failed")
                   (ende-render--join (plist-get presentation :body) warning)
                   (if failed 'error 'warning))))
          presentation)
      (error (ende-render--presentation "Response returned"
                                         (ende-render--readable value)
                                         (cond (failed 'error)
                                               ((and envelope (eq (ende-render--get value "ok") t)
                                                     (not (equal name "spawn_agent"))) 'success)
                                               (t 'neutral)))))))

(defun ende-render-tool-response (name content &optional arguments)
  "Present tool NAME's CONTENT, optionally using request ARGUMENTS as context."
  (save-match-data
    (let ((value (ende-render--decode content))
          (args (ende-render--decode arguments))
          (name (if (symbolp name) (symbol-name name) name)))
      (condition-case nil
          (ende-render--response-envelope name value args)
        (error (ende-render--presentation "Response returned"
                                         (ende-render--readable value)))))))

(defun ende-render-reply (content)
  "Present only a complete JSON object with exactly title and text in CONTENT.
Both fields must be strings.
Return nil for prose, fenced JSON, incomplete JSON, and other shapes."
  (save-match-data
    (when (stringp content)
      (let ((value (ende-render--decode content)))
        (when (and (hash-table-p value)
                   (= (hash-table-count value) 2)
                   (stringp (gethash "title" value))
                   (stringp (gethash "text" value)))
          (ende-render--presentation (gethash "title" value) (gethash "text" value)
                                     'neutral t))))))

(provide 'ende-render)
;;; ende-render.el ends here
