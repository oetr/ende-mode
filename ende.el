;;; ende.el --- Local-LLM text assistance for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter Samarin
;;
;; Author: Peter Samarin <peter.samarin@gmail.com>
;; Maintainer: Peter Samarin <peter.samarin@gmail.com>
;; Version: 0.3
;; Package-Requires: ((emacs "30.1") (websocket "1.12"))
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

;; ende is a minor mode that queues requests to an OpenAI-compatible LLM.
;;
;;   C-c e   - ende-selector-open
;;
;; Requirements: Emacs 30.1 or later, the `websocket' package, and the external
;; curl executable on the local `exec-path', with HTTPS support for remote
;; providers.  Install curl using your operating system's package manager;
;; `Package-Requires' describes Emacs Lisp dependencies only.
;;
;; TRAMP source buffers are supported: constructed request bodies are sent to
;; local provider processes.  Generation, provider discovery, Codex model
;; probing, and OAuth refresh always run curl locally, never on the source
;; buffer's remote host.  Provider processes and their header/config files use
;; `temporary-file-directory', which must be an existing absolute local
;; directory; remote or relative values are rejected.

;;; Code:

(require 'cl-lib)
(require 'eldoc)
(require 'org-element)
(require 'ende-core)
(require 'ende-artifact)
(require 'ende-tool-store)
(require 'ende-openrouter-batch)
(require 'ende-operation)
(require 'ende-provider)
(require 'ende-review)
(require 'ende-project)
(require 'ende-url)
(require 'ende-target)
(require 'ende-ui)
(require 'ende-llm)
(require 'ende-work-buffer)
(require 'ende-queue)
(require 'ende-patch)
(require 'ende-web-fetch)
(require 'ende-tools)
(require 'ende-operations)
(require 'ende-chat)
(require 'ende-commands)
(require 'ende-rubber-duck)
(require 'ende-history)
(require 'ende-speech)
(require 'ende-selector)
(require 'ende-queue-ui)

;;;; Keymap and Minor Mode

(defun ende--make-mode-map ()
  "Build the keymap for `ende-mode'."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c e") #'ende-selector-open)
    map))

(defvar ende-mode-map (ende--make-mode-map) "Keymap for `ende-mode'.")

(defvar-local ende--mode-line-string " ende[0]"
  "Mode-line lighter showing jobs and pending reviews for the current buffer.")

(defun ende--job-targets-buffer-p (job buffer)
  "Return non-nil when active or queued JOB originates in BUFFER."
  (and (memq (ende-job-state job) '(queued running))
       (eq (ende-job-origin-buffer job) buffer)))

(defun ende--buffer-job-count (buffer)
  "Return the number of active or queued jobs targeting BUFFER."
  (cl-count-if
   (lambda (job) (ende--job-targets-buffer-p job buffer))
   (append (ende-running-jobs) (ende-physical-waiting-jobs))))

(defun ende--buffer-review-count (buffer)
  "Return the number of pending reviews in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (cl-count-if #'ende-review-overlay-p
                     (overlays-in (point-min) (point-max)))))))

(defun ende--update-mode-line (&optional buffer)
  "Update the ende lighter for BUFFER, or the current buffer."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((jobs (ende--buffer-job-count buffer))
              (reviews (ende--buffer-review-count buffer)))
          (setq ende--mode-line-string
                (if (> reviews 0)
                    (format " ende[%d|%d]" jobs reviews)
                  (format " ende[%d]" jobs))))
        (force-mode-line-update)))))

(defun ende--mode-line-job-event (job event)
  "Refresh the origin mode-line count when EVENT can change live membership."
  (when (memq (plist-get event :type)
              '(:queued :started :resumed :suspended :queued-for-resumption
                :settled))
    (ende--update-mode-line (ende-job-origin-buffer job))))

(defun ende--mode-line-review-event (buffer)
  "Refresh the mode-line count for reviews in BUFFER."
  (ende--update-mode-line buffer))

(add-hook 'ende-queue-abort-functions #'ende-provider-abort-discovery)
(add-hook 'ende-queue-abort-functions #'ende-openrouter-batch-abort)
(add-hook 'ende-queue-abort-functions #'ende-ui-clear-all)
(add-hook 'ende-job-event-functions #'ende-ui-job-event)
(add-hook 'ende-job-event-functions #'ende--mode-line-job-event)
(add-hook 'ende-review-change-functions #'ende--mode-line-review-event)

(defconst ende--config-vocabulary
  '(("root" top-level "root PATH — Set the configuration root directory.")
    ("podman-image" top-level
     "podman-image IMAGE — Select a digest-pinned Podman image.")
    ("group" top-level "group NAME TOOL... — Define a named tool group.")
    ("resource" top-level "resource NAME ... — Define a named file or URL resource.")
    ("tools" top-level "tools NAME... — Activate one or more tools or tool groups.")
    ("clear-tools" top-level
     "clear-tools [TOOL...] — Clear all tools, or selectively disable named tools.")
    ("restrict-host-commands" top-level
     "restrict-host-commands RULE... | :unrestricted — Configure host-command policy independently of tool activation. Descendants inherit it; each declaration replaces the inherited policy. Sole :unrestricted explicitly removes restrictions. Strings allow any arguments; :fixed-args allows only the specified argv; :prefix-args allows that argv prefix plus any remaining arguments. Changes may prevent reuse of cached prompt prefixes.")
    ("set-tool-resources" top-level
     "set-tool-resources TOOL... :resources RESOURCE... — Replace resource assignments for the selected tools.")
    ("add-tool-resources" top-level
     "add-tool-resources TOOL... :resources RESOURCE... — Add resource assignments for the selected tools.")
    ("clear-tool-resources" top-level
     "clear-tool-resources — Clear resource assignments.")
    ("include" rule "include PATTERN — Allow paths or URLs matching PATTERN.")
    ("exclude" rule "exclude PATTERN — Exclude paths or URLs matching PATTERN.")
    (":type" option ":type TYPE — Select resource type: file or url.")
    (":root" option ":root PATH — Set the root directory for a file resource.")
    (":allow-private-addresses" option
     ":allow-private-addresses t|nil — Allow private addresses for a URL resource.")
    (":resources" option
     ":resources RESOURCE... — Separate tools from resources in an assignment.")
    (":fixed-args" option
     ":fixed-args COMMAND ARG... — Allow only this exact host-command argv.")
    (":prefix-args" option
     ":prefix-args COMMAND ARG... — Allow host-command argv beginning with this prefix.")
    ("file" value "file — A local file or directory resource.")
    ("url" value "url — A URL resource.")
    ("t" value "t — Enable the associated boolean option.")
    ("nil" value "nil — Disable the associated boolean option.")
    ("#+begin_ende" delimiter "#+begin_ende — Start an Ende configuration block.")
    ("#+end_ende" delimiter "#+end_ende — End an Ende configuration block."))
  "Presentation metadata for Ende configuration tokens and their help text.")

(defun ende--config-block-at-p (position)
  "Return non-nil if POSITION is inside an Org special block of type ende."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (condition-case nil
          (let* ((element (org-element-at-point position))
                 (block (if (eq (org-element-type element) 'special-block)
                            element
                          (org-element-lineage
                           element '(special-block) t))))
            (and block
                 (string-equal
                  (downcase (or (org-element-property :type block) ""))
                  "ende")
                 (<= (org-element-property :begin block) position)
                 (< position (org-element-property :end block))))
        (error nil)))))

(defun ende--font-lock-match-delimiter (limit)
  "Match Ende block delimiters before LIMIT."
  (let ((case-fold-search t)
        found)
    (while (and (not found)
                (re-search-forward
                 "^[ \t]*\\(#\\+\\(?:begin\\|end\\)_ende\\)\\b"
                 limit t))
      (let ((start (match-beginning 1))
            (end (match-end 1)))
        (when (ende--config-block-at-p start)
          (set-match-data (list start end start end))
          (setq found t))
        ;; Org parsing can move point; keep searching after rejected text.
        (goto-char end)))
    found))

(defun ende--config-entry (token)
  "Return vocabulary metadata for TOKEN, case-insensitively."
  (assoc-string token ende--config-vocabulary t))

(defun ende--token-character-p (character)
  "Return non-nil if CHARACTER can be part of an Ende token."
  (and character
       (or (= character ?:)
           (string-match-p "[[:alnum:]_-]" (char-to-string character)))))

(defun ende--comment-before-p (position)
  "Return non-nil if a semicolon precedes POSITION on the same line."
  (save-excursion
    (goto-char position)
    (let (found)
      (while (and (not found)
                  (re-search-backward ";" (line-beginning-position) t))
        (unless (nth 3 (syntax-ppss (point)))
          (setq found t)))
      found)))

(defun ende--config-token-context-p (start end)
  (and (not (ende--token-character-p (char-before start)))
       (not (ende--token-character-p (char-after end)))
       (not (nth 3 (syntax-ppss start)))
       (not (ende--comment-before-p start))
       (ende--config-block-at-p start)))

(defun ende--font-lock-match-class (class limit)
  "Match vocabulary entries of CLASS before LIMIT."
  (let (found)
    (while (and (not found)
                (re-search-forward
                 ":[[:alnum:]_-]+\\|[[:alnum:]_-]+" limit t))
      (let* ((start (match-beginning 0))
             (end (match-end 0))
             (token (match-string-no-properties 0))
             (entry (ende--config-entry token)))
        (when (and entry
                   (eq (nth 1 entry) class)
                   (ende--config-token-context-p start end))
          (set-match-data (list start end))
          (setq found t))
        ;; Some syntax/parser helpers move point, so advance only after all
        ;; candidate checks have finished.
        (goto-char end)))
    found))

(defun ende--font-lock-match-top-level (limit)
  "Match top-level Ende forms before LIMIT."
  (ende--font-lock-match-class 'top-level limit))

(defun ende--font-lock-match-rule (limit)
  "Match Ende resource rules before LIMIT."
  (ende--font-lock-match-class 'rule limit))

(defun ende--font-lock-match-option (limit)
  "Match Ende options before LIMIT."
  (ende--font-lock-match-class 'option limit))

(defun ende--font-lock-match-value (limit)
  "Match fixed Ende values before LIMIT."
  (ende--font-lock-match-class 'value limit))

(defun ende--font-lock-match-comment (limit)
  "Match Ende comment delimiters and text before LIMIT."
  (let (found)
    (while (and (not found)
                (re-search-forward "\\(;\\)\\(.*\\)$" limit t))
      (let ((matches (match-data))
            (start (match-beginning 1)))
        (when (and (not (nth 3 (syntax-ppss start)))
                   (ende--config-block-at-p start))
          (set-match-data matches)
          (setq found t))
        (unless found
          (goto-char (1+ start)))))
    found))

(defconst ende--font-lock-keywords
  '((ende--font-lock-match-delimiter
     (1 font-lock-preprocessor-face t))
    (ende--font-lock-match-top-level (0 font-lock-keyword-face t))
    (ende--font-lock-match-rule (0 font-lock-builtin-face t))
    (ende--font-lock-match-option (0 font-lock-type-face t))
    (ende--font-lock-match-value (0 font-lock-constant-face t))
    (ende--font-lock-match-comment
     (1 font-lock-comment-delimiter-face t)
     (2 font-lock-comment-face t)))
  "Font-lock rules installed by `ende-mode'.")

(defun ende--config-entry-at-point ()
  "Return vocabulary metadata for the Ende token at point, or nil."
  (let ((position (point))
        entry)
    (save-excursion
      (beginning-of-line)
      (let ((case-fold-search t))
        (when (looking-at
               "^[ \t]*\\(#\\+\\(?:begin\\|end\\)_ende\\)\\b")
          (let ((start (match-beginning 1))
                (end (match-end 1))
                (token (match-string-no-properties 1)))
            (when (and (<= start position) (< position end)
                       (ende--config-block-at-p start))
              (setq entry (ende--config-entry token))))))
      (unless entry
        (let ((line-end (line-end-position)))
          (while (and (not entry)
                      (re-search-forward
                       ":[[:alnum:]_-]+\\|[[:alnum:]_-]+" line-end t))
            (let ((start (match-beginning 0))
                  (end (match-end 0))
                  (token (match-string-no-properties 0)))
              (when (and (<= start position) (< position end)
                         (ende--config-token-context-p start end))
                (setq entry (ende--config-entry token)))
              (goto-char end))))))
    entry))

(defun ende--eldoc-documentation (callback)
  "Report Ende configuration help at point through CALLBACK."
  (let ((entry (ende--config-entry-at-point)))
    (when entry
      (funcall callback (nth 2 entry)))))

(defun ende--refresh-fontification ()
  "Refresh the current buffer's fontification."
  (when (fboundp 'font-lock-flush)
    (font-lock-flush (point-min) (point-max)))
  (when (fboundp 'font-lock-ensure)
    (font-lock-ensure (point-min) (point-max))))

(defun ende--configure-presentation (enabled)
  "Install or remove Ende presentation support according to ENABLED."
  (if enabled
      (progn
        (font-lock-add-keywords nil ende--font-lock-keywords 'append)
        (add-hook 'eldoc-documentation-functions
                  #'ende--eldoc-documentation nil t))
    (font-lock-remove-keywords nil ende--font-lock-keywords)
    (remove-hook 'eldoc-documentation-functions
                 #'ende--eldoc-documentation t))
  (ende--refresh-fontification))

;;;###autoload
(define-minor-mode ende-mode
  "Minor mode for queueing LLM requests."
  :lighter ende--mode-line-string
  :keymap ende-mode-map
  (ende--configure-presentation ende-mode)
  (ende-artifact-configure ende-mode)
  (ende--configure-org-template)
  (if ende-mode
      (progn
        (ende-tool-store--initialize-buffer)
        (ende-model-selection-initialize-buffer))
    (remove-hook 'org-mode-hook #'ende-tool-store--initialize-buffer t)
    (kill-local-variable 'ende-tool-store--local-file))
  (ende--update-mode-line))

(defun ende--expand-config-template ()
  "Expand `<ende' at point into an Org configuration block."
  (when (and ende-mode
             (derived-mode-p 'org-mode)
             (eolp)
             (looking-back "^[ \t]*<ende" (line-beginning-position)))
    (let* ((start (- (point) (length "<ende")))
           (indent (buffer-substring-no-properties
                    (line-beginning-position) start)))
      (delete-region start (point))
      (insert "#+begin_ende\n" indent "\n" indent "#+end_ende")
      (forward-line -1)
      (end-of-line)
      t)))

(defun ende--configure-org-template ()
  "Configure Org template expansion for the current buffer."
  (if (and ende-mode (derived-mode-p 'org-mode))
      (add-hook 'org-tab-first-hook
                #'ende--expand-config-template nil t)
    (remove-hook 'org-tab-first-hook
                 #'ende--expand-config-template t)))

;;;###autoload
(defun ende-reload--files (directory)
  "Return Ende source files under DIRECTORY in require dependency order."
  (let* ((sources
          (cl-remove-if
           (lambda (file)
             (or (string-match-p "-tests?\\.el\\'" file)
                 (equal file "ende-chat-benchmark.el")))
           (directory-files directory nil "^ende-.*[.]el$")))
         (features
          (mapcar (lambda (file) (file-name-sans-extension file)) sources))
         (dependencies (make-hash-table :test #'equal))
         ordered
         visiting
         visited)
    (dolist (file sources)
      (with-temp-buffer
        (insert-file-contents (expand-file-name file directory))
        (let (required)
          (while (re-search-forward "^(require 'ende-[[:alnum:]-]+" nil t)
            (let ((feature (substring (match-string-no-properties 0) 10)))
              (when (member feature features)
                (push feature required))))
          (puthash (file-name-sans-extension file) required dependencies))))
    (cl-labels
        ((visit (feature)
           (unless (or (member feature visited) (member feature visiting))
             (push feature visiting)
             (dolist (dependency (gethash feature dependencies))
               (visit dependency))
             (setq visiting (delete feature visiting)
                   visited (cons feature visited))
             (push feature ordered))))
      (dolist (feature features)
        (unless (equal feature "ende")
          (visit feature)))
    (append (mapcar (lambda (feature) (concat feature ".el"))
                    (nreverse ordered))
            '("ende.el")))))

(defun ende-reload ()
  "Abort active jobs and reload every Ende source file in dependency order."
  (interactive)
  (ende-abort-queue)
  (when-let* ((selector (get-buffer ende-selector-buffer-name)))
    (with-current-buffer selector (ende-selector-close)))
  (let ((dir (or (file-name-directory (locate-library "ende.el"))
                 (user-error "ende.el not found on load-path"))))
    (dolist (file (ende-reload--files dir))
      (load (expand-file-name file dir) nil t t t))
    (setq ende-mode-map (ende--make-mode-map))
    (ende-artifact-clear-cache)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when ende-mode
          (ende-tool-store--initialize-buffer)
          (ende--configure-org-template)
          (ende-artifact-configure t))))
    (message "ende: reloaded from %s" dir)))

(provide 'ende)

;;; ende.el ends here
