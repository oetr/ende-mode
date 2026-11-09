;;; ende-work-buffer.el --- Disposable transcript inspection -*- lexical-binding: t; -*-

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

;; Disposable transcript inspection for Ende.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'org)
(require 'subr-x)
(require 'ende-org)
(require 'ende-provider)
(require 'ende-queue)
(require 'ende-transcript)
(require 'ende-tool-store)

(declare-function ende-queue-open "ende-queue-ui" ())

(cl-defstruct (ende-work-buffer--section
               (:constructor ende-work-buffer--make-section))
  entry response start body-start body-end heading
  payload-start payload-end rendered-length prefix-state prefix-marker
  (revision 0) summary-source summary materialized stale)

(defvar ende-work-buffer--viewers (make-hash-table :test #'eq)
  "Transcripts with live, disposable viewers.")
(defvar-local ende-work-buffer--job nil)
(defvar-local ende-work-buffer--transcript nil)
(defvar-local ende-work-buffer--sections nil)
(defvar-local ende-work-buffer--responses nil)
(defvar-local ende-work-buffer--rounds nil)
(defvar-local ende-work-buffer--metadata nil)
(defvar-local ende-work-buffer--dependents nil)
(defvar-local ende-work-buffer--task nil)
(defconst ende-work-buffer--refresh-delay 0.1)
(defvar-local ende-work-buffer--refresh-timer nil)
(defvar-local ende-work-buffer--pending nil)
(defvar-local ende-work-buffer--pending-order nil)
(defvar-local ende-work-buffer--rendering nil)

(define-minor-mode ende-work-buffer-mode
  "Inspect a job transcript with ordinary, read-only Org navigation."
  :lighter " Work"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "q") #'ende-work-buffer-quit)
            (define-key map (kbd "r") #'ende-work-buffer-inspect-raw)
            map)
  (when ende-work-buffer-mode (font-lock-mode 1))
  (read-only-mode (if ende-work-buffer-mode 1 -1)))

(defun ende-work-buffer--one-line (value)
  "Make VALUE safe for a host-owned heading."
  (replace-regexp-in-string "[[:cntrl:]]" " " (ende-work-buffer--bounded value)))

(defun ende-work-buffer--bounded (text)
  "Bound TEXT before doing heading work."
  (let ((text (or text "")))
    (substring text 0 (min 180 (length text)))))

(defun ende-work-buffer--json-value (value &optional depth)
  "Render decoded JSON VALUE readably at DEPTH, retaining literal escapes.
Only actual newlines in decoded string values become display newlines."
  (let* ((depth (or depth 0))
         (indent (make-string (* 2 depth) ?\s))
         (next-indent (concat indent "  ")))
    (cond
     ((stringp value) (concat "\"" value "\""))
     ((hash-table-p value)
      (let (fields)
        (maphash (lambda (key item)
                   (push (concat next-indent (json-serialize key) ": "
                                 (ende-work-buffer--json-value item (1+ depth)))
                         fields)) value)
        (if fields
            (concat "{\n" (string-join (nreverse fields) ",\n") "\n" indent "}")
          "{}")))
     ((vectorp value)
      (if (= (length value) 0) "[]"
        (concat "[\n"
                (mapconcat (lambda (item)
                             (concat next-indent
                                     (ende-work-buffer--json-value item (1+ depth))))
                           value ",\n")
                "\n" indent "]")))
     ((eq value :null) "null")
     ((eq value :false) "false")
     ((eq value t) "true")
     (t (json-serialize value)))))

(defun ende-work-buffer--readable-json (text)
  "Decode JSON TEXT structurally for display, or return non-JSON unchanged."
  (condition-case nil
      (ende-work-buffer--json-value
       (json-parse-string text :object-type 'hash-table :array-type 'array
                          :null-object :null :false-object :false))
    (error text)))

(defun ende-work-buffer--payload-text (payload)
  "Return a textual representation of original semantic PAYLOAD."
  (cond ((stringp payload) payload)
        ((or (hash-table-p payload) (vectorp payload))
         (json-serialize payload :null-object nil :false-object :false))
        ((null payload) "")
        (t (pp-to-string payload))))

(defun ende-work-buffer--remember-position (position)
  "Remember POSITION relative to its section, or as an ordinary marker."
  (let* ((section (get-text-property position 'ende-work-buffer-section))
         (body (and section
                    (>= position (ende-work-buffer--section-body-start section)))))
    (list (= position (point-max)) (copy-marker position t) section body
          (and section
               (- position (if body (ende-work-buffer--section-body-start section)
                             (ende-work-buffer--section-start section))))
          (and section (if body (ende-work-buffer--section-revision section)
                         (ende-work-buffer--section-heading section))))))

(defun ende-work-buffer--restore-position (saved)
  "Resolve and release a SAVED reader position."
  (pcase-let ((`(,following ,marker ,section ,body ,offset ,revision) saved))
    (prog1
        (cond
         (following (point-max))
         ((and section
               (equal revision (if body (ende-work-buffer--section-revision section)
                                 (ende-work-buffer--section-heading section))))
          (marker-position marker))
         ((and section (marker-buffer (ende-work-buffer--section-start section)))
          (min (+ (if body (ende-work-buffer--section-body-start section)
                    (ende-work-buffer--section-start section)) offset)
               (if body (ende-work-buffer--section-body-end section)
                 (1- (ende-work-buffer--section-body-start section)))))
         (t (marker-position marker)))
      (set-marker marker nil))))

(defun ende-work-buffer--update (function)
  "Apply presentation FUNCTION without moving readers or rebuilding the buffer."
  (let ((position (ende-work-buffer--remember-position (point)))
        (windows (mapcar
                  (lambda (window)
                    (list window
                          (ende-work-buffer--remember-position (window-point window))
                          (ende-work-buffer--remember-position (window-start window))))
                  (get-buffer-window-list (current-buffer) nil t)))
        (inhibit-read-only t))
    (unwind-protect
        (save-restriction
          (widen)
          (funcall function)
          (set-buffer-modified-p nil))
      (goto-char (ende-work-buffer--restore-position position))
      (dolist (state windows)
        (pcase-let ((`(,window ,saved-point ,saved-start) state))
          (let ((point (ende-work-buffer--restore-position saved-point))
                (start (ende-work-buffer--restore-position saved-start)))
            (when (window-live-p window)
              (set-window-start window start t)
              (set-window-point window point))))))))

(defun ende-work-buffer--insert (section text)
  "Insert new TEXT with SECTION's inspection properties."
  (insert (propertize text 'ende-work-buffer-section section
                      'ende-work-buffer-entry (ende-work-buffer--section-entry section)
                      'rear-nonsticky '(ende-work-buffer-section ende-work-buffer-entry))))

(defun ende-work-buffer--append-example (section suffix)
  "Append raw SUFFIX to SECTION, inspecting only its new characters.
The prefix marker stays after indentation and before any leading commas,
so a directive completed by a later chunk can still be escaped in place."
  (let ((end (ende-work-buffer--section-payload-end section))
        (prefix (ende-work-buffer--section-prefix-marker section))
        (state (ende-work-buffer--section-prefix-state section)))
    (goto-char end)
    ;; The payload end excludes the optional separating newline.
    (when (eq (char-after) ?\n) (delete-char 1))
    (let ((begin (point)))
      (ende-work-buffer--insert section suffix)
      (goto-char begin)
      (seq-doseq (character suffix)
        (forward-char)
        (cond
         ((eq character ?\n)
          (setq state 'indent)
          (set-marker prefix (point)))
         ((and (eq state 'indent) (memq character '(?\s ?\t)))
          (set-marker prefix (point)))
         ((and (memq state '(indent commas)) (eq character ?,))
          (setq state 'commas))
         ((or (and (memq state '(indent commas)) (eq character ?*))
              (and (eq state 'hash) (eq character ?+)))
          (save-excursion
            (goto-char prefix)
            (ende-work-buffer--insert section ","))
          (setq state 'text))
         ((and (memq state '(indent commas)) (eq character ?#))
          (setq state 'hash))
         (t (setq state 'text)))))
    (goto-char end)
    (unless (eq (char-before) ?\n)
      (let ((position (point)))
        (ende-work-buffer--insert section "\n")
        (set-marker end position)))
    (setf (ende-work-buffer--section-prefix-state section) state)
    (cl-incf (ende-work-buffer--section-rendered-length section) (length suffix))))

(defun ende-work-buffer--replace-example (section text)
  "Initially render or replace SECTION's example with raw TEXT."
  (let ((folded (ende-work-buffer--folded-p section))
        (start (marker-position (ende-work-buffer--section-start section)))
        (begin (marker-position (ende-work-buffer--section-body-start section))))
    (delete-region begin (ende-work-buffer--section-body-end section))
    (goto-char begin)
    (ende-work-buffer--insert section "#+begin_example\n")
    (set-marker (ende-work-buffer--section-payload-start section) (point))
    (set-marker (ende-work-buffer--section-prefix-marker section) (point))
    (let ((payload (point)))
      (ende-work-buffer--insert section "#+end_example\n\n")
      (set-marker (ende-work-buffer--section-payload-end section) payload))
    (set-marker (ende-work-buffer--section-body-start section) begin)
    (set-marker (ende-work-buffer--section-body-end section) (point))
    (setf (ende-work-buffer--section-prefix-state section) 'indent
          (ende-work-buffer--section-rendered-length section) 0)
    (cl-incf (ende-work-buffer--section-revision section))
    (ende-work-buffer--append-example section (or text ""))
    (set-marker (ende-work-buffer--section-start section) start)
    (when folded
      (save-excursion
        (goto-char (ende-work-buffer--section-start section))
        (org-fold-hide-subtree)))))

(defun ende-work-buffer--folded-p (section)
  "Return whether SECTION's body is folded."
  (save-excursion
    (goto-char (ende-work-buffer--section-start section))
    (org-fold-folded-p (line-end-position) 'outline)))

(defun ende-work-buffer--ensure-section (key entry)
  "Return KEY's section for ENTRY, allocating only its boundaries."
  (or (gethash key ende-work-buffer--sections)
      (let ((start (point-max)))
        ;; Do not let the preceding subtree absorb the new heading's newline.
        (when (> start (point-min))
          (org-fold-region (1- start) start nil 'outline))
        (puthash key
                 (ende-work-buffer--make-section
                  :entry entry :start (copy-marker start t)
                  :body-start (copy-marker start) :body-end (copy-marker start)
                  :payload-start (make-marker) :payload-end (copy-marker nil t)
                  :prefix-marker (make-marker))
                 ende-work-buffer--sections))))

(defun ende-work-buffer--update-heading (section entry)
  "Update SECTION's bounded heading for ENTRY, leaving its body untouched."
  (let* ((title (ende-work-buffer--title entry section))
         (properties (ende-work-buffer--properties entry))
         (drawer (not (string-empty-p properties)))
         (old (ende-work-buffer--section-heading section))
         (heading (concat "* " (ende-work-buffer--one-line title) "\n"
                          (when drawer (concat ":PROPERTIES:\n" properties ":END:\n")) "\n")))
    (when-let* ((response (ende-work-buffer--response-for entry)))
      (unless (eq response (ende-work-buffer--section-response section))
        (when-let* ((entries (gethash (ende-work-buffer--section-response section)
                                     ende-work-buffer--dependents)))
          (remhash entry entries))
        (let ((entries (or (gethash response ende-work-buffer--dependents)
                           (puthash response (make-hash-table :test #'eq)
                                    ende-work-buffer--dependents))))
          (puthash entry t entries))
        (setf (ende-work-buffer--section-response section) response)))
    (unless (equal old heading)
      (let ((folded (and old (ende-work-buffer--folded-p section)))
            (empty (= (ende-work-buffer--section-body-start section)
                      (ende-work-buffer--section-body-end section)))
            (hide-drawer
             (and drawer
                  (or (not old) (not (string-match-p "\n:PROPERTIES:\n" old))
                      (save-excursion
                        (goto-char (ende-work-buffer--section-start section))
                        (forward-line)
                        (org-fold-folded-p (line-end-position) 'drawer)))))
            (start (marker-position (ende-work-buffer--section-start section))))
        (delete-region start (ende-work-buffer--section-body-start section))
        (goto-char start)
        (ende-work-buffer--insert section heading)
        (set-marker (ende-work-buffer--section-start section) start)
        (set-marker (ende-work-buffer--section-body-start section) (point))
        (when empty (set-marker (ende-work-buffer--section-body-end section) (point)))
        (setf (ende-work-buffer--section-heading section) heading)
        (when folded
          (save-excursion
            (goto-char start)
            (org-fold-region (line-end-position) (ende-work-buffer--section-body-start section)
                             t 'outline)))
        (when hide-drawer
          (save-excursion
            (goto-char start)
            (forward-line)
            (org-fold-hide-drawer-toggle t t)))))))

(defun ende-work-buffer--record-response (entry)
  "Index ENTRY's attribution and displayed metadata."
  (puthash (ende-transcript-entry-id entry) entry ende-work-buffer--responses)
  (unless (plist-get (ende-transcript-entry-data entry) :synthetic)
    (puthash (ende-transcript-entry-round entry) entry ende-work-buffer--rounds))
  (let* ((data (ende-transcript-entry-data entry))
         (profile (ende-job-profile ende-work-buffer--job))
         (metadata
          (copy-sequence
           (or (plist-get data :metadata)
               (ende-provider-response-metadata
                profile (or (plist-get data :response-metadata)
                            (list :model (plist-get profile :model)))
                (or (plist-get data :result) (plist-get data :partial-result))
                (plist-get data :usage)))))
         (id (ende-transcript-entry-id entry)))
    (when (plist-get data :synthetic)
      (dolist (field '(:input-tokens :output-tokens :cached-tokens))
        (setq metadata (plist-put metadata field nil))))
    (puthash id (ende-org--metadata-properties metadata) ende-work-buffer--metadata)))

(defun ende-work-buffer--response-for (entry)
  "Return the originating response for ENTRY."
  (or (and (eq (ende-transcript-entry-kind entry) 'response) entry)
      (gethash (ende-transcript-entry-response-id entry) ende-work-buffer--responses)
      (gethash (ende-transcript-entry-round entry) ende-work-buffer--rounds)))

(defun ende-work-buffer--properties (entry)
  "Return available provenance and tool identity properties for ENTRY."
  (let* ((response (ende-work-buffer--response-for entry))
         (kind (ende-transcript-entry-kind entry))
         (call (plist-get (ende-transcript-entry-data entry) :call)))
    (concat
     (and response (gethash (ende-transcript-entry-id response) ende-work-buffer--metadata))
     (ende-org--property "ENDE_CHAT_REQUEST_ID"
                         (plist-get (ende-job-instruction ende-work-buffer--job) :request-id))
     (when (eq kind 'response)
       (ende-org--property "ENDE_CHAT_RESPONSE_ID" (ende-transcript-entry-id entry)))
     (when (memq kind '(response tool-result))
       (ende-org--duration-property (plist-get (ende-transcript-entry-data entry) :duration-ms)))
     (when (memq kind '(tool-call tool-result))
       (concat (ende-org--property "ENDE_TOOL_EXECUTION_ID" (plist-get call :execution-id))
               (ende-org--property "ENDE_TOOL_NAME" (plist-get call :name))
               (ende-org--property "ENDE_TOOL_KIND" (if (eq kind 'tool-call) "call" "result")))))))

(defun ende-work-buffer--tool-title (entry section)
  "Return a bounded tool heading for ENTRY, caching SECTION's summary.
Incomplete arguments need no parsing; completed arguments use a raw excerpt."
  (let* ((call (plist-get (ende-transcript-entry-data entry) :call))
         (streaming (eq (ende-transcript-entry-status entry) 'streaming))
         (arguments (unless streaming
                      (if (and (eq (ende-transcript-entry-kind entry) 'tool-call)
                               (> (ende-transcript-entry-length entry) 0))
                          (ende-transcript-entry-prefix entry 180)
                        (ende-work-buffer--bounded (plist-get call :arguments))))))
    (when (and (not streaming)
               (not (equal arguments (ende-work-buffer--section-summary-source section))))
      (setf (ende-work-buffer--section-summary-source section) arguments
            (ende-work-buffer--section-summary section)
            (string-trim (ende-work-buffer--one-line arguments))))
    (ende-org--truncate-heading
     (concat (when (eq (ende-transcript-entry-kind entry) 'tool-call) "--> ")
             (ende-work-buffer--bounded (or (plist-get call :name) "tool"))
             (if streaming " (streaming)"
               (when-let* ((summary (ende-work-buffer--section-summary section))
                           ((not (string-empty-p summary))))
                 (concat ": " summary)))) 1)))

(defun ende-work-buffer--summary-title (text)
  "Return a prominent Org heading for summary TEXT."
  (let ((text (string-trim (replace-regexp-in-string "\\*\\*" "*" (ende-work-buffer--bounded text)))))
    (concat "Agent: " (if (string-prefix-p "*" text) text (concat "*" text "*")))))

(defun ende-work-buffer--title (entry section)
  "Return ENTRY's heading without formatting its body."
  (let ((data (ende-transcript-entry-data entry)))
    (pcase (ende-transcript-entry-kind entry)
      ('response (ende-org--provider-call-title data))
      ('text
       (let* ((response (ende-work-buffer--response-for entry))
              (response-data (and response (ende-transcript-entry-data response)))
              (final (and (plist-get response-data :final)
                          (eq entry (cl-find 'text (reverse (plist-get response-data :items))
                                            :key #'ende-transcript-entry-kind)))))
         (cond ((eq (ende-transcript-entry-status entry) 'interrupted) "Agent: Interrupted reply")
               (final "Agent: Final reply") (t "Agent: Reply"))))
      ((or 'tool-call 'tool-result) (ende-work-buffer--tool-title entry section))
      ('opaque "Agent: Opaque BS")
      ('thinking "Agent: Reasoning")
      ('summary (ende-work-buffer--summary-title (ende-transcript-entry-prefix entry 180)))
      ('progress (ende-work-buffer--summary-title
                  (concat (ende-work-buffer--bounded (plist-get data :title))
                          " — " (ende-transcript-entry-prefix entry 180))))
      ('status (format "Status: %s" (ende-transcript-entry-status entry))))))

(defun ende-work-buffer--display-text (entry)
  "Return ENTRY's current readable body, only when it will be rendered."
  (let ((kind (ende-transcript-entry-kind entry))
        (data (ende-transcript-entry-data entry)))
    (pcase kind
      ((or 'tool-call 'tool-result 'opaque)
       (let ((raw (ende-work-buffer--payload-text (ende-work-buffer--raw-payload entry))))
         (if (and (eq kind 'tool-call) (not (eq (ende-transcript-entry-status entry) 'complete)))
             raw
           (ende-work-buffer--readable-json raw))))
      ('text (let ((content (ende-transcript-entry-content entry)))
               (if (and (plist-get data :normalized) (string-blank-p (or content "")))
                   (or (plist-get (plist-get data :normalized) :content) content "")
                 content)))
      ('status (ende-work-buffer--payload-text (plist-get data :error)))
      (_ (or (ende-transcript-entry-content entry) "")))))

(defun ende-work-buffer--entry (entry change)
  "Render ENTRY's CHANGE in its first-observation position."
  (let* ((id (ende-transcript-entry-id entry))
         (new (not (gethash id ende-work-buffer--sections)))
         (kind (ende-transcript-entry-kind entry)))
    (unless (and (eq kind 'response) (plist-get (ende-transcript-entry-data entry) :synthetic))
      (let ((section (ende-work-buffer--ensure-section id entry)))
        (ende-work-buffer--update-heading section entry)
        (cond
         ((ende-work-buffer--detail-p section)
          (when new
            (ende-work-buffer--replace-example section "Expand to read payload."))
          (unless (and (not new) (eq change 'metadata))
            (setf (ende-work-buffer--section-stale section)
                  (ende-work-buffer--merge-change
                   (ende-work-buffer--section-stale section) (if new 'replace change)))
            (unless (or new (ende-work-buffer--folded-p section))
              (ende-work-buffer--render-body
               section entry (ende-work-buffer--section-stale section)))))
         ((not (or (eq kind 'response) (and (not new) (eq change 'metadata))))
          (ende-work-buffer--render-body section entry (if new 'replace change))))
        (when (and new (memq kind '(tool-call tool-result opaque thinking summary progress)))
          (save-excursion
            (goto-char (ende-work-buffer--section-start section))
            (org-fold-hide-subtree)))))))

(defun ende-work-buffer--render-body (section entry change)
  "Materialize ENTRY's CHANGE in SECTION, retaining just the displayed body."
  (if (and (ende-work-buffer--section-materialized section) (eq change 'append))
      (ende-work-buffer--append-example
       section
       (let ((start (ende-work-buffer--section-rendered-length section)))
         (if (and (memq (ende-transcript-entry-kind entry) '(text thinking summary progress))
                  (not (plist-get (ende-transcript-entry-data entry) :normalized)))
             (ende-transcript-entry-substring entry start)
           (substring (ende-work-buffer--display-text entry) start))))
    (ende-work-buffer--replace-example section (ende-work-buffer--display-text entry)))
  (setf (ende-work-buffer--section-materialized section) t
        (ende-work-buffer--section-stale section) nil))

(defun ende-work-buffer--detail-p (section)
  "Return whether SECTION owns an expensive, deferrable payload."
  (when-let* ((entry (ende-work-buffer--section-entry section)))
    (memq (ende-transcript-entry-kind entry) '(tool-call tool-result opaque))))

(defun ende-work-buffer--queue-exposed (section)
  "Queue SECTION's deferred body only if it is exposed and stale."
  (when (and section (ende-work-buffer--detail-p section)
             (not (ende-work-buffer--folded-p section))
             (ende-work-buffer--section-stale section))
    (ende-work-buffer--queue-change (ende-work-buffer--section-entry section)
                                   (ende-work-buffer--section-stale section))))

(defun ende-work-buffer--visibility-changed (state)
  "Enqueue details exposed by ordinary Org cycling to STATE."
  (pcase state
    ((or 'children 'subtree)
     (save-excursion
       (org-back-to-heading t)
       (ende-work-buffer--queue-exposed (get-text-property (point) 'ende-work-buffer-section))))
    ('all
     (maphash (lambda (_ section) (ende-work-buffer--queue-exposed section))
              ende-work-buffer--sections)))
  (ende-work-buffer--schedule-refresh))

(defun ende-work-buffer--merge-change (old new)
  "Merge pending OLD and NEW change kinds."
  (cond ((or (eq old 'replace) (eq new 'replace)) 'replace)
        ((or (eq old 'append) (eq new 'append)) 'append)
        (t 'metadata)))

(defun ende-work-buffer--queue-change (entry change)
  "Retain one pending CHANGE for canonical ENTRY in observation order."
  (let ((old (gethash entry ende-work-buffer--pending)))
    (unless old (push entry ende-work-buffer--pending-order))
    (puthash entry (ende-work-buffer--merge-change old change) ende-work-buffer--pending)))

(defun ende-work-buffer--pending-p ()
  "Return whether this viewer has runnable presentation work."
  (or ende-work-buffer--pending-order
      (not (eq ende-work-buffer--task (ende-transcript-task ende-work-buffer--transcript)))))

(defun ende-work-buffer--viewer-p ()
  "Return whether this buffer is still its job's registered viewer."
  (and ende-work-buffer-mode ende-work-buffer--job
       (eq (ende-job-work-buffer ende-work-buffer--job) (current-buffer))
       (eq (gethash ende-work-buffer--transcript ende-work-buffer--viewers) (current-buffer))))

(defun ende-work-buffer--schedule-refresh ()
  "Schedule one visible refresh from the first pending change."
  (when (and (ende-work-buffer--viewer-p)
             (ende-work-buffer--pending-p)
             (get-buffer-window (current-buffer) 'visible)
             (not ende-work-buffer--rendering)
             (not ende-work-buffer--refresh-timer))
    (setq ende-work-buffer--refresh-timer
          (run-at-time ende-work-buffer--refresh-delay nil
                       #'ende-work-buffer--refresh (current-buffer)))))

(defun ende-work-buffer--render-pending ()
  "Consume a snapshot, leaving notifications during rendering pending."
  (let ((pending ende-work-buffer--pending)
        (order (nreverse ende-work-buffer--pending-order))
        responses)
    (setq ende-work-buffer--pending (make-hash-table :test #'eq)
          ende-work-buffer--pending-order nil)
    ;; Index response metadata before any dependent headings are constructed.
    (dolist (entry order)
      (when (eq (ende-transcript-entry-kind entry) 'response)
        (ende-work-buffer--record-response entry)
        (push entry responses)))
    (ende-work-buffer--update
     (lambda ()
       (unless (and (gethash 'task ende-work-buffer--sections)
                    (eq ende-work-buffer--task (ende-transcript-task ende-work-buffer--transcript)))
         (setq ende-work-buffer--task (ende-transcript-task ende-work-buffer--transcript))
         (ende-work-buffer--replace-example
          (ende-work-buffer--ensure-section 'task nil) ende-work-buffer--task))
       (dolist (entry order)
         (ende-work-buffer--entry entry (gethash entry pending)))
       (dolist (response responses)
         (when-let* ((entries (gethash response ende-work-buffer--dependents)))
           (maphash (lambda (entry _)
                      (unless (or (eq entry response) (gethash entry pending))
                        (ende-work-buffer--entry entry 'metadata)))
                    entries)))))))

(defun ende-work-buffer--refresh (buffer)
  "Refresh BUFFER if it is still a displayed viewer; dispose on failure."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq ende-work-buffer--refresh-timer nil)
      (when (and (ende-work-buffer--viewer-p)
                 (get-buffer-window buffer 'visible)
                 (not ende-work-buffer--rendering))
        (condition-case failure
            (progn
              (let ((ende-work-buffer--rendering t))
                (ende-work-buffer--render-pending))
              (ende-work-buffer--schedule-refresh))
          (error
           (kill-buffer buffer)
           (display-warning 'ende (format "Work display: %s" (error-message-string failure))))
          (quit
           (kill-buffer buffer)
           (signal (car failure) (cdr failure))))))))

(defun ende-work-buffer--window-change (window)
  "Catch up when WINDOW displays a Work buffer."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (when ende-work-buffer-mode (ende-work-buffer--schedule-refresh)))))

(defun ende-work-buffer--transcript-change (transcript entry change)
  "Invalidate ENTRY's CHANGE without rendering TRANSCRIPT's viewer."
  (when-let* ((buffer (gethash transcript ende-work-buffer--viewers))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (when (and ende-work-buffer-mode (eq transcript ende-work-buffer--transcript))
        (ende-work-buffer--queue-change entry change)
        (ende-work-buffer--schedule-refresh)))))

(add-hook 'ende-transcript-change-functions #'ende-work-buffer--transcript-change)

(defun ende-work-buffer--detach ()
  "Detach this disposable viewer without changing job execution."
  (when (eq (gethash ende-work-buffer--transcript ende-work-buffer--viewers) (current-buffer))
    (remhash ende-work-buffer--transcript ende-work-buffer--viewers))
  (when (and ende-work-buffer--job
             (eq (ende-job-work-buffer ende-work-buffer--job) (current-buffer)))
    (setf (ende-job-work-buffer ende-work-buffer--job) nil))
  (when (timerp ende-work-buffer--refresh-timer)
    (cancel-timer ende-work-buffer--refresh-timer))
  (when ende-work-buffer--sections
    (maphash (lambda (_ section)
               (dolist (marker (list (ende-work-buffer--section-start section)
                                     (ende-work-buffer--section-body-start section)
                                     (ende-work-buffer--section-body-end section)
                                     (ende-work-buffer--section-payload-start section)
                                     (ende-work-buffer--section-payload-end section)
                                     (ende-work-buffer--section-prefix-marker section)))
                 (set-marker marker nil)))
             ende-work-buffer--sections))
  (with-silent-modifications
    (remove-text-properties (point-min) (point-max)
                            '(ende-work-buffer-section nil ende-work-buffer-entry nil)))
  (setq ende-work-buffer-mode nil ende-work-buffer--refresh-timer nil
        ende-work-buffer--rendering nil
        ende-work-buffer--pending nil ende-work-buffer--pending-order nil
        ende-work-buffer--job nil ende-work-buffer--transcript nil
        ende-work-buffer--sections nil ende-work-buffer--responses nil
        ende-work-buffer--rounds nil ende-work-buffer--metadata nil
        ende-work-buffer--dependents nil ende-work-buffer--task nil))

(defun ende-work-buffer-shutdown ()
  "Discard all disposable viewers without touching retained transcripts."
  (let (buffers)
    (maphash (lambda (_ buffer) (push buffer buffers)) ende-work-buffer--viewers)
    (dolist (buffer buffers)
      (when (buffer-live-p buffer) (kill-buffer buffer)))
    (clrhash ende-work-buffer--viewers)))

(defun ende-work-buffer-open (job)
  "Return JOB's Work viewer, creating or recreating it lazily.
The buffer is an ordinary, read-only Org buffer.  This factory does not display
it, execute work, prepare artifacts, or allocate transcript or execution IDs."
  (let* ((transcript (ende-job-ensure-transcript job))
         (existing (ende-job-work-buffer job)))
    (if (and (buffer-live-p existing)
             (eq transcript (buffer-local-value 'ende-work-buffer--transcript existing)))
        (progn
          (with-current-buffer existing (ende-work-buffer--schedule-refresh))
          existing)
      (let ((buffer (generate-new-buffer (format "*ende-work-%s*" (ende-job-id job))))
            ready)
        (unwind-protect
            (with-current-buffer buffer
              (org-mode)
              ;; Cache invalidation otherwise rescans long streamed lines.
              (setq-local org-element-use-cache nil)
              (setq ende-work-buffer--job job ende-work-buffer--transcript transcript
                    ende-work-buffer--sections (make-hash-table :test #'eql)
                    ende-work-buffer--responses (make-hash-table :test #'eql)
                    ende-work-buffer--rounds (make-hash-table :test #'eql)
                    ende-work-buffer--metadata (make-hash-table :test #'eql)
                    ende-work-buffer--dependents (make-hash-table :test #'eq)
                    ende-work-buffer--pending (make-hash-table :test #'eq)
                    ende-work-buffer--task 'unrendered)
              (when-let* ((root (plist-get (ende-job-access-policy job) :root)))
                (setq-local default-directory (file-name-as-directory root)))
              (buffer-disable-undo)
              (ende-work-buffer-mode 1)
              (add-hook 'kill-buffer-hook #'ende-work-buffer--detach nil t)
              (add-hook 'change-major-mode-hook #'ende-work-buffer--detach nil t)
              (add-hook 'window-buffer-change-functions #'ende-work-buffer--window-change nil t)
              (add-hook 'org-cycle-hook #'ende-work-buffer--visibility-changed nil t)
              (let ((inhibit-read-only t))
                (insert "#+COLUMNS: " ende-org-columns-format "\n\n")
                (set-buffer-modified-p nil))
              (dolist (entry (ende-transcript-entries transcript))
                (ende-work-buffer--queue-change entry 'replace))
              (setf (ende-job-work-buffer job) buffer)
              (puthash transcript buffer ende-work-buffer--viewers)
              (setq ready t)
              buffer)
          (unless ready (kill-buffer buffer)))))))

(defun ende-work-buffer--raw-payload (entry)
  "Return ENTRY's original payload, using only explicit store metadata if needed."
  (let* ((data (ende-transcript-entry-data entry))
         (call (plist-get data :call))
         (file (plist-get call :store-file))
         (execution-id (plist-get call :execution-id)))
    (pcase (ende-transcript-entry-kind entry)
      ('tool-call
       (or (ende-transcript-entry-arguments entry)
           (and file execution-id
                (plist-get (ende-tool-store-load-execution execution-id file) :arguments))))
      ('tool-result
       (or (ende-transcript-entry-content entry)
           (and file execution-id
                (plist-get (ende-tool-store-load-execution execution-id file) :content))))
      ('opaque
       (or (plist-get data :item)
           (and (plist-get data :store-file) (plist-get data :item-id)
                (ende-tool-store-load-opaque (plist-get data :item-id)
                                            (plist-get data :provider) (plist-get data :kind)
                                            (plist-get data :store-file)))))
      ('status (plist-get data :error))
      (_ (ende-transcript-entry-content entry)))))

(defun ende-work-buffer-inspect-raw ()
  "Inspect the original payload of the transcript entry at point.
Unlike the readable viewer, string payloads are inserted verbatim.  Storage
fallbacks use the entry's explicit filename, never the viewer's local defaults."
  (interactive)
  (let ((entry (get-text-property (point) 'ende-work-buffer-entry)))
    (unless entry (user-error "No transcript entry at point"))
    (let* ((data (ende-transcript-entry-data entry))
           (file (or (plist-get (plist-get data :call) :store-file) (plist-get data :store-file)))
           (payload (ende-work-buffer--payload-text (ende-work-buffer--raw-payload entry)))
           (buffer (get-buffer-create "*ende work raw*")))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert payload)
          (special-mode)
          (setq-local header-line-format
                      (ende-work-buffer--one-line
                       (format "Original %s payload%s" (ende-transcript-entry-kind entry)
                               (if file (format " — Store: %s" file) ""))))
          (goto-char (point-min))))
      (pop-to-buffer buffer)
      buffer)))

(defun ende-work-buffer-quit ()
  "Quit the current inspection window and return to the queue dashboard."
  (interactive)
  (quit-window)
  (require 'ende-queue-ui)
  (ende-queue-open))

(provide 'ende-work-buffer)
;;; ende-work-buffer.el ends here
