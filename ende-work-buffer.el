;;; ende-work-buffer.el --- Disposable transcript inspection -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'org)
(require 'subr-x)
(require 'ende-queue)
(require 'ende-transcript)
(require 'ende-tool-store)

(declare-function ende-queue-open "ende-queue-ui" ())

(cl-defstruct (ende-work-buffer--section
               (:constructor ende-work-buffer--make-section))
  key entry parent level start body-start body-end end heading body)

(defconst ende-work-buffer--separator
  "* ────────────────────────────────────────\n"
  "A sibling heading, so even a folded final section has a visible divider.")

(defvar ende-work-buffer--viewers (make-hash-table :test #'eq)
  "Transcripts with live, disposable viewers.")
(defvar-local ende-work-buffer--job nil)
(defvar-local ende-work-buffer--transcript nil)
(defvar-local ende-work-buffer--sections nil)
(defvar-local ende-work-buffer--calls nil)
(defvar-local ende-work-buffer--last-response nil)
(defvar-local ende-work-buffer--dirty-start nil)
(defvar-local ende-work-buffer--dirty-end nil)

(define-minor-mode ende-work-buffer-mode
  "Inspect a job transcript with ordinary, read-only Org navigation."
  :lighter " Work"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "q") #'ende-work-buffer-quit)
            (define-key map (kbd "r") #'ende-work-buffer-inspect-raw)
            map)
  (when ende-work-buffer-mode
    (font-lock-mode 1)
    (font-lock-ensure))
  (read-only-mode (if ende-work-buffer-mode 1 -1)))

(defun ende-work-buffer--one-line (value)
  "Make VALUE safe for a host-owned heading."
  (replace-regexp-in-string "[[:cntrl:]]" " " (format "%s" (or value ""))))

(defun ende-work-buffer--example (text)
  "Enclose arbitrary TEXT in an Org example without allowing an escape."
  (let ((text (org-escape-code-in-string (or text ""))))
    (concat "#+begin_example\n" text
            (unless (string-suffix-p "\n" text) "\n")
            "#+end_example\n\n")))

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
    (list (= position (point-max)) (copy-marker position) section body
          (and section
               (- position (if body (ende-work-buffer--section-body-start section)
                             (ende-work-buffer--section-start section)))))))

(defun ende-work-buffer--restore-position (saved)
  "Resolve and release a SAVED reader position."
  (pcase-let ((`(,following ,marker ,section ,body ,offset) saved))
    (prog1
        (cond
         (following (point-max))
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
          (when ende-work-buffer--dirty-start
            ;; `font-lock-mode' may remain nil in batch Emacs.  Explicitly
            ;; fontify changed text, rather than merely requesting the mode.
            (font-lock-flush ende-work-buffer--dirty-start ende-work-buffer--dirty-end)
            (font-lock-ensure ende-work-buffer--dirty-start ende-work-buffer--dirty-end))
          (set-buffer-modified-p nil))
      (when ende-work-buffer--dirty-start
        (set-marker ende-work-buffer--dirty-start nil)
        (set-marker ende-work-buffer--dirty-end nil)
        (setq ende-work-buffer--dirty-start nil ende-work-buffer--dirty-end nil))
      (goto-char (ende-work-buffer--restore-position position))
      (dolist (state windows)
        (pcase-let ((`(,window ,saved-point ,saved-start) state))
          (let ((point (ende-work-buffer--restore-position saved-point))
                (start (ende-work-buffer--restore-position saved-start)))
            (when (window-live-p window)
              (set-window-start window start t)
              (set-window-point window point))))))))

(defun ende-work-buffer--splice (begin end text &optional owner)
  "Replace BEGIN through END with TEXT, retaining OWNER's ancestor boundaries."
  (let ((section owner) boundaries)
    (while section
      (when (= (ende-work-buffer--section-end section) end)
        (push (ende-work-buffer--section-end section) boundaries))
      (setq section (ende-work-buffer--section-parent section)))
    (goto-char begin)
    (delete-region begin end)
    (insert text)
    (dolist (marker boundaries) (set-marker marker (point)))
    (if ende-work-buffer--dirty-start
        (progn
          (set-marker ende-work-buffer--dirty-start
                      (min begin ende-work-buffer--dirty-start))
          (set-marker ende-work-buffer--dirty-end
                      (max (point) ende-work-buffer--dirty-end)))
      (setq ende-work-buffer--dirty-start (copy-marker begin)
            ende-work-buffer--dirty-end (copy-marker (point) t)))))

(defun ende-work-buffer--replace (begin old new owner)
  "Update OLD at BEGIN to NEW, changing only their differing span in OWNER."
  (unless (equal old new)
    (let* ((old-length (length old)) (new-length (length new))
           (prefix 0) (suffix 0))
      (while (and (< prefix (min old-length new-length))
                  (= (aref old prefix) (aref new prefix)))
        (cl-incf prefix))
      (while (and (< suffix (- (min old-length new-length) prefix))
                  (= (aref old (- old-length suffix 1))
                     (aref new (- new-length suffix 1))))
        (cl-incf suffix))
      (ende-work-buffer--splice (+ begin prefix) (+ begin (- old-length suffix))
                                (substring new prefix (- new-length suffix)) owner))))

(defun ende-work-buffer--folded-p (section)
  "Return whether SECTION itself is folded, not merely inside a hidden parent."
  (save-excursion
    (goto-char (ende-work-buffer--section-start section))
    (and (not (org-fold-folded-p (point) 'outline))
         (org-fold-folded-p (line-end-position) 'outline))))

(defun ende-work-buffer--folds (section)
  "Return folded SECTION and ancestors whose folds an insertion must extend."
  (let (folds)
    (while section
      (when (ende-work-buffer--folded-p section) (push section folds))
      (setq section (ende-work-buffer--section-parent section)))
    folds))

(defun ende-work-buffer--refold (sections)
  "Extend the original folds of SECTIONS over newly inserted content."
  (dolist (section sections)
    (save-excursion
      (goto-char (ende-work-buffer--section-start section))
      (org-fold-hide-subtree))))

(defun ende-work-buffer--put (key entry title body &optional parent before folded)
  "Insert or update KEY's ENTRY section with TITLE and BODY.
PARENT owns nested sections; BEFORE is a section to insert before.
FOLDED hides a newly inserted section, never overriding a reader's choice."
  (let* ((section (gethash key ende-work-buffer--sections))
         (parent (if section (ende-work-buffer--section-parent section) parent))
         (level (if parent (1+ (ende-work-buffer--section-level parent)) 1))
         (heading (concat (make-string level ?*) " "
                          (ende-work-buffer--one-line title) "\n\n"))
         (folds (ende-work-buffer--folds (or section parent))))
    (if section
        (let ((start (marker-position (ende-work-buffer--section-start section))))
          (ende-work-buffer--replace start (ende-work-buffer--section-heading section)
                                     heading section)
          (set-marker (ende-work-buffer--section-start section) start)
          (set-marker (ende-work-buffer--section-body-start section) (+ start (length heading)))
          (let ((body-start (marker-position (ende-work-buffer--section-body-start section))))
            (ende-work-buffer--replace body-start (ende-work-buffer--section-body section)
                                       body section)
            (set-marker (ende-work-buffer--section-body-start section) body-start)
            (set-marker (ende-work-buffer--section-body-end section) (+ body-start (length body)))))
      (let* ((start (cond (before (marker-position (ende-work-buffer--section-start before)))
                          (parent (marker-position (ende-work-buffer--section-end parent)))
                          (t (point-max))))
             (body-start (+ start (length heading)))
             (end (+ body-start (length body))))
        (ende-work-buffer--splice start start
                                  (concat heading body (unless parent ende-work-buffer--separator))
                                  parent)
        (setq section (ende-work-buffer--make-section
                       :key key :parent parent :level level
                       :start (copy-marker start t) :body-start (copy-marker body-start)
                       :body-end (copy-marker end) :end (copy-marker end)))
        (puthash key section ende-work-buffer--sections)
        (when folded (push section folds))
        (unless parent
          (put-text-property end (+ end (length ende-work-buffer--separator))
                             'ende-work-buffer-separator t))))
    (setf (ende-work-buffer--section-entry section) entry
          (ende-work-buffer--section-heading section) heading
          (ende-work-buffer--section-body section) body)
    (with-silent-modifications
      (add-text-properties
       (ende-work-buffer--section-start section) (ende-work-buffer--section-body-end section)
       (list 'ende-work-buffer-section section 'ende-work-buffer-entry entry
             'rear-nonsticky '(ende-work-buffer-section ende-work-buffer-entry))))
    (ende-work-buffer--refold folds)
    section))

(defun ende-work-buffer--remove (section)
  "Remove the provisional leaf SECTION, not its surrounding response."
  (let ((folds (ende-work-buffer--folds (ende-work-buffer--section-parent section))))
    (ende-work-buffer--splice (marker-position (ende-work-buffer--section-start section))
                              (marker-position (ende-work-buffer--section-end section)) ""
                              (ende-work-buffer--section-parent section))
    (ende-work-buffer--refold folds))
  (remhash (ende-work-buffer--section-key section) ende-work-buffer--sections)
  (dolist (marker (list (ende-work-buffer--section-start section)
                        (ende-work-buffer--section-body-start section)
                        (ende-work-buffer--section-body-end section)
                        (ende-work-buffer--section-end section)))
    (set-marker marker nil)))

(defun ende-work-buffer--response-title (entry)
  "Return the host-owned response heading for ENTRY."
  (format "Response %s (%s)%s" (ende-transcript-entry-round entry)
          (ende-transcript-entry-status entry)
          (if (plist-get (ende-transcript-entry-data entry) :final)
              " — Final reply" "")))

(defun ende-work-buffer--response (entry)
  "Render ENTRY's response boundary and its ordered semantic items."
  (when (and ende-work-buffer--last-response
             (not (eq entry ende-work-buffer--last-response)))
    (let ((previous ende-work-buffer--last-response))
      (ende-work-buffer--put (ende-transcript-entry-id previous) previous
                            (ende-work-buffer--response-title previous) "")))
  (setq ende-work-buffer--last-response entry)
  (let* ((id (ende-transcript-entry-id entry))
         (parent (ende-work-buffer--put id entry (ende-work-buffer--response-title entry) ""))
         (items (plist-get (ende-transcript-entry-data entry) :items))
         (stream-key (cons 'stream id))
         (stream (gethash stream-key ende-work-buffer--sections)))
    (if (null items)
        (ende-work-buffer--put stream-key entry "Assistant"
                              (ende-work-buffer--example (ende-transcript-entry-content entry)) parent)
      (let ((first-text (cl-find 'text items :key #'ende-transcript-entry-kind)))
        (when stream
          (if first-text
              ;; Adopt the streamed text in place.  Its heading, reader anchors,
              ;; and folding choice survive the semantic acceptance boundary.
              (progn
                (remhash stream-key ende-work-buffer--sections)
                (setf (ende-work-buffer--section-key stream) (ende-transcript-entry-id first-text))
                (puthash (ende-transcript-entry-id first-text) stream ende-work-buffer--sections))
            (ende-work-buffer--remove stream)
            (setq stream nil)))
        (dolist (item items)
          (ende-work-buffer--entry item parent (and stream (not (eq item first-text)) stream))
          (when (eq item first-text) (setq stream nil)))))))

(defun ende-work-buffer--tool-result (entry)
  "Attach exact returned ENTRY to its existing execution's tool subtree."
  (let* ((call (plist-get (ende-transcript-entry-data entry) :call))
         (parent (gethash (plist-get call :execution-id) ende-work-buffer--calls)))
    (when parent
      (ende-work-buffer--put
       (ende-transcript-entry-id entry) entry
       (format "Result (%s)" (ende-transcript-entry-status entry))
       (ende-work-buffer--example
        (ende-work-buffer--readable-json (ende-work-buffer--raw-payload entry))) parent nil t))))

(defun ende-work-buffer--entry (entry &optional parent before)
  "Render changed ENTRY, optionally beneath PARENT and BEFORE a sibling."
  (let ((id (ende-transcript-entry-id entry))
        (data (ende-transcript-entry-data entry))
        (content (ende-transcript-entry-content entry)))
    (pcase (ende-transcript-entry-kind entry)
      ('response (ende-work-buffer--response entry))
      ('text
       (ende-work-buffer--put
        id entry "Assistant"
        (ende-work-buffer--example
         (if (string-blank-p (or content ""))
             (or (plist-get (plist-get data :normalized) :content) content)
           content)) parent before))
      ('tool-call
       (let* ((call (plist-get data :call))
              (section (ende-work-buffer--put
                        id entry (concat "Tool: " (plist-get call :name)) "" parent before t)))
         (puthash (plist-get call :execution-id) section ende-work-buffer--calls)
         (ende-work-buffer--put
          (cons 'arguments id) entry "Arguments"
          (ende-work-buffer--example
           (ende-work-buffer--readable-json (ende-work-buffer--raw-payload entry))) section nil t)))
      ('tool-result (ende-work-buffer--tool-result entry))
      ('opaque
       (ende-work-buffer--put
        id entry (format "Provider item: %s / %s" (plist-get data :provider) (plist-get data :kind))
        (ende-work-buffer--example
         (ende-work-buffer--readable-json (ende-work-buffer--payload-text (plist-get data :item))))
        parent before t))
      ('summary (ende-work-buffer--put id entry "Reasoning summary" (ende-work-buffer--example content)
                                      nil nil t))
      ('progress (ende-work-buffer--put id entry (concat "Progress: " (or (plist-get data :title) ""))
                                       (ende-work-buffer--example content) nil nil t))
      ('status
       (when ende-work-buffer--last-response
         (let ((response ende-work-buffer--last-response))
           (ende-work-buffer--put (ende-transcript-entry-id response) response
                                 (ende-work-buffer--response-title response) "")))
       (ende-work-buffer--put
        id entry (format "Status: %s" (ende-transcript-entry-status entry))
        (ende-work-buffer--example (ende-work-buffer--payload-text (plist-get data :error))))))))

(defun ende-work-buffer--transcript-change (transcript entry)
  "Incrementally display changed ENTRY in TRANSCRIPT's existing viewer only."
  (when-let* ((buffer (gethash transcript ende-work-buffer--viewers))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (when (eq transcript ende-work-buffer--transcript)
        (ende-work-buffer--update
         (lambda ()
           (ende-work-buffer--put 'task nil "Task"
                                  (ende-work-buffer--example (ende-transcript-task transcript)))
           (ende-work-buffer--entry entry)))))))

(add-hook 'ende-transcript-change-functions #'ende-work-buffer--transcript-change)

(defun ende-work-buffer--detach ()
  "Detach this disposable viewer without changing job execution."
  (when (eq (gethash ende-work-buffer--transcript ende-work-buffer--viewers) (current-buffer))
    (remhash ende-work-buffer--transcript ende-work-buffer--viewers))
  (when (and ende-work-buffer--job
             (eq (ende-job-work-buffer ende-work-buffer--job) (current-buffer)))
    (setf (ende-job-work-buffer ende-work-buffer--job) nil)))

(defun ende-work-buffer-open (job)
  "Return JOB's Work viewer, creating or recreating it lazily.
The buffer is an ordinary, read-only Org buffer.  This factory does not display
it, execute work, prepare artifacts, or allocate transcript or execution IDs."
  (let* ((transcript (ende-job-ensure-transcript job))
         (existing (ende-job-work-buffer job)))
    (if (and (buffer-live-p existing)
             (eq transcript (buffer-local-value 'ende-work-buffer--transcript existing)))
        existing
      (let ((buffer (generate-new-buffer (format "*ende-work-%s*" (ende-job-id job))))
            ready)
        (unwind-protect
            (with-current-buffer buffer
              (org-mode)
              (setq ende-work-buffer--job job ende-work-buffer--transcript transcript
                    ende-work-buffer--sections (make-hash-table :test #'equal)
                    ende-work-buffer--calls (make-hash-table :test #'equal))
              (when-let* ((root (plist-get (ende-job-access-policy job) :root)))
                (setq-local default-directory (file-name-as-directory root)))
              (buffer-disable-undo)
              (ende-work-buffer-mode 1)
              (add-hook 'kill-buffer-hook #'ende-work-buffer--detach nil t)
              (add-hook 'change-major-mode-hook #'ende-work-buffer--detach nil t)
              (ende-work-buffer--update
               (lambda ()
                 (ende-work-buffer--put 'task nil "Task"
                                       (ende-work-buffer--example (ende-transcript-task transcript)))
                 (dolist (entry (ende-transcript-entries transcript))
                   (ende-work-buffer--entry entry))))
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
       (or (plist-get call :arguments)
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
      ('response (or (plist-get data :result) (ende-transcript-entry-content entry)))
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
