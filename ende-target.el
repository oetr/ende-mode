;;; ende-target.el --- Buffer targets for ende jobs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools

;;; Commentary:

;; Stable buffer targets, source validation, restrictions, conflict checks,
;; mutation monitoring, and queued insertion-anchor movement for ende.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ende-core)
(require 'ende-review)

(defcustom ende-target-max-context-chars nil
  "Maximum number of characters sent to the LLM as target context.
  Tokens are estimated as chars/4.  When nil, send the complete target."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Maximum characters"))
  :group 'ende)

(defcustom ende-target-window-chars nil
  "Characters kept on each side of the cursor when target context is elided."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Maximum characters"))
  :group 'ende)

(cl-defstruct (ende-target (:constructor ende-target--create))
  "A stable, restricted buffer target for a queued operation."
  buffer start-marker end-marker cursor-marker placement
  source-snapshot source-digest restriction-beg restriction-end selected offset
  review-policy applying invalidated
  kill-buffer-hook before-change-hook after-change-hook)

;;;; Creation and positions

(defun ende-target--make-marker (buffer position insertion-type)
  "Create a marker in BUFFER at POSITION with INSERTION-TYPE."
  (let ((marker (make-marker)))
    (set-marker marker position buffer)
    (set-marker-insertion-type marker insertion-type)
    marker))

(defun ende-target--marker-position (marker)
  "Return MARKER's position, or nil when it has no live buffer."
  (and (markerp marker) (marker-position marker)))

(defun ende-target--anchor-position (anchor)
  "Return ANCHOR as a live marker position or numeric position."
  (if (markerp anchor) (marker-position anchor) anchor))

(defun ende-target-region-digest (start end)
  "Return a stable digest of the current buffer between START and END."
  (secure-hash 'sha256 (current-buffer) start end))

(cl-defun ende-target-create
    (&key buffer start end cursor selected placement offset source-snapshot
          (review-policy 'block))
  "Create a target in BUFFER from START to END anchored at CURSOR.
SELECTED means START..END is immutable source.  PLACEMENT and OFFSET describe
an insertion relative to that source.  SOURCE-SNAPSHOT is an optional textual
fallback for callers that cannot capture a digest.  REVIEW-POLICY controls
whether pending reviews are checked for this target: `block' checks them,
`resolve' leaves conflict resolution to the caller, and `ignore' permits
read-only access to reviewed text."
  (unless (buffer-live-p buffer)
    (error "target buffer no longer exists"))
  (setq review-policy (or review-policy 'block))
  (unless (memq review-policy '(block resolve ignore))
    (error "unknown target review policy: %s" review-policy))
  (with-current-buffer buffer
    (when (eq review-policy 'block)
      (ende-review-assert-available buffer start end))
    (ende-target--create
     :buffer buffer
     :start-marker (ende-target--make-marker buffer start (and selected t))
     :end-marker (ende-target--make-marker buffer end nil)
     :cursor-marker (ende-target--make-marker buffer cursor nil)
     :placement placement
     :source-snapshot source-snapshot
     :source-digest (and selected
                         (null source-snapshot)
                         (ende-target-region-digest start end))
     :restriction-beg (ende-target--make-marker buffer (point-min) nil)
     :restriction-end (ende-target--make-marker buffer (point-max) t)
     :selected selected
     :offset offset
     :review-policy review-policy)))

(defun ende-target-start (target)
  "Return TARGET's current start position, or nil."
  (ende-target--marker-position (ende-target-start-marker target)))

(defun ende-target-end (target)
  "Return TARGET's current end position, or nil."
  (ende-target--marker-position (ende-target-end-marker target)))

(defun ende-target-cursor (target)
  "Return TARGET's current cursor position, or nil."
  (ende-target--marker-position (ende-target-cursor-marker target)))

(defun ende-target-live-p (target)
  "Return non-nil when TARGET's buffer is live."
  (and (ende-target-p target)
       (buffer-live-p (ende-target-buffer target))))

(defconst ende-target--source-metadata-field-max-length 256
  "Maximum length of a source metadata field sent to an LLM.")

(defun ende-target--safe-buffer-name (name)
  "Return NAME without an absolute or remote path component."
  (if (and (stringp name)
           (or (file-name-absolute-p name)
               (file-remote-p name)))
      (file-name-nondirectory name)
    name))

(defun ende-target-source-metadata (target)
  "Return bounded source metadata for TARGET's buffer, or nil.

Visiting files contribute only their basename.  Other buffers contribute their
buffer name unless it looks like an absolute or remote path."
  (let ((buffer (and (ende-target-p target)
                     (ende-target-buffer target))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let* ((file-name (and (stringp buffer-file-name)
                               (not (string-empty-p buffer-file-name))
                               buffer-file-name))
               (raw-name (if file-name
                             (file-name-nondirectory file-name)
                           (ende-target--safe-buffer-name
                            (buffer-name))))
               (mode (if (symbolp major-mode)
                         (symbol-name major-mode)
                       (format "%s" major-mode))))
          (list :name (or (ende-bounded-single-line
                           raw-name
                           ende-target--source-metadata-field-max-length
                           t)
                          "unnamed-buffer")
                :type (if file-name "file" "buffer")
                :major-mode (or (ende-bounded-single-line
                                 mode
                                 ende-target--source-metadata-field-max-length
                                 t)
                                "unknown")))))))

;;;; Restrictions, source, context, and conflicts

(defun ende-target-context-window (length offset)
  "Return bounded (START . END) offsets around OFFSET in text of LENGTH.
The returned source span never exceeds `ende-target-max-context-chars', and
each side is additionally capped by `ende-target-window-chars'."
  (unless (or (null ende-target-max-context-chars)
              (and (integerp ende-target-max-context-chars)
                   (> ende-target-max-context-chars 0)))
    (error "ende-target-max-context-chars must be nil or a positive integer"))
  (unless (or (null ende-target-window-chars)
              (and (integerp ende-target-window-chars)
                   (>= ende-target-window-chars 0)))
    (error "ende-target-window-chars must be nil or a nonnegative integer"))
  (setq offset (max 0 (min length offset)))
  (if (or (null ende-target-max-context-chars)
          (<= length ende-target-max-context-chars))
      (cons 0 length)
    (let* ((left-cap (if ende-target-window-chars
                         (min offset ende-target-window-chars)
                       offset))
           (right-cap (if ende-target-window-chars
                          (min (- length offset) ende-target-window-chars)
                        (- length offset)))
           (budget ende-target-max-context-chars)
           (left (min left-cap (/ budget 2)))
           (right (min right-cap (- budget left)))
           (remaining (- budget left right))
           (add-left (min remaining (- left-cap left))))
      (setq left (+ left add-left))
      (setq remaining (- remaining add-left))
      (setq right (+ right (min remaining (- right-cap right))))
      (cons (- offset left) (+ offset right)))))

(defun ende-target-format-marker-context
    (before after omitted-before omitted-after)
  "Format BEFORE and AFTER around a cursor, marking omitted sides."
  (concat
   (when omitted-before "...[characters elided]...\n")
   before
   "<CURSOR>"
   after
   (when omitted-after "\n...[characters elided]...")))

(defun ende-target-string-marker-context (text offset)
  "Return bounded TEXT context with a <CURSOR> marker at OFFSET."
  (setq offset (max 0 (min (length text) offset)))
  (let* ((bounds (ende-target-context-window (length text) offset))
         (start (car bounds))
         (end (cdr bounds)))
    (ende-target-format-marker-context
     (substring text start offset)
     (substring text offset end)
     (> start 0)
     (< end (length text)))))

(defun ende-target-restriction-valid-p (target start end)
  "Return non-nil when START and END are accessible for TARGET."
  (let ((restriction-start
         (ende-target--anchor-position
          (ende-target-restriction-beg target)))
        (restriction-end
         (ende-target--anchor-position
          (ende-target-restriction-end target))))
    (and (number-or-marker-p start) (number-or-marker-p end)
         restriction-start restriction-end
         (<= (point-min) restriction-start start)
         (<= start end restriction-end (point-max)))))

(defmacro ende-target-with-restriction (target &rest body)
  "Evaluate BODY within TARGET's original and current restrictions."
  (declare (indent 1) (debug t))
  `(let ((current-start (point-min))
         (current-end (point-max)))
     (save-restriction
       (widen)
       (let* ((original-start
               (ende-target--anchor-position
                (ende-target-restriction-beg ,target)))
              (original-end
               (ende-target--anchor-position
                (ende-target-restriction-end ,target)))
              (restriction-start
               (and original-start (max original-start current-start)))
              (restriction-end
               (and original-end (min original-end current-end))))
         (unless (and restriction-start restriction-end
                      (<= (point-min) restriction-start restriction-end
                          (point-max)))
           (error "original buffer restriction no longer exists"))
         (narrow-to-region restriction-start restriction-end)
         ,@body))))

(defun ende-target-validate-source (target start end)
  "Signal when TARGET's selected source at START..END has changed."
  (unless (ende-target-restriction-valid-p target start end)
    (error "target is outside the original restriction"))
  (let ((digest (ende-target-source-digest target))
        (snapshot (ende-target-source-snapshot target)))
    (cond
     (digest
      (unless (string= digest (ende-target-region-digest start end))
        (error "selected source changed before dispatch")))
     (snapshot
      (unless (string= snapshot
                       (buffer-substring-no-properties start end))
        (error "selected source changed before dispatch"))))))

(defun ende-target-selected-source (target start end)
  "Return TARGET's selected source at START..END, or signal if changed."
  (ende-target-validate-source target start end)
  (buffer-substring-no-properties start end))

(defun ende-target-buffer-marker-context (start end offset)
  "Return bounded buffer context from START to END with a marker at OFFSET."
  (setq offset (max start (min end offset)))
  (let* ((length (- end start))
         (relative-offset (- offset start))
         (bounds (ende-target-context-window length relative-offset))
         (window-start (+ start (car bounds)))
         (window-end (+ start (cdr bounds))))
    (ende-target-format-marker-context
     (buffer-substring-no-properties window-start offset)
     (buffer-substring-no-properties offset window-end)
     (> window-start start) (< window-end end))))

(defun ende-target-ensure-size (start end operation)
  "Reject an OPERATION target larger than the configured context limit."
  (when (and ende-target-max-context-chars
             (> (- end start) ende-target-max-context-chars))
    (user-error "%s target is %d characters; limit is %d"
                operation (- end start) ende-target-max-context-chars)))

(defun ende-target-assert-available (target)
  "Signal when TARGET intersects output awaiting review."
  (unless (eq (ende-target-review-policy target) 'ignore)
    (let ((buffer (ende-target-buffer target))
          (start (if (ende-target-selected target)
                     (ende-target-start target)
                   (ende-target-cursor target)))
          (end (if (ende-target-selected target)
                   (ende-target-end target)
                 (ende-target-cursor target))))
      (unless (and (buffer-live-p buffer) start end)
        (error "target anchor no longer exists"))
      (ende-review-assert-available buffer start end))))

;;;; Active mutation monitoring

(defun ende-target--add-local-hook (hook function)
  "Add FUNCTION to buffer-local HOOK in the current buffer."
  (add-hook hook function nil t))

(defun ende-target--remove-local-hook (hook function)
  "Remove FUNCTION from buffer-local HOOK in the current buffer."
  (remove-hook hook function t))

(defun ende-target--change-overlaps-p (target beg end)
  "Return non-nil when BEG..END overlaps TARGET's selected source."
  (let ((start (ende-target-start target))
        (finish (ende-target-end target)))
    (and (ende-target-selected target)
         start finish
         (if (= beg end)
             (and (> beg start) (< beg finish))
           (< (max beg start) (min end finish))))))

(defun ende-target-activate (target on-invalid &optional allow-source-changes)
  "Monitor TARGET and call ON-INVALID if it becomes unusable.
When ALLOW-SOURCE-CHANGES is non-nil, changes to TARGET's selected source do
not invalidate it.  The target buffer being killed remains fatal."
  (let ((buffer (ende-target-buffer target)))
    (when (buffer-live-p buffer)
      (let ((kill-hook
             (lambda ()
               (funcall on-invalid "target buffer was killed")))
            (before-hook
             (lambda (beg end)
               (when (not (ende-target-applying target))
                 (when (and (not allow-source-changes)
                            (ende-target--change-overlaps-p target beg end))
                   (setf (ende-target-invalidated target) t)))))
            (after-hook
             (lambda (&rest _)
               (when (and (not allow-source-changes)
                          (ende-target-invalidated target))
                 (funcall on-invalid
                          "selected source changed during generation")))))
        (setf (ende-target-kill-buffer-hook target) kill-hook
              (ende-target-before-change-hook target) before-hook
              (ende-target-after-change-hook target) after-hook)
        (with-current-buffer buffer
          (ende-target--add-local-hook 'kill-buffer-hook kill-hook)
          (ende-target--add-local-hook
           'before-change-functions before-hook)
          (ende-target--add-local-hook
           'after-change-functions after-hook)))))
  target)

(defmacro ende-target-with-application (target &rest body)
  "Evaluate BODY while TARGET ignores mutations made by its own result."
  (declare (indent 1) (debug t))
  `(progn
     (setf (ende-target-applying ,target) t)
     (unwind-protect
         (progn ,@body)
       (setf (ende-target-applying ,target) nil))))

(defun ende-target--remove-hooks (target)
  "Remove lifecycle hooks installed for TARGET."
  (let ((buffer (ende-target-buffer target))
        (kill-hook (ende-target-kill-buffer-hook target))
        (before-hook (ende-target-before-change-hook target))
        (after-hook (ende-target-after-change-hook target)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when kill-hook
          (ende-target--remove-local-hook 'kill-buffer-hook kill-hook))
        (when before-hook
          (ende-target--remove-local-hook
           'before-change-functions before-hook))
        (when after-hook
          (ende-target--remove-local-hook
           'after-change-functions after-hook))))
    (setf (ende-target-kill-buffer-hook target) nil
          (ende-target-before-change-hook target) nil
          (ende-target-after-change-hook target) nil)))

(defun ende-target-release (target)
  "Remove TARGET's hooks and detach all of its markers."
  (ende-target--remove-hooks target)
  (dolist (marker (list (ende-target-start-marker target)
                        (ende-target-end-marker target)
                        (ende-target-cursor-marker target)
                        (ende-target-restriction-beg target)
                        (ende-target-restriction-end target)))
    (when (markerp marker)
      (set-marker marker nil)))
  target)

;;;; Pending insertion anchors

(defun ende-target-advance-queued-anchors
    (targets buffer position new-position &optional on-move)
  "Advance TARGETS' cursor anchors at POSITION to NEW-POSITION in BUFFER.
Call ON-MOVE with each moved target when it is non-nil."
  (dolist (target targets)
    (let ((marker (ende-target-cursor-marker target)))
      (when (and (markerp marker)
                 (eq (marker-buffer marker) buffer)
                 (= (marker-position marker) position))
        (set-marker marker new-position)
        (when on-move
          (funcall on-move target))))))

(provide 'ende-target)

;;; ende-target.el ends here
