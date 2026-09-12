;;; deslop-target.el --- Buffer targets for deslop jobs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools

;;; Commentary:

;; Stable buffer targets, source validation, restrictions, conflict checks,
;; mutation monitoring, and queued insertion-anchor movement for deslop.

;;; Code:

(require 'cl-lib)
(require 'deslop-core)
(require 'deslop-review)

(defcustom deslop-target-max-context-chars 24000
  "Maximum number of characters sent to the LLM as target context.
Tokens are estimated as chars/4."
  :type 'integer
  :group 'deslop)

(defcustom deslop-target-window-chars 8000
  "Characters kept on each side of the cursor when target context is elided."
  :type 'integer
  :group 'deslop)

(cl-defstruct (deslop-target (:constructor deslop-target--create))
  "A stable, restricted buffer target for a queued operation."
  buffer start-marker end-marker cursor-marker placement
  source-snapshot source-digest restriction-beg restriction-end selected offset
  applying invalidated kill-buffer-hook before-change-hook after-change-hook)

;;;; Creation and positions

(defun deslop-target--make-marker (buffer position insertion-type)
  "Create a marker in BUFFER at POSITION with INSERTION-TYPE."
  (let ((marker (make-marker)))
    (set-marker marker position buffer)
    (set-marker-insertion-type marker insertion-type)
    marker))

(defun deslop-target--marker-position (marker)
  "Return MARKER's position, or nil when it has no live buffer."
  (and (markerp marker) (marker-position marker)))

(defun deslop-target--anchor-position (anchor)
  "Return ANCHOR as a live marker position or numeric position."
  (if (markerp anchor) (marker-position anchor) anchor))

(defun deslop-target-region-digest (start end)
  "Return a stable digest of the current buffer between START and END."
  (secure-hash 'sha256 (current-buffer) start end))

(cl-defun deslop-target-create
    (&key buffer start end cursor selected placement offset source-snapshot)
  "Create a target in BUFFER from START to END anchored at CURSOR.
SELECTED means START..END is immutable source.  PLACEMENT and OFFSET describe
an insertion relative to that source.  SOURCE-SNAPSHOT is an optional textual
fallback for callers that cannot capture a digest."
  (unless (buffer-live-p buffer)
    (error "target buffer no longer exists"))
  (with-current-buffer buffer
    (deslop-review-assert-available buffer start end)
    (deslop-target--create
     :buffer buffer
     :start-marker (deslop-target--make-marker buffer start (and selected t))
     :end-marker (deslop-target--make-marker buffer end nil)
     :cursor-marker (deslop-target--make-marker buffer cursor nil)
     :placement placement
     :source-snapshot source-snapshot
     :source-digest (and selected
                         (null source-snapshot)
                         (deslop-target-region-digest start end))
     :restriction-beg (deslop-target--make-marker buffer (point-min) nil)
     :restriction-end (deslop-target--make-marker buffer (point-max) t)
     :selected selected
     :offset offset)))

(defun deslop-target-start (target)
  "Return TARGET's current start position, or nil."
  (deslop-target--marker-position (deslop-target-start-marker target)))

(defun deslop-target-end (target)
  "Return TARGET's current end position, or nil."
  (deslop-target--marker-position (deslop-target-end-marker target)))

(defun deslop-target-cursor (target)
  "Return TARGET's current cursor position, or nil."
  (deslop-target--marker-position (deslop-target-cursor-marker target)))

(defun deslop-target-live-p (target)
  "Return non-nil when TARGET's buffer is live."
  (and (deslop-target-p target)
       (buffer-live-p (deslop-target-buffer target))))

;;;; Restrictions, source, context, and conflicts

(defun deslop-target-context-window (length offset)
  "Return bounded (START . END) offsets around OFFSET in text of LENGTH.
The returned source span never exceeds `deslop-target-max-context-chars', and
each side is additionally capped by `deslop-target-window-chars'."
  (unless (and (integerp deslop-target-max-context-chars)
               (> deslop-target-max-context-chars 0))
    (error "deslop-target-max-context-chars must be a positive integer"))
  (unless (and (integerp deslop-target-window-chars)
               (>= deslop-target-window-chars 0))
    (error "deslop-target-window-chars must be a nonnegative integer"))
  (setq offset (max 0 (min length offset)))
  (if (<= length deslop-target-max-context-chars)
      (cons 0 length)
    (let* ((left-cap (min offset deslop-target-window-chars))
           (right-cap (min (- length offset) deslop-target-window-chars))
           (budget deslop-target-max-context-chars)
           (left (min left-cap (/ budget 2)))
           (right (min right-cap (- budget left)))
           (remaining (- budget left right))
           (add-left (min remaining (- left-cap left))))
      (setq left (+ left add-left))
      (setq remaining (- remaining add-left))
      (setq right (+ right (min remaining (- right-cap right))))
      (cons (- offset left) (+ offset right)))))

(defun deslop-target-format-marker-context
    (before after omitted-before omitted-after)
  "Format BEFORE and AFTER around a cursor, marking omitted sides."
  (concat
   (when omitted-before "...[characters elided]...\n")
   before
   "<CURSOR>"
   after
   (when omitted-after "\n...[characters elided]...")))

(defun deslop-target-string-marker-context (text offset)
  "Return bounded TEXT context with a <CURSOR> marker at OFFSET."
  (setq offset (max 0 (min (length text) offset)))
  (let* ((bounds (deslop-target-context-window (length text) offset))
         (start (car bounds))
         (end (cdr bounds)))
    (deslop-target-format-marker-context
     (substring text start offset)
     (substring text offset end)
     (> start 0)
     (< end (length text)))))

(defun deslop-target-restriction-valid-p (target start end)
  "Return non-nil when START and END are accessible for TARGET."
  (let ((restriction-start
         (deslop-target--anchor-position
          (deslop-target-restriction-beg target)))
        (restriction-end
         (deslop-target--anchor-position
          (deslop-target-restriction-end target))))
    (and (number-or-marker-p start) (number-or-marker-p end)
         restriction-start restriction-end
         (<= (point-min) restriction-start start)
         (<= start end restriction-end (point-max)))))

(defmacro deslop-target-with-restriction (target &rest body)
  "Evaluate BODY within TARGET's original and current restrictions."
  (declare (indent 1) (debug t))
  `(let ((current-start (point-min))
         (current-end (point-max)))
     (save-restriction
       (widen)
       (let* ((original-start
               (deslop-target--anchor-position
                (deslop-target-restriction-beg ,target)))
              (original-end
               (deslop-target--anchor-position
                (deslop-target-restriction-end ,target)))
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

(defun deslop-target-validate-source (target start end)
  "Signal when TARGET's selected source at START..END has changed."
  (unless (deslop-target-restriction-valid-p target start end)
    (error "target is outside the original restriction"))
  (let ((digest (deslop-target-source-digest target))
        (snapshot (deslop-target-source-snapshot target)))
    (cond
     (digest
      (unless (string= digest (deslop-target-region-digest start end))
        (error "selected source changed before dispatch")))
     (snapshot
      (unless (string= snapshot
                       (buffer-substring-no-properties start end))
        (error "selected source changed before dispatch"))))))

(defun deslop-target-selected-source (target start end)
  "Return TARGET's selected source at START..END, or signal if changed."
  (deslop-target-validate-source target start end)
  (buffer-substring-no-properties start end))

(defun deslop-target-buffer-marker-context (start end offset)
  "Return bounded buffer context from START to END with a marker at OFFSET."
  (setq offset (max start (min end offset)))
  (let* ((length (- end start))
         (relative-offset (- offset start))
         (bounds (deslop-target-context-window length relative-offset))
         (window-start (+ start (car bounds)))
         (window-end (+ start (cdr bounds))))
    (deslop-target-format-marker-context
     (buffer-substring-no-properties window-start offset)
     (buffer-substring-no-properties offset window-end)
     (> window-start start) (< window-end end))))

(defun deslop-target-ensure-size (start end operation)
  "Reject an OPERATION target larger than the configured context limit."
  (when (> (- end start) deslop-target-max-context-chars)
    (user-error "%s target is %d characters; limit is %d"
                operation (- end start) deslop-target-max-context-chars)))

(defun deslop-target-assert-available (target)
  "Signal when TARGET intersects output awaiting review."
  (let ((buffer (deslop-target-buffer target))
        (start (if (deslop-target-selected target)
                   (deslop-target-start target)
                 (deslop-target-cursor target)))
        (end (if (deslop-target-selected target)
                 (deslop-target-end target)
               (deslop-target-cursor target))))
    (unless (and (buffer-live-p buffer) start end)
      (error "target anchor no longer exists"))
    (deslop-review-assert-available buffer start end)))

;;;; Active mutation monitoring

(defun deslop-target--add-local-hook (hook function)
  "Add FUNCTION to buffer-local HOOK in the current buffer."
  (add-hook hook function nil t))

(defun deslop-target--remove-local-hook (hook function)
  "Remove FUNCTION from buffer-local HOOK in the current buffer."
  (remove-hook hook function t))

(defun deslop-target--change-overlaps-p (target beg end)
  "Return non-nil when BEG..END overlaps TARGET's selected source."
  (let ((start (deslop-target-start target))
        (finish (deslop-target-end target)))
    (and (deslop-target-selected target)
         start finish
         (if (= beg end)
             (and (> beg start) (< beg finish))
           (< (max beg start) (min end finish))))))

(defun deslop-target-activate (target on-invalid)
  "Monitor TARGET and call ON-INVALID with a message if it becomes unusable."
  (let ((buffer (deslop-target-buffer target)))
    (when (buffer-live-p buffer)
      (let ((kill-hook
             (lambda ()
               (funcall on-invalid "target buffer was killed")))
            (before-hook
             (lambda (beg end)
               (when (and (not (deslop-target-applying target))
                          (deslop-target--change-overlaps-p target beg end))
                 (setf (deslop-target-invalidated target) t))))
            (after-hook
             (lambda (&rest _)
               (when (deslop-target-invalidated target)
                 (funcall on-invalid
                          "selected source changed during generation")))))
        (setf (deslop-target-kill-buffer-hook target) kill-hook
              (deslop-target-before-change-hook target) before-hook
              (deslop-target-after-change-hook target) after-hook)
        (with-current-buffer buffer
          (deslop-target--add-local-hook 'kill-buffer-hook kill-hook)
          (deslop-target--add-local-hook
           'before-change-functions before-hook)
          (deslop-target--add-local-hook
           'after-change-functions after-hook)))))
  target)

(defmacro deslop-target-with-application (target &rest body)
  "Evaluate BODY while TARGET ignores mutations made by its own result."
  (declare (indent 1) (debug t))
  `(progn
     (setf (deslop-target-applying ,target) t)
     (unwind-protect
         (progn ,@body)
       (setf (deslop-target-applying ,target) nil))))

(defun deslop-target--remove-hooks (target)
  "Remove lifecycle hooks installed for TARGET."
  (let ((buffer (deslop-target-buffer target))
        (kill-hook (deslop-target-kill-buffer-hook target))
        (before-hook (deslop-target-before-change-hook target))
        (after-hook (deslop-target-after-change-hook target)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when kill-hook
          (deslop-target--remove-local-hook 'kill-buffer-hook kill-hook))
        (when before-hook
          (deslop-target--remove-local-hook
           'before-change-functions before-hook))
        (when after-hook
          (deslop-target--remove-local-hook
           'after-change-functions after-hook))))
    (setf (deslop-target-kill-buffer-hook target) nil
          (deslop-target-before-change-hook target) nil
          (deslop-target-after-change-hook target) nil)))

(defun deslop-target-release (target)
  "Remove TARGET's hooks and detach all of its markers."
  (deslop-target--remove-hooks target)
  (dolist (marker (list (deslop-target-start-marker target)
                        (deslop-target-end-marker target)
                        (deslop-target-cursor-marker target)
                        (deslop-target-restriction-beg target)
                        (deslop-target-restriction-end target)))
    (when (markerp marker)
      (set-marker marker nil)))
  target)

;;;; Pending insertion anchors

(defun deslop-target-advance-queued-anchors
    (targets buffer position new-position &optional on-move)
  "Advance TARGETS' cursor anchors at POSITION to NEW-POSITION in BUFFER.
Call ON-MOVE with each moved target when it is non-nil."
  (dolist (target targets)
    (let ((marker (deslop-target-cursor-marker target)))
      (when (and (markerp marker)
                 (eq (marker-buffer marker) buffer)
                 (= (marker-position marker) position))
        (set-marker marker new-position)
        (when on-move
          (funcall on-move target))))))

(provide 'deslop-target)

;;; deslop-target.el ends here
