;;; deslop-ui.el --- Visual overlays and animations for deslop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools

;;; Commentary:

;; Visual indicator overlays, badges, and spinner animations for deslop jobs.

;;; Code:

(require 'cl-lib)
(require 'deslop-common)

;;;; Internal State

(defvar deslop-ui--managed-overlays nil
  "List of all active job overlays across queue and execution.")

(defvar deslop-ui--spinner-frames ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
  "Frames for the dynamic overlay spinner animation.")

(defvar deslop-ui--spinner-index 0
  "Current frame index in `deslop-ui--spinner-frames'.")

(defvar deslop-ui--spinner-timer nil
  "Timer instance for driving the overlay spinner animation.")

;;;; Badge Formatting & Animation

(defun deslop-ui--format-badge (ov spinner)
  "Format the display badge string for overlay OV using SPINNER frame."
  (let* ((status (or (overlay-get ov 'status) "Queued"))
         (is-queued (string= status "Queued"))
         (icon (if is-queued "⏳" spinner))
          (tokens (or (overlay-get ov 'tokens) 0))
          (speed (overlay-get ov 'speed))
          (estimated (overlay-get ov 'estimated))
          (metric-str
           (cond
            ((and speed (> speed 0))
             (format " (%s%d tok, %.1f t/s)"
                     (if estimated "~" "") tokens speed))
            ((> tokens 0)
             (format " (%s%d tok)" (if estimated "~" "") tokens))
            (t "")))
         (badge (format " [%s %s%s] " icon status metric-str)))
     (propertize badge 'face 'deslop-badge-face)))

(defun deslop-ui--prune-dead-overlays ()
  "Remove any deleted overlays or overlays on dead buffers."
  (setq deslop-ui--managed-overlays
        (delq nil (mapcar (lambda (ov)
                            (and (overlayp ov)
                                 (buffer-live-p (overlay-buffer ov))
                                 ov))
                          deslop-ui--managed-overlays))))

(defun deslop-ui--refresh-all-overlays ()
  "Refresh all active overlays.  Queued items remain static; active items spin."
  (deslop-ui--prune-dead-overlays)
  (if (null (cl-remove-if-not
             (lambda (ov)
               (not (string= (or (overlay-get ov 'status) "Queued") "Queued")))
             deslop-ui--managed-overlays))
      (deslop-ui--stop-spinner)
    (setq deslop-ui--spinner-index
          (% (1+ deslop-ui--spinner-index) (length deslop-ui--spinner-frames)))
    (let ((spinner (aref deslop-ui--spinner-frames deslop-ui--spinner-index)))
       (dolist (ov deslop-ui--managed-overlays)
         (unless (string= (or (overlay-get ov 'status) "Queued") "Queued")
           (overlay-put ov 'before-string
                        (deslop-ui--format-badge ov spinner)))))))

(defun deslop-ui--start-spinner ()
  "Start spinner animation timer if not currently running."
  (unless (or deslop-reduced-motion (timerp deslop-ui--spinner-timer))
    (setq deslop-ui--spinner-index 0)
    (setq deslop-ui--spinner-timer
          (run-with-timer 0 0.1 #'deslop-ui--refresh-all-overlays))))

(defun deslop-ui--stop-spinner ()
  "Stop the overlay spinner timer."
  (when (timerp deslop-ui--spinner-timer)
    (cancel-timer deslop-ui--spinner-timer)
    (setq deslop-ui--spinner-timer nil)))

;;;; Overlay Lifecycle API

(defun deslop-ui-create-overlay (buf beg end &optional status)
  "Create an indicator overlay in BUF from BEG to END with STATUS."
  (with-current-buffer buf
    (let* ((empty (= beg end))
           (ov (make-overlay beg end buf empty empty)))
      (when (> end beg)
        (overlay-put ov 'face 'deslop-working-face))
      (overlay-put ov 'status (or status "Queued"))
      (overlay-put ov 'tokens 0)
      (overlay-put ov 'speed nil)
      (overlay-put ov 'estimated nil)
      (overlay-put ov 'before-string
                   (deslop-ui--format-badge ov (aref deslop-ui--spinner-frames 0)))
      (push ov deslop-ui--managed-overlays)
      (deslop-ui--start-spinner)
      ov)))

(defun deslop-ui-update-overlay (ov &rest props)
  "Update properties of overlay OV with PROPS plist and update badge."
  (when (and (overlayp ov) (buffer-live-p (overlay-buffer ov)))
    (while props
      (let ((key (car props))
            (val (cadr props)))
        (setq props (cddr props))
        (pcase key
           (:status (overlay-put ov 'status val))
           (:tokens (overlay-put ov 'tokens val))
           (:speed  (overlay-put ov 'speed val))
           (:estimated (overlay-put ov 'estimated val)))))
    (let ((spinner (aref deslop-ui--spinner-frames deslop-ui--spinner-index)))
      (overlay-put ov 'before-string (deslop-ui--format-badge ov spinner)))))

(defun deslop-ui-delete-overlay (ov)
  "Delete overlay OV and clean up managed tracking."
  (when (overlayp ov)
    (setq deslop-ui--managed-overlays (delq ov deslop-ui--managed-overlays))
    (delete-overlay ov)
    (when (null deslop-ui--managed-overlays)
      (deslop-ui--stop-spinner))))

(defun deslop-ui-clear-all ()
  "Delete all managed overlays and stop spinner animation."
  (dolist (ov deslop-ui--managed-overlays)
    (when (overlayp ov)
      (delete-overlay ov)))
  (setq deslop-ui--managed-overlays nil)
  (deslop-ui--stop-spinner))

(provide 'deslop-ui)

;;; deslop-ui.el ends here
