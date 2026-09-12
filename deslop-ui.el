;;; deslop-ui.el --- Visual overlays and animations for deslop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools

;;; Commentary:

;; Visual indicator overlays, badges, and spinner animations for deslop jobs.

;;; Code:

(require 'cl-lib)
(require 'deslop-core)

(defcustom deslop-reduced-motion nil
  "Non-nil to disable the animated overlay spinner."
  :type 'boolean
  :group 'deslop)

(defface deslop-working-face
  '((t :underline (:color "#38bdf8" :style line)))
  "Face applied to buffer text regions currently being processed by LLM."
  :group 'deslop)

;;;; Internal State

(defvar deslop-ui--managed-overlays nil
  "List of all active job overlays across queue and execution.")

(defvar deslop-ui--spinning-overlays nil
  "List of running job overlays that require animation.")

(defvar deslop-ui--spinner-frames ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
  "Frames for the dynamic overlay spinner animation.")

(defvar deslop-ui--spinner-index 0
  "Current frame index in `deslop-ui--spinner-frames'.")

(defvar deslop-ui--spinner-timer nil
  "Timer instance for driving the overlay spinner animation.")

;;;; Badge Formatting & Animation

(defun deslop-ui--display-label (value)
  "Return VALUE as a bounded, single-line display label."
  (deslop--bounded-single-line value 80 t))

(defun deslop-ui--format-badge (ov spinner)
  "Format the display badge string for overlay OV using SPINNER frame."
  (let* ((status (or (overlay-get ov 'status) "Queued"))
         (is-queued (string= status "Queued"))
         (icon (if is-queued "⏳" spinner))
         (client (deslop-ui--display-label (overlay-get ov 'client)))
         (model (deslop-ui--display-label (overlay-get ov 'model)))
         (provider-str
          (let ((parts (delq nil (list client model))))
            (if parts
                (format " %s" (mapconcat #'identity parts " "))
              "")))
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
         (badge (format " [%s %s%s%s] " icon status provider-str metric-str)))
    (propertize badge 'face 'deslop-badge-face)))

(defun deslop-ui--put-badge (ov spinner)
  "Place the SPINNER badge on OV's appropriate side.
Zero-width indicators use `after-string' so point can remain before them;
range overlays keep their badge before the covered text."
  (let ((badge (deslop-ui--format-badge ov spinner)))
    (if (= (overlay-start ov) (overlay-end ov))
        (progn
          (overlay-put ov 'before-string nil)
          (overlay-put ov 'after-string badge))
      (overlay-put ov 'after-string nil)
      (overlay-put ov 'before-string badge))))

(defun deslop-ui--prune-dead-overlays ()
  "Remove any deleted overlays or overlays on dead buffers from tracking."
  (let ((live-overlays
         (lambda (overlays)
           (cl-remove-if-not
            (lambda (ov)
              (and (overlayp ov) (buffer-live-p (overlay-buffer ov))))
            overlays))))
    (setq deslop-ui--managed-overlays
          (funcall live-overlays deslop-ui--managed-overlays)
          deslop-ui--spinning-overlays
          (funcall live-overlays deslop-ui--spinning-overlays))))

(defun deslop-ui--refresh-all-overlays ()
  "Refresh all active overlays. Queued items remain static; active items spin."
  (deslop-ui--prune-dead-overlays)
  (if (or deslop-reduced-motion (null deslop-ui--spinning-overlays))
      (deslop-ui--stop-spinner)
    (setq deslop-ui--spinner-index
          (% (1+ deslop-ui--spinner-index) (length deslop-ui--spinner-frames)))
    (let ((spinner (aref deslop-ui--spinner-frames deslop-ui--spinner-index)))
      (dolist (ov deslop-ui--spinning-overlays)
        (deslop-ui--put-badge ov spinner)))))

(defun deslop-ui--start-spinner ()
  "Start spinner animation timer if not currently running."
  (unless (or deslop-reduced-motion (timerp deslop-ui--spinner-timer))
    (setq deslop-ui--spinner-index 0)
    (setq deslop-ui--spinner-timer
          (run-with-timer 0 0.5 #'deslop-ui--refresh-all-overlays))))

(defun deslop-ui--stop-spinner ()
  "Stop the overlay spinner timer."
  (when (timerp deslop-ui--spinner-timer)
    (cancel-timer deslop-ui--spinner-timer)
    (setq deslop-ui--spinner-timer nil)))

(defun deslop-ui-create-overlay (buf beg end &optional status)
  "Create an indicator overlay in BUF from BEG to END with STATUS."
  (with-current-buffer buf
    (let ((ov (make-overlay beg end buf nil nil)))
      (when (> end beg)
        (overlay-put ov 'face 'deslop-working-face))
      (overlay-put ov 'status (or status "Queued"))
      (overlay-put ov 'tokens 0)
      (overlay-put ov 'speed nil)
      (overlay-put ov 'estimated nil)
      (deslop-ui--put-badge ov (aref deslop-ui--spinner-frames 0))
      (push ov deslop-ui--managed-overlays)
      ov)))

(defun deslop-ui-update-overlay (ov &rest props)
  "Update properties of overlay OV with PROPS plist and update badge."
  (when (and (overlayp ov) (buffer-live-p (overlay-buffer ov)))
    (while props
      (let ((key (car props))
            (val (cadr props)))
        (setq props (cddr props))
        (pcase key
          (:status
           (overlay-put ov 'status val)
           (if (string= (or val "Queued") "Queued")
               (setq deslop-ui--spinning-overlays
                     (delq ov deslop-ui--spinning-overlays))
             (cl-pushnew ov deslop-ui--spinning-overlays)
             (deslop-ui--start-spinner)))
          (:client (overlay-put ov 'client val))
          (:model (overlay-put ov 'model val))
          (:tokens (overlay-put ov 'tokens val))
          (:speed  (overlay-put ov 'speed val))
          (:estimated (overlay-put ov 'estimated val)))))
    (let ((spinner (aref deslop-ui--spinner-frames deslop-ui--spinner-index)))
      (deslop-ui--put-badge ov spinner))))

(defun deslop-ui-delete-overlay (ov)
  "Delete overlay OV and clean up managed tracking."
  (when (overlayp ov)
    (setq deslop-ui--managed-overlays (delq ov deslop-ui--managed-overlays))
    (setq deslop-ui--spinning-overlays (delq ov deslop-ui--spinning-overlays))
    (delete-overlay ov)
    (when (null deslop-ui--spinning-overlays)
      (deslop-ui--stop-spinner))))

(defun deslop-ui-clear-all ()
  "Delete all managed overlays and stop spinner animation."
  (dolist (ov deslop-ui--managed-overlays)
    (when (overlayp ov)
      (delete-overlay ov)))
  (setq deslop-ui--managed-overlays nil)
  (setq deslop-ui--spinning-overlays nil)
  (deslop-ui--stop-spinner))

(provide 'deslop-ui)

;;; deslop-ui.el ends here
