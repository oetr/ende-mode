;;; ende-ui.el --- Visual overlays and animations for ende -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools

;;; Commentary:

;; Visual indicator overlays, badges, and spinner animations for ende jobs.
;; Agent activity is transient plain text outside the folded tool transcript;
;; host execution phases and token metrics do not replace that activity.

;;; Code:

(require 'cl-lib)
(require 'ende-core)
(require 'ende-queue)

(defcustom ende-reduced-motion nil
  "Non-nil to disable the animated overlay spinner."
  :type 'boolean
  :group 'ende)

(defface ende-working-face
  '((t :underline (:color "#38bdf8" :style line)))
  "Face applied to buffer text regions currently being processed by LLM."
  :group 'ende)

;;;; Internal State

(defvar ende-ui--managed-overlays nil
  "List of all active job overlays across queue and execution.")

(defvar ende-ui--spinning-overlays nil
  "List of running job overlays that require animation.")

(defvar ende-ui--spinner-frames ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
  "Frames for the dynamic overlay spinner animation.")

(defvar ende-ui--spinner-index 0
  "Current frame index in `ende-ui--spinner-frames'.")

(defvar ende-ui--spinner-timer nil
  "Timer instance for driving the overlay spinner animation.")

;;;; Badge Formatting & Animation

(defun ende-ui--display-label (value)
  "Return VALUE as a bounded, single-line display label."
  (ende-bounded-single-line value 80 t))

(defun ende-ui--format-badge (ov spinner)
  "Format the display badge string for overlay OV using SPINNER frame."
  (let* ((status (or (overlay-get ov 'status) "Queued"))
         (is-queued (string= status "Queued"))
         (icon (if is-queued "⏳" spinner))
         (activity (ende-ui--display-label (overlay-get ov 'activity-title)))
         (client (ende-ui--display-label (overlay-get ov 'client)))
         (model (ende-ui--display-label (overlay-get ov 'model)))
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
         (badge (format " [%s %s%s%s%s] " icon status
                        (if activity (concat " — " activity) "")
                        provider-str metric-str)))
    (propertize badge 'face 'ende-badge-face)))

(defun ende-ui--activity-paragraph (status)
  "Return safely displayed, wrapped plain-text STATUS for a live indicator.
Format only when the report changes, not on each spinner tick."
  (when status
    (with-temp-buffer
      (insert (replace-regexp-in-string
               "[[:cntrl:]]+" " " (substring-no-properties status)))
      (let ((fill-column 72))
        (fill-region (point-min) (point-max)))
      (propertize
       (concat "\n    "
               (replace-regexp-in-string "\n" "\n    " (buffer-string))
               "\n")
       'face 'shadow))))

(defun ende-ui--put-badge (ov spinner)
  "Place the SPINNER badge on OV's appropriate side.
Zero-width indicators use `after-string' so point can remain before them;
range overlays keep their badge before the covered text."
  (let ((badge (concat (ende-ui--format-badge ov spinner)
                       (overlay-get ov 'activity-paragraph))))
    (if (= (overlay-start ov) (overlay-end ov))
        (progn
          (overlay-put ov 'before-string nil)
          (overlay-put ov 'after-string badge))
      (overlay-put ov 'after-string nil)
      (overlay-put ov 'before-string badge))))

(defun ende-ui--prune-dead-overlays ()
  "Remove any deleted overlays or overlays on dead buffers from tracking."
  (let ((live-overlays
         (lambda (overlays)
           (cl-remove-if-not
            (lambda (ov)
              (and (overlayp ov) (buffer-live-p (overlay-buffer ov))))
            overlays))))
    (setq ende-ui--managed-overlays
          (funcall live-overlays ende-ui--managed-overlays)
          ende-ui--spinning-overlays
          (funcall live-overlays ende-ui--spinning-overlays))))

(defun ende-ui--refresh-all-overlays ()
  "Refresh all active overlays. Queued items remain static; active items spin."
  (ende-ui--prune-dead-overlays)
  (if (or ende-reduced-motion (null ende-ui--spinning-overlays))
      (ende-ui--stop-spinner)
    (setq ende-ui--spinner-index
          (% (1+ ende-ui--spinner-index) (length ende-ui--spinner-frames)))
    (let ((spinner (aref ende-ui--spinner-frames ende-ui--spinner-index)))
      (dolist (ov ende-ui--spinning-overlays)
        (ende-ui--put-badge ov spinner)))))

(defun ende-ui--start-spinner ()
  "Start spinner animation timer if not currently running."
  (unless (or ende-reduced-motion (timerp ende-ui--spinner-timer))
    (setq ende-ui--spinner-index 0)
    (setq ende-ui--spinner-timer
          (run-with-timer 0 0.5 #'ende-ui--refresh-all-overlays))))

(defun ende-ui--stop-spinner ()
  "Stop the overlay spinner timer."
  (when (timerp ende-ui--spinner-timer)
    (cancel-timer ende-ui--spinner-timer)
    (setq ende-ui--spinner-timer nil)))

(defun ende-ui-create-overlay (buf beg end &optional status appearance)
  "Create an indicator overlay in BUF from BEG to END with STATUS.
When APPEARANCE is `badge-only', do not apply a face to the covered text."
  (with-current-buffer buf
    (let ((ov (make-overlay beg end buf nil nil)))
      (when (and (> end beg) (not (eq appearance 'badge-only)))
        (overlay-put ov 'face 'ende-working-face))
      (overlay-put ov 'status (or status "Queued"))
      (overlay-put ov 'tokens 0)
      (overlay-put ov 'speed nil)
      (overlay-put ov 'estimated nil)
      (ende-ui--put-badge ov (aref ende-ui--spinner-frames 0))
      (push ov ende-ui--managed-overlays)
      ov)))

(defun ende-ui-update-overlay (ov &rest props)
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
               (setq ende-ui--spinning-overlays
                     (delq ov ende-ui--spinning-overlays))
             (cl-pushnew ov ende-ui--spinning-overlays)
             (ende-ui--start-spinner)))
          (:client (overlay-put ov 'client val))
          (:model (overlay-put ov 'model val))
          (:activity-title (overlay-put ov 'activity-title val))
          (:activity-status
           (overlay-put ov 'activity-paragraph
                        (ende-ui--activity-paragraph val)))
          (:tokens (overlay-put ov 'tokens val))
          (:speed  (overlay-put ov 'speed val))
          (:estimated (overlay-put ov 'estimated val)))))
    (let ((spinner (aref ende-ui--spinner-frames ende-ui--spinner-index)))
      (ende-ui--put-badge ov spinner))))

(defun ende-ui-job-event (job event)
  "Update JOB's indicator in response to lifecycle EVENT.
Queue ownership stays in `ende-queue'; this function is the presentation
adapter for queued and terminal states."
  (let ((overlay (ende-job-overlay job)))
    (pcase (plist-get event :type)
      (:queued
       (when overlay
         (ende-ui-update-overlay
          overlay
          :client (plist-get event :client)
          :model (plist-get event :model))))
      (:agent-activity
       (when overlay
         (ende-ui-update-overlay
          overlay :activity-title (ende-job-activity-title job)
          :activity-status (ende-job-activity-status job))))
      (:settled
       (when overlay
         (ende-ui-delete-overlay overlay)
         (setf (ende-job-overlay job) nil))))))

(defun ende-ui-delete-overlay (ov)
  "Delete overlay OV and clean up managed tracking."
  (when (overlayp ov)
    (setq ende-ui--managed-overlays (delq ov ende-ui--managed-overlays))
    (setq ende-ui--spinning-overlays (delq ov ende-ui--spinning-overlays))
    (delete-overlay ov)
    (when (null ende-ui--spinning-overlays)
      (ende-ui--stop-spinner))))

(defun ende-ui-clear-all ()
  "Delete all managed overlays and stop spinner animation."
  (dolist (ov ende-ui--managed-overlays)
    (when (overlayp ov)
      (delete-overlay ov)))
  (setq ende-ui--managed-overlays nil)
  (setq ende-ui--spinning-overlays nil)
  (ende-ui--stop-spinner))

(provide 'ende-ui)

;;; ende-ui.el ends here
