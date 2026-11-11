;;; ende-ui.el --- Visual overlays and animations for ende -*- lexical-binding: t; -*-

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

;; Visual indicator overlays, badges, and spinner animations for ende jobs.
;; Agent activity is transient plain text outside the folded tool transcript;
;; host execution phases and token metrics do not replace that activity.

;;; Code:

(require 'cl-lib)
(require 'svg)
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

(defvar ende-ui--svg-spinner-frames nil
  "Cached SVG image strings for the circular overlay spinner.")

(defconst ende-ui--fallback-spinner-frames ["◴" "◷" "◶" "◵"]
  "Unicode spinner frames for displays without SVG image support.")

(defvar ende-ui--spinner-index 0
  "Current frame index in the selected spinner frame vector.")

(defvar ende-ui--spinner-timer nil
  "Timer instance for driving the overlay spinner animation.")

;;;; Badge Formatting & Animation

(defun ende-ui--spinner-frames ()
  "Return cached SVG spinner frames, or terminal fallback frames."
  (if (and (display-graphic-p) (image-type-available-p 'svg))
      (or ende-ui--svg-spinner-frames
          (setq ende-ui--svg-spinner-frames
                (let ((frames (make-vector 12 nil)))
                  (dotimes (frame 12)
                    (let ((svg (svg-create 18 18)))
                      (dotimes (stroke 12)
                        (let* ((angle (- (* stroke (/ (* 2.0 float-pi) 12))
                                         (/ float-pi 2)))
                               (opacity (/ (float (1+ (mod (- stroke frame) 12)))
                                           12.0))
                               (inner 5.0)
                               (outer 7.0)
                               (cx 9.0)
                               (cy 9.0))
                          (svg-line svg
                                    (+ cx (* inner (cos angle)))
                                    (+ cy (* inner (sin angle)))
                                    (+ cx (* outer (cos angle)))
                                    (+ cy (* outer (sin angle)))
                                    :stroke "#38bdf8"
                                    :stroke-width 1.8
                                    :stroke-linecap "round"
                                    :opacity opacity)))
                      (aset frames frame
                            (propertize " " 'display
                                        (svg-image svg :ascent 'center)))))
                  frames)))
    ende-ui--fallback-spinner-frames))

(defun ende-ui--spinner-frame (&optional index static)
  "Return spinner frame INDEX, or the first when STATIC is non-nil."
  (let* ((frames (ende-ui--spinner-frames))
         (frame-index (if static 0 (% (or index 0) (length frames)))))
    (aref frames frame-index)))

(defun ende-ui--display-label (value)
  "Return VALUE as a bounded, single-line display label."
  (ende-bounded-single-line value 80 t))

(defun ende-ui--format-count (value)
  "Return numeric VALUE as a grouped integer, or nil when unknown."
  (when (and (numberp value) (>= value 0))
    (let* ((digits (number-to-string (truncate value)))
           (end (length digits))
           chunks)
      (while (> end 3)
        (push (substring digits (- end 3) end) chunks)
        (setq end (- end 3)))
      (push (substring digits 0 end) chunks)
      (mapconcat #'identity chunks ","))))

(defun ende-ui--format-paragraph (text)
  "Return safely displayed, wrapped activity STATUS."
  (when (and (stringp text) (not (string-empty-p text)))
    (with-temp-buffer
      (insert (replace-regexp-in-string
               "[[:cntrl:]]+" " " (substring-no-properties text)))
      (let ((fill-column 72))
        (fill-region (point-min) (point-max)))
      (propertize
       (concat "    "
               (replace-regexp-in-string "\n" "\n    " (buffer-string))
               "\n")
       'face 'shadow))))

(defun ende-ui--focus-paragraph (text)
  "Format a bounded, safely displayed TEXT with a bold Focus label."
  (when (and (stringp text) (not (string-blank-p text)))
    (let* ((plain (replace-regexp-in-string
                   "\\*\\*\\([^*]+\\)\\*\\*" "\\1"
                   (substring-no-properties text)))
           (limit 800)
           (label "Focus: "))
      (setq plain (concat (substring plain 0 (min limit (length plain)))
                          (if (> (length plain) limit) "…" "")))
      (with-temp-buffer
        (insert label (replace-regexp-in-string "[[:cntrl:]]+" " " plain))
        (let ((fill-column 72))
          (fill-region (point-min) (point-max)))
        (let ((paragraph (propertize (buffer-string) 'face 'shadow)))
          (add-face-text-property 0 (length label) 'bold t paragraph)
          (concat paragraph "\n"))))))

(defun ende-ui--format-badge (ov)
  "Format cached header, metrics, and activity line content for OV."
  (let* ((status (or (overlay-get ov 'status) "Queued"))
         (activity (ende-ui--display-label (overlay-get ov 'activity-title)))
         (client (ende-ui--display-label (overlay-get ov 'client)))
         (model (ende-ui--display-label (overlay-get ov 'model)))
         (effort (ende-ui--display-label (overlay-get ov 'reasoning-effort)))
         (counts (ende--usage-token-counts (overlay-get ov 'usage)))
         (input (ende-ui--format-count (plist-get counts :input)))
         (cached (ende-ui--format-count (plist-get counts :cached)))
         (estimated (overlay-get ov 'estimated))
         (output (ende-ui--format-count
                  (if estimated
                      (or (overlay-get ov 'tokens) (plist-get counts :output))
                    (or (plist-get counts :output) (overlay-get ov 'tokens)))))
         (speed (overlay-get ov 'speed))
         (phase (ende-ui--display-label status)))
    (concat
     (propertize
      (mapconcat #'identity (delq nil (list client model effort)) " ")
      'face 'ende-badge-face)
     (when (and (numberp speed) (> speed 0))
       (propertize (format " - %.1f t/s" speed) 'face 'shadow))
     "\n"
     (when input
       (propertize (format "Tokens: %s\n" input) 'face 'shadow))
     (when cached
       (propertize (format "Cached: %s\n" cached) 'face 'shadow))
     (when output
       (propertize (format "Output: %s%s\n"
                           (if estimated "~" "") output)
                   'face 'shadow))
     (when (and phase
                (or activity (null (overlay-get ov 'reasoning-paragraph))))
       (propertize
        (concat phase (if activity (concat "\n" activity) "") "\n")
        'face 'font-lock-keyword-face)))))

(defun ende-ui--activity-paragraph (status)
  "Return safely displayed, wrapped plain-text STATUS for a live indicator.
Format only when the report changes, not on each spinner tick."
  (ende-ui--format-paragraph status))

(defun ende-ui--put-badge (ov spinner)
  "Place the SPINNER badge on OV's appropriate side.
Zero-width indicators use `after-string' so point can remain before them;
range overlays keep their badge before the covered text."
  (let ((badge (concat
                (if (string= (or (overlay-get ov 'status) "Queued") "Queued")
                    "⏳"
                  spinner)
                " "
                (or (overlay-get ov 'badge-content) "")
                (or (overlay-get ov 'activity-paragraph) "")
                (or (overlay-get ov 'reasoning-paragraph) ""))))
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
  (cond
   ((null ende-ui--spinning-overlays)
    (ende-ui--stop-spinner))
   (ende-reduced-motion
    (ende-ui--stop-spinner)
    (dolist (ov ende-ui--spinning-overlays)
      (ende-ui--put-badge ov (ende-ui--spinner-frame 0 t))))
   (t
    (setq ende-ui--spinner-index
          (% (1+ ende-ui--spinner-index)
             (length (ende-ui--spinner-frames))))
    (let ((spinner (ende-ui--spinner-frame ende-ui--spinner-index)))
      (dolist (ov ende-ui--spinning-overlays)
        (ende-ui--put-badge ov spinner))))))

(defun ende-ui--start-spinner ()
  "Start spinner animation timer if not currently running."
  (unless (or ende-reduced-motion (timerp ende-ui--spinner-timer))
    (setq ende-ui--spinner-index 0)
    (setq ende-ui--spinner-timer
          (run-with-timer 0 0.12 #'ende-ui--refresh-all-overlays))))

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
      (overlay-put ov 'tokens nil)
      (overlay-put ov 'speed nil)
      (overlay-put ov 'estimated nil)
      (overlay-put ov 'usage nil)
      (overlay-put ov 'activity-paragraph nil)
      (overlay-put ov 'reasoning-paragraph nil)
      (overlay-put ov 'badge-content (ende-ui--format-badge ov))
      (ende-ui--put-badge ov (ende-ui--spinner-frame 0 t))
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
          (:reasoning-effort (overlay-put ov 'reasoning-effort val))
          (:activity-title (overlay-put ov 'activity-title val))
          (:activity-status
           (overlay-put ov 'activity-paragraph
                        (ende-ui--activity-paragraph val)))
          (:reasoning-summary
           (overlay-put ov 'reasoning-paragraph
                        (ende-ui--focus-paragraph val)))
          (:usage (overlay-put ov 'usage val))
          (:tokens (overlay-put ov 'tokens val))
          (:speed  (overlay-put ov 'speed val))
          (:estimated (overlay-put ov 'estimated val)))))
    (overlay-put ov 'badge-content (ende-ui--format-badge ov))
    (let ((spinner (ende-ui--spinner-frame ende-ui--spinner-index
                                             ende-reduced-motion)))
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
          :model (plist-get event :model)
          :reasoning-effort
          (ende-provider-active-reasoning-effort (ende-job-profile job)))))
      (:agent-activity
       (when overlay
         (ende-ui-update-overlay
          overlay :activity-title (ende-job-activity-title job)
          :activity-status (ende-job-activity-status job))))
      (:reasoning-summary
       (when overlay
         (ende-ui-update-overlay
          overlay :reasoning-summary (ende-job-reasoning-summary-text job))))
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
