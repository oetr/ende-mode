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
  (when value
    (let* ((text (replace-regexp-in-string
                  "[[:cntrl:]]+" " " (format "%s" value)))
           (trimmed (string-trim text)))
      (unless (string-empty-p trimmed)
        (if (> (length trimmed) 80)
            (concat (substring trimmed 0 77) "...")
          trimmed)))))

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
  "Remove any deleted overlays or overlays on dead buffers."
  (setq deslop-ui--managed-overlays
        (delq nil (mapcar (lambda (ov)
                            (and (overlayp ov)
                                 (buffer-live-p (overlay-buffer ov))
                                 ov))
                           deslop-ui--managed-overlays)))
  (setq deslop-ui--spinning-overlays
        (delq nil (mapcar (lambda (ov)
                            (and (overlayp ov)
                                 (buffer-live-p (overlay-buffer ov))
                                 ov))
                          deslop-ui--spinning-overlays))))

(defun deslop-ui--prune-spinning-overlays ()
  "Remove deleted overlays from the small set that requires animation."
  (setq deslop-ui--spinning-overlays
        (delq nil (mapcar (lambda (ov)
                            (and (overlayp ov)
                                 (buffer-live-p (overlay-buffer ov))
                                 ov))
                          deslop-ui--spinning-overlays))))

(defun deslop-ui--refresh-all-overlays ()
  "Refresh all active overlays.  Queued items remain static; active items spin."
  (deslop-ui--prune-spinning-overlays)
  (if (null deslop-ui--spinning-overlays)
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
          (run-with-timer 0 0.1 #'deslop-ui--refresh-all-overlays))))

(defun deslop-ui--stop-spinner ()
  "Stop the overlay spinner timer."
  (when (timerp deslop-ui--spinner-timer)
    (cancel-timer deslop-ui--spinner-timer)
    (setq deslop-ui--spinner-timer nil)))

;;;; Review Highlight

(defconst deslop-ui--named-colors
  '(("white" . "#ffffff")
    ("black" . "#000000")
    ("gray" . "#bebebe")
    ("grey" . "#bebebe")
    ("lightgray" . "#d3d3d3")
    ("lightgrey" . "#d3d3d3")
    ("darkgray" . "#a9a9a9")
    ("darkgrey" . "#a9a9a9")
    ("red" . "#ff0000")
    ("green" . "#00ff00")
    ("blue" . "#0000ff")
    ("yellow" . "#ffff00")
    ("cyan" . "#00ffff")
    ("magenta" . "#ff00ff"))
  "Common theme color names mapped to approximate hex values.")

(defun deslop-ui--hex-components (color)
  "Return COLOR, a '#rrggbb' string, as a list of three 0-255 integers.
Return nil when COLOR is not a six-digit hex color."
  (when (and (stringp color)
             (string-match-p "^#[0-9a-fA-F]\\{6\\}$" color))
    (list (string-to-number (substring color 1 3) 16)
          (string-to-number (substring color 3 5) 16)
          (string-to-number (substring color 5 7) 16))))

(defun deslop-ui--hex-join (components)
  "Return COMPONENTS, a list of three 0-255 integers, as a '#rrggbb' string."
  (when (and (listp components) (= (length components) 3))
    (apply #'format "#%02x%02x%02x" components)))

(defun deslop-ui--blend-color (base fg blend)
  "Return BASE color shifted BLEND fraction toward FG, as a hex string.
Returns nil when BASE or FG is not a parseable hex color."
  (let* ((base-c (deslop-ui--hex-components base))
         (fg-c (deslop-ui--hex-components fg))
         (blend (max 0.0 (min 1.0 (float (or blend 0.0))))))
    (and base-c fg-c
         (deslop-ui--hex-join
          (cl-loop for b in base-c
                   for f in fg-c
                   collect (max 0 (min 255 (round (+ b (* (- f b) blend))))))))))

(defun deslop-ui--unspecified-p (value)
  "Return non-nil when VALUE is an unresolved `face-attribute' placeholder."
  (and (symbolp value)
       (string-prefix-p "unspecified" (symbol-name value))))

(defun deslop-ui--face-attr (spec attr)
  "Return attribute ATTR of face SPEC, or nil when it is not resolvable.
SPEC may be a face name, a face-spec list, or a list of such specifications."
  (cond
   ((symbolp spec)
    (condition-case nil
        (let ((value (face-attribute spec attr)))
          (unless (deslop-ui--unspecified-p value) value))
      (error nil)))
   ((listp spec)
    (if (keywordp (car spec))
        (let ((value (plist-get spec attr))
              (inherit (plist-get spec :inherit)))
          (or value
              (and inherit (deslop-ui--face-attr inherit attr))))
      (let ((found nil))
        (dolist (candidate spec)
          (unless found
            (setq found (deslop-ui--face-attr candidate attr))))
        found)))))

(defun deslop-ui--scale-channel (channel)
  "Normalize a color channel that may span 0-1 or 0-255 to 0-255."
  (if (> channel 1.0) channel (/ (* channel 255.0) 1.0)))

(defun deslop-ui--named-color-to-hex (name)
  "Return NAME as an approximate hex color, or nil when unknown.
Uses the display when available and falls back to a built-in name table."
  (and (stringp name)
       (or (let ((rgb
                  (condition-case nil
                      (and (display-color-p)
                           (fboundp 'x-color-to-rgb)
                           (x-color-to-rgb name))
                    (error nil))))
             (and rgb
                  (deslop-ui--hex-join
                   (list (round (deslop-ui--scale-channel (nth 0 rgb)))
                         (round (deslop-ui--scale-channel (nth 1 rgb)))
                         (round (deslop-ui--scale-channel (nth 2 rgb)))))))
           (cdr (assoc (downcase name) deslop-ui--named-colors)))))

(defun deslop-ui--light-theme-p ()
  "Return non-nil when the frame background is light."
  (let ((mode (condition-case nil
                  (if (fboundp 'frame-background-mode)
                      (frame-background-mode)
                    (background-mode))
                (error 'dark))))
    (eq mode 'light)))

(defun deslop-ui--color-or-fallback (color light-fallback dark-fallback)
  "Return COLOR as a hex string, using fallbacks when unresolvable.
LIGHT-FALLBACK and DARK-FALLBACK are hex strings chosen by `background-mode'."
  (cond
   ((and (stringp color) (deslop-ui--hex-components color))
    (downcase color))
   ((and (stringp color) (deslop-ui--named-color-to-hex color))
    (deslop-ui--named-color-to-hex color))
   ((deslop-ui--light-theme-p) light-fallback)
   (t dark-fallback)))

(defun deslop-ui--review-style (buf start)
  "Return the (FG . BG) hex pair for highlighting text inserted at START in BUF.
FG is the foreground of the face in effect immediately before START and BG
is that face's background shifted `deslop-review-highlight-blend' toward FG."
  (with-current-buffer buf
    (let* ((face (or (and (> start (point-min))
                          (get-char-property (1- start) 'face))
                     'default))
           (fg (or (deslop-ui--face-attr face :foreground)
                   (deslop-ui--face-attr 'default :foreground)))
           (bg (or (deslop-ui--face-attr face :background)
                   (deslop-ui--face-attr 'default :background)))
           (fg-hex (deslop-ui--color-or-fallback fg "#000000" "#e6e6e6"))
           (bg-hex (deslop-ui--color-or-fallback bg "#ffffff" "#1e1e1e")))
      (cons fg-hex (or (deslop-ui--blend-color bg-hex fg-hex
                                                deslop-review-highlight-blend)
                       bg-hex)))))

;;;; Overlay Lifecycle API

(defun deslop-ui--format-review-badge (client model)
  "Return a display-only review badge for CLIENT and MODEL."
  (let ((parts (delq nil (list (deslop-ui--display-label client)
                               (deslop-ui--display-label model)))))
    (when parts
      (propertize (format " [%s] " (mapconcat #'identity parts " "))
                 'face 'deslop-badge-face))))

(defun deslop-ui--review-overlays-in-buffer ()
  "Return all live review overlays in the current buffer."
  (save-restriction
    (widen)
    (cl-remove-if-not #'deslop-ui-review-overlay-p
                      (overlays-in (point-min) (point-max)))))

(defun deslop-ui--prevent-unreviewed-save ()
  "Prevent saving a buffer that still contains unapproved model output."
  (if (deslop-ui--review-overlays-in-buffer)
      (user-error "Approve or reject deslop output before saving")
    (remove-hook 'before-save-hook #'deslop-ui--prevent-unreviewed-save t)
    (remove-hook 'auto-save-hook #'deslop-ui--prevent-unreviewed-save t)))

(defun deslop-ui-create-review-overlay
    (buf beg end &optional model client original)
  "Mark generated text from BEG to END in BUF for later approval.
Edits inside the marked range remain marked; text inserted immediately before
or after the range remains outside the review mark.  ORIGINAL is restored when
the generated output is rejected.  When `deslop-review-highlight' is non-nil,
the inserted characters are tinted using the face in effect immediately
before BEG."
  (with-current-buffer buf
    (let ((ov (make-overlay beg end buf t nil)))
      (overlay-put ov 'deslop-review t)
      (overlay-put ov 'client client)
      (overlay-put ov 'model model)
      (overlay-put ov 'deslop-original-text (or original ""))
      (overlay-put ov 'priority 1)
      (overlay-put ov 'evaporate nil)
      (overlay-put ov 'before-string
                   (deslop-ui--format-review-badge client model))
      (when deslop-review-highlight
        (let ((style (deslop-ui--review-style buf beg)))
          (add-text-properties beg end
                               (list 'foreground-color (car style)
                                     'background-color (cdr style)))))
      (add-hook 'before-save-hook #'deslop-ui--prevent-unreviewed-save nil t)
      (add-hook 'auto-save-hook #'deslop-ui--prevent-unreviewed-save nil t)
      ov)))

(defun deslop-ui-review-overlay-p (overlay)
  "Return non-nil when OVERLAY marks output awaiting approval."
  (and (overlayp overlay)
       (overlay-buffer overlay)
       (overlay-get overlay 'deslop-review)))

(defun deslop-ui-delete-review-overlay (overlay &optional clean-highlight)
  "Remove review OVERLAY when it has been approved.
When CLEAN-HIGHLIGHT is non-nil, also strip the highlight text properties
from the marked range.  Approval uses this; rejection does not, because the
marked range is replaced by the restored original text first."
  (when (deslop-ui-review-overlay-p overlay)
    (let ((buffer (overlay-buffer overlay))
          (start (overlay-start overlay))
          (end (overlay-end overlay)))
      (delete-overlay overlay)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when clean-highlight
            (remove-text-properties start end '(foreground-color))
            (remove-text-properties start end '(background-color)))
          (unless (deslop-ui--review-overlays-in-buffer)
            (remove-hook 'before-save-hook
                         #'deslop-ui--prevent-unreviewed-save t)
            (remove-hook 'auto-save-hook
                         #'deslop-ui--prevent-unreviewed-save t)))))))

(defun deslop-ui-create-overlay (buf beg end &optional status)
  "Create an indicator overlay in BUF from BEG to END with STATUS."
  (with-current-buffer buf
    ;; Keep zero-width indicators fixed when text is inserted at their point;
    ;; Emacs still shifts them when text is inserted before them.
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
