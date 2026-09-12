;;; deslop-review.el --- Pending output review for deslop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools

;;; Commentary:

;; Review overlays, highlighting, save guards, and approval/rejection commands
;; for generated deslop output.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'deslop-core)

(defcustom deslop-review-highlight t
  "Non-nil when pending LLM output is tinted with a soft highlight.
The highlight fixes the foreground to the face in effect at the insertion
point and shifts its background slightly toward that foreground."
  :type 'boolean
  :group 'deslop)

(defcustom deslop-review-highlight-blend 0.15
  "Fraction of the foreground color blended into the review highlight background."
  :type 'number
  :group 'deslop)

;;;; Review Highlight & Color Helpers

(defun deslop-review--color-rgb (color)
  "Return COLOR as normalized RGB components, or nil when unresolved."
  (when (stringp color)
    (cond
     ((string-prefix-p "#" color)
      (when (string-match
             "\\`#\\([[:xdigit:]]\\{2\\}\\)\\([[:xdigit:]]\\{2\\}\\)\\([[:xdigit:]]\\{2\\}\\)\\'"
             color)
        (mapcar (lambda (component)
                  (/ (string-to-number component 16) 255.0))
                (list (match-string 1 color)
                      (match-string 2 color)
                      (match-string 3 color)))))
     (t
      (condition-case nil
          (color-name-to-rgb color)
        (error nil))))))

(defun deslop-review--rgb-to-hex (rgb)
  "Return normalized RGB values RGB as a two-digit hexadecimal color."
  (apply #'color-rgb-to-hex
         (append (mapcar (lambda (component)
                           (/ (round (* 255 component)) 255.0))
                         rgb)
                 '(2))))

(defun deslop-review--hex-components (color)
  "Return integer RGB components for COLOR, or nil when unresolved."
  (when (and (stringp color)
             (string-match
              "\\`#\\([[:xdigit:]]\\{2\\}\\)\\([[:xdigit:]]\\{2\\}\\)\\([[:xdigit:]]\\{2\\}\\)\\'"
              color))
    (mapcar (lambda (component) (string-to-number component 16))
            (list (match-string 1 color)
                  (match-string 2 color)
                  (match-string 3 color)))))

(defun deslop-review--hex-join (components)
  "Return COMPONENTS as a hexadecimal RGB string, or nil when invalid."
  (when (and (= (length components) 3)
             (cl-every (lambda (component)
                         (and (integerp component)
                              (<= 0 component 255)))
                       components))
    (deslop-review--rgb-to-hex
     (mapcar (lambda (component) (/ component 255.0)) components))))

(defun deslop-review--color-to-hex (color fallback)
  "Convert COLOR name or hex string to '#rrggbb', or return FALLBACK."
  (let ((rgb (deslop-review--color-rgb color)))
    (if rgb
        (deslop-review--rgb-to-hex rgb)
      fallback)))

(defun deslop-review--blend-color (base fg blend)
  "Return BASE color shifted BLEND fraction toward FG as a hex string."
  (let ((base-rgb (deslop-review--color-rgb base))
        (fg-rgb (deslop-review--color-rgb fg))
        (ratio (max 0.0 (min 1.0 (float (or blend 0.0))))))
    (when (and base-rgb fg-rgb)
      (deslop-review--rgb-to-hex
       (color-blend fg-rgb base-rgb ratio)))))

(defun deslop-review--unspecified-p (value)
  "Return non-nil when VALUE is an unresolved `face-attribute' placeholder."
  (and (symbolp value)
       (string-prefix-p "unspecified" (symbol-name value))))

(defun deslop-review--face-attr (spec attr)
  "Return attribute ATTR of face SPEC, or nil when it is not resolvable.
SPEC may be a face name, a face-spec list, or a list of such specifications."
  (cond
   ((symbolp spec)
    (condition-case nil
        (let ((value (face-attribute spec attr)))
          (unless (deslop-review--unspecified-p value) value))
      (error nil)))
   ((listp spec)
    (if (keywordp (car spec))
        (let ((value (plist-get spec attr))
              (inherit (plist-get spec :inherit)))
          (or value
              (and inherit (deslop-review--face-attr inherit attr))))
      (let ((found nil))
        (dolist (candidate spec)
          (unless found
            (setq found (deslop-review--face-attr candidate attr))))
        found)))))

(defun deslop-review--light-theme-p ()
  "Return non-nil when the frame background is light."
  (eq (frame-parameter nil 'background-mode) 'light))

(defun deslop-review--color-or-fallback (color light-fallback dark-fallback)
  "Return COLOR as a hex string, using fallbacks when unresolvable.
LIGHT-FALLBACK and DARK-FALLBACK are hex strings chosen by `background-mode'."
  (let ((fallback (if (deslop-review--light-theme-p)
                      light-fallback
                    dark-fallback)))
    (deslop-review--color-to-hex color fallback)))

(defun deslop-review--style (buffer start)
  "Return the (FG . BG) hex pair for highlighting text at START in BUFFER.
FG is the foreground of the face in effect immediately before START and BG
is that face's background shifted `deslop-review-highlight-blend' toward FG."
  (with-current-buffer buffer
    (let* ((face (or (and (> start (point-min))
                          (get-char-property (1- start) 'face))
                     'default))
           (fg (or (deslop-review--face-attr face :foreground)
                   (deslop-review--face-attr 'default :foreground)))
           (bg (or (deslop-review--face-attr face :background)
                   (deslop-review--face-attr 'default :background)))
           (fg-hex (deslop-review--color-or-fallback
                    fg "#000000" "#e6e6e6"))
           (bg-hex (deslop-review--color-or-fallback
                    bg "#ffffff" "#1e1e1e")))
      (cons fg-hex (or (deslop-review--blend-color
                        bg-hex fg-hex deslop-review-highlight-blend)
                       bg-hex)))))

;;;; Overlay Lifecycle

(defun deslop-review--format-badge (client model)
  "Return a display-only review badge for CLIENT and MODEL."
  (let ((parts (delq nil (list (deslop--bounded-single-line client 80 t)
                               (deslop--bounded-single-line model 80 t)))))
    (when parts
      (propertize (format " [%s] " (mapconcat #'identity parts " "))
                  'face 'deslop-badge-face))))

(defun deslop-review-overlay-p (overlay)
  "Return non-nil when OVERLAY marks output awaiting approval."
  (and (overlayp overlay)
       (overlay-buffer overlay)
       (overlay-get overlay 'deslop-review)))

(defun deslop-review--overlays-in-buffer ()
  "Return all live review overlays in the current buffer."
  (save-restriction
    (widen)
    (cl-remove-if-not #'deslop-review-overlay-p
                      (overlays-in (point-min) (point-max)))))

(defun deslop-review-overlaps-p (buffer start end)
  "Return non-nil when START..END in BUFFER intersects pending output.
A zero-width target is considered inside a review from its start up to, but
not including, its end."
  (with-current-buffer buffer
    (cl-some
     (lambda (overlay)
       (let ((review-start (overlay-start overlay))
             (review-end (overlay-end overlay)))
         (if (= start end)
             (and (<= review-start start) (< start review-end))
           (< (max start review-start) (min end review-end)))))
     (deslop-review--overlays-in-buffer))))

(defun deslop-review-assert-available (buffer start end)
  "Signal a user error if BUFFER's START..END intersects pending output."
  (when (deslop-review-overlaps-p buffer start end)
    (user-error
     "Approve or reject existing deslop output before starting another task here")))

(defun deslop-review--update-save-guards ()
  "Keep save hooks enabled exactly while review overlays remain."
  (if (deslop-review--overlays-in-buffer)
      (progn
        (add-hook 'before-save-hook #'deslop-review--prevent-save nil t)
        (add-hook 'auto-save-hook #'deslop-review--prevent-save nil t))
    (remove-hook 'before-save-hook #'deslop-review--prevent-save t)
    (remove-hook 'auto-save-hook #'deslop-review--prevent-save t)))

(defun deslop-review--prevent-save ()
  "Prevent saving a buffer that still contains unapproved model output."
  (if (deslop-review--overlays-in-buffer)
      (user-error "Approve or reject deslop output before saving")
    (deslop-review--update-save-guards)))

(defun deslop-review-create-overlay
    (buffer beg end &optional model client original)
  "Mark generated text from BEG to END in BUFFER for later approval.
Edits inside the marked range remain marked; text inserted immediately before
or after the range remains outside the review mark.  ORIGINAL is restored when
the generated output is rejected.  When `deslop-review-highlight' is non-nil,
the inserted characters are tinted using the review overlay's face."
  (with-current-buffer buffer
    (when (deslop-review-overlaps-p buffer beg end)
      (error "Generated output overlaps an existing review"))
    ;; Resolve fallible presentation values before creating persistent state.
    (let ((badge (deslop-review--format-badge client model))
          (style (and deslop-review-highlight
                      (deslop-review--style buffer beg)))
          (overlay nil)
          (complete nil))
      (unwind-protect
          (progn
            (setq overlay (make-overlay beg end buffer t nil))
            (overlay-put overlay 'deslop-review t)
            (overlay-put overlay 'client client)
            (overlay-put overlay 'model model)
            (overlay-put overlay 'deslop-original-text (or original ""))
            (overlay-put overlay 'priority 1)
            (overlay-put overlay 'evaporate nil)
            (overlay-put overlay 'before-string badge)
            (when style
              (overlay-put
               overlay 'face
               (list :foreground (car style)
                     :background (cdr style))))
            (deslop-review--update-save-guards)
            (setq complete t)
            overlay)
        (unless complete
          (when overlay
            (delete-overlay overlay))
          (deslop--ignore-errors #'deslop-review--update-save-guards))))))

(defun deslop-review-delete-overlay (overlay)
  "Remove review OVERLAY when it has been approved or rejected."
  (when (deslop-review-overlay-p overlay)
    (let ((buffer (overlay-buffer overlay)))
      (delete-overlay overlay)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (deslop-review--update-save-guards))))))

;;;; Interactive Commands

(defun deslop-review--overlays-in-target ()
  "Return active review overlays intersecting region or point."
  (let ((overlays
         (if (use-region-p)
             (overlays-in (region-beginning) (region-end))
           (apply #'append
                  (mapcar #'overlays-at
                          (delq nil
                                (list (point)
                                      (and (> (point) (point-min))
                                           (1- (point))))))))))
    (delete-dups (cl-remove-if-not #'deslop-review-overlay-p overlays))))

(defun deslop-approve-output ()
  "Approve generated output touching point or active region."
  (interactive)
  (let ((overlays (deslop-review--overlays-in-target)))
    (unless overlays
      (user-error "No deslop output awaiting approval here"))
    (dolist (overlay overlays)
      (deslop-review-delete-overlay overlay))
    (message "deslop: approved %d generated region%s"
             (length overlays) (if (= (length overlays) 1) "" "s"))))

(defun deslop-reject-output ()
  "Reject generated output touching point or active region."
  (interactive)
  (let ((overlays (deslop-review--overlays-in-target)))
    (unless overlays
      (user-error "No deslop output awaiting review here"))
    (setq overlays
          (sort overlays
                (lambda (left right)
                  (> (or (overlay-start left) 0)
                     (or (overlay-start right) 0)))))
    (let ((buffer (overlay-buffer (car overlays))))
      (unless (cl-every (lambda (overlay)
                          (eq (overlay-buffer overlay) buffer))
                        overlays)
        (error "Cannot reject reviews from multiple buffers at once"))
      (with-current-buffer buffer
        (barf-if-buffer-read-only)
        (save-restriction
          (widen)
          (atomic-change-group
            (save-excursion
              (dolist (overlay overlays)
                (unless (deslop-review-overlay-p overlay)
                  (error "Review overlay is no longer live"))
                (let ((start (overlay-start overlay))
                      (end (overlay-end overlay))
                      (original
                       (or (overlay-get overlay 'deslop-original-text) "")))
                  (delete-region start end)
                  (goto-char start)
                  (insert original))))))))
    (dolist (overlay overlays)
      (deslop-review-delete-overlay overlay))
    (message "deslop: rejected %d generated region%s"
             (length overlays) (if (= (length overlays) 1) "" "s"))))

(provide 'deslop-review)

;;; deslop-review.el ends here
