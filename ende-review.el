;;; ende-review.el --- Pending output review for ende -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools

;;; Commentary:

;; Review overlays, highlighting, and approval/rejection commands for generated
;; ende output.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'ende-core)

(defcustom ende-review-highlight t
  "Non-nil when pending LLM output is tinted with a soft highlight.
The highlight fixes the foreground to the face in effect at the insertion
point and shifts its background slightly toward that foreground."
  :type 'boolean
  :group 'ende)

(defcustom ende-review-highlight-blend 0.15
  "Fraction of the foreground color blended into the review highlight background."
  :type 'number
  :group 'ende)

(defcustom ende-review-show-badges nil
  "Non-nil to display badges on pending review regions.
This option is buffer-local.  Buffers without a local value use the global
default."
  :type 'boolean
  :local t
  :group 'ende)

(defcustom ende-review-conflict-policy 'confirm
  "How source-mutating jobs handle pending reviews they overlap.
`confirm' asks before approving the overlapping reviews, `approve' approves
them automatically, and `block' preserves the original blocking behavior.
Read-only operations such as `summarize' do not use this policy."
  :type '(choice (const :tag "Ask before approving" confirm)
                 (const :tag "Approve automatically" approve)
                 (const :tag "Block the job" block))
  :group 'ende)

(defvar ende-review--keymap (make-sparse-keymap)
  "Keymap active while point is on generated output awaiting review.")

(defun ende-review--install-keybindings ()
  "Install review bindings without hijacking ordinary editing keys."
  (dolist (key '("RET" "C-m" "DEL" "<backspace>" "C-d"))
    (define-key ende-review--keymap (kbd key) nil))
  (define-key ende-review--keymap (kbd "C-c C-c") #'ende-approve-output)
  (define-key ende-review--keymap (kbd "C-c C-k") #'ende-reject-output))

(ende-review--install-keybindings)

(defvar ende-review-change-functions nil
  "Functions called with a buffer whenever its pending reviews change.")

(defun ende-review--notify-change (buffer)
  "Notify observers that pending reviews changed in BUFFER."
  (dolist (function ende-review-change-functions)
    (ende-ignore-errors function buffer)))

;;;; Review Highlight & Color Helpers

(defun ende-review--color-rgb (color)
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

(defun ende-review--rgb-to-hex (rgb)
  "Return normalized RGB values RGB as a two-digit hexadecimal color."
  (apply #'color-rgb-to-hex
         (append (mapcar (lambda (component)
                           (/ (round (* 255 component)) 255.0))
                         rgb)
                 '(2))))

(defun ende-review--hex-components (color)
  "Return integer RGB components for COLOR, or nil when unresolved."
  (when (and (stringp color)
             (string-match
              "\\`#\\([[:xdigit:]]\\{2\\}\\)\\([[:xdigit:]]\\{2\\}\\)\\([[:xdigit:]]\\{2\\}\\)\\'"
              color))
    (mapcar (lambda (component) (string-to-number component 16))
            (list (match-string 1 color)
                  (match-string 2 color)
                  (match-string 3 color)))))

(defun ende-review--hex-join (components)
  "Return COMPONENTS as a hexadecimal RGB string, or nil when invalid."
  (when (and (= (length components) 3)
             (cl-every (lambda (component)
                         (and (integerp component)
                              (<= 0 component 255)))
                       components))
    (ende-review--rgb-to-hex
     (mapcar (lambda (component) (/ component 255.0)) components))))

(defun ende-review--color-to-hex (color fallback)
  "Convert COLOR name or hex string to '#rrggbb', or return FALLBACK."
  (let ((rgb (ende-review--color-rgb color)))
    (if rgb
        (ende-review--rgb-to-hex rgb)
      fallback)))

(defun ende-review--blend-color (base fg blend)
  "Return BASE color shifted BLEND fraction toward FG as a hex string."
  (let ((base-rgb (ende-review--color-rgb base))
        (fg-rgb (ende-review--color-rgb fg))
        (ratio (max 0.0 (min 1.0 (float (or blend 0.0))))))
    (when (and base-rgb fg-rgb)
      (ende-review--rgb-to-hex
       (color-blend fg-rgb base-rgb ratio)))))

(defun ende-review--unspecified-p (value)
  "Return non-nil when VALUE is an unresolved `face-attribute' placeholder."
  (and (symbolp value)
       (string-prefix-p "unspecified" (symbol-name value))))

(defun ende-review--face-attr (spec attr)
  "Return attribute ATTR of face SPEC, or nil when it is not resolvable.
SPEC may be a face name, a face-spec list, or a list of such specifications."
  (cond
   ((symbolp spec)
    (condition-case nil
        (let ((value (face-attribute spec attr)))
          (unless (ende-review--unspecified-p value) value))
      (error nil)))
   ((listp spec)
    (if (keywordp (car spec))
        (let ((value (plist-get spec attr))
              (inherit (plist-get spec :inherit)))
          (or value
              (and inherit (ende-review--face-attr inherit attr))))
      (let ((found nil))
        (dolist (candidate spec)
          (unless found
            (setq found (ende-review--face-attr candidate attr))))
        found)))))

(defun ende-review--light-theme-p ()
  "Return non-nil when the frame background is light."
  (eq (frame-parameter nil 'background-mode) 'light))

(defun ende-review--color-or-fallback (color light-fallback dark-fallback)
  "Return COLOR as a hex string, using fallbacks when unresolvable.
LIGHT-FALLBACK and DARK-FALLBACK are hex strings chosen by `background-mode'."
  (let ((fallback (if (ende-review--light-theme-p)
                      light-fallback
                    dark-fallback)))
    (ende-review--color-to-hex color fallback)))

(defun ende-review--style (buffer start)
  "Return the (FG . BG) hex pair for highlighting text at START in BUFFER.
FG is the foreground of the face in effect immediately before START and BG
is that face's background shifted `ende-review-highlight-blend' toward FG."
  (with-current-buffer buffer
    (let* ((face (or (and (> start (point-min))
                          (get-char-property (1- start) 'face))
                     'default))
           (fg (or (ende-review--face-attr face :foreground)
                   (ende-review--face-attr 'default :foreground)))
           (bg (or (ende-review--face-attr face :background)
                   (ende-review--face-attr 'default :background)))
           (fg-hex (ende-review--color-or-fallback
                    fg "#000000" "#e6e6e6"))
           (bg-hex (ende-review--color-or-fallback
                    bg "#ffffff" "#1e1e1e")))
      (cons fg-hex (or (ende-review--blend-color
                        bg-hex fg-hex ende-review-highlight-blend)
                       bg-hex)))))

;;;; Overlay Lifecycle

(defun ende-review--format-badge (client model)
  "Return a display-only review badge for CLIENT and MODEL."
  (let ((parts (delq nil (list (ende-bounded-single-line client 80 t)
                               (ende-bounded-single-line model 80 t)))))
    (when parts
      (propertize (format " [%s] " (mapconcat #'identity parts " "))
                  'face 'ende-badge-face))))

(defun ende-review--refresh-badges (&optional buffer)
  "Refresh review badges in BUFFER, or the current buffer."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (dolist (overlay (ende-review--overlays-in-buffer))
          (overlay-put
           overlay 'before-string
           (and ende-review-show-badges
                (ende-review--format-badge
                 (overlay-get overlay 'client)
                 (overlay-get overlay 'model)))))))))

(defun ende-review--refresh-inherited-badges ()
  "Refresh review badges in buffers using the global visibility setting."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (unless (local-variable-p 'ende-review-show-badges)
        (ende-review--refresh-badges)))))

(defun ende-review-overlay-p (overlay)
  "Return non-nil when OVERLAY marks output awaiting approval."
  (and (overlayp overlay)
       (overlay-buffer overlay)
       (overlay-get overlay 'ende-review)))

(defun ende-review--overlays-in-buffer ()
  "Return all live review overlays in the current buffer."
  (save-restriction
    (widen)
    (cl-remove-if-not #'ende-review-overlay-p
                      (overlays-in (point-min) (point-max)))))

(defun ende-review--overlay-overlaps-p (overlay start end)
  "Return non-nil when OVERLAY intersects START..END."
  (let ((review-start (overlay-start overlay))
        (review-end (overlay-end overlay)))
    (and review-start review-end
         (if (= start end)
             (and (<= review-start start) (< start review-end))
           (< (max start review-start) (min end review-end))))))

(defun ende-review--overlays-overlapping (buffer start end)
  "Return pending reviews in BUFFER intersecting START..END."
  (with-current-buffer buffer
    (cl-remove-if-not
     (lambda (overlay)
       (ende-review--overlay-overlaps-p overlay start end))
     (ende-review--overlays-in-buffer))))

(defun ende-review-overlaps-p (buffer start end)
  "Return non-nil when START..END in BUFFER intersects pending output.
A zero-width target is considered inside a review from its start up to, but
not including, its end."
  (not (null (ende-review--overlays-overlapping buffer start end))))

(defun ende-review-assert-available (buffer start end)
  "Signal a user error if BUFFER's START..END intersects pending output."
  (when (ende-review-overlaps-p buffer start end)
    (user-error
     "Approve or reject existing ende output before starting another task here")))

(defun ende-review-create-overlay
    (buffer beg end &optional model client original)
  "Mark generated text from BEG to END in BUFFER for later approval.
Edits inside the marked range remain marked; text inserted immediately before
or after the range remains outside the review mark.  ORIGINAL is restored when
the generated output is rejected.  When `ende-review-highlight' is non-nil,
the inserted characters are tinted using the review overlay's face."
  (with-current-buffer buffer
    (when (ende-review-overlaps-p buffer beg end)
      (error "Generated output overlaps an existing review"))
    ;; Resolve fallible presentation values before creating persistent state.
    (let ((badge (and ende-review-show-badges
                      (ende-review--format-badge client model)))
          (style (and ende-review-highlight
                      (ende-review--style buffer beg)))
          (overlay nil)
          (complete nil))
      (unwind-protect
          (progn
            (setq overlay (make-overlay beg end buffer t nil))
            (overlay-put overlay 'ende-review t)
            (overlay-put overlay 'client client)
            (overlay-put overlay 'model model)
            (overlay-put overlay 'ende-original-text (or original ""))
            (overlay-put overlay 'priority 1)
            (overlay-put overlay 'evaporate nil)
            (overlay-put overlay 'keymap ende-review--keymap)
            (overlay-put overlay 'before-string badge)
            (when style
              (overlay-put
               overlay 'face
               (list :foreground (car style)
                     :background (cdr style))))
            (setq complete t)
            overlay)
        (unless complete
          (when overlay
            (delete-overlay overlay))))
      (when complete
        (ende-review--notify-change buffer))
      overlay)))

(defun ende-review-delete-overlay (overlay)
  "Remove review OVERLAY when it has been approved or rejected."
  (when (ende-review-overlay-p overlay)
    (let ((buffer (overlay-buffer overlay)))
      (delete-overlay overlay)
      (ende-review--notify-change buffer))))

(defun ende-review-resolve-conflicts (buffer start end)
  "Resolve pending reviews overlapping START..END in BUFFER.
The behavior is controlled by `ende-review-conflict-policy'.  Return the
overlays that were resolved, or nil when there was no conflict."
  (let ((overlays (ende-review--overlays-overlapping buffer start end)))
    (when overlays
      (let ((approve
             (pcase ende-review-conflict-policy
               ('block
                (ende-review-assert-available buffer start end))
               ('approve t)
               ('confirm
                (or (yes-or-no-p
                     (format
                      "Approve %d overlapping ende review%s and continue? "
                      (length overlays)
                      (if (= (length overlays) 1) "" "s")))
                    (user-error
                     "ende: job cancelled; pending reviews were not changed")))
               (_
                (error "unknown ende review conflict policy: %s"
                       ende-review-conflict-policy)))))
        (when approve
          (dolist (overlay overlays)
            (ende-review-delete-overlay overlay))
          (message "ende: approved %d overlapping generated region%s"
                   (length overlays)
                   (if (= (length overlays) 1) "" "s")))))
    overlays))

;;;; Interactive Commands

(defun ende-toggle-review-badges (&optional global)
  "Toggle visibility of pending review badges.
Without a prefix argument, toggle badges in the current buffer and keep that
choice local to it.  With a prefix argument, toggle the global default for
buffers without an explicit local value."
  (interactive "P")
  (if global
      (let ((show (not (default-value 'ende-review-show-badges))))
        (set-default 'ende-review-show-badges show)
        (ende-review--refresh-inherited-badges)
        (message "ende: review badges %s by default"
                 (if show "on" "off")))
    (setq-local ende-review-show-badges
                (not ende-review-show-badges))
    (ende-review--refresh-badges)
    (message "ende: review badges %s in %s"
             (if ende-review-show-badges "on" "off")
             (buffer-name))))

(defun ende-review--overlays-in-target ()
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
    (delete-dups (cl-remove-if-not #'ende-review-overlay-p overlays))))

(defun ende-approve-output ()
  "Approve generated output touching point or active region."
  (interactive)
  (let ((overlays (ende-review--overlays-in-target)))
    (unless overlays
      (user-error "No ende output awaiting approval here"))
    (dolist (overlay overlays)
      (ende-review-delete-overlay overlay))
    (message "ende: approved %d generated region%s"
             (length overlays) (if (= (length overlays) 1) "" "s"))))

(defun ende-review--reject-overlays (overlays)
  "Reject every review overlay in OVERLAYS atomically.
Add each rejected generated region separately to the kill ring."
  (setq overlays
        (sort overlays
              (lambda (left right)
                (> (or (overlay-start left) 0)
                   (or (overlay-start right) 0)))))
  (let ((buffer (overlay-buffer (car overlays)))
        (inhibit-read-only nil)
        (rejected-texts nil))
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
              (unless (ende-review-overlay-p overlay)
                (error "Review overlay is no longer live"))
              (let ((start (overlay-start overlay))
                    (end (overlay-end overlay))
                    (rejected (filter-buffer-substring
                               (overlay-start overlay) (overlay-end overlay)))
                    (original
                     (or (overlay-get overlay 'ende-original-text) "")))
                (push rejected rejected-texts)
                (delete-region start end)
                (goto-char start)
                (insert original)))))))
    (dolist (overlay overlays)
      (ende-review-delete-overlay overlay))
    (dolist (text (nreverse rejected-texts))
      (kill-new text))
    overlays))

(defun ende-reject-output ()
  "Reject generated output touching point or active region."
  (interactive)
  (let ((overlays (ende-review--overlays-in-target)))
    (unless overlays
      (user-error "No ende output awaiting review here"))
    (ende-review--reject-overlays overlays)
    (message "ende: rejected %d generated region%s"
             (length overlays) (if (= (length overlays) 1) "" "s"))))

(defun ende-approve-all-output (&optional buffer)
  "Approve every generated review overlay in BUFFER."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (unless (buffer-live-p buffer)
      (user-error "Source buffer no longer exists"))
    (let ((overlays (with-current-buffer buffer
                      (ende-review--overlays-in-buffer))))
      (unless overlays
        (user-error "No ende output awaiting approval in this buffer"))
      (dolist (overlay overlays)
        (ende-review-delete-overlay overlay))
      (message "ende: approved all %d generated regions" (length overlays)))))

(defun ende-reject-all-output (&optional buffer)
  "Reject every generated review overlay in BUFFER."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (unless (buffer-live-p buffer)
      (user-error "Source buffer no longer exists"))
    (let ((overlays (with-current-buffer buffer
                      (ende-review--overlays-in-buffer))))
      (unless overlays
        (user-error "No ende output awaiting rejection in this buffer"))
      (ende-review--reject-overlays overlays)
      (message "ende: rejected all %d generated regions" (length overlays)))))

(provide 'ende-review)

;;; ende-review.el ends here
