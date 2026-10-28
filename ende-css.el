;;; ende-css.el --- Static SVG stylesheet validation -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'css-mode)

(defconst ende-css-properties
  '("alignment-baseline" "baseline-shift" "clip" "clip-path" "clip-rule"
    "color" "color-interpolation" "color-interpolation-filters" "color-rendering"
    "cx" "cy" "d" "direction" "display" "dominant-baseline"
    "fill" "fill-opacity" "fill-rule" "filter" "flood-color" "flood-opacity"
    "font" "font-family" "font-feature-settings" "font-kerning" "font-size"
    "font-size-adjust" "font-stretch" "font-style" "font-variant"
    "font-variant-caps" "font-variant-ligatures" "font-variant-numeric"
    "font-weight" "height" "image-rendering" "isolation" "letter-spacing"
    "lighting-color" "line-height" "marker" "marker-end" "marker-mid"
    "marker-start" "mask" "mask-type" "mix-blend-mode" "opacity" "overflow"
    "paint-order" "pointer-events" "r" "rx" "ry" "shape-rendering"
    "stop-color" "stop-opacity" "stroke" "stroke-dasharray" "stroke-dashoffset"
    "stroke-linecap" "stroke-linejoin" "stroke-miterlimit" "stroke-opacity"
    "stroke-width" "text-anchor" "text-decoration" "text-decoration-color"
    "text-decoration-style" "text-rendering" "text-transform" "transform"
    "transform-box" "transform-origin" "unicode-bidi" "vector-effect"
    "visibility" "white-space" "width" "word-spacing" "writing-mode" "x" "y")
  "Presentation properties accepted in static SVG CSS.")

(defconst ende-css--functions
  '("rgb" "rgba" "hsl" "hsla" "hwb" "lab" "lch" "oklab" "oklch"
    "color" "color-mix" "light-dark" "calc" "min" "max" "clamp"
    "matrix" "matrix3d" "translate" "translatex" "translatey" "translatez"
    "translate3d" "scale" "scalex" "scaley" "scalez" "scale3d"
    "rotate" "rotatex" "rotatey" "rotatez" "rotate3d" "skew" "skewx" "skewy"
    "perspective" "blur" "brightness" "contrast" "drop-shadow" "grayscale"
    "hue-rotate" "invert" "opacity" "saturate" "sepia"
    "rect" "inset" "circle" "ellipse" "polygon" "path"))

(defun ende-css-local-reference-p (value)
  "Return non-nil when VALUE is a simple same-document fragment."
  (string-match-p "\\`#[[:alnum:]_.-]+\\'" value))

(defun ende-css--skip ()
  "Skip CSS whitespace and complete comments, returning the next character."
  (skip-chars-forward " \t\r\n\f")
  (while (looking-at "/\\*")
    (unless (forward-comment 1)
      (error "Unterminated SVG CSS comment"))
    (skip-chars-forward " \t\r\n\f"))
  (char-after))

(defun ende-css--take (character)
  "Consume the required CSS CHARACTER."
  (unless (eq (char-after) character)
    (error "Expected %c in SVG CSS" character))
  (forward-char))

(defun ende-css--escape (&optional string)
  "Read a CSS escape, allowing line continuation inside STRING."
  (ende-css--take ?\\)
  (cond
   ((eobp) (error "Incomplete SVG CSS escape"))
   ((looking-at "[[:xdigit:]]\\{1,6\\}")
    (let ((code (string-to-number (match-string 0) 16)))
      (goto-char (match-end 0))
      (when (looking-at "\r\n\\|[ \t\r\n\f]")
        (goto-char (match-end 0)))
      (unless (and (<= 1 code #x10ffff) (not (<= #xd800 code #xdfff)))
        (error "Invalid SVG CSS character escape"))
      (char-to-string code)))
   ((looking-at "\r\n\\|[\r\n\f]")
    (unless string (error "Invalid SVG CSS identifier escape"))
    (goto-char (match-end 0))
    "")
   (t (prog1 (char-to-string (char-after)) (forward-char)))))

(defun ende-css--identifier ()
  "Read a CSS name, decoding its escapes."
  (let (pieces)
    (while (cond
            ((looking-at "[-_[:alnum:]]+\\|[^\0-\177]+")
             (push (match-string 0) pieces)
             (goto-char (match-end 0))
             t)
            ((eq (char-after) ?\\)
             (push (ende-css--escape) pieces)
             t)))
    (unless pieces (error "Expected an SVG CSS identifier"))
    (apply #'concat (nreverse pieces))))

(defun ende-css--string ()
  "Read a quoted CSS string, decoding its escapes."
  (let ((end (scan-sexps (point) 1)) pieces)
    (unless end (error "Unterminated SVG CSS string"))
    (forward-char)
    (while (< (point) (1- end))
      (cond
       ((eq (char-after) ?\\) (push (ende-css--escape t) pieces))
       ((memq (char-after) '(?\n ?\r ?\f))
        (error "Newline in SVG CSS string"))
       (t
        (let ((start (point)))
          (while (and (< (point) (1- end))
                      (not (memq (char-after) '(?\\ ?\n ?\r ?\f))))
            (forward-char))
          (push (buffer-substring-no-properties start (point)) pieces)))))
    (goto-char end)
    (apply #'concat (nreverse pieces))))

(defun ende-css--url ()
  "Read a url() argument and require a same-document reference."
  (skip-chars-forward " \t\r\n\f")
  (let ((reference
         (if (memq (char-after) '(?\" ?\'))
             (ende-css--string)
           (let (pieces)
             (while (not (memq (char-after) '(nil ?\) ?\s ?\t ?\r ?\n ?\f)))
               (push (if (eq (char-after) ?\\)
                         (ende-css--escape)
                       (prog1 (char-to-string (char-after)) (forward-char)))
                     pieces))
             (apply #'concat (nreverse pieces))))))
    (skip-chars-forward " \t\r\n\f")
    (ende-css--take ?\))
    (unless (ende-css-local-reference-p reference)
      (error "External SVG CSS resource"))))

(defun ende-css-validate (text &optional context)
  "Validate static SVG CSS TEXT in CONTEXT.
CONTEXT is `stylesheet', `value', or the default `declarations'."
  (when (> (string-bytes text) (* 256 1024))
    (error "SVG CSS exceeds the size limit"))
  (with-temp-buffer
    (set-syntax-table css-mode-syntax-table)
    (insert text)
    (goto-char (point-min))
    (when (re-search-forward "[\0-\10\13\16-\37\177]" nil t)
      (error "Invalid SVG CSS control character"))
    (goto-char (point-min))
    (let ((tokens 0))
      (cl-labels
          ((tick (depth)
             (when (or (> (cl-incf tokens) 20000) (> depth 32))
               (error "SVG CSS exceeds the complexity limit")))
           (name-p () (looking-at-p "[-_[:alnum:]\\\\]\\|[^\0-\177]"))
           (selector (end depth)
             (let ((seen nil))
               (while (not (eq (ende-css--skip) end))
                 (tick depth)
                 (setq seen t)
                 (cond
                  ((memq (char-after) '(?\" ?\')) (ende-css--string))
                  ((name-p) (ende-css--identifier))
                  ((memq (char-after) '(?\[ ?\())
                   (let ((close (if (eq (char-after) ?\[) ?\] ?\))))
                     (forward-char)
                     (selector close (1+ depth))))
                  ((memq (char-after) '(?# ?. ?* ?> ?, ?+ ?~ ?= ?| ?^ ?$ ?: ?-))
                   (forward-char))
                  (t (error "Unsupported SVG CSS selector"))))
               (unless seen (error "Empty SVG CSS selector"))
               (ende-css--take end)))
           (value (ends depth)
             (let ((seen nil))
               (while (not (memq (ende-css--skip) ends))
                 (tick depth)
                 (setq seen t)
                 (cond
                  ((memq (char-after) '(?\" ?\')) (ende-css--string))
                  ((name-p)
                   (let ((name (downcase (ende-css--identifier))))
                     (when (eq (ende-css--skip) ?\()
                       (forward-char)
                       (if (equal name "url")
                           (ende-css--url)
                         (unless (member name ende-css--functions)
                           (error "Unsupported SVG CSS function: %s" name))
                         (value '(?\)) (1+ depth))
                         (ende-css--take ?\))))))
                  ((eq (char-after) ?\()
                   (forward-char)
                   (value '(?\)) (1+ depth))
                   (ende-css--take ?\)))
                  ((memq (char-after) '(?# ?% ?, ?. ?+ ?- ?* ?/))
                   (forward-char))
                  (t (error "Unsupported SVG CSS value"))))
               (unless seen (error "Empty SVG CSS value"))))
           (declarations (end depth)
             (while (not (eq (ende-css--skip) end))
               (tick depth)
               (if (eq (char-after) ?\;)
                   (forward-char)
                 (let ((property (downcase (ende-css--identifier))))
                   (unless (member property ende-css-properties)
                     (error "Unsupported SVG CSS property: %s" property)))
                 (ende-css--skip)
                 (ende-css--take ?:)
                 (value '(nil ?\; ?\} ?!) depth)
                 (when (eq (char-after) ?!)
                   (forward-char)
                   (ende-css--skip)
                   (unless (equal (downcase (ende-css--identifier)) "important")
                     (error "Unsupported SVG CSS priority"))
                   (ende-css--skip))
                 (unless (or (eq (char-after) end) (eq (char-after) ?\;))
                   (error "Unexpected SVG CSS declaration terminator"))))
             (when end (ende-css--take end)))
           (stylesheet (end depth)
             (while (not (eq (ende-css--skip) end))
               (tick depth)
               (if (eq (char-after) ?@)
                   (progn
                     (forward-char)
                     (unless (member (downcase (ende-css--identifier))
                                     '("media" "supports"))
                       (error "Unsupported SVG CSS at-rule"))
                     (selector ?\{ (1+ depth))
                     (stylesheet ?\} (1+ depth)))
                 (selector ?\{ depth)
                 (declarations ?\} depth)))
             (when end (ende-css--take end))))
        (pcase context
          ('stylesheet (stylesheet nil 0))
          ('value (value '(nil) 0))
          ((or 'nil 'declarations) (declarations nil 0))
          (_ (error "Unknown SVG CSS context: %s" context)))))
    t))

(provide 'ende-css)

;;; ende-css.el ends here
