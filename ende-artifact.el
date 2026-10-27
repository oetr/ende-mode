;;; ende-artifact.el --- Org conversation image artifacts -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-element)
(require 'image)
(require 'xml)

(defgroup ende-artifact nil "Conversation images." :group 'ende)

(defcustom ende-artifact-preview-width 440
  "Maximum inline preview width in pixels."
  :type 'integer :group 'ende-artifact)

(defcustom ende-artifact-preview-height 360
  "Maximum inline preview height in pixels."
  :type 'integer :group 'ende-artifact)

(defun ende-artifact--valid-name-p (name)
  "Return non-nil for a simple artifact NAME."
  (and (stringp name)
       (string-match-p "\\`[[:alnum:]_][[:alnum:]_.-]*\\'" name)
       (equal name (file-name-nondirectory name))
       (not (member name '("." "..")))))

(defun ende-artifact-base ()
  "Return the document's local artifact base, independent of project roots."
  (let ((base ".ende-artifacts/"))
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (let ((end (save-excursion
                     (if (re-search-forward org-heading-regexp nil t)
                         (line-beginning-position) (point-max))))
              (case-fold-search t))
          (while (re-search-forward
                  "^[ \t]*#\\+ENDE_ARTIFACTS_BASE_PATH:[ \t]*\\(.*\\)$" end t)
            (let ((value (string-trim (match-string-no-properties 1)))
                  (begin (match-beginning 0)))
              (when (eq (org-element-type (org-element-at-point begin)) 'keyword)
                (setq base value)))))))
    (when (or (string-empty-p base) (file-remote-p base))
      (error "Artifact base must be a nonempty local directory"))
    (unless (or (file-name-absolute-p base) buffer-file-name)
      (error "Relative artifact storage requires an Org buffer visiting a file"))
    (when (and buffer-file-name (file-remote-p buffer-file-name)
               (not (file-name-absolute-p base)))
      (error "Relative artifact storage is unavailable for remote Org files"))
    (file-name-as-directory
     (expand-file-name base (and buffer-file-name
                                 (file-name-directory buffer-file-name))))))

(defun ende-artifact-directory (base chat-id &optional create)
  "Resolve CHAT-ID under BASE, optionally CREATE directories.
Check containment even when intermediate directories are symlinks."
  (unless (and (stringp base) (file-name-absolute-p base)
               (not (file-remote-p base))
               (ende-artifact--valid-name-p chat-id))
    (error "Artifact storage requires a local base and a valid ENDE_CHAT_ID"))
  (let* ((base (file-name-as-directory (file-truename base)))
         (directory (expand-file-name chat-id base)))
    (unless (string-prefix-p base (file-truename directory))
      (error "Conversation artifact directory escapes the configured base"))
    (when create
      (make-directory directory t))
    (file-name-as-directory directory)))

(defun ende-artifact--read-bytes (path)
  "Read PATH without character conversion."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defun ende-artifact--svg (bytes)
  "Parse and validate static SVG BYTES, rejecting unsupported active content."
  (with-temp-buffer
    (insert (decode-coding-string bytes 'utf-8))
    (goto-char (point-min))
    (when (re-search-forward "<!\\(?:DOCTYPE\\|ENTITY\\)\\|<\\?" nil t)
      (error "SVG declarations, entities and processing instructions are unsupported"))
    (let* ((nodes (xml-parse-region (point-min) (point-max)))
           (root (car nodes))
           (allowed '(svg g defs title desc rect circle ellipse line polyline
                          polygon path text tspan linearGradient radialGradient
                          stop clipPath mask marker use symbol)))
      (unless (and (= (length nodes) 1) (eq (car-safe root) 'svg)
                   (equal (cdr (assq 'xmlns (cadr root)))
                          "http://www.w3.org/2000/svg"))
        (error "Expected one complete SVG document with the SVG namespace"))
      (cl-labels
          ((walk (node)
             (when (consp node)
               (unless (memq (car node) allowed)
                 (error "Unsupported SVG element: %s" (car node)))
               (dolist (attribute (cadr node))
                 (let ((key (symbol-name (car attribute)))
                       (value (cdr attribute))
                       (case-fold-search t))
                   (unless (member key
                                   '("xmlns" "xmlns:xlink" "id" "class" "viewBox"
                                     "width" "height" "x" "y" "x1" "y1" "x2" "y2"
                                     "cx" "cy" "r" "rx" "ry" "d" "points" "dx" "dy"
                                     "fill" "fill-opacity" "fill-rule" "stroke"
                                     "stroke-width" "stroke-opacity" "stroke-linecap"
                                     "stroke-linejoin" "stroke-dasharray" "stroke-dashoffset"
                                     "opacity" "transform" "font-family" "font-size"
                                     "font-weight" "font-style" "text-anchor"
                                     "dominant-baseline" "alignment-baseline" "xml:space"
                                     "style" "offset" "stop-color" "stop-opacity"
                                     "gradientUnits" "gradientTransform" "spreadMethod"
                                     "fx" "fy" "clip-path" "clip-rule" "clipPathUnits"
                                     "mask" "maskUnits" "maskContentUnits" "marker-start"
                                     "marker-mid" "marker-end" "markerWidth" "markerHeight"
                                     "refX" "refY" "orient" "markerUnits" "preserveAspectRatio"
                                     "href" "xlink:href" "color" "vector-effect"))
                     (error "Unsupported SVG attribute: %s" key))
                   (when (and (equal key "style") (string-match-p "[/<>]" value))
                     (error "Unsupported SVG CSS syntax"))
                   (when (or (string-prefix-p "on" (downcase key))
                             (member key '("xml:base" "src"))
                             (and (string-match-p "\\(?:^\\|:\\)href\\'" key)
                                  (not (string-match-p
                                        "\\`#[[:alnum:]_.-]+\\'" value)))
                             (string-match-p "@\\|\\\\" value)
                             (and (not (member key '("xmlns" "xmlns:xlink")))
                                  (string-match-p
                                   "\\(?:https?:\\|file:\\|data:\\|ftp:\\|//\\)" value)))
                     (error "Unsafe SVG attribute: %s" key))
                   (let ((start 0) (case-fold-search t))
                     (while (string-match "url[ \t\n]*(\\([^)]*\\))" value start)
                       (let ((reference (match-string 1 value))
                             (next (match-end 0)))
                         (unless (string-match-p "\\`#[[:alnum:]_.-]+\\'"
                                                 (string-trim reference))
                           (error "External SVG CSS resource"))
                         (setq start next))))))
               (mapc #'walk (cddr node)))))
        (walk root))
      root)))

(defun ende-artifact-image-info (path &optional expected)
  "Validate image PATH, optionally requiring EXPECTED media type.
Return a path-based image part; no encoded image data is retained."
  (unless (and (not (file-remote-p path))
               (file-regular-p path) (file-readable-p path))
    (error "Image is not a readable regular local file"))
  (let* ((bytes (ende-artifact--read-bytes path))
         (extension (downcase (or (file-name-extension path) "")))
         (type (cond ((equal extension "svg") 'svg)
                     ((equal extension "png") 'png)
                     ((member extension '("jpg" "jpeg")) 'jpeg)))
         (media (cdr (assq type '((png . "image/png") (jpeg . "image/jpeg")
                                  (svg . "image/svg+xml")))))
         dimensions)
    (unless (and media (or (null expected) (equal expected media)))
      (error "Unsupported image format or mismatched media type"))
    (if (eq type 'svg)
        (ende-artifact--svg bytes)
      (unless (eq (image-type-from-data bytes) type)
        (error "Image content does not match its format")))
    (setq dimensions
          (if (display-graphic-p)
              (image-size (create-image bytes type t) t)
            (if (eq type 'svg)
                (let* ((attributes (cadr (ende-artifact--svg bytes)))
                       (view (split-string (or (cdr (assq 'viewBox attributes))
                                               "0 0 300 150") "[, \t\n]+" t)))
                  (cons (string-to-number (or (cdr (assq 'width attributes))
                                             (nth 2 view) "300"))
                        (string-to-number (or (cdr (assq 'height attributes))
                                             (nth 3 view) "150"))))
              (unless (executable-find "identify")
                (error "Image validation requires a graphical Emacs or ImageMagick identify"))
              (with-temp-buffer
                (unless (zerop (call-process "identify" nil (list t nil) nil
                                            "-regard-warnings" "-format" "%w %h"
                                            path))
                  (error "Malformed raster image"))
                (let ((numbers (split-string (buffer-string))))
                  (cons (string-to-number (or (car numbers) "0"))
                        (string-to-number (or (cadr numbers) "0"))))))))
    (unless (and dimensions (> (car dimensions) 0) (> (cdr dimensions) 0))
      (error "Malformed image content"))
    (list :type :image :path path :media-type media
          :width (car dimensions) :height (cdr dimensions)
          :bytes (length bytes))))

(defun ende-artifact-resolve (base chat-id name)
  "Resolve and validate NAME using only BASE and CHAT-ID.
Errors identify the reference and expected directory, never link targets."
  (let ((directory (expand-file-name (or chat-id "<missing-id>") base)))
    (condition-case failure
        (progn
          (unless (ende-artifact--valid-name-p name)
            (error "invalid artifact filename"))
          (setq directory (ende-artifact-directory base chat-id))
          (let ((path (expand-file-name name directory)))
            (when (file-symlink-p path)
              (error "final-component symlinks are not allowed"))
            (unless (string-prefix-p (file-name-as-directory
                                      (file-truename directory))
                                     (file-truename path))
              (error "artifact escapes its conversation directory"))
            (append (ende-artifact-image-info path)
                    (list :artifact-base base :chat-id chat-id :name name))))
      (error (error "Artifact %s in conversation %s, expected directory %s: %s"
                    name chat-id directory (error-message-string failure))))))

(defun ende-artifact-store (base chat-id bytes media)
  "Exclusively store validated BYTES of MEDIA under BASE and CHAT-ID.
Return the chosen filename.  Insert no Org reference until writing succeeds."
  (let* ((extension (cdr (assoc media '(("image/png" . "png")
                                       ("image/jpeg" . "jpg")
                                       ("image/svg+xml" . "svg")))))
         (directory (ende-artifact-directory base chat-id t))
         (index 1) name path written complete)
    (unless extension (error "Unsupported generated image media type"))
    (while (progn
             (setq name (format "image-%03d.%s" index extension)
                   path (expand-file-name name directory))
             (cl-incf index)
             (or (file-exists-p path) (file-symlink-p path))))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'no-conversion))
            (write-region bytes nil path nil 'silent nil 'excl))
          (setq written t)
          (ende-artifact-resolve base chat-id name)
          (setq complete t)
          name)
      (unless complete
        (when (and written (file-exists-p path) (not (file-symlink-p path)))
          (delete-file path))))))

(defun ende-artifact-link (base chat-id name org-file)
  "Construct the ordinary Org link for NAME relative to ORG-FILE."
  (let* ((path (expand-file-name name (ende-artifact-directory base chat-id)))
         (target (if org-file
                     (file-relative-name path (file-name-directory org-file))
                   path)))
    (org-link-make-string (concat "file:" (org-link-escape target)) name)))

(defun ende-artifact-content (text base-function chat-id role authorize)
  "Parse TEXT into ordered parts, retaining strings for text-only content.
BASE-FUNCTION lazily resolves document storage.  AUTHORIZE checks standalone
assistant file links; user links are explicit user attachments."
  (let (edits consumed)
    (with-temp-buffer
      (insert text)
      (org-mode)
      (let ((tree (org-element-parse-buffer)))
        (org-element-map tree 'keyword
          (lambda (element)
            (when (equal (upcase (org-element-property :key element)) "ENDE_ARTIFACT")
              (let* ((name (string-trim (org-element-property :value element)))
                     (begin (org-element-property :begin element))
                     (next (save-excursion (goto-char begin) (forward-line 1) (point)))
                     (end (save-excursion (goto-char next) (min (point-max) (1+ (line-end-position)))))
                     (line (buffer-substring-no-properties next end))
                     (part (ende-artifact-resolve (funcall base-function) chat-id name)))
                (unless (string-match-p
                         "\\`[ \t]*\\[\\[file:[^]\n]+\\]\\(?:\\[[^]\n]*\\]\\)?\\][ \t]*\n?\\'" line)
                  (error "Artifact %s in conversation %s needs a following file link" name chat-id))
                (push (cons next end) consumed)
                (push (list begin end part) edits)))))
        (org-element-map tree 'link
          (lambda (link)
            (let ((begin (org-element-property :begin link)))
              (when (and (equal (org-element-property :type link) "file")
                         (not (cl-some (lambda (range) (and (<= (car range) begin)
                                                            (< begin (cdr range)))) consumed)))
                (let* ((target (org-link-unescape (org-element-property :path link)))
                       (path (funcall authorize target role))
                       (extension (downcase (or (file-name-extension path) "")))
                       (end (- (org-element-property :end link)
                               (org-element-property :post-blank link)))
                       (part (if (member extension '("png" "jpg" "jpeg" "svg"))
                                 (ende-artifact-image-info path)
                               (list :type :text :text
                                     (concat "\nAttached file contents:\n"
                                             (decode-coding-string
                                              (ende-artifact--read-bytes path) 'utf-8) "\n")))))
                  (push (list end end part) edits))))))))
    (if (null edits) text
      (let ((position 1) parts)
        (dolist (edit (sort edits (lambda (a b) (< (car a) (car b)))))
          (when (< position (car edit))
            (push (list :type :text :text (substring text (1- position) (1- (car edit)))) parts))
          (push (nth 2 edit) parts)
          (setq position (cadr edit)))
        (when (<= position (length text))
          (push (list :type :text :text (substring text (1- position))) parts))
        (nreverse parts)))))

(defun ende-artifact-generated-p (text)
  "Return non-nil when TEXT has an actionable artifact or standalone SVG."
  (with-temp-buffer
    (insert text)
    (org-mode)
    (or (org-element-map (org-element-parse-buffer) 'keyword
          (lambda (keyword)
            (equal (upcase (org-element-property :key keyword)) "ENDE_ARTIFACT")) nil t)
        (progn
          (goto-char (point-min))
          (let (found)
            (while (and (not found) (re-search-forward "^[ \t]*<svg[ \t\n>]" nil t))
              (unless (org-element-lineage
                       (org-element-context)
                       '(src-block example-block quote-block fixed-width code verbatim special-block) t)
                (setq found t)))
            found)))))

(defun ende-artifact-prepare-text (text base chat-id org-file)
  "Store standalone SVG in TEXT and resolve generated markers to Org links."
  (with-temp-buffer
    (insert text)
    (org-mode)
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*<svg\\(?:[ \t\n>]\\)" nil t)
      (let* ((begin (match-beginning 0))
             (context (org-element-context))
             (excluded (org-element-lineage context
                                           '(src-block example-block quote-block
                                             fixed-width code verbatim special-block) t)))
        (unless excluded
          (goto-char begin)
          (let ((xml-begin (progn (skip-chars-forward " \t") (point)))
                (end (save-excursion
                       (goto-char begin) (skip-chars-forward " \t")
                       (let ((limit (save-excursion (search-forward ">" nil t))))
                         (when (and limit
                                    (not (string-match-p "<!\\|<\\?"
                                                         (buffer-substring-no-properties begin limit))))
                           (condition-case nil
                               (progn (xml-parse-tag-1) (point))
                             (error nil)))))))
            (unless end (error "Incomplete generated SVG fragment"))
            (when (save-excursion (goto-char end) (looking-at "[ \t]*$"))
              (let* ((xml (buffer-substring-no-properties xml-begin end))
                     (name (ende-artifact-store base chat-id
                                                (encode-coding-string xml 'utf-8)
                                                "image/svg+xml")))
                (delete-region begin end)
                (insert "#+ENDE_ARTIFACT: " name)
                (goto-char begin)
                (forward-line 1)))))))
    (let ((tree (org-element-parse-buffer)) edits)
      (org-element-map tree 'keyword
        (lambda (keyword)
          (when (equal (upcase (org-element-property :key keyword)) "ENDE_ARTIFACT")
            (let* ((name (string-trim (org-element-property :value keyword)))
                   (begin (org-element-property :begin keyword))
                   (next (save-excursion (goto-char begin) (forward-line 1) (point)))
                   (end (save-excursion
                          (goto-char next)
                          (if (looking-at "[ \t]*\\[\\[file:")
                              (min (point-max) (1+ (line-end-position))) next))))
              (ende-artifact-resolve base chat-id name)
              (push (list begin end
                          (concat "#+ENDE_ARTIFACT: " name "\n"
                                  (ende-artifact-link base chat-id name org-file) "\n")) edits)))))
      (dolist (edit (sort edits (lambda (a b) (> (car a) (car b)))))
        (delete-region (car edit) (cadr edit))
        (goto-char (car edit))
        (insert (nth 2 edit))))
    (buffer-string)))

(defvar-local ende-artifact--overlays nil)
(defvar-local ende-artifact--timer nil)
(defvar-local ende-artifact--dirty nil)

(defun ende-artifact--clear (&optional begin end)
  "Remove Ende previews, optionally only between BEGIN and END."
  (setq ende-artifact--overlays
        (cl-delete-if
         (lambda (overlay)
           (when (or (null begin) (null (overlay-buffer overlay))
                     (and (<= (overlay-start overlay) end)
                          (>= (overlay-end overlay) begin)))
             (delete-overlay overlay) t)) ende-artifact--overlays)))

(defun ende-artifact-refresh (&optional begin end)
  "Refresh artifact previews between BEGIN and END without previewing other links."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (save-restriction
        (widen)
        (let ((begin (or begin (point-min))) (end (or end (point-max)))
              (case-fold-search t))
          (ende-artifact--clear begin end)
          (goto-char begin)
          (while (re-search-forward "^[ \t]*#\\+ENDE_ARTIFACT:[ \t]*\\(.*\\)$" end t)
            (let ((name (string-trim (match-string-no-properties 1)))
                  (marker (match-beginning 0))
                  (next (line-beginning-position 2)))
              (condition-case nil
                  (when (eq (org-element-type (org-element-at-point marker)) 'keyword)
                    (let* ((id (save-excursion
                                 (goto-char marker) (org-back-to-heading t)
                                 (while (> (org-current-level) 1) (org-up-heading-safe))
                                 (org-entry-get nil "ENDE_CHAT_ID")))
                           (part (ende-artifact-resolve (ende-artifact-base) id name))
                           (path (plist-get part :path))
                           (width (plist-get part :width))
                           (height (plist-get part :height))
                           (scale (min 1.0 (/ (float (min ende-artifact-preview-width
                                                          (max 80 (- (window-pixel-width) 40)))) width)
                                       (/ (float ende-artifact-preview-height) height))))
                      (goto-char next)
                      (when (looking-at "[ \t]*\\[\\[file:[^]\n]+\\]\\(?:\\[[^]\n]*\\]\\)?\\][ \t]*$")
                        (let* ((image (create-image path nil nil :width (max 1 (round (* width scale)))
                                                    :height (max 1 (round (* height scale)))))
                               (overlay (make-overlay next (line-end-position) nil t nil)))
                          (overlay-put overlay 'ende-artifact t)
                          (overlay-put overlay 'evaporate t)
                          (overlay-put overlay 'after-string
                                       (concat "\n" (propertize " " 'display image) "\n"))
                          (push overlay ende-artifact--overlays)))))
                (error nil))
              (goto-char next))))))))

(defun ende-artifact--refresh-dirty (buffer)
  "Refresh BUFFER's coalesced dirty region after editing settles."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq ende-artifact--timer nil)
      (when ende-artifact--dirty
        (let ((region ende-artifact--dirty))
          (setq ende-artifact--dirty nil)
          (unwind-protect
              (ende-artifact-refresh (car region) (cdr region))
            (set-marker (car region) nil) (set-marker (cdr region) nil)))))))

(defun ende-artifact--after-change (begin end _old)
  "Schedule a line-bounded preview update around BEGIN and END."
  (save-excursion
    (goto-char begin)
    (let ((start (line-beginning-position 0))
          (finish (progn (goto-char end) (line-beginning-position 3))))
      (if ende-artifact--dirty
          (progn
            (set-marker (car ende-artifact--dirty) (min start (car ende-artifact--dirty)))
            (set-marker (cdr ende-artifact--dirty) (max finish (cdr ende-artifact--dirty))))
        (setq ende-artifact--dirty (cons (copy-marker start) (copy-marker finish t))))))
  (unless ende-artifact--timer
    (setq ende-artifact--timer
          (run-with-idle-timer 0.4 nil #'ende-artifact--refresh-dirty (current-buffer)))))

(defun ende-artifact-configure (enabled)
  "Install or remove buffer-local artifact presentation according to ENABLED."
  (remove-hook 'after-change-functions #'ende-artifact--after-change t)
  (remove-hook 'kill-buffer-hook #'ende-artifact--disable t)
  (when (timerp ende-artifact--timer) (cancel-timer ende-artifact--timer))
  (setq ende-artifact--timer nil)
  (when ende-artifact--dirty
    (set-marker (car ende-artifact--dirty) nil)
    (set-marker (cdr ende-artifact--dirty) nil)
    (setq ende-artifact--dirty nil))
  (ende-artifact--clear)
  (when (and enabled (derived-mode-p 'org-mode))
    (add-hook 'after-change-functions #'ende-artifact--after-change nil t)
    (add-hook 'kill-buffer-hook #'ende-artifact--disable nil t)
    (ende-artifact-refresh)))

(defun ende-artifact--disable ()
  "Release buffer-owned artifact presentation resources."
  (ende-artifact-configure nil))

(provide 'ende-artifact)
;;; ende-artifact.el ends here
