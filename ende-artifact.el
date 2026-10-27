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
                       (value (cdr attribute)))
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
                       (unless (string-match-p
                                "\\`#[[:alnum:]_.-]+\\'"
                                (string-trim (match-string 1 value)))
                         (error "External SVG CSS resource"))
                       (setq start (match-end 0))))))
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
            (ende-artifact-image-info path)))
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

(provide 'ende-artifact)
;;; ende-artifact.el ends here
