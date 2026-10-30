;;; ende-artifact.el --- Org conversation image artifacts -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-element)
(require 'image)
(require 'xml)
(require 'nxml-mode)
(require 'ende-org)
(require 'ende-css)

(defgroup ende-artifact nil "Conversation images." :group 'ende)

(defcustom ende-artifact-preview-width 440
  "Maximum inline preview width in pixels."
  :type 'integer :group 'ende-artifact)

(defcustom ende-artifact-preview-height 360
  "Maximum inline preview height in pixels."
  :type 'integer :group 'ende-artifact)

(defconst ende-artifact--svg-byte-limit (* 4 1024 1024))

(define-error 'ende-artifact-validation-error "Invalid artifact")

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
    (if (equal (downcase (or (file-name-extension path) "")) "svg")
        (progn
          (insert-file-contents-literally path nil 0 (1+ ende-artifact--svg-byte-limit))
          (when (> (buffer-size) ende-artifact--svg-byte-limit)
            (signal 'ende-artifact-validation-error '("SVG exceeds the size limit"))))
      (insert-file-contents-literally path))
    (buffer-string)))

(defun ende-artifact--svg (bytes)
  "Parse and validate static SVG BYTES, rejecting unsupported active content."
  (when (> (string-bytes bytes) ende-artifact--svg-byte-limit)
    (error "SVG exceeds the size limit"))
  (with-temp-buffer
    (insert (decode-coding-string bytes 'utf-8))
    (goto-char (point-min))
    (when (re-search-forward "<!\\(?:DOCTYPE\\|ENTITY\\)\\|<\\?" nil t)
      (error "SVG declarations, entities and processing instructions are unsupported"))
    (let* ((nodes (xml-parse-region (point-min) (point-max)))
           (root (car nodes))
           (elements 0)
           (allowed '(svg g defs title desc style rect circle ellipse line polyline
                          polygon path text tspan linearGradient radialGradient
                          stop clipPath mask marker use symbol pattern)))
      (unless (and (= (length nodes) 1) (eq (car-safe root) 'svg)
                   (equal (cdr (assq 'xmlns (cadr root)))
                          "http://www.w3.org/2000/svg"))
        (error "Expected one complete SVG document with the SVG namespace"))
      (cl-labels
          ((walk (node depth)
             (when (consp node)
               (when (or (> (cl-incf elements) 20000) (> depth 64))
                 (error "SVG exceeds the complexity limit"))
               (unless (memq (car node) allowed)
                 (error "Unsupported SVG element: %s" (car node)))
               (dolist (attribute (cadr node))
                 (let ((key (symbol-name (car attribute)))
                       (value (cdr attribute)))
                   (cond
                    ((member key '("xmlns" "xmlns:xlink"))
                     (unless (equal value (if (equal key "xmlns")
                                              "http://www.w3.org/2000/svg"
                                            "http://www.w3.org/1999/xlink"))
                       (error "Unsupported SVG namespace: %s" value)))
                    ((equal key "style") (ende-css-validate value))
                    ((member key ende-css-properties)
                     (ende-css-validate value 'value))
                    ((member key '("href" "xlink:href"))
                     (unless (ende-css-local-reference-p value)
                       (error "External SVG reference")))
                    ((and (eq (car node) 'style) (equal key "type"))
                     (unless (equal value "text/css")
                       (error "Unsupported SVG stylesheet type")))
                    ((and (eq (car node) 'style) (equal key "media"))
                     (ende-css-validate value 'value))
                    ((not (member key
                                  '("id" "class" "viewBox" "role" "aria-label"
                                    "aria-labelledby" "aria-describedby" "aria-hidden"
                                    "focusable" "x1" "y1" "x2" "y2" "points" "dx" "dy"
                                    "xml:space" "offset" "gradientUnits"
                                    "gradientTransform" "spreadMethod" "patternUnits"
                                    "patternContentUnits" "patternTransform" "fx" "fy"
                                    "clipPathUnits" "maskUnits" "maskContentUnits"
                                    "markerWidth" "markerHeight" "refX" "refY" "orient"
                                    "markerUnits" "preserveAspectRatio")))
                     (error "Unsupported SVG attribute: %s" key)))))
               (when (eq (car node) 'style)
                 (unless (cl-every #'stringp (cddr node))
                   (error "SVG stylesheets must contain only CSS text"))
                 (ende-css-validate (apply #'concat (cddr node)) 'stylesheet))
               (dolist (child (cddr node))
                 (walk child (1+ depth))))))
        (walk root 0))
      root)))

(defun ende-artifact--u16be (bytes offset)
  "Read an unsigned big-endian 16-bit integer from BYTES at OFFSET."
  (logior (ash (aref bytes offset) 8) (aref bytes (1+ offset))))

(defun ende-artifact--u32be (bytes offset)
  "Read an unsigned big-endian 32-bit integer from BYTES at OFFSET."
  (logior (ash (ende-artifact--u16be bytes offset) 16)
          (ende-artifact--u16be bytes (+ offset 2))))

(defun ende-artifact--png-dimensions (bytes)
  "Return PNG dimensions from BYTES, requiring a complete, valid IHDR."
  (unless (and (>= (length bytes) 33)
               (string-prefix-p "\211PNG\r\n\032\n" bytes)
               (= (ende-artifact--u32be bytes 8) 13)
               (equal (substring bytes 12 16) "IHDR")
               (memq (aref bytes 24)
                     (cdr (assq (aref bytes 25)
                                '((0 1 2 4 8 16) (2 8 16) (3 1 2 4 8)
                                  (4 8 16) (6 8 16)))))
               (= (aref bytes 26) 0) (= (aref bytes 27) 0)
               (memq (aref bytes 28) '(0 1)))
    (error "Malformed or truncated PNG header"))
  (let ((width (ende-artifact--u32be bytes 16))
        (height (ende-artifact--u32be bytes 20)))
    (unless (and (<= 1 width #x7fffffff) (<= 1 height #x7fffffff))
      (error "Invalid PNG dimensions"))
    (cons width height)))

(defun ende-artifact--jpeg-dimensions (bytes)
  "Return JPEG dimensions by walking BYTES to a supported frame header."
  (let ((size (length bytes)) (offset 2))
    (unless (and (>= size 4) (string-prefix-p "\377\330" bytes))
      (error "Malformed or truncated JPEG header"))
    (catch 'dimensions
      (while (< offset size)
        (unless (= (aref bytes offset) #xff)
          (error "Invalid JPEG marker boundary"))
        (while (and (< offset size) (= (aref bytes offset) #xff))
          (cl-incf offset))
        (when (= offset size) (error "Truncated JPEG marker"))
        (let ((marker (aref bytes offset)))
          (cl-incf offset)
          (cond
           ((or (= marker 0) (<= #xd0 marker #xda))
            (error "JPEG has no supported frame header"))
           ((= marker #x01))
           (t
            (unless (<= (+ offset 2) size) (error "Truncated JPEG segment"))
            (let* ((length (ende-artifact--u16be bytes offset))
                   (end (+ offset length)))
              (unless (and (>= length 2) (<= end size))
                (error "Invalid JPEG segment length"))
              (when (memq marker '(#xc0 #xc1 #xc2 #xc3 #xc5 #xc6 #xc7
                                        #xc9 #xca #xcb #xcd #xce #xcf))
                (unless (and (>= length 8) (> (aref bytes (+ offset 7)) 0)
                             (= length (+ 8 (* 3 (aref bytes (+ offset 7))))))
                  (error "Malformed JPEG frame header"))
                (throw 'dimensions
                       (cons (ende-artifact--u16be bytes (+ offset 5))
                             (ende-artifact--u16be bytes (+ offset 3)))))
              (setq offset end))))))
      (error "JPEG has no supported frame header"))))

(defun ende-artifact--local-path (path)
  "Resolve local PATH against the source Org file and check readability."
  (when (or (file-remote-p path)
            (and buffer-file-name (file-remote-p buffer-file-name)
                 (not (file-name-absolute-p path))))
    (error "Remote file attachments are unsupported: %s" path))
  (unless (or (file-name-absolute-p path) buffer-file-name)
    (error "Relative file attachments require an Org file: %s" path))
  (let ((absolute (expand-file-name
                   path (and (not (file-name-absolute-p path)) buffer-file-name
                             (file-name-directory buffer-file-name)))))
    (when (file-remote-p absolute)
      (error "Remote file attachments are unsupported: %s" path))
    (unless (and (file-regular-p absolute) (file-readable-p absolute))
      (error "Attachment is not a readable regular local file: %s" path))
    (file-truename absolute)))

(defconst ende-artifact--cache-entry-limit 64)
(defconst ende-artifact--cache-byte-limit (* 16 1024 1024))
(defconst ende-artifact--cache-payload-limit (* 4 1024 1024))
(defvar ende-artifact--cache (make-hash-table :test #'equal))
(defvar ende-artifact--cache-clock 0)

(defun ende-artifact-clear-cache ()
  "Release process-local validated image metadata and provider payloads."
  (clrhash ende-artifact--cache)
  (setq ende-artifact--cache-clock 0))

(defvar ende-artifact--record-stored-file nil)

(defun ende-artifact--call-with-transaction (function)
  "Call FUNCTION, removing newly stored artifacts if it fails."
  (let* ((files nil)
         (ende-artifact--record-stored-file (lambda (path) (push path files)))
         complete)
    (unwind-protect
        (prog1 (funcall function)
          (setq complete t))
      (unless complete
        (dolist (path files)
          (remhash path ende-artifact--cache)
          (when (and (file-exists-p path) (not (file-symlink-p path)))
            (delete-file path)))))))

(defun ende-artifact--file-signature (path)
  "Return PATH's practical file identity and mutable preconditions."
  (let ((attributes (file-attributes path 'integer)))
    (unless attributes (error "Image file does not exist: %s" path))
    (list (file-attribute-file-identifier attributes)
          (file-attribute-size attributes)
          (file-attribute-modification-time attributes)
          (file-attribute-status-change-time attributes)
          (file-attribute-modes attributes))))

(defun ende-artifact--cache-bytes ()
  "Return the total bytes retained in the image cache."
  (let ((size 0))
    (maphash (lambda (_path entry)
               (cl-incf size (length (plist-get entry :data)))
               (dolist (payload (plist-get entry :payloads))
                 (cl-incf size (length (cdr payload))))) ende-artifact--cache)
    size))

(defun ende-artifact--cache-room (size)
  "Evict least recently used entries to make room for SIZE bytes."
  (while (and (> (hash-table-count ende-artifact--cache) 0)
              (or (>= (hash-table-count ende-artifact--cache) ende-artifact--cache-entry-limit)
                  (> (+ size (ende-artifact--cache-bytes)) ende-artifact--cache-byte-limit)))
    (let (oldest tick)
      (maphash (lambda (path entry)
                 (when (or (null tick) (< (plist-get entry :used) tick))
                   (setq oldest path tick (plist-get entry :used)))) ende-artifact--cache)
      (remhash oldest ende-artifact--cache))))

(defun ende-artifact--validate-image (path bytes)
  "Validate image BYTES for PATH and return path-based metadata."
  (let* ((extension (downcase (or (file-name-extension path) "")))
         (type (cond ((equal extension "svg") 'svg)
                     ((equal extension "png") 'png)
                     ((member extension '("jpg" "jpeg")) 'jpeg)))
         (media (cdr (assq type '((png . "image/png") (jpeg . "image/jpeg")
                                  (svg . "image/svg+xml")))))
         dimensions)
    (unless media (error "Unsupported image format: %s" path))
    (setq dimensions
          (if (eq type 'svg)
              (let* ((attributes (cadr (ende-artifact--svg bytes)))
                     (view (split-string (or (cdr (assq 'viewBox attributes))
                                             "0 0 300 150") "[, \t\n]+" t)))
                (cons (string-to-number (or (cdr (assq 'width attributes))
                                            (nth 2 view) "300"))
                      (string-to-number (or (cdr (assq 'height attributes))
                                            (nth 3 view) "150"))))
            (unless (eq (image-type-from-data bytes) type)
              (error "Image content does not match its format"))
            (if (eq type 'png) (ende-artifact--png-dimensions bytes)
              (ende-artifact--jpeg-dimensions bytes))))
    (unless (and dimensions (> (car dimensions) 0) (> (cdr dimensions) 0))
      (error "Malformed image content"))
    (when (and (eq type 'svg)
               (or (> (car dimensions) 8192) (> (cdr dimensions) 8192)
                   (> (* (car dimensions) (cdr dimensions)) (* 16 1024 1024))))
      (error "SVG exceeds the dimension limit"))
    (list :type :image :path path :media-type media
          :width (car dimensions) :height (cdr dimensions)
          :bytes (length bytes))))

(defun ende-artifact--image-entry (path &optional expected)
  "Return the current validated entry for PATH, checking EXPECTED even on hits."
  (setq path (ende-artifact--local-path path))
  (let* ((signature (ende-artifact--file-signature path))
         (entry (gethash path ende-artifact--cache)))
    (unless (equal signature (plist-get entry :signature))
      (remhash path ende-artifact--cache)
      (let* ((bytes (ende-artifact--read-bytes path))
             (info (condition-case failure (ende-artifact--validate-image path bytes)
                     (error
                      (signal 'ende-artifact-validation-error
                              (list (error-message-string failure))))))
             (retain (<= (length bytes) ende-artifact--cache-payload-limit)))
        (ende-artifact--cache-room (if retain (length bytes) 0))
        (setq entry (list :signature signature :info info :used 0
                          :data (and retain bytes) :payloads nil))
        (puthash path entry ende-artifact--cache)
        ;; Oversized bytes are available to this caller, but not retained.
        (unless retain (setq entry (plist-put (copy-sequence entry) :data bytes)))))
    (setf (plist-get entry :used) (cl-incf ende-artifact--cache-clock))
    (when (and expected (not (equal expected (plist-get (plist-get entry :info) :media-type))))
      (error "Image %s has mismatched media type; expected %s" path expected))
    entry))

(defun ende-artifact--load-image (path &optional expected)
  "Load PATH through the shared cache, optionally requiring EXPECTED media type."
  (copy-sequence (plist-get (ende-artifact--image-entry path expected) :info)))

(defun ende-artifact--image-data (path &optional expected)
  "Return validated raster bytes for PATH and EXPECTED at the provider boundary."
  (let* ((entry (ende-artifact--image-entry path expected))
         (info (plist-get entry :info))
         (path (plist-get info :path))
         (media (plist-get info :media-type)))
    (if (not (equal media "image/svg+xml"))
        (cons media (or (plist-get entry :data) (ende-artifact--read-bytes path)))
      (let* ((program (or (executable-find "rsvg-convert") (executable-find "convert")))
             (_available
              (unless program
                (error "Cannot send SVG image input: install rsvg-convert (preferred) or ImageMagick convert to rasterize it")))
             (arguments (if (equal (file-name-nondirectory program) "rsvg-convert")
                            '("--format=png") '("svg:-" "-strip" "png:-")))
             (key (list program (ende-artifact--file-signature program) arguments))
             (cached (assoc key (plist-get entry :payloads))))
        (cons "image/png"
              (or (cdr cached)
                  (let ((data
                         (with-temp-buffer
                           (set-buffer-multibyte nil)
                           (insert (or (plist-get entry :data) (ende-artifact--read-bytes path)))
                           (let* ((coding-system-for-read 'no-conversion)
                                  (coding-system-for-write 'no-conversion)
                                  (status (apply #'call-process-region
                                                 (point-min) (point-max) program t (list t nil) nil arguments)))
                             (unless (and (integerp status) (zerop status)
                                          (eq (image-type-from-data (buffer-string)) 'png))
                               (error "SVG to PNG conversion failed: %s" path))
                             (ende-artifact--png-dimensions (buffer-string))
                             (buffer-string)))))
                    ;; One derived payload per entry; converter changes replace it.
                    (when (and (eq entry (gethash path ende-artifact--cache))
                               (<= (length data) ende-artifact--cache-payload-limit))
                      (setf (plist-get entry :payloads) nil)
                      (ende-artifact--cache-room (length data))
                      (when (eq entry (gethash path ende-artifact--cache))
                        (setf (plist-get entry :payloads) (list (cons key data)))))
                    data)))))))

(defun ende-artifact-resolve (base chat-id name)
  "Resolve creation-time shorthand NAME under BASE and CHAT-ID.
Keep generated filenames contained in their output directory."
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
            (ende-artifact--load-image path)))
      (error
       (signal (car failure)
               (list (format "Artifact %s in conversation %s: %s"
                             name chat-id
                             (if (eq (car failure) 'ende-artifact-validation-error)
                                 (cadr failure)
                               (error-message-string failure)))))))))

(defun ende-artifact--store (base chat-id media writer &optional bytes)
  "Store MEDIA under BASE and CHAT-ID using WRITER; return its filename.
WRITER receives the destination and returns validated metadata.
BYTES, when supplied, seed the bounded source cache."
  (let* ((extension (cdr (assoc media '(("image/png" . "png")
                                       ("image/jpeg" . "jpg")
                                       ("image/svg+xml" . "svg")))))
         (directory (progn
                      (unless extension
                        (error "Unsupported generated image media type"))
                      (ende-artifact-directory base chat-id t)))
         (index 1) name path written complete)
    (while (progn
             (setq name (format "image-%03d.%s" index extension)
                   path (expand-file-name name directory))
             (cl-incf index)
             (or (file-exists-p path) (file-symlink-p path))))
    (unwind-protect
        (let* ((info (funcall writer path))
               (retain (<= (length bytes) ende-artifact--cache-payload-limit)))
          (setq written t)
          (ende-artifact--cache-room (if retain (length bytes) 0))
          (puthash path (list :signature (ende-artifact--file-signature path)
                             :info info :used (cl-incf ende-artifact--cache-clock)
                             :data (and retain bytes) :payloads nil)
                   ende-artifact--cache)
          (when ende-artifact--record-stored-file
            (funcall ende-artifact--record-stored-file path))
          (setq complete t)
          name)
      (unless complete
        (remhash path ende-artifact--cache)
        (when (and written (file-exists-p path) (not (file-symlink-p path)))
          (delete-file path))))))

(defun ende-artifact-store (base chat-id bytes media)
  "Validate and exclusively store BYTES of MEDIA under BASE and CHAT-ID."
  (ende-artifact--store
   base chat-id media
   (lambda (path)
     (let ((info (condition-case failure (ende-artifact--validate-image path bytes)
                   (error
                    (signal 'ende-artifact-validation-error
                            (list (format "Artifact %s in conversation %s: %s"
                                          (file-name-nondirectory path) chat-id
                                          (error-message-string failure))))))))
       (let ((coding-system-for-write 'no-conversion))
         (write-region bytes nil path nil 'silent nil 'excl))
       info))
   bytes))

(defun ende-artifact-import (base chat-id source info)
  "Move validated SOURCE with metadata INFO under BASE and CHAT-ID."
  (ende-artifact--store
   base chat-id (plist-get info :media-type)
   (lambda (path)
     (rename-file source path nil)
     (plist-put (copy-sequence info) :path path))))

(defun ende-artifact--caption (caption)
  "Return a nonempty single-line CAPTION, or nil."
  (unless (or (null caption) (stringp caption))
    (error "Artifact caption must be a string"))
  (let ((text
         (string-trim
          (replace-regexp-in-string "[\r\n]+" " " (or caption "")))))
    (unless (string-empty-p text)
      text)))

(defun ende-artifact-render (base chat-id name org-file &optional caption)
  "Render NAME as a complete exportable Org reference with optional CAPTION.
Use a plain file link relative to ORG-FILE, or absolute without ORG-FILE."
  (unless (ende-artifact--valid-name-p name)
    (error "Invalid artifact filename: %s" name))
  (let* ((path (expand-file-name name (ende-artifact-directory base chat-id)))
         (target (if org-file
                     (file-relative-name path (file-name-directory org-file))
                   path))
         (caption (ende-artifact--caption caption)))
    (unless (or (file-name-absolute-p target)
                (string-prefix-p "../" target) (string-prefix-p "./" target))
      (setq target (concat "./" target)))
    ;; Escaping for bracket links does not protect whitespace in plain links.
    (unless (with-temp-buffer
              (org-mode)
              (insert "file:" target "\n")
              (goto-char (point-min))
              (let ((link (ende-artifact--plain-file-link)))
                (and link (not (org-element-property :search-option link))
                     (equal target (org-link-unescape (org-element-property :path link))))))
      (error "Artifact path cannot be represented losslessly as a plain Org file link: %s" target))
    (concat "#+ENDE_ARTIFACT: " name "\n"
            (when caption (concat "#+CAPTION: " caption "\n"))
            "file:" target "\n\n")))

(defun ende-artifact--plain-file-link ()
  "Return the sole plain file link on the current line, or nil."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (let* ((begin (point))
           (link (org-element-context))
           (end (and (eq (org-element-type link) 'link)
                     (- (org-element-property :end link)
                        (org-element-property :post-blank link)))))
      (when (and end (eq (org-element-property :format link) 'plain)
                 (equal (org-element-property :type link) "file")
                 (= begin (org-element-property :begin link))
                 (<= end (line-end-position))
                 (string-blank-p (buffer-substring-no-properties end (line-end-position))))
        link))))

(defun ende-artifact--references (text &optional generated attachments)
  "Discover active references in TEXT, using source buffer coordinates.
GENERATED includes standalone SVG and bare generation-time names.
ATTACHMENTS includes ordinary user file links.  No files are read or written."
  (with-temp-buffer
    (org-mode)
    (insert (plist-get (ende-org--blocks text) :parse-content))
    (let ((tree (org-element-parse-buffer))
          (consumed (make-hash-table :test #'eql)) references)
      (org-element-map tree 'keyword
        (lambda (element)
          (when (equal (upcase (org-element-property :key element)) "ENDE_ARTIFACT")
            (save-excursion
              (let* ((begin (org-element-property :begin element))
                     (name (string-trim (org-element-property :value element)))
                     (reference (list :part nil :begin begin :name name)))
                (goto-char begin)
                (forward-line 1)
                ;; CAPTION is affiliated metadata, not an ordinary keyword node.
                (let ((case-fold-search t))
                  (when (looking-at "[ \t]*#\\+CAPTION:[ \t]*\\(.*\\)$")
                    (setq reference (plist-put reference :caption (match-string-no-properties 1)))
                    (forward-line 1)))
                (let ((link (ende-artifact--plain-file-link)))
                  (when link
                    (puthash (org-element-property :begin link) t consumed)
                    (setq reference
                          (append reference
                                  (list :path (org-link-unescape (org-element-property :path link))
                                        :link-begin (org-element-property :begin link)
                                        :link-end (- (org-element-property :end link)
                                                     (org-element-property :post-blank link)))))
                    (forward-line 1)))
                (while (and (< (point) (point-max)) (looking-at "[ \t]*$"))
                  (forward-line 1))
                (push (plist-put reference :end (point)) references))))))
      (when attachments
        (org-element-map tree 'link
          (lambda (link)
            (when (and (equal (org-element-property :type link) "file")
                       (not (gethash (org-element-property :begin link) consumed)))
              (let ((end (- (org-element-property :end link)
                            (org-element-property :post-blank link))))
                (push (list :part nil :begin end :end end :attachment t
                            :path (org-link-unescape (org-element-property :path link))) references))))))
      (when generated
        (goto-char (point-min))
        (let ((case-fold-search nil))
          (while (re-search-forward "^[ \t]*<svg[ \t\n>]" nil t)
            (let ((begin (match-beginning 0)))
              (unless (org-element-lineage (org-element-context)
                                          '(fixed-width code verbatim) t)
                (let ((end (with-temp-buffer
                             (insert (substring text (1- begin)))
                             (let ((nxml-auto-insert-xml-declaration-flag nil)
                                   (inhibit-message t))
                               (nxml-mode))
                             (goto-char (point-min))
                             (skip-chars-forward " \t")
                             (condition-case nil
                                 (progn (nxml-forward-element) (+ begin (1- (point))))
                               (error
                                (signal 'ende-artifact-validation-error
                                        '("Incomplete generated SVG fragment")))))))
                  (goto-char end)
                  (when (looking-at "[ \t]*$")
                    (let ((finish (min (point-max) (1+ (line-end-position)))))
                      (push (list :part nil :begin begin :end finish
                                  :svg (string-trim (substring text (1- begin) (1- end))))
                            references))
                    (forward-line 1))))))))
      (let ((end 0) active)
        (dolist (reference (sort references (lambda (a b) (< (plist-get a :begin) (plist-get b :begin)))))
          ;; SVG text is part of the enclosing image, not an Org directive.
          (when (>= (plist-get reference :begin) end)
            (push reference active)
            (when (plist-get reference :svg) (setq end (plist-get reference :end)))))
        (nreverse active)))))

(defun ende-artifact--parts (text references)
  "Split TEXT at resolved REFERENCES without rediscovering them."
  (let ((position 1) parts)
    (dolist (reference references)
      (let ((begin (plist-get reference :begin)))
        (when (< position begin)
          (push (list :type :text :text (substring text (1- position) (1- begin))) parts))
        (when-let* ((caption (ende-artifact--caption
                             (if (plist-member reference :resolved-caption)
                                 (plist-get reference :resolved-caption)
                               (plist-get reference :caption)))))
          (push (list :type :text :text (concat caption "\n")) parts))
        (push (plist-get reference :part) parts)
        (setq position (plist-get reference :end))))
    (when (<= position (length text))
      (push (list :type :text :text (substring text (1- position))) parts))
    (setq parts (nreverse parts))
    (if (cl-every (lambda (part) (eq (plist-get part :type) :text)) parts)
        (mapconcat (lambda (part) (plist-get part :text)) parts "")
      parts)))

(defun ende-artifact-content (text &optional resolve-file)
  "Read safe TEXT into ordered parts without altering its source.
Explicit references use their file link; RESOLVE-FILE enables user attachments."
  (let ((references (ende-artifact--references text nil resolve-file)))
    (dolist (reference references)
      (let* ((target (plist-get reference :path))
             (path (if (plist-get reference :attachment)
                       (funcall resolve-file target) target))
             (extension (and path (downcase (or (file-name-extension path) "")))))
        (unless path
          (error "Artifact %s needs a following file link" (plist-get reference :name)))
        (setf (plist-get reference :part)
              (if (or (not (plist-get reference :attachment))
                      (member extension '("png" "jpg" "jpeg" "svg")))
                  (ende-artifact--load-image path)
                (let ((bytes (ende-artifact--read-bytes path)))
                  (when (or (member extension '("gif" "webp" "bmp" "tif" "tiff" "avif" "heic"))
                            (image-type-from-data bytes) (string-match-p (string 0) bytes))
                    (error "Unsupported file attachment format; use PNG, JPEG, SVG or text: %s" target))
                  (list :type :text :text
                        (concat "\nAttached file contents:\n"
                                (decode-coding-string bytes 'utf-8) "\n")))))))
    (ende-artifact--parts text references)))

(defun ende-artifact-prepare-text (text storage org-file &optional artifacts)
  "Resolve normalized TEXT once into persisted Org and ordered content parts.
STORAGE lazily returns (BASE CHAT-ID) for creation-time references only.
ARTIFACTS supplies descriptors, overridden by explicit reference captions."
  (let ((references (ende-artifact--references text t)) edits)
    (dolist (reference references)
      (let* ((generated (cl-find (plist-get reference :name) artifacts
                                :key (lambda (artifact) (plist-get artifact :name)) :test #'equal))
             (caption (ende-artifact--caption
                       (if (plist-member reference :caption)
                           (plist-get reference :caption)
                         (plist-get generated :caption))))
             (path (plist-get reference :path))
             (part
              (if path (ende-artifact--load-image path)
                (pcase-let* ((`(,base ,id) (funcall storage))
                             (name (if (plist-get reference :svg)
                                       (ende-artifact-store
                                        base id (encode-coding-string (plist-get reference :svg) 'utf-8)
                                        "image/svg+xml")
                                     (plist-get reference :name))))
                  (ende-artifact-resolve base id name))))
             (directory (directory-file-name (file-name-directory (plist-get part :path)))))
        (setf (plist-get reference :part) part)
        (nconc reference (list :resolved-caption caption))
        ;; Complete references retain their resolved location, even across questions.
        (push (list (plist-get reference :begin) (plist-get reference :end)
                    (ende-artifact-render
                     (file-name-directory directory) (file-name-nondirectory directory)
                     (file-name-nondirectory (plist-get part :path)) org-file
                     caption)) edits)))
    (list :content (ende-org--apply-edits text edits)
          :parts (ende-artifact--parts text references)
          :references references)))

(defvar-local ende-artifact--overlays nil)
(defvar-local ende-artifact--timer nil)
(defvar-local ende-artifact--dirty nil)
(defvar-local ende-artifact--literal-regions nil)
(defvar-local ende-artifact--analysis-valid nil)
(defvar-local ende-artifact--structural-change nil)

(defun ende-artifact--clear (&optional begin end retain)
  "Return complete preview bounds, removing affected overlays unless RETAIN."
  (let ((start (or begin (point-min)))
        (finish (or end (point-max))))
    (setq ende-artifact--overlays
          (cl-delete-if
           (lambda (overlay)
             (cond
              ((null (overlay-buffer overlay)) t)
              ((or (null begin)
                   (and (<= (overlay-start overlay) end)
                        (>= (overlay-end overlay) begin)))
               (setq start (min start (overlay-start overlay))
                     finish (max finish (overlay-end overlay)))
               (unless retain (delete-overlay overlay))
               (not retain))))
           ende-artifact--overlays))
    (cons start finish)))

(defun ende-artifact--release-analysis ()
  "Release buffer-owned literal-region markers."
  (dolist (range ende-artifact--literal-regions)
    (set-marker (car range) nil) (set-marker (cdr range) nil))
  (setq ende-artifact--literal-regions nil ende-artifact--analysis-valid nil
        ende-artifact--structural-change nil))

(defun ende-artifact--analyze ()
  "Update literal regions when structure changed; return changed region bounds."
  (unless ende-artifact--analysis-valid
    (let* ((old (mapcar (lambda (range) (cons (marker-position (car range))
                                             (marker-position (cdr range))))
                        ende-artifact--literal-regions))
           (new (plist-get (ende-org--blocks (buffer-substring-no-properties (point-min) (point-max))) :ranges))
           (known (make-hash-table :test #'equal)) changed)
      (dolist (range old) (puthash range t known))
      (dolist (range new)
        (if (gethash range known) (remhash range known) (push range changed)))
      (maphash (lambda (range _value) (push range changed)) known)
      (ende-artifact--release-analysis)
      (setq ende-artifact--literal-regions
            (mapcar (lambda (range) (cons (copy-marker (car range))
                                          (copy-marker (cdr range) t))) new)
            ende-artifact--analysis-valid t)
      (when changed
        (cons (apply #'min (mapcar #'car changed))
              (apply #'max (mapcar #'cdr changed)))))))

(defun ende-artifact--preview-text (begin end)
  "Return BEGIN through END with known literal regions masked."
  (let (ranges)
    (dolist (range ende-artifact--literal-regions)
      (when (and (< (car range) end) (> (cdr range) begin))
        (push (cons (1+ (- (max begin (car range)) begin))
                    (1+ (- (min end (cdr range)) begin))) ranges)))
    (ende-org--mask-ranges (buffer-substring-no-properties begin end) ranges)))

(defun ende-artifact-refresh (&optional begin end)
  "Reconcile previews in BEGIN through END, retaining unchanged displays."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (save-restriction
        (widen)
        (unless (or begin end) (setq ende-artifact--analysis-valid nil))
        (let* ((changed (ende-artifact--analyze))
               (begin (min (or begin (point-min)) (or (car changed) (point-max))))
               (end (max (or end (point-max)) (or (cdr changed) (point-min))))
               (bounds (ende-artifact--clear
                        (progn (goto-char begin) (line-beginning-position -1))
                        (progn (goto-char end) (line-beginning-position 4)) t))
               (begin (car bounds)) (end (cdr bounds))
               (existing (make-hash-table :test #'eql))
               (kept (make-hash-table :test #'eq)))
          (dolist (overlay ende-artifact--overlays)
            (when (and (<= (overlay-start overlay) end) (>= (overlay-end overlay) begin))
              (puthash (overlay-start overlay) overlay existing)))
          (dolist (reference (ende-artifact--references (ende-artifact--preview-text begin end)))
            (when (plist-get reference :path)
              ;; A bad editor reference must not break unrelated previews.
              (condition-case nil
                  (let* ((entry (ende-artifact--image-entry (plist-get reference :path)))
                         (part (plist-get entry :info))
                         (path (plist-get part :path))
                         (width (plist-get part :width)) (height (plist-get part :height))
                         (scale (min 1.0 (/ (float (min ende-artifact-preview-width
                                                       (max 80 (- (window-pixel-width) 40)))) width)
                                     (/ (float ende-artifact-preview-height) height)))
                         (pixels-x (max 1 (round (* width scale))))
                         (pixels-y (max 1 (round (* height scale))))
                         (signature (list path (plist-get entry :signature) pixels-x pixels-y))
                         (start (+ begin (1- (plist-get reference :begin))))
                         (finish (save-excursion
                                   (goto-char (+ begin (1- (plist-get reference :link-end))))
                                   (line-end-position)))
                         (overlay (or (gethash start existing)
                                      (cl-find-if
                                       (lambda (candidate)
                                         (and (overlay-get candidate 'ende-artifact)
                                              (not (gethash candidate kept))))
                                       (overlays-at (max start (1- finish)))))))
                    (unless overlay
                      (setq overlay (make-overlay start finish nil t nil))
                      (overlay-put overlay 'ende-artifact t)
                      (overlay-put overlay 'evaporate t)
                      (push overlay ende-artifact--overlays))
                    (unless (equal signature (overlay-get overlay 'ende-artifact-signature))
                      (let ((image (if (equal (plist-get part :media-type) "image/svg+xml")
                                       (create-image (plist-get entry :data) 'svg t
                                                     :width pixels-x :height pixels-y)
                                     (create-image path nil nil :width pixels-x :height pixels-y)))
                            ;; The link's end can remain visible outside a fold.
                            (collapse `(when (invisible-p (overlay-start ,overlay)) . "")))
                        (when (display-graphic-p) (image-flush image t))
                        (overlay-put overlay 'after-string
                                     (propertize
                                      (concat (propertize "\n" 'display collapse)
                                              (propertize " " 'display (list collapse image)))
                                      'face 'default))
                        (overlay-put overlay 'ende-artifact-signature signature)))
                    (move-overlay overlay start finish)
                    (puthash overlay t kept))
                (error nil))))
          (setq ende-artifact--overlays
                (cl-delete-if
                 (lambda (overlay)
                   (when (and (<= (overlay-start overlay) end) (>= (overlay-end overlay) begin)
                              (not (gethash overlay kept)))
                     (delete-overlay overlay) t)) ende-artifact--overlays)))))))

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

(defun ende-artifact--structure-at (begin end)
  "Return non-nil if BEGIN through END touches a structural delimiter line."
  (save-excursion
    (goto-char begin)
    (beginning-of-line)
    (let ((limit (save-excursion (goto-char end) (line-end-position)))
          (case-fold-search t))
      (re-search-forward "^[ \t]*\\(?:#\\+\\(?:begin\\|end\\)_\\|\\*+[ \t]\\)" limit t))))

(defun ende-artifact--before-change (begin end)
  "Remember structural edits before delimiter text can disappear."
  (setq ende-artifact--structural-change (ende-artifact--structure-at begin end)))

(defun ende-artifact--after-change (begin end _old)
  "Debounce preview edits, invalidating literal analysis for structural changes."
  (when (or ende-artifact--structural-change (ende-artifact--structure-at begin end))
    (setq ende-artifact--analysis-valid nil))
  (setq ende-artifact--structural-change nil)
  (save-excursion
    (goto-char begin)
    (let ((start (line-beginning-position -1))
          (finish (progn (goto-char end) (line-beginning-position 4))))
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
  (remove-hook 'before-change-functions #'ende-artifact--before-change t)
  (remove-hook 'after-change-functions #'ende-artifact--after-change t)
  (remove-hook 'kill-buffer-hook #'ende-artifact--disable t)
  (when (timerp ende-artifact--timer) (cancel-timer ende-artifact--timer))
  (setq ende-artifact--timer nil)
  (when ende-artifact--dirty
    (set-marker (car ende-artifact--dirty) nil)
    (set-marker (cdr ende-artifact--dirty) nil)
    (setq ende-artifact--dirty nil))
  (ende-artifact--release-analysis)
  (unless enabled (ende-artifact--clear))
  (when (and enabled (derived-mode-p 'org-mode))
    (dolist (overlay ende-artifact--overlays)
      (overlay-put overlay 'ende-artifact-signature nil))
    (add-hook 'before-change-functions #'ende-artifact--before-change nil t)
    (add-hook 'after-change-functions #'ende-artifact--after-change nil t)
    (add-hook 'kill-buffer-hook #'ende-artifact--disable nil t)
    (ende-artifact-refresh)))

(defun ende-artifact--disable ()
  "Release buffer-owned artifact presentation resources."
  (ende-artifact-configure nil))

(provide 'ende-artifact)
;;; ende-artifact.el ends here
