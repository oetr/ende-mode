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
                          stop clipPath mask marker use symbol pattern)))
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
                                     "role" "aria-label" "aria-labelledby" "aria-describedby"
                                     "aria-hidden" "focusable"
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
                                     "patternUnits" "patternContentUnits" "patternTransform"
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
                   path (and buffer-file-name
                             (file-name-directory buffer-file-name)))))
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
  "Validate image BYTES read from PATH and return path-based metadata."
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
                     (error (error "Image %s: %s" path (error-message-string failure)))))
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
      (error (error "Artifact %s in conversation %s, expected directory %s: %s"
                    name chat-id directory (error-message-string failure))))))

(defun ende-artifact-store (base chat-id bytes media)
  "Exclusively store validated BYTES of MEDIA under BASE and CHAT-ID.
Return the chosen filename.  Insert no Org reference until writing succeeds."
  (let* ((extension (cdr (assoc media '(("image/png" . "png")
                                       ("image/jpeg" . "jpg")
                                       ("image/svg+xml" . "svg")))))
         (_validated (when (equal media "image/svg+xml")
                       (ende-artifact--svg bytes)))
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

(defun ende-artifact--following-link (position)
  "Return the sole file link on the line after marker POSITION, or nil."
  (save-excursion
    (goto-char position)
    (forward-line 1)
    (skip-chars-forward " \t")
    (let* ((begin (point))
           (link (org-element-context))
           (end (and (eq (org-element-type link) 'link)
                     (- (org-element-property :end link)
                        (org-element-property :post-blank link)))))
      (when (and end (equal (org-element-property :type link) "file")
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
            (let* ((begin (org-element-property :begin element))
                   (name (string-trim (org-element-property :value element)))
                   (link (ende-artifact--following-link begin))
                   (end (save-excursion
                          (goto-char begin) (forward-line (if link 2 1)) (point))))
              (when link (puthash (org-element-property :begin link) t consumed))
              (push (list :part nil :begin begin :end end :name name
                          :path (and link (org-link-unescape (org-element-property :path link))))
                    references)))))
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
                               (error (error "Incomplete generated SVG fragment"))))))
                  (goto-char end)
                  (when (looking-at "[ \t]*$")
                    (let ((finish (min (point-max) (1+ (line-end-position)))))
                      (push (list :part nil :begin begin :end finish
                                  :svg (string-trim (substring text (1- begin) (1- end))))
                            references))
                    (forward-line 1))))))))
      (sort references (lambda (a b) (< (plist-get a :begin) (plist-get b :begin)))))))

(defun ende-artifact--parts (text references)
  "Split TEXT at resolved REFERENCES without rediscovering them."
  (let ((position 1) parts)
    (dolist (reference references)
      (let ((begin (plist-get reference :begin)))
        (when (< position begin)
          (push (list :type :text :text (substring text (1- position) (1- begin))) parts))
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

(defun ende-artifact-prepare-text (text storage org-file)
  "Resolve normalized TEXT once into persisted Org and ordered content parts.
STORAGE lazily returns (BASE CHAT-ID) for creation-time references only."
  (let ((references (ende-artifact--references text t)) edits)
    (dolist (reference references)
      (let ((path (plist-get reference :path)))
        (if path
            (setf (plist-get reference :part) (ende-artifact--load-image path))
          (pcase-let* ((`(,base ,id) (funcall storage))
                       (name (if (plist-get reference :svg)
                                 (ende-artifact-store
                                  base id (encode-coding-string (plist-get reference :svg) 'utf-8)
                                  "image/svg+xml")
                               (plist-get reference :name)))
                       (part (ende-artifact-resolve base id name)))
            (setf (plist-get reference :part) part)
            (push (list (plist-get reference :begin) (plist-get reference :end)
                        (concat "#+ENDE_ARTIFACT: " name "\n"
                                (ende-artifact-link base id name org-file) "\n")) edits)))))
    (list :content (ende-org--apply-edits text edits)
          :parts (ende-artifact--parts text references)
          :references references)))

(defvar-local ende-artifact--overlays nil)
(defvar-local ende-artifact--timer nil)
(defvar-local ende-artifact--dirty nil)

(defun ende-artifact--clear (&optional begin end)
  "Remove previews and return the complete affected bounds."
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
               (delete-overlay overlay)
               t)))
           ende-artifact--overlays))
    (cons start finish)))

(defun ende-artifact-refresh (&optional begin end)
  "Refresh artifact previews between BEGIN and END without previewing other links."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (save-restriction
        (widen)
        (let* ((bounds (ende-artifact--clear
                        (or begin (point-min))
                        (or end (point-max))))
               (begin (save-excursion
                        (goto-char (car bounds))
                        (line-beginning-position)))
               (end (cdr bounds))
               (case-fold-search t))
          (goto-char begin)
          (while (re-search-forward "^[ \t]*#\\+ENDE_ARTIFACT:[ \t]*\\(.*\\)$" end t)
            (let ((marker (match-beginning 0))
                  (next (line-beginning-position 2)))
              (condition-case nil
                  (when (eq (org-element-type (org-element-at-point marker)) 'keyword)
                    (let* ((link (ende-artifact--following-link marker))
                           (part (when link
                                   (ende-artifact--load-image
                                    (org-link-unescape (org-element-property :path link)))))
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
                               (overlay (make-overlay marker (line-end-position) nil t nil)))
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
