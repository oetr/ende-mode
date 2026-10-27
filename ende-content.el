;;; ende-content.el --- Ordered message content -*- lexical-binding: t; -*-

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'ende-artifact)
(require 'ende-protocol)

(defun ende-content-role (message)
  "Return MESSAGE's role."
  (if (stringp (car-safe message)) (car message) (plist-get message :role)))

(defun ende-content-body (message)
  "Return MESSAGE's string or ordered content parts."
  (if (stringp (car-safe message)) (cdr message) (plist-get message :content)))

(defun ende-content-concat (contents)
  "Concatenate CONTENTS without changing part order."
  (if (cl-every #'stringp contents) (mapconcat #'identity contents "")
    (cl-mapcan (lambda (content)
                 (if (stringp content) (list (list :type :text :text content))
                   (copy-sequence content))) contents)))

(defun ende-content-readable (content)
  "Return an inspection-only rendering of CONTENT without encoded bytes."
  (if (stringp content) content
    (mapconcat
     (lambda (part)
       (pcase (plist-get part :type)
         (:text (plist-get part :text))
         (:image (format "\n[%s: %s, %s×%s, %.1f KB]\n"
                         (plist-get part :media-type)
                         (file-name-nondirectory (plist-get part :path))
                         (plist-get part :width) (plist-get part :height)
                         (/ (or (plist-get part :bytes) 0) 1024.0)))
         (_ (error "Unsupported message content part")))) content "")))

(defun ende-content-empty-p (content)
  "Return non-nil if CONTENT has neither images nor nonblank text."
  (if (stringp content) (string-blank-p content)
    (cl-every (lambda (part) (and (eq (plist-get part :type) :text)
                                 (string-blank-p (plist-get part :text)))) content)))

(defun ende-content-image-data (part)
  "Return validated PART as raster bytes and its actual media type."
  (let* ((path (plist-get part :path))
         (info (if (plist-get part :artifact-base)
                   (ende-artifact-resolve (plist-get part :artifact-base)
                                          (plist-get part :chat-id) (plist-get part :name))
                 (ende-artifact-image-info path (plist-get part :media-type))))
         (media (plist-get info :media-type))
         (bytes (ende-artifact--read-bytes path)))
    (if (not (equal media "image/svg+xml")) (cons media bytes)
      (let ((program (or (executable-find "rsvg-convert")
                         (executable-find "convert"))))
        (unless program
          (error "SVG image input requires rsvg-convert or ImageMagick convert"))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert bytes)
          (let* ((coding-system-for-read 'no-conversion)
                 (coding-system-for-write 'no-conversion)
                 (arguments (if (equal (file-name-nondirectory program) "rsvg-convert")
                                '("--format=png") '("svg:-" "-strip" "png:-")))
                 (status (apply #'call-process-region
                                (point-min) (point-max) program t (list t nil) nil arguments)))
            (unless (and (integerp status) (zerop status)
                         (eq (image-type-from-data (buffer-string)) 'png))
              (error "SVG to PNG conversion failed"))
            (cons "image/png" (buffer-string))))))))

(defun ende-content-data-url (part)
  "Encode PART only at the provider boundary."
  (let ((data (ende-content-image-data part)))
    (concat "data:" (car data) ";base64," (base64-encode-string (cdr data) t))))

(defun ende-content-provider-messages (messages)
  "Place assistant images in explicitly identified, valid user input positions.
The original role and order stay intact internally.  Tool calls remain after
all their assistant content and before the corresponding results."
  (cl-mapcan
   (lambda (message)
     (let ((role (ende-content-role message))
           (content (ende-content-body message)))
       (if (or (not (equal role "assistant")) (null content) (stringp content))
           (list message)
         (let (result (index 0))
           (dolist (part content)
             (if (eq (plist-get part :type) :image)
                 (push (list :role "user" :content
                             (list (list :type :text :text
                                         (format "Image from the preceding assistant response (part %d):"
                                                 (cl-incf index))) part)) result)
               (push (cons "assistant" (plist-get part :text)) result)))
           (when (plist-get message :tool-calls)
             (push (list :role "assistant" :content ""
                         :tool-calls (plist-get message :tool-calls)
                         :reasoning-content (plist-get message :reasoning-content)) result))
           (nreverse result))))) messages))

(defun ende-content-provider-content (role content wire &optional profile)
  "Encode CONTENT for ROLE and WIRE, respecting PROFILE image capability."
  (if (or (null content) (stringp content))
      (if (eq wire 'chat) content
        (if (equal role "system") content
          (list (list (cons :type (if (equal role "assistant") "output_text" "input_text"))
                      (cons :text content)))))
    (unless (equal role "user")
      (error "Images are unsupported in %s messages on %s" role wire))
    (when (and (plist-member profile :image-input) (not (plist-get profile :image-input)))
      (error "Selected provider does not support image input"))
    (mapcar
     (lambda (part)
       (pcase (plist-get part :type)
         (:text (list (cons :type (if (eq wire 'chat) "text" "input_text"))
                      (cons :text (plist-get part :text))))
         (:image (let ((url (ende-content-data-url part)))
                   (if (eq wire 'chat)
                       (list (cons :type "image_url") (cons :image_url (list (cons :url url))))
                     (list (cons :type "input_image") (cons :image_url url)))))
         (_ (error "Unsupported multimodal content part")))) content)))

(defun ende-content-image-descriptor (media bytes)
  "Return a stable debug descriptor for image BYTES and MEDIA."
  (list (cons :type "image") (cons :sha256 (secure-hash 'sha256 bytes))
        (cons :media_type media) (cons :bytes (length bytes))))

(defun ende-content-redact (value)
  "Replace encoded images and internal image parts in VALUE with descriptors."
  (cond
   ((and (stringp value) (string-prefix-p "data:image/" value))
    (if (string-match "\\`data:\\([^;]+\\);base64," value)
        (let ((media (match-string 1 value)) (start (match-end 0)))
          (condition-case nil
              (ende-content-image-descriptor media (base64-decode-string (substring value start)))
            (error "[invalid image data redacted]")))
      "[image data redacted]"))
   ((and (listp value) (eq (car value) :type)
         (eq (plist-get value :type) :generated-image))
    (ende-content-image-descriptor (plist-get value :media-type) (plist-get value :bytes)))
   ((and (listp value) (eq (car value) :type) (eq (plist-get value :type) :image))
    (ende-content-image-descriptor (plist-get value :media-type)
                                   (ende-artifact--read-bytes (plist-get value :path))))
   ((hash-table-p value)
    (let ((copy (copy-hash-table value)))
      (maphash (lambda (key item)
                 (puthash key
                          (if (member key '("b64_json" "result"))
                              (if (stringp item) "[provider payload redacted]" (ende-content-redact item))
                            (ende-content-redact item)) copy)) value)
      copy))
   ((consp value) (cons (ende-content-redact (car value)) (ende-content-redact (cdr value))))
   ((vectorp value) (vconcat (mapcar #'ende-content-redact value)))
   (t value)))

(defun ende-content-generated-image (data &optional media)
  "Decode provider image DATA, optionally checking MEDIA, without exposing bytes."
  (unless (stringp data) (error "Provider image data is missing"))
  (when (string-prefix-p "data:" data)
    (unless (string-match "\\`data:\\(image/[[:alnum:]+.-]+\\);base64," data)
      (error "Unsupported generated image data URL"))
    (setq media (match-string 1 data) data (substring data (match-end 0))))
  (let* ((bytes (condition-case nil (base64-decode-string data)
                  (error (error "Malformed generated image encoding"))))
         (type (image-type-from-data bytes))
         (actual (cdr (assq type '((png . "image/png") (jpeg . "image/jpeg"))))))
    (unless (and actual (or (null media) (equal media actual)))
      (error "Unsupported generated image content or media type"))
    (list :type :generated-image :bytes bytes :media-type actual)))

(defun ende-content-output-parts (text images)
  "Normalize provider TEXT and IMAGES in their documented output order."
  (let (parts)
    (cond
     ((stringp text) (unless (string-empty-p text)
                       (push (list :type :text :text text) parts)))
     ((or (listp text) (vectorp text))
      (mapc (lambda (part)
              (let ((type (ende-protocol-json-get "type" part)))
                (cond
                 ((member type '("text" "output_text"))
                  (push (list :type :text :text (ende-protocol-json-get "text" part)) parts))
                 ((member type '("image_url" "image"))
                  (push (ende-content-generated-image
                         (ende-protocol-json-get "url" (ende-protocol-json-get "image_url" part))) parts))
                 (t (error "Unsupported provider output content part"))))) text))
     (t (error "Unsupported provider response content")))
    (mapc (lambda (image)
            (push (ende-content-generated-image
                   (ende-protocol-json-get "url" (ende-protocol-json-get "image_url" image))) parts)) images)
    (nreverse parts)))

(provide 'ende-content)
;;; ende-content.el ends here
