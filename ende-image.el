;;; ende-image.el --- Optional asynchronous image generation -*- lexical-binding: t; -*-

;;; Code:
(require 'ende-artifact)
(require 'ende-content)
(require 'ende-tools)
(require 'ende-provider)
(require 'ende-queue)
(require 'ende-curl)
(require 'ende-transport)

(declare-function ende-chat--build-document "ende-chat")
(declare-function ende-chat--find-by-property "ende-chat")
(declare-function ende-chat--root "ende-chat")
(declare-function ende-chat--property "ende-chat")

(defgroup ende-image nil
  "Image generation."
  :group 'ende-artifact)

(defcustom ende-image-provider 'openrouter
  "Provider profile supplying image request authentication and headers."
  :type 'symbol
  :group 'ende-image)

(defcustom ende-image-endpoint "https://openrouter.ai/api/v1/images"
  "Images API endpoint returning inline base64 image data."
  :type 'string
  :group 'ende-image)

(defcustom ende-image-model nil
  "Image model to request, or nil to use the server default."
  :type '(choice (const :tag "Server default" nil) string)
  :group 'ende-image)

(defconst ende-image--request-properties
  '((:prompt . ((:type . "string") (:minLength . 1)))
    (:negative_prompt . ((:type . "string")))
    (:width . ((:type . "integer") (:minimum . 1)))
    (:height . ((:type . "integer") (:minimum . 1)))
    (:seed . ((:type . "integer")))
    (:steps . ((:type . "integer") (:minimum . 1)))
    (:cfg_scale . ((:type . "number")))
    (:sample_method . ((:type . "string") (:minLength . 1)
                       (:description . "Backend sampler name.")))
    (:batch_count . ((:type . "integer") (:minimum . 1)
                     (:description . "Number of images to request; requires server support."))))
  "Generation fields accepted by the image tool.")

(defun ende-image--provider-names ()
  "Return available provider names, including image-only profiles."
  (delete-dups (append '(openrouter local) (ende-provider-names)
                       (mapcar #'car ende-provider-profiles))))

(defun ende-image--profile ()
  "Return a validated snapshot of the image request settings."
  (unless (and ende-image-provider
               (memq ende-image-provider (ende-image--provider-names)))
    (user-error "Select an available image provider profile"))
  (unless (or (null ende-image-model)
              (and (stringp ende-image-model)
                   (not (string-blank-p ende-image-model))))
    (user-error "Image model must be a nonblank string or nil"))
  (let ((profile (ende-plist-merge
                  (ende-provider-profile-snapshot ende-image-provider)
                  (list :url ende-image-endpoint :model ende-image-model
                        :allow-http t))))
    (when (plist-get profile :auth-method)
      (user-error "Images API requires an auth-source or unauthenticated provider profile"))
    (ende-curl--validated-url profile)
    profile))

(defconst ende-image--configuration-variables
  '(ende-image-provider ende-image-endpoint ende-image-model)
  "Image settings edited in the configuration buffer.")

(defvar ende-image-configure-mode-map (make-sparse-keymap)
  "Keymap for `ende-image-configure-mode'.")

(define-key ende-image-configure-mode-map (kbd "p") #'ende-image--configure-provider)
(define-key ende-image-configure-mode-map (kbd "e") #'ende-image--configure-endpoint)
(define-key ende-image-configure-mode-map (kbd "m") #'ende-image--configure-model)
(define-key ende-image-configure-mode-map (kbd "RET") #'ende-image--configure-save)
(define-key ende-image-configure-mode-map (kbd "q") #'ende-image--configure-quit)
(define-key ende-image-configure-mode-map (kbd "<escape>") #'ende-image--configure-quit)

(define-derived-mode ende-image-configure-mode special-mode "Ende-Image-Configure"
  "Read-only settings view for image generation."
  (dolist (variable ende-image--configuration-variables)
    (set (make-local-variable variable) (default-value variable))))

(defun ende-image--render-configuration ()
  "Render the image settings in the current configuration buffer."
  (let ((inhibit-read-only t)
        (encrypted (equal (downcase (or (url-type (url-generic-parse-url
                                                 ende-image-endpoint)) ""))
                          "https")))
    (erase-buffer)
    (insert (format "%-8s - %-18s %s\n" "p" "Provider profile:" ende-image-provider)
            (format "%-8s - %-18s %s\n" "e" "Endpoint:" ende-image-endpoint)
            (format "%-8s - %-18s " "" "Encryption:")
            (propertize (if encrypted
                            "On - TLS with certificate verification"
                          "Off - prompt and image sent unencrypted!")
                        'face (if encrypted 'success 'error))
            "\n"
            (format "%-8s - %-18s %s\n" "m" "Model:" (or ende-image-model "Server default"))
            "\n"
            (format "%-8s - Quit without saving\n" "q/Esc")
            (format "%-8s - Save and quit\n" "Enter"))))

(defun ende-image--configure-provider ()
  "Choose the provider profile for image authentication and headers."
  (interactive)
  (setq ende-image-provider
        (intern (completing-read "Image provider profile: "
                                 (mapcar #'symbol-name (ende-image--provider-names))
                                 nil t nil nil (symbol-name ende-image-provider))))
  (ende-image--render-configuration))

(defun ende-image--configure-endpoint ()
  "Set the Images API endpoint."
  (interactive)
  (let ((endpoint (string-trim (read-string "Image endpoint: " ende-image-endpoint))))
    (ende-curl--validated-url (list :url endpoint :allow-http t))
    (setq ende-image-endpoint endpoint))
  (ende-image--render-configuration))

(defun ende-image--configure-model ()
  "Set the image model, or use the server default with an empty value."
  (interactive)
  (let ((model (string-trim (read-string "Image model (empty uses server default): "
                                       ende-image-model))))
    (setq ende-image-model (unless (string-empty-p model) model)))
  (ende-image--render-configuration))

(defun ende-image--configure-save ()
  "Apply pending image settings for this Emacs session and close the buffer."
  (interactive)
  (ende-image--profile)
  (dolist (variable ende-image--configuration-variables)
    (set-default variable (symbol-value variable)))
  (quit-window t))

(defun ende-image--configure-quit ()
  "Discard pending image settings and close the configuration buffer."
  (interactive)
  (quit-window t))

(defun ende-image-configure ()
  "Open the image generation configuration buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*ende-image-configure*")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'ende-image-configure-mode)
        (ende-image-configure-mode))
      (ende-image--render-configuration))
    (pop-to-buffer buffer)))

(defun ende-image--request-body (arguments profile)
  "Return a validated image request body for ARGUMENTS and PROFILE."
  (let* ((fields
          (mapcar (lambda (entry)
                    (ende-tools--property-name (car entry)))
                  ende-image--request-properties))
         (body (ende-tools--json-object arguments fields)))
    (ende-tools--assert-properties
     arguments (cons "caption" fields) "generate_image")
    (ende-tools--required-string arguments "prompt")
    (maphash
     (lambda (name value)
       (unless
           (pcase name
             ((or "width" "height" "steps" "batch_count")
              (and (integerp value) (> value 0)))
             ("seed" (integerp value))
             ("cfg_scale" (numberp value))
             ("sample_method"
              (and (stringp value) (not (string-empty-p value))))
             (_ (stringp value)))
         (error "Invalid generate_image argument %s" name)))
     body)
    (when-let* ((model (plist-get profile :model)))
      (puthash "model" model body))
    body))

(defun ende-image--request (arguments callback register)
  "Generate from ARGUMENTS, calling CALLBACK with parts or an error.
REGISTER attaches cancellation to the existing tool lifecycle."
  (let* ((profile (ende-image--profile))
         (url (plist-get profile :url))
         (data (json-encode (ende-image--request-body arguments profile)))
         (lifecycle (ende-transport-session-create nil nil))
         (chunks nil) (size 0) (settled nil))
    (cl-labels
        ((finish (parts error)
           (unless settled
             (setq settled t chunks nil)
             (setf (ende-transport-session-terminal lifecycle) t)
             (ende-transport-session-cleanup lifecycle)
             (when register (funcall register nil))
             (funcall callback parts error))))
      (condition-case failure
          (let ((request
                 (ende-curl-start-request
                  :profile profile :url url :name "ende-generate-image"
                  :command (lambda (headers config)
                             (ende-curl-json-command profile headers config "POST"
                                                     "application/json" t url))
                  :filter (lambda (_process chunk)
                            (cl-incf size (string-bytes chunk))
                            (if (and ende-max-response-bytes (> size ende-max-response-bytes))
                                (finish nil "Generated image response exceeds ende-max-response-bytes")
                              (push chunk chunks)))
                  :sentinel
                  (lambda (process _event)
                    (when (and (not settled) (memq (process-status process) '(exit signal)))
                      (condition-case nil
                          (let* ((status (car (ende-curl-response-headers
                                               (ende-transport-session-headers-file lifecycle))))
                                 (body (and (zerop (process-exit-status process))
                                            status (<= 200 status) (< status 300)
                                            (json-parse-string (apply #'concat (nreverse chunks))
                                                               :object-type 'hash-table)))
                                 (images (and body (gethash "data" body)))
                                 parts)
                            (unless (and (or (vectorp images) (listp images)) (> (length images) 0))
                              (error "Image request failed"))
                            (condition-case nil
                                (progn
                                  (mapc (lambda (image)
                                          (push (ende-content-generated-image
                                                 (gethash "b64_json" image)
                                                 (gethash "media_type" image)) parts)) images)
                                  (finish (nreverse parts) nil))
                              (error (finish nil "Backend returned unsupported or malformed image data; expected PNG or JPEG"))))
                        (error (finish nil "Image generation failed or returned invalid image data"))))))))
            (ende-transport-session-set-resources lifecycle request)
            (when register (funcall register (lambda () (finish nil "Image generation cancelled"))))
            (unless settled
              (process-send-string (plist-get request :process) data)
              (process-send-eof (plist-get request :process))))
        (error (finish nil (error-message-string failure)))))))

(defun ende-image--generate (arguments _root callback)
  "Generate only into the current conversation; never accept model paths."
  (let* ((caption (ende-artifact--caption (ende-tools--argument arguments "caption")))
         (job (and ende-tools--execution-owner
                   (ende-queue-live-job-by-id ende-tools--execution-owner)))
         (buffer (and job (ende-target-buffer (ende-job-target job))))
         (generation (and job (ende-job-execution-generation job))))
    (unless (and job (eq (ende-job-type job) 'chat) (buffer-live-p buffer))
      (error "generate_image requires an active Org conversation"))
    (with-current-buffer buffer
      (let* ((instruction (ende-job-instruction job))
             (document (ende-chat--build-document))
             (question (ende-chat--find-by-property document "ENDE_CHAT_ID"
                                                    (plist-get instruction :question-id)))
             (id (ende-chat--property document (ende-chat--root document question) :id))
             (base (ende-artifact-base)))
        (ende-image--request
         arguments
         (lambda (parts error)
           (if error (funcall callback nil error)
             (when (ende-job-execution-current-p job generation)
               (condition-case nil
                   (let (artifacts)
                     (dolist (part parts)
                       (push (list :name (ende-artifact-store base id (plist-get part :bytes)
                                                             (plist-get part :media-type))
                                   :caption caption) artifacts))
                     (setq artifacts (nreverse artifacts))
                     (setf (ende-job-instruction job)
                           (plist-put (ende-job-instruction job) :generated-artifacts
                                      (append (plist-get (ende-job-instruction job) :generated-artifacts) artifacts)))
                     (funcall callback (list :ok t :artifact (plist-get (car artifacts) :name)
                                             :status "created") nil))
                 (error (funcall callback nil "Generated image could not be validated or stored"))))))
         ende-tools--register-cancellation-function)))))

(ende-tool-register
 'generate_image
 "Generate an image from a prompt using the user-configured image model. Caption is optional; an omitted or empty caption means no displayed caption. Return an artifact filename; emit #+ENDE_ARTIFACT: FILENAME to attach it."
 `((:type . "object")
   (:description . "Optional generation fields are backend-dependent; omitted fields use server defaults.")
   (:properties . ,(append ende-image--request-properties
                          '((:caption . ((:type . "string"))))))
   (:required . ["prompt"]) (:additionalProperties . :json-false))
 #'ende-image--generate :write :no-resource
 '(:requires-root nil :requires-resources nil :async t))

(provide 'ende-image)
;;; ende-image.el ends here
