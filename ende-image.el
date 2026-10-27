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
  "Model used by the opt-in generate_image tool.
Nil disables generation even when the tool is activated."
  :type '(choice (const nil) string)
  :group 'ende-image)

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

(defun ende-image--request (prompt callback register)
  "Generate PROMPT asynchronously, calling CALLBACK with parts or an error.
REGISTER attaches cancellation to the existing tool lifecycle."
  (unless (and (stringp ende-image-model) (not (string-blank-p ende-image-model)))
    (user-error "Configure an image model with M-x ende-image-configure"))
  (let* ((profile (ende-image--profile))
         (url (plist-get profile :url))
         (data (json-encode `((:model . ,(plist-get profile :model)) (:prompt . ,prompt))))
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
  (ende-tools--assert-properties arguments '("prompt") "generate_image")
  (let* ((prompt (ende-tools--required-string arguments "prompt"))
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
         prompt
         (lambda (parts error)
           (if error (funcall callback nil error)
             (when (ende-job-execution-current-p job generation)
               (condition-case nil
                   (let (names)
                     (dolist (part parts)
                       (push (ende-artifact-store base id (plist-get part :bytes)
                                                  (plist-get part :media-type)) names))
                     (setq names (nreverse names))
                     (setf (ende-job-instruction job)
                           (plist-put (ende-job-instruction job) :generated-artifacts
                                      (append (plist-get (ende-job-instruction job) :generated-artifacts) names)))
                     (funcall callback (list :ok t :artifact (car names) :status "created") nil))
                 (error (funcall callback nil "Generated image could not be validated or stored"))))))
         ende-tools--register-cancellation-function)))))

(ende-tool-register
 'generate_image
 "Generate an image from a prompt using the user-configured image model. Return an artifact filename; emit #+ENDE_ARTIFACT: FILENAME to attach it."
 '((:type . "object")
   (:properties . ((:prompt . ((:type . "string") (:minLength . 1)))))
   (:required . ["prompt"]) (:additionalProperties . :json-false))
 #'ende-image--generate :write :no-resource
 '(:requires-root nil :requires-resources nil :async t))

(provide 'ende-image)
;;; ende-image.el ends here
