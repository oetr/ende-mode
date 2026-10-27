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

(defcustom ende-image-model nil
  "OpenRouter Images model used by the opt-in generate_image tool.
Nil disables generation even when the tool is activated."
  :type '(choice (const nil) string) :group 'ende-artifact)

(defun ende-image--request (prompt callback register)
  "Generate PROMPT asynchronously, calling CALLBACK with parts or an error.
REGISTER attaches cancellation to the existing tool lifecycle."
  (unless (and (stringp ende-image-model) (not (string-blank-p ende-image-model)))
    (error "Configure ende-image-model before using generate_image"))
  (let* ((url "https://openrouter.ai/api/v1/images")
         (profile (plist-put (ende-provider-profile-snapshot 'openrouter) :url url))
         (data (json-encode `((:model . ,ende-image-model) (:prompt . ,prompt)
                              (:n . 1) (:output_format . "png"))))
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
                            (mapc (lambda (image)
                                    (push (ende-content-generated-image
                                           (gethash "b64_json" image) (gethash "media_type" image)) parts)) images)
                            (finish (nreverse parts) nil))
                        (error (finish nil "OpenRouter image generation failed or returned invalid image data"))))))))
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
