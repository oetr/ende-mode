;;; ende-image.el --- Optional asynchronous image generation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter Samarin
;;
;; Author: Peter Samarin <peter.samarin@gmail.com>
;; Maintainer: Peter Samarin <peter.samarin@gmail.com>
;; Keywords: convenience, tools
;; URL: https://github.com/oetr/ende-mode
;; SPDX-License-Identifier: AGPL-3.0-only

;; This file is part of Ende.

;; Ende is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, version 3 of the License.
;;
;; Ende is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with Ende.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Optional asynchronous image generation for Ende.

;;; Code:
(require 'ende-artifact)
(require 'ende-content)
(require 'ende-tools)
(require 'ende-provider)
(require 'ende-queue)
(require 'ende-curl)
(require 'ende-transport)
(require 'ende-worker)

(declare-function ende-chat--build-document "ende-chat")
(declare-function ende-chat--find-by-property "ende-chat")
(declare-function ende-chat--root "ende-chat")
(declare-function ende-chat--property "ende-chat")

(defgroup ende-image nil
  "Image generation."
  :group 'ende-artifact)

(defcustom ende-image-api 'openai
  "API used to encode image requests and decode responses."
  :type '(choice (const :tag "OpenAI-compatible" openai)
                 (const :tag "Stable Diffusion" stable-diffusion))
  :group 'ende-image)

(defcustom ende-image-auth-profile 'openrouter
  "Optional provider profile supplying authentication and headers."
  :type '(choice (const :tag "None" nil) symbol)
  :group 'ende-image)

(defcustom ende-image-endpoint "https://openrouter.ai/api/v1/images"
  "Images API endpoint returning inline base64 image data."
  :type 'string
  :group 'ende-image)

(defcustom ende-image-model nil
  "Image model to request, or nil to use the server default."
  :type '(choice (const :tag "Server default" nil) string)
  :group 'ende-image)

(defconst ende-image--common-properties
  '((:prompt . ((:type . "string") (:minLength . 1)))
    (:negative_prompt . ((:type . "string")))
    (:width . ((:type . "integer") (:minimum . 1)))
    (:height . ((:type . "integer") (:minimum . 1)))
    (:seed . ((:type . "integer")))
    (:steps . ((:type . "integer") (:minimum . 1)))
    (:cfg_scale . ((:type . "number"))))
  "Generation fields shared by image APIs.")

(defconst ende-image--apis
  '((openai
     :name "OpenAI-compatible" :model t
     :images-key "data" :decode-image ende-image--openai-image
     :properties
     ((:sample_method . ((:type . "string") (:minLength . 1)))
      (:batch_count . ((:type . "integer") (:minimum . 1)))))
    (stable-diffusion
     :name "Stable Diffusion"
     :images-key "images" :decode-image ende-content-generated-image
     :properties
     ((:sampler_name . ((:type . "string") (:minLength . 1)))
      (:scheduler . ((:type . "string") (:minLength . 1)))
      (:batch_size . ((:type . "integer") (:minimum . 1))))))
  "Image API schemas and response adapters.")

(defun ende-image--adapter (&optional api)
  "Return the adapter for API, defaulting to the selected API."
  (or (alist-get (or api ende-image-api) ende-image--apis)
      (user-error "Unknown image API: %s" (or api ende-image-api))))

(defun ende-image--request-properties (adapter)
  "Return generation properties supported by ADAPTER."
  (append ende-image--common-properties (plist-get adapter :properties)))

(defun ende-image--schema (_configuration)
  "Return the selected image API's tool schema."
  `((:type . "object")
    (:properties . ,(append (ende-image--request-properties (ende-image--adapter))
                           '((:caption . ((:type . "string"))))))
    (:required . ["prompt"]) (:additionalProperties . :json-false)))

(defun ende-image--auth-profile-names ()
  "Return profiles available for image authentication and headers."
  (delete-dups (append '(openrouter) (ende-provider-names)
                       (mapcar #'car ende-custom-providers)
                       (mapcar #'car ende-builtin-provider-settings))))

(defun ende-image--profile ()
  "Return a validated snapshot of the image request settings."
  (let* ((adapter (ende-image--adapter))
         (model (and (plist-get adapter :model) ende-image-model)))
    (when (and ende-image-auth-profile
               (not (memq ende-image-auth-profile (ende-image--auth-profile-names))))
      (user-error "Select an available authentication profile"))
    (unless (or (null model)
                (and (stringp model) (not (string-blank-p model))))
      (user-error "Image model must be a nonblank string or nil"))
    (let ((profile (ende-plist-merge
                    (when ende-image-auth-profile
                      (ende-provider-profile-snapshot ende-image-auth-profile))
                    (list :url ende-image-endpoint :model model
                          :image-api ende-image-api))))
      (when (memq (plist-get profile :auth-method) '(chatgpt-plan-oauth codex-oauth))
        (user-error "Images API requires an auth-source or unauthenticated profile"))
      (ende-curl--validated-url profile)
      profile)))

(defconst ende-image--configuration-variables
  '(ende-image-api ende-image-auth-profile ende-image-endpoint ende-image-model)
  "Image settings edited in the configuration buffer.")

(defvar ende-image-configure-mode-map (make-sparse-keymap)
  "Keymap for `ende-image-configure-mode'.")

(define-key ende-image-configure-mode-map (kbd "a") #'ende-image--configure-api)
(define-key ende-image-configure-mode-map (kbd "p") #'ende-image--configure-auth-profile)
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
        (adapter (ende-image--adapter))
        (encrypted (equal (downcase (or (url-type (url-generic-parse-url
                                                 ende-image-endpoint)) ""))
                          "https")))
    (erase-buffer)
    (insert (format "%-8s - %-18s %s\n" "a" "API:" (plist-get adapter :name))
            (format "%-8s - %-18s %s\n" "p" "Authentication:"
                    (or ende-image-auth-profile "None"))
            (format "%-8s - %-18s %s\n" "e" "Endpoint:" ende-image-endpoint)
            (format "%-8s - %-18s " "" "Encryption:")
            (propertize (if encrypted
                            "On - TLS with certificate verification"
                          "Off - prompt and image sent unencrypted!")
                        'face (if encrypted 'success 'error))
            "\n"
            (format "%-8s - %-18s %s\n"
                    (if (plist-get adapter :model) "m" "") "Model:"
                    (if (plist-get adapter :model)
                        (or ende-image-model "Server default")
                      "Server-loaded"))
            "\n"
            (format "%-8s - Quit without saving\n" "q/Esc")
            (format "%-8s - Save and quit\n" "Enter"))))

(defun ende-image--configure-api ()
  "Choose the image API without changing connection settings."
  (interactive)
  (let ((choices (mapcar (lambda (entry)
                          (cons (plist-get (cdr entry) :name) (car entry)))
                        ende-image--apis)))
    (setq ende-image-api
          (cdr (assoc (completing-read "Image API: " choices nil t nil nil
                                       (plist-get (ende-image--adapter) :name))
                      choices))))
  (ende-image--render-configuration))

(defun ende-image--configure-auth-profile ()
  "Choose an optional profile for image authentication and headers."
  (interactive)
  (let ((choices (cons '("None")
                       (mapcar (lambda (name) (cons (symbol-name name) name))
                               (ende-image--auth-profile-names)))))
    (setq ende-image-auth-profile
          (cdr (assoc (completing-read "Authentication profile: " choices nil t nil nil
                                       (if ende-image-auth-profile
                                           (symbol-name ende-image-auth-profile) "None"))
                      choices))))
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
  (unless (plist-get (ende-image--adapter) :model)
    (user-error "This API uses the server-loaded model"))
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
  (let* ((adapter (ende-image--adapter (plist-get profile :image-api)))
         (properties (ende-image--request-properties adapter))
         (fields (mapcar (lambda (entry) (ende-tools--property-name (car entry)))
                         properties))
         (body (ende-tools--json-object arguments fields)))
    (ende-tools--assert-properties arguments (cons "caption" fields) "generate_image")
    (ende-tools--required-string arguments "prompt")
    (dolist (entry properties)
      (let* ((name (ende-tools--property-name (car entry)))
             (schema (cdr entry))
             (value (gethash name body :absent)))
        (unless (eq value :absent)
          (unless (and (pcase (alist-get :type schema)
                         ("string" (stringp value))
                         ("integer" (integerp value))
                         ("number" (numberp value)))
                       (or (null (alist-get :minimum schema))
                           (>= value (alist-get :minimum schema)))
                       (or (null (alist-get :minLength schema))
                           (>= (length value) (alist-get :minLength schema))))
            (error "Invalid generate_image argument %s" name)))))
    (when-let* ((model (and (plist-get adapter :model) (plist-get profile :model))))
      (puthash "model" model body))
    body))

(defun ende-image--openai-image (image)
  "Decode one inline image object from the Images API."
  (ende-content-generated-image (gethash "b64_json" image)
                                (gethash "media_type" image)))

(defun ende-image--response (body profile)
  "Normalize image response BODY using PROFILE's API."
  (let* ((adapter (ende-image--adapter (plist-get profile :image-api)))
         (name (plist-get adapter :name))
         (key (plist-get adapter :images-key))
         (images (and (hash-table-p body) (gethash key body))))
    (unless (and (or (vectorp images) (listp images)) (> (length images) 0))
      (error "%s response is missing a nonempty %s array" name key))
    (condition-case nil
        (mapcar (plist-get adapter :decode-image) images)
      (error (error "%s returned unsupported or malformed image data; expected PNG or JPEG"
                    name)))))

(defun ende-image--store-response (file api directory)
  "Decode FILE for API and store validated images in DIRECTORY."
  (let ((body (with-temp-buffer
                (insert-file-contents file)
                (json-parse-buffer :object-type 'hash-table))))
    (mapcar
     (lambda (part)
       (let ((name (ende-artifact-store directory "images" (plist-get part :bytes)
                                        (plist-get part :media-type))))
         (ende-artifact-resolve directory "images" name)))
     (ende-image--response body (list :image-api api)))))

(defun ende-image--request (arguments profile base id callback register)
  "Generate from ARGUMENTS using PROFILE into BASE and ID.
Call CALLBACK with artifact names or an error.
REGISTER attaches cancellation to the existing tool lifecycle."
  (let* ((url (plist-get profile :url))
         (data (json-encode (ende-image--request-body arguments profile)))
         (lifecycle (ende-transport-session-create nil nil))
         (directory (make-temp-file (expand-file-name ".pending-" (ende-artifact-directory base id t)) t))
         (file (expand-file-name "response.json" directory))
         (settled nil) (processing nil))
    (cl-labels
        ((finish (names error)
           (unless settled
             (setq settled t)
             (setf (ende-transport-session-terminal lifecycle) t)
             (ende-transport-session-cleanup lifecycle)
             (ende-ignore-errors #'delete-directory directory t)
             (when register (funcall register nil))
             (funcall callback names error)))
         (import-images (images error)
           (unless settled
             (if error (finish nil error)
               (condition-case failure
                   (finish
                    (ende-artifact--call-with-transaction
                     (lambda ()
                       (mapcar (lambda (info)
                                 (ende-artifact-import base id (plist-get info :path) info))
                               images))) nil)
                 (error (finish nil (error-message-string failure)))))))
         (receive (process _event)
           (when (and (not settled) (not processing)
                      (memq (process-status process) '(exit signal)))
             (setq processing t)
             (condition-case failure
                 (let ((status (car (ende-curl-response-headers
                                     (ende-transport-session-headers-file lifecycle)))))
                   (unless (and (zerop (process-exit-status process))
                                status (<= 200 status) (< status 300))
                     (error "Image request failed"))
                   (when (and ende-max-response-bytes
                              (> (file-attribute-size (file-attributes file)) ende-max-response-bytes))
                     (error "Generated image response exceeds ende-max-response-bytes"))
                   (ende-transport-session-add-canceller
                    lifecycle
                    (ende-worker-run
                     'ende-image #'ende-image--store-response
                     (list file (plist-get profile :image-api) directory) nil #'import-images)))
               (error (finish nil (error-message-string failure)))))))
      (condition-case failure
          (let ((request
                 (ende-curl-start-request
                  :profile profile :url url :name "ende-generate-image"
                  :output-file file
                  :command (lambda (headers config)
                             (ende-curl-json-command profile headers config "POST"
                                                     "application/json" t url))
                  :filter #'ignore
                  :sentinel #'receive)))
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
         arguments (plist-get (ende-job-profile job) :image-profile) base id
         (lambda (names error)
           (if error (funcall callback nil error)
             (when (ende-job-execution-current-p job generation)
               (condition-case nil
                   (let* ((instruction (ende-job-instruction job))
                          (artifacts (mapcar (lambda (name) (list :name name :caption caption)) names)))
                     (setf (ende-job-instruction job)
                           (plist-put instruction :generated-artifacts
                                      (append (plist-get instruction :generated-artifacts) artifacts)))
                     (funcall
                      callback
                      (list :ok t
                            :artifacts
                            (vconcat names))
                      nil))
                 (error (funcall callback nil "Generated image could not be validated or stored"))))))
         ende-tools--register-cancellation-function)))))

(ende-tool-register
 'generate_image
 "Generate one or more images from a prompt using the user-configured image backend. Sampling options depend on that backend; omitted generation options use server defaults. Caption is optional local presentation metadata, never sent to the backend; an omitted or empty caption means no displayed caption. The result contains artifact filenames in the artifacts array. Emit one #+ENDE_ARTIFACT: FILENAME marker for each image being attached."
 #'ende-image--schema
 #'ende-image--generate :write :no-resource
 '(:requires-root nil :requires-resources nil :async t))

(provide 'ende-image)
;;; ende-image.el ends here
