;;; ende-transport.el --- Shared transport helpers for ende -*- lexical-binding: t; -*-

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

;; Small, protocol-independent helpers shared by streaming transports.

;;; Code:

(require 'cl-lib)
(require 'ende-core)
(require 'ende-protocol)

(cl-defstruct (ende-transport-session
               (:constructor ende-transport--make-session))
  "Shared lifecycle state for one asynchronous transport request."
  callbacks callback-error-handler process process-buffer stderr-buffer
  stderr-process headers-file config-file terminal auxiliary-cancellers)

(defun ende-transport-session-create (callbacks callback-error-handler)
  "Create a transport session for CALLBACKS and CALLBACK-ERROR-HANDLER."
  (ende-transport--make-session
   :callbacks callbacks
   :callback-error-handler callback-error-handler))

(defun ende-transport-session-invoke (session name &rest arguments)
  "Invoke callback NAME for SESSION with ARGUMENTS, safely."
  (when (plist-get (ende-transport-session-callbacks session) name)
    (condition-case callback-error
        (apply (plist-get (ende-transport-session-callbacks session) name)
               arguments)
      ((error quit)
       (unless (eq name :on-error)
         (let ((handler
                (ende-transport-session-callback-error-handler session)))
           (when handler
             (funcall
              handler
              (ende-error-create
               :kind 'callback
               :message (error-message-string callback-error))))))))))

(defun ende-transport-session-add-canceller (session cancel)
  "Register CANCEL as an auxiliary resource owned by SESSION."
  (when (functionp cancel)
    (if (ende-transport-session-terminal session)
        (ende-ignore-errors cancel)
      (push cancel (ende-transport-session-auxiliary-cancellers session)))))

(defun ende-transport-session-set-resources (session request)
  "Copy transport resource handles from REQUEST into SESSION."
  (setf (ende-transport-session-process session)
        (plist-get request :process)
        (ende-transport-session-process-buffer session)
        (plist-get request :response-buffer)
        (ende-transport-session-stderr-buffer session)
        (plist-get request :stderr-buffer)
        (ende-transport-session-stderr-process session)
        (plist-get request :stderr-process)
        (ende-transport-session-headers-file session)
        (plist-get request :headers-file)
        (ende-transport-session-config-file session)
        (plist-get request :config-file)))

(defun ende-transport-cleanup-resources (buffers files &optional processes)
  "Kill BUFFERS, delete FILES, and terminate PROCESSES safely."
  (dolist (process processes)
    (when (processp process)
      (ende-ignore-errors #'delete-process process)))
  (dolist (buffer buffers)
    (when (buffer-live-p buffer)
      (ende-ignore-errors #'kill-buffer buffer)))
  (dolist (file files)
    (when (and file (file-exists-p file))
      (ende-ignore-errors #'delete-file file))))

(defun ende-transport-session-cleanup (session)
  "Release all process resources owned by SESSION."
  (let ((cancellers (ende-transport-session-auxiliary-cancellers session)))
    (setf (ende-transport-session-auxiliary-cancellers session) nil)
    (dolist (cancel cancellers)
      (ende-ignore-errors cancel)))
  (ende-transport-cleanup-resources
   (list (ende-transport-session-process-buffer session)
         (ende-transport-session-stderr-buffer session))
   (list (ende-transport-session-headers-file session)
         (ende-transport-session-config-file session))
   (list (ende-transport-session-process session)
         (ende-transport-session-stderr-process session)))
  (setf (ende-transport-session-process session) nil
        (ende-transport-session-process-buffer session) nil
        (ende-transport-session-stderr-buffer session) nil
        (ende-transport-session-stderr-process session) nil
        (ende-transport-session-headers-file session) nil
        (ende-transport-session-config-file session) nil))

(defun ende-transport-session-transfer (session)
  "Mark SESSION transferred to another transport and release its callbacks."
  (unless (ende-transport-session-terminal session)
    (setf (ende-transport-session-terminal session) 'transferred
          (ende-transport-session-callbacks session) nil)))

(defun ende-transport-session-settle
    (session outcome value &optional partial-result)
  "Settle SESSION once and dispatch VALUE with optional PARTIAL-RESULT."
  (unless (ende-transport-session-terminal session)
    (setf (ende-transport-session-terminal session) outcome)
    (let ((process (ende-transport-session-process session)))
      (when (processp process)
        (process-put process :ende-settled t)
        (when (process-live-p process)
          (ende-ignore-errors #'delete-process process))))
    (ende-transport-session-cleanup session)
    (unwind-protect
        (progn
          (when partial-result
            (ende-transport-session-invoke
             session :on-event
             (list :type :partial-result :result partial-result)))
          (if (eq outcome 'success)
              (ende-transport-session-invoke session :on-done value)
            (ende-transport-session-invoke
             session :on-event
             (list :type :error :error value :result partial-result))
            (ende-transport-session-invoke session :on-error value)))
      (setf (ende-transport-session-callbacks session) nil))))

(defun ende-transport-byte-prefix (text max-bytes)
  "Return the longest character prefix of TEXT within MAX-BYTES."
  (if (<= (string-bytes text) max-bytes)
      text
    (let ((low 0)
          (high (length text)))
      (while (< low high)
        (let ((middle (/ (+ low high 1) 2)))
          (if (<= (string-bytes (substring text 0 middle)) max-bytes)
              (setq low middle)
            (setq high (1- middle)))))
      (substring text 0 low))))

(defun ende-transport-safe-error-text (text)
  "Prevent private provider payloads from entering error messages."
  (when (and (stringp text)
             (let ((case-fold-search t))
               (string-match-p
                "encrypted_content\\|prompt_cache_key\\|session[-_]id\\|authorization\\|\\bbearer[[:space:]]+\\|\\(?:access\\|refresh\\|id\\)[-_]token"
                text)))
    (setq text "Provider error contained private replay or cache data (payload redacted)"))
  (if (and (stringp text)
           (let ((case-fold-search t))
             (string-match-p "data:image/\\|b64_json\\|image_generation_call" text)))
      "Provider error contained image data (payload redacted)"
    text))

(defun ende-transport-error-detail (preview)
  "Return a useful provider error detail parsed from PREVIEW, or nil."
  (condition-case nil
      (let* ((preview (ende-transport-safe-error-text preview))
             (payload (ende-protocol-json-read-string preview))
             (error-payload (ende-protocol-json-get "error" payload)))
        (or (ende-protocol-json-get "message" error-payload)
            (ende-protocol-json-get "detail" error-payload)
            (ende-protocol-json-get "message" payload)
            (ende-protocol-json-get "detail" payload)
            (and (stringp error-payload) error-payload)))
    (error nil)))

(provide 'ende-transport)

;;; ende-transport.el ends here
