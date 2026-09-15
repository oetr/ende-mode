;;; deslop-transport.el --- Shared transport helpers for deslop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools, llm

;;; Commentary:

;; Small, protocol-independent helpers shared by streaming transports.

;;; Code:

(require 'cl-lib)
(require 'deslop-core)
(require 'deslop-protocol)

(cl-defstruct (deslop-transport-session
               (:constructor deslop-transport--make-session))
  "Shared lifecycle state for one asynchronous transport request."
  callbacks callback-error-handler process process-buffer stderr-buffer
  stderr-process headers-file config-file terminal)

(defun deslop-transport-session-create (callbacks callback-error-handler)
  "Create a transport session for CALLBACKS and CALLBACK-ERROR-HANDLER."
  (deslop-transport--make-session
   :callbacks callbacks
   :callback-error-handler callback-error-handler))

(defun deslop-transport-session-invoke (session name &rest arguments)
  "Invoke callback NAME for SESSION with ARGUMENTS, safely."
  (when (plist-get (deslop-transport-session-callbacks session) name)
    (condition-case callback-error
        (apply (plist-get (deslop-transport-session-callbacks session) name)
               arguments)
      ((error quit)
       (unless (eq name :on-error)
         (let ((handler
                (deslop-transport-session-callback-error-handler session)))
           (when handler
             (funcall
              handler
              (deslop-error-create
               :kind 'callback
               :message (error-message-string callback-error))))))))))

(defun deslop-transport-session-set-resources (session request)
  "Copy transport resource handles from REQUEST into SESSION."
  (setf (deslop-transport-session-process session)
        (plist-get request :process)
        (deslop-transport-session-process-buffer session)
        (plist-get request :response-buffer)
        (deslop-transport-session-stderr-buffer session)
        (plist-get request :stderr-buffer)
        (deslop-transport-session-stderr-process session)
        (plist-get request :stderr-process)
        (deslop-transport-session-headers-file session)
        (plist-get request :headers-file)
        (deslop-transport-session-config-file session)
        (plist-get request :config-file)))

(defun deslop-transport-cleanup-resources (buffers files &optional processes)
  "Kill BUFFERS, delete FILES, and terminate PROCESSES safely."
  (dolist (process processes)
    (when (processp process)
      (deslop-ignore-errors #'delete-process process)))
  (dolist (buffer buffers)
    (when (buffer-live-p buffer)
      (deslop-ignore-errors #'kill-buffer buffer)))
  (dolist (file files)
    (when (and file (file-exists-p file))
      (deslop-ignore-errors #'delete-file file))))

(defun deslop-transport-session-cleanup (session)
  "Release all process resources owned by SESSION."
  (deslop-transport-cleanup-resources
   (list (deslop-transport-session-process-buffer session)
         (deslop-transport-session-stderr-buffer session))
   (list (deslop-transport-session-headers-file session)
         (deslop-transport-session-config-file session))
   (list (deslop-transport-session-process session)
         (deslop-transport-session-stderr-process session)))
  (setf (deslop-transport-session-process session) nil
        (deslop-transport-session-process-buffer session) nil
        (deslop-transport-session-stderr-buffer session) nil
        (deslop-transport-session-stderr-process session) nil
        (deslop-transport-session-headers-file session) nil
        (deslop-transport-session-config-file session) nil))

(defun deslop-transport-session-settle
    (session outcome value &optional partial-result)
  "Settle SESSION once and dispatch VALUE with optional PARTIAL-RESULT."
  (unless (deslop-transport-session-terminal session)
    (setf (deslop-transport-session-terminal session) outcome)
    (let ((process (deslop-transport-session-process session)))
      (when (processp process)
        (process-put process :deslop-settled t)
        (when (process-live-p process)
          (deslop-ignore-errors #'delete-process process))))
    (deslop-transport-session-cleanup session)
    (unwind-protect
        (progn
          (when partial-result
            (deslop-transport-session-invoke
             session :on-event
             (list :type :partial-result :result partial-result)))
          (if (eq outcome 'success)
              (deslop-transport-session-invoke session :on-done value)
            (deslop-transport-session-invoke
             session :on-event
             (list :type :error :error value :result partial-result))
            (deslop-transport-session-invoke session :on-error value)))
      (setf (deslop-transport-session-callbacks session) nil))))

(defun deslop-transport-byte-prefix (text max-bytes)
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

(defun deslop-transport-error-detail (preview)
  "Return a useful provider error detail parsed from PREVIEW, or nil."
  (condition-case nil
      (let* ((payload (deslop-protocol-json-read-string preview))
             (error-payload (deslop-protocol-json-get "error" payload)))
        (or (deslop-protocol-json-get "message" error-payload)
            (deslop-protocol-json-get "detail" error-payload)
            (deslop-protocol-json-get "message" payload)
            (deslop-protocol-json-get "detail" payload)
            (and (stringp error-payload) error-payload)))
    (error nil)))

(provide 'deslop-transport)

;;; deslop-transport.el ends here
