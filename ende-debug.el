;;; ende-debug.el --- LLM request diagnostics for ende -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools, llm

;;; Commentary:

;; Opt-in recording of the exact request bodies and metadata sent by Ende's
;; streaming LLM transports.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url-parse)
(require 'ende-core)

(defvar ende-debugging-enabled nil
  "Non-nil when new streaming LLM requests should be recorded.")

(defvar ende-debug--session-directory nil
  "Directory holding the latest Ende debugging session.")

(defvar ende-debug--sequence 0
  "Next request sequence number in the active debugging session.")

(defun ende-debug--timestamp (&optional time)
  "Format TIME as a local ISO-like timestamp, or the current time."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z" time))

(defun ende-debug--new-session-directory ()
  "Create and return a private, unique debugging session directory."
  (make-directory user-emacs-directory t)
  (let* ((prefix (format-time-string "ende-debug-%Y%m%d-%H%M%S-"))
         (path (make-temp-name (expand-file-name prefix user-emacs-directory))))
    (let ((default-file-modes #o700))
      (make-directory path))
    (set-file-modes path #o700)
    path))

(defun ende-debugging-on ()
  "Enable recording of subsequent LLM requests in a new private session."
  (interactive)
  (setq ende-debug--session-directory (ende-debug--new-session-directory)
        ende-debug--sequence 0
        ende-debugging-enabled t)
  (message "ende debugging enabled; recordings: %s"
           ende-debug--session-directory))

(defun ende-debugging-off ()
  "Disable recording of new LLM requests."
  (interactive)
  (setq ende-debugging-enabled nil)
  (message "ende debugging disabled; existing recordings remain in %s"
           (or ende-debug--session-directory "(no debugging session)")))

(defun ende-debug--safe-endpoint (endpoint)
  "Return a credential-free endpoint identification for ENDPOINT."
  (when (stringp endpoint)
    (condition-case nil
        (let* ((parsed (url-generic-parse-url endpoint))
               (scheme (url-type parsed))
               (host (url-host parsed))
               (port (url-port parsed))
               (path (or (url-filename parsed) "/")))
          (setq path (car (split-string path "[?#]")))
          (when (and scheme host)
            (concat scheme "://" host
                    (if (and port
                             (not (or (and (equal scheme "https") (= port 443))
                                      (and (equal scheme "http") (= port 80)))))
                        (format ":%d" port)
                      "")
                    (if (string-empty-p path) "/" path))))
      (error nil))))

(defun ende-debug--request-settings (request-body)
  "Return non-conversation settings from serialized REQUEST-BODY."
  (cl-loop for (key . value) in request-body
           unless (memq key '(:messages :input :instructions :tools :model))
           collect (cons key value)))

(defun ende-debug--write-file (path contents)
  "Write CONTENTS literally to PATH with owner-only permissions."
  (let ((default-file-modes #o600)
        (coding-system-for-write 'no-conversion))
    (write-region contents nil path nil 'silent))
  (set-file-modes path #o600))

(defun ende-debug--write-meta (record)
  "Write RECORD's metadata to its private JSON file."
  (let* ((directory (plist-get record :directory))
         (stem (plist-get record :stem))
         (meta (plist-get record :metadata)))
    (ende-debug--write-file
     (expand-file-name (concat stem "-meta.json") directory)
     (concat (json-encode meta) "\n"))))

(defun ende-debug-record-completion (record outcome usage http-status value)
  "Write RECORD's terminal OUTCOME, normalized USAGE, and HTTP-STATUS.
VALUE is the successful result or structured transport error.  Recording
errors are reported but never propagated into transport completion."
  (when record
    (condition-case failure
        (let* ((counts (ende--usage-token-counts usage))
               (error-kind (and (ende-error-p value)
                                (ende-error-kind value)))
               (state (cond
                       ((eq outcome 'success) "success")
                       ((eq error-kind 'cancelled) "cancelled")
                       (t "failure")))
               (completed (float-time))
               (completion
                `(("outcome" . ,state)
                  ("error_kind" . ,(and error-kind (symbol-name error-kind)))
                  ("http_status" . ,http-status)
                  ("completed_timestamp" . ,(ende-debug--timestamp))
                  ("completion_time_epoch" . ,completed)
                  ("elapsed_seconds" . ,(- completed
                                            (plist-get record :started)))
                  ("input_tokens" . ,(plist-get counts :input))
                  ("output_tokens" . ,(plist-get counts :output))
                  ("cached_input_tokens" . ,(plist-get counts :cached))
                  ("cached_tokens_reported" . ,(if (numberp
                                                    (plist-get counts :cached))
                                                   t json-false))
                  ("cache_write_tokens" . ,(plist-get counts :cache-write))
                  ("cache_write_tokens_reported" . ,(if (numberp
                                                        (plist-get counts
                                                                   :cache-write))
                                                       t json-false))))
               (metadata (plist-get record :metadata)))
          (setf (plist-get record :metadata)
                (append (cl-remove-if
                         (lambda (entry)
                           (equal (car entry) "completion"))
                         metadata)
                        (list (cons "completion" completion))))
          (ende-debug--write-meta record))
       ((error quit)
        (message "ende debugging: could not record completion: %s"
                (error-message-string failure))))))

(defun ende-debug-record-request
    (serialized-body request-body messages profile transport endpoint
     &optional session-id)
  "Record exact SERIALIZED-BODY and internal MESSAGES for a request.
REQUEST-BODY is the encoded request object used to describe its settings.
PROFILE, TRANSPORT, and ENDPOINT describe the transport.  SESSION-ID is the
Codex session-id header value when applicable.  Return an opaque record used
to associate later completion metadata, or nil when debugging is disabled."
  (when (and ende-debugging-enabled ende-debug--session-directory)
    (condition-case failure
        (let* ((sequence (cl-incf ende-debug--sequence))
               (stem (format "%04d" sequence))
               (directory ende-debug--session-directory)
               (started (float-time))
               (reasoning
                (cl-loop for (key . value) in request-body
                         when (memq key '(:reasoning :reasoning_effort))
                         collect (cons key value)))
               (metadata
                `(("recording_id" . ,(format "%s-%s"
                                             (file-name-nondirectory
                                              (directory-file-name directory))
                                             stem))
                  ("sequence" . ,sequence)
                  ("timestamp" . ,(ende-debug--timestamp))
                  ("start_time_epoch" . ,started)
                  ("job_id" . ,(plist-get profile :job-id))
                  ("transport" . ,transport)
                  ("provider" . ,(format "%s"
                                         (or (plist-get profile :name)
                                             transport)))
                  ("model" . ,(or (alist-get :model request-body)
                                  (plist-get profile :model)))
                  ("reasoning_effort" . ,(or (plist-get profile :reasoning-effort)
                                               (alist-get :reasoning_effort
                                                          request-body)))
                  ("reasoning_settings" . ,reasoning)
                  ("request_settings" . ,(ende-debug--request-settings
                                            request-body))
                  ("endpoint" . ,(ende-debug--safe-endpoint endpoint))
                  ("codex_session_id" . ,session-id)))
               (record (list :directory directory :stem stem
                             :started started :metadata metadata)))
          (ende-debug--write-file
           (expand-file-name (concat stem "-request.json") directory)
           serialized-body)
          (ende-debug--write-file
           (expand-file-name (concat stem "-messages.el") directory)
           (let ((print-circle t)
                 (print-level nil)
                 (print-length nil))
             (concat (prin1-to-string messages) "\n")))
          (ende-debug--write-meta record)
          record)
      ((error quit)
       (message "ende debugging: could not record request: %s"
                (error-message-string failure))
       nil))))

(provide 'ende-debug)

;;; ende-debug.el ends here
