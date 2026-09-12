;;; deslop-core.el --- Core definitions and utilities for deslop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter Samarin
;; Author: Peter Samarin
;; Keywords: convenience, tools

;;; Commentary:

;; Package-wide configuration, errors, and generic resource helpers.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup deslop nil
  "Local-LLM text assistance: insert at cursor, improve, summarize."
  :prefix "deslop-"
  :group 'convenience)

;;;; Transport and request resource options

(defcustom deslop-connect-timeout nil
  "Maximum seconds to wait while establishing an LLM connection."
  :type '(choice (const :tag "No connection deadline" nil)
                 (number :tag "Maximum seconds"))
  :group 'deslop)

(defcustom deslop-request-timeout nil
  "Maximum total seconds allowed for one LLM request.

When nil, an active stream has no total wall-clock deadline."
  :type '(choice (const :tag "No total deadline" nil)
                 (number :tag "Maximum seconds"))
  :group 'deslop)

(defcustom deslop-response-idle-timeout 60
  "Maximum seconds with no response data before an LLM request fails."
  :type 'number
  :group 'deslop)

(defcustom deslop-max-request-chars nil
  "Maximum combined characters in all messages sent for one request.

When nil, do not impose a request-size limit."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Maximum characters"))
  :group 'deslop)

(defcustom deslop-max-output-tokens nil
  "Maximum completion tokens requested from an LLM provider.

When nil, omit the request limit and let the provider use its own maximum."
  :type '(choice (const :tag "Provider default" nil)
                 (integer :tag "Maximum tokens"))
  :group 'deslop)

(defcustom deslop-max-response-bytes nil
  "Maximum number of response-body bytes accepted from one LLM request.

When nil, do not impose a response-size limit."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Maximum bytes"))
  :group 'deslop)

(defcustom deslop-max-sse-event-bytes nil
  "Maximum number of bytes accepted in one SSE line or event.

When nil, do not impose an SSE event-size limit."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Maximum bytes"))
  :group 'deslop)

(defcustom deslop-max-output-chars nil
  "Maximum combined reasoning and completion characters retained per request.

When nil, do not impose a parsed-output character limit."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Maximum characters"))
  :group 'deslop)

(defcustom deslop-max-response-header-bytes nil
  "Maximum response-header bytes parsed from curl's header output.

When nil, do not impose a response-header size limit."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Maximum bytes"))
  :group 'deslop)

(defcustom deslop-max-error-body-bytes nil
  "Maximum response bytes retained for provider error diagnostics.

When nil, retain the complete error response."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Maximum bytes"))
  :group 'deslop)

(defcustom deslop-allow-environment-proxy nil
  "Non-nil to allow curl to honor ambient proxy environment variables.
Loopback requests always bypass proxies."
  :type 'boolean
  :group 'deslop)

;;;; Shared face

(defface deslop-badge-face
  '((t :foreground "#facc15" :weight bold))
  "Face used for deslop badges."
  :group 'deslop)

;;;; Errors and generic utilities

(cl-defstruct (deslop-error (:constructor deslop-error-create))
  "Structured error passed between the transport and job scheduler."
  kind message details)

(defun deslop--ignore-errors (function &rest arguments)
  "Call FUNCTION with ARGUMENTS while suppressing cleanup failures and quits."
  (condition-case nil
      (apply function arguments)
    ((error quit) nil)))

(defun deslop--bounded-single-line (value max-length &optional trim)
  "Return VALUE as a bounded, single-line string of MAX-LENGTH.
When TRIM is non-nil, remove surrounding whitespace before bounding the
result.  The returned string uses an ellipsis when it is truncated."
  (when value
    (let ((text (replace-regexp-in-string
                 "[[:cntrl:]]+" " " (format "%s" value))))
      (when trim
        (setq text (string-trim text)))
      (unless (and trim (string-empty-p text))
        (if (> (length text) max-length)
            (concat (substring text 0 (- max-length 3)) "...")
          text)))))

(defun deslop--owned-buffer (current name owner-variable initializer)
  "Return an owned buffer named NAME, initializing it with INITIALIZER.
CURRENT is the package's existing buffer object, OWNER-VARIABLE is the
buffer-local ownership marker, and INITIALIZER is called in a new or reused
owned buffer."
  (if (buffer-live-p current)
      current
    (let* ((existing (get-buffer name))
           (buffer (if (and existing
                            (not (buffer-local-value owner-variable existing)))
                       (generate-new-buffer name)
                     (get-buffer-create name))))
      (with-current-buffer buffer
        (funcall initializer)
        (set (make-local-variable owner-variable) t))
      buffer)))

(defun deslop--safe-message-string (value)
  "Return VALUE as a bounded, single-line diagnostic string."
  (deslop--bounded-single-line value 1000))

(defun deslop-error-display-message (error)
  "Return a displayable message for structured or ordinary ERROR."
  (cond
   ((deslop-error-p error)
    (let ((kind (deslop-error-kind error))
          (message (deslop--safe-message-string
                    (or (deslop-error-message error) "unknown error"))))
      (if kind
          (format "%s: %s" kind message)
        message)))
   ((stringp error) (deslop--safe-message-string error))
   ((and (consp error) (eq (car error) 'error))
    (condition-case nil
        (deslop--safe-message-string (error-message-string error))
      (error (deslop--safe-message-string error))))
   (t (deslop--safe-message-string error))))

(defun deslop--plist-merge (base overrides)
  "Return BASE plist with OVERRIDES plist values applied."
  (let ((result (copy-sequence base))
        (rest overrides))
    (while rest
      (setq result (plist-put result (pop rest) (pop rest))))
    result))

(provide 'deslop-core)

;;; deslop-core.el ends here
