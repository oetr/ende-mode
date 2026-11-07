;;; ende-core.el --- Core definitions and utilities for ende -*- lexical-binding: t; -*-

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

;; Package-wide configuration, errors, and generic resource helpers.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup ende nil
  "Local-LLM text assistance: insert at cursor, improve, summarize."
  :prefix "ende-"
  :group 'convenience)


(defcustom ende-tool-diagnostics t
  "Non-nil to retain tool diagnostics in job history for every provider.

Diagnostics show the tool names sent and response item types.  They omit tool
arguments and response content."
  :type 'boolean
  :group 'ende)

(defconst ende-client-name "ende-mode"
  "Client name advertised in outbound HTTP requests.")

(defconst ende-client-version "1.0.0"
  "Ende version advertised to providers that require a client version.")

;;;; Transport and request resource options

(defcustom ende-connect-timeout nil
  "Maximum seconds to wait while establishing an LLM connection."
  :type '(choice (const :tag "No connection deadline" nil)
                 (number :tag "Maximum seconds"))
  :group 'ende)

(defcustom ende-request-timeout nil
  "Maximum total seconds allowed for one LLM request.

When nil, an active stream has no total wall-clock deadline."
  :type '(choice (const :tag "No total deadline" nil)
                 (number :tag "Maximum seconds"))
  :group 'ende)

(defcustom ende-response-idle-timeout 300
  "Maximum seconds without response-body data before a request fails.

The wait starts when the curl process starts and resets on each nonempty
response chunk, including SSE heartbeat comments.  Headers alone do not
reset it.  A server that sends nothing during prompt prefill can therefore
reach this limit before generating output.  Increase it for slow prefill.

The value is captured when each request starts; changes affect new requests."
  :type 'number
  :group 'ende)

(defcustom ende-max-request-chars nil
  "Maximum combined characters in all messages sent for one request.

When nil, do not impose a request-size limit."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Maximum characters"))
  :group 'ende)

(defcustom ende-source-name-disclosure t
  "Non-nil to include source file or buffer names in operation prompts.

A provider profile can override this setting with its
`:source-name-disclosure' value.  Source type and major mode are included
regardless of this setting."
  :type 'boolean
  :group 'ende)

(defcustom ende-default-output-tokens nil
  "Fallback requested output budget for generation requests.

This is not model capacity.  Nil leaves the requested budget unspecified."
  :type '(choice (const :tag "Provider default" nil)
                 (integer :tag "Maximum tokens"))
  :group 'ende)

(defcustom ende-max-response-bytes (* 256 1024 1024)
  "Maximum number of response-body bytes accepted from one LLM request.

The default is deliberately generous so slow or verbose local inference is not
treated as an ordinary product limit.  Set it to nil to disable the breaker."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Maximum bytes"))
  :group 'ende)

(defcustom ende-max-sse-event-bytes (* 16 1024 1024)
  "Maximum number of bytes accepted in one SSE line or event.

The default is deliberately generous.  Set it to nil to disable the breaker."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Maximum bytes"))
  :group 'ende)

(defcustom ende-max-output-chars 128000000
  "Maximum combined reasoning and completion characters retained per request.

The default is deliberately generous.  Set it to nil to disable the breaker."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Maximum characters"))
  :group 'ende)

(defcustom ende-max-response-header-bytes (* 1 1024 1024)
  "Maximum response-header bytes parsed from curl's header output.

The default is a safety breaker rather than an ordinary response limit.  Set it
to nil to disable the breaker."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Maximum bytes"))
  :group 'ende)

(defcustom ende-max-error-body-bytes (* 1 1024 1024)
  "Maximum response bytes retained for provider error diagnostics.

The default limits only diagnostic previews, not successful response content.
Set it to nil to retain complete error responses."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Maximum bytes"))
  :group 'ende)

(defcustom ende-max-oauth-refresh-bytes (* 1 1024 1024)
  "Maximum response-body bytes accepted from an OAuth refresh request."
  :type 'integer
  :group 'ende)

(defcustom ende-max-stderr-bytes (* 16 1024 1024)
  "Maximum stderr bytes retained from one curl process."
  :type 'integer
  :group 'ende)

(defcustom ende-provider-max-discovery-bytes (* 16 1024 1024)
  "Maximum response-body bytes accepted from provider model discovery."
  :type 'integer
  :group 'ende)

(defcustom ende-allow-environment-proxy nil
  "Non-nil to allow curl to honor ambient proxy environment variables.
Loopback requests always bypass proxies."
  :type 'boolean
  :group 'ende)

;;;; Shared face

(defface ende-badge-face
  '((t :foreground "#facc15" :weight bold))
  "Face used for ende badges."
  :group 'ende)

;;;; Errors and generic utilities

(defun ende-duration-ms (started &optional finished)
  "Return elapsed milliseconds from STARTED to FINISHED, or now."
  (when started
    (max 0 (round (* 1000 (- (or finished (float-time)) started))))))

(defun ende-trim-trailing (text)
  "Remove trailing newlines from TEXT, preserving inline trailing spaces."
  (string-trim-right text "[\n\r]+"))

(defun ende--usage-get (object key)
  "Return KEY from provider usage OBJECT.
KEY is a keyword for plist objects and a string key in JSON hash tables."
  (cond
   ((hash-table-p object)
    (gethash (substring (symbol-name key) 1) object))
   ((listp object) (plist-get object key))))

(defun ende--usage-number (usage &rest keys)
  "Return the first numeric field in USAGE named by KEYS."
  (cl-loop for key in keys
           for value = (ende--usage-get usage key)
           when (numberp value) return value))

(defun ende--usage-token-counts (usage)
  "Return normalized input, output, cached, cache-write, and total counts.
Unknown counts are nil.  Total is derived from input and output when absent."
  (let* ((input (ende--usage-number usage :input_tokens :prompt_tokens))
         (output (ende--usage-number usage :output_tokens :completion_tokens))
         (total (or (ende--usage-number usage :total_tokens)
                    (and (numberp input) (numberp output)
                         (+ input output))))
         (cached (or (ende--usage-number
                      (ende--usage-get usage :input_tokens_details)
                      :cached_tokens)
                     (ende--usage-number
                      (ende--usage-get usage :prompt_tokens_details)
                      :cached_tokens)
                     (ende--usage-number usage :prompt_cache_hit_tokens)))
         (cache-write
          (or (ende--usage-number
               (ende--usage-get usage :input_tokens_details)
               :cache_write_tokens :cache_creation_input_tokens
               :cache_creation_tokens)
              (ende--usage-number
               (ende--usage-get usage :prompt_tokens_details)
               :cache_write_tokens :cache_creation_input_tokens
               :cache_creation_tokens)
              (ende--usage-number usage :cache_write_tokens
                                  :cache_write_input_tokens
                                  :cache_creation_input_tokens
                                  :cache_creation_tokens))))
    (list :input input :output output :cached cached
          :cache-write cache-write :total total)))

(defun ende-validate-resource-options ()
  "Validate transport resource options before starting external processes."
  (dolist (option (list (cons 'ende-connect-timeout ende-connect-timeout)
                        (cons 'ende-request-timeout ende-request-timeout)))
    (unless (or (null (cdr option))
                (and (numberp (cdr option)) (> (cdr option) 0)))
      (error "%s must be nil or a positive number" (car option))))
  (unless (and (numberp ende-response-idle-timeout)
               (> ende-response-idle-timeout 0))
    (error "ende-response-idle-timeout must be a positive number"))
  (dolist (option
           (list (cons 'ende-default-output-tokens ende-default-output-tokens)
                 (cons 'ende-max-response-bytes ende-max-response-bytes)
                 (cons 'ende-max-sse-event-bytes ende-max-sse-event-bytes)
                 (cons 'ende-max-output-chars ende-max-output-chars)
                 (cons 'ende-max-response-header-bytes
                       ende-max-response-header-bytes)
                 (cons 'ende-max-error-body-bytes
                       ende-max-error-body-bytes)
                 (cons 'ende-max-oauth-refresh-bytes
                       ende-max-oauth-refresh-bytes)
                 (cons 'ende-max-stderr-bytes ende-max-stderr-bytes)
                 (cons 'ende-provider-max-discovery-bytes
                       ende-provider-max-discovery-bytes)))
    (unless (or (null (cdr option))
                (and (integerp (cdr option)) (> (cdr option) 0)))
      (error "%s must be nil or a positive integer" (car option))))
  t)

(cl-defstruct (ende-error (:constructor ende-error-create))
  "Structured error passed between the transport and job scheduler."
  kind message details)

(defun ende-ignore-errors (function &rest arguments)
  "Call FUNCTION with ARGUMENTS while suppressing cleanup failures and quits.

This is the package-wide boundary for best-effort cleanup callbacks."
  (condition-case nil
      (apply function arguments)
    ((error quit) nil)))

(defun ende-bounded-single-line (value max-length &optional trim)
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

(defun ende-owned-buffer (current name owner-variable initializer)
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

(defun ende--safe-message-string (value)
  "Return VALUE as a bounded, single-line diagnostic string."
  (ende-bounded-single-line value 1000))

(defun ende-error-display-message (error)
  "Return a displayable message for structured or ordinary ERROR."
  (cond
   ((ende-error-p error)
    (let ((kind (ende-error-kind error))
          (message (ende--safe-message-string
                    (or (ende-error-message error) "unknown error"))))
      (if kind
          (format "%s: %s" kind message)
        message)))
   ((stringp error) (ende--safe-message-string error))
   ((and (consp error) (symbolp (car error))
         (memq 'error (get (car error) 'error-conditions)))
    (condition-case nil
        (ende--safe-message-string
         (if (and (stringp (cadr error)) (null (cddr error)))
             (cadr error)
           (error-message-string error)))
      (error (ende--safe-message-string error))))
   (t (ende--safe-message-string error))))

(defun ende-plist-merge (base overrides)
  "Return BASE plist with OVERRIDES plist values applied."
  (let ((result (copy-sequence base))
        (rest overrides))
    (while rest
      (setq result (plist-put result (pop rest) (pop rest))))
    result))

(provide 'ende-core)

;;; ende-core.el ends here
