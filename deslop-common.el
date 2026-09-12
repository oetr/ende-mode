;;; deslop-common.el --- Shared definitions and utilities for deslop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools

;;; Commentary:

;; Shared configurations, faces, prompts, and context helpers for deslop.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)

(defgroup deslop nil
  "Local-LLM text assistance: insert at cursor, improve, summarize."
  :prefix "deslop-"
  :group 'convenience)

;;;; User Options

(defcustom deslop-url "http://localhost:8080/v1/chat/completions"
  "OpenAI-compatible chat completions endpoint."
  :type 'string
  :group 'deslop)

(defcustom deslop-model "qwen3.8-27b"
  "Model name sent to the LLM server."
  :type 'string
  :group 'deslop)

(defcustom deslop-temperature 0.2
  "Sampling temperature for LLM requests."
  :type 'number
  :group 'deslop)

(defcustom deslop-default-provider 'local
  "Provider profile used for newly queued jobs."
  :type 'symbol
  :group 'deslop)

(defcustom deslop-provider-profiles nil
  "Additional provider profiles keyed by symbol.

Each value is a plist.  Secrets must not be stored here; use auth-source
fields :auth-source-host, :auth-source-user, and :auth-source-port instead.
An optional :display-name overrides endpoint-reported provider names for the
UI."
  :type 'sexp
  :group 'deslop)

(defcustom deslop-connect-timeout 10
  "Maximum seconds to wait while establishing an LLM connection."
  :type 'number
  :group 'deslop)

(defcustom deslop-request-timeout 300
  "Maximum seconds allowed for one LLM request."
  :type 'number
  :group 'deslop)

(defcustom deslop-max-context-chars 24000
  "Maximum number of characters sent to the LLM as context.
Tokens are estimated as chars/4."
  :type 'integer
  :group 'deslop)

(defcustom deslop-max-request-chars 32000
  "Maximum combined characters in all messages sent for one request."
  :type 'integer
  :group 'deslop)

(defcustom deslop-window-chars 8000
  "Characters kept on each side of the cursor when context is elided."
  :type 'integer
  :group 'deslop)

(defcustom deslop-max-output-tokens 4096
  "Maximum completion tokens requested from an LLM provider."
  :type 'integer
  :group 'deslop)

(defcustom deslop-max-response-bytes (* 4 1024 1024)
  "Maximum number of response-body bytes accepted from one LLM request."
  :type 'integer
  :group 'deslop)

(defcustom deslop-max-sse-event-bytes (* 512 1024)
  "Maximum number of bytes accepted in one SSE line or event."
  :type 'integer
  :group 'deslop)

(defcustom deslop-max-output-chars (* 512 1024)
  "Maximum combined reasoning and completion characters retained per request."
  :type 'integer
  :group 'deslop)

(defcustom deslop-max-discovery-bytes (* 512 1024)
  "Maximum response-body bytes accepted from provider discovery."
  :type 'integer
  :group 'deslop)

(defcustom deslop-max-response-header-bytes (* 64 1024)
  "Maximum response-header bytes parsed from curl's header output."
  :type 'integer
  :group 'deslop)

(defcustom deslop-max-error-body-bytes 8192
  "Maximum response bytes retained for provider error diagnostics."
  :type 'integer
  :group 'deslop)

(defcustom deslop-allow-environment-proxy nil
  "Non-nil to allow curl to honor ambient proxy environment variables.
Loopback requests always bypass proxies."
  :type 'boolean
  :group 'deslop)

(defcustom deslop-insert-instruction
  "Write the most natural continuation of the text at the cursor position."
  "Instruction given to the LLM for deslop-insert-at-cursor."
  :type 'string
  :group 'deslop)

(defcustom deslop-history-enabled t
  "Non-nil when completed jobs are retained in a history buffer."
  :type 'boolean
  :group 'deslop)

(defcustom deslop-history-retain-prompts nil
  "Non-nil when history entries retain complete prompts and source context."
  :type 'boolean
  :group 'deslop)

(defcustom deslop-history-retain-thinking nil
  "Non-nil when history entries retain provider reasoning traces."
  :type 'boolean
  :group 'deslop)

(defcustom deslop-history-max-entries 100
  "Maximum number of completed jobs retained in the history buffer."
  :type 'integer
  :group 'deslop)

(defcustom deslop-history-max-chars (* 2 1024 1024)
  "Maximum approximate size of the retained history buffer."
  :type 'integer
  :group 'deslop)

(defcustom deslop-provider-discovery-cache-size 64
  "Maximum number of provider discovery results retained in memory."
  :type 'integer
  :group 'deslop)

(defcustom deslop-history-buffer-name "*deslop-history*"
  "Name of the buffer used for retained LLM job history."
  :type 'string
  :group 'deslop)

(defcustom deslop-summary-buffer-name "*deslop-summary*"
  "Name of the buffer used for summaries."
  :type 'string
  :group 'deslop)

(defcustom deslop-reduced-motion nil
  "Non-nil to disable the animated overlay spinner."
  :type 'boolean
  :group 'deslop)

(defcustom deslop-review-highlight t
  "Non-nil when pending LLM output is tinted with a soft highlight.
The highlight fixes the foreground to the face in effect at the insertion
point and shifts its background slightly toward that foreground."
  :type 'boolean
  :group 'deslop)

(defcustom deslop-review-highlight-blend 0.5
  "Fraction of the foreground color blended into the review highlight background."
  :type 'number
  :group 'deslop)

;;;; Faces

(defface deslop-working-face
  '((t :underline (:color "#38bdf8" :style line)))
  "Face applied to buffer text regions currently being processed by LLM."
  :group 'deslop)

(defface deslop-badge-face
  '((t :foreground "#facc15" :weight bold))
  "Face used for deslop progress badges."
  :group 'deslop)

;;;; Errors and provider profiles

(cl-defstruct (deslop-error (:constructor deslop-error-create))
  "Structured error passed between the transport and job scheduler."
  kind message details)

(defun deslop--safe-message-string (value)
  "Return VALUE as a bounded, single-line diagnostic string."
  (let ((text (replace-regexp-in-string
               "[[:cntrl:]]+" " " (format "%s" value))))
    (if (> (length text) 1000)
        (concat (substring text 0 997) "...")
      text)))

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

(defun deslop-provider-profile (&optional name)
  "Return a non-secret provider profile for NAME.

The legacy URL/model customizations remain the local-provider defaults.
OpenRouter is available as a profile template and requires an auth-source
entry before it can be dispatched."
  (let* ((name (or name deslop-default-provider))
         (base
          (pcase name
            ('openrouter
             '(:url "https://openrouter.ai/api/v1/chat/completions"
               :model "openai/gpt-4o-mini"
                :temperature 0.2
                :auth-source-host "openrouter.ai"
                :auth-source-user "deslop"
                :auth-source-port 443
                :require-sse t))
            (_
             (list :url deslop-url
                   :model deslop-model
                   :temperature deslop-temperature
                   :require-sse t))))
         (custom (alist-get name deslop-provider-profiles)))
    (deslop--plist-merge
     (plist-put (copy-sequence base) :name name)
     custom)))

(defun deslop-provider-profile-snapshot (&optional name)
  "Return a detached, non-secret snapshot of provider profile NAME."
  (let ((snapshot (copy-tree (deslop-provider-profile name))))
    (dolist (key '(:token :api-key :secret :authorization :auth-token))
      (setq snapshot (plist-put snapshot key nil)))
    snapshot))

(defun deslop-provider--friendly-name (name)
  "Return a friendly display spelling for provider NAME."
  (if (not (stringp name))
      (format "%s" name)
    (let ((downcased (downcase name)))
      (cond
       ((member downcased '("openai" "openai.com")) "OpenAI")
       ((member downcased '("openrouter" "openrouter.ai")) "OpenRouter")
       ((member downcased '("llama.cpp" "llama-cpp")) "llama.cpp")
       (t name)))))

(defun deslop-provider-client-name (profile &optional metadata)
  "Return a concise display name for provider PROFILE.
METADATA may contain a provider/client reported by the endpoint.  Explicit
profile names and known service hosts take precedence over that report; local
hosts and IP addresses otherwise retain their host spelling."
  (let* ((url (plist-get profile :url))
         (parsed (and (stringp url) (url-generic-parse-url url)))
         (host (and parsed (url-host parsed)))
         (downcased (and host (downcase host)))
         (reported (and metadata
                        (or (plist-get metadata :client)
                            (plist-get metadata :provider)
                            (plist-get metadata :owner))))
         (explicit (plist-get profile :display-name)))
    (cond
     ((and (stringp explicit) (not (string-empty-p explicit)))
      (deslop-provider--friendly-name explicit))
     ((and downcased (string= downcased "openrouter.ai")) "OpenRouter")
     ((and downcased
           (string-match-p "\\(?:\\`\\|\\.\\)openai\\.com\\'"
                           downcased))
      "OpenAI")
     ((and (stringp reported) (not (string-empty-p reported)))
      (deslop-provider--friendly-name reported))
     ((null host)
      (deslop-provider--friendly-name
       (format "%s" (or (plist-get profile :name) "provider"))))
     (t host))))

(defun deslop-provider-display-metadata (profile &optional metadata)
  "Return display CLIENT and MODEL values for PROFILE and METADATA."
  (list :client (deslop-provider-client-name profile metadata)
        :model (or (plist-get metadata :model)
                   (plist-get profile :model))))

;;;; System Prompts

(defconst deslop-insert-system
  "You are an inline code and text completion engine. You will receive a document containing a <CURSOR> marker indicating where your output will be inserted.

### Output Constraints
- Output ONLY the exact text to be inserted at <CURSOR>. 
- Never wrap your response in markdown code blocks (```) or add explanatory commentary.
- Output text VERBATIM. Use real line breaks for multi-line completions, never literal \"\\n\" strings.

### Completion Rules
1. No Duplication: Do not repeat words, code, or symbols that already exist immediately before or after <CURSOR>.
2. Comment Awareness: If <CURSOR> sits directly after a comment marker (e.g., `;;`, `//`, `#`), do NOT start your output with another comment marker. If your completion spans multiple lines inside a comment block, prefix only the new subsequent lines with the appropriate comment symbol.
3. No Empty Responses: Do not return an empty string. If the cursor sits at a natural boundary, insert a single space or a logical continuation phrase.

### Examples

Input:
;; <CURSOR>refactor this later
Output:
TODO: 

Input:
// <CURSOR>
// next line
Output:
first line

Input:
def calculate_total(price, tax):
    <CURSOR>
Output:
return price * (1 + tax)"
  "System prompt instructions for cursor completion.")

(defconst deslop-improve-system
  "You improve text while preserving its intent and useful information. Return ONLY the improved text, without markdown fences or commentary."
  "System prompt instructions for improving a target region.")

(defconst deslop-summarize-system
  "You summarize the supplied text accurately and concisely. Return ONLY the summary, without markdown fences or commentary."
  "System prompt instructions for summarizing a target region.")

;;;; String & Context Utilities

(defun deslop-whitespace-p (c)
  "Non-nil if C is a whitespace character."
  (memq c '(32 9 10 13 11 12)))

(defun deslop-trim-trailing (s)
  "Remove trailing newlines from S, preserving inline trailing spaces."
  (let ((end (length s)))
    (while (and (> end 0) (memq (aref s (1- end)) '(?\n ?\r)))
      (setq end (1- end)))
    (substring s 0 end)))

(defun deslop-clean-whitespace (s)
  "Trim whitespace at both ends of S."
  (let ((start 0)
        (end (length s)))
    (while (and (< start end) (deslop-whitespace-p (aref s start)))
      (setq start (1+ start)))
    (while (and (> end start) (deslop-whitespace-p (aref s (1- end))))
      (setq end (1- end)))
    (substring s start end)))

(defun deslop-context-window (length offset)
  "Return bounded (START . END) offsets around OFFSET in text of LENGTH.
The returned source span never exceeds `deslop-max-context-chars', and each
side is additionally capped by `deslop-window-chars'."
  (unless (and (integerp deslop-max-context-chars)
               (> deslop-max-context-chars 0))
    (error "deslop-max-context-chars must be a positive integer"))
  (unless (and (integerp deslop-window-chars)
               (>= deslop-window-chars 0))
    (error "deslop-window-chars must be a nonnegative integer"))
  (setq offset (max 0 (min length offset)))
  (if (<= length deslop-max-context-chars)
      (cons 0 length)
    (let* ((left-cap (min offset deslop-window-chars))
           (right-cap (min (- length offset) deslop-window-chars))
           (budget deslop-max-context-chars)
           (left (min left-cap (/ budget 2)))
           (right (min right-cap (- budget left)))
           (remaining (- budget left right))
           (add-left (min remaining (- left-cap left))))
      (setq left (+ left add-left))
      (setq remaining (- remaining add-left))
      (setq right (+ right (min remaining (- right-cap right))))
      (cons (- offset left) (+ offset right)))))

(defun deslop-format-marker-context
    (before after omitted-before omitted-after)
  "Format BEFORE and AFTER around a cursor, marking omitted sides."
  (concat
   (when omitted-before "...[characters elided]...\n")
   before
   "<CURSOR>"
   after
   (when omitted-after "\n...[characters elided]...")))

(defun deslop-marker-context (text offset)
  "Return TEXT with a <CURSOR> marker at OFFSET.
If TEXT exceeds `deslop-max-context-chars', keep a window of
`deslop-window-chars' characters on each side of OFFSET and mark the cut
points with elision markers."
  (setq offset (max 0 (min (length text) offset)))
  (let* ((bounds (deslop-context-window (length text) offset))
         (start (car bounds))
         (end (cdr bounds)))
    (deslop-format-marker-context
     (substring text start offset)
     (substring text offset end)
     (> start 0)
     (< end (length text)))))

(defun deslop-current-region ()
  "Return (START . END) of the active region, or nil."
  (when (use-region-p)
    (cons (region-beginning) (region-end))))

(provide 'deslop-common)

;;; deslop-common.el ends here
;; 
