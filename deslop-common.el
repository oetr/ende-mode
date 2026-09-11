;;; deslop-common.el --- Shared definitions and utilities for deslop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools

;;; Commentary:

;; Shared configurations, faces, prompts, and context helpers for deslop.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

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
fields such as :auth-source-host and :auth-source-user instead."
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

(defcustom deslop-window-chars 8000
  "Characters kept on each side of the cursor when context is elided."
  :type 'integer
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

;;;; Faces

(defface deslop-working-face
  '((t :underline (:color "#38bdf8" :style line)))
  "Face applied to buffer text regions currently being processed by LLM."
  :group 'deslop)

(defface deslop-badge-face
  '((t :foreground "#facc15" :weight bold))
  "Face used for deslop progress badges."
  :group 'deslop)

(defface deslop-review-face
  '((t :background "#5a4a20"))
  "Face applied to LLM output awaiting user approval."
  :group 'deslop)

;;;; Errors and provider profiles

(cl-defstruct (deslop-error (:constructor deslop-error-create))
  "Structured error passed between the transport and job scheduler."
  kind message details)

(defun deslop-error-display-message (error)
  "Return a displayable message for structured or ordinary ERROR."
  (cond
   ((deslop-error-p error)
    (let ((kind (deslop-error-kind error))
          (message (deslop-error-message error)))
      (if (and kind (not (string-empty-p message)))
          (format "%s: %s" kind message)
        message)))
   ((stringp error) error)
   ((and (consp error) (eq (car error) 'error))
    (error-message-string error))
   (t (format "%s" error))))

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

(defun deslop-marker-context (text offset)
  "Return TEXT with a <CURSOR> marker at OFFSET.
If TEXT exceeds `deslop-max-context-chars', keep a window of
`deslop-window-chars' characters on each side of OFFSET and mark the cut
points with elision markers."
  (setq offset (max 0 (min (length text) offset)))
  (if (<= (length text) deslop-max-context-chars)
      (concat (substring text 0 offset) "<CURSOR>" (substring text offset))
    (let* ((w deslop-window-chars)
           (bs (max 0 (- offset w)))
           (be (min (length text) (+ offset w)))
           (win (substring text bs be))
           (co (- offset bs)))
      (concat
       (when (> bs 0) (format "...[%d characters elided]...\n" bs))
       (substring win 0 co)
       "<CURSOR>"
       (substring win co)
       (when (> (- (length text) be) 0)
         (format "\n...[%d characters elided]..." (- (length text) be)))))))

(defun deslop-current-region ()
  "Return (START . END) of the active region, or nil."
  (when (use-region-p)
    (cons (region-beginning) (region-end))))

(provide 'deslop-common)

;;; deslop-common.el ends here
;; 
