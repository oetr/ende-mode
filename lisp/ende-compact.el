;;; ende-compact.el --- Context budgeting for Ende -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter Samarin
;;
;; Author: Peter Samarin <peter.samarin@gmail.com>
;; Maintainer: Peter Samarin <peter.samarin@gmail.com>
;; Keywords: lisp, tools
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

;; Pure policy, capacity, and request-size estimates for compaction.

;;; Code:

(require 'cl-lib)
(require 'ende-provider)

(defcustom ende-auto-compact nil
  "Chat compaction policy, or nil to disable automatic compaction.
Use (:tokens N) or (:percent P :fallback-tokens N).  N is a positive
integer; P is strictly between zero and 100.  The fallback applies
when model capacity is unknown."
  :type '(choice (const :tag "Disabled" nil)
                 (plist :tag "Policy" :options (:tokens :percent :fallback-tokens)))
  :group 'ende)

(defcustom ende-agent-auto-compact nil
  "Agent compaction policy, independent of `ende-auto-compact'.
Accept the same forms; nil disables automatic agent compaction."
  :type '(choice (const :tag "Disabled" nil)
                 (plist :tag "Policy" :options (:tokens :percent :fallback-tokens)))
  :group 'ende)

(defun ende-compact--positive (value)
  "Return VALUE if it is a positive integer, otherwise nil."
  (and (integerp value) (> value 0) value))

(defun ende-compact--validate-policy (policy)
  "Validate POLICY and return a detached copy, or nil when disabled."
  (ende-provider-config--plist policy "Compaction policy")
  (when policy
    (unless
        (or (and (= (length policy) 2)
                 (ende-compact--positive (plist-get policy :tokens)))
            (and (= (length policy) 4)
                 (cl-loop for (key _value) on policy by #'cddr
                          always (memq key '(:percent :fallback-tokens)))
                 (ende-provider-config--finite-p (plist-get policy :percent))
                 (< 0 (plist-get policy :percent))
                 (< (plist-get policy :percent) 100)
                 (ende-compact--positive (plist-get policy :fallback-tokens))))
      (error "Compaction policy: use (:tokens N) or (:percent P :fallback-tokens N)")))
  (copy-sequence policy))

(defun ende-compact-policy (job-type)
  "Return the validated compaction policy for JOB-TYPE.
Only chat and agent jobs have policies; other job types return nil."
  (ende-compact--validate-policy
   (pcase job-type
     ('chat ende-auto-compact)
     ('agent ende-agent-auto-compact))))

(defun ende-compact-budget (profile policy)
  "Return :trigger, :capacity and :target for resolved PROFILE and POLICY.
Nil POLICY disables budgeting.  Explicit profile limits precede the
selected model's unexpired cached metadata; no discovery is started.
An explicit :context-window overrides all cached size limits.
Capacity is the usable input ceiling, or nil when unknown.  A context
window reserves the actual requested :output-tokens, not the model's
:max-output-tokens.  A separate input limit also caps usable capacity.
Anthropic input-only limits reserve :output-tokens too, since input and
output share context; input and output maxima are never added together.
Codex does not enforce :output-tokens: reserve 20% of known capacity as
conservative headroom instead, not as a guaranteed output allowance.
A percentage uses the full context window (or input-only limit), capped
at usable capacity.  Unknown capacity uses :fallback-tokens.  Absolute
triggers are also capped.  Target is floor(60% of trigger); exhausted
capacity yields zero.  These are estimates, not context-fit guarantees."
  (when (ende-compact--validate-policy policy)
    (let* ((explicit-window (ende-compact--positive (plist-get profile :context-window)))
           (model (plist-get profile :model))
           (detail (and (not explicit-window) model
                        (cl-find model (ende-provider-model-details profile)
                                 :key (lambda (item) (plist-get item :id))
                                 :test #'equal)))
           (window (or explicit-window
                       (ende-compact--positive (plist-get detail :context-window))))
           (input (or (ende-compact--positive (plist-get profile :max-input-tokens))
                      (ende-compact--positive (plist-get detail :max-input-tokens))))
           (basis (or window input))
           (codex (eq (plist-get profile :service) 'openai-codex))
           (reserve (if codex
                        (if basis (ceiling basis 5) 0)
                      (or (ende-compact--positive (plist-get profile :output-tokens)) 0)))
           (capacity (cond (window (max 0 (- window reserve)))
                           (input (if (or codex (eq (plist-get profile :api) 'anthropic-messages))
                                      (max 0 (- input reserve))
                                    input))))
           (trigger (or (plist-get policy :tokens)
                        (and basis (floor (* basis (plist-get policy :percent)) 100))
                        (plist-get policy :fallback-tokens))))
      (when (and capacity input) (setq capacity (min capacity input)))
      (when capacity (setq trigger (min trigger capacity)))
      (list :trigger trigger :capacity capacity :target (floor (* trigger 60) 100)))))

(defun ende-compact--usage-count (usage key &optional default)
  "Return nonnegative integer KEY from USAGE, or DEFAULT if absent."
  (let ((value (ende--usage-get usage key)))
    (if (null value) default
      (and (integerp value) (>= value 0) value))))

(defun ende-compact-input-tokens (usage protocol)
  "Return inclusive input tokens for one request's USAGE and PROTOCOL.
Use the latest request, never session totals.  OpenAI-compatible input
already includes cached tokens.  Anthropic input excludes cache reads
and writes, so add those.  Valid zero is retained; unknown or invalid
counts return nil.  PROTOCOL is the profile's canonical :api symbol."
  (pcase protocol
    ('openai-chat-completions (ende-compact--usage-count usage :prompt_tokens))
    ('openai-responses (ende-compact--usage-count usage :input_tokens))
    ('anthropic-messages
     (let ((input (ende-compact--usage-count usage :input_tokens))
           (read (ende-compact--usage-count usage :cache_read_input_tokens 0))
           (write (ende-compact--usage-count usage :cache_creation_input_tokens 0)))
       (and input read write (+ input read write))))))

(defun ende-compact--field (object key)
  "Return keyword KEY from a plist, JSON object or wire alist OBJECT."
  (cond
   ((hash-table-p object) (ende--usage-get object key))
   ((and (consp object) (proper-list-p object))
    (if (keywordp (car object)) (plist-get object key)
      (when (consp (car object))
        (or (alist-get key object)
            (cdr (assoc (substring (symbol-name key) 1) object))))))))

(defun ende-compact-estimate (messages tool-definitions)
  "Estimate input tokens for full MESSAGES and TOOL-DEFINITIONS.
Count text at one token per three bytes plus structural overhead,
including system text, reasoning, tool calls/results and tool schemas.
Images and unresolved sources reserve 16384 tokens each without reading
files or decoding payloads.  Opaque references reserve the same per item.
Encrypted reasoning and signatures are bounded to 1024..16384 tokens.
Deep or cyclic structures stop at depth 32 with a 16384-token allowance.
This conservative heuristic is not a tokenizer or a context-fit guarantee."
  (cl-labels
      ((text (value) (ceiling (string-bytes value) 3))
       (field (key value depth)
         (+ 4
            (cond
             ((member key '(:protocol-opaque "protocol-opaque"))
              (* 16384 (if (proper-list-p value) (length value) 1)))
             ((member key '(:encrypted_content "encrypted_content" :signature "signature"))
              (if (stringp value) (min 16384 (max 1024 (text value))) 16384))
             (t (+ (walk key depth) (walk value depth))))))
       (walk (value depth)
         (cond
          ((null value) 0)
          ((stringp value)
           (if (string-prefix-p "data:image/" value) 16384 (text value)))
          ((>= depth 32) 16384)
          ((member (ende-compact--field value :type)
                   '(:image :source "image" "input_image" "image_url" "base64" "redacted_thinking"))
           16384)
          ((hash-table-p value)
           (let ((count 4))
             (maphash (lambda (key item)
                        (cl-incf count (field key item (1+ depth)))) value)
             count))
          ((vectorp value)
           (+ 4 (cl-loop for item across value sum (walk item (1+ depth)))))
          ((and (consp value) (proper-list-p value))
           (+ 4 (if (keywordp (car value))
                    (cl-loop for (key item) on value by #'cddr
                             sum (field key item (1+ depth)))
                  (cl-loop for item in value sum (walk item (1+ depth))))))
          ((consp value) (field (car value) (cdr value) (1+ depth)))
          (t 4))))
    (+ 16 (walk messages 0) (walk tool-definitions 0))))

(provide 'ende-compact)
;;; ende-compact.el ends here
