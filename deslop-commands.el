;;; deslop-commands.el --- Interactive deslop commands -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools, llm

;;; Commentary:

;; Target capture, job creation, and user-facing operation commands.

;;; Code:

(require 'cl-lib)
(require 'deslop-core)
(require 'deslop-operation)
(require 'deslop-provider)
(require 'deslop-target)
(require 'deslop-ui)
(require 'deslop-queue)

(defun deslop-trim-trailing (text)
  "Remove trailing newlines from TEXT, preserving inline trailing spaces."
  (string-trim-right text "[\n\r]+"))

(defun deslop-current-region ()
  "Return active region bounds as a cons cell, or nil."
  (when (use-region-p)
    (cons (region-beginning) (region-end))))

(defun deslop-enqueue-operation (type &optional instruction profile)
  "Create and enqueue a TYPE operation for the current buffer.
INSTRUCTION and PROFILE override the operation defaults."
  (let* ((spec (deslop-operation-for type))
         (buffer (current-buffer))
         (region (deslop-current-region))
         (point (point))
         (target-kind (deslop-operation-spec-target-kind spec))
         (selected (pcase target-kind
                     ('document t)
                     ('selection-or-buffer t)
                     ('cursor-or-selection (and region t))))
         (start (pcase target-kind
                  ('document (point-min))
                  ('selection-or-buffer (if region (car region) (point-min)))
                  ('cursor-or-selection (if region (car region) point))))
         (end (pcase target-kind
                ('document (point-max))
                ('selection-or-buffer (if region (cdr region) (point-max)))
                ('cursor-or-selection (if region (cdr region) point))))
         (placement (and region
                         (cond ((= point (car region)) :before)
                               ((= point (cdr region)) :after)
                               (t :inside))))
         (target (progn
                   (unless (and (eq target-kind 'cursor-or-selection)
                                (eq type 'insert))
                     (deslop-target-ensure-size
                      start end (capitalize (symbol-name type))))
                   (deslop-target-create
                    :buffer buffer :start start :end end :cursor point
                    :selected selected :placement placement
                    :offset (and region (- point (car region))))))
         (overlay nil)
         (committed nil))
    (unwind-protect
        (progn
           (setq overlay
                 (unless (deslop-operation-whole-buffer-p type)
                   (deslop-ui-create-overlay buffer start end "Queued")))
          (let ((data (list :target target :overlay overlay)))
            (when profile
              (setq data (plist-put data :profile profile)))
            (when instruction
              (setq data (plist-put data :instruction instruction)))
            (let ((job (deslop-enqueue (cons type data))))
              (setq committed t)
              (message "deslop: queued %s job %d" type (deslop-job-id job))
              job)))
      (unless committed
        (deslop--ignore-errors #'deslop-target-release target)
        (when overlay
          (deslop--ignore-errors #'deslop-ui-delete-overlay overlay))))))

(defun deslop-insert-at-cursor ()
  "Queue a request to insert LLM text at cursor position."
  (interactive)
  (deslop-enqueue-operation 'insert))

(defun deslop-write-anywhere (&optional instruction)
  "Queue a request for the LLM to write multiple edits anywhere in the buffer."
  (interactive (list (read-string "Prompt: ")))
  (deslop-enqueue-operation 'write-anywhere instruction))

(defun deslop-insert-anywhere (&optional instruction)
  "Queue a request for the LLM to insert text at multiple buffer positions."
  (interactive (list (read-string "Prompt: ")))
  (deslop-enqueue-operation 'insert-anywhere instruction))

(defun deslop-improve-at-cursor (instruction)
  "Queue an improvement of the active region or accessible buffer."
  (interactive (list (read-string "How should the text be improved? ")))
  (deslop-enqueue-operation 'improve instruction))

(defun deslop-summarize ()
  "Queue a summary of the active region or accessible buffer."
  (interactive)
  (deslop-enqueue-operation 'summarize))

(provide 'deslop-commands)

;;; deslop-commands.el ends here
