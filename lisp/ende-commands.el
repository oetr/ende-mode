;;; ende-commands.el --- Interactive ende commands -*- lexical-binding: t; -*-

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

;; Target capture, job creation, and user-facing operation commands.

;;; Code:

(require 'cl-lib)
(require 'ende-core)
(require 'ende-operation)
(require 'ende-provider)
(require 'ende-target)
(require 'ende-ui)
(require 'ende-queue)

(defun ende-current-region ()
  "Return active region bounds as a cons cell, or nil."
  (when (use-region-p)
    (cons (region-beginning) (region-end))))

(defun ende-enqueue-operation (type &optional instruction profile)
  "Create and enqueue a TYPE operation for the current buffer.
INSTRUCTION and PROFILE override the operation defaults."
  (let* ((spec (ende-operation-for type))
         (buffer (current-buffer))
         (region (ende-current-region))
         (point (point))
         (target-kind (ende-operation-spec-target-kind spec))
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
                     (ende-target-ensure-size
                      start end (capitalize (symbol-name type))))
                   (save-restriction
                     ;; Structured matching and repair share this saved scope.
                     (when (and region
                                (ende-operation-spec-structured-result spec))
                       (narrow-to-region start end))
                     (ende-target-create
                      :buffer buffer :start start :end end :cursor point
                      :selected selected :placement placement
                      :offset (and region (- point (car region)))
                      :review-policy
                      (ende-operation-review-policy type)))))
         (overlay nil)
         (reviews nil)
         (job nil)
         (committed nil))
    (unwind-protect
        (progn
          (when (eq (ende-target-review-policy target) 'resolve)
            (setq reviews
                  (ende-review-conflicts-to-approve
                   buffer (ende-target-start target) (ende-target-end target))))
          (setq overlay (ende-ui-create-overlay
                         buffer start end "Queued" 'badge-only))
          (let ((data (list :target target :overlay overlay)))
            (when profile
              (setq data (plist-put data :profile profile)))
            (when instruction
              (setq data (plist-put data :instruction instruction)))
            (let ((ende--queue-suspended t))
              (setq job (ende-enqueue (cons type data)))
              (setq committed t)
              (mapc #'ende-review-delete-overlay reviews)))
          (ende-process-next)
          (message "ende: queued %s job %d" type (ende-job-id job))
          job)
      (unless committed
        (ende-ignore-errors #'ende-target-release target)
        (when overlay
          (ende-ignore-errors #'ende-ui-delete-overlay overlay))))))

(defun ende-insert-at-cursor ()
  "Queue a request to insert LLM text at cursor position."
  (interactive)
  (ende-enqueue-operation 'insert))

(defun ende-write-anywhere (&optional instruction)
  "Queue multiple edits within the active region or accessible buffer."
  (interactive (list (read-string "Prompt: ")))
  (ende-enqueue-operation 'write-anywhere instruction))

(defun ende-insert-anywhere (&optional instruction)
  "Queue multiple insertions within the active region or accessible buffer."
  (interactive (list (read-string "Prompt: ")))
  (ende-enqueue-operation 'insert-anywhere instruction))

(defun ende-improve-at-cursor (instruction)
  "Queue an improvement of the active region or accessible buffer."
  (interactive (list (read-string "How should the text be improved? ")))
  (ende-enqueue-operation 'improve instruction))

(defun ende-summarize ()
  "Queue a summary of the active region or accessible buffer."
  (interactive)
  (ende-enqueue-operation 'summarize))

(provide 'ende-commands)

;;; ende-commands.el ends here
