;;; ende-operation.el --- Operation definitions for ende -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools, llm

;;; Commentary:

;; Static capabilities shared by queueing, target capture, and execution.

;;; Code:

(require 'cl-lib)

(defcustom ende-insert-instruction
  "Write the most natural continuation of the text at the cursor position."
  "Instruction given to the LLM for ende-insert-at-cursor."
  :type 'string
  :group 'ende)

(defcustom ende-write-anywhere-instruction
  "Make the requested changes in the document, wherever they belong."
  "Instruction given to the LLM for `ende-write-anywhere'."
  :type 'string
  :group 'ende)

(defcustom ende-insert-anywhere-instruction
  "Insert the requested text in the document wherever it belongs."
  "Instruction given to the LLM for `ende-insert-anywhere'."
  :type 'string
  :group 'ende)

(cl-defstruct (ende-operation-spec
               (:constructor ende-operation-spec--create))
  "Static behavior for one ende operation."
  type target-kind writes-buffer whole-buffer
  allows-source-changes structured-result review-policy)

(defconst ende-operation-specs
  (list
   (ende-operation-spec--create
    :type 'insert :target-kind 'cursor-or-selection :writes-buffer t
    :review-policy 'resolve)
   (ende-operation-spec--create
    :type 'improve :target-kind 'selection-or-buffer :writes-buffer t
    :review-policy 'resolve)
   (ende-operation-spec--create
    :type 'summarize :target-kind 'selection-or-buffer
    :allows-source-changes t
    :review-policy 'ignore)
   (ende-operation-spec--create
    :type 'rubber-duck :target-kind 'document :writes-buffer t
    :whole-buffer t :review-policy 'resolve)
   (ende-operation-spec--create
    :type 'chat :target-kind 'document :writes-buffer t
    :allows-source-changes t :review-policy 'ignore)
   (ende-operation-spec--create
    :type 'chat-compact :target-kind 'document :writes-buffer t
    :whole-buffer t :allows-source-changes t :review-policy 'ignore)
   (ende-operation-spec--create
    :type 'write-anywhere :target-kind 'selection-or-buffer :writes-buffer t
    :whole-buffer t :allows-source-changes t :structured-result 'replacement
    :review-policy 'resolve)
   (ende-operation-spec--create
    :type 'insert-anywhere :target-kind 'selection-or-buffer :writes-buffer t
    :whole-buffer t :allows-source-changes t :structured-result 'insertion
    :review-policy 'resolve))
  "Registered operation specifications.")

(defun ende-operation--find (type)
  (cl-find type ende-operation-specs :key #'ende-operation-spec-type :test #'eq))

(defun ende-operation-for (type)
  "Return the specification for operation TYPE, or signal an error."
  (or (ende-operation--find type)
      (error "Unknown ende operation: %s" type)))

(defun ende-operation-writes-buffer-p (type)
  "Return non-nil when TYPE can modify a buffer."
  (let ((spec (ende-operation--find type)))
    (and spec (ende-operation-spec-writes-buffer spec))))

(defun ende-operation-whole-buffer-p (type)
  "Return non-nil when TYPE examines the entire accessible document."
  (let ((spec (ende-operation--find type)))
    (and spec (ende-operation-spec-whole-buffer spec))))

(defun ende-operation-allows-source-changes-p (type)
  "Return non-nil when TYPE tolerates source changes during generation."
  (let ((spec (ende-operation--find type)))
    (and spec (ende-operation-spec-allows-source-changes spec))))

(defun ende-operation-structured-result (type)
  "Return TYPE's structured result kind, or nil for plain text results."
  (let ((spec (ende-operation--find type)))
    (and spec (ende-operation-spec-structured-result spec))))

(defun ende-operation-review-policy (type)
  "Return the pending-review policy for operation TYPE."
  (let ((spec (ende-operation--find type)))
    (and spec (ende-operation-spec-review-policy spec))))

(provide 'ende-operation)

;;; ende-operation.el ends here
