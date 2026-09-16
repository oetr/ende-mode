;;; deslop-operation.el --- Operation definitions for deslop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools, llm

;;; Commentary:

;; Static capabilities shared by queueing, target capture, and execution.

;;; Code:

(require 'cl-lib)

(defcustom deslop-insert-instruction
  "Write the most natural continuation of the text at the cursor position."
  "Instruction given to the LLM for deslop-insert-at-cursor."
  :type 'string
  :group 'deslop)

(defcustom deslop-write-anywhere-instruction
  "Make the requested changes in the document, wherever they belong."
  "Instruction given to the LLM for `deslop-write-anywhere'."
  :type 'string
  :group 'deslop)

(defcustom deslop-insert-anywhere-instruction
  "Insert the requested text in the document wherever it belongs."
  "Instruction given to the LLM for `deslop-insert-anywhere'."
  :type 'string
  :group 'deslop)

(cl-defstruct (deslop-operation-spec
               (:constructor deslop-operation-spec--create))
  "Static behavior for one deslop operation."
   type target-kind writes-buffer whole-buffer
  allows-source-changes structured-result)

(defconst deslop-operation-specs
  (list
   (deslop-operation-spec--create
    :type 'insert :target-kind 'cursor-or-selection :writes-buffer t)
   (deslop-operation-spec--create
    :type 'improve :target-kind 'selection-or-buffer :writes-buffer t)
   (deslop-operation-spec--create
    :type 'summarize :target-kind 'selection-or-buffer)
   (deslop-operation-spec--create
    :type 'rubber-duck :target-kind 'document :writes-buffer t
    :whole-buffer t)
   (deslop-operation-spec--create
    :type 'write-anywhere :target-kind 'selection-or-buffer :writes-buffer t
    :whole-buffer t :allows-source-changes t :structured-result 'replacement)
   (deslop-operation-spec--create
    :type 'insert-anywhere :target-kind 'selection-or-buffer :writes-buffer t
     :whole-buffer t :allows-source-changes t :structured-result 'insertion))
  "Registered operation specifications.")

(defun deslop-operation-for (type)
  "Return the specification for operation TYPE, or signal an error."
  (or (cl-find type deslop-operation-specs
               :key #'deslop-operation-spec-type :test #'eq)
      (error "Unknown deslop operation: %s" type)))

(defun deslop-operation-writes-buffer-p (type)
  "Return non-nil when TYPE can modify a buffer."
  (let ((spec (cl-find type deslop-operation-specs
                       :key #'deslop-operation-spec-type :test #'eq)))
    (and spec (deslop-operation-spec-writes-buffer spec))))

(defun deslop-operation-whole-buffer-p (type)
  "Return non-nil when TYPE examines the entire accessible document."
  (let ((spec (cl-find type deslop-operation-specs
                       :key #'deslop-operation-spec-type :test #'eq)))
    (and spec (deslop-operation-spec-whole-buffer spec))))

(defun deslop-operation-allows-source-changes-p (type)
  "Return non-nil when TYPE tolerates source changes during generation."
  (let ((spec (cl-find type deslop-operation-specs
                       :key #'deslop-operation-spec-type :test #'eq)))
    (and spec (deslop-operation-spec-allows-source-changes spec))))

(defun deslop-operation-structured-result (type)
  "Return TYPE's structured result kind, or nil for plain text results."
  (let ((spec (cl-find type deslop-operation-specs
                       :key #'deslop-operation-spec-type :test #'eq)))
    (and spec (deslop-operation-spec-structured-result spec))))

(provide 'deslop-operation)

;;; deslop-operation.el ends here
