;;; ende-test.el --- ERT tests for ende -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools, llm

;;; Commentary:

;; ERT coverage for synchronous operations, asynchronous lifecycle behavior,
;; target conflicts, and the incremental streaming parsers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'ende-provider)
(require 'ende)

;;;; Test harness

(require 'ende-test-helpers)
(require 'ende-chat-test-helpers)

;;;; Normal operations

;;;; Queue lifecycle

;;;; Target failure paths

;;;; Anchor ordering and conflicts

;;;; Incremental reasoning parser

;;;; SSE parser

;;;; Profiles, history, and transport command safety

;;;; Overlay lifecycle

;;;; Review highlight

;;;; Rubber-duck exercises

;;;; Org-backed chat

(provide 'ende-test)

;;; ende-test.el ends here
