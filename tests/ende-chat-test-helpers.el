;;; ende-chat-test-helpers.el --- Shared chat test helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; Fixtures for constructing and inspecting Org chat configurations.

;;; Code:

(require 'ende-test-helpers)
(require 'cl-lib)
(require 'org)
(require 'ende-chat)

(defun ende-test--chat-config-block (forms)
  "Render data FORMS as an Org chat configuration block."
  (concat "#+begin_ende\n"
          (mapconcat #'prin1-to-string forms "\n")
          "\n#+end_ende\n"))

(defun ende-test--chat-config-at (document title)
  "Resolve configuration for the headline named TITLE in DOCUMENT."
  (let ((headline
         (cl-find-if (lambda (node)
                       (equal (org-element-property :raw-value node) title))
                     (ende-chat--document-headlines document))))
    (should headline)
    (ende-chat-config-resolve (ende-chat--document-tree document)
                              (ende-chat--document-parent document) headline)))

(provide 'ende-chat-test-helpers)

;;; ende-chat-test-helpers.el ends here
