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

;;;; Org-backed chat

(defun ende-test--chat-goto (heading)
  "Move point to the Org headline HEADING, ignoring trailing tags."
  (goto-char (point-min))
  (let ((regexp
         (concat "^" (regexp-quote heading)
                 "\\(?:[ \t]+:[-[:alnum:]_@#%:]+:\\)?[ \t]*$")))
    (unless (re-search-forward regexp nil t)
      (error "No Org headline matching %s" heading))
    (org-back-to-heading t)))

(defun ende-test--chat-context-text (messages)
  "Return all serialized chat message text in MESSAGES."
  (mapconcat (lambda (message) (cdr message)) messages "\n"))

(defun ende-test--chat-context-headings (messages)
  "Return the headings present in serialized MESSAGES, in order."
  (with-temp-buffer
    (org-mode)
    (insert (ende-test--chat-context-text messages))
    (org-element-map (org-element-parse-buffer) 'headline
		     (lambda (headline) (org-element-property :raw-value headline)))))

(defun ende-test--chat-message-role (message)
  "Return the role of an ordinary or protocol-shaped chat MESSAGE."
  (if (and (consp message) (stringp (car message)))
      (car message)
    (plist-get message :role)))

(defun ende-test--chat-protocol-messages (messages)
  "Return protocol-shaped messages from serialized MESSAGES."
  (cl-remove-if-not
   (lambda (message)
     (and (not (and (consp message) (stringp (car message))))
          (plist-member message :role)))
   messages))

(defun ende-test--chat-control-drawers ()
  "Return the Ende control property drawers in the current buffer."
  (cl-remove-if-not
   (lambda (drawer)
     (cl-every #'ende-chat--reserved-property-p
               (org-element-map drawer 'node-property #'identity)))
   (org-element-map (org-element-parse-buffer) 'property-drawer #'identity)))

(defun ende-test--chat-control-drawer-hidden-p (drawer)
  "Return non-nil when DRAWER's first property line is folded."
  (save-excursion
    (goto-char (org-element-property :begin drawer))
    (forward-line 1)
    (invisible-p (point))))

(defun ende-test--chat-response-headlines ()
  "Return generated Agent: Reply headlines in document order."
  (cl-remove-if-not
   (lambda (headline)
     (string= (org-element-property :raw-value headline) "Agent: Reply"))
   (org-element-map (org-element-parse-buffer) 'headline #'identity)))

(defun ende-test--chat-reasoning-headlines ()
  "Return generated Agent: Reasoning headlines in document order."
  (cl-remove-if-not
   (lambda (headline)
     (string= (org-element-property :raw-value headline) "Agent: Reasoning"))
   (org-element-map (org-element-parse-buffer) 'headline #'identity)))

(defun ende-test--chat-headline-property (headline property)
  "Return PROPERTY from HEADLINE."
  (save-excursion
    (goto-char (org-element-property :begin headline))
    (org-entry-get nil property)))

(defun ende-test--chat-headline-text-hidden-p (headline text)
  "Return non-nil when TEXT in HEADLINE's subtree is folded."
  (save-excursion
    (goto-char (org-element-property :begin headline))
    (when (search-forward text (org-element-property :end headline) t)
      (invisible-p (1- (point))))))

(provide 'ende-chat-test-helpers)

;;; ende-chat-test-helpers.el ends here
