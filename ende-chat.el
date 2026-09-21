;;; ende-chat.el --- Org-backed chat for ende -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools, llm

;;; Commentary:

;; Chat treats an ordinary Org buffer as the durable conversation document.
;; The only in-memory request state is the queue job and its destination marker;
;; context is reconstructed from the current Org tree when a job starts.
;;
;; Durable headline properties are ENDE_ROLE (user or assistant) and
;; ENDE_VISIBILITY (normal, hidden, or reasoning).  Roles inherit from the
;; nearest marked ancestor; hidden visibility wins over every descendant and
;; reasoning is sent only when `ende-chat-include-reasoning' is non-nil.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'subr-x)
(require 'ende-core)
(require 'ende-operation)
(require 'ende-operations)
(require 'ende-provider)
(require 'ende-queue)
(require 'ende-target)
(require 'ende-ui)

;;;; Configuration and durable fields

(defcustom ende-chat-buffer-name "*ende-chat*"
  "Name of the buffer created by `ende-chat'."
  :type 'string
  :group 'ende)

(defcustom ende-chat-context-policy 'root-prefix
  "Policy used when serializing a chat request.

`root-prefix' includes eligible sections before the asked section in the
active level-one root, followed by the asked section's eligible body.
`current-section-only' includes only that body.  Both policies retain the
current question as the final user message."
  :type '(choice (const :tag "Root prefix" root-prefix)
                 (const :tag "Current section only" current-section-only))
  :group 'ende)

(defcustom ende-chat-include-reasoning nil
  "Non-nil to include reasoning-marked Org content in chat requests.

Hidden content remains excluded even when this is non-nil.  Included reasoning
keeps the effective user or assistant role of its enclosing section."
  :type 'boolean
  :group 'ende)

(defconst ende-chat-role-property "ENDE_ROLE"
  "Org property storing the durable chat role.")

(defconst ende-chat-visibility-property "ENDE_VISIBILITY"
  "Org property storing the durable chat visibility.")

(defconst ende-chat-id-property "ENDE_CHAT_ID"
  "Org property storing a durable identity for a chat question or reply.")

(defconst ende-chat-reply-to-property "ENDE_CHAT_REPLY_TO"
  "Org property associating a reply with its question.")

(defconst ende-chat-request-id-property "ENDE_CHAT_REQUEST_ID"
  "Org property storing the request identity for a reply.")

(defconst ende-chat-state-property "ENDE_CHAT_STATE"
  "Org property storing the state of an unfinished chat reply.")

(defconst ende-chat--reserved-properties
  '("ENDE_ROLE" "ENDE_VISIBILITY" "ENDE_CHAT_ID" "ENDE_CHAT_REPLY_TO"
    "ENDE_CHAT_REQUEST_ID" "ENDE_CHAT_STATE")
  "Control properties that model output may not author.")

(defconst ende-chat--title-key "ENDE_CHAT_TITLE"
  "Keyword used by the model to propose an empty question's title.")

(defvar ende-chat--buffer-object nil
  "Chat buffer owned by `ende-chat', when one exists.")

(defvar-local ende-chat--owned-buffer nil
  "Non-nil in a chat buffer created by ende.")

(defvar ende-chat--next-id 0
  "Session-local counter used to make fresh chat identifiers.")

(cl-defstruct (ende-chat--document (:constructor ende-chat--make-document))
  "Parsed Org document and its headline relationships."
  tree headlines parent children properties)

;;;; Org document model

(defun ende-chat--new-id (prefix)
  "Return a fresh durable identifier beginning with PREFIX."
  (format "%s-%s-%d" prefix (format-time-string "%Y%m%d%H%M%S%N")
          (cl-incf ende-chat--next-id)))

(defun ende-chat--org-buffer-p ()
  "Return non-nil when the current buffer uses Org mode."
  (derived-mode-p 'org-mode))

(defun ende-chat--local-property (headline property)
  "Return local PROPERTY from HEADLINE, without inheritance."
  (save-excursion
    (goto-char (org-element-property :begin headline))
    (org-entry-get nil property nil)))

(defun ende-chat--normalize-role (value headline)
  "Return normalized role VALUE, or signal for an invalid property."
  (let ((role (and value (downcase (string-trim value)))))
    (unless (or (null role) (member role '("user" "assistant")))
      (error "Invalid %s on headline: %s"
             ende-chat-role-property
             (org-element-property :raw-value headline)))
    role))

(defun ende-chat--normalize-visibility (value headline)
  "Return normalized visibility VALUE, or signal for an invalid property."
  (let ((visibility (and value (downcase (string-trim value)))))
    (unless (or (null visibility)
                (member visibility '("normal" "hidden" "reasoning")))
      (error "Invalid %s on headline: %s"
             ende-chat-visibility-property
             (org-element-property :raw-value headline)))
    visibility))

(defun ende-chat--build-document ()
  "Parse the current Org buffer and return an `ende-chat--document'."
  (unless (ende-chat--org-buffer-p)
    (user-error "Chat commands require an Org buffer"))
  (let* ((tree (org-element-parse-buffer))
         (headlines (org-element-map tree 'headline #'identity))
         (sorted (sort (copy-sequence headlines)
                       (lambda (left right)
                         (< (org-element-property :begin left)
                            (org-element-property :begin right)))))
         (parent (make-hash-table :test #'eq))
         (children (make-hash-table :test #'eq))
         (properties (make-hash-table :test #'eq))
         (stack nil))
    (dolist (headline sorted)
      (let ((level (org-element-property :level headline)))
        (while (and stack
                    (>= (org-element-property :level (car stack)) level))
          (pop stack))
        (when stack
          (puthash headline (car stack) parent)
          (puthash (car stack)
                   (cons headline (gethash (car stack) children))
                   children))
        (let* ((local-role (ende-chat--local-property
                            headline ende-chat-role-property))
               (local-visibility (ende-chat--local-property
                                  headline ende-chat-visibility-property))
               (values (list :id (ende-chat--local-property
                                  headline ende-chat-id-property)
                             :reply-to (ende-chat--local-property
                                        headline ende-chat-reply-to-property)
                             :request-id (ende-chat--local-property
                                          headline ende-chat-request-id-property)
                             :state (ende-chat--local-property
                                     headline ende-chat-state-property))))
          (when local-role
            (setq values
                  (plist-put values :role
                             (ende-chat--normalize-role local-role headline))
                  values
                  (plist-put values :role-marked t)))
          (when local-visibility
            (setq values
                  (plist-put
                   values :visibility
                   (ende-chat--normalize-visibility
                    local-visibility headline))
                  values
                  (plist-put values :visibility-marked t)))
          (puthash headline values properties))
        (push headline stack)))
    (maphash (lambda (headline value)
               (puthash headline (nreverse value) children))
             children)
    (ende-chat--make-document :tree tree :headlines sorted :parent parent
                              :children children :properties properties)))

(defun ende-chat--property-key (property)
  "Return the parsed-document key corresponding to PROPERTY."
  (if (keywordp property)
      property
    (cond
     ((equal property ende-chat-role-property) :role)
     ((equal property ende-chat-visibility-property) :visibility)
     ((equal property ende-chat-id-property) :id)
     ((equal property ende-chat-reply-to-property) :reply-to)
     ((equal property ende-chat-request-id-property) :request-id)
     ((equal property ende-chat-state-property) :state)
     (t (error "Unknown chat property: %s" property)))))

(defun ende-chat--property (document headline property)
  "Return PROPERTY's local value for HEADLINE in DOCUMENT."
  (plist-get (gethash headline (ende-chat--document-properties document))
             (ende-chat--property-key property)))

(defun ende-chat--parent (document headline)
  "Return HEADLINE's enclosing headline, or nil."
  (gethash headline (ende-chat--document-parent document)))

(defun ende-chat--children (document headline)
  "Return HEADLINE's direct children in document order."
  (gethash headline (ende-chat--document-children document)))

(defun ende-chat--root (document headline)
  "Return the level-one root enclosing HEADLINE."
  (let ((current headline))
    (while (and current
                (> (org-element-property :level current) 1))
      (setq current (ende-chat--parent document current)))
    (unless current
      (user-error "Question must be under a level-one Org heading"))
    current))

(defun ende-chat--hidden-p (document headline)
  "Return non-nil when HEADLINE has a hidden ancestor or is hidden."
  (let ((current headline)
        (hidden nil))
    (while current
      (when (equal (ende-chat--property document current :visibility)
                   "hidden")
        (setq hidden t))
      (setq current (ende-chat--parent document current)))
    hidden))

(defun ende-chat--effective-visibility (document headline)
  "Return HEADLINE's effective visibility.

Hidden is absolute.  Otherwise the nearest explicit visibility controls the
subtree, so a normal descendant can deliberately leave a reasoning section."
  (if (ende-chat--hidden-p document headline)
      'hidden
    (let ((current headline)
          (visibility nil))
      (while (and current (null visibility))
        (setq visibility (ende-chat--property document current :visibility)
              current (ende-chat--parent document current)))
      (intern (or visibility "normal")))))

(defun ende-chat--effective-role (document headline)
  "Return HEADLINE's role inherited from its nearest marked ancestor."
  (let ((current headline)
        (role nil))
    (while (and current (null role))
      (setq role (ende-chat--property document current :role)
            current (ende-chat--parent document current)))
    (intern (or role "user"))))

(defun ende-chat--eligible-p (document headline)
  "Return non-nil when HEADLINE's section may enter context."
  (pcase (ende-chat--effective-visibility document headline)
    ('hidden nil)
    ('reasoning ende-chat-include-reasoning)
    (_ t)))

(defun ende-chat--independent-subtree-p (document headline)
  "Return non-nil when HEADLINE explicitly starts a separate subtree."
  (let ((properties (gethash headline
                             (ende-chat--document-properties document))))
    (or (plist-get properties :role-marked)
        (and (plist-get properties :visibility-marked)
             (not (equal (plist-get properties :visibility) "normal"))))))

(defun ende-chat--headline-end (document headline)
  "Return the end of HEADLINE's own section, before its first child."
  (if-let* ((child (car (ende-chat--children document headline))))
      (org-element-property :begin child)
    (org-element-property :end headline)))

(defun ende-chat--remove-ranges (text ranges)
  "Remove source RANGES from TEXT, where each range is (BEGIN END)."
  (let ((offset 0)
        (result nil))
    (dolist (range (sort (copy-sequence ranges)
                         (lambda (left right) (< (car left) (car right)))))
      (let ((start (car range))
            (end (cdr range)))
        (push (substring text offset start) result)
        (setq offset end)))
    (push (substring text offset) result)
    (apply #'concat (nreverse result))))

(defun ende-chat--reserved-property-p (property)
  "Return non-nil when PROPERTY is an Ende control property."
  (member (upcase (or (org-element-property :key property) ""))
          ende-chat--reserved-properties))

(defun ende-chat--section-exclusions (document start end)
  "Return control and reasoning ranges to omit from a section.

Unrelated Org properties remain ordinary reference material.  A drawer made
only of Ende control properties is removed as a whole; mixed drawers retain
their user properties while removing the control lines."
  (let* ((tree (ende-chat--document-tree document))
         (drawer-ranges nil)
         (ranges nil))
    (org-element-map tree 'property-drawer
      (lambda (drawer)
        (let ((drawer-start (org-element-property :begin drawer))
              (drawer-end (org-element-property :end drawer))
              (properties (org-element-map drawer 'node-property #'identity)))
          (when (and (<= start drawer-start)
                     (<= drawer-end end)
                     (cl-every #'ende-chat--reserved-property-p properties))
            (push (cons drawer-start drawer-end) drawer-ranges))))
      nil nil nil)
    (org-element-map tree '(node-property special-block)
      (lambda (element)
        (let ((element-start (org-element-property :begin element))
              (element-end (org-element-property :end element)))
          (when (and (<= start element-start)
                     (<= element-end end)
                     (or (and (eq (org-element-type element) 'node-property)
                              (ende-chat--reserved-property-p element)
                              (not
                               (cl-some
                                (lambda (range)
                                  (and (<= (car range) element-start)
                                       (<= element-end (cdr range))))
                                drawer-ranges)))
                         (and (eq (org-element-type element) 'special-block)
                              (string= (downcase
                                        (or (org-element-property :type element)
                                            ""))
                                       "reasoning")
                              (not ende-chat-include-reasoning))))
            (push (cons element-start element-end) ranges))))
      nil nil nil)
    (dolist (range drawer-ranges)
      (push range ranges))
    (mapcar (lambda (range)
              (cons (- (car range) start) (- (cdr range) start)))
            ranges)))

(defun ende-chat--section-text (document headline)
  "Return HEADLINE's own heading and body, without control material."
  (let* ((start (org-element-property :begin headline))
         (end (ende-chat--headline-end document headline))
         (text (buffer-substring-no-properties start end))
         (ranges (ende-chat--section-exclusions document start end)))
    (ende-chat--remove-ranges text ranges)))

(defun ende-chat--prefix-headlines (document root question)
  "Return eligible headlines before QUESTION within ROOT."
  (let ((question-begin (org-element-property :begin question))
        (root-end (org-element-property :end root))
        result)
    (dolist (headline (ende-chat--document-headlines document))
      (when (and (>= (org-element-property :begin headline)
                     (org-element-property :begin root))
                 (< (org-element-property :begin headline) question-begin)
                 (< (org-element-property :begin headline) root-end)
                 (ende-chat--eligible-p document headline))
        (push headline result)))
    (nreverse result)))

(defun ende-chat--current-headlines (document question)
  "Return QUESTION and eligible unmarked descendants in source order."
  (let ((result nil))
    (cl-labels
        ((walk (headline root)
           (when (or (eq headline root)
                     (and (not (ende-chat--independent-subtree-p
                                document headline))
                          (ende-chat--eligible-p document headline)))
             (when (ende-chat--eligible-p document headline)
               (push headline result))
             (dolist (child (ende-chat--children document headline))
               (walk child root)))))
      (walk question question))
    (nreverse result)))

(defun ende-chat--message (role text)
  "Return one role-tagged chat message for TEXT."
  (cons (symbol-name role) text))

(defun ende-chat--serialize (document question)
  "Serialize QUESTION's active root into ordered role-tagged messages."
  (let* ((root (ende-chat--root document question))
         (current (ende-chat--current-headlines document question))
         (prefix (and (eq ende-chat-context-policy 'root-prefix)
                      (ende-chat--prefix-headlines document root question)))
         (messages nil))
    (unless (memq ende-chat-context-policy '(root-prefix current-section-only))
      (error "Invalid ende-chat-context-policy: %s"
             ende-chat-context-policy))
    (dolist (headline prefix)
      (push (ende-chat--message
             (ende-chat--effective-role document headline)
             (ende-chat--section-text document headline))
            messages))
    (let ((current-text
           (mapconcat (lambda (headline)
                        (ende-chat--section-text document headline))
                      current "")))
      (unless (string-blank-p current-text)
        (push (ende-chat--message 'user current-text) messages)))
    (nreverse messages)))

(defun ende-chat-context-at-point ()
  "Return the serialized chat context for the Org headline at point.

The result excludes the system prompt and is useful to preview callers and
tests."
  (interactive)
  (let* ((document (ende-chat--build-document))
         (question (ende-chat--headline-at-point document)))
    (ende-chat--serialize document question)))

(defun ende-chat--headline-at-point (document)
  "Return the headline containing point from DOCUMENT."
  (save-excursion
    (condition-case nil
        (progn
          (org-back-to-heading t)
          (or (cl-find-if
               (lambda (headline)
                 (= (org-element-property :begin headline) (point)))
               (ende-chat--document-headlines document))
              (user-error "Point is not in an Org headline")))
      (error (user-error "Point is not in an Org headline")))))

(defun ende-chat--find-by-property (document property value)
  "Return the unique headline with PROPERTY equal to VALUE, or nil.
Signal when the durable identifier is ambiguous."
  (let ((matches
         (cl-remove-if-not
          (lambda (headline)
            (equal (ende-chat--property document headline property) value))
          (ende-chat--document-headlines document))))
    (when (> (length matches) 1)
      (error "Duplicate %s value: %s" property value))
    (car matches)))

;;;; Context and response protocol

(defconst ende-chat--system
  "You are answering a question in an Org-backed Ende chat.

Use the supplied role-tagged Org context as reference and conversation history.
Return only readable Org-mode content for the assistant response. Do not emit
level-one headings. Do not emit Ende control properties such as ENDE_ROLE,
ENDE_VISIBILITY, ENDE_CHAT_ID, ENDE_CHAT_REPLY_TO, ENDE_CHAT_REQUEST_ID, or
ENDE_CHAT_STATE.

If the final question's headline has no title but has a request body, begin the
response with exactly one keyword of the form `#+ENDE_CHAT_TITLE: Short title`.
Choose a concise, accurate title for that question. If the headline already
has a title, do not emit this keyword. The keyword is control metadata and will
be removed before your response is inserted.

Plain prose is valid. Use Org headings below level one when structure helps,
and use Org source/example/quote blocks normally. Do not include commentary
about these instructions."
  "System prompt for Org-backed chat requests.")

(defun ende-chat--title-keyword-p (element)
  "Return non-nil when ELEMENT is the private chat title keyword."
  (and (eq (org-element-type element) 'keyword)
       (string= (upcase (or (org-element-property :key element) ""))
                ende-chat--title-key)))

(defun ende-chat--response-control-ranges (tree)
  "Return response ranges for private title and control properties."
  (let (ranges)
    (org-element-map tree '(keyword node-property)
      (lambda (element)
        (cond
         ((ende-chat--title-keyword-p element)
          (push (cons (org-element-property :begin element)
                      (org-element-property :end element))
                ranges))
         ((and (eq (org-element-type element) 'node-property)
               (member (upcase (or (org-element-property :key element) ""))
                       ende-chat--reserved-properties))
          (push (cons (org-element-property :begin element)
                      (org-element-property :end element))
                ranges))))
      nil nil nil)
    ranges))

(defun ende-chat--response-title (tree)
  "Return the first private title value in response TREE."
  (let (title)
    (org-element-map tree 'keyword
      (lambda (element)
        (when (and (null title) (ende-chat--title-keyword-p element))
          (setq title (string-trim
                       (or (org-element-property :value element) "")))))
      nil nil nil)
    (and (not (string-empty-p title)) title)))

(defun ende-chat--remove-response-control (buffer ranges)
  "Remove response control RANGES from BUFFER."
  (with-current-buffer buffer
    (dolist (range (sort (copy-sequence ranges)
                         (lambda (left right) (> (car left) (car right)))))
      (delete-region (car range) (cdr range)))))

(defun ende-chat--normalize-response (content wrapper-level)
  "Return normalized CONTENT for a wrapper at WRAPPER-LEVEL.

The returned plist contains `:content' and an optional `:title'.  Actual Org
headlines are shifted uniformly beneath the host-created assistant wrapper;
stars in literal blocks are left untouched by Org's parser."
  (with-temp-buffer
    (org-mode)
    (insert (or content ""))
    (let* ((tree (org-element-parse-buffer))
           (title (ende-chat--response-title tree))
           (ranges (ende-chat--response-control-ranges tree)))
      (ende-chat--remove-response-control (current-buffer) ranges)
      (goto-char (point-min))
      (let* ((tree (org-element-parse-buffer))
             (headlines (org-element-map tree 'headline #'identity))
             (minimum (and headlines
                           (apply #'min
                                  (mapcar (lambda (headline)
                                            (org-element-property :level headline))
                                          headlines))))
             (target (and minimum (1+ wrapper-level))))
        (when minimum
          (dolist (headline
                   (sort (copy-sequence headlines)
                         (lambda (left right)
                           (> (org-element-property :begin left)
                              (org-element-property :begin right)))))
            (let ((level (org-element-property :level headline))
                  (new-level (+ (org-element-property :level headline)
                                (- target minimum))))
              (goto-char (org-element-property :begin headline))
              (delete-char level)
              (insert (make-string new-level ?*)))))
        (list :title title
              :content (ende-trim-trailing (buffer-string)))))))

(defun ende-chat--set-headline-title (headline title)
  "Set HEADLINE's title to TITLE using Org's headline editor."
  (save-excursion
    (goto-char (org-element-property :begin headline))
    (org-edit-headline title)))

;;;; Placeholder and response placement

(defun ende-chat--response-property (name value)
  "Return one Org property line for NAME and VALUE."
  (format ":%s: %s\n" name value))

(defun ende-chat--placeholder-text (level question-id request-id)
  "Return a hidden pending response section at LEVEL."
  (let ((stars (make-string level ?*)))
    (concat stars " Ende response\n"
            ":PROPERTIES:\n"
            (ende-chat--response-property ende-chat-role-property "assistant")
            (ende-chat--response-property ende-chat-visibility-property "hidden")
            (ende-chat--response-property ende-chat-reply-to-property question-id)
            (ende-chat--response-property ende-chat-request-id-property request-id)
            (ende-chat--response-property ende-chat-state-property "pending")
            ":END:\n\n")))

(defun ende-chat--reply-sibling-p (document headline question-id)
  "Return non-nil when HEADLINE is an Ende reply to QUESTION-ID."
  (equal (ende-chat--property document headline :reply-to) question-id))

(defun ende-chat--insertion-position (document question)
  "Return the position where a new response placeholder belongs."
  (let* ((question-id (ende-chat--property document question :id))
         (end (org-element-property :end question)))
    (if (= (org-element-property :level question) 1)
        end
      (let ((parent (ende-chat--parent document question)))
        (catch 'stop
          (dolist (sibling
                   (cdr (memq question
                              (ende-chat--children document parent))))
            (if (ende-chat--reply-sibling-p document sibling question-id)
                (setq end (org-element-property :end sibling))
              (throw 'stop nil))))
        end))))

(defun ende-chat--insert-placeholder (document question question-id request-id)
  "Insert a pending reply for QUESTION and return its heading position."
  (let* ((level (if (= (org-element-property :level question) 1)
                    2
                  (org-element-property :level question)))
         (position (ende-chat--insertion-position document question))
         (text (ende-chat--placeholder-text level question-id request-id)))
    (goto-char position)
    (unless (bolp)
      (insert "\n"))
    (let ((position (point)))
      (insert text)
      position)))

(defun ende-chat--placeholder (job)
  "Return JOB's current pending reply headline, or nil."
  (let* ((instruction (ende-job-instruction job))
         (buffer (ende-target-buffer (ende-job-target job)))
         (request-id (plist-get instruction :request-id)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-restriction
          (widen)
          (let* ((document (ende-chat--build-document))
                 (headline (ende-chat--find-by-property
                            document ende-chat-request-id-property request-id)))
            (when (and headline
                       (equal (ende-chat--property
                               document headline :state)
                              "pending"))
              (list document headline))))))))

(defun ende-chat--failure-text (error)
  "Return a readable failure body for ERROR."
  (if error
      (format "Ende chat request failed: %s\n"
              (ende-error-display-message error))
    "Ende chat request was cancelled or skipped.\n"))

(defun ende-chat--mark-failed (job &optional error)
  "Mark JOB's pending reply as failed, if it still exists."
  (condition-case nil
      (let ((placeholder (ende-chat--placeholder job)))
        (when placeholder
          (pcase-let ((`(,_document ,headline) placeholder))
            (let ((buffer (ende-target-buffer (ende-job-target job))))
              (with-current-buffer buffer
                (save-restriction
                  (widen)
                  (let ((inhibit-read-only t))
                    (atomic-change-group
                      (goto-char (org-element-property :begin headline))
                      (org-entry-put nil ende-chat-state-property "failed")
                      (let* ((updated (ende-chat--build-document))
                             (failed
                              (ende-chat--find-by-property
                               updated ende-chat-request-id-property
                               (plist-get (ende-job-instruction job)
                                          :request-id))))
                        (when failed
                          (goto-char (org-element-property :end failed))
                          (unless (bolp) (insert "\n"))
                          (insert (ende-chat--failure-text error))))))))))))
    ((error quit) nil)))

(defun ende-chat--assistant-text (level question-id request-id normalized thinking)
  "Return the host-owned assistant wrapper for NORMALIZED and THINKING."
  (let* ((body (plist-get normalized :content))
         (stars (make-string level ?*)))
    (when (string-empty-p body)
      (error "Chat completion was empty"))
    (concat stars " Ende response\n"
            ":PROPERTIES:\n"
            (ende-chat--response-property ende-chat-role-property "assistant")
            (ende-chat--response-property ende-chat-reply-to-property question-id)
            (ende-chat--response-property ende-chat-request-id-property request-id)
            ":END:\n\n"
            body
            (when (and (stringp thinking) (not (string-empty-p thinking)))
              (concat "\n\n#+begin_reasoning\n"
                      (ende-trim-trailing thinking)
                      "\n#+end_reasoning"))
            "\n")))

(defun ende-chat--apply-result (job result)
  "Replace JOB's pending reply with a completed assistant response."
  (when (ende-job-current-p job)
    (condition-case failure
        (let* ((content (ende--result-content result))
               (thinking (and (listp result) (plist-get result :thinking)))
               (instruction (ende-job-instruction job))
               (question-id (plist-get instruction :question-id))
               (request-id (plist-get instruction :request-id))
               (placeholder (ende-chat--placeholder job)))
          (unless (and (stringp content) (not (string-empty-p content)))
            (error "Chat completion was empty"))
          (unless placeholder
            (error "Chat response destination no longer exists"))
          (pcase-let ((`(,document ,old-headline) placeholder))
            (let* ((level (org-element-property :level old-headline))
                   (normalized (ende-chat--normalize-response content level))
                   (response (ende-chat--assistant-text
                              level question-id request-id normalized thinking))
                   (buffer (ende-target-buffer (ende-job-target job)))
                   (question (ende-chat--find-by-property
                              document ende-chat-id-property question-id)))
              (with-current-buffer buffer
                (save-restriction
                  (widen)
                  (atomic-change-group
                    (when (and question
                               (string-blank-p
                                (org-element-property :raw-value question))
                               (plist-get normalized :title))
                      (ende-chat--set-headline-title
                       question (plist-get normalized :title)))
                    (let* ((updated (ende-chat--build-document))
                           (headline
                            (ende-chat--find-by-property
                             updated ende-chat-request-id-property request-id)))
                      (unless headline
                        (error "Chat response destination no longer exists"))
                      (delete-region (org-element-property :begin headline)
                                     (org-element-property :end headline))
                      (goto-char (org-element-property :begin headline))
                      (insert response)))))))
          (ende-job-finish job result))
      ((error quit)
       (ende-job-fail job (ende--application-error failure))))))

;;;; Queue execution and commands

(defun ende-chat--run (job)
  "Serialize and dispatch a chat JOB at the moment it starts."
  (ende--call-with-job-error
   job
   (lambda ()
     (let* ((instruction (ende-job-instruction job))
            (buffer (ende-target-buffer (ende-job-target job)))
            (question-id (plist-get instruction :question-id)))
       (with-current-buffer buffer
         (save-restriction
           (widen)
           (let* ((document (ende-chat--build-document))
                  (question (ende-chat--find-by-property
                             document ende-chat-id-property question-id)))
             (unless question
               (error "Chat question no longer exists"))
             (unless (and (eq (ende-chat--effective-role document question)
                              'user)
                          (eq (ende-chat--effective-visibility document question)
                              'normal))
               (error "Chat question is no longer eligible"))
             (let ((messages (ende-chat--serialize document question)))
               (unless messages
                 (error "Chat question has no eligible content"))
               (ende--dispatch-llm
                job
                (cons (cons "system" ende-chat--system) messages)
                (lambda (result) (ende-chat--apply-result job result)))))))))))

(ende-register-job-handler 'chat #'ende-chat--run)

(defun ende-chat--job-event (job event)
  "Retain a failure marker for unfinished chat replies."
  (when (and (eq (ende-job-type job) 'chat)
             (memq (plist-get event :type) '(:error :cancelled :skipped)))
    (ende-chat--mark-failed job (plist-get event :error))))

(add-hook 'ende-job-event-functions #'ende-chat--job-event)

(defun ende-chat--ask-data (document question)
  "Return validated and durable metadata for QUESTION."
  (let ((question-id (or (ende-chat--property document question :id)
                         (ende-chat--new-id "question"))))
    (when (eq (ende-chat--effective-visibility document question) 'hidden)
      (user-error "Cannot ask from hidden chat content"))
    (when (eq (ende-chat--effective-visibility document question) 'reasoning)
      (user-error "Cannot ask from reasoning-only chat content"))
    (when (eq (ende-chat--effective-role document question) 'assistant)
      (user-error "Cannot ask from assistant-owned chat content"))
    (list :question-id question-id)))

(defun ende-chat-ask ()
  "Ask the Org headline at point and reserve its response position.

The question receives `ENDE_ROLE: user' when it is first asked.  Context is
serialized when its queue job starts, not when this command is invoked."
  (interactive)
  (let* ((document (ende-chat--build-document))
         (question (ende-chat--headline-at-point document))
         (data (ende-chat--ask-data document question))
         (question-id (plist-get data :question-id))
         (request-id (ende-chat--new-id "request"))
         (role-was-marked
          (ende-chat--property document question :role))
         (position nil)
         (target nil)
         (overlay nil)
          (job nil)
          (committed nil))
    (save-excursion
      (atomic-change-group
        (goto-char (org-element-property :begin question))
        (unless role-was-marked
          (org-entry-put nil ende-chat-role-property "user"))
        (unless (ende-chat--property document question :id)
          (org-entry-put nil ende-chat-id-property question-id))
        (let* ((marked (ende-chat--build-document))
               (marked-question
                (ende-chat--find-by-property marked ende-chat-id-property
                                               question-id)))
          (setq position
                (ende-chat--insert-placeholder
                 marked marked-question question-id request-id))))
      (unwind-protect
          (progn
            (setq target
                  (ende-target-create
                   :buffer (current-buffer)
                   :start (point-min)
                   :end (point-max)
                   :cursor position
                   :review-policy 'ignore))
            (setq overlay
                  (ende-ui-create-overlay
                   (current-buffer) position position "Queued" 'badge-only))
            (setq job
                  (ende-enqueue
                   (list 'chat :target target :overlay overlay
                         :instruction (list :question-id question-id
                                            :request-id request-id))))
            (setq committed t)
            (message "ende: queued chat request %d" (ende-job-id job))
            job)
        (unless committed
          (when overlay
            (ende-ui-delete-overlay overlay))
          (when target
            (ende-target-release target))
          (when (and (buffer-live-p (current-buffer)) position)
            (save-excursion
              (goto-char position)
              (condition-case nil
                  (let* ((failed-document (ende-chat--build-document))
                         (failed (ende-chat--find-by-property
                                  failed-document
                                  ende-chat-request-id-property request-id)))
                    (when failed
                      (delete-region (org-element-property :begin failed)
                                     (org-element-property :end failed))))
                ((error quit) nil)))))))))

(defun ende-chat-buffer ()
  "Return the Org buffer owned by `ende-chat'."
  (setq ende-chat--buffer-object
        (ende-owned-buffer ende-chat--buffer-object
                           ende-chat-buffer-name
                           'ende-chat--owned-buffer
                           #'org-mode)))

;;;###autoload
(defun ende-chat ()
  "Open the Ende chat Org buffer."
  (interactive)
  (pop-to-buffer (ende-chat-buffer)))

;;;###autoload
(defun ende-chat-new (title)
  "Create a new level-one chat root titled TITLE."
  (interactive (list (read-string "Chat root title: ")))
  (let ((buffer (ende-chat-buffer)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (unless (or (bobp) (bolp))
        (insert "\n"))
      (insert "* " title "\n\n")
      (backward-char 1))
    (pop-to-buffer buffer)))

(defun ende-chat--buffer-killed ()
  "Forget the owned chat buffer after it is killed."
  (when (eq (current-buffer) ende-chat--buffer-object)
    (setq ende-chat--buffer-object nil)))

(add-hook 'kill-buffer-hook #'ende-chat--buffer-killed)

(provide 'ende-chat)

;;; ende-chat.el ends here
