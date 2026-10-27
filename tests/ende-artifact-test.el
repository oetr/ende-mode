;;; ende-artifact-test.el --- Conversation image tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'ende-artifact)

(defconst ende-artifact-test-svg
  "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"80\" height=\"40\"><rect width=\"80\" height=\"40\" fill=\"blue\"/></svg>")

(defmacro ende-artifact-test-with-files (&rest body)
  "Run BODY in an Org buffer visiting a temporary conversation."
  (declare (indent 0) (debug t))
  `(let ((directory (make-temp-file "ende-images-" t)))
     (unwind-protect
         (with-temp-buffer
           (org-mode)
           (setq buffer-file-name (expand-file-name "chat.org" directory))
           ,@body)
       (delete-directory directory t))))

(ert-deftest ende-artifact-paths-and-validation ()
  (ende-artifact-test-with-files
    (let* ((default-directory temporary-file-directory)
           (base (ende-artifact-base))
           (name (ende-artifact-store base "chat-123" ende-artifact-test-svg
                                      "image/svg+xml"))
           (part (ende-artifact-resolve base "chat-123" name)))
      (should (equal base (expand-file-name ".ende-artifacts/" directory)))
      (should (equal (ende-artifact--read-bytes (plist-get part :path))
                     ende-artifact-test-svg))
      (should-error (ende-artifact-resolve base "chat-123" "missing.png"))
      (should-error (ende-artifact-resolve base "chat-123" "../bad.svg"))
      (make-symbolic-link (plist-get part :path)
                          (expand-file-name "escape.svg"
                                            (ende-artifact-directory base "chat-123")))
      (should-error (ende-artifact-resolve base "chat-123" "escape.svg"))
      (make-symbolic-link directory (expand-file-name "escape" base))
      (should-error (ende-artifact-directory base "escape"))
      (should-error (ende-artifact-store base "chat-123"
                                         "<svg><script/></svg>" "image/svg+xml"))
      (should (= 1 (length (directory-files
                            (ende-artifact-directory base "chat-123") nil
                            "^image-")))))))

(ert-deftest ende-artifact-document-base-and-svg-safety ()
  (with-temp-buffer
    (org-mode)
    (should-error (ende-artifact-base))
    (insert "#+ENDE_ARTIFACTS_BASE_PATH: /tmp/explicit-images/\n* Topic\n"
            "#+ENDE_ARTIFACTS_BASE_PATH: /tmp/not-document-config/\n")
    (should (equal (ende-artifact-base) "/tmp/explicit-images/")))
  (dolist (payload '("<script/>" "<foreignObject/>"
                     "<use href=\"file:/etc/passwd\"/>"
                     "<rect style=\"fill:url(https://example.com/a)\"/>"))
    (should-error (ende-artifact--svg
                   (concat "<svg xmlns=\"http://www.w3.org/2000/svg\">"
                           payload "</svg>")))))

(require 'ende-chat)

(ert-deftest ende-artifact-org-multimodal-and-attachments ()
  (ende-artifact-test-with-files
    (let* ((base (ende-artifact-base))
           (name (ende-artifact-store base "chat-123" ende-artifact-test-svg "image/svg+xml"))
           (standalone (expand-file-name "original.svg" directory)))
      (copy-file (plist-get (ende-artifact-resolve base "chat-123" name) :path) standalone)
      (insert "#+ENDE_ARTIFACTS_BASE_PATH: .ende-artifacts/\n"
              "* Topic\n:PROPERTIES:\n:ENDE_CHAT_ID: chat-123\n:END:\nExplain.\n"
              "** Assistant\n:PROPERTIES:\n:ENDE_ROLE: assistant\n:END:\nBefore image.\n"
              "#+ENDE_ARTIFACT: " name "\n"
              (ende-artifact-link base "chat-123" name buffer-file-name) "\nAfter image.\n"
              "** Follow up\n:PROPERTIES:\n:ENDE_ROLE: user\n:END:\nCompare.\n"
              "[[file:original.svg][Original]]\n")
      (goto-char (point-max))
      (let* ((messages (ende-chat-context-at-point))
             (assistant (nth 1 messages))
             (user (nth 2 messages))
             (parts (ende-content-body assistant)))
        (should (equal (mapcar #'ende-content-role messages) '("user" "assistant" "user")))
        (should (equal (plist-get (nth 1 parts) :media-type) "image/svg+xml"))
        (should-not (string-match-p "ENDE_ARTIFACT\\|.ende-artifacts" (plist-get (car parts) :text)))
        (should (string-match-p "Original" (ende-content-readable (ende-content-body user))))
        (should (= 1 (cl-count :image (ende-content-body user) :key (lambda (p) (plist-get p :type)))))))))

(ert-deftest ende-artifact-persisted-pairs-and-origin-policy ()
  (ende-artifact-test-with-files
    (let* ((base (ende-artifact-base))
           (name (ende-artifact-store base "chat-123" ende-artifact-test-svg "image/svg+xml")))
      (should-error (ende-artifact-content
                     (concat "#+ENDE_ARTIFACT: " name "\nNo link.\n")
                     (lambda () base) "chat-123" 'user #'ignore))
      (insert "* Topic\n:PROPERTIES:\n:ENDE_CHAT_ID: chat-123\n:END:\nQuestion\n"
              "** Reply\n:PROPERTIES:\n:ENDE_ROLE: assistant\n:END:\n"
              "[[file:/etc/hosts]]\n** Next\n:PROPERTIES:\n:ENDE_ROLE: user\n:END:\nContinue.\n")
      (goto-char (point-max))
      (should-error (ende-chat-context-at-point)))))

(ert-deftest ende-artifact-provider-input-and-redaction ()
  (ende-artifact-test-with-files
    (let* ((base (ende-artifact-base))
           (name (ende-artifact-store base "chat" ende-artifact-test-svg "image/svg+xml"))
           (image (ende-artifact-resolve base "chat" name))
           (parts (list '(:type :text :text "Diagram:") image))
           (messages (list (list :role "assistant" :content parts)))
           (profile '(:model "vision"))
           (chat (ende-llm--request-body messages profile))
           (responses (ende-codex--request-messages messages))
           (batch (ende-openrouter-batch--request-body messages profile "fixture"))
           (url (alist-get :url (alist-get :image_url
                                 (nth 1 (alist-get :content (nth 1 (alist-get :messages chat))))))))
      (should (string-prefix-p "data:image/png;base64," url))
      (should (equal (mapcar (lambda (m) (alist-get :role m)) responses) '("assistant" "user")))
      (should (equal (alist-get :type (nth 1 (alist-get :content (nth 1 responses)))) "input_image"))
      (should (equal (alist-get :messages (alist-get :body batch)) (alist-get :messages chat)))
      (should (equal chat (ende-llm--request-body messages profile)))
      (should-not (string-match-p "base64" (json-encode (ende-content-redact chat))))
      (should-error (ende-llm--request-body messages '(:image-input nil))))))

(require 'ende-chat-test-helpers)

(ert-deftest ende-artifact-generated-roundtrip-and-svg-boundaries ()
  (ende-test--reset)
  (ende-artifact-test-with-files
    (let* ((base (ende-artifact-base))
           (xml ende-artifact-test-svg)
           (raw (concat "Before\n" xml "\nAfter\n#+begin_src svg\n" xml "\n#+end_src\n"))
           (prepared (ende-artifact-prepare-text raw base "chat" buffer-file-name)))
      (should (= 1 (length (directory-files (ende-artifact-directory base "chat") nil "^image-"))))
      (should (string-match-p (regexp-quote "#+begin_src svg\n<svg") prepared))
      (should-not (string-match-p "Before\n<svg" prepared)))
    (let* ((png (cdr (ende-content-image-data
                      (ende-artifact-resolve (ende-artifact-base) "chat" "image-001.svg"))))
           (ende-llm-function
            (ende-test--fake-llm
             (list (list :content "" :output-parts
                         (list (list :type :text :text "Here is the image.\n")
                               (list :type :generated-image :bytes png :media-type "image/png")))))))
      (erase-buffer)
      (insert "* Conversation\nMake a diagram.\n")
      (goto-char (point-min))
      (let ((job (ende-ask)))
        (should (eq (ende-job-state job) 'completed))
        (should (string-match-p (regexp-quote "#+ENDE_ARTIFACT: image-001.png") (buffer-string)))
        (goto-char (point-max))
        (insert "** Follow up\nWhat is blue?\n")
        (let* ((messages (ende-chat-context-at-point))
               (image (cl-find :image (ende-content-body (nth 1 messages))
                               :key (lambda (p) (plist-get p :type)))))
          (should image)
          (should (equal png (ende-artifact--read-bytes (plist-get image :path))))))))
  (ende-test--reset))

(provide 'ende-artifact-test)
