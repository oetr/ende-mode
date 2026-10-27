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

(provide 'ende-artifact-test)
