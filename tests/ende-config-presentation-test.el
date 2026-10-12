;;; ende-config-presentation-test.el --- Ende config presentation tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Coverage for Ende configuration fontification and point-sensitive help.

;;; Code:

(require 'ert)
(require 'ende)
(require 'org)

(defun ende-test--presentation-face-at (text &optional start)
  "Return the face at TEXT in the current buffer, searching from START."
  (goto-char (or start (point-min)))
  (unless (search-forward text nil t)
    (ert-fail (format "Could not find %S" text)))
  (get-text-property (- (point) (length text)) 'face))

(defun ende-test--presentation-doc-at (text &optional start)
  "Return Ende ElDoc help with point on TEXT, searching from START."
  (goto-char (or start (point-min)))
  (unless (search-forward text nil t)
    (ert-fail (format "Could not find %S" text)))
  (goto-char (- (point) (length text)))
  (ende--eldoc-documentation))

(ert-deftest ende-test/config-presentation-font-lock-is-block-scoped ()
  (with-temp-buffer
    (org-mode)
    (insert "Ordinary prose: tools inspect and resource outside.\n"
            "#+begin_ende\n"
            "(resource docs (:type file) (include \"file;url\") (exclude \"tmp\") ; note\n"
            "(tools inspect)\n"
            "#+end_ende\n")
    (ende-mode 1)
    (font-lock-ensure (point-min) (point-max))
    (let ((block-start (save-excursion
                         (goto-char (point-min))
                         (search-forward "#+begin_ende")
                         (line-beginning-position)))
          (quoted-value-start (save-excursion
                                (goto-char (point-min))
                                (search-forward "\"file;url\"")
                                (- (point) (length "file;url\"")))))
      (should-not (ende-test--presentation-face-at "tools" (point-min)))
      (should-not (ende-test--presentation-face-at "resource" (point-min)))
      (should (eq (ende-test--presentation-face-at "#+begin_ende")
                  'font-lock-preprocessor-face))
      (should (eq (ende-test--presentation-face-at "#+end_ende")
                  'font-lock-preprocessor-face))
      (should (eq (ende-test--presentation-face-at "resource" block-start)
                  'font-lock-keyword-face))
      (should (eq (ende-test--presentation-face-at "include" block-start)
                  'font-lock-builtin-face))
      (should (eq (ende-test--presentation-face-at "exclude" block-start)
                  'font-lock-builtin-face))
      (should (eq (ende-test--presentation-face-at ":type" block-start)
                  'font-lock-type-face))
      (should (eq (ende-test--presentation-face-at "file" block-start)
                  'font-lock-constant-face))
      (should-not (ende-test--presentation-face-at "file" quoted-value-start))
      (should-not (ende-test--presentation-face-at ";" block-start))
      (should (eq (ende-test--presentation-face-at "; note" (point-min))
                  'font-lock-comment-delimiter-face))
      (should (eq (ende-test--presentation-face-at "note" block-start)
                  'font-lock-comment-face)))))

(ert-deftest ende-test/config-presentation-eldoc-and-disable-are-buffer-local ()
  (with-temp-buffer
    (org-mode)
    (insert "resource outside\ninclude outside\n:type outside\n"
            "set-tool-resources outside\n#+begin_ende\n"
            "(resource docs (:type file) (include \"*.el\"))\n"
            "(set-tool-resources inspect :resources docs)\n"
            "#+end_ende\n")
    (let ((unrelated-provider (lambda () "other help"))
          (unrelated-keyword '("outside" . font-lock-warning-face)))
      (font-lock-add-keywords nil (list unrelated-keyword))
      (add-hook 'eldoc-documentation-functions unrelated-provider nil t)
      (ende-mode 1)
      (let ((block-start (save-excursion
                           (goto-char (point-min))
                           (search-forward "#+begin_ende")
                           (line-beginning-position))))
        (dolist (token '("resource" "include" ":type"
                         "set-tool-resources" "#+begin_ende"))
          (should (> (length (ende-test--presentation-doc-at token block-start))
                     0))))
      (dolist (token '("resource" "include" ":type" "set-tool-resources"))
        (should-not (ende-test--presentation-doc-at token (point-min))))
      (should (memq #'ende--eldoc-documentation
                    eldoc-documentation-functions))
      (ende-mode -1)
      (should-not (memq #'ende--eldoc-documentation
                        eldoc-documentation-functions))
      (should (memq unrelated-provider eldoc-documentation-functions))
      (font-lock-ensure (point-min) (point-max))
      (should (eq (ende-test--presentation-face-at "outside")
                  'font-lock-warning-face))
      (should-not (eq (ende-test--presentation-face-at "#+begin_ende")
                      'font-lock-preprocessor-face)))))

(provide 'ende-config-presentation-test)

;;; ende-config-presentation-test.el ends here
