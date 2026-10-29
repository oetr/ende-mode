;;; ende-title-test.el --- Title protocol integration -*- lexical-binding: t; -*-

(require 'ende-chat-test-helpers)

(ert-deftest ende-test/title-tool-is-internal-and-phase-gated ()
  (should (member "generate_title"
                  (ende-test--tool-definition-names (ende-tool-definitions))))
  (dolist (forms '(((tools generate_title))
                   ((group titles generate_title) (tools titles))
                   ((agent-config worker (tools generate_title))
                    (spawn-agent-config worker))))
    (with-temp-buffer
      (org-mode)
      (insert (ende-test--chat-config-block forms) "* Task\nWork.\n")
      (let ((document (ende-chat--build-document)))
        (should-error
         (ende-chat-config-resolve-environment
          (ende-chat--document-tree document)
          (ende-chat--document-parent document)
          (car (ende-chat--document-headlines document)))))))
  (let ((title nil)
        (ende-tools--request-phase 'title))
    (let ((ende-tools--generate-title-function
           (lambda (value) (setq title value))))
      (should-not (plist-get (ende-tool-call 'about_thinking
                                            '(:title "Work" :status "Work.")) :ok))
      (dolist (value '("" "  " "two\nlines" "* Heading" "\"Quoted\""))
        (should-not (plist-get (ende-tool-call 'generate_title
                                              (list :title value)) :ok)))
      (should (plist-get (ende-tool-call 'generate_title
                                        '(:title "Inspect queue lifecycle")
                                        nil '(:active-tools nil)) :ok))
      (should (equal title "Inspect queue lifecycle"))))
  (should-not (plist-get (ende-tool-call 'generate_title
                                        '(:title "Forbidden in work")) :ok)))

(provide 'ende-title-test)
