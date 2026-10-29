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
      (dolist (value '("" "  " "two\nlines" "* Heading" "\"Quoted\""
                       "'Quoted'" "/Italic/" "_Underlined_" "+Struck+"))
        (should-not (plist-get (ende-tool-call 'generate_title
                                              (list :title value)) :ok)))
      (should (plist-get (ende-tool-call 'generate_title
                                        '(:title "Inspect queue lifecycle")
                                        "/not/a/required/project"
                                        '(:active-tools nil :root "/missing")) :ok))
      (should (equal title "Inspect queue lifecycle"))))
  (should-not (plist-get (ende-tool-call 'generate_title
                                        '(:title "Forbidden in work")) :ok)))

(ert-deftest ende-test/title-phase-isolates-a-whole-batch-and-continues ()
  (ende-test--reset)
  (let* ((ende-test--title-prelude t)
         (requests nil) (effects 0)
         (ende-tools--registry (copy-sequence ende-tools--registry))
         (ende-llm-function
          (lambda (messages callbacks &optional profile)
            (push (cons messages profile) requests)
            (funcall
             (plist-get callbacks :on-done)
             (if (= (length requests) 1)
                 '(:content "Title-only prose" :tool-calls
                   ((:id "title" :name "generate_title"
                     :arguments "{\"title\":\"Inspect queue lifecycle\"}")
                    (:id "spawn" :name "spawn_agent" :arguments "{\"task\":\"Forbidden\"}")
                    (:id "effect" :name "title_test_effect" :arguments "{}")))
               (if (= (length requests) 2)
                   '(:content "" :tool-calls
                     ((:id "effect-work" :name "title_test_effect" :arguments "{}")))
                 '(:content "Actual answer")))))))
    (ende-tool-register 'title_test_effect "Test effect" '((:type . "object"))
                        (lambda (&rest _) (cl-incf effects)) nil :no-resource
                        '(:requires-root nil))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert (ende-test--chat-config-block '((tools title_test_effect)))
                  "* Root\n** \nInspect the queue.\n** Next\nContinue.\n")
          (ende-test--chat-goto "** ")
          (let ((job (ende-ask)))
            (should (eq (ende-job-state job) 'completed))
            (should (equal (ende-job-title job) "Inspect queue lifecycle"))
            (should (= effects 1))
            (should-not (ende-job-children job))
            (should (= (length requests) 3))
            (let* ((ordered (reverse requests))
                   (first (car ordered)) (second (cadr ordered)))
              (should (equal (car (car first)) (car (car second))))
              (should (equal (cdr first) (cdr second)))
              (should (equal (car first) (seq-take (car second) (length (car first))))))
            (should (string-match-p "Actual answer" (buffer-string)))
            (should (string-match-p "ENDE_CHAT_GENERATED_TITLE: t" (buffer-string)))
            (ende-test--chat-goto "** Next")
            (should-not (string-match-p "\\*\\* Inspect queue lifecycle"
                                        (ende-test--chat-context-text
                                         (ende-chat-context-at-point))))))
      (ende-test--reset))))

(ert-deftest ende-test/title-correction-is-bounded-and-keeps-the-local-fallback ()
  (ende-test--reset)
  (let* ((ende-test--title-prelude t)
         (phases nil)
         (ende-llm-function
          (lambda (_messages callbacks &optional profile)
            (let ((job (ende-queue-live-job-by-id (plist-get profile :job-id))))
              (push (ende-job-request-phase job) phases))
            (funcall (plist-get callbacks :on-done)
                     (if (< (length phases) 3)
                         '(:content "Do not use this as the answer")
                       '(:content "Actual final answer"))))))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "* User heading\nDo the real task.\n")
          (goto-char (point-min))
          (let ((job (ende-ask)))
            (should (equal (nreverse phases) '(title title work)))
            (should (equal (ende-job-title job) "User heading"))
            (should (eq (ende-job-state job) 'completed))
            (should (string-match-p "Actual final answer" (buffer-string)))))
      (ende-test--reset))))

(ert-deftest ende-test/delegated-title-prelude-does-not-restart-on-parent-resumption ()
  (ende-test--reset)
  (let* ((ende-test--title-prelude t)
         (requests nil)
         (ende-llm-function
          (lambda (messages callbacks &optional profile)
            (setq requests (append requests (list (list messages callbacks profile))))))
         (title '(:content "" :tool-calls
                  ((:id "title" :name "generate_title"
                    :arguments "{\"title\":\"Stable generated title\"}")))))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert (ende-test--chat-config-block
                   '((tools spawn_agent) (agent-config worker) (spawn-agent-config worker)))
                  "* Root\nDelegate the task.\n")
          (ende-test--chat-goto "* Root")
          (let ((root (ende-ask '(:name remote :model "fixture" :parallel t :max-concurrency 2))))
            (funcall (plist-get (nth 1 (nth 0 requests)) :on-done) title)
            (funcall (plist-get (nth 1 (nth 1 requests)) :on-done)
                     '(:content "" :tool-calls
                       ((:id "spawn" :name "spawn_agent" :arguments "{\"task\":\"Child task\"}"))))
            (should (eq (ende-job-state root) 'suspended))
            (let ((child (car (ende-job-children root))))
              (should (eq (ende-job-request-phase child) 'title))
              (funcall (plist-get (nth 1 (nth 2 requests)) :on-done) title)
              (should (eq (ende-job-request-phase child) 'work))
              (funcall (plist-get (nth 1 (nth 3 requests)) :on-done) '(:content "Child answer"))
              (should (eq (ende-job-state child) 'completed))
              (should (eq (ende-job-state root) 'running))
              (should (= (ende-job-title-attempts root) 1))
              (should (eq (ende-job-request-phase root) 'work))
              (funcall (plist-get (nth 1 (nth 4 requests)) :on-done) '(:content "Parent answer"))
              (should (eq (ende-job-state root) 'completed))
              (should (equal (ende-job-title child) "Stable generated title"))
              (should (= (length requests) 5)))))
      (ende-test--reset))))

(provide 'ende-title-test)
