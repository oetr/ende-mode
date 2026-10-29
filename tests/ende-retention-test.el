;;; ende-retention-test.el --- Family lifetime integration -*- lexical-binding: t; -*-

(require 'ende-chat-test-helpers)

(ert-deftest ende-test/retention-owns-complete-families-until-eviction ()
  (ende-test--reset)
  (let* ((ende-queue-finished-limit 1)
         (ende-llm-function (ende-test--fake-llm nil t))
         (ende--queue-suspended t)
         (ende--job-handlers (cons '(insert . ignore) ende--job-handlers))
         root child old-family child-work active second)
    (unwind-protect
        (with-temp-buffer
          (insert "user-owned source")
          (setq root (ende-enqueue (ende-test--dashboard-request
                                   ende-test--dashboard-profile)))
          (setq old-family (ende-job-family root))
          (let ((ende--queue-suspended nil)) (ende-process-next))
          (setq child
                (ende-job-publish-child
                 root (list 'agent :profile ende-test--dashboard-profile
                            :instruction '(:task "Retained child")
                            :prepare #'ende-agent-private-target)
                 (ende-job-next-child-path root)))
          (setq child-work (ende-job-work-buffer child))
          (ende-history-begin child '(("user" . "private prompt")))
          (ende--job-settle child 'completed "child answer")
          (should-not (ende-job-children root))
          (should (= (length (ende-job-family-members old-family)) 2))
          (should (eq child (ende-queue-retained-job-by-id (ende-job-id child))))
          (should-not ende--finished-families)
          (ende-job-finish root "root answer")
          (should (equal ende--finished-families (list old-family)))
          (should (buffer-live-p child-work))
          (setq active (ende-enqueue (ende-test--dashboard-request
                                     ende-test--dashboard-profile))
                second (ende-enqueue (ende-test--dashboard-request
                                     ende-test--dashboard-profile)))
          (ende--job-settle second 'completed "newest")
          (should-not (gethash (ende-job-id child) ende--retained-jobs))
          (should-not (gethash (ende-job-id root) ende--retained-jobs))
          (should-not (buffer-live-p child-work))
          (should-not (ende-job-family-members old-family))
          (should-not (ende-job-instruction child))
          (should (eq active (gethash (ende-job-id active) ende--retained-jobs)))
          (should (buffer-live-p (current-buffer)))
          (should (string-prefix-p "user-owned source" (buffer-string)))
          (with-current-buffer (ende-history--buffer)
            (should-not (string-match-p "private prompt\\|child answer" (buffer-string))))
          (let ((ende-queue-finished-limit 0))
            (ende--job-settle active 'cancelled)
            (should-not ende--finished-families)))
      (ende-test--reset))))

(provide 'ende-retention-test)
