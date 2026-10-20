;;; ende-queue-dashboard-render-test.el --- Dashboard render regressions -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'ende-test-helpers)
(require 'ende-queue-ui)

(ert-deftest ende-test/dashboard-expanded-running-job-does-not-parse-source ()
  (ende-test--reset)
  (when (buffer-live-p ende-queue-ui--buffer)
    (kill-buffer ende-queue-ui--buffer))
  (clrhash ende-queue-ui--expanded-jobs)
  (let ((source (generate-new-buffer " *ende-dashboard-render-source*"))
        dashboard job)
    (unwind-protect
        (progn
          (with-current-buffer source
          (org-mode)
          (setq job
                (ende--make-job
                 :id 814 :type 'chat :state 'running
                 :target (ende-target-create
                          :buffer source :start (point-min)
                          :end (point-max) :cursor (point-min)
                          :review-policy 'ignore)
                 :profile '(:name remote :model "fixture"
                                  :tool-definitions (tool))
                 :activity-title "Inspecting source"
                 :activity-status "Still running"
                 :activity-updated-at 0)))
          (ende-job-latest-user-set job "Please inspect this source.")
          (setq ende--active-jobs (list job))
          (setq dashboard (ende-queue-open))
          (should (ende-job-p job))
          (cl-letf (((symbol-function 'ende-chat--build-document)
                     (lambda () (error "Unexpected source parse"))))
            (with-current-buffer dashboard
              (goto-char (ende-queue-ui--position-for-key
                          (cons 'job (ende-job-id job))))
              (ende-queue-ui-toggle-entry)
              (should (string-match-p "Activity: Inspecting source"
                                      (buffer-string)))
              (should (string-match-p "Execution: Running" (buffer-string)))
              (should (string-match-p "Tool calls: 0" (buffer-string)))
              (should (string-match-p "Latest user message" (buffer-string)))
              (should (gethash (ende-job-id job)
                               ende-queue-ui--expanded-jobs)))
            (ende-queue-ui--refresh-callback dashboard)
            (with-current-buffer dashboard
              (should (string-match-p "Activity: Inspecting source"
                                      (buffer-string)))
              (should (string-match-p "Tool calls: 0" (buffer-string)))
              (should (gethash (ende-job-id job)
                               ende-queue-ui--expanded-jobs)))))
      (when (buffer-live-p dashboard)
        (kill-buffer dashboard))
      (when (buffer-live-p source)
        (kill-buffer source))
      (ende-test--reset)
      (clrhash ende-queue-ui--expanded-jobs))))

(provide 'ende-queue-dashboard-render-test)

;;; ende-queue-dashboard-render-test.el ends here
