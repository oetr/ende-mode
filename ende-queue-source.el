;;; ende-queue-source.el --- Retained task source navigation -*- lexical-binding: t; -*-

;;; Code:

(require 'ende-queue)
(require 'org)

(defun ende-queue-source--inside-work-p ()
  "Return non-nil when the current heading is inside generated agent work."
  (save-excursion
    (let (inside)
      (while (and (not inside) (org-up-heading-safe))
        (setq inside (equal (org-entry-get nil "ENDE_CHAT_WORK" nil) "t")))
      inside)))

(defun ende-queue-source--chat-position (family)
  "Find FAMILY's reply or question by durable identity in the current buffer."
  (let ((request (ende-job-family-request-id family))
        (question (ende-job-family-question-id family)))
    (when (and (derived-mode-p 'org-mode) (or request question))
      (save-excursion
        (goto-char (point-min))
        (let (reply question-position)
        (while (re-search-forward org-heading-regexp nil t)
          (let ((position (match-beginning 0)))
            (goto-char position)
            (when (and question
                       (equal (org-entry-get nil "ENDE_CHAT_ID" nil) question)
                       (equal (org-entry-get nil "ENDE_ROLE" nil) "user"))
              (setq question-position position))
            (when (and request
                       (equal (org-entry-get nil "ENDE_CHAT_REQUEST_ID" nil) request)
                       (equal (org-entry-get nil "ENDE_ROLE" nil) "assistant")
                       (not (org-entry-get nil "ENDE_CHAT_REASONING_FOR" nil))
                       (not (org-entry-get nil "ENDE_CHAT_WORK" nil))
                       (not (ende-queue-source--inside-work-p)))
              (setq reply position)))
          (forward-line 1))
        (or reply question-position))))))

(defun ende-queue-visit-source (job)
  "Visit retained JOB's originating source, reopening its file if necessary."
  (let* ((family (ende-job-family job))
         (origin (ende-job-family-origin family))
         (file (ende-job-family-source-file family))
         (buffer (or (and (buffer-live-p origin) origin)
                     (and file (file-exists-p file) (find-file-noselect file))))
         (marker (ende-job-family-source-marker family))
         (line (ende-job-family-source-line family))
         position)
    (unless buffer
      (user-error "Source unavailable: %s" (ende-job-family-origin-name family)))
    (with-current-buffer buffer
      (widen)
      (setq position
            (or (ende-queue-source--chat-position family)
                (and (markerp marker) (eq (marker-buffer marker) buffer)
                     (marker-position marker))
                (and line (save-excursion
                            (goto-char (point-min))
                            (forward-line (1- line)) (point)))
                (point-min))))
    (pop-to-buffer buffer)
    (goto-char position)
    (when (derived-mode-p 'org-mode) (org-reveal))))

(provide 'ende-queue-source)
;;; ende-queue-source.el ends here
