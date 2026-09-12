;;; deslop-history.el --- Retained deslop job output -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools

;;; Commentary:

;; History is an event sink.  Failures in this optional sink must never affect
;; queue settlement or buffer application.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'outline)
(require 'subr-x)
(require 'deslop-core)
(require 'deslop-queue)

(defcustom deslop-history-enabled t
  "Non-nil when completed jobs are retained in a history buffer."
  :type 'boolean
  :group 'deslop)

(defcustom deslop-history-retain-prompts t
  "Non-nil when history entries retain complete prompts and source context."
  :type 'boolean
  :group 'deslop)

(defcustom deslop-history-retain-thinking t
  "Non-nil when history entries retain provider reasoning traces."
  :type 'boolean
  :group 'deslop)

(defcustom deslop-history-max-entries 100
  "Maximum number of completed jobs retained in the history buffer."
  :type 'integer
  :group 'deslop)

(defcustom deslop-history-buffer-name "*deslop-history*"
  "Name of the buffer used for retained LLM job history."
  :type 'string
  :group 'deslop)

(cl-defstruct (deslop-history--entry
               (:constructor deslop-history--make-entry))
  job type prompt thinking-parts content-parts status metadata usage result error
  started-at finished-at)

(defvar deslop-history--entries (make-hash-table :test #'eq)
  "In-progress history entries keyed by job object.")

(defvar deslop-history--buffer-object nil
  "History buffer owned by deslop, when one exists.")

(defvar-local deslop-history--owned-buffer nil
  "Non-nil in a history buffer created by deslop.")

(defvar-local deslop-history--internal-write nil
  "Non-nil while deslop is updating the history buffer.")

(defun deslop-history--prevent-user-edit (&rest _)
  "Prevent edits to the history buffer outside deslop internals."
  (unless deslop-history--internal-write
    (user-error "deslop history is maintained by deslop")))

(define-derived-mode deslop-history-mode outline-mode "deslop-history"
  "Major mode for retained deslop request history."
  (setq-local outline-regexp "^\\*+ ")
  (buffer-disable-undo)
  (add-hook 'before-change-functions
            #'deslop-history--prevent-user-edit nil t)
  (setq-local buffer-read-only t))

(defun deslop-history--prompt-string (messages)
  "Format MESSAGES for storage in a history entry."
  (mapconcat (lambda (message)
               (format "%s:\n%s" (car message) (cdr message)))
             messages "\n\n"))

(defun deslop-history--entry (job)
  "Return the active history entry for JOB."
  (gethash job deslop-history--entries))

(defun deslop-history-begin (job messages)
  "Start retaining JOB with MESSAGES as its prompt."
  (when deslop-history-enabled
    (condition-case nil
        (puthash job
                  (deslop-history--make-entry
                   :job job
                   :type (deslop-job-type job)
                   :prompt (and deslop-history-retain-prompts
                                (deslop-history--prompt-string messages))
                  :status "Queued"
                  :started-at (float-time))
                 deslop-history--entries)
       ((error quit) nil))))

(defun deslop-history--finish-entry (entry status &optional result error)
  "Set terminal fields on ENTRY, flush it, and return nil."
  (setf (deslop-history--entry-status entry) status
        (deslop-history--entry-result entry) result
        (deslop-history--entry-error entry) error
        (deslop-history--entry-finished-at entry) (float-time))
  (deslop-history--flush entry))

(defun deslop-history-event (job event)
  "Record normalized EVENT for JOB without propagating sink errors."
  (let ((type (plist-get event :type)))
    (if (eq type :begin)
        (deslop-history-begin job (plist-get event :messages))
      (let* ((entry (deslop-history--entry job))
             (terminal (memq type '(:done :error :cancelled))))
        (when entry
          (unwind-protect
              (condition-case nil
                  (when deslop-history-enabled
                    (pcase type
                      (:status
                       (setf (deslop-history--entry-status entry)
                             (plist-get event :status)))
                      (:delta
                       (if (eq (plist-get event :channel) :thinking)
                           (when deslop-history-retain-thinking
                             (push
                              (plist-get event :text)
                              (deslop-history--entry-thinking-parts entry)))
                         (push (plist-get event :text)
                               (deslop-history--entry-content-parts entry))))
                      (:usage
                       (setf (deslop-history--entry-usage entry) event))
                      (:metadata
                       (setf (deslop-history--entry-metadata entry)
                             (plist-get event :metadata)))
                      (:done
                       (let* ((content (plist-get event :content))
                              (result (plist-get event :result))
                              (thinking (and deslop-history-retain-thinking
                                             (listp result)
                                             (plist-get result :thinking)))
                              (usage (and (listp result)
                                           (plist-get result :usage))))
                         (when (and (stringp thinking)
                                    (> (length thinking) 0)
                                    (null
                                     (deslop-history--entry-thinking-parts
                                      entry)))
                           (setf (deslop-history--entry-thinking-parts entry)
                                 (list thinking)))
                         (when (and (stringp content)
                                    (> (length content) 0))
                           (setf
                            (deslop-history--entry-content-parts entry)
                            (list content)))
                         (when usage
                           (setf (deslop-history--entry-usage entry)
                                 (list :type :usage
                                       :usage usage
                                       :tokens (plist-get result :tokens)
                                       :speed (plist-get result :speed)
                                       :estimated
                                       (plist-get result :estimated)))))
                       (deslop-history--finish-entry
                        entry "Completed" (plist-get event :result)))
                      (:error
                       (deslop-history--finish-entry
                        entry "Failed" nil (plist-get event :error)))
                      (:cancelled
                       (deslop-history--finish-entry entry "Cancelled"))))
                ((error quit) nil))
            (when terminal
              (remhash job deslop-history--entries))))))))

(defun deslop-history--buffer ()
  "Return the history buffer, configuring outline folding once."
  (setq deslop-history--buffer-object
        (deslop--owned-buffer deslop-history--buffer-object
                              deslop-history-buffer-name
                              'deslop-history--owned-buffer
                              #'deslop-history-mode))
  (with-current-buffer deslop-history--buffer-object
    (setq-local buffer-read-only t))
  deslop-history--buffer-object)

(defun deslop-history--insert-section (title text)
  "Insert a history section titled TITLE containing safely indented TEXT."
  (when (and (stringp text) (not (string-empty-p text)))
    (insert "** " title "\n")
    (dolist (line (split-string text "\n" nil))
      (insert "  " line "\n"))))

(defun deslop-history--trim-buffer ()
  "Remove oldest complete entries until the entry limit is met."
  (let ((continue t))
    (while continue
      (goto-char (point-min))
      (let ((count 0)
            (second nil))
        (while (re-search-forward "^\\* Job " nil t)
          (setq count (1+ count))
          (when (= count 2)
            (setq second (line-beginning-position))))
        (if (and second
                 (> count (max 1 deslop-history-max-entries)))
            (delete-region (point-min) second)
          (setq continue nil))))))

(defun deslop-history--value-string (value)
  "Return VALUE as readable JSON when possible, otherwise printed Lisp."
  (condition-case nil
      (if (or (hash-table-p value) (vectorp value) (listp value))
          (json-encode value)
        (format "%s" value))
    (error (format "%S" value))))

(defun deslop-history--usage-string (event)
  "Return readable usage information from normalized USAGE EVENT."
  (let ((raw (plist-get event :usage)))
    (if raw
        (format "Stream tokens: %s\nEstimated: %s\nSpeed: %.2f t/s\nProvider usage: %s"
                (or (plist-get event :tokens) 0)
                (if (plist-get event :estimated) "yes" "no")
                (or (plist-get event :speed) 0.0)
                (deslop-history--value-string raw))
      (format "%S" event))))

(defun deslop-history--flush (entry)
  "Append ENTRY to the entry-bounded history buffer."
  (when deslop-history-enabled
    (let* ((job (deslop-history--entry-job entry))
           (result (deslop-history--entry-result entry))
           (error (deslop-history--entry-error entry))
           (thinking (mapconcat #'identity
                                (nreverse
                                 (deslop-history--entry-thinking-parts entry))
                                ""))
           (content (mapconcat #'identity
                               (nreverse
                                (deslop-history--entry-content-parts entry))
                               ""))
            (usage (deslop-history--entry-usage entry))
            (metadata (deslop-history--entry-metadata entry))
           (status (deslop-history--entry-status entry))
           (buffer (deslop-history--buffer)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (deslop-history--internal-write t))
          (save-restriction
            (widen)
            (save-excursion
              (goto-char (point-max))
              (insert (format "\n* Job %d: %s [%s] %s\n"
                              (deslop-job-id job)
                              (deslop-job-type job)
                              status
                              (format-time-string "%Y-%m-%d %H:%M:%S")))
              (deslop-history--insert-section
               "Prompt" (deslop-history--entry-prompt entry))
              (deslop-history--insert-section "Thinking" thinking)
              (deslop-history--insert-section "Output" content)
              (when metadata
                (deslop-history--insert-section
                 "Provider"
                 (format "Client: %s\nModel: %s"
                         (or (plist-get metadata :client)
                             (plist-get metadata :provider)
                             "unknown")
                         (or (plist-get metadata :model) "unknown"))))
              (when (listp result)
                (deslop-history--insert-section
                 "Metrics"
                 (format "Tokens: %s\nEstimated: %s\nSpeed: %.2f t/s"
                         (or (plist-get result :tokens) 0)
                         (if (plist-get result :estimated) "yes" "no")
                         (or (plist-get result :speed) 0.0))))
              (when usage
                (deslop-history--insert-section
                 "Usage" (deslop-history--usage-string usage)))
              (when error
                (deslop-history--insert-section
                 "Error" (deslop-error-display-message error)))
              (deslop-history--trim-buffer))))))))

(defun deslop-history ()
  "Display the retained deslop job history buffer."
  (interactive)
  (display-buffer (deslop-history--buffer)))

(defun deslop-history-clear ()
  "Clear all retained and in-progress deslop history."
  (interactive)
  (clrhash deslop-history--entries)
  (when (buffer-live-p deslop-history--buffer-object)
    (with-current-buffer deslop-history--buffer-object
      (let ((inhibit-read-only t)
            (deslop-history--internal-write t))
        (erase-buffer))))
  (message "deslop: history cleared"))

(add-hook 'deslop-job-event-functions #'deslop-history-event)

(provide 'deslop-history)

;;; deslop-history.el ends here
