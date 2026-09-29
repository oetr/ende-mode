;;; ende-history.el --- Retained ende job output -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools

;;; Commentary:

;; History is an event sink.  Failures in this optional sink must never affect
;; queue settlement or buffer application.

;;; Code:

(require 'cl-lib)
(require 'outline)
(require 'subr-x)
(require 'ende-core)
(require 'ende-edit)
(require 'ende-queue)

(defcustom ende-history-enabled t
  "Non-nil when completed jobs are retained in a history buffer."
  :type 'boolean
  :group 'ende)

(defcustom ende-history-retain-prompts t
  "Non-nil when history entries retain complete prompts and source context."
  :type 'boolean
  :group 'ende)

(defcustom ende-history-retain-thinking t
  "Non-nil when history entries retain provider reasoning traces."
  :type 'boolean
  :group 'ende)

(defcustom ende-history-max-entries 100
  "Maximum number of completed jobs retained in the history buffer."
  :type 'integer
  :group 'ende)

(defcustom ende-history-buffer-name "*ende-history*"
  "Name of the buffer used for retained LLM job history."
  :type 'string
  :group 'ende)

(cl-defstruct (ende-history--entry
               (:constructor ende-history--make-entry))
  job type prompt thinking-parts content-parts diagnostic-parts status metadata
  usage result partial-result finish-reason error started-at finished-at)

(defvar ende-history--entries (make-hash-table :test #'eq)
  "In-progress history entries keyed by job object.")

(defvar ende-history--buffer-object nil
  "History buffer owned by ende, when one exists.")

(defvar-local ende-history--owned-buffer nil
  "Non-nil in a history buffer created by ende.")

(defvar-local ende-history--internal-write nil
  "Non-nil while ende is updating the history buffer.")

(defvar-local ende-history--entry-count 0
  "Number of top-level job entries currently rendered in the buffer.")

(defun ende-history--prevent-user-edit (&rest _)
  "Prevent edits to the history buffer outside ende internals."
  (unless ende-history--internal-write
    (user-error "ende history is maintained by ende")))

(define-derived-mode ende-history-mode outline-mode "ende-history"
  "Major mode for retained ende request history."
  (setq-local outline-regexp "^\\*+ ")
  (buffer-disable-undo)
  (add-hook 'before-change-functions
            #'ende-history--prevent-user-edit nil t)
  (setq-local buffer-read-only t))

(defun ende-history--prompt-string (messages)
  "Format MESSAGES for storage in a history entry."
  (mapconcat (lambda (message)
               (format "%s:\n%s" (car message) (cdr message)))
             messages "\n\n"))

(defun ende-history--entry (job)
  "Return the active history entry for JOB."
  (gethash job ende-history--entries))

(defun ende-history-begin (job messages)
  "Start retaining JOB with MESSAGES as its prompt."
  (when ende-history-enabled
    (condition-case nil
        (puthash job
                 (ende-history--make-entry
                  :job job
                  :type (ende-job-type job)
                  :prompt (and ende-history-retain-prompts
                               (ende-history--prompt-string messages))
                  :status "Queued"
                  :started-at (float-time))
                 ende-history--entries)
      ((error quit) nil))))

(defun ende-history--finish-entry (entry status &optional result error)
  "Set terminal fields on ENTRY, flush it, and return nil."
  (setf (ende-history--entry-status entry) status
        (ende-history--entry-result entry) result
        (ende-history--entry-error entry) error
        (ende-history--entry-finished-at entry) (float-time))
  (ende-history--flush entry))

(defun ende-history--record-partial-result (entry result)
  "Record normalized partial RESULT in ENTRY."
  (when (and result (listp result))
    (setf (ende-history--entry-partial-result entry) result
          (ende-history--entry-finish-reason entry)
          (plist-get result :finish-reason))
    (let ((content (plist-get result :content))
          (thinking (plist-get result :thinking)))
      (when (and (stringp content) (> (length content) 0))
        (setf (ende-history--entry-content-parts entry) (list content)))
      (when (and ende-history-retain-thinking
                 (stringp thinking) (> (length thinking) 0))
        (setf (ende-history--entry-thinking-parts entry) (list thinking))))))

(defun ende-history-event (job event)
  "Record normalized EVENT for JOB without propagating sink errors."
  (let ((type (plist-get event :type)))
    (if (eq type :begin)
        (ende-history-begin job (plist-get event :messages))
      (let* ((entry (ende-history--entry job))
             (terminal (memq type '(:done :error :cancelled))))
        (when entry
          (unwind-protect
              (condition-case caught
                  (when ende-history-enabled
                    (cond
                     ((eq type :status)
                      (setf (ende-history--entry-status entry)
                            (plist-get event :status)))
                     ((eq type :delta)
                      (if (eq (plist-get event :channel) :thinking)
                          (when ende-history-retain-thinking
                            (push (plist-get event :text)
                                  (ende-history--entry-thinking-parts
                                   entry)))
                        (push (plist-get event :text)
                              (ende-history--entry-content-parts entry))))
                     ((eq type :partial-result)
                      (ende-history--record-partial-result
                       entry (plist-get event :result)))
                     ((eq type :usage)
                      (setf (ende-history--entry-usage entry) event))
                     ((eq type :metadata)
                      (setf (ende-history--entry-metadata entry)
                            (plist-get event :metadata)))
                     ((eq type :diagnostic)
                      (when-let* ((text (plist-get event :text)))
                        (push text (ende-history--entry-diagnostic-parts
                                    entry))))
                     ((eq type :done)
                      (let* ((content (plist-get event :content))
                             (result (plist-get event :result))
                             (thinking (and ende-history-retain-thinking
                                            (listp result)
                                            (plist-get result :thinking)))
                             (usage (and (listp result)
                                         (plist-get result :usage)))
                             (summary (ende-history--structured-summary
                                       result)))
                        (when (and (stringp thinking)
                                   (> (length thinking) 0)
                                   (null
                                    (ende-history--entry-thinking-parts
                                     entry)))
                          (setf (ende-history--entry-thinking-parts entry)
                                (list thinking)))
                        (when (and (stringp content)
                                   (> (length content) 0))
                          (setf (ende-history--entry-content-parts entry)
                                (list content)))
                        (when usage
                          (setf (ende-history--entry-usage entry)
                                (list :type :usage
                                      :usage usage
                                      :tokens (plist-get result :tokens)
                                      :speed (plist-get result :speed)
                                      :estimated (plist-get result :estimated))))
                        (when (listp result)
                          (setf (ende-history--entry-finish-reason entry)
                                (plist-get result :finish-reason)))
                        (ende-history--finish-entry
                         entry
                         (if (> (or (plist-get summary :skipped) 0) 0)
                             "COMPLETED WITH WARNINGS"
                           "Completed")
                         result)))
                     ((eq type :error)
                      (ende-history--record-partial-result
                       entry (plist-get event :result))
                      (ende-history--finish-entry
                       entry "Failed" nil (plist-get event :error)))
                     ((eq type :cancelled)
                      (ende-history--finish-entry entry "Cancelled"))))
                (error (ignore caught))
                (quit (ignore caught)))
            (when terminal
              (remhash job ende-history--entries))))))))

(defun ende-history--buffer ()
  "Return the history buffer, configuring outline folding once."
  (setq ende-history--buffer-object
        (ende-owned-buffer ende-history--buffer-object
                           ende-history-buffer-name
                           'ende-history--owned-buffer
                           #'ende-history-mode))
  (with-current-buffer ende-history--buffer-object
    (setq-local buffer-read-only t))
  ende-history--buffer-object)

(defun ende-history--insert-section (title text)
  "Insert a history section titled TITLE containing safely indented TEXT."
  (when (and (stringp text) (not (string-empty-p text)))
    (insert "** " title "\n")
    (dolist (line (split-string text "\n" nil))
      (insert "  " line "\n"))))

(defun ende-history--trim-buffer ()
  "Remove oldest complete entries until the entry limit is met."
  (let ((limit (max 1 ende-history-max-entries)))
    (while (> ende-history--entry-count limit)
      (goto-char (point-min))
      (if (re-search-forward "^\\* Job " nil t)
          (let ((start (line-beginning-position))
                (end (save-excursion
                       (forward-line 1)
                       (if (re-search-forward "^\\* Job " nil t)
                           (line-beginning-position)
                         (point-max)))))
            (delete-region start end)
            (cl-decf ende-history--entry-count))
        (setq ende-history--entry-count 0)))))

(defun ende-history--usage-get (object key)
  "Return KEY from provider usage OBJECT.
KEY is a keyword for plist objects and a string key in JSON hash tables."
  (cond
   ((hash-table-p object)
    (gethash (substring (symbol-name key) 1) object))
   ((listp object) (plist-get object key))))

(defun ende-history--usage-number (usage &rest keys)
  "Return the first numeric field in USAGE named by KEYS."
  (cl-loop for key in keys
           for value = (ende-history--usage-get usage key)
           when (numberp value) return value))

(defun ende-history--format-token-count (count)
  "Return integer COUNT with thousands separators."
  (let* ((digits (number-to-string (max 0 (truncate count))))
         (end (length digits))
         chunks)
    (while (> end 3)
      (push (substring digits (- end 3) end) chunks)
      (setq end (- end 3)))
    (push (substring digits 0 end) chunks)
    (mapconcat #'identity chunks ",")))

(defun ende-history--usage-message-summary (usage)
  "Summarize attributed message counts in provider USAGE, when available."
  (let* ((attribution (ende-history--usage-get usage :attribution))
         (items (ende-history--usage-get attribution :items)))
    (when (hash-table-p items)
      (let ((messages 0)
            (input-messages 0)
            (output-messages 0))
        (maphash
         (lambda (id item)
           (when (and (stringp id) (string-prefix-p "msg_" id))
             (let ((input (ende-history--usage-number
                           item :input_tokens))
                   (output (ende-history--usage-number
                            item :output_tokens)))
               (cl-incf messages)
               (when (and (numberp input) (> input 0))
                 (cl-incf input-messages))
               (when (and (numberp output) (> output 0))
                 (cl-incf output-messages)))))
         items)
        (format "Messages: %s (%s input, %s output)"
                (ende-history--format-token-count messages)
                (ende-history--format-token-count input-messages)
                (ende-history--format-token-count output-messages))))))

(defun ende-history--usage-token-summary (usage)
  "Summarize aggregate token counts in provider USAGE, when available."
  (let* ((input (ende-history--usage-number
                 usage :input_tokens :prompt_tokens))
         (output (ende-history--usage-number
                  usage :output_tokens :completion_tokens))
         (total (or (ende-history--usage-number usage :total_tokens)
                    (and (numberp input) (numberp output)
                         (+ input output)))))
    (when (numberp total)
      (if (and (numberp input) (numberp output))
          (format "Tokens: %s total (%s input, %s output)"
                  (ende-history--format-token-count total)
                  (ende-history--format-token-count input)
                  (ende-history--format-token-count output))
        (format "Tokens: %s total"
                (ende-history--format-token-count total))))))

(defun ende-history--skipped-edit-errors-string (errors)
  "Format skipped structured-edit ERRORS for a history entry."
  (ende-edit-format-failures errors))

(defun ende-history--structured-summary (result)
  "Return structured-edit counters from RESULT, when present."
  (and (listp result)
       (listp (plist-get result :structured-summary))
       (plist-get result :structured-summary)))

(defun ende-history--structured-summary-string (summary)
  "Format structured-edit SUMMARY for a history entry."
  (format "Requested: %d\nFirst-pass applied: %d\nRepaired: %d\nSkipped: %d\nRepair attempts: %d"
          (or (plist-get summary :requested) 0)
          (or (plist-get summary :first-pass-applied) 0)
          (or (plist-get summary :repaired) 0)
          (or (plist-get summary :skipped) 0)
          (or (plist-get summary :repair-attempts) 0)))

(defun ende-history--usage-string (event)
  "Return readable usage information from normalized USAGE EVENT."
  (let ((raw (plist-get event :usage)))
    (if raw
        (mapconcat
         #'identity
         (delq nil
               (list (format "Stream tokens: %s"
                             (or (plist-get event :tokens) 0))
                     (format "Estimated: %s"
                             (if (plist-get event :estimated) "yes" "no"))
                     (format "Speed: %.2f t/s"
                             (or (plist-get event :speed) 0.0))
                     (ende-history--usage-message-summary raw)
                     (ende-history--usage-token-summary raw)))
         "\n")
      (format "%S" event))))

(defun ende-history--flush (entry)
  "Append ENTRY to the entry-bounded history buffer."
  (when ende-history-enabled
    (let* ((job (ende-history--entry-job entry))
           (result (ende-history--entry-result entry))
           (error (ende-history--entry-error entry))
           (thinking (mapconcat #'identity
                                (nreverse
                                 (ende-history--entry-thinking-parts entry))
                                ""))
           (content (mapconcat #'identity
                               (nreverse
                                (ende-history--entry-content-parts entry))
                               ""))
           (diagnostics (mapconcat #'identity
                                   (nreverse
                                    (ende-history--entry-diagnostic-parts entry))
                                   "\n"))
           (usage (ende-history--entry-usage entry))
           (metrics-result (or result
                               (ende-history--entry-partial-result entry)))
           (finish-reason (or (ende-history--entry-finish-reason entry)
                              (and (listp metrics-result)
                                   (plist-get metrics-result :finish-reason))))
           (partial-result (ende-history--entry-partial-result entry))
           (structured-summary
            (or (ende-history--structured-summary result)
                (ende-history--structured-summary partial-result)))
           (skipped-edit-errors
            (or (and (listp result)
                     (or (plist-get result :write-anywhere-errors)
                         (plist-get result :insert-anywhere-errors)))
                (and (listp partial-result)
                     (or (plist-get partial-result :write-anywhere-errors)
                         (plist-get partial-result :insert-anywhere-errors)))))
           (metadata (ende-history--entry-metadata entry))
           (status (ende-history--entry-status entry))
           (buffer (ende-history--buffer)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (ende-history--internal-write t))
          (save-restriction
            (widen)
            (save-excursion
              (goto-char (point-max))
              (insert (format "\n* Job %d: %s [%s] %s\n"
                              (ende-job-id job)
                              (ende-job-type job)
                              status
                              (format-time-string "%Y-%m-%d %H:%M:%S")))
              (ende-history--insert-section
               "Prompt" (ende-history--entry-prompt entry))
              (ende-history--insert-section "Thinking" thinking)
              (ende-history--insert-section "Output" content)
              (ende-history--insert-section "Diagnostics" diagnostics)
              (when skipped-edit-errors
                (ende-history--insert-section
                 "Skipped edits"
                 (ende-history--skipped-edit-errors-string
                  skipped-edit-errors)))
              (when structured-summary
                (ende-history--insert-section
                 "Structured edits"
                 (ende-history--structured-summary-string
                  structured-summary)))
              (when metadata
                (ende-history--insert-section
                 "Provider"
                 (format "Client: %s\nModel: %s"
                         (or (plist-get metadata :client)
                             (plist-get metadata :provider)
                             "unknown")
                         (or (plist-get metadata :model) "unknown"))))
              (when (or (listp metrics-result) finish-reason)
                (ende-history--insert-section
                 "Metrics"
                 (format "Tokens: %s\nEstimated: %s\nSpeed: %.2f t/s%s"
                         (or (and (listp metrics-result)
                                  (plist-get metrics-result :tokens))
                             0)
                         (if (and (listp metrics-result)
                                  (plist-get metrics-result :estimated))
                             "yes" "no")
                         (or (and (listp metrics-result)
                                  (plist-get metrics-result :speed))
                             0.0)
                         (if finish-reason
                             (format "\nFinish reason: %s" finish-reason)
                           ""))))
              (when usage
                (ende-history--insert-section
                 "Usage" (ende-history--usage-string usage)))
              (when error
                (ende-history--insert-section
                 "Error" (ende-error-display-message error)))
              (cl-incf ende-history--entry-count)
              (ende-history--trim-buffer))))))))

(defun ende-history ()
  "Display the retained ende job history buffer."
  (interactive)
  (display-buffer (ende-history--buffer)))

(defun ende-history-clear ()
  "Clear all retained and in-progress ende history."
  (interactive)
  (clrhash ende-history--entries)
  (when (buffer-live-p ende-history--buffer-object)
    (with-current-buffer ende-history--buffer-object
      (let ((inhibit-read-only t)
            (ende-history--internal-write t))
        (erase-buffer)
        (setq ende-history--entry-count 0))))
  (message "ende: history cleared"))

(add-hook 'ende-job-event-functions #'ende-history-event)

(provide 'ende-history)

;;; ende-history.el ends here
