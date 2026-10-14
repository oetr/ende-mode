;;; ende-agent.el --- Shared agent request and tool loop -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'ende-operations)
(require 'ende-tools)
(require 'ende-queue)

(defcustom ende-agent-max-tool-rounds 100000
  "Maximum model replies requesting tools for one agent job."
  :type 'integer
  :group 'ende)

(defun ende-agent--invoke (callbacks key &rest arguments)
  "Invoke CALLBACKS function at KEY with ARGUMENTS, when present."
  (when-let* ((function (plist-get callbacks key)))
    (apply function arguments)))

(defun ende-agent--send (job messages configuration callbacks rounds)
  "Send MESSAGES for JOB under CONFIGURATION using loop CALLBACKS."
  (ende--dispatch-llm
   job messages
   (lambda (result)
     (when (ende-job-current-p job)
       (ende--call-with-job-error
        job
        (lambda ()
          (ende-agent--handle-result
           job result messages configuration callbacks rounds)))))
   nil nil t))

(defun ende-agent--handle-result
    (job result messages configuration callbacks rounds)
  "Handle model RESULT for JOB under CONFIGURATION using CALLBACKS."
  (let ((calls (ende-agent--invoke callbacks :tool-calls result)))
    (if (null calls)
        (let ((next (ende-agent--invoke callbacks :before-final
                                        job result messages)))
          (if (and (listp next) (not (eq next :finish)))
              (progn
                (ende--validate-messages next)
                (ende-agent--send job next configuration callbacks rounds))
            (ende-agent--invoke callbacks :on-final job result messages)))
      (let ((definitions (plist-get (ende-job-profile job) :tool-definitions))
            (maximum ende-agent-max-tool-rounds))
        (unless definitions
          (error "The provider returned an unrequested tool call"))
        (unless (and (integerp maximum) (> maximum 0))
          (error "ende-agent-max-tool-rounds must be a positive integer"))
        (when (>= rounds maximum)
          (error "Agent exceeded %d tool-call rounds" maximum))
        (let* ((assistant-data
                (ende-agent--invoke callbacks :assistant-message
                                    job result calls))
               (assistant (if (plist-member assistant-data :message)
                             (plist-get assistant-data :message)
                           assistant-data))
               (execution-calls (or (plist-get assistant-data :calls) calls))
               (next-messages (append messages (when assistant (list assistant)))))
          (ende-agent--invoke callbacks :on-tool-request job result calls)
          (ende-agent--run-tools
           job execution-calls configuration callbacks
           (lambda (tool-messages)
             (when (and tool-messages (ende-job-current-p job))
               (let* ((next-messages (append next-messages tool-messages))
                      (next-messages
                       (or (ende-agent--invoke callbacks :after-tool-round
                                               job next-messages)
                           next-messages)))
                 (ende--validate-messages next-messages)
                 (ende-agent--send job next-messages configuration callbacks
                                  (1+ rounds)))))))))))

(defun ende-agent--run-tools (job calls configuration callbacks done)
  "Run CALLS sequentially for JOB and return ordered results through DONE."
  (let ((remaining (copy-sequence calls))
        (messages nil))
    (cl-labels
        ((next ()
           (if (or (null remaining) (not (ende-job-current-p job)))
               (when (and (null remaining) (ende-job-current-p job))
                 (funcall done (nreverse messages)))
             (let* ((call (pop remaining))
                    (_count
                     (ende-job-emit-event
                      job (list :type :tool-call-count
                                :count (1+ (or (ende-job-tool-call-count job) 0)))))
                    (prior-cancel (ende-job-cancel-function job))
                    (installed-cancel nil)
                    (generation (ende-job-execution-generation job))
                    (synchronous t)
                    (completed nil)
                    (ende-tools--execution-owner (ende-job-id job))
                    (ende-tools--report-activity-function
                     (lambda (title status)
                       (ende-job-report-activity job title status)))
                    (ende-tools--register-cancellation-function
                     (lambda (cancel)
                       (if cancel
                           (setq installed-cancel cancel)
                         (when (and installed-cancel
                                    (eq installed-cancel
                                        (ende-job-cancel-function job)))
                           (setf (ende-job-cancel-function job) prior-cancel)))
                       (when cancel
                         (ende-job-cancellation-set job cancel)))))
               (funcall (plist-get callbacks :execute-tool)
                call configuration
                (lambda (content)
                  (setq completed t)
                  (when (ende-job-execution-current-p job generation)
                    (ende--call-with-job-error
                     job
                     (lambda ()
                       (push (ende-agent--invoke
                              callbacks :tool-result-message job call content)
                             messages)
                       (unless synchronous (next)))))))
               (setq synchronous nil)
               (when completed
                 (next))))))
      (next))))

(defun ende-agent-run (job messages configuration callbacks)
  "Run the reusable request and tool loop for JOB.

CALLBACKS supplies :tool-calls, :execute-tool, :on-final,
:assistant-message, :tool-request, and :tool-result-message functions, with
optional :before-final and :after-tool-round message transforms.
Chat persistence and guidance stay in those callbacks."
  (let* ((definitions (ende-tool-definitions configuration))
         (profile (plist-put (copy-tree (ende-job-profile job))
                             :tool-definitions definitions)))
    (when (and definitions
               (or (not (memq 'about_thinking
                              (plist-get configuration :active-tools)))
                   (eq (plist-get profile :transport) 'openrouter-batch)))
      (error (concat "Agent chat requires about_thinking and cannot use the "
                     "OpenRouter batch transport; choose an interactive transport")))
    (setf (ende-job-profile job) profile
          (ende-job-agent-tool-rounds job) 0)
    (ende-job-emit-event job '(:type :tool-call-count :count 0))
    (ende-agent--send
     job (ende-operations--add-source-metadata job messages)
     configuration callbacks 0)))

(provide 'ende-agent)

;;; ende-agent.el ends here
