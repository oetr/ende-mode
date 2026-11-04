;;; ende-transcript.el --- Job-owned execution transcripts -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ende-protocol)

(cl-defstruct (ende-transcript (:constructor ende-transcript-create))
  task entries tail (next-id 0) (round 0) response terminal)

(cl-defstruct (ende-transcript-entry (:constructor ende-transcript--entry))
  id kind round content status data)

(defvar ende-transcript-change-functions nil
  "Presentation observers called with the changed transcript and entry.")

(defun ende-transcript-notify (transcript entry)
  "Notify presentation observers of ENTRY in TRANSCRIPT, isolating failures."
  (dolist (function ende-transcript-change-functions)
    (condition-case failure
        (funcall function transcript entry)
      ((error quit) (display-warning 'ende (format "Work display: %s" (error-message-string failure)))))))

(defun ende-transcript-new-entry (transcript kind &optional content data status)
  "Allocate an unattached KIND entry in TRANSCRIPT."
  (ende-transcript--entry :id (cl-incf (ende-transcript-next-id transcript))
                          :kind kind :round (ende-transcript-round transcript)
                          :content content :data data :status status))

(defun ende-transcript-append (transcript kind &optional content data status)
  "Record a KIND entry in TRANSCRIPT and return it."
  (let* ((entry (ende-transcript-new-entry transcript kind content data status))
         (cell (list entry)))
    (if (ende-transcript-tail transcript)
        (setcdr (ende-transcript-tail transcript) cell)
      (setf (ende-transcript-entries transcript) cell))
    (setf (ende-transcript-tail transcript) cell)
    (ende-transcript-notify transcript entry)
    entry))

(defun ende-transcript-begin-response (transcript)
  "Start a provider response boundary in TRANSCRIPT."
  (when-let* ((entry (ende-transcript-response transcript)))
    (when (eq (ende-transcript-entry-status entry) 'streaming)
      (setf (ende-transcript-entry-status entry) 'interrupted)))
  (cl-incf (ende-transcript-round transcript))
  (setf (ende-transcript-response transcript) nil))

(defun ende-transcript--response (transcript)
  "Return TRANSCRIPT's current response, allocating it when necessary."
  (or (ende-transcript-response transcript)
      (setf (ende-transcript-response transcript)
            (ende-transcript-append transcript 'response "" nil 'streaming))))

(defun ende-transcript-item-text (item)
  "Return the visible text in a Responses ITEM."
  (let ((content (ende-protocol-json-get "content" item)))
    (if (stringp content) content
      (mapconcat
       (lambda (part) (ende-protocol-json-get "text" part))
       (cl-remove-if-not
        (lambda (part)
          (and (member (ende-protocol-json-get "type" part) '("text" "output_text" "refusal"))
               (stringp (ende-protocol-json-get "text" part))))
        (append content nil)) ""))))

(defun ende-transcript-protocol-events (protocol-items)
  "Return semantic events in the original PROTOCOL-ITEMS order."
  (when (memq (car-safe protocol-items) '(openai-responses anthropic-messages))
    (let ((provider (symbol-name (car protocol-items))))
      (mapcar
       (lambda (item)
         (let ((type (ende-protocol-json-get "type" item)))
           (cond
            ((and (equal provider "openai-responses") (equal type "message"))
             (list :type 'text :text (ende-transcript-item-text item) :item item))
            ((and (equal provider "anthropic-messages") (equal type "text"))
             (list :type 'text :text (ende-protocol-json-get "text" item) :item item))
            ((or (and (equal provider "openai-responses") (equal type "function_call"))
                 (and (equal provider "anthropic-messages") (equal type "tool_use")))
             (list :type 'tool-call :id (ende-protocol-json-get
                                       (if (equal type "function_call") "call_id" "id") item)
                   :item item))
            (t (list :type 'opaque :provider provider
                     :kind (concat "opaque-" (or type "unknown")) :item item)))))
       (cdr protocol-items)))))

(defun ende-transcript-items (transcript result calls)
  "Allocate ordered items for RESULT and prepared CALLS in TRANSCRIPT."
  (let ((events (ende-transcript-protocol-events (plist-get result :protocol-items)))
        seen items)
    (dolist (event events)
      (let* ((kind (plist-get event :type))
             (call (and (eq kind 'tool-call)
                        (cl-find (plist-get event :id) calls
                                 :key (lambda (call) (plist-get call :id)) :test #'equal))))
        (when (or (not (eq kind 'tool-call)) call)
          (when call (push call seen))
          (push (ende-transcript-new-entry
                 transcript kind (plist-get event :text)
                 (if call (list :call call :item (plist-get event :item)) event) 'complete)
                items))))
    (unless events
      (push (ende-transcript-new-entry transcript 'text (or (plist-get result :content) "") nil 'complete) items))
    (dolist (call calls)
      (unless (memq call seen)
        (push (ende-transcript-new-entry transcript 'tool-call nil (list :call call) 'complete) items)))
    (nreverse items)))

(defun ende-transcript-accept (transcript result calls &optional data final)
  "Accept a completed RESULT and CALLS, retaining prepared semantic DATA.
DATA may include :items, :prepared, :metadata, and :synthetic.  FINAL marks
one final reply; accepting the current response again does not duplicate it."
  (let* ((synthetic (plist-get data :synthetic))
         (entry (if synthetic
                    (ende-transcript-append transcript 'response "" nil 'streaming)
                  (ende-transcript--response transcript))))
    (when (eq (ende-transcript-entry-status entry) 'streaming)
      (setf (ende-transcript-entry-content entry) (or (plist-get result :content) "")
            (ende-transcript-entry-data entry)
            (append (list :result result :calls calls :final final
                          :items (or (plist-get data :items)
                                     (ende-transcript-items transcript result calls)))
                    data)
            (ende-transcript-entry-status entry) 'complete)
      (ende-transcript-notify transcript entry))
    entry))

(defun ende-transcript-tool-result (transcript call content)
  "Record exact tool CONTENT associated with prepared CALL.
Only an explicit outer JSON boolean `ok' determines success or failure."
  (let ((ok (condition-case nil
                (ende-protocol-json-get "ok" (ende-protocol-json-read-string content))
              (error nil))))
    (ende-transcript-append transcript 'tool-result content (list :call call)
                            (cond ((eq ok t) 'succeeded)
                                  ((eq ok :false) 'failed)
                                  (t 'returned)))))

(defun ende-transcript-event (transcript event)
  "Record public streaming and activity EVENT in TRANSCRIPT."
  (pcase (plist-get event :type)
    (:delta
     (when (memq (plist-get event :channel) '(:content :thinking))
       (let* ((entry (ende-transcript--response transcript))
              (text (plist-get event :text)))
         (when (and (stringp text) (eq (ende-transcript-entry-status entry) 'streaming))
           (if (eq (plist-get event :channel) :thinking)
               (setf (ende-transcript-entry-data entry)
                     (plist-put (ende-transcript-entry-data entry) :thinking
                                (concat (plist-get (ende-transcript-entry-data entry) :thinking) text)))
             (setf (ende-transcript-entry-content entry)
                   (concat (ende-transcript-entry-content entry) text)))
           (ende-transcript-notify transcript entry)))))
    (:reasoning-summary
     (let* ((round (ende-transcript-round transcript))
            (item (plist-get event :item))
            (entry (cl-find-if
                    (lambda (entry)
                      (and (eq (ende-transcript-entry-kind entry) 'summary)
                           (= (ende-transcript-entry-round entry) round)
                           (equal (plist-get (ende-transcript-entry-data entry) :item) item)))
                    (ende-transcript-entries transcript))))
       (unless entry
         (setq entry (ende-transcript-append transcript 'summary "" (list :item item))))
       (setf (ende-transcript-entry-content entry)
             (if (plist-get event :replace) (plist-get event :text)
               (concat (ende-transcript-entry-content entry) (plist-get event :text))))
       (ende-transcript-notify transcript entry)))
    (:agent-activity
     (ende-transcript-append transcript 'progress (plist-get event :status)
                             (list :title (plist-get event :title))))))

(defun ende-transcript-finish (transcript state &optional error)
  "Retain terminal STATE and ERROR in TRANSCRIPT exactly once."
  (let ((entry (or (ende-transcript-terminal transcript)
                   (setf (ende-transcript-terminal transcript)
                         (ende-transcript-append transcript 'status)))))
    (when-let* ((response (ende-transcript-response transcript)))
      (when (eq (ende-transcript-entry-status response) 'streaming)
        (setf (ende-transcript-entry-status response) 'interrupted)))
    (setf (ende-transcript-entry-status entry) state
          (ende-transcript-entry-data entry) (list :error error))
    (ende-transcript-notify transcript entry)))

(provide 'ende-transcript)
;;; ende-transcript.el ends here
