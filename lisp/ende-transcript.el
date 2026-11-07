;;; ende-transcript.el --- Job-owned execution transcripts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter Samarin
;;
;; Author: Peter Samarin <peter.samarin@gmail.com>
;; Maintainer: Peter Samarin <peter.samarin@gmail.com>
;; Keywords: convenience, tools
;; URL: https://github.com/oetr/ende-mode
;; SPDX-License-Identifier: AGPL-3.0-only

;; This file is part of Ende.

;; Ende is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, version 3 of the License.
;;
;; Ende is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with Ende.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Job-owned execution transcripts for Ende.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ende-core)
(require 'ende-protocol)

(cl-defstruct (ende-transcript (:constructor ende-transcript-create))
  task entries tail (next-id 0) (round 0) response terminal metadata usage
  (sources (make-hash-table :test #'equal)))

(cl-defstruct (ende-transcript-entry (:constructor ende-transcript--entry))
  id kind round response-id content status data)

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
  (ende-transcript--append-entry
   transcript (ende-transcript-new-entry transcript kind content data status)))

(defun ende-transcript--append-entry (transcript entry)
  "Attach ENTRY to TRANSCRIPT's observation order."
  (let ((cell (list entry)))
    (if (ende-transcript-tail transcript)
        (setcdr (ende-transcript-tail transcript) cell)
      (setf (ende-transcript-entries transcript) cell))
    (setf (ende-transcript-tail transcript) cell)
    (ende-transcript-notify transcript entry)
    entry))

(defun ende-transcript--interrupt-response (transcript)
  "Retain unfinished entries in TRANSCRIPT as interrupted."
  (when-let* ((response (ende-transcript-response transcript)))
    (when (eq (ende-transcript-entry-status response) 'streaming)
      (setf (ende-transcript-entry-status response) 'interrupted)
      (maphash (lambda (_key entry)
                 (when (eq (ende-transcript-entry-status entry) 'streaming)
                   (setf (ende-transcript-entry-status entry) 'interrupted)
                   (ende-transcript-notify transcript entry)))
               (ende-transcript-sources transcript))
      (ende-transcript-notify transcript response))))

(defun ende-transcript-begin-response (transcript)
  "Start a provider response boundary in TRANSCRIPT."
  (ende-transcript--interrupt-response transcript)
  (cl-incf (ende-transcript-round transcript))
  (setf (ende-transcript-response transcript) nil
        (ende-transcript-metadata transcript) nil
        (ende-transcript-usage transcript) nil
        (ende-transcript-sources transcript) (make-hash-table :test #'equal)))

(defun ende-transcript--response (transcript)
  "Return TRANSCRIPT's current response, allocating it when necessary."
  (or (ende-transcript-response transcript)
      (setf (ende-transcript-response transcript)
            (ende-transcript-append
             transcript 'response ""
             (list :response-metadata (ende-transcript-metadata transcript)
                   :usage (ende-transcript-usage transcript)) 'streaming))))

(defun ende-transcript--snapshot-metadata (transcript)
  "Retain TRANSCRIPT's metadata on its current streaming response."
  (when-let* ((entry (ende-transcript-response transcript))
              ((eq (ende-transcript-entry-status entry) 'streaming)))
    (setf (ende-transcript-entry-data entry)
          (ende-plist-merge
           (ende-transcript-entry-data entry)
           (list :response-metadata (ende-transcript-metadata transcript)
                 :usage (ende-transcript-usage transcript))))
    (ende-transcript-notify transcript entry)))

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

(defun ende-transcript-protocol-events (protocol-items &optional item-keys)
  "Return semantic events in PROTOCOL-ITEMS order with parallel ITEM-KEYS."
  (when (memq (car-safe protocol-items) '(openai-responses anthropic-messages))
    (let ((provider (symbol-name (car protocol-items))))
      (cl-loop
       for item in (cdr protocol-items) for index from 0
       for key = (if item-keys (pop item-keys) index)
       for type = (ende-protocol-json-get "type" item)
       collect
       (append
        (list :key key :provider provider :item item)
        (cond
         ((and (equal provider "openai-responses") (equal type "message"))
          (list :type 'text :text (ende-transcript-item-text item)))
         ((and (equal provider "anthropic-messages") (equal type "text"))
          (list :type 'text :text (ende-protocol-json-get "text" item)))
         ((or (and (equal provider "openai-responses") (equal type "function_call"))
              (and (equal provider "anthropic-messages") (equal type "tool_use")))
          (list :type 'tool-call :id (ende-protocol-json-get
                                    (if (equal type "function_call") "call_id" "id") item)))
         (t (list :type 'opaque :kind (concat "opaque-" (or type "unknown"))))))))))

(defun ende-transcript--upsert (transcript sources response event &optional append status)
  "Reconcile semantic EVENT in SOURCES, appending only its first observation.
RESPONSE supplies attribution, not ownership.  APPEND appends text instead of
replacing it; STATUS defaults to streaming."
  (let* ((kind (plist-get event :type))
         (key (cons kind (plist-get event :key)))
         (entry (gethash key sources))
         (new (not entry))
         (text (plist-get event :text)))
    (when (or entry (not (memq kind '(text thinking)))
              (and (stringp text) (not (string-empty-p text))))
      (unless entry
        (setq entry (ende-transcript-new-entry transcript kind)))
      (when (stringp text)
        (setf (ende-transcript-entry-content entry)
              (if append (concat (ende-transcript-entry-content entry) text) text)))
      (setf (ende-transcript-entry-data entry)
            (ende-plist-merge (ende-transcript-entry-data entry) event)
            (ende-transcript-entry-status entry) (or status 'streaming))
      (when response
        (setf (ende-transcript-entry-response-id entry) (ende-transcript-entry-id response)))
      (puthash key entry sources)
      (if new (ende-transcript--append-entry transcript entry)
        (ende-transcript-notify transcript entry))
      ;; A thinking block has both readable reasoning and an opaque replay facet.
      (when (eq kind 'opaque)
        (when-let* ((thinking (ende-protocol-json-get "thinking" (plist-get event :item))))
          (ende-transcript--upsert
           transcript sources response
           (list :type 'thinking :key (plist-get event :key) :text thinking) nil status)))
      entry)))

(defun ende-transcript--fallback-text (transcript sources response kind text status)
  "Reconcile aggregate TEXT of KIND when no individually keyed facets exist."
  (let ((key (if (eq kind 'thinking) :thinking :content)))
    (when (or (gethash (cons kind key) sources)
              (not (cl-loop for source being the hash-keys of sources
                            thereis (eq (car source) kind))))
      (ende-transcript--upsert transcript sources response
                              (list :type kind :key key :text text) nil status))))

(defun ende-transcript-items (transcript result calls &optional synthetic partial)
  "Reconcile RESULT and CALLS, returning canonical items in provider order.
SYNTHETIC uses fresh leaves without touching the current response's source map.
PARTIAL retains incomplete observations without preparing or executing calls."
  (let* ((response (unless synthetic (ende-transcript--response transcript)))
         (sources (if synthetic (make-hash-table :test #'equal)
                    (ende-transcript-sources transcript)))
         (status (if partial 'streaming 'complete))
         (events (ende-transcript-protocol-events
                  (plist-get result :protocol-items) (plist-get result :protocol-item-keys)))
         seen items)
    (dolist (event events)
      (let* ((kind (plist-get event :type))
             (call (and (eq kind 'tool-call)
                        (or (cl-find (plist-get event :key) calls
                                     :key (lambda (call) (plist-get call :key)) :test #'equal)
                            (cl-find (plist-get event :id) calls
                                     :key (lambda (call) (plist-get call :id)) :test #'equal)))))
        (when (or (not (eq kind 'tool-call)) call partial)
          (when call (push call seen))
          (when call (setq event (plist-put event :call call)))
          (when (and partial (eq kind 'text))
            (when-let* ((streamed (gethash (cons kind (plist-get event :key)) sources))
                        (text (ende-transcript-entry-content streamed))
                        (snapshot (or (plist-get event :text) ""))
                        ((string-prefix-p snapshot text)))
              (setq event (plist-put event :text text))))
          (when-let* ((entry (ende-transcript--upsert transcript sources response event nil status)))
            (push entry items)))))
    (ende-transcript--fallback-text
     transcript sources response 'thinking
     (or (plist-get result :thinking) (plist-get result :reasoning-content)) status)
    (unless events
      (when-let* ((entry (ende-transcript--fallback-text
                         transcript sources response 'text (plist-get result :content) status)))
        (push entry items)))
    (dolist (call calls)
      (unless (memq call seen)
        (push (ende-transcript--upsert
               transcript sources response
               (list :type 'tool-call :key (or (plist-get call :key) (plist-get call :id))
                     :call call) nil status) items)))
    (nreverse items)))

(defun ende-transcript-accept (transcript result calls &optional data final)
  "Accept a completed RESULT and CALLS, retaining prepared semantic DATA.
DATA may include :items, :prepared, :metadata, and :synthetic.  FINAL marks
one final reply; accepting the current response again does not duplicate it."
  (let* ((synthetic (plist-get data :synthetic))
         (entry (if synthetic
                    (ende-transcript-append transcript 'response "" '(:synthetic t) 'streaming)
                  (ende-transcript--response transcript))))
    (when (eq (ende-transcript-entry-status entry) 'streaming)
      (let ((items (if (plist-member data :items) (plist-get data :items)
                     (ende-transcript-items transcript result calls synthetic))))
        (setf (ende-transcript-entry-content entry) (or (plist-get result :content) "")
              (ende-transcript-entry-data entry)
              (ende-plist-merge
               (ende-transcript-entry-data entry)
               (append data
                       (list :result result :calls calls :final final
                             :response-metadata (ende-transcript-metadata transcript)
                             :usage (or (plist-get result :usage) (ende-transcript-usage transcript))
                             :duration-ms (or (plist-get result :duration-ms)
                                              (plist-get (ende-transcript-entry-data entry) :duration-ms))
                             :duration-kind (or (plist-get result :duration-kind)
                                                (plist-get (ende-transcript-entry-data entry) :duration-kind))
                             :items items
                             :thinking (if (plist-member data :thinking) (plist-get data :thinking)
                                         (or (plist-get result :thinking)
                                             (plist-get result :reasoning-content)
                                             (plist-get (ende-transcript-entry-data entry) :thinking))))))
              (ende-transcript-entry-status entry) 'complete)
        (dolist (item items)
          (setf (ende-transcript-entry-response-id item) (ende-transcript-entry-id entry)
                (ende-transcript-entry-status item) 'complete)
          (unless (memq item (ende-transcript-entries transcript))
            (ende-transcript--append-entry transcript item))
          (ende-transcript-notify transcript item))
        (unless synthetic
          (maphash (lambda (_key item)
                     (when (eq (ende-transcript-entry-status item) 'streaming)
                       (setf (ende-transcript-entry-status item) 'complete)
                       (ende-transcript-notify transcript item)))
                   (ende-transcript-sources transcript))))
      (ende-transcript-notify transcript entry))
    entry))

(defun ende-transcript-tool-call-entry (transcript call)
  "Return TRANSCRIPT's current entry for prepared CALL."
  (let ((entry (gethash (cons 'tool-call (or (plist-get call :key) (plist-get call :id)))
                        (ende-transcript-sources transcript))))
    (if (and entry (eq call (plist-get (ende-transcript-entry-data entry) :call)))
        entry
      (when-let* ((response (ende-transcript-response transcript)))
        (cl-find call (plist-get (ende-transcript-entry-data response) :items)
                 :key (lambda (item) (plist-get (ende-transcript-entry-data item) :call))
                 :test #'eq)))))

(defun ende-transcript-tool-result (transcript call content)
  "Record exact tool CONTENT associated with prepared CALL.
Only an explicit outer JSON boolean `ok' determines success or failure."
  (let* ((ok (condition-case nil
                 (ende-protocol-json-get "ok" (ende-protocol-json-read-string content))
               (error nil)))
         (response (cl-find-if
                    (lambda (entry)
                      (and entry (eq (ende-transcript-entry-kind entry) 'response)
                           (memq call (plist-get (ende-transcript-entry-data entry) :calls))))
                    (cons (ende-transcript-response transcript) (ende-transcript-entries transcript))))
         (call-entry (ende-transcript-tool-call-entry transcript call))
         (entry (ende-transcript-new-entry
                 transcript 'tool-result content
                 (list :call call :duration-ms
                       (and call-entry (plist-get (ende-transcript-entry-data call-entry) :duration-ms)))
                 (cond ((eq ok t) 'succeeded) ((eq ok :false) 'failed) (t 'returned)))))
    (when response
      (setf (ende-transcript-entry-response-id entry) (ende-transcript-entry-id response)))
    (ende-transcript--append-entry transcript entry)))

(defun ende-transcript-event (transcript event)
  "Record public streaming and activity EVENT in TRANSCRIPT."
  (pcase (plist-get event :type)
    (:duration
     (let ((entry (ende-transcript--response transcript)))
       (setf (ende-transcript-entry-data entry)
             (ende-plist-merge
              (ende-transcript-entry-data entry)
              (list :duration-ms (plist-get event :duration-ms)
                    :duration-kind (plist-get event :duration-kind))))
       (ende-transcript-notify transcript entry)))
    (:response-restarted
     (ende-transcript-begin-response transcript)
     (ende-transcript-append transcript 'progress (plist-get event :text)
                             '(:title "Reconnecting")))
    (:metadata
     (let ((metadata (plist-get event :metadata)))
       (unless (and (eq (plist-get (ende-transcript-metadata transcript) :source) :response)
                    (not (eq (plist-get metadata :source) :response)))
         (unless (equal metadata (ende-transcript-metadata transcript))
           (setf (ende-transcript-metadata transcript) metadata)
           (ende-transcript--snapshot-metadata transcript)))))
    (:usage
     (when-let* ((usage (plist-get event :usage))
                 ((not (equal usage (ende-transcript-usage transcript)))))
       (setf (ende-transcript-usage transcript) usage)
       (ende-transcript--snapshot-metadata transcript)))
    (:partial-result
     (let* ((entry (ende-transcript--response transcript))
            (result (plist-get event :result)))
       (when (eq (ende-transcript-entry-status entry) 'streaming)
         (ende-transcript-items transcript result (plist-get result :tool-calls) nil t)
         (setf (ende-transcript-entry-data entry)
               (plist-put (ende-transcript-entry-data entry) :partial-result result))
         (when (plist-member result :content)
           (setf (ende-transcript-entry-content entry) (plist-get result :content)))
         (when (plist-member result :thinking)
           (setf (ende-transcript-entry-data entry)
                 (plist-put (ende-transcript-entry-data entry) :thinking (plist-get result :thinking))))
         (ende-transcript-notify transcript entry))))
    (:delta
     (when (memq (plist-get event :channel) '(:content :thinking))
       (let* ((entry (ende-transcript--response transcript))
              (text (plist-get event :text)))
         (when (and (stringp text) (eq (ende-transcript-entry-status entry) 'streaming))
           (ende-transcript--upsert
            transcript (ende-transcript-sources transcript) entry
            (list :type (if (eq (plist-get event :channel) :thinking) 'thinking 'text)
                  :key (or (plist-get event :item-key) (plist-get event :channel)) :text text) t)
           (if (eq (plist-get event :channel) :thinking)
               (setf (ende-transcript-entry-data entry)
                     (plist-put (ende-transcript-entry-data entry) :thinking
                                (concat (plist-get (ende-transcript-entry-data entry) :thinking) text)))
             (setf (ende-transcript-entry-content entry)
                   (concat (ende-transcript-entry-content entry) text)))
           (ende-transcript-notify transcript entry)))))
    (:item
     (let ((response (ende-transcript--response transcript))
           (observation (plist-put (copy-sequence event) :type (plist-get event :kind))))
       (when (eq (ende-transcript-entry-status response) 'streaming)
         (when (eq (plist-get event :kind) 'opaque)
           (setq observation
                 (plist-put observation :kind
                            (concat "opaque-" (or (ende-protocol-json-get
                                                   "type" (plist-get event :item)) "unknown")))))
         (ende-transcript--upsert transcript (ende-transcript-sources transcript)
                                 response observation))))
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
         (setq entry (ende-transcript-append
                      transcript 'summary ""
                      (list :item item :item-key (plist-get event :item-key)))))
       (when (plist-member event :item-key)
         (setf (ende-transcript-entry-data entry)
               (plist-put (ende-transcript-entry-data entry) :item-key (plist-get event :item-key))))
       (setf (ende-transcript-entry-content entry)
             (if (plist-get event :replace) (plist-get event :text)
               (concat (ende-transcript-entry-content entry) (plist-get event :text))))
       (ende-transcript-notify transcript entry)))
    (:agent-activity
     (ende-transcript-append transcript 'progress (plist-get event :status)
                             (list :title (plist-get event :title))))))

(defun ende-transcript-finish (transcript state &optional error)
  "Retain terminal STATE and ERROR in TRANSCRIPT exactly once."
  (ende-transcript--interrupt-response transcript)
  (let ((entry (or (ende-transcript-terminal transcript)
                   (setf (ende-transcript-terminal transcript)
                         (ende-transcript-append transcript 'status)))))
    (setf (ende-transcript-entry-status entry) state
          (ende-transcript-entry-data entry) (list :error error))
    (ende-transcript-notify transcript entry)))

(provide 'ende-transcript)
;;; ende-transcript.el ends here
