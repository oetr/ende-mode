;;; ende-protocol.el --- Streaming protocol parsers for ende -*- lexical-binding: t; -*-

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

;; JSON, SSE, and reasoning-tag parsing for ende transports.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'ende-core)

(defun ende-protocol-output-items-error (count)
  "Return a resource error if Responses output COUNT exceeds its limit."
  (when (and ende-responses-max-output-items
             (> count ende-responses-max-output-items))
    (ende-error-create
     :kind 'resource
     :message "Responses output exceeds ende-responses-max-output-items")))

;;;; Reasoning parser

(cl-defstruct (ende-protocol--reasoning
               (:constructor ende-protocol--make-reasoning))
  "Incremental state for XML-like reasoning tags."
  mode pending)

(defun ende-protocol-make-reasoning ()
  "Create an opaque incremental reasoning parser."
  (ende-protocol--make-reasoning :mode 'content :pending ""))

(defun ende-protocol--emit-delta (callbacks type text)
  "Invoke :on-delta in CALLBACKS with TYPE and nonempty TEXT."
  (let ((cb (plist-get callbacks :on-delta)))
    (when (and cb (stringp text) (> (length text) 0))
      (funcall cb type text))))

(defun ende-protocol--emit-status (callbacks status)
  "Invoke :on-status in CALLBACKS with STATUS."
  (let ((cb (plist-get callbacks :on-status)))
    (when cb
      (funcall cb status))))

(defun ende-protocol--longest-tag-suffix (text tag)
  "Return the longest suffix of TEXT that prefixes TAG.
The complete TAG is intentionally excluded because complete tags are handled
before this helper is called."
  (let ((limit (min (1- (length tag)) (length text)))
        (found ""))
    (cl-loop for length from limit downto 1
             for prefix = (substring tag 0 length)
             when (string-suffix-p prefix text)
             do (setq found prefix) and return found)
    found))

(defun ende-protocol-reasoning-feed (parser content callbacks)
  "Feed CONTENT into incremental PARSER and return visible answer text.
Thinking and content deltas are emitted through CALLBACKS."
  (let* ((input (concat (ende-protocol--reasoning-pending parser)
                        (or content "")))
         (position 0)
         (size (length input))
         (visible nil)
         (open-tag "<think>")
         (close-tag "</think>"))
    (setf (ende-protocol--reasoning-pending parser) "")
    (while (< position size)
      (if (eq (ende-protocol--reasoning-mode parser) 'thinking)
          (let ((close (string-match (regexp-quote close-tag) input position)))
            (if close
                (let ((thinking (substring input position close)))
                  (ende-protocol--emit-delta callbacks :thinking thinking)
                  (setq position (+ close (length close-tag)))
                  (setf (ende-protocol--reasoning-mode parser) 'content)
                  (ende-protocol--emit-status callbacks "Generating"))
              (let* ((tail (substring input position))
                     (suffix (ende-protocol--longest-tag-suffix
                              tail close-tag))
                     (body-end (- size (length suffix))))
                (when (> body-end position)
                  (ende-protocol--emit-delta
                   callbacks :thinking (substring input position body-end)))
                (setf (ende-protocol--reasoning-pending parser) suffix)
                (setq position size))))
        (let ((open (string-match (regexp-quote open-tag) input position)))
          (if open
              (let ((answer (substring input position open)))
                (when (> (length answer) 0)
                  (push answer visible)
                  (ende-protocol--emit-delta callbacks :content answer)
                  (ende-protocol--emit-status callbacks "Generating"))
                (setq position (+ open (length open-tag)))
                (setf (ende-protocol--reasoning-mode parser) 'thinking)
                (ende-protocol--emit-status callbacks "Thinking"))
            (let* ((tail (substring input position))
                   (suffix (ende-protocol--longest-tag-suffix tail open-tag))
                   (body-end (- size (length suffix))))
              (when (> body-end position)
                (let ((answer (substring input position body-end)))
                  (push answer visible)
                  (ende-protocol--emit-delta callbacks :content answer)
                  (ende-protocol--emit-status callbacks "Generating")))
              (setf (ende-protocol--reasoning-pending parser) suffix)
              (setq position size))))))
    (apply #'concat (nreverse visible))))

(defun ende-protocol-reasoning-finish (parser callbacks)
  "Flush PARSER and return a structured error for unclosed thinking."
  (let ((pending (ende-protocol--reasoning-pending parser)))
    (when (> (length pending) 0)
      (if (eq (ende-protocol--reasoning-mode parser) 'thinking)
          (ende-protocol--emit-delta callbacks :thinking pending)
        (ende-protocol--emit-delta callbacks :content pending))
      (setf (ende-protocol--reasoning-pending parser) ""))
    (when (eq (ende-protocol--reasoning-mode parser) 'thinking)
      (ende-error-create
       :kind 'protocol
       :message "stream ended inside a thinking section"))))

;;;; Incremental SSE parser

(cl-defstruct (ende-protocol--sse
               (:constructor ende-protocol--make-sse))
  "Incremental SSE line/event state."
  buffer-parts buffer-bytes data-lines data-bytes done)

(defun ende-protocol-make-sse ()
  "Create an opaque incremental SSE parser."
  (ende-protocol--make-sse
   :buffer-parts nil :buffer-bytes 0 :data-lines nil :data-bytes 0 :done nil))

(defun ende-protocol--sse-pending-buffer (parser)
  "Return the unparsed SSE text accumulated in PARSER."
  (let ((parts (ende-protocol--sse-buffer-parts parser)))
    (if (cdr parts)
        (mapconcat #'identity (reverse parts) "")
      (or (car parts) ""))))

(defun ende-protocol--sse-event (parser)
  "Turn the accumulated SSE data lines in PARSER into one event."
  (let ((payload (mapconcat #'identity
                            (nreverse
                             (ende-protocol--sse-data-lines parser))
                            "\n")))
    (setf (ende-protocol--sse-data-lines parser) nil)
    (setf (ende-protocol--sse-data-bytes parser) 0)
    (when (> (length payload) 0)
      (if (string= (string-trim payload) "[DONE]")
          (progn
            (setf (ende-protocol--sse-done parser) t)
            '(:type :done))
        (let ((json (condition-case err
                        (ende-protocol-json-read-string payload)
                      ((error quit)
                       (signal 'error
                               (list (if (string-match-p
                                          "data:image/\\|b64_json\\|image_generation_call" payload)
                                         "Malformed SSE JSON containing image data (payload redacted)"
                                       (format "malformed SSE JSON: %s"
                                               (error-message-string err)))))))))
          (list :type :json :value json))))))

(defun ende-protocol--sse-handle-line (parser line)
  "Consume one normalized SSE LINE for PARSER and return an event if complete."
  (when (and ende-max-sse-event-bytes
             (> (string-bytes line) ende-max-sse-event-bytes))
    (error "SSE line exceeds ende-max-sse-event-bytes"))
  (cond
   ((ende-protocol--sse-done parser) nil)
   ((string-empty-p line)
    (when (ende-protocol--sse-data-lines parser)
      (ende-protocol--sse-event parser)))
   ((string-prefix-p ":" line) nil)
   ((string-prefix-p "data:" line)
    (let ((value (substring line 5)))
      (when (and (> (length value) 0) (= (aref value 0) ?\s))
        (setq value (substring value 1)))
      (let ((bytes (+ (or (ende-protocol--sse-data-bytes parser) 0)
                      (if (ende-protocol--sse-data-lines parser) 1 0)
                      (string-bytes value))))
        (when (and ende-max-sse-event-bytes
                   (> bytes ende-max-sse-event-bytes))
          (error "SSE event exceeds ende-max-sse-event-bytes"))
        (setf (ende-protocol--sse-data-bytes parser) bytes))
      (push value (ende-protocol--sse-data-lines parser)))
    nil)
   (t nil)))

(defun ende-protocol--sse-handle-raw-line (parser line)
  "Normalize raw SSE LINE and pass it to PARSER."
  (when (string-suffix-p "\r" line)
    (setq line (substring line 0 -1)))
  (ende-protocol--sse-handle-line parser line))

(defun ende-protocol-sse-read (parser chunk start end)
  "Read CHUNK from START up to END or the next event into PARSER.
Return (OFFSET . EVENT), where OFFSET is the next unread character and EVENT
is nil if incomplete.  Errors are signaled to the transport."
  (let ((position start) event)
    (while (and (< position end) (not event))
      (let* ((newline (cl-position ?\n chunk :start position :end end))
             (part (substring chunk position (or newline end))))
        (unless (string-empty-p part)
          (push part (ende-protocol--sse-buffer-parts parser))
          (cl-incf (ende-protocol--sse-buffer-bytes parser)
                   (string-bytes part)))
        (setq position (if newline (1+ newline) end))
        (if newline
            (let ((line (ende-protocol--sse-pending-buffer parser)))
              (setf (ende-protocol--sse-buffer-parts parser) nil
                    (ende-protocol--sse-buffer-bytes parser) 0)
              (setq event (ende-protocol--sse-handle-raw-line parser line)))
          (when (and ende-max-sse-event-bytes
                     (> (- (ende-protocol--sse-buffer-bytes parser)
                           (if (string-suffix-p "\r" part) 1 0))
                        ende-max-sse-event-bytes))
            (error "SSE line exceeds ende-max-sse-event-bytes")))))
    (cons position event)))

(defun ende-protocol-sse-feed (parser chunk)
  "Feed raw SSE CHUNK into PARSER and return complete events.
Errors are signaled so the transport can turn them into protocol failures."
  (let ((position 0) events)
    (while (< position (length chunk))
      (let ((read (ende-protocol-sse-read parser chunk position (length chunk))))
        (setq position (car read))
        (when (cdr read) (push (cdr read) events))))
    (nreverse events)))

(defun ende-protocol-sse-flush (parser)
  "Flush and return any final complete event buffered by PARSER."
  (let ((events nil)
        (buffer (ende-protocol--sse-pending-buffer parser)))
    (setf (ende-protocol--sse-buffer-parts parser) nil
          (ende-protocol--sse-buffer-bytes parser) 0)
    (when (> (length buffer) 0)
      (let ((event (ende-protocol--sse-handle-raw-line parser buffer)))
        (when event
          (push event events))))
    (when (ende-protocol--sse-data-lines parser)
      (let ((event (ende-protocol--sse-event parser)))
        (when event
          (push event events))))
    (nreverse events)))

(defun ende-protocol-sse-finish (parser)
  "Flush a final complete SSE event and validate PARSER state."
  (let ((events (ende-protocol-sse-flush parser)))
    (unless (ende-protocol--sse-done parser)
      (signal 'error (list "SSE stream ended without [DONE]")))
    events))

;;;; JSON helpers

(defun ende-protocol-json-read-string (text)
  "Parse JSON TEXT without interning object keys as Emacs symbols."
  (json-parse-string text
                     :object-type 'hash-table
                     :array-type 'array
                     :null-object nil
                     :false-object :false))

(defun ende-protocol-json-get (key object)
  "Return string KEY from parsed JSON hash table OBJECT."
  (and (hash-table-p object)
       (gethash key object)))

(defun ende-protocol-json-equal-p (left right)
  "Compare parsed JSON LEFT and RIGHT, ignoring only object-key order.
Arrays retain their order; false and null remain distinct."
  (cond
   ((hash-table-p left)
    (and (hash-table-p right) (= (hash-table-count left) (hash-table-count right))
         (cl-loop for key being the hash-keys of left using (hash-values value)
                  always (and (ende-protocol-json-has-key-p key right)
                              (ende-protocol-json-equal-p value (gethash key right))))))
   ((and (vectorp left) (not (stringp left)))
    (and (vectorp right) (not (stringp right)) (= (length left) (length right))
         (cl-loop for a across left for b across right
                  always (ende-protocol-json-equal-p a b))))
   ((numberp left) (and (numberp right) (= left right)))
   (t (equal left right))))

(defun ende-protocol-json-for-request (value)
  "Copy parsed JSON VALUE using the request encoder's boolean representation."
  (cond
   ((eq value :false) json-false)
   ((hash-table-p value)
    (let ((copy (make-hash-table :test #'equal)))
      (maphash (lambda (key item)
                 (puthash (substring-no-properties key)
                          (ende-protocol-json-for-request item) copy))
               value)
      copy))
   ((vectorp value) (vconcat (mapcar #'ende-protocol-json-for-request value)))
   ((stringp value) (substring-no-properties value))
   (t value)))

(defun ende-protocol-json-has-key-p (key object)
  "Return non-nil when parsed JSON hash table OBJECT contains KEY."
  (and (hash-table-p object)
       (let ((missing (make-symbol "missing")))
         (not (eq (gethash key object missing) missing)))))

(provide 'ende-protocol)

;;; ende-protocol.el ends here
