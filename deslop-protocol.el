;;; deslop-protocol.el --- Streaming protocol parsers for deslop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools

;;; Commentary:

;; JSON, SSE, and reasoning-tag parsing for deslop transports.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'deslop-core)

;;;; Reasoning parser

(cl-defstruct (deslop-protocol--reasoning
               (:constructor deslop-protocol--make-reasoning))
  "Incremental state for XML-like reasoning tags."
  mode pending)

(defun deslop-protocol--emit-delta (callbacks type text)
  "Invoke :on-delta in CALLBACKS with TYPE and nonempty TEXT."
  (let ((cb (plist-get callbacks :on-delta)))
    (when (and cb (stringp text) (> (length text) 0))
      (funcall cb type text))))

(defun deslop-protocol--emit-status (callbacks status)
  "Invoke :on-status in CALLBACKS with STATUS."
  (let ((cb (plist-get callbacks :on-status)))
    (when cb
      (funcall cb status))))

(defun deslop-protocol--longest-tag-suffix (text tag)
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

(defun deslop-protocol--reasoning-feed (parser content callbacks)
  "Feed CONTENT into incremental PARSER and return visible answer text.
Thinking and content deltas are emitted through CALLBACKS."
  (let* ((input (concat (deslop-protocol--reasoning-pending parser)
                        (or content "")))
         (position 0)
         (size (length input))
         (visible nil)
         (open-tag "<think>")
         (close-tag "</think>"))
    (setf (deslop-protocol--reasoning-pending parser) "")
    (while (< position size)
      (if (eq (deslop-protocol--reasoning-mode parser) 'thinking)
          (let ((close (string-match (regexp-quote close-tag) input position)))
            (if close
                (let ((thinking (substring input position close)))
                  (deslop-protocol--emit-delta callbacks :thinking thinking)
                  (setq position (+ close (length close-tag)))
                  (setf (deslop-protocol--reasoning-mode parser) 'content)
                  (deslop-protocol--emit-status callbacks "Generating"))
              (let* ((tail (substring input position))
                     (suffix (deslop-protocol--longest-tag-suffix
                              tail close-tag))
                     (body-end (- size (length suffix))))
                (when (> body-end position)
                  (deslop-protocol--emit-delta
                   callbacks :thinking (substring input position body-end)))
                (setf (deslop-protocol--reasoning-pending parser) suffix)
                (setq position size))))
        (let ((open (string-match (regexp-quote open-tag) input position)))
          (if open
              (let ((answer (substring input position open)))
                (when (> (length answer) 0)
                  (push answer visible)
                  (deslop-protocol--emit-delta callbacks :content answer)
                  (deslop-protocol--emit-status callbacks "Generating"))
                (setq position (+ open (length open-tag)))
                (setf (deslop-protocol--reasoning-mode parser) 'thinking)
                (deslop-protocol--emit-status callbacks "Thinking"))
            (let* ((tail (substring input position))
                   (suffix (deslop-protocol--longest-tag-suffix tail open-tag))
                   (body-end (- size (length suffix))))
              (when (> body-end position)
                (let ((answer (substring input position body-end)))
                  (push answer visible)
                  (deslop-protocol--emit-delta callbacks :content answer)
                  (deslop-protocol--emit-status callbacks "Generating")))
              (setf (deslop-protocol--reasoning-pending parser) suffix)
              (setq position size))))))
    (apply #'concat (nreverse visible))))

(defun deslop-protocol--reasoning-finish (parser callbacks)
  "Flush PARSER and return a structured error for unclosed thinking."
  (let ((pending (deslop-protocol--reasoning-pending parser)))
    (when (> (length pending) 0)
      (if (eq (deslop-protocol--reasoning-mode parser) 'thinking)
          (deslop-protocol--emit-delta callbacks :thinking pending)
        (deslop-protocol--emit-delta callbacks :content pending))
      (setf (deslop-protocol--reasoning-pending parser) ""))
    (when (eq (deslop-protocol--reasoning-mode parser) 'thinking)
      (deslop-error-create
       :kind 'protocol
       :message "stream ended inside a thinking section"))))

;;;; Incremental SSE parser

(cl-defstruct (deslop-protocol--sse
               (:constructor deslop-protocol--make-sse))
  "Incremental SSE line/event state."
  buffer-parts buffer-bytes data-lines data-bytes done)

(defun deslop-protocol--sse-pending-buffer (parser)
  "Return the unparsed SSE text accumulated in PARSER."
  (let ((parts (deslop-protocol--sse-buffer-parts parser)))
    (if parts (apply #'concat (nreverse parts)) "")))

(defun deslop-protocol--sse-event (parser)
  "Turn the accumulated SSE data lines in PARSER into one event."
  (let ((payload (mapconcat #'identity
                            (nreverse
                             (deslop-protocol--sse-data-lines parser))
                            "\n")))
    (setf (deslop-protocol--sse-data-lines parser) nil)
    (setf (deslop-protocol--sse-data-bytes parser) 0)
    (when (> (length payload) 0)
      (if (string= (string-trim payload) "[DONE]")
          (progn
            (setf (deslop-protocol--sse-done parser) t)
            '(:type :done))
        (let ((json (condition-case err
                         (deslop-protocol-json-read-string payload)
                      ((error quit)
                       (signal 'error
                               (list (format "malformed SSE JSON: %s"
                                             (error-message-string err))))))))
          (list :type :json :value json))))))

(defun deslop-protocol--sse-handle-line (parser line)
  "Consume one normalized SSE LINE for PARSER and return an event if complete."
  (when (and deslop-max-sse-event-bytes
             (> (string-bytes line) deslop-max-sse-event-bytes))
    (error "SSE line exceeds deslop-max-sse-event-bytes"))
  (cond
   ((deslop-protocol--sse-done parser) nil)
   ((string-empty-p line)
    (when (deslop-protocol--sse-data-lines parser)
      (deslop-protocol--sse-event parser)))
   ((string-prefix-p ":" line) nil)
   ((string-prefix-p "data:" line)
    (let ((value (substring line 5)))
      (when (and (> (length value) 0) (= (aref value 0) ?\s))
        (setq value (substring value 1)))
      (let ((bytes (+ (or (deslop-protocol--sse-data-bytes parser) 0)
                      (if (deslop-protocol--sse-data-lines parser) 1 0)
                      (string-bytes value))))
         (when (and deslop-max-sse-event-bytes
                    (> bytes deslop-max-sse-event-bytes))
          (error "SSE event exceeds deslop-max-sse-event-bytes"))
        (setf (deslop-protocol--sse-data-bytes parser) bytes))
      (push value (deslop-protocol--sse-data-lines parser)))
    nil)
   (t nil)))

(defun deslop-protocol--sse-handle-raw-line (parser line)
  "Normalize raw SSE LINE and pass it to PARSER."
  (when (string-suffix-p "\r" line)
    (setq line (substring line 0 -1)))
  (deslop-protocol--sse-handle-line parser line))

(defun deslop-protocol--sse-feed (parser chunk)
  "Feed raw SSE CHUNK into PARSER and return complete events.
Errors are signaled so the transport can turn them into protocol failures."
  (let ((parts (deslop-protocol--sse-buffer-parts parser))
        (events nil))
    (when (and (stringp chunk) (> (length chunk) 0))
      (push chunk parts))
    (setf (deslop-protocol--sse-buffer-parts parser) parts
          (deslop-protocol--sse-buffer-bytes parser)
          (+ (or (deslop-protocol--sse-buffer-bytes parser) 0)
             (string-bytes (or chunk ""))))
    (if (not (string-match-p "\n" (or chunk "")))
           (when (and deslop-max-sse-event-bytes
                      (> (deslop-protocol--sse-buffer-bytes parser)
                         deslop-max-sse-event-bytes))
          (error "SSE line exceeds deslop-max-sse-event-bytes"))
      (let* ((input (apply #'concat (nreverse parts)))
             (start 0)
             position line)
        (while (string-match "\n" input start)
          (setq position (match-beginning 0))
          (setq line (substring input start position))
          (setq start (1+ position))
          (let ((event (deslop-protocol--sse-handle-raw-line parser line)))
            (when event
              (push event events))))
        (let ((remainder (substring input start)))
          (setf (deslop-protocol--sse-buffer-parts parser)
                (and (> (length remainder) 0) (list remainder))
                (deslop-protocol--sse-buffer-bytes parser)
                (string-bytes remainder))
          (when (and deslop-max-sse-event-bytes
                     (> (deslop-protocol--sse-buffer-bytes parser)
                        deslop-max-sse-event-bytes))
            (error "SSE line exceeds deslop-max-sse-event-bytes"))))
      (nreverse events))))

(defun deslop-protocol--sse-finish (parser)
  "Flush a final complete SSE event and validate PARSER state."
  (let ((events nil)
        (buffer (deslop-protocol--sse-pending-buffer parser)))
    (when (> (length buffer) 0)
      (setf (deslop-protocol--sse-buffer-parts parser) (list buffer)
            (deslop-protocol--sse-buffer-bytes parser) (string-bytes buffer))
      (let ((event (deslop-protocol--sse-handle-raw-line parser buffer)))
        (when event
          (push event events))))
    (when (deslop-protocol--sse-data-lines parser)
      (let ((event (deslop-protocol--sse-event parser)))
        (when event
          (push event events))))
    (unless (deslop-protocol--sse-done parser)
      (signal 'error (list "SSE stream ended without [DONE]")))
    (nreverse events)))

;;;; JSON helpers

(defun deslop-protocol-json-read-string (text)
  "Parse JSON TEXT without interning object keys as Emacs symbols."
  (json-parse-string text
                     :object-type 'hash-table
                     :array-type 'array
                     :null-object nil
                     :false-object :false))

(defun deslop-protocol-json-get (key object)
  "Return string KEY from parsed JSON hash table OBJECT."
  (and (hash-table-p object)
       (gethash key object)))

(defun deslop-protocol-json-has-key-p (key object)
  "Return non-nil when parsed JSON hash table OBJECT contains KEY."
  (and (hash-table-p object)
       (hash-table-contains-p key object)))

;; Keep the old names for packages that loaded the previous internal helpers.
(defalias 'deslop-protocol--json-read-string #'deslop-protocol-json-read-string)
(defalias 'deslop-protocol--json-get #'deslop-protocol-json-get)
(defalias 'deslop-protocol--json-has-key-p #'deslop-protocol-json-has-key-p)

(provide 'deslop-protocol)

;;; deslop-protocol.el ends here
