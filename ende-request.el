;;; ende-request.el --- Asynchronous multimodal request preparation -*- lexical-binding: t; -*-

(require 'ende-content)
(require 'ende-curl)
(require 'ende-debug)
(require 'ende-worker)

(defun ende-request--write (library builder messages profile file debug)
  "Build and write a request to FILE; return payload-free diagnostics."
  (require library)
  (let* ((body (funcall builder messages profile))
         (data (encode-coding-string (json-encode body) 'utf-8))
         (coding-system-for-write 'no-conversion))
    (write-region data nil file nil 'silent)
    (list :body (list (assq :tools body))
          :debug-record (when debug
                          (apply #'ende-debug-record-request data body messages profile debug)))))

(defun ende-request-prepare (lifecycle library builder messages profile debug callback)
  "Prepare MESSAGES for PROFILE and call CALLBACK with data, body and record.
Image-bearing requests use a worker and a file owned by LIFECYCLE.
DEBUG contains transport, endpoint and optional session ID for recording."
  (let* ((debug (when (and ende-debugging-enabled ende-debug--session-directory) debug))
         (images (cl-some (lambda (message)
                            (let ((content (ende-content-body message)))
                              (and (listp content)
                                   (cl-some (lambda (part) (eq (plist-get part :type) :image)) content))))
                          messages)))
    (if (not images)
        (let* ((body (funcall builder messages profile))
               (data (encode-coding-string (json-encode body) 'utf-8))
               (record (when debug
                         (apply #'ende-debug-record-request data body messages profile debug))))
          (funcall callback data body record))
      (let* ((file (make-temp-file (expand-file-name "ende-request-" (ende-curl-local-directory))))
             (bindings (mapcar (lambda (variable) (cons variable (symbol-value variable)))
                               '(ende-max-output-tokens ende-debugging-enabled
                                 ende-debug--session-directory ende-debug--sequence)))
             (failed (ende-transport-session-callback-error-handler lifecycle)))
        (when debug (cl-incf ende-debug--sequence))
        (ende-transport-session-add-canceller
         lifecycle (lambda () (ende-ignore-errors #'delete-file file)))
        (ende-transport-session-add-canceller
         lifecycle
         (ende-worker-run
          'ende-request #'ende-request--write
          (list library builder messages profile file debug) bindings
          (lambda (result error)
            (unless (ende-transport-session-terminal lifecycle)
              (condition-case failure
                  (if error
                      (funcall failed (ende-error-create :kind 'startup :message error))
                    (funcall callback (list :file file) (plist-get result :body)
                             (plist-get result :debug-record)))
                ((error quit)
                 (funcall failed (ende-error-create :kind 'startup
                                                   :message (error-message-string failure)))))))))))))

(defun ende-request-send (process data)
  "Send prepared DATA to PROCESS, unless curl reads it from a file."
  (unless (listp data)
    (process-send-string process data))
  (process-send-eof process))

(provide 'ende-request)
