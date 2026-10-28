;;; ende-worker.el --- Cancellable batch Emacs workers -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(defconst ende-worker--directory
  (file-name-directory (or load-file-name buffer-file-name)))

(defun ende-worker--print (value)
  "Serialize VALUE as one complete line of Lisp data."
  (let ((print-length nil)
        (print-level nil)
        (print-escape-newlines t)
        (print-escape-control-characters t))
    (concat (prin1-to-string value) "\n")))

(defun ende-worker--main ()
  "Execute one request from standard input in a batch Emacs."
  (let ((result
         (condition-case caught
             (pcase-let ((`(,library ,function ,arguments ,bindings)
                          (read (read-string ""))))
               (require library)
               (list :value
                     (cl-progv (mapcar #'car bindings) (mapcar #'cdr bindings)
                       (apply function arguments))))
           ((error quit) (list :error (error-message-string caught))))))
    (princ (ende-worker--print result))))

(defun ende-worker-run
    (library function arguments bindings callback &optional register-cancellation)
  "Call FUNCTION with ARGUMENTS and BINDINGS in a clean Emacs loading LIBRARY.
CALLBACK receives a value and an error.  REGISTER-CANCELLATION receives the
cancellation function on startup and nil when the worker finishes."
  (let ((output (generate-new-buffer " *ende-worker*"))
        (errors (generate-new-buffer " *ende-worker-errors*"))
        (process nil)
        (finished nil)
        (registered nil))
    (cl-labels
        ((finish (value error)
           (unless finished
             (setq finished t)
             (when (and process (process-live-p process))
               (delete-process process))
             (when-let* ((stderr (get-buffer-process errors)))
               (delete-process stderr))
             (kill-buffer output)
             (kill-buffer errors)
             (when registered
               (setq registered nil)
               (ignore-errors (funcall register-cancellation nil)))
             (funcall callback value error)))
         (cancel ()
           (finish nil "worker cancelled"))
         (sentinel (proc _event)
           (when (and (not finished) (memq (process-status proc) '(exit signal)))
             (let ((reply
                    (condition-case caught
                        (if (zerop (process-exit-status proc))
                            (with-current-buffer output
                              (goto-char (point-min))
                              (let ((value (read (current-buffer))))
                                (unless (or (plist-member value :value)
                                            (plist-member value :error))
                                  (error "invalid worker result"))
                                value))
                          (error "worker failed: %s"
                                 (with-current-buffer errors
                                   (string-trim (buffer-string)))))
                      (error (list :error (error-message-string caught))))))
               (finish (plist-get reply :value) (plist-get reply :error))))))
      (condition-case caught
          (let ((default-directory ende-worker--directory))
            (setq process
                  (make-process
                   :name "ende-worker" :buffer output :stderr errors
                   :connection-type 'pipe :coding 'utf-8-unix :noquery t
                   :command (list (expand-file-name invocation-name invocation-directory)
                                  "-Q" "--batch" "-L" ende-worker--directory
                                  "-l" (expand-file-name "ende-worker.el" ende-worker--directory)
                                  "-f" "ende-worker--main")
                   :sentinel #'sentinel))
            (process-send-string
             process (ende-worker--print (list library function arguments bindings)))
            (process-send-eof process)
            (when register-cancellation
              (setq registered t)
              (funcall register-cancellation #'cancel)))
        ((error quit) (finish nil (error-message-string caught))))
      #'cancel)))

(provide 'ende-worker)
;;; ende-worker.el ends here
