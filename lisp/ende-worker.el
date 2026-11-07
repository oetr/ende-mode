;;; ende-worker.el --- Cancellable batch Emacs workers -*- lexical-binding: t; -*-

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

;; Cancellable batch Emacs workers for Ende.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst ende-worker--directory
  (file-name-directory (or load-file-name buffer-file-name)))

(defconst ende-worker--startup-timeout 10
  "Seconds allowed for a standby's readiness handshake.")

(defconst ende-worker--idle-timeout 60
  "Seconds a ready standby may wait for its only request.")

(cl-defstruct (ende-worker--worker (:constructor ende-worker--create))
  process output errors timer phase signature dependencies
  callback register-cancellation registered)

(defvar ende-worker--standby nil
  "The sole starting or ready project worker, never an assigned worker.")

(defvar ende-worker--generation 0
  "Shutdown generation preventing old requests from replenishing a standby.")

(defun ende-worker--print (value)
  "Serialize VALUE as one complete line of Lisp data."
  (let ((print-length nil)
        (print-level nil)
        (print-escape-newlines t)
        (print-escape-control-characters t))
    (concat (prin1-to-string value) "\n")))

(defun ende-worker--main (&optional preload)
  "Execute one request from standard input after optionally loading PRELOAD."
  (let ((result
         (condition-case caught
             (let ((load-prefer-newer t))
               (when preload
                 (require preload)
                 (princ (ende-worker--print
                         (list :ready (ende-worker--dependency-signature)))))
               (pcase-let ((`(,library ,function ,arguments ,bindings)
                            (read (read-string ""))))
                 (require library)
                 (list :value
                       (cl-progv (mapcar #'car bindings) (mapcar #'cdr bindings)
                         (apply function arguments)))))
           ((error quit) (list :error (error-message-string caught))))))
    (princ (ende-worker--print result))))

(defun ende-worker--file-signature (files)
  "Snapshot FILES' attributes, excluding access times changed by loading."
  (mapcar
   (lambda (file)
     (let ((attributes (file-attributes file 'integer)))
       (list file (file-attribute-size attributes)
             (file-attribute-modification-time attributes)
             (file-attribute-status-change-time attributes)
             (file-attribute-inode-number attributes)
             (file-attribute-device-number attributes))))
   files))

(defun ende-worker--source-signature ()
  "Snapshot local sources, bytecode, and membership before worker startup."
  (ende-worker--file-signature
   (directory-files ende-worker--directory t "\\.elc?\\'")))

(defun ende-worker--dependency-signature ()
  "Snapshot loaded libraries and their source/bytecode alternatives."
  (ende-worker--file-signature
   (delete-dups
    (cl-loop for (file . _) in load-history
             when (and (stringp file) (file-name-absolute-p file))
             append
             (if (string-match "\\.el\\(?:c\\|\\.gz\\)?\\'" file)
                 (let ((base (substring file 0 (match-beginning 0))))
                   (mapcar (lambda (suffix) (concat base suffix))
                           '(".el" ".elc" ".el.gz")))
               (list file))))))

(defun ende-worker--cancel-timer (worker)
  "Release WORKER's startup or expiry timer."
  (when-let* ((timer (ende-worker--worker-timer worker)))
    (cancel-timer timer)
    (setf (ende-worker--worker-timer worker) nil)))

(defun ende-worker--finish (worker value error)
  "Dispose of WORKER and deliver VALUE or ERROR at most once."
  (unless (eq (ende-worker--worker-phase worker) 'finished)
    (let ((callback (ende-worker--worker-callback worker))
          (register (ende-worker--worker-register-cancellation worker))
          (registered (ende-worker--worker-registered worker))
          (process (ende-worker--worker-process worker))
          (output (ende-worker--worker-output worker))
          (errors (ende-worker--worker-errors worker)))
      (setf (ende-worker--worker-phase worker) 'finished
            (ende-worker--worker-callback worker) nil
            (ende-worker--worker-register-cancellation worker) nil
            (ende-worker--worker-registered worker) nil)
      (when (eq worker ende-worker--standby)
        (setq ende-worker--standby nil))
      (ende-worker--cancel-timer worker)
      (when (and process (process-live-p process))
        (delete-process process))
      (when-let* ((stderr (and (buffer-live-p errors) (get-buffer-process errors))))
        (when (process-live-p stderr) (delete-process stderr)))
      (dolist (buffer (list output errors))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (let ((kill-buffer-hook nil)
                  (kill-buffer-query-functions nil)
                  (buffer-list-update-hook nil))
              (kill-buffer buffer)))))
      (when registered
        (ignore-errors (funcall register nil)))
      (when callback (funcall callback value error)))))

(defun ende-worker--sentinel (worker process)
  "Settle WORKER when PROCESS exits."
  (when (and (not (eq (ende-worker--worker-phase worker) 'finished))
             (memq (process-status process) '(exit signal)))
    (let ((reply
           (condition-case caught
               (if (and (eq (ende-worker--worker-phase worker) 'running)
                        (zerop (process-exit-status process)))
                   (with-current-buffer (ende-worker--worker-output worker)
                     (goto-char (point-min))
                     (let ((value (read (current-buffer))))
                       (unless (or (plist-member value :value)
                                   (plist-member value :error))
                         (error "invalid worker result"))
                       value))
                 (error "worker failed: %s"
                        (with-current-buffer (ende-worker--worker-errors worker)
                          (string-trim (buffer-string)))))
             (error (list :error (error-message-string caught))))))
      (ende-worker--finish worker (plist-get reply :value) (plist-get reply :error)))))

(defun ende-worker--ready-filter (worker process text)
  "Accept only a complete readiness handshake from WORKER's PROCESS in TEXT."
  (when (eq (ende-worker--worker-phase worker) 'starting)
    (with-current-buffer (ende-worker--worker-output worker)
      (goto-char (point-max))
      (insert text)
      (when (save-excursion (goto-char (point-min)) (search-forward "\n" nil t))
        (condition-case nil
            (progn
              (goto-char (point-min))
              (let ((reply (read (current-buffer))))
                (skip-chars-forward " \t\r\n")
                (unless (and (eobp) (proper-list-p reply) (= (length reply) 2)
                             (eq (car reply) :ready) (consp (cadr reply))
                             (cl-every (lambda (entry)
                                         (and (consp entry) (stringp (car entry))))
                                       (cadr reply)))
                  (error "invalid worker readiness handshake"))
                (setf (ende-worker--worker-dependencies worker) (cadr reply)))
              (erase-buffer)
              (set-process-filter process nil)
              (ende-worker--cancel-timer worker)
              (setf (ende-worker--worker-phase worker) 'ready
                    (ende-worker--worker-timer worker)
                    (run-at-time ende-worker--idle-timeout nil
                                 #'ende-worker--finish worker nil "worker expired")))
          (error (ende-worker--finish worker nil "invalid worker readiness handshake")))))))

(defun ende-worker--start (worker &optional preload)
  "Start WORKER, optionally preloading project code before accepting input."
  (let ((default-directory ende-worker--directory))
    (setf (ende-worker--worker-output worker) (generate-new-buffer " *ende-worker*" t)
          (ende-worker--worker-errors worker) (generate-new-buffer " *ende-worker-errors*" t)
          (ende-worker--worker-process worker)
          (make-process
           :name "ende-worker" :buffer (ende-worker--worker-output worker)
           :stderr (ende-worker--worker-errors worker)
           :connection-type 'pipe :coding 'utf-8-unix :noquery t
           :command (list (expand-file-name invocation-name invocation-directory)
                          "-Q" "--batch" "-L" ende-worker--directory
                          "-l" (expand-file-name "ende-worker.el" ende-worker--directory)
                          "--eval" (if preload
                                       "(ende-worker--main 'ende-tools)"
                                     "(ende-worker--main)"))
           :filter (when preload
                     (lambda (process text)
                       (ende-worker--ready-filter worker process text)))
           :sentinel (lambda (process _event)
                       (ende-worker--sentinel worker process))))))

(defun ende-worker-shutdown ()
  "Invalidate prewarming and dispose of the sole unassigned project worker.
Assigned workers remain owned by their requests' cancellation lifecycles."
  (cl-incf ende-worker--generation)
  (when ende-worker--standby
    (ende-worker--finish ende-worker--standby nil "worker shutdown")))

(add-hook 'kill-emacs-hook #'ende-worker-shutdown)

(defun ende-worker-prewarm ()
  "Start at most one single-use project standby, without any request data.
Startup failure or expiry never schedules a replacement."
  (unless ende-worker--standby
    (let ((worker (ende-worker--create :phase 'starting)))
      (setq ende-worker--standby worker)
      (condition-case caught
          (progn
            (setf (ende-worker--worker-signature worker) (ende-worker--source-signature)
                  (ende-worker--worker-timer worker)
                  (run-at-time ende-worker--startup-timeout nil
                               #'ende-worker--finish worker nil "worker startup timed out"))
            (ende-worker--start worker t))
        ((error quit) (ende-worker--finish worker nil (error-message-string caught)))))))

(defun ende-worker--checkout ()
  "Detach a ready, live, source-compatible standby, or return nil immediately."
  (when-let* ((worker ende-worker--standby))
    (cond
     ((not (and (process-live-p (ende-worker--worker-process worker))
                (condition-case nil
                    (and (equal (ende-worker--worker-signature worker)
                                (ende-worker--source-signature))
                         (equal (ende-worker--worker-dependencies worker)
                                (ende-worker--file-signature
                                 (mapcar #'car (ende-worker--worker-dependencies worker)))))
                  (error nil))))
      (ende-worker--finish worker nil "worker sources changed")
      nil)
     ((eq (ende-worker--worker-phase worker) 'ready)
      (setq ende-worker--standby nil)
      (ende-worker--cancel-timer worker)
      worker))))

(defun ende-worker--run
    (library function arguments bindings callback register-cancellation &optional worker)
  "Dispatch one request to WORKER, or a new cold worker, using shared cleanup."
  (let* ((worker (or worker (ende-worker--create)))
         (cancel (lambda () (ende-worker--finish worker nil "worker cancelled"))))
    (setf (ende-worker--worker-phase worker) 'running
          (ende-worker--worker-callback worker) callback
          (ende-worker--worker-register-cancellation worker) register-cancellation)
    (condition-case caught
        (progn
          (unless (ende-worker--worker-process worker)
            (ende-worker--start worker))
          (when register-cancellation
            (setf (ende-worker--worker-registered worker) t)
            (funcall register-cancellation cancel))
          (unless (eq (ende-worker--worker-phase worker) 'finished)
            (let ((process (ende-worker--worker-process worker)))
              (process-send-string
               process (ende-worker--print (list library function arguments bindings)))
              (process-send-eof process))))
      ((error quit) (ende-worker--finish worker nil (error-message-string caught))))
    cancel))

(defun ende-worker-run
    (library function arguments bindings callback &optional register-cancellation)
  "Call FUNCTION with ARGUMENTS and BINDINGS in a clean Emacs loading LIBRARY.
Always start a cold, single-use process.  CALLBACK receives a value and an
error.  REGISTER-CANCELLATION receives the cancellation function on startup
and nil when the worker finishes."
  (ende-worker--run library function arguments bindings callback register-cancellation))

(defun ende-worker-run-prewarmed
    (library function arguments bindings callback &optional register-cancellation)
  "Like `ende-worker-run', using a ready single-use project standby if possible.
Only LIBRARY `ende-tools' is eligible; all other libraries stay cold.
An unready or incompatible standby never delays request dispatch."
  (if (not (eq library 'ende-tools))
      (ende-worker-run library function arguments bindings callback register-cancellation)
    (let* ((generation ende-worker--generation)
           (worker (ende-worker--checkout))
           (cancel
            (ende-worker--run
             library function arguments bindings
             (lambda (value error)
               (when (and (not worker) (= generation ende-worker--generation))
                 (ende-worker-prewarm))
               (funcall callback value error))
             register-cancellation worker)))
      (when (and worker (= generation ende-worker--generation))
        (ende-worker-prewarm))
      cancel)))

(provide 'ende-worker)
;;; ende-worker.el ends here
