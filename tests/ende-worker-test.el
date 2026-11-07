;;; ende-worker-test.el --- Worker lifecycle tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter Samarin
;;
;; Author: Peter Samarin <peter.samarin@gmail.com>
;; Maintainer: Peter Samarin <peter.samarin@gmail.com>
;; Keywords: lisp, tools
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

;; Tests for worker lifecycles.

;;; Code:

(require 'ert)
(require 'ende-worker)
(require 'ende-test-wait)

(ert-deftest ende-worker/round-trip-keeps-the-event-loop-free ()
  :tags '(integration)
  (let* ((load-path (cons "/tmp/ende-parent-only-load-path" load-path))
         (tick nil)
         (done nil)
         (value nil)
         (failure nil)
         (cancel nil)
         (timer (run-at-time 0 nil (lambda () (setq tick t)))))
    (unwind-protect
        (progn
          (setq cancel
                (ende-worker-run
                 'cl-lib 'eval
                 '((progn (sleep-for 0.05)
                          (list (emacs-pid) case-fold-search "λ\nquoted \"text\""
                                (member "/tmp/ende-parent-only-load-path" load-path))))
                 '((case-fold-search . nil))
                 (lambda (result error)
                   (setq value result failure error done t))))
          (should-not done)
          (ende-test--wait-for (lambda () done)
                               :description "worker result callback")
          (should tick)
          (should-not failure)
          (should-not (= (car value) (emacs-pid)))
          (should (equal (cdr value) '(nil "λ\nquoted \"text\"" nil))))
      (when cancel (funcall cancel))
      (cancel-timer timer))))

(ert-deftest ende-worker/errors-and-cancellation-settle-once ()
  :tags '(integration)
  (dolist (cancel-p '(nil t))
    (let ((done nil)
          (calls 0)
          (failure nil)
          (cancel nil)
          (registered nil))
      (unwind-protect
          (progn
            (setq cancel
                  (ende-worker-run
                   'cl-lib (if cancel-p 'sleep-for 'error)
                   (if cancel-p '(60) '("worker error")) nil
                   (lambda (_value error)
                     (setq failure error done t calls (1+ calls)))
                   (lambda (function) (setq registered function))))
            (should (functionp registered))
            (when cancel-p (funcall registered))
            (ende-test--wait-for (lambda () done)
                                 :description "worker result callback")
            (should (stringp failure))
            (should-not registered)
            (funcall cancel)
            (accept-process-output nil 0.05)
            (should (= calls 1)))
        (when cancel (funcall cancel))))))

(ert-deftest ende-worker/project-loading-keeps-dependencies-small ()
  :tags '(integration)
  (let (done value failure)
    (ende-worker-run
     'ende-tools 'eval
     '((mapcar #'featurep '(ende-tools ende-queue ende-provider ende-ui))) nil
     (lambda (result error) (setq value result failure error done t)))
    (ende-test--wait-for (lambda () done) :description "lean project worker")
    (should-not failure)
    (should (equal value '(t nil nil nil)))))

(defmacro ende-worker-test--with-standby (&rest body)
  "Run BODY with an isolated standby lifecycle."
  (declare (indent 0) (debug t))
  `(progn
     (ende-worker-shutdown)
     (let ((ende-worker--standby nil)
           (ende-worker--generation 0))
       (unwind-protect (progn ,@body)
         (ende-worker-shutdown)))))

(defun ende-worker-test--ready ()
  "Return a ready standby after a bounded wait."
  (ende-worker-prewarm)
  (ende-test--wait-for
   (lambda () (and ende-worker--standby
                   (eq (ende-worker--worker-phase ende-worker--standby) 'ready)))
   :description "project worker readiness")
  ende-worker--standby)

(defun ende-worker-test--call (runner form &optional bindings)
  "Return RUNNER's evaluation of FORM with BINDINGS in a project worker."
  (let (done value failure cancel)
    (unwind-protect
        (progn
          (setq cancel
                (funcall runner 'ende-tools 'eval (list form) bindings
                         (lambda (result error)
                           (setq done t value result failure error))))
          (ende-test--wait-for (lambda () done) :description "project worker result")
          (should-not failure)
          value)
      (when cancel (funcall cancel)))))

(ert-deftest ende-worker/standby-is-ready-only-single-use-and-never-generic ()
  :tags '(integration)
  (ende-worker-test--with-standby
    (let* ((worker (ende-worker-test--ready))
           (process (ende-worker--worker-process worker))
           (pid (process-id process))
           (output (ende-worker--worker-output worker))
           (errors (ende-worker--worker-errors worker))
           (timer (ende-worker--worker-timer worker)))
      (dotimes (_ 2) (ende-worker-prewarm))
      (should (eq worker ende-worker--standby))
      (should-not (= pid (ende-worker-test--call #'ende-worker-run '(emacs-pid))))
      (let (done cold-pid)
        (ende-worker-run-prewarmed
         'cl-lib 'emacs-pid nil nil
         (lambda (value error) (should-not error) (setq cold-pid value done t)))
        (ende-test--wait-for (lambda () done) :description "incompatible cold worker")
        (should-not (= pid cold-pid)))
      (should (eq worker ende-worker--standby))
      (should
       (equal (ende-worker-test--call
               #'ende-worker-run-prewarmed
               '(list (emacs-pid) (boundp 'ende-worker-test-secret) case-fold-search
                      (mapcar #'featurep '(ende-tools ende-queue ende-provider ende-ui)))
               '((ende-worker-test-secret . "request data") (case-fold-search . nil)))
              (list pid t nil '(t nil nil nil))))
      (should-not (process-live-p process))
      (should-not (buffer-live-p output))
      (should-not (buffer-live-p errors))
      (should-not (memq timer timer-list))
      (let* ((replacement (ende-worker-test--ready))
             (next-pid (process-id (ende-worker--worker-process replacement))))
        (should-not (= pid next-pid))
        (should
         (equal (ende-worker-test--call
                 #'ende-worker-run-prewarmed
                 '(list (emacs-pid) (boundp 'ende-worker-test-secret) case-fold-search))
                (list next-pid nil t)))))))

(ert-deftest ende-worker/starting-standby-falls-back-without-waiting ()
  :tags '(integration)
  (ende-worker-test--with-standby
    (cl-letf (((symbol-function 'ende-worker--ready-filter) #'ignore))
      (ende-worker-prewarm)
      (let* ((worker ende-worker--standby)
             (pid (process-id (ende-worker--worker-process worker)))
             (cancel nil)
             (done nil)
             (value nil))
        (unwind-protect
            (progn
              (setq cancel
                    (ende-worker-run-prewarmed
                     'ende-tools 'emacs-pid nil nil
                     (lambda (result error) (should-not error) (setq done t value result))))
              (should (eq (ende-worker--worker-phase worker) 'starting))
              (should-not done)
              (ende-test--wait-for (lambda () done) :description "immediate cold fallback")
              (should-not (= value pid))
              (should (eq worker ende-worker--standby))
              (should (eq (ende-worker--worker-phase worker) 'starting)))
          (when cancel (funcall cancel)))))))

(ert-deftest ende-worker/immediate-registration-cancels-before-dispatch-once ()
  :tags '(integration)
  (dolist (warm '(nil t))
    (ende-worker-test--with-standby
      (let* ((worker (when warm (ende-worker-test--ready)))
             (assigned (when worker (ende-worker--worker-process worker)))
             (start (symbol-function 'ende-worker--start))
             (delete (symbol-function 'delete-process))
             (send (symbol-function 'process-send-string))
             (deletions 0) (sends 0) (calls 0) (registrations 0) cancel)
        (cl-letf (((symbol-function 'ende-worker--start)
                   (lambda (state &optional preload)
                     (funcall start state preload)
                     (unless preload
                       (setq assigned (ende-worker--worker-process state)))))
                  ((symbol-function 'delete-process)
                   (lambda (process)
                     (when (eq process assigned) (cl-incf deletions))
                     (funcall delete process)))
                  ((symbol-function 'process-send-string)
                   (lambda (process text)
                     (when (eq process assigned) (cl-incf sends))
                     (funcall send process text))))
          (setq cancel
                (funcall (if warm #'ende-worker-run-prewarmed #'ende-worker-run)
                         'ende-tools 'sleep-for '(60) nil
                         (lambda (value error)
                           (should-not value)
                           (should (equal error "worker cancelled"))
                           (cl-incf calls))
                         (lambda (function)
                           (cl-incf registrations)
                           (when function (funcall function)))))
          (funcall cancel)
          (accept-process-output nil 0.05)
          (should (= calls 1))
          (should (= registrations 2))
          (should (= deletions 1))
          (should (zerop sends))
          (should-not (process-live-p assigned)))))))

(ert-deftest ende-worker/standby-deadlines-and-startup-failure-do-not-restart ()
  :tags '(integration)
  (ende-worker-test--with-standby
    (dolist (starting '(t nil))
      (let ((ende-worker--startup-timeout (if starting 0.02 10))
            (ende-worker--idle-timeout 0.02)
            (filter (symbol-function 'ende-worker--ready-filter)))
        (cl-letf (((symbol-function 'ende-worker--ready-filter)
                   (if starting #'ignore filter)))
          (ende-worker-prewarm)
          (let ((worker ende-worker--standby))
            (ende-test--wait-for (lambda () (null ende-worker--standby))
                                 :description "standby deadline cleanup")
            (should (eq (ende-worker--worker-phase worker) 'finished))
            (should-not (ende-worker--worker-timer worker))
            (should-not (process-live-p (ende-worker--worker-process worker)))
            (should-not (buffer-live-p (ende-worker--worker-output worker)))
            (should-not (buffer-live-p (ende-worker--worker-errors worker)))
            (accept-process-output nil 0.05)
            (should-not ende-worker--standby)))))
    (let ((before (buffer-list)) (attempts 0))
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest _) (cl-incf attempts) (error "startup failure"))))
        (ende-worker-prewarm)
        (accept-process-output nil 0.05)
        (should (= attempts 1))
        (should-not ende-worker--standby)
        (should-not
         (cl-some (lambda (buffer)
                    (string-prefix-p " *ende-worker" (buffer-name buffer)))
                  (cl-set-difference (buffer-list) before)))))))

(ert-deftest ende-worker/dependency-source-and-bytecode-changes-force-fresh-code ()
  :tags '(integration)
  (ende-worker-test--with-standby
    (let* ((source (expand-file-name "ende-worker.el" ende-worker--directory))
           (directory (make-temp-file "ende-worker-libraries-" t))
           (ende-worker--directory directory)
           (dependency (make-temp-file "ende-worker-dependency-" nil ".el"))
           (linked-dependency (expand-file-name "dependency.el" directory)))
      (unwind-protect
          (progn
            (copy-file source (expand-file-name "ende-worker.el" directory))
            (make-symbolic-link dependency linked-dependency)
            (make-symbolic-link (byte-compile-dest-file dependency)
                                (byte-compile-dest-file linked-dependency))
            (with-temp-file (expand-file-name "ende-tools.el" directory)
              (insert (format "(require 'ende-worker-test-dependency %S)\n(provide 'ende-tools)\n"
                              (file-name-sans-extension linked-dependency))))
            (with-temp-file dependency
              (insert ";;; -*- lexical-binding: t; -*-\n"
                      "(defconst ende-worker-test-version 'old)\n"
                      "(provide 'ende-worker-test-dependency)\n"))
            (byte-compile-file dependency)
            (let* ((worker (ende-worker-test--ready))
                   (process (ende-worker--worker-process worker)))
              (with-temp-file dependency
                (insert ";;; -*- lexical-binding: t; -*-\n"
                        "(defconst ende-worker-test-version 'source)\n"
                        "(provide 'ende-worker-test-dependency)\n"))
              (set-file-times dependency (time-add (current-time) 10))
              (let ((value (ende-worker-test--call
                            #'ende-worker-run-prewarmed
                            '(list (emacs-pid) ende-worker-test-version))))
                (should-not (= (car value) (process-id process)))
                (should (eq (cadr value) 'source)))
              (should-not (process-live-p process)))
            (let* ((worker (ende-worker-test--ready))
                   (process (ende-worker--worker-process worker)))
              (with-temp-file dependency
                (insert ";;; -*- lexical-binding: t; -*-\n"
                        "(defconst ende-worker-test-version 'bytecode)\n"
                        "(provide 'ende-worker-test-dependency)\n"))
              (byte-compile-file dependency)
              (with-temp-file dependency
                (insert ";;; -*- lexical-binding: t; -*-\n"
                        "(defconst ende-worker-test-version 'source)\n"
                        "(provide 'ende-worker-test-dependency)\n"))
              (set-file-times dependency (time-subtract (current-time) 10))
              (let ((value (ende-worker-test--call
                            #'ende-worker-run-prewarmed
                            '(list (emacs-pid) ende-worker-test-version))))
                (should-not (= (car value) (process-id process)))
                (should (eq (cadr value) 'bytecode)))
              (should-not (process-live-p process))))
        (ende-worker-shutdown)
        (delete-file dependency)
        (delete-file (byte-compile-dest-file dependency))
        (delete-directory directory t)))))

(ert-deftest ende-worker/shutdown-abort-and-reload-invalidate-without-replenishment ()
  :tags '(integration)
  (require 'ende)
  (ende-worker-test--with-standby
    (let ((cancel nil) (done nil))
      (unwind-protect
          (progn
            (setq cancel
                  (ende-worker-run-prewarmed
                   'ende-tools 'eval '((progn (sleep-for 0.05) t)) nil
                   (lambda (value error) (should-not error) (setq done value))))
            (ende-worker-shutdown)
            (ende-test--wait-for (lambda () done) :description "request after shutdown")
            (should-not ende-worker--standby))
        (when cancel (funcall cancel))))
    (dolist (action (list #'ende-abort-queue #'ende-reload))
      (let* ((worker (ende-worker-test--ready))
             (process (ende-worker--worker-process worker))
             (output (ende-worker--worker-output worker))
             (errors (ende-worker--worker-errors worker))
             (timer (ende-worker--worker-timer worker)))
        (funcall action)
        (should-not ende-worker--standby)
        (should-not (process-live-p process))
        (should-not (buffer-live-p output))
        (should-not (buffer-live-p errors))
        (should-not (memq timer timer-list))))
    (should (= 1 (cl-count #'ende-worker-shutdown ende-queue-abort-functions)))
    (should (= 1 (cl-count #'ende-worker-shutdown kill-emacs-hook)))))

(provide 'ende-worker-test)

;;; ende-worker-test.el ends here
