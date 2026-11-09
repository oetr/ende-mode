;;; ende-request.el --- Asynchronous multimodal request preparation -*- lexical-binding: t; -*-

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

;; Asynchronous multimodal request preparation for Ende.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'ende-core)
(require 'ende-transport)
(require 'ende-content)
(require 'ende-curl)
(require 'ende-debug)
(require 'ende-worker)
(require 'ende-provider-config)

(declare-function ende--validate-messages "ende-operations" (messages))

(cl-defstruct ende-request-retained
  source owner (readers 0) debug-origin)

(defun ende-request-retained--collect (retained)
  "Release RETAINED's source only after its owner and worker readers leave."
  (when (and (not (ende-request-retained-owner retained))
             (zerop (ende-request-retained-readers retained)))
    (when-let* ((source (ende-request-retained-source retained))
                (file (or (plist-get source :full-file) (plist-get source :file))))
      (ende-ignore-errors #'delete-file file))
    (setf (ende-request-retained-source retained) nil
          (ende-request-retained-debug-origin retained) nil)))

(defun ende-request-retained-release (retained owner)
  "Idempotently release OWNER's claim on RETAINED."
  (when (and retained (eq owner (ende-request-retained-owner retained)))
    (setf (ende-request-retained-owner retained) nil)
    (ende-request-retained--collect retained)))

(defun ende-request-retained-read (retained)
  "Protect RETAINED during preparation; return an idempotent reader release."
  (when retained
    (cl-incf (ende-request-retained-readers retained))
    (let (released)
      (lambda ()
        (unless released
          (setq released t)
          (cl-decf (ende-request-retained-readers retained))
          (ende-request-retained--collect retained))))))

(defun ende-request--retain (source lifecycle)
  "Give LIFECYCLE initial ownership of immutable logical SOURCE."
  (let ((retained (make-ende-request-retained :source source :owner lifecycle)))
    (ende-transport-session-add-canceller
     lifecycle (lambda () (ende-request-retained-release retained lifecycle)))
    retained))

(defun ende-request--encode (builder messages profile debug preparation &optional finalization replay)
  "Build MESSAGES for PROFILE with immutable debug PREPARATION.
Return the outgoing bytes, encoded body, and optional capture record."
  (unless replay
    (let ((resolved (ende-content-resolve-messages messages)))
      (unless (eq resolved messages)
        (require 'ende-operations)
        (setq messages (ende--validate-messages resolved)))))
  (let* ((ende-debugging-enabled (and debug t))
         (ende-debug--session-directory (or (plist-get preparation :directory) ende-debug--session-directory))
         (body (funcall builder messages profile))
         (prepared (when debug
                     (if replay
                         (cons body preparation)
                     (condition-case nil (ende-debug-prepare-body body preparation)
                       ((error quit) (cons body preparation))))))
         (body (if prepared (car prepared) body))
         (full-body (when finalization
                      (if replay body (ende-provider-config-copy body))))
         (body (if finalization
                   (apply (car finalization) body (cdr finalization))
                 body))
         (data (encode-coding-string (json-encode body) 'utf-8))
         (record (when debug
                   (ende-debug-record-request data body messages profile
                                              (car debug) (cadr debug) (cdr prepared)))))
    (list (if finalization (list :data data :full-body full-body :debug-preparation (cdr prepared)) data)
          body record)))

(defun ende-request--write (library builder messages profile file full-file debug preparation finalization replay)
  "Build and write a request to FILE; return payload-free diagnostics."
  (require library)
  (pcase-let* ((`(,data ,body ,record) (ende-request--encode builder messages profile debug preparation finalization replay))
               (origin (and finalization (plist-get data :debug-preparation)))
               (coding-system-for-write 'no-conversion))
    (when (and finalization full-file)
      (write-region (encode-coding-string (ende-worker--print (plist-get data :full-body)) 'utf-8)
                    nil full-file nil 'silent))
    (when finalization (setq data (plist-get data :data)))
    (write-region data nil file nil 'silent)
    (list :bytes (length data) :body (list (assq :tools body))
          :debug-preparation origin
          :debug-record record)))

(defconst ende-request--worker-threshold (* 64 1024))

(defun ende-request--worker-p (messages)
  "Return non-nil when MESSAGES warrant asynchronous preparation."
  (let ((bytes 0))
    (cl-some
     (lambda (message)
       (let ((content (ende-content-body message)))
         (if (stringp content)
             (> (cl-incf bytes (string-bytes content)) ende-request--worker-threshold)
           (cl-some (lambda (part)
                      (or (memq (plist-get part :type) '(:image :source))
                          (> (cl-incf bytes (string-bytes (or (plist-get part :text) "")))
                             ende-request--worker-threshold))) content)))) messages)))

(defun ende-request-prepare (lifecycle library builder messages profile debug callback
                                     &optional finalization file-backed retained baseline current-p)
  "Prepare MESSAGES for PROFILE and call CALLBACK with data, body and record.
Image-bearing and large requests use a worker and a file owned by LIFECYCLE.
DEBUG contains transport, endpoint, and optional OAuth account continuity.
FINALIZATION is (FUNCTION . ARGUMENTS), containing only serializable data.
It converts the complete logical body to the actual captured wire request.
Finalized data retains :full-body or :full-file separately for recovery.
FILE-BACKED forces worker preparation.  RETAINED reuses a logical source;
BASELINE protects the predecessor source while finalization reads it.
CURRENT-P optionally checks that an asynchronous result still owns its attempt.
Only source descriptors and serializable finalization data enter the worker."
  (let* ((replay (and retained t))
         (origin (and retained (ende-request-retained-debug-origin retained)))
         (debug (if replay (plist-get origin :debug)
                  (when (and ende-debugging-enabled ende-debug--session-directory) debug)))
         (preparation (when debug
                        (if replay (ende-debug-prepare-retry origin)
                          (ende-debug-prepare-request
                           profile (car debug) (cadr debug) (nth 2 debug)
                           (memq builder '(ende-responses--request-body ende-codex--request-body))))))
         (counter (if replay (plist-get origin :counter) ende-debug--sequence-context))
         (worker (and (not retained) (ende-request--worker-p messages)))
         release-baseline)
    (if (not (or worker file-backed))
        (unwind-protect
            (progn
              (setq release-baseline (ende-request-retained-read baseline))
              (pcase-let ((`(,data ,body ,record)
                           (ende-request--encode builder messages profile debug preparation finalization replay)))
              (when finalization
                (unless retained
                  (setq retained (ende-request--retain (list :full-body (plist-get data :full-body)) lifecycle)))
                (when (and debug (not replay))
                  (setf (ende-request-retained-debug-origin retained)
                        (list :debug debug :preparation (plist-get data :debug-preparation) :counter counter)))
                (setq data (plist-put data :retained retained)))
              (if (or (ende-transport-session-terminal lifecycle)
                      (and current-p (not (funcall current-p))))
                  (ende-ignore-errors #'ende-debug-record-completion record 'discarded nil nil nil)
                (funcall callback data body record))))
          (when release-baseline (funcall release-baseline)))
      (let* (file wire full-file release-wire release-source
             (bindings (list (cons 'ende-debugging-enabled (and debug t))
                             (cons 'ende-max-request-chars ende-max-request-chars)
                             (cons 'ende-debug--session-directory (plist-get preparation :directory))
                             (cons 'ende-debug--sequence (or (plist-get preparation :sequence) 0))))
             (failed (ende-transport-session-callback-error-handler lifecycle))
             cancel finished)
        (cl-labels
            ((finish (result error)
               ;; The worker has stopped before any file protection is released.
               (unless finished
                 (setq finished t)
                 (when release-wire (funcall release-wire))
                 (when release-source (funcall release-source))
                 (when release-baseline (funcall release-baseline))
                 (if (or (ende-transport-session-terminal lifecycle)
                         (and current-p (not (funcall current-p))))
                     (progn
                       (ende-request-retained-release wire lifecycle)
                       (unless replay (ende-request-retained-release retained lifecycle))
                       (ende-ignore-errors #'ende-debug-record-completion
                                           (plist-get result :debug-record) 'discarded nil nil nil))
                   (condition-case nil
                       (if error
                           (progn
                             (ende-request-retained-release wire lifecycle)
                             (unless replay (ende-request-retained-release retained lifecycle))
                             (funcall failed (ende-error-create :kind 'startup :message "Request preparation failed")))
                         (progn
                           (when (and retained debug (not replay))
                             (setf (ende-request-retained-debug-origin retained)
                                   (list :debug debug :preparation (plist-get result :debug-preparation) :counter counter)))
                           (funcall callback
                                  (if finalization
                                      (append (list :file file :bytes (plist-get result :bytes)
                                                    :wire-source wire :retained retained)
                                              (ende-request-retained-source retained))
                                    (list :file file :bytes (plist-get result :bytes) :wire-source wire))
                                  (plist-get result :body) (plist-get result :debug-record))))
                     ((error quit)
                      (funcall failed (ende-error-create :kind 'startup :message "Request preparation failed"))))))))
          (ende-transport-session-add-canceller
           lifecycle (lambda ()
                       (when cancel (funcall cancel))
                       (when finished (ende-request-retained-release wire lifecycle))))
          (condition-case nil
              (progn
                (setq file (make-temp-file (expand-file-name "ende-request-" (ende-curl-local-directory))))
                (setq wire (ende-request--retain (list :file file) lifecycle)
                      release-wire (ende-request-retained-read wire))
                (when (and finalization (not retained))
                  (setq full-file (make-temp-file (expand-file-name "ende-request-full-" (ende-curl-local-directory)))
                        retained (ende-request--retain (list :full-file full-file) lifecycle)))
                (setq release-source (ende-request-retained-read retained)
                      release-baseline (ende-request-retained-read baseline))
                (ende-worker-run
                 'ende-request #'ende-request--write
                 (list library builder messages profile file full-file debug preparation finalization replay) bindings
                 #'finish
                 (lambda (function)
                   (setq cancel function)
                   (when (and function (ende-transport-session-terminal lifecycle))
                     (funcall function)))))
            ((error quit)
             (when cancel (funcall cancel))
             (finish nil t))))))))

(defun ende-request--retained-body (data _profile)
  "Read the complete logical request retained in DATA."
  (if-let* ((file (plist-get data :full-file)))
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8-unix))
          (insert-file-contents file))
        (goto-char (point-min))
        (read (current-buffer)))
    (plist-get data :full-body)))

(defun ende-request-reprepare (lifecycle library retained profile debug callback finalization &optional current-p)
  "Finalize RETAINED again without rebuilding or changing logical input.
LIFECYCLE owns any files; LIBRARY provides FINALIZATION.  PROFILE and DEBUG
scope a fresh capture.  CALLBACK receives the prepared request."
  (let ((source (ende-request-retained-source retained)))
    (ende-request-prepare lifecycle library #'ende-request--retained-body
                          source profile debug callback finalization
                          (plist-get source :full-file) retained nil current-p)))

(defun ende-request-send (process data)
  "Send prepared DATA to PROCESS, unless curl reads it from a file."
  (unless (listp data)
    (process-send-string process data))
  (process-send-eof process))

(provide 'ende-request)

;;; ende-request.el ends here
