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

(defun ende-request--encode (builder messages profile debug preparation &optional finalization)
  "Build MESSAGES for PROFILE with immutable debug PREPARATION.
Return the outgoing bytes, encoded body, and optional capture record."
  (let* ((body (funcall builder messages profile))
         (prepared (when debug
                     (if (eq builder #'ende-request--retained-body)
                         (cons body preparation)
                     (condition-case nil (ende-debug-prepare-body body preparation)
                       ((error quit) (cons body preparation))))))
         (body (if prepared (car prepared) body))
         (full-body body)
         (body (if finalization
                   (apply (car finalization) body (cdr finalization))
                 body))
         (data (encode-coding-string (json-encode body) 'utf-8))
         (record (when debug
                   (ende-debug-record-request data body messages profile
                                              (car debug) (cadr debug) (cdr prepared)))))
    (list (if finalization (list :data data :full-body full-body) data)
          body record)))

(defun ende-request--write (library builder messages profile file full-file debug preparation finalization)
  "Build and write a request to FILE; return payload-free diagnostics."
  (require library)
  (pcase-let* ((`(,data ,body ,record) (ende-request--encode builder messages profile debug preparation finalization))
               (coding-system-for-write 'no-conversion))
    (when finalization
      (write-region (encode-coding-string (json-encode (plist-get data :full-body)) 'utf-8)
                    nil full-file nil 'silent)
      (setq data (plist-get data :data)))
    (write-region data nil file nil 'silent)
    (list :body (list (assq :tools body))
          :debug-record record)))

(defun ende-request-prepare (lifecycle library builder messages profile debug callback
                                     &optional finalization file-backed)
  "Prepare MESSAGES for PROFILE and call CALLBACK with data, body and record.
Image-bearing requests use a worker and a file owned by LIFECYCLE.
DEBUG contains transport, endpoint, and optional OAuth account continuity.
FINALIZATION is (FUNCTION . ARGUMENTS), containing only serializable data.
It converts the complete logical body to the actual captured wire request.
Finalized data retains :full-body or :full-file separately for recovery.
FILE-BACKED forces worker preparation, including for a retained request."
  (let* ((debug (when (and ende-debugging-enabled ende-debug--session-directory) debug))
         (preparation (when debug
                        (ende-debug-prepare-request
                         profile (car debug) (cadr debug) (nth 2 debug)
                         (memq builder '(ende-responses--request-body ende-codex--request-body)))))
         (images (cl-some (lambda (message)
                            (let ((content (ende-content-body message)))
                              (and (listp content)
                                   (cl-some (lambda (part) (eq (plist-get part :type) :image)) content))))
                          messages)))
    (if (not (or images file-backed))
        (apply callback (ende-request--encode builder messages profile debug preparation finalization))
      (let* ((file (make-temp-file (expand-file-name "ende-request-" (ende-curl-local-directory))))
             (full-file nil)
             (bindings (mapcar (lambda (variable) (cons variable (symbol-value variable)))
                               '(ende-debugging-enabled
                                 ende-debug--session-directory ende-debug--sequence)))
             (failed (ende-transport-session-callback-error-handler lifecycle)))
        (ende-transport-session-add-canceller
         lifecycle (lambda ()
                     (ende-ignore-errors #'delete-file file)
                     (when full-file (ende-ignore-errors #'delete-file full-file))))
        (when finalization
          (setq full-file (make-temp-file (expand-file-name "ende-request-full-" (ende-curl-local-directory)))))
        (ende-transport-session-add-canceller
         lifecycle
         (ende-worker-run
          'ende-request #'ende-request--write
          (list library builder messages profile file full-file debug preparation finalization) bindings
          (lambda (result error)
            (unless (ende-transport-session-terminal lifecycle)
              (condition-case failure
                  (if error
                      (funcall failed (ende-error-create :kind 'startup :message error))
                    (funcall callback (if finalization
                                          (list :file file :full-file full-file)
                                        (list :file file))
                             (plist-get result :body)
                             (plist-get result :debug-record)))
                ((error quit)
                 (funcall failed (ende-error-create :kind 'startup
                                                   :message (error-message-string failure)))))))))))))

(defun ende-request--retained-body (data _profile)
  "Read the complete logical request retained in DATA."
  (if-let* ((file (plist-get data :full-file)))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((json-object-type 'alist) (json-key-type 'keyword)
              (json-array-type 'vector))
          (json-read-from-string (buffer-string))))
    (plist-get data :full-body)))

(defun ende-request-reprepare (lifecycle library data profile debug callback finalization)
  "Finalize retained DATA again without rebuilding or changing logical input.
LIFECYCLE owns any files; LIBRARY provides FINALIZATION.  PROFILE and DEBUG
scope a fresh capture.  CALLBACK receives the prepared request."
  (ende-request-prepare lifecycle library #'ende-request--retained-body
                        data profile debug callback finalization
                        (plist-get data :full-file)))

(defun ende-request-send (process data)
  "Send prepared DATA to PROCESS, unless curl reads it from a file."
  (unless (listp data)
    (process-send-string process data))
  (process-send-eof process))

(provide 'ende-request)

;;; ende-request.el ends here
