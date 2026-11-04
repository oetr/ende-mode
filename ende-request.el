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

(defun ende-request--encode (builder messages profile debug preparation)
  "Build MESSAGES for PROFILE with immutable debug PREPARATION.
Return the outgoing bytes, encoded body, and optional capture record."
  (let* ((body (funcall builder messages profile))
         (prepared (when debug
                     (condition-case nil (ende-debug-prepare-body body preparation)
                       ((error quit) (cons body preparation)))))
         (body (if prepared (car prepared) body))
         (data (encode-coding-string (json-encode body) 'utf-8))
         (record (when debug
                   (ende-debug-record-request data body messages profile
                                              (car debug) (cadr debug) (cdr prepared)))))
    (list data body record)))

(defun ende-request--write (library builder messages profile file debug preparation)
  "Build and write a request to FILE; return payload-free diagnostics."
  (require library)
  (pcase-let* ((`(,data ,body ,record) (ende-request--encode builder messages profile debug preparation))
               (coding-system-for-write 'no-conversion))
    (write-region data nil file nil 'silent)
    (list :body (list (assq :tools body))
          :debug-record record)))

(defun ende-request-prepare (lifecycle library builder messages profile debug callback)
  "Prepare MESSAGES for PROFILE and call CALLBACK with data, body and record.
Image-bearing requests use a worker and a file owned by LIFECYCLE.
DEBUG contains transport, endpoint, and optional OAuth account continuity."
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
    (if (not images)
        (apply callback (ende-request--encode builder messages profile debug preparation))
      (let* ((file (make-temp-file (expand-file-name "ende-request-" (ende-curl-local-directory))))
             (bindings (mapcar (lambda (variable) (cons variable (symbol-value variable)))
                               '(ende-debugging-enabled
                                 ende-debug--session-directory ende-debug--sequence)))
             (failed (ende-transport-session-callback-error-handler lifecycle)))
        (ende-transport-session-add-canceller
         lifecycle (lambda () (ende-ignore-errors #'delete-file file)))
        (ende-transport-session-add-canceller
         lifecycle
         (ende-worker-run
          'ende-request #'ende-request--write
          (list library builder messages profile file debug preparation) bindings
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

;;; ende-request.el ends here
