;;; deslop-transport.el --- Shared transport helpers for deslop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Keywords: convenience, tools, llm

;;; Commentary:

;; Small, protocol-independent helpers shared by streaming transports.

;;; Code:

(require 'deslop-protocol)

(defun deslop-transport-byte-prefix (text max-bytes)
  "Return the longest character prefix of TEXT within MAX-BYTES."
  (if (<= (string-bytes text) max-bytes)
      text
    (let ((low 0)
          (high (length text)))
      (while (< low high)
        (let ((middle (/ (+ low high 1) 2)))
          (if (<= (string-bytes (substring text 0 middle)) max-bytes)
              (setq low middle)
            (setq high (1- middle)))))
      (substring text 0 low))))

(defun deslop-transport-error-detail (preview)
  "Return a useful provider error detail parsed from PREVIEW, or nil."
  (condition-case nil
      (let* ((payload (deslop-protocol--json-read-string preview))
             (error-payload (deslop-protocol--json-get "error" payload)))
        (or (deslop-protocol--json-get "message" error-payload)
            (deslop-protocol--json-get "detail" error-payload)
            (deslop-protocol--json-get "message" payload)
            (deslop-protocol--json-get "detail" payload)
            (and (stringp error-payload) error-payload)))
    (error nil)))

(provide 'deslop-transport)

;;; deslop-transport.el ends here
