;;; ende-content.el --- Ordered message content -*- lexical-binding: t; -*-

;;; Code:
(require 'cl-lib)
(require 'subr-x)

(defun ende-content-role (message)
  "Return MESSAGE's role."
  (if (stringp (car-safe message)) (car message) (plist-get message :role)))

(defun ende-content-body (message)
  "Return MESSAGE's string or ordered content parts."
  (if (stringp (car-safe message)) (cdr message) (plist-get message :content)))

(defun ende-content-concat (contents)
  "Concatenate CONTENTS without changing part order."
  (if (cl-every #'stringp contents) (mapconcat #'identity contents "")
    (cl-mapcan (lambda (content)
                 (if (stringp content) (list (list :type :text :text content))
                   (copy-sequence content))) contents)))

(defun ende-content-readable (content)
  "Return an inspection-only rendering of CONTENT without encoded bytes."
  (if (stringp content) content
    (mapconcat
     (lambda (part)
       (pcase (plist-get part :type)
         (:text (plist-get part :text))
         (:image (format "\n[%s: %s, %s×%s, %.1f KB]\n"
                         (plist-get part :media-type)
                         (file-name-nondirectory (plist-get part :path))
                         (plist-get part :width) (plist-get part :height)
                         (/ (or (plist-get part :bytes) 0) 1024.0)))
         (_ (error "Unsupported message content part")))) content "")))

(defun ende-content-empty-p (content)
  "Return non-nil if CONTENT has neither images nor nonblank text."
  (if (stringp content) (string-blank-p content)
    (cl-every (lambda (part) (and (eq (plist-get part :type) :text)
                                 (string-blank-p (plist-get part :text)))) content)))

(provide 'ende-content)
;;; ende-content.el ends here
