;;; ende-controls-test.el --- Shared control encoder tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Coverage for pure control mapping and JSON replacement.

;;; Code:

(require 'ert)
(require 'ende-controls)

(ert-deftest ende-test/controls-encode-flat-and-nested-fields-with-replacement ()
  (let* ((body '((:temperature . 1)
                 ("temperature" . 2)
                 (:reasoning . ((:effort . "old")
                                ("mode" . "keep")))))
         (encoded
          (ende-controls-encode
           body '((:temperature :path ("temperature"))
                  (:reasoning-effort :path ("reasoning" "effort")))
           '(:temperature 0 :reasoning-effort "high"))))
    (should (= (length (cl-remove-if-not
                        (lambda (entry)
                          (equal (ende-controls--json-key-name (car entry))
                                 "temperature"))
                        encoded))
               1))
    (should (= (alist-get :temperature encoded) 0))
    (should (equal (alist-get :effort (alist-get :reasoning encoded)) "high"))
    (should (equal (cdr (assoc "mode" (alist-get :reasoning encoded)))
                   "keep"))))

(ert-deftest ende-test/controls-encode-thinking-off-keeps-effort-preference-inactive ()
  (let* ((values '(:thinking off :reasoning-effort "high"))
         (encoded
          (ende-controls-encode
           nil '((:thinking :path ("thinking" "type")
                            :choices (on off)
                            :values ((on . "enabled") (off . "disabled")))
                 (:reasoning-effort :path ("reasoning_effort")
                                    :choices ("low" "high")))
           values)))
    (should (equal (alist-get :type (alist-get :thinking encoded)) "disabled"))
    (should-not (assq :reasoning_effort encoded))
    (should (equal (plist-get values :reasoning-effort) "high"))))

(ert-deftest ende-test/controls-merge-replaces-properties-and-disables-mappings ()
  (should
   (equal
    (ende-controls-merge-descriptors
     '((:temperature :path ("temperature"))
       (:reasoning-effort :path ("reasoning" "effort")
                          :choices ("low" "high")))
     '((:reasoning-effort :choices ("low" "max"))
       (:temperature)))
    '((:reasoning-effort :path ("reasoning" "effort")
                         :choices ("low" "max"))))))

(ert-deftest ende-test/controls-reject-invalid-or-conflicting-mappings ()
  (dolist (descriptors
           '(((:other :path ("other")))
             ((:temperature :path))
             ((:temperature :path nil))
             ((:temperature :path ("messages" "0")))
             ((:temperature :path ("control"))
              (:thinking :path ("control" "mode")))
             ((:thinking :path ("thinking") :choices ("enabled")))))
    (should-error (ende-controls-validate-descriptors descriptors)))
  (should-error
   (ende-controls-encode
    nil nil '(:temperature 0.2)))
  (should-error
   (ende-controls-encode
    nil '((:thinking :path ("thinking") :choices (on off)))
    '(:thinking "off"))))

(provide 'ende-controls-test)

;;; ende-controls-test.el ends here
