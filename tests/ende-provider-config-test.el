;;; ende-provider-config-test.el --- Provider value contracts -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'ende-provider-config)

(ert-deftest ende-provider-config-json-values ()
  (let* ((base '(:top_p 0.9 :nested (:x 1) :keep nil))
         (overrides '("top_p" 0.8 :nested (:y [t :json-false nil])))
         (merged (ende-provider-config-merge-parameters base overrides))
         (json (ende-provider-config-parameters merged)))
    (should (= (gethash "top_p" json) 0.8))
    (should-not (gethash "x" (gethash "nested" json)))
    (should (equal (gethash "y" (gethash "nested" json))
                   (vector t json-false nil)))
    (should (equal base '(:top_p 0.9 :nested (:x 1) :keep nil)))
    (should-not (ende-provider-config-merge-parameters base nil))
    (should (equal (json-read-from-string (json-encode json))
                   '((keep) (top_p . 0.8) (nested (y . [t :json-false nil])))))))

(ert-deftest ende-provider-config-rejects-invalid-values ()
  (dolist (value (list '(:x 1 "x" 2) '(:x) '(:x . 1)
                       '(:x symbol) '(:x (:y 1 :y 2))
                       (list :x 1.0e+INF) (list :x 0.0e+NaN)
                       '(:nested (:api_key "not-a-real-key"))))
    (should-error (ende-provider-config-parameters value)))
  (let ((cyclic (list :x nil)))
    (setcar (cdr cyclic) cyclic)
    (should-error (ende-provider-config-parameters cyclic))
    (should-error (ende-provider-config-copy cyclic))))

(ert-deftest ende-provider-config-protection-and-detachment ()
  (dolist (parameters '((:model "m") (:reasoning (:summary "auto"))
                         (:reasoning nil)))
    (should-error
     (ende-provider-config-parameters
      parameters '(("model") ("reasoning" "effort")))))
  (let* ((source (list :string (copy-sequence "abc")
                       :array (vector (copy-sequence "xyz"))
                       :object (make-hash-table :test #'equal)))
         (copy (ende-provider-config-copy source)))
    (aset (plist-get copy :string) 0 ?X)
    (aset (aref (plist-get copy :array) 0) 0 ?X)
    (puthash "new" t (plist-get copy :object))
    (should (equal (plist-get source :string) "abc"))
    (should (equal (plist-get source :array) ["xyz"]))
    (should (zerop (hash-table-count (plist-get source :object))))))

(provide 'ende-provider-config-test)
;;; ende-provider-config-test.el ends here
