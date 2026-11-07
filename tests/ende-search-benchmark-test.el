;;; ende-search-benchmark-test.el --- Search benchmark checks -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path
             (expand-file-name "../benchmark" (file-name-directory
                                              (or load-file-name buffer-file-name))))
(require 'ende-search-benchmark)

(ert-deftest ende-search-benchmark-end-to-end ()
  (let ((ende-search-benchmark-sizes '(4))
        (ende-search-benchmark-samples 2)
        (ende-search-benchmark-instrument t)
        (original (symbol-function 'make-temp-file)) roots report)
    (cl-letf (((symbol-function 'make-temp-file)
               (lambda (prefix &rest arguments)
                 (let ((path (apply original prefix arguments)))
                   (when (string-prefix-p "ende-search-benchmark-" prefix) (push path roots))
                   path))))
      (with-temp-buffer
        (let ((standard-output (current-buffer)))
          (setq report (ende-search-benchmark-run))
          (should (equal (string-trim (buffer-string)) (json-encode report))))))
    (should (= (length roots) 2))
    (should-not (cl-some #'file-exists-p roots))
    (should (= (length (plist-get report :fixtures)) 2))
    (seq-doseq (fixture (plist-get report :fixtures))
      (let ((cases (plist-get fixture :cases)))
        (should (> (length cases) 0))
        (should (member "direct" (mapcar (lambda (c) (plist-get c :layer)) cases)))
        (should (member "worker-json" (mapcar (lambda (c) (plist-get c :layer)) cases)))
        (seq-doseq (case cases)
          (dolist (field '(:wall :gc_count :gc_seconds :parent_timer_max_lag))
            (let* ((raw (mapcar (lambda (sample) (plist-get sample field))
                                (plist-get case :samples)))
                   (summary (plist-get case field)))
              (should (= (length raw) ende-search-benchmark-samples))
              (should (= (plist-get summary :median) (/ (apply #'+ raw) 2.0)))
              (should (= (plist-get summary :p95) (apply #'max raw)))))
          (seq-doseq (sample (plist-get case :samples))
            (dolist (field '(:wall :gc_count :gc_seconds :parent_timer_max_lag
                             :result_bytes :files_inspected :matching_lines))
              (should (numberp (plist-get sample field)))
              (should (>= (plist-get sample field) 0)))))))))

(ert-deftest ende-search-benchmark-validation-fails-closed ()
  (let ((ende-search-benchmark-sizes '(1))
        (ende-search-benchmark-samples 1)
        (original (symbol-function 'make-temp-file)) roots)
    (cl-letf (((symbol-function 'make-temp-file)
               (lambda (prefix &rest arguments)
                 (let ((path (apply original prefix arguments)))
                   (when (string-prefix-p "ende-search-benchmark-" prefix) (push path roots))
                   path)))
              ((symbol-function 'ende-tools--search-project)
               (lambda (&rest _) (list :files [] :files_inspected 0 :truncated nil))))
      (with-temp-buffer
        (let ((standard-output (current-buffer)))
          (should (string-match-p
                   "validation failed"
                   (error-message-string (should-error (ende-search-benchmark-run)))))
          (should (string-empty-p (buffer-string))))))
    (should roots)
    (should-not (cl-some #'file-exists-p roots))))

(provide 'ende-search-benchmark-test)
;;; ende-search-benchmark-test.el ends here
