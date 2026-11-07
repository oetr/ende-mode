;;; ende-search-benchmark.el --- Standalone project search benchmark -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'ende-tools)

(defvar ende-search-benchmark-sizes '(100 1000 10000)
  "Numbers of text blocks per fixture.")
(defvar ende-search-benchmark-samples 5
  "Uninstrumented samples per workload, layer and cache state.")
(defvar ende-search-benchmark-instrument nil
  "Also collect separate, untimed direct-call counters.")
(defvar ende-search-benchmark-timeout 30
  "Maximum seconds to wait for each worker callback.")
(defvar ende-search-benchmark--counters nil)

(defun ende-search-benchmark--fixture (root size large)
  "Write deterministic ROOT fixture of SIZE blocks; group blocks when LARGE.
Return an independent in-memory manifest of paths and block counts."
  (make-directory (expand-file-name "data" root))
  (let ((remaining size) (index 0) manifest)
    (while (> remaining 0)
      (let* ((blocks (min remaining (if large 100 1)))
             (path (format "data/%06d.txt" index))
             (last (= blocks remaining)))
        (with-temp-file (expand-file-name path root)
          (set-buffer-file-coding-system 'utf-8-unix)
          (dotimes (_ blocks) (insert "préface\nα needle\nbridge\nomega\n"))
          (when last (insert "rare-final\n")))
        (push (list path blocks last) manifest)
        (cl-incf index)
        (cl-decf remaining blocks)))
    (dotimes (index (max 1 (/ size 10)))
      (dolist (pattern '("data/hidden%06d.txt" "denied%06d.bin" "excluded/secret%06d.txt"))
        (let ((path (expand-file-name (format pattern index) root)))
          (make-directory (file-name-directory path) t)
          (with-temp-file path (insert "SENTINEL needle rare-final\n")))))
    (nreverse manifest)))

(defun ende-search-benchmark--configuration (root)
  "Return a resolved-style, restricted file policy for ROOT."
  (list :root root :active-tools '(search_project)
        :tool-resources '((search_project project) (list_project_files project))
        :resources '((project :type file :include-regexps ("\\.txt\\'")
                              :exclude-regexps ("hidden[^/]*\\.txt\\'")))))

(defun ende-search-benchmark--workloads ()
  "Return the bounded representative matrix."
  '((exact ("query" . "needle") ("glob" . "data/000000.txt"))
    (narrow ("query" . "needle") ("glob" . "data/000000*.txt"))
    (absent ("query" . "SENTINEL"))
    (rare ("query" . "rare-final"))
    (dense ("query" . "needle") ("max_results" . 3))
    (multi ("patterns" . ["needle" "omega"]) ("glob" . "data/000000.txt"))
    (multiline ("regexp" . "needle\nbridge") ("glob" . "data/000000.txt"))
    (context ("query" . "needle") ("glob" . "data/000000.txt")
             ("context_before" . 1) ("context_after" . 1))
    (byte-cap ("query" . "needle") ("context_before" . 1) ("context_after" . 1))))

(defun ende-search-benchmark--oracle (manifest workload)
  "Project expected results from MANIFEST and WORKLOAD, without searching."
  (let ((kind (car workload)) (count 0) files)
    (dolist (entry manifest)
      (pcase-let ((`(,path ,blocks ,last) entry))
        (when (or (memq kind '(absent rare dense byte-cap))
                  (equal path "data/000000.txt"))
          (let (matches)
            (dotimes (block blocks)
              (when (and (not (memq kind '(absent rare byte-cap)))
                         (or (not (eq kind 'dense)) (< count 3)))
                (push (append (list :line (+ 2 (* block 4)) :column 2 :match "α needle")
                              (when (eq kind 'context)
                                (list :context "préface\nα needle\nbridge"))) matches)
                (cl-incf count)
                (when (eq kind 'multi)
                  (push (list :line (+ 4 (* block 4)) :column 0 :match "omega") matches))))
            (when (and last (eq kind 'rare))
              (push (list :line (1+ (* blocks 4)) :column 0 :match "rare-final") matches))
            (when matches (push (list :path path :matches (nreverse matches)) files))))))
    (list :files (nreverse files)
          :truncated (if (or (eq kind 'byte-cap)
                             (and (eq kind 'dense)
                                  (>= (apply #'+ (mapcar #'cadr manifest)) 3)))
                         t :json-false))))

(defun ende-search-benchmark--decode (string)
  "Decode JSON STRING consistently across benchmark layers."
  (json-parse-string string :object-type 'plist :array-type 'list
                     :false-object :json-false :null-object nil))

(defun ende-search-benchmark--validate (value expected)
  "Fail closed unless VALUE's complete semantic projection equals EXPECTED."
  (let ((projection
         (list :files
               (mapcar (lambda (file)
                         (list :path (plist-get file :path)
                               :matches (plist-get file :matches)))
                       (plist-get value :files))
               :truncated (plist-get value :truncated))))
    (unless (equal projection expected)
      (error "Search benchmark validation failed: expected %S, got %S" expected projection))
    (unless (natnump (plist-get value :files_inspected))
      (error "Search benchmark invalid inspection count"))))

(defun ende-search-benchmark--invoke (layer arguments root configuration cache)
  "Invoke LAYER with ARGUMENTS under CONFIGURATION and shared CACHE."
  (let* ((ende-tools--access-config configuration)
         (ende-tools--access-tool 'search_project)
         (ende-tools--project-cache cache)
         (ende-tools--project-cache-delta nil)
         (ende-tools--project-cache-room
          (- ende-project-cache-limit (hash-table-count (ende-project-cache-allowed cache))))
         (ende-tools--commit-project-cache
          (lambda (key delta)
            (ende-project-cache-context cache key)
            (ende-project-cache-merge cache delta))))
    (if (eq layer 'direct)
        (let ((ende-tools--project-context
               (or ende-tools--project-context
                   (ende-tools--prepare-project-context arguments root))))
          (ende-project-cache-context cache (plist-get ende-tools--project-context :key))
          (ende-tools--search-project arguments root))
      (let* ((done nil) result cancel
             (deadline (+ (float-time) ende-search-benchmark-timeout))
             (ende-tools--register-cancellation-function
              (lambda (function) (setq cancel function))))
        (unwind-protect
            (progn
              (ende-tools-call-async
               (list :id "benchmark" :name "search_project" :arguments (json-encode arguments))
               configuration (lambda (json) (setq result json done t)))
              (while (and (not done) (< (float-time) deadline))
                (accept-process-output nil 0.01))
              (unless done (error "Search benchmark worker timed out"))
              result)
          (when cancel (funcall cancel)))))))

(defun ende-search-benchmark--result (raw layer)
  "Decode RAW from LAYER outside the measurement window."
  (if (eq layer 'direct)
      (ende-search-benchmark--decode (ende-tools--json-encode-tool-result raw))
    (let ((reply (ende-search-benchmark--decode raw)))
      (unless (eq (plist-get reply :ok) t)
        (error "Search benchmark worker failed: %S" reply))
      (plist-get reply :result))))

(defun ende-search-benchmark--count (name original &rest arguments)
  "Count NAME and forward ARGUMENTS to ORIGINAL in a separate diagnostic run."
  (let ((result (apply original arguments)))
    (cl-incf (alist-get name ende-search-benchmark--counters 0))
    (when (eq name 'insert-file-contents)
      (cl-incf (alist-get 'read-bytes ende-search-benchmark--counters 0)
               (string-bytes (buffer-string))))
    result))

(defun ende-search-benchmark--instrument (function)
  "Call FUNCTION with temporary direct-only filesystem counters."
  (let (wrappers (ende-search-benchmark--counters
                  '((file-truename . 0) (file-attributes . 0)
                    (insert-file-contents . 0) (read-bytes . 0)
                    (ende-tools--matching-path-scopes . 0))))
    (setq ende-search-benchmark--counters (copy-tree ende-search-benchmark--counters))
    (unwind-protect
        (progn
          (dolist (name '(file-truename file-attributes insert-file-contents
                         ende-tools--matching-path-scopes))
            (let ((wrapper (apply-partially #'ende-search-benchmark--count name)))
              (push (cons name wrapper) wrappers)
              (advice-add name :around wrapper)))
          (funcall function)
          (mapcar (lambda (entry) (cons (symbol-name (car entry)) (cdr entry)))
                  ende-search-benchmark--counters))
      (dolist (entry wrappers) (advice-remove (car entry) (cdr entry))))))

(defun ende-search-benchmark--summary (samples field)
  "Return median and nearest-rank p95 for FIELD in SAMPLES."
  (let* ((values (sort (mapcar (lambda (sample) (plist-get sample field)) samples) #'<))
         (n (length values)))
    (list :median (if (cl-oddp n) (nth (/ n 2) values)
                    (/ (+ (nth (1- (/ n 2)) values) (nth (/ n 2) values)) 2.0))
          :p95 (nth (1- (ceiling (* 0.95 n))) values))))

(defun ende-search-benchmark--case (root configuration manifest workload layer state)
  "Measure one WORKLOAD, LAYER and cache STATE in ROOT."
  (let* ((arguments (if (assoc "max_results" (cdr workload)) (cdr workload)
                      (append (cdr workload) '(("max_results" . 20000)))))
         (expected (ende-search-benchmark--oracle manifest workload))
         (cache (ende-project-cache-create))
         (ende-tools-max-search-output-bytes (if (eq (car workload) 'byte-cap) 80 10000000))
         (ende-tools--access-config configuration)
         (ende-tools--access-tool 'search_project)
         (ende-tools--project-context
          (when (eq layer 'direct) (ende-tools--prepare-project-context arguments root)))
         samples counters)
    (when (eq state 'prewarmed)
      (ende-search-benchmark--validate
       (ende-search-benchmark--result
        (ende-search-benchmark--invoke layer arguments root configuration cache) layer) expected))
    (dotimes (_ ende-search-benchmark-samples)
      (when (eq state 'fresh) (setq cache (ende-project-cache-create)))
      (garbage-collect)
      (let* ((lag 0.0) (next (+ (float-time) 0.01))
             (timer (when (eq layer 'worker-json)
                      (run-at-time 0.01 0.01
                                   (lambda ()
                                     (let ((now (float-time)))
                                       (setq lag (max lag (- now next))
                                             next (+ now 0.01)))))))
             raw sample)
        (unwind-protect
            (let ((gc-before gcs-done) (gc-time-before gc-elapsed)
                  (start (float-time)))
              (setq raw (ende-search-benchmark--invoke layer arguments root configuration cache)
                    sample (list :wall (- (float-time) start)
                                 :gc_count (- gcs-done gc-before)
                                 :gc_seconds (- gc-elapsed gc-time-before)
                                 :parent_timer_max_lag lag)))
          (when timer (cancel-timer timer)))
        (ende-search-benchmark--validate (ende-search-benchmark--result raw layer) expected)
        (push sample samples)))
    (when (and ende-search-benchmark-instrument (eq layer 'direct))
      (when (eq state 'fresh) (setq cache (ende-project-cache-create)))
      (setq counters
            (ende-search-benchmark--instrument
             (lambda ()
               (ende-search-benchmark--validate
                (ende-search-benchmark--result
                 (ende-search-benchmark--invoke layer arguments root configuration cache) layer)
                expected)))))
    (list :workload (symbol-name (car workload)) :arguments arguments
          :output_byte_limit ende-tools-max-search-output-bytes
          :layer (symbol-name layer) :cache (symbol-name state)
          :samples (vconcat (nreverse samples))
          :wall (ende-search-benchmark--summary samples :wall)
          :gc_count (ende-search-benchmark--summary samples :gc_count)
          :gc_seconds (ende-search-benchmark--summary samples :gc_seconds)
          :parent_timer_max_lag (ende-search-benchmark--summary samples :parent_timer_max_lag)
          :instrumentation counters)))

;;;###autoload
(defun ende-search-benchmark-run ()
  "Return a validated JSON-serializable search benchmark report.
In batch mode also print JSON.  Configure `ende-search-benchmark-sizes',
`ende-search-benchmark-samples' and `ende-search-benchmark-instrument' before
calling.  Fresh refers to policy cache, never to a cold operating-system cache."
  (interactive)
  (unless (and (consp ende-search-benchmark-sizes)
               (cl-every (lambda (n) (and (integerp n) (> n 0))) ende-search-benchmark-sizes)
               (integerp ende-search-benchmark-samples) (> ende-search-benchmark-samples 0)
               (numberp ende-search-benchmark-timeout) (> ende-search-benchmark-timeout 0))
    (error "Search benchmark sizes and samples must be positive integers"))
  (let ((gc-cons-threshold 800000)
        (gc-cons-percentage 0.1)
        (case-fold-search nil)
        (ende-project-excluded-path-patterns '("\\`excluded/"))
        (ende-project-max-file-bytes 10000000)
        (ende-tools-max-search-files 1000000)
        (ende-tools-max-search-results 20000)
        (ende-tools-max-search-context-lines 20)
        fixtures)
    (dolist (size ende-search-benchmark-sizes)
      (dolist (large '(nil t))
        (let ((root (file-name-as-directory (make-temp-file "ende-search-benchmark-" t))))
          (unwind-protect
              (let* ((manifest (ende-search-benchmark--fixture root size large))
                     (configuration (ende-search-benchmark--configuration root)) cases)
                (dolist (workload (ende-search-benchmark--workloads))
                  (dolist (layer '(direct worker-json))
                    (dolist (state '(fresh prewarmed))
                      (push (ende-search-benchmark--case root configuration manifest workload layer state) cases))))
                (push (list :blocks size :layout (if large "fewer-large" "many-small")
                            :blocks_per_file (if large 100 1)
                            :disallowed_files (* 3 (max 1 (/ size 10)))
                            :allowed_files (length manifest)
                            :allowed_bytes (+ (* size (string-bytes "préface\nα needle\nbridge\nomega\n"))
                                              (string-bytes "rare-final\n"))
                            :cases (vconcat (nreverse cases))) fixtures))
            (delete-directory root t)))))
    (let ((report
           (list :environment
                 (list :emacs emacs-version :system system-configuration
                       :system_type (symbol-name system-type) :ende (or (bound-and-true-p ende-version) "unknown")
                       :tools_library (locate-library "ende-tools")
                       :tools_sha256 (with-temp-buffer
                                       (insert-file-contents-literally (locate-library "ende-tools"))
                                       (secure-hash 'sha256 (current-buffer)))
                       :benchmark_compiled (byte-code-function-p (symbol-function 'ende-search-benchmark-run))
                       :native_compilation (if (and (fboundp 'native-comp-available-p)
                                                   (native-comp-available-p)) t :json-false))
                 :settings (list :sizes (vconcat ende-search-benchmark-sizes)
                                 :samples ende-search-benchmark-samples :case_fold :json-false
                                 :parent_gc_threshold gc-cons-threshold
                                 :parent_gc_percentage gc-cons-percentage
                                 :max_file_bytes ende-project-max-file-bytes
                                 :max_search_files ende-tools-max-search-files
                                 :max_results ende-tools-max-search-results
                                 :context_limit ende-tools-max-search-context-lines
                                 :cache_limit ende-project-cache-limit
                                 :policy "include .txt; deny hidden*.txt; exclude excluded/"
                                 :os_cache "uncontrolled, fixtures recently written; fresh means policy cache only"
                                 :gc_scope "parent process only; worker GC excluded"
                                 :timing "direct: prepared policy; worker-json: dispatch through JSON callback"
                                 :timer "worker-json only, 10ms repeating parent timer"
                                 :units "wall, gc_seconds, parent_timer_max_lag: seconds"
                                 :instrumentation "separate direct-only run; read-bytes counts decoded buffer bytes"
                                 :worker_timeout ende-search-benchmark-timeout)
                 :fixtures (vconcat (nreverse fixtures)))))
      (when noninteractive (princ (concat (json-encode report) "\n")))
      report)))

(provide 'ende-search-benchmark)
;;; ende-search-benchmark.el ends here
