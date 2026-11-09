;;; ende-websocket-autobahn.el --- Autobahn server runner -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'ende-websocket)

(defconst ende-websocket-autobahn--source
  (expand-file-name "../lisp/ende-websocket.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defconst ende-websocket-autobahn--groups
  '(("core" :cases ["1.*" "2.*" "3.*" "4.*" "5.*" "6.*" "7.*"]
     :count 246 :pattern "\\`[1-7]\\."
     :behavior ("OK" "NON-STRICT" "INFORMATIONAL")
     :close ("OK" "INFORMATIONAL"))
    ("perf" :cases ["9.*" "10.*"] :count 55 :pattern "\\`\\(?:9\\|10\\)\\."
     :behavior ("OK") :close ("OK")
     :limits (:frame-bytes 67108864 :message-bytes 67108864
              :write-message-bytes 67108864
              :input-bytes 134217728 :output-bytes 134217728
              :fragment-count 1000000 :message-timeout 120 :closing-timeout 3))))

(defun ende-websocket-autobahn--environment (name)
  "Return the required nonempty environment variable NAME."
  (let ((value (getenv name)))
    (unless (and value (not (string-empty-p value)))
      (error "Set %s before running Autobahn" name))
    value))

(defun ende-websocket-autobahn--log (directory format-string &rest arguments)
  "Append a formatted diagnostic to DIRECTORY's server log."
  (write-region (concat (apply #'format format-string arguments) "\n")
                nil (expand-file-name "server.log" directory) t 'silent))

(defun ende-websocket-autobahn--check-reports (directory group settings)
  "Check GROUP's reports in DIRECTORY against SETTINGS and save a summary."
  (let* ((reports (expand-file-name "reports" directory))
         (index (with-temp-buffer
                  (insert-file-contents (expand-file-name "index.json" reports))
                  (json-parse-buffer :object-type 'hash-table)))
         (cases (gethash (concat "ende-server-" group) index))
         behaviors closes problems)
    (unless (and (= (hash-table-count index) 1) (hash-table-p cases))
      (error "Missing or unexpected Autobahn server results"))
    (unless (= (hash-table-count cases) (plist-get settings :count))
      (push (format "Expected %d cases, received %d"
                    (plist-get settings :count) (hash-table-count cases)) problems))
    (maphash
     (lambda (id result)
       (let ((behavior (gethash "behavior" result))
             (close (gethash "behaviorClose" result))
             (report (gethash "reportfile" result)))
         (cl-incf (alist-get behavior behaviors 0 nil #'equal))
         (cl-incf (alist-get close closes 0 nil #'equal))
         (unless (and (string-match-p (plist-get settings :pattern) id)
                      (member behavior (plist-get settings :behavior))
                      (member close (plist-get settings :close))
                      (stringp report)
                      (file-regular-p (expand-file-name report reports)))
           (push (format "%s: behavior=%S behaviorClose=%S report=%S"
                         id behavior close report) problems))))
     cases)
    (let ((summary (format "%s: %d/%d cases\nbehavior: %S\nbehaviorClose: %S\n%s"
                           group (hash-table-count cases) (plist-get settings :count)
                           behaviors closes
                           (mapconcat (lambda (problem) (concat problem "\n"))
                                      (nreverse problems) ""))))
      (write-region summary nil (expand-file-name "summary.txt" directory) nil 'silent)
      (princ summary))
    (when problems (error "Autobahn reports failed or incomplete cases"))))

(defun ende-websocket-autobahn--run (directory group settings image container)
  "Run GROUP using IMAGE and CONTAINER, writing reports to DIRECTORY."
  (let* ((config (expand-file-name "config" directory))
         (reports (expand-file-name "reports" directory))
         (started (float-time))
         listener process port)
    (when (file-exists-p reports)
      (error "Use a fresh Autobahn report directory: %s" directory))
    (make-directory config t)
    (make-directory reports)
    (unwind-protect
        (progn
          (setq listener
                (ende-websocket-listen
                 :host "127.0.0.1" :service t :limits (plist-get settings :limits)
                 :admit (lambda (_ request)
                          (when (and (equal (plist-get request :target) "/autobahn")
                                     (equal (plist-get request :authority)
                                            (format "127.0.0.1:%d" port))
                                     (equal (plist-get request :origin) "http://127.0.0.1"))
                            '(:accept t)))
                 :on-message
                 (lambda (connection type message)
                   (condition-case err
                       (if (eq type 'text)
                           (ende-websocket-send-text connection message)
                         (ende-websocket-send-binary connection message))
                     (error
                      (ende-websocket-autobahn--log directory "Echo error: %S" err)
                      (signal (car err) (cdr err)))))
                 :on-close (lambda (_ info)
                             (ende-websocket-autobahn--log directory "Close: %S" info))))
          (setq port (ende-websocket-listener-port listener))
          (with-temp-file (expand-file-name "fuzzingclient.json" config)
            (insert
             (json-encode
              `((outdir . "/reports")
                (servers . [((agent . ,(concat "ende-server-" group))
                             (url . ,(format "ws://127.0.0.1:%d/autobahn" port))
                             (origin . "http://127.0.0.1"))])
                (cases . ,(plist-get settings :cases)) (exclude-cases . [])))))
          (ende-websocket-autobahn--log
           directory "Start: Emacs %s; source=%s; group=%s; port=%d; limits=%S"
           emacs-version ende-websocket-autobahn--source group port
           (ende-websocket--listener-limits listener))
          (setq process
                (make-process
                 :name "ende-autobahn" :connection-type 'pipe :noquery t
                 :sentinel #'ignore
                 :filter (lambda (_ text)
                           (write-region text nil (expand-file-name "runner.log" directory) t 'silent))
                 :command
                 (list "docker" "run" "--rm" "--name" container
                       "--platform" "linux/amd64" "--network" "host"
                       "--user" (format "%d:%d" (user-uid) (group-gid))
                       "--cap-drop" "ALL" "--security-opt" "no-new-privileges=true"
                       "--env" "PYTHONUNBUFFERED=1" "--env" "PYTHONDONTWRITEBYTECODE=1"
                       "--mount" (format "type=bind,src=%s,dst=/config,readonly" config)
                       "--mount" (format "type=bind,src=%s,dst=/reports" reports)
                       "--entrypoint" "wstest" image
                       "--mode" "fuzzingclient" "--spec" "/config/fuzzingclient.json")))
          (while (and (process-live-p process) (< (- (float-time) started) 1320))
            (accept-process-output nil 0.05))
          (when (process-live-p process) (error "Autobahn exceeded its 22-minute deadline"))
          (unless (= (process-exit-status process) 0)
            (error "Autobahn exited %d; see runner.log" (process-exit-status process)))
          (ende-websocket-autobahn--check-reports directory group settings))
      (when (and process (process-live-p process)) (delete-process process))
      (when process
        (ignore-errors (process-file "docker" nil nil nil "rm" "--force" container)))
      (ende-websocket-shutdown)
      (ende-websocket-autobahn--log
       directory "Cleanup: connections=%d listeners=%d elapsed=%.1fs"
       (hash-table-count ende-websocket--connections)
       (hash-table-count ende-websocket--listeners) (- (float-time) started)))))

(defun ende-websocket-autobahn-run ()
  "Run the configured Autobahn server group and exit with its CI status."
  (condition-case err
      (let* ((group (ende-websocket-autobahn--environment "AUTOBAHN_GROUP"))
             (settings (cdr (assoc group ende-websocket-autobahn--groups)))
             (directory (expand-file-name
                         (ende-websocket-autobahn--environment "AUTOBAHN_DIRECTORY")))
             (image (ende-websocket-autobahn--environment "AUTOBAHN_IMAGE"))
             (container (ende-websocket-autobahn--environment "AUTOBAHN_CONTAINER")))
        (unless settings (error "Unknown Autobahn group: %s" group))
        (load ende-websocket-autobahn--source nil t t)
        (ende-websocket-autobahn--run directory group settings image container)
        (kill-emacs 0))
    ((error quit)
     (message "Autobahn failed: %s" (error-message-string err))
     (kill-emacs 1))))

(provide 'ende-websocket-autobahn)
;;; ende-websocket-autobahn.el ends here
