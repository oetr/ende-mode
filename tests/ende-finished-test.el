;;; ende-finished-test.el --- Finished-job navigation tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'ende)
(require 'ende-test-helpers)

(defconst ende-finished-test--profile
  '(:name remote :model "finished-test" :max-concurrency 1 :parallel nil))

(defun ende-finished-test--enqueue (buffer operation &optional instruction)
  "Enqueue OPERATION targeting BUFFER with optional INSTRUCTION."
  (with-current-buffer buffer
    (let ((position (point)))
      (ende-enqueue
       (list operation
             :target (ende-target-create
                      :buffer buffer :start position :end position
                      :cursor position :review-policy 'ignore)
             :profile ende-finished-test--profile
             :instruction instruction)))))

(ert-deftest ende-test/finished-jobs-track-terminal-snapshots-and-bound-retention ()
  (ende-test--reset)
  (let ((ende-finished-limit 2)
        (ende-finished-jobs nil)
        (ende-finished--live-jobs (make-hash-table :test #'eql))
        (source (generate-new-buffer " *ende-finished-lifecycle*"))
        active)
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert "* Source heading\nbody\n")
            (org-mode)
            (goto-char (point-max)))
          (ende-register-job-handler
           'finished-test (lambda (job) (setq active job)))
          (dolist (_ '(1 2 3))
            (let ((job (ende-finished-test--enqueue source 'finished-test)))
              (should (eq job active))
              (ende-job-finish job "done")))
          (should (= (length ende-finished-jobs) 2))
          (should (= (ende-finished-job-id (car ende-finished-jobs))
                     (ende-job-id active)))
          (should (cl-every (lambda (entry)
                             (and (ende-finished-job-p entry)
                                  (not (ende-job-p entry))))
                           ende-finished-jobs))
          (should (equal (ende-finished-job-title (car ende-finished-jobs))
                         "Source heading"))
          (should (integerp (ende-finished-job-line (car ende-finished-jobs))))
          (with-current-buffer source
            (goto-char (point-min)))
          (let ((cancelled (ende-finished-test--enqueue
                            source 'finished-test)))
            (let ((buffer-name (buffer-name source)))
              (kill-buffer source)
              (setq source nil)
              (let ((snapshot
                     (cl-find (ende-job-id cancelled) ende-finished-jobs
                              :key #'ende-finished-job-id)))
                (should snapshot)
                (should (eq (ende-finished-job-state snapshot) 'cancelled))
                (should (equal (ende-finished-job-buffer-name snapshot)
                               buffer-name))
                (should (integerp (ende-finished-job-line snapshot)))
                (should (ende-finished-job-p snapshot))))))
      (when (buffer-live-p source)
        (kill-buffer source))
      (setq ende--job-handlers
            (assq-delete-all 'finished-test ende--job-handlers))
      (ende-test--reset))))

(defun ende-finished-test--entry (buffer file)
  "Make one navigation fixture for BUFFER and FILE."
  (ende-finished--make-job
   :id 9001 :operation 'chat :state 'completed :finished-at (float-time)
   :buffer buffer :buffer-name (buffer-name buffer) :file file
   :line 6 :title "Source heading" :request-id "request-9001"))

(ert-deftest ende-test/finished-job-navigation-degrades-through-anchors ()
  (let* ((file (make-temp-file "ende-finished-nav-" nil ".org"))
         (buffer nil)
         (entry nil)
         (finished-buffer (get-buffer-create ende-finished-buffer-name))
         (text (concat
                "* Source heading\n"
                "one\ntwo\nthree\nfour\nfive\n"
                "** Agent: Work\n"
                ":PROPERTIES:\n:ENDE_CHAT_WORK: t\n"
                ":ENDE_CHAT_REQUEST_ID: request-9001\n:END:\n"
                "*** Thinking\n"
                ":PROPERTIES:\n:ENDE_CHAT_REQUEST_ID: request-9001\n"
                ":END:\nnot the reply\n"
                "** Agent: Reply\n"
                ":PROPERTIES:\n:ENDE_ROLE: assistant\n"
                ":ENDE_CHAT_REQUEST_ID: request-9001\n:END:\nanswer\n")))
    (unwind-protect
        (progn
          (with-temp-file file (insert text))
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (org-mode)
            (save-buffer))
          (setq entry (ende-finished-test--entry buffer file))
          (with-current-buffer buffer
            (ende-finished-visit-job entry)
            (should (equal (org-get-heading t t t t) "Agent: Reply"))
            (should-not (equal (org-get-heading t t t t) "Thinking"))
            (goto-char (point-min))
            (re-search-forward "^\\*\\* Agent: Reply$")
            (delete-region (line-beginning-position) (point-max))
            (insert "\n")
            (ende-finished-visit-job entry)
            (should (equal (org-get-heading t t t t) "Source heading"))
            (goto-char (point-min))
            (re-search-forward "^\\* Source heading$")
            (replace-match "* Renamed heading")
            (goto-char (point-min))
            (forward-line 5)
            (let ((expected (point)))
              (ende-finished-visit-job entry)
              (should (= (point) expected)))
            (erase-buffer)
            (insert "short file\n")
            (goto-char (point-min))
            (ende-finished-visit-job entry)
            (should (= (point) (point-min)))
            (save-buffer))
          (kill-buffer buffer)
          (setq buffer nil)
          (ende-finished-visit-job entry)
          (setq buffer (get-file-buffer file))
          (should (buffer-live-p buffer))
          (with-current-buffer buffer
            (should (equal (buffer-string) "short file\n")))
          (kill-buffer buffer)
          (setq buffer nil)
          (delete-file file)
          (let ((messages nil)
                (current-buffer-before (current-buffer)))
            (cl-letf (((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (push (apply #'format format-string args) messages))))
              (ende-finished-visit-job entry))
            (should (string-match-p "source file no longer exists"
                                    (car messages)))
            (should (eq (current-buffer) current-buffer-before)))
          (with-current-buffer finished-buffer
            (ende-finished-mode)
            (setq ende-finished-jobs (list entry))
            (tabulated-list-print t)
            (should (string-match-p "9001" (buffer-string)))
            (let ((visited nil))
              (cl-letf (((symbol-function 'ende-finished--resolve-buffer)
                         (lambda (job)
                           (setq visited (ende-finished-job-id job))
                           nil))
                        ((symbol-function 'tabulated-list-get-id)
                         (lambda () 9001)))
                (call-interactively
                 (key-binding (kbd "RET"))))
              (should (= visited 9001)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (when (file-exists-p file) (delete-file file))
      (when (buffer-live-p finished-buffer) (kill-buffer finished-buffer)))))

(ert-deftest ende-test/finished-selector-entry-is-advertised-and-opens-list ()
  (let ((closed nil)
        (opened nil))
    (with-temp-buffer
      (ende-selector-mode)
      (setq ende-selector--providers '(remote)
            ende-selector--provider-index 0
            ende-selector--models '("model")
            ende-selector--model-index 0
            ende-selector--model-state :ready)
      (ende-selector--render)
      (should (string-match-p "f  - finished jobs" (buffer-string)))
      (should (eq (key-binding (kbd "f"))
                  #'ende-selector-finished-jobs))
      (cl-letf (((symbol-function 'ende-selector-close)
                 (lambda () (setq closed t)))
                ((symbol-function 'ende-finished-open)
                 (lambda () (setq opened t))))
        (call-interactively (key-binding (kbd "f")))))
    (should closed)
    (should opened)))

(ert-deftest ende-test/finished-module-reloads-before-selector-with-one-observer ()
  (let* ((directory (file-name-directory (locate-library "ende.el")))
         (files (ende-reload--files directory)))
    (should (< (cl-position "ende-finished.el" files :test #'equal)
               (cl-position "ende-selector.el" files :test #'equal)))
    (cl-letf (((symbol-function 'message) #'ignore))
      (ende-reload))
    (should (featurep 'ende-finished))
    (should (eq (lookup-key ende-selector-mode-map (kbd "f"))
                #'ende-selector-finished-jobs))
    (should (= (cl-count #'ende-finished--job-event
                         ende-job-event-functions)
               1))))

(provide 'ende-finished-test)

;;; ende-finished-test.el ends here
