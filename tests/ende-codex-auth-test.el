;;; ende-codex-auth-test.el --- Private Codex account tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter Samarin
;;
;; Author: Peter Samarin <peter.samarin@gmail.com>
;; Maintainer: Peter Samarin <peter.samarin@gmail.com>
;; Keywords: lisp, tools
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

;; Tests for private Codex accounts.

;;; Code:

(require 'ert)
(require 'ende-codex-auth)
(require 'ende-test-helpers)

(defmacro ende-codex-auth-test--with-store (&rest body)
  "Run BODY with an isolated private credential store."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "ende-codex-accounts-" t))
          (ende-codex-auth--directory (expand-file-name "private" root)))
     (unwind-protect (progn ,@body)
       (delete-directory root t))))

(defun ende-codex-auth-test--credentials (&optional suffix)
  "Return synthetic credentials for a registration named SUFFIX."
  (let ((suffix (or suffix "one")))
    (list :client-id (concat "issued-client-" suffix)
          :subject (concat "validated-subject-" suffix)
          :access-token (concat "synthetic-access-" suffix)
          :refresh-token (concat "synthetic-refresh-" suffix)
          :id-token (concat "synthetic-id-" suffix)
          :scope (ende-codex--auth-property 'chatgpt-plan-oauth :scopes)
          :expires-at (+ (floor (float-time)) 3600))))

(ert-deftest ende-codex-auth-storage-independent-records-and-host ()
  (ende-codex-auth-test--with-store
   (let* ((personal (ende-codex-auth--register 'chatgpt-plan-oauth
                     'personal (ende-codex-auth-test--credentials)))
          (work (ende-codex-auth--register 'chatgpt-plan-oauth
                 'work (ende-codex-auth-test--credentials "two"))))
     (should (equal personal (ende-codex-auth-read 'chatgpt-plan-oauth 'personal)))
     (should (equal work (ende-codex-auth-read 'chatgpt-plan-oauth 'work)))
     (should (equal (plist-get personal :host-id) (plist-get work :host-id)))
     (should-not (equal (plist-get personal :registration-id)
                        (plist-get work :registration-id)))
     (should (= (logand (file-modes ende-codex-auth--directory) #o777) #o700))
     (should (= (logand (file-modes (expand-file-name "connections.json"
                                                      ende-codex-auth--directory))
                        #o777) #o600))
     ;; Reads are detached, and there is no persistent current-account field.
     (setf (plist-get personal :access-token) "mutated")
     (should-not (equal personal (ende-codex-auth-read 'chatgpt-plan-oauth 'personal)))
     (with-temp-buffer
       (insert-file-contents (expand-file-name "connections.json" ende-codex-auth--directory))
       (should-not (string-match-p "current" (buffer-string)))))))

(ert-deftest ende-codex-auth-storage-replacement-requires-explicit-cas ()
  (ende-codex-auth-test--with-store
   (let* ((first (ende-codex-auth--register 'chatgpt-plan-oauth 'personal (ende-codex-auth-test--credentials)))
          (other (ende-codex-auth-test--credentials "two")))
     (should-error (ende-codex-auth--register 'chatgpt-plan-oauth 'personal other)
                   :type 'ende-codex-auth-conflict)
     (should-error (ende-codex-auth--register 'chatgpt-plan-oauth 'personal other nil t)
                   :type 'ende-codex-auth-conflict)
     (let ((second (ende-codex-auth--register 'chatgpt-plan-oauth 'personal other (plist-get first :revision) t)))
       (should-not (ende-codex-auth--same-registration-p first second))
       (should-error (ende-codex-auth--register 'chatgpt-plan-oauth 'personal other (plist-get first :revision) t)
                     :type 'ende-codex-auth-conflict)
       (should (equal second (ende-codex-auth-read 'chatgpt-plan-oauth 'personal)))))))

(ert-deftest ende-codex-auth-storage-rotation-preserves-registration-not-revision ()
  (ende-codex-auth-test--with-store
   (let* ((credentials (ende-codex-auth-test--credentials))
          (personal (ende-codex-auth--register 'chatgpt-plan-oauth 'personal credentials))
          (work (ende-codex-auth--register 'chatgpt-plan-oauth 'work credentials)))
     ;; Identical subjects/client IDs still need independent registrations.
     (should-not (ende-codex-auth--same-registration-p personal work))
     (setq credentials (plist-put credentials :refresh-token "synthetic-rotated"))
     (let ((rotated (ende-codex-auth--rotate personal credentials)))
       (should (ende-codex-auth--same-registration-p personal rotated))
       (should-not (equal (plist-get personal :revision) (plist-get rotated :revision)))
       (should-error (ende-codex-auth--rotate personal credentials)
                     :type 'ende-codex-auth-conflict)
       (should (equal work (ende-codex-auth-read 'chatgpt-plan-oauth 'work)))
       (setq credentials (plist-put credentials :subject "another-identity"))
       (should-error (ende-codex-auth--rotate rotated credentials)
                     :type 'ende-codex-auth-conflict)))))

(ert-deftest ende-codex-auth-storage-rejects-remote-symlink-and-insecure-paths ()
  (ende-codex-auth-test--with-store
   (let ((ende-codex-auth--directory "/ssh:unreachable:/credentials/"))
     (should-error (ende-codex-auth-read 'chatgpt-plan-oauth 'personal) :type 'ende-codex-auth-error))
   (make-directory ende-codex-auth--directory)
   (set-file-modes ende-codex-auth--directory #o755)
   (should-error (ende-codex-auth--register 'chatgpt-plan-oauth 'personal (ende-codex-auth-test--credentials))
                 :type 'ende-codex-auth-error)
   (set-file-modes ende-codex-auth--directory #o700)
   (ende-codex-auth--register 'chatgpt-plan-oauth 'personal (ende-codex-auth-test--credentials))
   (let ((file (expand-file-name "connections.json" ende-codex-auth--directory)))
     (set-file-modes file #o644)
     (should-error (ende-codex-auth-read 'chatgpt-plan-oauth 'personal) :type 'ende-codex-auth-error)
     (set-file-modes file #o600)
     (rename-file file (concat file ".real"))
     (make-symbolic-link (concat file ".real") file)
     (should-error (ende-codex-auth-read 'chatgpt-plan-oauth 'personal) :type 'ende-codex-auth-error)
     (delete-file file)
     (make-symbolic-link (concat file ".missing") file)
     (should-error (ende-codex-auth-read 'chatgpt-plan-oauth 'personal) :type 'ende-codex-auth-error))
   (let ((link (expand-file-name "link" root)))
     (make-symbolic-link ende-codex-auth--directory link)
     (let ((ende-codex-auth--directory (expand-file-name "child/" link)))
       (should-error (ende-codex-auth--register 'chatgpt-plan-oauth 'personal (ende-codex-auth-test--credentials))
                     :type 'ende-codex-auth-error)))))

(ert-deftest ende-codex-auth-storage-rejects-hardlinks-and-untrusted-ancestors ()
  (ende-codex-auth-test--with-store
   (ende-codex-auth--register 'chatgpt-plan-oauth 'personal (ende-codex-auth-test--credentials))
   (let ((file (expand-file-name "connections.json" ende-codex-auth--directory)))
     (add-name-to-file file (concat file ".link"))
     (should-error (ende-codex-auth-read 'chatgpt-plan-oauth 'personal) :type 'ende-codex-auth-error)
     (delete-file (concat file ".link")))
   (set-file-modes root #o777)
   (unwind-protect
       (should-error (ende-codex-auth-read 'chatgpt-plan-oauth 'personal) :type 'ende-codex-auth-error)
     (set-file-modes root #o700))))

(ert-deftest ende-codex-auth-storage-atomic-failure-leaves-old-record ()
  (ende-codex-auth-test--with-store
   (let* ((credentials (ende-codex-auth-test--credentials))
          (old (ende-codex-auth--register 'chatgpt-plan-oauth 'personal credentials)))
     (cl-letf (((symbol-function 'rename-file) (lambda (&rest _) (error "write failed"))))
       (should-error (ende-codex-auth--rotate old credentials) :type 'ende-codex-auth-error))
     (should (equal old (ende-codex-auth-read 'chatgpt-plan-oauth 'personal)))
     (should-not (directory-files ende-codex-auth--directory nil "\\`\\.accounts-"))
     (should (file-exists-p (expand-file-name ".credentials.lock.sqlite" ende-codex-auth--directory))))))

(ert-deftest ende-codex-auth-storage-errors-never-print-credentials ()
  (ende-codex-auth-test--with-store
   (let* ((credentials (ende-codex-auth-test--credentials))
          (secret (plist-get credentials :access-token)))
     (setq credentials (plist-put credentials :expires-at secret))
     (let ((failure (should-error (ende-codex-auth--register 'chatgpt-plan-oauth 'personal credentials))))
       (should-not (string-match-p (regexp-quote secret) (prin1-to-string failure))))
     (should-not (custom-variable-p 'ende-codex-auth--directory))
     (should-not (file-exists-p ende-codex-auth--directory)))))

(ert-deftest ende-codex-auth-storage-malformed-json-and-wrong-owner-fail-closed ()
  (ende-codex-auth-test--with-store
   (ende-codex-auth--register 'chatgpt-plan-oauth 'personal (ende-codex-auth-test--credentials))
   (let ((file (expand-file-name "connections.json" ende-codex-auth--directory))
         (real-attributes (symbol-function 'file-attributes)))
     (cl-letf (((symbol-function 'file-attributes)
                (lambda (path &optional format)
                  (let ((attributes (funcall real-attributes path format)))
                    (when (and attributes (equal path file))
                      (setq attributes (copy-sequence attributes))
                      (setf (nth 2 attributes) (1+ (user-uid))))
                    attributes))))
       (should-error (ende-codex-auth-read 'chatgpt-plan-oauth 'personal) :type 'ende-codex-auth-error))
     (with-temp-file file (insert "{synthetic-private-json-secret"))
     (let ((failure (should-error (ende-codex-auth-read 'chatgpt-plan-oauth 'personal)
                                  :type 'ende-codex-auth-error)))
       (should-not (string-match-p "synthetic-private" (prin1-to-string failure)))))))

(defun ende-codex-auth-test--profile (account)
  "Return an internal request profile bound to Ende ACCOUNT."
  (ende-codex--account-profile
   (list :name 'codex-test :service 'openai-codex :account account
         :api 'openai-responses :auth-method 'chatgpt-plan-oauth :model "synthetic-model"
         :url "https://api.openai.com/v1/responses"
         :models-url "https://api.openai.com/v1/models")))

(ert-deftest ende-codex-auth-binding-two-queued-accounts-reject-replacement ()
  (ende-codex-auth-test--with-store
   (let* ((personal (ende-codex-auth--register 'chatgpt-plan-oauth 'personal (ende-codex-auth-test--credentials)))
          (work (ende-codex-auth--register 'chatgpt-plan-oauth 'work (ende-codex-auth-test--credentials "two")))
          (ende-credentials--bindings (make-hash-table :test #'eq)))
     (with-temp-buffer
       (let* ((target (ende-target-create :buffer (current-buffer) :start 1 :end 1 :cursor 1))
              (personal-job (ende--job-from-request
                             (list 'insert :target target
                                   :profile (ende-codex-auth-test--profile 'personal))))
              (work-job (ende--job-from-request
                         (list 'insert :target target
                               :profile (ende-codex-auth-test--profile 'work)))))
         (should-not (ende-codex--check-binding (ende-job-profile personal-job) personal))
         (should-not (ende-codex--check-binding (ende-job-profile work-job) work))
         (ende-codex-auth--register 'chatgpt-plan-oauth 'personal (ende-codex-auth-test--credentials "replacement")
                                    (plist-get personal :revision) t)
         (should-error (ende-codex--check-binding
                        (ende-job-profile personal-job) (ende-codex-auth-read 'chatgpt-plan-oauth 'personal)))
         (should-not (ende-codex--check-binding
                      (ende-job-profile work-job) (ende-codex-auth-read 'chatgpt-plan-oauth 'work)))
         (dolist (job (list personal-job work-job))
           (let ((printed (prin1-to-string (list (ende-job-profile job)
                                                 (ende-job-inference-template job)))))
             (dolist (secret '("synthetic-access" "synthetic-refresh" "synthetic-id"))
               (should-not (string-match-p secret printed)))))
         (ende-target-release target))))))

(ert-deftest ende-codex-auth-binding-rotation-survives-and-capture-stays-failed-closed ()
  (ende-codex-auth-test--with-store
   (let* ((credentials (ende-codex-auth-test--credentials))
          (record (ende-codex-auth--register 'chatgpt-plan-oauth 'personal credentials))
          (profile (ende-codex-capture-binding (ende-codex-auth-test--profile 'personal)))
          (missing (ende-codex-capture-binding (ende-codex-auth-test--profile 'missing))))
     (setq credentials (plist-put credentials :refresh-token "synthetic-rotation"))
     (should-not (ende-codex--check-binding profile (ende-codex-auth--rotate record credentials)))
     (let ((new (ende-codex-auth--register 'chatgpt-plan-oauth 'missing (ende-codex-auth-test--credentials))))
       (should-error (ende-codex--check-binding missing new))
       (should-error (ende-codex--check-binding (ende-codex-capture-binding missing) new)))
     ;; Changing an alias, store, or opaque binding cannot rebind an old job.
     (should-error (ende-codex--check-binding
                    (plist-put (copy-sequence profile) :account 'work)
                    (ende-codex-auth-read 'chatgpt-plan-oauth 'personal)))
     (should-error (ende-codex--check-binding
                    (plist-put (copy-sequence profile) :codex-binding (make-symbol "lost"))
                    (ende-codex-auth-read 'chatgpt-plan-oauth 'personal)))
     (let ((ende-codex-auth--directory (expand-file-name "another" root)))
       (should-error (ende-codex--check-binding profile record))))))

(ert-deftest ende-codex-auth-refresh-is-account-and-registration-scoped ()
  (ende-codex-auth-test--with-store
   (let* ((credentials (plist-put (ende-codex-auth-test--credentials) :expires-at 0))
          (personal (ende-codex-auth--register 'chatgpt-plan-oauth 'personal credentials))
          (personal-new (ende-codex-auth--rotate personal credentials))
          (work (ende-codex-auth--register 'chatgpt-plan-oauth 'work credentials))
          (ende-codex--refresh-flights (make-hash-table :test #'equal))
          flights cancellations)
     (cl-letf (((symbol-function 'ende-codex--refresh-flight-begin)
                (lambda (flight) (push flight flights))))
       (unwind-protect
           (progn
             (push (ende-codex--refresh-auth personal #'ignore) cancellations)
             (push (ende-codex--refresh-auth personal-new #'ignore) cancellations)
             (should (= (length flights) 1))
             (should (equal (ende-codex--refresh-key personal)
                            (ende-codex--refresh-key personal-new)))
             (push (ende-codex--refresh-auth work #'ignore) cancellations)
             (should (= (length flights) 2))
             (mapc #'funcall cancellations)
             (setq cancellations nil)
             (let ((replacement (ende-codex-auth--register 'chatgpt-plan-oauth
                                 'personal credentials (plist-get personal-new :revision) t)))
               (push (ende-codex--refresh-auth replacement #'ignore) cancellations)
               (should (= (length flights) 3))))
         (mapc #'funcall cancellations))))))

(ert-deftest ende-codex-auth-refresh-flight-shares-outcome-and-isolates-cancellation ()
  (let* ((ende-codex--refresh-flights (make-hash-table :test #'equal))
         (received nil)
         (cancelled (ende-codex--make-refresh-waiter :callback #'ignore))
         (waiting (ende-codex--make-refresh-waiter
                   :callback (lambda (value) (setq received value))))
         (flight (ende-codex--make-refresh-flight
                  :key 'registration :waiters (list cancelled waiting))))
    (puthash 'registration flight ende-codex--refresh-flights)
    (ende-codex--refresh-cancel-waiter flight cancelled)
    (should-not (ende-codex--refresh-flight-finished flight))
    (should (equal (ende-codex--refresh-flight-waiters flight) (list waiting)))
    (ende-codex--refresh-finish flight 'shared-result)
    (should (eq received 'shared-result))
    (should (ende-codex--refresh-flight-finished flight))
    (should-not (gethash 'registration ende-codex--refresh-flights))))

(ert-deftest ende-codex-auth-refresh-flight-callback-errors-do-not-strand-waiters ()
  (let* ((ende-codex--refresh-flights (make-hash-table :test #'equal))
         (called nil)
         (first (ende-codex--make-refresh-waiter :callback (lambda (_) (error "callback failed"))))
         (second (ende-codex--make-refresh-waiter :callback (lambda (value) (setq called value))))
         (flight (ende-codex--make-refresh-flight :key 'key :waiters (list first second))))
    (puthash 'key flight ende-codex--refresh-flights)
    (ende-codex--refresh-finish flight 'shared-result)
    (should (eq called 'shared-result))
    (should (ende-codex--refresh-flight-finished flight))
    (should-not (gethash 'key ende-codex--refresh-flights))))

(ert-deftest ende-codex-auth-refresh-before-http-adopts-newer-current-record ()
  (ende-codex-auth-test--with-store
   (let* ((expired (plist-put (ende-codex-auth-test--credentials) :expires-at 0))
          (old (ende-codex-auth--register 'chatgpt-plan-oauth 'personal expired))
          (current (ende-codex-auth--rotate old (ende-plist-merge
                                                  expired (list :expires-at (+ (floor (float-time)) 3600)))))
          (ende-codex--refresh-flights (make-hash-table :test #'equal))
          (calls 0) result)
     (let ((flight (ende-codex--make-refresh-flight
                    :key (ende-codex--refresh-key old) :auth old
                    :waiters (list (ende-codex--make-refresh-waiter
                                    :callback (lambda (value) (setq result value)))))))
       (puthash (ende-codex--refresh-flight-key flight) flight ende-codex--refresh-flights)
       (cl-letf (((symbol-function 'ende-codex--start-refresh-flight)
                  (lambda (_) (cl-incf calls))))
         (ende-codex--refresh-flight-begin flight))
       (should (equal result current))
       (should (= calls 0))
       (should-not (gethash (ende-codex--refresh-flight-key flight) ende-codex--refresh-flights))))))

(ert-deftest ende-codex-auth-refresh-stale-success-adopts-newer-record ()
  (ende-codex-auth-test--with-store
   (let* ((expired (plist-put (ende-codex-auth-test--credentials) :expires-at 0))
          (old (ende-codex-auth--register 'chatgpt-plan-oauth 'personal expired))
          (newer (ende-codex-auth--rotate
                  old (ende-plist-merge expired
                                        (list :access-token "newer-access"
                                              :refresh-token "newer-refresh"
                                              :expires-at (+ (floor (float-time)) 3600)))))
          (ende-codex--refresh-flights (make-hash-table :test #'equal))
          result
          (flight (ende-codex--make-refresh-flight
                   :key 'flight :auth old
                   :waiters (list (ende-codex--make-refresh-waiter
                                   :callback (lambda (value) (setq result value))))))
          (payload (ende-codex-auth--json
                    "{\"access_token\":\"stale-access\",\"refresh_token\":\"stale-refresh\",\"token_type\":\"Bearer\",\"expires_in\":3600}")))
     (puthash 'flight flight ende-codex--refresh-flights)
     (ende-codex--refresh-rotate flight payload nil)
     (should (equal newer result))
     (should (equal newer (ende-codex-auth-read 'chatgpt-plan-oauth 'personal)))
     (should-not (equal "stale-refresh"
                        (plist-get (ende-codex-auth-read 'chatgpt-plan-oauth 'personal)
                                   :refresh-token))))))

(ert-deftest ende-codex-auth-refresh-rejected-token-adopts-newer-same-registration ()
  (ende-codex-auth-test--with-store
   (let* ((expired (plist-put (ende-codex-auth-test--credentials) :expires-at 0))
          (old (ende-codex-auth--register 'chatgpt-plan-oauth 'personal expired))
          (rotated (ende-codex-auth--rotate old
                                            (ende-plist-merge expired
                                                              (list :refresh-token "newer-refresh"
                                                                    :expires-at (+ (floor (float-time)) 3600)))))
          (ende-codex--refresh-flights (make-hash-table :test #'equal))
          (result nil)
          (flight (ende-codex--make-refresh-flight
                   :key 'flight :auth old
                   :waiters (list (ende-codex--make-refresh-waiter
                                   :callback (lambda (value) (setq result value)))))))
     (puthash 'flight flight ende-codex--refresh-flights)
     (cl-letf (((symbol-function 'ende-codex--refresh-finish)
                (lambda (current value &optional _cancelled)
                  (setq result value)
                  (setf (ende-codex--refresh-flight-finished current) t))))
       (ende-codex--refresh-response
        flight 400 (ende-codex-auth--json "{\"error\":\"invalid_grant\"}")))
     (should (equal result rotated))
     (should-not (plist-get (ende-codex-auth-read 'chatgpt-plan-oauth 'personal) :needs-login)))))

(ert-deftest ende-codex-auth-refresh-conflict-never-adopts-reconnect ()
  (ende-codex-auth-test--with-store
   (let* ((expired (plist-put (ende-codex-auth-test--credentials) :expires-at 0))
          (old (ende-codex-auth--register 'chatgpt-plan-oauth 'personal expired))
          (ende-codex--refresh-flights (make-hash-table :test #'equal))
          (result nil)
          (flight (ende-codex--make-refresh-flight
                   :key 'flight :auth old
                   :waiters (list (ende-codex--make-refresh-waiter
                                   :callback (lambda (value) (setq result value)))))))
     (ende-codex-auth--register 'chatgpt-plan-oauth 'personal
                                (ende-codex-auth-test--credentials "reconnect")
                                (plist-get old :revision) t)
     (puthash 'flight flight ende-codex--refresh-flights)
     (cl-letf (((symbol-function 'run-at-time)
                (lambda (_delay _repeat function &rest args)
                  (when (eq function #'ende-codex--refresh-rejected)
                    (setf (ende-codex--refresh-flight-attempts flight) 2)
                    (apply function args))))
               ((symbol-function 'ende-codex--refresh-finish)
                (lambda (current value &optional _cancelled)
                  (setq result value)
                  (setf (ende-codex--refresh-flight-finished current) t))))
       (ende-codex--refresh-response
        flight 400 (ende-codex-auth--json "{\"error\":\"invalid_grant\"}")))
     (should (ende-error-p result))
     (should-not (plist-get (ende-codex-auth-read 'chatgpt-plan-oauth 'personal) :needs-login)))))

(ert-deftest ende-codex-auth-refresh-uses-issued-client-and-private-stdin ()
  (ende-codex-auth-test--with-store
   (let* ((credentials (plist-put (ende-codex-auth-test--credentials) :expires-at 0))
          (old (ende-codex-auth--register 'chatgpt-plan-oauth 'personal credentials))
          (profile (ende-codex-capture-binding (ende-codex-auth-test--profile 'personal)))
          (ende-codex--refresh-flights (make-hash-table :test #'equal))
          (ende-codex--refresh-processes nil)
          (real-make-process (symbol-function 'make-process))
          process command sent result cancel)
     (cl-letf (((symbol-function 'make-process)
                (lambda (&rest args)
                  (setq command (plist-get args :command)
                        process (funcall real-make-process :name "ende-test-refresh"
                                         :command '("cat") :connection-type 'pipe
                                         :noquery t :filter #'ignore :sentinel #'ignore))))
               ((symbol-function 'process-send-string)
                (lambda (_process text) (setq sent text)))
               ((symbol-function 'process-send-eof) #'ignore))
       (unwind-protect
           (progn
             (setq cancel (ende-codex--resolve-auth (lambda (value) (setq result value)) profile))
             (should (string-match-p "client_id=issued-client-one" sent))
             (should (string-match-p "refresh_token=synthetic-refresh-one" sent))
             (should-not (string-match-p "synthetic-" (prin1-to-string command)))
             (let ((flight (gethash (ende-codex--refresh-key old) ende-codex--refresh-flights)))
               (with-temp-file (ende-codex--refresh-flight-headers-file flight)
                 (insert "HTTP/1.1 200 OK\r\n\r\n"))
               (ende-codex--refresh-filter
                flight process
                "{\"access_token\":\"synthetic-new-access\",\"refresh_token\":\"synthetic-new-refresh\",\"token_type\":\"Bearer\",\"expires_in\":3600}")
               (cl-letf (((symbol-function 'process-status) (lambda (_) 'exit))
                         ((symbol-function 'process-exit-status) (lambda (_) 0)))
                 (ende-codex--refresh-sentinel flight process "finished\n")))
             (should-not (ende-error-p result))
             (should (equal (plist-get result :refresh-token) "synthetic-new-refresh"))
             (should (ende-codex-auth--same-registration-p old result))
             (should-not (ende-codex--check-binding profile result)))
         (when cancel (funcall cancel))
         (when (and process (process-live-p process)) (delete-process process)))))))

(ert-deftest ende-codex-auth-refresh-stale-winner-must-have-the-same-registration ()
  (ende-codex-auth-test--with-store
   (let* ((credentials (ende-codex-auth-test--credentials))
          (old (ende-codex-auth--register 'chatgpt-plan-oauth 'personal credentials))
          (rotated (ende-codex-auth--rotate old credentials)))
     (should-error (ende-codex--store-refresh old "stale-access" "stale-refresh" 3600 nil)
                   :type 'ende-codex-auth-conflict)
     (should (equal rotated (ende-codex--refresh-fresh-winner old)))
     (ende-codex-auth--register 'chatgpt-plan-oauth 'personal credentials (plist-get rotated :revision) t)
     (should-not (ende-codex--refresh-fresh-winner old)))))

(ert-deftest ende-codex-auth-refresh-replacement-while-waiting-fails-closed ()
  (ende-codex-auth-test--with-store
   (let* ((credentials (plist-put (ende-codex-auth-test--credentials) :expires-at 0))
          (old (ende-codex-auth--register 'chatgpt-plan-oauth 'personal credentials))
          (profile (ende-codex-capture-binding (ende-codex-auth-test--profile 'personal)))
          callback result)
     (cl-letf (((symbol-function 'ende-codex--refresh-auth)
                (lambda (_auth function) (setq callback function) #'ignore)))
       (ende-codex--resolve-auth (lambda (value) (setq result value)) profile)
       (should-not result)
       (let ((new (ende-codex-auth--register 'chatgpt-plan-oauth 'personal (ende-codex-auth-test--credentials)
                                             (plist-get old :revision) t)))
         (funcall callback new)
         (should (ende-error-p result)))))))

(ert-deftest ende-codex-auth-refresh-defers-rotation-until-verification ()
  (ende-codex-auth-test--with-store
   (let* ((result nil)
          (verify nil)
          (old (ende-codex-auth--register 'chatgpt-plan-oauth 'personal (ende-codex-auth-test--credentials)))
          (ende-codex--refresh-flights (make-hash-table :test #'equal))
          (payload (ende-codex-auth--json
                    "{\"access_token\":\"new-access\",\"refresh_token\":\"new-refresh\",\"id_token\":\"replacement\",\"token_type\":\"Bearer\",\"expires_in\":3600}"))
          (waiter (ende-codex--make-refresh-waiter
                   :callback (lambda (value) (setq result value))))
          (flight (ende-codex--make-refresh-flight
                   :key 'refresh :auth old :waiters (list waiter))))
     (puthash 'refresh flight ende-codex--refresh-flights)
     (cl-letf (((symbol-function 'ende-codex-auth--verify-refresh)
                (lambda (_id-token _record callback) (setq verify callback) #'ignore)))
       (ende-codex--refresh-response flight 200 payload)
       (should verify)
       (should-not result)
       (should (equal old (ende-codex-auth-read 'chatgpt-plan-oauth 'personal)))
       (funcall verify 'claims nil)
       (should (equal (plist-get result :refresh-token) "new-refresh"))
       (should (equal (plist-get (ende-codex-auth-read 'chatgpt-plan-oauth 'personal) :refresh-token)
                      "new-refresh")))
     (let* ((verify nil)
            (old (ende-codex-auth--register 'chatgpt-plan-oauth 'work (ende-codex-auth-test--credentials "two")))
            (waiter (ende-codex--make-refresh-waiter :callback #'ignore))
            (flight (ende-codex--make-refresh-flight
                     :key 'cancelled :auth old :waiters (list waiter))))
       (puthash 'cancelled flight ende-codex--refresh-flights)
       (cl-letf (((symbol-function 'ende-codex-auth--verify-refresh)
                  (lambda (_id-token _record callback) (setq verify callback) #'ignore)))
         (ende-codex--refresh-response flight 200 payload)
         (ende-codex--refresh-cancel-waiter flight waiter)
         (funcall verify 'claims nil)
         (should (equal old (ende-codex-auth-read 'chatgpt-plan-oauth 'work))))))))

(ert-deftest ende-codex-auth-resolve-never-falls-back-or-sends-replaced-account ()
  (ende-codex-auth-test--with-store
   (let (result)
     (ende-codex--resolve-auth (lambda (value) (setq result value))
                               (ende-codex-auth-test--profile 'missing))
     (should (ende-error-p result)))
   (let* ((credentials (ende-codex-auth-test--credentials))
          (old (ende-codex-auth--register 'chatgpt-plan-oauth 'personal credentials))
          (profile (ende-codex-capture-binding (ende-codex-auth-test--profile 'personal)))
          result)
     (ende-codex--resolve-auth (lambda (value) (setq result value)) profile)
     (should (equal old result))
     (ende-codex-auth--register 'chatgpt-plan-oauth 'personal credentials (plist-get old :revision) t)
     (ende-codex--resolve-auth (lambda (value) (setq result value)) profile)
     (should (ende-error-p result)))))

(ert-deftest ende-codex-auth-validation-precedes-all-queue-credential-access ()
  (let ((base (ende-codex-auth-test--profile 'personal))
        (credential-accesses 0))
    (cl-letf (((symbol-function 'ende-codex--profile-auth)
               (lambda (&rest _) (cl-incf credential-accesses) (error "No credentials")))
              ((symbol-function 'ende-credentials--resolve)
               (lambda (&rest _) (cl-incf credential-accesses) (error "No credentials"))))
      (with-temp-buffer
        (let ((target (ende-target-create :buffer (current-buffer) :start 1 :end 1 :cursor 1)))
          (dolist (overrides '((:model nil) (:api unsupported) (:account nil)
                               (:auth-method none) (:stream-usage invalid)
                               (:url "https://attacker.test/responses")
                               (:parameters (:access_token "synthetic-secret"))
                               (:parameters (:nested (:id_token "synthetic-secret")))
                               (:access-token "synthetic-secret")))
            (should-error (ende--job-from-request
                           (list 'insert :target target :profile (ende-plist-merge base overrides)))))
          (should (= credential-accesses 0))
          (ende-target-release target))))))

(ert-deftest ende-codex-auth-validation-retained-transport-profile-before-network ()
  (let ((base (ende-codex-auth-test--profile 'personal))
        (accesses 0))
    (cl-letf (((symbol-function 'ende-codex--profile-auth)
               (lambda (&rest _) (cl-incf accesses) (error "Credential access")))
              ((symbol-function 'make-process)
               (lambda (&rest _) (cl-incf accesses) (error "Network access"))))
      (dolist (overrides '((:model nil) (:api unsupported) (:account nil)
                           (:auth-method none) (:stream-usage invalid)
                           (:url "https://attacker.test/responses")
                           (:access-token "synthetic-secret")))
        (let (failure)
          (ende-codex-default '(("user" . "test"))
                              (list :on-error (lambda (value) (setq failure value)))
                              (ende-plist-merge base overrides))
          (should (ende-error-p failure))))
      (should (= accesses 0)))))

(ert-deftest ende-codex-auth-validation-no-unstaged-alias-or-unknown-source ()
  (let ((base (ende-codex-auth-test--profile 'personal)))
    (dolist (source '(nil unknown))
      (let ((profile (plist-put (copy-sequence base) :codex-auth-source source))
            failure)
        (should-error (ende-provider-validate-profile profile))
        (ende-codex-default '(("user" . "test"))
                            (list :on-error (lambda (value) (setq failure value)))
                            profile)
        (should (ende-error-p failure))))))

(ert-deftest ende-codex-auth-host-exists-before-registration ()
  (ende-codex-auth-test--with-store
   (let ((host (ende-codex-auth-host-id)))
     (should (ende-codex-auth--id-p host))
     (should (equal host (ende-codex-auth-host-id)))
     (should (equal host (plist-get (ende-codex-auth--register 'chatgpt-plan-oauth
                                     'personal (ende-codex-auth-test--credentials)) :host-id))))))

(ert-deftest ende-codex-auth-storage-second-emacs-preserves-newer-records ()
  :tags '(integration)
  (ende-codex-auth-test--with-store
   (let* ((credentials (ende-codex-auth-test--credentials))
          (old (ende-codex-auth--register 'chatgpt-plan-oauth 'personal credentials))
          (host (plist-get old :host-id))
          (executable (expand-file-name invocation-name invocation-directory))
          (library-directory (file-name-directory (locate-library "ende-codex-auth"))))
     ;; Tokens are read and generated inside the child, not passed on argv.
     (with-temp-buffer
       (should
        (= 0 (call-process
              executable nil t nil "-Q" "--batch" "-L" library-directory
              "-l" "ende-codex-auth" "--eval"
              (prin1-to-string
               `(let* ((ende-codex-auth--directory ,ende-codex-auth--directory)
                       (record (ende-codex-auth-read 'chatgpt-plan-oauth 'personal))
                       (credentials (cl-loop for key in ende-codex-auth--credential-fields
                                             when (plist-member record key)
                                             append (list key (plist-get record key)))))
                  (ende-codex-auth--rotate record credentials)
                  (ende-codex-auth--register 'chatgpt-plan-oauth 'work credentials)))))))
     (should-error (ende-codex-auth--rotate old credentials) :type 'ende-codex-auth-conflict)
     (should (equal host (plist-get (ende-codex-auth-read 'chatgpt-plan-oauth 'work) :host-id)))
     (should (ende-codex-auth--same-registration-p old (ende-codex-auth-read 'chatgpt-plan-oauth 'personal))))))

(ert-deftest ende-codex-auth-secrets-absent-from-selections-debug-and-reload ()
  (ende-codex-auth-test--with-store
   (let* ((record (ende-codex-auth--register 'chatgpt-plan-oauth 'personal (ende-codex-auth-test--credentials)))
          (profile (ende-codex-capture-binding (ende-codex-auth-test--profile 'personal)))
          (ende-providers '(codex-personal codex-work))
          (ende-builtin-provider-settings
           '((codex-personal :provider openai-codex :account personal :models ("synthetic-model"))
             (codex-work :provider openai-codex :account work :models ("synthetic-model"))))
          (ende-model-selection--last-used nil)
          (ende-model-selection--buffer-selections (make-hash-table :test #'equal))
          (ende-model-selection--provenance (make-hash-table :test #'equal))
          (ende-model-selection--shared-control-preferences (make-hash-table :test #'equal))
          (ende-model-selection--buffer-control-preferences (make-hash-table :test #'equal))
          (debug-directory (expand-file-name "debug" root))
          (ende-debugging-enabled t)
          (ende-debug--session-directory debug-directory)
          (ende-debug--sequence 0)
          (savehist-file (expand-file-name "history.el" root))
          (savehist-additional-variables
           '(ende-model-selection--last-used ende-model-selection--buffer-selections
					     ende-model-selection--provenance ende-model-selection--shared-control-preferences
					     ende-model-selection--buffer-control-preferences)))
     (make-directory debug-directory)
     ;; Configuration resolution and persisted selections use :account.
     (let ((personal (ende-provider-config-resolve 'codex-personal))
           (work (ende-provider-config-resolve 'codex-work)))
       (should (eq (plist-get personal :account) 'personal))
       (should (eq (plist-get work :account) 'work)))
     (cl-letf (((symbol-function 'ende-provider-profile-for-model)
                (lambda (name model &optional api)
                  (ende-provider-config-resolve name model api))))
       (ende-model-selection-save '(:provider codex-personal :model "synthetic-model") nil t))
     (savehist-save t)
     (let* ((messages '(("user" . "Synthetic question")))
            (body (ende-codex--request-body messages profile)))
       (should (ende-debug-record-request (json-encode body) body messages profile
                                          "openai-responses" (plist-get profile :url))))
     (dolist (file (cons savehist-file (directory-files debug-directory t "^[^.].*")))
       (with-temp-buffer
         (insert-file-contents file)
         (dolist (key '(:access-token :refresh-token :id-token))
           (should-not (string-match-p (regexp-quote (plist-get record key)) (buffer-string))))))
     (dolist (variable savehist-additional-variables)
       (should-not (string-match-p "synthetic-access\\|synthetic-refresh\\|synthetic-id"
                                   (prin1-to-string (symbol-value variable)))))
     (let* ((files (ende-reload--files (file-name-directory (locate-library "ende.el")))))
       (should (< (cl-position "ende-codex-auth.el" files :test #'equal)
                  (cl-position "ende-codex.el" files :test #'equal)))
       (should (< (cl-position "ende-codex.el" files :test #'equal)
                  (cl-position "ende-selector.el" files :test #'equal))))
     (ende-reload)
     (should (equal (plist-get record :host-id) (ende-codex-auth-host-id)))
     (should-not (ende-codex--check-binding profile (ende-codex-auth-read 'chatgpt-plan-oauth 'personal))))))

(provide 'ende-codex-auth-test)
;;; ende-codex-auth-test.el ends here
