;;; ende-codex-usage.el --- ChatGPT plan usage -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'ende-core)
(require 'ende-curl)
(require 'ende-protocol)
(require 'ende-codex)

(defconst ende-codex-usage--url "https://chatgpt.com/backend-api/wham/usage")
(defconst ende-codex-usage--response-limit (* 64 1024))
(defconst ende-codex-usage--cache-seconds 60)
(defconst ende-codex-usage--failure-cache-seconds 30)

(defvar ende-codex-usage--cache (make-hash-table :test #'equal))
(defvar ende-codex-usage--flights (make-hash-table :test #'equal))

(cl-defstruct (ende-codex-usage--request
               (:constructor ende-codex-usage--make-request))
  callback resources auth-cancel (bytes 0) finished)

(cl-defstruct (ende-codex-usage--waiter
               (:constructor ende-codex-usage--make-waiter))
  callback)

(cl-defstruct (ende-codex-usage--flight
               (:constructor ende-codex-usage--make-flight))
  key waiters cancel)

(defun ende-codex-usage--account-id (auth)
  "Return the ChatGPT account ID in AUTH's stored ID token, if present."
  (let* ((token (plist-get auth :id-token))
         (parts (and (stringp token) (split-string token "\\." nil)))
         (claims (when (and parts (= (length parts) 3))
                   (condition-case nil
                       (ende-codex-auth--json (ende-rs256--decode (cadr parts)))
                     ((error quit) nil))))
         (auth-claims (ende-protocol-json-get "https://api.openai.com/auth" claims))
         (organizations (ende-protocol-json-get "organizations" claims))
         (id (or (ende-protocol-json-get "chatgpt_account_id" auth-claims)
                 (ende-protocol-json-get "chatgpt_account_id" claims)
                 (and (vectorp organizations) (> (length organizations) 0)
                      (ende-protocol-json-get "id" (aref organizations 0))))))
    (and (ende-codex-auth--text-p id)
         (not (string-match-p "[[:cntrl:]]" id)) id)))

(defun ende-codex-usage--window (entry)
  "Normalize a quota WINDOW JSON object, or return nil."
  (when (hash-table-p entry)
    (let ((used (ende-protocol-json-get "used_percent" entry))
          (reset (ende-protocol-json-get "reset_at" entry))
          (duration (ende-protocol-json-get "limit_window_seconds" entry)))
      (when (numberp used)
        (list :used-percent (max 0 (min 100 used))
              :reset-at (and (numberp reset) reset)
              :window-seconds (and (numberp duration) (> duration 0) duration))))))

(defun ende-codex-usage-parse (text)
  "Parse TEXT into primary and secondary quota windows."
  (let* ((payload (ende-protocol-json-read-string text))
         (rate (ende-protocol-json-get "rate_limit" payload))
         (primary (and (hash-table-p rate)
                       (ende-codex-usage--window
                        (ende-protocol-json-get "primary_window" rate))))
         (secondary (and (hash-table-p rate)
                         (ende-codex-usage--window
                          (ende-protocol-json-get "secondary_window" rate)))))
    (unless (or primary secondary) (error "Usage response has no quota windows"))
    (list :primary primary :secondary secondary)))

(defun ende-codex-usage--duration (seconds)
  "Format SECONDS as a compact duration."
  (let* ((seconds (max 0 (floor seconds)))
         (days (/ seconds 86400))
         (hours (/ (% seconds 86400) 3600))
         (minutes (/ (% seconds 3600) 60)))
    (cond ((> days 0) (format "%dd %dh" days hours))
          ((> hours 0) (format "%dh%s" hours
                              (if (> minutes 0) (format " %dm" minutes) "")))
          ((> minutes 0) (format "%dm" minutes))
          (t "now"))))

(defun ende-codex-usage--window-label (window now primary)
  "Return WINDOW's reset label relative to NOW."
  (let ((seconds (if (and primary (numberp (plist-get window :reset-at)))
                     (- (plist-get window :reset-at) now)
                   (plist-get window :window-seconds))))
    (cond ((and (numberp seconds) (>= seconds (* 7 86400))) "weekly")
          ((numberp seconds) (ende-codex-usage--duration seconds))
          (t "quota"))))

(defun ende-codex-usage-string (quota &optional now)
  "Format QUOTA for the selector, using NOW when supplied."
  (let ((now (or now (float-time))) segments reset-at)
    (dolist (key '(:primary :secondary))
      (let ((window (plist-get quota key)))
        (when window
          (push (format "%s: %d%% left"
                        (ende-codex-usage--window-label
                         window now (eq key :primary))
                        (round (- 100 (plist-get window :used-percent))))
                segments)
          (when (numberp (plist-get window :reset-at))
            (setq reset-at (if reset-at
                               (min reset-at (plist-get window :reset-at))
                             (plist-get window :reset-at)))))))
    (setq segments (nreverse segments))
    (when reset-at
      (setq segments (append segments
                             (list (format "reset in %s"
                                           (ende-codex-usage--duration
                                            (- reset-at now)))))))
    (mapconcat #'identity segments " · ")))

(defun ende-codex-usage--finish (request quota error &optional cancelled)
  "Finish REQUEST with QUOTA or ERROR, unless CANCELLED."
  (unless (ende-codex-usage--request-finished request)
    (setf (ende-codex-usage--request-finished request) t)
    (when (and cancelled (functionp (ende-codex-usage--request-auth-cancel request)))
      (funcall (ende-codex-usage--request-auth-cancel request)))
    (setf (ende-codex-usage--request-auth-cancel request) nil)
    (let ((resources (ende-codex-usage--request-resources request)))
      (when resources
        (ende-curl-cleanup-resources
         (list (plist-get resources :response-buffer)
               (plist-get resources :stderr-buffer))
         (list (plist-get resources :headers-file) (plist-get resources :config-file))
         (list (plist-get resources :process) (plist-get resources :stderr-process)))
        (setf (ende-codex-usage--request-resources request) nil)))
    (unless cancelled
      (condition-case nil
          (funcall (ende-codex-usage--request-callback request) quota error)
        ((error quit) nil)))))

(defun ende-codex-usage--filter (request _process chunk)
  "Append response CHUNK to REQUEST within the response size limit."
  (unless (ende-codex-usage--request-finished request)
    (let ((size (+ (ende-codex-usage--request-bytes request) (string-bytes chunk))))
      (setf (ende-codex-usage--request-bytes request) size)
      (if (> size ende-codex-usage--response-limit)
          (ende-codex-usage--finish
           request nil (ende-error-create :kind 'resource
                                          :message "Usage response exceeds its size limit"))
        (with-current-buffer
            (plist-get (ende-codex-usage--request-resources request) :response-buffer)
          (goto-char (point-max))
          (insert chunk))))))

(defun ende-codex-usage--sentinel (request process _event)
  "Handle terminal PROCESS for REQUEST without exposing response contents."
  (when (and (not (ende-codex-usage--request-finished request))
             (memq (process-status process) '(exit signal closed failed)))
    (if (/= (process-exit-status process) 0)
        (ende-codex-usage--finish
         request nil (or (process-get process :ende-idle-error)
                         (ende-error-create :kind 'transport
                                            :message "Usage request failed")))
      (let* ((resources (ende-codex-usage--request-resources request))
             (status (car (ende-curl-response-headers
                           (plist-get resources :headers-file)))))
        (if (not (equal status 200))
            (ende-codex-usage--finish
             request nil (ende-error-create :kind 'http
                                            :message (format "Usage request returned HTTP %s"
                                                             (or status "unknown"))
                                            :details (list :status status)))
          (condition-case nil
              (ende-codex-usage--finish
               request
               (ende-codex-usage-parse
                (with-current-buffer (plist-get resources :response-buffer)
                  (buffer-string)))
               nil)
            ((error quit)
             (ende-codex-usage--finish
              request nil (ende-error-create :kind 'protocol
                                             :message "Usage response is malformed")))))))))

(defun ende-codex-usage--start (request auth)
  "Start the isolated usage request for REQUEST with AUTH."
  (let* ((url ende-codex-usage--url)
         (account-id (ende-codex-usage--account-id auth))
         (headers (append (list (concat "Authorization: Bearer "
                                        (ende-codex--access-token auth)))
                          (when account-id
                            (list (concat "ChatGPT-Account-ID: " account-id)))))
         (resources
          (ende-curl-start-request
           :profile (list :url url) :name "ende-codex-usage"
           :internal-headers headers
           :command
           (lambda (headers-file config-file)
             (append (ende-curl-base-command url)
                     (ende-curl-response-options headers-file)
                     '("--request" "GET" "--header" "Accept: application/json")
                     (when config-file (list "--config" config-file))
                     (ende-curl-timeout-options
                      ende-request-timeout ende-codex-usage--response-limit)
                     (list "--url" url)))
           :filter (apply-partially #'ende-codex-usage--filter request)
           :sentinel (apply-partially #'ende-codex-usage--sentinel request))))
    (setf (ende-codex-usage--request-resources request) resources)))

(defun ende-codex-usage--auth-ready (request auth)
  "Start REQUEST after Ende's OAuth resolver delivers AUTH."
  (unless (ende-codex-usage--request-finished request)
    (if (ende-error-p auth)
        (ende-codex-usage--finish request nil auth)
      (condition-case nil
          (ende-codex-usage--start request auth)
        ((error quit)
         (ende-codex-usage--finish
          request nil (ende-error-create :kind 'startup
                                         :message "Usage request could not be started")))))))

(defun ende-codex-usage--cancel (request)
  "Cancel REQUEST without delivering a callback."
  (ende-codex-usage--finish request nil nil t))

(defun ende-codex-usage-fetch (profile callback)
  "Fetch usage for PROFILE and call CALLBACK with (QUOTA ERROR).
Return a cancellation function."
  (let* ((request (ende-codex-usage--make-request :callback callback))
         (cancel (condition-case nil
                     (ende-codex--resolve-auth
                      (apply-partially #'ende-codex-usage--auth-ready request)
                      profile)
                   ((error quit)
                    (ende-codex-usage--finish
                     request nil (ende-error-create :kind 'auth
                                                    :message "Usage authentication failed"))
                    nil))))
    (unless (ende-codex-usage--request-finished request)
      (setf (ende-codex-usage--request-auth-cancel request) cancel))
    (apply-partially #'ende-codex-usage--cancel request)))

(defun ende-codex-usage--finish-flight (flight quota error)
  "Cache QUOTA or ERROR for FLIGHT and notify its active waiters."
  (let* ((key (ende-codex-usage--flight-key flight))
         (old (gethash key ende-codex-usage--cache))
         (value (or quota (plist-get old :quota)))
         (delay (if error ende-codex-usage--failure-cache-seconds
                  ende-codex-usage--cache-seconds)))
    (when (eq (gethash key ende-codex-usage--flights) flight)
      (remhash key ende-codex-usage--flights)
      (puthash key (list :quota value :error error
                         :retry-at (+ (float-time) delay))
               ende-codex-usage--cache)
      (let ((waiters (ende-codex-usage--flight-waiters flight)))
        (setf (ende-codex-usage--flight-waiters flight) nil)
        (dolist (waiter waiters)
          (when (ende-codex-usage--waiter-callback waiter)
            (condition-case nil
                (funcall (ende-codex-usage--waiter-callback waiter)
                         (copy-tree value) error)
              ((error quit) nil))))))))

(defun ende-codex-usage--cancel-waiter (flight waiter)
  "Remove WAITER from FLIGHT and cancel its request when unused."
  (when (ende-codex-usage--waiter-callback waiter)
    (setf (ende-codex-usage--waiter-callback waiter) nil
          (ende-codex-usage--flight-waiters flight)
          (delq waiter (ende-codex-usage--flight-waiters flight)))
    (when (null (ende-codex-usage--flight-waiters flight))
      (let ((key (ende-codex-usage--flight-key flight)))
        (when (eq (gethash key ende-codex-usage--flights) flight)
          (remhash key ende-codex-usage--flights)
          (when (functionp (ende-codex-usage--flight-cancel flight))
            (funcall (ende-codex-usage--flight-cancel flight))))))))

(defun ende-codex-usage-probe (profile callback)
  "Get cached or fresh usage for PROFILE and call CALLBACK with (QUOTA ERROR).
Return a cancellation function for this callback."
  (let* ((auth (condition-case nil (ende-codex--profile-auth profile)
                 ((error quit) nil)))
         (key (and auth (ende-codex--account-binding-state auth)))
         (cached (and key (gethash key ende-codex-usage--cache)))
         (flight (and key (gethash key ende-codex-usage--flights)))
         (waiter (ende-codex-usage--make-waiter :callback callback)))
    (cond
     ((null key)
      (funcall callback nil
               (ende-error-create :kind 'auth
                                  :message "Codex quota requires a connected account"))
      #'ignore)
     ((and cached (< (float-time) (plist-get cached :retry-at)))
      (funcall callback (copy-tree (plist-get cached :quota))
               (plist-get cached :error))
      #'ignore)
     (t
      (unless flight
        (setq flight (ende-codex-usage--make-flight :key key))
        (puthash key flight ende-codex-usage--flights))
      (push waiter (ende-codex-usage--flight-waiters flight))
      (unless (ende-codex-usage--flight-cancel flight)
        (let ((cancel
               (ende-codex-usage-fetch
                profile
                (apply-partially #'ende-codex-usage--finish-flight flight))))
          (when (eq (gethash key ende-codex-usage--flights) flight)
            (setf (ende-codex-usage--flight-cancel flight) cancel))))
      (apply-partially #'ende-codex-usage--cancel-waiter flight waiter)))))

(defun ende-codex-usage-clear-cache ()
  "Clear cached quota results."
  (clrhash ende-codex-usage--cache))

(defun ende-codex-usage-login-complete (record failure)
  "Refresh usage after successful login for RECORD, ignoring quota failures."
  (unless failure
    (condition-case nil
        (progn
          (ende-codex-usage-clear-cache)
          (let* ((profile (ende-codex--account-profile
                           (ende-provider-profile 'openai-codex)))
                 (profile (plist-put profile :account (plist-get record :account))))
            (ende-codex-usage-probe profile #'ignore)))
      ((error quit) nil))))

(add-hook 'ende-codex-login-complete-functions
          #'ende-codex-usage-login-complete)

(provide 'ende-codex-usage)
;;; ende-codex-usage.el ends here
