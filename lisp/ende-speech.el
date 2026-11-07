;;; ende-speech.el --- Local realtime speech dictation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter Samarin
;;
;; Author: Peter Samarin <peter.samarin@gmail.com>
;; Maintainer: Peter Samarin <peter.samarin@gmail.com>
;; Version: 0.3
;; Keywords: convenience, tools
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

;; Local microphone capture and realtime transcription, independent of Ende's
;; job and LLM machinery.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'ende-websocket)

(defgroup ende-speech nil
  "Local realtime speech dictation."
  :group 'ende)

(defcustom ende-speech-api-path "/v1/audio/transcriptions/realtime"
  "WebSocket endpoint path for realtime speech transcription."
  :type 'string
  :group 'ende-speech)

(defcustom ende-speech-encryption nil
  "Whether to encrypt speech WebSocket connections using TLS."
  :type 'boolean
  :group 'ende-speech)

(defcustom ende-speech-input-device nil
  "PulseAudio/PipeWire source identifier, or nil for the system default."
  :type '(choice (const :tag "System default" nil) string)
  :group 'ende-speech)

(defcustom ende-speech-server "127.0.0.1"
  "Hostname or IP address of the realtime speech server."
  :type 'string
  :group 'ende-speech)

(defcustom ende-speech-port 8081
  "Port of the realtime speech server."
  :type 'integer
  :group 'ende-speech)

(cl-defstruct (ende-speech--session
               (:constructor ende-speech--make-session))
  state marker buffer source-window transcript-window websocket recorder
  recorder-stderr clear-pending needs-separator status)

(defvar-local ende-speech--command-point nil)
(defvar-local ende-speech--command-tick nil)


(defvar ende-speech--active-session nil
  "The one active realtime speech session, if any.")

(defconst ende-speech--configuration-variables
  '(ende-speech-api-path ende-speech-encryption ende-speech-input-device
    ende-speech-port ende-speech-server)
  "Speech settings edited in the configuration buffer.")

(defvar ende-speech-configure-mode-map (make-sparse-keymap)
  "Keymap for `ende-speech-configure-mode'.")

(define-key ende-speech-configure-mode-map (kbd "a") #'ende-speech--configure-api-path)
(define-key ende-speech-configure-mode-map (kbd "e") #'ende-speech--configure-encryption)
(define-key ende-speech-configure-mode-map (kbd "i") #'ende-speech--configure-input)
(define-key ende-speech-configure-mode-map (kbd "p") #'ende-speech--configure-port)
(define-key ende-speech-configure-mode-map (kbd "s") #'ende-speech--configure-server)
(define-key ende-speech-configure-mode-map (kbd "q") #'ende-speech--configure-quit)
(define-key ende-speech-configure-mode-map (kbd "RET") #'ende-speech--configure-save)

(define-derived-mode ende-speech-configure-mode special-mode "Ende-Speech-Configure"
  "Read-only settings view for realtime speech dictation."
  (dolist (variable ende-speech--configuration-variables)
    (set (make-local-variable variable) (default-value variable))))

(defvar ende-speech-transcript-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'ende-speech--accept)
    (define-key map (kbd "C-c C-k") #'ende-speech--reject)
    map)
  "Keymap for `ende-speech-transcript-mode'.")

(define-derived-mode ende-speech-transcript-mode fundamental-mode "Ende-Speech"
  "Editable transcript buffer for realtime speech dictation."
  (add-hook 'pre-command-hook #'ende-speech--before-command nil t)
  (add-hook 'post-command-hook #'ende-speech--after-command nil t)
  (setq-local header-line-format '(:eval (ende-speech--header-line))))

(defun ende-speech--header-line ()
  "Return the status line for the current transcript buffer."
  (let ((session (and (boundp 'ende-speech--active-session)
                      ende-speech--active-session)))
    (if (and session (eq (ende-speech--session-buffer session)
                         (current-buffer)))
        (format "Ende speech — %s | C-c C-c accept | C-c C-k cancel"
                (ende-speech--session-status session))
      "Ende speech — closed")))

(defun ende-speech--configuration-buffer ()
  "Return the live speech configuration buffer, creating it if needed."
  (get-buffer-create "*ende-speech-configure*"))

(defun ende-speech--render-configuration ()
  "Render the speech settings into the current configuration buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (format "%-8s - %-18s %s\n"
                    "a" "API Path:" ende-speech-api-path))
    (insert
     (format "%-8s - %-18s " "e" "Encryption:")
     (propertize
      (if ende-speech-encryption
          "On - TLS with certificate verification"
        "Off - voice sent unencrypted!")
      'face (if ende-speech-encryption 'success 'error))
     "\n")
    (insert (format "%-8s - %-18s %s\n"
                    "i" "Input:" (or ende-speech-input-device
                                     "System default")))
    (insert (format "%-8s - %-18s %s\n"
                    "p" "Port:" ende-speech-port))
    (insert (format "%-8s - %-18s %s\n"
                    "s" "Server:" ende-speech-server))
    (insert "\n"
            (format "%-8s - Quit without saving\n" "q/Esc")
            (format "%-8s - Save and quit\n" "Enter"))))

(defun ende-speech--configure-save ()
  "Apply pending speech settings for this Emacs session and close the buffer."
  (interactive)
  (ende-speech--server-url)
  (dolist (variable ende-speech--configuration-variables)
    (set-default variable (symbol-value variable)))
  (quit-window t))

(defun ende-speech--configure-quit ()
  "Discard pending speech settings and close the configuration buffer."
  (interactive)
  (quit-window t))

(defun ende-speech-configure ()
  "Open the speech configuration buffer."
  (interactive)
  (let ((buffer (ende-speech--configuration-buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'ende-speech-configure-mode)
        (ende-speech-configure-mode))
      (ende-speech--render-configuration))
    (pop-to-buffer buffer)))

(defun ende-speech--json-value (key object)
  "Return KEY's value from JSON alist OBJECT, accepting symbol or string keys."
  (and (listp object)
       (or (assq key object)
           (assoc (symbol-name key) object))))

(defun ende-speech--json-get (key object)
  "Return the value for symbol KEY from JSON alist OBJECT."
  (let ((entry (ende-speech--json-value key object)))
    (cdr entry)))

(defun ende-speech--pactl-output (pactl &rest arguments)
  "Run PACTL with ARGUMENTS and return (STATUS . OUTPUT)."
  (with-temp-buffer
    (let ((status
           (condition-case nil
               (apply #'process-file pactl nil t nil arguments)
             (error nil))))
      (cons status (buffer-string)))))

(defun ende-speech--json-source-devices (output)
  "Parse pactl JSON OUTPUT into (t . DEVICES), or return nil if invalid."
  (condition-case nil
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (json-key-type 'symbol)
             (sources (json-read-from-string output)))
        (when (listp sources)
          (cons t
                (delq nil
                      (mapcar
                       (lambda (source)
                         (let ((name (ende-speech--json-get 'name source))
                               (description
                                (ende-speech--json-get 'description source)))
                           (when (and (stringp name)
                                      (not (string-empty-p name)))
                             (cons (if (and (stringp description)
                                            (not (string-empty-p description)))
                                       description
                                     name)
                                   name))))
                       sources)))))
    (error nil)))

(defun ende-speech--text-source-devices (output)
  "Parse legacy text-format pactl OUTPUT into (t . DEVICES), or nil."
  (let ((case-fold-search nil)
        devices)
    (dolist (source (split-string output "^Source #[^\n]*\n" t))
      (let ((name (and (string-match "^[ \t]*Name:[ \t]*\\(.*\\)$" source)
                       (string-trim (match-string 1 source))))
            (description
             (and (string-match "^[ \t]*Description:[ \t]*\\(.*\\)$" source)
                  (string-trim (match-string 1 source)))))
        (when (and name (not (string-empty-p name)))
          (push (cons (if (and description (not (string-empty-p description)))
                          description name)
                      name)
                devices))))
    (cons t (nreverse devices))))

(defun ende-speech--device-candidates (devices)
  "Build friendly completion candidates from description/name pairs DEVICES."
  (let ((labels (mapcar #'car devices)))
    (cons
     (cons "System default" nil)
     (mapcar
      (lambda (device)
        (let ((label (car device))
              (name (cdr device)))
          (cons (if (or (equal label "System default")
                        (> (cl-count label labels :test #'equal) 1))
                    (format "%s [%s]" label name)
                  label)
                name)))
      devices))))

(defun ende-speech--discover-input-devices ()
  "Return display-name/source-name pairs discovered using pactl.
Try JSON first, then older pactl's normal text listing."
  (let ((default-directory temporary-file-directory)
        pactl json-result text-result)
    (setq pactl (executable-find "pactl"))
    (unless pactl
      (user-error "Cannot list audio inputs: pactl is unavailable"))
    (setq json-result
          (ende-speech--pactl-output pactl "--format=json" "list" "sources"))
    (let ((devices
           (and (integerp (car json-result))
                (= (car json-result) 0)
                (ende-speech--json-source-devices (cdr json-result)))))
      (unless devices
        (setq text-result (ende-speech--pactl-output pactl "list" "sources"))
        (setq devices
              (and (integerp (car text-result))
                   (= (car text-result) 0)
                   (ende-speech--text-source-devices (cdr text-result)))))
      (unless devices
        (user-error "Cannot list audio inputs: pactl enumeration failed"))
      (ende-speech--device-candidates (cdr devices)))))

(defun ende-speech--validate-api-path (path)
  "Return PATH if it is a valid URL path, otherwise signal a user error."
  (let ((case-fold-search nil))
    (unless (and (stringp path)
                 (string-match-p
                  "\\`/\\(?:[A-Za-z0-9._~!$&'()*+,;=:@/-]\\|%[0-9A-Fa-f]\\{2\\}\\)*\\'"
                  path))
      (user-error "API path must be an absolute URL path; percent-encode special characters")))
  path)

(defun ende-speech--validate-server (host)
  "Return HOST if it is a hostname or IP address, otherwise signal a user error."
  (let ((address (if (and (stringp host)
                          (string-prefix-p "[" host)
                          (string-suffix-p "]" host))
                     (substring host 1 -1)
                   host))
        (case-fold-search t))
    (unless
        (and (stringp address)
             (if (string-match-p ":" address)
                 (and (string-match-p "\\`[[:xdigit:]:.]+\\'" address)
                      (network-lookup-address-info address 'ipv6 'numeric))
               (and (equal host address)
                    (<= (length (string-remove-suffix "." address)) 253)
                    (cl-every
                     (lambda (label)
                       (string-match-p
                        "\\`[a-z0-9]\\(?:[a-z0-9-]\\{0,61\\}[a-z0-9]\\)?\\'"
                        label))
                     (split-string (string-remove-suffix "." address) "\\."))
                    (or (not (string-match-p
                              "\\`[0-9]+\\(?:\\.[0-9]+\\)\\{3\\}\\.?\\'" address))
                        (network-lookup-address-info
                         (string-remove-suffix "." address) 'ipv4 'numeric)))))
      (user-error "Server must be a hostname or IP address, not a URL or host:port")))
  host)

(defun ende-speech--validate-port (port)
  "Return PORT if it is an integer from 1 to 65535, otherwise signal a user error."
  (unless (and (integerp port) (<= 1 port 65535))
    (user-error "Port must be an integer from 1 to 65535"))
  port)

(defun ende-speech--configure-api-path ()
  "Set the realtime speech API path."
  (interactive)
  (setq ende-speech-api-path
        (ende-speech--validate-api-path
         (read-string "API Path: " ende-speech-api-path)))
  (ende-speech--render-configuration))

(defun ende-speech--configure-encryption ()
  "Configure encryption for speech WebSocket connections."
  (interactive)
  (setq ende-speech-encryption
        (equal (completing-read
                "Encryption: " '("Off" "On") nil t nil nil
                (if ende-speech-encryption "On" "Off"))
               "On"))
  (ende-speech--render-configuration))

(defun ende-speech--configure-input ()
  "Choose a PulseAudio/PipeWire input device."
  (interactive)
  (let* ((devices (ende-speech--discover-input-devices))
         (names (mapcar #'car devices))
         (current
          (or (car (cl-find ende-speech-input-device (cdr devices)
                            :key #'cdr :test #'equal))
              "System default"))
         (completion-ignore-case t)
         (completion-styles '(substring))
         (choice (completing-read "Input: " names nil t nil nil current))
         (selected (assoc choice devices)))
    (unless selected
      (user-error "Select an available audio input"))
    (setq ende-speech-input-device (cdr selected))
    (ende-speech--render-configuration)))

(defun ende-speech--configure-server ()
  "Set the realtime speech server host."
  (interactive)
  (setq ende-speech-server
        (ende-speech--validate-server
         (read-string "Server: " ende-speech-server)))
  (ende-speech--render-configuration))

(defun ende-speech--configure-port ()
  "Set the realtime speech server port."
  (interactive)
  (setq ende-speech-port
        (ende-speech--validate-port
         (read-number "Port: " ende-speech-port)))
  (ende-speech--render-configuration))

(defun ende-speech--session-active-p (session)
  "Return non-nil if SESSION is the current non-terminal speech session."
  (and session
       (eq session ende-speech--active-session)
       (not (eq (ende-speech--session-state session) 'terminal))))

(defun ende-speech--transcript-text (session)
  "Return the visible transcript text for SESSION."
  (let ((buffer (ende-speech--session-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun ende-speech--before-command ()
  "Remember point and the buffer modification tick before a command."
  (setq ende-speech--command-point (point)
        ende-speech--command-tick (buffer-chars-modified-tick)))

(defun ende-speech--after-command ()
  "Clear recognition after point moves without a buffer edit."
  (let ((session ende-speech--active-session))
    (when (and (ende-speech--session-active-p session)
               ende-speech--command-point
               (/= (point) ende-speech--command-point)
               (= (buffer-chars-modified-tick) ende-speech--command-tick))
      (ende-speech--clear-recognition session)))
  (setq ende-speech--command-point nil
        ende-speech--command-tick nil))

(defun ende-speech--clear-recognition (session)
  "Clear SESSION's recognition context and pause audio until acknowledged."
  (unless (ende-speech--session-clear-pending session)
    (setf (ende-speech--session-clear-pending session) t
          (ende-speech--session-needs-separator session) t)
    (ende-speech--set-status session "Clearing...")
    (condition-case err
        (ende-speech--send-json
         (ende-speech--session-websocket session)
         '((type . "input_audio_buffer.clear")))
      (error
       (ende-speech--fail
        session (format "recognition reset failed: %s"
                        (error-message-string err)))))))

(defun ende-speech--handle-delta (session delta)
  "Insert DELTA at point in SESSION's transcript buffer."
  (let ((buffer (ende-speech--session-buffer session)))
    (when (and (buffer-live-p buffer) (not (string-empty-p delta)))
      (with-current-buffer buffer
        (when (and (ende-speech--session-needs-separator session)
                   (> (point) (point-min))
                   (eq (char-syntax (char-before)) ?w)
                   (not (string-match-p "\\`[ \t\n\r,.!?:;]" delta)))
          (insert " "))
        (insert delta)
        (setf (ende-speech--session-needs-separator session) nil)
        (force-mode-line-update)))))

(defun ende-speech--set-status (session status)
  "Set SESSION's status line to STATUS."
  (let ((buffer (ende-speech--session-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setf (ende-speech--session-status session) status)
        (force-mode-line-update)))))

(defun ende-speech--websocket-open-p (websocket)
  "Return non-nil when WEBSOCKET can currently send data."
  (and websocket (eq (ende-websocket-state websocket) 'open)))

(defun ende-speech--send-json (websocket object)
  "Send OBJECT as a JSON text frame over WEBSOCKET."
  (ende-websocket-send-text websocket (json-encode object)))

(defun ende-speech--start-recorder (session)
  "Start parec for SESSION after protocol configuration is acknowledged."
  (let* ((default-directory temporary-file-directory)
         (command
          (append
           (list (or (executable-find "parec")
                     (user-error "parec is unavailable; cannot record audio"))
                 "--raw" "--format=s16le" "--channels=1" "--rate=16000")
           (when ende-speech-input-device
             (list (concat "--device=" ende-speech-input-device)))))
         (stderr-buffer (generate-new-buffer " *ende-speech-stderr*"))
         (coding-system-for-read 'no-conversion)
         (coding-system-for-write 'no-conversion)
         process)
    (setf (ende-speech--session-recorder-stderr session) stderr-buffer)
    (unwind-protect
        (progn
          (setq process
                (make-process
                 :name "ende-speech-parec"
                 :command command
                 :connection-type 'pipe
                 :stderr stderr-buffer
                 :noquery t
                 :coding 'no-conversion
                 :filter
                 (lambda (_process chunk)
                   (when (and (ende-speech--session-active-p session)
                              (not (ende-speech--session-clear-pending session)))
                     (let ((websocket
                            (ende-speech--session-websocket session)))
                       (condition-case err
                           (ende-websocket-send-binary
                            websocket (encode-coding-string chunk 'binary))
                         (error
                          (ende-speech--fail
                           session
                           (format "audio forwarding failed: %s"
                                   (error-message-string err))))))))
                 :sentinel
                 (lambda (_process event)
                   (when (and (ende-speech--session-active-p session)
                              (not (string-match-p "running" event)))
                     (ende-speech--fail
                      session (format "recorder stopped (%s)"
                                      (string-trim event)))))))
          (setf (ende-speech--session-recorder session) process)
          (set-process-coding-system process 'no-conversion 'no-conversion)
          (setf (ende-speech--session-state session) 'listening))
      (unless process
        (when (buffer-live-p stderr-buffer)
          (kill-buffer stderr-buffer))))))

(defun ende-speech--stop-recorder (session)
  "Stop SESSION's recorder if it is still running."
  (let ((process (ende-speech--session-recorder session)))
    (when (and process (process-live-p process))
      (condition-case nil
          (delete-process process)
        (error nil)))
    (setf (ende-speech--session-recorder session) nil)
    (let ((stderr-buffer (ende-speech--session-recorder-stderr session)))
      (when (buffer-live-p stderr-buffer)
        (kill-buffer stderr-buffer))
      (setf (ende-speech--session-recorder-stderr session) nil))))

(defun ende-speech--close-websocket (session)
  "Clear pending audio and close SESSION's WebSocket when usable."
  (let ((websocket (ende-speech--session-websocket session)))
    (when websocket
      (when (ende-speech--websocket-open-p websocket)
        (condition-case nil
            (ende-speech--send-json
             websocket '((type . "input_audio_buffer.clear")))
          (error (message "ende speech: could not clear audio before closing"))))
      ;; Graceful close drains accepted output, including the audio clear.
      (condition-case nil
          (ende-websocket-close websocket)
        (error (ende-websocket-abort websocket))))
    (setf (ende-speech--session-websocket session) nil)))

(defun ende-speech--clear-session (session kill-buffer)
  "Finish cleanup for SESSION; KILL-BUFFER controls transcript-buffer killing."
  (let* ((buffer (ende-speech--session-buffer session))
         (marker (ende-speech--session-marker session))
         (source (and (markerp marker) (marker-buffer marker)))
         (source-window (ende-speech--session-source-window session))
         (transcript-window (ende-speech--session-transcript-window session)))
    (when (and kill-buffer (window-live-p transcript-window)
               (not (eq transcript-window source-window))
               (eq (window-buffer transcript-window) buffer)
               (not (window-dedicated-p transcript-window)))
      (delete-window transcript-window))
    (when (and kill-buffer (buffer-live-p buffer))
      (condition-case nil (kill-buffer buffer) (error nil)))
    (when (markerp marker)
      (set-marker marker nil))
    (when (eq session ende-speech--active-session)
      (setq ende-speech--active-session nil))
    (when (and (buffer-live-p source)
               (window-live-p source-window))
      (select-window source-window)
      (unless (eq (window-buffer source-window) source)
        (set-window-buffer source-window source)))))

(defun ende-speech--insert-at-marker (session text)
  "Insert TEXT at SESSION's saved source marker."
  (let* ((marker (ende-speech--session-marker session))
         (buffer (and (markerp marker) (marker-buffer marker))))
    (unless (and (buffer-live-p buffer) (marker-position marker))
      (error "Original destination buffer or marker no longer exists"))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (goto-char marker)
        (insert text)))))

(defun ende-speech--terminate (session accept &optional report kill-buffer)
  "Terminate SESSION, accepting its transcript when ACCEPT is non-nil.
REPORT, when non-nil, is shown to the user; KILL-BUFFER controls buffer killing."
  (when (ende-speech--session-active-p session)
    ;; Terminal state comes first so process and WebSocket callbacks are inert.
    (setf (ende-speech--session-state session) 'terminal)
    (let ((text (and accept (ende-speech--transcript-text session)))
          inserted insertion-error)
      (unwind-protect
          (progn
            (ende-speech--stop-recorder session)
            (ende-speech--close-websocket session)
            (when accept
              (condition-case err
                  (progn
                    (ende-speech--insert-at-marker session text)
                    (setq inserted t))
                ((error quit) (setq insertion-error err)))))
        (ende-speech--clear-session
         session (and kill-buffer (or (not accept) inserted))))
      (cond
       (insertion-error
        (user-error "Insertion failed; transcript preserved in %s: %s"
                    (buffer-name (ende-speech--session-buffer session))
                    (error-message-string insertion-error)))
       (report (message "ende speech: %s" report))))))

(defun ende-speech--fail (session reason)
  "Stop SESSION after reporting REASON."
  (when (ende-speech--session-active-p session)
    (ende-speech--terminate session nil reason t)))

(defun ende-speech--transcript-buffer-killed ()
  "Cancel the active session if its transcript buffer is killed."
  (let ((session ende-speech--active-session))
    (when (and (ende-speech--session-active-p session)
               (eq (ende-speech--session-buffer session) (current-buffer)))
      (ende-speech--terminate session nil nil nil))))

(defun ende-speech--current-session ()
  (let ((session ende-speech--active-session))
    (unless (and (ende-speech--session-active-p session)
                 (eq (ende-speech--session-buffer session) (current-buffer)))
      (user-error "No active speech session"))
    session))

(defun ende-speech--accept ()
  "Stop recording and insert the currently visible transcript."
  (interactive)
  (ende-speech--terminate (ende-speech--current-session) t nil t))

(defun ende-speech--reject ()
  "Stop recording and discard the currently visible transcript."
  (interactive)
  (ende-speech--terminate (ende-speech--current-session) nil nil t))

(defun ende-speech--handle-event (session event)
  "Handle the supported realtime transcription EVENT for SESSION."
  (when (ende-speech--session-active-p session)
    (let ((type (ende-speech--json-get 'type event)))
      (cond
       ((equal type "session.created")
        (when (eq (ende-speech--session-state session) 'connecting)
          (setf (ende-speech--session-state session) 'configuring)
          (ende-speech--set-status session "Configuring...")
          (condition-case err
              (ende-speech--send-json
               (ende-speech--session-websocket session)
               '((type . "session.update") (session . ((sample_rate . 16000)))))
            (error
             (ende-speech--fail
              session (format "session setup failed: %s"
                              (error-message-string err)))))))
       ((equal type "session.updated")
        (when (eq (ende-speech--session-state session) 'configuring)
          (condition-case err
              (progn
                (ende-speech--start-recorder session)
                (ende-speech--set-status session "Listening..."))
            (error
             (ende-speech--fail
              session (format "could not start microphone: %s"
                              (error-message-string err)))))))
       ((equal type "input_audio_buffer.cleared")
        (setf (ende-speech--session-clear-pending session) nil)
        (ende-speech--set-status session "Listening..."))
       ((equal type "conversation.item.input_audio_transcription.delta")
        (let ((delta (ende-speech--json-get 'delta event)))
          (when (and (not (ende-speech--session-clear-pending session))
                     (stringp delta))
            (ende-speech--handle-delta session delta))))
       ((equal type "error")
        (let* ((error-object (ende-speech--json-get 'error event))
               (message-text (or (ende-speech--json-get 'message error-object)
                                 (ende-speech--json-get 'message event)
                                 "speech server reported an error")))
          (ende-speech--fail session (format "%s" message-text))))))))

(defun ende-speech--websocket-message (session websocket type text)
  "Decode and dispatch TEXT for SESSION, ignoring non-text TYPE messages."
  (when (and (ende-speech--session-active-p session)
             (eq websocket (ende-speech--session-websocket session))
             (eq type 'text))
    (condition-case nil
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (json-key-type 'symbol)
               (event (json-read-from-string text)))
          (ende-speech--handle-event session event))
      (error nil))))

(defun ende-speech--server-url ()
  "Validate the speech settings and return the WebSocket endpoint URL."
  (let ((host (ende-speech--validate-server ende-speech-server)))
    (ende-speech--validate-port ende-speech-port)
    (ende-speech--validate-api-path ende-speech-api-path)
    (when (and (string-match-p ":" host)
               (not (string-prefix-p "[" host)))
      (setq host (concat "[" host "]")))
    (format "%s://%s:%d%s"
            (if ende-speech-encryption "wss" "ws")
            host ende-speech-port ende-speech-api-path)))

(defun ende-speech-start-talking ()
  "Start a realtime speech session at the current point."
  (interactive)
  (when ende-speech--active-session
    (user-error "A speech session is already active"))
  (let ((default-directory temporary-file-directory))
    (unless (executable-find "parec")
      (user-error "parec is unavailable; cannot record audio")))
  (let* ((source-window (selected-window))
         (source-buffer (current-buffer))
         (marker (copy-marker (point) t))
         (buffer (generate-new-buffer "*ende-speech*"))
         (session (ende-speech--make-session
                   :state 'connecting :marker marker :buffer buffer
                   :source-window source-window :status "Connecting...")))
    (setq ende-speech--active-session session)
    (with-current-buffer buffer
      (ende-speech-transcript-mode)
      (add-hook 'kill-buffer-hook #'ende-speech--transcript-buffer-killed nil t))
    (condition-case err
        (setf (ende-speech--session-websocket session)
              (ende-websocket-connect
               (ende-speech--server-url)
               :on-message
               (lambda (websocket type text)
                 (ende-speech--websocket-message session websocket type text))
               :on-close
               (lambda (websocket info)
                 (when (and (ende-speech--session-active-p session)
                            (eq websocket (ende-speech--session-websocket session)))
                   (ende-speech--fail
                    session (format "WebSocket closed (%s, code %s): %s"
                                    (plist-get info :cause)
                                    (plist-get info :code)
                                    (plist-get info :reason)))))))
      (error
       (ende-speech--terminate
        session nil (format "could not connect: %s" (error-message-string err)) t)
       (signal (car err) (cdr err))))
    (let ((window (display-buffer
                   buffer
                   '((display-buffer-pop-up-window)
                     (inhibit-same-window . t)))))
      (unless (and (window-live-p window)
                   (not (eq window source-window)))
        (ende-speech--terminate session nil nil t)
        (user-error "Cannot show transcript without hiding the source buffer"))
      (setf (ende-speech--session-transcript-window session) window)
      (select-window window)
      (unless (eq (window-buffer source-window) source-buffer)
        (set-window-buffer source-window source-buffer)))))

(provide 'ende-speech)

;;; ende-speech.el ends here
