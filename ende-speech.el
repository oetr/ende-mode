;;; ende-speech.el --- Local realtime speech dictation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter
;; Author: Peter
;; Version: 0.3
;; Keywords: convenience, speech

;;; Commentary:

;; Local microphone capture and realtime transcription, independent of Ende's
;; job and LLM machinery.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'websocket)

(defgroup ende-speech nil
  "Local realtime speech dictation."
  :group 'ende)

(defcustom ende-speech-input-device nil
  "PulseAudio/PipeWire source identifier, or nil for the system default."
  :type '(choice (const :tag "System default" nil) string)
  :group 'ende-speech)

(defcustom ende-speech-server "127.0.0.1"
  "Host or IP address of the realtime speech server."
  :type 'string
  :group 'ende-speech)

(defcustom ende-speech-port 8081
  "Port of the realtime speech server."
  :type 'integer
  :group 'ende-speech)

;;; Code:

(cl-defstruct (ende-speech--session
               (:constructor ende-speech--make-session))
  state marker buffer websocket recorder recorder-stderr finalized partial status)


(defvar ende-speech--active-session nil
  "The one active realtime speech session, if any.")

(defvar ende-speech-configure-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") #'ende-speech--configure-input)
    (define-key map (kbd "s") #'ende-speech--configure-server)
    (define-key map (kbd "p") #'ende-speech--configure-port)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "<escape>") #'quit-window)
    map)
  "Keymap for `ende-speech-configure-mode'.")

(define-derived-mode ende-speech-configure-mode special-mode "Ende-Speech-Configure"
  "Read-only settings view for realtime speech dictation.")

(defvar ende-speech-transcript-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'ende-speech--accept)
    (define-key map (kbd "C-c C-k") #'ende-speech--reject)
    map)
  "Keymap for `ende-speech-transcript-mode'.")

(define-derived-mode ende-speech-transcript-mode special-mode "Ende-Speech"
  "Read-only transcript view for realtime speech dictation."
  (setq-local header-line-format '(:eval (ende-speech--header-line))))

(defun ende-speech--header-line ()
  "Return the status line for the current transcript buffer."
  (let ((session (and (boundp 'ende-speech--active-session)
                      ende-speech--active-session)))
    (format " Ende speech — %s"
            (if (and session (eq (ende-speech--session-buffer session)
                                 (current-buffer)))
                (ende-speech--session-status session)
              "Closed"))))

(defun ende-speech--configuration-buffer ()
  "Return the live speech configuration buffer, creating it if needed."
  (get-buffer-create "*ende-speech-configure*"))

(defun ende-speech--render-configuration ()
  "Render the speech settings into the current configuration buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (format "%-8s - %-18s %s\n"
                    "i" "Input:" (or ende-speech-input-device
                                     "System default")))
    (insert (format "%-8s - %-18s %s\n"
                    "s" "Server:" ende-speech-server))
    (insert (format "%-8s - %-18s %s\n"
                    "p" "Port:" ende-speech-port))))

(defun ende-speech-configure ()
  "Open the speech configuration buffer."
  (interactive)
  (let ((buffer (ende-speech--configuration-buffer)))
    (with-current-buffer buffer
      (ende-speech-configure-mode)
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
        (read-string "Server: " ende-speech-server))
  (ende-speech--render-configuration))

(defun ende-speech--configure-port ()
  "Set the realtime speech server port."
  (interactive)
  (let ((port (read-number "Port: " ende-speech-port)))
    (unless (and (integerp port) (<= 1 port 65535))
      (user-error "Port must be an integer from 1 to 65535"))
    (setq ende-speech-port port)
    (ende-speech--render-configuration)))

(defun ende-speech--session-active-p (session)
  "Return non-nil if SESSION is the current non-terminal speech session."
  (and session
       (eq session ende-speech--active-session)
       (not (eq (ende-speech--session-state session) 'terminal))))

(defun ende-speech--transcript-text (session)
  "Return the visible transcript text for SESSION."
  (let ((buffer (ende-speech--session-buffer session)))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (buffer-substring-no-properties (point-min) (point-max)))
      (concat (ende-speech--session-finalized session)
              (ende-speech--session-partial session)))))

(defun ende-speech--join-transcript (first second)
  "Join transcript fragments FIRST and SECOND with sensible spacing."
  (cond
   ((string-empty-p first) second)
   ((string-empty-p second) first)
   ((or (string-match-p "[ \t\n\\r]\\'" first)
        (string-match-p "\\`[ \t\n\\r]" second))
    (concat first second))
   (t (concat first " " second))))

(defun ende-speech--render-transcript (session)
  "Render SESSION's finalized and partial transcript."
  (when (buffer-live-p (ende-speech--session-buffer session))
    (with-current-buffer (ende-speech--session-buffer session)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (ende-speech--join-transcript
                 (ende-speech--session-finalized session)
                 (ende-speech--session-partial session))))
      (force-mode-line-update))))

(defun ende-speech--set-status (session status)
  "Set SESSION's status line to STATUS."
  (let ((buffer (ende-speech--session-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setf (ende-speech--session-status session) status)
        (force-mode-line-update)))))

(defun ende-speech--websocket-open-p (websocket)
  "Return non-nil when WEBSOCKET can currently send data."
  (and websocket
       (condition-case nil
           (websocket-openp websocket)
         (error nil))))

(defun ende-speech--send-json (websocket object)
  "Send OBJECT as a JSON text frame over WEBSOCKET."
  (websocket-send-text websocket (json-encode object)))

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
                   (when (ende-speech--session-active-p session)
                     (let ((websocket
                            (ende-speech--session-websocket session)))
                       (when (ende-speech--websocket-open-p websocket)
                         (condition-case err
                             (websocket-send
                              websocket
                              (make-websocket-frame
                               :opcode 'binary
                               :payload (encode-coding-string chunk 'binary)
                               :completep t))
                           (error
                            (ende-speech--fail
                             session
                             (format "audio forwarding failed: %s"
                                     (error-message-string err)))))))))
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
          (error nil)))
      (condition-case nil
          (websocket-close websocket)
        (error nil)))
    (setf (ende-speech--session-websocket session) nil)))

(defun ende-speech--clear-session (session kill-buffer)
  "Finish cleanup for SESSION; KILL-BUFFER controls transcript-buffer killing."
  (let* ((buffer (ende-speech--session-buffer session))
         (marker (ende-speech--session-marker session))
         (source (and (markerp marker) (marker-buffer marker))))
    (when (and kill-buffer (buffer-live-p buffer))
      (condition-case nil (kill-buffer buffer) (error nil)))
    (when (markerp marker)
      (set-marker marker nil))
    (when (eq session ende-speech--active-session)
      (setq ende-speech--active-session nil))
    (when (and kill-buffer (buffer-live-p source))
      (switch-to-buffer source))))

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
          insertion-error)
      (unwind-protect
          (progn
            (ende-speech--stop-recorder session)
            (ende-speech--close-websocket session)
            (when accept
              (condition-case err
                  (ende-speech--insert-at-marker session text)
                (error (setq insertion-error err)))))
        (ende-speech--clear-session session kill-buffer))
      (cond
       (insertion-error
        (user-error "Speech stopped, but transcript insertion failed: %s"
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

(defun ende-speech--accept ()
  "Stop recording and insert the currently visible transcript."
  (interactive)
  (let ((session ende-speech--active-session))
    (unless (and (ende-speech--session-active-p session)
                 (eq (ende-speech--session-buffer session) (current-buffer)))
      (user-error "No active speech session"))
    (ende-speech--terminate session t nil t)))

(defun ende-speech--reject ()
  "Stop recording and discard the currently visible transcript."
  (interactive)
  (let ((session ende-speech--active-session))
    (unless (and (ende-speech--session-active-p session)
                 (eq (ende-speech--session-buffer session) (current-buffer)))
      (user-error "No active speech session"))
    (ende-speech--terminate session nil nil t)))

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
              (ende-speech--start-recorder session)
            (error
             (ende-speech--fail
              session (format "could not start microphone: %s"
                              (error-message-string err)))))))
       ((equal type "conversation.item.input_audio_transcription.delta")
        (let ((delta (ende-speech--json-get 'delta event)))
          (when (stringp delta)
            (setf (ende-speech--session-partial session)
                  (concat (ende-speech--session-partial session) delta))
            (ende-speech--render-transcript session))))
       ((equal type "conversation.item.input_audio_transcription.completed")
        (let ((transcript (ende-speech--json-get 'transcript event)))
          (when (stringp transcript)
            (setf (ende-speech--session-finalized session)
                  (ende-speech--join-transcript
                   (ende-speech--session-finalized session) transcript)
                  (ende-speech--session-partial session) "")
            (ende-speech--render-transcript session))))
       ((equal type "error")
        (let* ((error-object (ende-speech--json-get 'error event))
               (message-text (or (ende-speech--json-get 'message error-object)
                                 (ende-speech--json-get 'message event)
                                 "speech server reported an error")))
          (ende-speech--fail session (format "%s" message-text))))))))

(defun ende-speech--websocket-message (session websocket frame)
  "Decode and dispatch FRAME for SESSION, if it is still active."
  (when (and (ende-speech--session-active-p session)
             (eq websocket (ende-speech--session-websocket session)))
    (condition-case nil
        (let* ((text (websocket-frame-text frame))
               (json-object-type 'alist)
               (json-array-type 'list)
               (json-key-type 'symbol)
               (event (json-read-from-string text)))
          (ende-speech--handle-event session event))
      (error nil))))

(defun ende-speech--server-url ()
  "Return the fixed realtime WebSocket endpoint URL."
  (let ((host ende-speech-server))
    (when (and (string-match-p ":" host)
               (not (string-prefix-p "[" host)))
      (setq host (concat "[" host "]")))
    (format "ws://%s:%d/v1/audio/transcriptions/realtime"
            host ende-speech-port)))

(defun ende-speech-start-talking ()
  "Start a realtime speech session at the current point."
  (interactive)
  (when ende-speech--active-session
    (user-error "A speech session is already active"))
  (let ((default-directory temporary-file-directory))
    (unless (executable-find "parec")
      (user-error "parec is unavailable; cannot record audio")))
  (let* ((marker (copy-marker (point) t))
         (buffer (get-buffer-create "*ende-speech*"))
         (session (ende-speech--make-session
                   :state 'connecting :marker marker :buffer buffer
                   :finalized "" :partial "" :status "Connecting...")))
    (setq ende-speech--active-session session)
    (with-current-buffer buffer
      (ende-speech-transcript-mode)
      (add-hook 'kill-buffer-hook #'ende-speech--transcript-buffer-killed nil t)
      (let ((inhibit-read-only t)) (erase-buffer)))
    (condition-case err
        (setf (ende-speech--session-websocket session)
              (websocket-open
               (ende-speech--server-url)
               :nowait t
               :on-open (lambda (_websocket) nil)
               :on-message
               (lambda (websocket frame)
                 (ende-speech--websocket-message session websocket frame))
               :on-close
               (lambda (_websocket)
                 (when (ende-speech--session-active-p session)
                   (ende-speech--fail session "WebSocket connection closed")))
               :on-error
               (lambda (_websocket _type error)
                 (when (ende-speech--session-active-p session)
                   (ende-speech--fail
                    session (format "WebSocket error: %s"
                                    (if (and (consp error)
                                             (symbolp (car error)))
                                        (error-message-string error)
                                      error)))))))
      (error
       (ende-speech--terminate
        session nil (format "could not connect: %s" (error-message-string err)) t)
       (signal (car err) (cdr err))))
    (switch-to-buffer buffer)))

(provide 'ende-speech)

;;; ende-speech.el ends here
