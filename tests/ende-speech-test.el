;;; ende-speech-test.el --- Realtime speech tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ende-speech)
(require 'ende)

(defvar ende-speech-test--open-args nil)
(defvar ende-speech-test--sent nil)
(defvar ende-speech-test--closed nil)
(defvar ende-speech-test--killed-recorder nil)
(defvar ende-speech-test--made-process nil)
(defvar ende-speech-test--switched-buffers nil)
(defvar ende-speech-test--switch-target nil)

(defvar ende-speech-test--transcript-buffer nil)

(defun ende-speech-test--with-session (function)
  "Start a mocked speech session and call FUNCTION with its session."
  (let ((ende-speech--active-session nil)
        (buffer (generate-new-buffer " *ende-speech-source*"))
        (speech-buffer nil)
        (websocket 'mock-websocket)
        (recorder 'mock-recorder)
        (ende-speech-server "127.0.0.1")
        (ende-speech-port 8081)
        (ende-speech-input-device nil)
        (ende-speech-test--open-args nil)
        (ende-speech-test--sent nil)
        (ende-speech-test--closed nil)
        (ende-speech-test--killed-recorder nil)
        (ende-speech-test--made-process nil)
        (ende-speech-test--switched-buffers nil)
        (ende-speech-test--switch-target nil)
        (ende-speech-test--transcript-buffer nil))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "abcd")
            (goto-char 3)
            (cl-letf (((symbol-function 'executable-find)
                       (lambda (_) "/mock/parec"))
                      ((symbol-function 'websocket-open)
                       (lambda (url &rest args)
                         (setq ende-speech-test--open-args (cons url args))
                         websocket))
                      ((symbol-function 'switch-to-buffer)
                       (lambda (target &rest _)
                         (push target ende-speech-test--switched-buffers))))
              (ende-speech-start-talking)))
          (setq speech-buffer (get-buffer "*ende-speech*")
                ende-speech-test--transcript-buffer speech-buffer)
          (let ((session ende-speech--active-session))
            (cl-letf (((symbol-function 'websocket-openp)
                       (lambda (_) t))
                      ((symbol-function 'websocket-send-text)
                       (lambda (_ws text)
                         (push (json-parse-string text :object-type 'alist
                                                  :array-type 'list)
                               ende-speech-test--sent)))
                      ((symbol-function 'websocket-close)
                       (lambda (_) (setq ende-speech-test--closed t)))
                      ((symbol-function 'websocket-send)
                       (lambda (_ws frame)
                         (push frame ende-speech-test--sent)))
                      ((symbol-function 'process-live-p)
                       (lambda (_) t))
                      ((symbol-function 'delete-process)
                       (lambda (_) (setq ende-speech-test--killed-recorder t)))
                      ((symbol-function 'make-process)
                       (lambda (&rest args)
                         (setq ende-speech-test--made-process args)
                         recorder))
                      ((symbol-function 'set-process-coding-system) #'ignore)
                      ((symbol-function 'executable-find)
                       (lambda (_) "/mock/parec")))
              (funcall function session buffer speech-buffer))))
      (when (buffer-live-p speech-buffer)
        (with-current-buffer speech-buffer
          (remove-hook 'kill-buffer-hook
                       #'ende-speech--transcript-buffer-killed t))
        (kill-buffer speech-buffer))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (setq ende-speech--active-session nil))))

(defun ende-speech-test--event (session type &rest data)
  "Send an event of TYPE and DATA to SESSION."
  (ende-speech--handle-event session (append `((type . ,type)) data)))

(ert-deftest ende-speech/reload-discovers-modules-and-loads-source-files ()
  (let ((directory (make-temp-file "ende-reload-test" t))
        (real-load (symbol-function 'load))
        loaded)
    (unwind-protect
        (progn
          (dolist (file '("ende-speech.el" "ende-new-module.el"
                          "ende-test.el" "ende-chat-benchmark.el" "ende.el"))
            (with-temp-file (expand-file-name file directory)
              (insert (cond
                       ((equal file "ende-new-module.el")
                        (concat "(require 'ende-speech)\n"
                                "(defvar ende-new-module-loaded t)\n"
                                "(provide 'ende-new-module)\n"))
                       (t (format "(provide '%s)\n"
                                  (file-name-sans-extension file)))))))
          (with-temp-file (expand-file-name "ende-speech.elc" directory)
            (insert "stale compiled file"))
          (cl-letf (((symbol-function 'ende-abort-queue) #'ignore)
                    ((symbol-function 'locate-library)
                     (lambda (_) (expand-file-name "ende.el" directory)))
                    ((symbol-function 'load)
                     (lambda (file &rest arguments)
                       (push (list file arguments) loaded)
                       (apply real-load file arguments)))
                    ((symbol-function 'ende--make-mode-map) #'ignore))
            (ende-reload))
          (let ((load-order (nreverse loaded)))
            (let ((speech-load
                   (cl-find-if (lambda (entry)
                                 (string-suffix-p "/ende-speech.el" (car entry)))
                               load-order))
                  (new-module-load
                   (cl-find-if (lambda (entry)
                                 (string-suffix-p "/ende-new-module.el" (car entry)))
                               load-order)))
              (should speech-load)
              (should new-module-load)
              (should (< (cl-position speech-load load-order)
                         (cl-position new-module-load load-order)))
              (should (nth 2 (cadr speech-load)))
              (should (nth 3 (cadr speech-load))))
            (should-not (cl-some (lambda (entry)
                                   (string-match-p "-test\\.el\\'" (car entry)))
                                 load-order))
            (should-not (cl-some (lambda (entry)
                                   (string-suffix-p "/ende-chat-benchmark.el"
                                                    (car entry)))
                                 load-order))
            (should (string-suffix-p "/ende.el" (caar (last load-order))))
            (should (featurep 'ende-speech))
            (should (bound-and-true-p ende-new-module-loaded))))
      (delete-directory directory t))))

(ert-deftest ende-speech/configuration-rendering-aligned ()
  (let ((buffer (generate-new-buffer " *ende-speech-config-test*"))
        (ende-speech-input-device nil)
        (ende-speech-server "127.0.0.1")
        (ende-speech-port 8081))
    (unwind-protect
        (with-current-buffer buffer
          (ende-speech-configure-mode)
          (ende-speech--render-configuration)
          (should (equal (buffer-string)
                         (concat "i        - Input:             System default\n"
                                 "s        - Server:            127.0.0.1\n"
                                 "p        - Port:              8081\n"))))
      (kill-buffer buffer))))

(ert-deftest ende-speech/input-device-discovery-and-selection ()
  (let ((ende-speech-input-device nil)
        (buffer (generate-new-buffer " *ende-speech-config-test*"))
        (completion-ignore-case nil)
        (completion-styles nil)
        called)
    (unwind-protect
        (with-current-buffer buffer
          (ende-speech-configure-mode)
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (_) "/mock/pactl"))
                    ((symbol-function 'process-file)
                     (lambda (_program _in destination _display &rest _args)
                       (if (eq destination t)
                           (insert "[{\"name\":\"alsa_input.usb-mic\","
                                   "\"description\":\"USB Microphone\"}]")
                         (with-current-buffer destination
                           (insert "[{\"name\":\"alsa_input.usb-mic\","
                                   "\"description\":\"USB Microphone\"}]")))
                       0))
                    ((symbol-function 'completing-read)
                     (lambda (prompt collection predicate require-match
                                     initial history default &rest _)
                       (setq called (list prompt collection predicate
                                          require-match initial history default
                                          completion-styles completion-ignore-case))
                       "System default")))
            (should (equal (ende-speech--discover-input-devices)
                           '(("System default")
                             ("USB Microphone" . "alsa_input.usb-mic"))))
            (ende-speech--configure-input))
          (should (equal ende-speech-input-device nil))
          (should (equal (nth 0 called) "Input: "))
          (should (equal (nth 1 called)
                         '("System default" "USB Microphone")))
          (should (nth 3 called))
          (should (equal (nth 6 called) "System default"))
          (should (equal (nth 7 called) '(substring)))
          (should (nth 8 called)))
      (kill-buffer buffer))))

(ert-deftest ende-speech/input-device-discovery-falls-back-to-pactl-text ()
  (let ((calls 0))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_) "/mock/pactl"))
              ((symbol-function 'process-file)
               (lambda (_program _in _destination _display &rest arguments)
                 (setq calls (1+ calls))
                 (if (member "--format=json" arguments)
                     2
                   (insert "Source #1\n"
                           "\tName: alsa_input.usb-mic\n"
                           "\tDescription: USB Microphone\n"
                           "\tDriver: module-alsa\n")
                   0))))
      (should (equal (ende-speech--discover-input-devices)
                     '(("System default")
                       ("USB Microphone" . "alsa_input.usb-mic"))))
      (should (= calls 2)))))

(ert-deftest ende-speech/input-device-selects-real-identifier ()
  (let ((ende-speech-input-device nil)
        (buffer (generate-new-buffer " *ende-speech-config-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (ende-speech-configure-mode)
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (_) "/mock/pactl"))
                    ((symbol-function 'process-file)
                     (lambda (_program _in destination _display &rest _args)
                       (if (eq destination t)
                           (insert "[{\"name\":\"source.1\","
                                   "\"description\":\"Studio Mic\"}]")
                         (with-current-buffer destination
                           (insert "[{\"name\":\"source.1\","
                                   "\"description\":\"Studio Mic\"}]")))
                       0))
                    ((symbol-function 'completing-read)
                     (lambda (&rest _) "Studio Mic")))
            (ende-speech--configure-input))
          (should (equal ende-speech-input-device "source.1")))
      (kill-buffer buffer))))

(ert-deftest ende-speech/start-captures-marker-and-refuses-second-session ()
  (ende-speech-test--with-session
   (lambda (session source _speech-buffer)
     (should (equal (car ende-speech-test--open-args)
                    "ws://127.0.0.1:8081/v1/audio/transcriptions/realtime"))
     (should (= (marker-position (ende-speech--session-marker session)) 3))
     (should (eq (marker-buffer (ende-speech--session-marker session)) source))
     (should (eq (car ende-speech-test--switched-buffers)
                 (ende-speech--session-buffer session)))
     (should-error
      (with-current-buffer source
        (ende-speech-start-talking))
      :type 'user-error))))

(ert-deftest ende-speech/recorder-forwards-binary-audio-and-device ()
  (ende-speech-test--with-session
   (lambda (session _source _buffer)
     (setq ende-speech-input-device "alsa_input.actual-source")
     (ende-speech-test--event session "session.created")
       (ende-speech-test--event session "session.updated")
       (let* ((command (plist-get ende-speech-test--made-process :command))
              (filter (plist-get ende-speech-test--made-process :filter)))
         (should (member "--format=s16le" command))
         (should (member "--channels=1" command))
         (should (member "--rate=16000" command))
         (should (member "--device=alsa_input.actual-source" command))
         (should-not (member "--device" command))
         (funcall filter 'mock-recorder (unibyte-string 0 127 255))
         (let ((frame (car ende-speech-test--sent)))
           (should (websocket-frame-p frame))
           (should (eq (websocket-frame-opcode frame) 'binary))
           (should (equal (websocket-frame-payload frame)
                          (unibyte-string 0 127 255))))))))

(ert-deftest ende-speech/recorder-starts-only-after-acknowledgement ()
  (ende-speech-test--with-session
   (lambda (session _source _speech-buffer)
     (should-not ende-speech-test--made-process)
     (ende-speech-test--event session "session.created")
     (should-not ende-speech-test--made-process)
     (should (= (length ende-speech-test--sent) 1))
     (should (equal (alist-get 'type (car ende-speech-test--sent))
                    "session.update"))
     (should (equal (alist-get 'sample_rate
                               (alist-get 'session (car ende-speech-test--sent)))
                    16000))
     (ende-speech-test--event session "session.updated")
     (should ende-speech-test--made-process)
     (should (equal (car (plist-get ende-speech-test--made-process
                                    :command))
                    "/mock/parec")))))

(ert-deftest ende-speech/delta-completed-events-render-without-duplication ()
  (ende-speech-test--with-session
   (lambda (session _source buffer)
     (ende-speech-test--event
      session "conversation.item.input_audio_transcription.delta"
      '(delta . "Hello"))
     (should (equal (with-current-buffer buffer (buffer-string)) "Hello"))
     (ende-speech-test--event
      session "conversation.item.input_audio_transcription.completed"
      '(transcript . "Hello"))
     (should (equal (with-current-buffer buffer (buffer-string)) "Hello"))
     (ende-speech-test--event
      session "conversation.item.input_audio_transcription.delta"
      '(delta . "world"))
     (ende-speech-test--event
      session "conversation.item.input_audio_transcription.completed"
      '(transcript . "world"))
     (should (equal (with-current-buffer buffer (buffer-string))
                    "Hello world")))))

(ert-deftest ende-speech/accept-cleans-inserts-at-marker-and-ignores-events ()
  (ende-speech-test--with-session
   (lambda (session source buffer)
     (ende-speech-test--event session "session.created")
     (ende-speech-test--event session "session.updated")
     (ende-speech-test--event
      session "conversation.item.input_audio_transcription.delta"
      '(delta . "Visible partial"))
     (cl-letf (((symbol-function 'switch-to-buffer)
                (lambda (target &rest _)
                  (setq ende-speech-test--switch-target target))))
       (with-current-buffer buffer (ende-speech--accept)))
     (should (eq ende-speech-test--switch-target source))
     (should (equal (with-current-buffer source (buffer-string))
                    "abVisible partialcd"))
     (should ende-speech-test--killed-recorder)
     (should ende-speech-test--closed)
     (should-not ende-speech--active-session)
     (should (member "input_audio_buffer.clear"
                     (mapcar (lambda (entry) (alist-get 'type entry))
                             (cl-remove-if-not #'listp
                                               ende-speech-test--sent))))
     (should-not (member "input_audio_buffer.commit"
                         (mapcar (lambda (entry) (alist-get 'type entry))
                                 (cl-remove-if-not #'listp
                                                   ende-speech-test--sent))))
     (let ((before (with-current-buffer source (buffer-string))))
       (ende-speech-test--event
        session "conversation.item.input_audio_transcription.delta"
        '(delta . "late"))
       (should (equal (with-current-buffer source (buffer-string)) before))))))

(ert-deftest ende-speech/saved-marker-follows-source-edits ()
  (ende-speech-test--with-session
   (lambda (session source buffer)
     (ende-speech-test--event session "session.created")
     (ende-speech-test--event session "session.updated")
     (with-current-buffer source
       (goto-char 2)
       (insert "X")
       (goto-char (point-max))
       (insert "Y"))
     (with-current-buffer buffer (ende-speech--accept))
     (should (equal (with-current-buffer source (buffer-string)) "aXbcdY")))))

(ert-deftest ende-speech/accept-inserts-at-marker-outside-narrowing ()
  (ende-speech-test--with-session
   (lambda (session source buffer)
     (ende-speech-test--event
      session "conversation.item.input_audio_transcription.delta"
      '(delta . "dictated"))
     (with-current-buffer source
       (narrow-to-region 3 5))
     (with-current-buffer buffer (ende-speech--accept))
     (should (equal (with-current-buffer source
                      (save-restriction
                        (widen)
                        (buffer-string)))
                    "abdictatedcd")))))

(ert-deftest ende-speech/reject-cleans-without-insertion ()
  (ende-speech-test--with-session
   (lambda (session source buffer)
     (ende-speech-test--event session "session.created")
     (ende-speech-test--event session "session.updated")
     (ende-speech-test--event
      session "conversation.item.input_audio_transcription.delta"
      '(delta . "discard me"))
     (with-current-buffer buffer (ende-speech--reject))
     (should (equal (with-current-buffer source (buffer-string)) "abcd"))
     (should ende-speech-test--killed-recorder)
     (should ende-speech-test--closed)
     (should-not ende-speech--active-session))))

(ert-deftest ende-speech/killing-transcript-buffer-cancels-recording ()
  (ende-speech-test--with-session
   (lambda (session _source buffer)
     (ende-speech-test--event session "session.created")
     (ende-speech-test--event session "session.updated")
     (kill-buffer buffer)
     (should ende-speech-test--killed-recorder)
     (should ende-speech-test--closed)
     (should-not ende-speech--active-session))))

(ert-deftest ende-speech/accept-cleans-after-destination-disappears ()
  (ende-speech-test--with-session
   (lambda (session source buffer)
     (ende-speech-test--event session "session.created")
     (ende-speech-test--event session "session.updated")
     (kill-buffer source)
     (should-error (with-current-buffer buffer (ende-speech--accept))
                   :type 'user-error)
     (should ende-speech-test--killed-recorder)
     (should ende-speech-test--closed)
     (should-not ende-speech--active-session))))

(ert-deftest ende-speech/accept-cleans-after-insertion-fails ()
  (ende-speech-test--with-session
   (lambda (session source buffer)
     (ende-speech-test--event session "session.created")
     (ende-speech-test--event session "session.updated")
     (ende-speech-test--event
      session "conversation.item.input_audio_transcription.delta"
      '(delta . "text"))
     (with-current-buffer source (setq buffer-read-only t))
     (should-error (with-current-buffer buffer (ende-speech--accept))
                   :type 'user-error)
     (should ende-speech-test--killed-recorder)
     (should ende-speech-test--closed)
     (should-not ende-speech--active-session))))

(provide 'ende-speech-test)

;;; ende-speech-test.el ends here
