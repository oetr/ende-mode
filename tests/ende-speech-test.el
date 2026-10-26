;;; ende-speech-test.el --- Realtime speech tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ende-speech)
(require 'ende)

(defvar ende-speech-test--open-args nil)
(defvar ende-speech-test--verify-error nil)
(defvar ende-speech-test--sent nil)
(defvar ende-speech-test--closed nil)
(defvar ende-speech-test--killed-recorder nil)
(defvar ende-speech-test--made-process nil)
(defvar ende-speech-test--source-window nil)
(defvar ende-speech-test--transcript-window nil)
(defvar ende-speech-test--selected-window nil)
(defvar ende-speech-test--window-buffers nil)
(defvar ende-speech-test--deleted-windows nil)

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
        (ende-speech-api-path "/speech-test")
        (ende-speech-input-device nil)
        (ende-speech-test--open-args nil)
        (ende-speech-test--sent nil)
        (ende-speech-test--closed nil)
        (ende-speech-test--killed-recorder nil)
        (ende-speech-test--made-process nil)
        (ende-speech-test--source-window 'mock-source-window)
        (ende-speech-test--transcript-window 'mock-transcript-window)
        (ende-speech-test--selected-window 'mock-source-window)
        (ende-speech-test--window-buffers
         (make-hash-table :test #'eq))
        (ende-speech-test--deleted-windows nil)
        (ende-speech-test--transcript-buffer nil))
    (unwind-protect
        (progn
          (puthash ende-speech-test--source-window buffer
                   ende-speech-test--window-buffers)
          (with-current-buffer buffer
            (insert "abcd")
            (goto-char 3)
            (cl-letf (((symbol-function 'executable-find)
                       (lambda (_) "/mock/parec"))
                      ((symbol-function 'websocket-open)
                       (lambda (url &rest args)
                         (setq ende-speech-test--open-args (cons url args))
                         websocket))
                      ((symbol-function 'selected-window)
                       (lambda () ende-speech-test--selected-window))
                      ((symbol-function 'select-window)
                       (lambda (window &rest _)
                         (setq ende-speech-test--selected-window window)))
                      ((symbol-function 'display-buffer)
                       (lambda (target &rest _)
                         (puthash ende-speech-test--transcript-window target
                                  ende-speech-test--window-buffers)
                         ende-speech-test--transcript-window))
                      ((symbol-function 'window-buffer)
                       (lambda (window &rest _)
                         (gethash window ende-speech-test--window-buffers)))
                      ((symbol-function 'set-window-buffer)
                       (lambda (window target &rest _)
                         (puthash window target ende-speech-test--window-buffers)))
                      ((symbol-function 'window-live-p)
                       (lambda (window)
                         (and (memq window (list ende-speech-test--source-window
                                                 ende-speech-test--transcript-window))
                              (not (memq window ende-speech-test--deleted-windows)))))
                      ((symbol-function 'window-dedicated-p) #'ignore)
                      ((symbol-function 'delete-window)
                       (lambda (window &rest _)
                         (push window ende-speech-test--deleted-windows)))
                      ((symbol-function 'websocket-openp) #'ignore)
                      ((symbol-function 'force-mode-line-update) #'ignore))
              (let ((websocket-stub
                     (lambda (url &rest args)
                       (setq ende-speech-test--open-args (cons url args)
                             ende-speech-test--verify-error gnutls-verify-error)
                       websocket)))
                (cl-letf (((symbol-function 'websocket-open) websocket-stub))
                  (ende-speech-start-talking)))))
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
                      ((symbol-function 'window-live-p)
                       (lambda (window)
                         (and (memq window
                                    (list ende-speech-test--source-window
                                          ende-speech-test--transcript-window))
                              (not (memq window ende-speech-test--deleted-windows)))))
                      ((symbol-function 'window-buffer)
                       (lambda (window &rest _)
                         (gethash window ende-speech-test--window-buffers)))
                      ((symbol-function 'select-window)
                       (lambda (window &rest _)
                         (setq ende-speech-test--selected-window window)))
                      ((symbol-function 'set-window-buffer)
                       (lambda (window target &rest _)
                         (puthash window target ende-speech-test--window-buffers)))
                      ((symbol-function 'window-dedicated-p) #'ignore)
                      ((symbol-function 'delete-window)
                       (lambda (window &rest _)
                         (push window ende-speech-test--deleted-windows)))
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
                       (apply real-load file arguments))))
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

(ert-deftest ende-speech/endpoint-configuration ()
  (pcase-dolist (`(,encryption ,server ,port ,path ,expected)
                `((nil "127.0.0.1" 8081 ,(default-value 'ende-speech-api-path)
                       ,(concat "ws://127.0.0.1:8081"
                                (default-value 'ende-speech-api-path)))
                  (t "speech.example.com" 443 "/transcribe"
                     "wss://speech.example.com:443/transcribe")
                  (nil "speech.example.com" 9000 "/transcribe"
                       "ws://speech.example.com:9000/transcribe")
                  (nil "192.168.1.50" 9000 "/transcribe"
                       "ws://192.168.1.50:9000/transcribe")
                  (t "::1" 8081 "/transcribe"
                     "wss://[::1]:8081/transcribe")))
    (let ((ende-speech-encryption encryption)
          (ende-speech-server server)
          (ende-speech-port port)
          (ende-speech-api-path path))
      (should (equal (ende-speech--server-url) expected))))
  (dolist (path '("/" "/my%20speech%20endpoint" "/api/v1/model:large"
                  "/transcribe%2Fcustom/%c3%a9"))
    (should (eq (ende-speech--validate-api-path path) path)))
  (let ((ende-speech-server "localhost")
        (ende-speech-port 8081)
        (ende-speech-api-path "/transcribe"))
    (pcase-dolist (`(,variable . ,invalid-values)
                  '((ende-speech-server "" "ws://localhost" "host/path"
                                        "user@host" "host name" "host\n"
                                        "-host" "host..com" "host:80" "1::2::3"
                                        "[localhost]" "999.999.999.999.")
                    (ende-speech-port 0 -1 65536 1.5 "8081")
                    (ende-speech-api-path "" "transcribe" "/path?query"
                                          "/path#fragment" "/path\r\n" "/path\0"
                                          "/my speech endpoint" "/speech%"
                                          "/speech%2" "/speech%ZZ"
                                          "/speech<test>" "/café")))
      (dolist (value invalid-values)
        (cl-progv (list variable) (list value)
          (should-error (ende-speech--server-url) :type 'user-error))))))

(ert-deftest ende-speech/configuration-interface ()
  (let ((ende-speech-input-device nil)
        (ende-speech-server "127.0.0.1")
        (ende-speech-port 8081)
        (ende-speech-api-path "/original")
        (ende-speech-encryption nil))
    (with-temp-buffer
      (ende-speech-configure-mode)
      (ende-speech--render-configuration)
      (let (settings)
        (goto-char (point-min))
        (while (re-search-forward "^\\([a-z]\\) +- \\([^:]+:\\)" nil t)
          (push (cons (match-string 1) (match-string 2)) settings))
        (should (equal (nreverse settings)
                       '(("a" . "API Path:") ("e" . "Encryption:")
                         ("i" . "Input:") ("p" . "Port:") ("s" . "Server:")))))
      (dolist (encryption '(nil t))
        (setq ende-speech-encryption encryption)
        (ende-speech--render-configuration)
        (goto-char (point-min))
        (search-forward (if encryption
                            "On - TLS with certificate verification"
                          "Off - voice sent unencrypted!"))
        (should-not (text-property-not-all
                     (match-beginning 0) (match-end 0) 'face
                     (if encryption 'success 'error)))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt choices _predicate require-match
                                    _initial _history default &rest _)
                     (should (equal choices '("Off" "On")))
                     (should require-match)
                     (should (equal default (if encryption "On" "Off")))
                     (if encryption "Off" "On"))))
          (call-interactively (key-binding (kbd "e"))))
        (should (eq ende-speech-encryption (not encryption)))
        (should (equal ende-speech-server "127.0.0.1"))
        (should (= ende-speech-port 8081)))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (prompt initial &rest _)
                   (if (equal prompt "API Path: ")
                       (progn (should (equal initial "/original")) "/custom")
                     "speech.example.com")))
                ((symbol-function 'read-number) (lambda (&rest _) 443))
                ((symbol-function 'ende-speech--discover-input-devices)
                 (lambda () '(("Microphone" . "source.1"))))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) "Microphone")))
        (dolist (key '("a" "i" "p" "s"))
          (call-interactively (key-binding (kbd key)))))
      (should (equal (list ende-speech-api-path ende-speech-input-device
                           ende-speech-port ende-speech-server)
                     '("/custom" "source.1" 443 "speech.example.com")))
      (dolist (value '("/custom" "source.1" "443" "speech.example.com"))
        (should (string-match-p (regexp-quote value) (buffer-string))))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "invalid?"))
                ((symbol-function 'read-number) (lambda (&rest _) 0)))
        (dolist (key '("a" "p" "s"))
          (should-error (call-interactively (key-binding (kbd key)))
                        :type 'user-error)))
      (should (equal (list ende-speech-api-path ende-speech-port ende-speech-server)
                     '("/custom" 443 "speech.example.com")))
      (should (string-match-p "q/Esc +- Quit without saving" (buffer-string)))
      (should (string-match-p "Enter +- Save and quit" (buffer-string))))))

(ert-deftest ende-speech/tls-enforcement-and-failure-cleanup ()
  (let ((ende-speech-encryption t)
        (gnutls-verify-error nil)
        (ende-speech-test--verify-error nil))
    (cl-letf (((symbol-function 'gnutls-available-p) (lambda () t)))
      (ende-speech-test--with-session
       (lambda (session _source buffer)
         (should (string-prefix-p "wss://" (car ende-speech-test--open-args)))
         (should (plist-get (cdr ende-speech-test--open-args) :nowait))
         (should (eq ende-speech-test--verify-error t))
         (should-not gnutls-verify-error)
         (funcall (plist-get (cdr ende-speech-test--open-args) :on-close)
                  (ende-speech--session-websocket session))
         (should (eq (ende-speech--session-state session) 'terminal))
         (should-not ende-speech--active-session)
         (should-not (buffer-live-p buffer))
         (should-not (ende-speech--session-websocket session))
         (should-not (marker-buffer (ende-speech--session-marker session)))
         (ende-speech-test--event session "session.created")
         (ende-speech-test--event session "session.updated")
         (should-not ende-speech-test--made-process)
         (should-not (cl-some #'websocket-frame-p ende-speech-test--sent)))))
    (let ((ende-speech--active-session nil))
      (cl-letf (((symbol-function 'gnutls-available-p) #'ignore)
                ((symbol-function 'websocket-open)
                 (lambda (&rest _) (ert-fail "Must not open without GnuTLS"))))
        (should-error (ende-speech-start-talking) :type 'user-error)
        (should-not ende-speech--active-session)))))

(ert-deftest ende-speech/configuration-discards-pending-edits ()
  (let ((ende-speech-api-path "/original")
        (ende-speech-encryption nil)
        (ende-speech-input-device nil)
        (ende-speech-port 8081)
        (ende-speech-server "127.0.0.1"))
    (save-window-excursion
      (let ((buffer (generate-new-buffer " *ende-speech-config-test*")))
        (unwind-protect
            (cl-letf (((symbol-function 'ende-speech--configuration-buffer)
                       (lambda () buffer))
                      ((symbol-function 'read-string) (lambda (&rest _) "/draft")))
              (ende-speech-configure)
              (call-interactively (key-binding (kbd "a")))
              (should (equal ende-speech-api-path "/draft"))
              (should (equal (default-value 'ende-speech-api-path) "/original"))
              (ende-speech-configure)
              (should (equal ende-speech-api-path "/draft"))
              (should (eq (key-binding (kbd "<escape>"))
                          #'ende-speech--configure-quit))
              (call-interactively (key-binding (kbd "q")))
              (should-not (buffer-live-p buffer))
              (should (equal ende-speech-api-path "/original"))
              (should (equal (ende-speech--server-url)
                             "ws://127.0.0.1:8081/original")))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest ende-speech/configuration-saves-for-current-session ()
  (let* ((directory (make-temp-file "ende-speech-save-test" t))
         (custom-file (expand-file-name "custom.el" directory))
         (user-init-file nil)
         (buffer (generate-new-buffer " *ende-speech-config-test*"))
         (defaults (mapcar (lambda (variable)
                             (cons variable (default-value variable)))
                           ende-speech--configuration-variables))
         (properties (mapcar (lambda (variable)
                               (cons variable (copy-sequence (symbol-plist variable))))
                             ende-speech--configuration-variables))
         (user-theme-settings (copy-tree (get 'user 'theme-settings))))
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'ende-speech--configuration-buffer)
                     (lambda () buffer)))
            (ende-speech-configure))
          (setq ende-speech-api-path "/custom"
                ende-speech-encryption t
                ende-speech-input-device "source.1"
                ende-speech-port 443
                ende-speech-server "speech.example.com")
          (should-not (file-exists-p custom-file))
          (should (equal (default-value 'ende-speech-server)
                         (cdr (assq 'ende-speech-server defaults))))
          (let ((saved-settings (mapcar (lambda (variable)
                                         (cons variable (symbol-value variable)))
                                       ende-speech--configuration-variables)))
            (call-interactively (key-binding (kbd "RET")))
            (should-not (buffer-live-p buffer))
            (should-not (directory-files directory nil directory-files-no-dot-files-regexp))
            (dolist (setting saved-settings)
              (should (equal (default-value (car setting)) (cdr setting))))
            (dolist (entry properties)
              (should (equal (symbol-plist (car entry)) (cdr entry))))
            (should (equal (get 'user 'theme-settings) user-theme-settings))
            (should (equal (ende-speech--server-url)
                           "wss://speech.example.com:443/custom"))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (dolist (entry defaults) (set-default (car entry) (cdr entry)))
      (dolist (entry properties) (setplist (car entry) (cdr entry)))
      (put 'user 'theme-settings user-theme-settings)
      (delete-directory directory t))))

(ert-deftest ende-speech/configuration-rejects-invalid-draft ()
  (let ((ende-speech-api-path "/original")
        (ende-speech-encryption nil))
    (with-temp-buffer
      (ende-speech-configure-mode)
      (setq ende-speech-api-path "/invalid?query"
            ende-speech-encryption t)
      (cl-letf (((symbol-function 'quit-window)
                 (lambda (&rest _) (ert-fail "Must not quit with invalid settings"))))
        (should-error (call-interactively (key-binding (kbd "RET")))
                      :type 'user-error))
      (should (equal ende-speech-api-path "/invalid?query"))
      (should (equal (default-value 'ende-speech-api-path) "/original"))
      (should-not (default-value 'ende-speech-encryption)))))

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
                    (ende-speech--server-url)))
     (should (= (marker-position (ende-speech--session-marker session)) 3))
     (should (eq (marker-buffer (ende-speech--session-marker session)) source))
     (should (eq (ende-speech--session-source-window session)
                 ende-speech-test--source-window))
     (should (eq (ende-speech--session-transcript-window session)
                 ende-speech-test--transcript-window))
     (should (eq ende-speech-test--selected-window
                 ende-speech-test--transcript-window))
     (should (eq (gethash ende-speech-test--source-window
                          ende-speech-test--window-buffers)
                 source))
     (should (eq (gethash ende-speech-test--transcript-window
                          ende-speech-test--window-buffers)
                 (ende-speech--session-buffer session)))
     (should (eq ende-speech-test--selected-window
                 ende-speech-test--transcript-window))
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
                    "/mock/parec"))
     (should (equal (ende-speech--session-state session) 'listening))
     (should (string-match-p
              "Listening"
              (with-current-buffer (ende-speech--session-buffer session)
                (ende-speech--header-line)))))))

(ert-deftest ende-speech/deltas-insert-at-point-and-ignore-completion-text ()
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
      '(delta . " world"))
     (ende-speech-test--event
      session "conversation.item.input_audio_transcription.completed"
      '(transcript . "different final wording"))
     (should (equal (with-current-buffer buffer (buffer-string))
                    "Hello world"))
     (should (= (with-current-buffer buffer (point)) 12))
     (should (string-match-p
              "C-c C-c accept.*C-c C-k cancel"
              (with-current-buffer buffer
                (ende-speech--header-line)))))))

(ert-deftest ende-speech/transcript-is-editable-and-accepts-manual-changes ()
  (ende-speech-test--with-session
   (lambda (_session source buffer)
     (with-current-buffer buffer
       (should-not buffer-read-only)
       (should (eq (key-binding (kbd "C-c C-c")) #'ende-speech--accept))
       (should (eq (key-binding (kbd "C-c C-k")) #'ende-speech--reject))
       (insert "typed ")
       (delete-char -1)
       (insert "!")
       (undo-boundary))
     (with-current-buffer buffer
       (ende-speech--accept))
     (should (equal (with-current-buffer source (buffer-string))
                    "abtyped!cd")))))

(ert-deftest ende-speech/deltas-advance-current-point ()
  (ende-speech-test--with-session
   (lambda (session _source buffer)
     (ende-speech-test--event
      session "conversation.item.input_audio_transcription.delta"
      '(delta . "hello"))
     (ende-speech-test--event
      session "conversation.item.input_audio_transcription.delta"
      '(delta . " world"))
     (should (equal (with-current-buffer buffer (buffer-string)) "hello world"))
     (should (= (with-current-buffer buffer (point)) 12))
     (ende-speech-test--event
      session "conversation.item.input_audio_transcription.completed"
      '(transcript . "changed completion"))
     (should (equal (with-current-buffer buffer (buffer-string))
                    "hello world"))
     (should (= (with-current-buffer buffer (point)) 12)))))

(ert-deftest ende-speech/cursor-move-clears-and-resumes-at-point ()
  (ende-speech-test--with-session
   (lambda (session _source buffer)
     (ende-speech-test--event session "session.created")
     (ende-speech-test--event session "session.updated")
     (with-current-buffer buffer
       (insert "one two")
       (ende-speech--before-command)
       (insert "!")
       (ende-speech--after-command)
       (should-not (ende-speech--session-clear-pending session))
       (ende-speech--before-command)
       (delete-char -1)
       (ende-speech--after-command)
       (should-not (ende-speech--session-clear-pending session))
       (ende-speech--before-command)
       (goto-char (point-min))
       (ende-speech--after-command))
     (should (ende-speech--session-clear-pending session))
     (should (= (cl-count "input_audio_buffer.clear"
                          (mapcar (lambda (event) (alist-get 'type event))
                                  (cl-remove-if-not #'listp ende-speech-test--sent))
                          :test #'equal)
                1))
     (let ((filter (plist-get ende-speech-test--made-process :filter))
           (sent-count (length ende-speech-test--sent)))
       (funcall filter 'mock-recorder "dropped audio")
       (should (= (length ende-speech-test--sent) sent-count)))
     (ende-speech-test--event
      session "conversation.item.input_audio_transcription.delta"
      '(delta . "stale"))
     (should (equal (with-current-buffer buffer (buffer-string)) "one two"))
     (with-current-buffer buffer
       (ende-speech--before-command)
       (goto-char (point-max))
       (ende-speech--after-command))
     (should (= (cl-count "input_audio_buffer.clear"
                          (mapcar (lambda (event) (alist-get 'type event))
                                  (cl-remove-if-not #'listp ende-speech-test--sent))
                          :test #'equal)
                1))
     (ende-speech-test--event session "input_audio_buffer.cleared")
     (should-not (ende-speech--session-clear-pending session))
     (with-current-buffer buffer (goto-char (point-min)))
     (ende-speech-test--event
      session "conversation.item.input_audio_transcription.delta"
      '(delta . "new"))
     (should (equal (with-current-buffer buffer (buffer-string)) "newone two"))
     (should (= (with-current-buffer buffer (point)) 4)))))

(ert-deftest ende-speech/completed-event-does-not-rewrite-visible-text ()
  (ende-speech-test--with-session
   (lambda (session _source buffer)
     (ende-speech-test--event
      session "conversation.item.input_audio_transcription.delta"
      '(delta . "wrong words"))
     (with-current-buffer buffer
       (goto-char (point-min))
       (delete-char 5)
       (insert "right"))
     (ende-speech-test--event
      session "conversation.item.input_audio_transcription.completed"
      '(transcript . "a replacement"))
     (should (equal (with-current-buffer buffer (buffer-string))
                    "right words")))))

(ert-deftest ende-speech/accept-cleans-inserts-at-marker-and-ignores-events ()
  (ende-speech-test--with-session
   (lambda (session source buffer)
     (ende-speech-test--event session "session.created")
     (ende-speech-test--event session "session.updated")
     (ende-speech-test--event
      session "conversation.item.input_audio_transcription.delta"
      '(delta . "Visible partial"))
     (with-current-buffer buffer (ende-speech--accept))
     (should (eq ende-speech-test--selected-window
                 ende-speech-test--source-window))
     (should (eq (window-buffer ende-speech-test--source-window) source))
     (should (memq ende-speech-test--transcript-window
                   ende-speech-test--deleted-windows))
     (should (equal ende-speech-test--deleted-windows
                    (list ende-speech-test--transcript-window)))
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
     (should (eq ende-speech-test--selected-window
                 ende-speech-test--source-window))
     (should (memq ende-speech-test--transcript-window
                   ende-speech-test--deleted-windows))
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
