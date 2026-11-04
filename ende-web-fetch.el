;;; ende-web-fetch.el --- Bounded unauthenticated web fetch -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter Samarin
;;
;; Author: Peter Samarin <peter.samarin@gmail.com>
;; Maintainer: Peter Samarin <peter.samarin@gmail.com>
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

;; An asynchronous, cancellable curl transport for the model-facing web_fetch
;; tool.  Every URL is authorized before DNS, all DNS answers are classified,
;; and DNS names are pinned with curl --resolve.  Provider credentials and
;; transport profiles are intentionally not involved.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-expand)
(require 'url-parse)
(require 'ende-url)

(defcustom ende-web-fetch-connect-timeout 10
  "Maximum seconds allowed for one HTTP connection attempt."
  :type 'integer
  :group 'ende)

(defcustom ende-web-fetch-call-timeout 30
  "Maximum seconds for DNS, redirects, and all response transfers combined."
  :type 'integer
  :group 'ende)

(defcustom ende-web-fetch-max-redirects 5
  "Maximum redirects followed by `web_fetch'."
  :type 'integer
  :group 'ende)

(defcustom ende-web-fetch-max-response-bytes (* 2 1024 1024)
  "Maximum cumulative response body bytes across redirects."
  :type 'integer
  :group 'ende)

(defcustom ende-web-fetch-max-response-header-bytes (* 64 1024)
  "Maximum cumulative response header bytes across redirects."
  :type 'integer
  :group 'ende)

(defcustom ende-web-fetch-max-output-chars 50000
  "Maximum characters returned by one successful `web_fetch'."
  :type 'integer
  :group 'ende)

(defconst ende-web-fetch--address-blocks
  '((#x00000000 8 unspecified)
    (#x0a000000 8 private)
    (#x64400000 10 special)
    (#x7f000000 8 loopback)
    (#xa9fe0000 16 link-local)
    (#xac100000 12 private)
    (#xc0000000 24 special)
    (#xc0000200 24 documentation)
    (#xc0586300 24 special)
    (#xc01fc400 24 special)
    (#xc034c100 24 special)
    (#xc0a80000 16 private)
    (#xc0af3000 24 special)
    (#xc6120000 15 special)
    (#xc6336400 24 documentation)
    (#xc7007100 24 documentation)
    (#xe0000000 4 multicast)
    (#xf0000000 4 reserved))
  "IPv4 non-global ranges rejected by web_fetch, as (NETWORK BITS CLASS).

The explicit private exception is limited to the RFC 1918 entries and 127/8.
Other non-global classes, including shared, link-local, documentation,
benchmarking, multicast, and reserved ranges remain denied.")

(cl-defstruct (ende-web-fetch--transfer (:constructor ende-web-fetch--make-transfer))
  "One fetch call's request-owned asynchronous lifecycle state."
  deadline deadline-timer phase phase-deadline phase-timer
  directory callback dns-callback authorize register-cancellation cancel-function
  requested-url current-url max-chars max-redirects connect-timeout
  max-response-bytes max-header-bytes max-stderr-bytes redirects
  addresses allow-private connected
  process stderr-process stderr-buffer resolver-buffer
  hop header-bytes body-bytes failure cancelled settled)

(cl-defstruct (ende-web-fetch--hop (:constructor ende-web-fetch--make-hop))
  "One pinned HTTP response and its private buffers."
  body-buffer stderr-buffer pending-header in-body status headers)

(defun ende-web-fetch--local-directory ()
  "Return the existing local directory used to launch network helpers."
  (unless (and (stringp temporary-file-directory)
               (file-name-absolute-p temporary-file-directory)
               (not (file-remote-p temporary-file-directory))
               (file-directory-p temporary-file-directory))
    (user-error "Ende requires an existing absolute local temporary-file-directory"))
  (file-name-as-directory (expand-file-name temporary-file-directory)))

(defun ende-web-fetch--prefix-p (number network bits width)
  "Return non-nil if NUMBER is in NETWORK/BITS for address WIDTH."
  (= (ash number (- bits width)) (ash network (- bits width))))

(defun ende-web-fetch--ipv4-string (number)
  "Format numeric IPv4 NUMBER as canonical dotted decimal."
  (format "%d.%d.%d.%d"
          (logand (ash number -24) 255)
          (logand (ash number -16) 255)
          (logand (ash number -8) 255)
          (logand number 255)))

(defun ende-web-fetch--ipv4-class (number)
  "Return the fixed special-use class for IPv4 NUMBER, or `public'."
  (or (cl-loop for (network bits class) in ende-web-fetch--address-blocks
               when (ende-web-fetch--prefix-p number network bits 32)
               return class)
      'public))

(defun ende-web-fetch--ipv6-class (number groups)
  "Return the fixed special-use class for IPv6 NUMBER and GROUPS."
  (cond
   ((= number 0) 'unspecified)
   ((= number 1) 'loopback)
   ((ende-web-fetch--prefix-p number #xfc000000000000000000000000000000 7 128)
    'private)
   ((ende-web-fetch--prefix-p number #xfe800000000000000000000000000000 10 128)
    'link-local)
   ((ende-web-fetch--prefix-p number #xff000000000000000000000000000000 8 128)
    'multicast)
   ((ende-web-fetch--prefix-p number #x20010db8000000000000000000000000 32 128)
    'documentation)
   ((ende-web-fetch--prefix-p number #x20010000000000000000000000000000 23 128)
    'special)
   ((ende-web-fetch--prefix-p number #x20020000000000000000000000000000 16 128)
    'transition)
   ((ende-web-fetch--prefix-p number #x0064ff9b000000000000000000000000 96 128)
    'transition)
   ((ende-web-fetch--prefix-p number #x0064ff9b000100000000000000000000 48 128)
    'transition)
   ((ende-web-fetch--prefix-p number #x3fff0000000000000000000000000000 20 128)
    'documentation)
   ((and (memq (nth 6 groups) '(#x5efe))
         (memq (nth 5 groups) '(0 #x0200)))
    'transition)
   ((ende-web-fetch--prefix-p number #x20000000000000000000000000000000 3 128)
    'public)
   (t 'special)))

(defun ende-web-fetch-classify-address (address)
  "Parse numeric ADDRESS and return (CLASS . CANONICAL-ADDRESS).
Unsupported or malformed forms signal rather than bypassing classification."
  (unless (and (stringp address) (not (string-empty-p address)))
    (error "resolver returned an empty address"))
  (let ((ipv4 (ende-url--ipv4-number address)))
    (if ipv4
        (cons (ende-web-fetch--ipv4-class ipv4)
              (ende-web-fetch--ipv4-string ipv4))
      (when (string-match-p "[^0-9a-fA-F:.]" address)
        (error "resolver returned a non-numeric address: %s" address))
      (let* ((groups (ende-url--ipv6-groups address))
             (number (ende-url--ipv6-number groups)))
        (if (= (ash number -32) #xffff)
            (let* ((mapped (logand number #xffffffff))
                   (class (ende-web-fetch--ipv4-class mapped)))
              (cons class (ende-web-fetch--ipv4-string mapped)))
          (when (string-match-p "\\." address)
            (error "unsupported embedded IPv4 transition address"))
          (cons (ende-web-fetch--ipv6-class number groups)
                (ende-url--ipv6-string groups)))))))

(defun ende-web-fetch--address-allowed-p (class allow-private)
  "Return non-nil when address CLASS passes the selected policy."
  (or (eq class 'public)
      (and allow-private (memq class '(private loopback)))))

(defun ende-web-fetch--assert-addresses (addresses allow-private)
  "Require every parsed (CLASS . ADDRESS) in ADDRESSES to be permitted."
  (unless addresses
    (error "hostname resolved to no addresses"))
  (dolist (entry addresses)
    (unless (ende-web-fetch--address-allowed-p (car entry) allow-private)
      (error "destination address is blocked by web_fetch policy (%s): %s"
             (car entry) (cdr entry))))
  addresses)

(defun ende-web-fetch--remaining-time (transfer)
  "Return TRANSFER's remaining deadline time in seconds."
  (- (ende-web-fetch--transfer-deadline transfer) (float-time)))

(defun ende-web-fetch--cancel-phase-timer (transfer)
  (let ((timer (ende-web-fetch--transfer-phase-timer transfer)))
    (when (timerp timer)
      (cancel-timer timer)))
  (setf (ende-web-fetch--transfer-phase-timer transfer) nil))

(defun ende-web-fetch--cancel-timers (transfer)
  (let ((timer (ende-web-fetch--transfer-deadline-timer transfer)))
    (when (timerp timer)
      (cancel-timer timer)))
  (setf (ende-web-fetch--transfer-deadline-timer transfer) nil)
  (ende-web-fetch--cancel-phase-timer transfer))

(defun ende-web-fetch--release-process (transfer)
  (let ((process (ende-web-fetch--transfer-process transfer))
        (stderr-process (ende-web-fetch--transfer-stderr-process transfer))
        (stderr-buffer (ende-web-fetch--transfer-stderr-buffer transfer)))
    (setf (ende-web-fetch--transfer-process transfer) nil
          (ende-web-fetch--transfer-stderr-process transfer) nil
          (ende-web-fetch--transfer-stderr-buffer transfer) nil)
    (when (processp process)
      (set-process-filter process nil)
      (set-process-sentinel process nil)
      (set-process-query-on-exit-flag process nil))
    (when (processp stderr-process)
      (set-process-filter stderr-process nil)
      (set-process-sentinel stderr-process nil)
      (set-process-query-on-exit-flag stderr-process nil))
    (when (and (processp process) (process-live-p process))
      (delete-process process))
    (when (and (processp stderr-process) (process-live-p stderr-process))
      (delete-process stderr-process))
    (when (buffer-live-p stderr-buffer)
      (kill-buffer stderr-buffer))))

(defun ende-web-fetch--release-hop (hop)
  "Kill private buffers owned by completed HOP."
  (when hop
    (dolist (buffer (list (ende-web-fetch--hop-body-buffer hop)
                          (ende-web-fetch--hop-stderr-buffer hop)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun ende-web-fetch--settle (transfer outcome result error)
  (unless (ende-web-fetch--transfer-settled transfer)
    (setf (ende-web-fetch--transfer-settled transfer) outcome)
    (ende-web-fetch--cancel-timers transfer)
    (ende-web-fetch--release-process transfer)
    (let ((resolver-buffer (ende-web-fetch--transfer-resolver-buffer transfer))
          (hop (ende-web-fetch--transfer-hop transfer))
          (register (ende-web-fetch--transfer-register-cancellation transfer))
          (callback (ende-web-fetch--transfer-callback transfer)))
      (setf (ende-web-fetch--transfer-resolver-buffer transfer) nil
            (ende-web-fetch--transfer-dns-callback transfer) nil
            (ende-web-fetch--transfer-hop transfer) nil
            (ende-web-fetch--transfer-register-cancellation transfer) nil
            (ende-web-fetch--transfer-callback transfer) nil
            (ende-web-fetch--transfer-authorize transfer) nil
            (ende-web-fetch--transfer-cancel-function transfer) nil
            (ende-web-fetch--transfer-current-url transfer) nil)
      (when (buffer-live-p resolver-buffer)
        (kill-buffer resolver-buffer))
      (ende-web-fetch--release-hop hop)
      (when register
        (condition-case nil
            (funcall register nil)
          ((error quit) nil)))
      (when callback
        (condition-case nil
            (funcall callback result error)
          ((error quit) nil))))))

(defun ende-web-fetch--address-list (addresses)
  (mapconcat #'cdr addresses ","))

(defun ende-web-fetch--destination-label (transfer)
  (let* ((url (ende-web-fetch--transfer-current-url transfer))
         (host (plist-get url :host))
         (port (plist-get url :port)))
    (format "%s:%d" host port)))

(defun ende-web-fetch--overall-timeout (transfer)
  (unless (ende-web-fetch--transfer-settled transfer)
    (let ((phase (ende-web-fetch--transfer-phase transfer)))
      (ende-web-fetch--settle
       transfer 'timeout nil
       (format "web_fetch overall deadline expired during %s for %s"
               (pcase phase
                 ('dns "DNS resolution")
                 ('curl (if (or (ende-web-fetch--transfer-connected transfer)
                                (and (ende-web-fetch--transfer-hop transfer)
                                     (ende-web-fetch--hop-status
                                      (ende-web-fetch--transfer-hop transfer))))
                            "response transfer"
                          "connection"))
                 (_ "request"))
               (ende-web-fetch--destination-label transfer))))))

(defun ende-web-fetch--phase-timeout (transfer host process)
  (when (and (not (ende-web-fetch--transfer-settled transfer))
             (eq process (ende-web-fetch--transfer-process transfer)))
    (let ((phase-deadline (ende-web-fetch--transfer-phase-deadline transfer))
          (deadline (ende-web-fetch--transfer-deadline transfer)))
      (ende-web-fetch--settle
       transfer 'timeout nil
       (if (< phase-deadline deadline)
           (format "web_fetch DNS timeout resolving %s" host)
         (format "web_fetch overall deadline expired while resolving %s" host))))))

(defun ende-web-fetch--append-resolver-output (buffer transfer process chunk)
  (let ((size (+ (with-current-buffer buffer (buffer-size))
                 (string-bytes chunk))))
    (if (> size (ende-web-fetch--transfer-max-header-bytes transfer))
        (progn
          (setf (ende-web-fetch--transfer-failure transfer)
                "DNS resolver output exceeded its byte limit")
          (when (process-live-p process)
            (delete-process process)))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert chunk)))))

(defun ende-web-fetch--append-stderr (buffer transfer process chunk)
  (let* ((size (with-current-buffer buffer (buffer-size)))
         (remaining (- (ende-web-fetch--transfer-max-stderr-bytes transfer)
                       size)))
    (when (> remaining 0)
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert (substring chunk 0 (min remaining (length chunk))))))
    (when (> (string-bytes chunk) remaining)
      (process-put process :ende-web-fetch-stderr-truncated t))
    (when (and (eq (ende-web-fetch--transfer-phase transfer) 'curl)
               (with-current-buffer buffer
                 (save-excursion
                   (goto-char (point-min))
                   (search-forward "Connected to " nil t))))
      (setf (ende-web-fetch--transfer-connected transfer) t))))

(defun ende-web-fetch--new-stderr-process (transfer)
  (let ((buffer (generate-new-buffer " *ende-web-fetch-stderr*")))
    (with-current-buffer buffer
      (set-buffer-multibyte nil))
    (condition-case caught
        (let ((process
               (make-pipe-process
                :name (generate-new-buffer-name "ende-web-fetch-stderr")
                :buffer nil :noquery t :coding 'binary :sentinel #'ignore
                :filter (apply-partially
                         #'ende-web-fetch--append-stderr buffer transfer))))
          (setf (ende-web-fetch--transfer-stderr-buffer transfer) buffer
                (ende-web-fetch--transfer-stderr-process transfer) process)
          process)
      ((error quit)
       (when (buffer-live-p buffer)
         (kill-buffer buffer))
       (signal (car caught) (cdr caught))))))

(defun ende-web-fetch--stderr-text (transfer)
  (let ((buffer (ende-web-fetch--transfer-stderr-buffer transfer)))
    (if (buffer-live-p buffer)
        (let ((text (with-current-buffer buffer
                      (decode-coding-string (buffer-string) 'utf-8))))
          (string-trim (substring text 0 (min (length text) 1024))))
      "")))

(defun ende-web-fetch--resolver-addresses (buffer)
  (let (addresses)
    (with-current-buffer buffer
      (dolist (line (split-string (buffer-string) "\n" t))
        (let ((token (car (split-string line "[ \t]+" t))))
          (when token
            (push (ende-web-fetch-classify-address token) addresses)))))
    (delete-dups (nreverse addresses))))

(defun ende-web-fetch--dns-sentinel (transfer host buffer process _event)
  (when (and (not (ende-web-fetch--transfer-settled transfer))
             (eq process (ende-web-fetch--transfer-process transfer))
             (memq (process-status process) '(exit signal failed closed)))
    (let* ((exit (process-exit-status process))
           (stderr (ende-web-fetch--stderr-text transfer))
           (failure (ende-web-fetch--transfer-failure transfer))
           (callback (ende-web-fetch--transfer-dns-callback transfer))
           (addresses nil)
           (parse-error nil))
      (ende-web-fetch--cancel-phase-timer transfer)
      (ende-web-fetch--release-process transfer)
      (setf (ende-web-fetch--transfer-phase transfer) nil
            (ende-web-fetch--transfer-failure transfer) nil
            (ende-web-fetch--transfer-resolver-buffer transfer) nil
            (ende-web-fetch--transfer-dns-callback transfer) nil)
      (when (and (not failure) (eq (process-status process) 'exit) (zerop exit))
        (condition-case caught
            (setq addresses (ende-web-fetch--resolver-addresses buffer))
          ((error quit)
           (setq parse-error (error-message-string caught)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (cond
       (failure
        (funcall callback nil failure))
       (parse-error
        (funcall callback nil
                 (format "DNS resolution returned invalid addresses for %s: %s"
                         host parse-error)))
       ((not (and (eq (process-status process) 'exit) (zerop exit)))
        (funcall callback nil
                 (format "DNS resolution failed for %s%s"
                         host (if (string-empty-p stderr) ""
                                (concat ": " stderr)))))
       ((null addresses)
        (funcall callback nil
                 (format "DNS resolution returned no addresses for %s" host)))
       (t
        (funcall callback addresses nil))))))

(defun ende-web-fetch--resolve-host (transfer host callback)
  (let* ((buffer (generate-new-buffer " *ende-web-fetch-dns*"))
         (remaining (ende-web-fetch--remaining-time transfer))
         (phase-duration
          (min (ende-web-fetch--transfer-connect-timeout transfer) remaining))
         (default-directory (ende-web-fetch--transfer-directory transfer))
         (process nil))
    (setf (ende-web-fetch--transfer-dns-callback transfer) callback
          (ende-web-fetch--transfer-phase transfer) 'dns
          (ende-web-fetch--transfer-phase-deadline transfer)
          (+ (float-time) phase-duration)
          (ende-web-fetch--transfer-resolver-buffer transfer) buffer)
    (with-current-buffer buffer
      (set-buffer-multibyte nil))
    (if (<= phase-duration 0)
        (funcall callback nil
                 (format "web_fetch overall deadline expired before DNS for %s"
                         host))
      (condition-case caught
          (let ((stderr-process
                 (ende-web-fetch--new-stderr-process transfer)))
            (setq process
                  (make-process
                   :name (generate-new-buffer-name "ende-web-fetch-dns")
                   :buffer nil :stderr stderr-process
                   :command (list "getent" "ahosts" host)
                   :connection-type 'pipe :coding 'binary :noquery t
                   :filter (apply-partially
                            #'ende-web-fetch--append-resolver-output
                            buffer transfer)
                   :sentinel (apply-partially
                              #'ende-web-fetch--dns-sentinel
                              transfer host buffer)))
            (setf (ende-web-fetch--transfer-process transfer) process
                  (ende-web-fetch--transfer-phase-timer transfer)
                  (run-at-time phase-duration nil
                               #'ende-web-fetch--phase-timeout
                               transfer host process)))
        ((error quit)
         (let ((message (error-message-string caught)))
           (ende-web-fetch--release-process transfer)
           (when (buffer-live-p buffer)
             (kill-buffer buffer))
           (setf (ende-web-fetch--transfer-resolver-buffer transfer) nil
                 (ende-web-fetch--transfer-phase transfer) nil)
           (funcall callback nil
                    (format "DNS resolver could not start for %s: %s"
                            host message))))))))

(defun ende-web-fetch--resolve-option (host port addresses)
  (format "%s:%d:%s" host port
          (mapconcat (lambda (entry)
                       (let ((address (cdr entry)))
                         (if (string-match-p ":" address)
                             (concat "[" address "]")
                           address)))
                     addresses ",")))

(defun ende-web-fetch--command (transfer normalized addresses)
  (let* ((url (plist-get normalized :url))
         (host (plist-get normalized :host))
         (port (plist-get normalized :port))
         (remaining (max 0.001 (ende-web-fetch--remaining-time transfer)))
         (connect-timeout
          (max 0.001
               (min (ende-web-fetch--transfer-connect-timeout transfer)
                    remaining)))
         (dns-host (eq (plist-get normalized :host-kind) 'dns)))
    (append
     (list "curl" "--disable" "--silent" "--show-error" "--no-buffer"
           "--verbose" "--globoff" "--path-as-is" "--proto" "=http,https"
           "--proxy" "" "--noproxy" "*"
           "--connect-timeout"
           (number-to-string connect-timeout)
           "--max-time" (number-to-string remaining)
           "--include"
           "--header" "Accept-Encoding: identity"
           "--header"
           "Accept: text/html, text/plain, application/json, application/xml, text/xml")
     (when dns-host
       (list "--resolve"
             (ende-web-fetch--resolve-option host port addresses)))
     (list "--url" url))))

(defun ende-web-fetch--header-value (headers name)
  "Return the last value for NAME from response HEADERS."
  (cdr (assoc-string name headers t)))

(defun ende-web-fetch--parse-header-block (block)
  "Parse a completed raw HTTP header BLOCK to (STATUS . HEADERS)."
  (let* ((text (replace-regexp-in-string "\r" "" block t t))
         (lines (split-string text "\n" nil))
         (status-line (car lines))
         (case-fold-search t)
         (status nil)
         (headers nil))
    (unless (and status-line
                 (string-match
                  "\\`HTTP/[0-9]+\\(?:\\.[0-9]+\\)?[ \t]+\\([0-9][0-9][0-9]\\)"
                  status-line))
      (error "web_fetch received malformed HTTP response headers"))
    (setq status (string-to-number (match-string 1 status-line)))
    (dolist (line (cdr lines))
      (unless (string-empty-p line)
        (unless (string-match "\\`\\([^:]+\\):[ \t]*\\(.*\\)\\'" line)
          (error "web_fetch received a malformed HTTP header"))
        (let ((name (downcase (match-string 1 line)))
              (value (string-trim (match-string 2 line))))
          (let ((previous (assoc-string name headers t)))
            (when (and previous
                       (member name '("content-encoding" "content-type"
                                      "location"))
                       (not (string-equal (cdr previous) value)))
              (error "web_fetch received conflicting %s headers" name))
            (setq headers
                  (cons (cons name value)
                        (cl-remove-if
                         (lambda (header) (string-equal (car header) name))
                         headers)))))))
    (cons status headers)))

(defun ende-web-fetch--append-body (transfer hop bytes process)
  "Append response BYTES to HOP under TRANSFER's cumulative byte ceiling."
  (let ((total (+ (ende-web-fetch--transfer-body-bytes transfer)
                  (string-bytes bytes))))
    (if (> total (ende-web-fetch--transfer-max-response-bytes transfer))
        (progn
          (setf (ende-web-fetch--transfer-failure transfer)
                "web_fetch response bodies exceed the configured byte limit")
          (when (process-live-p process) (delete-process process)))
      (setf (ende-web-fetch--transfer-body-bytes transfer) total)
      (with-current-buffer (ende-web-fetch--hop-body-buffer hop)
        (goto-char (point-max))
        (insert bytes)))))

(defun ende-web-fetch--consume-response-chunk (transfer hop process chunk)
  (if (ende-web-fetch--hop-in-body hop)
      (ende-web-fetch--append-body transfer hop chunk process)
    (let ((pending (concat (ende-web-fetch--hop-pending-header hop) chunk)))
      (while (and pending (not (ende-web-fetch--hop-in-body hop)))
        (let* ((crlf (string-match "\r\n\r\n" pending))
               (lf (string-match "\n\n" pending))
               (delimiter
                (cond
                 ((and crlf (or (null lf) (<= crlf lf)))
                  (list crlf (+ crlf 4)))
                 (lf (list lf (+ lf 2))))))
          (if (null delimiter)
              (progn
                (when (> (+ (ende-web-fetch--transfer-header-bytes transfer)
                            (string-bytes pending))
                         (ende-web-fetch--transfer-max-header-bytes transfer))
                  (error "web_fetch response headers exceed the configured byte limit"))
                (setf (ende-web-fetch--hop-pending-header hop) pending)
                (setq pending nil))
            (let* ((start (car delimiter))
                   (end (cadr delimiter))
                   (block (substring pending 0 start))
                   (header-size (string-bytes (substring pending 0 end)))
                   (parsed (ende-web-fetch--parse-header-block block))
                   (status (car parsed)))
              (cl-incf (ende-web-fetch--transfer-header-bytes transfer)
                       header-size)
              (when (> (ende-web-fetch--transfer-header-bytes transfer)
                       (ende-web-fetch--transfer-max-header-bytes transfer))
                (error "web_fetch response headers exceed the configured byte limit"))
              (setf (ende-web-fetch--hop-status hop) status
                    (ende-web-fetch--hop-headers hop) (cdr parsed)
                    (ende-web-fetch--hop-pending-header hop) "")
              (setq pending (substring pending end))
              (if (and (>= status 100) (< status 200))
                  (when (= status 101)
                    (error "web_fetch does not support protocol upgrades"))
                (setf (ende-web-fetch--hop-in-body hop) t)
                (unless (string-empty-p pending)
                  (ende-web-fetch--append-body
                   transfer hop pending process))))))))))

(defun ende-web-fetch--response-filter (transfer hop process chunk)
  "Parse HTTP response CHUNK for PROCESS, enforcing live byte ceilings."
  (unless (or (ende-web-fetch--transfer-settled transfer)
              (string-empty-p chunk)
              (ende-web-fetch--transfer-failure transfer))
    (condition-case caught
        (ende-web-fetch--consume-response-chunk transfer hop process chunk)
      (error
       (setf (ende-web-fetch--transfer-failure transfer)
             (error-message-string caught))
       (when (process-live-p process)
         (delete-process process))))))

(defun ende-web-fetch--curl-error (transfer exit hop stderr)
  (let* ((destination (ende-web-fetch--destination-label transfer))
         (addresses (ende-web-fetch--address-list
                     (ende-web-fetch--transfer-addresses transfer)))
         (candidates (if (string-empty-p addresses) "literal address" addresses))
         (diagnostic
          (string-join
           (cl-remove-if
            (lambda (line) (string-match-p "\\`[*<>] " line))
            (split-string stderr "\n" t))
           "\n"))
         (detail (if (string-empty-p diagnostic) ""
                   (concat ": " diagnostic))))
    (cond
     ((and (= exit 28)
           (or (ende-web-fetch--transfer-connected transfer)
               (ende-web-fetch--hop-status hop)))
      (format "web_fetch response deadline exceeded from %s via [%s] (curl exit 28)%s"
              destination candidates detail))
     ((= exit 28)
      (format "web_fetch connection timeout to %s via [%s] (curl exit 28)%s"
              destination candidates detail))
     ((memq exit '(51 60 77))
      (format "web_fetch TLS certificate verification failed for %s via [%s] (curl exit %d)%s"
              destination candidates exit detail))
     ((= exit 35)
      (format "web_fetch TLS handshake failed for %s via [%s] (curl exit 35)%s"
              destination candidates detail))
     ((= exit 6)
      (format "web_fetch DNS resolution failed for %s despite pinned candidates [%s] (curl exit 6)%s"
              destination candidates detail))
     ((and (= exit 7) (null (ende-web-fetch--hop-status hop)))
      (format "web_fetch connection failed to %s via [%s] (curl exit 7)%s"
              destination candidates detail))
     ((null (ende-web-fetch--hop-status hop))
      (format "web_fetch connection failed to %s via [%s] (curl exit %d)%s"
              destination candidates exit detail))
     (t
      (format "web_fetch response transfer failed from %s via [%s] (curl exit %d)%s"
              destination candidates exit detail)))))

(defun ende-web-fetch--finish-response (transfer hop)
  (let* ((headers (ende-web-fetch--hop-headers hop))
         (raw-content-type (ende-web-fetch--header-value headers "content-type"))
         (mime (ende-web-fetch--text-content-type headers))
         (encoding (ende-web-fetch--header-value headers "content-encoding")))
    (when (and encoding (not (string-equal (downcase encoding) "identity")))
      (error "web_fetch does not support Content-Encoding: %s" encoding))
    (let* ((bytes (with-current-buffer (ende-web-fetch--hop-body-buffer hop)
                    (buffer-string)))
           (content (ende-web-fetch--decode-body
                     bytes (or raw-content-type (car mime))))
           (limit (ende-web-fetch--transfer-max-chars transfer)))
      (list :requested_url (ende-web-fetch--transfer-requested-url transfer)
            :final_url
            (plist-get (ende-web-fetch--transfer-current-url transfer) :url)
            :status (ende-web-fetch--hop-status hop)
            :content_type (cdr mime)
            :content (if (> (length content) limit)
                         (substring content 0 limit)
                       content)
            :truncated (if (> (length content) limit) t :json-false)))))

(defun ende-web-fetch--start-curl (transfer normalized addresses)
  (let* ((body (generate-new-buffer " *ende-web-fetch-body*"))
         (hop (ende-web-fetch--make-hop
               :body-buffer body :pending-header "" :in-body nil))
         (default-directory (ende-web-fetch--transfer-directory transfer))
         (stderr-process nil)
         (process nil))
    (with-current-buffer body
      (set-buffer-multibyte nil))
    (setf (ende-web-fetch--transfer-hop transfer) hop
          (ende-web-fetch--transfer-phase transfer) 'curl
          (ende-web-fetch--transfer-connected transfer) nil)
    (condition-case caught
        (progn
          (setq stderr-process (ende-web-fetch--new-stderr-process transfer))
          (setf (ende-web-fetch--hop-stderr-buffer hop)
                (ende-web-fetch--transfer-stderr-buffer transfer))
          (setq process
                (make-process
                 :name (generate-new-buffer-name "ende-web-fetch")
                 :buffer nil :stderr stderr-process
                 :command (ende-web-fetch--command
                           transfer normalized addresses)
                 :connection-type 'pipe :coding 'binary :noquery t
                 :filter (apply-partially
                          #'ende-web-fetch--response-filter transfer hop)
                 :sentinel (apply-partially
                            #'ende-web-fetch--curl-sentinel transfer hop)))
          (setf (ende-web-fetch--transfer-process transfer) process))
      ((error quit)
       (let ((message (error-message-string caught)))
         (ende-web-fetch--release-process transfer)
         (ende-web-fetch--settle
          transfer 'failure nil
          (format "web_fetch could not start curl for %s: %s"
                  (ende-web-fetch--destination-label transfer) message)))))))

(defun ende-web-fetch--start-with-addresses
    (transfer normalized addresses allow-private)
  (condition-case caught
      (progn
        (ende-web-fetch--assert-addresses addresses allow-private)
        (setf (ende-web-fetch--transfer-addresses transfer) addresses)
        (ende-web-fetch--start-curl transfer normalized addresses))
    ((error quit)
     (ende-web-fetch--settle
      transfer 'failure nil
      (format "web_fetch destination authorization failed for %s: %s"
              (ende-web-fetch--destination-label transfer)
              (error-message-string caught))))))

(defun ende-web-fetch--handle-hop (transfer hop)
  (let ((status (ende-web-fetch--hop-status hop))
        (location (ende-web-fetch--header-value
                   (ende-web-fetch--hop-headers hop) "location")))
    (if (and (memq status '(301 302 303 307 308)) location)
        (if (>= (ende-web-fetch--transfer-redirects transfer)
                (ende-web-fetch--transfer-max-redirects transfer))
            (ende-web-fetch--settle
             transfer 'failure nil "web_fetch exceeded its redirect limit")
          (condition-case caught
              (let ((redirect
                     (ende-web-fetch--redirect-url
                      location (ende-web-fetch--transfer-current-url transfer))))
                (ende-web-fetch--release-hop hop)
                (setf (ende-web-fetch--transfer-hop transfer) nil
                      (ende-web-fetch--transfer-current-url transfer) redirect
                      (ende-web-fetch--transfer-redirects transfer)
                      (1+ (ende-web-fetch--transfer-redirects transfer)))
                (ende-web-fetch--start-request transfer))
            ((error quit)
             (ende-web-fetch--settle
              transfer 'failure nil
              (format "web_fetch redirect failed from %s: %s"
                      (ende-web-fetch--destination-label transfer)
                      (error-message-string caught))))))
      (condition-case caught
          (ende-web-fetch--settle
           transfer 'success (ende-web-fetch--finish-response transfer hop) nil)
        ((error quit)
         (ende-web-fetch--settle
          transfer 'failure nil
          (format "web_fetch response processing failed for %s: %s"
                  (ende-web-fetch--destination-label transfer)
                  (error-message-string caught))))))))

(defun ende-web-fetch--curl-sentinel (transfer hop process _event)
  (when (and (not (ende-web-fetch--transfer-settled transfer))
             (eq process (ende-web-fetch--transfer-process transfer))
             (memq (process-status process) '(exit signal failed closed)))
    (let* ((status (process-status process))
           (exit (process-exit-status process))
           (stderr (ende-web-fetch--stderr-text transfer))
           (failure (ende-web-fetch--transfer-failure transfer)))
      (ende-web-fetch--cancel-phase-timer transfer)
      (ende-web-fetch--release-process transfer)
      (setf (ende-web-fetch--transfer-phase transfer) nil
            (ende-web-fetch--transfer-failure transfer) nil)
      (cond
       (failure
        (ende-web-fetch--settle transfer 'failure nil failure))
       ((not (and (eq status 'exit) (zerop exit)))
        (ende-web-fetch--settle
         transfer 'failure nil
         (ende-web-fetch--curl-error transfer exit hop stderr)))
       ((null (ende-web-fetch--hop-status hop))
        (ende-web-fetch--settle
         transfer 'failure nil
         (format "web_fetch received no HTTP response from %s"
                 (ende-web-fetch--destination-label transfer))))
       (t
        (ende-web-fetch--handle-hop transfer hop))))))

(defun ende-web-fetch--start-with-permission
    (transfer normalized permission)
  (let ((allow-private (plist-get permission :allow-private-addresses)))
    (setf (ende-web-fetch--transfer-allow-private transfer) allow-private)
    (pcase (plist-get normalized :host-kind)
      ('dns
       (ende-web-fetch--resolve-host
        transfer (plist-get normalized :host)
        (lambda (addresses error)
          (unless (ende-web-fetch--transfer-settled transfer)
            (if error
                (ende-web-fetch--settle transfer 'failure nil error)
              (ende-web-fetch--start-with-addresses
               transfer normalized addresses allow-private))))))
      ((or 'ipv4 'ipv4-mapped-ipv6 'ipv6)
       (condition-case caught
           (let ((address (plist-get normalized :host)))
             (when (memq (plist-get normalized :host-kind)
                         '(ipv4-mapped-ipv6 ipv6))
               (setq address (substring address 1 -1)))
             (ende-web-fetch--start-with-addresses
              transfer normalized
              (list (ende-web-fetch-classify-address address)) allow-private))
         ((error quit)
          (ende-web-fetch--settle
           transfer 'failure nil
           (format "web_fetch destination authorization failed for %s: %s"
                   (ende-web-fetch--destination-label transfer)
                   (error-message-string caught))))))
      (_
       (ende-web-fetch--settle
        transfer 'failure nil "web_fetch encountered an unsupported host type")))))

(defun ende-web-fetch--start-request (transfer)
  (unless (ende-web-fetch--transfer-settled transfer)
    (let* ((normalized (ende-web-fetch--transfer-current-url transfer))
           (permission
            (condition-case caught
                (funcall (ende-web-fetch--transfer-authorize transfer)
                         normalized)
              ((error quit)
               (ende-web-fetch--settle
                transfer 'failure nil
                (format "web_fetch authorization failed for %s: %s"
                        (ende-web-fetch--destination-label transfer)
                        (error-message-string caught)))
               :failed))))
      (cond
       ((eq permission :failed) nil)
       ((not permission)
        (ende-web-fetch--settle
         transfer 'failure nil
         (format "URL is not authorized by this chat's URL resources: %s"
                 (plist-get normalized :url))))
       (t (ende-web-fetch--start-with-permission
           transfer normalized permission))))))

(defun ende-web-fetch--text-content-type (headers)
  "Return normalized textual MIME type in HEADERS or signal."
  (let* ((content-type (ende-web-fetch--header-value headers "content-type"))
         (media-type (and content-type
                          (downcase (string-trim
                                     (car (split-string content-type ";")))))))
    (unless (and media-type
                 (or (string-prefix-p "text/" media-type)
                     (member media-type '("application/json" "application/xml"
                                          "text/xml" "application/xhtml+xml"))
                     (string-match-p
                      "\\`application/[^;]+[+]\\(?:json\\|xml\\)\\'"
                      media-type)))
      (error "web_fetch does not support content type %s"
             (or media-type "(missing)")))
    (cons media-type content-type)))

(defun ende-web-fetch--decode-body (bytes content-type)
  "Decode raw text BYTES using charset parameter of CONTENT-TYPE."
  (let* ((charset (and (string-match
                        "[;][ \t]*charset[ \t]*=[ \t]*\\(?:\"\\([^\"]+\\)\"\\|\\([^; \t]+\\)\\)"
                        content-type)
                       (or (match-string 1 content-type)
                           (match-string 2 content-type))))
         (coding (and charset (coding-system-from-name charset))))
    (when (and charset (null coding))
      (error "unsupported response character set: %s" charset))
    (decode-coding-string bytes (or coding 'utf-8))))

(defun ende-web-fetch--redirect-url (location current)
  "Resolve redirect LOCATION relative to CURRENT and normalize the result."
  (unless (and (stringp location) (not (string-empty-p location)))
    (error "web_fetch redirect has no Location header"))
  (let ((case-fold-search t))
    (ende-url-normalize
     (if (string-match-p "\\`https?://" location)
         location
       (url-expand-file-name location (plist-get current :url))))))

(defun ende-web-fetch
    (url authorize callback &optional max-chars register-cancellation)
  (unless (and (functionp authorize) (functionp callback))
    (error "web_fetch requires authorization and completion functions"))
  (let (transfer)
    (condition-case caught
        (let* ((initial (ende-url-normalize url))
               (output-limit ende-web-fetch-max-output-chars)
               (limit (or max-chars output-limit))
               (connect-timeout ende-web-fetch-connect-timeout)
               (call-timeout ende-web-fetch-call-timeout))
          (unless (and (integerp limit) (> limit 0) (<= limit output-limit))
            (error "max_chars must be a positive integer no greater than %d"
                   output-limit))
          (unless (and (numberp connect-timeout) (> connect-timeout 0)
                       (numberp call-timeout) (> call-timeout 0))
            (error "web_fetch timeouts must be positive numbers"))
          (let* ((directory (ende-web-fetch--local-directory))
                 (deadline (+ (float-time) call-timeout)))
            (setq transfer
                  (ende-web-fetch--make-transfer
                   :deadline deadline
                   :directory directory
                   :callback callback
                   :authorize authorize
                   :register-cancellation register-cancellation
                   :requested-url (plist-get initial :url)
                   :current-url initial
                   :max-chars limit
                   :max-redirects ende-web-fetch-max-redirects
                   :connect-timeout connect-timeout
                   :max-response-bytes ende-web-fetch-max-response-bytes
                   :max-header-bytes ende-web-fetch-max-response-header-bytes
                   :max-stderr-bytes 8192
                   :redirects 0 :header-bytes 0 :body-bytes 0))
            (setf (ende-web-fetch--transfer-cancel-function transfer)
                  (lambda ()
                    (unless (ende-web-fetch--transfer-settled transfer)
                      (setf (ende-web-fetch--transfer-cancelled transfer) t)
                      (ende-web-fetch--settle
                       transfer 'cancelled nil "web_fetch was cancelled"))))
            (setf (ende-web-fetch--transfer-deadline-timer transfer)
                  (run-at-time (max 0 (- deadline (float-time))) nil
                               #'ende-web-fetch--overall-timeout transfer))
            (when register-cancellation
              (funcall register-cancellation
                       (ende-web-fetch--transfer-cancel-function transfer)))
            (ende-web-fetch--start-request transfer)
            transfer))
      ((error quit)
       (if transfer
           (ende-web-fetch--settle
            transfer 'failure nil (error-message-string caught))
         (condition-case nil
             (funcall callback nil (error-message-string caught))
           ((error quit) nil)))
       transfer))))

(provide 'ende-web-fetch)

;;; ende-web-fetch.el ends here
