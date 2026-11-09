;;; ende-artifact-test.el --- Conversation image tests -*- lexical-binding: t; -*-

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

;; Tests for conversation images.

;;; Code:

(require 'ert)
(require 'ende-artifact)
(require 'ende-content)
(require 'ende-chat-test-helpers)
(require 'ox-html)
(require 'ox-latex)

(defconst ende-artifact-test-svg
  "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"80\" height=\"40\"><rect width=\"80\" height=\"40\" fill=\"blue\"/></svg>")

(defconst ende-artifact-test-png
  (base64-decode-string
   "iVBORw0KGgoAAAANSUhEUgAAAAIAAAADCAIAAAA2iEnWAAAAFElEQVQI12NkYPjPwMDAxMDAgKAAEkIBBbnAb5QAAAAASUVORK5CYII="))

(defconst ende-artifact-test-jpeg
  (base64-decode-string
   (concat "/9j/4AAQSkZJRgABAQAAAQABAAD/2wBDAAMCAgICAgMCAgIDAwMDBAYEBAQEBAgGBgUGCQgKCgkICQkKDA8MCgsOCwkJDRENDg8QEBEQCgwSExIQEw8QEBD/"
           "2wBDAQMDAwQDBAgEBAgQCwkLEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBD/wAARCAADAAIDAREAAhEBAxEB/"
           "8QAFAABAAAAAAAAAAAAAAAAAAAACf/EABQQAQAAAAAAAAAAAAAAAAAAAAD/xAAVAQEBAAAAAAAAAAAAAAAAAAAGCf/EABQRAQAAAAAAAAAAAAAAAAAAAAD/2gAMAwEAAhEDEQA/AD3VTB3/2Q==")))

(defmacro ende-artifact-test-with-files (&rest body)
  "Run BODY in an Org buffer visiting a temporary conversation."
  (declare (indent 0) (debug t))
  `(let ((directory (make-temp-file "ende-images-" t)))
     (unwind-protect
         (with-temp-buffer
           (org-mode)
           (setq buffer-file-name (expand-file-name "chat.org" directory))
           ,@body)
       (delete-directory directory t))))

(ert-deftest ende-artifact-image-handling ()
  (ende-artifact-test-with-files
    (let ((base (ende-artifact-base))
          (exec-path nil))
      (should-error (ende-artifact-store base "unsupported" "GIF89a" "image/gif"))
      (should-not (file-exists-p (ende-artifact-directory base "unsupported")))
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
        (dolist (fixture `(("image/png" . ,ende-artifact-test-png)
                           ("image/jpeg" . ,ende-artifact-test-jpeg)))
          (let* ((media (car fixture))
                 (bytes (cdr fixture))
                 (name (ende-artifact-store base "raster" bytes media))
                 (part (ende-artifact-resolve base "raster" name)))
            (should (= (plist-get part :width) 2))
            (should (= (plist-get part :height) 3))
            (should (equal (ende-content-image-data part) fixture))
            (should-error (ende-content-image-data
                           (plist-put (copy-sequence part) :path "/not-the-artifact")))
            (should-error (ende-artifact-store base "raster" (substring bytes 0 24) media))
            (should-error (ende-artifact-store base "raster" "not an image" media))))
        (let* ((bytes (concat (substring ende-artifact-test-jpeg 0 2)
                              (unibyte-string #xff #xe1 0 8 #xff #xc0 0 1 0 0)
                              (substring ende-artifact-test-jpeg 2)))
               (name (ende-artifact-store base "raster" bytes "image/jpeg")))
          (should (= (plist-get (ende-artifact-resolve base "raster" name) :width) 2))
          (aset bytes 5 #xff)
          (should-error (ende-artifact-store base "raster" bytes "image/jpeg")))))
    (let* ((base (ende-artifact-base))
           (name (ende-artifact-store base "chat" ende-artifact-test-png "image/png"))
           (image (ende-artifact-resolve base "chat" name))
           (messages (list (list :role "assistant" :content (list image))))
           (chat (ende-llm--request-body messages '(:model "vision")))
           (responses (ende-codex--request-messages messages))
           (batch (ende-openrouter-batch--request-body messages '(:model "vision") "fixture"))
           (ende-debug--session-directory directory)
           (ende-debugging-enabled t)
           (ende-debug--sequence 0))
      (should (string-match-p "data:image/png;base64," (json-encode chat)))
      (should (equal (mapcar (lambda (m) (alist-get :role m)) responses) '("user")))
      (should (cl-find "input_image" (alist-get :content (car responses))
                       :key (lambda (part) (alist-get :type part)) :test #'equal))
      (should (equal (alist-get :messages (alist-get :body batch)) (alist-get :messages chat)))
      (should-error (ende-llm--request-body messages '(:image-input nil)))
      (should-error (ende-codex--request-body messages '(:image-input nil)))
      (should-error (ende-openrouter-batch--request-body messages '(:image-input nil) "fixture"))
      (should (ende-debug-record-request (json-encode chat) chat messages nil "chat" "https://example.test/v1"))
      (dolist (text (list (ende-history--prompt-string messages)
                          (ende-artifact--read-bytes (expand-file-name "0001-request.json" directory))
                          (ende-artifact--read-bytes (expand-file-name "0001-messages.el" directory))))
        (should (string-match-p "image/png" text))
        (should-not (string-match-p "base64" text))
        (should-not (string-match-p (regexp-quote (base64-encode-string ende-artifact-test-png t)) text))
        (should-not (string-match-p (regexp-quote ende-artifact-test-png) text)))
      (let* ((url (ende-content-data-url image))
             (parser (ende-protocol-make-sse))
             (failure (should-error (ende-protocol-sse-feed parser (concat "data: {\"image\":\"" url "\n\n")))))
        (should-not (string-match-p "base64" (ende-transport-safe-error-text url)))
        (should-not (string-match-p "base64" (error-message-string failure)))))))

(ert-deftest ende-artifact-wire-content-is-stable ()
  (ende-artifact-test-with-files
    (let* ((base (ende-artifact-base))
           (names (cl-letf (((symbol-function 'ende-artifact--read-bytes)
                            (lambda (_) (ert-fail "New artifacts need no readback"))))
                    (list (ende-artifact-store base "stable" ende-artifact-test-png "image/png")
                          (ende-artifact-store base "stable" ende-artifact-test-jpeg "image/jpeg"))))
           (text (mapconcat (lambda (name)
                              (ende-artifact-render base "stable" name buffer-file-name)) names ""))
           (messages (list (list :role "assistant" :content (ende-artifact-content text))))
           (encode (lambda (history)
                     (list (json-encode (ende-llm--request-body history '(:model "vision")))
                           (json-encode (ende-codex--request-messages history)))))
           (expected (funcall encode messages)))
      (should (equal expected (funcall encode messages)))
      (ende-artifact-clear-cache)
      (should (equal expected (funcall encode messages)))
      (should (equal expected
                     (funcall encode (list (list :role "assistant"
                                                 :content (ende-artifact-content text)))))))))

(ert-deftest ende-artifact-worker-requests-preserve-wire-content ()
  :tags '(integration)
  (ende-artifact-test-with-files
    (let* ((base (ende-artifact-base))
           (name (ende-artifact-store base "request" ende-artifact-test-png "image/png"))
           (image (ende-artifact-resolve base "request" name))
           (messages (list (cons "user" "Draw")
                           (list :role "assistant" :content (list image))
                           (cons "user" "Continue")))
           (profile '(:name fixture :model "vision" :api openai-responses
                      :auth-method none :job-id 17 :url "https://example.test/responses"))
           (debug '("openai-responses" "https://example.test/responses"))
           (ende-debugging-enabled t)
           (ende-debug-provider-cache-comparisons t)
           (ende-debug--session-directory directory)
           (ende-debug--sequence 0)
           (ende-debug--baselines (make-hash-table :test #'equal))
           (ende-default-output-tokens 123))
      (dolist (builder '(ende-llm--request-body ende-responses--request-body ende-codex--request-body))
        (let* ((library (if (eq builder 'ende-llm--request-body) 'ende-llm 'ende-codex))
               (lifecycle (ende-transport-session-create nil #'ert-fail))
               seed done file actual record expected)
          (unwind-protect
              (progn
                (ende-request-prepare lifecycle library builder '(("user" . "seed")) profile debug
                                      (lambda (_data _body capture) (setq seed capture)))
                (ende-debug-record-response seed (ende-protocol-json-read-string "{\"id\":\"resp-worker-baseline\"}") t)
                (ende-debug-record-completion seed 'success nil 200 nil)
                (setq expected (ende-request--encode
                                builder messages profile debug
                                (ende-debug-prepare-request profile (car debug) (cadr debug) nil
                                                            (not (eq library 'ende-llm)))))
                (ende-artifact-clear-cache)
                (ende-request-prepare
                 lifecycle library builder messages profile debug
                 (lambda (data _body capture)
                   (setq file (plist-get data :file)
                         actual (ende-artifact--read-bytes file) record capture done t)))
                (should-not done)
                (clrhash ende-debug--baselines)
                (ende-test--wait-for (lambda () done))
                (should (equal (car expected) actual))
                (dolist (key '("input_fingerprints" "input_reasoning_items" "comparison_response_id"
                               "comparison_source" "comparison_request_sequence"))
                  (should (equal (alist-get key (plist-get (nth 2 expected) :metadata) nil nil #'equal)
                                 (alist-get key (plist-get record :metadata) nil nil #'equal)))))
            (ende-transport-session-cleanup lifecycle))
          (should-not (file-exists-p file)))))))

(ert-deftest ende-artifact-worker-finalizes-and-replays-wire-request ()
  :tags '(integration)
  (ende-artifact-test-with-files
    (let* ((base (ende-artifact-base))
           (name (ende-artifact-store base "finalize" ende-artifact-test-png "image/png"))
           (image (ende-artifact-resolve base "finalize" name))
           (messages (list (list :role "user" :content (list image))))
           (profile (list :model "vision" :parameters
                          (list :custom (vector (make-hash-table :test #'equal)
                                                nil :json-false t [] "λ"))))
           (lifecycle (ende-transport-session-create nil #'ert-fail))
           (finalization '(ende-controls--set-path ("type") "response.create"))
           prepared replay done files release)
      (unwind-protect
          (progn
            (ende-request-prepare
             lifecycle 'ende-codex #'ende-responses--request-body messages profile nil
             (lambda (data body _record)
               (should-not (assq :input body))
               (setq prepared data done t)) finalization)
            (ende-test--wait-for (lambda () done))
            (setq done nil files (list (plist-get prepared :file) (plist-get prepared :full-file)))
            (dolist (file files) (should (= (logand (file-modes file) #o777) #o600)))
            (let ((wire (ende-protocol-json-read-string
                         (ende-artifact--read-bytes (plist-get prepared :file))))
                  (full (ende-request--retained-body prepared profile)))
              (should (equal (gethash "type" wire) "response.create"))
              (should (string-match-p "data:image/png;base64," (json-encode full)))
              (should-not (assq :type full)))
            (ende-request-reprepare
             lifecycle 'ende-codex (plist-get prepared :retained) profile nil
             (lambda (data _body _record) (setq replay data done t)) finalization)
            (ende-test--wait-for (lambda () done))
            (should (eq (plist-get prepared :retained) (plist-get replay :retained)))
            (should (equal (plist-get prepared :full-file) (plist-get replay :full-file)))
            (setq files (append files (list (plist-get replay :file) (plist-get replay :full-file))))
            (should (equal (ende-artifact--read-bytes (plist-get prepared :file))
                           (ende-artifact--read-bytes (plist-get replay :file))))
            (setq release (ende-request-retained-read (plist-get prepared :retained))))
        (ende-transport-session-cleanup lifecycle))
      (should (file-exists-p (plist-get prepared :full-file)))
      (funcall release)
      (funcall release)
      (dolist (file files) (should-not (file-exists-p file))))))

(ert-deftest ende-artifact-round-trip-and-native-export ()
  (ende-artifact-test-with-files
    (let* ((base (ende-artifact-base))
           (name (ende-artifact-store base "figures" ende-artifact-test-png "image/png"))
           (path (plist-get (ende-artifact-resolve base "figures" name) :path))
           (storage (lambda () (list base "figures"))))
      (dolist (caption '("  The *road* goes on\r\nand on...  " nil "" " \t\r\n "))
        (let* ((normalized (ende-artifact--caption caption))
               (block (ende-artifact-render base "figures" name buffer-file-name caption))
               (shorthand (concat "Before\n#+ENDE_ARTIFACT: " name "\n"
                                  (when caption
                                    (concat "#+CAPTION: " (or normalized "") "\n"))
                                  "After\n"))
               (prepared (ende-artifact-prepare-text shorthand storage buffer-file-name))
               (persisted (plist-get prepared :content))
               (reference (car (ende-artifact--references block)))
               (html (org-export-string-as (concat block "Following prose\n") 'html t))
               (latex (org-export-string-as (concat block "Following prose\n") 'latex t)))
          (should (equal persisted (concat "Before\n" block "After\n")))
          (should (string-suffix-p "\n\n" block))
          (should (string-match-p "\nfile:\\./.ende-artifacts/figures/image-001.png\n\n" block))
          (should (= (plist-get reference :end) (1+ (length block))))
          (should (equal (substring block (1- (plist-get reference :link-begin))
                                    (1- (plist-get reference :link-end)))
                         "file:./.ende-artifacts/figures/image-001.png"))
          (should (equal (plist-get prepared :parts) (ende-artifact-content persisted)))
          (should (equal (cl-find :image (plist-get prepared :parts)
                                  :key (lambda (part) (plist-get part :type)))
                         (ende-artifact-resolve base "figures" name)))
          (let ((text (ende-content-readable (plist-get prepared :parts))))
            (should-not (string-match-p "ENDE_ARTIFACT\\|CAPTION:\\|file:" text))
            (when normalized (should (string-match-p (regexp-quote normalized) text))))
          (should (= 1 (cl-count :image (ende-artifact-content persisted #'ende-artifact--local-path)
                                       :key (lambda (part) (plist-get part :type)))))
          (should (equal persisted
                         (plist-get (ende-artifact-prepare-text
                                     persisted (lambda () (ert-fail "Persisted reference used storage"))
                                     buffer-file-name) :content)))
          (should (string-match-p "<img " html))
          (should (string-match-p (regexp-quote "\\includegraphics") latex))
          (if normalized
              (progn
                (should (equal (plist-get reference :caption) normalized))
                (should (string-match-p "The <b>road</b> goes on and on" html))
                (should (string-match-p (regexp-quote "\\caption") latex))
                (should (string-match-p (regexp-quote "The \\textbf{road} goes on and on") latex)))
            (should-not (plist-member reference :caption))
            (should-not (string-match-p "CAPTION:" persisted))
            (should-not (string-match-p (regexp-quote "\\caption") latex)))))
      (let* ((text (concat "#+ENDE_ARTIFACT: " name "\n#+CAPTION:\n"))
             (reference (car (ende-artifact--references text))))
        (should (plist-member reference :caption))
        (should (equal "" (plist-get reference :caption))))
      (should-error (ende-artifact--caption 42))
      (should (equal "Title #+NAME: not-a-directive"
                     (ende-artifact--caption "Title\n#+NAME: not-a-directive")))
      (should (string-match-p "\nfile:/" (ende-artifact-render base "figures" name nil)))
      (should (string-match-p "\nfile:\\.\\./"
                              (ende-artifact-render base "figures" name
                                                    (expand-file-name "sub/chat.org" directory))))
      (dolist (subdirectory '("has space" "has\ttab" "has\nnewline" "has::search"))
        (let ((failure (should-error
                        (ende-artifact-render (expand-file-name subdirectory directory)
                                              "figures" name buffer-file-name))))
          (should (string-match-p "plain Org file link" (error-message-string failure)))))
      (should (equal path (plist-get (car (ende-artifact-content
                                         (ende-artifact-render base "figures" name buffer-file-name))) :path))))))

(ert-deftest ende-artifact-storage-and-isolation ()
  (ende-artifact-test-with-files
    (let* ((default-directory temporary-file-directory)
           (base (ende-artifact-base))
           (name (ende-artifact-store base "chat" ende-artifact-test-svg "image/svg+xml"))
           (part (ende-artifact-resolve base "chat" name)))
      (should (equal base (expand-file-name ".ende-artifacts/" directory)))
      (should (equal (ende-artifact--read-bytes (plist-get part :path))
                     ende-artifact-test-svg))
      (should-error (ende-artifact-resolve base "other-chat" name))
      (should-error (ende-artifact-resolve base "chat" "missing.png"))
      (should-error (ende-artifact-resolve base "chat" "../bad.svg"))
      (should-error (ende-artifact-resolve base "../chat" name))
      (should-error (ende-artifact-resolve base "chat" (plist-get part :path)))
      (make-symbolic-link (plist-get part :path)
                          (expand-file-name "escape.svg" (ende-artifact-directory base "other-chat" t)))
      (should-error (ende-artifact-resolve base "other-chat" "escape.svg"))
      (make-symbolic-link directory (expand-file-name "escape" base))
      (should-error (ende-artifact-directory base "escape"))
      (let* ((reply "#+ENDE_ARTIFACTS_BASE_PATH: /etc/\n* Heading\n:PROPERTIES:\n:ENDE_CHAT_ID: evil\n:END:\nAnswer\n")
             (normalized (plist-get (ende-chat--normalize-response reply 2) :content)))
        (should (string-match-p "Answer" normalized))
        (should-not (string-match-p "evil\\|/etc/" normalized)))
      (dolist (setting '("images/" "absolute/"))
        (let ((configured (if (equal setting "absolute/")
                              (expand-file-name setting directory) setting)))
          (erase-buffer)
          (insert "#+ENDE_ARTIFACTS_BASE_PATH: " configured "\n* Topic\n"
                  "#+ENDE_ARTIFACTS_BASE_PATH: /not-document-config/\n")
          (let* ((base (ende-artifact-base))
                 (name (ende-artifact-store base "chat" ende-artifact-test-svg "image/svg+xml")))
            (should (equal base (expand-file-name setting directory)))
            (should (file-exists-p (plist-get (ende-artifact-resolve base "chat" name) :path)))))))))

(ert-deftest ende-artifact-citations-stay-text ()
  (with-temp-buffer
    (org-mode)
    (insert "* Topic\n")
    (let* ((document (ende-chat--build-document))
           (headline (car (ende-chat--document-headlines document)))
           (text "See [[file:missing.el::42][source]] and [[file:missing.svg][diagram]].")
           (result (list :content text)))
      (should (equal text (ende-chat--content document headline text 'assistant)))
      (should (equal text (plist-get (ende-chat--prepare-response
                                      (ende--make-job :target (ende-target--create :buffer (current-buffer))) result 2) :parts))))))

(ert-deftest ende-artifact-org-multimodal-and-attachments ()
  (ende-artifact-test-with-files
    (let* ((base (ende-artifact-base))
           (name (ende-artifact-store base "chat-123" ende-artifact-test-svg "image/svg+xml"))
           (standalone (expand-file-name "original.svg" directory))
           (citation "[[file:original.svg][Original]]"))
      (copy-file (plist-get (ende-artifact-resolve base "chat-123" name) :path) standalone)
      (insert "#+ENDE_ARTIFACTS_BASE_PATH: .ende-artifacts/\n"
              "* Topic\n:PROPERTIES:\n:ENDE_CHAT_ID: chat-123\n:END:\nExplain.\n"
              "** Assistant\n:PROPERTIES:\n:ENDE_ROLE: assistant\n:END:\nBefore image.\n"
              citation "\n"
              (ende-artifact-render base "chat-123" name buffer-file-name) "After image.\n"
              "** Follow up\n:PROPERTIES:\n:ENDE_ROLE: user\n:END:\nCompare.\n"
              "[[file:original.svg][Original]]\n")
      (goto-char (point-max))
      (let* ((messages (ende-chat-context-at-point))
             (assistant (nth 1 messages))
             (user (nth 2 messages))
             (parts (ende-content-body assistant)))
        (should (equal (mapcar #'ende-content-role messages) '("user" "assistant" "user")))
        (should (= 1 (cl-count :image parts :key (lambda (part) (plist-get part :type)))))
        (dolist (part parts)
          (when (eq (plist-get part :type) :text)
            (should-not (string-match-p "ENDE_ARTIFACT\\|.ende-artifacts" (plist-get part :text)))))
        (should (string-match-p (regexp-quote citation) (ende-content-readable parts)))
        (should (string-match-p "Original" (ende-content-readable (ende-content-body user))))
        (should (= 1 (cl-count :image (ende-content-body user) :key (lambda (p) (plist-get p :type))))))
      (should-error (ende-artifact-content
                     (concat "#+ENDE_ARTIFACT: " name "\nNo link.\n")))
      (delete-file (plist-get (ende-artifact-resolve base "chat-123" name) :path))
      (goto-char (point-max))
      (should-error (ende-chat-context-at-point))
      (let ((path (expand-file-name "image.gif" directory)))
        (with-temp-file path (insert "GIF89a"))
        (should-error (ende-artifact-content "[[file:image.gif]]" (lambda (_) path))))
      (let ((path (expand-file-name "note.txt" directory)))
        (with-temp-file path (insert "Attached text"))
        (let ((content (ende-artifact-content "See [[file:note.txt][Note]]" (lambda (_) path))))
          (should (stringp content))
          (should (string-match-p "Attached text" content)))))))

(ert-deftest ende-artifact-portable-reference ()
  (ende-artifact-test-with-files
    (let* ((base (ende-artifact-base))
           (name (ende-artifact-store base "source" ende-artifact-test-png "image/png"))
           (source (plist-get (ende-artifact-resolve base "source" name) :path)))
      (ende-artifact-store base "destination" ende-artifact-test-png "image/png")
      (insert "* Other question\n"
              (ende-artifact-render base "source" name buffer-file-name "Copied image"))
      (goto-char (point-max))
      (let* ((messages (ende-chat-context-at-point))
             (images (cl-remove-if-not (lambda (part) (eq (plist-get part :type) :image))
                                       (ende-content-body (car messages)))))
        (should (= 1 (length images)))
        (should (equal source (plist-get (car images) :path)))
        (should (string-match-p "Copied image" (ende-content-readable (ende-content-body (car messages))))))
      (cl-letf (((symbol-function 'create-image)
                 (lambda (path &rest _) (should (equal path source)) '(image :type png))))
        (ende-artifact-refresh)
        (should (= 1 (length ende-artifact--overlays))))
      (delete-file source)
      (should-error (ende-chat-context-at-point))
      (ende-artifact-refresh)
      (should-not ende-artifact--overlays))))

(ert-deftest ende-artifact-svg-safety-and-rasterization ()
  (ende-artifact-test-with-files
    (let* ((base (ende-artifact-base))
           (xml (concat "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"80\" height=\"40\""
                        " role=\"img\" aria-labelledby=\"title\"><title id=\"title\">A &amp; B</title>"
                        "<style type=\"text/css\"><![CDATA["
                        ".painted { fill: url('#dots'); stroke: rgb(0, 0, 0); }"
                        "]]></style>"
                        "<text>\n#+ENDE_ARTIFACT: text-not-a-reference\n</text>"
                        "<defs><pattern id=\"dots\" width=\"8\" height=\"8\" patternUnits=\"userSpaceOnUse\">"
                        "<circle cx=\"2\" cy=\"2\" r=\"1\"/></pattern></defs>"
                        "<rect class=\"painted\" width=\"80\" height=\"40\""
                        " style=\"opacity: .8; /* Inline styling */ stroke-width: 1\"/></svg>"))
           (code (concat "#+begin_src svg\n" xml "\n#+end_src\n"))
           (prepared (ende-artifact-prepare-text (concat xml "\n" code) (lambda () (list base "chat")) buffer-file-name))
           (parts (plist-get prepared :parts))
           (image (car parts)))
      (should (equal (ende-artifact--read-bytes (plist-get image :path)) xml))
      (should (string-suffix-p code (plist-get prepared :content)))
      (should (equal code (plist-get (ende-artifact-prepare-text code (lambda () (ert-fail "Literal storage")) buffer-file-name) :parts)))
      (dolist (payload '("<script/>" "<foreignObject/>" "<use href=\"file:/etc/passwd\"/>"
                         "<pattern href=\"https://example.test/tile.svg\"/>"
                         "<rect onload=\"alert(1)\"/>"
                         "<style>@import 'theme.css';</style>"
                         "<style>rect { fill: url(../secret.svg); }</style>"
                         "<rect fill=\"u\\72l('file:/secret')\"/>"
                         "<rect xmlns=\"http://www.w3.org/1999/xhtml\"/>"
                         "<rect style=\"fill:url(https://example.test/a)\"/>"))
        (should-error (ende-artifact-store base "chat"
                                           (concat "<svg xmlns=\"http://www.w3.org/2000/svg\">" payload "</svg>")
                                           "image/svg+xml")))
      (let ((exec-path nil))
        (should (ende-artifact--load-image (plist-get image :path)))
        (let ((failure (should-error (ende-content-data-url image))))
          (should (string-match-p "install rsvg-convert.*ImageMagick" (error-message-string failure)))))
      (when (or (executable-find "rsvg-convert") (executable-find "convert"))
        (let ((data (ende-content-image-data image)))
          (should (equal (car data) "image/png"))
          (should (eq (image-type-from-data (cdr data)) 'png)))))))

(ert-deftest ende-artifact-generated-svg-exclusions ()
  (let* ((literal (concat "#+begin_example\n" ende-artifact-test-svg "\n#+end_example\n"
                          "~literal\n" ende-artifact-test-svg "\nend~\n"
                          "=literal\n" ende-artifact-test-svg "\nend=\n"))
         (text (concat literal ende-artifact-test-svg "\n")))
    (should (equal (ende-artifact--references text t)
                   (list (list :part nil :begin (1+ (length literal))
                               :end (1+ (length text)) :svg ende-artifact-test-svg))))))

(ert-deftest ende-artifact-generated-svg-boundaries ()
  (let* ((svg (concat "<svg xmlns=\"http://www.w3.org/2000/svg\">\n"
                      "<g><svg><rect width=\"1\" height=\"1\"/></svg></g>\n"
                      "<!-- </svg>\n#+ENDE_ARTIFACT: comment.png\n-->\n"
                      "<text><![CDATA[</svg>\n<svg >\n"
                      "#+ENDE_ARTIFACT: cdata.png\n]]></text>\n"
                      "<text>\n#+ENDE_ARTIFACT: text.png\nfile:./text.png\n</text>\n</svg>"))
         (before "Before λ <!-- not an XML document\n")
         (first (concat " \t" svg " \t\n"))
         (between (concat "Middle\n" ende-artifact-test-svg " trailing prose\n"))
         (text (concat before first between ende-artifact-test-svg)))
    (should (equal (ende-artifact--references text t t)
                   (list (list :part nil :begin (1+ (length before))
                               :end (1+ (+ (length before) (length first))) :svg svg)
                         (list :part nil :begin (1+ (+ (length before) (length first)
                                                       (length between)))
                               :end (1+ (length text)) :svg ende-artifact-test-svg))))
    (should-error (ende-artifact--references
                   (concat text "\n<svg xmlns=\"http://www.w3.org/2000/svg\"><g>\n") t)
                  :type 'ende-artifact-validation-error)))

(ert-deftest ende-artifact-cache-reuse-and-invalidation ()
  (ende-artifact-test-with-files
    (let* ((base (ende-artifact-base))
           (name (ende-artifact-store base "cache" ende-artifact-test-svg "image/svg+xml"))
           (path (plist-get (ende-artifact-resolve base "cache" name) :path))
           (validate (symbol-function 'ende-artifact--validate-image))
           (convert (symbol-function 'call-process-region))
           (validations 0) (conversions 0))
      (ende-artifact-clear-cache)
      (cl-letf (((symbol-function 'ende-artifact--validate-image)
                 (lambda (&rest args) (cl-incf validations) (apply validate args)))
                ((symbol-function 'call-process-region)
                 (lambda (&rest args) (cl-incf conversions) (apply convert args))))
        (let ((image (ende-artifact--load-image path)))
          (dotimes (_ 3) (should (equal image (ende-artifact--load-image path))))
          (should (= 1 validations))
          (should-error (ende-artifact--load-image path "image/jpeg"))
          (should (= 1 validations))
          (when (or (executable-find "rsvg-convert") (executable-find "convert"))
            (let ((first (ende-content-image-data image)))
              (should (equal first (ende-content-image-data image)))
              (should (= 1 conversions)))))
        (let ((replacement (expand-file-name "replacement.svg" directory)))
          (with-temp-file replacement
            (insert (replace-regexp-in-string "blue" "red" ende-artifact-test-svg)))
          (rename-file replacement path t))
        (let ((image (ende-artifact--load-image path)))
          (should (= 2 validations))
          (when (or (executable-find "rsvg-convert") (executable-find "convert"))
            (ende-content-image-data image)
            (should (= 2 conversions))))
        (delete-file path)
        (should-error (ende-artifact--load-image path))))))

(ert-deftest ende-artifact-svg-bounds-work ()
  (should-error (ende-artifact--svg (make-string (* 8 1024 1024) ?\s)))
  (should-error
   (ende-artifact--svg
    (concat "<svg xmlns=\"http://www.w3.org/2000/svg\">"
            (apply #'concat (make-list 100 "<g>"))
            (apply #'concat (make-list 100 "</g>")) "</svg>")))
  (should-error
   (ende-artifact--validate-image
    "large.svg" "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"100000\" height=\"100000\"/>")))

(ert-deftest ende-artifact-response-preparation-is-atomic ()
  (ende-artifact-test-with-files
    (insert "* Question\n:PROPERTIES:\n:ENDE_CHAT_ID: chat\n:END:\nAsk\n")
    (let* ((base (ende-artifact-base))
           (existing (ende-artifact-store base "chat" ende-artifact-test-png "image/png"))
           (job (ende--make-job
                 :target (ende-target--create :buffer (current-buffer))
                 :instruction '(:question-id "chat")))
           (result (list :content
                         (concat ende-artifact-test-svg
                                 "\n<svg xmlns=\"http://www.w3.org/2000/svg\"><script/></svg>\n"))))
      (let* ((failure (should-error (ende-chat--prepare-response job result 2)
                                    :type 'ende-artifact-validation-error))
             (message (ende-chat--failure-text failure)))
        (should (string-match-p "Artifact .* in conversation chat:" message))
        (should-not (string-match-p (regexp-quote directory) message))
        (should-not (string-match-p "ende-artifact-validation-error\\|Invalid artifact" message))
        (should (string-match-p "Unsupported SVG element" message)))
      (should (equal (directory-files (ende-artifact-directory base "chat")
                                      nil directory-files-no-dot-files-regexp)
                     (list existing))))))

(ert-deftest ende-artifact-chat-repair-is-bounded ()
  (let* ((rejected "<svg xmlns=\"http://www.w3.org/2000/svg\"><script/></svg>\n")
         (diagnostic (error-message-string
                      (should-error (ende-artifact--validate-image "rejected.svg" rejected)))))
    (unwind-protect
        (dolist (response (list ende-artifact-test-svg rejected :error))
          (ende-test--reset)
          (ende-artifact-test-with-files
            (insert "* Question\nDraw\n")
            (goto-char (point-max))
            (let* ((ende-llm-function
                    (ende-test--fake-llm
                     (if (eq response :error) (list :error) (list rejected response))))
                   (job (ende-ask)))
              (should (eq (ende-job-state job)
                          (if (equal response ende-artifact-test-svg) 'completed 'failed)))
              (should (= (length ende-test--calls) (if (eq response :error) 1 2)))
              (unless (eq response :error)
                (let* ((messages (plist-get (cadr ende-test--calls) :messages))
                       (feedback (car (last messages))))
                  (should (equal (ende-content-body (nth (- (length messages) 2) messages)) rejected))
                  (should (equal (ende-content-role feedback) "user"))
                  (should (string-match-p (regexp-quote diagnostic) (ende-content-body feedback)))))
              (when (eq (ende-job-state job) 'completed)
                (should (= 1 (length (directory-files-recursively (ende-artifact-base) "\\.svg\\'"))))))))
      (ende-test--reset))))

(ert-deftest ende-artifact-top-level-svg-finalization-callbacks ()
  (ende-test--reset)
  (unwind-protect
      (ende-artifact-test-with-files
        (insert "* Question\nDraw a simple diagram.\n")
        (goto-char (point-max))
        (ende-mode 1)
        (let* ((ende-llm-function (ende-test--fake-llm (list ende-artifact-test-svg)))
               (invoke (symbol-function 'ende-agent--invoke))
               (prepare (symbol-function 'ende-chat--prepare-response))
               (apply-result (symbol-function 'ende-chat--apply-result))
               (refresh (symbol-function 'ende-artifact-refresh))
               (events nil))
          (cl-letf (((symbol-function 'ende-agent--invoke)
                     (lambda (callbacks key &rest arguments)
                       (when (memq key '(:before-final :on-final))
                         (setq events (append events (list key))))
                       (apply invoke callbacks key arguments)))
                    ((symbol-function 'ende-chat--prepare-response)
                     (lambda (&rest arguments)
                       (setq events (append events '(:prepare-response)))
                       (apply prepare arguments)))
                    ((symbol-function 'ende-chat--apply-result)
                     (lambda (&rest arguments)
                       (setq events (append events '(:apply-result)))
                       (apply apply-result arguments)))
                    ((symbol-function 'ende-artifact-refresh)
                     (lambda (&rest arguments)
                       (setq events (append events '(:preview-refresh)))
                       (apply refresh arguments)))
                    ((symbol-function 'create-image)
                     (lambda (&rest _) '(image :type svg))))
            (let ((job (ende-ask)))
              (ende-test--wait-for (lambda () (ende-job-terminal-p job)))
              (should (eq (ende-job-state job) 'completed)))
            (should (equal (cl-subseq events 0 5)
                           '(:before-final :prepare-response :on-final
                             :apply-result :preview-refresh)))
            (should (string-match-p "#\\+ENDE_ARTIFACT:" (buffer-string)))
            (should-not (string-match-p (regexp-quote ende-artifact-test-svg)
                                        (buffer-string)))
            (should (= 1 (length ende-artifact--overlays)))
            (goto-char (point-max))
            (let* ((messages (ende-chat-context-at-point))
                   (assistant
                    (car (last
                          (cl-remove-if-not
                           (lambda (message)
                             (equal (ende-content-role message) "assistant"))
                           messages))))
                   (parts (and assistant (ende-content-body assistant))))
              (should (cl-some (lambda (part)
                                 (eq (plist-get part :type) :image))
                               parts))))))
    (ende-test--reset)))

(ert-deftest ende-artifact-prepared-protocol-fragments ()
  (ende-test--reset)
  (unwind-protect
      (ende-artifact-test-with-files
        (let* ((base (ende-artifact-base))
               (source (ende-artifact-store base "source" ende-artifact-test-png "image/png"))
               (reused (ende-artifact-store base "generated" ende-artifact-test-png "image/png"))
               (pending (ende-artifact-store base "generated" ende-artifact-test-jpeg "image/jpeg"))
               (before (concat "* Earlier\n"
                               (ende-artifact-render base "source" source buffer-file-name)
                               "#+ENDE_ARTIFACT: " reused "\n#+CAPTION: Reused image\n"
                               "Literal block follows\n#+begin_example\n* Literal heading\n"
                               "#+ENDE_ARTIFACT: not-an-image\n#+end_example\n"))
               (after (concat "** Final\n" ende-artifact-test-svg
                              "\nAfter SVG\n#+begin_example\n* Trailing literal"))
               (message (lambda (text)
                          (ende-protocol-json-read-string
                           (json-encode (list :type "message" :role "assistant"
                                              :content (vector (list :type "output_text" :text text)))))))
               (opaque (ende-protocol-json-read-string
                        "{\"type\":\"reasoning\",\"encrypted_content\":\"retained\"}"))
               (result (list :content (concat before after)
                             :output-parts
                             (list (list :type :text :text before)
                                   (list :type :generated-image :bytes ende-artifact-test-png
                                         :media-type "image/png")
                                   (list :type :text :text after))
                             :protocol-items (list 'openai-responses
                                                   (funcall message before) opaque
                                                   (funcall message after) opaque)))
               (original (prin1-to-string result))
               (ende-llm-function (ende-test--fake-llm (list result) t))
               (prepare (symbol-function 'ende-artifact-prepare-text))
               (store (symbol-function 'ende-artifact-store))
               (preparations 0) (stores 0))
          (insert "* Topic\n:PROPERTIES:\n:ENDE_CHAT_ID: generated\n:END:\n** Question\nDraw\n")
          (goto-char (point-max))
          (let ((job (ende-ask)))
            (setf (ende-job-instruction job)
                  (plist-put (ende-job-instruction job) :generated-artifacts
                             (list (list :name reused :caption "Default caption")
                                   (list :name pending :caption "Pending tool image"))))
            (cl-letf (((symbol-function 'ende-artifact-prepare-text)
                       (lambda (&rest arguments)
                         (cl-incf preparations) (apply prepare arguments)))
                      ((symbol-function 'ende-artifact-store)
                       (lambda (&rest arguments)
                         (cl-incf stores) (apply store arguments))))
              (ende-test--complete-call (car ende-test--calls)))
            (should (eq (ende-job-state job) 'completed))
            (should (= preparations 1))
            (should (= stores 2))
            (should (equal original (prin1-to-string result)))
            (let* ((entry (ende-transcript-response (ende-job-transcript job)))
                   (data (ende-transcript-entry-data entry))
                   (prepared (plist-get data :prepared))
                   (texts (cl-remove-if-not
                           (lambda (item) (eq (ende-transcript-entry-kind item) 'text))
                           (plist-get data :items)))
                   (parts (plist-get prepared :parts))
                   (retained (prin1-to-string parts))
                   (images (cl-remove-if-not (lambda (part) (eq (plist-get part :type) :image)) parts))
                   (rendered (ende-chat--render-transcript job)))
              (should (eq result (plist-get data :result)))
              (should (equal (mapcar #'ende-transcript-entry-content texts) (list before after)))
              (cl-mapc (lambda (item normalized)
                         (should (eq normalized (plist-get (ende-transcript-entry-data item) :normalized))))
                       texts (plist-get prepared :fragments))
              (should (= 5 (length images)))
              (should (equal (mapcar (lambda (part) (plist-get part :path)) images)
                             (cl-mapcan
                              (lambda (normalized)
                                (mapcar (lambda (part) (plist-get part :path))
                                        (cl-remove-if-not
                                         (lambda (part) (eq (plist-get part :type) :image))
                                         (ende-artifact-content (plist-get normalized :content)))))
                              (plist-get prepared :fragments))))
              (should (string-match-p (regexp-quote "* Earlier") (ende-content-readable parts)))
              (should (string-match-p (regexp-quote "** Final") (ende-content-readable parts)))
              (should (string-match-p "Pending tool image" rendered))
              (should (string-match-p ",\\* Trailing literal\n#\\+end_example" rendered))
              (should-not (string-match-p (regexp-quote ende-artifact-test-svg) rendered))
              (should (string-match-p (concat ":" ende-chat-reply-reference-property ": t") rendered))
              (cl-letf (((symbol-function 'ende-artifact-prepare-text) (lambda (&rest _) (ert-fail "Reprepared")))
                        ((symbol-function 'ende-artifact-store) (lambda (&rest _) (ert-fail "Stored while rendering"))))
                (should (equal rendered (ende-chat--render-transcript job))))
              (should (equal retained (prin1-to-string parts))))
            (let* ((document (ende-chat--build-document))
                   (headlines (ende-chat--document-headlines document))
                   (earlier (cl-find "Earlier" headlines :key (lambda (h) (org-element-property :raw-value h))
                                     :test #'equal))
                   (final (cl-find "Final" headlines :key (lambda (h) (org-element-property :raw-value h))
                                   :test #'equal)))
              (should (= 4 (org-element-property :level earlier)))
              (should (= 3 (org-element-property :level final)))
              (should (equal "Agent: Reply" (org-element-property :raw-value (ende-chat--parent document earlier))))
              (should (equal "Agent: Reply" (org-element-property :raw-value (ende-chat--parent document final))))))))
    (ende-test--reset)))

(ert-deftest ende-artifact-opaque-only-image-completion ()
  (ende-test--reset)
  (unwind-protect
      (ende-artifact-test-with-files
        (let* ((result (list :content ""
                             :output-parts (list (list :type :generated-image :bytes ende-artifact-test-png
                                                       :media-type "image/png"))
                             :protocol-items
                             (list 'openai-responses
                                   (ende-protocol-json-read-string
                                    "{\"type\":\"image_generation_call\",\"id\":\"generated\"}"))))
               (original (prin1-to-string result))
               (ende-llm-function (ende-test--fake-llm (list result)))
               (prepare (symbol-function 'ende-artifact-prepare-text))
               (preparations 0))
          (insert "* Topic\n** Question\nDraw\n")
          (goto-char (point-max))
          (cl-letf (((symbol-function 'ende-artifact-prepare-text)
                     (lambda (&rest arguments)
                       (cl-incf preparations) (apply prepare arguments))))
            (let* ((job (ende-ask))
                   (data (ende-transcript-entry-data (ende-transcript-response (ende-job-transcript job))))
                   (items (plist-get data :items)))
              (should (eq (ende-job-state job) 'completed))
              (should (= preparations 1))
              (should (equal original (prin1-to-string result)))
              (should (equal '(opaque text) (mapcar #'ende-transcript-entry-kind items)))
              (should (equal "" (ende-transcript-entry-content (cadr items))))
              (should (string-match-p "#\\+ENDE_ARTIFACT:" (plist-get (ende-chat--render-response-items
                                                                      job (ende-transcript-response
                                                                           (ende-job-transcript job))) :reply)))))))
    (ende-test--reset)))

(ert-deftest ende-artifact-structural-persistence ()
  (ende-test--reset)
  (unwind-protect
      (ende-artifact-test-with-files
        (let* ((base (ende-artifact-base))
               (name (ende-artifact-store base "source" ende-artifact-test-png "image/png"))
               (reference (ende-artifact-render base "source" name buffer-file-name))
               (literal (concat "  #+BeGiN_example\n"
                                "  #+ENDE_ARTIFACT: ../invalid.svg\n"
                                "  * Literal heading\n" ende-artifact-test-svg "\n"))
               (source (concat "Before\n" reference
                               "* Real heading\n" literal "  #+END_EXAMPLE\n"
                               "#+begin_src org\n#+ENDE_ARTIFACT: missing.png\n"
                               ende-artifact-test-svg))
               (ende-llm-function
                (ende-test--fake-llm
                 (list (list :content source :tool-calls
                             '((:id "inspection" :name "project_info" :arguments "{}")))
                       (list :output-parts
                             (list (list :type :text :text source)
                                   (list :type :generated-image :bytes ende-artifact-test-png
                                         :media-type "image/png")
                                   (list :type :text :text "After native image\n** Last heading\n")))))))
          (insert (ende-test--chat-config-block `((root ,directory) (tools project_info)))
                  "* Topic\n** Question\n:PROPERTIES:\n:ENDE_CHAT_ID: destination\n:END:\nAsk\n")
          (goto-char (point-max))
          (let ((job (ende-ask)))
            (ende-test--wait-for (lambda () (ende-job-terminal-p job)))
            (should (eq (ende-job-state job) 'completed)))
          (should-not (string-match-p "#\\+CAPTION:" (buffer-string)))
          (let* ((live (plist-get (nth 1 ende-test--calls) :messages))
                 (turn (cl-find-if (lambda (message) (plist-get message :tool-calls)) live)))
            (should (= 1 (cl-count :image (ende-content-body turn)
                                         :key (lambda (part) (plist-get part :type)))))
            (should (string-match-p (regexp-quote "* Real heading")
                                    (ende-content-readable (ende-content-body turn)))))
          (goto-char (point-max))
          (insert "\n** Follow up\nContinue\n")
          (let* ((history (ende-chat-context-at-point))
                 (images (cl-mapcan
                          (lambda (message)
                            (let ((body (ende-content-body message)))
                              (unless (stringp body)
                                (cl-remove-if-not (lambda (part) (eq (plist-get part :type) :image)) body))))
                          history))
                 (text (mapconcat (lambda (message) (ende-content-readable (ende-content-body message))) history "\n")))
            (should (= 3 (length images)))
            (should (string-match-p (regexp-quote literal) text))
            (should (string-match-p (regexp-quote "* Real heading") text))
            (should (string-match-p (regexp-quote "** Last heading") text))
            (let* ((final (ende-content-body
                           (car (last (cl-remove-if-not
                                       (lambda (message) (equal "assistant" (ende-content-role message))) history)))))
                   (types (mapcar (lambda (part) (plist-get part :type)) final)))
              (should (equal types '(:text :image :text :image :text)))))
          (let* ((job (ende--make-job
                       :target (ende-target--create :buffer (current-buffer))
                       :instruction (list :question-id "destination" :generated-artifacts
                                          (list (list :name name :caption nil)))))
                 ;; Pending host data must not be appended inside the raw block.
                 (pending-base (ende-artifact-directory base "destination" t)))
            (copy-file (plist-get (ende-artifact-resolve base "source" name) :path)
                       (expand-file-name name pending-base))
            (let ((prepared (ende-chat--prepare-response job (list :content literal) 1)))
              (should (= 1 (cl-count :image (plist-get prepared :parts)
                                           :key (lambda (part) (plist-get part :type)))))))
          (should (equal literal (ende-artifact-content literal #'ende-artifact--local-path)))
          (erase-buffer)
          (insert "* Topic\n** User\n  #+BeGiN_example\n"
                  "*** Literal child\n#+ende_artifact: missing.png\nfile:./missing.png\n\n"
                  "** Follow up\n:PROPERTIES:\n:ENDE_ROLE: user\n:END:\nContinue\n")
          (goto-char (point-max))
          (let ((history (ende-chat-context-at-point)))
            (should (cl-every (lambda (message) (stringp (ende-content-body message))) history)))))
    (ende-test--reset)))

(ert-deftest ende-artifact-incremental-refresh-matches-full ()
  (ende-artifact-test-with-files
    (let* ((base (ende-artifact-base))
           (name (ende-artifact-store base "chat" ende-artifact-test-svg "image/svg+xml")))
      (insert "* Topic\n:PROPERTIES:\n:ENDE_CHAT_ID: chat\n:END:\n")
      (dotimes (_ 2)
        (insert (ende-artifact-render base "chat" name buffer-file-name)))
      (cl-letf (((symbol-function 'create-image) (lambda (&rest _) '(image :type svg))))
        (cl-labels
            ((spans ()
               (sort (mapcar (lambda (overlay)
                               (should (eq (overlay-buffer overlay) (current-buffer)))
                               (cons (overlay-start overlay) (overlay-end overlay)))
                             ende-artifact--overlays)
                     (lambda (left right) (< (car left) (car right)))))
             (check-refresh (expected)
               (should ende-artifact--dirty)
               (when (timerp ende-artifact--timer)
                 (cancel-timer ende-artifact--timer))
               (ende-artifact--refresh-dirty (current-buffer))
               (let ((incremental (spans)))
                 (ende-artifact-refresh)
                 (should (equal incremental (spans)))
                 (should (= expected (length incremental))))))
          (ende-mode 1)
          (should (= 2 (length (spans))))
          (let ((overlays (copy-sequence ende-artifact--overlays))
                (validate (symbol-function 'ende-artifact--validate-image))
                (validations 0))
            (cl-letf (((symbol-function 'ende-artifact--validate-image)
                       (lambda (&rest args) (cl-incf validations) (apply validate args))))
              (dotimes (_ 3) (ende-artifact-refresh))
              (should (equal overlays ende-artifact--overlays))
              (should (zerop validations))
              (goto-char (point-min))
              (insert "Moved without changing references\n")
              (check-refresh 2)
              (should (equal overlays ende-artifact--overlays))
              (let* ((overlay (car overlays))
                     (display (overlay-get overlay 'after-string))
                     (replacement (expand-file-name "replacement.svg" directory))
                     (path (plist-get (ende-artifact-resolve base "chat" name) :path)))
                (with-temp-file replacement
                  (insert (replace-regexp-in-string "blue" "red" ende-artifact-test-svg)))
                (rename-file replacement path t)
                (ende-artifact-refresh)
                (should (equal overlays ende-artifact--overlays))
                (should-not (eq display (overlay-get overlay 'after-string)))
                (should (= 1 validations)))))
          (let* ((overlay (car ende-artifact--overlays))
                 (display (overlay-get overlay 'after-string)))
            (goto-char (overlay-start overlay))
            (insert " ")
            (check-refresh 2)
            (should (memq overlay ende-artifact--overlays))
            (should (eq display (overlay-get overlay 'after-string))))
          (let* ((second (ende-artifact-store base "chat" ende-artifact-test-svg "image/svg+xml"))
                 (overlay (car ende-artifact--overlays))
                 (old-signature (overlay-get overlay 'ende-artifact-signature)))
            (goto-char (overlay-start overlay))
            (forward-line 1)
            (let ((begin (point)))
              (insert "#+CAPTION: Preview caption\n")
              (check-refresh 2)
              (should (memq overlay ende-artifact--overlays))
              (goto-char begin)
              (delete-region begin (line-beginning-position 2))
              (check-refresh 2)
              (goto-char begin)
              (insert "#+CAPTION: Preview caption\n")
              (check-refresh 2))
            (let ((begin (point)))
              (delete-region begin (line-end-position))
              (insert "file:./.ende-artifacts/chat/" second))
            (check-refresh 2)
            (should (memq overlay ende-artifact--overlays))
            (should-not (equal old-signature (overlay-get overlay 'ende-artifact-signature))))
          (goto-char (point-max))
          (insert "\n** Follow up\n")
          (check-refresh 2)
          (goto-char (point-min))
          (search-forward "file:")
          (delete-char -1)
          (check-refresh 1)
          (insert ":")
          (check-refresh 2)
          ;; A delimiter edit can affect references far beyond neighboring lines.
          (goto-char (point-min))
          (search-forward "#+ENDE_ARTIFACT:")
          (beginning-of-line)
          (let ((start (point)))
            (insert "  #+BeGiN_example\n")
            (check-refresh 0)
            (delete-region start (+ start (length "  #+BeGiN_example\n")))
            (check-refresh 2))
          (ende-mode -1))))))

(ert-deftest ende-artifact-preview-layout-and-folding ()
  (ende-artifact-test-with-files
    (let* ((base (ende-artifact-base))
           (name (ende-artifact-store base "chat" ende-artifact-test-svg "image/svg+xml"))
           (reference (ende-artifact-render base "chat" name buffer-file-name "Preview caption")))
      (insert "* Topic\n** First\n" reference "\n** Last\n" reference)
      (font-lock-ensure)
      (cl-letf (((symbol-function 'create-image) (lambda (&rest _) '(image :type svg))))
        (ende-artifact-refresh)
        (should (= 2 (length ende-artifact--overlays)))
        (dolist (overlay ende-artifact--overlays)
          (goto-char (overlay-end overlay))
          (should (eolp))
          (beginning-of-line)
          (should (looking-at-p "file:")))
        (save-window-excursion
          (set-window-buffer (selected-window) (current-buffer))
          (dolist (action '(org-fold-hide-subtree org-fold-show-children org-fold-show-subtree))
            (goto-char (point-min))
            (funcall action)
            (let ((height (cdr (window-text-pixel-size nil (point-min) (point-max) t)))
                  (strings (mapcar (lambda (overlay) (overlay-get overlay 'after-string))
                                   ende-artifact--overlays)))
              (unwind-protect
                  (progn
                    (dolist (overlay ende-artifact--overlays)
                      (overlay-put overlay 'after-string nil))
                    (should (= (- height (cdr (window-text-pixel-size nil (point-min) (point-max) t)))
                               (if (eq action 'org-fold-show-subtree) 2 0))))
                (cl-mapc (lambda (overlay string) (overlay-put overlay 'after-string string))
                         ende-artifact--overlays strings)))))))))

(ert-deftest ende-artifact-preview-lifecycle ()
  (ende-artifact-test-with-files
    (let* ((base (ende-artifact-base))
           (svg (replace-regexp-in-string
                 "<rect" "<style>rect { fill: blue; }</style><rect"
                 ende-artifact-test-svg))
           (name (ende-artifact-store base "chat" svg "image/svg+xml")))
      (insert "* Topic\n:PROPERTIES:\n:ENDE_CHAT_ID: chat\n:END:\n"
              (ende-artifact-render base "chat" name buffer-file-name "Reload caption")
              "#+begin_src svg\n" ende-artifact-test-svg "\n#+end_src\n")
      (cl-letf (((symbol-function 'create-image) (lambda (&rest _) '(image :type svg))))
        (ende-mode 1)
        (should (= 1 (length ende-artifact--overlays)))
        (let ((overlay (car ende-artifact--overlays)))
          (dolist (action (list (lambda () (ende-mode 1)) #'ende-reload))
            (goto-char (point-max))
            (insert "Edit before reconfiguration\n")
            (let ((dirty ende-artifact--dirty))
              (funcall action)
              (should-not (marker-buffer (car dirty)))
              (should-not (marker-buffer (cdr dirty))))
            (should-not ende-artifact--timer)
            (should (= 1 (length ende-artifact--overlays)))
            (should (eq overlay (car ende-artifact--overlays)))
            (should (= 1 (cl-count #'ende-artifact--before-change before-change-functions)))
            (should (= 1 (cl-count #'ende-artifact--after-change after-change-functions)))
            (should (= 1 (cl-count #'ende-artifact--disable kill-buffer-hook)))))
        (goto-char (point-max))
        (insert "Edited text\n")
        (should ende-artifact--timer)
        (ende-mode -1)
        (should-not ende-artifact--timer)
        (should-not ende-artifact--overlays)
        (should-not ende-artifact--literal-regions)
        (should-not (memq #'ende-artifact--before-change before-change-functions))
        (should-not (memq #'ende-artifact--after-change after-change-functions))
        (ende-mode 1)
        (should (= 1 (length ende-artifact--overlays)))
        (ende-mode -1)))))

(ert-deftest ende-image-configuration-lifecycle ()
  (let* ((ende-image-api 'openai)
         (ende-image-auth-profile nil)
         (ende-image-endpoint "https://images.example.test/v1/images")
         (ende-image-model nil)
         (ende-custom-providers
          (append ende-test--custom-providers (ende-test--custom-profile 'studio)))
         (initial (mapcar #'default-value ende-image--configuration-variables))
         (draft '(stable-diffusion studio
                  "http://127.0.0.1:8080/sdapi/v1/txt2img" "image-model"))
         buffer)
    (save-window-excursion
      (unwind-protect
          (dolist (save '(nil t))
            (ende-image-configure)
            (setq buffer (current-buffer))
            (should (text-property-any (point-min) (point-max) 'face 'success))
            (cl-mapc #'set ende-image--configuration-variables draft)
            (ende-image--render-configuration)
            (should (text-property-any (point-min) (point-max) 'face 'error))
            (should (equal initial (mapcar #'default-value ende-image--configuration-variables)))
            (if save
                (progn
                  (let ((ende-image-endpoint "not-a-url"))
                    (should-error (ende-image--configure-save)))
                  (should (buffer-live-p buffer))
                  (should (equal initial (mapcar #'default-value ende-image--configuration-variables)))
                  (ende-image--configure-save))
              (ende-image--configure-quit))
            (should-not (buffer-live-p buffer))
            (should (equal (if save draft initial)
                           (mapcar #'default-value ende-image--configuration-variables))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest ende-image-api-contract ()
  (dolist (case '((openai (:sample_method "euler" :batch_count 2))
                  (stable-diffusion (:sampler_name "euler" :scheduler "normal" :batch_size 2))))
    (let* ((ende-image-api (car case))
           (profile (list :image-api ende-image-api :model "image-model"))
           (arguments (append '(:prompt "Draw a box." :seed -1 :caption "A box") (cadr case)))
           (body (ende-image--request-body arguments profile))
           (definition (cl-find "generate_image" (ende-tool-definitions)
                                :key (lambda (item)
                                       (alist-get :name (alist-get :function item)))
                                :test #'equal))
           (properties (alist-get :properties
                                  (alist-get :parameters (alist-get :function definition))))
           (encoded (mapcar (lambda (bytes) (base64-encode-string bytes t))
                            (list ende-artifact-test-png ende-artifact-test-jpeg)))
           (payload (if (eq ende-image-api 'stable-diffusion)
                        `((images . ,(vconcat encoded)) (info . "{}") (parameters . ()))
                      `((data . ,(vconcat (mapcar (lambda (data) `((b64_json . ,data)))
                                                 encoded))))))
           (response (json-parse-string (json-encode payload))))
      (cl-loop for (key value) on arguments by #'cddr do
               (should (assq key properties))
               (unless (eq key :caption)
                 (should (equal value (gethash (substring (symbol-name key) 1) body)))))
      (should-not (gethash "caption" body))
      (should (equal (gethash "model" body)
                     (unless (eq ende-image-api 'stable-diffusion) "image-model")))
      (should (= 1 (hash-table-count
                    (ende-image--request-body '(:prompt "Draw")
                                              (list :image-api ende-image-api)))))
      (should (equal (mapcar (lambda (part) (plist-get part :bytes))
                            (ende-image--response response profile))
                     (list ende-artifact-test-png ende-artifact-test-jpeg)))
      (dolist (data encoded)
        (should-not (string-match-p (regexp-quote data)
                                    (json-encode (ende-content-redact response)))))
      (should-error (ende-image--request-body '(:prompt "Draw" :width 0) profile))
      (should-error (ende-image--request-body '(:prompt "Draw" :model "untrusted") profile))
      (should-error (ende-image--response (make-hash-table) profile)))))

(ert-deftest ende-artifact-caption-lifecycle ()
  (ende-test--reset)
  (unwind-protect
      (dolist (scenario '((nil nil nil) ("" nil nil)
                          ("  Tool\ncaption  " nil "Tool caption")
                          ("Tool caption" "" "Tool caption")
                          ("Tool caption" "#+CAPTION:\n" nil)
                          ("Tool caption" "#+CAPTION: Override\n" "Override")))
        (ende-test--reset)
        (ende-artifact-test-with-files
          (pcase-let* ((`(,caption ,caption-line ,expected) scenario)
                       (normalized (ende-artifact--caption caption))
                       (base (ende-artifact-base))
                       (source (ende-artifact-store base "source" ende-artifact-test-png "image/png"))
                       (arguments (append '((:prompt . "Draw a box."))
                                          (when caption (list (cons :caption caption)))))
                       (ende-llm-function
                        (ende-test--fake-llm
                         (list (list :tool-calls
                                     (list (list :id "image" :name "generate_image"
                                                 :arguments (json-encode arguments))))
                               (list :content "#+ENDE_ARTIFACT: image-001.png\n"
                                     :tool-calls '((:id "inspect" :name "project_info" :arguments "{}")))
                               (list :content
                                     (concat "Final reply\n"
                                             (ende-artifact-render base "source" source buffer-file-name "Copied source")
                                             (when caption-line
                                               (concat "#+ENDE_ARTIFACT: image-001.png\n" caption-line)))))
                         t)))
            (insert (ende-test--chat-config-block `((root ,directory) (tools generate_image project_info)))
                    "* Topic\n:PROPERTIES:\n:ENDE_CHAT_ID: generated\n:END:\n** Question\nDraw\n")
            (goto-char (point-max))
            (cl-letf (((symbol-function 'ende-image--request)
                       (lambda (request-arguments _profile base id callback _register)
                         (should (equal (ende-tools--argument request-arguments "prompt")
                                        "Draw a box."))
                         (funcall callback
                                  (list (ende-artifact-store base id ende-artifact-test-png "image/png")
                                        (ende-artifact-store base id ende-artifact-test-png "image/png")) nil))))
              (let ((job (ende-ask)))
                (ende-test--complete-call (car ende-test--calls))
                (let* ((descriptors (list (list :name "image-001.png" :caption normalized)
                                          (list :name "image-002.png" :caption normalized)))
                       (messages (plist-get (cadr ende-test--calls) :messages))
                       (tool-message (cl-find-if
                                      (lambda (message)
                                        (equal (plist-get message :tool-call-id) "image"))
                                      messages))
                       (result (gethash "result" (json-parse-string
                                                  (plist-get tool-message :content))))
                       (handoff (car (last messages)))
                       (body (ende-content-body handoff)))
                  (should (eq t (gethash "ok" result)))
                  (should (equal (gethash "artifacts" result)
                                 ["image-001.png" "image-002.png"]))
                  (should (= 2 (hash-table-count result)))
                  (should-not (plist-get (ende-job-instruction job) :generated-artifacts))
                  (should (equal descriptors (plist-get (ende-job-instruction job) :reply-artifacts)))
                  (should (= 2 (cl-count :image body :key (lambda (part) (plist-get part :type)))))
                  (when normalized
                    (should (= 2 (ende-test--count-matches (regexp-quote normalized) (ende-content-readable body)))))
                  (ende-test--complete-call (cadr ende-test--calls))
                  (ende-test--wait-for (lambda () (= (length ende-test--calls) 3)))
                  (should (equal descriptors (plist-get (ende-job-instruction job) :reply-artifacts)))
                  (should-not (plist-get (ende-job-instruction job) :generated-artifacts))
                  (let* ((turn (cl-find-if
                                (lambda (message)
                                  (equal (plist-get (car (plist-get message :tool-calls)) :id) "inspect"))
                                (plist-get (nth 2 ende-test--calls) :messages)))
                         (parts (ende-content-body turn)))
                    (should (= 1 (cl-count :image parts :key (lambda (part) (plist-get part :type)))))
                    (when normalized
                      (should (string-match-p (regexp-quote normalized) (ende-content-readable parts)))))
                  (ende-test--complete-call (nth 2 ende-test--calls))
                  (should (eq (ende-job-state job) 'completed)))
                (let* ((headline (car (last (ende-test--chat-response-headlines))))
                       (text (buffer-substring-no-properties (org-element-property :begin headline)
                                                             (org-element-property :end headline)))
                       (references (ende-artifact--references text))
                       (images (cl-remove-if-not (lambda (part) (eq (plist-get part :type) :image))
                                                (ende-artifact-content text))))
                  ;; The copied source has the same filename as the generated image.
                  (should (= 3 (length references) (length images)))
                  (should (equal (mapcar (lambda (part) (plist-get part :path)) images)
                                 (list (plist-get (ende-artifact-resolve base "source" source) :path)
                                       (plist-get (ende-artifact-resolve base "generated" "image-001.png") :path)
                                       (plist-get (ende-artifact-resolve base "generated" "image-002.png") :path))))
                  (should (equal (mapcar (lambda (ref) (plist-get ref :caption)) references)
                                 (list "Copied source" expected normalized)))
                  (should-not (string-match-p "^[ \t]*#\\+CAPTION:[ \t]*$" text))
                  (goto-char (point-max))
                  (insert "\n** Follow up\nContinue\n")
                  (let* ((history (ende-chat-context-at-point))
                         (final (car (last (cl-remove-if-not
                                           (lambda (message) (equal (ende-content-role message) "assistant")) history)))))
                    (should (equal images
                                   (cl-remove-if-not (lambda (part) (eq (plist-get part :type) :image))
                                                     (ende-content-body final)))))))))))
    (ende-test--reset)))

(ert-deftest ende-image-settings-are-frozen-per-run ()
  (let ((ende-image-api 'stable-diffusion)
        (ende-image-auth-profile nil)
        (ende-image-endpoint "http://127.0.0.1:8080/sdapi/v1/txt2img")
        (ende-image-model nil))
    (ende-test--reset)
    (unwind-protect
        (ende-artifact-test-with-files
          (ende-reload)
          (insert (ende-test--chat-config-block `((root ,directory) (tools generate_image)))
                  "* Topic\n** Question\nDraw a box.\n")
          (goto-char (point-max))
          (let* ((ende-llm-function
                  (ende-test--fake-llm
                   '((:tool-calls ((:id "image" :name "generate_image"
                                   :arguments "{\"prompt\":\"Draw a box.\"}")))
                     (:content "#+ENDE_ARTIFACT: image-001.png\n")) t))
                 (job (ende-ask))
                 (settings (ende-image--profile))
                 (definitions (plist-get (ende-job-profile job) :tool-definitions)))
            (setq ende-image-api 'openai
                  ende-image-endpoint "https://images.example.test/v1/images"
                  ende-image-model "another-model")
            (should-not (equal definitions (ende-tool-definitions)))
            (cl-letf (((symbol-function 'ende-image--request)
                       (lambda (_arguments profile base id callback _register)
                         (should (equal profile settings))
                         (funcall callback
                                  (list (ende-artifact-store base id ende-artifact-test-png "image/png")) nil))))
              (ende-test--complete-call (car ende-test--calls))
              (ende-test--complete-call (cadr ende-test--calls)))
            (should (eq (ende-job-state job) 'completed))))
      (ende-test--reset))))

(ert-deftest ende-artifact-generation-http-lifecycle ()
  (ende-artifact-test-with-files
    (let* ((ende-image-api 'openai)
           (ende-image-auth-profile 'studio)
           (ende-image-endpoint "https://192.168.1.50:8080/v1/images/generations")
           (ende-image-model "fixture-image-model")
           (ende-custom-providers
            (append ende-test--custom-providers
                    (ende-test--custom-profile 'studio :base-url "https://192.168.1.50:8080/v1"
                                               :auth-method 'auth-source
                                               :request-headers '("X-Studio: image"))))
           (real-start (symbol-function 'ende-curl-start-request))
           (real-send (symbol-function 'process-send-string))
           request sent cancel parts failure (calls 0))
      (dolist (scenario '(png jpeg unsupported cancelled))
        (setq request nil sent nil cancel nil parts nil failure nil calls 0)
        (let* ((bytes (pcase scenario
                        ('jpeg ende-artifact-test-jpeg)
                        ('unsupported "GIF89a")
                        (_ ende-artifact-test-png)))
               (response (json-encode `(("data" . [(("b64_json" . ,(base64-encode-string bytes t)))])))))
          (cl-letf (((symbol-function 'ende-curl--auth-secret) (lambda (_) nil))
                    ((symbol-function 'ende-curl-start-request)
                     (lambda (&rest arguments)
                       (let ((profile (plist-get arguments :profile)))
                         (should (eq (plist-get profile :name) ende-image-auth-profile))
                         (should (equal (plist-get arguments :url) ende-image-endpoint))
                         (should (equal (plist-get profile :request-headers)
                                        '("X-Studio: image"))))
                       (let ((output (plist-get arguments :output-file)))
                         (setq arguments
                               (plist-put arguments :command
                                          (lambda (_headers _config)
                                            (list "sh" "-c" "cat > \"$1\"" "sh" output)))))
                       (setq request (apply real-start arguments))
                       request))
                    ((symbol-function 'process-send-string)
                     (lambda (process data)
                       (if (equal (process-name process) "ende-generate-image")
                           (progn
                             (setq sent data)
                             (funcall real-send process response))
                         (funcall real-send process data)))))
            (ende-image--request
             '(:prompt "Draw a box.") (ende-image--profile) (ende-artifact-base) "generated"
             (lambda (value error) (cl-incf calls) (setq parts value failure error))
             (lambda (function) (setq cancel function))))
          (let ((data (json-parse-string sent)))
            (should (= (hash-table-count data) 2))
            (should (equal (gethash "model" data) "fixture-image-model"))
            (should (equal (gethash "prompt" data) "Draw a box.")))
          (if (eq scenario 'cancelled) (funcall cancel)
            (with-temp-file (plist-get request :headers-file)
              (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n")))
          (let ((deadline (+ (float-time) 3)))
            (while (and (zerop calls) (< (float-time) deadline))
              (accept-process-output nil 0.02)))
          (should (= calls 1))
          (pcase scenario
            ('cancelled (should (string-match-p "cancelled" failure)))
            ('unsupported (should (string-match-p "unsupported.*PNG or JPEG" failure)))
            (_ (should-not failure)
               (should (equal (ende-content-image-data
                               (ende-artifact-resolve (ende-artifact-base) "generated" (car parts)))
                              (cons (if (eq scenario 'jpeg) "image/jpeg" "image/png") bytes)))))
          (should-not cancel)
          (should-not (buffer-live-p (plist-get request :response-buffer)))
          (should-not (file-exists-p (plist-get request :headers-file))))))))

(provide 'ende-artifact-test)

;;; ende-artifact-test.el ends here
