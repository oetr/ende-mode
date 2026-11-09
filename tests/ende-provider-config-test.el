;;; ende-provider-config-test.el --- Provider value contracts -*- lexical-binding: t; -*-

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

;; Tests for provider value contracts.

;;; Code:

(require 'ert)
(require 'ende-provider-config)

(ert-deftest ende-provider-config-identities-and-inheritance ()
  (let* ((ende-providers '(work local))
         (ende-builtin-provider-settings
          '((openai :temperature 0.2)
            (work :provider openai :account work)))
         (ende-custom-providers
          '((local :api openai-chat-completions :auth-method none
                   :base-url "http://127.0.0.1:8080/v1/"
                   :models ("a" "b")
                   :model-settings (("a" :context-window 32 :parameters (:top_k 20)))
                   :context-window 128 :output-tokens nil
                   :parameters (:top_p 0.9))))
         (ende-default-output-tokens 100)
         (local (ende-provider-config-resolve 'local))
         (work (ende-provider-config-resolve 'work)))
    (should (eq (plist-get work :service) 'openai))
    (should (eq (plist-get work :api) 'openai-responses))
    (should (equal (plist-get work :auth-source-user) "work"))
    (should-not (plist-member work :temperature))
    (should-not (plist-get work :model))
    (should (equal (plist-get local :url) "http://127.0.0.1:8080/v1/chat/completions"))
    (should (equal (plist-get local :models-url) "http://127.0.0.1:8080/v1/models"))
    (should (equal (plist-get local :model) "a"))
    (should (= (plist-get local :context-window) 32))
    (should-not (plist-get local :output-tokens))
    (should (equal (plist-get local :parameters) '(:top_p 0.9 :top_k 20)))
    (should (= (plist-get (ende-provider-config-resolve 'local "b") :context-window) 128))
    (should-error (ende-provider-config-resolve 'local "unlisted"))
    (should-error (ende-provider-config-resolve 'missing))
    (should-error (ende-provider-config-resolve 'work nil 'anthropic-messages))))

(ert-deftest ende-provider-config-schema-is-strict ()
  (let ((ende-providers nil)
        (ende-builtin-provider-settings nil)
        (ende-custom-providers nil))
    (should (ende-provider-config-validate))
    (dolist (settings '((:model "a") (:temperature 1 :temperature 2)
                        (:provider anthropic) (:api anthropic-messages)
                        (:account ../work) (:account Work) (:transport openrouter-batch)
                        (:parameters (:model "bad"))
                        (:model-settings (("dormant" :parameters (:input []))))
                        (:max-concurrency 0) (:models ())
                        (:models ("a" "a"))
                        (:models ("a") :model-settings (("b" :output-tokens 1)))
                        (:request-timeout 1.0e+INF)))
      (let ((ende-builtin-provider-settings (list (cons 'openai settings))))
        (should-error (ende-provider-config-validate))))
    (let ((ende-providers '(unknown)))
      (should-error (ende-provider-config-validate)))
    (let ((ende-providers '(openai openai)))
      (should-error (ende-provider-config-validate)))
    (let ((ende-builtin-provider-settings '((openai) (openai))))
      (should-error (ende-provider-config-validate)))
    (dolist (name '(openai anthropic t nil :bad))
      (let ((ende-custom-providers
             (list (list name :api 'openai-responses :base-url "https://example.test/v1"
                         :auth-method 'none))))
        (should-error (ende-provider-config-validate))))))

(ert-deftest ende-provider-config-responses-transports ()
  (let ((ende-providers nil) (ende-custom-providers nil))
    (dolist (settings '((openai :transport websocket)
                        (openai-codex :auth-method chatgpt-plan-oauth :transport websocket)
                        (openai-codex :auth-method codex-oauth :transport websocket)))
      (let* ((ende-builtin-provider-settings (list settings))
             (profile (ende-provider-config-resolve (car settings) "test")))
        (should (eq (plist-get profile :transport) 'websocket))
        (should (eq (plist-get (ende-provider-config-resolve
                               (car settings) "test" nil '(:transport auto)) :transport)
                    'auto))
        (should-error (ende-provider-config-validate-transport
                       (plist-put (copy-sequence profile) :url "https://other.test/responses")))
        (when (eq (car settings) 'openai)
          (should-error (ende-provider-config-resolve 'openai "test" 'openai-chat-completions)))))
    (dolist (settings '((deepseek :transport websocket) (openrouter :transport websocket)
                        (openai :api openai-chat-completions :transport websocket)))
      (let ((ende-builtin-provider-settings (list settings)))
        (should-error (ende-provider-config-validate))))
    (let ((ende-builtin-provider-settings '((openai :transport auto))))
      (should (eq (plist-get (ende-provider-config-resolve 'openai) :transport) 'auto))
      (should (eq (plist-get (ende-provider-config-resolve
                             'openai "test" nil '(:transport websocket)) :transport)
                  'websocket))
      (should-error (ende-provider-config-resolve
                     'openai "test" 'openai-chat-completions '(:transport websocket)))
      (should-error (ende-provider-config-resolve 'deepseek "test" nil '(:transport websocket))))
    (let ((ende-builtin-provider-settings '((openrouter :transport openrouter-batch))))
      (should (eq (plist-get (ende-provider-config-resolve
                             'openrouter "test" nil '(:transport openrouter-batch)) :transport)
                  'openrouter-batch)))))

(ert-deftest ende-provider-config-json-values ()
  (let* ((base '(:top_p 0.9 :nested (:x 1) :keep nil))
         (overrides '("top_p" 0.8 :nested (:y [t :json-false nil])))
         (merged (ende-provider-config-merge-parameters base overrides))
         (json (ende-provider-config-parameters merged)))
    (should (= (gethash "top_p" json) 0.8))
    (should-not (gethash "x" (gethash "nested" json)))
    (should (equal (gethash "y" (gethash "nested" json))
                   (vector t json-false nil)))
    (should (equal base '(:top_p 0.9 :nested (:x 1) :keep nil)))
    (should-not (ende-provider-config-merge-parameters base nil))
    (should (equal (json-read-from-string (json-encode json))
                   '((keep) (top_p . 0.8) (nested (y . [t :json-false nil])))))))

(ert-deftest ende-provider-config-rejects-invalid-values ()
  (dolist (value (list '(:x 1 "x" 2) '(:x) '(:x . 1)
                       '(:x symbol) '(:x (:y 1 :y 2))
                       (list :x 1.0e+INF) (list :x 0.0e+NaN)
                       '(:nested (:api_key "not-a-real-key")) '(:token "bad")))
    (should-error (ende-provider-config-parameters value)))
  (let ((cyclic (list :x nil)))
    (setcar (cdr cyclic) cyclic)
    (should-error (ende-provider-config-parameters cyclic))
    (should-error (ende-provider-config-copy cyclic))))

(ert-deftest ende-provider-config-protection-and-detachment ()
  (dolist (parameters '((:model "m") (:reasoning (:summary "auto"))
                         (:reasoning nil)))
    (should-error
     (ende-provider-config-parameters
      parameters '(("model") ("reasoning" "effort")))))
  (let* ((source (list :string (copy-sequence "abc")
                       :array (vector (copy-sequence "xyz"))
                       :object (make-hash-table :test #'equal)))
         (copy (ende-provider-config-copy source)))
    (aset (plist-get copy :string) 0 ?X)
    (aset (aref (plist-get copy :array) 0) 0 ?X)
    (puthash "new" t (plist-get copy :object))
    (should (equal (plist-get source :string) "abc"))
    (should (equal (plist-get source :array) ["xyz"]))
    (should (zerop (hash-table-count (plist-get source :object))))))

(ert-deftest ende-provider-config-copy-graphs ()
  (let* ((shared (list (copy-sequence "shared")))
         (table (make-hash-table :test #'equal))
         (source (vector (cons shared shared) table (number-sequence 1 20000))))
    (puthash (copy-sequence "key") shared table)
    (let* ((copy (ende-provider-config-copy source))
           (pair (aref copy 0)))
      (should (equal (aref copy 0) (aref source 0)))
      (should (equal (gethash "key" (aref copy 1)) shared))
      (should (equal (aref copy 2) (aref source 2)))
      (should-not (eq (car pair) (cdr pair)))
      (aset (caar pair) 0 ?X)
      (aset (car (gethash "key" (aref copy 1))) 0 ?Y)
      (should (equal shared '("shared")))
      (should (= (length (aref copy 2)) 20000)))
    (puthash "cycle" source table)
    (should-error (ende-provider-config-copy source)))
  (let* ((source (cons (copy-sequence "car") (copy-sequence "cdr")))
         (copy (ende-provider-config-copy source)))
    (should (equal source copy))
    (aset (cdr copy) 0 ?X)
    (should (equal (cdr source) "cdr"))
    (setcdr source source)
    (should-error (ende-provider-config-copy source))))

(provide 'ende-provider-config-test)
;;; ende-provider-config-test.el ends here
