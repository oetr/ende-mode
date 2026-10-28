;;; ende-css-test.el --- Static CSS tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'ende-css)

(ert-deftest ende-css-static-styling ()
  (should (ende-css-validate
           (concat "/* Diagram */ rect.node, g > text:first-child {"
                   "f\\69ll: u\\72l(\"#paint\");"
                   "font: italic 12px/1.5 'A; B: C';"
                   "stroke-width: calc(1px + (2px * 2)); }"
                   "@media (prefers-color-scheme: dark) { text { fill: white; } }")
           'stylesheet))
  (should (ende-css-validate "fill: blue; opacity: .5 !important"))
  (should (ende-css-validate "url('#paint')" 'value)))

(ert-deftest ende-css-rejects-unsafe-or-incomplete-styling ()
  (dolist (case '(("@import 'theme.css';" . stylesheet)
                  ("fill: url(../secret.svg)")
                  ("fill: u\\72l('\\66ile:/secret')")
                  ("fill: url(/* comment */#paint)")
                  ("font: expression(alert(1))")
                  ("fill: var(--unvalidated)")
                  ("rect { fill: red;" . stylesheet)
                  ("fill: red; /* incomplete")))
    (should-error (ende-css-validate (car case) (cdr case)))))

(provide 'ende-css-test)

;;; ende-css-test.el ends here
