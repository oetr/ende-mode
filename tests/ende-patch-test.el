;;; ende-patch-test.el --- Limited patch grammar tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Behavioral tests for the pure parser and exact logical-line hunk applier.

;;; Code:

(require 'ert)
(require 'ende-patch)

(ert-deftest ende-test/patch-parses-mixed-operations-and-applies-ordered-hunks ()
  (let* ((patch
          (concat
           "*** Begin Patch\n"
           "*** Add File: src/new.txt\n+alpha\n+beta\n"
           "*** Update File: src/main.txt\n"
           "@@\n one\n-two\n+TWO\n"
           "@@\n three\n-four\n+FOUR\n*** End of File\n"
           "*** Delete File: tmp/old.txt\n"
           "*** End Patch\n"))
         (operations (ende-patch-parse patch)))
    (should (equal (mapcar (lambda (operation)
                             (list (plist-get operation :operation)
                                   (plist-get operation :path)))
                           operations)
                   '((add "src/new.txt")
                     (update "src/main.txt")
                     (delete "tmp/old.txt"))))
    (should (equal (plist-get (car operations) :content) "alpha\nbeta\n"))
    (should (equal (ende-patch-apply-update
                    "one\ntwo\nthree\nfour\n"
                    (plist-get (cadr operations) :hunks))
                   "one\nTWO\nthree\nFOUR\n"))))

(ert-deftest ende-test/patch-preserves-final-newline-and-supports-empty-additions ()
  (let* ((hunks (plist-get
                 (car (ende-patch-parse
                       (concat "*** Begin Patch\n*** Update File: file\n"
                               "@@\n hello\n+world\n*** End Patch\n")))
                 :hunks))
         (empty-hunks (plist-get
                       (car (ende-patch-parse
                             (concat "*** Begin Patch\n*** Update File: empty\n"
                                     "@@\n+first\n*** End Patch\n")))
                       :hunks)))
    (should (equal (ende-patch-apply-update "hello" hunks) "hello\nworld"))
    (should (equal (ende-patch-apply-update "hello\n" hunks)
                   "hello\nworld\n"))
    (should (equal (ende-patch-apply-update "" empty-hunks) "first"))
    (should (equal (plist-get
                    (car (ende-patch-parse
                          "*** Begin Patch\n*** Add File: empty\n*** End Patch"))
                    :content)
                   ""))))

(ert-deftest ende-test/patch-rejects-malformed-ambiguous-and-misordered-input ()
  (dolist (patch
           (list
            "*** Add File: one\n*** End Patch"
            (concat "*** Begin Patch\n*** Add File: a\n+x\n"
                    "*** Delete File: a\n*** End Patch")
            "*** Begin Patch\n*** Delete File: ../secret\n*** End Patch"
            "*** Begin Patch\n*** Delete File: a\n+content\n*** End Patch"))
    (ert-info ((format "Rejected patch %S" patch))
      (should-error (ende-patch-parse patch))))
  (let* ((ambiguous (ende-patch-parse
                     (concat "*** Begin Patch\n*** Update File: a\n"
                             "@@\n-one\n+ONE\n*** End Patch")))
         (hunks (plist-get (car ambiguous) :hunks)))
    (should-error (ende-patch-apply-update "one\none" hunks)))
  (let* ((missing (ende-patch-parse
                   (concat "*** Begin Patch\n*** Update File: a\n"
                           "@@\n-x\n*** End Patch")))
         (hunks (plist-get (car missing) :hunks)))
    (should-error (ende-patch-apply-update "not-x" hunks)))
  (let* ((out-of-order
          (ende-patch-parse
           (concat "*** Begin Patch\n*** Update File: a\n"
                   "@@\n b\n@@\n a\n*** End Patch")))
         (hunks (plist-get (car out-of-order) :hunks)))
    (should-error (ende-patch-apply-update "a\nb\n" hunks)))
  (let* ((insertion (ende-patch-parse
                     (concat "*** Begin Patch\n*** Update File: a\n"
                             "@@\n+inserted\n*** End Patch")))
         (hunks (plist-get (car insertion) :hunks)))
    (should-error (ende-patch-apply-update "nonempty\n" hunks))))

(ert-deftest ende-test/patch-updates-long-sources-with-or-without-eof-constraint ()
  (let ((prefix (apply #'concat (make-list 20000 "line\n"))))
    (dolist (eof '(nil t))
      (let* ((patch (concat "*** Begin Patch\n*** Update File: file\n"
                            "@@\n-last\n+changed\n"
                            (when eof "*** End of File\n") "*** End Patch"))
             (hunks (plist-get (car (ende-patch-parse patch)) :hunks)))
        (should (equal (ende-patch-apply-update (concat prefix "last\n") hunks)
                       (concat prefix "changed\n")))))))

(provide 'ende-patch-test)

;;; ende-patch-test.el ends here
