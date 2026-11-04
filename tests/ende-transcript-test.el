;;; ende-transcript-test.el --- Transcript ownership contracts -*- lexical-binding: t; -*-

(require 'ert)
(require 'ende-transcript)

(ert-deftest ende-transcript-response-boundaries ()
  (let ((transcript (ende-transcript-create :task "Task")))
    (dotimes (_ 2)
      (ende-transcript-begin-response transcript)
      (ende-transcript-event transcript '(:type :delta :channel :content :text "partial"))
      (ende-transcript-event transcript '(:type :reasoning-summary :item "reused" :text "old"))
      (ende-transcript-event transcript '(:type :reasoning-summary :item "reused" :text "plan" :replace t))
      (ende-transcript-accept transcript '(:content "complete") nil)
      (ende-transcript-accept transcript '(:content "complete") nil))
    (ende-transcript-finish transcript 'cancelled)
    (ende-transcript-finish transcript 'cancelled)
    (let ((entries (ende-transcript-entries transcript)))
      (should (equal (mapcar #'ende-transcript-entry-kind entries)
                     '(response summary response summary status)))
      (should (equal (mapcar #'ende-transcript-entry-content (butlast entries))
                     '("complete" "plan" "complete" "plan")))
      (should (= (length entries)
                 (length (delete-dups (mapcar #'ende-transcript-entry-id entries))))))))

(provide 'ende-transcript-test)
;;; ende-transcript-test.el ends here
