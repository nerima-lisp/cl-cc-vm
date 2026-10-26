(in-package #:cl-cc-vm/test)
(describe-sequential "debugging feature API"
  (it "evaluates a form with VM step mode enabled"
    (expect (cl-cc/vm/debugging:vm-step '(+ 1 2)) :to-be 3)))
