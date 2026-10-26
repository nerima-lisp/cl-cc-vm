(in-package #:cl-cc-vm/test)
(describe-sequential "environment feature API"
  (it "produces the same random sequence from the same seed"
    (let* ((first-state (cl-cc/vm/environment:make-random-state 42))
           (second-state (cl-cc/vm/environment:make-random-state 42))
           (first-value (cl-cc/vm/environment:random 100 first-state))
           (second-value (cl-cc/vm/environment:random 100 second-state)))
      (expect
       (and (integerp first-value) (<= 0 first-value) (< first-value 100))
       :to-be t)
      (expect first-value :to-be second-value))))
