(in-package #:cl-cc-vm/test)
(describe-sequential "io feature API"
  (it "reads a balanced form from a stream"
    (with-input-from-string (stream "(1 2)")
      (expect
       (equal (cl-cc/vm/io:read-balanced-form stream) '(1 2))
       :to-be t))))
