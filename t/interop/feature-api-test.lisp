(in-package #:cl-cc/vm/interop/test)

(defun register-feature-api-tests ()
  nil
  (describe-sequential "interop feature API"
                       (it "returns regex match bounds and captured text"
                           (multiple-value-bind (start end groups)
                               (cl-cc/vm/interop:regex-scan "(ab)" "zabx")
                             (expect start :to-be 1)
                             (expect end :to-be 3)
                             (expect (string= (aref groups 0) "ab") :to-be t)))))
