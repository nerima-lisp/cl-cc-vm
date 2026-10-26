(in-package #:cl-cc-vm/test)
(describe-sequential "clos feature API"
  (it "classifies built-in arguments for VM generic dispatch"
    (expect
     (string= (symbol-name (cl-cc/vm/clos:vm-classify-arg nil nil)) "NULL")
     :to-be t)
    (expect
     (string= (symbol-name
               (cl-cc/vm/clos:vm-classify-arg "value" nil))
              "STRING")
     :to-be t)))
