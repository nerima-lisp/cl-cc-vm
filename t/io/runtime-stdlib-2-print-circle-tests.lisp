(in-package #:cl-cc/vm/io/test)
(defun register-runtime-stdlib-2-print-circle-tests ()
  (it-sequential "runtime-stdlib-2-print-circle-surface"
  (expect (find-symbol "*PRINT-CIRCLE*" :cl-cc/vm) :to-be-truthy)))
