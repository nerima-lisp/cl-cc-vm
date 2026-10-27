(in-package #:cl-cc/vm/numbers/test)

(defun register-runtime-stdlib-3-ryu-tests ()
  (it-sequential "stdlib3-ryu-float-to-string"
  (expect (cl-cc/vm:vm-float-to-string 1.5d0) :to-equal "1.5")
  (expect (search "e" (cl-cc/vm:vm-float-to-string 1000.0d0 :mode :exponential)) :to-be-truthy)
  (expect (cl-cc/vm:vm-float-to-string sb-ext:double-float-positive-infinity) :to-equal "+inf.0")
  (expect (cl-cc/vm:vm-float-to-string (sb-kernel:make-double-float #x7FF80000 0)) :to-equal "+nan.0")))
