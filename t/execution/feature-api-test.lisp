(in-package #:cl-cc-vm/test)
(describe-sequential "execution feature API"
  (it "exposes the VM register vector on an execution state"
    (let ((state (cl-cc/vm/execution:make-vm-state)))
      (expect
       (length (cl-cc/vm/execution:vm-state-registers state))
       :to-be cl-cc/vm/execution:+vm-register-count+)
      (setf (svref (cl-cc/vm/execution:vm-state-registers state) 7)
            :feature-test-value)
      (expect
       (svref (cl-cc/vm/execution:vm-state-registers state) 7)
       :to-be :feature-test-value))))
