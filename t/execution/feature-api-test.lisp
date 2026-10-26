(in-package #:cl-cc-vm/test)
(describe-sequential "execution feature API"
  (it "exposes register storage through an execution state"
    (let* ((state (cl-cc/vm/execution:make-vm-state))
           (registers (cl-cc/vm/execution:vm-state-registers state)))
      (expect (hash-table-p registers) :to-be t)
      (setf (gethash :feature-test-register registers)
            :feature-test-value)
      (expect
       (gethash :feature-test-register registers)
       :to-be :feature-test-value))))
