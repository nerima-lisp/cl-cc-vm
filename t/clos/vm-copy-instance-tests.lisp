(in-package #:cl-cc/vm/clos/test)

(defun register-vm-copy-instance-tests ()
  (it-sequential "mop-copy-instance-compatible-with-vector-layout"
                 (let ((state (make-clos-vm)))
                   (make-test-instance state :R0 :R1 'mop-copy-vector '(x y))
                   (cl-cc/vm:vm-reg-set state :R2 10)
                   (cl-cc/vm:vm-reg-set state :R3 20)
                   (cl-cc/vm/protocols:execute-instruction
                    (cl-cc/vm/clos:make-vm-slot-write
                     :obj-reg :R1 :slot-name 'x :value-reg :R2)
                    state 0 nil)
                   (cl-cc/vm/protocols:execute-instruction
                    (cl-cc/vm/clos:make-vm-slot-write
                     :obj-reg :R1 :slot-name 'y :value-reg :R3)
                    state 0 nil)
                   (let* ((object (cl-cc/vm:vm-reg-get state :R1))
                          (copy (cl-cc/vm/clos:copy-instance object)))
                     (cl-cc/vm:vm-reg-set state :R4 copy)
                     (cl-cc/vm/protocols:execute-instruction
                      (cl-cc/vm/clos:make-vm-slot-read
                       :dst :R5 :obj-reg :R4 :slot-name 'x)
                      state 0 nil)
                     (cl-cc/vm/protocols:execute-instruction
                      (cl-cc/vm/clos:make-vm-slot-read
                       :dst :R6 :obj-reg :R4 :slot-name 'y)
                      state 0 nil)
                     (expect (and (vectorp copy)
                                  (not (hash-table-p copy))
                                  (= (cl-cc/vm:vm-reg-get state :R5) 10)
                                  (= (cl-cc/vm:vm-reg-get state :R6) 20))
                             :to-be-truthy)
                     (cl-cc/vm:vm-reg-set state :R7 99)
                     (cl-cc/vm/protocols:execute-instruction
                      (cl-cc/vm/clos:make-vm-slot-write
                       :obj-reg :R4 :slot-name 'x :value-reg :R7)
                      state 0 nil)
                     (cl-cc/vm/protocols:execute-instruction
                      (cl-cc/vm/clos:make-vm-slot-read
                       :dst :R8 :obj-reg :R1 :slot-name 'x)
                      state 0 nil)
                     (cl-cc/vm/protocols:execute-instruction
                      (cl-cc/vm/clos:make-vm-slot-read
                       :dst :R9 :obj-reg :R4 :slot-name 'x)
                      state 0 nil)
                     (expect (= (cl-cc/vm:vm-reg-get state :R8) 10)
                             :to-be-truthy)
                     (expect (= (cl-cc/vm:vm-reg-get state :R9) 99)
                             :to-be-truthy)))))
