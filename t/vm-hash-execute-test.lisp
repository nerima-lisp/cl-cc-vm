;;;; t/vm-hash-execute-test.lisp — VM-MAKE-HASH-TABLE/VM-SETHASH/VM-GETHASH
;;;; instruction execution
;;;;
;;;; Exercises the hash-table instructions end to end through RUN-COMPILED,
;;;; the same pattern t/vm-list-execute-test.lisp and
;;;; t/vm-array-execute-test.lisp use for their respective instructions.

(in-package :cl-cc-vm/test)

(describe-sequential "VM-MAKE-HASH-TABLE/VM-SETHASH/VM-GETHASH instruction execution"
  (it "stores and retrieves a value under a key"
    (let* ((program (cl-cc/vm:make-vm-program
                      :instructions
                      (list (cl-cc/vm:make-vm-make-hash-table :dst :r0)
                            (cl-cc/vm:make-vm-const :dst :r1 :value 7)
                            (cl-cc/vm:make-vm-const :dst :r2 :value 99)
                            (cl-cc/vm:make-vm-sethash :key :r1 :value :r2 :table :r0)
                            (cl-cc/vm:make-vm-gethash :dst :r3 :key :r1 :table :r0)
                            (cl-cc/vm:make-vm-halt :reg :r3))
                      :result-register :r3)))
      (expect (cl-cc/vm:run-compiled program) :to-be 99)))

  (it "counts entries after two distinct writes"
    (let* ((program (cl-cc/vm:make-vm-program
                      :instructions
                      (list (cl-cc/vm:make-vm-make-hash-table :dst :r0)
                            (cl-cc/vm:make-vm-const :dst :r1 :value 1)
                            (cl-cc/vm:make-vm-const :dst :r2 :value 2)
                            (cl-cc/vm:make-vm-const :dst :r3 :value 10)
                            (cl-cc/vm:make-vm-const :dst :r4 :value 20)
                            (cl-cc/vm:make-vm-sethash :key :r1 :value :r3 :table :r0)
                            (cl-cc/vm:make-vm-sethash :key :r2 :value :r4 :table :r0)
                            (cl-cc/vm:make-vm-hash-table-count :dst :r5 :table :r0)
                            (cl-cc/vm:make-vm-halt :reg :r5))
                      :result-register :r5)))
      (expect (cl-cc/vm:run-compiled program) :to-be 2))))
