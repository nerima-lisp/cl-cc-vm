;;;; t/vm-array-execute-test.lisp — VM-MAKE-ARRAY/VM-AREF/VM-ASET instruction execution
;;;;
;;;; Exercises the array instructions end to end through RUN-COMPILED, the
;;;; same pattern t/vm-list-execute-test.lisp uses for cons cells.

(in-package :cl-cc-vm/test)

(describe-sequential "VM-MAKE-ARRAY/VM-AREF/VM-ASET instruction execution"
  (it "creates an array and reads back a freshly-written element"
    (let* ((program (cl-cc/vm:make-vm-program
                      :instructions
                      (list (cl-cc/vm:make-vm-const :dst :r0 :value 3)
                            (cl-cc/vm:make-vm-make-array :dst :r1 :size-reg :r0)
                            (cl-cc/vm:make-vm-const :dst :r2 :value 1)
                            (cl-cc/vm:make-vm-const :dst :r4 :value 9)
                            (cl-cc/vm:make-vm-aset :array-reg :r1 :index-reg :r2 :val-reg :r4)
                            (cl-cc/vm:make-vm-aref :dst :r3 :array-reg :r1 :index-reg :r2)
                            (cl-cc/vm:make-vm-halt :reg :r3))
                      :result-register :r3)))
      (expect (cl-cc/vm:run-compiled program) :to-be 9)))

  (it "keeps writes to different indices independent"
    (let* ((program (cl-cc/vm:make-vm-program
                      :instructions
                      (list (cl-cc/vm:make-vm-const :dst :r0 :value 3)
                            (cl-cc/vm:make-vm-make-array :dst :r1 :size-reg :r0)
                            (cl-cc/vm:make-vm-const :dst :r2 :value 0)
                            (cl-cc/vm:make-vm-const :dst :r3 :value 2)
                            (cl-cc/vm:make-vm-const :dst :r4 :value 100)
                            (cl-cc/vm:make-vm-const :dst :r5 :value 200)
                            (cl-cc/vm:make-vm-aset :array-reg :r1 :index-reg :r2 :val-reg :r4)
                            (cl-cc/vm:make-vm-aset :array-reg :r1 :index-reg :r3 :val-reg :r5)
                            (cl-cc/vm:make-vm-aref :dst :r6 :array-reg :r1 :index-reg :r2)
                            (cl-cc/vm:make-vm-aref :dst :r7 :array-reg :r1 :index-reg :r3)
                            (cl-cc/vm:make-vm-add :dst :r8 :lhs :r6 :rhs :r7)
                            (cl-cc/vm:make-vm-halt :reg :r8))
                      :result-register :r8)))
      (expect (cl-cc/vm:run-compiled program) :to-be 300))))
