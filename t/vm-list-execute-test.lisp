;;;; t/vm-list-execute-test.lisp — VM-CONS/VM-CAR/VM-CDR instruction execution
;;;;
;;;; Exercises the cons-cell instructions end to end through RUN-COMPILED,
;;;; the same pattern t/vm-boundary-test.lisp's "runs a compiled program end
;;;; to end" case uses, rather than calling internal helpers directly --
;;;; this is real guest-visible VM behavior, not an implementation detail.

(in-package :cl-cc-vm/test)

(describe-sequential "VM-CONS/VM-CAR/VM-CDR instruction execution"
  (it "conses two values and extracts the car"
    (let* ((program (cl-cc/vm:make-vm-program
                      :instructions
                      (list (cl-cc/vm:make-vm-const :dst :r0 :value 11)
                            (cl-cc/vm:make-vm-const :dst :r1 :value 22)
                            (cl-cc/vm:make-vm-cons :dst :r2 :car-src :r0 :cdr-src :r1)
                            (cl-cc/vm:make-vm-car :dst :r3 :src :r2)
                            (cl-cc/vm:make-vm-halt :reg :r3))
                      :result-register :r3)))
      (expect (cl-cc/vm:run-compiled program) :to-be 11)))

  (it "conses two values and extracts the cdr"
    (let* ((program (cl-cc/vm:make-vm-program
                      :instructions
                      (list (cl-cc/vm:make-vm-const :dst :r0 :value 11)
                            (cl-cc/vm:make-vm-const :dst :r1 :value 22)
                            (cl-cc/vm:make-vm-cons :dst :r2 :car-src :r0 :cdr-src :r1)
                            (cl-cc/vm:make-vm-cdr :dst :r3 :src :r2)
                            (cl-cc/vm:make-vm-halt :reg :r3))
                      :result-register :r3)))
      (expect (cl-cc/vm:run-compiled program) :to-be 22))))
