;;;; t/vm-clos-execute-test.lisp — CLOS instruction execution
;;;;
;;;; Exercises VM-CLASS-DEF/VM-MAKE-OBJ/VM-SLOT-WRITE/VM-SLOT-READ end to
;;;; end through RUN-COMPILED, the same pattern the other
;;;; vm-*-execute-test.lisp files use for their respective instructions.

(in-package :cl-cc-vm/test)

(describe-sequential "CLOS instruction execution"
  (it "defines a class, creates an instance, and round-trips a slot"
    (let* ((program (cl-cc/vm:make-vm-program
                      :instructions
                      (list (cl-cc/vm:make-vm-class-def
                             :dst :r0 :class-name 'test-point :superclasses nil
                             :slot-names '(x) :slot-initargs nil :slot-initform-regs nil
                             :slot-types nil :default-initarg-regs nil :class-slots nil
                             :metaclass-reg nil :sealed nil)
                            (cl-cc/vm:make-vm-make-obj :dst :r1 :class-reg :r0
                                                        :initarg-regs nil)
                            (cl-cc/vm:make-vm-const :dst :r2 :value 42)
                            (cl-cc/vm:make-vm-slot-write :obj-reg :r1 :slot-name 'x
                                                          :value-reg :r2)
                            (cl-cc/vm:make-vm-slot-read :dst :r3 :obj-reg :r1
                                                         :slot-name 'x)
                            (cl-cc/vm:make-vm-halt :reg :r3))
                      :result-register :r3)))
      (expect (cl-cc/vm:run-compiled program) :to-be 42))))
