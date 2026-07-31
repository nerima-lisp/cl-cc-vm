;;;; t/vm-string-execute-test.lisp — string instruction execution
;;;;
;;;; Exercises VM-CONCATENATE/VM-STRING-UPCASE/VM-STRING-LENGTH end to end
;;;; through RUN-COMPILED, the same pattern the other vm-*-execute-test.lisp
;;;; files use for their respective instruction families.

(in-package :cl-cc-vm/test)

(describe-sequential "string instruction execution"
  (it "concatenates two strings"
    (let* ((program (cl-cc/vm:make-vm-program
                      :instructions
                      (list (cl-cc/vm:make-vm-const :dst :r0 :value "foo")
                            (cl-cc/vm:make-vm-const :dst :r1 :value "bar")
                            (cl-cc/vm:make-vm-concatenate :dst :r2 :str1 :r0 :str2 :r1)
                            (cl-cc/vm:make-vm-halt :reg :r2))
                      :result-register :r2)))
      (expect (string= (cl-cc/vm:run-compiled program) "foobar") :to-be-truthy)))

  (it "uppercases a string"
    (let* ((program (cl-cc/vm:make-vm-program
                      :instructions
                      (list (cl-cc/vm:make-vm-const :dst :r0 :value "hello")
                            (cl-cc/vm:make-vm-string-upcase :dst :r1 :src :r0)
                            (cl-cc/vm:make-vm-halt :reg :r1))
                      :result-register :r1)))
      (expect (string= (cl-cc/vm:run-compiled program) "HELLO") :to-be-truthy)))

  (it "measures string length"
    (let* ((program (cl-cc/vm:make-vm-program
                      :instructions
                      (list (cl-cc/vm:make-vm-const :dst :r0 :value "hello")
                            (cl-cc/vm:make-vm-string-length :dst :r1 :src :r0)
                            (cl-cc/vm:make-vm-halt :reg :r1))
                      :result-register :r1)))
      (expect (cl-cc/vm:run-compiled program) :to-be 5))))
