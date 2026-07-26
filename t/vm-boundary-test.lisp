;;;; t/vm-boundary-test.lisp — module boundary tests for cl-cc-vm
;;;;
;;;; These assert the property that made the extraction possible, rather than
;;;; re-testing VM behaviour: cl-cc's own suite does that, against this system.

(in-package :cl-cc-vm/test)

(describe-sequential "cl-cc-vm dependency closure"
  (it "loads without any of the cl-cc compiler packages present"
    ;; The design document ruled this system out of scope as part of a
    ;; self-referential core. It is not: it depends on bootstrap and runtime.
    (dolist (name '("CL-CC/EXPAND" "CL-CC/COMPILE" "CL-CC/OPTIMIZE"
                    "CL-CC/PARSE" "CL-CC/AST" "CL-CC/TYPE"))
      (expect (find-package name) :to-be nil)))

  (it "has its two declared dependencies loaded"
    (expect (find-package "CL-CC/BOOTSTRAP") :to-be-truthy)
    (expect (find-package "CL-CC/RUNTIME") :to-be-truthy)))

(describe-sequential "cl-cc-vm public surface"
  (it "exports the instruction types an out-of-tree pass dispatches on"
    ;; §5-2 of the split design: an external repository cannot reach an
    ;; internal symbol, so anything a pass names has to be external here.
    (dolist (name '("VM-ADD" "VM-CALL" "VM-CONST" "VM-JUMP-ZERO" "VM-LABEL"))
      (expect (nth-value 1 (find-symbol name :cl-cc/vm)) :to-be :external)))

  (it "exports the slot accessors those passes read"
    (dolist (name '("VM-DST" "VM-LHS" "VM-RHS" "VM-SRC"
                    "VM-SLOT-READ-DST" "VM-GET-GLOBAL-NAME"))
      (expect (nth-value 1 (find-symbol name :cl-cc/vm)) :to-be :external)))

  (it "keeps its own helpers internal"
    (dolist (name '("%VM-CLOSURE-OBJECT-P" "%VM-CALL-CLOSURE-SYNC"))
      (let ((found (nth-value 1 (find-symbol name :cl-cc/vm))))
        (expect (eq found :external) :to-be nil)))))

(describe-sequential "cl-cc-vm executes"
  (it "runs a compiled program end to end"
    (let* ((program (cl-cc/vm:make-vm-program
                     :instructions (list (cl-cc/vm:make-vm-const :dst :r0 :value 41)
                                         (cl-cc/vm:make-vm-const :dst :r1 :value 1)
                                         (cl-cc/vm:make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                                         (cl-cc/vm:make-vm-halt :reg :r2))
                     :result-register :r2)))
      (expect (cl-cc/vm:run-compiled program) :to-be 42))))
