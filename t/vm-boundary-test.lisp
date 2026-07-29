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

(progn
  (describe-sequential "cl-cc-vm package locks"
    (it "constructs the SBCL package lock condition with complete initargs"
      (let ((package (make-package (symbol-name (gensym "LOCK-TEST-")) :use nil)))
        (unwind-protect
             (progn
               (cl-cc/vm::vm-lock-package package)
               (let ((condition
                       (handler-case
                           (progn
                             (cl-cc/vm::check-package-lock package)
                             nil)
                         (sb-ext:package-locked-error (condition) condition))))
                 (expect condition :to-be-truthy)))
          (cl-cc/vm::vm-unlock-package package)
          (delete-package package)))))

  (describe-sequential "cl-cc-vm FASL serialization"
    (it "round-trips programs and load-time-value cells through explicit wire data"
      (let* ((unresolved
               (cl-cc/vm:make-vm-load-time-value-cell
                :id 7 :form (quote (+ 20 22)) :read-only-p t))
             (resolved-value (list :resolved 42))
             (resolved
               (cl-cc/vm:make-vm-load-time-value-cell
                :id 8 :resolved-p t :value resolved-value))
             (program
               (cl-cc/vm:make-vm-program
                :instructions
                (list (cl-cc/vm:make-vm-load-time-value :dst :r0 :cell-id 7)
                      (cl-cc/vm:make-vm-halt :reg :r0))
                :result-register :r0
                :leaf-p t
                :calling-convention :internal
                :function-conventions (quote ((helper . :internal)))
                :deopt-info (quote ((0 . checkpoint)))
                :osr-entry-points (quote ((loop . 1)))
                :tier1-entry-points (quote ((entry . 0)))
                :load-time-value-cells (list unresolved resolved)
                :compilation-tier 1))
             (wire
               (with-output-to-string (stream)
                 (cl-cc/vm:vm-write-to-fasl program stream)))
             (copy
               (with-input-from-string (stream wire)
                 (cl-cc/vm:vm-read-from-fasl stream)))
             (instructions (cl-cc/vm:vm-program-instructions copy))
             (cells (cl-cc/vm::vm-program-load-time-value-cells copy))
             (unresolved-copy (first cells))
             (resolved-copy (second cells)))
        (expect (search "(:VM-FASL-PROGRAM" wire) :to-be-truthy)
        (expect (search "#S(VM-PROGRAM" wire) :to-be nil)
        (expect (typep copy (quote cl-cc/vm:vm-program)) :to-be-truthy)
        (expect (equal (mapcar (function cl-cc/vm:instruction->sexp) instructions) (quote ((:load-time-value :r0 7) (:halt :r0)))) :to-be-truthy)
        (expect (cl-cc/vm:vm-load-time-value-cell-id unresolved-copy) :to-be 7)
        (expect (equal (cl-cc/vm:vm-load-time-value-cell-form unresolved-copy) (quote (+ 20 22))) :to-be-truthy)
        (expect (cl-cc/vm:vm-load-time-value-cell-read-only-p unresolved-copy)
                :to-be-truthy)
        (expect (cl-cc/vm:vm-load-time-value-cell-resolved-p unresolved-copy)
                :to-be nil)
        (expect (cl-cc/vm:vm-load-time-value-cell-id resolved-copy) :to-be 8)
        (expect (cl-cc/vm:vm-load-time-value-cell-resolved-p resolved-copy)
                :to-be-truthy)
        (expect (equal (cl-cc/vm:vm-load-time-value-cell-value resolved-copy) resolved-value) :to-be-truthy)
        (expect (cl-cc/vm:vm-program-result-register copy) :to-be :r0)
        (expect (cl-cc/vm:vm-program-leaf-p copy) :to-be-truthy)
        (expect (cl-cc/vm:vm-program-calling-convention copy) :to-be :internal)
        (expect (equal (cl-cc/vm:vm-program-function-conventions copy) (quote ((helper . :internal)))) :to-be-truthy)
        (expect (equal (cl-cc/vm:vm-program-deopt-info copy) (quote ((0 . checkpoint)))) :to-be-truthy)
        (expect (equal (cl-cc/vm:vm-program-osr-entry-points copy) (quote ((loop . 1)))) :to-be-truthy)
        (expect (equal (cl-cc/vm:vm-program-tier1-entry-points copy) (quote ((entry . 0)))) :to-be-truthy)
        (expect (cl-cc/vm:vm-program-compilation-tier copy) :to-be 1)))))

(describe-sequential "FR-433 Posit arithmetic"
  (it "encodes and exactly decodes canonical posit8 values"
    (dolist (sample '((0 0) (1 64) (2 96) (1/2 32) (-1 192)))
      (destructuring-bind (value bits) sample
        (let ((posit (cl-cc/vm:vm-posit-encode value :nbits 8 :es 0)))
          (expect (cl-cc/vm:vm-posit-bits posit) :to-be bits)
          (expect (cl-cc/vm:vm-posit-decode posit) :to-be value)))))

  (it "reserves one representation for NaR and propagates it"
    (let* ((one (cl-cc/vm:vm-posit-encode 1 :nbits 8 :es 0))
           (zero (cl-cc/vm:vm-posit-encode 0 :nbits 8 :es 0))
           (nar (cl-cc/vm:vm-posit-div one zero)))
      (expect (cl-cc/vm:vm-posit-bits nar) :to-be 128)
      (expect (cl-cc/vm:vm-posit-nar-p nar) :to-be-truthy)
      (expect (cl-cc/vm:vm-posit-nar-p (cl-cc/vm:vm-posit-add nar one))
              :to-be-truthy)))

  (it "rounds halfway cases to the representation with an even payload"
    (expect (cl-cc/vm:vm-posit-bits
             (cl-cc/vm:vm-posit-encode 65/64 :nbits 8 :es 0))
            :to-be 64)
    (expect (cl-cc/vm:vm-posit-bits
             (cl-cc/vm:vm-posit-encode 67/64 :nbits 8 :es 0))
            :to-be 66))

  (it "provides more precision near one than near maxpos"
    (let ((near-one (- (cl-cc/vm:vm-posit-decode
                        (cl-cc/vm:vm-posit-from-bits 65 :nbits 8 :es 0))
                       (cl-cc/vm:vm-posit-decode
                        (cl-cc/vm:vm-posit-from-bits 64 :nbits 8 :es 0))))
          (near-max (- (cl-cc/vm:vm-posit-decode
                        (cl-cc/vm:vm-posit-from-bits 127 :nbits 8 :es 0))
                       (cl-cc/vm:vm-posit-decode
                        (cl-cc/vm:vm-posit-from-bits 126 :nbits 8 :es 0)))))
      (expect (< near-one near-max) :to-be-truthy)))

  (it "implements rounded arithmetic in a shared format"
    (let ((three (cl-cc/vm:vm-posit-encode 3 :nbits 8 :es 0))
          (two (cl-cc/vm:vm-posit-encode 2 :nbits 8 :es 0)))
      (expect (cl-cc/vm:vm-posit-decode (cl-cc/vm:vm-posit-add three two))
              :to-be 5)
      (expect (cl-cc/vm:vm-posit-decode (cl-cc/vm:vm-posit-sub three two))
              :to-be 1)
      (expect (cl-cc/vm:vm-posit-decode (cl-cc/vm:vm-posit-mul three two))
              :to-be 6)
      (expect (cl-cc/vm:vm-posit-decode (cl-cc/vm:vm-posit-div three two))
              :to-be 3/2)))

  (it "accumulates products exactly and rounds only at Quire conversion"
    (let* ((quire (cl-cc/vm:make-vm-quire :nbits 8 :es 0))
           (half (cl-cc/vm:vm-posit-encode 1/2 :nbits 8 :es 0))
           (two (cl-cc/vm:vm-posit-encode 2 :nbits 8 :es 0))
           (three (cl-cc/vm:vm-posit-encode 3 :nbits 8 :es 0))
           (one (cl-cc/vm:vm-posit-encode 1 :nbits 8 :es 0)))
      (cl-cc/vm:vm-quire-add-product! quire half two)
      (cl-cc/vm:vm-quire-add-product! quire three one)
      (expect (cl-cc/vm:vm-quire-value quire) :to-be 4)
      (expect (cl-cc/vm:vm-posit-decode (cl-cc/vm:vm-quire-to-posit quire))
              :to-be 4))))
