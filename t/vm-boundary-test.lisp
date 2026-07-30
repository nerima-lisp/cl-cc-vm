;;;; t/vm-boundary-test.lisp — module boundary tests for cl-cc-vm
;;;;
;;;; These assert the property that made the extraction possible, rather than
;;;; re-testing VM behaviour: cl-cc's own suite does that, against this system.

(in-package :cl-cc-vm/test)

  (describe-sequential "floating-point instruction precision"
    (it "defaults constructors to f64"
      (dolist (instruction
               (list (cl-cc/vm:make-vm-float-add :dst :r0 :lhs :r1 :rhs :r2)
                     (cl-cc/vm:make-vm-float-sub :dst :r0 :lhs :r1 :rhs :r2)
                     (cl-cc/vm:make-vm-float-mul :dst :r0 :lhs :r1 :rhs :r2)
                     (cl-cc/vm:make-vm-float-div :dst :r0 :lhs :r1 :rhs :r2)
                     (cl-cc/vm:make-vm-fma :dst :r0 :a :r1 :b :r2 :c :r3)))
        (expect (cl-cc/vm:vm-float-precision instruction) :to-be :f64)))

    (it "round-trips explicit precision"
      (dolist (instruction
               (list (cl-cc/vm:make-vm-float-add :dst :r0 :lhs :r1 :rhs :r2 :precision :f32)
                     (cl-cc/vm:make-vm-float-sub :dst :r0 :lhs :r1 :rhs :r2 :precision :f32)
                     (cl-cc/vm:make-vm-float-mul :dst :r0 :lhs :r1 :rhs :r2 :precision :f32)
                     (cl-cc/vm:make-vm-float-div :dst :r0 :lhs :r1 :rhs :r2 :precision :f32)
                     (cl-cc/vm:make-vm-fma :dst :r0 :a :r1 :b :r2 :c :r3 :precision :f32)))
        (let* ((sexp (cl-cc/vm:instruction->sexp instruction))
               (copy (cl-cc/vm:sexp->instruction sexp)))
          (expect (equal (car (last sexp)) :f32) :to-be-truthy)
          (expect (cl-cc/vm:vm-float-precision copy) :to-be :f32)
          (expect (equal (cl-cc/vm:instruction->sexp copy) sexp) :to-be-truthy))))

    (it "reads legacy sexps as f64"
      (dolist (sexp '((:fadd :r0 :r1 :r2)
                      (:fsub :r0 :r1 :r2)
                      (:fmul :r0 :r1 :r2)
                      (:fdiv :r0 :r1 :r2)
                      (:fma :r0 :r1 :r2 :r3)))
        (expect (cl-cc/vm:vm-float-precision
                 (cl-cc/vm:sexp->instruction sexp))
                :to-be :f64)))

    (it "rejects invalid precision"
      (expect
       (handler-case
           (progn
             (cl-cc/vm:make-vm-float-add :precision :invalid)
             nil)
         (type-error () t))
       :to-be-truthy)))

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
      (expect (cl-cc/vm:run-compiled program) :to-be 42)))

  (it "routes managed cons mutation through the runtime write barrier"
    (let* ((heap (cl-cc/runtime::make-rt-heap :young-size 32 :old-size 32))
           (state (cl-cc/vm:make-vm-state))
           (object-address 32)
           (slot-offset 1)
           (object-pointer (cl-cc/runtime:encode-pointer object-address cl-cc/runtime:+tag-cons+))
           (old-value (cl-cc/runtime:encode-pointer 5 cl-cc/runtime:+tag-cons+))
           (new-value (cl-cc/runtime:encode-pointer 6 cl-cc/runtime:+tag-cons+)))
      (cl-cc/runtime::rt-heap-set-header
       heap object-address
       (cl-cc/runtime:header-set-mark
        (cl-cc/runtime:make-rt-header 3 cl-cc/runtime:+rt-tag-cons+ :gc-bits 0)))
      (cl-cc/runtime::rt-heap-set heap (+ object-address slot-offset) old-value)
      (setf (cl-cc/runtime::rt-heap-gc-state heap) :major-gc
            (gethash :managed-rt-heap (cl-cc/vm:vm-state-heap state)) heap)
      (cl-cc/vm::vm-reg-set state :pair object-pointer)
      (cl-cc/vm::vm-reg-set state :value new-value)
      (let ((cl-cc/runtime::*rt-use-barrier-batching* nil))
        (cl-cc/vm:execute-instruction
         (cl-cc/vm:make-vm-rplaca :cons :pair :val :value)
         state 0 (make-hash-table)))
      (expect (cl-cc/runtime::rt-heap-ref heap (+ object-address slot-offset)) :to-be new-value)
      (expect (cl-cc/runtime::rt-card-dirty-p heap object-address) :to-be-truthy)
      (expect (member old-value (cl-cc/runtime:rt-heap-satb-queue heap)) :to-be-truthy)))

  (it "services pending GC through run-program-slice"
    (let* ((heap (cl-cc/runtime:make-rt-heap :young-size 32 :old-size 32))
           (state (cl-cc/vm:make-vm-state))
           (instructions (vector (cl-cc/vm:make-vm-const :dst :r0 :value 42)
                                 (cl-cc/vm:make-vm-halt :reg :r0))))
      (let ((cl-cc/runtime::*gc-pending* nil))
        (cl-cc/runtime:rt-gc-request heap)
        (expect (cl-cc/vm:run-program-slice
                 instructions (cl-cc/vm::build-label-table instructions) 0 state
                 :gc-heap heap)
                :to-be 42)
        (expect (cl-cc/runtime:rt-heap-gc-pending heap) :to-be nil))))

  (it "services pending GC through run-compiled"
    (let* ((heap (cl-cc/runtime:make-rt-heap :young-size 32 :old-size 32))
           (program (cl-cc/vm:make-vm-program
                     :instructions (list (cl-cc/vm:make-vm-const :dst :r0 :value 42)
                                         (cl-cc/vm:make-vm-halt :reg :r0))
                     :result-register :r0)))
      (let ((cl-cc/runtime::*gc-pending* nil))
        (cl-cc/runtime:rt-gc-request heap)
        (expect (cl-cc/vm:run-compiled program :gc-heap heap) :to-be 42)
        (expect (cl-cc/runtime:rt-heap-gc-pending heap) :to-be nil))))

  (it "resolves function references through CL-CC, CL parent, then bridge lookup"
    (let* ((name "MAKE-INSTANCES-OBSOLETE")
           (cl-cc-symbol (find-symbol name :cl-cc))
           (cl-symbol (find-symbol name :cl))
           (parent-functions (make-hash-table :test #'eq))
           (parent-function (lambda () :parent))
           (local-function (lambda () :local))
           (parent-env (cl-cc/vm::make-vm-parent-environment
                        :functions parent-functions))
           (state (cl-cc/vm:make-vm-instance :parent-env parent-env))
           (instruction (cl-cc/vm:make-vm-func-ref :dst :r0 :label name)))
      (expect cl-cc-symbol :not :to-be nil)
      (expect cl-symbol :not :to-be nil)
      (expect (eq cl-cc-symbol cl-symbol) :to-be nil)
      (setf (gethash cl-symbol parent-functions) parent-function)
      (cl-cc/vm:execute-instruction instruction state 0 (make-hash-table))
      (expect (cl-cc/vm::vm-reg-get state :r0) :to-be parent-function)
      (setf (gethash cl-cc-symbol (cl-cc/vm:vm-function-registry state)) local-function)
      (cl-cc/vm:execute-instruction instruction state 0 (make-hash-table))
      (expect (cl-cc/vm::vm-reg-get state :r0) :to-be local-function)
      (remhash cl-cc-symbol (cl-cc/vm:vm-function-registry state))
      (remhash cl-symbol parent-functions)
      (cl-cc/vm:execute-instruction instruction state 0 (make-hash-table))
      (expect (cl-cc/vm::vm-reg-get state :r0)
              :to-be (cl-cc/vm:vm-bridge-callable cl-symbol)))))

(describe-sequential "package-independent generic function names"
  (it "matches symbols and SETF names without matching malformed names"
    (let ((left (make-symbol "SLOT-VALUE-USING-CLASS"))
          (right (make-symbol "SLOT-VALUE-USING-CLASS")))
      (expect (cl-cc/vm::%vm-same-function-name-p left right) :to-be-truthy)
      (expect (cl-cc/vm::%vm-same-function-name-p (list 'setf left) (list 'setf right))
              :to-be-truthy)
      (expect (cl-cc/vm::%vm-same-function-name-p (list 'setf left) (list 'other right))
              :to-be nil)
      (expect (cl-cc/vm::%vm-same-function-name-p (list 'setf left 'extra) (list 'setf right))
              :to-be nil)
      (expect (cl-cc/vm::%vm-same-function-name-p (cons 'setf left) (list 'setf right))
              :to-be nil)))

  (it "finds a SETF generic function registered with another package's symbol"
    (let* ((state (cl-cc/vm:make-vm-state))
           (registered-name (list 'setf (make-symbol "SLOT-VALUE-USING-CLASS")))
           (lookup-name (list 'setf (make-symbol "SLOT-VALUE-USING-CLASS")))
           (generic-function (make-hash-table :test #'eq)))
      (setf (gethash :__methods__ generic-function) (make-hash-table :test #'equal)
            (gethash registered-name (cl-cc/vm:vm-global-vars state)) generic-function)
      (expect (cl-cc/vm::%vm-global-generic-function state lookup-name)
              :to-be generic-function))))

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

(describe-sequential "continuation physical stack ownership"
  (it "restores an independently owned chain on every invocation"
    (let* ((state (cl-cc/vm:make-vm-state))
           (captured-root (cl-cc/runtime::stack-segment-note-frame nil 128))
           (captured-current (cl-cc/runtime::stack-segment-note-frame captured-root 9000)))
      (setf (cl-cc/vm:vm-current-stack-segment state) captured-current)
      (let ((continuation (cl-cc/vm:vm-capture-continuation state 17 :r0)))
        (cl-cc/vm:vm-invoke-continuation state continuation :first)
        (let ((first (cl-cc/vm:vm-current-stack-segment state)))
          (expect (not (eq first captured-current)) :to-be-truthy)
          (expect (cl-cc/runtime::stack-segment-used captured-current) :to-be 0)
          (cl-cc/vm:vm-invoke-continuation state continuation :second)
          (let ((second (cl-cc/vm:vm-current-stack-segment state)))
            (expect (not (eq second first)) :to-be-truthy)
            (expect (cl-cc/runtime::stack-segment-used first) :to-be 0)
            (expect (cl-cc/runtime::stack-segment-prev first) :to-be-null)
            (expect (cl-cc/runtime::stack-segment-used second) :to-be 9000)
            (expect (cl-cc/runtime::stack-segment-used (cl-cc/runtime::stack-segment-prev second)) :to-be 128)
            (cl-cc/runtime::release-stack-segment-chain second)
            (setf (cl-cc/vm:vm-current-stack-segment state) nil)))))))
(describe-sequential
    "allocate-instance direct method detection"
  (it
      "accepts only the primary allocation protocol shape owned by the generic function"
    (let* ((gf (make-hash-table :test (function eq)))
           (methods (make-hash-table :test (function equal)))
           (method (make-hash-table :test (function eq)))
           (metaclass (quote allocation-metaclass)))
      (setf (gethash :__methods__ gf) methods
            (gethash :function method) (function identity)
            (gethash :qualifiers method) nil
            (gethash :specializer method) (list metaclass t t)
            (gethash :gf method) gf
            (gethash (list metaclass t t) methods) method)
      (expect (cl-cc/vm::%vm-direct-primary-method-p gf metaclass) :to-be-truthy)
      (remhash (list metaclass t t) methods)
      (setf (gethash (list metaclass (quote non-top) t) methods) method)
      (expect (cl-cc/vm::%vm-direct-primary-method-p gf metaclass) :to-be nil)
      (clrhash methods)
      (setf (gethash :qualifiers method) (list :before)
            (gethash (list metaclass t) methods) method)
      (expect (cl-cc/vm::%vm-direct-primary-method-p gf metaclass) :to-be nil)
      (setf (gethash :qualifiers method) nil
            (gethash :gf method) (make-hash-table :test (function eq)))
      (expect (cl-cc/vm::%vm-direct-primary-method-p gf metaclass) :to-be nil)))
  (it
      "orders scalar rest fallbacks without leaking into fixed-arity dispatch"
    (let ((gf (make-hash-table :test (function equal)))
          (methods (make-hash-table :test (function equal)))
          (before (make-hash-table :test (function equal)))
          (after (make-hash-table :test (function equal)))
          (state (cl-cc/vm:make-vm-state))
          (args (list 42 :x :value)))
      (setf (gethash :__lambda-list__ gf) (list (quote object) (quote &rest) (quote initargs))
            (gethash :__methods__ gf) methods
            (gethash :__before__ gf) before
            (gethash :__after__ gf) after
            (gethash (list (quote integer) t t) methods) (quote specific)
            (gethash t methods) (quote fallback)
            (gethash (list (quote integer) t t) before) (quote before-specific)
            (gethash t before) (quote before-fallback)
            (gethash (list (quote integer) t t) after) (quote after-specific)
            (gethash t after) (quote after-fallback))
      (expect (cl-cc/vm:vm-get-all-applicable-methods gf state args)
              :to-equal (list (quote specific) (quote fallback)))
      (expect (cl-cc/vm::%lookup-qualified-methods gf :__before__ state args)
              :to-equal (list (quote before-specific) (quote before-fallback)))
      (expect (cl-cc/vm::%lookup-qualified-methods gf :__after__ state args)
              :to-equal (list (quote after-specific) (quote after-fallback)))
      (setf (gethash t methods) (quote specific))
      (expect (cl-cc/vm:vm-get-all-applicable-methods gf state args)
              :to-equal (list (quote specific)))
      (setf (gethash :__lambda-list__ gf)
            (list (quote first) (quote second) (quote third))
            (gethash t methods) (quote fallback))
      (expect (cl-cc/vm:vm-get-all-applicable-methods gf state args)
              :to-equal (list (quote specific))))))
