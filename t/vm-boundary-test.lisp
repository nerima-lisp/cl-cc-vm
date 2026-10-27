;;;; t/vm-boundary-test.lisp — module boundary tests for cl-cc-vm
;;;;
;;;; These assert the property that made the extraction possible, rather than
;;;; re-testing VM behaviour: cl-cc's own suite does that, against this system.

(in-package :cl-cc-vm/test)

(defclass bootstrap-helper-slot-holder ()
    ((value :initform nil
            :accessor bootstrap-helper-slot-holder-value)))
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
             (funcall (symbol-function (find-symbol "MAKE-VM-FLOAT-ADD" "CL-CC/VM")) :precision :invalid)
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

  (it "exports closure helpers used by downstream consumers"
    (dolist (name '("%VM-CLOSURE-OBJECT-P" "%VM-CALL-CLOSURE-SYNC"))
      (expect (nth-value 1 (find-symbol name :cl-cc/vm)) :to-be :external))))

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
    (let* ((heap (cl-cc/runtime:make-rt-heap :young-size 32 :old-size 32))
           (state (cl-cc/vm:make-vm-state))
           (object-address 32)
           (slot-offset 1)
           (object-pointer (cl-cc/runtime:encode-pointer object-address cl-cc/runtime:+tag-cons+))
           (old-value (cl-cc/runtime:encode-pointer 5 cl-cc/runtime:+tag-cons+))
           (new-value (cl-cc/runtime:encode-pointer 6 cl-cc/runtime:+tag-cons+)))
      (cl-cc/runtime:rt-heap-set-header
       heap object-address
       (cl-cc/runtime:header-set-mark
        (cl-cc/runtime:make-rt-header 3 cl-cc/runtime:+rt-tag-cons+ :gc-bits 0)))
      (cl-cc/runtime:rt-heap-set heap (+ object-address slot-offset) old-value)
      (setf (cl-cc/runtime:rt-heap-gc-state heap) :major-gc
            (gethash :managed-rt-heap (cl-cc/vm:vm-state-heap state)) heap)
      (cl-cc/vm/protocols:vm-reg-set state :pair object-pointer)
      (cl-cc/vm/protocols:vm-reg-set state :value new-value)
      (let ((cl-cc/runtime:*rt-use-barrier-batching* nil))
        (cl-cc/vm:execute-instruction
         (cl-cc/vm:make-vm-rplaca :cons :pair :val :value)
         state 0 (make-hash-table)))
      (expect (cl-cc/runtime:rt-heap-ref heap (+ object-address slot-offset)) :to-be new-value)
      (expect (cl-cc/runtime:rt-card-dirty-p heap object-address) :to-be-truthy)
      (expect (member old-value (cl-cc/runtime:rt-heap-satb-queue heap)) :to-be-truthy)))

  (it "services pending GC through run-program-slice"
    (let* ((heap (cl-cc/runtime:make-rt-heap :young-size 32 :old-size 32))
           (state (cl-cc/vm:make-vm-state))
           (instructions (vector (cl-cc/vm:make-vm-const :dst :r0 :value 42)
                                 (cl-cc/vm:make-vm-halt :reg :r0))))
      (let ((cl-cc/runtime:*gc-pending* nil))
        (cl-cc/runtime:rt-gc-request heap)
        (expect (cl-cc/vm:run-program-slice
                 instructions (cl-cc/vm/protocols:build-label-table instructions) 0 state
                 :gc-heap heap)
                :to-be 42)
        (expect (cl-cc/runtime:rt-heap-gc-pending heap) :to-be nil))))

  (it "services pending GC through run-compiled"
    (let* ((heap (cl-cc/runtime:make-rt-heap :young-size 32 :old-size 32))
           (program (cl-cc/vm:make-vm-program
                     :instructions (list (cl-cc/vm:make-vm-const :dst :r0 :value 42)
                                         (cl-cc/vm:make-vm-halt :reg :r0))
                     :result-register :r0)))
      (let ((cl-cc/runtime:*gc-pending* nil))
        (cl-cc/runtime:rt-gc-request heap)
        (expect (cl-cc/vm:run-compiled program :gc-heap heap) :to-be 42)
        (expect (cl-cc/runtime:rt-heap-gc-pending heap) :to-be nil))))

  (it "resolves configured guest symbols before CL and bridge fallback"
    (let* ((name "MAKE-INSTANCES-OBSOLETE")
           (function-package
             (or (find-package "CL-CC/VM-BOUNDARY-GUESTS")
                 (make-package "CL-CC/VM-BOUNDARY-GUESTS" :use nil)))
           (guest-symbol (intern name function-package))
           (cl-symbol (find-symbol name :cl))
           (parent-functions (make-hash-table :test #'eq))
           (parent-function (lambda () :parent))
           (local-function (lambda () :local))
           (parent-env (cl-cc/vm/execution:make-vm-parent-environment
                        :functions parent-functions))
           (state (cl-cc/vm:make-vm-instance :parent-env parent-env))
           (instruction (cl-cc/vm:make-vm-func-ref :dst :r0 :label name)))
      (let ((cl-cc/vm:*vm-guest-function-package* function-package))
        (expect guest-symbol :not :to-be nil)
        (expect cl-symbol :not :to-be nil)
        (setf (gethash cl-symbol parent-functions) parent-function)
        (cl-cc/vm:execute-instruction instruction state 0 (make-hash-table))
        (expect (cl-cc/vm/protocols:vm-reg-get state :r0) :to-be parent-function)
        (setf (gethash guest-symbol (cl-cc/vm:vm-function-registry state)) local-function)
        (cl-cc/vm:execute-instruction instruction state 0 (make-hash-table))
        (expect (cl-cc/vm/protocols:vm-reg-get state :r0) :to-be local-function)
        (remhash guest-symbol (cl-cc/vm:vm-function-registry state))
        (remhash cl-symbol parent-functions)
        (cl-cc/vm:execute-instruction instruction state 0 (make-hash-table))
        (expect (cl-cc/vm/protocols:vm-reg-get state :r0)
                :to-be (cl-cc/vm/protocols:vm-lookup-bridge-callable cl-symbol))))))

(describe-sequential "catch tag identity"
  (it "does not match distinct EQL numeric tags"
    (let* ((digits "10000000000000000000000000000000000000001")
           (outer-tag (parse-integer digits))
           (inner-tag (parse-integer digits))
           (program
             (cl-cc/vm:make-vm-program
              :instructions
              (list (cl-cc/vm:make-vm-const :dst :outer-tag :value outer-tag)
                    (cl-cc/vm:make-vm-establish-catch
                     :tag-reg :outer-tag :handler-label :outer-handler :result-reg :result)
                    (cl-cc/vm:make-vm-const :dst :inner-tag :value inner-tag)
                    (cl-cc/vm:make-vm-establish-catch
                     :tag-reg :inner-tag :handler-label :inner-handler :result-reg :result)
                    (cl-cc/vm:make-vm-const :dst :throw-tag :value outer-tag)
                    (cl-cc/vm:make-vm-const :dst :throw-value :value :outer)
                    (cl-cc/vm:make-vm-throw :tag-reg :throw-tag :value-reg :throw-value)
                    (cl-cc/vm:make-vm-label :name :inner-handler)
                    (cl-cc/vm:make-vm-const :dst :result :value :inner)
                    (cl-cc/vm:make-vm-halt :reg :result)
                    (cl-cc/vm:make-vm-label :name :outer-handler)
                    (cl-cc/vm:make-vm-halt :reg :result))
              :result-register :result)))
      (expect (eql outer-tag inner-tag) :to-be-truthy)
      (expect (eq outer-tag inner-tag) :to-be nil)
      (expect (cl-cc/vm:run-compiled program) :to-be :outer))))

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

(progn
  (describe-sequential "bootstrap runtime helpers"
    (it "adds a new indicator to an empty plist"
      (expect (cl-cc/bootstrap:rt-plist-put nil :a 1) :to-equal (list :a 1)))
    (it "replaces an existing indicator value without a duplicate"
      (expect (cl-cc/bootstrap:rt-plist-put (list :a 1 :b 2) :a 9)
              :to-equal (list :a 9 :b 2)))
    (it "leaves the original plist unmodified"
      (let ((plist (list :a 1)))
        (cl-cc/bootstrap:rt-plist-put plist :a 2)
        (expect plist :to-equal (list :a 1))))
    (it "keeps a key before the one being replaced"
      (expect (cl-cc/bootstrap:rt-plist-put (list :b 2 :a 1) :a 9)
              :to-equal (list :b 2 :a 9)))
    (it "appends a new indicator after unrelated entries"
      (expect (cl-cc/bootstrap:rt-plist-put (list :b 2) :a 9)
              :to-equal (list :b 2 :a 9)))
    (it-property "getf on the result always returns the value just put"
        ((indicator (gen-member (list :a :b :c)))
         (value (gen-integer :min -100 :max 100)))
      (expect (getf (cl-cc/bootstrap:rt-plist-put nil indicator value) indicator)
              :to-be value))
    (it "sets a CLOS slot and returns the value set"
      (let ((holder (make-instance (quote bootstrap-helper-slot-holder))))
        (expect (cl-cc/bootstrap:rt-slot-set
                 holder (find-symbol "VALUE" "CL-CC-VM/TEST") 42)
                :to-be 42)
        (expect (bootstrap-helper-slot-holder-value holder) :to-be 42)))
    (it "registers rt-plist-put as a VM host bridge"
      (let* ((symbol (find-symbol "RT-PLIST-PUT" "CL-CC/BOOTSTRAP"))
             (callable (cl-cc/vm/protocols:vm-lookup-bridge-callable symbol)))
        (expect (functionp callable) :to-be-truthy)
        (expect (funcall callable nil :a 1) :to-equal (list :a 1))))
    (it "registers rt-slot-set as a VM host bridge"
      (let* ((symbol (find-symbol "RT-SLOT-SET" "CL-CC/BOOTSTRAP"))
             (callable (cl-cc/vm/protocols:vm-lookup-bridge-callable symbol))
             (holder (make-instance (quote bootstrap-helper-slot-holder))))
        (expect (functionp callable) :to-be-truthy)
        (expect (funcall callable holder
                         (find-symbol "VALUE" "CL-CC-VM/TEST") 42)
                :to-be 42)))))

(describe-sequential "configurable guest function package"
  (it "defaults to CL-USER and is exported from the public packages"
    (expect cl-cc/vm:*vm-guest-function-package* :to-be :cl-user)
    (expect (nth-value 1 (find-symbol "*VM-GUEST-FUNCTION-PACKAGE*" :cl-cc/vm))
            :to-be :external)
    (expect (nth-value 1 (find-symbol "*VM-GUEST-FUNCTION-PACKAGE*" :cl-cc/vm/protocols))
            :to-be :external))

  (it "uses its package for bridge aliases"
    (let* ((guest-package
             (or (find-package "CL-CC/VM-BOUNDARY-ALIASES")
                 (make-package "CL-CC/VM-BOUNDARY-ALIASES" :use nil)))
           (source-package
             (or (find-package "CL-CC/VM-BOUNDARY-SOURCE")
                 (make-package "CL-CC/VM-BOUNDARY-SOURCE" :use nil)))
           (name "VM-BOUNDARY-ALIAS")
           (guest-symbol (intern name guest-package))
           (source-symbol (intern name source-package))
           (callable (lambda () :bridge)))
      (unwind-protect
           (let ((cl-cc/vm:*vm-guest-function-package* guest-package))
             (cl-cc/vm:vm-register-host-bridge guest-symbol callable)
             (expect (cl-cc/vm/protocols:vm-lookup-bridge-callable source-symbol) :to-be callable))
        (remhash guest-symbol cl-cc/vm/protocols:*vm-host-bridge-functions*))))

  (it "registers qualified guest functions in its package"
    (let* ((guest-package
             (or (find-package "CL-CC/VM-BOUNDARY-REGISTRY")
                 (make-package "CL-CC/VM-BOUNDARY-REGISTRY" :use nil)))
           (runtime-package (make-hash-table :test #'eq))
           (state (cl-cc/vm:make-vm-state))
           (name (make-symbol "HELLO-WORLD"))
           (closure (lambda () :guest)))
      (setf (gethash :name runtime-package) "VM-BOUNDARY-GUESTS"
            (gethash (find-symbol "*PACKAGE*" :cl) (cl-cc/vm:vm-global-vars state))
            runtime-package)
      (cl-cc/vm:vm-reg-set state :closure closure)
      (let ((cl-cc/vm:*vm-guest-function-package* guest-package))
        (cl-cc/vm:execute-instruction
         (cl-cc/vm:make-vm-register-function :name name :src :closure)
         state 0 (make-hash-table))
        (expect (gethash name (cl-cc/vm:vm-function-registry state)) :to-be closure)
        (cl-cc/vm:execute-instruction
         (cl-cc/vm:make-vm-func-ref :dst :r1 :label "VM-BOUNDARY-GUESTS::HELLO-WORLD")
         state 0 (make-hash-table))
        (expect (cl-cc/vm/protocols:vm-reg-get state :r1) :to-be closure))))

  (it "uses the configured package for immediate-symbol gating"
    (let* ((guest-package
             (or (find-package "GUEST-FUNCTION-BOUNDARY-GATE")
                 (make-package "GUEST-FUNCTION-BOUNDARY-GATE" :use nil)))
           (existing-umbrella-package (find-package :cl-cc))
           (umbrella-package-created-p (null existing-umbrella-package))
           (umbrella-package
             (or existing-umbrella-package
                 (make-package :cl-cc :use nil)))
           (implementation-package
             (or (find-package "CL-CC/VM-BOUNDARY-IMPL")
                 (make-package "CL-CC/VM-BOUNDARY-IMPL" :use nil))))
      (unwind-protect
           (progn
             (let ((cl-cc/vm:*vm-guest-function-package* guest-package)
                   (*package* guest-package))
               (expect (cl-cc/vm/execution::vm-immediate-intern-enabled-p)
                 :to-be nil))
             (let ((cl-cc/vm:*vm-guest-function-package* guest-package)
                   (*package* (find-package :cl-user)))
               (expect (cl-cc/vm/execution::vm-immediate-intern-enabled-p)
                 :to-be-truthy))
             (let ((cl-cc/vm:*vm-guest-function-package* :cl-cc)
                   (*package* umbrella-package))
               (expect (cl-cc/vm/execution::vm-immediate-intern-enabled-p)
                 :to-be nil))
             (let ((cl-cc/vm:*vm-guest-function-package* :cl-cc)
                   (*package* implementation-package))
               (expect (cl-cc/vm/execution::vm-immediate-intern-enabled-p)
                 :to-be nil)))
        (when umbrella-package-created-p
          (delete-package umbrella-package))))))
