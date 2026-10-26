;;;; tests/unit/vm/primitives-tests.lisp — VM Primitive Instruction Tests
;;;
;;; Tests for execute-instruction on type predicates, comparisons,
;;; arithmetic extensions, boolean ops, and vm-typep.
;;;
;;; Relies on make-test-vm / exec1 helpers from list-tests.lisp.

(in-package #:cl-cc/vm/data/test)

;;; Tests that call with-replaced-function mutate the global symbol-function
;;; binding and are therefore not safe to run concurrently with other workers.

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Helpers: construct and execute unary/binary VM instructions
;;; ═══════════════════════════════════════════════════════════════════════════

(defun %with-unary-vm-state (value thunk)
  "Run THUNK with a fresh VM state whose unary source register is preloaded."
  (let ((state (make-test-vm)))
    (cl-cc/vm:vm-reg-set state 1 value)
    (funcall thunk state)))

(defun %with-binary-vm-state (lhs rhs thunk)
  "Run THUNK with a fresh VM state whose binary operand registers are preloaded.
This centralizes the repeated :R1/:R2 setup used by binary instruction tests."
  (let ((state (make-test-vm)))
    (cl-cc/vm:vm-reg-set state 1 lhs)
    (cl-cc/vm:vm-reg-set state 2 rhs)
    (funcall thunk state)))

(defun %run-unary-inst-with (instruction-thunk src-val)
  "Run INSTRUCTION-THUNK against SRC-VAL and return the destination register.
INSTRUCTION-THUNK receives the source register index and must build the VM instruction."
  (%with-unary-vm-state
   src-val
   (lambda (state)
     (exec1 (funcall instruction-thunk 1) state)
     (cl-cc/vm:vm-reg-get state 0))))

(defun %run-unary-inst (ctor-fn src-val)
  "Run a unary VM instruction constructor against SRC-VAL and return DST."
  (%run-unary-inst-with
   (lambda (src)
     (funcall ctor-fn :dst 0 :src src))
   src-val))

(defun %run-binary-inst (ctor-fn lhs rhs)
  "Run a binary VM instruction constructor against LHS/RHS and return DST."
  (%with-binary-vm-state
   lhs rhs
   (lambda (state)
     (exec1 (funcall ctor-fn :dst 0 :lhs 1 :rhs 2) state)
     (cl-cc/vm:vm-reg-get state 0))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 1: Type Predicates (pred1 pattern — return 1/0)
;;; ═══════════════════════════════════════════════════════════════════════════

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 2: EQL comparison (pred2 — return 1/0)
;;; ═══════════════════════════════════════════════════════════════════════════

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 3: Numeric Comparisons (pred2 — return 1/0)
;;; ═══════════════════════════════════════════════════════════════════════════

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 4: Unary Arithmetic
;;; ═══════════════════════════════════════════════════════════════════════════

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 5: Binary Arithmetic
;;; ═══════════════════════════════════════════════════════════════════════════

;;; Division by zero errors

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 6: Multiple-Value Division (truncate/floor/ceiling/round)
;;; ═══════════════════════════════════════════════════════════════════════════

;;; ═══════════════════════════════════════════════════════════════════════════
;;; FR-885: Float Rounding (ffloor/fceiling/ftruncate/fround)
;;; ═══════════════════════════════════════════════════════════════════════════

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 7: Boolean Operations
;;; ═══════════════════════════════════════════════════════════════════════════

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 8: Dynamic / TypeRep Runtime API
;;; ═══════════════════════════════════════════════════════════════════════════

(defun register-primitives-tests ()
(it-sequential "vm-type-rep-equality-and-inference"
  (let ((integer-rep (cl-cc/vm:vm-type-rep 'integer))
        (string-rep (cl-cc/vm:vm-type-rep 'string)))
    (expect (cl-cc/vm:vm-type-rep-equal integer-rep (cl-cc/vm:make-vm-type-rep 'integer)) :to-be-truthy)
    (expect (cl-cc/vm:vm-type-rep-equal integer-rep string-rep) :to-be-falsy)
    (expect (cl-cc/vm:vm-type-rep-specifier (cl-cc/vm:vm-type-rep-of 42)) :to-equal 'fixnum)
    (expect (cl-cc/vm:vm-type-rep-specifier (cl-cc/vm:vm-type-rep-of "hello")) :to-equal 'string)))
(it-sequential "vm-wrap-and-unwrap-dynamic-success"
  (let ((dynamic-value (cl-cc/vm:vm-wrap-dynamic 42 'integer)))
    (expect (cl-cc/vm:vm-dynamic-p dynamic-value) :to-be-truthy)
    (multiple-value-bind (value ok)
        (cl-cc/vm:vm-unwrap-dynamic dynamic-value 'integer)
      (expect ok :to-be-truthy)
      (expect (= 42 value) :to-be-truthy)))
  (let ((dynamic-value (cl-cc/vm:vm-wrap-dynamic 42)))
    (multiple-value-bind (value ok)
        (cl-cc/vm:vm-unwrap-dynamic dynamic-value 'integer)
      (expect ok :to-be-truthy)
      (expect (= 42 value) :to-be-truthy))))
(it-sequential "vm-unwrap-dynamic-mismatch-fails-safely"
  (multiple-value-bind (value ok)
      (cl-cc/vm:vm-unwrap-dynamic (cl-cc/vm:vm-wrap-dynamic 42 'integer) 'string)
    (expect value :to-be-null)
    (expect ok :to-be-falsy)))
(it-sequential "vm-cast-with-type-rep-works-for-plain-and-dynamic-values"
  (multiple-value-bind (value ok)
      (cl-cc/vm:vm-cast-with-type-rep 42 'integer)
    (expect ok :to-be-truthy)
    (expect (= 42 value) :to-be-truthy))
  (multiple-value-bind (value ok)
      (cl-cc/vm:vm-cast-with-type-rep 42 'fixnum)
    (expect ok :to-be-truthy)
    (expect (= 42 value) :to-be-truthy))
  (multiple-value-bind (value ok)
      (cl-cc/vm:vm-cast-with-type-rep (cl-cc/vm:vm-wrap-dynamic "hello" 'string) 'string)
    (expect ok :to-be-truthy)
    (expect value :to-equal "hello"))
  (multiple-value-bind (value ok)
      (cl-cc/vm:vm-cast-with-type-rep 42 'string)
    (expect value :to-be-null)
    (expect ok :to-be-falsy)))
)

