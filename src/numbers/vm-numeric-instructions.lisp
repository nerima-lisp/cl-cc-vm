(in-package #:cl-cc/vm/numbers)

(define-vm-instruction vm-simd-vector-op (vm-instruction)
  "Backend-neutral SIMD array-map marker.

Semantics: for LANES elements starting at INDEX-REG, compute
  DST-ARRAY[i+k] = (OP LHS-ARRAY[i+k] RHS-ARRAY[i+k])
for k in [0, LANES).  ELEMENT-TYPE describes the packed lane type.  Native
backends lower supported combinations to SIMD instructions; unsupported
lane/type/op combinations must signal a clear error."
  (op nil :reader vm-simd-vector-op-op)
  (dst-array nil :reader vm-simd-vector-op-dst-array)
  (lhs-array nil :reader vm-simd-vector-op-lhs-array)
  (rhs-array nil :reader vm-simd-vector-op-rhs-array)
  (index-reg nil :reader vm-simd-vector-op-index-reg)
  (lanes 4 :reader vm-simd-vector-op-lanes)
  (element-type :i32 :reader vm-simd-vector-op-element-type)
  (:sexp-tag :simd-vector-op)
  (:sexp-slots op dst-array lhs-array rhs-array index-reg lanes element-type))

(define-vm-instruction vm-binop (vm-instruction)
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs))

(define-vm-instruction vm-add (vm-binop)
  (:sexp-tag :add)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-sub (vm-binop)
  (:sexp-tag :sub)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-mul (vm-binop)
  (:sexp-tag :mul)
  (:sexp-slots dst lhs rhs))

;;; Specialized arithmetic instructions used by type-directed codegen.
(define-vm-instruction vm-integer-add (vm-add)
  (:sexp-tag :iadd)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-integer-sub (vm-sub)
  (:sexp-tag :isub)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-integer-mul (vm-mul)
  (:sexp-tag :imul)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-integer-mul-high-u (vm-mul)
  (:sexp-tag :umulh)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-integer-mul-high-s (vm-mul)
  (:sexp-tag :smulh)
  (:sexp-slots dst lhs rhs))

;;; Checked arithmetic instructions (FR-303 overflow detection).
;;; These behave identically to their unchecked counterparts in the VM
;;; interpreter (CL arithmetic auto-promotes to bignum), but signal the
;;; native codegen to emit hardware overflow detection + trap.
(define-vm-instruction vm-add-checked (vm-binop)
  (:sexp-tag :add-checked)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-sub-checked (vm-binop)
  (:sexp-tag :sub-checked)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-mul-checked (vm-binop)
  (:sexp-tag :mul-checked)
  (:sexp-slots dst lhs rhs))

(progn
  (define-vm-instruction vm-float-add (vm-add)
    (precision :f64 :reader vm-float-precision :type (member :f32 :f64))
    (:sexp-tag :fadd)
    (:sexp-slots dst lhs rhs precision))
  (setf (gethash :fadd *instruction-constructors*)
        (lambda (sexp)
          (make-vm-float-add
           :dst (second sexp) :lhs (third sexp) :rhs (fourth sexp)
           :precision (if (cddddr sexp) (fifth sexp) :f64)))))

(progn
  (define-vm-instruction vm-float-sub (vm-sub)
    (precision :f64 :reader vm-float-precision :type (member :f32 :f64))
    (:sexp-tag :fsub)
    (:sexp-slots dst lhs rhs precision))
  (setf (gethash :fsub *instruction-constructors*)
        (lambda (sexp)
          (make-vm-float-sub
           :dst (second sexp) :lhs (third sexp) :rhs (fourth sexp)
           :precision (if (cddddr sexp) (fifth sexp) :f64)))))

(progn
  (define-vm-instruction vm-float-mul (vm-mul)
    (precision :f64 :reader vm-float-precision :type (member :f32 :f64))
    (:sexp-tag :fmul)
    (:sexp-slots dst lhs rhs precision))
  (setf (gethash :fmul *instruction-constructors*)
        (lambda (sexp)
          (make-vm-float-mul
           :dst (second sexp) :lhs (third sexp) :rhs (fourth sexp)
           :precision (if (cddddr sexp) (fifth sexp) :f64)))))
