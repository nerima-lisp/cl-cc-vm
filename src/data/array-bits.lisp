(in-package #:cl-cc/vm/data)

;;; VM Bit Array, adjust-array, and displacement Instructions (FR-605, FR-606, FR-648)
;;;
;;; Depends on array.lisp (vm-arr/vm-idx/vm-val accessors, base array instructions).
;;; Load order: immediately after array.lisp.

;;; ─── FR-606: Bit array operations ────────────────────────────────────────────

(define-vm-instruction vm-bit-access (vm-instruction)
  "Access element of bit array: (bit array index)."
  (dst nil :reader vm-dst) (arr nil :reader vm-arr) (idx nil :reader vm-idx)
  (:sexp-tag :bit-access) (:sexp-slots dst arr idx))

(define-vm-instruction vm-bit-set (vm-instruction)
  "Set element of bit array: (setf (bit array index) val)."
  (dst nil :reader vm-dst) (arr nil :reader vm-arr) (idx nil :reader vm-idx)
  (val nil :reader vm-val)
  (:sexp-tag :bit-set) (:sexp-slots dst arr idx val))

(define-vm-instruction vm-bit-and (vm-instruction)
  "Element-wise AND of two bit arrays."
  (dst nil :reader vm-dst) (lhs nil :reader vm-lhs) (rhs nil :reader vm-rhs)
  (:sexp-tag :bit-and) (:sexp-slots dst lhs rhs))
(define-simple-instruction vm-bit-and :binary bit-and)

(define-vm-instruction vm-bit-or (vm-instruction)
  "Element-wise OR of two bit arrays."
  (dst nil :reader vm-dst) (lhs nil :reader vm-lhs) (rhs nil :reader vm-rhs)
  (:sexp-tag :bit-or) (:sexp-slots dst lhs rhs))
;; The VM instruction name stays short; source-level builtin lookup uses ANSI bit-ior.
(define-simple-instruction vm-bit-or :binary bit-ior)

(define-vm-instruction vm-bit-xor (vm-instruction)
  "Element-wise XOR of two bit arrays."
  (dst nil :reader vm-dst) (lhs nil :reader vm-lhs) (rhs nil :reader vm-rhs)
  (:sexp-tag :bit-xor) (:sexp-slots dst lhs rhs))
(define-simple-instruction vm-bit-xor :binary bit-xor)

(define-vm-instruction vm-bit-not (vm-instruction)
  "Element-wise NOT of a bit array."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :bit-not) (:sexp-slots dst src))
(define-simple-instruction vm-bit-not :unary bit-not)

(define-vm-instruction vm-sbit (vm-instruction)
  "Access element of simple bit-vector: (sbit svec index)."
  (dst nil :reader vm-dst) (arr nil :reader vm-arr) (idx nil :reader vm-idx)
  (:sexp-tag :sbit) (:sexp-slots dst arr idx))

;;; ─── FR-605: adjust-array and array-displacement ─────────────────────────────

(define-vm-instruction vm-adjust-array (vm-instruction)
  "Adjust dimensions of an adjustable array. Supports common ADJUST-ARRAY options."
  (dst nil :reader vm-dst) (arr nil :reader vm-arr) (dims nil :reader vm-dims)
  (initial-element nil :reader vm-initial-element)
  (fill-pointer nil :reader vm-fill-pointer)
  (fill-pointer-reg nil :reader vm-fill-pointer-reg)
  (element-type nil :reader vm-element-type)
  (element-type-reg nil :reader vm-element-type-reg)
  (displaced-to-reg nil :reader vm-displaced-to-reg)
  (displaced-index-offset-reg nil :reader vm-displaced-index-offset-reg)
  (:sexp-tag :adjust-array)
  (:sexp-slots dst arr dims initial-element fill-pointer fill-pointer-reg
                element-type element-type-reg displaced-to-reg
                displaced-index-offset-reg))

(define-vm-instruction vm-array-displacement (vm-instruction)
  "Return displaced-to array and offset (both nil for non-displaced arrays)."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :array-displacement) (:sexp-slots dst src))

;;; ─── simple-vector-p (FR-648) ────────────────────────────────────────────────
(define-vm-unary-instruction vm-simple-vector-p :simple-vector-p "Test if value is a simple-vector. Returns 1/0.")
(define-simple-instruction vm-simple-vector-p :pred1 simple-vector-p)
