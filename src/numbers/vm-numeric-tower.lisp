(in-package :cl-cc/vm)

;;; VM instructions built on the native number tower: rounding, float
;;; introspection (precision/radix/sign/digits), scale-float, decode-float,
;;; and integer-decode-float. The tower's own types (vm-bignum, vm-ratio,
;;; vm-complex) live in vm-numeric-bignum.lisp, vm-numeric-rational.lisp,
;;; and vm-numeric-complex-native.lisp (all load before this file).

;;; FR-301: round (binary, returns quotient + remainder as multiple values)

(define-vm-instruction vm-round-inst (vm-instruction)
  "Round number to nearest integer (banker's rounding)."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :round)
  (:sexp-slots dst lhs rhs))

(defmethod execute-instruction ((inst vm-round-inst) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (q r)
      (round (vm-reg-get state (vm-lhs inst))
             (vm-reg-get state (vm-rhs inst)))
    (vm-reg-set state (vm-dst inst) q)
    (setf (vm-values-list state) (list q r)))
  (values (1+ pc) nil nil))


;;; FR-305: Float Operations
;; define-vm-unary-instruction / define-vm-binary-instruction defined in vm.lisp.

;;; FR-099: FMA (Fused Multiply-Add)
(progn
  (define-vm-instruction vm-fma (vm-instruction)
    "Fused multiply-add: dst = a * b + c.  Single rounding, no intermediate rounding.
 On x86-64: VFMADD231SD (FMA3) or VFMADD213SD (FMA4) in XMM registers."
    (dst nil :reader vm-dst)
    (a nil :reader vm-a)
    (b nil :reader vm-b)
    (c nil :reader vm-c)
    (precision :f64 :reader vm-float-precision :type (member :f32 :f64))
    (:sexp-tag :fma)
    (:sexp-slots dst a b c precision))
  (setf (gethash :fma *instruction-constructors*)
        (lambda (sexp)
          (make-vm-fma
           :dst (second sexp) :a (third sexp) :b (fourth sexp) :c (fifth sexp)
           :precision (if (cddddr (cdr sexp)) (sixth sexp) :f64)))))

(defmethod execute-instruction ((inst vm-fma) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (+ (* (vm-reg-get state (vm-a inst))
                    (vm-reg-get state (vm-b inst)))
                 (vm-reg-get state (vm-c inst))))
  (values (1+ pc) nil nil))

(define-vm-unary-instruction vm-float-inst       :float           "Convert number to float.")
(define-vm-unary-instruction vm-float-precision  :float-precision "Number of significant radix digits in a float.")
(define-vm-unary-instruction vm-float-radix      :float-radix     "Radix of the float representation.")
(define-vm-unary-instruction vm-float-sign       :float-sign      "Sign of a float as float.")
(define-vm-unary-instruction vm-float-digits     :float-digits    "Number of radix digits in the float mantissa.")
(define-vm-binary-instruction vm-scale-float     :scale-float     "Scale a float by a power of the radix.")

(define-simple-instruction vm-float-inst      :unary float)
(define-simple-instruction vm-float-precision :unary float-precision)
(define-simple-instruction vm-float-radix     :unary float-radix)
(define-simple-instruction vm-float-sign      :unary float-sign)
(define-simple-instruction vm-float-digits    :unary float-digits)
(define-simple-instruction vm-scale-float     :binary scale-float)

(define-vm-unary-instruction vm-decode-float :decode-float "Decode float into significand, exponent, sign (3 multiple values).")

(defmethod execute-instruction ((inst vm-decode-float) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (sig exp sign)
      (decode-float (vm-reg-get state (vm-src inst)))
    (vm-reg-set state (vm-dst inst) sig)
    (setf (vm-values-list state) (list sig exp sign)))
  (values (1+ pc) nil nil))

(define-vm-unary-instruction vm-integer-decode-float :integer-decode-float "Decode float into integer significand, exponent, sign (3 multiple values).")

(defmethod execute-instruction ((inst vm-integer-decode-float) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (sig exp sign)
      (integer-decode-float (vm-reg-get state (vm-src inst)))
    (vm-reg-set state (vm-dst inst) sig)
    (setf (vm-values-list state) (list sig exp sign)))
  (values (1+ pc) nil nil))

;;; Environment predicates (vm-boundp, vm-fboundp, etc.) are in vm-environment.lisp (loads after).
