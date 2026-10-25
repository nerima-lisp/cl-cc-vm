(in-package :cl-cc/vm)

;;; Native VM rational value: the vm-ratio struct, additive/multiplicative
;;; ops, VM-RATIONAL/VM-RATIONALIZE/VM-FLOOR, and print-object. Depends on
;;; vm-numeric-bignum.lisp (loads before) for vm-bignum-gcd.

(defstruct (vm-ratio
            (:constructor %make-vm-ratio (numerator denominator))
            (:copier nil))
  "Native VM rational represented by normalized numerator/denominator integers."
  numerator
  denominator)

(defun %vm-number->integer (value)
  (if (vm-bignum-p value) (vm-bignum-to-integer value) value))

(defun %vm-externalize-ratio (ratio)
  (/ (vm-ratio-numerator ratio) (vm-ratio-denominator ratio)))

(defun vm-make-ratio (numerator denominator)
  "Construct a normalized VM ratio."
  (let ((n (%vm-number->integer numerator))
        (d (%vm-number->integer denominator)))
    (when (zerop d)
      (error "Ratio denominator is zero"))
    (when (minusp d)
      (setf n (- n) d (- d)))
    (let ((g (vm-bignum-to-integer (vm-bignum-gcd n d))))
      (%make-vm-ratio (/ n g) (/ d g)))))

(defun %vm-coerce-ratio (value)
  (cond
    ((vm-ratio-p value) value)
    ((integerp value) (vm-make-ratio value 1))
    ((rationalp value) (vm-make-ratio (numerator value) (denominator value)))
    (t (error "Not a rational value: ~S" value))))

(defun vm-rational-add (lhs rhs)
  (let ((a (%vm-coerce-ratio lhs))
        (b (%vm-coerce-ratio rhs)))
    (vm-make-ratio (+ (* (vm-ratio-numerator a) (vm-ratio-denominator b))
                      (* (vm-ratio-numerator b) (vm-ratio-denominator a)))
                   (* (vm-ratio-denominator a) (vm-ratio-denominator b)))))

(defun vm-rational-sub (lhs rhs)
  (let ((a (%vm-coerce-ratio lhs))
        (b (%vm-coerce-ratio rhs)))
    (vm-make-ratio (- (* (vm-ratio-numerator a) (vm-ratio-denominator b))
                      (* (vm-ratio-numerator b) (vm-ratio-denominator a)))
                   (* (vm-ratio-denominator a) (vm-ratio-denominator b)))))

(defun vm-rational-mul (lhs rhs)
  (let ((a (%vm-coerce-ratio lhs))
        (b (%vm-coerce-ratio rhs)))
    (vm-make-ratio (* (vm-ratio-numerator a) (vm-ratio-numerator b))
                   (* (vm-ratio-denominator a) (vm-ratio-denominator b)))))

(defun vm-rational-div (lhs rhs)
  (let ((a (%vm-coerce-ratio lhs))
        (b (%vm-coerce-ratio rhs)))
    (when (zerop (vm-ratio-numerator b))
      (error "Division by zero"))
    (vm-make-ratio (* (vm-ratio-numerator a) (vm-ratio-denominator b))
                   (* (vm-ratio-denominator a) (vm-ratio-numerator b)))))

(defun vm-rational (number)
  "Convert NUMBER to an exact native VM rational."
  (cond
    ((vm-ratio-p number) number)
    ((integerp number) (vm-make-ratio number 1))
    ((rationalp number) (vm-make-ratio (numerator number) (denominator number)))
    ((floatp number)
     (multiple-value-bind (mantissa exponent sign) (integer-decode-float number)
       (if (minusp exponent)
           (vm-make-ratio (* sign mantissa) (ash 1 (- exponent)))
           (vm-make-ratio (* sign mantissa (ash 1 exponent)) 1))))
    (t (error "Cannot convert to rational: ~S" number))))

(defun vm-rationalize (number &optional (tolerance 1.0d-12))
  "Return a human-friendly rational approximation as a VM ratio."
  (cond
    ((or (integerp number) (rationalp number) (vm-ratio-p number)) (vm-rational number))
    ((floatp number)
     (let* ((sign (if (minusp number) -1 1))
            (x (abs (float number 1.0d0)))
            (h1 1) (h0 0)
            (k1 0) (k0 1)
            (b x))
       (loop
         for a = (floor b)
         for h = (+ (* a h1) h0)
         for k = (+ (* a k1) k0)
         do (when (or (zerop (- b a))
                      (< (abs (- x (/ h k))) tolerance))
              (return (vm-make-ratio (* sign h) k)))
            (setf h0 h1 h1 h
                  k0 k1 k1 k
                  b (/ (- b a))))))
    (t (error "Cannot rationalize: ~S" number))))

(defun vm-floor (number &optional (divisor 1))
  "Floor returning quotient and rational remainder for VM ratios."
  (let ((ratio (vm-rational-div number divisor)))
    (multiple-value-bind (q r) (vm-bignum-burnikel-ziegler-divide
                                (vm-ratio-numerator ratio)
                                (vm-ratio-denominator ratio)
                                :rounding :floor)
      (values q (vm-make-ratio r (vm-ratio-denominator ratio))))))

(defmethod print-object ((value vm-ratio) stream)
  (format stream "~D/~D" (vm-ratio-numerator value) (vm-ratio-denominator value)))
