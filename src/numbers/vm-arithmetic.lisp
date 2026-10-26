(in-package #:cl-cc/vm/numbers)

(defun vm-fixnum51-p (value)
  "Return T when VALUE fits in the VM 51-bit signed fixnum range."
  (and (integerp value)
       (<= +vm-min-fixnum51+ value +vm-max-fixnum51+)))

(defun vm-fixnum51-overflow-p (value)
  "Return T when integer VALUE cannot be represented as a VM fixnum."
  (and (integerp value)
       (not (vm-fixnum51-p value))))

(defun %vm-fixnum-rational-p (value)
  "Return T when VALUE is a rational with fixnum numerator and denominator."
  (and (rationalp value)
       (typep (numerator value) 'fixnum)
       (typep (denominator value) 'fixnum)))

(defun %vm-rational-add (lhs rhs)
  "Fast path for rational addition with fixnum numerators and denominators."
  (let* ((a (numerator lhs))
         (b (denominator lhs))
         (c (numerator rhs))
         (d (denominator rhs))
         (g (gcd b d))
         (d/g (/ d g)))
    (/ (+ (* a d/g) (* c (/ b g)))
       (* b d/g))))

(defun %vm-rational-sub (lhs rhs)
  "Fast path for rational subtraction with fixnum numerators and denominators."
  (let* ((a (numerator lhs))
         (b (denominator lhs))
         (c (numerator rhs))
         (d (denominator rhs))
         (g (gcd b d))
         (d/g (/ d g)))
    (/ (- (* a d/g) (* c (/ b g)))
       (* b d/g))))

(defun %vm-rational-mul (lhs rhs)
  "Fast path for rational multiplication with cross-cancellation."
  (let* ((a (numerator lhs))
         (b (denominator lhs))
         (c (numerator rhs))
         (d (denominator rhs))
         (g1 (gcd (abs a) d))
         (g2 (gcd (abs c) b)))
    (/ (* (/ a g1) (/ c g2))
       (* (/ b g2) (/ d g1)))))

(defun %vm-externalize-number (value)
  "Convert native VM number tower values to guest-visible CL numeric values."
  (cond
    ((vm-bignum-p value) (vm-bignum-to-integer value))
    ((vm-ratio-p value) (%vm-externalize-ratio value))
    ((vm-complex-p value) (%vm-externalize-complex value))
    (t value)))

(defun %vm-complex-number-p (value)
  (or (complexp value) (vm-complex-p value)))

(defun %vm-exact-rational-number-p (value)
  (or (rationalp value) (vm-ratio-p value) (vm-bignum-p value)))

(defun %vm-float-contagion-p (&rest values)
  (some #'floatp values))

(defun %vm-add-with-overflow-fallback (lhs rhs)
  (cond
    ((or (%vm-complex-number-p lhs) (%vm-complex-number-p rhs))
     (%vm-externalize-number (vm-complex-add lhs rhs)))
    ((%vm-float-contagion-p lhs rhs)
     (+ lhs rhs))
    ((and (typep lhs 'fixnum) (typep rhs 'fixnum))
     (let ((result (+ lhs rhs)))
       (if (vm-fixnum51-overflow-p result)
            (vm-bignum-add-integers lhs rhs)
            result)))
    ((and (integerp lhs) (integerp rhs)
          (or (vm-fixnum51-overflow-p lhs)
              (vm-fixnum51-overflow-p rhs)))
     (vm-bignum-add-integers lhs rhs))
    ((and (%vm-exact-rational-number-p lhs) (%vm-exact-rational-number-p rhs))
     (%vm-externalize-number (vm-rational-add lhs rhs)))
    ((and (%vm-fixnum-rational-p lhs) (%vm-fixnum-rational-p rhs))
      (%vm-rational-add lhs rhs))
    (t
     (+ lhs rhs))))

(defun %vm-sub-with-overflow-fallback (lhs rhs)
  (cond
    ((or (%vm-complex-number-p lhs) (%vm-complex-number-p rhs))
     (%vm-externalize-number (vm-complex-sub lhs rhs)))
    ((%vm-float-contagion-p lhs rhs)
     (- lhs rhs))
    ((and (typep lhs 'fixnum) (typep rhs 'fixnum))
     (let ((result (- lhs rhs)))
       (if (vm-fixnum51-overflow-p result)
            (vm-bignum-subtract-integers lhs rhs)
            result)))
    ((and (integerp lhs) (integerp rhs)
          (or (vm-fixnum51-overflow-p lhs)
              (vm-fixnum51-overflow-p rhs)))
     (vm-bignum-subtract-integers lhs rhs))
    ((and (%vm-exact-rational-number-p lhs) (%vm-exact-rational-number-p rhs))
     (%vm-externalize-number (vm-rational-sub lhs rhs)))
    ((and (%vm-fixnum-rational-p lhs) (%vm-fixnum-rational-p rhs))
      (%vm-rational-sub lhs rhs))
    (t
     (- lhs rhs))))

(defun %vm-mul-with-overflow-fallback (lhs rhs)
  (cond
    ((or (%vm-complex-number-p lhs) (%vm-complex-number-p rhs))
     (%vm-externalize-number (vm-complex-mul lhs rhs)))
    ((%vm-float-contagion-p lhs rhs)
     (* lhs rhs))
    ((and (typep lhs 'fixnum) (typep rhs 'fixnum))
     (let ((result (* lhs rhs)))
       (if (vm-fixnum51-overflow-p result)
            (vm-bignum-multiply-integers lhs rhs)
            result)))
    ((and (integerp lhs) (integerp rhs)
          (or (typep lhs 'bignum) (typep rhs 'bignum)))
     (vm-bignum-multiply-integers lhs rhs))
    ((and (%vm-exact-rational-number-p lhs) (%vm-exact-rational-number-p rhs))
     (%vm-externalize-number (vm-rational-mul lhs rhs)))
    ((and (%vm-fixnum-rational-p lhs) (%vm-fixnum-rational-p rhs))
      (%vm-rational-mul lhs rhs))
    (t
     (* lhs rhs))))

(defmethod execute-instruction ((inst vm-add) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (%vm-add-with-overflow-fallback
               (vm-reg-get state (vm-lhs inst))
               (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-sub) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (%vm-sub-with-overflow-fallback
               (vm-reg-get state (vm-lhs inst))
               (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-mul) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (%vm-mul-with-overflow-fallback
               (vm-reg-get state (vm-lhs inst))
               (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

;;; Checked arithmetic (FR-149): detect VM fixnum overflow and use the bignum
;;; fallback path. Native backends lower these to hardware overflow checks.
(defmethod execute-instruction ((inst vm-add-checked) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (%vm-add-with-overflow-fallback
               (vm-reg-get state (vm-lhs inst))
               (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-sub-checked) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (%vm-sub-with-overflow-fallback
               (vm-reg-get state (vm-lhs inst))
               (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-mul-checked) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (%vm-mul-with-overflow-fallback
               (vm-reg-get state (vm-lhs inst))
               (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))
