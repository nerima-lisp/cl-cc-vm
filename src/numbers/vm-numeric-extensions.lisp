(in-package #:cl-cc/vm/numbers)

(defmacro %define-generic-binop-table (&rest entries)
  `(progn
     ,@(loop for (name tag result-form) in entries
             collect
             `(progn
                (define-vm-instruction ,name (vm-instruction)
                  ,(format nil "Polymorphic ~(~A~)." tag)
                  (dst nil :reader vm-dst)
                  (lhs nil :reader vm-lhs)
                  (rhs nil :reader vm-rhs)
                  (:sexp-tag ,tag)
                  (:sexp-slots dst lhs rhs))
                (defmethod execute-instruction ((inst ,name) state pc labels)
                  (declare (ignore labels))
                  (let* ((l (vm-reg-get state (vm-lhs inst)))
                         (r (vm-reg-get state (vm-rhs inst))))
                    (vm-reg-set state (vm-dst inst) ,result-form)
                    (values (1+ pc) nil nil)))))))

(%define-generic-binop-table
  (vm-generic-add :generic-add (+ l r))
  (vm-generic-sub :generic-sub (- l r))
  (vm-generic-mul :generic-mul (* l r))
  (vm-generic-eq  :generic-eq  (if (equal l r) t nil))
  (vm-generic-lt  :generic-lt  (if (< l r) t nil))
  (vm-generic-gt  :generic-gt  (if (> l r) t nil)))

(define-vm-instruction vm-generic-div (vm-instruction)
  "Polymorphic division (floor)."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :generic-div)
  (:sexp-slots dst lhs rhs))

(defmethod execute-instruction ((inst vm-generic-div) state pc labels)
  (declare (ignore labels))
  (let* ((l (vm-reg-get state (vm-lhs inst)))
         (r (vm-reg-get state (vm-rhs inst))))
    (when (zerop r)
      (error "vm-generic-div: division by zero"))
    (vm-reg-set state (vm-dst inst) (floor l r))
    (values (1+ pc) nil nil)))

(defun vm-floor (dividend &optional (divisor 1))
  "FR-884/FR-955: floor with two-value return.

For native VM ratios, return an integer quotient and a normalized VM ratio
remainder.  Other numbers preserve the existing host-compatible behavior."
  (if (or (vm-ratio-p dividend) (vm-ratio-p divisor))
      (let ((ratio (vm-rational-div dividend divisor)))
        (multiple-value-bind (q r) (vm-bignum-burnikel-ziegler-divide
                                    (vm-ratio-numerator ratio)
                                    (vm-ratio-denominator ratio)
                                    :rounding :floor)
          (values q (vm-make-ratio r (vm-ratio-denominator ratio)))))
      (multiple-value-bind (q r) (floor dividend divisor)
        (values q r))))

(defun vm-ffloor (dividend divisor) "FR-885: float floor with two-value float return." (multiple-value-bind (q r) (ffloor dividend divisor) (values q r)))
