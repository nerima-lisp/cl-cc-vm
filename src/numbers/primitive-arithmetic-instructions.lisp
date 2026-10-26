(in-package #:cl-cc/vm/numbers)

(define-vm-binary-instruction vm-div          :div      "Integer division. DST = floor(LHS / RHS).")

(define-vm-binary-instruction vm-cl-div       :cl-div   "CL division. DST = LHS / RHS (rational-preserving).")

(define-vm-binary-instruction vm-mod          :mod      "Modulo operation. DST = LHS mod RHS.")

(define-vm-unary-instruction  vm-neg          :neg      "Negation. DST = -SRC.")

(define-vm-unary-instruction  vm-abs          :abs      "Absolute value. DST = |SRC|.")

(define-vm-unary-instruction  vm-inc          :inc      "Increment. DST = SRC + 1.")

(define-vm-unary-instruction  vm-dec          :dec      "Decrement. DST = SRC - 1.")

(define-vm-binary-instruction vm-min          :min      "Minimum. DST = min(LHS, RHS).")

(define-vm-binary-instruction vm-max          :max      "Maximum. DST = max(LHS, RHS).")

(define-vm-binary-instruction vm-truncate     :truncate "Truncate division. DST = truncate(LHS / RHS).")

(define-vm-binary-instruction vm-floor-inst   :floor    "Floor division. DST = floor(LHS / RHS).")

(define-vm-binary-instruction vm-ceiling-inst :ceiling  "Ceiling division. DST = ceiling(LHS / RHS).")

(define-vm-binary-instruction vm-round-inst   :round    "Round division (banker's rounding). DST = round(LHS / RHS).")

(define-vm-binary-instruction vm-rem          :rem      "Remainder. DST = rem(LHS, RHS).")

;;; Instruction Execution - Arithmetic Extensions

(defun %vm-cl-div-fast-path (lhs rhs)
  "Return LHS/RHS and the selected rational arithmetic path.
The path value is diagnostic and keeps the runtime specialization testable."
  (cond
    ((and (typep lhs 'fixnum) (typep rhs 'fixnum))
      ;; Standard CL / already produces the correct ratio for fixnum division.
      (values (/ lhs rhs) :fixnum))
    ((and (%vm-fixnum-rational-p lhs) (%vm-fixnum-rational-p rhs))
      (values (/ (* (numerator lhs) (denominator rhs))
                 (* (denominator lhs) (numerator rhs)))
             :fixnum-rational))
    ((and (typep lhs 'fixnum) (%vm-fixnum-rational-p rhs))
     (values (/ (* lhs (denominator rhs))
                (numerator rhs))
             :fixnum-rational))
    ((and (%vm-fixnum-rational-p lhs) (typep rhs 'fixnum))
     (values (/ (numerator lhs)
                (* (denominator lhs) rhs))
             :fixnum-rational))
    (t
     (values (/ lhs rhs) :generic))))

(defmethod execute-instruction ((inst vm-div) state pc labels)
  (declare (ignore labels))
  (let* ((lhs (vm-reg-get state (vm-lhs inst)))
         (divisor (vm-reg-get state (vm-rhs inst))))
    (if (zerop divisor)
        (error "vm-div: Division by zero")
        (let ((result (if (or (typep lhs 'bignum) (typep divisor 'bignum))
                          (nth-value 0 (vm-bignum-burnikel-ziegler-divide lhs divisor :rounding :floor))
                          (floor lhs divisor))))
          (vm-reg-set state (vm-dst inst) result)
          (values (1+ pc) nil nil)))))

(defmethod execute-instruction ((inst vm-mul) state pc labels)
  (declare (ignore labels))
  (let* ((lhs (vm-reg-get state (vm-lhs inst)))
         (rhs (vm-reg-get state (vm-rhs inst)))
         (result (%vm-mul-with-overflow-fallback lhs rhs)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-cl-div) state pc labels)
  (declare (ignore labels))
  (let ((divisor (vm-reg-get state (vm-rhs inst))))
    (if (zerop divisor)
        (error "Division by zero")
        (let ((result (%vm-cl-div-fast-path (vm-reg-get state (vm-lhs inst)) divisor)))
          (vm-reg-set state (vm-dst inst) result)
          (values (1+ pc) nil nil)))))
(progn
  (define-vm-instruction vm-float-div (vm-cl-div)
    (precision :f64 :reader vm-float-precision :type (member :f32 :f64))
    (:sexp-tag :fdiv)
    (:sexp-slots dst lhs rhs precision))
  (setf (gethash :fdiv *instruction-constructors*)
        (lambda (sexp)
          (make-vm-float-div
           :dst (second sexp) :lhs (third sexp) :rhs (fourth sexp)
           :precision (if (cddddr sexp) (fifth sexp) :f64)))))

(defmethod execute-instruction ((inst vm-mod) state pc labels)
  (declare (ignore labels))
  (let ((divisor (vm-reg-get state (vm-rhs inst))))
    (if (zerop divisor)
        (error "vm-mod: Division by zero")
        (let ((result (mod (vm-reg-get state (vm-lhs inst)) divisor)))
          (vm-reg-set state (vm-dst inst) result)
          (values (1+ pc) nil nil)))))

(define-simple-instruction vm-neg :unary  -)

(define-simple-instruction vm-abs :unary  abs)

(define-simple-instruction vm-inc :unary  1+)

(define-simple-instruction vm-dec :unary  1-)

(define-simple-instruction vm-min :binary min)

(define-simple-instruction vm-max :binary max)

;;; Division family: data-driven via macro (truncate, floor, ceiling share identical structure)

(defmacro define-vm-division-executors (&rest specs)
  "Generate execute-instruction methods for integer division variants.
Each SPEC is (instruction-type host-function)."
  `(progn
     ,@(loop for (inst-type host-fn) in specs
             collect `(defmethod execute-instruction ((inst ,inst-type) state pc labels)
                         (declare (ignore labels))
                         (let* ((lhs (vm-reg-get state (vm-lhs inst)))
                                (rhs (vm-reg-get state (vm-rhs inst))))
                           (when (zerop rhs)
                             (error "Division by zero"))
                           (multiple-value-bind (q r)
                               (cond
                                 ((and (typep lhs 'fixnum) (typep rhs 'fixnum))
                                  (,host-fn lhs rhs))
                                 ((or (typep lhs 'bignum) (typep rhs 'bignum))
                                  (vm-bignum-burnikel-ziegler-divide
                                   lhs rhs
                                   :rounding ,(case host-fn
                                                (truncate :truncate)
                                                (floor :floor)
                                                (ceiling :ceiling)
                                                (round :round)
                                                (otherwise :truncate))))
                                 (t
                                  (,host-fn lhs rhs)))
                             (vm-reg-set state (vm-dst inst) q)
                             (setf (vm-values-list state) (list q r))))
                         (values (1+ pc) nil nil)))))

(define-vm-division-executors
  (vm-truncate     truncate)
  (vm-floor-inst   floor)
  (vm-ceiling-inst ceiling)
  (vm-round-inst   round))

(define-simple-instruction vm-rem   :binary rem)

;;; FR-885: Float rounding (ffloor/fceiling/ftruncate/fround)
;;; Return float quotient + integer remainder. Map to host CL float rounding.

(define-vm-binary-instruction vm-ffloor :ffloor "Float floor: returns float quotient and float remainder.")

(define-vm-binary-instruction vm-fceiling :fceiling "Float ceiling: returns float quotient and float remainder.")

(define-vm-binary-instruction vm-ftruncate :ftruncate "Float truncate: returns float quotient and float remainder.")

(define-vm-binary-instruction vm-fround :fround "Float round: returns float quotient and float remainder.")

(defmacro define-vm-float-rounding-executors (&rest specs)
  "Generate execute-instruction methods for float rounding variants.
Each SPEC is (instruction-type host-function)."
  `(progn
     ,@(loop for (inst-type host-fn) in specs
             collect `(defmethod execute-instruction ((inst ,inst-type) state pc labels)
                        (declare (ignore labels))
                        (let* ((lhs (vm-reg-get state (vm-lhs inst)))
                               (rhs (vm-reg-get state (vm-rhs inst))))
                          (when (zerop rhs)
                            (error "Division by zero"))
                          (multiple-value-bind (q r)
                              (,host-fn lhs rhs)
                            (vm-reg-set state (vm-dst inst) q)
                            (setf (vm-values-list state) (list q r))))
                        (values (1+ pc) nil nil)))))

(define-vm-float-rounding-executors
  (vm-ffloor ffloor)
  (vm-fceiling fceiling)
  (vm-ftruncate ftruncate)
  (vm-fround fround))
