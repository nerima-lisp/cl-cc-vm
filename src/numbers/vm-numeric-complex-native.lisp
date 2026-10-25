(in-package :cl-cc/vm)

;;; Native VM complex value: the vm-complex struct, its arithmetic and
;;; transcendental operations, and PRINT-OBJECT / externalization. Distinct
;;; from vm-numeric-complex.lisp's unboxing *plans*, which describe how a
;;; codegen backend may lay one out -- this is the runtime representation
;;; those plans describe.

(defstruct (vm-complex
            (:constructor %make-vm-complex (real imag))
            (:copier nil))
  "Native VM complex value."
  real
  imag)

(defun vm-complex-make (real imag)
  "Construct a native VM complex value."
  (if (and (zerop imag) (realp real))
      real
      (%make-vm-complex real imag)))

(defun %vm-coerce-complex (value)
  (cond
    ((vm-complex-p value) value)
    ((complexp value) (%make-vm-complex (realpart value) (imagpart value)))
    ((numberp value) (%make-vm-complex value 0))
    (t (error "Not a complex numeric value: ~S" value))))

(defun vm-realpart (number)
  (if (vm-complex-p number) (vm-complex-real number) (realpart number)))

(defun vm-imagpart (number)
  (if (vm-complex-p number) (vm-complex-imag number) (imagpart number)))

(defun vm-complex-add (lhs rhs)
  (let ((a (%vm-coerce-complex lhs))
        (b (%vm-coerce-complex rhs)))
    (vm-complex-make (+ (vm-complex-real a) (vm-complex-real b))
                     (+ (vm-complex-imag a) (vm-complex-imag b)))))

(defun vm-complex-sub (lhs rhs)
  (let ((a (%vm-coerce-complex lhs))
        (b (%vm-coerce-complex rhs)))
    (vm-complex-make (- (vm-complex-real a) (vm-complex-real b))
                     (- (vm-complex-imag a) (vm-complex-imag b)))))

(defun vm-complex-mul (lhs rhs)
  (let* ((a (%vm-coerce-complex lhs))
         (b (%vm-coerce-complex rhs))
         (ar (vm-complex-real a)) (ai (vm-complex-imag a))
         (br (vm-complex-real b)) (bi (vm-complex-imag b)))
    (vm-complex-make (- (* ar br) (* ai bi))
                     (+ (* ar bi) (* ai br)))))

(defun vm-complex-conjugate (number)
  (vm-complex-make (vm-realpart number) (- (vm-imagpart number))))

(defun vm-complex-abs (number)
  (let ((r (vm-realpart number))
        (i (vm-imagpart number)))
    (sqrt (+ (* r r) (* i i)))))

(defun vm-complex-phase (number)
  (atan (coerce (vm-imagpart number) 'double-float)
        (coerce (vm-realpart number) 'double-float)))

(defun vm-complex-sqrt (number)
  (let* ((x (vm-realpart number))
         (y (vm-imagpart number)))
    (if (zerop y)
        (if (minusp x)
            (vm-complex-make 0 (sqrt (- x)))
            (sqrt x))
        (let* ((r (vm-complex-abs number))
               (real (sqrt (/ (+ r x) 2)))
               (imag (if (minusp y)
                         (- (sqrt (/ (- r x) 2)))
                         (sqrt (/ (- r x) 2)))))
          (vm-complex-make real imag)))))

(defmethod print-object ((value vm-complex) stream)
  (format stream "#C(~S ~S)" (vm-complex-real value) (vm-complex-imag value)))

(defun %vm-externalize-complex (value)
  (if (vm-complex-p value)
      (complex (vm-complex-real value) (vm-complex-imag value))
      value))
