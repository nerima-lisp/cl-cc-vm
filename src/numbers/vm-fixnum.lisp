(in-package #:cl-cc/vm/numbers)

(defconstant +vm-min-fixnum51+ (- (ash 1 50))
  "Smallest integer encodable by the VM's 51-bit signed fixnum representation.")

(defconstant +vm-max-fixnum51+ (1- (ash 1 50))
  "Largest integer encodable by the VM's 51-bit signed fixnum representation.")

;;; ──── FR-724: Numeric Tower Optimization ────
(defun fixnum-p (x)
  "Return T if X is a tagged fixnum."
  (and (integerp x) (zerop (logand x #b1))))

(defun make-fixnum (n)
  "Create a tagged fixnum from integer N."
  (ash n 1))

(defun fixnum-value (x)
  "Extract the integer value from fixnum X."
  (ash x -1))

(defun fixnum-add (a b)
  "Add two fixnums. Returns (values result overflow-p)."
  (let ((result (+ (fixnum-value a) (fixnum-value b))))
    (if (<= (- (ash 1 62)) result (1- (ash 1 62)))
        (values (make-fixnum result) nil)
        (values nil t))))

(defun fixnum-mul (a b)
  "Multiply two fixnums. Returns (values result overflow-p)."
  (let ((result (* (fixnum-value a) (fixnum-value b))))
    (if (<= (- (ash 1 62)) result (1- (ash 1 62)))
        (values (make-fixnum result) nil)
        (values nil t))))
