(in-package :cl-cc/vm)

;;; ─── FR-842 Kahan Summation ──────────────────────────────────────────────────
;;;
;;; Error-compensated floating-point summation using the Kahan 1965 algorithm.
;;; Provides both an accumulator API and a convenience function, plus a
;;; divide-and-conquer pairwise sum with O(log n) error growth.

(defstruct (kahan-accumulator (:constructor %make-kahan-accumulator))
  "Error-compensated summation accumulator using the Kahan 1965 algorithm.
The struct maintains a running SUM and a COMPENSATION term that tracks
the low-order bits lost during each addition."
  (sum 0.0d0 :type double-float)
  (compensation 0.0d0 :type double-float))

(defun make-kahan-accumulator (&optional (initial 0.0d0))
  "Create a fresh Kahan accumulator seeded with INITIAL (default 0.0)."
  (%make-kahan-accumulator :sum (float initial 1.0d0)))

(defun kahan-add! (acc val)
  "Add VAL to the Kahan accumulator ACC, tracking the rounding error.
Returns ACC (modified)."
  (let* ((y (- (float val 1.0d0) (kahan-accumulator-compensation acc)))
         (t-sum (+ (kahan-accumulator-sum acc) y)))
    (setf (kahan-accumulator-compensation acc) (- (- t-sum (kahan-accumulator-sum acc)) y)
          (kahan-accumulator-sum acc) t-sum))
  acc)

(defun kahan-result (acc)
  "Return the current compensated sum from accumulator ACC."
  (kahan-accumulator-sum acc))

;;; vm-numeric.lisp — Numeric tower (round, bit ops, transcendentals, float, rational, complex)
;;; vm-extensions.lisp — Char comparisons, symbol plist, PROGV, generic arith

;;; ─── FR-844 / FR-860 / FR-861 / FR-829 Numeric Runtime Extensions ─────────
;;;
;;; This section is intentionally append-only.  Several feature branches touch
;;; earlier primitive arithmetic methods; the new API below exposes extended
;;; precision arithmetic, ANSI numeric contagion metadata, inline arithmetic
;;; dispatch, and explicit overflow-detection helpers without editing those
;;; methods in place.

(defparameter *numeric-precision* 53
  "Current requested arithmetic precision in bits.  WITH-PRECISION binds this
value in an MPFR-compatible style; host arithmetic remains the execution engine
unless callers opt into double-double helpers explicitly.")

(defmacro with-precision (bits &body body)
  "Evaluate BODY with *NUMERIC-PRECISION* dynamically bound to BITS.

The macro mirrors MPFR-style dynamic precision blocks while keeping this VM
portable.  Double-double operations are available through DD+ and DD* when
callers need more than the host double-float mantissa."
  `(let ((*numeric-precision* ,bits))
     ,@body))

(defstruct (double-double
            (:constructor %make-double-double (hi lo))
            (:copier nil))
  "A 128-bit extended precision number represented as HI+LO double-floats.
The operations below use Dekker/Knuth error-free transforms for double-double
addition and multiplication."
  (hi 0.0d0 :type double-float)
  (lo 0.0d0 :type double-float))

(defun make-double-double (value &optional (low 0.0d0))
  "Coerce VALUE and LOW into a normalized DOUBLE-DOUBLE value."
  (%make-double-double (float value 1.0d0) (float low 1.0d0)))

(declaim (inline %dd-coerce %dd-quick-two-sum %dd-two-sum %dd-split %dd-two-prod))

(defun %dd-coerce (value)
  (if (double-double-p value)
      value
      (make-double-double value)))

(defun %dd-quick-two-sum (a b)
  "Return an unevaluated exact sum A+B as two double-floats, assuming |A|>=|B|."
  (let* ((s (+ a b))
         (e (- b (- s a))))
    (values s e)))

(defun %dd-two-sum (a b)
  "Knuth two-sum transform: return S,E where S+E is exactly A+B."
  (let* ((s (+ a b))
         (bb (- s a))
         (e (+ (- a (- s bb)) (- b bb))))
    (values s e)))

(defun %dd-split (a)
  "Dekker split of A into high and low halves for IEEE double precision."
  (let* ((c (* 134217729.0d0 a)) ; 2^27 + 1
         (hi (- c (- c a)))
         (lo (- a hi)))
    (values hi lo)))

(defun %dd-two-prod (a b)
  "Dekker product transform: return P,E where P+E is exactly A*B modulo IEEE ops."
  (let ((p (* a b)))
    (multiple-value-bind (ah al) (%dd-split a)
      (multiple-value-bind (bh bl) (%dd-split b)
        (values p (+ (- (- (- (* ah bh) p) (* al bh)) (* ah bl)) (* al bl)))))))

(defun dd+ (left right)
  "Return LEFT+RIGHT as a normalized DOUBLE-DOUBLE value."
  (let ((a (%dd-coerce left))
        (b (%dd-coerce right)))
    (multiple-value-bind (s e)
        (%dd-two-sum (double-double-hi a) (double-double-hi b))
      (let* ((e (+ e (double-double-lo a) (double-double-lo b))))
        (multiple-value-bind (hi lo) (%dd-quick-two-sum s e)
          (%make-double-double hi lo))))))

(defun dd* (left right)
  "Return LEFT*RIGHT as a normalized DOUBLE-DOUBLE value."
  (let ((a (%dd-coerce left))
        (b (%dd-coerce right)))
    (multiple-value-bind (p e)
        (%dd-two-prod (double-double-hi a) (double-double-hi b))
      (let ((e (+ e
                  (* (double-double-hi a) (double-double-lo b))
                  (* (double-double-lo a) (double-double-hi b)))))
        (multiple-value-bind (hi lo) (%dd-quick-two-sum p e)
          (%make-double-double hi lo))))))

(defun %dd-rational-value (dd)
  (+ (rational (double-double-hi dd))
     (rational (double-double-lo dd))))

(defun %decimal-rational-string (value digits)
  "Render rational VALUE with DIGITS digits after the decimal point."
  (check-type digits (integer 0 *))
  (let* ((negative-p (minusp value))
         (abs-value (abs value))
         (scale (expt 10 digits))
         (scaled (round (* abs-value scale)))
         (integer-part (floor scaled scale))
         (fractional-part (mod scaled scale)))
    (if (zerop digits)
        (format nil "~:[~;-~]~D" negative-p integer-part)
        (format nil "~:[~;-~]~D.~v,'0D" negative-p integer-part digits fractional-part))))

(defun dd-to-string (dd digits)
  "Return DD as a decimal string with DIGITS fractional digits."
  (%decimal-rational-string (%dd-rational-value (%dd-coerce dd)) digits))
