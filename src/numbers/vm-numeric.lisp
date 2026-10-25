(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — Numeric Tower Extensions (redirector)
;;;
;;; Content has been split into three focused files loaded after this one:
;;;
;;;   vm-numeric-bignum-algorithms.lisp — schoolbook/karatsuba/Burnikel-Ziegler
;;;   vm-numeric-complex.lisp           — complex unboxing plan helpers
;;;   vm-numeric-tower.lisp             — native limb bignum, ratio, complex,
;;;                                       VM instructions (FR-301/305/952/955/956)
;;;
;;; Load order: after vm-transcendental.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(progn
  (defstruct (vm-posit (:constructor %make-vm-posit (nbits es bits)))
    (nbits 32 :type (integer 2 *))
    (es 2 :type (integer 0 *))
    (bits 0 :type integer))

  (defun %posit-validate-format (nbits es)
    (unless (and (integerp nbits) (>= nbits 2))
      (error "Posit NBITS must be an integer of at least two bits: ~S" nbits))
    (unless (and (integerp es) (>= es 0) (<= es (- nbits 2)))
      (error "Posit ES must be between zero and NBITS - 2: ~S" es))
    (values nbits es))

  (defun %posit-modulus (nbits)
    (ash 1 nbits))

  (defun %posit-nar-bits (nbits)
    (ash 1 (1- nbits)))

  (defun vm-posit-from-bits (bits &key (nbits 32) (es 2))
    "Construct a Posit value from its unsigned NBITS-wide representation."
    (%posit-validate-format nbits es)
    (unless (and (integerp bits) (<= 0 bits) (< bits (%posit-modulus nbits)))
      (error "Posit bits do not fit in ~D bits: ~S" nbits bits))
    (%make-vm-posit nbits es bits))

  (defun vm-posit-zero-p (posit)
    (zerop (vm-posit-bits posit)))

  (defun vm-posit-nar-p (posit)
    (= (vm-posit-bits posit) (%posit-nar-bits (vm-posit-nbits posit))))

  (defun %posit-decode-positive-bits (bits nbits es)
    (let ((position (- nbits 2)))
      (let ((regime-bit (ldb (byte 1 position) bits))
            (run 0))
        (loop while (and (>= position 0)
                         (= (ldb (byte 1 position) bits) regime-bit))
              do (incf run)
                 (decf position))
        (let ((regime (if (= regime-bit 1) (1- run) (- run))))
          (when (>= position 0)
            (decf position))
          (let ((exponent 0))
            (loop repeat es
                  do (setf exponent
                           (+ (ash exponent 1)
                              (if (>= position 0)
                                  (prog1 (ldb (byte 1 position) bits)
                                    (decf position))
                                  0))))
            (let* ((fraction-bits (1+ position))
                   (fraction (if (plusp fraction-bits)
                                 (ldb (byte fraction-bits 0) bits)
                                 0))
                   (significand (/ (+ (ash 1 fraction-bits) fraction)
                                   (ash 1 fraction-bits))))
              (* (expt 2 (* regime (ash 1 es)))
                 (expt 2 exponent)
                 significand)))))))

  (defun vm-posit-decode (posit)
    "Decode POSIT to an exact rational, or return :NAR for Not-a-Real."
    (check-type posit vm-posit)
    (let* ((bits (vm-posit-bits posit))
           (nbits (vm-posit-nbits posit))
           (nar (%posit-nar-bits nbits)))
      (cond ((zerop bits) 0)
            ((= bits nar) :nar)
            ((logbitp (1- nbits) bits)
             (- (%posit-decode-positive-bits
                 (mod (- bits) (%posit-modulus nbits))
                 nbits
                 (vm-posit-es posit))))
            (t
             (%posit-decode-positive-bits bits nbits (vm-posit-es posit))))))

  (defun %posit-encode-positive (value nbits es)
    (let ((high (1- (%posit-nar-bits nbits)))
          (low 0))
      (loop while (> (- high low) 1)
            for middle = (floor (+ low high) 2)
            for decoded = (%posit-decode-positive-bits middle nbits es)
            do (if (<= decoded value)
                   (setf low middle)
                   (setf high middle)))
      (let* ((low-value (if (zerop low)
                            0
                            (%posit-decode-positive-bits low nbits es)))
             (high-value (%posit-decode-positive-bits high nbits es))
             (low-distance (- value low-value))
             (high-distance (- high-value value)))
        (cond ((< low-distance high-distance) low)
              ((> low-distance high-distance) high)
              ((evenp low) low)
              (t high)))))

  (defun vm-posit-encode (value &key (nbits 32) (es 2) (rounding :nearest-even))
    "Encode a real VALUE using nearest, ties-to-even Posit rounding."
    (%posit-validate-format nbits es)
    (unless (eq rounding :nearest-even)
      (error "Unsupported Posit rounding mode: ~S" rounding))
    (cond ((eq value :nar)
           (%make-vm-posit nbits es (%posit-nar-bits nbits)))
          ((not (realp value))
           (error "Cannot encode a non-real Posit value: ~S" value))
          ((zerop value)
           (%make-vm-posit nbits es 0))
          (t
           (let* ((negative (minusp value))
                  (magnitude (abs (rational value)))
                  (positive-bits (%posit-encode-positive magnitude nbits es))
                  (bits (if negative
                            (mod (- positive-bits) (%posit-modulus nbits))
                            positive-bits)))
             (%make-vm-posit nbits es bits)))))

  (defun %posit-compatible-p (left right)
    (and (= (vm-posit-nbits left) (vm-posit-nbits right))
         (= (vm-posit-es left) (vm-posit-es right))))

  (defun %posit-binary-operation (left right operation)
    (check-type left vm-posit)
    (check-type right vm-posit)
    (unless (%posit-compatible-p left right)
      (error "Posit operands use different formats: ~S and ~S" left right))
    (let ((lhs (vm-posit-decode left))
          (rhs (vm-posit-decode right)))
      (vm-posit-encode
       (if (or (eq lhs :nar) (eq rhs :nar))
           :nar
           (funcall operation lhs rhs))
       :nbits (vm-posit-nbits left)
       :es (vm-posit-es left))))

  (defun vm-posit-add (left right)
    (%posit-binary-operation left right #'+))

  (defun vm-posit-sub (left right)
    (%posit-binary-operation left right #'-))

  (defun vm-posit-mul (left right)
    (%posit-binary-operation left right #'*))

  (defun vm-posit-div (left right)
    (%posit-binary-operation
     left right
     (lambda (lhs rhs)
       (if (zerop rhs) :nar (/ lhs rhs)))))

  (defstruct (vm-quire (:constructor %make-vm-quire (nbits es value)))
    (nbits 32 :type (integer 2 *))
    (es 2 :type (integer 0 *))
    (value 0 :type (or rational keyword)))

  (defun make-vm-quire (&key (nbits 32) (es 2))
    "Create an exact Posit dot-product accumulator."
    (%posit-validate-format nbits es)
    (%make-vm-quire nbits es 0))

  (defun vm-quire-add-product! (quire left right)
    "Accumulate LEFT * RIGHT exactly, without intermediate Posit rounding."
    (check-type quire vm-quire)
    (unless (and (%posit-compatible-p left right)
                 (= (vm-quire-nbits quire) (vm-posit-nbits left))
                 (= (vm-quire-es quire) (vm-posit-es left)))
      (error "Quire and Posit operand formats differ"))
    (let ((lhs (vm-posit-decode left))
          (rhs (vm-posit-decode right)))
      (if (or (eq (vm-quire-value quire) :nar)
              (eq lhs :nar)
              (eq rhs :nar))
          (setf (vm-quire-value quire) :nar)
          (incf (vm-quire-value quire) (* lhs rhs))))
    quire)

  (defun vm-quire-to-posit (quire)
    "Round the exact QUIRE total to its Posit format once."
    (check-type quire vm-quire)
    (vm-posit-encode (vm-quire-value quire)
                     :nbits (vm-quire-nbits quire)
                     :es (vm-quire-es quire))))
