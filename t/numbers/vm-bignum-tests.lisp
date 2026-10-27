;;; vm-bignum-tests.lisp — Unit tests for the VM native bignum/rational/complex tower
;;;
;;; Covers: vm-bignum-{add,sub,negate,div,mul}, vm-bignum-to-string with non-decimal
;;; radix, vm-integer->bignum roundtrip, vm-rational-{sub,mul,div}, vm-complex-
;;; {add,sub,mul,div,conjugate,abs,phase,log}, and
;;; vm-bignum-burnikel-ziegler-divide rounding modes.

(in-package #:cl-cc/vm/numbers/test)



;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 1: vm-integer->bignum roundtrip
;;; ═══════════════════════════════════════════════════════════════════════════

(defun register-vm-bignum-tests ()
(it-sequential "bignum-integer-roundtrip zero"
  (destructuring-bind (n) (list 0)
    (let ((bn (cl-cc/vm/numbers::vm-integer->bignum n)))
    (expect (cl-cc/vm/numbers::vm-bignum-p bn) :to-be-truthy)
    (expect (= n (cl-cc/vm/numbers::vm-bignum-to-integer bn)) :to-be-truthy))))
(it-sequential "bignum-integer-roundtrip positive"
  (destructuring-bind (n) (list 12345)
    (let ((bn (cl-cc/vm/numbers::vm-integer->bignum n)))
    (expect (cl-cc/vm/numbers::vm-bignum-p bn) :to-be-truthy)
    (expect (= n (cl-cc/vm/numbers::vm-bignum-to-integer bn)) :to-be-truthy))))
(it-sequential "bignum-integer-roundtrip negative"
  (destructuring-bind (n) (list -99999)
    (let ((bn (cl-cc/vm/numbers::vm-integer->bignum n)))
    (expect (cl-cc/vm/numbers::vm-bignum-p bn) :to-be-truthy)
    (expect (= n (cl-cc/vm/numbers::vm-bignum-to-integer bn)) :to-be-truthy))))
(it-sequential "bignum-integer-roundtrip power-of-2"
  (destructuring-bind (n) (list (expt 2 128))
    (let ((bn (cl-cc/vm/numbers::vm-integer->bignum n)))
    (expect (cl-cc/vm/numbers::vm-bignum-p bn) :to-be-truthy)
    (expect (= n (cl-cc/vm/numbers::vm-bignum-to-integer bn)) :to-be-truthy))))
(it-sequential "bignum-integer-roundtrip large-neg"
  (destructuring-bind (n) (list (- (expt 2 200)))
    (let ((bn (cl-cc/vm/numbers::vm-integer->bignum n)))
    (expect (cl-cc/vm/numbers::vm-bignum-p bn) :to-be-truthy)
    (expect (= n (cl-cc/vm/numbers::vm-bignum-to-integer bn)) :to-be-truthy))))
(it-sequential "bignum-add pos+pos"
  (destructuring-bind (a b expected) (list (expt 2 128) (expt 2 64) (+ (expt 2 128) (expt 2 64)))
    (let ((result (cl-cc/vm/numbers::vm-bignum-add a b)))
    (expect (= expected (cl-cc/vm/numbers::vm-bignum-to-integer result)) :to-be-truthy))))
(it-sequential "bignum-add pos+neg"
  (destructuring-bind (a b expected) (list (expt 2 64) (- (expt 2 32)) (- (expt 2 64) (expt 2 32)))
    (let ((result (cl-cc/vm/numbers::vm-bignum-add a b)))
    (expect (= expected (cl-cc/vm/numbers::vm-bignum-to-integer result)) :to-be-truthy))))
(it-sequential "bignum-add neg+neg"
  (destructuring-bind (a b expected) (list (- (expt 2 64)) (- (expt 2 64)) (- (expt 2 65)))
    (let ((result (cl-cc/vm/numbers::vm-bignum-add a b)))
    (expect (= expected (cl-cc/vm/numbers::vm-bignum-to-integer result)) :to-be-truthy))))
(it-sequential "bignum-add sum-zero"
  (destructuring-bind (a b expected) (list (expt 2 64) (- (expt 2 64)) 0)
    (let ((result (cl-cc/vm/numbers::vm-bignum-add a b)))
    (expect (= expected (cl-cc/vm/numbers::vm-bignum-to-integer result)) :to-be-truthy))))
(it-sequential "bignum-sub large-sub"
  (destructuring-bind (a b expected) (list (expt 2 200) (expt 2 100) (- (expt 2 200) (expt 2 100)))
    (expect (= expected (cl-cc/vm/numbers::vm-bignum-to-integer (cl-cc/vm/numbers::vm-bignum-sub a b))) :to-be-truthy)))
(it-sequential "bignum-sub neg-result"
  (destructuring-bind (a b expected) (list (expt 2 64) (expt 2 128) (- (expt 2 64) (expt 2 128)))
    (expect (= expected (cl-cc/vm/numbers::vm-bignum-to-integer (cl-cc/vm/numbers::vm-bignum-sub a b))) :to-be-truthy)))
(it-sequential "bignum-sub sub-self"
  (destructuring-bind (a b expected) (list (expt 2 64) (expt 2 64) 0)
    (expect (= expected (cl-cc/vm/numbers::vm-bignum-to-integer (cl-cc/vm/numbers::vm-bignum-sub a b))) :to-be-truthy)))
(it-sequential "bignum-negate negate-positive"
  (destructuring-bind (n expected) (list (expt 2 100) (- (expt 2 100)))
    (expect (= expected (cl-cc/vm/numbers::vm-bignum-to-integer (cl-cc/vm/numbers::vm-bignum-negate n))) :to-be-truthy)))
(it-sequential "bignum-negate negate-negative"
  (destructuring-bind (n expected) (list (- (expt 2 100)) (expt 2 100))
    (expect (= expected (cl-cc/vm/numbers::vm-bignum-to-integer (cl-cc/vm/numbers::vm-bignum-negate n))) :to-be-truthy)))
(it-sequential "bignum-negate negate-zero"
  (destructuring-bind (n expected) (list 0 0)
    (expect (= expected (cl-cc/vm/numbers::vm-bignum-to-integer (cl-cc/vm/numbers::vm-bignum-negate n))) :to-be-truthy)))
(it-sequential "bignum-div pos-div-pos"
  (destructuring-bind (dividend divisor expected-q expected-r) (list (expt 2 128) (expt 2 64) (expt 2 64) 0)
    (multiple-value-bind (q r) (cl-cc/vm/numbers::vm-bignum-div dividend divisor)
    (expect (= expected-q (cl-cc/vm/numbers::vm-bignum-to-integer q)) :to-be-truthy)
    (expect (= expected-r (cl-cc/vm/numbers::vm-bignum-to-integer r)) :to-be-truthy))))
(it-sequential "bignum-div large-modulo"
  (destructuring-bind (dividend divisor expected-q expected-r) (list (* (expt 2 100) 7) 7 (expt 2 100) 0)
    (multiple-value-bind (q r) (cl-cc/vm/numbers::vm-bignum-div dividend divisor)
    (expect (= expected-q (cl-cc/vm/numbers::vm-bignum-to-integer q)) :to-be-truthy)
    (expect (= expected-r (cl-cc/vm/numbers::vm-bignum-to-integer r)) :to-be-truthy))))
(it-sequential "bignum-div non-exact"
  (destructuring-bind (dividend divisor expected-q expected-r) (list (+ (* (expt 2 64) 5) 3) 5 (expt 2 64) 3)
    (multiple-value-bind (q r) (cl-cc/vm/numbers::vm-bignum-div dividend divisor)
    (expect (= expected-q (cl-cc/vm/numbers::vm-bignum-to-integer q)) :to-be-truthy)
    (expect (= expected-r (cl-cc/vm/numbers::vm-bignum-to-integer r)) :to-be-truthy))))
(it-sequential "bignum-div-by-zero-signals-error"
  (signals error (cl-cc/vm/numbers::vm-bignum-div (expt 2 64) 0)))
(it-sequential "bignum-bz-divide-rounding-modes truncate-pos"
  (destructuring-bind (dividend divisor rounding expected-q expected-r) (list 7 2 :truncate 3 1)
    (multiple-value-bind (q r)
      (cl-cc/vm/numbers::vm-bignum-burnikel-ziegler-divide dividend divisor :rounding rounding)
    (expect (= expected-q q) :to-be-truthy)
    (expect (= expected-r r) :to-be-truthy))))
(it-sequential "bignum-bz-divide-rounding-modes truncate-neg"
  (destructuring-bind (dividend divisor rounding expected-q expected-r) (list -7 2 :truncate -3 -1)
    (multiple-value-bind (q r)
      (cl-cc/vm/numbers::vm-bignum-burnikel-ziegler-divide dividend divisor :rounding rounding)
    (expect (= expected-q q) :to-be-truthy)
    (expect (= expected-r r) :to-be-truthy))))
(it-sequential "bignum-bz-divide-rounding-modes floor-pos"
  (destructuring-bind (dividend divisor rounding expected-q expected-r) (list 7 2 :floor 3 1)
    (multiple-value-bind (q r)
      (cl-cc/vm/numbers::vm-bignum-burnikel-ziegler-divide dividend divisor :rounding rounding)
    (expect (= expected-q q) :to-be-truthy)
    (expect (= expected-r r) :to-be-truthy))))
(it-sequential "bignum-bz-divide-rounding-modes floor-neg"
  (destructuring-bind (dividend divisor rounding expected-q expected-r) (list -7 2 :floor -4 1)
    (multiple-value-bind (q r)
      (cl-cc/vm/numbers::vm-bignum-burnikel-ziegler-divide dividend divisor :rounding rounding)
    (expect (= expected-q q) :to-be-truthy)
    (expect (= expected-r r) :to-be-truthy))))
(it-sequential "bignum-bz-divide-rounding-modes ceiling-pos"
  (destructuring-bind (dividend divisor rounding expected-q expected-r) (list 7 2 :ceiling 4 -1)
    (multiple-value-bind (q r)
      (cl-cc/vm/numbers::vm-bignum-burnikel-ziegler-divide dividend divisor :rounding rounding)
    (expect (= expected-q q) :to-be-truthy)
    (expect (= expected-r r) :to-be-truthy))))
(it-sequential "bignum-bz-divide-rounding-modes ceiling-neg"
  (destructuring-bind (dividend divisor rounding expected-q expected-r) (list -7 2 :ceiling -3 -1)
    (multiple-value-bind (q r)
      (cl-cc/vm/numbers::vm-bignum-burnikel-ziegler-divide dividend divisor :rounding rounding)
    (expect (= expected-q q) :to-be-truthy)
    (expect (= expected-r r) :to-be-truthy))))
(it-sequential "bignum-bz-divide-rounding-modes round-tie-up"
  (destructuring-bind (dividend divisor rounding expected-q expected-r) (list 5 2 :round 2 1)
    (multiple-value-bind (q r)
      (cl-cc/vm/numbers::vm-bignum-burnikel-ziegler-divide dividend divisor :rounding rounding)
    (expect (= expected-q q) :to-be-truthy)
    (expect (= expected-r r) :to-be-truthy))))
(it-sequential "bignum-bz-divide-rounding-modes round-exact"
  (destructuring-bind (dividend divisor rounding expected-q expected-r) (list 6 2 :round 3 0)
    (multiple-value-bind (q r)
      (cl-cc/vm/numbers::vm-bignum-burnikel-ziegler-divide dividend divisor :rounding rounding)
    (expect (= expected-q q) :to-be-truthy)
    (expect (= expected-r r) :to-be-truthy))))
(it-sequential "rational-sub half-minus-third"
  (destructuring-bind (an ad bn bd expected-n expected-d) (list 1 2 1 3 1 6)
    (let ((result (cl-cc/vm/numbers::vm-rational-sub (cl-cc/vm/numbers::vm-make-ratio an ad)
                                            (cl-cc/vm/numbers::vm-make-ratio bn bd))))
    (expect (= expected-n (cl-cc/vm/numbers::vm-ratio-numerator result)) :to-be-truthy)
    (expect (= expected-d (cl-cc/vm/numbers::vm-ratio-denominator result)) :to-be-truthy))))
(it-sequential "rational-sub third-minus-half"
  (destructuring-bind (an ad bn bd expected-n expected-d) (list 1 3 1 2 -1 6)
    (let ((result (cl-cc/vm/numbers::vm-rational-sub (cl-cc/vm/numbers::vm-make-ratio an ad)
                                            (cl-cc/vm/numbers::vm-make-ratio bn bd))))
    (expect (= expected-n (cl-cc/vm/numbers::vm-ratio-numerator result)) :to-be-truthy)
    (expect (= expected-d (cl-cc/vm/numbers::vm-ratio-denominator result)) :to-be-truthy))))
(it-sequential "rational-sub same-value"
  (destructuring-bind (an ad bn bd expected-n expected-d) (list 3 4 3 4 0 1)
    (let ((result (cl-cc/vm/numbers::vm-rational-sub (cl-cc/vm/numbers::vm-make-ratio an ad)
                                            (cl-cc/vm/numbers::vm-make-ratio bn bd))))
    (expect (= expected-n (cl-cc/vm/numbers::vm-ratio-numerator result)) :to-be-truthy)
    (expect (= expected-d (cl-cc/vm/numbers::vm-ratio-denominator result)) :to-be-truthy))))
(it-sequential "rational-mul half-times-third"
  (destructuring-bind (an ad bn bd expected-n expected-d) (list 1 2 1 3 1 6)
    (let ((result (cl-cc/vm/numbers::vm-rational-mul (cl-cc/vm/numbers::vm-make-ratio an ad)
                                            (cl-cc/vm/numbers::vm-make-ratio bn bd))))
    (expect (= expected-n (cl-cc/vm/numbers::vm-ratio-numerator result)) :to-be-truthy)
    (expect (= expected-d (cl-cc/vm/numbers::vm-ratio-denominator result)) :to-be-truthy))))
(it-sequential "rational-mul two-thirds-times-3"
  (destructuring-bind (an ad bn bd expected-n expected-d) (list 2 3 3 1 2 1)
    (let ((result (cl-cc/vm/numbers::vm-rational-mul (cl-cc/vm/numbers::vm-make-ratio an ad)
                                            (cl-cc/vm/numbers::vm-make-ratio bn bd))))
    (expect (= expected-n (cl-cc/vm/numbers::vm-ratio-numerator result)) :to-be-truthy)
    (expect (= expected-d (cl-cc/vm/numbers::vm-ratio-denominator result)) :to-be-truthy))))
(it-sequential "rational-mul neg-product"
  (destructuring-bind (an ad bn bd expected-n expected-d) (list -1 2 1 3 -1 6)
    (let ((result (cl-cc/vm/numbers::vm-rational-mul (cl-cc/vm/numbers::vm-make-ratio an ad)
                                            (cl-cc/vm/numbers::vm-make-ratio bn bd))))
    (expect (= expected-n (cl-cc/vm/numbers::vm-ratio-numerator result)) :to-be-truthy)
    (expect (= expected-d (cl-cc/vm/numbers::vm-ratio-denominator result)) :to-be-truthy))))
(it-sequential "rational-div half-div-third"
  (destructuring-bind (an ad bn bd expected-n expected-d) (list 1 2 1 3 3 2)
    (let ((result (cl-cc/vm/numbers::vm-rational-div (cl-cc/vm/numbers::vm-make-ratio an ad)
                                            (cl-cc/vm/numbers::vm-make-ratio bn bd))))
    (expect (= expected-n (cl-cc/vm/numbers::vm-ratio-numerator result)) :to-be-truthy)
    (expect (= expected-d (cl-cc/vm/numbers::vm-ratio-denominator result)) :to-be-truthy))))
(it-sequential "rational-div third-div-neg"
  (destructuring-bind (an ad bn bd expected-n expected-d) (list 1 3 -1 2 -2 3)
    (let ((result (cl-cc/vm/numbers::vm-rational-div (cl-cc/vm/numbers::vm-make-ratio an ad)
                                            (cl-cc/vm/numbers::vm-make-ratio bn bd))))
    (expect (= expected-n (cl-cc/vm/numbers::vm-ratio-numerator result)) :to-be-truthy)
    (expect (= expected-d (cl-cc/vm/numbers::vm-ratio-denominator result)) :to-be-truthy))))
(it-sequential "rational-div integer-result"
  (destructuring-bind (an ad bn bd expected-n expected-d) (list 2 3 2 9 3 1)
    (let ((result (cl-cc/vm/numbers::vm-rational-div (cl-cc/vm/numbers::vm-make-ratio an ad)
                                            (cl-cc/vm/numbers::vm-make-ratio bn bd))))
    (expect (= expected-n (cl-cc/vm/numbers::vm-ratio-numerator result)) :to-be-truthy)
    (expect (= expected-d (cl-cc/vm/numbers::vm-ratio-denominator result)) :to-be-truthy))))
(it-sequential "rational-div-by-zero-signals-error"
  (signals error (cl-cc/vm/numbers::vm-rational-div (cl-cc/vm/numbers::vm-make-ratio 1 2)
                                (cl-cc/vm/numbers::vm-make-ratio 0 1))))
(it-sequential "complex-add pos-plus-pos"
  (destructuring-bind (ar ai br bi expected-r expected-i) (list 1 2 3 4 4 6)
    (let ((result (cl-cc/vm/numbers::vm-complex-add (cl-cc/vm/numbers::vm-complex-make ar ai)
                                          (cl-cc/vm/numbers::vm-complex-make br bi))))
    (expect (= expected-r (cl-cc/vm/numbers::vm-realpart result)) :to-be-truthy)
    (expect (= expected-i (cl-cc/vm/numbers::vm-imagpart result)) :to-be-truthy))))
(it-sequential "complex-add cancel-imag"
  (destructuring-bind (ar ai br bi expected-r expected-i) (list 2 5 1 -5 3 0)
    (let ((result (cl-cc/vm/numbers::vm-complex-add (cl-cc/vm/numbers::vm-complex-make ar ai)
                                          (cl-cc/vm/numbers::vm-complex-make br bi))))
    (expect (= expected-r (cl-cc/vm/numbers::vm-realpart result)) :to-be-truthy)
    (expect (= expected-i (cl-cc/vm/numbers::vm-imagpart result)) :to-be-truthy))))
(it-sequential "complex-add zero-plus-z"
  (destructuring-bind (ar ai br bi expected-r expected-i) (list 0 0 3 4 3 4)
    (let ((result (cl-cc/vm/numbers::vm-complex-add (cl-cc/vm/numbers::vm-complex-make ar ai)
                                          (cl-cc/vm/numbers::vm-complex-make br bi))))
    (expect (= expected-r (cl-cc/vm/numbers::vm-realpart result)) :to-be-truthy)
    (expect (= expected-i (cl-cc/vm/numbers::vm-imagpart result)) :to-be-truthy))))
(it-sequential "complex-sub simple-sub"
  (destructuring-bind (ar ai br bi expected-r expected-i) (list 5 7 2 3 3 4)
    (let ((result (cl-cc/vm/numbers::vm-complex-sub (cl-cc/vm/numbers::vm-complex-make ar ai)
                                          (cl-cc/vm/numbers::vm-complex-make br bi))))
    (expect (= expected-r (cl-cc/vm/numbers::vm-realpart result)) :to-be-truthy)
    (expect (= expected-i (cl-cc/vm/numbers::vm-imagpart result)) :to-be-truthy))))
(it-sequential "complex-sub neg-result"
  (destructuring-bind (ar ai br bi expected-r expected-i) (list 1 1 3 4 -2 -3)
    (let ((result (cl-cc/vm/numbers::vm-complex-sub (cl-cc/vm/numbers::vm-complex-make ar ai)
                                          (cl-cc/vm/numbers::vm-complex-make br bi))))
    (expect (= expected-r (cl-cc/vm/numbers::vm-realpart result)) :to-be-truthy)
    (expect (= expected-i (cl-cc/vm/numbers::vm-imagpart result)) :to-be-truthy))))
(it-sequential "complex-sub sub-self"
  (destructuring-bind (ar ai br bi expected-r expected-i) (list 3 4 3 4 0 0)
    (let ((result (cl-cc/vm/numbers::vm-complex-sub (cl-cc/vm/numbers::vm-complex-make ar ai)
                                          (cl-cc/vm/numbers::vm-complex-make br bi))))
    (expect (= expected-r (cl-cc/vm/numbers::vm-realpart result)) :to-be-truthy)
    (expect (= expected-i (cl-cc/vm/numbers::vm-imagpart result)) :to-be-truthy))))
(it-sequential "complex-mul i-squared"
  (destructuring-bind (ar ai br bi expected-r expected-i) (list 0 1 0 1 -1 0)
    (let ((result (cl-cc/vm/numbers::vm-complex-mul (cl-cc/vm/numbers::vm-complex-make ar ai)
                                          (cl-cc/vm/numbers::vm-complex-make br bi))))
    (expect (= expected-r (cl-cc/vm/numbers::vm-realpart result)) :to-be-truthy)
    (expect (= expected-i (cl-cc/vm/numbers::vm-imagpart result)) :to-be-truthy))))
(it-sequential "complex-mul real-times-z"
  (destructuring-bind (ar ai br bi expected-r expected-i) (list 3 0 2 5 6 15)
    (let ((result (cl-cc/vm/numbers::vm-complex-mul (cl-cc/vm/numbers::vm-complex-make ar ai)
                                          (cl-cc/vm/numbers::vm-complex-make br bi))))
    (expect (= expected-r (cl-cc/vm/numbers::vm-realpart result)) :to-be-truthy)
    (expect (= expected-i (cl-cc/vm/numbers::vm-imagpart result)) :to-be-truthy))))
(it-sequential "complex-mul standard"
  (destructuring-bind (ar ai br bi expected-r expected-i) (list 1 2 3 4 -5 10)
    (let ((result (cl-cc/vm/numbers::vm-complex-mul (cl-cc/vm/numbers::vm-complex-make ar ai)
                                          (cl-cc/vm/numbers::vm-complex-make br bi))))
    (expect (= expected-r (cl-cc/vm/numbers::vm-realpart result)) :to-be-truthy)
    (expect (= expected-i (cl-cc/vm/numbers::vm-imagpart result)) :to-be-truthy))))
(it-sequential "complex-conjugate-negates-imaginary"
  (let ((result (cl-cc/vm/numbers::vm-complex-conjugate (cl-cc/vm/numbers::vm-complex-make 3 4))))
    (expect (= 3 (cl-cc/vm/numbers::vm-realpart result)) :to-be-truthy)
    (expect (= -4 (cl-cc/vm/numbers::vm-imagpart result)) :to-be-truthy)))
(it-sequential "complex-abs-3-4-is-5"
  (expect (= 5.0 (cl-cc/vm/numbers::vm-complex-abs (cl-cc/vm/numbers::vm-complex-make 3 4))) :to-be-truthy))
(it-sequential "complex-phase-pure-imaginary-is-half-pi"
  (let ((phase (cl-cc/vm/numbers::vm-complex-phase (cl-cc/vm/numbers::vm-complex-make 0 1))))
    (expect (< (abs (- phase (/ pi 2))) 1.0d-12) :to-be-truthy)))
 (describe-sequential "native VM bignum arithmetic"
  (it-property "round-trips through vm-integer->bignum and vm-bignum-to-integer"
      ((n (gen-integer :min -1000000000000000000000 :max 1000000000000000000000)))
    (expect (cl-cc/vm/numbers::vm-bignum-to-integer (cl-cc/vm/numbers::vm-integer->bignum n)) :to-be n))

  (it-property "adds the same as host arithmetic"
      ((a (gen-integer :min -1000000000000000000000 :max 1000000000000000000000))
       (b (gen-integer :min -1000000000000000000000 :max 1000000000000000000000)))
    (expect (cl-cc/vm/numbers::vm-bignum-to-integer
             (cl-cc/vm/numbers::vm-bignum-add (cl-cc/vm/numbers::vm-integer->bignum a)
                                       (cl-cc/vm/numbers::vm-integer->bignum b)))
            :to-be (+ a b)))

  (it-property "subtracts the same as host arithmetic"
      ((a (gen-integer :min -1000000000000000000000 :max 1000000000000000000000))
       (b (gen-integer :min -1000000000000000000000 :max 1000000000000000000000)))
    (expect (cl-cc/vm/numbers::vm-bignum-to-integer
             (cl-cc/vm/numbers::vm-bignum-sub (cl-cc/vm/numbers::vm-integer->bignum a)
                                       (cl-cc/vm/numbers::vm-integer->bignum b)))
            :to-be (- a b)))

  (it-property "multiplies the same as host arithmetic"
      ((a (gen-integer :min -100000000000 :max 100000000000))
       (b (gen-integer :min -100000000000 :max 100000000000)))
    (let* ((product (cl-cc/vm/numbers::vm-bignum-mul (cl-cc/vm/numbers::vm-integer->bignum a)
                                              (cl-cc/vm/numbers::vm-integer->bignum b))))
      (expect (cl-cc/vm/numbers::vm-bignum-to-integer product) :to-be (* a b))))

  (it-property "negation matches host negation"
      ((n (gen-integer :min -1000000000000000000000 :max 1000000000000000000000)))
    (expect (cl-cc/vm/numbers::vm-bignum-to-integer (cl-cc/vm/numbers::vm-bignum-negate
                                              (cl-cc/vm/numbers::vm-integer->bignum n)))
            :to-be (- n)))

  (it-property "gcd matches host gcd for positive operands"
      ((a (gen-integer :min 1 :max 1000000000000))
       (b (gen-integer :min 1 :max 1000000000000)))
    (expect (cl-cc/vm/numbers::vm-bignum-to-integer
             (cl-cc/vm/numbers::vm-bignum-gcd (cl-cc/vm/numbers::vm-integer->bignum a)
                                       (cl-cc/vm/numbers::vm-integer->bignum b)))
            :to-be (gcd a b)))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 2: vm-bignum-add
;;; ═══════════════════════════════════════════════════════════════════════════

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 3: vm-bignum-sub and vm-bignum-negate
;;; ═══════════════════════════════════════════════════════════════════════════

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 4: vm-bignum-div
;;; ═══════════════════════════════════════════════════════════════════════════

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 5: vm-bignum-to-string with non-decimal radix
;;; ═══════════════════════════════════════════════════════════════════════════

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 6: vm-bignum-burnikel-ziegler-divide rounding modes
;;; ═══════════════════════════════════════════════════════════════════════════

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 7: vm-rational-sub, vm-rational-mul, vm-rational-div
;;; ═══════════════════════════════════════════════════════════════════════════

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 8: vm-complex-add, vm-complex-sub, vm-complex-mul, vm-complex-div
;;; ═══════════════════════════════════════════════════════════════════════════

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 9: vm-complex-conjugate, vm-complex-abs, vm-complex-phase,
;;;            vm-complex-log
;;; ═══════════════════════════════════════════════════════════════════════════

