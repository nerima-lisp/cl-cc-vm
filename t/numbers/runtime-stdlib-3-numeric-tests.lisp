;;; runtime-stdlib-3-numeric-tests.lisp — FR-952/955/956 native VM number tower

(in-package #:cl-cc/vm/numbers/test)



(defun register-runtime-stdlib-3-numeric-tests ()
(it-sequential "fr-952-bignum-large-multiplication"
  (let* ((lhs (1- (expt 2 257)))
         (rhs (+ (expt 2 193) 12345))
         (product (cl-cc/vm/numbers::vm-bignum-mul lhs rhs)))
    (expect (cl-cc/vm/numbers::vm-bignum-p product) :to-be-truthy)
    (expect (= (* lhs rhs) (cl-cc/vm/numbers::vm-bignum-to-integer product)) :to-be-truthy)))
(it-sequential "fr-952-bignum-gcd"
  (let* ((a (* (expt 2 180) 45))
         (b (* (expt 2 120) 75))
         (g (cl-cc/vm/numbers::vm-bignum-gcd a b)))
    (expect (cl-cc/vm/numbers::vm-bignum-p g) :to-be-truthy)
    (expect (= (gcd a b) (cl-cc/vm/numbers::vm-bignum-to-integer g)) :to-be-truthy)))
(it-sequential "fr-955-rational-addition-normalizes"
  (let ((result (cl-cc/vm/numbers::vm-rational-add
                 (cl-cc/vm/numbers::vm-make-ratio 1 3)
                 (cl-cc/vm/numbers::vm-make-ratio 1 6))))
    (expect (cl-cc/vm/numbers::vm-ratio-p result) :to-be-truthy)
    (expect (= 1 (cl-cc/vm/numbers::vm-ratio-numerator result)) :to-be-truthy)
    (expect (= 2 (cl-cc/vm/numbers::vm-ratio-denominator result)) :to-be-truthy)))
(it-sequential "fr-955-rationalize-decimal"
  (let ((result (cl-cc/vm/numbers:vm-rationalize 0.1d0)))
    (expect (cl-cc/vm/numbers::vm-ratio-p result) :to-be-truthy)
    (expect (= 1 (cl-cc/vm/numbers::vm-ratio-numerator result)) :to-be-truthy)
    (expect (= 10 (cl-cc/vm/numbers::vm-ratio-denominator result)) :to-be-truthy)))
(it-sequential "fr-955-vm-floor-rational-remainder"
  (multiple-value-bind (q r)
      (cl-cc/vm/numbers::vm-floor (cl-cc/vm/numbers::vm-make-ratio 7 3))
    (expect (= 2 q) :to-be-truthy)
    (expect (cl-cc/vm/numbers::vm-ratio-p r) :to-be-truthy)
    (expect (= 1 (cl-cc/vm/numbers::vm-ratio-numerator r)) :to-be-truthy)
    (expect (= 3 (cl-cc/vm/numbers::vm-ratio-denominator r)) :to-be-truthy)))
(it-sequential "fr-956-complex-sqrt-minus-one"
  (let ((result (cl-cc/vm/numbers::vm-complex-sqrt -1)))
    (expect (cl-cc/vm/numbers::vm-complex-p result) :to-be-truthy)
    (expect (= 0 (cl-cc/vm/numbers::vm-realpart result)) :to-be-truthy)
    (expect (= 1 (cl-cc/vm/numbers::vm-imagpart result)) :to-be-truthy)))
(it-sequential "fr-956-complex-print-format"
  (expect (write-to-string (cl-cc/vm/numbers::vm-complex-make 3 4)) :to-equal "#C(3 4)"))
)
