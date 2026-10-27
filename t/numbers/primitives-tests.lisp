;;;; tests/unit/vm/primitives-tests.lisp — VM Primitive Instruction Tests
;;;
;;; Tests for execute-instruction on type predicates, comparisons,
;;; arithmetic extensions, boolean ops, and vm-typep.
;;;
;;; Relies on make-test-vm / exec1 helpers from list-tests.lisp.

(in-package #:cl-cc/vm/numbers/test)

;;; Tests that call with-replaced-function mutate the global symbol-function
;;; binding and are therefore not safe to run concurrently with other workers.

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Helpers: construct and execute unary/binary VM instructions
;;; ═══════════════════════════════════════════════════════════════════════════

(defun %with-unary-vm-state (value thunk)
  "Run THUNK with a fresh VM state whose unary source register is preloaded."
  (let ((state (make-test-vm)))
    (cl-cc/vm:vm-reg-set state 1 value)
    (funcall thunk state)))

(defun %with-binary-vm-state (lhs rhs thunk)
  "Run THUNK with a fresh VM state whose binary operand registers are preloaded.
This centralizes the repeated :R1/:R2 setup used by binary instruction tests."
  (let ((state (make-test-vm)))
    (cl-cc/vm:vm-reg-set state 1 lhs)
    (cl-cc/vm:vm-reg-set state 2 rhs)
    (funcall thunk state)))

(defun %run-unary-inst-with (instruction-thunk src-val)
  "Run INSTRUCTION-THUNK against SRC-VAL and return the destination register.
INSTRUCTION-THUNK receives the source register index and must build the VM instruction."
  (%with-unary-vm-state
   src-val
   (lambda (state)
     (exec1 (funcall instruction-thunk 1) state)
     (cl-cc/vm:vm-reg-get state 0))))

(defun %run-unary-inst (ctor-fn src-val)
  "Run a unary VM instruction constructor against SRC-VAL and return DST."
  (%run-unary-inst-with
   (lambda (src)
     (funcall ctor-fn :dst 0 :src src))
   src-val))

(defun %run-binary-inst (ctor-fn lhs rhs)
  "Run a binary VM instruction constructor against LHS/RHS and return DST."
  (%with-binary-vm-state
   lhs rhs
   (lambda (state)
     (exec1 (funcall ctor-fn :dst 0 :lhs 1 :rhs 2) state)
     (cl-cc/vm:vm-reg-get state 0))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 1: Type Predicates (pred1 pattern — return 1/0)
;;; ═══════════════════════════════════════════════════════════════════════════

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 2: EQL comparison (pred2 — return 1/0)
;;; ═══════════════════════════════════════════════════════════════════════════

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 3: Numeric Comparisons (pred2 — return 1/0)
;;; ═══════════════════════════════════════════════════════════════════════════

(defun register-primitives-tests ()
(it-sequential "prim-comparison lt-true"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc/vm:make-vm-lt 3 5 1)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))
(it-sequential "prim-comparison lt-false"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc/vm:make-vm-lt 5 3 0)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))
(it-sequential "prim-comparison lt-equal"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc/vm:make-vm-lt 3 3 0)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))
(it-sequential "prim-comparison gt-true"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc/vm:make-vm-gt 5 3 1)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))
(it-sequential "prim-comparison gt-false"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc/vm:make-vm-gt 3 5 0)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))
(it-sequential "prim-comparison le-true-lt"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc/vm:make-vm-le 3 5 1)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))
(it-sequential "prim-comparison le-true-eq"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc/vm:make-vm-le 3 3 1)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))
(it-sequential "prim-comparison le-false"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc/vm:make-vm-le 5 3 0)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))
(it-sequential "prim-comparison ge-true-gt"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc/vm:make-vm-ge 5 3 1)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))
(it-sequential "prim-comparison ge-true-eq"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc/vm:make-vm-ge 3 3 1)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))
(it-sequential "prim-comparison ge-false"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc/vm:make-vm-ge 3 5 0)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))
(it-sequential "prim-comparison num-eq-true"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc/vm:make-vm-num-eq 7 7 1)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))
(it-sequential "prim-comparison num-eq-false"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc/vm:make-vm-num-eq 7 8 0)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))
(it-sequential "prim-unary-arith neg-pos"
  (destructuring-bind (ctor src expected) (list #'cl-cc/vm:make-vm-neg 5 -5)
    (expect (= expected (%run-unary-inst ctor src)) :to-be-truthy)))
(it-sequential "prim-unary-arith neg-neg"
  (destructuring-bind (ctor src expected) (list #'cl-cc/vm:make-vm-neg -3 3)
    (expect (= expected (%run-unary-inst ctor src)) :to-be-truthy)))
(it-sequential "prim-unary-arith neg-zero"
  (destructuring-bind (ctor src expected) (list #'cl-cc/vm:make-vm-neg 0 0)
    (expect (= expected (%run-unary-inst ctor src)) :to-be-truthy)))
(it-sequential "prim-unary-arith abs-pos"
  (destructuring-bind (ctor src expected) (list #'cl-cc/vm:make-vm-abs 5 5)
    (expect (= expected (%run-unary-inst ctor src)) :to-be-truthy)))
(it-sequential "prim-unary-arith abs-neg"
  (destructuring-bind (ctor src expected) (list #'cl-cc/vm:make-vm-abs -7 7)
    (expect (= expected (%run-unary-inst ctor src)) :to-be-truthy)))
(it-sequential "prim-unary-arith abs-zero"
  (destructuring-bind (ctor src expected) (list #'cl-cc/vm:make-vm-abs 0 0)
    (expect (= expected (%run-unary-inst ctor src)) :to-be-truthy)))
(it-sequential "prim-unary-arith inc-pos"
  (destructuring-bind (ctor src expected) (list #'cl-cc/vm:make-vm-inc 5 6)
    (expect (= expected (%run-unary-inst ctor src)) :to-be-truthy)))
(it-sequential "prim-unary-arith inc-neg"
  (destructuring-bind (ctor src expected) (list #'cl-cc/vm:make-vm-inc -1 0)
    (expect (= expected (%run-unary-inst ctor src)) :to-be-truthy)))
(it-sequential "prim-unary-arith dec-pos"
  (destructuring-bind (ctor src expected) (list #'cl-cc/vm:make-vm-dec 5 4)
    (expect (= expected (%run-unary-inst ctor src)) :to-be-truthy)))
(it-sequential "prim-unary-arith dec-zero"
  (destructuring-bind (ctor src expected) (list #'cl-cc/vm:make-vm-dec 0 -1)
    (expect (= expected (%run-unary-inst ctor src)) :to-be-truthy)))
(it-sequential "prim-binary-arith div-10/3"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc/vm:make-vm-div 10 3 3)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))
(it-sequential "prim-binary-arith div-neg"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc/vm:make-vm-div -7 2 -4)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))
(it-sequential "prim-binary-arith cl-div-rational"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc/vm:make-vm-cl-div 3 4 3/4)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))
(it-sequential "prim-binary-arith cl-div-ratio-by-ratio"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc/vm:make-vm-cl-div 1/3 4/5 5/12)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))
(it-sequential "prim-binary-arith mod-10/3"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc/vm:make-vm-mod 10 3 1)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))
(it-sequential "prim-binary-arith mod-neg"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc/vm:make-vm-mod -7 2 1)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))
(it-sequential "prim-binary-arith min-lhs"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc/vm:make-vm-min 3 5 3)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))
(it-sequential "prim-binary-arith min-rhs"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc/vm:make-vm-min 5 3 3)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))
(it-sequential "prim-binary-arith max-lhs"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc/vm:make-vm-max 5 3 5)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))
(it-sequential "prim-binary-arith max-rhs"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc/vm:make-vm-max 3 5 5)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))
(it-sequential "prim-binary-arith rem-10/3"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc/vm:make-vm-rem 10 3 1)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))
(it-sequential "prim-binary-arith rem-neg"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc/vm:make-vm-rem -7 2 -1)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))
(it-sequential "prim-add-fixnum51-overflow-uses-bignum-fallback"
  (let ((called nil))
    (with-replaced-function (cl-cc/vm/numbers::vm-bignum-add-integers
                             (lambda (lhs rhs)
                               (setf called t)
                               (+ lhs rhs)))
      (expect (= (ash 1 50) (%run-binary-inst #'cl-cc/vm:make-vm-add cl-cc/vm/numbers::+vm-max-fixnum51+ 1)) :to-be-truthy)
      (expect called :to-be-truthy))))
(it-sequential "prim-add-fixnum51-fast-path-skips-bignum-fallback"
  (let ((called nil))
    (with-replaced-function (cl-cc/vm/numbers::vm-bignum-add-integers
                             (lambda (lhs rhs)
                               (declare (ignore lhs rhs))
                               (setf called t)
                               -1))
      (expect (= 42 (%run-binary-inst #'cl-cc/vm:make-vm-add 40 2)) :to-be-truthy)
      (expect called :to-be-null))))
(it-sequential "prim-sub-fixnum51-overflow-uses-bignum-fallback"
  (let ((called nil))
    (with-replaced-function (cl-cc/vm/numbers::vm-bignum-subtract-integers
                             (lambda (lhs rhs)
                               (setf called t)
                               (- lhs rhs)))
      (expect (= (1- cl-cc/vm/numbers::+vm-min-fixnum51+) (%run-binary-inst #'cl-cc/vm:make-vm-sub cl-cc/vm/numbers::+vm-min-fixnum51+ 1)) :to-be-truthy)
      (expect called :to-be-truthy))))
(it-sequential "prim-mul-fixnum51-overflow-uses-bignum-fallback"
  (let ((called nil))
    (with-replaced-function (cl-cc/vm/numbers::vm-bignum-multiply-integers
                             (lambda (lhs rhs &key base threshold)
                               (declare (ignore base threshold))
                               (setf called t)
                               (* lhs rhs)))
      (expect (= (* cl-cc/vm/numbers::+vm-max-fixnum51+ 2) (%run-binary-inst #'cl-cc/vm:make-vm-mul cl-cc/vm/numbers::+vm-max-fixnum51+ 2)) :to-be-truthy)
      (expect called :to-be-truthy))))
(it-sequential "prim-cl-div-fast-path-selection fixnum"
  (destructuring-bind (lhs rhs expected expected-path) (list 3 4 3/4 :fixnum)
    (multiple-value-bind (result path)
      (cl-cc/vm/numbers::%vm-cl-div-fast-path lhs rhs)
    (expect (= expected result) :to-be-truthy)
    (expect path :to-be expected-path))))
(it-sequential "prim-cl-div-fast-path-selection ratio-ratio"
  (destructuring-bind (lhs rhs expected expected-path) (list 1/3 4/5 5/12 :fixnum-rational)
    (multiple-value-bind (result path)
      (cl-cc/vm/numbers::%vm-cl-div-fast-path lhs rhs)
    (expect (= expected result) :to-be-truthy)
    (expect path :to-be expected-path))))
(it-sequential "prim-cl-div-fast-path-selection fixnum-ratio"
  (destructuring-bind (lhs rhs expected expected-path) (list 2 3/5 10/3 :fixnum-rational)
    (multiple-value-bind (result path)
      (cl-cc/vm/numbers::%vm-cl-div-fast-path lhs rhs)
    (expect (= expected result) :to-be-truthy)
    (expect path :to-be expected-path))))
(it-sequential "prim-cl-div-fast-path-selection ratio-fixnum"
  (destructuring-bind (lhs rhs expected expected-path) (list 2/3 5 2/15 :fixnum-rational)
    (multiple-value-bind (result path)
      (cl-cc/vm/numbers::%vm-cl-div-fast-path lhs rhs)
    (expect (= expected result) :to-be-truthy)
    (expect path :to-be expected-path))))
(it-sequential "prim-div-by-zero div"
  (destructuring-bind (make-inst) (list #'cl-cc/vm:make-vm-div)
    (%with-binary-vm-state
   10 0
   (lambda (state)
     (signals error (exec1 (funcall make-inst :dst 0 :lhs 1 :rhs 2) state))))))
(it-sequential "complex-div-by-zero-signals-error" (%with-binary-vm-state #C(1 2) #C(0 0) (lambda (state) (signals error (exec1 (cl-cc/vm:make-vm-cl-div :dst 0 :lhs 1 :rhs 2) state)))))
(it-sequential "prim-div-by-zero mod"
  (destructuring-bind (make-inst) (list #'cl-cc/vm:make-vm-mod)
    (%with-binary-vm-state
   10 0
   (lambda (state)
     (signals error (exec1 (funcall make-inst :dst 0 :lhs 1 :rhs 2) state))))))
(it-sequential "prim-rounding-behavior truncate"
  (destructuring-bind (make-inst expected-q expected-vals) (list #'cl-cc/vm:make-vm-truncate 3 '(3  1))
    (%with-binary-vm-state
   7 2
   (lambda (state)
     (exec1 (funcall make-inst :dst 0 :lhs 1 :rhs 2) state)
      (expect (= expected-q (cl-cc/vm:vm-reg-get state 0)) :to-be-truthy)
      (when expected-vals
        (expect (cl-cc/vm:vm-values-list state) :to-equal expected-vals))))))
(it-sequential "prim-rounding-behavior floor"
  (destructuring-bind (make-inst expected-q expected-vals) (list #'cl-cc/vm:make-vm-floor-inst 3 '(3  1))
    (%with-binary-vm-state
   7 2
   (lambda (state)
     (exec1 (funcall make-inst :dst 0 :lhs 1 :rhs 2) state)
      (expect (= expected-q (cl-cc/vm:vm-reg-get state 0)) :to-be-truthy)
      (when expected-vals
        (expect (cl-cc/vm:vm-values-list state) :to-equal expected-vals))))))
(it-sequential "prim-rounding-behavior ceiling"
  (destructuring-bind (make-inst expected-q expected-vals) (list #'cl-cc/vm:make-vm-ceiling-inst 4 '(4 -1))
    (%with-binary-vm-state
   7 2
   (lambda (state)
     (exec1 (funcall make-inst :dst 0 :lhs 1 :rhs 2) state)
      (expect (= expected-q (cl-cc/vm:vm-reg-get state 0)) :to-be-truthy)
      (when expected-vals
        (expect (cl-cc/vm:vm-values-list state) :to-equal expected-vals))))))
(it-sequential "prim-rounding-behavior round"
  (destructuring-bind (make-inst expected-q expected-vals) (list #'cl-cc/vm:make-vm-round-inst 4 '(4 -1))
    (%with-binary-vm-state
   7 2
   (lambda (state)
     (exec1 (funcall make-inst :dst 0 :lhs 1 :rhs 2) state)
      (expect (= expected-q (cl-cc/vm:vm-reg-get state 0)) :to-be-truthy)
      (when expected-vals
        (expect (cl-cc/vm:vm-values-list state) :to-equal expected-vals))))))
(it-sequential "prim-float-rounding-behavior ffloor"
  (destructuring-bind (make-inst expected-q expected-vals) (list #'cl-cc/vm:make-vm-ffloor 2.0 '(2.0 1))
    (%with-binary-vm-state
   7 3
   (lambda (state)
     (exec1 (funcall make-inst :dst 0 :lhs 1 :rhs 2) state)
     (expect (= expected-q (cl-cc/vm:vm-reg-get state 0)) :to-be-truthy)
     (expect (cl-cc/vm:vm-values-list state) :to-equal expected-vals)))))
(it-sequential "prim-float-rounding-behavior fceiling"
  (destructuring-bind (make-inst expected-q expected-vals) (list #'cl-cc/vm:make-vm-fceiling 3.0 '(3.0 -2))
    (%with-binary-vm-state
   7 3
   (lambda (state)
     (exec1 (funcall make-inst :dst 0 :lhs 1 :rhs 2) state)
     (expect (= expected-q (cl-cc/vm:vm-reg-get state 0)) :to-be-truthy)
     (expect (cl-cc/vm:vm-values-list state) :to-equal expected-vals)))))
(it-sequential "prim-float-rounding-behavior ftruncate"
  (destructuring-bind (make-inst expected-q expected-vals) (list #'cl-cc/vm:make-vm-ftruncate 2.0 '(2.0 1))
    (%with-binary-vm-state
   7 3
   (lambda (state)
     (exec1 (funcall make-inst :dst 0 :lhs 1 :rhs 2) state)
     (expect (= expected-q (cl-cc/vm:vm-reg-get state 0)) :to-be-truthy)
     (expect (cl-cc/vm:vm-values-list state) :to-equal expected-vals)))))
(it-sequential "prim-float-rounding-behavior fround"
  (destructuring-bind (make-inst expected-q expected-vals) (list #'cl-cc/vm:make-vm-fround 2.0 '(2.0 1))
    (%with-binary-vm-state
   7 3
   (lambda (state)
     (exec1 (funcall make-inst :dst 0 :lhs 1 :rhs 2) state)
     (expect (= expected-q (cl-cc/vm:vm-reg-get state 0)) :to-be-truthy)
     (expect (cl-cc/vm:vm-values-list state) :to-equal expected-vals)))))
(it-sequential "prim-float-rounding-negative ffloor-neg-denom"
  (destructuring-bind (make-inst expected-q expected-vals) (list #'cl-cc/vm:make-vm-ffloor -3.0 '(-3.0  2))
    (%with-binary-vm-state
   -7 3
   (lambda (state)
     (exec1 (funcall make-inst :dst 0 :lhs 1 :rhs 2) state)
     (expect (= expected-q (cl-cc/vm:vm-reg-get state 0)) :to-be-truthy)
     (expect (cl-cc/vm:vm-values-list state) :to-equal expected-vals)))))
(it-sequential "prim-float-rounding-negative fceiling-neg-denom"
  (destructuring-bind (make-inst expected-q expected-vals) (list #'cl-cc/vm:make-vm-fceiling -2.0 '(-2.0 -1))
    (%with-binary-vm-state
   -7 3
   (lambda (state)
     (exec1 (funcall make-inst :dst 0 :lhs 1 :rhs 2) state)
     (expect (= expected-q (cl-cc/vm:vm-reg-get state 0)) :to-be-truthy)
     (expect (cl-cc/vm:vm-values-list state) :to-equal expected-vals)))))
(it-sequential "prim-float-rounding-negative ftruncate-neg-denom"
  (destructuring-bind (make-inst expected-q expected-vals) (list #'cl-cc/vm:make-vm-ftruncate -2.0 '(-2.0 -1))
    (%with-binary-vm-state
   -7 3
   (lambda (state)
     (exec1 (funcall make-inst :dst 0 :lhs 1 :rhs 2) state)
     (expect (= expected-q (cl-cc/vm:vm-reg-get state 0)) :to-be-truthy)
     (expect (cl-cc/vm:vm-values-list state) :to-equal expected-vals)))))
(it-sequential "prim-float-rounding-negative fround-neg-denom"
  (destructuring-bind (make-inst expected-q expected-vals) (list #'cl-cc/vm:make-vm-fround -2.0 '(-2.0 -1))
    (%with-binary-vm-state
   -7 3
   (lambda (state)
     (exec1 (funcall make-inst :dst 0 :lhs 1 :rhs 2) state)
     (expect (= expected-q (cl-cc/vm:vm-reg-get state 0)) :to-be-truthy)
     (expect (cl-cc/vm:vm-values-list state) :to-equal expected-vals)))))
(it-sequential "prim-truncate-bignum-uses-vm-bignum-division-path"
  (let ((called nil))
    (with-replaced-function (cl-cc/vm/numbers::vm-bignum-burnikel-ziegler-divide
                             (lambda (lhs rhs &key base block-size rounding)
                               (declare (ignore lhs rhs base block-size rounding))
                               (setf called t)
                               (values 9 1)))
      (%with-binary-vm-state
       (+ most-positive-fixnum 10)
       3
       (lambda (state)
         (exec1 (cl-cc/vm:make-vm-truncate :dst 0 :lhs 1 :rhs 2) state)
         (expect called :to-be-truthy)
         (expect (= 9 (cl-cc/vm:vm-reg-get state 0)) :to-be-truthy)
         (expect (cl-cc/vm:vm-values-list state) :to-equal '(9 1)))))))
(it-sequential "prim-div-bignum-uses-vm-bignum-division-path"
  (let ((called nil))
    (with-replaced-function (cl-cc/vm/numbers::vm-bignum-burnikel-ziegler-divide
                             (lambda (lhs rhs &key base block-size rounding)
                               (declare (ignore lhs rhs base block-size rounding))
                               (setf called t)
                               (values 8 2)))
      (%with-binary-vm-state
       (+ most-positive-fixnum 10)
       3
       (lambda (state)
         (exec1 (cl-cc/vm:make-vm-div :dst 0 :lhs 1 :rhs 2) state)
         (expect called :to-be-truthy)
         (expect (= 8 (cl-cc/vm:vm-reg-get state 0)) :to-be-truthy))))))
(it-sequential "prim-div-bignum-matches-floor-for-negative-operands"
  (let* ((lhs (- (+ most-positive-fixnum 10)))
         (rhs 3))
    (%with-binary-vm-state
     lhs rhs
     (lambda (state)
       (exec1 (cl-cc/vm:make-vm-div :dst 0 :lhs 1 :rhs 2) state)
       (expect (= (floor lhs rhs) (cl-cc/vm:vm-reg-get state 0)) :to-be-truthy)))))
(it-sequential "prim-mul-bignum-uses-vm-bignum-multiply-path"
  (let ((called nil))
    (with-replaced-function (cl-cc/vm/numbers::vm-bignum-multiply-integers
                             (lambda (lhs rhs &key base threshold)
                               (declare (ignore lhs rhs base threshold))
                               (setf called t)
                               77))
      (%with-binary-vm-state
       (+ most-positive-fixnum 10)
       3
       (lambda (state)
         (exec1 (cl-cc/vm:make-vm-mul :dst 0 :lhs 1 :rhs 2) state)
         (expect called :to-be-truthy)
         (expect (= 77 (cl-cc/vm:vm-reg-get state 0)) :to-be-truthy))))))
)

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 4: Unary Arithmetic
;;; ═══════════════════════════════════════════════════════════════════════════

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 5: Binary Arithmetic
;;; ═══════════════════════════════════════════════════════════════════════════

;;; Division by zero errors

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 6: Multiple-Value Division (truncate/floor/ceiling/round)
;;; ═══════════════════════════════════════════════════════════════════════════

;;; ═══════════════════════════════════════════════════════════════════════════
;;; FR-885: Float Rounding (ffloor/fceiling/ftruncate/fround)
;;; ═══════════════════════════════════════════════════════════════════════════

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 7: Boolean Operations
;;; ═══════════════════════════════════════════════════════════════════════════

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 8: Dynamic / TypeRep Runtime API
;;; ═══════════════════════════════════════════════════════════════════════════

