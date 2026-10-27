;;;; packages/vm/tests/fr-668-669-754-757-tests.lisp

(in-package #:cl-cc/vm/numbers/test)


(defun %fr-vm-unary (ctor value)
  (let ((state (make-test-vm)))
    (cl-cc/vm:vm-reg-set state 1 value)
    (exec1 (funcall ctor :dst 0 :src 1) state)
    (values (cl-cc/vm:vm-reg-get state 0)
            (cl-cc/vm:vm-values-list state)
            state)))

(defun %fr-vm-binary (ctor lhs rhs)
  (let ((state (make-test-vm)))
    (cl-cc/vm:vm-reg-set state 1 lhs)
    (cl-cc/vm:vm-reg-set state 2 rhs)
    (exec1 (funcall ctor :dst 0 :lhs 1 :rhs 2) state)
    (values (cl-cc/vm:vm-reg-get state 0)
            (cl-cc/vm:vm-values-list state)
            state)))

(defun register-vm-regression-tests ()
  (it-sequential "fr-668-parse-float-returns-float-and-position"
    (multiple-value-bind (value values-list)
        (%fr-vm-unary #'cl-cc/vm:make-vm-parse-float " -12.5e1 rest")
      (expect (= -125.0d0 value) :to-be-truthy)
      (expect (= -125.0d0 (first values-list)) :to-be-truthy)
      (expect (= 8 (second values-list)) :to-be-truthy)))
  (it-sequential "fr-668-parse-integer-records-position"
    (multiple-value-bind (value values-list)
        (%fr-vm-unary #'cl-cc/vm:make-vm-parse-integer "12345")
      (expect (= 12345 value) :to-be-truthy)
      (expect values-list :to-equal '(12345 5))))
  (it-sequential "fr-669-float-decode-functions-match-cl-values"
    (let ((float 6.5d0))
      (multiple-value-bind (expected-sig expected-exp expected-sign) (decode-float float)
        (multiple-value-bind (actual values-list)
            (%fr-vm-unary #'cl-cc/vm:make-vm-decode-float float)
          (expect (= expected-sig actual) :to-be-truthy)
          (expect values-list :to-equal (list expected-sig expected-exp expected-sign))))
      (multiple-value-bind (expected-sig expected-exp expected-sign) (integer-decode-float float)
        (multiple-value-bind (actual values-list)
            (%fr-vm-unary #'cl-cc/vm:make-vm-integer-decode-float float)
          (expect (= expected-sig actual) :to-be-truthy)
          (expect values-list :to-equal (list expected-sig expected-exp expected-sign))))))
  (it-sequential "fr-669-float-inspection-and-scale radix"
    (destructuring-bind (ctor input expected) (list #'cl-cc/vm:make-vm-float-radix 1.0d0 2)
      (multiple-value-bind (actual) (%fr-vm-unary ctor input)
        (expect (= expected actual) :to-be-truthy))))
  (it-sequential "fr-669-float-inspection-and-scale digits"
    (destructuring-bind (ctor input expected) (list #'cl-cc/vm:make-vm-float-digits 1.0d0 53)
      (multiple-value-bind (actual) (%fr-vm-unary ctor input)
        (expect (= expected actual) :to-be-truthy))))
  (it-sequential "fr-669-float-inspection-and-scale sign"
    (destructuring-bind (ctor input expected) (list #'cl-cc/vm:make-vm-float-sign -2.0d0 -1.0d0)
      (multiple-value-bind (actual) (%fr-vm-unary ctor input)
        (expect (= expected actual) :to-be-truthy))))
  (it-sequential "fr-669-scale-float-scales-by-binary-exponent"
    (multiple-value-bind (actual) (%fr-vm-binary #'cl-cc/vm:make-vm-scale-float 1.5d0 3)
      (expect (= 12.0d0 actual) :to-be-truthy))))

