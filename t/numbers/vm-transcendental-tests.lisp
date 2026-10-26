;;;; tests/unit/vm/vm-transcendental-tests.lisp — VM transcendental instruction tests

(in-package #:cl-cc/vm/numbers/test)



(defun %vm-trans-unary (ctor-fn src-val)
  (let ((s (make-test-vm)))
    (cl-cc/vm:vm-reg-set s 1 src-val)
    (exec1 (funcall ctor-fn :dst 0 :src 1) s)
    (cl-cc/vm:vm-reg-get s 0)))

(defun %vm-trans-binary (ctor-fn lhs rhs)
  (let ((s (make-test-vm)))
    (cl-cc/vm:vm-reg-set s 1 lhs)
    (cl-cc/vm:vm-reg-set s 2 rhs)
    (exec1 (funcall ctor-fn :dst 0 :lhs 1 :rhs 2) s)
    (cl-cc/vm:vm-reg-get s 0)))

(defun register-vm-transcendental-tests ()
(it-sequential "vm-transcendental-unary sqrt-4"
  (destructuring-bind (ctor input expected) (list #'cl-cc/vm:make-vm-sqrt 4.0 2.0)
    (expect (= expected (%vm-trans-unary ctor input)) :to-be-truthy)))
(it-sequential "vm-transcendental-unary exp-0"
  (destructuring-bind (ctor input expected) (list #'cl-cc/vm:make-vm-exp-inst 0.0 1.0)
    (expect (= expected (%vm-trans-unary ctor input)) :to-be-truthy)))
(it-sequential "vm-transcendental-unary log-1"
  (destructuring-bind (ctor input expected) (list #'cl-cc/vm:make-vm-log-inst 1.0 0.0)
    (expect (= expected (%vm-trans-unary ctor input)) :to-be-truthy)))
(it-sequential "vm-transcendental-unary sin-0"
  (destructuring-bind (ctor input expected) (list #'cl-cc/vm:make-vm-sin-inst 0.0 0.0)
    (expect (= expected (%vm-trans-unary ctor input)) :to-be-truthy)))
(it-sequential "vm-transcendental-unary cos-0"
  (destructuring-bind (ctor input expected) (list #'cl-cc/vm:make-vm-cos-inst 0.0 1.0)
    (expect (= expected (%vm-trans-unary ctor input)) :to-be-truthy)))
(it-sequential "vm-transcendental-unary tan-0"
  (destructuring-bind (ctor input expected) (list #'cl-cc/vm:make-vm-tan-inst 0.0 0.0)
    (expect (= expected (%vm-trans-unary ctor input)) :to-be-truthy)))
(it-sequential "vm-transcendental-unary sinh-0"
  (destructuring-bind (ctor input expected) (list #'cl-cc/vm:make-vm-sinh-inst 0.0 0.0)
    (expect (= expected (%vm-trans-unary ctor input)) :to-be-truthy)))
(it-sequential "vm-transcendental-unary cosh-0"
  (destructuring-bind (ctor input expected) (list #'cl-cc/vm:make-vm-cosh-inst 0.0 1.0)
    (expect (= expected (%vm-trans-unary ctor input)) :to-be-truthy)))
(it-sequential "vm-transcendental-unary tanh-0"
  (destructuring-bind (ctor input expected) (list #'cl-cc/vm:make-vm-tanh-inst 0.0 0.0)
    (expect (= expected (%vm-trans-unary ctor input)) :to-be-truthy)))
(it-sequential "vm-atan2"
  (expect (= 0.0 (%vm-trans-binary #'cl-cc/vm:make-vm-atan2-inst 0.0 1.0)) :to-be-truthy))
)

