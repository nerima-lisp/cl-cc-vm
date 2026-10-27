;;;; tests/unit/vm/vm-extensions-tests.lisp — VM language extension instruction tests

(in-package #:cl-cc/vm/numbers/test)



(defun %vm-ext-unary (ctor-fn src-val)
  (let ((s (make-test-vm)))
    (cl-cc/vm:vm-reg-set s 1 src-val)
    (exec1 (funcall ctor-fn :dst 0 :src 1) s)
    (cl-cc/vm:vm-reg-get s 0)))

(defun %vm-ext-binary (ctor-fn lhs rhs)
  (let ((s (make-test-vm)))
    (cl-cc/vm:vm-reg-set s 1 lhs)
    (cl-cc/vm:vm-reg-set s 2 rhs)
    (exec1 (funcall ctor-fn :dst 0 :lhs 1 :rhs 2) s)
    (cl-cc/vm:vm-reg-get s 0)))

(defun register-vm-extensions-tests ()
(it-sequential "vm-generic-arith add"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc/vm:make-vm-generic-add 10 3 13)
    (expect (= expected (%vm-ext-binary ctor lhs rhs)) :to-be-truthy)))
(it-sequential "vm-generic-arith sub"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc/vm:make-vm-generic-sub 10 3 7)
    (expect (= expected (%vm-ext-binary ctor lhs rhs)) :to-be-truthy)))
(it-sequential "vm-generic-arith mul"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc/vm:make-vm-generic-mul 10 3 30)
    (expect (= expected (%vm-ext-binary ctor lhs rhs)) :to-be-truthy)))
(it-sequential "vm-generic-arith div"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc/vm:make-vm-generic-div 10 3 3)
    (expect (= expected (%vm-ext-binary ctor lhs rhs)) :to-be-truthy)))
(it-sequential "vm-generic-comparison eq-equal"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc/vm:make-vm-generic-eq "hello" "hello" t)
    (if expected
      (expect (%vm-ext-binary ctor lhs rhs) :to-equal t)
      (expect (%vm-ext-binary ctor lhs rhs) :to-be-null))))
(it-sequential "vm-generic-comparison eq-unequal"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc/vm:make-vm-generic-eq "hello" "world" nil)
    (if expected
      (expect (%vm-ext-binary ctor lhs rhs) :to-equal t)
      (expect (%vm-ext-binary ctor lhs rhs) :to-be-null))))
(it-sequential "vm-generic-comparison lt-true"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc/vm:make-vm-generic-lt 3 5 t)
    (if expected
      (expect (%vm-ext-binary ctor lhs rhs) :to-equal t)
      (expect (%vm-ext-binary ctor lhs rhs) :to-be-null))))
(it-sequential "vm-generic-comparison gt-false"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc/vm:make-vm-generic-gt 3 5 nil)
    (if expected
      (expect (%vm-ext-binary ctor lhs rhs) :to-equal t)
      (expect (%vm-ext-binary ctor lhs rhs) :to-be-null))))
(it-sequential "vm-generic-div-by-zero"
  (let ((s (make-test-vm)))
    (cl-cc/vm:vm-reg-set s 1 10)
    (cl-cc/vm:vm-reg-set s 2 0)
    (signals error (exec1 (cl-cc/vm:make-vm-generic-div :dst 0 :lhs 1 :rhs 2) s))))
)

