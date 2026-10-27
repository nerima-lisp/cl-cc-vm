;;;; packages/vm/tests/fr-668-669-754-757-tests.lisp

(in-package #:cl-cc/vm/data/test)


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

(defun register-fr-668-669-754-757-tests ()
(it-sequential "fr-668-parse-integer-records-position"
  (multiple-value-bind (value values-list)
      (%fr-vm-unary #'cl-cc/vm:make-vm-parse-integer "12345")
    (expect (= 12345 value) :to-be-truthy)
    (expect values-list :to-equal '(12345 5))))
)

