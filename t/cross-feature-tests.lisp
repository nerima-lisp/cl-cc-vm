(in-package #:cl-cc-vm/test)

(it-sequential "vm-type-of random-state"
  (destructuring-bind (src expected)
      (list (cl-cc/vm/environment:make-random-state t) 'cl-cc/vm:random-state)
    (let ((s (cl-cc/vm-test-support:make-test-vm)))
      (cl-cc/vm/protocols:vm-reg-set s 1 src)
      (cl-cc/vm-test-support:exec1
       (cl-cc/vm/execution:make-vm-type-of :dst 0 :src 1)
       s)
      (expect (cl-cc/vm/protocols:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-type-of pathname"
  (destructuring-bind (src expected) (list #p"/tmp" 'pathname)
    (let ((s (cl-cc/vm-test-support:make-test-vm)))
      (cl-cc/vm/protocols:vm-reg-set s 1 src)
      (cl-cc/vm-test-support:exec1
       (cl-cc/vm/execution:make-vm-type-of :dst 0 :src 1)
       s)
      (expect (cl-cc/vm/protocols:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-type-of cons"
  (destructuring-bind (src expected) (list '(a) 'cons)
    (let ((s (cl-cc/vm-test-support:make-test-vm)))
      (cl-cc/vm/protocols:vm-reg-set s 1 src)
      (cl-cc/vm-test-support:exec1
       (cl-cc/vm/execution:make-vm-type-of :dst 0 :src 1)
       s)
      (expect (cl-cc/vm/protocols:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-type-of sym"
  (destructuring-bind (src expected) (list 'foo 'symbol)
    (let ((s (cl-cc/vm-test-support:make-test-vm)))
      (cl-cc/vm/protocols:vm-reg-set s 1 src)
      (cl-cc/vm-test-support:exec1
       (cl-cc/vm/execution:make-vm-type-of :dst 0 :src 1)
       s)
      (expect (cl-cc/vm/protocols:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-type-of char"
  (destructuring-bind (src expected) (list #\a 'character)
    (let ((s (cl-cc/vm-test-support:make-test-vm)))
      (cl-cc/vm/protocols:vm-reg-set s 1 src)
      (cl-cc/vm-test-support:exec1
       (cl-cc/vm/execution:make-vm-type-of :dst 0 :src 1)
       s)
      (expect (cl-cc/vm/protocols:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-type-of str"
  (destructuring-bind (src expected) (list "hello" 'string)
    (let ((s (cl-cc/vm-test-support:make-test-vm)))
      (cl-cc/vm/protocols:vm-reg-set s 1 src)
      (cl-cc/vm-test-support:exec1
       (cl-cc/vm/execution:make-vm-type-of :dst 0 :src 1)
       s)
      (expect (cl-cc/vm/protocols:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-type-of float"
  (destructuring-bind (src expected) (list 1.0f0 'single-float)
    (let ((s (cl-cc/vm-test-support:make-test-vm)))
      (cl-cc/vm/protocols:vm-reg-set s 1 src)
      (cl-cc/vm-test-support:exec1
       (cl-cc/vm/execution:make-vm-type-of :dst 0 :src 1)
       s)
      (expect (cl-cc/vm/protocols:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-type-of ratio"
  (destructuring-bind (src expected) (list 1/2 'ratio)
    (let ((s (cl-cc/vm-test-support:make-test-vm)))
      (cl-cc/vm/protocols:vm-reg-set s 1 src)
      (cl-cc/vm-test-support:exec1
       (cl-cc/vm/execution:make-vm-type-of :dst 0 :src 1)
       s)
      (expect (cl-cc/vm/protocols:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-type-of bignum"
  (destructuring-bind (src expected) (list (+ most-positive-fixnum 1) 'bignum)
    (let ((s (cl-cc/vm-test-support:make-test-vm)))
      (cl-cc/vm/protocols:vm-reg-set s 1 src)
      (cl-cc/vm-test-support:exec1
       (cl-cc/vm/execution:make-vm-type-of :dst 0 :src 1)
       s)
      (expect (cl-cc/vm/protocols:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-type-of fixnum"
  (destructuring-bind (src expected) (list 42 'fixnum)
    (let ((s (cl-cc/vm-test-support:make-test-vm)))
      (cl-cc/vm/protocols:vm-reg-set s 1 src)
      (cl-cc/vm-test-support:exec1
       (cl-cc/vm/execution:make-vm-type-of :dst 0 :src 1)
       s)
      (expect (cl-cc/vm/protocols:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-type-of nil"
  (destructuring-bind (src expected) (list nil 'null)
    (let ((s (cl-cc/vm-test-support:make-test-vm)))
      (cl-cc/vm/protocols:vm-reg-set s 1 src)
      (cl-cc/vm-test-support:exec1
       (cl-cc/vm/execution:make-vm-type-of :dst 0 :src 1)
       s)
      (expect (cl-cc/vm/protocols:vm-reg-get s 0) :to-equal expected))))

(it-sequential "prim-type-pred oddp-false"
  (destructuring-bind (ctor src-val expected)
      (list #'cl-cc/vm/numbers:make-vm-oddp 4 0)
    (expect (= expected (cl-cc/vm-test-support:%run-unary-inst ctor src-val))
            :to-be-truthy)))

(it-sequential "prim-type-pred oddp-true"
  (destructuring-bind (ctor src-val expected)
      (list #'cl-cc/vm/numbers:make-vm-oddp 3 1)
    (expect (= expected (cl-cc/vm-test-support:%run-unary-inst ctor src-val))
            :to-be-truthy)))

(it-sequential "prim-type-pred evenp-false"
  (destructuring-bind (ctor src-val expected)
      (list #'cl-cc/vm/numbers:make-vm-evenp 3 0)
    (expect (= expected (cl-cc/vm-test-support:%run-unary-inst ctor src-val))
            :to-be-truthy)))

(it-sequential "prim-type-pred evenp-true"
  (destructuring-bind (ctor src-val expected)
      (list #'cl-cc/vm/numbers:make-vm-evenp 4 1)
    (expect (= expected (cl-cc/vm-test-support:%run-unary-inst ctor src-val))
            :to-be-truthy)))

(it-sequential "prim-type-pred integer-p-false"
  (destructuring-bind (ctor src-val expected)
      (list #'cl-cc/vm/numbers:make-vm-integer-p 3.14 0)
    (expect (= expected (cl-cc/vm-test-support:%run-unary-inst ctor src-val))
            :to-be-truthy)))

(it-sequential "prim-type-pred integer-p-true"
  (destructuring-bind (ctor src-val expected)
      (list #'cl-cc/vm/numbers:make-vm-integer-p 42 1)
    (expect (= expected (cl-cc/vm-test-support:%run-unary-inst ctor src-val))
            :to-be-truthy)))

(it-sequential "prim-type-pred number-p-false"
  (destructuring-bind (ctor src-val expected)
      (list #'cl-cc/vm/numbers:make-vm-number-p "hello" 0)
    (expect (= expected (cl-cc/vm-test-support:%run-unary-inst ctor src-val))
            :to-be-truthy)))

(it-sequential "prim-type-pred number-p-true"
  (destructuring-bind (ctor src-val expected)
      (list #'cl-cc/vm/numbers:make-vm-number-p 42 1)
    (expect (= expected (cl-cc/vm-test-support:%run-unary-inst ctor src-val))
            :to-be-truthy)))
