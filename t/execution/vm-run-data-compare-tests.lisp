(in-package #:cl-cc/vm/execution/test)

(it-sequential "run-vm-basic-ops vector-ref/set"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-load-fixnum+
                      1
                      3
                      nil
                      cl-cc/vm:+op2-load-fixnum+
                      2
                      0
                      nil
                      cl-cc/vm:+op2-load-fixnum+
                      3
                      7
                      nil
                      cl-cc/vm:+op2-make-vector+
                      4
                      1
                      2
                      cl-cc/vm:+op2-vector-set+
                      4
                      2
                      3
                      cl-cc/vm:+op2-vector-ref+
                      0
                      4
                      2
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       7)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (cl-cc/vm/execution::%run-vm-core bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops hash-ref/set"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-load-fixnum+
                      1
                      8
                      nil
                      cl-cc/vm:+op2-make-hash+
                      4
                      1
                      nil
                      cl-cc/vm:+op2-const+
                      2
                      'foo
                      nil
                      cl-cc/vm:+op2-load-fixnum+
                      3
                      77
                      nil
                      cl-cc/vm:+op2-hash-set+
                      4
                      2
                      3
                      cl-cc/vm:+op2-hash-ref+
                      0
                      4
                      2
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       77)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (cl-cc/vm/execution::%run-vm-core bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops global-ref/set"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-load-fixnum+
                      1
                      55
                      nil
                      cl-cc/vm:+op2-set-global+
                      'foo
                      1
                      nil
                      cl-cc/vm:+op2-get-global+
                      0
                      'foo
                      nil
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       55)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (cl-cc/vm/execution::%run-vm-core bytecode s) :to-equal expected))))

(it-sequential "run-vm-immediate-ops add-imm"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-const+
                      1
                      3
                      nil
                      cl-cc/vm:+op2-add-imm2+
                      0
                      1
                      4
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       7)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (= expected (cl-cc/vm/execution::%run-vm-core bytecode s))
              :to-be-truthy))))

(it-sequential "run-vm-immediate-ops cmp-imm"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-const+
                      1
                      7
                      nil
                      cl-cc/vm:+op2-num-gt-imm2+
                      0
                      1
                      5
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       1)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (= expected (cl-cc/vm/execution::%run-vm-core bytecode s))
              :to-be-truthy))))

(it-sequential "run-vm-immediate-ops cmp-imm-false"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-const+
                      1
                      2
                      nil
                      cl-cc/vm:+op2-num-eq-imm2+
                      0
                      1
                      5
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       0)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (= expected (cl-cc/vm/execution::%run-vm-core bytecode s))
              :to-be-truthy))))

(it-sequential "run-vm-generic-compare-ops num-eq-true"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-const+
                      1
                      5
                      nil
                      cl-cc/vm:+op2-const+
                      2
                      5
                      nil
                      cl-cc/vm:+op2-num-eq2+
                      0
                      1
                      2
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       1)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (= expected (cl-cc/vm/execution::%run-vm-core bytecode s))
              :to-be-truthy))))

(it-sequential "run-vm-generic-compare-ops num-lt-true"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-const+
                      1
                      3
                      nil
                      cl-cc/vm:+op2-const+
                      2
                      9
                      nil
                      cl-cc/vm:+op2-num-lt2+
                      0
                      1
                      2
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       1)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (= expected (cl-cc/vm/execution::%run-vm-core bytecode s))
              :to-be-truthy))))

(it-sequential "run-vm-generic-compare-ops num-gt-false"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-const+
                      1
                      3
                      nil
                      cl-cc/vm:+op2-const+
                      2
                      9
                      nil
                      cl-cc/vm:+op2-num-gt2+
                      0
                      1
                      2
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       0)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (= expected (cl-cc/vm/execution::%run-vm-core bytecode s))
              :to-be-truthy))))

(it-sequential "run-vm-generic-compare-ops num-le-true"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-const+
                      1
                      9
                      nil
                      cl-cc/vm:+op2-const+
                      2
                      9
                      nil
                      cl-cc/vm:+op2-num-le2+
                      0
                      1
                      2
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       1)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (= expected (cl-cc/vm/execution::%run-vm-core bytecode s))
              :to-be-truthy))))

(it-sequential "run-vm-generic-compare-ops num-ge-false"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-const+
                      1
                      3
                      nil
                      cl-cc/vm:+op2-const+
                      2
                      9
                      nil
                      cl-cc/vm:+op2-num-ge2+
                      0
                      1
                      2
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       0)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (= expected (cl-cc/vm/execution::%run-vm-core bytecode s))
              :to-be-truthy))))

(it-sequential "run-vm-generic-compare-ops eq-true"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-const+
                      1
                      7
                      nil
                      cl-cc/vm:+op2-move+
                      2
                      1
                      nil
                      cl-cc/vm:+op2-eq+
                      0
                      1
                      2
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       1)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (= expected (cl-cc/vm/execution::%run-vm-core bytecode s))
              :to-be-truthy))))

(it-sequential "run-vm-generic-compare-ops eql-true"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-const+
                      1
                      7
                      nil
                      cl-cc/vm:+op2-const+
                      2
                      7
                      nil
                      cl-cc/vm:+op2-eql+
                      0
                      1
                      2
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       1)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (= expected (cl-cc/vm/execution::%run-vm-core bytecode s))
              :to-be-truthy))))

(it-sequential "run-vm-generic-compare-ops equal-true"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-const+
                      1
                      '(1 2)
                      nil
                      cl-cc/vm:+op2-const+
                      2
                      '(1 2)
                      nil
                      cl-cc/vm:+op2-equal+
                      0
                      1
                      2
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       1)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (= expected (cl-cc/vm/execution::%run-vm-core bytecode s))
              :to-be-truthy))))

