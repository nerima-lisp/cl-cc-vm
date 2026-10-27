(in-package #:cl-cc/vm/execution/test)

(it-sequential "run-vm-basic-ops mod"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-const+
                      1
                      22
                      nil
                      cl-cc/vm:+op2-const+
                      2
                      5
                      nil
                      cl-cc/vm:+op2-mod+
                      0
                      1
                      2
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       2)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (cl-cc/vm/execution::%run-vm-core bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops jump"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-jump+
                      8
                      nil
                      nil
                      cl-cc/vm:+op2-const+
                      0
                      1
                      nil
                      cl-cc/vm:+op2-const+
                      0
                      2
                      nil
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       2)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (cl-cc/vm/execution::%run-vm-core bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops jump-if-nil"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-load-nil+
                      1
                      nil
                      nil
                      cl-cc/vm:+op2-jump-if-nil+
                      1
                      8
                      nil
                      cl-cc/vm:+op2-const+
                      0
                      1
                      nil
                      cl-cc/vm:+op2-const+
                      0
                      2
                      nil
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       2)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (cl-cc/vm/execution::%run-vm-core bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops jump-if-true"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-load-true+
                      1
                      nil
                      nil
                      cl-cc/vm:+op2-jump-if-true+
                      1
                      8
                      nil
                      cl-cc/vm:+op2-const+
                      0
                      1
                      nil
                      cl-cc/vm:+op2-const+
                      0
                      2
                      nil
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       2)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (cl-cc/vm/execution::%run-vm-core bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops values/recv-values"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-load-fixnum+
                      0
                      10
                      nil
                      cl-cc/vm:+op2-load-fixnum+
                      1
                      20
                      nil
                      cl-cc/vm:+op2-values+
                      2
                      nil
                      nil
                      cl-cc/vm:+op2-load-nil+
                      0
                      nil
                      nil
                      cl-cc/vm:+op2-load-nil+
                      1
                      nil
                      nil
                      cl-cc/vm:+op2-recv-values+
                      2
                      nil
                      nil
                      cl-cc/vm:+op2-halt2+
                      1
                      nil
                      nil)
       20)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (cl-cc/vm/execution::%run-vm-core bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops return"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-load-fixnum+
                      1
                      33
                      nil
                      cl-cc/vm:+op2-return+
                      1
                      nil
                      nil)
       33)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (cl-cc/vm/execution::%run-vm-core bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops return-nil"
  (destructuring-bind (bytecode expected)
      (list (make-bytecode cl-cc/vm:+op2-return-nil+ 0 nil nil) nil)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (cl-cc/vm/execution::%run-vm-core bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops fixnump"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-load-fixnum+
                      1
                      123
                      nil
                      cl-cc/vm:+op2-fixnump+
                      0
                      1
                      nil
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       1)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (cl-cc/vm/execution::%run-vm-core bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops consp"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-const+
                      1
                      '(a . b)
                      nil
                      cl-cc/vm:+op2-consp+
                      0
                      1
                      nil
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       1)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (cl-cc/vm/execution::%run-vm-core bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops symbolp"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-const+
                      1
                      'foo
                      nil
                      cl-cc/vm:+op2-symbolp+
                      0
                      1
                      nil
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       1)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (cl-cc/vm/execution::%run-vm-core bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops functionp"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-const+
                      1
                      #'car
                      nil
                      cl-cc/vm:+op2-functionp+
                      0
                      1
                      nil
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       1)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (cl-cc/vm/execution::%run-vm-core bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops stringp"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-const+
                      1
                      "hi"
                      nil
                      cl-cc/vm:+op2-stringp+
                      0
                      1
                      nil
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       1)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (cl-cc/vm/execution::%run-vm-core bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops cons/car"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-const+
                      1
                      'a
                      nil
                      cl-cc/vm:+op2-const+
                      2
                      'b
                      nil
                      cl-cc/vm:+op2-cons+
                      3
                      1
                      2
                      cl-cc/vm:+op2-car+
                      0
                      3
                      nil
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       'a)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (cl-cc/vm/execution::%run-vm-core bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops cdr"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-const+
                      1
                      'a
                      nil
                      cl-cc/vm:+op2-const+
                      2
                      'b
                      nil
                      cl-cc/vm:+op2-cons+
                      3
                      1
                      2
                      cl-cc/vm:+op2-cdr+
                      0
                      3
                      nil
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       'b)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (cl-cc/vm/execution::%run-vm-core bytecode s) :to-equal expected))))
