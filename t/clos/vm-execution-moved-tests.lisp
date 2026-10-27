(in-package #:cl-cc/vm/clos/test)

(defun register-vm-execution-moved-tests ()
  (it-sequential "vm-classify-arg-primitive integer"
                   (destructuring-bind (value expected-class) (list 42 'integer)
                     (expect (cl-cc/vm/clos:vm-classify-arg value nil) :to-be expected-class)))

  (it-sequential "vm-classify-arg-primitive string"
                   (destructuring-bind (value expected-class) (list "hello" 'string)
                     (expect (cl-cc/vm/clos:vm-classify-arg value nil) :to-be expected-class)))

  (it-sequential "vm-classify-arg-primitive symbol"
                   (destructuring-bind (value expected-class) (list 'foo 'symbol)
                     (expect (cl-cc/vm/clos:vm-classify-arg value nil) :to-be expected-class)))

  (it-sequential "vm-classify-arg-hash-table-no-class"
                   (let ((ht (make-hash-table :test #'eq)))
                     (expect (cl-cc/vm/clos:vm-classify-arg ht nil) :to-be t)))

  (it-sequential "vm-classify-arg-hash-table-with-class"
                   (let* ((class-ht (make-hash-table :test #'eq))
                          (obj-ht   (make-hash-table :test #'eq)))
                     (setf (gethash :__name__ class-ht) 'my-class)
                     (setf (gethash :__class__ obj-ht) class-ht)
                     (expect (cl-cc/vm/clos:vm-classify-arg obj-ht nil) :to-be 'my-class)))

  (it-sequential "vm-generic-function-p plain-hash-table"
                   (destructuring-bind (value verify) (list (make-hash-table :test #'eq) (lambda (value)
                                                                                           (expect (cl-cc/vm/execution:vm-generic-function-p value) :to-be-falsy)))
                     (funcall verify value)))

  (it-sequential "vm-generic-function-p integer"
                   (destructuring-bind (value verify) (list 99 (lambda (value)
                                                                 (expect (cl-cc/vm/execution:vm-generic-function-p value) :to-be-falsy)))
                     (funcall verify value)))

  (it-sequential "vm-generic-function-p hash-with-methods"
                   (destructuring-bind (value verify) (list (let ((ht (make-hash-table :test #'eq)))
                                                              (setf (gethash :__generic__ ht) t (gethash :__methods__ ht) (make-hash-table :test #'equal))
                                                              ht) (lambda (value)
                                                                    (expect (cl-cc/vm/execution:vm-generic-function-p value) :to-be-truthy)))
                     (funcall verify value)))

  (it-sequential "vm-execute-vm-classify-arg integer"
                   (destructuring-bind (arg expected) (list 42 'integer)
                     (let ((s (make-test-vm)))
                       (expect (cl-cc/vm/clos:vm-classify-arg arg s) :to-be expected))))

  (it-sequential "vm-execute-vm-classify-arg string"
                   (destructuring-bind (arg expected) (list "hello" 'string)
                     (let ((s (make-test-vm)))
                       (expect (cl-cc/vm/clos:vm-classify-arg arg s) :to-be expected))))

  (it-sequential "vm-execute-vm-classify-arg symbol"
                   (destructuring-bind (arg expected) (list 'foo 'symbol)
                     (let ((s (make-test-vm)))
                       (expect (cl-cc/vm/clos:vm-classify-arg arg s) :to-be expected))))

  (it-sequential "vm-execute-vm-classify-arg list"
                   (destructuring-bind (arg expected) (list '(a b c) 'cons)
                     (let ((s (make-test-vm)))
                       (expect (cl-cc/vm/clos:vm-classify-arg arg s) :to-be expected))))

  (it-sequential "vm-execute-vm-classify-arg float"
                   (destructuring-bind (arg expected) (list 3.14 'float)
                     (let ((s (make-test-vm)))
                       (expect (cl-cc/vm/clos:vm-classify-arg arg s) :to-be expected))))

  (it-sequential "vm-execute-vm-classify-arg hash-table"
                   (destructuring-bind (arg expected) (list (make-hash-table) 't)
                     (let ((s (make-test-vm)))
                       (expect (cl-cc/vm/clos:vm-classify-arg arg s) :to-be expected))))

  (it-sequential "fr-function-exists fr-888-allocate-fast"
                   (destructuring-bind (sym) (list 'cl-cc/vm/clos::allocate-instance-vector)
                     (expect (fboundp sym) :to-be-truthy)))

  (it-sequential "fr-function-exists fr-889-initargs-cache"
                   (destructuring-bind (sym) (list 'cl-cc/vm/clos::class-slot-vector-index)
                     (expect (fboundp sym) :to-be-truthy)))

  (it-sequential "fr-function-exists fr-930-mop"
                   (destructuring-bind (sym) (list 'cl-cc/vm/clos:class-slots)
                     (expect (fboundp sym) :to-be-truthy)))

  (it-sequential "fr-function-exists fr-931-camuc"
                   (destructuring-bind (sym) (list 'cl-cc/vm/clos::compute-applicable-methods-using-classes)
                     (expect (fboundp sym) :to-be-truthy)))
)
