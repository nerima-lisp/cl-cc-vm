(in-package #:cl-cc/vm/clos/test)

(defstruct deep-copy-test-structure
  left
  right
  self)

(defclass deep-copy-test-object ()
  ((child :initarg :child :accessor deep-copy-test-object-child)))

(defun register-vm-deep-copy-tests ()
  (it-sequential "deep-copy-immediates-return-same-object nil"
                 (destructuring-bind (value) (list nil)
                   (expect (cl-cc/vm/clos:deep-copy value) :to-be value)))
  (it-sequential "deep-copy-immediates-return-same-object integer"
                 (destructuring-bind (value) (list 42)
                   (expect (cl-cc/vm/clos:deep-copy value) :to-be value)))
  (it-sequential "deep-copy-immediates-return-same-object float"
                 (destructuring-bind (value) (list 1.5)
                   (expect (cl-cc/vm/clos:deep-copy value) :to-be value)))
  (it-sequential "deep-copy-immediates-return-same-object symbol"
                 (destructuring-bind (value) (list 'foo)
                   (expect (cl-cc/vm/clos:deep-copy value) :to-be value)))
  (it-sequential "deep-copy-immediates-return-same-object character"
                 (destructuring-bind (value) (list #\a)
                   (expect (cl-cc/vm/clos:deep-copy value) :to-be value)))
  (it-sequential "deep-copy-function-returns-same-object"
                 (let ((fn #'cl:+))
                   (expect (cl-cc/vm/clos:deep-copy fn) :to-be fn)))
  (it-sequential "deep-copy-cycle-in-cons-is-preserved"
                 (let ((pair (cons :head nil)))
                   (setf (cdr pair) pair)
                   (let ((copy (cl-cc/vm/clos:deep-copy pair)))
                     (expect (eq pair copy) :to-be-falsy)
                     (expect (cdr copy) :to-be copy)
                     (expect (car copy) :to-be :head))))
  (it-sequential "deep-copy-hash-table-self-cycle-is-preserved"
                 (let ((original (make-hash-table :test #'eq)))
                   (setf (gethash :self original) original)
                   (let ((copy (cl-cc/vm/clos:deep-copy original)))
                     (expect (eq original copy) :to-be-falsy)
                     (expect (gethash :self copy) :to-be copy))))
  (it-sequential "deep-copy-nested-hash-table-copies-recursively"
                 (let* ((inner (list 10 20))
                        (outer (make-hash-table :test #'equal)))
                   (setf (gethash "key" outer) inner)
                   (let ((copy (cl-cc/vm/clos:deep-copy outer)))
                     (expect (eq outer copy) :to-be-falsy)
                     (expect (eq inner (gethash "key" copy)) :to-be-falsy)
                     (expect (gethash "key" copy) :to-equal inner))))
  (it-sequential "deep-copy-string-produces-fresh-equal-copy"
                 (let* ((original "mutable")
                        (copy (cl-cc/vm/clos:deep-copy original)))
                   (expect copy :to-equal original)
                   (expect (eq original copy) :to-be-falsy)))
  (it-sequential "deep-copy-vector-elements-are-recursively-copied"
                 (let* ((inner (list :a))
                        (original (vector inner 99))
                        (copy (cl-cc/vm/clos:deep-copy original)))
                   (expect (eq original copy) :to-be-falsy)
                   (expect (eq inner (cl:aref copy 0)) :to-be-falsy)
                   (expect (cl:aref copy 0) :to-equal inner)
                   (expect (= 99 (cl:aref copy 1)) :to-be-truthy)))
  (it-sequential "deep-copy-preserves-shared-references"
                 (let* ((shared (list :value))
                        (original (vector shared shared))
                        (copy (cl-cc/vm/clos:deep-copy original)))
                   (expect (eq shared (cl:aref copy 0)) :to-be-falsy)
                   (expect (cl:aref copy 0) :to-be (cl:aref copy 1))))
  (it-sequential "deep-copy-preserves-specialized-array-type"
                 (let* ((original #*10101)
                        (copy (cl-cc/vm/clos:deep-copy original)))
                   (expect (eq original copy) :to-be-falsy)
                   (expect (typep copy 'bit-vector) :to-be-truthy)
                   (expect copy :to-equal original)))
  (it-sequential "deep-copy-standard-object-cycle-is-preserved"
                 (let ((original (make-instance 'deep-copy-test-object)))
                   (setf (deep-copy-test-object-child original) original)
                   (let ((copy (cl-cc/vm/clos:deep-copy original)))
                     (expect (eq original copy) :to-be-falsy)
                     (expect (deep-copy-test-object-child copy) :to-be copy))))
  (it-sequential "deep-copy-structure-cycles-and-shared-slots"
                 (let* ((shared (list :child))
                        (original
                         (make-deep-copy-test-structure :left
                                                        shared
                                                        :right
                                                        shared)))
                   (setf (deep-copy-test-structure-self original) original)
                   (let ((copy (cl-cc/vm/clos:deep-copy original)))
                     (expect (eq original copy) :to-be-falsy)
                     (expect (deep-copy-test-structure-self copy) :to-be copy)
                     (expect (eq shared (deep-copy-test-structure-left copy))
                             :to-be-falsy)
                     (expect (deep-copy-test-structure-left copy)
                             :to-be
                             (deep-copy-test-structure-right copy))
                     (expect (deep-copy-test-structure-left copy)
                             :to-equal
                             shared)))))
