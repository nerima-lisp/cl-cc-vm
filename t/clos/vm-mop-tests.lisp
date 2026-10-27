(in-package #:cl-cc/vm/clos/test)

(defun make-mop-test-class (name &key superclasses direct-slots slots initargs initforms
                                   slot-types cl-cc/vm/clos:class-slots cpl direct-subclasses)
  "Build a VM class descriptor for MOP tests."
  (let ((class (make-hash-table :test #'eq)))
    (setf (gethash :__name__ class) name
          (gethash :__superclasses__ class) superclasses
          (gethash :__direct-slots__ class) direct-slots
          (gethash :__slots__ class) (or slots direct-slots)
          (gethash :__initargs__ class) initargs
          (gethash :__initforms__ class) initforms
          (gethash :__slot-types__ class) slot-types
          (gethash :__class-slots__ class) cl-cc/vm/clos:class-slots
          (gethash :__slot-locations__ class)
          (loop for slot in (or slots direct-slots)
                for index from 0
                collect (cons slot index))
          (gethash :__cpl__ class) (or cpl (list name t))
          (gethash :__direct-subclasses__ class) direct-subclasses)
    class))

(defun make-mop-test-method (specializers &key qualifiers function)
  "Build a VM method descriptor for MOP tests."
  (let ((method (make-hash-table :test #'eq)))
    (setf (gethash :specializers method) specializers
          (gethash :specializer method)
          (if (= (cl-cc/vm/data:length specializers) 1) (first specializers) specializers)
          (gethash :qualifiers method) qualifiers
          (gethash :function method) (or function #'identity))
    method))



(defun register-vm-mop-tests ()
  (it-sequential "vm-mop-loads-with-vm"
                 :timeout
                 5
                 (expect (asdf:find-system :cl-cc-vm nil) :to-be-truthy))

  (it-sequential "fr-930-satiating-gfs-p-introspection"
                 :timeout
                 10
                 (let ((gf (cl-cc/vm/clos::ensure-generic-function 'mop-satiated :lambda-list '(x))))
                   (expect (cl-cc/vm/clos:satiating-gfs-p) :to-be-falsy)
                   (expect (cl-cc/vm/clos:satiating-gfs-p gf) :to-be-falsy)
                   (setf (gethash :__satiated__ gf) t)
                   (expect (cl-cc/vm/clos:satiating-gfs-p gf) :to-be-truthy)
                   (setf (gethash :__satiated__ gf) nil)
                   (expect (cl-cc/vm/clos:satiating-gfs-p gf) :to-be-falsy)))

  (it-sequential "fr-930-compute-applicable-methods"
                 :timeout
                 10
                 (let* ((gf (cl-cc/vm/clos::ensure-generic-function 'mop-applicable :lambda-list '(x)))
                        (fallback (make-mop-test-method '(t)))
                        (integer-method (make-mop-test-method '(integer)))
                        (string-method (make-mop-test-method '(string))))
                   (cl-cc/vm/clos::add-method gf fallback)
                   (cl-cc/vm/clos::add-method gf integer-method)
                   (cl-cc/vm/clos::add-method gf string-method)
                   (let ((methods (cl-cc/vm/clos::compute-applicable-methods gf (list 42))))
                     (expect (member integer-method methods) :to-be-truthy)
                     (expect (member fallback methods) :to-be-truthy)
                     (expect (member string-method methods) :to-be-falsy))))

  (it-sequential "fr-931-compute-applicable-methods-using-classes"
                 :timeout
                 10
                 (let* ((gf (cl-cc/vm/clos::ensure-generic-function 'mop-camuc :lambda-list '(x)))
                        (fallback (make-mop-test-method '(t)))
                        (integer-method (make-mop-test-method '(integer))))
                   (cl-cc/vm/clos::add-method gf fallback)
                   (cl-cc/vm/clos::add-method gf integer-method)
                   (multiple-value-bind (methods definitivep)
                       (cl-cc/vm/clos::compute-applicable-methods-using-classes gf '(integer))
                     (expect definitivep :to-be-truthy)
                     (expect (member integer-method methods) :to-be-truthy)
                     (expect (member fallback methods) :to-be-truthy))))

  (it-sequential "fr-931-find-add-remove-method"
                 :timeout
                 10
                 (let* ((gf (cl-cc/vm/clos::ensure-generic-function 'mop-dynamic :lambda-list '(x)))
                        (method (make-mop-test-method '(integer))))
                   (cl-cc/vm/clos::add-method gf method)
                   (expect (cl-cc/vm/clos::find-method gf nil '(integer) nil) :to-be method)
                   (cl-cc/vm/clos::remove-method gf method)
                   (expect (cl-cc/vm/clos::find-method gf nil '(integer) nil) :to-be-null)))
  (it-sequential "fr-930-generic-function-method-introspection"
                 :timeout
                 10
                 (let* ((gf (cl-cc/vm/clos::ensure-generic-function 'mop-gf :lambda-list '(x)))
                        (primary (make-mop-test-method '(integer)))
                        (before (make-mop-test-method '(integer) :qualifiers '(:before))))
                   (cl-cc/vm/clos::add-method gf primary)
                   (cl-cc/vm/clos::add-method gf before)
                   (expect (member primary (cl-cc/vm/clos:generic-function-methods gf)) :to-be-truthy)
                   (expect (member before (cl-cc/vm/clos:generic-function-methods gf)) :to-be-truthy)
                   (expect (cl-cc/vm/clos::method-specializers primary) :to-equal '(integer))
                   (expect (cl-cc/vm/clos::method-qualifiers before) :to-equal '(:before))
                   (expect (cl-cc/vm/clos:generic-function-method-combination gf) :to-be 'standard)))

  )
