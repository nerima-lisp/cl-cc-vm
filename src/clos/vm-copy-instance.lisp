(in-package #:cl-cc/vm/clos)

(defun copy-instance (instance)
  "Return a shallow copy of a CLOS/VM instance, preserving all slot contents."
  (cond
    ((hash-table-p instance)
     (let ((class (gethash :__class__ instance))
           (copy (make-hash-table :test (hash-table-test instance))))
       (unless class
         (error "copy-instance: ~S is not a VM CLOS instance" instance))
       (maphash (lambda (key value)
                  (setf (gethash key copy) value))
                instance)
       copy))
    ((and (vectorp instance)
          (> (cl:length instance) 0)
          (hash-table-p (cl:aref instance 0)))
     (copy-seq instance))
    ((typep instance 'standard-object)
     (let* ((class (class-of instance))
            (copy (allocate-instance class)))
       (dolist (slot (sb-mop:class-slots class) copy)
         (let ((slot-name (sb-mop:slot-definition-name slot)))
           (when (slot-boundp instance slot-name)
             (setf (slot-value copy slot-name)
                   (slot-value instance slot-name)))))))
    (t
     (error "copy-instance: ~S is not a CLOS instance" instance))))

(defmethod vm-copy-instance ((instance t)) (copy-instance instance))

(defun deep-copy (object)
  "Return a recursive copy of mutable objects, preserving graph identity and cycles."
  (let ((seen (make-hash-table :test #'eq)))
    (labels ((copy-object (object)
               (cond
                 ((or (null object)
                      (symbolp object)
                      (numberp object)
                      (characterp object)
                      (functionp object)
                      (typep object 'sb-mop:metaobject)) object)
                 (t
                  (multiple-value-bind (copy foundp) (gethash object seen)
                    (if foundp
                        copy
                        (cond
                          ((consp object)
                           (let ((copy (cons nil nil)))
                             (setf (gethash object seen) copy)
                             (setf (car copy) (copy-object (car object))
                                   (cdr copy) (copy-object (cdr object)))
                             copy))
                          ((hash-table-p object)
                           (let ((copy
                                  (make-hash-table :test
                                                   (hash-table-test object)
                                                   :size
                                                   (hash-table-size object)
                                                   :rehash-size
                                                   (hash-table-rehash-size
                                                    object)
                                                   :rehash-threshold
                                                   (hash-table-rehash-threshold
                                                    object))))
                             (setf (gethash object seen) copy)
                             (maphash
                              (lambda (key value)
                                (setf (gethash (copy-object key) copy) (copy-object
                                                                        value)))
                              object)
                             copy))
                          ((arrayp object)
                           (let* ((dimensions (array-dimensions object))
                                  (copy
                                   (if (and (= (array-rank object) 1)
                                            (array-has-fill-pointer-p object))
                                       (make-array dimensions
                                                   :element-type
                                                   (array-element-type object)
                                                   :adjustable
                                                   (adjustable-array-p object)
                                                   :fill-pointer
                                                   (fill-pointer object))
                                       (make-array dimensions
                                                   :element-type
                                                   (array-element-type object)
                                                   :adjustable
                                                   (adjustable-array-p object)))))
                             (setf (gethash object seen) copy)
                             (dotimes (index (array-total-size object))
                               (setf (row-major-aref copy index) (copy-object
                                                                  (row-major-aref
                                                                   object
                                                                   index))))
                             copy))
                          ((or (typep object 'structure-object)
                               (and (typep object 'standard-object)
                                    (typep (class-of object)
                                           'sb-mop:standard-class)))
                           (let* ((class (class-of object))
                                  (copy (allocate-instance class)))
                             (setf (gethash object seen) copy)
                             (dolist (slot (sb-mop:class-slots class))
                               (when
                                   (and
                                    (eq :instance
                                        (sb-mop:slot-definition-allocation slot))
                                    (slot-boundp object
                                                 (sb-mop:slot-definition-name
                                                  slot)))
                                 (setf (slot-value copy
                                                   (sb-mop:slot-definition-name
                                                    slot)) (copy-object
                                                            (slot-value object
                                                                        (sb-mop:slot-definition-name
                                                                         slot))))))
                             copy))
                          (t object))))))))
      (copy-object object))))
