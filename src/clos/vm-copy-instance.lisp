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
