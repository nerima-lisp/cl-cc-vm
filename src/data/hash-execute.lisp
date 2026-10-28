;;;; hash-execute.lisp - VM Hash Table Instruction Execution Methods
;;;;
;;;; Depends on hash.lisp being loaded first (instruction struct definitions,
;;;; helper functions, vm-hash-table-get-internal, resolve-hash-test).

(in-package #:cl-cc/vm/data)

(defun %execute-vm-gethash (inst state pc)
  "Execute generic or statically specialized GETHASH instruction INST."
  (let* ((key (vm-reg-get state (vm-hash-key inst)))
         (table-obj (vm-reg-get state (vm-hash-table-reg inst)))
           (table (vm-hash-table-get-internal table-obj))
         (default-val (when (vm-hash-default inst)
                         (vm-reg-get state (vm-hash-default inst)))))
    (when (typep table-obj 'vm-hash-table-object)
      (vm-sweep-weak-hash-table table-obj))
    (if (typep table-obj 'vm-hash-table-object)
        (vm-hash-with-lock-fallback
         table-obj
         (lambda ()
           (multiple-value-bind (value found-p)
               (if default-val
                   (gethash key table default-val)
                   (gethash key table))
             (vm-reg-set state (vm-dst inst) value)
             (when (vm-found-dst inst)
               (vm-reg-set state (vm-found-dst inst) found-p))
             (setf (vm-values-list state) (list value found-p)))))
        (multiple-value-bind (value found-p)
            (if default-val
                (gethash key table default-val)
                (gethash key table))
          (vm-reg-set state (vm-dst inst) value)
           (when (vm-found-dst inst)
             (vm-reg-set state (vm-found-dst inst) found-p))
           (setf (vm-values-list state) (list value found-p))))
     (values (1+ pc) nil nil)))

(defun %execute-vm-specialized-gethash (inst state pc)
  "Execute a GETHASH instruction for a compiler-proven VM hash-table object."
  (let* ((key (vm-reg-get state (vm-hash-key inst)))
         (table-obj (vm-reg-get state (vm-hash-table-reg inst)))
         (table (vm-hash-table-internal table-obj))
         (default-val (when (vm-hash-default inst)
                        (vm-reg-get state (vm-hash-default inst)))))
    (vm-sweep-weak-hash-table table-obj)
    (vm-hash-with-lock-fallback
     table-obj
     (lambda ()
       (multiple-value-bind (value found-p)
           (if default-val
               (gethash key table default-val)
               (gethash key table))
         (vm-reg-set state (vm-dst inst) value)
         (when (vm-found-dst inst)
           (vm-reg-set state (vm-found-dst inst) found-p))
         (setf (vm-values-list state) (list value found-p)))))
    (values (1+ pc) nil nil)))

(define-vm-hash-property-executors)
