(in-package :cl-cc/vm)

;;; FR-157: Managed cons allocation
;;;
;;; cl-cc-vm intentionally remains independently loadable, so all calls into the
;;; runtime heap are resolved lazily by name.  When the full cl-cc system is
;;; loaded, VM-CONS allocates a real runtime heap object and returns the runtime's
;;; NaN-boxed cons pointer.  Standalone :cl-cc-vm loads keep the previous host
;;; CONS fallback because the :cl-cc/runtime package is not present there.

(defun %vm-runtime-symbol (name &optional errorp)
  "Return the CL-CC/RUNTIME symbol named NAME, or NIL when runtime is absent."
  (let ((package (find-package "CL-CC/RUNTIME")))
    (multiple-value-bind (symbol status)
        (if package
            (find-symbol name package)
            (values nil nil))
      (cond
        ((and status symbol) symbol)
        (errorp (error "cl-cc/vm: runtime symbol ~A is not available" name))
        (t nil)))))

(defun %vm-runtime-function (name &optional errorp)
  "Return the runtime function named NAME, or NIL when unavailable."
  (let ((symbol (%vm-runtime-symbol name errorp)))
    (cond
      ((and symbol (fboundp symbol)) (symbol-function symbol))
      (errorp (error "cl-cc/vm: runtime function ~A is not available" name))
      (t nil))))

(defun %vm-runtime-value (name &optional errorp)
  "Return the runtime special/constant value named NAME."
  (let ((symbol (%vm-runtime-symbol name errorp)))
    (cond
      ((and symbol (boundp symbol)) (symbol-value symbol))
      (errorp (error "cl-cc/vm: runtime value ~A is not available" name))
      (t nil))))

(defun %vm-managed-cons-runtime-available-p ()
  "Return true when all runtime entry points needed for managed conses exist."
  (and (%vm-runtime-function "MAKE-RT-HEAP")
       (%vm-runtime-function "RT-GC-ALLOC")
       (%vm-runtime-function "MAKE-RT-HEADER")
       (%vm-runtime-function "RT-HEAP-SET")
       (%vm-runtime-function "RT-HEAP-REF")
       (%vm-runtime-function "ENCODE-POINTER")
       (%vm-runtime-function "DECODE-POINTER")
       (%vm-runtime-function "VAL-CONS-P")
       (%vm-runtime-value "+RT-TAG-CONS+")
       (%vm-runtime-value "+TAG-CONS+")))

(defun %vm-managed-cons-heap-table (state)
  "Return STATE's VM metadata heap table, or NIL for states without one."
  (when (typep state 'vm-state)
    (vm-state-heap state)))

(defun %vm-managed-cons-heap (state)
  "Return STATE's lazily-created runtime heap for managed VM cons cells."
  (let ((table (%vm-managed-cons-heap-table state)))
    (when (and table (%vm-managed-cons-runtime-available-p))
      (or (gethash :managed-rt-heap table)
          (setf (gethash :managed-rt-heap table)
                (funcall (%vm-runtime-function "MAKE-RT-HEAP" t)))))))

(defun %vm-managed-cons-register-root (state reg value heap)
  "Keep register-held managed cons values visible to runtime GC roots."
  (let ((table (%vm-managed-cons-heap-table state))
        (add-root (%vm-runtime-function "RT-GC-ADD-ROOT")))
    (when (and table add-root)
      (let ((roots (or (gethash :managed-rt-roots table)
                       (setf (gethash :managed-rt-roots table)
                             (make-hash-table :test #'equal)))))
        (let ((root-cell (gethash reg roots)))
          (if root-cell
              (setf (cdr root-cell) value)
              (let ((new-root (cons reg value)))
                (setf (gethash reg roots) new-root)
                (funcall add-root heap new-root))))))))

(defun %vm-managed-cons-pointer-p (value)
  "Return true if VALUE is a runtime NaN-boxed cons pointer."
  (and (integerp value)
       (typep value '(unsigned-byte 64))
       (let ((predicate (%vm-runtime-function "VAL-CONS-P")))
         (and predicate (funcall predicate value)))))

(defvar *vm-managed-cons-sequence-state* nil
  "Dynamically bound VM state for managed cons sequence generic operations.")

(defvar *vm-managed-cons-allocation-enabled* nil
  "When T, vm-cons creates managed cons pointers via %vm-managed-cons-alloc.
When NIL (default), host CL cons cells are used.  Enable for native codegen
paths that require NaN-boxed cons pointers; the VM interpreter stores host
values directly.")

(defun %vm-managed-nil-p (value)
  "Return true if VALUE is host NIL or the runtime NaN-boxed NIL singleton."
  (or (null value)
      (and (integerp value)
           (typep value '(unsigned-byte 64))
           (let ((predicate (%vm-runtime-function "VAL-NIL-P")))
             (and predicate (funcall predicate value))))))

(defun %vm-managed-cons-alloc (state dst-reg car-value cdr-value)
  "Allocate a fresh managed runtime cons cell and return its NaN-boxed pointer.

The runtime heap layout stores a normal object header followed by the two cons
payload words: payload word 0 is CAR, payload word 1 is CDR.  The header is what
lets RT-OBJECT-POINTER-SLOTS trace slots 1 and 2 during minor/major GC."
  (let ((heap (%vm-managed-cons-heap state)))
    (if heap
        (let* ((rt-tag-cons (%vm-runtime-value "+RT-TAG-CONS+" t))
               (ptr-tag-cons (%vm-runtime-value "+TAG-CONS+" t))
               (alloc (%vm-runtime-function "RT-GC-ALLOC" t))
               (make-rt-header (%vm-runtime-function "MAKE-RT-HEADER" t))
               (heap-set (%vm-runtime-function "RT-HEAP-SET" t))
               (encode-pointer (%vm-runtime-function "ENCODE-POINTER" t))
               (object-size 3)
               (addr (funcall alloc heap rt-tag-cons object-size)))
          (funcall heap-set heap addr
                   (funcall make-rt-header object-size rt-tag-cons :gc-bits 0))
          (funcall heap-set heap (+ addr 1) car-value)
          (funcall heap-set heap (+ addr 2) cdr-value)
          (let ((boxed (funcall encode-pointer addr ptr-tag-cons)))
            (%vm-managed-cons-register-root state dst-reg boxed heap)
            boxed))
        (cons car-value cdr-value))))

(defun %vm-managed-cons-slot (state pointer offset)
  "Read managed cons POINTER payload slot OFFSET (1 = car, 2 = cdr)."
  (let ((heap (%vm-managed-cons-heap state)))
    (unless heap
      (error "cl-cc/vm: managed cons pointer has no associated runtime heap"))
    (let* ((decode-pointer (%vm-runtime-function "DECODE-POINTER" t))
           (heap-ref (%vm-runtime-function "RT-HEAP-REF" t))
           (addr (funcall decode-pointer pointer)))
      (funcall heap-ref heap (+ addr offset)))))

(defun %vm-managed-cons-car (state pointer)
  "Return the CAR payload of managed cons POINTER."
  (%vm-managed-cons-slot state pointer 1))

(defun %vm-managed-cons-cdr (state pointer)
  "Return the CDR payload of managed cons POINTER."
  (%vm-managed-cons-slot state pointer 2))

(defun %vm-managed-list-length (state list)
  "Return the length of a proper list that may contain managed cons pointers."
  (unless state
    (error "cl-cc/vm: managed cons sequence operation requires VM state"))
  (loop with count = 0
        for tail = list then (cond
                               ((%vm-managed-cons-pointer-p tail)
                                (%vm-managed-cons-cdr state tail))
                               ((consp tail)
                                (cdr tail))
                               (t tail))
        do (cond
             ((%vm-managed-nil-p tail)
              (return count))
             ((or (%vm-managed-cons-pointer-p tail) (consp tail))
              (incf count))
             (t
              (error 'type-error :datum tail :expected-type 'list)))))

(defun %vm-managed-list-elt (state list index)
  "Return the INDEXth element of a list that may contain managed cons pointers."
  (check-type index (integer 0 *))
  (unless state
    (error "cl-cc/vm: managed cons sequence operation requires VM state"))
  (loop for tail = list then (cond
                               ((%vm-managed-cons-pointer-p tail)
                                (%vm-managed-cons-cdr state tail))
                               ((consp tail)
                                (cdr tail))
                               (t tail))
        for i from 0
        do (cond
             ((%vm-managed-nil-p tail)
              (return nil))
             ((= i index)
              (return (if (%vm-managed-cons-pointer-p tail)
                          (%vm-managed-cons-car state tail)
                          (car tail))))
             ((not (or (%vm-managed-cons-pointer-p tail) (consp tail)))
              (error 'type-error :datum tail :expected-type 'list)))))

(defun %vm-managed-list-tail (state list index)
  "Return the INDEXth CDR of a list that may contain managed cons pointers."
  (check-type index (integer 0 *))
  (unless state
    (error "cl-cc/vm: managed cons sequence operation requires VM state"))
  (loop for tail = list then (cond
                               ((%vm-managed-cons-pointer-p tail)
                                (%vm-managed-cons-cdr state tail))
                               ((consp tail)
                                (cdr tail))
                               (t tail))
        for i from 0
        do (cond
             ((= i index)
              (return (if (%vm-managed-nil-p tail) nil tail)))
             ((%vm-managed-nil-p tail)
              (return nil))
             ((not (or (%vm-managed-cons-pointer-p tail) (consp tail)))
              (error 'type-error :datum tail :expected-type 'list)))))

(defun %vm-managed-member (state item list)
  "Return the first tail whose CAR is EQL to ITEM for a managed/host mixed list."
  (unless state
    (error "cl-cc/vm: managed cons sequence operation requires VM state"))
  (loop for tail = list then (cond
                               ((%vm-managed-cons-pointer-p tail)
                                (%vm-managed-cons-cdr state tail))
                               ((consp tail)
                                (cdr tail))
                               (t tail))
        do (cond
             ((%vm-managed-nil-p tail)
              (return nil))
             ((%vm-managed-cons-pointer-p tail)
              (when (eql item (%vm-managed-cons-car state tail))
                (return tail)))
             ((consp tail)
              (when (eql item (car tail))
                (return tail)))
             (t
              (error 'type-error :datum tail :expected-type 'list)))))

(defun %vm-managed-tree-materialize (state value)
  "Copy a managed/host mixed cons tree into ordinary host cons cells."
  (cond
    ((%vm-managed-nil-p value) nil)
    ((%vm-managed-cons-pointer-p value)
     (cons (%vm-managed-tree-materialize state (%vm-managed-cons-car state value))
           (%vm-managed-tree-materialize state (%vm-managed-cons-cdr state value))))
    ((consp value)
     (cons (%vm-managed-tree-materialize state (car value))
           (%vm-managed-tree-materialize state (cdr value))))
    (t value)))

(defun %vm-managed-cons-materialize (state value)
  "Materialize VALUE when it is a managed cons pointer; otherwise return it."
  (if (%vm-managed-cons-pointer-p value)
      (%vm-managed-tree-materialize state value)
      value))

(defun %vm-list-structure-materialize (state value)
  "Materialize VALUE for list operations, unwrapping managed cons and COW wrappers."
  (if (%vm-managed-cons-pointer-p value)
      (%vm-managed-tree-materialize state value)
      (%vm-cow-list-materialize value)))

(defmethod vm-sequence-length ((sequence integer))
  (if (%vm-managed-cons-pointer-p sequence)
      (%vm-managed-list-length *vm-managed-cons-sequence-state* sequence)
      (error 'type-error :datum sequence :expected-type 'sequence)))

(defmethod vm-sequence-elt ((sequence integer) index)
  (if (%vm-managed-cons-pointer-p sequence)
      (%vm-managed-list-elt *vm-managed-cons-sequence-state* sequence index)
      (error 'type-error :datum sequence :expected-type 'sequence)))

(defun %vm-managed-cons-set-slot (state pointer offset value)
  "Write VALUE into managed cons POINTER payload slot OFFSET."
  (let ((heap (%vm-managed-cons-heap state)))
    (unless heap
      (error "cl-cc/vm: managed cons pointer has no associated runtime heap"))
    (let* ((decode-pointer (%vm-runtime-function "DECODE-POINTER" t))
           (write-barrier (%vm-runtime-function "RT-GC-WRITE-BARRIER" t))
           (addr (funcall decode-pointer pointer)))
      (funcall write-barrier heap addr offset value)
      pointer)))
