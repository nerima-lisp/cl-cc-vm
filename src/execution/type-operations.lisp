(in-package #:cl-cc/vm/execution)

(define-vm-unary-instruction  vm-cons-p    :cons-p    "Type predicate for cons cells. Returns 1 if SRC is a cons cell, 0 otherwise.")

(define-vm-unary-instruction  vm-null-p    :null-p    "Type predicate for nil. Returns 1 if SRC is nil, 0 otherwise.")

(define-vm-unary-instruction  vm-symbol-p  :symbol-p  "Type predicate for symbols. Returns 1 if SRC is a symbol, 0 otherwise.")

;;; Fast path for immediate symbols: no hash-table lookup needed.
(defmethod execute-instruction :around ((inst vm-symbol-p) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (if (vm-immediate-symbol-p value)
        (progn (vm-reg-set state (vm-dst inst) 1) (values (1+ pc) nil nil))
        (call-next-method))))

(define-vm-unary-instruction  vm-function-p :function-p "Type predicate for functions/closures. Returns 1 if SRC is a function, 0 otherwise.")

;;; General Type Predicate

(define-vm-instruction vm-typep (vm-instruction)
  "General type check. Returns 1 if SRC is of TYPE-NAME, 0 otherwise.
TYPE-NAME is a symbol like INTEGER, STRING, SYMBOL, CONS, NULL, LIST, etc."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (type-name nil :reader vm-type-name)
  (:sexp-tag :typep)
  (:sexp-slots dst src type-name))

(define-simple-instruction vm-cons-p    :pred1 consp)

(define-simple-instruction vm-null-p    :pred1 null)

(define-simple-instruction vm-symbol-p  :pred1 symbolp)

(defmethod execute-instruction ((inst vm-function-p) state pc labels)
  (declare (ignore labels))
  (let* ((value (vm-reg-get state (vm-src inst)))
         (result (if (typep value 'vm-closure-object) 1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(define-vm-unary-instruction vm-type-of :type-of "Get the type of an object.")

(defmethod execute-instruction ((inst vm-type-of) state pc labels)
  (declare (ignore labels))
  (let* ((value  (vm-reg-get state (vm-src inst)))
         (result (vm-runtime-type-of value)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defun %vm-typep-normalize-sym (type-sym)
  "Normalize TYPE-SYM: unwrap HM type primitives and intern into CL package."
  (when (%vm-call-type-refinement-accessor "TYPE-PRIMITIVE-P" type-sym)
    (setf type-sym
          (%vm-call-type-refinement-accessor "TYPE-PRIMITIVE-NAME" type-sym)))
  (when (symbolp type-sym)
    (let ((cl-sym (find-symbol (symbol-name type-sym) :cl)))
      (when cl-sym (setf type-sym cl-sym))))
  type-sym)

(defmethod %vm-typep-call-predicate (value predicate)
  "Invoke PREDICATE on VALUE, supporting function/symbol/lambda forms."
  (cond
    ((functionp predicate)
     (ignore-errors (funcall predicate value)))
    ((and (symbolp predicate) (fboundp predicate))
     (ignore-errors (funcall (symbol-function predicate) value)))
    ((and (consp predicate) (eq (car predicate) 'lambda))
     (let ((fn (ignore-errors (eval predicate))))
       (and fn (ignore-errors (funcall fn value)))))
      (t nil)))

(defun %vm-type-refinement-accessor (name)
  "Resolve exported cl-cc/type refinement accessors when the type subsystem is available."
  (let ((pkg (find-package :cl-cc/type)))
    (when pkg
      (let ((sym (find-symbol name pkg)))
        (when (and sym (fboundp sym))
          (symbol-function sym))))))

(defun %vm-call-type-refinement-accessor (name value)
  "Call the exported cl-cc/type accessor NAME on VALUE when available.
Returns NIL when the type subsystem or accessor is unavailable."
  (let ((fn (%vm-type-refinement-accessor name)))
    (when fn
      (funcall fn value))))

(defun %vm-type-refinement-components (type-sym)
  "Return structural refinement data for TYPE-SYM when available.
Values are: refinement-p, base-type, predicate. When TYPE-SYM is not a
cl-cc/type refinement object, returns NIL for all values."
  (let ((refinement-p (%vm-call-type-refinement-accessor "TYPE-REFINEMENT-P" type-sym)))
    (if refinement-p
        (values t
                (%vm-call-type-refinement-accessor "TYPE-REFINEMENT-BASE" type-sym)
                (%vm-call-type-refinement-accessor "TYPE-REFINEMENT-PREDICATE" type-sym))
        (values nil nil nil))))

(defun %vm-typep-clos-class (value)
  "Return VALUE's VM CLOS class descriptor, or NIL for non-instances."
  (cond
    ((and (hash-table-p value) (gethash :__class__ value))
     (gethash :__class__ value))
    ((and (vectorp value)
          (plusp (length value))
          (hash-table-p (aref value 0)))
     (aref value 0))
    (t nil)))

(defun %vm-typep-clos-instance-p (value type-sym)
  "Return T when VALUE is a VM CLOS instance of TYPE-SYM or its superclasses."
  (let* ((class-ht   (%vm-typep-clos-class value))
         (class-name (and (hash-table-p class-ht) (gethash :__name__ class-ht)))
         (cpl        (and (hash-table-p class-ht) (gethash :__cpl__ class-ht))))
    (and class-ht
         (or (eq class-name type-sym)
             (member type-sym cpl :test #'eq)))))

;;; Data table: maps compound type-specifier head symbols to handler functions.
;;; Each handler is (value type-sym) → boolean.
;;; Note: lambdas here call vm-typep-check by symbol — resolved at call time, not load time.
(defmethod %vm-typep-range-check (value low high)
  "Return true when VALUE is inside LOW/HIGH bounds from CL numeric type specs."
  (and (or (eq low '*) (null low)
           (if (consp low) (> value (car low)) (>= value low)))
       (or (eq high '*) (null high)
           (if (consp high) (< value (car high)) (<= value high)))))

(defun vm-typecase-dispatch (value clauses)
  "Return the first clause whose type specifier accepts VALUE.

CLAUSES is an alist of (TYPE . PAYLOAD).  Primitive tags use the same hash-table
fast path as VM-TYPEP-CHECK, giving TYPECASE-like users a compact jump-table API;
compound forms such as `(or fixnum float)`, ranges such as `(integer 0 100)`, and
`(satisfies pred)` use the normal TYPEP fallback path."
  (loop for (type . payload) in clauses
        when (vm-typep-check value type)
          do (return (values payload type))
        finally (return (values nil nil))))

(defmethod vm-typep-check (value type-sym)
  "Check if VALUE is of TYPE-SYM using table dispatch for primitive and compound types."
  (setf type-sym (%vm-typep-normalize-sym type-sym))
  (let ((pred (and (symbolp type-sym)
                   (gethash type-sym *vm-primitive-type-dispatch*))))
    (cond
      ;; Fast table lookup for all primitive types
      (pred
       (funcall pred value))
      ;; Compound type specifiers: dispatch via *vm-compound-type-handlers*
      ((consp type-sym)
       (let ((handler (gethash (car type-sym) *vm-compound-type-handlers*)))
         (if handler
             (funcall handler value type-sym)
             (ignore-errors (typep value type-sym)))))
      ;; Structural refinement type objects
      (t
       (multiple-value-bind (refinement-p base-type predicate)
           (%vm-type-refinement-components type-sym)
         (cond
           (refinement-p
            (and (vm-typep-check value base-type)
                 (%vm-typep-call-predicate value predicate)))
        ;; VM CLOS instances — check class name / CPL.
        ;; Standard instances are vector-backed; custom metaclass instances may
        ;; still use hash-table storage.
            ((%vm-typep-clos-class value)
             (%vm-typep-clos-instance-p value type-sym))
           ;; Fallback to host typep
           (t (ignore-errors (typep value type-sym)))))))))

(defmethod execute-instruction ((inst vm-typep) state pc labels)
  (declare (ignore labels))
  (let* ((value (vm-reg-get state (vm-src inst)))
         (type-sym (vm-type-name inst))
         (result (if (vm-typep-check value type-sym) 1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction :around ((inst vm-cons-p) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (if (%vm-managed-cons-pointer-p value)
        (progn
          (vm-reg-set state (vm-dst inst) 1)
          (values (1+ pc) nil nil))
        (call-next-method))))
