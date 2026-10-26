;;; Type-Of Instruction

(in-package #:cl-cc/vm/data)

(defvar *vm-type-of-dispatch*)

;;; FR-2501 / FR-2502: Dynamic / TypeRep runtime values

(defstruct (vm-type-rep (:constructor %make-vm-type-rep))
  "Runtime representation of a VM-visible type designator."
  (specifier 't))

(defun make-vm-type-rep (specifier)
  "Construct a runtime type representation for SPECIFIER."
  (%make-vm-type-rep :specifier specifier))

(defun vm-type-rep (specifier)
  "Coerce SPECIFIER into a vm-type-rep object."
  (if (vm-type-rep-p specifier)
      specifier
      (make-vm-type-rep specifier)))

(defun vm-type-rep-equal (left right)
  "Return T when LEFT and RIGHT represent the same runtime type."
  (equal (vm-type-rep-specifier (vm-type-rep left))
         (vm-type-rep-specifier (vm-type-rep right))))

(defun %vm-type-rep-value-compatible-p (value expected-type)
  "Return T when VALUE satisfies EXPECTED-TYPE's runtime type specifier."
  (vm-typep-check value (vm-type-rep-specifier (vm-type-rep expected-type))))

(defun %vm-type-rep-compatible-p (stored-type value expected-type)
  "Return T when STORED-TYPE exactly matches EXPECTED-TYPE or VALUE satisfies it."
  (or (vm-type-rep-equal stored-type expected-type)
      (%vm-type-rep-value-compatible-p value expected-type)))

(defstruct (vm-dynamic (:constructor %make-vm-dynamic))
  "A runtime value paired with its TypeRep."
  (type-rep (make-vm-type-rep 't) :type vm-type-rep)
  (value nil))

(defun make-vm-dynamic (type-rep value)
  "Construct a typed runtime Dynamic wrapper."
  (%make-vm-dynamic :type-rep (vm-type-rep type-rep) :value value))

(defun vm-runtime-type-of (value)
  "Return the VM-visible runtime type designator for VALUE."
  (or (cdr (assoc-if (lambda (pred) (funcall pred value))
                     *vm-type-of-dispatch*))
      (and (hash-table-p value)
           (gethash :__class__ value)
           (let ((class-ht (gethash :__class__ value)))
             (and (hash-table-p class-ht)
                  (or (gethash :__name__ class-ht) 't))))
      't))

(defun vm-type-rep-of (value)
  "Return the runtime TypeRep of VALUE."
  (make-vm-type-rep (vm-runtime-type-of value)))

(defun vm-wrap-dynamic (value &optional type-specifier)
  "Wrap VALUE in a Dynamic with TYPE-SPECIFIER or the inferred runtime type."
  (make-vm-dynamic (or type-specifier (vm-type-rep-of value)) value))

(defun vm-unwrap-dynamic (dynamic expected-type)
  "Return (values value t) when DYNAMIC matches EXPECTED-TYPE, else (values nil nil)."
  (let ((dynamic-value (if (vm-dynamic-p dynamic)
                            dynamic
                            (vm-wrap-dynamic dynamic))))
    (let ((payload (vm-dynamic-value dynamic-value)))
      (if (%vm-type-rep-compatible-p (vm-dynamic-type-rep dynamic-value)
                                     payload
                                     expected-type)
          (values payload t)
          (values nil nil)))))

(defun vm-cast-with-type-rep (value expected-type)
  "Attempt to cast VALUE to EXPECTED-TYPE using runtime TypeRep compatibility."
  (if (vm-dynamic-p value)
      (vm-unwrap-dynamic value expected-type)
      (let ((expected-rep (vm-type-rep expected-type)))
        (if (%vm-type-rep-compatible-p (vm-type-rep-of value)
                                       value
                                       expected-rep)
            (values value t)
            (values nil nil)))))

(defparameter *vm-primitive-type-predicates*
  (list
   (cons 'integer #'integerp)
   (cons 'fixnum (lambda (v) (typep v 'fixnum)))
   (cons 'bignum (lambda (v) (typep v 'bignum)))
   (cons 'ratio (lambda (v) (typep v 'ratio)))
   (cons 'float #'floatp)
   (cons 'single-float (lambda (v) (typep v 'single-float)))
   (cons 'double-float (lambda (v) (typep v 'double-float)))
   (cons 'short-float (lambda (v) (typep v 'short-float)))
   (cons 'long-float (lambda (v) (typep v 'long-float)))
   (cons 'real #'realp)
   (cons 'rational #'rationalp)
   (cons 'complex #'complexp)
   (cons 'string #'stringp)
   (cons 'symbol #'symbolp)
   (cons 'keyword #'keywordp)
   (cons 'cons #'consp)
   (cons 'null #'null)
   (cons 'list #'listp)
   (cons 'number #'numberp)
   (cons 'character #'characterp)
   (cons 'base-char #'characterp)
   (cons 'standard-char #'characterp)
   (cons 'atom #'atom)
   (cons 'function #'functionp)
   (cons 'vector #'vectorp)
   (cons 'package #'packagep)
   (cons 'stream #'streamp)
   (cons 'input-stream #'input-stream-p)
   (cons 'output-stream #'output-stream-p)
   (cons 'bit-vector (lambda (v) (typep v 'bit-vector)))
   (cons 'simple-vector (lambda (v) (typep v 'simple-vector)))
   (cons 'simple-array (lambda (v) (typep v 'simple-array)))
   (cons 'array #'arrayp)
   (cons 'file-stream (lambda (v) (typep v 'file-stream)))
   (cons 'string-stream (lambda (v) (typep v 'string-stream)))
   (cons 'broadcast-stream (lambda (v) (typep v 'broadcast-stream)))
   (cons 'two-way-stream (lambda (v) (typep v 'two-way-stream)))
   (cons 'echo-stream (lambda (v) (typep v 'echo-stream)))
   (cons 'concatenated-stream (lambda (v) (typep v 'concatenated-stream)))
   (cons 'synonym-stream (lambda (v) (typep v 'synonym-stream)))
   (cons 'hash-table (lambda (v) (typep v 'vm-hash-table-object)))
   (cons 'bit (lambda (v) (or (eql v 0) (eql v 1)))))
  "Alist mapping type-symbol to single-argument predicate function for vm-typep-check.")

(defparameter *vm-primitive-type-dispatch*
  (let ((table (make-hash-table :test #'eq)))
    (dolist (entry *vm-primitive-type-predicates* table)
      (setf (gethash (car entry) table) (cdr entry))))
  "Tag-oriented primitive TYPEP dispatch table.

This is the fast path for fixnum/cons/null/symbol and other primitive type
specifiers.  Compound specifiers fall through to `*vm-compound-type-handlers*` or
host TYPEP, including `(satisfies predicate)` fallback.")

;;; Ordered dispatch list for vm-type-of: first matching predicate wins.
(defparameter *vm-type-of-dispatch*
  (list
   (cons #'null                                'null)
   (cons (lambda (v) (typep v 'fixnum))        'fixnum)
   (cons (lambda (v) (typep v 'bignum))        'bignum)
   (cons (lambda (v) (typep v 'ratio))         'ratio)
   (cons (lambda (v) (typep v 'double-float))  'double-float)
   (cons (lambda (v) (typep v 'long-float))    'long-float)
   (cons (lambda (v) (or (typep v 'single-float)
                         (typep v 'short-float)
                         (floatp v)))          'single-float)
   (cons (lambda (v) (typep v 'bit-vector))    'bit-vector)
   (cons #'stringp                             'string)
   (cons (lambda (v) (typep v 'simple-vector)) 'simple-vector)
   (cons (lambda (v) (typep v 'simple-array))  'simple-array)
   (cons #'characterp                          'character)
   (cons #'symbolp                             'symbol)
   (cons #'pathnamep                           'pathname)
   (cons (lambda (v) (typep v 'random-state))  'random-state)
   (cons #'packagep                            'package)
   (cons #'consp                               'cons)
   (cons #'complexp                            'complex)
   (cons (lambda (v) (or (functionp v) (vm-closure-object-p v))) 'function)
   (cons #'vectorp                             'vector)
   (cons #'arrayp                              'array))
  "Ordered dispatch list for vm-type-of: first matching predicate wins.")

(defparameter *vm-compound-type-handlers*
  (let ((ht (make-hash-table :test #'eq)))
    (flet ((reg (sym fn) (setf (gethash sym ht) fn)))
      (reg 'refine    (lambda (v ts) (and (vm-typep-check v (second ts))
                                           (%vm-typep-call-predicate v (third ts)))))
      (reg 'satisfies (lambda (v ts) (ignore-errors (funcall (second ts) v))))
      (reg 'or        (lambda (v ts) (some  (lambda (t2) (vm-typep-check v t2)) (cdr ts))))
      (reg 'and       (lambda (v ts) (every (lambda (t2) (vm-typep-check v t2)) (cdr ts))))
      (reg 'not       (lambda (v ts) (not (vm-typep-check v (second ts)))))
      (reg 'member    (lambda (v ts) (member v (cdr ts) :test #'eql)))
      (reg 'integer   (lambda (v ts)
                        (and (integerp v)
                             (%vm-typep-range-check v (second ts) (third ts)))))
      (reg 'real      (lambda (v ts)
                        (and (realp v)
                             (%vm-typep-range-check v (second ts) (third ts)))))
      (reg 'eql       (lambda (v ts) (eql v (second ts))))
      (reg 'values    (lambda (v ts) (declare (ignore v ts)) t))
      (reg 'function  (lambda (v ts) (declare (ignore ts))
                         (or (vm-closure-object-p v) (functionp v)))))
    ht)
  "Hash table mapping compound type-specifier head symbols to handlers (value type-sym) → boolean.
Used by vm-typep-check for compound forms: (or ...), (and ...), (not ...), etc.")
