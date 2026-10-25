(in-package :cl-cc/vm)

;;; Hash-consing: interns structurally-equal cons trees to a canonical
;;; shared instance, weakly keyed so unreferenced trees can still be GC'd.

(defparameter *vm-hash-cons-table*
  (make-hash-table :test #'equal :weakness :value)
  "Runtime hash-cons table keyed by (car cdr).
Values are weak so unreferenced interned cons cells can be reclaimed by GC.")

(defun vm-clear-hash-cons-table ()
  "Clear the runtime hash-cons table and return it."
  (clrhash *vm-hash-cons-table*)
  *vm-hash-cons-table*)

(defun %vm-hash-cons-intern-pair (car-value cdr-value)
  "Intern one cons pair in the runtime hash-cons table."
  (let* ((key (list car-value cdr-value))
         (existing (gethash key *vm-hash-cons-table*)))
    (or existing
        (setf (gethash key *vm-hash-cons-table*)
              (cons car-value cdr-value)))))

(defun %vm-hash-cons-canonicalize (value seen)
  "Recursively canonicalize VALUE by hash-consing nested cons trees.

SEEN tracks already-visited cons cells to avoid infinite recursion on cyclic
inputs and to preserve graph sharing while canonicalizing."
  (if (consp value)
      (multiple-value-bind (cached present-p) (gethash value seen)
        (cond
          ;; Active recursion marker => cycle detected on this branch.
          ((and present-p (eq cached :visiting))
           (values value t))
          (present-p
           (values cached nil))
          (t
           (setf (gethash value seen) :visiting)
           (multiple-value-bind (canon-car car-cyclic-p)
               (%vm-hash-cons-canonicalize (car value) seen)
             (multiple-value-bind (canon-cdr cdr-cyclic-p)
                 (%vm-hash-cons-canonicalize (cdr value) seen)
               (if (or car-cyclic-p cdr-cyclic-p)
                   (progn
                     ;; Avoid interning cyclic structures in equal-hash tables.
                     (setf (gethash value seen) value)
                     (values value t))
                   (let ((canon (%vm-hash-cons-intern-pair canon-car canon-cdr)))
                     (setf (gethash value seen) canon)
                     (values canon nil))))))))
      (values value nil)))

(defun vm-hash-cons (car-value cdr-value)
  "Return a shared cons cell for CAR-VALUE/CDR-VALUE.

Nested cons values are recursively canonicalized so structurally equivalent
list/tree shapes can share storage under explicit hash-cons opt-in."
  (let ((seen (make-hash-table :test #'eq)))
    (multiple-value-bind (canon-car car-cyclic-p)
        (%vm-hash-cons-canonicalize car-value seen)
      (multiple-value-bind (canon-cdr cdr-cyclic-p)
          (%vm-hash-cons-canonicalize cdr-value seen)
        (if (or car-cyclic-p cdr-cyclic-p)
            (cons canon-car canon-cdr)
            (%vm-hash-cons-intern-pair canon-car canon-cdr))))))
