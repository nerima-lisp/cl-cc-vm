(in-package #:cl-cc/vm/data)

(defconstant +mt19937-n+ 624)

(defstruct (vm-random-state (:constructor %make-vm-random-state))
  "MT19937 state used by the CL-CC VM random API."
  (mt (make-array +mt19937-n+ :element-type '(unsigned-byte 32) :initial-element 0))
  (index +mt19937-n+ :type fixnum))

(deftype random-state () 'vm-random-state)

(defun random-state-p (object)
  "Return true when OBJECT is a CL-CC VM random-state."
  (vm-random-state-p object))

