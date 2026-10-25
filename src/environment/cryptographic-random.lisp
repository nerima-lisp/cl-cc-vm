(in-package #:cl-cc/vm/environment)

(defun crypto-random (n &key (secure t))
  (declare (ignore secure))
  (make-array n :element-type '(unsigned-byte 8) :initial-element 0))
