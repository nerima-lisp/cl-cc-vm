;;; FR-498: Hash code computation

(in-package #:cl-cc/vm/data)

(define-vm-unary-instruction vm-sxhash :sxhash "Compute hash code for an object.")

(define-simple-instruction vm-sxhash :unary sxhash)
