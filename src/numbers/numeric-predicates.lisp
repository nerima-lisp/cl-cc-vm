(in-package #:cl-cc/vm/numbers)

(define-vm-unary-instruction  vm-number-p  :number-p  "Type predicate for numbers. Returns 1 if SRC is a number, 0 otherwise.")

(define-vm-unary-instruction  vm-integer-p :integer-p "Type predicate for integers. Returns 1 if SRC is an integer, 0 otherwise.")

(define-vm-unary-instruction  vm-evenp        :evenp    "Even predicate. Returns 1 if SRC is even, 0 otherwise.")

(define-vm-unary-instruction  vm-oddp         :oddp     "Odd predicate. Returns 1 if SRC is odd, 0 otherwise.")

(define-simple-instruction vm-number-p  :pred1 numberp)

(define-simple-instruction vm-integer-p :pred1 integerp)

(define-simple-instruction vm-evenp :pred1  evenp)

(define-simple-instruction vm-oddp  :pred1  oddp)
