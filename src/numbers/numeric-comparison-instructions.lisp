;;; Comparison Operations

(in-package #:cl-cc/vm/numbers)

(define-vm-binary-instruction vm-lt     :lt     "Less than comparison. Returns 1 if LHS < RHS, 0 otherwise.")

(define-vm-binary-instruction vm-gt     :gt     "Greater than comparison. Returns 1 if LHS > RHS, 0 otherwise.")

(define-vm-binary-instruction vm-le     :le     "Less than or equal comparison. Returns 1 if LHS <= RHS, 0 otherwise.")

(define-vm-binary-instruction vm-ge     :ge     "Greater than or equal comparison. Returns 1 if LHS >= RHS, 0 otherwise.")

(define-vm-binary-instruction vm-num-eq :num-eq "Numeric equality comparison. Returns 1 if LHS = RHS, 0 otherwise.")

;;; Instruction Execution - Comparison Operations

(define-simple-instruction vm-lt     :pred2 <)

(define-simple-instruction vm-gt     :pred2 >)

(define-simple-instruction vm-le     :pred2 <=)

(define-simple-instruction vm-ge     :pred2 >=)

(define-simple-instruction vm-num-eq :pred2 =)
