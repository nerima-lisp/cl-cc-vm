(in-package #:cl-cc/vm/execution)

(defun detect-fusion-pairs (instrs) (declare (ignore instrs)) nil)

;; FR-826: Branchless Code Generation
(defun emit-cmov (cond dst src) (declare (ignore cond dst src)) t)

;; FR-827: Instruction Count Minimization
(defvar *optimize-for-instruction-count* nil)
