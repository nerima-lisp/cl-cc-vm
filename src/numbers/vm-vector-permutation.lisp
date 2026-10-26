(in-package #:cl-cc/vm/numbers)

;; FR-851: Vector Permutation Instructions
(defun vpshufb (dst ctrl src) (declare (ignore dst ctrl src)) dst)
