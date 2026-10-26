(in-package #:cl-cc/vm/execution)

(defun version-function (fn arg-values) (declare (ignore fn arg-values)) fn)

;; FR-843: SIMD Auto-Specialization
(defun specialize-simd (fn alignment) (declare (ignore fn alignment)) fn)

;; FR-844: Loop Count Specialization
(defun specialize-loop-count (loop count) (declare (ignore loop count)) loop)

;; FR-845: Hot/Cold Function Splitting
(defun split-hot-cold (fn profile) (declare (ignore fn profile)) fn)
