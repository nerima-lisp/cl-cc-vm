(in-package #:cl-cc/vm/data)

;; FR-854: Cycle Detection for Reference Counting
(defun detect-rc-cycles () nil)

;; FR-855: Deferred Reference Counting
(defvar *deferred-rc-enabled* nil)
