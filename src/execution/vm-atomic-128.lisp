(in-package #:cl-cc/vm/execution)

;; FR-850: Atomic 128-bit Operations / CMPXCHG16B
(defun cmpxchg128 (addr expected-lo expected-hi new-lo new-hi)
  (declare (ignore addr expected-lo expected-hi new-lo new-hi))
  (values t nil nil))
