(in-package #:cl-cc/vm/execution)

(defmacro static-assert (condition &optional (message ""))
  `(eval-when (:compile-toplevel) (unless ,condition (error "Static assertion failed: ~A" ,message))))

(defmacro compile-time-test (name expr)
  `(eval-when (:compile-toplevel) (assert ,expr () "Compile-time test ~A failed" ,name)))
