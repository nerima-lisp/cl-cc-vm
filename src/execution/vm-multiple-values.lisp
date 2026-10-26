(in-package #:cl-cc/vm/execution)

(defun vm-nth-value (n form-values)
  "FR-865: Return the Nth value from FORM-VALUES (a list of multiple values)."
  (nth n form-values))
