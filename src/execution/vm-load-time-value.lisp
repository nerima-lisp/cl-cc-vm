(in-package #:cl-cc/vm/execution)

(defmacro vm-load-time-value (form &optional read-only-p)
  "FR-892: Evaluate FORM at load time and return its value."
  (declare (ignore read-only-p))
  `(load-time-value ,form))
