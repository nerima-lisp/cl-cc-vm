(in-package #:cl-cc/vm/execution)

(defmacro the-value (val) `(the (eql ,val) ,val))

;; FR-833: Existential Types
(defmacro pack (val &rest spec) (declare (ignore spec)) val)

(defmacro unpack ((var packed) &body body) `(let ((,var ,packed)) ,@body))
