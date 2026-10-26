(in-package #:cl-cc/vm/io)

(defmacro defsession (name &rest proto) (declare (ignore proto)) `(defvar ,name nil))
