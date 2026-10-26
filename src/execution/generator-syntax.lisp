(in-package #:cl-cc/vm/execution)

(defmacro defgenerator (name args &body body)
  `(defun ,name ,args (let ((state nil)) (lambda () (block yield ,@body)))))
