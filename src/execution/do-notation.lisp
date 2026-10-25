(in-package #:cl-cc/vm/execution)

(defmacro cl-cc-do (bindings &body body)
  (reduce (lambda (acc binding)
            `(bind ,(third binding) (lambda (,(second binding)) ,acc)))
          (reverse bindings) :initial-value `(progn ,@body)))
