(in-package #:cl-cc/vm/execution)

(defmacro list-of (expr &rest clauses)
  `(loop ,@(mapcan (lambda (c) (list (first c) (second c))) clauses)
         collect ,expr))
