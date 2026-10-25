(in-package #:cl-cc/vm/execution)

(defmacro bracket (expr) `',expr)

(defmacro splice (expr) expr)

(defmacro define-syntax (name &rest clauses)
  (declare (ignore clauses))
  `(defmacro ,name (&rest args) `(list ',',name ,@args)))
