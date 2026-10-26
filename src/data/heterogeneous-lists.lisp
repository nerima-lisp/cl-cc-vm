(in-package #:cl-cc/vm/data)

(defun hnil () nil)

(defun hcons (x xs) (cons x xs))

(defun hhead (xs) (car xs))

(defun htail (xs) (cdr xs))
