(in-package #:cl-cc/vm/io)

(defun read-balanced-form (stream) (read stream nil nil))
