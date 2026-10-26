(in-package #:cl-cc/vm/environment)

(defmacro defmodule (name &rest body) (declare (ignore body)) `(defvar ,name nil))
