(in-package :cl-cc/vm/protocols)

(defgeneric vm-random-integer (limit)
  (:documentation "Return an integer below LIMIT using the environment's default random state."))

