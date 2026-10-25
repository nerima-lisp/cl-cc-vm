(in-package #:cl-cc/vm/numbers)

(defun swar-and (a b mask) (logand (logand a mask) (logand b mask)))
