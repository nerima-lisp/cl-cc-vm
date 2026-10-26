(in-package #:cl-cc/vm/data)

(defun string-contains-simd (haystack needle)
  (search needle haystack))
