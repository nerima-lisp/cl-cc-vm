(in-package #:cl-cc/vm/serialization/test)

(defun register-runtime-stdlib-3-fasl-tests ()
  (it-sequential "stdlib3-fasl-roundtrip"
  (let ((stream (make-string-output-stream))
        (object '(:answer 42 :items (a b c))))
    (cl-cc/vm:vm-write-to-fasl object stream)
    (expect (cl-cc/vm:vm-read-from-fasl
                   (make-string-input-stream (get-output-stream-string stream))) :to-equal object))))
