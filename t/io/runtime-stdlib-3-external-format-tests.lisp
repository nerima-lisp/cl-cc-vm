(in-package #:cl-cc/vm/io/test)
(defun register-runtime-stdlib-3-external-format-tests ()
  (it-sequential "runtime-stdlib-3-external-format-roundtrip"
  (let* ((text "hello")
         (octets (cl-cc/vm:encode-external-format text)))
    (expect (vectorp octets) :to-be-truthy)
    (expect (cl-cc/vm:decode-external-format octets) :to-equal text))))
