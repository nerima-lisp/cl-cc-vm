(in-package #:cl-cc/vm/serialization/test)

(defun register-feature-api-tests ()
  nil
  (describe-sequential "serialization feature API"
                       (it
                        "round-trips a value through the VM FASL stream format"
                        (let ((encoded
                               (with-output-to-string (stream)
                                 (cl-cc/vm/serialization:vm-write-to-fasl
                                  '(:payload "round-trip")
                                  stream))))
                          (with-input-from-string (stream encoded)
                            (expect
                             (equal
                              (cl-cc/vm/serialization:vm-read-from-fasl stream)
                              '(:payload "round-trip"))
                             :to-be
                             t))))))
