(in-package #:cl-cc/vm/data/test)

(defun register-feature-api-tests ()
  nil
  (describe-sequential "data feature API"
                       (it "reads and writes VM specialized array values"
                           (let ((array
                                  (cl-cc/vm/data:vm-make-specialized-array 3
                                                                           :fixnum)))
                             (expect
                              (cl-cc/vm/data:vm-specialized-array-p array)
                              :to-be
                              t)
                             (expect
                              (cl-cc/vm/data:vm-specialized-array-ref array 1)
                              :to-be
                              0)
                             (setf (cl-cc/vm/data:vm-specialized-array-ref array
                                                                           1) 42)
                             (expect
                              (cl-cc/vm/data:vm-specialized-array-ref array 1)
                              :to-be
                              42)))))
