(in-package #:cl-cc/vm/conditions/test)

(defun register-feature-api-tests ()
  nil
  (describe-sequential "conditions feature API"
                       (it "constructs VM errors in the VM condition hierarchy"
                           (let ((condition
                                  (cl:make-condition
                                   'cl-cc/vm/conditions:vm-simple-error
                                   :format-control
                                   "VM condition ~A"
                                   :format-arguments
                                   '("ready"))))
                             (expect
                              (typep condition
                                     'cl-cc/vm/conditions:vm-condition)
                              :to-be
                              t)
                             (expect (typep condition 'cl:simple-condition)
                                     :to-be
                                     t)))))
