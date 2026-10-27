(in-package #:cl-cc/vm/numbers/test)

(defun register-feature-api-tests ()
  nil
  (describe-sequential "numbers feature API"
                       (it "adds tagged fixnums and interpolates values"
                           (multiple-value-bind (sum overflow-p)
                               (cl-cc/vm/numbers:fixnum-add
                                (cl-cc/vm/numbers:make-fixnum 2)
                                (cl-cc/vm/numbers:make-fixnum 3))
                             (expect overflow-p :to-be nil)
                             (expect (cl-cc/vm/numbers:fixnum-value sum)
                                     :to-be
                                     5))
                           (expect
                            (= (cl-cc/vm/numbers:lerp 10.0d0 20.0d0 0.25d0)
                               12.5d0)
                            :to-be
                            t))))
