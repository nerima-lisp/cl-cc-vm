;;;; tests/unit/vm/package-tests.lisp — package export smoke tests

(in-package #:cl-cc/vm/execution/test)



(defun register-package-tests ()
  (it-sequential "cl-cc-vm-package-exists"
                 (expect (find-package :cl-cc/vm) :to-be-truthy))

  )

