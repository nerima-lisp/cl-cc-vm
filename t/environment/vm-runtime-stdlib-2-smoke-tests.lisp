(in-package #:cl-cc/vm/environment/test)

(defun register-vm-runtime-stdlib-2-smoke-tests ()
  (it-sequential "fr-function-exists fr-835-adaptive-runtime"
                   (destructuring-bind (sym) (list 'cl-cc/vm::runtime-tuning-report)
                     (expect (fboundp sym) :to-be-truthy)))

  (it-sequential "fr-function-exists fr-896-package-lock"
                   (destructuring-bind (sym) (list 'cl-cc/vm::lock-package)
                     (expect (fboundp sym) :to-be-truthy)))

  (it-sequential "fr-variable-exists fr-833-gc-tuning"
                   (destructuring-bind (sym) (list 'cl-cc/runtime::*gc-nursery-size*)
                     (expect (boundp sym) :to-be-truthy)))
)
