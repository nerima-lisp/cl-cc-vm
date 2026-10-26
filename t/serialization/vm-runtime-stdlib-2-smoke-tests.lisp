(in-package #:cl-cc/vm/serialization/test)

(defun register-vm-runtime-stdlib-2-smoke-tests ()
  (it-sequential "fr-function-exists fr-902-pgo"
                   (destructuring-bind (sym) (list 'cl-cc/vm::save-pgo-data)
                     (expect (fboundp sym) :to-be-truthy)))

  (it-sequential "fr-variable-exists fr-899-fasl-demand"
                   (destructuring-bind (sym) (list 'cl-cc/vm::*fasl-toc-enabled*)
                     (expect (boundp sym) :to-be-truthy)))
)
