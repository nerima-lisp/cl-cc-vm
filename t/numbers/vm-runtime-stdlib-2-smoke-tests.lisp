(in-package #:cl-cc/vm/numbers/test)

(defun register-vm-runtime-stdlib-2-smoke-tests ()
  (it-sequential "fr-function-exists fr-843-float-traps"
                   (destructuring-bind (sym) (list 'cl-cc/vm/numbers:with-float-traps-masked)
                     (expect (fboundp sym) :to-be-truthy)))

  (it-sequential "fr-function-exists fr-844-extended-prec"
                   (destructuring-bind (sym) (list 'cl-cc/vm/numbers:dd+)
                     (expect (fboundp sym) :to-be-truthy)))

  (it-sequential "fr-function-exists fr-884-floor"
                   (destructuring-bind (sym) (list 'cl-cc/vm/numbers::vm-floor)
                     (expect (fboundp sym) :to-be-truthy)))

  (it-sequential "fr-function-exists fr-885-ffloor"
                   (destructuring-bind (sym) (list 'cl-cc/vm/numbers::vm-ffloor)
                     (expect (fboundp sym) :to-be-truthy)))

  (it-sequential "fr-variable-exists fr-860-numeric-contagion"
                   (destructuring-bind (sym) (list 'cl-cc/vm/numbers:*numeric-contagion-table*)
                     (expect (boundp sym) :to-be-truthy)))

  (it-sequential "fr-variable-exists fr-861-inline-dispatch"
                   (destructuring-bind (sym) (list 'cl-cc/vm/numbers:*arith-dispatch-table*)
                     (expect (boundp sym) :to-be-truthy)))
  (it-sequential "fr-function-exists fr-829-overflow"
                 (destructuring-bind (sym) (list 'cl-cc/vm/numbers:vm-fixnum-overflow-detected-p)
                   (expect (fboundp sym) :to-be-truthy)))
)
