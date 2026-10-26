(in-package #:cl-cc/vm/protocols/test)

(defun register-vm-host-bridge-tests ()
  (it-sequential "vm-register-host-bridge-stores-in-table"
  (let ((sym (gensym "BRIDGE-TEST-")))
    (cl-cc/vm::vm-register-host-bridge sym (lambda () :ok))
    (expect (gethash sym cl-cc/vm::*vm-host-bridge-functions*) :to-be-truthy))) (it-sequential "vm-register-host-bridge-is-idempotent"
  (let ((sym (gensym "BRIDGE-IDEM-")))
    (cl-cc/vm::vm-register-host-bridge sym (lambda () :ok))
    (cl-cc/vm::vm-register-host-bridge sym (lambda () :ok))
    (expect (gethash sym cl-cc/vm::*vm-host-bridge-functions*) :to-be-truthy))) (it-sequential "vm-register-runtime-callable-and-lookup"
  (let ((name "RT-UNIT-TEST-CALLABLE"))
    (unwind-protect
         (progn
           (cl-cc/vm::vm-register-runtime-callable name (lambda () :ok))
            (expect (functionp (gethash name cl-cc/vm::*vm-runtime-callables*)) :to-be-truthy)
            (expect (funcall (cl-cc/vm::%vm-runtime-callable name)) :to-be :ok))
      (remhash name cl-cc/vm::*vm-runtime-callables*)))))
