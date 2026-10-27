(in-package #:cl-cc/vm/protocols/test)

(defun register-vm-execution-protocol-tests ()
  (it-sequential "vm-host-bridge-registration"
                   (let ((sym (gensym "VM-BRIDGE-")))
                     (unwind-protect
                         (progn
                           (expect (gethash sym cl-cc/vm/protocols:*vm-host-bridge-functions*) :to-be-null)
                           (cl-cc/vm/protocols:vm-register-host-bridge sym (lambda () :ok))
                           (expect (gethash sym cl-cc/vm/protocols:*vm-host-bridge-functions*) :to-be-truthy))
                       (remhash sym cl-cc/vm/protocols:*vm-host-bridge-functions*))))
)
