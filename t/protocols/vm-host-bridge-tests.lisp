(in-package #:cl-cc/vm/protocols/test)

(defun register-vm-host-bridge-tests ()
  (it-sequential "vm-register-host-bridge-stores-in-table"
  (let ((sym (gensym "BRIDGE-TEST-")))
    (cl-cc/vm/protocols:vm-register-host-bridge sym (lambda () :ok))
    (expect (gethash sym cl-cc/vm/protocols:*vm-host-bridge-functions*) :to-be-truthy)))
  (it-sequential "vm-register-host-bridge-is-idempotent" (let ((sym (gensym "BRIDGE-IDEM-"))) (cl-cc/vm/protocols:vm-register-host-bridge sym (lambda () :ok)) (cl-cc/vm/protocols:vm-register-host-bridge sym (lambda () :ok)) (expect (gethash sym cl-cc/vm/protocols:*vm-host-bridge-functions*) :to-be-truthy))))
