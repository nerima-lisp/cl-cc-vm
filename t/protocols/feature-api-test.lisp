(in-package #:cl-cc-vm/test)
(describe-sequential "protocols feature API"
  (it "registers a host callable and returns it"
    (let* ((name (gensym "FEATURE-TEST-"))
           (callable (lambda () :registered)))
      (unwind-protect
          (progn
            (expect
             (eq (cl-cc/vm/protocols:vm-register-host-bridge name callable)
                 callable)
             :to-be t)
            (expect
             (eq (gethash name cl-cc/vm/protocols:*vm-host-bridge-functions*)
                 callable)
             :to-be t))
        (remhash name cl-cc/vm/protocols:*vm-host-bridge-functions*)))))
