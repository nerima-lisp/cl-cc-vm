(in-package #:cl-cc/vm/clos/test)

(describe-concurrent "clos feature tests"
  (register-feature-api-tests)
  (register-vm-deep-copy-tests)
  (register-vm-clos-execute-extra-tests)
  (register-vm-clos-execute-tests)
  (register-vm-clos-tests)
  (register-vm-copy-instance-tests)
  (register-vm-dispatch-gf-multi-tests)
  (register-vm-dispatch-gf-tests)
  (register-vm-execution-moved-tests)
  (register-vm-mop-tests))
