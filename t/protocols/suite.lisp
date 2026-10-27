(in-package #:cl-cc/vm/protocols/test)

(describe-concurrent "protocols feature tests"
  (register-feature-api-tests)
  (register-vm-execution-protocol-tests)
  (register-vm-host-bridge-tests))
