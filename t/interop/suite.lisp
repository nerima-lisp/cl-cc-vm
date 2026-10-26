(in-package #:cl-cc/vm/interop/test)

(describe-concurrent "interop feature tests"
  (register-feature-api-tests)
  (register-vm-bridge-tests)
  (register-regex-tests))
