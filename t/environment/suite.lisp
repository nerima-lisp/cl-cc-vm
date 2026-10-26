(in-package #:cl-cc/vm/environment/test)

(describe-concurrent "environment feature tests"
  (register-feature-api-tests)
  (register-runtime-stdlib-2-completion-tests)
  (register-runtime-stdlib-3-tests)
  (register-vm-environment-tests)
  (register-vm-runtime-stdlib-2-smoke-tests))
