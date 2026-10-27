(in-package #:cl-cc/vm/serialization/test)

(describe-concurrent "serialization feature tests"
  (register-feature-api-tests)
  (register-json-compat-api-tests)
  (register-json-tests)
  (register-runtime-stdlib-3-fasl-tests)
  (register-vm-runtime-stdlib-2-smoke-tests))
