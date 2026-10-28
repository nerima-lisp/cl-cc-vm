(in-package #:cl-cc/vm/io/test)

(describe-concurrent "io feature tests"
  (register-feature-api-tests)
  (register-format-tests)
  (register-io-network-tests)
  (register-io-runners-tests)
  (register-io-tests)
  (register-runtime-stdlib-2-print-circle-tests)
  (register-runtime-stdlib-3-external-format-tests)
  (register-runtime-stdlib-3-io-tests)
  (register-vm-stream-bridge-regression-tests)
  (register-vm-runtime-stdlib-2-smoke-tests))
