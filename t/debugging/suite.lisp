(in-package #:cl-cc/vm/debugging/test)

(describe-concurrent "debugging feature tests"
  (register-feature-api-tests)
  (register-crash-report-tests)
  (register-runtime-stdlib-3-debug-tests)
  (register-runtime-stdlib-3-terminal-tests)
  (register-vm-debugging-moved-tests))
