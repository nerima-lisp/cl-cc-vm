(in-package #:cl-cc/vm/conditions/test)

(describe-concurrent "conditions feature tests"
  (register-feature-api-tests)
  (register-conditions-tests)
  (register-runtime-stdlib-3-condition-tests))
