(in-package #:cl-cc/vm/protocols)

(defgeneric vm-feature-initial-globals (feature))

(defgeneric vm-trace-output-symbol ())

(defgeneric vm-trace-output-stream ())

(defgeneric vm-lookup-bridge-callable (symbol))

(defgeneric vm-call-with-readable-printing (thunk))

(defgeneric vm-resolve-hash-test (test-symbol))

(defgeneric vm-environment-canary-value ())

(defgeneric vm-load-pathname ())
