;;; runtime-stdlib-3-debug-tests.lisp — FR-944/945/1085 debug surface

(in-package #:cl-cc/vm/debugging/test)

(defun register-runtime-stdlib-3-debug-tests ()
  (it-sequential "trace-helpers-log-call-and-return"
  (let ((output (make-string-output-stream)))
    (let ((cl-cc/vm:*trace-output* output))
      (cl-cc/vm:vm-untrace)
      (cl-cc/vm:vm-trace 'foo)
      (expect (= 3 (cl-cc/vm:vm-with-trace ('foo '(1 2)) (+ 1 2))) :to-be-truthy))
    (let ((text (get-output-stream-string output)))
      (expect (search "0: (FOO 1 2)" text) :to-be-truthy)
      (expect (search "0: FOO returned 3" text) :to-be-truthy)))) (it-sequential "debugger-helper-surface-is-callable"
  (expect (member 'car (cl-cc/vm:vm-apropos-list "CAR" :cl) :test #'eq) :to-be-truthy)
  (expect (cl-cc/vm:vm-ed 'dummy) :to-be-null)
  (let ((text (with-output-to-string (out)
                (cl-cc/vm:vm-describe 'car out))))
    (expect (search "CAR" text) :to-be-truthy))
  (expect (cl-cc/vm:vm-inspect 'car) :to-be 'car)))
