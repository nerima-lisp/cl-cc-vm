;;;; tests/unit/vm/io-runners-tests.lisp — I/O runner convenience coverage

(in-package #:cl-cc/vm/io/test)

(defun register-io-runners-tests ()
  (it-sequential "run-compiled-with-io-basic"
  (let* ((program (cl-cc/vm/execution:make-vm-program
                    :instructions (list (cl-cc/vm:make-vm-const :dst :r0 :value 42)
                                        (cl-cc/vm:make-vm-halt :reg :r0))
                    :result-register :r0))
         (out (make-string-output-stream)))
    (expect (cl-cc/vm/io:run-compiled-with-io program :output-stream out) :to-equal 42))) (it-sequential "run-compiled-with-io-binds-custom-streams"
  (let* ((program (cl-cc/vm/execution:make-vm-program
                    :instructions (list (cl-cc/vm:make-vm-const :dst :r0 :value 7)
                                        (cl-cc/vm:make-vm-halt :reg :r0))
                    :result-register :r0))
         (in (make-string-input-stream "input"))
         (out (make-string-output-stream)))
    (expect (cl-cc/vm/io:run-compiled-with-io program :input-stream in :output-stream out) :to-equal 7))) (it-sequential "run-string-with-io-uses-compile-hook"
  (let ((cl-cc/vm/protocols:*vm-compile-string-hook*
          (lambda (source)
            (declare (ignore source))
            (cl-cc/vm/execution:make-vm-program
             :instructions (list (cl-cc/vm:make-vm-const :dst :r0 :value 99)
                                 (cl-cc/vm:make-vm-halt :reg :r0))
             :result-register :r0))))
    (expect (cl-cc/vm/io:run-string-with-io "ignored source") :to-equal 99))))
