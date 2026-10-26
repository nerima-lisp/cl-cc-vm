(in-package #:cl-cc/vm/conditions/test)
(defun register-runtime-stdlib-3-condition-tests ()
  (it-sequential "rich-error-report-includes-location-context-and-suggestion"
  (let* ((state (make-instance 'cl-cc/vm::vm-io-state))
         (loc (cl-cc/vm:make-source-location :file "foo.lisp" :line 2 :column 4))
         (condition (cl-cc/vm::make-vm-unbound-variable
                     state 'pritn
                     :source-location loc
                     :source-text (format nil "(defun f ()~%  pritn~%  42)")
                     :suggestions '(print))))
    (let ((text (format nil "~A" condition)))
      (expect (search "foo.lisp:2:4" text) :to-be-truthy)
      (expect (search "Did you mean" text) :to-be-truthy)
      (expect (search "pritn" text) :to-be-truthy)
      (expect (search "^" text) :to-be-truthy)))) (it-sequential "rich-error-json-output-is-structured"
  (let* ((state (make-instance 'cl-cc/vm::vm-io-state))
         (loc (cl-cc/vm:make-source-location :file "foo.lisp" :line 1 :column 1))
         (condition (cl-cc/vm::make-vm-undefined-function
                     state 'mapcarr :source-location loc :suggestions '(mapcar))))
    (let ((json (with-output-to-string (out)
                  (cl-cc/vm:format-condition-json condition out))))
      (expect (search "\"location\"" json) :to-be-truthy)
      (expect (search "foo.lisp" json) :to-be-truthy)
      (expect (search "MAPCAR" json) :to-be-truthy)))) (it-sequential "restart-ui-describes-and-invokes-vm-restart-interactively"
  (let ((called 0))
    (let ((restart (cl-cc/vm::make-vm-restart 'use-value
                                              (lambda (value)
                                                (setf called value)))))
      (setf (cl-cc/vm::vm-restart-description restart) "Use supplied value")
      (setf (cl-cc/vm::vm-restart-interactive-function restart) (lambda () (list 42)))
      (expect (cl-cc/vm:describe-restart restart) :to-equal "Use supplied value")
      (cl-cc/vm:vm-invoke-restart-interactively restart)
      (expect (= 42 called) :to-be-truthy)))))
