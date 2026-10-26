(in-package #:cl-cc-vm/test)
(describe-sequential "execution feature API"
  (it "reads back a value written to a VM register"
    (let ((state (cl-cc/vm/execution:make-vm-state)))
      (expect
       (cl-cc/vm/protocols:vm-reg-set state 7 :feature-test-value)
       :to-be :feature-test-value)
      (expect
       (cl-cc/vm/protocols:vm-reg-get state 7)
       :to-be :feature-test-value)))
  (it "preserves the legacy VM2 initial global bindings"
    (let* ((state (cl-cc/vm/execution:make-vm2-state))
           (globals (cl-cc/vm/execution:vm2-state-global-vars state))
           (environment-package (find-package "CL-CC/VM/ENVIRONMENT"))
           (conditions-package (find-package "CL-CC/VM/CONDITIONS"))
           (debugging-package (find-package "CL-CC/VM/DEBUGGING"))
           (features-symbol (find-symbol "*FEATURES*" environment-package))
           (active-restarts-symbol
             (find-symbol "*ACTIVE-RESTARTS*" conditions-package))
           (trace-output-symbol
             (find-symbol "*TRACE-OUTPUT*" debugging-package))
           (expected
             (list
              (cons features-symbol (quote (:common-lisp :cl-cc)))
              (cons active-restarts-symbol nil)
              (cons (quote cl:*standard-output*) *standard-output*)
              (cons (quote cl:*standard-input*) *standard-input*)
              (cons (quote cl:*error-output*) *error-output*)
              (cons trace-output-symbol (symbol-value trace-output-symbol))
              (cons (quote cl:*debug-io*) *debug-io*)
              (cons (quote cl:*query-io*) *query-io*))))
      (expect (hash-table-count globals) :to-be 8)
      (expect
       (and (eq (symbol-package features-symbol) environment-package)
            (eq (symbol-package active-restarts-symbol) conditions-package)
            (eq (symbol-package trace-output-symbol) debugging-package))
       :to-be t)
      (expect
       (every (lambda (entry)
                (equal (gethash (car entry) globals :missing)
                       (cdr entry)))
              expected)
       :to-be t))))
