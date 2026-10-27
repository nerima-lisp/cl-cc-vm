(in-package #:cl-cc/vm/execution/test)

(defun %vm-symbol-representation-intern (name package)
  (let ((cl-cc/vm:*vm-guest-function-package* :cl-cc/vm))
    (cl-cc/vm:run-compiled
     (cl-cc/vm:make-vm-program
      :instructions
      (list (cl-cc/vm:make-vm-const :dst :r0 :value name)
            (cl-cc/vm:make-vm-const :dst :r1 :value package)
            (cl-cc/vm:make-vm-intern-symbol :dst :result :src :r0 :pkg :r1)
            (cl-cc/vm:make-vm-halt :reg :result))
      :result-register :result))))

(defun register-vm-symbol-representation-tests ()
  (describe-sequential "VM immediate symbol representation"
    (it "recognizes common symbols interned by the VM"
      (let* ((immediate-symbol
               (loop for symbol across cl-cc/vm/execution::*vm-immediate-symbol-table*
                     for package-name = (symbol-name symbol)
                     when (and (eq (symbol-package symbol) (find-package :cl))
                               (not (cl-cc/runtime:rt-find-package package-name))
                               (not (find-package package-name)))
                       return symbol
                     finally
                       (error "No unused common immediate symbol is available")))
             (interned
               (%vm-symbol-representation-intern
                (symbol-name immediate-symbol)
                (find-package :cl))))
        (expect (cl-cc/vm/execution::vm-immediate-symbol-p interned)
                :to-be t)))
    (it "recognizes package-designator candidates as immediate symbols"
      (dolist (symbol (list 'car 'cdr 'cons 'list 'append 'apply 'funcall))
        (let ((interned
                (%vm-symbol-representation-intern
                 (symbol-name symbol)
                 (find-package :cl))))
          (expect (and (cl-cc/vm/execution::vm-immediate-symbol-p interned)
                       (eq (cl-cc/vm/execution::vm-decode-symbol interned)
                           symbol))
                  :to-be t))))
    (it "preserves host package symbol identity when decoding an immediate"
      (let ((interned (%vm-symbol-representation-intern "CAR" (find-package :cl))))
        (expect (and (cl-cc/vm/execution::vm-immediate-symbol-p interned)
                     (eq (cl-cc/vm/execution::vm-decode-symbol interned)
                         'cl:car))
                :to-be t)))))
