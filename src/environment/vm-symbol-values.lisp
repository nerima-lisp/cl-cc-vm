(in-package #:cl-cc/vm/environment)

(define-vm-binary-instruction vm-set-symbol-value :set-symbol-value
  "Set the dynamic value cell of LHS to RHS and return RHS.")

(defmethod execute-instruction ((inst vm-set-symbol-value) state pc labels)
  (declare (ignore labels))
  (let* ((sym (vm-reg-get state (vm-lhs inst)))
         (val (vm-reg-get state (vm-rhs inst))))
    (setf (gethash sym (vm-global-vars state)) val)
    (setf (symbol-value sym) val)
    (when cl-cc/bootstrap:*runtime-set-symbol-value-fn*
      (funcall cl-cc/bootstrap:*runtime-set-symbol-value-fn* sym val))
    (vm-reg-set state (vm-dst inst) val)
    (values (1+ pc) nil nil)))
