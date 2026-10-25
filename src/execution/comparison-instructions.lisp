(in-package #:cl-cc/vm/execution)

(define-vm-binary-instruction vm-eq        :eq        "EQL comparison. Returns 1 if LHS equals RHS, 0 otherwise.")
(declaim (inline %vm-decode-immediate-symbol-for-eql))

(defun %vm-decode-immediate-symbol-for-eql (value)
  (if (vm-immediate-symbol-p value)
      (vm-decode-symbol value)
      value))

(defmethod execute-instruction ((inst vm-eq) state pc labels)
  (declare (ignore labels))
  (let* ((lhs (%vm-decode-immediate-symbol-for-eql
               (vm-reg-get state (vm-lhs inst))))
         (rhs (%vm-decode-immediate-symbol-for-eql
               (vm-reg-get state (vm-rhs inst))))
         (result (if (eql lhs rhs) 1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))
