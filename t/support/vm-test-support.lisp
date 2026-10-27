(in-package #:cl-cc/vm-test-support)

(defun make-test-vm ()
  (cl-cc/vm:make-vm-state))

(defmacro with-test-vm ((state &rest bindings) &body body)
  `(let ((,state (make-test-vm)))
     ,@(mapcar
        (lambda (binding)
          (destructuring-bind (register value) binding
            `(cl-cc/vm:vm-reg-set ,state ,register ,value)))
        bindings)
     ,@body))

(defun vm-exec (inst state
                     &optional
                     (pc 0)
                     (labels (make-hash-table
                              :test
                              #'equal)))
  (cl-cc/vm:execute-instruction inst state pc labels))

(defun exec1 (inst state &optional (pc 0))
  (vm-exec inst state pc (make-hash-table)))

(defun %run-unary-inst-with (instruction-thunk src-val)
  "Run INSTRUCTION-THUNK against SRC-VAL and return the destination register.
INSTRUCTION-THUNK receives the source register index and must build the VM instruction."
  (with-test-vm (state (1 src-val))
    (exec1 (funcall instruction-thunk 1) state)
    (cl-cc/vm:vm-reg-get state 0)))
(defun %run-unary-inst (ctor-fn src-val)
  "Run a unary VM instruction constructor against SRC-VAL and return DST."
  (%run-unary-inst-with
   (lambda (src)
     (funcall ctor-fn :dst 0 :src src))
   src-val))
(defun %run-binary-inst (ctor-fn lhs rhs)
  "Run a binary VM instruction constructor against LHS/RHS and return DST."
  (with-test-vm (state (1 lhs) (2 rhs))
    (exec1 (funcall ctor-fn :dst 0 :lhs 1 :rhs 2) state)
    (cl-cc/vm/protocols:vm-reg-get state 0)))
