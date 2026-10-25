;;; Runtime Eval — Meta-circular evaluation

(in-package #:cl-cc/vm/execution)

(define-vm-unary-instruction vm-eval :eval
  "Evaluate a form at runtime by compiling and running it in a fresh VM.
This enables meta-circular self-hosting: compiled code can call eval.")

(defmethod execute-instruction ((inst vm-eval) state pc labels)
  (declare (ignore labels))
  (let* ((form (vm-reg-get state (vm-src inst)))
         (result (funcall *vm-eval-hook* form)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

;;; FR-631: Macro expansion at runtime

(define-vm-unary-instruction vm-macroexpand-1-inst :macroexpand-1
  "Expand a macro form once. Returns the (possibly unexpanded) form.")

(defmethod execute-instruction ((inst vm-macroexpand-1-inst) state pc labels)
  (declare (ignore labels))
  (let* ((form (vm-reg-get state (vm-src inst)))
         (result (funcall *vm-macroexpand-1-hook* form)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(define-vm-unary-instruction vm-macroexpand-inst :macroexpand
  "Fully expand a macro form. Returns the expanded form.")

(defmethod execute-instruction ((inst vm-macroexpand-inst) state pc labels)
  (declare (ignore labels))
  (let* ((form (vm-reg-get state (vm-src inst)))
         (result (funcall *vm-macroexpand-hook* form)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))
