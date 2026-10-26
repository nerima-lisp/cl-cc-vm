(in-package #:cl-cc/vm/execution)

(define-vm-instruction vm-progv-enter (vm-instruction)
  "Save current global bindings for SYMS-REG, then bind each to the
   corresponding value in VALS-REG.  Store the saved alist in DST so
   vm-progv-exit can restore it."
  (dst nil :reader vm-dst)
  (syms nil :reader vm-syms-reg)
  (vals nil :reader vm-vals-reg)
  (:sexp-tag :progv-enter)
  (:sexp-slots dst syms vals))

(defmethod execute-instruction ((inst vm-progv-enter) state pc labels)
  (declare (ignore labels))
  (let* ((syms (vm-reg-get state (vm-syms-reg inst)))
         (vals (vm-reg-get state (vm-vals-reg inst)))
         (global-vars (vm-global-vars state))
         (saved nil))
    ;; Save old bindings (or :unbound sentinel)
    (dolist (sym syms)
      (multiple-value-bind (old-val found-p) (gethash sym global-vars)
        (push (cons sym (if found-p old-val :unbound)) saved)))
    ;; Bind new values
    (let ((val-ptr vals))
      (dolist (sym syms)
        (if val-ptr
            (progn
              (setf (gethash sym global-vars) (car val-ptr))
              (setf val-ptr (cdr val-ptr)))
            (setf (gethash sym global-vars) nil))))
    ;; Return saved alist so vm-progv-exit can restore
    (vm-reg-set state (vm-dst inst) (nreverse saved))
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-progv-exit (vm-instruction)
  "Restore global bindings from the saved alist produced by vm-progv-enter."
  (saved nil :reader vm-saved-reg)
  (:sexp-tag :progv-exit)
  (:sexp-slots saved))

(defmethod execute-instruction ((inst vm-progv-exit) state pc labels)
  (declare (ignore labels))
  (let ((saved (vm-reg-get state (vm-saved-reg inst)))
        (global-vars (vm-global-vars state)))
    (dolist (pair saved)
      (let ((sym (car pair))
            (old-val (cdr pair)))
        (if (eq old-val :unbound)
            (remhash sym global-vars)
            (setf (gethash sym global-vars) old-val))))
    (values (1+ pc) nil nil)))
