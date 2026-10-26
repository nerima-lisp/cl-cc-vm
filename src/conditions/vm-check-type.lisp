(in-package #:cl-cc/vm/conditions)

;;; ── FR-986: Runtime type checking surface ─────────────────────────────────
;;; TYPE-ERROR and its readers are already supplied by conditions.lisp through
;;; VM-TYPE-ERROR inheriting from CL:TYPE-ERROR.  CHECK-TYPE remains the host CL
;;; macro inherited into CL-CC/VM so existing source files keep compiling.

(defun vm-check-type (datum expected-type &optional place)
  "Return DATUM when it satisfies EXPECTED-TYPE, otherwise signal VM-TYPE-ERROR."
  (declare (ignore place))
  (unless (typep datum expected-type)
    (error 'vm-type-error :datum datum :expected-type expected-type))
  datum)

(eval-when (:load-toplevel :execute) (vm-register-host-bridge 'vm-check-type #'vm-check-type))
