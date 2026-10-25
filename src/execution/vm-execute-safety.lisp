(in-package :cl-cc/vm)

;;;; Runtime safety checks: OPTIMIZE-SAFETY-derived bounds/type checking and
;;;; stack-protection canaries, plus the execute-instruction methods for the
;;;; bounds-checked array/vector instructions that consult them.

(defparameter *safety-level* 1
  "Runtime safety level corresponding to OPTIMIZE (SAFETY N): 0 skips checks, 1 enables bounds/type checks, 3 also protects the stack.")

(defparameter *security-canaries* nil
  "When true, VM call frames carry random stack canaries verified on return.")

(define-condition memory-fault (error)
  ((reason :initarg :reason :reader memory-fault-reason))
  (:report (lambda (condition stream)
             (format stream "VM memory fault: ~A" (memory-fault-reason condition)))))

(defun vm-safety-level ()
  (max 0 (min 3 (or *safety-level* 0))))

(defun vm-safety-checks-enabled-p ()
  (>= (vm-safety-level) 1))

(defun vm-stack-protection-enabled-p ()
  (or *security-canaries* (>= (vm-safety-level) 3)))

(defun vm-random-canary ()
  (logxor (random most-positive-fixnum) (get-universal-time)))

(defun vm-check-index (sequence index &optional operation)
  "Check INDEX for SEQUENCE when safety is enabled; signal TYPE-ERROR on failure."
  (declare (ignore operation))
  (when (vm-safety-checks-enabled-p)
    (unless (and (integerp index) (<= 0 index) (< index (length sequence)))
      (error 'type-error
             :datum index
             :expected-type `(integer 0 ,(max 0 (1- (length sequence)))))))
  index)

(defun vm-check-row-major-index (array index)
  (when (vm-safety-checks-enabled-p)
    (unless (and (integerp index) (<= 0 index) (< index (array-total-size array)))
      (error 'type-error
             :datum index
             :expected-type `(integer 0 ,(max 0 (1- (array-total-size array)))))))
  index)

(defun vm-verify-stack-canary (saved-regs)
  (when (vm-stack-protection-enabled-p)
    (multiple-value-bind (canary foundp) (gethash :__stack-canary__ saved-regs)
      (unless (and foundp (eql canary (gethash :__stack-canary-check__ saved-regs)))
        (error 'memory-fault :reason :stack-canary-mismatch)))))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — execute-instruction Methods for Core Instructions
;;;
;;; Contains: vm-resolve-function, vm-falsep, call-frame helpers
;;; (vm-save/restore-registers, vm-push-call-frame, vm-bind-closure-args),
;;; vm-list-to-lisp-list, multi-dispatch helpers (vm-classify-arg,
;;; vm-resolve-gf-method, vm-resolve-multi-dispatch, vm-get-all-applicable-methods,
;;; vm-dispatch-generic-call, %vm-dispatch-call),
;;; and execute-instruction methods for: const, move, add/sub/mul, label,
;;; jump, jump-zero, print, halt, closure, call, tail-call, ret, func-ref,
;;; make-closure, closure-ref-idx, values, mv-bind, values-to-list,
;;; spread-values, clear-values, ensure-values, next-method-p,
;;; call-next-method, apply, register-function, set-global, get-global.
;;;
;;; Load order: after vm-dispatch.lisp, before vm-clos.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defmethod execute-instruction ((inst vm-const) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (vm-value inst))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-move) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (vm-reg-get state (vm-src inst)))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-prefetch) state pc labels)
  "Execute a cache prefetch hint as a semantic no-op in the VM interpreter."
  (declare (ignore inst state labels))
  (values (1+ pc) nil nil))
