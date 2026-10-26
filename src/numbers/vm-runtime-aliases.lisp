(in-package #:cl-cc/vm/numbers)

(defmacro define-vm-alias (vm-name host-name (&rest args))
  "Define VM-NAME as the guest-visible ARGS-arity alias for HOST-NAME."
  `(defun ,vm-name (,@args) (,host-name ,@args)))

(define-vm-alias vm-clamp clamp (value min max))

(define-vm-alias vm-wrap wrap (value min max))

(define-vm-alias vm-lerp lerp (start end amount))

(define-vm-alias vm-signum signum (value))
