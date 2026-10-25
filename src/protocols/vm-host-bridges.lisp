(in-package :cl-cc/vm/protocols)

;;; VM Host Callable Registration
;;; Owns the synchronized registries and explicit registration APIs for host
;;; and runtime callables. Lookup and invocation live in interop.

(defvar *vm-host-bridge-functions*
  (make-hash-table :test #'eq :synchronized t)
  "Map of bridgeable symbols to their callable implementation.
All entries must store an explicit function object.
Synchronized: parallel test workers populate this concurrently.")

(defvar *vm-runtime-callables*
  (make-hash-table :test #'equal :synchronized t)
  "Runtime callable registry keyed by exported runtime helper name string.
Synchronized: parallel test workers populate this concurrently.")

(defun vm-register-host-bridge (sym &optional callable)
  "Register SYM as a bridgeable function for the VM.
CALLABLE must be an explicit function object."
  (unless callable
    (error "Cannot register host bridge symbol without explicit callable: ~S" sym))
  (setf (gethash sym *vm-host-bridge-functions*) callable))

(defun vm-register-runtime-callable (name callable)
  "Register a runtime helper CALLABLE under NAME for VM bridge use."
  (unless callable
    (error "Cannot register runtime callable without a function: ~S" name))
  (setf (gethash name *vm-runtime-callables*) callable))
