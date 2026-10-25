(in-package :cl-cc/vm/protocols)

(defgeneric %vm-dispatch-call (func state
                                    pc
                                    labels
                                    arg-regs
                                    dst-reg
                                    tail-p
                                    &optional
                                    live-regs)
  (:documentation "Dispatch an executable function using VM call metadata."))

(defgeneric %vm-method-function (method)
  (:documentation
   "Return the callable function represented by a CLOS method descriptor."))

(defgeneric %vm-ret-qualified-dispatch (state result labels method-entry)
  (:documentation "Continue qualified method dispatch after a method returns."))

(defgeneric vm-resolve-gf-method (gf-ht state first-arg &optional all-args)
  (:documentation "Resolve an applicable generic-function method closure."))

(defgeneric vm-maybe-dump-type-profile (state)
  (:documentation "Optionally emit collected type feedback after VM execution."))
