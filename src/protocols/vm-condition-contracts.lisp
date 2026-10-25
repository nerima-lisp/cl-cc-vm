(in-package :cl-cc/vm/protocols)

(defgeneric vm-execute-instruction-guarded (instruction state pc labels)
  (:documentation
   "Execute INSTRUCTION, routing host errors through guest handlers."))
(defgeneric vm-fatal-error-p (condition)
  (:documentation
   "Return true when CONDITION is a VM fatal error."))
