(in-package :cl-cc/vm/protocols)

(defvar *vm-debugger-request-hook* nil)

(defun vm-request-debugger (condition &optional vm-state labels)
  (if *vm-debugger-request-hook*
      (funcall *vm-debugger-request-hook* condition vm-state labels)
      (cl:invoke-debugger condition)))
