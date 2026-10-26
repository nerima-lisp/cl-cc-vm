(in-package #:cl-cc/vm/protocols)

(progn
  (defgeneric vm-copy-instance (instance))
  (defgeneric vm-state-symbol-plists (state))
  (defgeneric vm-state-system-symbol-plists (state))
  (defgeneric vm-state-symbol-plist-lock (state))
  (defgeneric vm-state-symbol-plist-read-barrier (state))
  (defgeneric (setf vm-state-symbol-plist-read-barrier) (value state)))
