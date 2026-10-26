(in-package #:cl-cc/vm/environment)

;; FR-848: Thread-Local Storage / TLS
(defmacro defvar-tls (name &optional (init nil))
  `(progn (defvar ,name ,init)
          (setf (sb-thread:symbol-value-in-thread ',name sb-thread:*current-thread*) ,init)))
