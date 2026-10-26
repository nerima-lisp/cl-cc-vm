;;; ──── FR-766: Trampolining ────
(in-package #:cl-cc/vm/execution)

(defvar *trampoline-optimization-enabled* t
  "When T, automatically convert mutual recursion to trampolines.")

(defun make-thunk (fn)
  "Create a zero-argument thunk for FN (for trampoline use)."
  (lambda () (funcall fn)))

(defun trampoline (thunk)
  "Execute THUNK repeatedly in a trampoline loop.
Each iteration calls the thunk which returns either a value or a new thunk."
  (loop with result = (funcall thunk)
        while (functionp result)
        do (setf result (funcall result))
        finally (return result)))
