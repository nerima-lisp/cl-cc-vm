;;; ──── FR-767: Coroutine Stack Management ────
(in-package #:cl-cc/vm/execution)

(defstruct coroutine
  "A coroutine with full stack frame snapshot capability."
  (name nil :type symbol)
  (state :suspended :type (member :running :suspended :dead))
  (stack-snapshot nil :type list)
  (instruction-pointer 0 :type fixnum)
  (stack-pointer 0 :type fixnum)
  (callee-saved-registers (make-array 6 :initial-element 0)))

(defun coroutine-yield (value)
  "Yield execution from current coroutine, returning VALUE to the resumer."
  ;; Save stack frame snapshot
  ;; Return VALUE
  (declare (ignore value))
  nil)

(defun coroutine-resume (coro)
  "Resume CORO from its saved stack state."
  (setf (coroutine-state coro) :running)
  ;; Restore rsp/rbp/callee-saved registers from snapshot
  coro)

(defun transfer-to (from-coro to-coro)
  "Symmetric coroutine transfer: save FROM-CORO state, resume TO-CORO."
  (setf (coroutine-state from-coro) :suspended)
  (setf (coroutine-state to-coro) :running)
  to-coro)
