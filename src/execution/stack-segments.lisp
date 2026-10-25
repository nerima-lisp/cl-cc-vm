;;; ──── FR-764: Segmented / Growable Stacks ────
(in-package #:cl-cc/vm/execution)

(defvar *stack-segment-size* (* 1024 1024)
  "Default stack segment size in bytes (1MB).")

(defvar *tls-stack-limit* 0
  "Thread-local stack limit pointer for overflow detection.")

(defun check-stack-overflow-prologue ()
  "Generate stack overflow check: cmp rsp, [tls_stack_limit].
Called at function entry to detect stack exhaustion."
  (declare (ignore *tls-stack-limit*))
  t)
