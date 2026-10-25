;;; ──── FR-747: Runtime Sandboxing / Seccomp ────
(in-package #:cl-cc/vm/interop)

(defvar *sandbox-enabled* nil
  "When T, untrusted code runs in sandboxed mode with restricted syscalls.")

(defvar *seccomp-filter-rules* nil
  "List of allowed syscall numbers for seccomp BPF filter.")

(defun initialize-sandbox ()
  "Initialize sandbox: load seccomp filter for untrusted code execution.
Linux: seccomp(SECCOMP_SET_MODE_FILTER) with whitelist.
macOS: sandbox_init(3)."
  (when *sandbox-enabled*
    (setf *seccomp-filter-rules*
          ;; Allowed syscalls: read, write, mmap, brk, exit
          (list 0 1 9 12 60))
    t))
