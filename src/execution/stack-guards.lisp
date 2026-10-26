;;; ──── FR-765: Stack Overflow Detection / Guard Pages ────
(in-package #:cl-cc/vm/execution)

(defvar *stack-size* (* 8 1024 1024)
  "Default stack size in bytes (8MB).")

(defun install-stack-guard-page ()
  "Place a guard page at stack end using mmap(PROT_NONE).
SIGSEGV/SIGBUS on access is caught and converted to stack-overflow-error."
  #+(and sbcl linux)
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "mmap" (function sb-alien:unsigned-long
                                    sb-alien:unsigned-long sb-alien:unsigned-long
                                    sb-alien:int sb-alien:int sb-alien:int
                                    sb-alien:long))
   0 4096 0 #x22 -1 0) ;; MAP_PRIVATE | MAP_ANONYMOUS
  t)
