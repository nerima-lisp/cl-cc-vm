;;; ──── FR-749: Constant Blinding in AOT Code ────
(in-package #:cl-cc/vm/execution)

(defvar *blind-constants-enabled* nil
  "When T, sensitive constants in AOT code are XOR-masked.")

(defun blind-aot-constant (value &optional (key *jit-random-key*))
  "Blind a sensitive AOT constant VALUE with XOR mask.
Usage: MOV rax, 0xDEADBEEF → MOV rax, (0xDEADBEEF XOR KEY); XOR rax, KEY"
  (if *blind-constants-enabled*
      (logxor value key)
      value))
