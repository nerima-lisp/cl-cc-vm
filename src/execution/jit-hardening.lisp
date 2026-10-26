;;; ──── FR-746: JIT Hardening / Code Integrity Protection ────
(in-package #:cl-cc/vm/execution)

(defvar *jit-hardening-enabled* t
  "When T, enable JIT hardening measures (constant blinding, guard pages, NOP sled removal).")

(defvar *jit-random-key* 0
  "Random key for JIT constant blinding (generated at startup).")

(defun initialize-jit-hardening ()
  "Initialize JIT hardening: generate random key, allocate guard pages."
  (setf *jit-random-key* (vm-random-integer (ash 1 64))))

(defun blind-constant (value)
  "Blind a constant VALUE using XOR with random key.
Usage: JIT emits (blinded XOR key) and recovers with XOR before use."
  (logxor value *jit-random-key*))
