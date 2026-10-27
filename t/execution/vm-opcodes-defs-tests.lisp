;;;; tests/unit/vm/vm-opcodes-defs-tests.lisp
;;;; Unit tests for src/vm/vm-opcodes-defs.lisp public API.
;;;;
;;;; Covers:
;;;;   make-vm-state        — public constructor for canonical execution state
;;;;   vm-reg-get/vm-reg-set — polymorphic register accessors across state implementations
;;;;   vm-state-registers   — generic method on vm2-state
;;;;   vm-output-stream     — generic method on vm2-state
;;;;   vm-global-vars       — generic method on vm2-state
;;;;   run-vm-with-opcode-bigrams — bigram profiling variant of run-vm

(in-package #:cl-cc/vm/execution/test)

;;; ─── Helper ─────────────────────────────────────────────────────────────────

(defun make-bytecode2 (&rest words)
  "Build a simple-vector from WORDS for vm2 bytecode tests."
  (coerce words 'simple-vector))

;;; ─── make-vm-state ──────────────────────────────────────────────────────────

(defun register-vm-opcodes-defs-tests ()
  ;;; ─── vm-state-registers / vm-output-stream / vm-global-vars (generics) ─────

  (it-sequential "vm-opcodes-defs-vm-state-registers-returns-hash-table"
                 (let* ((s    (cl-cc/vm/execution:make-vm-state))
                        (regs (cl-cc/vm/execution:vm-state-registers s)))
                   (expect (hash-table-p regs) :to-be-truthy)))

  (it-sequential "vm-opcodes-defs-vm-global-vars-returns-hash-table"
                 (let ((s (cl-cc/vm/execution:make-vm-state)))
                   (expect (hash-table-p (cl-cc/vm/execution:vm-global-vars s)) :to-be-truthy)))

;;; ─── vm-reg-get / vm-reg-set on public VM state ─────────────────────────────

  (it-sequential "vm-opcodes-defs-reg-get-fresh-register-nil"
                 (let ((s (cl-cc/vm/execution:make-vm-state)))
                   (expect (= 0 (cl-cc/vm/protocols:vm-reg-get s 0)) :to-be-truthy)
                   (expect (= 0 (cl-cc/vm/protocols:vm-reg-get s 127)) :to-be-truthy)
                   (expect (= 0 (cl-cc/vm/protocols:vm-reg-get s 255)) :to-be-truthy)))

  (it-sequential "vm-opcodes-defs-reg-set-returns-stored-value"
                 (let ((s (cl-cc/vm/execution:make-vm-state)))
                   (let ((ret (cl-cc/vm/protocols:vm-reg-set s 0 42)))
                     (expect (= 42 ret) :to-be-truthy)
                     (expect (= 42 (cl-cc/vm/protocols:vm-reg-get s 0)) :to-be-truthy))))

  (it-sequential "vm-opcodes-defs-reg-set-overwrite-takes-last"
                 (let ((s (cl-cc/vm/execution:make-vm-state)))
                   (cl-cc/vm/protocols:vm-reg-set s 3 :first)
                   (cl-cc/vm/protocols:vm-reg-set s 3 :second)
                   (expect (cl-cc/vm/protocols:vm-reg-get s 3) :to-be :second)))

  (it-sequential "vm-opcodes-defs-reg-set-adjacent-registers-independent"
                 (let ((s (cl-cc/vm/execution:make-vm-state)))
                   (cl-cc/vm/protocols:vm-reg-set s 10 'alpha)
                   (cl-cc/vm/protocols:vm-reg-set s 11 'beta)
                   (expect (cl-cc/vm/protocols:vm-reg-get s 10) :to-be 'alpha)
                   (expect (cl-cc/vm/protocols:vm-reg-get s 11) :to-be 'beta)))

  (it-sequential "vm-opcodes-defs-reg-roundtrip-all-slots"
                 (let ((s (cl-cc/vm/execution:make-vm-state)))
                   (dotimes (i 64)
                     (cl-cc/vm/protocols:vm-reg-set s i i))
                   (dotimes (i 64)
                     (expect (= i (cl-cc/vm/protocols:vm-reg-get s i)) :to-be-truthy))))

;;; ─── vm2 low-level constructor remains available explicitly ────────────────

  (it-sequential "vm-opcodes-defs-make-vm2-state-returns-vm2-state"
                 (let ((s (cl-cc/vm:make-vm2-state)))
                   (expect (cl-cc/vm:vm2-state-p s) :to-be-truthy)))

  (it-sequential "vm-opcodes-defs-make-vm2-state-accepts-custom-output-stream"
                 (let* ((str (make-string-output-stream))
                        (s   (cl-cc/vm:make-vm2-state :output-stream str)))
                   (expect (cl-cc/vm:vm2-state-output-stream s) :to-be str)))

;;; ─── run-vm-with-opcode-bigrams ─────────────────────────────────────────────

  )
;;; ─── run-vm-with-opcode-bigrams ─────────────────────────────────────────────

;;; ─── vm-state-registers / vm-output-stream / vm-global-vars (generics) ─────

;;; ─── vm-reg-get / vm-reg-set on public VM state ─────────────────────────────

;;; ─── vm2 low-level constructor remains available explicitly ────────────────

;;; ─── run-vm-with-opcode-bigrams ─────────────────────────────────────────────

