;;; runtime-stdlib-2-completion-tests.lisp — Tests for runtime-stdlib-2 gaps

(in-package #:cl-cc/vm/environment/test)

(defun register-runtime-stdlib-2-completion-tests ())


;; ── FR-895: Symbol Table Freeze/Thaw (implementation in vm.lisp) ───────


;; ── FR-896: Package Lock (implementation in vm.lisp) ───────────────────
;; NOTE: lock-package and package-locked-p are defined in vm.lisp

;; ── FR-917: Reproducible Build ─────────────────────────────────────────


;; ── FR-920: Forward References ─────────────────────────────────────────
(it-sequential "runtime-stdlib-2-forward-reference-surface"
  (expect (fboundp 'cl-cc/vm/execution:vm-declare-forward-reference) :to-be-truthy)
  (expect (fboundp 'cl-cc/vm/execution:vm-resolve-forward-references) :to-be-truthy))

;; ── FR-820: Print-Circle ───────────────────────────────────────────────
