;;;; packages/vm/tests/runtime-stdlib-2-vm-tests.lisp
;;;; FR-787–FR-931: Symbol-existence smoke tests for runtime-stdlib-2 features.
;;;;
;;;; Each test asserts that the listed symbol is fboundp / boundp / a loadable package.
;;;; Groups are by check type; FR labels are encoded in the case label strings.

(in-package #:cl-cc/vm/execution/test)



(defun register-runtime-stdlib-2-vm-tests ()
  (it-sequential "runtime-stdlib-2-vm-system-loads"
                 :timeout
                 5
                 (expect (asdf:find-system :cl-cc-vm nil) :to-be-truthy))

;;; ─── Multi-symbol FR groups (cannot collapse to single-entry rows) ─────────

  (it-sequential "fr-791-logging-exists"
                 :timeout
                 5
                 (expect (fboundp 'cl-cc/runtime:log-error) :to-be-truthy)
                 (expect (fboundp 'cl-cc/runtime:log-warn) :to-be-truthy)
                 (expect (fboundp 'cl-cc/runtime:log-info) :to-be-truthy))

  (it-sequential "fr-792-metrics-exists"
                 :timeout
                 5
                 (expect (fboundp 'cl-cc/runtime:rt-make-counter) :to-be-truthy)
                 (expect (fboundp 'cl-cc/runtime:rt-counter-increment!) :to-be-truthy)
                 (expect (fboundp 'cl-cc/runtime:rt-make-histogram) :to-be-truthy))

  (it-sequential "fr-793-perf-counters-exists"
                 :timeout
                 5
                 (expect (fboundp 'cl-cc/runtime:rt-perf-init) :to-be-truthy)
                 (expect (fboundp 'cl-cc/runtime:rt-with-perf-counters) :to-be-truthy))

  (it-sequential "fr-812-c-embedding-exists"
                 :timeout
                 5
                 (expect (fboundp 'cl-cc/runtime:cl-cc-init) :to-be-truthy)
                 (expect (fboundp 'cl-cc/runtime:cl-cc-eval) :to-be-truthy)
                 (expect (fboundp 'cl-cc/runtime:cl-cc-call) :to-be-truthy))

;;; ─── Package existence ───────────────────────────────────────────────────────

  ;;; ─── Single-function fboundp checks ─────────────────────────────────────────

  (it-sequential "fr-function-exists fr-800-callcc"
                 (destructuring-bind (sym) (list 'cl-cc/vm/execution::call-with-current-continuation)
                   (expect (fboundp sym) :to-be-truthy)))

  (it-sequential "fr-function-exists fr-801-escape"
                 (destructuring-bind (sym) (list 'cl-cc/vm/execution:vm-call/cc)
                   (expect (fboundp sym) :to-be-truthy)))

  (it-sequential "fr-function-exists fr-813-multi-vm"
                 (destructuring-bind (sym) (list 'cl-cc/vm/execution:make-vm-instance)
                   (expect (fboundp sym) :to-be-truthy)))

  (it-sequential "fr-function-exists fr-816-arena"
                 (destructuring-bind (sym) (list 'cl-cc/runtime:make-arena)
                   (expect (fboundp sym) :to-be-truthy)))

  (it-sequential "fr-function-exists fr-817-object-pool"
                 (destructuring-bind (sym) (list 'cl-cc/runtime:make-object-pool)
                   (expect (fboundp sym) :to-be-truthy)))

  (it-sequential "fr-function-exists fr-828-stack-canary"
                 (destructuring-bind (sym) (list 'cl-cc/vm/execution::vm-verify-stack-canary)
                   (expect (fboundp sym) :to-be-truthy)))

  (it-sequential "fr-function-exists fr-847-mutex"
                 (destructuring-bind (sym) (list 'cl-cc/vm/execution::make-mutex)
                   (expect (fboundp sym) :to-be-truthy)))

  (it-sequential "fr-function-exists fr-848-rwlock"
                 (destructuring-bind (sym) (list 'cl-cc/vm/execution::make-rwlock)
                   (expect (fboundp sym) :to-be-truthy)))

  (it-sequential "fr-function-exists fr-865-mv-apply"
                 (destructuring-bind (sym) (list 'cl-cc/vm/execution:vm-nth-value)
                   (expect (fboundp sym) :to-be-truthy)))

  (it-sequential "fr-876-segmented-stack-api-exists"
                 (dolist (sym (list (quote cl-cc/runtime::grow-stack-segment)
                                    (quote cl-cc/runtime::release-stack-segment)
                                    (quote cl-cc/runtime::stack-segment-ensure-space)
                                    (quote cl-cc/runtime::stack-segment-note-frame)
                                    (quote cl-cc/runtime::stack-segment-release-frame)))
                   (expect (fboundp sym) :to-be-truthy)))

  (it-sequential "fr-function-exists fr-877-copying-stack"
                 (destructuring-bind (sym) (list 'cl-cc/runtime::relocate-stack-pointers)
                   (expect (fboundp sym) :to-be-truthy)))

  (it-sequential "fr-function-exists fr-892-load-time-value"
                 (destructuring-bind (sym) (list 'cl-cc/vm/execution:vm-load-time-value)
                   (expect (fboundp sym) :to-be-truthy)))

  ;;; ─── Single-variable boundp checks ──────────────────────────────────────────

  (it-sequential "fr-variable-exists fr-827-bounds-check"
                 (destructuring-bind (sym) (list 'cl-cc/vm/execution::*safety-level*)
                   (expect (boundp sym) :to-be-truthy)))

  (it-sequential "fr-variable-exists fr-834-jit-thresholds"
                 (destructuring-bind (sym) (list 'cl-cc/vm/execution:*jit-tier1-threshold*)
                   (expect (boundp sym) :to-be-truthy)))

  (it-sequential "fr-variable-exists fr-864-mv-frame"
                 (destructuring-bind (sym) (list 'cl-cc/vm/execution:+maximum-multiple-values+)
                   (expect (boundp sym) :to-be-truthy)))

  )

;;; ─── Multi-symbol FR groups (cannot collapse to single-entry rows) ─────────

;;; ─── Package existence ───────────────────────────────────────────────────────

;;; ─── Single-function fboundp checks ─────────────────────────────────────────

;;; ─── Single-variable boundp checks ──────────────────────────────────────────

