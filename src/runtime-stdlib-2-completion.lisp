;;; ─── Runtime Stdlib-2 Completion ─────────────────────────────────────
;;; Fills remaining gaps from docs/notes/runtime-stdlib-2.md (Phases 138-175).
;;; Features already implemented elsewhere (primitives.lisp, macros-lazy.lisp,
;;; vm-clos.lisp, stream.lisp, etc.) are NOT duplicated here.

(in-package :cl-cc/vm)

;; NOTE: FR-895 (freeze/thaw symbol table) and FR-896 (package lock)
;; are already implemented in vm.lisp lines 841-965 with superior
;; implementations (binary search compact vector, weak references).
;; Do NOT override those definitions.

;; ── FR-917: Reproducible Build Support ─────────────────────────────────

(defconstant +unix-to-universal-time-offset+ 2208988800
  "Seconds between the Common Lisp and Unix epochs.")

(defparameter *build-seed* nil
  "When non-NIL, fixed seed for reproducible builds; NIL preserves defaults.")

(defparameter *deterministic-hash-table-seed* nil
  "Stable hash seed used by deterministic hash helpers.")

(defun %parse-non-negative-integer (value)
  (when value
    (handler-case
        (let ((n (parse-integer value :junk-allowed nil)))
          (and (not (minusp n)) n))
      (error () nil))))

(defun source-date-epoch ()
  "Return SOURCE_DATE_EPOCH as Unix seconds, or NIL when unset/invalid."
  (%parse-non-negative-integer (host-kit:getenv "SOURCE_DATE_EPOCH")))

(defun build-timestamp ()
  "Return build timestamp as universal-time, honoring SOURCE_DATE_EPOCH."
  (let ((epoch (source-date-epoch)))
    (if epoch
        (+ epoch +unix-to-universal-time-offset+)
        (cl:get-universal-time))))

;; ── FR-920: Forward References ─────────────────────────────────────────

(defstruct vm-forward-reference-cell
  "Mutable function cell used for top-level forward references."
  (name nil :read-only t)
  (value nil))

(defvar *vm-unresolved-forward-refs* nil
  "Alist of (NAME . VM-FORWARD-REFERENCE-CELL) entries still unresolved.")

(defvar *vm-forward-reference-auto-resolve-enabled* t
  "When true, RUN-COMPILED performs a final forward-reference warning pass.")

(defun vm-forward-reference-cell-ref (cell)
  "Return CELL's current function value, or NIL when unresolved."
  (vm-forward-reference-cell-value cell))

(defun (setf vm-forward-reference-cell-ref) (value cell)
  "Publish VALUE into forward-reference CELL."
  (setf (vm-forward-reference-cell-value cell) value))

(defun vm-declare-forward-reference (state name)
  "Declare NAME as a forward-referenced function in STATE and return its cell."
  (let* ((registry (vm-function-registry state))
         (existing (gethash name registry)))
    (cond
      ((vm-forward-reference-cell-p existing) existing)
      (existing
       (let ((cell (make-vm-forward-reference-cell :name name :value existing)))
         (setf (gethash name registry) cell)
         cell))
      (t
       (let ((cell (make-vm-forward-reference-cell :name name)))
         (setf (gethash name registry) cell)
         (pushnew (cons name cell) *vm-unresolved-forward-refs* :key #'car :test #'eq)
         cell)))))

(defun vm-resolve-forward-references (&optional (state *vm-current-state*))
  "Warn for forward-reference cells in STATE that remain unresolved."
  (declare (ignore state))
  (let ((resolved nil)
        (unresolved nil)
        (remaining nil))
    (dolist (entry *vm-unresolved-forward-refs*)
      (let ((name (car entry))
            (cell (cdr entry)))
        (if (and (vm-forward-reference-cell-p cell)
                 (vm-forward-reference-cell-value cell))
            (push name resolved)
            (progn
              (push name unresolved)
              (push entry remaining)
              (warn "Unresolved forward reference: ~S" name)))))
    (setf *vm-unresolved-forward-refs* (nreverse remaining))
    (values (nreverse resolved) (nreverse unresolved))))
