(in-package #:cl-cc/vm/execution)

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
