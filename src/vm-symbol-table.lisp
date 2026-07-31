(in-package :cl-cc/vm)

;;; --- Symbol Table (freeze/thaw, lookup, weak symbols) ---
;;; --- FR-622 / FR-896: Package Locks ---
;;;
;;; Extracted from vm.lisp to give the symbol-table subsystem its own SRP
;;; boundary.  Loaded after vm-dsl.lisp in cl-cc-vm.asd.
;;;

;; FR-622: Package locks (enhanced by FR-896)
(defvar *vm-package-locks* (make-hash-table :test #'eq))

(eval-when (:load-toplevel :execute)
  (let ((cl-package (find-package :cl)))
    (when cl-package
      (setf (gethash cl-package *vm-package-locks*) t))))

(defun vm-lock-package (package)
  (setf (gethash package *vm-package-locks*) t) package)

(defun vm-unlock-package (package)
  (remhash package *vm-package-locks*) package)

(defun vm-package-locked-p (package)
  (gethash package *vm-package-locks*))

(defvar *symbol-table* (make-hash-table :test #'equal)
  "Dynamic symbol table mapping string-name -> symbol.
Used by the runtime package layer for intern operations.")

(defvar *symbol-table-frozen* nil
  "When non-NIL, *symbol-table* is frozen and no new symbols may be added.")

(defvar *symbol-table-compact* nil
  "When frozen, a sorted vector of (name-string . symbol) pairs for compact
binary-search lookups. NIL when thawed.")

(defvar *symbol-table-weak* (make-hash-table :test #'equal :weakness :key)
  "Weak hash-table for GC-collectible symbol tracking.
Entries are removed when the symbol is no longer referenced elsewhere.")

(defun %symbol-table-binary-search (pairs name)
  "Binary-search PAIRS (sorted compact vector of (name . symbol)) for NAME."
  (let ((low 0)
        (high (1- (length pairs))))
    (loop
      (when (> low high) (return nil))
      (let* ((mid (floor (+ low high) 2))
             (pair (aref pairs mid))
             (key (car pair)))
        (cond ((string= key name) (return (cdr pair)))
              ((string< key name) (setf low (1+ mid)))
              (t (setf high (1- mid))))))))

(defvar *symbol-index-table* nil
  "Lazily-built hash-table mapping symbol -> sequential index for profiling.")

;;; --- FR-896: Package Lock / Sealed ---

(defvar *locked-packages* (list (find-package :cl))
  "List of locked packages.  Defaults to COMMON-LISP.
Use LOCK-PACKAGE / UNLOCK-PACKAGE to manage.")

(defun lock-package (package &optional (lock t))
  "Lock PACKAGE (when LOCK is non-NIL) or unlock it.
Locked packages prevent new symbol internment, export, import, and shadowing.
Also engages SBCL's native package lock so that CL:INTERN signals an error."
  (if lock
      (progn (pushnew package *locked-packages* :test #'eq)
             (vm-lock-package package)
             (handler-case (sb-ext:lock-package package) (error () nil)))
      (progn (setf *locked-packages* (remove package *locked-packages* :test #'eq))
             (vm-unlock-package package)
             (handler-case (sb-ext:unlock-package package) (error () nil))))
  package)

(defun unlock-package (package)
  "Unlock PACKAGE (convenience wrapper)."
  (lock-package package nil))

;;; Alias package-locked-error to SBCL's native type.
;;; Under selfhost, package-locked-error is a standalone condition type.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (find-class 'package-locked-error)
        (find-class 'sb-ext:package-locked-error)))

(defun check-package-lock (package &optional (operation :intern))
  "Signal PACKAGE-LOCKED-ERROR when PACKAGE is locked.
OPERATION is :INTERN, :EXPORT, :IMPORT, or :SHADOW (for error messages)."
  (declare (ignore operation))
  (when (vm-package-locked-p package)
    (error 'package-locked-error :package package :references nil)))

