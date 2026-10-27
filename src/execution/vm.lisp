(in-package #:cl-cc/vm/execution)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow '(lisp-implementation-type lisp-implementation-version
            machine-type machine-version machine-instance
            software-type software-version short-site-name long-site-name
            room apropos apropos-list)))

;;; VM Heap Object Base Class

(defclass vm-heap-object ()
  ()
  (:documentation "Base class for all objects that can be stored on the VM heap.
Provides a common supertype for heap-allocated VM objects like cons cells,
closures, and reader states."))

(defmethod vm-getenv ((name t))
  "Return environment variable NAME marked as tainted external input."
  (let ((value (host-kit:getenv (string name))))
    (and value (taint-mark value :environment))))

;;; VM Heap Address Wrapper

(defstruct vm-heap-address
  "Wrapper for heap addresses to distinguish them from regular integers."
  (value 0 :type integer))

;;; VM Closure Object

(defparameter *vm-current-compilation-tier* 0
  "Compilation tier associated with closures allocated by the currently running program.")

(defclass vm-closure-object (vm-heap-object)
  ((entry-label :initarg :entry-label :reader vm-closure-entry-label
                :documentation "Label where function code begins")
   (dispatch-tag :initarg :dispatch-tag :initform nil :accessor vm-closure-dispatch-tag
                 :documentation "Optional defunctionalization dispatch tag for known closures.")
   (params :initarg :params :reader vm-closure-params
            :documentation "List of required parameter register names")
   (optional-params :initarg :optional-params :initform nil :reader vm-closure-optional-params
                    :documentation "List of (register default-value) for &optional")
   (rest-param :initarg :rest-param :initform nil :reader vm-closure-rest-param
                :documentation "Register name for &rest parameter, or nil")
   (key-params :initarg :key-params :initform nil :reader vm-closure-key-params
                :documentation "List of (keyword register default-value) for &key")
    (rest-stack-alloc-p :initarg :rest-stack-alloc-p :initform nil :reader vm-closure-rest-stack-alloc-p
                        :documentation "T when the &rest list may be stack-allocated")
    (captured-regs :initarg :captured-regs :initform #() :accessor vm-closure-captured-regs
                   :documentation "Captured lexical environment register names as a flat vector.")
   (captured-vals :initarg :captured-vals :initform #() :accessor vm-closure-captured-vals
                   :documentation "Captured lexical environment values as a flat vector parallel to CAPTURED-REGS.")
    (deopt-info :initarg :deopt-info :initform nil :accessor vm-closure-deopt-info
                :documentation "PC -> deoptimization reconstruction metadata for optimized closures.")
    (osr-entry-points :initarg :osr-entry-points :initform nil :accessor vm-closure-osr-entry-points
                      :documentation "Loop back-edge OSR entry metadata for this closure.")
    (tier1-entry-points :initarg :tier1-entry-points :initform nil :accessor vm-closure-tier1-entry-points
                        :documentation "Optional Tier-1 entry metadata used by the OSR stub.")
    (invocation-count :initarg :invocation-count :initform 0 :accessor vm-closure-invocation-count
                      :documentation "Dynamic invocation count used by tiered compilation.")
    (compilation-tier :initarg :compilation-tier :initform *vm-current-compilation-tier* :accessor vm-closure-compilation-tier
                      :documentation "Compilation tier for this closure: 0 fast, 1 optimized.")
    (program-flat :initarg :program-flat :initform nil :accessor vm-closure-program-flat
                  :documentation "Optional flat instruction vector that owns this closure's entry label.")
   (label-table :initarg :label-table :initform nil :accessor vm-closure-label-table
                :documentation "Optional label table paired with PROGRAM-FLAT for cross-program calls."))
  (:documentation "Represents a closure with code and captured environment."))

(defparameter *tier-upgrade-threshold* 50
  "Number of calls after which a Tier-0 closure is upgraded to Tier-1.")

(defvar *vm-recompile-function-hook* nil
  "Optional function called as (HOOK CLOSURE TARGET-TIER) for runtime tier upgrades.")

(defvar *vm-current-state* nil
  "Dynamically bound to the VM state while invoking host bridge callables.")

(defun vm-closure-note-invocation (closure)
  "Increment CLOSURE's invocation counter and return the new count."
  (incf (vm-closure-invocation-count closure)))

(defun vm-maybe-tier-upgrade-closure (closure &optional (target-tier 1))
  "Upgrade hot Tier-0 CLOSURE to TARGET-TIER using *VM-RECOMPILE-FUNCTION-HOOK*."
  (when (and (typep closure 'vm-closure-object)
             (< (vm-closure-compilation-tier closure) target-tier)
             (> (vm-closure-invocation-count closure) *tier-upgrade-threshold*))
    (when *vm-recompile-function-hook*
      (funcall *vm-recompile-function-hook* closure target-tier))
    (setf (vm-closure-compilation-tier closure) target-tier))
  closure)

;;; VM Cons Cell (Heap-based)

(defclass vm-cons-cell (vm-heap-object)
  ((car :initarg :car :accessor vm-cons-cell-car
        :documentation "The car (first) element of the cons cell")
   (cdr :initarg :cdr :accessor vm-cons-cell-cdr
        :documentation "The cdr (second) element of the cons cell"))
  (:documentation "Heap-allocated cons cell for VM list operations."))

(defmethod vm-closure-object-p ((object vm-closure-object)) (declare (ignore object)) t)

;;; FR-140: VM-local helpers for the runtime's immediate-symbol bit patterns.
;;; Kept here (rather than depending on cl-cc/runtime) because cl-cc-vm is built
;;; as an independent ASDF system.
(defconstant +vm-immediate-symbol-base+ #x7FFF000000000100)
(defconstant +vm-immediate-symbol-mask+ #xFFFFFFFFFFFFFF00)
(defconstant +vm-immediate-symbol-index-mask+ #xFF)

(defparameter *vm-immediate-symbol-table*
  #(:key :value :test :test-not :start :end :from-end :count :initial-value
    :element-type :initial-element :allow-other-keys :adjustable :fill-pointer
    quote lambda function declare setq setf if progn let let* block return-from
    tagbody go catch throw unwind-protect flet labels macrolet symbol-macrolet
    the values multiple-value-bind multiple-value-call eval-when locally and or
    cond case typecase ecase ccase loop do do* dolist dotimes defun defmacro
    defvar defparameter defconstant defclass defmethod defgeneric car cdr cons
    list append apply funcall))

(defparameter *vm-immediate-symbol-indexes*
  (let ((table (make-hash-table :test #'eq)))
    (loop for sym across *vm-immediate-symbol-table*
          for i from 0
          do (setf (gethash sym table) i))
    table))

(defmethod vm-immediate-symbol-p ((value t))
  (and (typep value '(unsigned-byte 64))
       (= (logand value +vm-immediate-symbol-mask+) +vm-immediate-symbol-base+)))

(defmethod vm-decode-symbol ((value t))
  (if (vm-immediate-symbol-p value)
      (svref *vm-immediate-symbol-table*
             (logand value +vm-immediate-symbol-index-mask+))
      value))

(defmethod vm-encode-common-symbol ((symbol t))
  (or (let ((index (and (symbolp symbol)
                        (gethash symbol *vm-immediate-symbol-indexes*))))
        (and index (logior +vm-immediate-symbol-base+ index)))
      symbol))

(defmethod vm-immediate-intern-enabled-p ()
  "Avoid immediate symbols in VM implementation and configured guest packages."
  (let* ((package (or *package* (find-package :cl-user)))
         (name (and package (package-name package)))
         (guest-function-package (find-package *vm-guest-function-package*)))
    (not (and name
              (or (eq package guest-function-package)
                  (and (>= (length name) 6)
                       (string= name "CL-CC/" :end1 6)))))))

;;; ─── Multi-VM instance support (FR-813) ─────────────────────────────────────

(defstruct vm-parent-environment
  "Read-only shared environment used as a parent for VM instances."
  (globals (make-hash-table :test #'eq) :read-only t)
  (functions (make-hash-table :test #'eq) :read-only t)
  (symbols (make-hash-table :test #'eq) :read-only t))

(defvar *vm-instance-parent-envs* (make-hash-table :test #'eq :weakness :key)
  "Weak map from vm-state objects to their optional shared parent environment.")

(defvar *vm-instance-locks* (make-hash-table :test #'eq :weakness :key)
  "Weak map from vm-state objects to per-instance mutexes.")

(defun vm-instance-parent-env (state)
  "Return STATE's read-only parent environment, if any."
  (gethash state *vm-instance-parent-envs*))

(defun vm-instance-lock (state)
  "Return STATE's isolation mutex, creating it lazily when needed."
  (or (gethash state *vm-instance-locks*)
      (setf (gethash state *vm-instance-locks*)
            (cl-cc/runtime:rt-make-lock "cl-cc/vm instance lock"))))

(defmacro with-vm-instance-lock ((state) &body body)
  "Execute BODY while holding STATE's per-instance isolation mutex."
  `(cl-cc/runtime:rt-with-lock ((vm-instance-lock ,state))
     ,@body))


(defun %vm-parent-hash (parent-env reader)
  "Return one hash table from PARENT-ENV using READER, accepting raw tables too."
  (cond
    ((null parent-env) nil)
    ((typep parent-env 'vm-parent-environment) (funcall reader parent-env))
    ((hash-table-p parent-env) parent-env)
    (t nil)))

(defun make-vm-instance (&key parent-env)
  "Create an independent vm-state with optional read-only PARENT-ENV.

Each instance receives fresh heap, register, global, function, class, and symbol
tables through vm-state initialization.  PARENT-ENV is recorded separately and is
consulted by helper lookups without mutating the shared environment."
  (let ((state (make-instance 'vm-state)))
    (when parent-env
      (setf (gethash state *vm-instance-parent-envs*) parent-env))
    (setf (gethash state *vm-instance-locks*)
          (cl-cc/runtime:rt-make-lock "cl-cc/vm instance lock"))
    state))

(defun vm-instance-global-value (state symbol &optional default)
  "Return SYMBOL's value from STATE or its read-only parent environment."
  (multiple-value-bind (value present-p)
      (gethash symbol (vm-global-vars state))
    (if present-p
        (values value t)
        (let ((parent-globals (%vm-parent-hash (vm-instance-parent-env state)
                                               #'vm-parent-environment-globals)))
          (if parent-globals
              (gethash symbol parent-globals default)
              (values default nil))))))

(defun vm-instance-function-value (state symbol &optional default)
  "Return SYMBOL's function from STATE or its read-only parent environment."
  (multiple-value-bind (value present-p)
      (gethash symbol (vm-function-registry state))
    (if present-p
        (values value t)
        (let ((parent-functions (%vm-parent-hash (vm-instance-parent-env state)
                                                 #'vm-parent-environment-functions)))
          (if parent-functions
              (gethash symbol parent-functions default)
              (values default nil))))))

(defun transfer-value (value from-vm to-vm)
  "Transfer VALUE from FROM-VM to TO-VM through readable serialization.

The current VM stores host-readable values, so a write/read round trip provides a
stable ownership boundary and avoids sharing mutable cons/vector/hash instances
between VM heaps.  Heap addresses are serialized as tagged address descriptors so
callers do not accidentally dereference addresses from another instance."
  (declare (ignore to-vm))
  (with-vm-instance-lock (from-vm)
    (vm-call-with-readable-printing
  (lambda ()
    (read-from-string
       (write-to-string
        (typecase value
          (vm-heap-address (list :vm-heap-address (vm-heap-address-value value)))
          (t value))))))))

;;; FR-155: Deoptimization / OSR metadata and checkpoint instructions.

(defstruct vm-deopt-info
  "Interpreter-state reconstruction metadata for one optimized-code PC."
  (pc nil)
  (label nil)
  (live-regs nil)
  (vreg->preg nil)
  (inline-stack nil)
  (env nil)
  (description nil))

(defstruct vm-deopt-frame
  "Runtime snapshot captured when a type guard or checkpoint deoptimizes."
  (pc nil)
  (reason nil)
  (registers nil)
  (physical-registers nil)
  (call-stack nil)
  (closure-env nil)
  (values-list nil)
  (inline-stack nil)
  (info nil))

(defconstant +maximum-multiple-values+ 64
  "Initial allocation-free capacity of the VM multiple-values frame buffer.")

(defstruct vm-load-time-value-cell
  "Serialized load-time-value cell metadata stored in VM programs/FASLs."
  (id 0 :type integer)
  (form nil)
  (read-only-p nil)
  (resolved-p nil)
  (value nil))

(defparameter *deopt-enabled* nil
  "When true, optimistic guards capture FR-522 deoptimization maps and frames.
When NIL, guards still take their safe interpreter fallback edge without storing
deoptimization history. Tests should dynamically bind this to T for deopt coverage.")

(defparameter *osr-enabled* nil
  "When true, loop-header OSR entries may transfer hot interpreter loops to Tier-1 code.
Tests should dynamically bind this to T for OSR coverage.")

(defvar *vm-current-program-deopt-info* nil
  "Dynamically bound PC -> deoptimization metadata for the currently running program.")

(define-vm-instruction vm-type-check (vm-instruction)
  "Runtime type guard for optimized code.  On failure, deoptimizes to LABEL."
  (src nil :reader vm-src)
  (type-name nil :reader vm-type-name)
  (label nil :reader vm-type-check-deopt-label)
  (id nil :reader vm-type-check-deopt-id)
  (:sexp-tag :type-check)
  (:sexp-slots src type-name label id))

(define-vm-instruction vm-deopt (vm-instruction)
  "Explicit deoptimization checkpoint.  Saves VM state and resumes in interpreter code."
  (label nil :reader vm-deopt-label)
  (id nil :reader vm-deopt-id)
  (reason :checkpoint :reader vm-deopt-reason)
  (:sexp-tag :deopt)
  (:sexp-slots label id reason))

(define-vm-instruction vm-osr-entry (vm-instruction)
  "Loop back-edge marker eligible for on-stack replacement into Tier-1 code."
  (label nil :reader vm-osr-label)
  (id nil :reader vm-osr-id)
  (:sexp-tag :osr-entry)
  (:sexp-slots label id))

;;; Environment introspection (FR-612: lisp-implementation-type, machine-*,
;;; software-*, room, apropos*) is in vm-environment-introspection.lisp.
;;; Readtable API (FR-607) is in vm-readtable.lisp (loaded immediately after).
;;; Symbol table and package locks (FR-622/FR-896) are in vm-symbol-table.lisp
;;; (loaded after vm-dsl.lisp).
;;;
;;; Shorthand macros (define-simple-instruction, define-vm-unary-instruction,
;;; define-vm-binary-instruction, define-vm-char-comparison), vm-program,
;;; and vm-state are in vm-dsl.lisp (loaded immediately after this file).
;;;
;;; VM state initialization, profiling, heap ops, and execute-instruction
;;; generic are in vm-state-init.lisp (loaded after vm-dsl).
;;;
;;; vm-bridge.lisp  — host function bridge + CLOS slot-definition helpers
;;; vm-execute.lisp — execute-instruction methods for core instructions
;;; vm-clos.lisp    — CLOS instruction defstructs + execute-instruction methods
;;; vm-run.lisp     — Handler-case, label table, run-vm, vm2-state
