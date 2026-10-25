(in-package :cl-cc/vm/protocols)

;;; VM Instruction DSL (Phase 8/9)

(defstruct vm-instruction
  "Base struct for all VM instructions.")

(defvar *instruction-constructors* (make-hash-table :test 'eq)
  "Hash table mapping sexp tags (keywords) to constructor functions for sexp->instruction.")

(defgeneric instruction->sexp (instruction)
  (:documentation "Convert a VM instruction to its sexp representation."))

(defgeneric sexp->instruction (sexp)
  (:documentation "Convert a sexp to a VM instruction."))

(defmethod sexp->instruction ((sexp cons))
  (let ((constructor (gethash (car sexp) *instruction-constructors*)))
    (if constructor
        (funcall constructor sexp)
        (error "Unknown instruction sexp: ~S" sexp))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %vm-instruction-sexp-position-form (position)
    "Return the SEXP accessor form for the 1-based instruction operand POSITION."
    (case position
      (1 '(second sexp))
      (2 '(third sexp))
      (3 '(fourth sexp))
      (4 '(fifth sexp))
      (5 '(sixth sexp))
      (6 '(seventh sexp))
      (t `(nth ,position sexp))))

  (defun %vm-instruction-constructor-args (slot-names &optional (position 1))
    "Build keyword/value constructor arguments for SLOT-NAMES from an instruction SEXP."
    (if (null slot-names)
        nil
        (append (list (intern (symbol-name (car slot-names)) :keyword)
                      (%vm-instruction-sexp-position-form position))
                (%vm-instruction-constructor-args (cdr slot-names) (+ position 1))))))

(defmacro define-vm-instruction (name (parent) &body body)
  "Define a VM instruction as a defstruct with auto-generated sexp serialization.
NAME is the struct name. PARENT is the parent struct to :include.
BODY contains slot definitions, an optional docstring, and options.

Slot definition: (slot-name initform &key reader type)
Options:
  (:sexp-tag :keyword) - keyword for instruction->sexp / sexp->instruction
  (:sexp-slots s1 s2 ...) - slot names in sexp field order (default: all own slots)"
  (let (docstring slots sexp-tag sexp-slots conc-name-override)
    (dolist (form body)
      (cond
        ((stringp form) (setf docstring form))
        ((and (consp form) (eq (car form) :sexp-tag))
         (setf sexp-tag (cadr form)))
        ((and (consp form) (eq (car form) :sexp-slots))
         (setf sexp-slots (cdr form)))
        ((and (consp form) (eq (car form) :conc-name))
         (setf conc-name-override (cadr form)))
        ((consp form) (push form slots))
        (t (error "Invalid form in define-vm-instruction: ~S" form))))
    (setf slots (nreverse slots))
     (unless sexp-slots
       (setf sexp-slots (mapcar #'car slots)))
     (let ((prefix (if conc-name-override
                       (string conc-name-override)
                       (concatenate 'string (symbol-name name) "-"))))
       (flet ((struct-acc (slot)
                (intern (format nil "~A~A" prefix (symbol-name slot))))
              (ctor ()
                (intern (format nil "MAKE-~A" (symbol-name name)))))
         (let* ((struct-options (append (list name (list :include parent))
                                        (when conc-name-override
                                          (list (list :conc-name conc-name-override)))))
                (struct-slots
                  (mapcar (lambda (slot)
                            (let ((sname (car slot))
                                  (init (cadr slot))
                                  (type (getf (cddr slot) :type)))
                              (if type
                                  (list sname init :type type)
                                  (list sname init))))
                          slots))
                (struct-form
                  (append (list 'defstruct struct-options)
                          (when docstring (list docstring))
                          struct-slots))
                (reader-forms
                  (loop for slot in slots
                        for sname = (car slot)
                        for reader = (getf (cddr slot) :reader)
                        when (and reader (not (eq reader (struct-acc sname))))
                        collect (list 'eval-when
                                      '(:compile-toplevel :load-toplevel :execute)
                                      (list 'unless
                                            (list 'fboundp (list 'quote reader))
                                            (list 'defgeneric reader '(inst)))
                                      (list 'defmethod reader
                                            (list (list 'inst name))
                                            (list (struct-acc sname) 'inst)))))
                 (sexp-forms
                   (when sexp-tag
                     (let ((ctor-args
                             (%vm-instruction-constructor-args sexp-slots)))
                       (list
                        (list 'defmethod 'instruction->sexp
                              (list (list 'inst name))
                              (cons 'list
                                    (cons sexp-tag
                                          (mapcar (lambda (s)
                                                    (list (struct-acc s) 'inst))
                                                  sexp-slots))))
                         (list 'setf
                               (list 'gethash sexp-tag '*instruction-constructors*)
                               (append (list 'lambda '(sexp))
                                       (unless sexp-slots
                                         (list '(declare (ignore sexp))))
                                       (list (cons (ctor) ctor-args)))))))))
            ;; Immediate-symbol :around methods for vm-intern-symbol,
            ;; vm-symbol-name, vm-keywordp, vm-symbol-p live in symbols.lisp
            ;; and primitives.lisp where those instructions are defined.
            (cons 'progn
                  (append (list struct-form)
                          reader-forms
                          sexp-forms)))))))

;;; ─── Execute-instruction Generic + Utilities ─────────────────────────────────

(defgeneric execute-instruction (instruction state pc labels))

(defgeneric vm-reg-get (state reg)
  (:documentation "Read register REG from STATE."))

(defgeneric vm-reg-set (state reg value)
  (:documentation "Write VALUE into register REG of STATE and return VALUE."))

;;; Simple Instruction Method Generator

(defmacro define-simple-instruction (name shape cl-func &key (src 'vm-src) (dst 'vm-dst) (lhs 'vm-lhs) (rhs 'vm-rhs))
  "Generate a simple execute-instruction method.
SHAPE is one of:
  :unary  — (CL-FUNC (vm-reg-get state (SRC inst))) → dst
  :binary — (CL-FUNC (vm-reg-get state (LHS inst)) (vm-reg-get state (RHS inst))) → dst
  :pred1  — (if (CL-FUNC (vm-reg-get state (SRC inst))) 1 0) → dst
  :pred2  — (if (CL-FUNC (vm-reg-get state (LHS inst)) (vm-reg-get state (RHS inst))) 1 0) → dst"
  (let ((inst (gensym "INST"))
        (state (gensym "STATE"))
        (pc (gensym "PC")))
    (let ((value-form
            (ecase shape
              (:unary
               (list cl-func (list 'vm-reg-get state (list src inst))))
              (:binary
               (list cl-func
                     (list 'vm-reg-get state (list lhs inst))
                     (list 'vm-reg-get state (list rhs inst))))
              (:pred1
               (list 'if
                     (list cl-func (list 'vm-reg-get state (list src inst)))
                     1
                     0))
              (:pred2
               (list 'if
                     (list cl-func
                           (list 'vm-reg-get state (list lhs inst))
                           (list 'vm-reg-get state (list rhs inst)))
                     1
                     0)))))
      (list 'defmethod 'execute-instruction
            (list (list inst name) state pc 'labels)
            (list 'declare (list 'ignore 'labels))
            (list 'vm-reg-set state (list dst inst) value-form)
            (list 'values (list '1+ pc) nil nil)))))

;;; Instruction Structure Shorthand Macros

(defmacro define-vm-unary-instruction (name tag docstring)
  "Define a VM instruction with a single (dst src) slot pair."
  `(define-vm-instruction ,name (vm-instruction)
     ,docstring
     (dst nil :reader vm-dst)
     (src nil :reader vm-src)
     (:sexp-tag ,tag)
     (:sexp-slots dst src)))

(defmacro define-vm-binary-instruction (name tag docstring)
  "Define a VM instruction with (dst lhs rhs) slots for binary operations."
  `(define-vm-instruction ,name (vm-instruction)
     ,docstring
     (dst nil :reader vm-dst)
     (lhs nil :reader vm-lhs)
     (rhs nil :reader vm-rhs)
     (:sexp-tag ,tag)
     (:sexp-slots dst lhs rhs)))

(defmacro define-vm-char-comparison (name tag docstring)
  "Define a VM instruction with (dst char1 char2) slots for binary char comparisons."
  `(define-vm-instruction ,name (vm-instruction)
     ,docstring
     (dst nil :reader vm-dst)
     (char1 nil :reader vm-char1)
     (char2 nil :reader vm-char2)
     (:sexp-tag ,tag)
     (:sexp-slots dst char1 char2)))

(defgeneric vm-typep-check (value type-sym) (:documentation "Check if VALUE is of TYPE-SYM using table dispatch for primitive and compound types."))

(defgeneric %vm-typep-call-predicate (value predicate) (:documentation "Invoke PREDICATE on VALUE, supporting function/symbol/lambda forms."))

(defgeneric %vm-typep-range-check (value low high) (:documentation "Return true when VALUE is inside LOW/HIGH bounds from CL numeric type specs."))

(defgeneric vm-closure-object-p (object))

(defgeneric vm-check-index (sequence index &optional operation) (:documentation "Check INDEX for SEQUENCE when safety is enabled; signal TYPE-ERROR on failure."))

(defgeneric vm-check-row-major-index (array index))

(defgeneric vm-safety-checks-enabled-p ())

(defgeneric vm-state-heap (state))

(defgeneric vm-values-list (state))
(defgeneric (setf vm-values-list) (value state))

(defmethod vm-closure-object-p ((object t)) (declare (ignore object)) nil)

(defmethod vm-state-heap ((state t)) (declare (ignore state)) nil)

(defvar *vm-self-host-mode* nil
  "When T, VM operations refuse host CL fallbacks.  Package operations
(vm-intern-symbol, vm-find-package) must use the runtime package registry
exclusively; FORMAT must use the native renderer without host SBCL fallback.
This mode exists to validate ANSI CL self-hosting readiness.
Set via (setf *vm-self-host-mode* t) or --self-host CLI flag.")

(defgeneric vm-getenv (name) (:documentation "Return environment variable NAME marked as tainted external input."))

(defgeneric vm-immediate-symbol-p (value) (:documentation "Return true when VALUE is encoded as a VM immediate symbol."))

(defgeneric vm-decode-symbol (value) (:documentation "Decode VALUE when it contains a VM immediate symbol."))

(defgeneric vm-encode-common-symbol (symbol) (:documentation "Encode SYMBOL when it has a VM immediate representation."))

(defgeneric vm-immediate-intern-enabled-p () (:documentation "Return whether immediate symbol encoding is enabled in the current package."))
