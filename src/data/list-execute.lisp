(in-package #:cl-cc/vm/data)

(define-simple-instruction vm-reverse :unary reverse)
(define-simple-instruction vm-append :binary append)

;;; Instruction Execution - Named Accessors

(define-simple-instruction vm-first :unary first)
(define-simple-instruction vm-second :unary second)
(define-simple-instruction vm-third :unary third)
(define-simple-instruction vm-fourth :unary fourth)
(define-simple-instruction vm-fifth   :unary fifth)
(define-simple-instruction vm-sixth   :unary sixth)
(define-simple-instruction vm-seventh :unary seventh)
(define-simple-instruction vm-eighth  :unary eighth)
(define-simple-instruction vm-ninth   :unary ninth)
(define-simple-instruction vm-tenth   :unary tenth)
(define-simple-instruction vm-rest :unary rest)
(define-simple-instruction vm-last :unary last)
(define-simple-instruction vm-butlast  :unary butlast)
(define-simple-instruction vm-nbutlast :unary nbutlast)

(defmacro define-managed-list-nth-accessor (instruction index)
  `(defmethod execute-instruction :around ((inst ,instruction) state pc labels)
     (declare (ignore labels))
     (let ((value (vm-reg-get state (vm-src inst))))
       (if (%vm-managed-cons-pointer-p value)
           (progn
             (vm-reg-set state (vm-dst inst)
                         (%vm-managed-list-elt state value ,index))
             (values (1+ pc) nil nil))
           (call-next-method)))))

(define-managed-list-nth-accessor vm-first 0)
(define-managed-list-nth-accessor vm-second 1)
(define-managed-list-nth-accessor vm-third 2)
(define-managed-list-nth-accessor vm-fourth 3)
(define-managed-list-nth-accessor vm-fifth 4)
(define-managed-list-nth-accessor vm-sixth 5)
(define-managed-list-nth-accessor vm-seventh 6)
(define-managed-list-nth-accessor vm-eighth 7)
(define-managed-list-nth-accessor vm-ninth 8)
(define-managed-list-nth-accessor vm-tenth 9)

;;; Instruction Execution - Destructive Operations

(defstruct (vm-cow-list (:constructor %make-vm-cow-list))
  (backing nil)
  (refcount 1 :type integer))

(defun %vm-cow-list-materialize (value)
  (if (vm-cow-list-p value)
      (vm-cow-list-backing value)
      value))

(defun %vm-cow-list-ensure-writable (value)
  (if (vm-cow-list-p value)
      (progn
        (when (> (vm-cow-list-refcount value) 1)
          (decf (vm-cow-list-refcount value))
          (setf (vm-cow-list-backing value) (copy-list (vm-cow-list-backing value))
                (vm-cow-list-refcount value) 1))
        (vm-cow-list-backing value))
      value))

(define-simple-instruction vm-nreconc  :binary nreconc)

;;; Instruction Execution - Extended List Operations

(define-simple-instruction vm-list-length :unary list-length)

;;; FR-597: identity / constantly / complement
(define-vm-unary-instruction vm-identity   :identity   "Return argument unchanged.")
(define-vm-unary-instruction vm-constantly :constantly "Return a function that always returns VALUE.")
(define-vm-unary-instruction vm-complement :complement "Return a function that negates the result of PREDICATE.")
(define-simple-instruction vm-identity   :unary identity)
(define-simple-instruction vm-constantly :unary constantly)
(define-simple-instruction vm-complement :unary complement)

;;; Association List and Utility Instructions

(define-vm-instruction vm-assoc (vm-instruction)
  "Look up KEY in ALIST using EQL. Returns the pair or NIL."
  (dst nil :reader vm-dst)
  (key nil :reader vm-key)
  (alist nil :reader vm-alist)
  (:sexp-tag :assoc)
  (:sexp-slots dst key alist))

(define-vm-instruction vm-equal (vm-instruction)
  "Deep structural equality. Returns 1/0."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :equal)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-nconc (vm-instruction)
  "Destructively concatenate two lists."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :nconc)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-copy-list (vm-instruction)
  "Shallow copy of a list."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :copy-list)
  (:sexp-slots dst src))

(define-vm-instruction vm-copy-tree (vm-instruction)
  "Deep copy of a tree (nested cons structure)."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :copy-tree)
  (:sexp-slots dst src))

(define-vm-instruction vm-subst (vm-instruction)
  "Substitute NEW for OLD in TREE."
  (dst nil :reader vm-dst)
  (new-val nil :reader vm-new-val)
  (old-val nil :reader vm-old-val)
  (tree nil :reader vm-tree)
  (:sexp-tag :subst)
  (:sexp-slots dst new-val old-val tree))

(define-vm-instruction vm-listp (vm-instruction)
  "Test if value is a list (nil or cons). Returns 1/0."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :listp)
  (:sexp-slots dst src))

(define-vm-instruction vm-atom (vm-instruction)
  "Test if value is an atom (not a cons). Returns 1/0."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :atom)
  (:sexp-slots dst src))

(in-package #:cl-cc/vm/data)

(define-vm-instruction vm-acons (vm-instruction)
  "Prepend (KEY . VALUE) to ALIST."
  (dst nil :reader vm-dst)
  (key nil :reader vm-key)
  (value nil :reader vm-value)
  (alist nil :reader vm-alist)
  (:sexp-tag :acons)
  (:sexp-slots dst key value alist))
