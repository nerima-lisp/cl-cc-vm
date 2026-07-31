(in-package :cl-cc/vm)

;;; Numeric contagion (ANSI CL §12.1.4 result-type inference) and the
;;; op×tag×tag inline arithmetic dispatch table VM-ARITH-DISPATCH and the
;;; fixnum overflow-detecting add/sub/mul helpers use.

(defparameter +numeric-contagion-types+
  #(integer rational single-float double-float complex)
  "ANSI CL §12.1.4 numeric contagion hierarchy used by the VM.")

(defparameter *numeric-contagion-table*
  (let* ((types +numeric-contagion-types+)
         (size (length types))
         (table (make-array (list size size))))
    (dotimes (i size table)
      (dotimes (j size)
        (setf (aref table i j) (aref types (max i j))))))
  "Type×type matrix for ANSI numeric contagion.  Indices follow
+NUMERIC-CONTAGION-TYPES+: integer < rational < single-float < double-float
< complex.")

(defun numeric-contagion-rank (type)
  "Return TYPE's numeric contagion rank, or NIL for an unsupported type."
  (position type +numeric-contagion-types+ :test #'eq))

(defun numeric-contagion-type-of (value)
  "Return VALUE's contagion type symbol."
  (cond
    ((complexp value) 'complex)
    ((typep value 'double-float) 'double-float)
    ((typep value 'single-float) 'single-float)
    ((integerp value) 'integer)
    ((rationalp value) 'rational)
    ((floatp value) 'double-float)
    (t (error "Not a VM numeric value: ~S" value))))

(defun numeric-contagion-result-type (left-type right-type)
  "Infer the ANSI numeric result type for LEFT-TYPE and RIGHT-TYPE."
  (let ((left-rank (numeric-contagion-rank left-type))
        (right-rank (numeric-contagion-rank right-type)))
    (unless (and left-rank right-rank)
      (error "Unsupported numeric contagion types: ~S × ~S" left-type right-type))
    (aref *numeric-contagion-table* left-rank right-rank)))

(defun infer-numeric-result-type (left right)
  "Compile-time friendly numeric result inference.
LEFT and RIGHT may be either type symbols or literal numeric values."
  (numeric-contagion-result-type
   (if (symbolp left) left (numeric-contagion-type-of left))
   (if (symbolp right) right (numeric-contagion-type-of right))))

(defconstant +arith-op-count+ 4)

(defconstant +arith-tag-count+ 5)

(defconstant +arith-dispatch-table-size+ (* +arith-op-count+ +arith-tag-count+ +arith-tag-count+))

(defparameter +arith-operation-tags+ #(+ - * /))

(defparameter +arith-type-tags+ #(fixnum integer rational float complex))

(defun arithmetic-op-tag (op)
  "Return the inline dispatch operation tag for OP."
  (or (position op +arith-operation-tags+ :test #'eq)
      (error "Unsupported arithmetic dispatch operation: ~S" op)))

(defun arithmetic-type-tag (value)
  "Extract VALUE's arithmetic dispatch type tag."
  (cond
    ((typep value 'fixnum) 0)
    ((integerp value) 1)
    ((rationalp value) 2)
    ((floatp value) 3)
    ((complexp value) 4)
    (t (error "Unsupported arithmetic dispatch operand: ~S" value))))

(defun arithmetic-dispatch-index (op-tag left-tag right-tag)
  "Flatten OP-TAG, LEFT-TAG, RIGHT-TAG into *ARITH-DISPATCH-TABLE* index."
  (+ (* op-tag +arith-tag-count+ +arith-tag-count+)
     (* left-tag +arith-tag-count+)
     right-tag))

(defun %set-arith-dispatch (table op left-tag right-tag function)
  (setf (aref table (arithmetic-dispatch-index (arithmetic-op-tag op) left-tag right-tag))
        (cons (list (aref +arith-type-tags+ left-tag)
                    (aref +arith-type-tags+ right-tag))
              function)))

(defparameter *arith-dispatch-table*
  (let ((table (make-array +arith-dispatch-table-size+ :initial-element nil)))
    (dolist (spec `((+ ,#'+ ,#'+ ,#'+)
                    (- ,#'- ,#'- ,#'-)
                    (* ,#'* ,#'* ,#'*)
                    (/ ,#'/ ,#'/ ,#'/)))
      (destructuring-bind (op ff fi dd) spec
        (%set-arith-dispatch table op 0 0 ff)
        (%set-arith-dispatch table op 0 3 fi)
        (%set-arith-dispatch table op 3 0 fi)
        (%set-arith-dispatch table op 3 3 dd)))
    table)
  "Flattened inline arithmetic dispatch table.  Each populated entry is
((LEFT-TYPE RIGHT-TYPE) . FUNCTION), so tag extraction plus index computation
selects a direct function jump for +, -, *, and /.")

(defun arithmetic-dispatch-entry (op left right)
  "Return the inline dispatch table entry selected by OP, LEFT, and RIGHT."
  (aref *arith-dispatch-table*
        (arithmetic-dispatch-index (arithmetic-op-tag op)
                                   (arithmetic-type-tag left)
                                   (arithmetic-type-tag right))))

(defun inline-arithmetic-dispatch (op left right)
  "Execute OP on LEFT and RIGHT through the inline arithmetic dispatch table.
Missing specialized entries intentionally fall back to host generic arithmetic."
  (let ((entry (arithmetic-dispatch-entry op left right)))
    (if entry
        (funcall (cdr entry) left right)
        (funcall (ecase op
                   (+ #'+) (- #'-) (* #'*) (/ #'/))
                 left right))))

(define-vm-instruction vm-arith-dispatch (vm-binop)
  "Inline arithmetic dispatch instruction.  OP is one of +, -, *, /."
  (op '+ :reader vm-arith-dispatch-op)
  (:sexp-tag :arith-dispatch)
  (:sexp-slots dst lhs rhs op))

(defmethod execute-instruction ((inst vm-arith-dispatch) state pc labels)
  (declare (ignore labels))
  (let* ((lhs (vm-reg-get state (vm-lhs inst)))
         (rhs (vm-reg-get state (vm-rhs inst)))
         (result (inline-arithmetic-dispatch (vm-arith-dispatch-op inst) lhs rhs)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defparameter *vm-arithmetic-safety* 1
  "Runtime arithmetic safety level.  A value of 0 skips explicit overflow checks.")

(defun vm-fixnum-overflow-detected-p (value)
  "Return T when VALUE overflows the VM fixnum representation."
  (and (integerp value)
       (fboundp 'vm-fixnum51-overflow-p)
       (vm-fixnum51-overflow-p value)))

(defun vm-fixnum-add-with-overflow-detection (left right &key (safety *vm-arithmetic-safety*))
  "Add fixnums with x86-64 JO-style overflow detection and bignum promotion.
When SAFETY is 0 the explicit overflow branch is skipped."
  (let ((result (+ left right)))
    (if (and (plusp safety) (vm-fixnum-overflow-detected-p result))
        (vm-bignum-add-integers left right)
        result)))

(defun vm-fixnum-sub-with-overflow-detection (left right &key (safety *vm-arithmetic-safety*))
  "Subtract fixnums with explicit overflow detection unless SAFETY is 0."
  (let ((result (- left right)))
    (if (and (plusp safety) (vm-fixnum-overflow-detected-p result))
        (vm-bignum-subtract-integers left right)
        result)))

(defun vm-fixnum-mul-with-overflow-detection (left right &key (safety *vm-arithmetic-safety*))
  "Multiply fixnums with explicit overflow detection unless SAFETY is 0."
  (let ((result (* left right)))
    (if (and (plusp safety) (vm-fixnum-overflow-detected-p result))
        (vm-bignum-multiply-integers left right)
        result)))
