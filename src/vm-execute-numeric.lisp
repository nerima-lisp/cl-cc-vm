(in-package :cl-cc/vm)

;;;; Continuation/trampoline machinery (vm-capture-continuation,
;;;; vm-invoke-continuation, vm-force-trampoline-result) and the 51-bit
;;;; fixnum/bignum/rational arithmetic helpers overflow-checked numeric
;;;; instructions dispatch through, plus those instructions'
;;;; execute-instruction methods.

(defconstant +vm-min-fixnum51+ (- (ash 1 50))
  "Smallest integer encodable by the VM's 51-bit signed fixnum representation.")

(defconstant +vm-max-fixnum51+ (1- (ash 1 50))
  "Largest integer encodable by the VM's 51-bit signed fixnum representation.")

(defstruct vm-trampoline-thunk
  "A VM-level zero-argument thunk for external trampoline dispatch."
  (function (lambda () nil) :type function))

(defstruct (vm-continuation (:constructor %make-vm-continuation))
  "Heap-copied VM continuation snapshot.

The register file, call stack, handler stack, method stack, prompt stack,
physical stack metadata, and closure environment are copied at capture time.
Invocation copies or restores each snapshot again, keeping continuations
multi-shot. KIND is :FULL, :ESCAPE, or :DELIMITED; escape continuations are
one-shot by convention."
  (kind :full :type keyword)
  pc
  dst-reg
  registers
  call-stack
  handler-stack
  method-call-stack
  prompt-stack
  stack-segment-snapshot
  closure-env
  values-list
  labels
  (used-p nil :type boolean))

(defun %vm-copy-register-table (table)
  (typecase table
    (hash-table
     (let ((copy (make-hash-table :test (hash-table-test table))))
       (maphash (lambda (key value) (setf (gethash key copy) value)) table)
       copy))
    (simple-vector
     (copy-seq table))
    (vector
     (copy-seq table))
    (t
     (error "Unsupported VM register file: ~S" table))))

(defun %vm-stack-segment-snapshot (state)
  (cl-cc/runtime::stack-segment-snapshot (vm-current-stack-segment state)))

(defun %vm-replace-stack-segments (state snapshot)
  "Replace STATE's chain only after SNAPSHOT has restored successfully."
  (let ((replacement (cl-cc/runtime::stack-segment-restore snapshot))
        (current (vm-current-stack-segment state)))
    (cl-cc/runtime::release-stack-segment-chain current)
    (setf (vm-current-stack-segment state) replacement)))

(defun vm-capture-continuation (state pc dst-reg &key (kind :full) prompt-frame labels)
  "Copy STATE's VM control stack to a reusable heap continuation object."
  (let* ((call-stack (copy-tree (vm-call-stack state)))
         (method-stack (copy-tree (vm-method-call-stack state)))
         (handler-stack (copy-tree (vm-handler-stack state)))
         (prompt-stack (copy-tree (vm-continuation-prompts state)))
         (stack-segment-snapshot
           (if prompt-frame
               (copy-tree (getf prompt-frame :stack-segment-snapshot))
               (%vm-stack-segment-snapshot state))))
    (when (and prompt-frame (member prompt-frame prompt-stack :test #'equal))
      (setf call-stack (copy-tree (getf prompt-frame :call-stack))
            method-stack (copy-tree (getf prompt-frame :method-call-stack))
            handler-stack (copy-tree (getf prompt-frame :handler-stack))))
    (%make-vm-continuation
     :kind kind
     :pc pc
     :dst-reg dst-reg
     :registers (%vm-copy-register-table (vm-state-registers state))
     :call-stack call-stack
     :handler-stack handler-stack
     :method-call-stack method-stack
     :prompt-stack prompt-stack
     :stack-segment-snapshot stack-segment-snapshot
     :closure-env (vm-closure-env state)
     :values-list (copy-list (vm-values-list state))
     :labels labels)))

(defun vm-invoke-continuation (state continuation value)
  "Restore CONTINUATION into STATE, store VALUE, and return the resume PC."
  (when (and (eq (vm-continuation-kind continuation) :escape)
             (vm-continuation-used-p continuation))
    (error "Escape continuation invoked more than once: ~S" continuation))
  (setf (vm-continuation-used-p continuation) t)
  (%vm-replace-stack-segments
   state (vm-continuation-stack-segment-snapshot continuation))
  (vm-restore-registers state (%vm-copy-register-table (vm-continuation-registers continuation)))
  (setf (vm-call-stack state) (copy-tree (vm-continuation-call-stack continuation))
        (vm-handler-stack state) (copy-tree (vm-continuation-handler-stack continuation))
        (vm-method-call-stack state) (copy-tree (vm-continuation-method-call-stack continuation))
        (vm-continuation-prompts state) (copy-tree (vm-continuation-prompt-stack continuation))
        (vm-closure-env state) (vm-continuation-closure-env continuation)
        (vm-values-list state) (copy-list (vm-continuation-values-list continuation))
        (vm-stack-depth state) (length (vm-continuation-call-stack continuation)))
  (vm-reg-set state (vm-continuation-dst-reg continuation) value)
  (vm-continuation-pc continuation))

(defun vm-force-trampoline-result (value)
  "Force VM trampoline thunks until VALUE is no longer a thunk."
  (loop for current = value then (funcall (vm-trampoline-thunk-function current))
        while (vm-trampoline-thunk-p current)
        finally (return current)))

(defun vm-fixnum51-p (value)
  "Return T when VALUE fits in the VM 51-bit signed fixnum range."
  (and (integerp value)
       (<= +vm-min-fixnum51+ value +vm-max-fixnum51+)))

(defun vm-fixnum51-overflow-p (value)
  "Return T when integer VALUE cannot be represented as a VM fixnum."
  (and (integerp value)
       (not (vm-fixnum51-p value))))

(defun %vm-fixnum-rational-p (value)
  "Return T when VALUE is a rational with fixnum numerator and denominator."
  (and (rationalp value)
       (typep (numerator value) 'fixnum)
       (typep (denominator value) 'fixnum)))

(defun %vm-rational-add (lhs rhs)
  "Fast path for rational addition with fixnum numerators and denominators."
  (let* ((a (numerator lhs))
         (b (denominator lhs))
         (c (numerator rhs))
         (d (denominator rhs))
         (g (gcd b d))
         (d/g (/ d g)))
    (/ (+ (* a d/g) (* c (/ b g)))
       (* b d/g))))

(defun %vm-rational-sub (lhs rhs)
  "Fast path for rational subtraction with fixnum numerators and denominators."
  (let* ((a (numerator lhs))
         (b (denominator lhs))
         (c (numerator rhs))
         (d (denominator rhs))
         (g (gcd b d))
         (d/g (/ d g)))
    (/ (- (* a d/g) (* c (/ b g)))
       (* b d/g))))

(defun %vm-rational-mul (lhs rhs)
  "Fast path for rational multiplication with cross-cancellation."
  (let* ((a (numerator lhs))
         (b (denominator lhs))
         (c (numerator rhs))
         (d (denominator rhs))
         (g1 (gcd (abs a) d))
         (g2 (gcd (abs c) b)))
    (/ (* (/ a g1) (/ c g2))
       (* (/ b g2) (/ d g1)))))

(defun %vm-externalize-number (value)
  "Convert native VM number tower values to guest-visible CL numeric values."
  (cond
    ((vm-bignum-p value) (vm-bignum-to-integer value))
    ((vm-ratio-p value) (%vm-externalize-ratio value))
    ((vm-complex-p value) (%vm-externalize-complex value))
    (t value)))

(defun %vm-complex-number-p (value)
  (or (complexp value) (vm-complex-p value)))

(defun %vm-exact-rational-number-p (value)
  (or (rationalp value) (vm-ratio-p value) (vm-bignum-p value)))

(defun %vm-float-contagion-p (&rest values)
  (some #'floatp values))

(defun %vm-add-with-overflow-fallback (lhs rhs)
  (cond
    ((or (%vm-complex-number-p lhs) (%vm-complex-number-p rhs))
     (%vm-externalize-number (vm-complex-add lhs rhs)))
    ((%vm-float-contagion-p lhs rhs)
     (+ lhs rhs))
    ((and (typep lhs 'fixnum) (typep rhs 'fixnum))
     (let ((result (+ lhs rhs)))
       (if (vm-fixnum51-overflow-p result)
            (vm-bignum-add-integers lhs rhs)
            result)))
    ((and (integerp lhs) (integerp rhs)
          (or (vm-fixnum51-overflow-p lhs)
              (vm-fixnum51-overflow-p rhs)))
     (vm-bignum-add-integers lhs rhs))
    ((and (%vm-exact-rational-number-p lhs) (%vm-exact-rational-number-p rhs))
     (%vm-externalize-number (vm-rational-add lhs rhs)))
    ((and (%vm-fixnum-rational-p lhs) (%vm-fixnum-rational-p rhs))
      (%vm-rational-add lhs rhs))
    (t
     (+ lhs rhs))))

(defun %vm-sub-with-overflow-fallback (lhs rhs)
  (cond
    ((or (%vm-complex-number-p lhs) (%vm-complex-number-p rhs))
     (%vm-externalize-number (vm-complex-sub lhs rhs)))
    ((%vm-float-contagion-p lhs rhs)
     (- lhs rhs))
    ((and (typep lhs 'fixnum) (typep rhs 'fixnum))
     (let ((result (- lhs rhs)))
       (if (vm-fixnum51-overflow-p result)
            (vm-bignum-subtract-integers lhs rhs)
            result)))
    ((and (integerp lhs) (integerp rhs)
          (or (vm-fixnum51-overflow-p lhs)
              (vm-fixnum51-overflow-p rhs)))
     (vm-bignum-subtract-integers lhs rhs))
    ((and (%vm-exact-rational-number-p lhs) (%vm-exact-rational-number-p rhs))
     (%vm-externalize-number (vm-rational-sub lhs rhs)))
    ((and (%vm-fixnum-rational-p lhs) (%vm-fixnum-rational-p rhs))
      (%vm-rational-sub lhs rhs))
    (t
     (- lhs rhs))))

(defun %vm-mul-with-overflow-fallback (lhs rhs)
  (cond
    ((or (%vm-complex-number-p lhs) (%vm-complex-number-p rhs))
     (%vm-externalize-number (vm-complex-mul lhs rhs)))
    ((%vm-float-contagion-p lhs rhs)
     (* lhs rhs))
    ((and (typep lhs 'fixnum) (typep rhs 'fixnum))
     (let ((result (* lhs rhs)))
       (if (vm-fixnum51-overflow-p result)
            (vm-bignum-multiply-integers lhs rhs)
            result)))
    ((and (integerp lhs) (integerp rhs)
          (or (typep lhs 'bignum) (typep rhs 'bignum)))
     (vm-bignum-multiply-integers lhs rhs))
    ((and (%vm-exact-rational-number-p lhs) (%vm-exact-rational-number-p rhs))
     (%vm-externalize-number (vm-rational-mul lhs rhs)))
    ((and (%vm-fixnum-rational-p lhs) (%vm-fixnum-rational-p rhs))
      (%vm-rational-mul lhs rhs))
    (t
     (* lhs rhs))))

(defmethod execute-instruction ((inst vm-add) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (%vm-add-with-overflow-fallback
               (vm-reg-get state (vm-lhs inst))
               (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-sub) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (%vm-sub-with-overflow-fallback
               (vm-reg-get state (vm-lhs inst))
               (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-mul) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (%vm-mul-with-overflow-fallback
               (vm-reg-get state (vm-lhs inst))
               (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

;;; Checked arithmetic (FR-149): detect VM fixnum overflow and use the bignum
;;; fallback path. Native backends lower these to hardware overflow checks.
(defmethod execute-instruction ((inst vm-add-checked) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (%vm-add-with-overflow-fallback
               (vm-reg-get state (vm-lhs inst))
               (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-sub-checked) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (%vm-sub-with-overflow-fallback
               (vm-reg-get state (vm-lhs inst))
               (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-mul-checked) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (%vm-mul-with-overflow-fallback
               (vm-reg-get state (vm-lhs inst))
               (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-label) state pc labels)
  (declare (ignore state labels))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-jump) state pc labels)
  (declare (ignore state pc))
  (values (vm-label-table-lookup labels (vm-label-name inst)) nil nil))

(defun vm-falsep (value)
  "Return T if VALUE is falsy.

The cl-cc execution model treats both NIL and numeric zero as false."
  (or (null value)
      (and (numberp value)
           (zerop value))))

(defmethod execute-instruction ((inst vm-jump-zero) state pc labels)
  (if (vm-falsep (vm-reg-get state (vm-reg inst)))
      (values (vm-label-table-lookup labels (vm-label-name inst)) nil nil)
      (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-select) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (if (vm-falsep (vm-reg-get state (vm-select-cond-reg inst)))
                  (vm-reg-get state (vm-select-else-reg inst))
                  (vm-reg-get state (vm-select-then-reg inst))))
  (values (1+ pc) nil nil))
