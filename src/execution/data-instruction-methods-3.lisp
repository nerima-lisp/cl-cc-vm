(in-package #:cl-cc/vm/execution)

(defmethod execute-instruction ((inst vm-rplaca) state pc labels)
  (declare (ignore labels))
  (let ((cons-val (vm-reg-get state (vm-cons-reg inst)))
        (new-val (vm-reg-get state (vm-val-reg inst))))
    (rplaca (%vm-cow-list-ensure-writable cons-val) new-val)
    (vm-reg-set state (vm-cons-reg inst) cons-val)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-nreverse) state pc labels)
  (declare (ignore labels))
  (let* ((value (vm-reg-get state (vm-src inst)))
         (writable (%vm-cow-list-ensure-writable value)))
    (vm-reg-set state (vm-dst inst) (nreverse writable))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction :around ((inst vm-butlast) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (if (%vm-managed-cons-pointer-p value)
        (progn
          (vm-reg-set state (vm-dst inst)
                      (butlast (%vm-managed-cons-materialize state value)))
          (values (1+ pc) nil nil))
        (call-next-method))))

(defmethod execute-instruction :around ((inst vm-last) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (if (%vm-managed-cons-pointer-p value)
        (progn
          (vm-reg-set state (vm-dst inst)
                      (last (%vm-managed-cons-materialize state value)))
          (values (1+ pc) nil nil))
        (call-next-method))))

(defmethod execute-instruction :around ((inst vm-rest) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (if (%vm-managed-cons-pointer-p value)
        (progn
          (vm-reg-set state (vm-dst inst) (%vm-managed-cons-cdr state value))
          (values (1+ pc) nil nil))
        (call-next-method))))

(defmethod execute-instruction ((inst vm-nthcdr) state pc labels)
  (declare (ignore labels))
  (let ((index (vm-reg-get state (vm-index-reg inst)))
        (list (vm-reg-get state (vm-list-reg inst))))
    (vm-reg-set state (vm-dst inst)
                (if (%vm-managed-cons-pointer-p list)
                    (%vm-managed-list-tail state list index)
                    (nthcdr index (%vm-cow-list-materialize list))))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-nth) state pc labels)
  (declare (ignore labels))
  (let ((index (vm-reg-get state (vm-index-reg inst)))
        (sequence (vm-reg-get state (vm-list-reg inst))))
    (vm-reg-set state (vm-dst inst)
                (let ((*vm-managed-cons-sequence-state* state))
                  (vm-sequence-elt sequence index)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-member) state pc labels)
  (declare (ignore labels))
  (let ((item (vm-reg-get state (vm-item-reg inst)))
        (list (vm-reg-get state (vm-list-reg inst))))
    (vm-reg-set state (vm-dst inst)
                (if (%vm-managed-cons-pointer-p list)
                    (%vm-managed-member state item list)
                    (member item (%vm-cow-list-materialize list))))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction :around ((inst vm-append) state pc labels)
  (declare (ignore labels))
  (let ((lhs (vm-reg-get state (vm-lhs inst)))
        (rhs (vm-reg-get state (vm-rhs inst))))
    (if (or (%vm-managed-cons-pointer-p lhs)
            (%vm-managed-cons-pointer-p rhs))
        (progn
          (vm-reg-set state (vm-dst inst)
                      (append (%vm-managed-cons-materialize state lhs)
                              (%vm-managed-cons-materialize state rhs)))
          (values (1+ pc) nil nil))
        (call-next-method))))

(defmethod execute-instruction :around ((inst vm-reverse) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (if (%vm-managed-cons-pointer-p value)
        (progn
          (vm-reg-set state (vm-dst inst)
                      (reverse (%vm-managed-cons-materialize state value)))
          (values (1+ pc) nil nil))
        (call-next-method))))

;;; Instruction Execution - List Accessors

(defmethod execute-instruction ((inst vm-length) state pc labels)
  (declare (ignore labels))
  (let ((sequence (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst)
                (let ((*vm-managed-cons-sequence-state* state))
                  (vm-sequence-length sequence)))
    (values (1+ pc) nil nil)))

;;; Instruction Execution - List Construction

(defmethod execute-instruction ((inst vm-make-list) state pc labels)
  (declare (ignore labels))
  (let ((n (vm-reg-get state (vm-size inst))))
    (vm-reg-set state (vm-dst inst) (make-list n))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-cdr) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst) (cdr (%vm-cow-list-materialize value)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-car) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst) (car (%vm-cow-list-materialize value)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-hash-cons) state pc labels)
  (declare (ignore labels))
  (let ((car-val (vm-reg-get state (vm-car-reg inst)))
        (cdr-val (vm-reg-get state (vm-cdr-reg inst))))
    (vm-reg-set state (vm-dst inst) (vm-hash-cons car-val cdr-val))
    (values (1+ pc) nil nil)))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — List Instruction Execution Methods
;;;
;;; Contains all execute-instruction methods for list VM instructions:
;;; cons/car/cdr, make-list, length/member/nth/nthcdr, named accessors
;;; (first-tenth), rest/last/butlast, nreverse/nbutlast, push/pop,
;;; assoc/acons/subst, equal, nconc, copy-list/copy-tree, listp/atom.
;;;
;;; Instruction struct definitions (define-vm-instruction forms) and the
;;; hash-cons table + sequence protocol are in list.lisp (loads before).
;;;
;;; Load order: after list.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Instruction Execution - Cons Cell Operations

(defmethod execute-instruction ((inst vm-cons) state pc labels)
  (declare (ignore labels))
  (let ((car-val (vm-reg-get state (vm-car-reg inst)))
        (cdr-val (vm-reg-get state (vm-cdr-reg inst))))
    ;; CL CONS must produce a fresh cell. Reusing globally interned cons cells
    ;; breaks destructive operators (RPLACA/RPLACD/NREVERSE family) and can
    ;; accidentally create cyclic/shared syntax structures in user programs.
    ;; Keep VM-HASH-CONS available as an explicit utility, but ordinary VM-CONS
    ;; follows ANSI CL allocation semantics.
    (vm-reg-set state (vm-dst inst) (cons car-val cdr-val))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-coerce-to-vector) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (coerce (vm-reg-get state (vm-src inst)) 'vector))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-coerce-to-list) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (coerce (vm-reg-get state (vm-src inst)) 'list))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-coerce-to-string) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (coerce (vm-reg-get state (vm-src inst)) 'string))
  (values (1+ pc) nil nil))
